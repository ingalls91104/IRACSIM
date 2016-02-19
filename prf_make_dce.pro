PRO PRF_MAKE_DCE,i,nx,ny,nz,sclk_obs,treset,framtime,clocktick,x0,y0,peak_dn,dt_subsample,$
                  pointing_model,xcen,ycen,$
                  lightcurve,t_light,prf,x_prf,y_prf,nsamp_clock,rn_per_read,fn,wt,channel,readmode,fovid,gain,$
                  exptime,fluxconv,pxscal1,pxscal2,label,reqkey,avg_readnoise,ronoise,out_dir,orig_header,$
                  object,ra0,dec0,position_angle,ra_multi,dec_multi,$
                  pointing_seed,poisson_seed,readnoise_seed,NODISTORTION=nodistortion,NO_NOISE=no_noise,$
                  SILENT=SILENT,VERBOSE=VERBOSE,FIRSTDCE=firstdce,GROUP_SCLK_START=group_sclk_start,$
                  SURFACE_BRIGHTNESS=SURFACE_BRIGHTNESS,DN=dn,SUBCENTER=subcenter,HEADER_COMMENTS=header_comments,$
                  NOISE_FACTOR=noise_factor,BG_IMG=bg_img,BG_VALUE=bg_value
;; 18 Aug 2014 - JGI changed Poisson noise model so that noise is added on the cumulative signal, which includes previous noise
;;               (used to be on the noiseless signal in the interval between samples)
     IF N_ELEMENTS(FIRSTDCE) EQ 0 THEN firstdce = 0LL
     IF N_ELEMENTS(group_sclk_start) EQ 0 THEN group_sclk_start = sclk_obs[0]
     tbegin = systime(/seconds)
     print,'X0,Y0',x0,y0
     nstars = n_elements(RA_MULTI)
;     position_seed=!null
;     readnoise_seed=!null
;     poisson_seed=!null
     nclocktick = 2*fn+wt
     fowler_samples = dblarr(nx,ny,nclocktick);;; DECLARE EXPID
     expid = i+firstdce
     IF ~KEYWORD_SET(SILENT) THEN PRINT,'Building '+STRNG(nx)+'x'+STRNG(ny)+'x'+STRNG(nz)+' fits file for exposure id "'+STRNG(expid,format='(i04)')+'".'
     image_cube = MAKE_ARRAY(nx,ny,nz,VALUE=0.d0)
     noise_cube = MAKE_ARRAY(nx,ny,nz,VALUE=0.d0)
     sub = STRNG(READMODE) EQ 'SUB'
     t_subframe = sclk_obs[i] + findgen(nz)*(treset + framtime)   ;;; Start time of each subframe
     t_subframe_end = t_subframe + framtime                       ;;; End time of each subframe
     IF KEYWORD_SET(VERBOSE) THEN PRINT,'          Time runs from '+STRNG(t_subframe[0])+' to '+STRNG(t_subframe_end[nz-1])
     
;;;  POINTING AND PRF REALIZATION

     ;;; (1) Simulate x,y locations over the course of the dce as deltas from the original position (X0,Y0).  These positions are in apparent 
     ;;;    (measured, distorted) coordinates at (X0,Y0). 
     pointing_model['DRIFT_TIMEZERO']=(sclk_obs[0]-group_sclk_start)
     irac_pointing_model,[t_subframe[0],t_subframe_end[nz-1]]-group_sclk_start,0.,0.,xptg_delta,yptg_delta,tfull=tfull,dt_sample=dt_subsample,seed=pointing_seed,verbose=0,$
                         pointing_model_hash=pointing_model
     tfull += group_sclk_start;sclk_obs[0]                         
     nptg = N_ELEMENTS(xptg_delta) < N_ELEMENTS(yptg_delta)
     IF KEYWORD_SET(verbose) THEN PRINT,'Making PRF Realization for each of the '+STRNG(nptg)+' subsamples in the DCE.'
     ;;; (2) Determine the average _observed_ position of (RA0,DEC0) over the course of the DCE
     ;;;
     xave0 = MEAN(x0 + xptg_delta,/NAN)
     yave0 = MEAN(y0 + yptg_delta,/NAN)     ;;; Create a header with (RA0,DEC0) at (XAVE0,YAVE0)
     current_header = PRF_GET_DEFAULT_ASTROMETRY(channel,ra0,dec0,xave0,yave0,SUB=sub,HEADER=orig_header,/RESET_CRPIX)
     PRINT,'Setting RA0,DEC0 = '+STRNG(ra0)+','+STRNG(dec0)+' to land on pixel '+STRNG(xave0)+','+STRNG(yave0)
     ;;; (3) Compute each of the stellar positions for the current astrometry
     ;;;
     IF nstars GT 1 THEN BEGIN
        EXTAST,current_header,current_astrometry,success
        AD2XY,ra_multi,dec_multi,current_astrometry,xstar0,ystar0
     ENDIF ELSE BEGIN
        xstar0 = x0
        ystar0 = y0
     ENDELSE
     ;;; (4) Since the stars may be at varying pixels, where distortion could be different, the pointing motions, which are in observed
     ;;;     coordinates, need to be "de-distorted" at (x0,y0) before applied to each target's original position (use _DD to mean "de-distorted")
     ;;;    For example, (x0,y0) gives the observed starting position of the star, (x0_DD,y0_DD) gives the starting position 
     ;;;    in square pixels. 
     ;;;    (a) Get de-distorted initial position of the first star
     ;;;    
     IF ~keyword_set(NODISTORTION) THEN IRAC_DISTORT,x0,y0,channel,x0_DD,y0_DD,/REVERSE ELSE BEGIN
        x0_DD = x0
        y0_DD = y0
     ENDELSE
     ;;;    (b) Get de-distorted pointing offsets
     ;;;
     IF ~KEYWORD_SET(NODISTORTION) THEN IRAC_DISTORT,x0+xptg_delta,y0+yptg_delta,channel,xplus_DD,yplus_DD,/REVERSE ELSE BEGIN
        xplus_DD = x0+xptg_delta
        yplus_DD = y0 + yptg_delta
     ENDELSE
     ;;; The pointing offsets in square pixels: 
     xptg_delta_dd = xplus_DD - x0_DD
     yptg_delta_dd = yplus_DD - y0_DD
     ;;;    (c) Get de-distorted stellar positions vs time
     ;;;
     ;;;  The de-distorted stellar initial positions
     IF ~KEYWORD_SET(NODISTORTION) THEN IRAC_DISTORT,xstar0,ystar0,channel,xstar0_DD,ystar0_DD,/REVERSE ELSE BEGIN
         xstar0_DD = x0
         ystar0_DD = y0
     ENDELSE
     ;;; The stellar images per sample:
     star_obs_subsample = MAKE_ARRAY(nx,ny,nptg,/DOUBLE)
     ;;; interpolate the input relative light curve to the current set of subsample times
     IF N_ELEMENTS(lightcurve) GT 1 THEN lightcurve_gridded = INTERPOL(lightcurve,t_light,tfull) ELSE lightcurve_gridded = REPLICATE(lightcurve,N_ELEMENTS(tfull))
     IF KEYWORD_SET(VERBOSE) AND NSTARS GT 1 THEN PRINT,STRNG(nstars)+' stars to add to image.'
     FOR istar = 0,nstars-1 DO BEGIN
        IF KEYWORD_SET(Verbose) THEN iterwait,istar,10,100,text='Adding Star'
        xstar_dd = xstar0_DD[istar] + xptg_delta_dd
        ystar_dd = ystar0_DD[istar] + yptg_delta_dd
     ;;;    (d) Get the distorted (observed) stellar positions vs time
        IF ~KEYWORD_SET(NODISTORTION) THEN IRAC_DISTORT,xstar_dd,ystar_dd,channel,xstar,ystar ELSE BEGIN
           xstar = xstar_dd
           ystar = ystar_dd
        ENDELSE

        print,'Xstar[0],Ystar[0]',xstar[0],ystar[0]
    ;    print,wwwww
        ;; Only run the prf for the instances where the star is on the image, or not too far off -- prf is sampled to about 
        ;; +/- 12 or so pixels so the star cannot be more than 12 pixels off the edge of the array. 
        IDO = WHERE(xstar LE nx+12 AND xstar GE -12 AND ystar LE ny+12 AND ystar GE -12,NDO)
        IF NDO NE 0 THEN BEGIN
           star_obs_subsample[*,*,ido] += PRF_REALIZE(channel,prf,x_prf,y_prf,xstar[ido],ystar[ido],/no_normalize,verbose=0,FACTOR=lightcurve_gridded,nx=nx,ny=ny,$
                                             peak_star=peak_dn[istar]*GAIN,BG_IMAGE=bg_img)
        ENDIF                                           
     ENDFOR                         
;; Store mean position in each subframe
     xframe = DBLARR(nz)
     yframe = DBLARR(nz)
     ;;;; FOWLER SAMPLING
     ;;; Loop over subframes to accumulate fowler-sampled images
     ;;; SCLK Time at integration start
     sclk_aintbeg = t_subframe[0]
     avg_readnoise = 0d0
     FOR j = 0,nz-1 DO BEGIN
        IF KEYWORD_SET(VERBOSE) THEN PRINT,'Subframe '+STRNG(j+1)
        DUM = MIN( ABS(tfull-t_subframe[j]),it0,/NAN ) ;;; determine index in tfull of start of subframe
        i_clocktick = it0 + nsamp_clock * (Lindgen(nclocktick) )  ;; index of start of each clock tick
        i_clocktick_end = i_clocktick + nsamp_clock < (N_ELEMENTS(tfull)-1) ;; index of end of each clock tick
;;; Measure mean location of point source (first one if more than one) over the course of the subframe
        xframe[j] = MEAN(x0 + xptg_delta[it0:MAX(i_clocktick_end)])
        yframe[j] = MEAN(y0 + yptg_delta[it0:MAX(i_clocktick_end)])
;        i_pedestal = i_clocktick[0:fn-1]    ;;; indices of the pedestal reads
;        i_wait = WT NE 0 ? i_clocktick[fn:fn+wt-1] : !null    ;;; indices of the wait ticks
;        i_signal = i_clocktick[fn+wt:nclocktick-1]      ;;;; indices of the signal reads
        readnoise_samples = make_array(nx,ny,nclocktick,value=0d0)
        ;;; Integrate the set of prf realizations corresponding to the ms subsamples 
        IF KEYWORD_SET(verbose) THEN BEGIN
           PRINT,'Sampling at each of the '+STRNG(nclocktick)+' clock ticks.'
           PRINT,'Encompassing subsamples '+STRNG(i_clocktick[0])+' to '+STRNG(i_clocktick_end[nclocktick-1])+'.'
        ENDIF
        FOR k = 0,nclocktick-1 DO BEGIN
        ;;; Add up all the PRF images for the ms subsamples during the clocktick
           this_sample_delta = TOTAL(star_obs_subsample[*,*,i_clocktick[k]:i_clocktick_end[k]],3,/DOUBLE,/NAN)
           prev_sample = k EQ 0 ? MAKE_ARRAY(nx,ny,VALUE=0d0) : fowler_samples[*,*,k-1]
           fowler_samples[*,*,k] = prev_sample + this_sample_delta 
        ;;; Store the input source positions for the set of subsamples in the clocktick
           poisson_noise = MAKE_ARRAY(nx,ny,VALUE=0.d0)
           IF (k LT FN OR k GE FN+WT) AND ~KEYWORD_SET(no_noise) THEN BEGIN
             ;;; Set the value of readnoise if this is a read
              readnoise_samples[*,*,k] = noise_factor*RANDOMN(readnoise_seed,nx,ny,/DOUBLE)*rn_per_read
              ;;; Estimate the Poisson noise based on the cumulative electron counts in each pixel (including the noise of previous samples)
              FOR l = 0L,nx-1 DO BEGIN
                FOR m = 0L,ny-1 DO BEGIN
                  IF fowler_samples[l,m,k] GT 0 THEN $
                    ;; subtract the mean to just get the poisson fluctuations
                    poisson_noise[l,m] = noise_factor * ( RANDOMU(poisson_seed,1,POISSON=ROUND(fowler_samples[l,m,k])>1,/DOUBLE) - ROUND(fowler_samples[l,m,k]) )
                ENDFOR
              ENDFOR
           ENDIF ELSE readnoise_samples[*,*,k] = MAKE_ARRAY(nx,ny,VALUE=0.d0) 
           ;;; Read noise is NOT integrated!!!
           fowler_samples[*,*,k] += poisson_noise
        ENDFOR
        IF KEYWORD_SET(verbose) THEN PRINT,'Getting cumulative sum for actual fowler samples'
        ;;; add in the read noise, since this is where we are effectively "reading" the array
        fowler_samples += readnoise_samples
        readnoise_image = Make_ARRAY(nx,ny,value=0d0)
        IF KEYWORD_SET(verbose) THEN PRINT,'Measure the fowler differences, divide by the fowler number, and add'
        FOR k2 = 0,fn-1 DO BEGIN
     ;;; Measure the fowler differences, divide by the fowler number, and add - this gives the average fowler difference for the set of fowler samples
           image_cube[*,*,j] += (fowler_samples[*,*,FN+WT+k2] - fowler_samples[*,*,k2])/fn 
           readnoise_image += (readnoise_samples[*,*,FN+WT+k2] - readnoise_samples[*,*,k2])/fn
        ;;;; QUESTION --- why is avg readnoise sqrt(2) higher in simulation than reported in header, but only for same FN?
        ENDFOR 
        noise_cube[*,*,j] = SQRT(image_cube[*,*,j] + STDDEV(readnoise_image)^2)  ;;; Approximate the uncertainty image
 ;      IF KEYWORD_SET(nonlin) EQ 1 THEN  star_obs = (FULL_WELL/GAIN) * (1. - Exp(-(star_obs*GAIN)/(FULL_WELL)) ) ;;; Apply slow exponential saturation curve. Relates an ideal linear photo counter to the actual number of electrons (and hence DN) measured.  For <~ 1000 DN the number is unchanged, otherwise it is reduced.
        avg_readnoise += STDDEV(readnoise_image)/nz   ;;; contribution to the average readnoise based on the standard deviation 
                                                         ;;; of readnoise values measured for the current subframe
     ENDFOR
     sclk_atimeend = tfull[i_clocktick_end[nclocktick-1]]
     tend = systime(/seconds)
     PRINT,'DCE Creation time: '+STRNG(tend-tbegin)+'s.'
     PRF_MAKE_OBS_SAVE,current_header,image_cube,noise_cube,framtime,clocktick,fn,wt,channel,readmode,fovid,expid,sclk_obs[i],gain,ronoise,$
                       exptime,fluxconv,pxscal1,pxscal2,ra0,dec0,xave0[0],yave0[0],xframe,yframe,sclk_aintbeg,sclk_atimeend,$
                       label,reqkey,out_dir,SURFACE_BRIGHTNESS=SURFACE_BRIGHTNESS,DN=dn,verbose=verbose,silent=silent,OBJECT=object,$
                       HEADER_COMMENTS=header_comments
                       

RETURN
END
