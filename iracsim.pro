PRO iracsim,verbose=verbose,DURATION=duration,STARTDCENUM=STARTDCENUM,$
                 x0=x0,y0=y0,RA0=ra0,DEC0=dec0,POSITION_ANGLE=position_angle,$
                 out_dir=out_dir,label=label,reqkey=reqkey,RA_MULTI=ra_multi,DEC_MULTI=dec_multi,$
                 prf_file=prf_file,CHANNEL=channel,SUBCENTER=subcenter,WARM_MISSION=warm_mission,$
                 SUBREADOUT=subreadout,maxprf=maxprf,full_well=full_well,nonlin=nonlin,$
                 FRAMTIME=framtime,SCLK_START=sclk_start,NREPEAT=nrepeat,FLUX_DENSITY=flux_density,$
                 BG_HASH=bg_hash,INPUT_POINTING_HASH=input_pointing_hash,NODISTORTION=nodistortion,$
                 T_LIGHT = t_light, LIGHTCURVE=lightcurve,DT_SUBSAMPLE=DT_SUBSAMPLE,SILENT=silent,GROUP_SCLK_START=group_sclk_start,$
                 MULTITHREAD=multithread,DISTORTION=DISTORTION,SURFACE_BRIGHTNESS=SURFACE_BRIGHTNESS,DN=dn,$
                 OBJECT=object,POISSON_SEED=poisson_seed,READNOISE_SEED=readnoise_seed,POINTING_SEED=pointing_seed,$
                 NO_NOISE=no_noise,HEADER_COMMENTS=header_comments,SNR=snr,NO_PRECENTER=no_precenter,$
                 NOISE_FACTOR=noise_factor,PRF_DIR=prf_dir,_EXTRA=extra
;;
;;  Simulate IRAC staring observations of one or more point sources.  Uses an IRAC PRF to realize point source images, with a realistic pointing model giving the fluctuations of
;;  position as a function of time.   Multiple source positions are specified in RA and DEC and will be placed according to the input correspondence between pixel and celestial coordinates
;;  as well as distortion.  For single sources, source flux can vary according to an input light curve.  IRAC BCD images are simulated using fowler sampling.
;;  The data will be produced as a single AOR.
;
;   KEYWORD INPUTS (OPTIONAL)
;;  
;;    *** ASTROMETRY ***
;;  (RA0,DEC0)  -  sky position of pixel (X0,Y0), in degrees.  In the case of a single target, this is also the target position.  Default is [0,0].
;;  (X0,Y0) - actual pixel position of (RA0,DEC0).  In the case of a single target, this is the initial location of the target.  Default is the center of the current 
;;            FOV, unless /SUBCENTER is set with SUBREADOUT=0, in which case the target is placed at the center of the subarray even though  a full array measurement is performed.
;;  (RA_MULTI,DEC_MULTI) - sky positions of multiple targets.  If set, then /DISTORTION will automatically be set.
;;  POSITION_ANGLE - The angle the Y axis of the array makes East of celestial North (deg).  Default is 0.0 degrees.
;;
;;   *** AOR PARAMETERS ***
;;   CHANNEL - IRAC channel 1 or 2 -- default is 1
;;   /SUBREADOUT - Subarray readout mode
;;   /SUBCENTER - Use the subarray nominal center (automatically set when /SUBREADOUT is set)
;;   FRAMTIME - Frame time (sec).  Enter it as a string of one of the following: '0.02' '0.1', '0.4', '2', '6', '12', '30', or '100'.  Default is '0.4'.
;;   NREPEAT - Number of repeats.  If DURATION is set, then calculate NREPEAT from this.  If not, the default is NREPEAT=1.
;;   STARTDCENUM - DCE number to start counting from.  Useful for maps, in which PRF_MAKE_OBS is embedded in a wrapper with 
;;                 different (X0,Y0) or (RA0,DEC0) and you shouldn't start at DCE 0 each time.
;;   OBJECT - Name of target being observed.
;;   /WARM_MISSION - use the warm PRF
;;
;;   *** MODEL INPUTS ***
;;   DURATION - total approximate duration of the AOR (sec).  Will be reestablished consistent with the instrument timing model and returned on output.
;;   FLUX_DENSITY - Aperture flux density of star(s) (Jy).  If multiple stars are given, has the same number of elements as RA_MULTI and DEC_MULTI.
;;   R_APER - If set, impose FLUX_DENSITY as the flux in the input radius (pixels), otherwise assume R_APER = 10, which is the standard calibration.
;;            This would have the effect that if FLUX_DENSITY is the "real" flux density, then the data for R_APER will be properly aperture corrected
;;            in advance.
;;   SNR - Alternative to FLUX_DENSITY - Signal to noise ratio in peak pixel.  If set, FLUX_DENSITY is ignored and the number of electrons 
;;         in the peak pixel will be scaled so that the integration parameters give the desired value of S/N.  Use Garnett & Forrest (1993), Equation 25.
;;         Only works for one star. 
;;   BG_HASH - Specifies the background surface brightness image.  Entries include:
;;            BG_HASH['VALUE'] - average value in MJy/sr
;;            BG_HASH['SLOPE'] - Rate of increase in average value, % per hour.
;;            BG_HASH['AMPLITUDE'] - peak amplitude, in % of 'VALUE', of the background (only used if 'INDEX' key exists)
;;            BG_HASH['INDEX'] - power spectral index of the background spatial structure.
;;            BG_HASH['SEED'] - fractional brownian motion seed for building the random phases -- this holds the structure fixed.
;;            If only 'VALUE' is set, then the background is a constant value over the array.  If 'SLOPE' is also set, then
;;            the value increases (or decreases) with time by this amount per hour.
;;            If 'INDEX' and 'P2P' are also set, then impart a random spatial structure to the background, with average given by
;;            'VALUE', peak-to-peak range given by 'P2P' and spatial structure given by a fractional brownian motion simulation
;;            with power spectral index 'INDEX'
;;   (T_LIGHT,LIGHTCURVE) - Flux of source as a function of SCLK time, relative to FLUX_DENSITY.  Only valid when inputting a single source.  The input values
;;                          will be interpolated onto the actual SCLK times of the observed samples.  Default is 1.0 for all samples. 
;;   INPUT_POINTING_HASH - IDL Hash table specifying the parameters of the pointing model, containing values of one or more of the following hash keys:
;;                               JITTER - sinusoid of period around 1 minute, with amplitude of about 1/50 of a pixel
;;                         JPERIOD: seconds (default 319.676)
;;                         JPHASE: radians from 0 to 2pi (default 0.239936)
;;                         JAMPLITUDE: pixels (default 0.007)
;;                         JAXIS: degrees (orientation) (default 47.4)
;;                               1/f NOISE - fBm simulation with peak-to-peak given, spectral index 0.9 to 1.7.  Noise is independently realized in each axis
;;                          FBM_PEAK: pixels (default 0.4)
;;                          FBM_INDEX: power spectrum index (default 1.0)
;;                               PRE-DRIFT - Short term "pre" drift - positive or negative, with asymptote and overshoot
;;                          PREDRIFT_ASYMPTOTE: Difference between starting position and final, pixels (default 0.76)
;;                          PREDRIFT_DECAY: e-folding of asymptotic behavior (seconds) (default 1565.)
;;                          PREDRIFT_PERIOD: sinusoidal period (seconds) (default 12492.8)
;;                          PREDRIFT_AXIS: orientation on pixel (degrees) (default -68)
;;                              LONG-TERM DRIFT - linear trend
;;                          DRIFT_MAGNITUDE: arcsec per hour (default 0.0067)
;;                          DRIFT_AXIS: orientation, degrees (default -77.3)
;;                              HEATER WOBBLE - sinusoid, or skewed sinusoid ("sawtooth")
;;                          WOBBLE_AXIS: orientation, degrees (default 41.74)
;;                          WOBBLE_AMPLITUDE: maximum excursion, pixels (default 0.091)
;;                          WOBBLE_PHASE: 0 to 2pi, radians (default 0.81)
;;                          WOBBLE_SKEW: fraction of a period at which the peak occurs. To skew to the left (fast rise), set between 0 and 1/4.  
;;                                       To skew to the right (slow rise), set between 1/4 and 1/2.  A normal sine curve is A=1/4. (default 0.167)
;;                          WOBBLE_PERIOD: seconds (default 2245.12)
;;                          If any of these keys are not set, values will be set to defaults given.  
;;                          WOBBLE_FM_MAX_EXCURSION: For time-varying period, the maximum excursion in period from
;;                                                   WOBBLE_PERIOD (seconds) (default 200)
;;                          WOBBLE_AM_MAX_EXCURSION: For time-varying amplitude, the maximum excursion in amplitude from
;;                                                   WOBBLE_AMPLITUDE (pixels) (default 0.05)
;;                              Spike - Gaussian-shaped excursions in pointing
;;                          SPIKE_TIME: Midpoint in time of the spike(s) (seconds) -- array of values (one per spike).  Default is [0]
;;                          SPIKE_AXIS: orientation of excursion(s), (degrees) -- array of values (default: [0.0])
;;                          SPIKE_AMPLITUDE: peak of excursion(s) (pixels) -- array of values (default: [0.1])
;;                          SPIKE_WIDTH: FWHM of excursion(s) (seconds) -- array of values (default: [0.05])
;;                           
;;   DT_SUBSAMPLE - The time resolution of the model (sec).  Default is 1/10 of a clock tick
;;   PRF_FILE - prf table file.  Will use defaults for CH1/2 Full or subarray if not input.  
;;   PRF_DIR  - directory holding prf table file.  Will use defaults for cryogenic or warm mission if not input.
;;   MAXPRF - DN value at peak of PRF before scaling to star flux - defaults to 1.0 (shouldn't have to change this, but it's there in case we have to tweak the fluxes
;;   SCLK_START - The spacecraft clock time of the start of the AOR.  Default is the computer clock time at program's execution.
;;   GROUP_SCLK_START - The time at the start of a group of AORs which this is part of - this allows us to incorporate progressive or transient events that span multiple
;;                      AORs into the pointing model (otherwise each AOR will start at the same place in the pointing model). Default is SCLK_START. 
;;   FULL_WELL - Unused.  May be used later when we incorporate nonlinearity
;;   /NONLIN - Unused.  May be used later when we incorporate nonlinearity
;;
;;   *** CONTROL PARAMETERS ***
;;   /NODISTORTION - turn off distortion model, to see what happens
;;                   strictly due to fowler sampling and the PRF.  Target(s) will be placed at the input X,Y position(s)
;;                   but no effort will be made to ensure the astrometry comes out correctly. 
;;   /NO_NOISE - turn off detector noise (readout and photon noise) in simulation
;;   NOISE_FACTOR - set this to a factor to multiply the poisson and gaussian deviates by
;;   /NO_PRECENTER - turn off precentering of the PRF -- normally the PRF is "centered" such that box_centroider agrees with the input position
;;                   at (0,0) pixel phase.
;;   HEADER_COMMENTS - A string array with comments that should be embedded in the FITS header. 
;;   /SILENT - don't print updates to screen
;;   POINTING_SEED, READNOISE_SEED, POISSON_SEED - Seeds for random number generation for the pointing model, readnoise, or poisson noise.  
;;                                                 If not set, they will be re-initialized.  To run again with the same set of pointings or noises
;;                                                 set these keywords.  To simulate multiple back-to-back AORs, use the same seeds as output from the
;;                                                 previous run, in the following run. 
;;   OUT_DIR - use this to specify where the resulting fits files will be put.
;;   LABEL - special title to label the output directory.  default is "synth_prf"
;;   REQKEY - AOR request key
;;   /SURFACE_BRIGHTNESS - Store results as MJy/sr images (DEFAULT)
;;   /DN - Store results as DN images
;;
;;; Initialize the random number generator seeds for the pointing fluctuations, readnoise, and poisson noise (if not set already)
 IF N_ELEMENTS(HEADER_COMMENTS) EQ 0 THEN header_comments = !null
     IF N_ELEMENTS(pointing_seed) EQ 0 THEN position_seed=!null
     IF N_ELEMENTS(readnoise_seed) EQ 0 THEN readnoise_seed=!null
     IF N_ELEMENTS(poisson_seed) EQ 0 THEN poisson_seed=!null
     SIGMA_READNOISE = LIST( HASH('0.02S',25.4,'0.1S',13.4,'0.4S',10.8,'2S',10.8,'0.4F',22.4,'0.6F',22.4,'2F',16.0,'6F',16.0,'12F',15.0,'30F',14.6,'100F',21.0),$
                             HASH('0.02S',23.7,'0.1S',12.1,'0.4S',9.4,'2S',9.4,'0.4F',23.7,'0.6F',23.7,'2F',12.1,'6F',10.0,'12F',10.4,'30F',10.4,'100F',16.0) )
;frametimes = HASH('0.02S',0.01,'0.1S',0.08,'0.4S',0.36,'2S',1.92,'0.4F',0.2,'2F',1.2,'6F',4.4,'12F',10.4,'30F',26.8)
;; Allow multiple stars:  X0, Y0, FLUX_DENSITY can be arrays
nstars = N_ELEMENTS(RA_MULTI) NE 0  ? N_ELEMENTS(RA_MULTI): 1
;IF N_ELEMENTS(RA_MULTI) NE 0 AND N_ELEMENTS(DEC_MULTI) NE 0 THEN DISTORTION=1 
IF ~KEYWORD_SET(verbose) THEN verbose = 0
IF ~KEYWORD_SET(silent) THEN silent=0
IF ~KEYWORD_SET(DN) THEN surface_brightness = 1
IF N_ELEMENTS(POSITION_ANGLE) EQ 0 THEN position_angle=0.0
IF N_ELEMENTS(RA0) EQ 0 THEN ra0 = 0.0
IF N_ELEMENTS(RA_MULTI) EQ 0 THEN RA_MULTI=RA0
IF N_ELEMENTS(DEC0) EQ 0 THEN dec0 = 0.0
IF N_ELEMENTS(DEC_MULTI) EQ 0 THEN DEC_MULTI=DEC0
IF N_ELEMENTS(R_APER) EQ 0 THEN R_APER = 10.0 ELSE header_comments = [header_comments,'Scaled so that R_APER = '+STRNG(r_aper,format='(f5.2)')+'px is aperture-corrected.']
IF N_ELEMENTS(NOISE_FACTOR) NE 0 THEN header_comments = [header_comments,'Noise multiplied by '+STRNG(noise_factor[0],format='(f6.2)')] ELSE noise_factor=1.0
;IF N_PARAMS() EQ 0 THEN BEGIN
;   position_seed = 0
;   readnoise_seed=0
;   poisson_seed=0
;ENDIF
  IF ~KEYWORD_SET(FLUX_DENSITY) THEN flux_density = 0.4  ;; Aperture flux of star (Jy)
;;; Enter SCLK_START for sclk of first observation, default is zero
;;; NREPEAT specifies the number of repeats
  
  IF ~KEYWORD_SET(SCLK_START) THEN sclk_start = TIME_SCLK(SYSTIME(/JULIAN),/JULIAN_TO_SCLK)
  IF N_ELEMENTS(GROUP_SCLK_START) EQ 0 THEN group_sclk_start = sclk_start
  IF ~KEYWORD_SET(channel) THEN channel = 1
  IF KEYWORD_SET(WARM_MISSION) THEN BEGIN
    header_comments = [header_comments,'Simulation uses WARM PRFs']
    iracprefix = 'IRACPC'
    ;prf_dir = './warm_prfs' 
  ENDIF ELSE iracprefix = 'IRAC';ELSE prf_dir = './070131_prfs'
  IF KEYWORD_SET(NO_DISTORTION) THEN header_comments = [header_comments,'Distortion model turned OFF in simulation']
  IF KEYWORD_SET(NO_NOISE) THEN header_comments = [header_comments,'Detector noise turned OFF in simulation']
  IF ~KEYWORD_SET(framtime) THEN framtime='0.4'
  IF KEYWORD_SET(SUBREADOUT) THEN BEGIN
     clocktick = 0.01d0
     IF ~KEYWORD_SET(DT_SUBSAMPLE) THEN dt_subsample = clocktick/10
     readmode = 'SUB     '
     nz = 64
     nx = 32
     ny = 32
 ;    IF ~keyword_set(prf_file) THEN prf_file = 'IRAC1_col025_row233.fits';'IRAC'+STRNG(channel)+'_col025_row233.fits'
     IF ~keyword_set(prf_file) THEN prf_file = iracprefix+STRNG(channel)+'_col025_row233.fits'
     nsamp_clock = ROUND(clocktick/dt_subsample)
     SUB=1  
     framestring = framtime + 'S'   
  ENDIF ELSE BEGIN 
     clocktick = 0.2d0
     IF ~KEYWORD_SET(DT_SUBSAMPLE) THEN dt_subsample = clocktick/10
     nsamp_clock = ROUND(clocktick/dt_subsample)
     readmode =  'FULL    '
     nz = 1
     nx = 256
     ny = 256
     IF ~keyword_set(PRF_FILE) THEN prf_file = '';'IRAC'+STRNG(channel)+'_col129_row129.fits'
     SUB=0     
     framestring = framtime + 'F'
  ENDELSE
  ;; Read in the PRF or PRF's
  PRF_READ,channel,x_prf,y_prf,prf,dir=prf_dir,verbose=~Keyword_set(SILENT),PRF_FILE=prf_file,WARM_MISSION=warm_mission,NOCENTER=no_precenter,_EXTRA=extra
              
  ; Enter keyword FRAMETIME as a string of one of the following:
;;; '0.01' '0.1', '0.4', '2', '6', '12', '30', or '100'  Default is '0.4'
  
  IRAC_OBS_DURATION,framtime,NREPEAT=nrepeat,DURATION=duration,SUB=sub,INTEGRATION_TIME=integration_time,FN=fn,WT=wt,EXPTIME=exptime,CLOCKTICK=clocktick,$
                    TLATENCY=tlatency,TRESET=treset
  
  sclk_obs = sclk_start + DINDGEN(nrepeat) * (treset + CEIL(nz * integration_time) + 1. + tlatency) ;start of each DCE first read
  
  IF ~KEYWORD_SET(SILENT) THEN print,framtime+'s '+STRLOWCASE(STRCOMPRESS(readmode,REMOVE_ALL=KEYWORD_SET(SUBREADOUT)))+'array, '+STRNG(nrepeat)+' repeats. Total AOR duration is '+STRNG(duration/3600.)+' hr.'
  ;;; Input the light curve specifying relative flux LIGHTCURVE as a function of time T_LIGHT.  This will be interpolated onto the
  ;;; sampling grid, so ensure that the input time samples span the full range here.
  IF N_ELEMENTS(lightcurve) EQ 0 THEN BEGIN
     t_light = sclk_obs
     lightcurve = MAKE_ARRAY(nrepeat,value=1.0) 
  ENDIF


;;; For readnoise, we need to estimate the read noise per Fowler sample.  The IST has measured the read noise per frame time, so 
;;; the RN per sample can be deduced from the measured fowler sampling integration read noise using the Garnett & Forrest (1993) equation 5: 
;;;        sigma^2(INTEGRATION) = 2 * sigma^2(PER SAMPLE) / FN
;;; I think G&F only did this for WT=0, so use WT=0 data
;;; sigma(INTEGRATION) = [22.4,23.7] for 0.02s (FN=1, WT=0), so readnoise per sample is 
;;; sigma(PER SAMPLE) = [15.8,16.8]
;;;
  apcorr_3p0 = 0.575 * EXP(-3.0/1.963d0) + 0.997
  apcorr_r = 0.575 * EXP(-R_APER/1.963d0) + 0.997
  ;;; peak_pixel dn per jy per second is given below for a 3-pixel aperture.  Scale it so we get the expected number of Jy in an
  ;;; R-pixel aperture (i.e., we assume it has been aperture corrected to R_APER).
  ap_scaling = apcorr_r/apcorr_3p0
  IF channel EQ 1 THEN BEGIN
     fluxconv = 0.1253
     gain = 3.7
     rn_per_read = 15.8
     pxscal1 = -1.22334117768332D0
     pxscal2 = 1.22328355209902D0
     peak_pixel_dn_perjy_persec = ap_scaling * 5200./.170/0.36
     fovid = KEYWORD_SET(SUBREADOUT) ? 70 : 67
     xcen = KEYWORD_SET(SUBREADOUT) ? 15.0 : 130.0
     ycen = KEYWORD_SET(SUBREADOUT) ? 15.0 : 127.0
  ENDIF ELSE BEGIN
     fluxconv = 0.1469
     gain = 3.71
     rn_per_read = 16.8
     pxscal1 = -1.21641835430637D0
     pxscal2 = 1.21585676679388D0
     fovid = KEYWORD_SET(SUBREADOUT) ? 77 : 74
     peak_pixel_dn_perjy_persec = ap_scaling * 3000./.485/0.08
     xcen = KEYWORD_SET(SUBREADOUT) ? 15.0 : 127.0
     ycen = KEYWORD_SET(SUBREADOUT) ? 15.0 : 128.0
  ENDELSE
  ronoise = (SIGMA_READNOISE[channel-1])[framestring] 
  IF N_ELEMENTS(x0) EQ 0 THEN x0 = xcen 
  IF N_ELEMENTS(y0) EQ 0 THEN y0 = ycen
;;; Adjustment to position to get in full array coordinates
   IF KEYWORD_SET(SUBCENTER) AND ~KEYWORD_SET(SUBREADOUT) THEN BEGIN  
 ;;; Only works for CH1 and CH2, so we need to enhance the code if we're doing this for CH3 and CH4
        xfull_offset = 8
        yfull_offset = 216
   ENDIF ELSE BEGIN
        xfull_offset = 0
        yfull_offset = 0
   ENDELSE
   x0 += xfull_offset
   y0 += yfull_offset  
  
  ;; The number of steradians per pixel^2
  sr_pix = ABS(pxscal1*pxscal2)/(206265.d0)^2
  ;; The number of electrons in a given jansky measurement per pixel 
  jy_to_electron = exptime * gain/(1e6*sr_pix * fluxconv)  ;; e * pixel^2 / Jy    
  IF N_ELEMENTS(maxprf) EQ 0 then maxprf = 1.d0 ;;;  peak_dn
  nmx = 2.0*fn + wt
  eta= FLOAT(fn)/nmx
  a = 1d0 - 2*eta/3d0 ;+ 1d0/(6.*eta*nmx^2)
  b = 1d0 - eta/2d0
  ;snr = 2.0
  IF N_ELEMENTS(SNR) EQ 0 THEN BEGIN
     peak_dn = maxprf * flux_density * dt_subsample * peak_pixel_dn_perjy_persec  ;;; Total number of dn in the peak pixel expected for a ms subsample
     e_s = gain * peak_dn/dt_subsample
     G_eta = 2 * rn_per_read^2 / (e_s * exptime * fn)
     snr = (SQRT(e_s * exptime) * b) / SQRT(G_eta + a) 
     header_comments = [header_comments,'Input flux yields approx S/N: '+STRNG(snr,format='(f7.1)')]
  ENDIF ELSE BEGIN
     ;; Garnett & Forrest (1993), equation 25 solved for F (it's quadratic in 1/F)
     e_s = ( 4*rn_per_read^2 / (exptime * fn) / ( SQRT(a^2 + (8*rn_per_read^2*b)/(fn * snr^2) ) - a )) 
     peak_dn = e_s * dt_subsample / gain 
     print,'Peak DN per subsample: '+STRING(peak_dn)
     ;IF peak_dn LT 1 THEN peak_dn = 1.0
     header_comments = [header_comments,'Flux of object scaled to obtain S/N: '+STRNG(snr,format='(f7.1)')]
  ENDELSE
  ;;; (at the peak of pixel response)
  bg_elec_subsample = dt_subsample * GAIN / fluxconv  ;; Number of electrons per pixel in the background per subsample, per MJy/sr
  bg_baseline = MAKE_ARRAY(nrepeat,value=0d)
  bg_img = MAKE_ARRAY(nx,ny,value=0d)
  IF N_ELEMENTS(BG_HASH) NE 0 THEN BEGIN
    IF BG_HASH.HasKey('VALUE') THEN bg_baseline[*] += BG_HASH['VALUE'] * bg_elec_subsample
    IF BG_HASH.HasKey('SLOPE') THEN bg_baseline += BG_HASH['VALUE'] * bg_elec_subsample * BG_HASH['SLOPE'] * 0.01 * (sclk_obs-group_sclk_start) / 3600d
    IF BG_HASH.HasKey('INDEX') THEN BEGIN
       IF BG_HASH.HasKey('AMPLITUDE') THEN amp = BG_HASH['AMPLITUDE'] * BG_HASH['VALUE'] * 0.01 * bg_elec_subsample ELSE amp = 0.01 * BG_HASH['VALUE'] * 0.01 * bg_elec_subsample
       FBMTOT,nx,BG_HASH['INDEX'],BG_HASH['SEED'],fbm_img,NPIX_Y=ny,/NOT_POS,/NOVIEW
       maxim = MAX(fbm_img,/ABSOLUTE)
       bg_img = fbm_img/maxim * amp
    ENDIF
  ENDIF
  
  
  IF KEYWORD_SET(label) EQ 0 THEN label='synth_prf'
  IF N_ELEMENTS(reqkey) EQ 0 THEN reqkey = '0000000000'
  IF KEYWORD_SET(out_dir) EQ 0 THEN out_dir = '~/data/IRAC/prf_simulations/'+label+'/'
  FILE_MKDIR,out_dir
  
  pointing_model_hash = HASH({jperiod:319.676,jphase:0.239936,jamplitude:0.007,jaxis:47.4,fbm_peak:0.4,fbm_index:1.0,$
                            predrift_asymptote:0.1,predrift_decay:1565.,predrift_period:12492.8,predrift_axis:-68.,$
                            drift_magnitude:0.0067,drift_axis:-77.3,wobble_axis:41.74,wobble_amplitude:0.091,wobble_phase:0.81,$
                            wobble_skew:0.167,wobble_period:2245.12})
  
  IF N_ELEMENTS(input_pointing_hash) NE 0 THEN BEGIN
;; Set up the pointing model, with changes based on the input  
     keys = input_pointing_hash.KEYS()
     FOREACH key,keys DO POINTING_MODEL_HASH[key] = input_pointing_hash[key]
  ENDIF
  avg_readnoise = 0d0
  
  ;;; Need to rework the multithreading part -- it's not working
  IF KEYWORD_SET(MULTITHREAD) THEN BEGIN
  ;;; Open a number of child processes to run the loop over DCEs, use SPLIT_FOR.PRO to handle the IDL BRIDGE stuff
  ;;; Child processes will run multithreaded in the background, I presume...
     NSPLIT=!CPU.HW_NCPU
     SPLIT_FOR,0LL,nrepeat-1,wait_interval=1,NSPLIT=NSPLIT,$
               VARNAMES=['nx','ny','nz','sclk_obs','treset','framtime','x0','y0','dt_subsample',$
                         'jperiod','jphase','jamplitude','jaxis','fbm_peak','fbm_index','predrift_asymptote',$
                         'predrift_decay','predrift_period','predrift_axis','drift_magnitude','drift_axis','wobble_axis',$
                         'wobble_amplitude','wobble_phase','wobble_skew','wobble_period','lightcurve','t_light','prf','x_prf',$
                         'y_prf','nsamp_clock','rn_per_read','fn','wt','channel','readmode','fovid','gain',$
                         'exptime','fluxconv','pxscal1','pxscal2','label','reqkey','avg_readnoise','out_dir','silent','verbose'],$
               COMMANDS=['PRF_MAKE_DCE,i,nx,ny,nz,sclk_obs,treset,framtime,x0,y0,dt_subsample,position_seed,readnoise_seed,poisson_seed,jperiod,jphase,jamplitude,jaxis,fbm_peak,fbm_index,predrift_asymptote,predrift_decay,predrift_period,predrift_axis,drift_magnitude,drift_axis,wobble_axis,wobble_amplitude,wobble_phase,wobble_skew,wobble_period,lightcurve,t_light,prf,x_prf,y_prf,nsamp_clock,rn_per_read,fn,wt,channel,readmode,fovid,gain,exptime,fluxconv,pxscal1,pxscal2,label,reqkey,avg_readnoise,out_dir,SILENT=SILENT,VERBOSE=VERBOSE']
                
   ENDIF ELSE BEGIN
;; Get the initial astrometry header that puts (RA0,DEC0) at (X0,Y0), setting the CRVAL's based on the expected CRPIX and using 
;; POSITION_ANGLE to orient the coordinate axes.
   orig_header = prf_get_default_astrometry(channel,RA0,DEC0,X0,Y0,POSITION_ANGLE,SUB=sub)
;;; Loop over DCEs
     IF N_ELEMENTS(STARTDCENUM) EQ 0 THEN startdcenum = 0LL
;;; Run the pointing model for the entire AOR     
;;     irac_pointing_model,[sclk_obs[0],sclk_obs[nrepeat-1]+framtime]-group_sclk_start,0.,0.,xptg_delta_aor,yptg_delta_aor,tfull=tfull_aor,dt_sample=dt_subsample,seed=pointing_seed,verbose=0,$
;;       pointing_model_hash=pointing_model_hash
;;     tfull_aor += group_sclk_start
     FOR i = 0,nrepeat-1 DO BEGIN
;; PRF_MAKE_DCE simulates the motion for each subsample over the course of the current DCE, and integrates the PRF images via fowler sampling
                  
        PRF_MAKE_DCE,i,nx,ny,nz,sclk_obs,treset,framtime,clocktick,x0,y0,peak_dn,dt_subsample,$
                     pointing_model_hash,xcen,ycen,$;xptg_delta,yptg_delta,tfull,$
                     lightcurve,t_light,prf,x_prf,y_prf,nsamp_clock,rn_per_read,fn,wt,channel,readmode,fovid,gain,$
                     exptime,fluxconv,pxscal1,pxscal2,label,reqkey,avg_readnoise,ronoise,out_dir,orig_header,$
                     object,ra0,dec0,position_angle,ra_multi,dec_multi,pointing_seed,poisson_seed,readnoise_seed,$
                     GROUP_SCLK_START=group_sclk_start,SUBCENTER=subcenter,SILENT=SILENT,VERBOSE=VERBOSE,FIRSTDCE=startdcenum,$
                     SURFACE_BRIGHTNESS=SURFACE_BRIGHTNESS,DN=dn,NODISTORTION=nodistortion,NO_NOISE=no_noise,$
                     HEADER_COMMENTS=header_comments,NOISE_FACTOR=noise_factor,BG_IMG=bg_baseline[i]+bg_img
     
     ENDFOR
  ENDELSE
RETURN
END

     
