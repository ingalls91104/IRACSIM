PRO exoplanet_simulate,exosystem,ch,frametime,nrepeat,DURATION_HR=duration_hr,FULLREADOUT=fullreadout,SUB=sub,MJD_START=mjd_start,OUT_DIR=out_dir,REQKEY=reqkey,$
                       SECONDARY_LAMBDA=secondary_lambda,TEQ_P=teq_p,X0=x0,Y0=y0,ALBEDO=albedo,TRAD=trad,OMROT=omrot,BG_HASH=bg_hash,$
                       INPUT_POINTING_HASH=input_pointing_hash,START_HR_BEFORE_TRANSIT=start_hr_before_transit,$
                       START_PHASE=start_phase,DAYTIME_START=daytime_start,LABEL=label,DN=dn,PEAKUP=peakup,EXODATA=exodata,$
                       BLIND_POINTING=blind_pointing,POISSON_SEED=poisson_seed,READNOISE_SEED=readnoise_seed,POINTING_SEED=pointing_seed,$
                       SINGLE_AOR=single_aor,PA=pa,WARM_MISSION=warm_mission,VERBOSE=verbose,NO_PLANET=no_planet,_EXTRA=extra
;; DAYTIME_START: Enter the start time of the observation in UTC as an array of [month (number), day, year, hour, minute, seconds]
;
;  DURATION_HR - set the full duration of the measurements.  Assume that we will add a 30min pre-AOR to the beginning, and divide the DURATION time 
;             into 12hr pieces, each of which gets a separate AOR (unless /SINGLE_AOR is set).  REQKEYs are incremented from the first one for each AOR.  
;             
;  START_HR_BEFORE_TRANSIT - set this to automatically start the data taking at a time this number of hours before transit.            
;             
;  START_PHASE - set this to automatically start the data taking at this phase of the orbit
;             
;  NREPEAT - If this has a value, then DURATION_HR is ignored, and the AOR(s) run for however many hours it takes to execute this number of repeats.
;            
;  /SINGLE_AOR - DO NOT divide the DURATION time into 12hr pieces, but use a single AOR to encompass the whole duration
;  
;  /PEAKUP - Assume the pointing is enhanced by PCRS Peakup, so that each AOR is repointed to (X0,Y0) with precision of 0.13 pixels.  
;                     Otherwise use (X0,Y0) exactly (unless /BLIND_POINTING is set).
;  /BLIND_POINTING - Assume the pointing is blind, so that each AOR is repointed to (X0,Y0) with precision of 0.3 pixels.  
;                     Otherwise use (X0,Y0) exactly (unless /PEAKUP is set).
;                     
;  BG_HASH - Specifies the background.  Entries include:
;            BG_HASH['VALUE'] - average value in MJy/sr
;            BG_HASH['AMPLITUDE'] - peak amplitude, in % of 'VALUE', of the background (only used if 'INDEX' key exists)
;            BG_HASH['INDEX'] - power spectral index of the background spatial structure.  
;            BG_HASH['SLOPE'] - Rate of increase in average value, % per hour.
;            BG_HASH['SEED'] - fractional brownian motion seed for building the random phases -- this holds the structure fixed.
;            If only 'VALUE' is set, then the background is a constant value over the array.  If 'SLOPE' is also set, then
;            the value increases (or decreases) with time by this amount per hour.
;            If 'INDEX' and 'P2P' are also set, then impart a random spatial structure to the background, with average given by
;            'VALUE', peak-to-peak range given by 'P2P' and spatial structure given by a fractional brownian motion simulation
;            with power spectral index 'INDEX'       
;;                     
;;;                     ***EXOPLANET ATMOSPHERE MODEL FREE PARAMETERS***
;; ALBEDO     = Planetary Bond albedo.  Default = 0.
;; TRAD       = Radiative timescale, days.  Default = 0.
;; OMROT      = Rotational angular velocity of planetary emitting layer, in units of the orbital angular velocity
;;              at periastron.  Default = 1.0.
;
;  /NO_PLANET - zero out the variations in the light curve due to the planet (i.e., only use the stellar flux).
;                     
;;   POINTING_SEED, READNOISE_SEED, POISSON_SEED - Seeds for random number generation for the pointing model, readnoise, or poisson noise.  
;;                                                 If not set, they will be re-initialized.  To run again with the same set of pointings or noises
;;                                                 set these keywords.  To simulate multiple back-to-back AORs, use the same seeds as output from the
;;                                                 previous run, in the following run. 
;
;;;; Systems known to have both transits AND secondary eclipses
;WASP-1 b,WASP-18 b,WASP-33 b,XO-3 b,WASP-12 b,CoRoT-1 b,XO-2 b,55 Cnc e,HD 80606 b,WASP-19 b,
;OGLE-TR-113 b,GJ 436 b,WASP-14 b,WASP-24 b,WASP-17 b,XO-1 b,HD 149026 b,TrES-3 b,TrES-4 b,WASP-3 b,
;TrES-1 b,Kepler-12 b,TrES-2 b,KOI-13 b,Kepler-7 b,CoRoT-2 b,HAT-P-7 b,Kepler-6 b,Kepler-17 b,
;Kepler-5 b,HD 189733 b,WASP-2 b,HD 209458 b,HAT-P-8 b,HAT-P-1 b,WASP-4 b,HAT-P-6 b,XO-4 b
IF N_ELEMENTS(SECONDARY_LAMBDA) EQ 0 THEN secondary_lambda = ch EQ 1 ? '3.6' : '4.5'
IF N_ELEMENTS(REQKEY) EQ 0 THEN reqkey = '0000000000'
;;; Initialize the random number generator seeds for the pointing fluctuations, readnoise, and poisson noise (if not set already)
IF N_ELEMENTS(pointing_seed) EQ 0 THEN position_seed=!null
IF N_ELEMENTS(readnoise_seed) EQ 0 THEN readnoise_seed=!null
IF N_ELEMENTS(poisson_seed) EQ 0 THEN poisson_seed=!null
trepoint = 240.0  ;;; The time (seconds) it takes to repoint the observatory using PCRS peakup between AORs (somewhat arbitrary)
;; Combine (1) AOR duration and start information (2) The exoplanet light curves and (3) the initial pointing information per AOR
;; 
;;
;;;   (1)  Determine the distribution of AORs 
;
;;;   First figure out the full duration of the observation, based on the inputs (either duration_hr, or nrepeat).  IF nothing is set, then 
;;;   IRAC_OBS_DURATION will set nrepeat=1.
IF KEYWORD_SET(DURATION_HR) THEN duration = duration_hr * 3600.d0 ELSE duration=!null  ;; duration in seconds
;;; Make frametime, nrepeat, and duration consistent.
IRAC_OBS_DURATION,frametime,NREPEAT=nrepeat,DURATION=duration,SUB=sub,INTEGRATION_TIME=integration_time,FN=fn,WT=wt,EXPTIME=exptime,CLOCKTICK=clocktick,$
                  TLATENCY=tlatency,TRESET=treset
duration_hr = duration/3600.d0                  ;; reset the actual total duration
;;; Determine the breakdown of AORs
IF ~KEYWORD_SET(SINGLE_AOR) THEN BEGIN
   n12 = FLOOR(duration_hr)/12 
   hrleft = duration_hr - n12*12.0
   IF n12 NE 0 THEN aor_durations = [0.5,replicate(12,n12),hrleft] ELSE aor_durations = [0.5,hrleft]
   nt = [300,replicate(10000L,n12+1)]
   naor = N_ELEMENTS(aor_durations)
   PRE_AOR = 1
ENDIF ELSE BEGIN
   aor_durations = duration_hr
   naor = 1
   nt = 10000L
   PRE_AOR = 0
ENDELSE

;; Determine the start time of all observations
IF N_ELEMENTS(DAYTIME_START) NE 0 THEN BEGIN
   mjd_start = TIME_SCLK(daytime_start[0],daytime_start[1],daytime_start[2],daytime_start[3],$
                         daytime_start[4],daytime_start[5],/DAY_TIME_TO_MJD)
ENDIF ELSE BEGIN
   ;;; Get the exoplanet system information
   ;print,'secondary lambda',secondary_lambda
   get_exoplanet_data,EXOSYSTEM=exosystem,MSINI=msini,MSTAR=mstar,TRANSIT_DEPTH=transit_depth,RP_RSTAR=rp_rstar,AR_SEMIMAJ=ar_semimaj,$
                       TEQ_P=teq_p,TEFF_STAR=teff_star,SECONDARY_DEPTH=secondary_depth,SECONDARY_LAMBDA=secondary_lambda,$
                       INCLINATION=inclination,MJD_TRANSIT=mjd_transit,P_ORBIT=p_orbit,EXODATA=exodata,RA=ra,DEC=dec,VMAG=vmag,$
                       DISTANCE=distance,ECC=ecc,T14=t14,F36=f36,F45=f45,FP_FSTAR0=fp_fstar0,VERBOSE=verbose
   mjd_start = get_exoplanet_start(p_orbit,mjd_transit,START_HR_BEFORE_TRANSIT=start_hr_before_transit,START_PHASE=start_phase,PRE_AOR=pre_aor)
ENDELSE
;;
;; (2) compute the light curve(s) for each AOR
;; 
tlight = LIST(LENGTH=naor)
rel_flux = LIST(LENGTH=naor)
IF NAOR GT 1 THEN mjd_start -= 0.5/24d0  ;; Start 30 minutes early
mstart = mjd_start    
FOR i = 0,naor-1 DO BEGIN
    aorkey = STRNG(LONG(reqkey) + i,format='(i010)')       
    exoplanet_light_curve_fullmodel,t,rf,EXOSYSTEM=exosystem,/plot,/pdf,VERBOSE=verbose,ra=ra,dec=dec,f36=f36,f45=f45,$
                            mjd_start=mstart,DURATION_HR=AOR_DURATIONS[i],EXODATA=exodata,SECONDARY_LAMBDA=secondary_lambda,$
                            NT=nt[i],TEQ_P=teq_p,LABEL=LABEL+'_'+aorkey,ALBEDO=albedo,TRAD=trad,OMROT=omrot,/TEXTOUT
    tlight[i] = t
    rel_flux[i] = KEYWORD_SET(NO_PLANET) ? MAKE_ARRAY(SIZE=SIZE(rf),VALUE=1d0) : rf
    mstart = t[nt[i]-1] + trepoint/86400D0  ;;; Start time for the next AOR, feeds into EXOPLANET_LIGHT_CURVE

ENDFOR
IF CH EQ 1 THEN flux_density = f36/1000. ELSE flux_density=f45/1000.                      
;;
;;    (3)  Set the initial positions of each AOR
;;
xstart = fltarr(naor)
ystart = fltarr(naor)
;; Sweet Spot phases
;xphase = [0.198,0.120]
;yphase = [0.020,0.085]
xphase = [0.198,0.15]
yphase = [0.020,0.2]

IF N_ELEMENTS(x0) EQ 0 THEN BEGIN
   CASE 1 OF 
      KEYWORD_SET(SUB): xstart[*] = 15.0
      KEYWORD_SET(FULLSUB): xstart[*] = 23.0
      ELSE: BEGIN
        IF ch EQ 1 THEN xstart[*] = 130.0 ELSE xstart[*] = 127.0
      END
   ENDCASE 
ENDIF ELSE xstart[*] = x0 
IF N_ELEMENTS(y0) EQ 0 THEN BEGIN
   CASE 1 OF 
      KEYWORD_SET(SUB): ystart[*] = 15.0
      KEYWORD_SET(FULLSUB): ystart[*] = 231.0
      ELSE: BEGIN
        IF ch EQ 1 THEN ystart[*] = 127.0 ELSE ystart[*] = 128.0
      END
   ENDCASE 
ENDIF ELSE ystart[*] = y0
;;; Add the pointing error
CASE 1 of 
   KEYWORD_SET(PEAKUP): ptg_sigma = 0.04
   KEYWORD_SET(BLIND_POINTING): ptg_sigma = 0.3
   ELSE: ptg_sigma = 0.0
ENDCASE
IF ptg_sigma NE 0 THEN BEGIN
;; Assume target is going for the sweet spot
   xstart += xphase[ch-1] + RANDOMN(pointing_seed,naor)*ptg_sigma
   ystart += yphase[ch-1] + RANDOMN(pointing_seed,naor)*ptg_sigma
ENDIF 
;IF KEYWORD_SET(VERBOSE) THEN BEGIN
  print,'XSTART: ',xstart
  print,'YSTART: ',ystart
;ENDIF
;;
;;  Make the observations
;;
group_sclk_start = TIME_SCLK((tlight[0])[0],/MJD_TO_SCLK)
IF KEYWORD_SET(VERBOSE) THEN PRINT,'Building AOR(s) to span total duration '+STRNG(DURATION_HR+PRE_AOR*0.5)+'hr' 
FOR i = 0,naor-1 DO BEGIN
   t_sclk = TIME_SCLK(tlight[i],/MJD_TO_SCLK) 
   sclk_start = t_sclk[0]  
   aorkey = STRNG(LONG(reqkey) + i,format='(i8)')              
   IF KEYWORD_SET(VERBOSE) THEN PRINT,'AORKEY: '+aorkey+' Duration: '+STRNG(AOR_DURATIONS[i])+' X0:'+STRNG(xstart[i])+' Y0:'+STRNG(ystart[i])
   IRACSIM,X0=xstart[i],Y0=ystart[i],OUT_DIR=out_dir,CHANNEL=ch,FRAMTIME=frametime,DURATION=aor_durations[i]*3600.D0,FLUX_DENSITY=flux_density,BG_HASH=bg_hash,$
                INPUT_POINTING_HASH=input_pointing_hash,T_LIGHT=t_sclk,LIGHTCURVE=rel_flux[i],REQKEY=aorkey,$
                DN=dn,SCLK_START=sclk_start,GROUP_SCLK_START=group_sclk_start,verbose=0,SUBREADOUT=sub,OBJECT=EXOSYSTEM,RA0=ra,DEC0=dec,PA=pa,$
                POISSON_SEED=poisson_seed,READNOISE_SEED=readnoise_seed,POINTING_SEED=pointing_seed,LABEL=label,WARM_MISSION=warm_mission,_EXTRA=extra
ENDFOR              
;                      pointing_model_hash = HASH({jperiod:319.676,jphase:0.239936,jamplitude:0.007,jaxis:47.4,fbm_peak:0.4,fbm_index:1.0,$
;                            predrift_asymptote:0.76,predrift_decay:1565.,predrift_period:12492.8,predrift_axis:-68.,$
;                            drift_magnitude:0.0067,drift_axis:-77.3,wobble_axis:41.74,wobble_amplitude:0.091,wobble_phase:0.81,$
;                            wobble_skew:0.167,wobble_period:2245.12})


RETURN
END             