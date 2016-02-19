pro irac_pointing_model,t,x0,y0,x,y,noresample=noresample,verbose=verbose,tfull=tfull,plot=plot,outfile=outfile,seed=seed,$
    dt_sample = dt_sample,pointing_model_hash=pmh,jitter_x=jitter_x,fbm_noise_x=fbm_noise_x,predrift_x=predrift_x,$
    drift_x=drift_x,wobble_x=wobble_x,spike_x=spike_x,pulse_x=pulse_x,jitter_y=jitter_y,fbm_noise_y=fbm_noise_y,$
    predrift_y=predrift_y,drift_y=drift_y,wobble_y=wobble_y,spike_y=spike_y,pulse_y=pulse_y,drift_timezero=drift_timezero
    
;;   POINTING_MODEL_HASH - IDL Hash table specifying the parameters of the pointing model, containing values of one or more of the following hash keys:
;
;;                             Jitter - sinusoid of period around 1 minute, with amplitude of about 1/50 of a pixel
;;                         JPERIOD: seconds (default 319.676)
;;                         JPHASE: radians from 0 to 2pi (default 0.239936)
;;                         JAMPLITUDE: pixels (default 0.007)
;;                         JAXIS: degrees (orientation) (default 47.4)
;
;;                               1/f Noise - fBm simulation with peak-to-peak given, spectral index 0.9 to 1.7.  Noise is independently realized in each axis
;;                          FBM_PEAK: pixels (default 0.4)
;;                          FBM_INDEX: power spectrum index (default 1.0)
;
;;                              Pre-Drift - Short term "pre" drift - positive or negative, with asymptote and overshoot
;;                          PREDRIFT_ASYMPTOTE: Difference between starting position and final, pixels (default 0.76)
;;                          PREDRIFT_DECAY: e-folding of asymptotic behavior (seconds) (default 1565.)
;;                          PREDRIFT_PERIOD: sinusoidal period (seconds) (default 12492.8)
;;                          PREDRIFT_AXIS: orientation on pixel (degrees) (default -68)
;
;;                              Long Term Drift - linear trend
;;                          DRIFT_MAGNITUDE: arcsec per hour (default 0.0067)
;;                          DRIFT_AXIS: orientation, degrees (default -77.3)
;;                          DRIFT_TIMEZERO: time at which drift is zero (y intercept in drift vs time plot) (default 0.00)
;
;;                              Heater Wobble - sinusoid, or skewed sinusoid ("sawtooth")
;;                          WOBBLE_AXIS: orientation, degrees (default 41.74)
;;                          WOBBLE_AMPLITUDE: maximum excursion, pixels (default 0.091)
;;                          WOBBLE_PHASE: 0 to 2pi, radians (default 0.81)
;;                          WOBBLE_SKEW: fraction of a period at which the peak occurs. To skew to the left (fast rise), set between 0 and 1/4.
;;                                       To skew to the right (slow rise), set between 1/4 and 1/2.  A normal sine curve is A=1/4. (default 0.167)
;;                          WOBBLE_PERIOD: seconds (default 2245.12)
;;                               Time-Varying Wobble: Amplitude and Period will vary according to a random walk 
;;                                                    (FBM with index 2) 
;;                          WOBBLE_FM_MAX_EXCURSION: For time-varying period, the maximum excursion in period from 
;;                                                   WOBBLE_PERIOD (seconds) (default 200)
;;                          WOBBLE_AM_MAX_EXCURSION: For time-varying amplitude, the maximum excursion in amplitude from
;;                                                   WOBBLE_AMPLITUDE (pixels) (default 0.05)
;
;;                              Spike - Gaussian-shaped excursions in pointing
;;                          SPIKE_TIME: Midpoint in time of the spike(s) (seconds) -- array of values (one per spike).  Default is [0], meaning no spike
;;                          SPIKE_AXIS: orientation of excursion(s), (degrees) -- array of values (default: [0.0])
;;                          SPIKE_AMPLITUDE: peak of excursion(s) (pixels) -- array of values (default: [0.1])
;;                          SPIKE_WIDTH: FWHM of excursion(s) (seconds) -- array of values (default: [0.05])
;;          
;;                              Pulse - Amplitude Modulated sinusoidal pulse in pointing - modulation envelope is symmetric about zero
;;                          PULSE_START: start time of the pulse(s) (seconds) -- array of values (one per pulse). Default is [0], meaning no pulse
;;                          PULSE_DURATION: Duration of the pulse(s) (seconds) -- array of values (one per pulse). Default is [10.]
;;                          PULSE_AMPLITUDE: Peak of excursion(s) (pixels) -- array of values (one per pulse). Default is [0.1]
;;                          PULSE_PERIOD: Period of high frequency carrier signal (seconds) -- array of values (one per pulse). Default is [0.1]
;;                          PULSE_AXIS: orientation of excursion(s), (degrees) -- array of values (default: [0.0])
;
;;  If any of these keys are not set, values will be set to defaults given.
;;; To zero out any features, set the corresponding component of the pointing model hash to the following:
;;;  POINTING_MODEL_HASH = HASH('JAMPLITUDE',0.0,$
;                               'FBM_PEAK',0.0,$
;                                'PREDRIFT_ASYMPTOTE',0.0,$
;                                'DRIFT_MAGNITUDE',0.0,$
;                                'WOBBLE_AMPLITUDE',0.0,$
;                                'WOBBLE_AM_MAX_EXCURSION',0.0)   
;                                
;                                 

IF N_ELEMENTS(pmh) EQ 0 THEN pmh = HASH()
;; Input a time array T (seconds) and the (X0,Y0) starting position, output the X and Y centroid positions 
;;
IF ~KEYWORD_SET(dt_sample) THEN dt_sample = 1d-3  ;; Sample in 1 ms increments 
;;
IF N_ELEMENTS(seed) EQ 0 THEN seed_start = -1 ELSE seed_start = seed
;IF KEYWORD_SET(verbose) THEN print,'Starting Seed: ',seed_start
trange = ji_minmax(t)
tdiff = trange[1]-trange[0]
nfullsamp = LONG64(tdiff / dt_sample)
IF N_ELEMENTS(tfull) EQ 0 THEN BEGIN
   tfull =  dindgen(nfullsamp)*dt_sample + trange[0]
   IF KEYWORD_SET(verbose) THEN BEGIN
      PRINT,'Creating a '+strng(nfullsamp)+'-element array of '+STRNG(dt_sample*1000,FORMAT='(f5.3)')+' ms time samples ranging from '+strng(trange[0])+' to '+strng(trange[1])+'s.'
;   HELP,tfull
      PRINT,''
   ENDIF
ENDIF

tsince0 = tfull;-min(tfull)

;;
;; The pointing is the sum of 5 features, each of which has an X and Y component.  Specify the feature along
;; an arbitrary axis, in most cases hovering around 45 degrees, with drift components coming out slightly skewed
;; towards the Y-direction (angle between 45 and 135, and 
;;  1) Jitter - sinusoid of period 1 minute (+/- 0.1), with random phase, and amplitude of 1/50 of a pixel
IF ~pmh.HasKey('JPERIOD') THEN pmh['JPERIOD'] = (randomu(seed,1)*0.2 + 0.9) * 60.  
jperiod = pmh['JPERIOD'];;; seconds
IF ~pmh.HasKey('JPHASE') THEN  pmh['JPHASE'] = randomu(seed,1) * 2.*!pi  
jphase = pmh['JPHASE'] ;;; 0 to 2pi 
IF ~pmh.HasKey('JAMPLITUDE') THEN pmh['JAMPLITUDE'] = 1./( randomu(seed,1) * 10 + 45 )   
jamplitude = pmh['JAMPLITUDE'] ;;; 1/45 to 1/55 pixel 
IF ~pmh.HasKey('JAXIS') THEN pmh['JAXIS'] = randomn(seed,1)*5 + 45 >35 < 55 
jaxis=pmh['JAXIS'];;  40 to 50 degrees, with gaussian peak at 45     

jitter_x = jamplitude[0] * sin(2.*!dpi * tsince0/jperiod[0] + jphase[0]) * cos(jaxis[0]/!radeg)
jitter_y = jamplitude[0] * sin(2.*!dpi * tsince0/jperiod[0] + jphase[0]) * sin(jaxis[0]/!radeg)
IF KEYWORD_SET(verbose) THEN BEGIN
   PRINT,'Jitter parameters: '
   PRINT,'     PERIOD: '+STRNG(jperiod)
   PRINT,'     PHASE: '+STRNG(jphase)
   PRINT,'     AMPLITUDE: '+STRNG(jamplitude)
   PRINT,'     AXIS: '+STRNG(jaxis)
;   HELP,jitter_x,jitter_y
   PRINT,''
ENDIF   

;;  2) 1/f noise - fBm simulation with peak-to-peak +/- 0.15 pix, spectral index 0.9 to 1.7
;;     noise is independently realized in each axis
IF ~pmh.HasKey('FBM_PEAK') THEN pmh['FBM_PEAK'] = 0.02 * randomu(seed,1) + 0.14 
fbm_peak = pmh['FBM_PEAK']
IF ~pmh.HasKey('FBM_INDEX') THEN pmh['FBM_INDEX'] = 0.8*randomu(seed,1)  + 0.9 
fbm_index=pmh['FBM_INDEX']
nfbm = N_ELEMENTS(fbm_peak)
fbm_noise_x = 0d
fbm_noise_y = 0d
FOR i = 0,nfbm-1 DO BEGIN 
   fbm1,nfullsamp,fbm_index[i],seed,z_x,maxpix=2048
;z_x -= MEAN(z_x)
   fbm1,nfullsamp,fbm_index[i],seed,z_y,maxpix=2048
;z_y -= MEAN(z_y)
   fbm_noise_x += z_x/MAX(z_x,/ABS) * fbm_peak[i]
   fbm_noise_y += z_y/MAX(z_y,/ABS) * fbm_peak[i]
ENDFOR
IF KEYWORD_SET(verbose) THEN BEGIN
   PRINT,'1/f noise parameters: '
   PRINT,'     PEAK: '+STRNG(fbm_peak)
   PRINT,'     SPECTRAL INDEX: '+STRNG(fbm_index)
;   HELP,fbm_noise_x,fbm_noise_y
   PRINT,''
ENDIF   

;;;
;;; 3) Short term "pre" drift - 0 to 80 second long, positive or negative, with asymptote and overshoot
predrift_phi = 1.75*!pi   ;; fix the phase shift at 7/4 pi for now
;; Difference between starting position and asymptote,px.  Second random number determines the sign:
IF ~pmh.HasKey('PREDRIFT_ASYMPTOTE') THEN pmh['PREDRIFT_ASYMPTOTE'] = randomu(seed,1)*1 
predrift_asymptote=pmh['PREDRIFT_ASYMPTOTE'] 
a = predrift_asymptote[0] / sin(predrift_phi[0])
IF ~pmh.HasKey('PREDRIFT_DECAY') THEN pmh['PREDRIFT_DECAY'] = randomu(seed,1) * 3600. + 0.1 
predrift_decay=pmh['PREDRIFT_DECAY'];; decay from 0.1s to 1 hr.
IF ~pmh.HasKey('PREDRIFT_PERIOD') THEN BEGIN
   ;; Make period proportional to the asymptote
;   period_mean = 60* (ABS(predrift_asymptote) * 100 + 100.)  ;;; 100  to 200 minutes for asymptote 0 to 1 pix 
   ;; Make period related to the decay
   ;; The decay should decrease to 2% of its value by the middle of the 1st period 
   period_mean = -predrift_decay * alog(0.02) * 2 ;60* ( (-predrift_decay/60)*190/30. + 200.   )  ;;; 0  to 200 minutes for decay 0 to 60 min
   pmh['PREDRIFT_PERIOD'] = (randomn(seed,1) * 5. )*60 + period_mean;; period fluctuations of +/- 5 min
ENDIF 
predrift_period=pmh['PREDRIFT_PERIOD']

omega = 2.*!dpi / predrift_period[0]
IF ~pmh.HasKey('PREDRIFT_AXIS') THEN pmh['PREDRIFT_AXIS'] = (-1)^ROUND(randomu(seed,1))*(randomn(seed,1)*15 + 90.) 
predrift_axis=pmh['PREDRIFT_AXIS'] ;; normal distribution centered on 90 +/- 45 degrees
;; (mostly Y)
predrift = a * exp(-tsince0/predrift_decay[0]) * sin(omega*tsince0 + predrift_phi[0]) 
predrift_x =  predrift * cos(predrift_axis[0]/!radeg)  
predrift_y =  predrift * sin(predrift_axis[0]/!radeg)
IF KEYWORD_SET(verbose) THEN BEGIN
   PRINT,'Short term "pre"-drift parameters: '
   PRINT,'     ASYMPTOTE: '+STRNG(predrift_asymptote)
   PRINT,'     DECAY: '+STRNG(predrift_decay)
   PRINT,'     PERIOD: '+STRNG(predrift_period)
   PRINT,'     AXIS: '+STRNG(predrift_axis)
;   HELP,predrift_x,predrift_y
   PRINT,''
ENDIF   

;;; 4) Long term drift, linear, ranging from +/- 0.03" per hour 
IF ~pmh.HasKey('DRIFT_MAGNITUDE') THEN pmh['DRIFT_MAGNITUDE'] = randomu(seed,1)*0.03/1.2  
drift_magnitude=pmh['DRIFT_MAGNITUDE'];; convert to pixels per hour
IF ~pmh.HasKey('DRIFT_AXIS') THEN pmh['DRIFT_AXIS'] =  (-1)^ROUND(randomu(seed,1)) *(randomn(seed,1)*15 + 90.)
drift_axis=pmh['DRIFT_AXIS'];; normal distribution centered on 90 +/- 45
IF ~pmh.HasKey('DRIFT_TIMEZERO') THEN pmh['DRIFT_TIMEZERO'] =  0.00
drift_timezero = pmh['DRIFT_TIMEZERO']
drift_x =   (drift_magnitude[0] * (tsince0-drift_timezero) / 3600.0) *cos(drift_axis[0]/!radeg)
drift_y = (drift_magnitude[0] * (tsince0-drift_timezero) / 3600.0) * sin(drift_axis[0]/!radeg)
IF KEYWORD_SET(verbose) THEN BEGIN
   PRINT,'Long term drift parameters: '
   PRINT,'  MAGNITUDE: '+STRNG(drift_magnitude)
   PRINT,'       AXIS: '+STRNG(drift_axis)
   PRINT,'  TIME ZERO: '+STRNG(drift_timezero)
;   HELP,drift_x,drift_y
   PRINT,''
ENDIF   


;;; 5) Wobble - sinusoid, or skewed sinusoid ("sawtooth") with amplitude ~0.06px, period ~3000sec.
;;;
IF ~pmh.HasKey('WOBBLE_AXIS') THEN pmh['WOBBLE_AXIS'] = randomn(seed,1)*5+ 45 >35 < 55 
wobble_axis=pmh['WOBBLE_AXIS'] ;; 40 to 50 degrees approx  (45 +/- 5 deg)
IF ~pmh.HasKey('WOBBLE_AMPLITUDE') THEN pmh['WOBBLE_AMPLITUDE'] = randomu(seed,1)*0.04 + 0.06 
wobble_amplitude=pmh['WOBBLE_AMPLITUDE']; (0.06 +/- .04)
IF ~pmh.HasKey('WOBBLE_PERIOD') THEN pmh['WOBBLE_PERIOD'] = randomu(seed,1) *  800 + 2000.0  
wobble_period=pmh['WOBBLE_PERIOD'];(2000 to 2800 s, or 33 to 46 minutes)
IF ~pmh.HasKey('WOBBLE_SKEW') THEN pmh['WOBBLE_SKEW'] = randomn(seed,1) *0.035 + 0.2 > 0.1 < 0.4 
wobble_skew=pmh['WOBBLE_SKEW'];; 0.2 +/- 0.025
IF ~pmh.HasKey('WOBBLE_PHASE') THEN pmh['WOBBLE_PHASE'] = randomu(seed,1)
wobble_phase=pmh['WOBBLE_PHASE'];;0 to 1
IF ~pmh.HasKey('WOBBLE_FM_MAX_EXCURSION') THEN pmh['WOBBLE_FM_MAX_EXCURSION'] = 200.0
wobble_fm_max_excursion=pmh['WOBBLE_FM_MAX_EXCURSION']
IF ~pmh.HasKey('WOBBLE_AM_MAX_EXCURSION') THEN pmh['WOBBLE_AM_MAX_EXCURSION'] = 0.02
wobble_am_max_excursion=pmh['WOBBLE_AM_MAX_EXCURSION']

fbm1,nfullsamp,4.0,seed,amp_excursion;,maxpix=16384000L
;; Rescale amplitude excursion 
amp_excursion -= MEAN(amp_excursion)
amp_excursion *= wobble_am_max_excursion/MAX(ABS(amp_excursion))
wob_amp_t = wobble_amplitude[0] + amp_excursion

fbm1,nfullsamp,4.0,seed,period_excursion;,maxpix=2048;,maxpix=16384000L
period_excursion -= MEAN(period_excursion)
period_excursion *= wobble_fm_max_excursion/MAX(ABS(period_excursion))
wob_per_t = wobble_period[0] + period_excursion


wobble = wob_amp_t * sin_skew(tsince0,wob_per_t,wobble_skew[0],phase=wobble_phase[0])
wobble_x = wobble * cos(wobble_axis[0]/!radeg)
wobble_y = wobble * sin(wobble_axis[0]/!radeg)
IF KEYWORD_SET(verbose) THEN BEGIN
  PRINT,'Wobble parameters: '
  PRINT,'     AMPLITUDE: '+STRNG(wobble_amplitude)
  PRINT,'     SKEW: '+STRNG(wobble_skew)
  PRINT,'     PERIOD: '+STRNG(wobble_period)
  PRINT,'     PHASE: '+STRNG(wobble_phase)
  PRINT,'     AXIS: '+STRNG(wobble_axis)
  PRINT,'     PERIOD_MAX_EXCURSION: '+STRNG(wobble_fm_max_excursion)
  PRINT,'     AMP_MAX_EXCURSION: '+STRNG(wobble_am_max_excursion)
  ;   HELP,wobble_x,wobble_y
  PRINT,''
ENDIF
;;; 6) Spikes - gaussian excursions in pointing   
;;;SPIKE_TIME: Midpoint in time of the spike(s) (seconds) -- array of values (one per spike).  Default is [0]
;;                          SPIKE_AXIS: orientation of excursion(s), (degrees) -- array of values (default: [45.0])
;;                          SPIKE_AMPLITUDE: peak of excursion(s) (pixels) -- array of values (default: [0.0])
;;                          SPIKE_WIDTH: FWHM of excursion(s) (seconds) -- array of values (default: [0.05])
spike_x = 0
spike_y = 0
IF ~pmh.HasKey('SPIKE_TIME') THEN pmh['SPIKE_TIME'] = 0
spike_time=pmh['SPIKE_TIME'] 
IF spike_time[0] NE 0 THEN BEGIN
   nspikes = N_ELEMENTS(spike_time) 
   IF ~pmh.HasKey('SPIKE_AMPLITUDE') THEN pmh['SPIKE_AMPLITUDE'] = MAKE_ARRAY(nspikes,VALUE=0.1)
   spike_amplitude = pmh['SPIKE_AMPLITUDE']
   IF ~pmh.HasKey('SPIKE_WIDTH') THEN pmh['SPIKE_WIDTH'] = MAKE_ARRAY(nspikes,VALUE=0.05)
   spike_width = pmh['SPIKE_WIDTH']
   IF ~pmh.HasKey('SPIKE_AXIS') THEN pmh['SPIKE_AXIS'] = MAKE_ARRAY(nspikes,VALUE=45.0)
   spike_axis = pmh['SPIKE_AXIS']
   FOR ispike = 0,nspikes-1 DO BEGIN
     spike_i = gaussfunc(tsince0,spike_time[ispike],spike_width[ispike],spike_amplitude[ispike]) 
     spike_x += spike_i * cos(spike_axis[ispike]/!radeg)
     spike_y += spike_i * sin(spike_axis[ispike]/!radeg)
   ENDFOR
   IF KEYWORD_SET(verbose) THEN BEGIN
     PRINT,'Spike parameters: '
     PRINT,'     '+STRNG(nspikes)+' Spikes'
     PRINT,'     TIME: '+STRJOIN(STRNG(spike_time),',')
     PRINT,'     AMPLITUDE: '+STRJOIN(STRNG(spike_amplitude),',')
     PRINT,'     WIDTH: '+STRJOIN(STRNG(spike_width),',')
     PRINT,'     AXIS: '+STRJOIN(STRNG(spike_axis),',')
     PRINT,''
   ENDIF
ENDIF
;;; 7) Pulses - amplitude modulated sinusoidal ringing with a finite pulse width. Modulation envelope is symmetric about zero
;;                          PULSE_START: start time of the pulse(s) (seconds) -- array of values (one per pulse). Default is [0], meaning no pulse
;;                          PULSE_DURATION: Duration of the pulse(s) (seconds) -- array of values (one per pulse). Default is [10.]
;;                          PULSE_AMPLITUDE: Peak of excursion(s) (pixels) -- array of values (one per pulse). Default is [0.1]
;;                          PULSE_PERIOD: Period of high frequency carrier signal (seconds) -- array of values (one per pulse). Default is [0.1]
;;                          PULSE_AXIS: orientation of excursion(s), (degrees) -- array of values (default: [0.0])
pulse_x = 0
pulse_y = 0
IF ~pmh.HasKey('PULSE_START') THEN pmh['PULSE_START'] = 0
pulse_start=pmh['PULSE_START']
IF pulse_start[0] NE 0 THEN BEGIN
  npulses = N_ELEMENTS(pulse_start)
  IF ~pmh.HasKey('PULSE_AMPLITUDE') THEN pmh['PULSE_AMPLITUDE'] = MAKE_ARRAY(npulses,VALUE=0.1)
  pulse_amplitude = pmh['PULSE_AMPLITUDE']
  IF ~pmh.HasKey('PULSE_DURATION') THEN pmh['PULSE_DURATION'] = MAKE_ARRAY(npulses,VALUE=10.)
  pulse_duration = pmh['PULSE_DURATION']
  IF ~pmh.HasKey('PULSE_AXIS') THEN pmh['PULSE_AXIS'] = MAKE_ARRAY(npulses,VALUE=45.0)
  pulse_axis = pmh['PULSE_AXIS']
  IF ~pmh.HasKey('PULSE_PERIOD') THEN pmh['PULSE_PERIOD'] = MAKE_ARRAY(npulses,VALUE=0.1)
  pulse_period = pmh['PULSE_PERIOD']
  FOR ipulse = 0,npulses-1 DO BEGIN
    pulse_modulation = SIN(2.*!dpi * (tsince0-pulse_start[ipulse])/(2*pulse_duration[ipulse]))
    pulse_window = WHERE(tsince0 GE pulse_start[ipulse] AND tsince0 LE pulse_start[ipulse] + pulse_duration[ipulse],nwindow,$
                         complement=pulse_zero,ncomplement=n_zero)
    IF n_zero NE 0 THEN pulse_modulation[pulse_zero] = 0.D
    IF nwindow NE 0 THEN BEGIN
       pulse_carrier = COS(2.*!dpi * tsince0 / pulse_period[ipulse])
       pulse_i = pulse_amplitude[ipulse] * pulse_modulation * pulse_carrier
    ENDIF ELSE pulse_i = tsince0 * 0.0
    pulse_x += pulse_i * cos(pulse_axis[ipulse]/!radeg)
    pulse_y += pulse_i * sin(pulse_axis[ipulse]/!radeg)
  ENDFOR
  IF KEYWORD_SET(verbose) THEN BEGIN
    PRINT,'Pulse parameters: '
    PRINT,'     '+STRNG(npulses)+' Pulses'
    PRINT,'     START TIME: '+STRJOIN(STRNG(pulse_start),',')
    PRINT,'     AMPLITUDE: '+STRJOIN(STRNG(pulse_amplitude),',')
    PRINT,'     DURATION: '+STRJOIN(STRNG(pulse_duration),',')
    PRINT,'     CARRIER PERIOD: '+STRJOIN(STRNG(pulse_period),',')
    PRINT,'     AXIS: '+STRJOIN(STRNG(pulse_axis),',')
    PRINT,''
  ENDIF
ENDIF

x = x0 + jitter_x + fbm_noise_x + predrift_x + drift_x + wobble_x + spike_x + pulse_x
y = y0 + jitter_y + fbm_noise_y + predrift_y + drift_y + wobble_y + spike_y + pulse_y

IF KEYWORD_SET(verbose) THEN HELP,x,y

IF KEYWORD_SET(NORESAMPLE) THEN BEGIN
   ;; Resample to the input time grid.
   resamp_amount = 1
ENDIF ELSE resamp_amount = 100
IF KEYWORD_SET(plot) THEN BEGIN
  nr = nfullsamp/resamp_amount
  trebin = CONGRID(tfull/3600.,nr)
  ;window,0,xsize=1500,ysize=1300
  IF N_ELEMENTS(outfile) EQ 0 THEN outfile = 'irac_pointing_model_'+STRNG(LONG64(systime(/SECONDS)))+'.eps'
  ifps,outfile,12,12,/encap,/color
  !p.multi = [0,2,5+(SPIKE_TIME[0] NE 0) + (PULSE_START[0] NE 0)]
  ;IF SPIKE_TIME[0] EQ 0 THEN !p.multi = [0,2,5] ELSE !p.multi = [0,2,6]
  cgplot,trebin*60.,CONGRID(jitter_x+fbm_noise_x,nr),ytitle='X Jitter + noise (px)',xrange=[0,60],xtitle='Time (min)';,psym=3
  cgplot,trebin*60.,CONGRID(jitter_y+fbm_noise_y,nr),ytitle='Y Jitter + noise (px)',xrange=[0,60],xtitle='Time (min)';,psym=3
  cgplot,trebin,CONGRID(predrift_x,nr),ytitle='X Short Term Drift (px)',xstyle=1,xtitle='Time (hr)'
  cgplot,trebin,CONGRID(predrift_y,nr),ytitle='Y Short Term Drift (px)',xstyle=1,xtitle='Time (hr)'
  cgplot,trebin,CONGRID(drift_x,nr),ytitle='X Long Term Drift (px)',xstyle=1,xtitle='Time (hr)'
  cgplot,trebin,CONGRID(drift_y,nr),ytitle='Y Long Term Drift (px)',xstyle=1,xtitle='Time (hr)'
  cgplot,trebin,CONGRID(wobble_x,nr),ytitle='X Wobble (px)',xstyle=1,xtitle='Time (hr)'
  cgplot,trebin,CONGRID(wobble_y,nr),ytitle='Y Wobble (px)',xstyle=1,xtitle='Time (hr)'
  IF SPIKE_TIME[0] NE 0 THEN BEGIN
    cgplot,trebin,CONGRID(spike_x,nr),ytitle='X Spikes (px)',xstyle=1,xtitle='Time (hr)'
    cgplot,trebin,CONGRID(spike_y,nr),ytitle='Y Spikes (px)',xstyle=1,xtitle='Time (hr)'
  ENDIF
  IF PULSE_START[0] NE 0 THEN BEGIN
    cgplot,trebin,CONGRID(pulse_x,nr),ytitle='X Pulses (px)',xstyle=1,xtitle='Time (hr)'
    cgplot,trebin,CONGRID(pulse_y,nr),ytitle='Y Pulses (px)',xstyle=1,xtitle='Time (hr)'
  ENDIF
  cgplot,trebin,CONGRID(x,nr),ytitle='X (px)',xstyle=1,xtitle='Time (hr)',yrange=[14.,16],psym=3;16,symsize=.5
  cgplot,trebin,CONGRID(y,nr),ytitle='Y (px)',xstyle=1,xtitle='Time (hr)',yrange=[14.,16],psym=3;16,symsize=0.5
  IF ISA(seed_start,/SCALAR) THEN xyouts,0.5,0.4,'Starting Seed: '+STRNG(seed_start),/NORM,ALIGNMENT=0.5,charsize=0.9
  endps,outfile,/png
  !p.multi = 0
ENDIF

RETURN
END
