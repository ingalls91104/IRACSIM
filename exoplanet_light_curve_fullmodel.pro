PRO exoplanet_light_curve_fullmodel,t,rel_flux,EXOSYSTEM=exosystem,MSINI=msini,MSTAR=mstar,RSTAR=rstar,TRANSIT_DEPTH=transit_depth,$
    TEQ_P=teq_p,TEFF_STAR=teff_star,INCLINATION=inclination,MJD_TRANSIT=mjd_transit,AR_SEMIMAJ=ar_semimaj,P_ORBIT=p_orbit,$
    NPERIOD=nperiod,MJD_START=mjd_start,SECONDARY_DEPTH=secondary_depth,SECONDARY_LAMBDA=secondary_lambda,RA=ra,DEC=dec,OM=om,$
    VMAG=vmag,DIST=distance,ECC=ecc,RP_RSTAR=rp_rstar,T14=t14,LIMB_DARKENING=limb_darkening,NT=nt,PLOT=plot,PDF=pdf,VERBOSE=verbose,$
    F36=f36,F45=f45,EXODATA=exodata,TPHASE=tphase,DURATION_HR=duration_hr,LABEL=label,START_HR_BEFORE_TRANSIT=start_hr_before_transit,$
    START_PHASE=start_phase,TEXTOUT=textout,ALBEDO=albedo,TRAD=trad,OMROT=omrot, _extra=extra
;;
;;   Model the light curve of an exoplanet, including transits, occultations, and phase curves.  Either input the parameters of the
;;   system by hand or use an actual planet from exoplanets.org.  
;; 
;  OUTPUT PARAMETERS
;    T - time array, in MJD
;    REL_FLUX - flux of system relative to the flux of the star 
;
;; INPUT KEYWORDS
;;
;;  EXOSYSTEM - the name of the exoplanet system, a string found in the exoplanets.org database (eg., "WASP-46 b").  
;;              If this is given then the other keywords can be passed as outputs.  Otherwise they will all have to be input
;;              to specify the parameters of the orbits.
;;  NPERIOD - the number of orbital periods to return.  Default is 1.  If DURATION_HR is set, then this returns the actual number of periods returned.
;;  DURATION_HR - the duration in hours to return.  Default is the orbital period.  Overrides NPERIOD.
;;  MJD_START - modified julian date at the start of the observation.  Default is MJD_TRANSIT - P/2, if MJD_TRANSIT
;;              exists.  Otherwise default is now.
;;  START_HR_BEFORE_TRANSIT - number of hours prior to transit to start the observation.  Phased to the nearest time subsequent to now.
;;  START_PHASE - phase of orbit at which to start the observation.  Phased to the nearest time subsequent to now.
;;  NT - number of elements in time array, default is 10000
;;  LABEL - extra label for the plot file (in addition to the exoplanet system name)
;;                     ***MODEL FREE PARAMETERS***
;;          ALBEDO     = Planetary Bond albedo.  Default = 0.
;;          TRAD       = Radiative timescale, days.  Default = 0.
;;          OMROT      = Rotational angular velocity of planetary emitting layer, in units of the orbital angular velocity 
;;                       at periastron.  Default = 1.0.
;;  /PLOT - set this if we want to plot the lightcurve
;;  /PDF  - send the plot to a pdf file, otherwise to screen
;;  /TEXTOUT - send the light curve to a text file
;;  /VERBOSE - print some progress to the standard output
;;  /TPHASE - return time in orbital phase 
;;
;; INPUTS if not available via the exoplanet database (or EXOSYSTEM is not given), OUTPUTS if determined within the code
;;
;;  MSINI - Minimum mass of planet, MEarth
;;  MSTAR - Mass of host star, MSun
;;  RSTAR - Radius of host star, RSun
;;  TRANSIT_DEPTH - Squared Radius of planet, in units of the host star radius squared
;;  RP_RSTAR - radius of planet, in units of star radius (alternate to TRANSIT_DEPTH 
;;             in the event that no transit has been detected, to allow for phase curves without
;;             transits at lower inclination). 
;;  AR_SEMIMAJ - semimajor axis of orbit, in units of the host star radius
;;  ARGPERI - argument of periastron
;;  ECC     - orbital eccentricity
;;  TEQ_P - Equilibrium temperature of the planet, Kelvins, if we are not modeling the planet atmosphere.
;;  TEFF_STAR - Effective temperature of star, Kelvins
;;  SECONDARY_DEPTH - Depth of secondary transit.  If given, then TEQ_P and TEFF_STAR will be ignored.
;;  SECONDARY_LAMBDA - band at which the secondary depth was measured, or is desired.  Values can be 
;;                     'J', 'H', 'KS', 'KP', '3.6', '4.5', '5.8', '8.0'
;;  INCLINATION - orbital inclination (measured from axis of revolution), degrees
;;  MJD_TRANSIT - Modified Julian date of mid-transit (JD-2400000) (days)
;;  P_ORBIT - orbital period (days). Does not have to be input if AR_SEMIMAJ is input.
;;  EXODATA - The exoplanet database structure, returned from a previous run
;;  LIMB_DARKENING - The set of stellar limb darkening coefficients to use to compute the transit profile.
;;                   See Claret (2000) for a description of the model, and Mandel & Agol (2002) for the
;;                   implications for transit profiles.  NOT AVAILABLE VIA DATABASE--MUST BE INPUT.
;;
;;  
;; OUTPUT KEYWORDS (output when EXOSYSTEM is input)
;;  RA - Right Ascension string (decimal hr, J2000, epoch 2000, from database)
;;  DEC - Declination string (decimal degrees, J2000, epoch 2000, from database)
;;  VMAG - V magnitude of the host star
;;  DISTANCE - distance of system, parsecs
;;  ECC - orbital eccentricity
;;  ARGPERI - argument of periastron, degrees
;;  T14 - time between first and fourth contacts (total transit time)
;;  F36 - Estimated 3.6 micron flux density (mJy) of star, extrapolated from VMAG and TEFF_STAR
;;  F45 - Estimated 4.5 micron flux density (mJy) of star, extrapolated from VMAG and TEFF_STAR
;;  EXODATA - Passes back the exoplanet database as a structure
;;;
;;;; Systems known to have both transits and secondary eclipses
;WASP-1 b,WASP-18 b,WASP-33 b,XO-3 b,WASP-12 b,CoRoT-1 b,XO-2 b,55 Cnc e,HD 80606 b,WASP-19 b,
;OGLE-TR-113 b,GJ 436 b,WASP-14 b,WASP-24 b,WASP-17 b,XO-1 b,HD 149026 b,TrES-3 b,TrES-4 b,WASP-3 b,
;TrES-1 b,Kepler-12 b,TrES-2 b,KOI-13 b,Kepler-7 b,CoRoT-2 b,HAT-P-7 b,Kepler-6 b,Kepler-17 b,
;Kepler-5 b,HD 189733 b,WASP-2 b,HD 209458 b,HAT-P-8 b,HAT-P-1 b,WASP-4 b,HAT-P-6 b,XO-4 b

;exoplanet_data_file = '/Users/jamesingalls/work/IRAC/prf_simulations/exoplanets.csv'

IF N_ELEMENTS(ALBEDO) EQ 0 THEN albedo = 0d
IF N_ELEMENTS(TRAD) EQ 0 THEN trad = 0d
IF N_ELEMENTS(OMROT) EQ 0 THEN omrot = 1d

degrad = !dpi / 180d
au = 149597870700d2  ;; cm
rsun = 6.955d10 ;; cm
SIMPLIFIED_LAMBDA = HASH('J','J','H','H','Ks2MASS','KS','K','KP','IRAC3.6','3.6','IRAC4.5','4.5','IRAC5.7','5.8','IRAC8.0','8.0')
IF N_ELEMENTS(lambda_band) EQ 0 THEN lambda_band = 'IRAC4.5'
 
get_exoplanet_data,EXOSYSTEM=exosystem,MSINI=msini,MSTAR=mstar,RSTAR=rstar,TRANSIT_DEPTH=transit_depth,RP_RSTAR=rp_rstar0,AR_SEMIMAJ=ar_semimaj0,$
                       TEQ_P=teq_p,TEFF_STAR=teff_star0,OM=argperi0,$
                       INCLINATION=inclination0,MJD_TRANSIT=mjd_transit0,P_ORBIT=p_orbit0,EXODATA=exodata,RA=ra,DEC=dec,VMAG=vmag,$
                       DISTANCE=distance,ECC=ecc0,T14=t14,F36=f36,F45=f45,FP_FSTAR0=fp_fstar0,VERBOSE=verbose,INFO_STRING=info_string
                     ;; If no input values do not override database values
IF ~N_ELEMENTS(AR_SEMIMAJ) THEN ar_semimaj=ar_semimaj0
IF ~N_ELEMENTS(ECC) THEN ecc=ecc0
IF ~N_ELEMENTS(ARGPERI) THEN argperi=argperi0
IF ~N_ELEMENTS(RP_RSTAR) THEN rp_rstar=rp_rstar0
IF ~N_ELEMENTS(TEFF_STAR) THEN teff_star=teff_star0
IF ~N_ELEMENTS(P_ORBIT) THEN p_orbit=p_orbit0
IF ~N_ELEMENTS(INCLINATION0) THEN inclination0 = 90.0
IF ~N_ELEMENTS(INCLINATION) THEN inclination=inclination0
IF ~N_ELEMENTS(MJD_TRANSIT) THEN mjd_transit=mjd_transit0

a_au = ar_semimaj * rstar * rsun/au

IF N_ELEMENTS(mjd_start) EQ 0 THEN BEGIN
   mjd_start = get_exoplanet_start(p_orbit,mjd_transit,START_HR_BEFORE_TRANSIT=start_hr_before_transit,START_PHASE=start_phase)
   IF N_ELEMENTS(start_phase) EQ 0 THEN start_phase = PHASEIFY(mjd_start,mjd_transit,p_orbit)
ENDIF ELSE BEGIN
  start_phase = PHASEIFY(mjd_start,mjd_transit,p_orbit)
ENDELSE

IF N_ELEMENTS(mjd_transit) EQ 0 THEN mjd_transit = mjd_start + p_orbit/4.
IF N_ELEMENTS(nperiod) EQ 0 THEN nperiod = 1.0
IF N_ELEMENTS(duration_hr) NE 0 THEN nperiod = duration_hr/24./p_orbit
IF N_ELEMENTS(nt) EQ 0 THEN nt = 1000L
 
t = DINDGEN(nt) / (nt-1) * nperiod * p_orbit + mjd_start
;print,www
exoplanet_phase_curve,ar_semimaj,ecc,argperi,rp_rstar,teff_star,p_orbit,lambda_band,albedo,trad,omrot,tperi,$
  fp_fstar,relative_flux,R=r,Theta=theta,OMORB_PERI=omorb_peri,$
  INCLINATION=inclination,A_AU=a_au,TIDALLY_LOCKED=tidally_locked,LIMB_DARKENING=limb_darkening,$
  ITRANSIT=itransit,IECLIPSE=ieclipse,TEMP_MAX=temp_max,LONG_TEMP_MAX=long_temp_max,AMPLITUDE=amplitude,$
  PHASE_FRACTION=phase_fraction,T_MAX=t_max,T_MIN=t_min,PHASE_MAX=phase_max,PHASE_MIN=phase_min,$
  TEMP_SUBSTELLAR=temp_substellar,TRANSIT_DEPTH=transit_depth,ECLIPSE_DEPTH=eclipse_depth
  
tval = t-mjd_start
phasearray = PHASEIFY(t,mjd_transit,p_orbit)
is = SORT(phase_fraction)
rel_flux = INTERPOL(relative_flux[is],phase_fraction[is],phasearray,/NAN)
IF KEYWORD_SET(TPHASE) THEN BEGIN
   is = sort(phasearray)
   phase = phasearray[is]
   rel_flux = rel_flux[is]
   tplot = phase
   xtitle = 'Orbital Phase'
ENDIF ELSE BEGIN
  tplot = tval
  xtitle = 'Time - MJD'+STRNG(MJD_START)+' (dy)'
ENDELSE
   
IF KEYWORD_SET(PLOT) OR KEYWORD_SET(PDF) OR KEYWORD_SET(TEXTOUT) THEN BEGIN
   IF N_ELEMENTS(exosystem) NE 0 THEN BEGIN
      exoplan_string = STRJOIN(STRSPLIT(exosystem,' ',/EXTRACT),'_',/SINGLE)
      plot_title = exoplan_string
   ENDIF ELSE BEGIN
      exoplan_string = 'exosystem'
      plot_title = ''
   ENDELSE
   IF N_ELEMENTS(LABEL) EQ 0 THEN lab=exoplan_string ELSE  lab = label
ENDIF
IF KEYWORD_SET(PLOT) OR KEYWORD_SET(PDF) THEN BEGIN
   psfile = lab+'_predicted_lightcurve.eps'
   ifps,psfile,12,6,/ENCAPSULATED,NOPOST=~KEYWORD_SET(PDF),/COLOR
      CGPLOT,tplot,rel_flux,title=plot_title,xtitle=xtitle,ytitle='F/F*',xstyle=1,ystyle=1,yrange=ji_minmax(rel_flux)*[0.999,1.001],color='Sky Blue',thick=4,_extra=extra;,psym=1
   ENDPS,psfile,PDF=pdf,NOPOST=~KEYWORD_SET(PDF)
ENDIF
IF KEYWORD_SET(TEXTOUT) THEN BEGIN
   txtfile = lab + '_predicted_lightcurve.txt'
   CASE secondary_lambda OF 
      '3.6':sfs = '  STELLAR FLUX IN BAND: '+STRNG(f36)+' mJy'
      '4.5':sfs = '  STELLAR FLUX IN BAND: '+STRNG(f45)+' mJy'
   ENDCASE
   OPENW,lun,txtfile,/get_lun
   PRINTF,lun,'#   '+info_string
   PRINTF,lun,'#   WAVEBAND: '+secondary_lambda+' '+sfs
   PRINTF,lun,'#   MODEL INPUTS: ALBEDO: '+STRNG(albedo)+'  T_RAD: '+STRNG(trad)+'(dy)   OMEGA_ROT: '+STRNG(omrot) 
   PRINTF,lun,'#   MODEL RESULTS: TRANSIT DEPTH: '+STRNG(transit_depth)+'  ECLIPSE DEPTH: '+STRNG(eclipse_depth)
   PRINTF,lun,'#                  PHASE CURVE AMPLITUDE: '+STRNG(amplitude)+' PHASE(MAX): '+STRNG(phase_max)+' PHASE(MIN): '+STRNG(phase_min) 
   PRINTF,lun,'#'
   PRINTF,lun,'#   MJD_START: '+STRNG(mjd_start)+'   PHASE_START: '+STRNG(start_phase)
   PRINTF,lun,'#  Time since MJD_START (dy)        Relative Flux'
   FOREACH torbit,tval,i DO PRINTF,lun,torbit,rel_flux[i]
   FREE_LUN,lun
   PRINT,'Text version of lightcurve saved in '+txtfile+'.'
ENDIF

RETURN
END

 

   