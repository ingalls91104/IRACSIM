FUNCTION exoplanet_phase_temp_ode,tnorm,temp_norm
;; Allow for nonzero normalized source function to be passed via common block
common t_ode, inorm_ij
RETURN,[inorm_ij - temp_norm^4]
END

PRO exoplanet_phase_curve,ar_semimaj,e,argperi,rp_rstar,teff_star,porbit,lambda_band,albedo,trad,omrot,tperi,$
                          fp_fstar,relative_flux,R=r,Theta=theta,OMORB_PERI=omorb_peri,OBS_ANOMALY=phase,$
                          INCLINATION=inclination,A_AU=a_au,LIMB_DARKENING=limb_darkening,PLOT=plot,VERBOSE=verbose,$
                          TIDALLY_LOCKED=tidally_locked,$
                          ITRANSIT=itransit,IECLIPSE=ieclipse,TEMP_MAX=temp_max,LONG_TEMP_MAX=long_temp_max,AMPLITUDE=amplitude,$
                          PHASE_FRACTION=phase_fraction,T_MAX=t_max,T_MIN=t_min,PHASE_MAX=phase_max,PHASE_MIN=phase_min,$
                          TEMP_SUBSTELLAR=temp_substellar,TRANSIT_DEPTH=transit_depth,ECLIPSE_DEPTH=eclipse_depth
;;
;;  Compute phase curve for exoplanet, the measured flux of the planet normalized by
;;  the stellar flux.
;;  INPUTS: 
;;          AR_SEMIMAJ = Orbital semimajor axis, in units of the stellar radius
;;          E          = Orbital eccentricity
;;          ARGPERI    = Argument of periastron, degrees (angle between plane of sky and periastron direction from star)
;;          RP_RSTAR   = Planetary radius, in units of the stellar radius
;;          TEFF_STAR  = Stellar effective temperature
;;          PORBIT     = Orbital period, days
;;         LAMBDA_BAND = band at which the measurement is to be made.  Values can be
;;                       'U', 'B', 'V', 'R', 'I', 'J', 'H', 'K', 'L', 'M', 'N', 'O', 'J2MASS', 'H2MASS', 'Ks2MASS', 
;;                       'IRAC3.6','IRAC4.5', 'IRAC5.7', 'IRAC8.0', 'g', 'r', 'i', 'z'
;;                     ***MODEL FREE PARAMETERS***
;;          ALBEDO     = Planetary Bond albedo.  May be taken to be 0 for most cases.
;;          TRAD       = Radiative timescale, days
;;          OMROT      = Rotational angular velocity of planetary emitting layer, in units of the orbital angular velocity 
;;                       at periastron.  If keyword /TIDALLY_LOCKED is set then this is ignored on input and output as 
;;                       the instantaneous orbital angular velocity.
;;  OUTPUTS:                       
;;          TPERI      = Time since periastron for output light curve, days.  This will be a vector of about 1000 points.
;;                       Even though the program will run through 3 or more orbits, to ensure steady state only a single 
;;                       orbit will be output.
;;          FP_FSTAR   = Unobstructed planet/star flux as a function of TPERI 
;;          RELATIVE_FLUX = The flux of the system (Star + Planet) as a function of TPERI, relative to the unobstructed
;;                          star flux.  This includes (possible) transits and eclipses.
;; 
;;  OPTIONAL KEYWORDS:
;;          INCLINATION = Orbital Inclination  (measured from axis of revolution), degrees
;;                        Currently only used for determining transit and eclipse durations, so default is 90.
;;          A_AU       = Semimajor axis, AU (if not input, assume 1)
;;               R     = Distance from star of planet over the course of TPERI, in units of the semimajor axis.
;;           THETA     = Polar angle of planetary orbit as a function of TPERI, degrees.  Measured from the line 
;;                       going from the star to the planet at periastron.  Also known as True Anomaly.
;;     LIMB_DARKENING  = The set of stellar limb darkening coefficients to use to compute the transit profile.
;;                       See Claret (2000) for a description of the model, and Mandel & Agol (2002) for the
;;                       effect on transit profiles.  Leave undefined for no limb darkening.
;;         OMORB_PERI  = Angular velocity as a function of TPERI, relative to the angular velocity at perihelion.
;;         OBS_ANOMALY = True Anomaly, phased to the line of sight, in radians, as a function of time.
;;                       This is the angle the planet's radius vector makes with the line of sight.
;;      PHASE_FRACTION = What transit observers mean by phase, i.e., the temporal fraction of the orbit relative to transit,
;;                       for each time point.    
;;         ITRANSIT    = Index to TPERI, FP_FSTAR, RELATIVE_FLUX, R, THETA, and PHASE arrays that is closest to transit.  If
;;                       there is no transit, then ITRANSIT=!NULL
;;         IECLIPSE    = Index to TPERI, FP_FSTAR, RELATIVE_FLUX, R, THETA, and PHASE arrays that is closest to eclipse.  If
;;                       there is no eclipse, then IECLIPSE=!NULL
;;       TRANSIT_DEPTH = The actual depth of transit in the light curve, i.e., the flux at ITRANSIT divided by the
;;                       unobstructed star + planet flux at that time.
;;       ECLIPSE_DEPTH = The actual depth of eclipse in the light curve, i.e., the flux at IECLIPSE divided by the
;;                       unobstructed star + planet flux at that time.
;;     TEMP_SUBSTELLAR = Temperature at the substellar point, as a function of time
;;         TEMP_MAX    = Maximum temperature on surface of planet, as a function of time.
;;     LONG_TEMP_MAX   = Longitude at which planet reaches TEMP_MAX, as a function of time.
;;         AMPLITUDE   = Amplitude of phase curve, in units of stellar flux. 
;;         T_MAX       = TPERI of peak amplitude.  May not be an actual time in the returned TPERI array.
;;         T_MIN       = TPERI of minimum amplitude.  May not be an actual time in the returned TPERI array.
;;     PHASE_MAX       = PHASE_FRACTION of peak amplitude.  May not be an actual value in the PHASE_FRACTION array.
;;     PHASE_MIN       = PHASE_FRACTION of minimum amplitude.  May not be an actual value in the PHASE_FRACTION array.
;;         /TIDALLY_LOCKED  = Force OMROT to be equal to the instantaneous orbital angular velocity.
;;         /PLOT       = Set this to produce a multipanel pdf plot of the light curve and other interesting quantities, similar
;;                       to Cowan & Agol (2011), Fig. 5. 
;;         /VERBOSE    = Set this to allow text output to the screen
;;
;;  This code follows the method outlined in Cowan and Agol 2011 "A Model for Thermal Phase Variations of Circular and Eccentric
;;  Exoplanets", ApJ 726, 82; and the treatment of the visibility and illumination (to a distant observer) of a planet's surface 
;;  regions in Appendix A of Cowan, et al, 2009, "Alien Maps of an Ocean-Bearing World," ApJ 700, 915
;;                                         
  common t_ode, inorm_ij  ;;; Pass normalized source function to differential equation solver via common block
  IF N_ELEMENTS(INCLINATION) EQ 0 THEN inclination = 90.0
  IF N_ELEMENTS(A_AU) EQ 0 then a_au = 1.0
  h = 6.626068d-27
  kb = 1.3806503d-16
  c = 2.99792458d10
  yrsec = 3.15569d7
  dysec = 24.*3600.
  Lambda_eff = HASH('U',0.36,'B',0.44,'V',0.55,'R',0.71,'I',0.97,'J',1.25,'H',1.60,'K',2.22,'L',3.54,'M',4.80,'N',10.6,'O',21.0,$
    'J2MASS',1.235,'H2MASS',1.662,'Ks2MASS',2.159,'IRAC3.6',3.5612,'IRAC4.5',4.5095,'IRAC5.7',5.6895,'IRAC8.0',7.9584,$
    'g',0.52,'r',0.67,'i',0.79,'z',0.91)  ;;  microns
  nu_eff = c / (Lambda_eff[lambda_band]*1d-4)
  t_hnu = h * nu_eff / kb
  ;;; Determine how many orbits we need to model to reach equilibrium.  Must be at least 3 * TRAD
  norbit_trad = trad GT 0 ?  CEIL(trad/porbit) : 1.0 
  ntime = 3000 * norbit_trad
  nlong = 40
  nlat = 20
  degrad = !dpi/180d0
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(0) Set up a grid of times, initial longitudes, and latitudes.'
  t = 3*norbit_trad * DINDGEN(ntime) / (ntime-1d) * PORBIT
  dt = t[1]-t[0]
  ;;; Longitude of each parcel is time dependent.  It takes values from 0 to 2*pi, with 0 measured from the substellar point 
  longitude_parcel = MAKE_ARRAY(ntime,nlong,VALUE=0d0)
  dum = 2*!dpi * DINDGEN(nlong+1)/FLOAT(nlong)  ;; Initial values
  longitude_parcel[0,*] = dum[0:nlong-1]
  dlong = longitude_parcel[0,1]-longitude_parcel[0,0]
  ;;; Latitude is measured from the North Pole (0) to the equator (pi/2), i.e., it's really CO-latitude.    
  latitude = 0.5 * !dpi * DINDGEN(nlat)/19.
  dlat = latitude[1]-latitude[0]

  Temp = MAKE_ARRAY(ntime,nlong,nlat,VALUE=0d0)
  Temp_norm = MAKE_ARRAY(ntime,nlong,VALUE=0d0)
  IF KEYWORD_SET(PLOT) THEN BEGIN
     psfile='exoplanet_phase_curve.eps'
     ifps,psfile,7,9,/ENCAP
     multiplot_jim,[1,6],/initialize
  ENDIF
  
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(1) Compute the orbit for each timestep from Kepler''s laws'
  ELLIPTICAL_ORBIT,t,porbit,e,r,theta
  omorb = 2 * !dpi * ar_semimaj^2 * SQRT(1d0-e^2) / Porbit  ;; radians per day
  omorb_peri =  (1d0 - e)^2/r^2 ;;; orbital angular velocity at each timestep, in units of the periastron (maximum) value
  omperi = 2*!dpi * SQRT(1d0+e) / (Porbit * (1d0-e)^1.5)  ;; Periastron angular velocity, radians per day
  prot = 2*!dpi / (omrot*omperi) ;;; rotational period of planet, days
  IF KEYWORD_SET(VERBOSE) THEN BEGIN
      PRINT,'Orbital distance ranges from '+STRJOIN(STRNG(JI_MINMAX(r*a_au)),' to ')+' AU'
      PRINT,'Angular Velocity at Periastron: '+STRNG(omperi)+' rad/day, or '+STRNG(omperi*porbit/(2*!dpi))+' revolutions per orbit'
  ENDIF
  IF KEYWORD_SET(TIDALLY_LOCKED) THEN omrot = 1.0
  
  
  IF KEYWORD_SET(PLOT) THEN BEGIN
     multiplot_jim
     cgplot,t*dysec,r*a_au,ytitle='r (AU)',yrange=[0,0.3],ystyle=1,xstyle=1,background='WHITE'
     multiplot_jim
     cgplot,t*dysec,omorb_peri,ytitle='!4w!3!dorb!n/!4w!3!dperi!n',yrange=[0,1],ystyle=1,xstyle=1
  ENDIF
  
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(2) Set the initial temperature value for all points on the planet''s surface to T0(latitude)'
  T0 = Teff_Star * ( (1d0-Albedo) * SIN(latitude) )^(0.25) * SQRT(1./(ar_semimaj * (1d0-e)))
  T0_nolat = Teff_Star * ( (1d0-Albedo) )^(0.25) * SQRT(1./(ar_semimaj * (1d0-e)))
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'Equilibrium temperature at the substellar point, T0: '+STRNG(MAX(T0))+' K'
  
  FOR i = 0,nlong-1 DO BEGIN
    Temp[0,i,*]  = T0
  ENDFOR
  Temp_norm[0,*] = 1d0  ;;; Temp_norm is temperature at the equator normalized by T0
  tnorm_zero = temp_norm  ;;; trad=0
  tnorm_deq = temp_norm  ;;; differential equation solution
  tnorm_long = temp_norm  ;;; limit of complete longitudinal recirculation
  tnorm_lat  = temp_norm  ;;; limit of complete latitudinal (and longitudinal) recirculation
  
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(3) For each timestep, each parcel gets assigned a new value of longitude based on the orbital and rotational angular velocity'
  omega_tot = omrot - omorb_peri; (omorb_peri + SHIFT(omorb_peri,1) + SHIFT(omorb_peri,-1))/3d0  ;; Total angular velocity at each timestep, in units of the periastron value
  FOR i = 1,ntime-1 DO longitude_parcel[i,*] = longitude_parcel[i-1,*] + omega_tot[i] * omperi * dt  ;; Longitude is zero at the substellar point
  
  IF KEYWORD_SET(PLOT) THEN BEGIN
     multiplot_jim
     cgplot,t*dysec,longitude_parcel[*,0]/(2*!dpi) mod 1,ytitle='!4F!3!dparcel!n/(2!4p!3)',yrange=[0,1],ystyle=1,xstyle=1
  ENDIF
  
  ;; Normalized intensity of stellar radiation experienced by each parcel at each time
  inorm = MAKE_ARRAY(ntime,nlong,VALUE=0d0)
  FOR j = 0,nlong-1 DO inorm[*,j] = ( (1d0-e)/r )^2 * (COS(longitude_parcel[*,j]) > 0)
  
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(4) Integrate Temp_norm for each parcel on the equator as a function of time. ' 
    ;;; There are 4 "versions" of the temperature, given by either limits or the differential equation solution
  ;; (1) trad=0 value
  tnorm_zero = (inorm)^0.25  ;; The temperature is the equilibrium value
  ;; (2) differential equation solution
  ;;; Only solve for the differential equation if the radiative timescale is in the range 0 to 3*Prot
  IF trad GT 0 AND trad LE 3*prot THEN BEGIN
     time_norm = t/trad  ;; Normalized time is in units of radiative timescale
     dtnorm = time_norm[1]-time_norm[0]
    ;;;  (Need to deal with dusk analytical solution later. For now just use numerical solution.
     IF KEYWORD_SET(VERBOSE) THEN tic
     FOR j = 0,nlong-1 DO BEGIN
        FOR i = 1,ntime-1 DO BEGIN
           lpnorm = longitude_parcel[i,j]/(2*!dpi) mod 1
           IF  lpnorm GT 0.25 and lpnorm LE 0.75 THEN inorm_ij = 0d0 ELSE inorm_ij = inorm[i,j]
        ;        IF inorm_ij NE 0 THEN BEGIN
        ;;; If the constant term is nonzero, we are on the day side, and the ODE needs to be solved numerically
           temp_prev = [tnorm_deq[i-1,j]]
           tnorm_deq[i,j] = LSODE(temp_prev,time_norm[i],dtnorm,'exoplanet_phase_temp_ode')
        ;       ENDIF ELSE BEGIN
        ;;; If the constant term is zero, then we are on the night side and the result can be solved analytically
        ENDFOR
     ENDFOR
     IF KEYWORD_SET(VERBOSE) THEN toc
  ENDIF
  ;;; (3) Complete longitudinal recirculation
  FOR j = 0,nlong-1 DO tnorm_long[*,j] = SQRT( (1d0 - e)/r ) / (!dpi^(0.25))
  ;;; (4) Complete longitudinal and latidudinal recirculation
  FOR i = 0,ntime-1 DO BEGIN
    FOR j = 0,nlong-1 DO tnorm_lat[i,j] = SQRT( (1d0 - e)/2d0 ) * (1d0 - e^2)^(-0.125) 
  ENDFOR
  ;;; Compute weights for each of the solutions
  wt = MAKE_ARRAY(4,VALUE=0d0)
  CASE 1 OF
     trad EQ 0: wt[0] = 1 
     trad GT 0 AND trad LE 0.1*prot: BEGIN
        wt[1] = trad/(0.1*prot)
        wt[0] = 1-wt[1]
     END
     trad GT 0.1*prot AND trad LE prot: wt[1] = 1
     trad GT prot AND trad LE 3*prot: BEGIN
        wt[2] = (trad-prot)/(2*prot)
        wt[1] = 1-wt[2]
     END
     trad GT 3*prot AND trad LT 3*porbit: BEGIN
        wt[3] = (trad-3*prot)/(3*porbit - 3*prot)
        wt[2] = 1-wt[3]
     END 
     ELSE: wt[3] = 1
  ENDCASE
  ;print,[0,0.1*prot,prot,3*prot,3*porbit]
  ;print,wt
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(5) Compute Temp for all latitudes by multiplying normalized temps by T0'
  FOR k = 0,nlat-1 DO BEGIN
    ;;; Combine the solutions together, with the various weights
    Temp[*,*,k] = (wt[0] * tnorm_zero + wt[1]*tnorm_deq + wt[2]*tnorm_long) * T0[k] + $
                  wt[3]*tnorm_lat * T0_nolat
  ENDFOR
  temp_substellar = MAKE_ARRAY(ntime,VALUE=0d0) ;;; Temp of the substellar point
  temp_max = MAKE_ARRAY(ntime,VALUE=0d0)
  long_temp_max = MAKE_ARRAY(ntime,VALUE=0.d0)
  FOR i = 0,ntime-1 DO BEGIN
     dum = MIN(longitude_parcel[i,*]/(2*!dpi) MOD 1,/ABSOLUTE,isubstellar,/NAN)
     temp_substellar[i] = temp[i,isubstellar,nlat-1]
     temp_max[i] = MAX(temp[i,*,nlat-1],imax,/NAN)
     long_temp_max[i] = 2.*!dpi * (longitude_parcel[i,imax]/(2.*!dpi) mod 1)
  ENDFOR

  IF KEYWORD_SET(PLOT) THEN BEGIN
     multiplot_jim
     cgplot,t*dysec,temp_substellar,ytitle='T (K)',yrange=[0,2500],ystyle=1,xstyle=1
     cgplot,t*dysec,temp_max,LINESTYLE=2,/OVERPLOT
  ENDIF

  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(6) Determine the sub-observer longitude as a function of time'
  longitude_subobs = 3*!dpi/2.0 - theta * degrad - argperi * degrad  
  
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(7) Compute the visibility of each parcel at each timestep'
  visibility = MAKE_ARRAY(ntime,nlong,nlat,VALUE=0D0)
  domega = MAKE_ARRAY(ntime,nlong,nlat,VALUE=0D0)  ;; Differential solid angle for integrating over the surface of a sphere, a function of colatitude.
  FOR k = 0,nlat-1 DO BEGIN 
    FOR j = 0,nlong-1 DO BEGIN
       visibility[*,j,k] = SIN(latitude[k]) * ( COS(longitude_parcel[*,j] - longitude_subobs) > 0 )
    ENDFOR
    domega[*,*,k] = SIN(latitude[k]) * dlong * dlat
  ENDFOR  
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(8) Compute the observed planet flux divided by stellar flux as a function of time'
  premult = (EXP(t_hnu / teff_star) - 1D) / !dpi * rp_rstar^2
  fp_fstar = MAKE_ARRAY(ntime,VALUE=premult)
  fp_fstar *=  TOTAL( 2*TOTAL( visibility * domega / (EXP(t_hnu / temp) - 1d), 3 ), 2 )  ; count each latitude twice
    
  IF KEYWORD_SET(PLOT) THEN BEGIN
     multiplot_jim
     cgplot,t*dysec,fp_fstar,ytitle='F!dp!n/F!d*!n',ystyle=1,xstyle=1,yrange=[0,0.0012]
  ENDIF
  
  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(9) Convert this to a phase curve by expressing theta as orbital phase and saving only the last 1 orbit''s worth of values'
  ineg = WHERE(theta LT 0,nneg)
  IF nneg NE 0 THEN theta[ineg] += 360.d0
  raw_phase = (theta + argperi)*degrad - !dpi/2.  ;; number of radians, phased to the line of sight.  This is wrapped, since Theta is wrapped
  raw_phase_norm = raw_phase/(2*!dpi)  ;;; number of orbits, phased to the line of sight, from periastron

  IF KEYWORD_SET(VERBOSE) THEN PRINT,'(10) Compute stellar and planetary obstruction as a function of tperi, and calculate light curve'
  EXOPLANET_TRANSIT_ECLIPSE,raw_phase,ar_semimaj,r,rp_rstar,inclination,star_frac,planet_frac,itransit_raw,ieclipse_raw,LIMB_DARKENING=limb_darkening
  relative_flux = star_frac + planet_frac * fp_fstar
  
  IF KEYWORD_SET(PLOT) THEN BEGIN
     multiplot_jim
     !x.tickname = ''
     cgplot,t*dysec,relative_flux,ytitle='F!dTOT!n/F!d*!n',xtitle='time (sec)',ystyle=1,xstyle=1
     multiplot_jim,/reset
     endps,psfile,/pdf
     !p.multi = 0
     !p.position = 0
  ENDIF
  
  
  ikeep = [ntime-1000:ntime-1] ;; keep only the last 1 orbit's worth
  isort = SORT(raw_phase_norm[ikeep] MOD 1)  ; MOD 1 wraps around unity, SORT ensures that it is re-wrapped, with the first phase close to 0
;  ikeep_sort = ikeep;[isort]
  tperi = porbit * ((t[ikeep] - (norbit_trad*3-1)*porbit)/porbit mod 1) 
  isort = SORT(tperi)
  ikeep_sort = ikeep[isort]
  tperi=tperi[isort]
  phase = raw_phase[ikeep_sort]
  r = r[ikeep_sort]
  theta = theta[ikeep_sort]
  omorb_peri = omorb_peri[ikeep_sort]
  ;;; Unobstructed planet/star flux curve
  fp_fstar = fp_fstar[ikeep_sort]
  relative_flux = relative_flux[ikeep_sort]
  temp_max = temp_max[ikeep_sort]
  temp_substellar=temp_substellar[ikeep_sort]
  long_temp_max = long_temp_max[ikeep_sort]
  dum = ji_minmax(fp_fstar,subscript_min=imin,subscript_max=imax)
  min1 = imin-15 > 0
  min2 = imin+15 < (N_ELEMENTS(tperi)-1)
  max1 = imax-15 > 0
  max2 = imax+15 < (N_ELEMENTS(tperi)-1)
  ;; Find the min and max of the phase curve, as close as possible
  dfdt = DERIV(tperi[min1:min2],fp_fstar[min1:min2])
  ;; Find the time of minimum
  t_min = INTERPOL(tperi[min1:min2],dfdt,0.0,/NAN)
  IF FINITE(t_min,/NAN) THEN t_min = tperi[imin]
  ;; Find the minimum value
  f_min = INTERPOL(fp_fstar[min1:min2],tperi[min1:min2],t_min,/NAN)
  dfdt = DERIV(tperi[max1:max2],fp_fstar[max1:max2])
  ;; Find the time of maximum
  t_max = INTERPOL(tperi[max1:max2],dfdt,0.0,/NAN)
  IF FINITE(t_max,/NAN) THEN t_max = tperi[imax]
  ;; Find the maximum value
  f_max = INTERPOL(fp_fstar[max1:max2],tperi[max1:max2],t_max,/NAN)
  amplitude=f_max-f_min
    

;;; Keep only last 1 orbit's worth of transit and eclipse
  transit_depth=!null
  eclipse_depth=!null
  itransit = !null
  ieclipse = !null
  IF N_ELEMENTS(itransit_RAW) NE 0 THEN BEGIN
     ikeep_transit = WHERE(itransit_raw ge (ntime-1000),nkeep_transit)
     IF nkeep_transit NE 0 THEN BEGIN
        itransit0 = itransit_raw[ikeep_transit]-(ntime-1000)
        dum = MAX(COS(phase[itransit0]),imid)
        itransit = itransit0[imid]
        transit_depth = 1d - relative_flux[itransit] / (1d + fp_fstar[itransit])
     ENDIF
;     transit_depth = 1d - relative_flux[itransit] / fp_fstar[itransit]
  ENDIF 
  IF N_ELEMENTS(ieclipse_raw) NE 0 THEN BEGIN
     ikeep_eclipse = WHERE(ieclipse_raw ge (ntime-1000),nkeep_eclipse)
     IF nkeep_eclipse NE 0 THEN BEGIN
        ieclipse0 = ieclipse_raw[ikeep_eclipse]-(ntime-1000)
        dum = MIN(COS(phase[ieclipse0]),imid)
        ieclipse = ieclipse0[imid]
;     eclipse_depth = 1d - relative_flux[ieclipse] / (1d + fp_fstar[ieclipse])
        eclipse_depth = fp_fstar[ieclipse]
     ENDIF
  ENDIF 
  theta_transit = 2.*!dpi * ( (!dpi/2. - argperi*degrad)/(2.*!dpi) mod 1 )    ;; Angle between periastron and transit
  IF theta_transit LT 0 THEN theta_transit += 2.*!dpi
  st = sort(theta)
  tperi_transit = INTERPOL(tperi[st],theta[st]*degrad,theta_transit,/NAN)
  phase_fraction = phaseify(tperi,tperi_transit,porbit)
  phase_max = phaseify(t_max,tperi_transit,porbit)
  phase_min = phaseify(t_min,tperi_transit,porbit)

  IF KEYWORD_SET(VERBOSE) THEN PRINT,'Done'
  
RETURN
END
