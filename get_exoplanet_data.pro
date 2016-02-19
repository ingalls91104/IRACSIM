PRO get_exoplanet_data,EXOSYSTEM=exosystem,MSINI=msini,MSTAR=mstar,RSTAR=rstar,TRANSIT_DEPTH=transit_depth,RP_RSTAR=rp_rstar,AR_SEMIMAJ=ar_semimaj,$
                       TEQ_P=teq_p,TEFF_STAR=teff_star,SECONDARY_DEPTH=secondary_depth,SECONDARY_LAMBDA=secondary_lambda,OM=om,$
                       INCLINATION=inclination,MJD_TRANSIT=mjd_transit,P_ORBIT=p_orbit,EXODATA=exodata,RA=ra,DEC=dec,VMAG=vmag,$
                       DISTANCE=distance,ECC=ecc,T14=t14,F36=f36,F45=f45,FP_FSTAR0=fp_fstar0,LIST_NAMES=list_names,VERBOSE=verbose,$
                       TRANSIT_SUMMARY=transit_summary,INFO_STRING=info_string
;;  EXOSYSTEM - the name of the exoplanet system, a string found in the exoplanets.org database (eg., "WASP-46 b").  
;;              If this is given then the other keywords can be passed as outputs.  Otherwise they will all have to be input
;;              to specify the parameters of the orbits.
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
;;  TEQ_P - Equilibrium temperature of the planet, Kelvins
;;  TEFF_STAR - Effective temperature of star, Kelvins
;;  SECONDARY_DEPTH - Depth of secondary transit.  If given, then TEQ_P and TEFF_STAR will be ignored.
;;  SECONDARY_LAMBDA - band at which the secondary depth was measured, or is desired.  Values can be 
;;                     'J', 'H', 'KS', 'KP', '3.6', '4.5', '5.8', '8.0'
;;  INCLINATION - orbital inclination (measured from axis of revolution), degrees
;;  MJD_TRANSIT - Modified Julian date of mid-transit (JD-2400000) (days)
;;  P_ORBIT - orbital period (days).  Does not have to be input if AR_SEMIMAJ is input.
;;  EXODATA - The exoplanet database structure, returned from a previous run
;;  LIST_NAMES - print all exoplanet names that satisfy the input wildcard (eg. LIST_NAMES='HD*')
;;  /TRANSIT_SUMMARY - print a summary of the database for planets with confirmed transits 
;;                    Sort planets by Orbital Period and Transit Depth 
;;  /VERBOSE - print info on progress
;;  INFO_STRING - returns a string array with all info printed to screen under /VERBOSE
;;  
;; OUTPUT KEYWORDS (output when EXOSYSTEM is input)
;;  RA - Right Ascension string (decimal hr, J2000, epoch 2000, from database)
;;  DEC - Declination string (decimal degrees, J2000, epoch 2000, from database)
;;  VMAG - V magnitude of the host star
;;  DISTANCE - distance of system, parsecs
;;  ECC - orbital eccentricity
;;  OM - argument of periastron, degrees
;;  T14 - time between first and fourth contacts (total transit time)
;;  F36 - Estimated 3.6 micron flux density (mJy) of star, extrapolated from VMAG and TEFF_STAR
;;  F45 - Estimated 4.5 micron flux density (mJy) of star, extrapolated from VMAG and TEFF_STAR
;;  FP_FSTAR0 - Planet to star flux ratio at the substellar point (i think).  This is either the secondary depth 
;;              (and is purely observational), or is derived from an estimated planetary equilibrium
;;              temperature (which depends on assumptions about the properties of the planet)
;;  EXODATA - Passes back the exoplanet database as a structure
;;  
;;;
;;;; Systems known to have both transits and secondary eclipses
;WASP-1 b,WASP-18 b,WASP-33 b,XO-3 b,WASP-12 b,CoRoT-1 b,XO-2 b,55 Cnc e,HD 80606 b,WASP-19 b,
;OGLE-TR-113 b,GJ 436 b,WASP-14 b,WASP-24 b,WASP-17 b,XO-1 b,HD 149026 b,TrES-3 b,TrES-4 b,WASP-3 b,
;TrES-1 b,Kepler-12 b,TrES-2 b,KOI-13 b,Kepler-7 b,CoRoT-2 b,HAT-P-7 b,Kepler-6 b,Kepler-17 b,
;Kepler-5 b,HD 189733 b,WASP-2 b,HD 209458 b,HAT-P-8 b,HAT-P-1 b,WASP-4 b,HAT-P-6 b,XO-4 b
INFO_STRING = !null
CASE 1 OF 
   N_ELEMENTS(LIST_NAMES) NE 0: BEGIN
     IF ~N_ELEMENTS(exodata) THEN BEGIN
       exoplanet_get_database,exoplanet_data_file
       exodata = READ_CSV_JIM(exoplanet_data_file,count=count)
     ENDIF
     inames = WHERE(STRMATCH(exodata.name,list_names),nnames)
     IF nnames NE 0 THEN PRINT,exodata.NAME[inames]
   END
   KEYWORD_SET(TRANSIT_SUMMARY): BEGIN
      IF ~N_ELEMENTS(exodata) THEN BEGIN
         exoplanet_get_database,exoplanet_data_file
         exodata = READ_CSV_JIM(exoplanet_data_file,count=count)
      ENDIF
      itransit = WHERE(exodata.TRANSIT,ntransit)
      period_range = [ [0,1],[1,2],[2,3],[3,4],[4,8],[8,16],[16,32],[32,64] ]
      szp = SIZE(period_range)
      nper = szp[2]
      ppm_range = [ [-1,50],[50,100],[100,200],[200,400],[400,800],[800,1600],[1600,5e6] ]
      szpp = SIZE(ppm_range)
      nppm = szpp[2]
      FOR i = 0,nper-1 DO BEGIN
        PRINT,STRNG(period_range[0,i])+'dy < P < '+STRNG(period_range[1,i])+'dy :'
        
        irange = WHERE(exodata.PER[itransit] GE period_range[0,i] AND exodata.PER[itransit] LT period_range[1,i] $
                       AND exodata.DEPTH[itransit] GT 0 AND exodata.DEPTH[itransit] LT 1,nrange)
        IF nrange NE 0 THEN BEGIN
            names = exodata.NAME[itransit[irange]]
            periods = exodata.PER[itransit[irange]]
            depths = exodata.DEPTH[itransit[irange]]
            known_secondary = exodata.SE[itransit[irange]]
            secondary = STRARR(nrange) 
            isec = WHERE(known_secondary,nsec)
            IF nsec NE 0 THEN secondary[isec] = 'SEC'
            vmag = exodata.V[itransit[irange]]
            teff_star = exodata.teff[itransit[irange]]
            F45 = MAGNITUDE_FLUX(VMAG,'V',TEFF_STAR,MAGSYSTEMTO='IRAC4.5')
            
            is = SORT(periods)
            
            PRINT,NAMES[is]+'(P:'+STRNG(periods[is],FORMAT='(f6.2)')+'d;TD:'+STRNG(depths[is]*1e6,FORMAT='(f8.2)')+'ppm;f4.5:'+STR_EXP(f45[is],3)+'mJy;'+secondary[is]+')'
          
        ENDIF
        PRINT,''
        
      ENDFOR
      
   END
   ELSE: BEGIN
      IF N_ELEMENTS(SECONDARY_LAMBDA) EQ 0 THEN SKIP_SECONDARY=1 ELSE SKIP_SECONDARY=0
      h = 6.626068d-27
      kb = 1.3806503d-16
      c = 2.99792458d10

      LAMBDA_HASH = HASH('J',1.25,'H',1.65,'KS',2.15,'KP',0.6,'3.6',3.6,'4.5',4.5,'5.8',5.8,'8.0',8.0)
      IF keyword_set(EXOSYSTEM) THEN BEGIN
         IF ~N_ELEMENTS(exodata) THEN BEGIN
            exoplanet_get_database,exoplanet_data_file
            exodata = READ_CSV_JIM(exoplanet_data_file,count=count)
         ENDIF
         iuse = WHERE(exodata.NAME EQ EXOSYSTEM,nuse)
         IF NUSE EQ 0 THEN BEGIN
            print,'GET_EXOPLANET_DATA: Cannot find "'+exosystem+'" .  Returning.'
            return
         ENDIF
         MSINI = exodata.MSINI[iuse[0]]
         MSTAR = exodata.MSTAR[iuse[0]]
         RA = exodata.RA[iuse[0]] * 15.
         DEC = exodata.DEC[iuse[0]]
         P_ORBIT = exodata.PER[iuse[0]]
         VMAG = exodata.V[iuse[0]]
         DISTANCE = exodata.DIST[iuse[0]]
         ECC = exodata.ECC[iuse[0]]
         OM = exodata.OM[iuse[0]]
         IF ECC GT 0.1 THEN PRINT,'WARNING: exoplanetary orbital eccentricity is '+STRNG(ecc)+', so circular approximation might not be valid.'
         a = exodata.A[iuse[0]]
         AR_SEMIMAJ = exodata.AR[iuse[0]]
         RSTAR = exodata.RSTAR[iuse[0]]
         TEFF_STAR = exodata.TEFF[iuse[0]]
         F36 = MAGNITUDE_FLUX(VMAG,'V',TEFF_STAR,MAGSYSTEMTO='IRAC3.6')
         F45 = MAGNITUDE_FLUX(VMAG,'V',TEFF_STAR,MAGSYSTEMTO='IRAC4.5')
         known_transit = exodata.TRANSIT[iuse[0]]
         INFO_STRING = [INFO_STRING,'System '+exosystem]
         INFO_STRING = [INFO_STRING,'STELLAR PARAMS (from DB): VMAG: '+STRNG(vmag)+'; M:'+STRNG(mstar)+'(Msun);  R:'+STRNG(rstar)+'(Rsun);;',$
                                    '                          Teff:'+STRNG(teff_star)+'(K); D:'+STRNG(distance)+'(pc)']
         INFO_STRING = [INFO_STRING,'ORBITAL PARAMS (from DB): P: '+STRNG(p_orbit)+'(dy); Msini:'+STRNG(msini)+'(MEarth);  e:'+STRNG(ecc)+';',$
                                    '                          a:'+STRNG(ar_semimaj)+'(Rstar); OM:'+STRNG(om)]
         IF known_transit THEN BEGIN
            TRANSIT_DEPTH = exodata.DEPTH[iuse[0]]
            RP_RSTAR = SQRT(transit_depth)
            t14 = exodata.T14[iuse[0]]
            MJD_TRANSIT = exodata.TT[iuse[0]]-2400000.5D0
            b_impact = exodata.B[iuse[0]]
            inclination = exodata.I[iuse[0]]
            INFO_STRING = [INFO_STRING,'TRANSIT_PARAMS (from DB): Depth: '+STRNG(transit_depth)+'; Rp:'+STRNG(rp_rstar)+'(Rstar);',$
                                       '                          T14:'+STRNG(t14)+'(dy); MJD (transit):'+STRNG(mjd_transit)+'(dy); i:'+STRNG(inclination)+'(deg)']
         ENDIF ELSE BEGIN
            INFO_STRING = [INFO_STRING,'No observed transit for this target.']
            b_impact = ar_semimaj * cos(inclination/!radeg)
            IF KEYWORD_SET(transit_DEPTH) THEN RP_RSTAR=SQRT(transit_depth)
            t14 = P_ORBIT/!dpi * ACOS(SQRT(1 - (1+rp_rstar)^2/ar_semimaj^2)/SIN(inclination/!radeg))
         ENDELSE
         IF ~SKIP_SECONDARY THEN BEGIN
            known_secondary = exodata.SE[iuse[0]]
            IF known_secondary THEN BEGIN
               SECONDARY_DEPTH_HASH = HASH('J', exodata.SEDEPTHJ[iuse[0]]) ;; 1.25 micron
               SECONDARY_DEPTH_HASH['H']  = exodata.SEDEPTHH[iuse[0]] ;; 1.65 micron
               SECONDARY_DEPTH_HASH['KS'] = exodata.SEDEPTHKS[iuse[0]] ;; Ks band, 2.15 micron
               SECONDARY_DEPTH_HASH['KP'] = exodata.SEDEPTHKP[iuse[0]] ;; Kepler 400-865 nm
               SECONDARY_DEPTH_HASH['3.6'] = exodata.SEDEPTH36[iuse[0]] ;; 3.6 micron
               SECONDARY_DEPTH_HASH['4.5'] = exodata.SEDEPTH45[iuse[0]] ;; 4.5 micron
               SECONDARY_DEPTH_HASH['5.8'] = exodata.SEDEPTH58[iuse[0]] ;; 5.8 micron
               SECONDARY_DEPTH_HASH['8.0'] = exodata.SEDEPTH80[iuse[0]] ;; 8.0 micron
               IF SECONDARY_DEPTH_HASH[SECONDARY_LAMBDA] EQ 0 THEN BEGIN
                  PRINT,'No observed secondary in the '+secondary_lambda+' band.  Using the input planetary temperature '+STRNG(teq_p)+' K.'
                  IF N_ELEMENTS(teq_p) EQ 0 THEN BEGIN
                     PRINT,'No planet equilibrium temperature input.  Returning'
                     RETURN
                  ENDIF
                  INFO_STRING = [INFO_STRING,'No observed secondary in the '+secondary_lambda+' band.  Using the input planetary temperature '+STRNG(teq_p)+' K.']
               ENDIF ELSE BEGIN
                  lambda = lambda_hash[secondary_lambda]
                  secondary_depth = secondary_depth_hash[secondary_lambda]
                  teq_p = (h*c/(lambda*1d-4*kb)) / ALOG( rp_rstar^2 * (exp(h*c/(lambda*1d-4*kb*teff_star)) - 1D0)/(secondary_depth) + 1 )
               ENDELSE
            ENDIF ELSE BEGIN
               INFO_STRING = [INFO_STRING,'No observed secondary eclipse.  Using the input planetary temperature '+STRNG(teq_p)+' K.']
               PRINT,'No known secondary eclipse.  Using the input planetary temperature.'
               lambda = lambda_hash[secondary_lambda]
            ENDELSE 
         ENDIF
      ENDIF ELSE BEGIN
         IF KEYWORD_SET(transit_DEPTH) THEN RP_RSTAR=SQRT(transit_depth) ELSE transit_depth = RP_RSTAR^2
         b_impact = ar_semimaj * cos(inclination/!radeg)
         IF b_impact GT 1+rp_rstar THEN known_transit = 0  ELSE known_transit=1
         IF N_ELEMENTS(P_ORBIT) EQ 0 THEN P_ORBIT = 0.11577d0 * SQRT( (ar_semimaj * rstar)^3/mstar ) 
         IF ~known_transit then t14 = 0 ELSE BEGIN
            t14 = P_ORBIT/!dpi * ACOS(SQRT(1 - (1+rp_rstar)^2/ar_semimaj^2)/SIN(inclination/!radeg))
            IF N_ELEMENTS(MJD_TRANSIT) EQ 0 THEN mjd_transit = systime(/julian) - 2400000.5D0  ;; set transit time to now
            INFO_STRING = [INFO_STRING,'TRANSIT_PARAMS (Input): Depth: '+STRNG(transit_depth)+'; Rp:'+STRNG(rp_rstar)+'(Rstar);',$
                                       '                        T14:'+STRNG(t14)+'(dy); MJD (transit):'+STRNG(mjd_transit)+'(dy); i:'+STRNG(inclination)+'(deg)']
         ENDELSE
         IF N_ELEMENTS(ecc) EQ 0 THEN ecc = 0
         IF N_ELEMENTS(om) EQ 0 THEN om = 90
         INFO_STRING = [INFO_STRING,'ORBITAL PARAMS (Input): P: '+STRNG(p_orbit)+'(dy); Msini:'+STRNG(msini)+'(MEarth);  e:'+STRNG(ecc)+';',$
                                    '                        a:'+STRNG(ar_semimaj)+'(Rstar); om:'+STRING(OM)]
      ENDELSE
      IF ~SKIP_SECONDARY THEN BEGIN
         lambda = lambda_hash[secondary_lambda]
         fp_fstar0 = (exp(h*c/(lambda * 1e-4 * kb * teff_star)) - 1d0) / (exp(h*c/(lambda * 1e-4 * kb * teq_p)) - 1d0) * rp_rstar^2
         INFO_STRING = [INFO_STRING,'FP/F* (eclipse depth) (Input): '+STRNG(fp_fstar0)]
      ENDIF
   END
ENDCASE
IF KEYWORD_SET(VERBOSE) THEN PRINT,INFO_STRING
RETURN
END

