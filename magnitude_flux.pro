FUNCTION magnitude_flux,magval,magsystemfrom,teff,TOMAG=tomag,LAMBDA_EFF_TO=lambda_eff_to,MAGSYSTEMTO=magsystemto
;;
;; Convert from magnitudes in one system to flux density in mJy (or magnitudes if /TOMAG is set) in another system.
;; Use a crude blackbody ratio.  Requires knowledge of the effective temperature. 
;;
;; INPUTS:  MAGVAL - the magnitude measured in a given system
;;          MAGSYSTEMFROM - the filter system used for the measured magnitude.  
;;                      Possibilities are: 'U','B','V','R','I','J','H','K','L','M','N','O','J2MASS','H2MASS',
;;                      'Ks2MASS','IRAC3.6','IRAC4.5','IRAC5.7','IRAC8.0','g','r','i','z'
;;          TEFF - the effective temperature of the source (Kelvins)
;;
;; OPTIONAL KEYWORDS:
;;          MAGSYSTEMTO - the filter system to convert to (same possible values as MAGSYSTEMFROM) - default is 'V' 
;;          LAMBDA_EFF_TO - alternately the effective wavelength (microns) of the filter to convert to (overrides MAGSYSTEMTO)
;;          TOMAG - give output in magnitudes.  Default is to output in mJy.  Will not give reasonable results if don't specify
;;                  MAGSYSTEMTO.
;; 
h = 6.626068d-27
kb = 1.3806503d-16
c = 2.99792458d10

Lambda_eff = HASH('U',0.36,'B',0.44,'V',0.55,'R',0.71,'I',0.97,'J',1.25,'H',1.60,'K',2.22,'L',3.54,'M',4.80,'N',10.6,'O',21.0,$
                  'J2MASS',1.235,'H2MASS',1.662,'Ks2MASS',2.159,'IRAC3.6',3.5612,'IRAC4.5',4.5095,'IRAC5.7',5.6895,'IRAC8.0',7.9584,$
                   'g',0.52,'r',0.67,'i',0.79,'z',0.91,'WISE1',3.4,'WISE2',4.6,'WISE3',12.,'WISE4',22.)  ;;  microns
zeropt = HASH('U',1823d3,'B',4130d3,'V',3781d3,'R',2941d3,'I',2635d3,'J',1603d3,'H',1075d3,'K',667d3,'L',288d3,'M',170d3,'N',36d3,'O',9.4d3,$
              'J2MASS',1594d3,'H2MASS',1024d3,'Ks2MASS',666.7d3,'IRAC3.6',278d3,'IRAC4.5',180d3,'IRAC5.7',117d3,'IRAC8.0',63.1d3,$
              'g',3730d3,'r',4490d3,'i',4760d3,'z',4810d3,'WISE1',309.54d3,'WISE2',171.79d3,'WISE3',31.676d3,'WISE4',8.3635d3)  ;; mJy
              
IF ~lambda_eff.HasKey(magsystemfrom) THEN BEGIN
   PRINT,'MAGNITUDE_FLUX: No information on magnitude system '+magstystemfrom+'.  Returning.'
   RETURN,!values.F_NAN
ENDIF

IF N_ELEMENTS(lambda_eff_to) EQ 0 AND ~lambda_eff.HasKey(magsystemto) THEN BEGIN
   PRINT,'MAGNITUDE_FLUX: No information on magnitude system '+magstystemto+'.  Returning.'
   RETURN,!values.F_NAN
ENDIF

IF N_ELEMENTS(magsystemto) EQ 0 THEN magsystemto = 'V'
zeropt_to = zeropt[magsystemto]

lambda_from = lambda_eff[magsystemfrom]
zeropt_from = zeropt[magsystemfrom]
IF N_ELEMENTS(lambda_eff_to) EQ 0 THEN lambda_eff_to = lambda_eff[magsystemto] 

F_TO = zeropt_from * 10d0^(-magval/2.5d0) * (lambda_from/lambda_eff_to)^3 * $
       ( exp(h*c/(lambda_from * 1d-4 * kb * teff)) - 1d0 ) /  ( exp(h*c/(lambda_eff_to * 1d-4 * kb * teff)) - 1d0 )
       
IF KEYWORD_SET(TOMAG) THEN BEGIN
   MAG_TO = -2.5 * ALOG10(F_TO / zeropt_to)
   RETURN,MAG_TO
ENDIF ELSE RETURN,F_TO

END       