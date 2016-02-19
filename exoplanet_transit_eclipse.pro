PRO ETE_LIMB,theta_phase,z,p,limb_darkening,lambda_star,itransit
;;
;; Compute the transit portion of a light curve for the case of nonzero limb darkening
;;
;; INPUTS:  
;;       THETA_PHASE   = Array of true anomaly values, phased to the transit, modulo 2*!dpi (radians).  In a circular
;;                       orbit, this is the observed orbital phase converted to radians.  In an elliptical orbit this is
;;                       the true anomaly minus the value of the anomaly at transit.
;;                    Z = Distance between centers of planet and star, in units of the stellar radius, as a function
;;                        of THETA_PHASE
;;                    P = Planet radius, in units of the stellar radius
;;
;;       LIMB_DARKENING = The set of stellar limb darkening coefficients to use to compute the transit profile.
;;                           See Claret (2000) for a description of the model, and Mandel & Agol (2002) for the
;;                           implications for transit profiles.
;;  OUTPUTS:
;;          LAMBDA_STAR = Amount of total stellar flux that is obscured by the planet, as a function of THETA_PHASE
;;          ITRANSIT = index of THETA_PHASE array during which planet is in full transit (!null if no transit)
;;

;;  Boolean for all situations when the planet is "in front of" the star relative to the earth
front = (theta_phase GT 1.5*!dpi OR theta_phase LE 0.5*!dpi)

;; Set up some variables
itransit = !null
ndat = N_ELEMENTS(THETA_PHASE)
lambda_star = MAKE_ARRAY(SIZE=SIZE(THETA_PHASE))
c = [1d - TOTAL(limb_darkening,/DOUBLE),limb_darkening]
omega = TOTAL(c/(DINDGEN(5)+4d),/DOUBLE)
c1 = DBLARR(5,ndat)
FOR i = 0,4 DO c1[i,*] = c[i]/(i+4d)
a = (z-p)^2
b = (z+p)^2

;;  There are 10 transit cases to cover for the nonlinear limb darkening situation (see Mandel & Agol (2002))
i1 = WHERE( (z GT (1+P)) OR ~front,n1,COMPLEMENT=itransit0,NCOMPLEMENT=ntransit0)    ;; Out of transit

i2 = WHERE( (z GE (0.5 + ABS(p-0.5))) AND (z LE (1+p)) AND front,n2)  ;; Planet on limb of stellar disk, but does not cover its center

i34 = WHERE( (p LT 0.5) AND (z GT p) AND (z LT (1-p)) AND front,n34)  ;; Planet disk entirely inside stellar disk, but does not cover the center

i5 = WHERE( (p LT 0.5) AND (z EQ p) AND front,n5)                     ;; Edge of planet touches the center of the stellar disk, and the planet lies 
                                                                      ;; entirely inside the stellar disk

i6 = WHERE( (p EQ 0.5) AND (z EQ 0.5) AND front, n6)                  ;; Planet's diameter equals the star's radius, and the edge of the planet's disk 
                                                                      ;; touches both the stellar center and the limb of the star.  

i7 = WHERE( (p GT 0.5) AND (z EQ p) AND front,n7)                     ;; Edge of planet's disk touches the stellar center, but the planet is not entirely
                                                                      ;; contained inside the area of the stellar disk

i8 = WHERE( (p GT 0.5) AND (z GT ABS(1-p)) AND (z LT p) AND front, n8);; Planet covers the center and limb of the stellar disk

i9 = WHERE( (p LT 1) AND (z LE (0.5 - ABS(p-0.5))) AND front, n9)     ;; Planet disk likes entirely inside the stellar disk, and covers the stellar center

;i10 = WHERE( (p LT 1) AND (z EQ 0) AND front, n10)                    ;; Planet is concentric with the disk of the star, at the precise bottom of the transit
                                                                      ;; (applies only to edge-on orbits with i=90 degrees)
;elminate case 10 for now, since it seems to be accomodated by 9d                                                                      

i11 = WHERE( (p GE 1) AND (z LE p-1) AND front, n11)                  ;; Planet completely eclipses the star                                                                                                                                             

;PRINT,'n1',n1
IF n1 NE 0 THEN lambda_star[i1] = 0d
;PRINT,'n2',n2
IF n2 NE 0 THEN BEGIN
  ;; Define N from Mandel & Agol, Equation (3)
  N = DBLARR(5,n2)
  FOR i = 0,4 DO BEGIN
    N[i,*] = ((1d - a[i2])^((i+6)/4d))/SQRT(b[i2]-a[i2]) * $
      BETA((i+8)/4d,0.5d,/DOUBLE) * $
      ( (z[i2]^2-p^2)/a[i2] * APPELLF1(0.5d,1,0.5d,(i+10)/4d,(a[i2] - 1d )/a[i2],(1d - a[i2])/(b[i2]-a[i2]),/REAL) -$
      HYPERGEOMETRIC2F1(0.5d, 0.5d, (i+10)/4d,(1d - a[i2])/(b[i2]-a[i2]),/REAL) )
  ENDFOR
  lambda_star[i2] = TOTAL(N*c1[*,i2],1,/DOUBLE) / (2*!dpi * omega)
ENDIF
;PRINT,'n34',n34
IF n34 NE 0 THEN BEGIN
  ;; Define M from Mandel & Agol, Equation (4)
  M = DBLARR(3,n34)
  FOR i = 0,2 DO BEGIN
    ii = i+1
    M[i,*] = ((1d - a[i34])^((ii+4)/4d)) * $
      ( (z[i34]^2-p^2)/a[i34] * APPELLF1(0.5d,-(ii+4)/4d,1,1,(b[i34]-a[i34])/(1d - a[i34]),(a[i34]-b[i34])/a[i34],/REAL) -$
      HYPERGEOMETRIC2F1(-(ii+4)/4d,0.5d,1,(b[i34]-a[i34])/(1d - a[i34]),/REAL) )
  ENDFOR
  L = p^2*(1d - p^2/2d - z[i34]^2)
  lambda_star[i34] = (c[0]*p^2 + 2d*TOTAL(M*c1[1:3,i34],1,/DOUBLE) + c[4]*L) / (4d * omega)
ENDIF
;PRINT,'n5',n5
IF n5 NE 0 THEN BEGIN
   K = DBLARR(5,n5)
   FOR i = 0,4 DO K[i,*] = HYPERGEOMETRIC2F1(0.5d,-(i+4)/4d,1,4d*p^2,/REAL)
   lambda_star[i5] = 0.5d - TOTAL(K*c1[*,i5],/DOUBLE) / (2d*omega)
ENDIF
;PRINT,'n6',n6
IF n6 NE 0 THEN BEGIN
   J = DBLARR(5)
   FOR i = 0,4 DO J[i] = GAMMA(1.5d + i/4d) / GAMMA(2d + i/4d)
   lambda_star[i6] = 0.5d - TOTAL(J*c1[*,i6[0]],/DOUBLE) / (2D*SQRT(!dpi)*omega)
ENDIF
;PRINT,'n7',n7
IF n7 NE 0 THEN BEGIN
   H = DBLARR(5)
   FOR i = 0,4 DO H[i] = BETA(0.5d,(i+8)/4d,/DOUBLE) * HYPERGEOMETRIC2F1(0.5d,0.5d,2.5d + i/4d, 1/(4d*p^2),/REAL)
   lambda_star[i7] = 0.5d - TOTAL(H*c1[*,i7[0]],/DOUBLE) / (4d*p*!dpi*omega)
ENDIF
;PRINT,'n8',n8
IF n8 NE 0 THEN BEGIN
  ;; Define N from Mandel & Agol, Equation (3)
  N = DBLARR(5,n8)
  FOR i = 0,4 DO BEGIN
    N[i,*] = ((1d - a[i8])^((i+6)/4d))/SQRT(b[i8]-a[i8]) * $
      BETA((i+8)/4d,0.5d,/DOUBLE) * $
      ( (z[i8]^2-p^2)/a[i8] * APPELLF1(0.5d,1,0.5d,(i+10)/4d,(a[i8] - 1d )/a[i8],(1d - a[i8])/(b[i8]-a[i8]),/REAL) -$
      HYPERGEOMETRIC2F1(0.5d, 0.5d, (i+10)/4d,(1d - a[i8])/(b[i8]-a[i8]),/REAL) )
  ENDFOR
  lambda_star[i8] = 1d + TOTAL(N*c1[*,i8],1,/DOUBLE) / (2*!dpi * omega)  ;; The value inside the TOTAL must be a negative number -- check
ENDIF
;PRINT,'n9',n9
IF n9 NE 0 THEN BEGIN
  ;; Define M from Mandel & Agol, Equation (4)
  M = DBLARR(3,n9)
  FOR i = 0,2 DO BEGIN
    ii = i+1
    M[i,*] = ((1d - a[i9])^((ii+4)/4d)) * $
      ( (z[i9]^2-p^2)/a[i9] * APPELLF1(0.5d,-(ii+4)/4d,1,1,(b[i9]-a[i9])/(1d - a[i9]),(a[i9]-b[i9])/a[i9],/REAL) -$
      HYPERGEOMETRIC2F1(-(ii+4)/4d,0.5d,1,(b[i9]-a[i9])/(1d - a[i9]),/REAL) )
  ENDFOR
  L = p^2*(1d - p^2/2d - z[i9]^2)
  lambda_star[i9] = 1d - (c[0]*(1d - p^2) - 2d*TOTAL(M*c1[1:3,i9],1,/DOUBLE) + c[4]*(0.5d - L)) / (4d * omega)
ENDIF
;PRINT,'n10',n10
;IF n10 NE 0 THEN BEGIN
;   G = DBLARR(5)
;   FOR i = 0,4 DO G[i] = (1d - p^2)^((i+4)/4d)
;   lambda_star[i10] = 1d - TOTAL(G*c1[*,i10[0]],/DOUBLE) / (4d*omega)
;ENDIF
;PRINT,'n11',n11
IF n11 NE 0 THEN lambda_star[i11] = 1d0

IF ntransit0 NE 0 THEN BEGIN
  ;; Define mid-transit to be the location in the light curve of maximum obscuration of the star by the planet.
  obscure_max = MAX(lambda_star,imax,/NAN)
  itransit = WHERE(ABS((lambda_star-obscure_max)/obscure_max) LE 1d-3) ;; Allow for multiple points where the obscuration is close to this value  
    ;;; (Helpful when there is more than one orbit represented)
ENDIF
                                                                
RETURN
END


PRO exoplanet_transit_eclipse,theta_phase,ar_semimaj,r,rp_rstar,inclination,star_frac,planet_frac,itransit,ieclipse,LIMB_DARKENING=limb_darkening
  ;;  INPUTS:
  ;;       THETA_PHASE   = Array of true anomaly values, phased to the transit, modulo 2*!dpi (radians).  In a circular
  ;;                       orbit, this is the observed orbital phase converted to radians.  In an elliptical orbit this is
  ;;                       the true anomaly minus the value of the anomaly at transit.
;;          AR_SEMIMAJ = Orbital semimajor axis, in units of the stellar radius
  ;;               R     = Array of distances from star to planet, in units of the semimajor axis.
  ;;          RP_RSTAR   = Planetary radius, in units of the stellar radius
  ;;          INCLINATION = Orbital Inclination  (measured from axis of revolution), degrees
  ;;
  ;;   OPTIONAL KEYWORDS  
  ;;          LIMB_DARKENING = The set of stellar limb darkening coefficients to use to compute the transit profile.
  ;;                           See Claret (2000) for a description of the model, and Mandel & Agol (2002) for the
  ;;                           implications for transit profiles.
  ;;
  ;;   OUTPUTS:
  ;;          STAR_FRAC   = Flux of star as a function of THETA_PHASE, relative to unobstructed stellar flux
  ;;          PLANET_FRAC = Flux of planet as a function of THETA_PHASE, relative to unobstructed planetary flux
  ;;          ITRANSIT = indices of input phase array during which planet is in full transit (!null if no transit)
  ;;          IECLIPSE = indices of input phase array during which planet is in full eclipse (!null if no eclipse)
  ;;
  ;;  Based on Mandel & Agol 2002, "Analytic Light Curves for Planetary Transit Searches", ApJ 580, 171

  itransit = !null
  ieclipse = !null
  ;;; Recondition phases to go between 0 and 2pi
  phase2 = (theta_phase / (2*!dpi)) MOD 1
  ineg = WHERE(phase2 LT 0,nneg)
  IF NNEG GT 0 THEN phase2[ineg] += 1
  theta_phase = phase2 * 2 * !dpi

  degrad = !dpi/180d0
  nt = N_ELEMENTS(theta_phase)
  ;;; Projected distance between centers of planet and star (in units of Rstar):  r/rstar * SQRT(sin^2(theta_phase) + cos^2(i)*cos^2(theta_phase))
  z = r * ar_semimaj * SQRT(1 - COS(theta_phase)^2 * SIN(inclination * degrad)^2)
  ;;; Planet radius in units of Rstar:
  p = rp_rstar
  kappa_1 = ACOS( (1-p^2+z^2)/(2*z) )
  kappa_2 = ACOS( (p^2+z^2-1)/(2*p*z) )
  lambda_2 = (kappa_1 + p^2*kappa_2 - 0.5*SQRT( 4*z^2 - (1 + z^2 - p^2)^2 ))/!dpi

  ;;;; TRANSIT
  IF N_ELEMENTS(LIMB_DARKENING) EQ 4 THEN BEGIN   ;;; Get the shape of the transit with limb darkening
     ETE_LIMB,theta_phase,z,p,limb_darkening,lambda_star,itransit 
  ENDIF ELSE BEGIN  ;; Get the shape of the transit without limb darkening (pure geometrical)
     i1 = WHERE( (z GT (1+P)) OR (0.5*!dpi LT theta_phase AND 1.5*!dpi GE theta_phase),n1)     ;; Out of transit
     i2 = WHERE( z GT (1-P) AND z LE (1+P) AND (theta_phase GT 1.5*!dpi OR theta_phase LE 0.5*!dpi),n2)   ;;; Partial transit
     i3 = WHERE( z LE (1-P)  AND (theta_phase GT 1.5*!dpi OR theta_phase LE 0.5*!dpi),n3)  ;; Full Transit
     lambda_star = DBLARR(nt)  ;;; Obscured fraction of stellar area
     IF n1 NE 0 THEN lambda_star[i1] = 0.0
     IF n2 NE 0 THEN lambda_star[i2] = lambda_2[i2]
     IF n3 NE 0 THEN BEGIN
       lambda_star[i3] = p^2
       itransit = i3
     ENDIF
  ENDELSE
  
;;;; ECLIPSE
  i1 = WHERE( (z GT (1+P)) OR (theta_phase GT 1.5*!dpi OR theta_phase LE 0.5*!dpi),n1)   ;; Out of contact
  i2 = WHERE( z GT (1-P) AND z LE (1+P) AND (0.5*!dpi LT theta_phase AND 1.5*!dpi GE theta_phase),n2)   ;;; Partial occultation
  i3 = WHERE( z LE (1-P)  AND (0.5*!dpi LT theta_phase AND 1.5*!dpi GE theta_phase),n3)    ;;; full occultaiton
  lambda_planet = DBLARR(nt)   ;;; Obscured fraction of planetary area
  IF n1 NE 0 THEN lambda_planet[i1] = 0.0
  IF n2 NE 0 THEN lambda_planet[i2] = lambda_2[i2]/P^2
  IF n3 NE 0 THEN BEGIN
    lambda_planet[i3] = 1.0
    ieclipse = i3
  ENDIF

  star_frac = 1.0 - lambda_star
  planet_frac = 1.0 - lambda_planet

  RETURN
END

