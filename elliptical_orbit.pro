PRO elliptical_orbit,t,p,e,r,theta
;
;   Solve for the polar coordinates (R,THETA) of an elliptical orbit
;
;  INPUTS:
;          T: time (in same units as P) since periastron.  Can be either scalar or vector.
;          P: orbital period
;          E: orbital eccentricity (0.0 to 1.0)
;          
;  OUTPUTS:
;          R: orbital radius (one per input T), in units of the semimajor axis.  
;          THETA: polar angle (degrees) between the major axis and orbital body, with vertex at
;                 the star (true anomaly).  0 at periastron
;
degrad = !dpi / 180d0

; "Mean Motion", a parametrization of time as an angle (radians)
M = 2* !dpi * t / p

;; ALPHA = Eccentric anomaly, the orbital angle measured from the center of a circle of radius a
;; to estimate alpha, we need to evaluate an infinite series of Besel functions.  First figure out where to truncate the series.
k = 1
current_term = 2d0/k * BESELJ(k*e,k,/DOUBLE) * sin(k*M)
series = current_term  
tol = 1d-6
maxiter = 100
WHILE MAX(current_term) GE tol OR k GT maxiter DO BEGIN
   k++
   current_term = 2d0/k * BESELJ(k*e,k,/DOUBLE) * sin(k*M)
   series += current_term
ENDWHILE
;print,k
ALPHA = M + series
;; use the half angle identity to express tan(alpha/2) as sin(alpha)/(1+cos(alpha))
THETA = 2. * ATAN( SQRT(1+e) * sin(ALPHA), SQRT(1-e) * (1+cos(ALPHA)) ) / degrad
;THETA = 2. * ATAN( (SQRT(1-e) * sin(ALPHA)) / (SQRT(1+e) * (1+cos(ALPHA))) ) / degrad
 
;; R(theta) in units of the semimajor axis
r = (1-e^2) / (1 + e*COS(degrad * theta))

RETURN
END