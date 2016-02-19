function sin_skew,x,P,a,phase=phase
;;
;;   Return a skewed sine function of X (any units), with period P (same units as X).  
;;   Parameter A specifies the fraction of one period that the first peak occurs.  
;;   In a normal sine curve, A = 1/4 (i.e., at PI/2).  To skew the sine to the left (fast rise), set  0<A<1/4
;;   To skew to the right (slow rise), set 1/4 < A < 1/2.  This gives a 'sawtooth'-like function, but smoother.
;;
;;   Allow for a phase shift (in +/- fraction of period) also, in keyword PHASE
;
phi = DBLARR(n_elements(x))
IF N_ELEMENTS(phase) EQ 0 then phase = 0
f = (x/P+phase) mod 1
region1 = WHERE(f GE 0 AND f LT A,n1)
region2 = WHERE(f GE A AND f LT 1-A,n2)
region3 = WHERE(f GE 1-A AND f LT 1, n3)

IF n1 NE 0 THEN  phi[region1] = !dpi * (1./(2*A) -2) * f[region1]
IF n2 NE 0 THEN  phi[region2] = !dpi * ( (f[region2] - A)/(1.0-2*A) - 2.*f[region2] + 0.5 )
IF n3 NE 0 THEN  phi[region3] = !dpi * (1./(2*A) - 2) * (f[region3] -1.)

RETURN,sin( 2.*!dpi * f + phi )
END
 


 