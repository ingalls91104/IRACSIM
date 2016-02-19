pro fbm1,npix,beta,seed,z,plot=plot,maxpix=maxpix
;; MAXPIX gives the size of the largest self-contained segment.  The output will be strung together from npix/maxpix (+ 1) 
;;        instances of the fbm noise.
; 
IF N_ELEMENTS(maxpix) NE 0 THEN BEGIN
   ninstance = CEIL(FLOAT(npix)/maxpix)
   npix_inst = MAKE_ARRAY(ninstance, VALUE = LONG(maxpix) )
   remainder = npix mod maxpix
   IF remainder NE 0 THEN npix_inst[ninstance-1] = remainder
ENDIF ELSE BEGIN
   ninstance = 1
   npix_inst = npix
ENDELSE
z = DBLARR(npix)
i2s = total(npix_inst,/CUMULATIVE,/INTEGER) - 1
FOR i = 0LL,ninstance-1 DO BEGIN
   i2 = i2s[i]
   i1 = i2-npix_inst[i]+1
   np = npix_inst[i]
   x = DINDGEN( (np-1)/2 ) + 1
   is_even = (np MOD 2) EQ 0
   uncon_ph = RANDOMU(seed,np) * 2*!dpi
   phi = DBLARR(np)
   phi[0] = uncon_ph[0]    ;;; Unconditioned phases
   IF is_even THEN BEGIN
      freq = [0.0,x,np/2,-np/2+x]/np
      phi[1:(np-1)/2] = uncon_ph[1:(np-1)/2] -REVERSE(uncon_ph[(np-1)/2+2:*])    ;; Convert phases to Hermitian
      phi[(np-1)/2+2:*] = -phi[1:(np-1)/2] 
   ENDIF ELSE BEGIN
      freq = [0.0,x,-(np/2+1) + x]/np
      phi[1:(np-1)/2] = uncon_ph[1:(np-1)/2] -REVERSE(uncon_ph[(np-1)/2+1:*])
      phi[(np-1)/2+1:*] = -phi[1:(np-1)/2] 
   ENDELSE
;   IF i EQ 0 THEN i1 = 0LL ELSE i1 = LONG64(total(npix_inst[0:i-1],/INTEGER))
   amp = dblarr(np)
   amp[0] = 1.0
   amp[1:*] = ABS(freq[1:*])^(-beta/2)

   fourier_component = dcomplexarr(np) 
   fourier_component = amp * COMPLEX( cos(phi) , sin(phi) )
   signal = FLOAT(fft(fourier_component,1))
   coef = POLY_FIT(findgen(np),signal,1)
   z[i1:i2] = signal - POLY(findgen(np),coef)
ENDFOR

;colorize
IF KEYWORD_SET(PLOT) THEN cgplot,z,xstyle=1
return
end
