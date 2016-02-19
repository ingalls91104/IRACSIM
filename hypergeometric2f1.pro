FUNCTION HYPERGEOMETRIC2F1,a,b,c,z,REAL=real
  ;;
  ;;  Compute Gauss's Hypergeometric function 2F1 FOR REAL or COMPLEX ARGUMENTS
  ;;  Output may be complex
  ;;  Wrapper for external Fortran package hyp_2F1
  ;;  Inputs: A,B,C, Real or complex scalars
  ;;          Z: real or complex, potentially vector
  ;;  Returns: The Gauss Hypergeometric function 2F1, a potentially
  ;;           complex number.
  ;;
  ;; Set /REAL if you want to return the real part only (eg. if the answer is expected to be pure real)
  shared_object = '/Users/jamesingalls/work/fortran/AEAE/hyp2f1_idl.so'
  entry = 'f21_idl_'
;  PRINT,'Entering HYPERGEOMETRIC2F1  a,b,c,z: ',a,b,c,z
  ;; Separate real and imaginary parts for input into Fortran
  ar = DOUBLE(REAL_PART(a))
  ai = DOUBLE(IMAGINARY(a))
  br = DOUBLE(REAL_PART(b))
  bi = DOUBLE(IMAGINARY(b))
  cr = DOUBLE(REAL_PART(c))
  ci = DOUBLE(IMAGINARY(c))
  zr = DOUBLE(REAL_PART(z))
  zi = DOUBLE(IMAGINARY(z))
  ndat = N_ELEMENTS(z)
  f21_r = DBLARR(ndat)
  f21_i = DBLARR(ndat)

;  result = CALL_EXTERNAL(shared_object,entry,a,b,c,$
;    z,ndat,f21_r,f21_i,/UNLOAD)
  result = CALL_EXTERNAL(shared_object,entry,ar,ai,br,bi,cr,ci,$
    zr,zi,ndat,f21_r,f21_i);,/UNLOAD)
  IF KEYWORD_SET(REAL) THEN RETURN,REFORM(f21_r) ELSE RETURN,DCOMPLEX(REFORM(f21_r),REFORM(f21_i))  ;; Answer is complex
END
