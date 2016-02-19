FUNCTION APPELLF1,a,b1,b2,c,x,y,REAL=real
  ;;
  ;;  Compute Appell's Hypergeometric function of two variables (X and Y).
  ;;  Wrapper for external Fortran package F1V3
  ;;
  ;;  Inputs: A,B1,B2,C, all (potentially) complex scalars
  ;;          X,Y: real numbers, potentially vector (must have the same number
  ;;               of elements).
  ;;  Returns: The Appell Hypergeometric function F1, a potentially
  ;;           complex number, with the ame number of elements as X and Y.
  ;;
  ;; Set /REAL if you want to return the real part only (eg. if the answer is pure real)
  shared_object = '/Users/jamesingalls/work/fortran/adsj_v1_0/f1_idl.so'
  entry = 'f1_idl_'
  ;print,'entered APPELF1:  a,b1,b2,c,x,y: ',a,b1,b2,c,x,y
  ;; Separate real and imaginary parts for input into Fortran
  ar = DOUBLE(REAL_PART(a))
  ai = DOUBLE(IMAGINARY(a))
  b1r = DOUBLE(REAL_PART(b1))
  b1i = DOUBLE(IMAGINARY(b1))
  b2r = DOUBLE(REAL_PART(b2))
  b2i = DOUBLE(IMAGINARY(b2))
  cr = DOUBLE(REAL_PART(c))
  ci = DOUBLE(IMAGINARY(c))
  x = DOUBLE(x)
  y = DOUBLE(y)
  ndat = N_ELEMENTS(x)
  f1_r = DBLARR(ndat)
  f1_i = DBLARR(ndat)

  result = CALL_EXTERNAL(shared_object,entry,ar,ai,b1r,b1i,b2r,b2i,cr,ci,$
    x,y,ndat,f1_r,f1_i);,/UNLOAD)
  IF KEYWORD_SET(REAL) THEN RETURN,REFORM(f1_r) ELSE RETURN,DCOMPLEX(REFORM(f1_r),REFORM(f1_i))
END



