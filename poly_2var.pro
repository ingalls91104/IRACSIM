Function Poly_2var,x,y,c,part_der=part_der
;;
;; Evaluates a polynomial function of two variables.
;; IDL> RESULT = POLY_2VAR(X,Y,C) 
;;        Returns:
;;          result = SUM  C[p,q]*x^p*y^q
;;                   p,q
;
;;  Return the partial derivative matrix of the result wrt each of the parameters in PART_DER (if set)
SZ = size(C)
n1 = SZ[1]
n2 = SZ[2]
ndat = N_ELEMENTS(x)
result = 0d0
FOR p=0,n1-1 DO BEGIN
  FOR q=0,n1-1 DO BEGIN
    result += C[p,q] * x^p * y^q
  ENDFOR
ENDFOR
IF KEYWORD_SET(part_der) THEN BEGIN
   part_der = MAKE_ARRAY(n1,n2,ndat,VALUE=0.d0)
   FOR p=0,n1-1 DO BEGIN
     FOR q=0,n1-1 DO BEGIN
       part_der[p,q,*] = x^p * y^q
     ENDFOR
   ENDFOR
ENDIF

RETURN,result
END
