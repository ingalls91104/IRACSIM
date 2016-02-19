FUNCTION STR_EXP,value,sigdig,PLOTSTRING=plotstring,ERROR=error
;;
;;  Print a number as a plottable string in exponential (actually scientific)
;;  notation.  Use SIGDIG number of significant digits
;;  If SIGDIG is set to 1, then drop the decimal.
;;  If each value has an error bar, include it as an array or scalar in keyword ERROR
;;  To produce a result suitable for plotting (in eg. CGTEXT or XYOUTS), set /PLOTSTRING
;;
negative = value LT 0 
inegative = WHERE(negative,nneg)
ndat = N_ELEMENTS(value)
CSIGN = STRARR(ndat)
IF nneg NE 0 THEN csign[inegative] =  '-'
exponent = LONARR(ndat)
decimal = ABS(value) LT 1
;; The values that have only single digits to the left of the decimal
isingle = WHERE(decimal AND FIX(ALOG10(ABS(value))) EQ ALOG10(ABS(value)),nsingle,COMPLEMENT=imulti,NCOMPLEMENT=nmulti)
IF nsingle NE 0 THEN exponent[isingle] = FIX(ALOG10(ABS(value[isingle])))
IF nmulti NE 0 THEN exponent[imulti] = FIX(ALOG10(ABS(value[imulti])))-1*decimal[imulti]

;IF decimal and FIX(ALOG10(ABS(value))) EQ ALOG10(ABS(value)) THEN exponent = FIX(ALOG10(ABS(value))) $
;ELSE exponent = FIX(ALOG10(ABS(value)))-1*decimal

significand = DOUBLE(value)*10.d0^(-double(exponent))
IF N_PARAMS() LT 2 THEN sigdig = SIZE(value,/TYPE) EQ 5 ? 8:6
sigdig_plus_sign = sigdig + 1*negative + 1
IF sigdig EQ 1 THEN BEGIN
   fmt = '(i'+STRNG(sigdig_plus_sign)+')'
   significand = ROUND(significand)
ENDIF ELSE BEGIN
   fmt = '(f'+STRNG(sigdig_plus_sign)+'.'+STRNG(sigdig_plus_sign-2-1*negative)+')'
ENDELSE
result = STRARR(ndat)
IF KEYWORD_SET(PLOTSTRING) THEN BEGIN
  timesten = ' x 10!u'
  tail = '!n'
  pm = CGSYMBOL('+-')
ENDIF ELSE BEGIN
  timesten = 'E'
  tail = ''
  pm = ‘+/-‘
ENDELSE
IF n_elements(ERROR) EQ 0 THEN BEGIN
   for i = 0,ndat-1 DO result[i] = STRING(significand[i],FORMAT=fmt[i])+timesten+STRNG(exponent[i])+tail
ENDIF ELSE BEGIN
   error_signif = DOUBLE(error)*10.d0^(-double(exponent))
   IF sigdig EQ 1 THEN error_signif = ROUND(error_signif)
   FOR i = 0,ndat-1 DO result[i] = '('+STRING(significand[i],FORMAT=fmt[i])+pm+STRING(error_signif[i],FORMAT=fmt[i])+')'+timesten+STRNG(exponent[i])+tail
ENDELSE
IF ndat EQ 1 THEN result = result[0]
RETURN,result
END
