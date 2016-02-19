pro iterwait,i,idot,inum,text=text
;     Keeps the user updated while the
;     machine is going through a long loop.  Updates the index
;     to the screen.
;
;     The form of the update is 
;     "Iteration number 1...............50..........100..... etc"
;
;     I is the iteration number we are on.  IDOT is the interval
;     between dots.  INUM is the interval between numbers. In the
;     example above, IDOT = 5 and INUM = 50.
x = FLOAT(i)/FLOAT(idot)
xx = x-FIX(x)
y = FLOAT(i)/FLOAT(inum)
yy = y-FIX(y)
IF N_ELEMENTS(text) EQ 0 THEN text = 'Iteration Number'
IF I EQ 0 THEN BEGIN
   PRINT,FORMAT='("'+text+' 0",$)'
ENDIF ELSE BEGIN
   IF yy EQ 0 THEN BEGIN
      nlen = STRLEN(STRNG(i))
      PRINT,STRNG(i),FORMAT='(a'+STRNG(nlen)+',$)'
   ENDIF ELSE BEGIN
      IF XX EQ 0 THEN PRINT,FORMAT='(".",$)
   ENDELSE
ENDELSE     
 
RETURN
END

 PRO Example
    a = 10.4 
    b = 6.2
    Print, a, Format='(F10.2, $)'
    Print, b
   END

;;; $Main$ program 
for i = 0,100 DO iterwait,i,1,20
print,' '
END