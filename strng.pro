FUNCTION STRNG,value,_Extra=extra
;;
;;  Print a number as a string with no whitespace.
;;
RETURN,STRTRIM(STRING(value,_Extra=extra),2)
END
