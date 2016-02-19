FUNCTION ji_minmax,array,subscript_max=subscript_max,subscript_min=subscript_min,_extra=extra

IF ISA(array,'LIST') THEN BEGIN
   nl = N_ELEMENTS(array)
   actual_array = !null
   FOREACH entry,array DO BEGIN
      IF ISA(entry,/NUMBER) THEN actual_array = [actual_array,entry]
   ENDFOREACH
   
ENDIF ELSE BEGIN
   actual_array = array
ENDELSE

RETURN,[MIN(actual_array,subscript_min,subscript_max=subscript_max,_extra=extra,max=maxval),maxval]
END
