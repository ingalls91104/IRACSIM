FUNCTION read_csv_jim,filename,header=header1,count=count,_extra=extra
;;
;;  IDL's READ_CSV, with the additional functionality of setting the 
;;  structure tags to the actual header keys (if they exist)
;; 
a = READ_CSV(filename,header=header,count=count,_extra=extra)
header1=header
IF count NE 0 AND KEYWORD_SET(header) THEN BEGIN
   FOREACH tag,header,i DO BEGIN
   
      temp_struc = CREATE_STRUCT(tag,a.(i))
      IF i EQ 0 THEN b = temp_struc ELSE b = CREATE_STRUCT(b,temp_struc)
   
   ENDFOREACH

ENDIF ELSE BEGIN
   b=a
   print,'READ_CSV_JIM: No header available.  Fields left anonymous.'
ENDELSE

RETURN,b
END  