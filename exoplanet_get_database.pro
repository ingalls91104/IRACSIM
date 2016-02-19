FUNCTION exoplanet_get_database_Url_Callback, status, progress, data
 
   ; print the info msgs from the url object
   PRINT, status
 
   ; return 1 to continue, return 0 to cancel
   RETURN, 1
END
 
;-----------------------------------------------------------------
PRO exoplanet_get_database,filename
   this_struc = ROUTINE_INFO('exoplanet_get_database',/SOURCE)   ;;; Get the path to this program
   this_dir = FILE_DIRNAME(this_struc.path[0],/MARK_DIRECTORY)  ;;; Extract directory name
   filename = this_dir + 'exoplanets.csv'
;;; Only download the file once per day:
;;;    
   a = file_info(filename)
   ALREADY_GOT = (a.exists EQ 1) AND (SYSTIME(/SECONDS) - a.ctime LT 86400.)
   IF Keyword_set(ALREADY_GOT) THEN RETURN   ;;; Don't download latest file from the server, just use what we have.
   ; If the url object throws an error it will be caught here
   CATCH, errorStatus
   IF (errorStatus NE 0) THEN BEGIN
      CATCH, /CANCEL
 
      ; Display the error msg in a dialog and in the IDL output log
      r = DIALOG_MESSAGE(!ERROR_STATE.msg, TITLE='URL Error.  Will just use local file.', $
         /ERROR)
      PRINT, !ERROR_STATE.msg
 
      ; Get the properties that will tell us more about the error.
      oUrl->GetProperty, RESPONSE_CODE=rspCode, $
         RESPONSE_HEADER=rspHdr, RESPONSE_FILENAME=rspFn
      PRINT, 'rspCode = ', rspCode
      PRINT, 'rspHdr= ', rspHdr
      PRINT, 'rspFn= ', rspFn
 
      ; Destroy the url object
      OBJ_DESTROY, oUrl
      RETURN
   ENDIF
 
   ; create a new IDLnetURL object
   oUrl = OBJ_NEW('IDLnetUrl')
 
   ; Specify the callback function
   oUrl->SetProperty, CALLBACK_FUNCTION ='exoplanet_get_database_Url_Callback'
 
   ; Set verbose to 1 to see more info on the transacton
   oUrl->SetProperty, VERBOSE = 0
 
   ; Set the transfer protocol as http
   oUrl->SetProperty, url_scheme = 'http'
 
   ; The Exoplanets.org host
   oUrl->SetProperty, URL_HOST = 'exoplanets.org'
 
   ; The FTP server path of the file to download
   ;http://exoplanets.org/csv-files/exoplanets.csv
   oUrl->SetProperty, URL_PATH = 'csv-files/exoplanets.csv'
 
   ; Make a request to the Exoplanets.org server
   ; Retrieve the csv file and write it
   ; to the current working directory.
 
   filename = oUrl->Get(FILENAME = filename )
 
   ; Print the path to the file retrieved from the remote server
;   PRINT, 'filename returned = ', filename
 
   ; Destroy the url object
   OBJ_DESTROY, oUrl
 
END
