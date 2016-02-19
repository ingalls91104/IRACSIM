pro endps,fname,post=post,NoEncap=NoEncap,pdf=pdf,noview=noview,Ghost=Ghost,$
          Orientation=Orientation,png=png,nopost=nopost,gif=gif
;;
;;  Closes a postscript file and converts to pdf and/or png/gif.
;;  This command must be preceded by 'ifps', to set up the post script
;;  file before sending graphics commands.
;;
;;  FNAME  the file that you created
;;  POST   set this keyword if it's a postscript file[Depreciated]
;          Now default is that it is a postscript file unless /NoPost is set,
;          or POST=0.  
;;  NoEncap set this keyword if you're making a PDF and you don't want
;;          encapsulated PDF
;;  PDF    set this keyword if you also want a pdf file
;;  PNG    set this keyword if you want a png file
;;  GIF    set this keyword if you want a gif file
;;  NOVIEW set this keyword if you don't want to view the file
;;  GHOST set this keyword to view the ps file using ghostview -
;;  otherwise use gv
;;  ORIENTATION - set to one of 'portrait', 'upside down',
;;                'landscape', 'seascape' for ghostview 
;;                
;;
;;  eg.   endps,'myfile.ps',/post,/pdf  
;;        this will create both a 
;;        PS and PDF file, and display the pdf file using Acrobat Reader
;;  

common devs,mydevice

IF KeyWord_Set(GHOST) THEN BEGIN 
    Viewer = 'ghostview' 
    IF N_Elements(ORIENTATION) NE 0 THEN Viewer = Viewer+' -'+Orientation+' -forceorientation'
ENDIF ELSE BEGIN 
    Viewer = 'gv'
ENDELSE

IF !version.os_name NE 'Mac OS X' THEN NOVIEW=1

fname_spawn = STRJOIN(STRSPLIT(fname,' ',/extract),'\ ')

IF N_ELEMENTS(post) EQ 0 THEN POST=1
IF KEYWORD_SET(nopost) EQ 0 AND post NE 0 THEN BEGIN
   device,/close
   print,'Created PostScript file '+fname
   IF ~keyword_set(pdf) and ~keyword_set(png) AND ~keyword_set(gif) THEN BEGIN
      IF ~keyword_set(noview) THEN spawn,Viewer+' '+fname_spawn
   ENDIF ELSE BEGIN
      idot = rstrpos(fname_spawn,'.')
      IF keyword_set(png) OR keyword_set(gif) THEN BEGIN
         extension = keyword_set(png) ? 'png' : 'gif'
         imgfile = strmid(fname_spawn,0,idot+1)+extension
         spawn,'convert -colorspace RGB -flatten -density 256 '+fname_spawn+'''[0]'' -resample 96 '+imgfile
         IF KEYWORD_SET(NOVIEW) NE 1 THEN spawn,'open '+imgfile
         print,'Created '+STRUPCASE(extension)+' file '+imgfile
      ENDIF
      IF keyword_set(PDF) EQ 1 THEN BEGIN
         make_pdf,fname_spawn,pdffile,NoEncap=NoEncap,noView=noView
      ENDIF
  ENDELSE
  print,'leaving ENDPS - DEVICE is '+mydevice
  set_plot,mydevice
  IF MYDEVICE eq 'x' or MYDEVICE eq 'X' THEN device,decomposed=0
ENDIF

return
end

