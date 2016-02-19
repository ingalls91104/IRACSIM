pro ifps,filenm,xsize,ysize,post=post,encapsulated=encapsulated,color=color,$
   r=r,g=g,b=b,landscape=landscape,nopost=nopost,_extra=extra
;
;   FILENM the file you want to create
;   XSIZE  the size in the x-direction ('X': 100pixels 'PS': inches)
;   YSIZE  the size in the y-direction ('X': 100pixels 'PS': inches)
;   POST   set this keyword if you want to create a postscript file [Depreciated]
;          Now default is to make a postscript file unless /NoPost is set,
;          or POST=0
;   /ENCAPSULATED  set this keyword if it's an encapsulated ps file
;   COLOR  set this keyword if it's a color file
;   R,G,B  these values will be passed back to the user if you need the
;          color table
;   /LANDSCAPE  set this to plot landscape - no guarantees that this works
;   /NOPOST  set this to plot to the screen, configured like a PS file 
;
;   eg.  ifps,'myfile.ps',6,6,/post,/encapsulated
;
;   This command must be followed by 'endps'

common devs,mydevice

;!p.background=255;FSC_COLOR('WHITE',!D.table_size-1)
;!p.color=0
!p.multi=0
!p.charsize=1.5
!p.charthick=2.0
!p.thick=1.0

IF keyword_set(ENCAPSULATED) EQ 1 then POST=1

IF KEYWORD_SET(NOPOST) EQ 0 AND POST NE 0 THEN BEGIN
   IF keyword_set(LANDSCAPE) then BEGIN
      xoffset = (8.5-ysize)/2.0
      yoffset = 11.0 - (11.0-xsize)/2.0
   ENDIF ELSE BEGIN
      xoffset = (8.5-xsize)/2.0
      yoffset = (11.0-ysize)/2.0
      landscape = 0
   ENDELSE
   mydevice = !D.NAME
   set_plot,'ps'
   device,file=filenm,ysize=ysize,xsize=xsize,xoffset=xoffset,color=color,$
      yoffset=yoffset,/inches,encapsulated=encapsulated,$
      landscape=landscape,_extra=extra
   device,/symbol,font_index=4
   device,/helvetica,font_index=3
   device,/helvetica
   IF N_ELEMENTS(extra) NE 0 THEN BEGIN
      IF N_ELEMENTS(extra.bits_per_pixel) EQ 0 THEN device,bits_per_pixel=8
   ENDIF ELSE device,bits_per_pixel=8
   !p.font=0
   IF keyword_set(COLOR) then TVLCT, r,g,b,/get 
ENDIF ELSE BEGIN
   mydevice = !D.NAME
   set_plot,strlowcase(!version.os_family) EQ 'windows' ? 'win':'x'
   device,decomposed=0
   window,0,xsize=xsize*100,ysize=ysize*100,retain=2
ENDELSE
;!p.background=255
;!p.color=0
!p.multi=0
;!p.charsize=1.5
!p.charthick=2.0
!p.thick=1.0

return
end
