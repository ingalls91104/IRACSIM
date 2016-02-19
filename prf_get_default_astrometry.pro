PRO prf_get_default_astrometry_ch1,header
MKHDR,header,4,[256,256]
SXADDPAR,header,'CHNLNUM',1
SXADDPAR,header,'CRVAL1',0.001019523620710D0,' [deg] RA at CRPIX1,CRPIX2 (using ptg model)'
SXADDPAR,header,'CRVAL2',6.66826192209D-09,' [deg] DEC at CRPIX1,CRPIX2 (using ptg model)'
SXADDPAR,header,'RADESYS','ICRS    ','International Celestial Reference System'
SXADDPAR,header,'EQUINOX', 2000.,'Equinox for ICRS celestial coord. system'
SXADDPAR,header,'CD1_1',-0.000339817306408D0,'CD matrix element'
SXADDPAR,header,'CD1_2',0.00000000000,'CD matrix element'
SXADDPAR,header,'CD2_1',0.00000000000,'CD matrix element'
SXADDPAR,header,'CD2_2',0.000339800986694D0,'CD matrix element'
SXADDPAR,header,'CTYPE1','RA---TAN-SIP','RA---TAN with distortion in pixel space'
SXADDPAR,header,'CTYPE2','DEC--TAN-SIP','DEC--TAN with distortion in pixel space'
SXADDPAR,header,'CRPIX1',128.0,'Reference pixel along axis 1'
SXADDPAR,header,'CRPIX2',128.0,'Reference pixel along axis 2'
SXADDPAR,header,'PXSCAL1',-1.22334117768332D0,'[arcsec/pix] Scale for axis 1 at CRPIX1,CRPIX2'
SXADDPAR,header,'PXSCAL2',1.22328355209902D0,'[arcsec/pix] Scale for axis 2 at CRPIX1,CRPIX2'
SXADDPAR,header,'A_ORDER',3,'poly order, axis 1, detector to sky'
SXADDPAR,header,'A_0_2',2.9656E-06,'distortion coefficient'
SXADDPAR,header,'A_0_3',3.7746E-09,'distortion coefficient'
SXADDPAR,header,'A_1_1',2.1886E-05,'distortion coefficient'
SXADDPAR,header,'A_1_2',-1.6847E-07,'distortion coefficient'
SXADDPAR,header,'A_2_0',-2.3863E-05,'distortion coefficient'
SXADDPAR,header,'A_2_1',-8.561E-09,'distortion coefficient'
SXADDPAR,header,'A_3_0',-1.4172E-07,'distortion coefficient'
SXADDPAR,header,'A_DMAX',1.394,'[pixel] maximum correction'
SXADDPAR,header,'B_ORDER',3,'poly order, axis 2, detector to sky'
SXADDPAR,header,'B_0_2',2.31E-05,'distortion coefficient'
SXADDPAR,header,'B_0_3',-1.6168E-07,'distortion coefficient'
SXADDPAR,header,'B_1_1',-2.4386E-05,'distortion coefficient'
SXADDPAR,header,'B_1_2',-5.7813E-09,'distortion coefficient'
SXADDPAR,header,'B_2_0', 2.1197E-06,'distortion coefficient'
SXADDPAR,header,'B_2_1',-1.6583E-07,'distortion coefficient'
SXADDPAR,header,'B_3_0',-2.0249E-08,'distortion coefficient'
SXADDPAR,header,'B_DMAX',1.501,'[pixel] maximum correction'
SXADDPAR,header,'AP_ORDER',3,'poly order, axis 1, sky to detector'
SXADDPAR,header,'AP_0_1',-6.4275E-07,'distortion coefficient'
SXADDPAR,header,'AP_0_2',-2.9425E-06,'distortion coefficient'
SXADDPAR,header,'AP_0_3', -3.582E-09,'distortion coefficient'
SXADDPAR,header,'AP_1_0',-1.4897E-05,'distortion coefficient'
SXADDPAR,header,'AP_1_1', -2.225E-05,'distortion coefficient'
SXADDPAR,header,'AP_1_2', 1.7195E-07,'distortion coefficient'
SXADDPAR,header,'AP_2_0', 2.4146E-05,'distortion coefficient'
SXADDPAR,header,'AP_2_1',  6.709E-09,'distortion coefficient'
SXADDPAR,header,'AP_3_0', 1.4492E-07,'distortion coefficient'
SXADDPAR,header,'BP_ORDER',3,'poly order, axis 2, sky to detector'
SXADDPAR,header,'BP_0_1',-1.6588E-05,'distortion coefficient'
SXADDPAR,header,'BP_0_2',-2.3424E-05,'distortion coefficient'
SXADDPAR,header,'BP_0_3',  1.651E-07,'distortion coefficient'
SXADDPAR,header,'BP_1_0',-2.6783E-06,'distortion coefficient'
SXADDPAR,header,'BP_1_1', 2.4753E-05,'distortion coefficient'
SXADDPAR,header,'BP_1_2', 3.8917E-09,'distortion coefficient'
SXADDPAR,header,'BP_2_0', -2.151E-06,'distortion coefficient'
SXADDPAR,header,'BP_2_1',    1.7E-07,'distortion coefficient'
SXADDPAR,header,'BP_3_0', 2.0482E-08,'distortion coefficient'

RETURN
END

PRO PRF_GET_DEFAULT_ASTROMETRY_CH2,header

MKHDR,header,4,[256,256]
 SXADDPAR,header,'CHNLNUM', 2,'1 digit instrument channel number'
 SXADDPAR,header,'CRVAL1',-4.60431692773E-12,'[deg] RA at CRPIX1,CRPIX2 (using ptg model)'
 SXADDPAR,header,'CRVAL2',-0.000337726314940,'[deg] DEC at CRPIX1,CRPIX2 (using ptg model)'
 SXADDPAR,header,'RADESYS','ICRS','International Celestial Reference System'
 SXADDPAR,header,'EQUINOX',2000.,'Equinox for ICRS celestial coord. sys.'
 SXADDPAR,header,'CD1_1',-0.000337893992738D0,'CD matrix element'
 SXADDPAR,header,'CD1_2',0.00000000000,'CD matrix element'
 SXADDPAR,header,'CD2_1',0.00000000000,'CD matrix element'
 SXADDPAR,header,'CD2_2',0.000337737990776D0,'CD matrix element'
 SXADDPAR,header,'CTYPE1','RA---TAN-SIP','RA---TAN with distortion in pixel space'
 SXADDPAR,header,'CTYPE2','DEC--TAN-SIP','DEC--TAN with distortion in pixel space'
 SXADDPAR,header,'CRPIX1',128.0,'Reference pixel along axis 1'
 SXADDPAR,header,'CRPIX2',128.0,'Reference pixel along axis 2'
 SXADDPAR,header,'PXSCAL1',-1.21641835430637D0,'[arcsec/pix] Scale for axis 1 at CRPIX1,CRPIX2'
 SXADDPAR,header,'PXSCAL2',1.21585676679388D0,'[arcsec/pix] Scale for axis 2 at CRPIX1,CRPIX2'
 SXADDPAR,header,'A_ORDER',3,'poly order, axis 1, detector to sky'
 SXADDPAR,header,'A_0_2',1.0582E-08,'distortion coefficient'
 SXADDPAR,header,'A_0_3',-3.0831E-09,'distortion coefficient'
 SXADDPAR,header,'A_1_1',3.0188E-05,'distortion coefficient'
 SXADDPAR,header,'A_1_2',-1.8817E-07,'distortion coefficient'
 SXADDPAR,header,'A_2_0',1.9063E-05,'distortion coefficient'
 SXADDPAR,header,'A_2_1',6.5975E-09,'distortion coefficient'
 SXADDPAR,header,'A_3_0',-1.8531E-07,'distortion coefficient'
 SXADDPAR,header,'A_DMAX',1.645,'[pixel] maximum correction'
 SXADDPAR,header,'B_ORDER',3,'poly order, axis 2, detector to sky'
 SXADDPAR,header,'B_0_2',3.4386E-05,'distortion coefficient'
 SXADDPAR,header,'B_0_3',-1.7897E-07,'distortion coefficient'
 SXADDPAR,header,'B_1_1',1.8781E-05,'distortion coefficient'
 SXADDPAR,header,'B_1_2',3.7246E-09,'distortion coefficient'
 SXADDPAR,header,'B_2_0',3.7734E-06,'distortion coefficient'
 SXADDPAR,header,'B_2_1',-1.7747E-07,'distortion coefficient'
 SXADDPAR,header,'B_3_0',-5.2103E-10,'distortion coefficient'
 SXADDPAR,header,'B_DMAX',1.738,'[pixel] maximum correction'
 SXADDPAR,header,'AP_ORDER',3,'poly order, axis 1, sky to detector'
 SXADDPAR,header,'AP_0_1',5.0237E-07,'distortion coefficient'
 SXADDPAR,header,'AP_0_2',-5.1645E-08,'distortion coefficient'
 SXADDPAR,header,'AP_0_3',3.0939E-09,'distortion coefficient'
 SXADDPAR,header,'AP_1_0',-2.1471E-05,'distortion coefficient'
 SXADDPAR,header,'AP_1_1',-3.0796E-05,'distortion coefficient'
 SXADDPAR,header,'AP_1_2',1.9379E-07,'distortion coefficient'
 SXADDPAR,header,'AP_2_0',-1.9359E-05,'distortion coefficient'
 SXADDPAR,header,'AP_2_1',-4.4162E-09,'distortion coefficient'
 SXADDPAR,header,'AP_3_0',1.8929E-07,'distortion coefficient'
 SXADDPAR,header,'BP_ORDER',3,'poly order, axis 2, sky to detector'
 SXADDPAR,header,'BP_0_1',-1.997E-05,'distortion coefficient'
 SXADDPAR,header,'BP_0_2',-3.491E-05,'distortion coefficient'
 SXADDPAR,header,'BP_0_3',1.8428E-07,'distortion coefficient'
 SXADDPAR,header,'BP_1_0',5.83E-07,'distortion coefficient'
 SXADDPAR,header,'BP_1_1',-1.9129E-05,'distortion coefficient'
 SXADDPAR,header,'BP_1_2',-1.2856E-09,'distortion coefficient'
 SXADDPAR,header,'BP_2_0',-3.8713E-06,'distortion coefficient'
 SXADDPAR,header,'BP_2_1',1.821E-07,'distortion coefficient'
 SXADDPAR,header,'BP_3_0',7.0846E-10,'distortion coefficient'
RETURN
END

FUNCTION prf_get_default_astrometry,channel,ra,dec,x,y,position_angle,SUB=sub,HEADER=header,RESET_CRPIX=reset_crpix
;;
;;  Build a default astrometry header for IRAC channel given, where the (RA,DEC) coordinates of a particular pixel (X,Y) are known, as is the FOV
;;  position angle.  
;;
;;  If the HEADER keyword is input, then don't need to get a dummy header from GET_IRAC_PTG_OFFSETS.  We'll only be changing 
;;  (CRVAL1,CRVAL2).
;;
;;  If /RESET_CRPIX is set, we will need to reset the reference pixel location in the destination header to the nominal value.
;;
;;  If HEADER and /RESET_CRPIX are set, use the input header, but ensure CRPIX1,2 are set to the nominal pixel (i.e., change only CRVAL1,2)
;;
IF N_ELEMENTS(header) EQ 0 OR KEYWORD_SET(RESET_CRPIX) THEN BEGIN
   RESOLVE_ROUTINE,'GET_IRAC_PTG_OFFSETS',/COMPILE_FULL_FILE,/NO_RECOMPILE
     ;; Get the default header (for RA,DEC = 0,0 and zero rotation)
   CASE CHANNEL OF
;   1: get_irac_astro_header_ch1, dummy_header
;   2: get_irac_astro_header_ch2, dummy_header
      1: prf_get_default_astrometry_ch1, dummy_header
      2: prf_get_default_astrometry_ch2, dummy_header
;      3: get_irac_astro_header_ch3, dummy_header
;      4: get_irac_astro_header_ch4, dummy_header
   ENDCASE
   IF KEYWORD_SET(SUB) THEN BEGIN
      SXADDPAR,dummy_header,'NAXIS',3
      SXADDPAR,dummy_header,'NAXIS1',32
      SXADDPAR,dummy_header,'NAXIS2',32
      SXADDPAR,dummy_header,'NAXIS3',64,AFTER='NAXIS2'
      SXADDPAR,dummy_header,'CRPIX1',16.5
      SXADDPAR,dummy_header,'CRPIX2',16.5
   ENDIF
ENDIF
IF N_ELEMENTS(HEADER) EQ 0 THEN BEGIN
;; No input header
   header=dummy_header
   SXADDPAR,header,'PA',position_angle,'[deg] Position angle of axis 2 (E of N)',AFTER='PXSCAL2'
   SXADDPAR,header,'CD1_1',SXPAR(header,'PXSCAL1')*cos(Position_Angle/!radeg)/3600.
   SXADDPAR,header,'CD1_2',SXPAR(header,'PXSCAL2')*sin(Position_Angle/!radeg)/3600.
   SXADDPAR,header,'CD2_1',-SXPAR(header,'PXSCAL1')*sin(Position_Angle/!radeg)/3600.
   SXADDPAR,header,'CD2_2',SXPAR(header,'PXSCAL2')*cos(Position_Angle/!radeg)/3600.
ENDIF 
IF KEYWORD_SET(RESET_CRPIX) THEN BEGIN
;; Reset to the expected nominal pixel
   SXADDPAR,header,'CRPIX1',SXPAR(dummy_header,'CRPIX1')
   SXADDPAR,header,'CRPIX2',SXPAR(dummy_header,'CRPIX2')
ENDIF

;; Determine the coordinates of the reference pixel
;   
;; (1) Copy header but set reference pixel to (RA,DEC) at (X,Y)
header2 = header
SXADDPAR,header2,'CRPIX1',x+1
SXADDPAR,header2,'CRPIX2',y+1
SXADDPAR,header2,'CRVAL1',ra
SXADDPAR,header2,'CRVAL2',dec
;; (2) Turn header copy into astrometry structure     
EXTAST,header2,astr2,success
;; (3) Determine (RA,DEC) of reference pixel in the actual frame to be returned
XY2AD,SXPAR(header,'CRPIX1')-1,SXPAR(header,'CRPIX2')-1,astr2,crval1,crval2
;; (4) Set the apropriate values in the actual header to be returned 
SXADDPAR,header,'CRVAL1',crval1
SXADDPAR,header,'CRVAL2',crval2
   
RETURN,header
END