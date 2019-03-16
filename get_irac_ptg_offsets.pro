; File contains bonus procedures linearize_irac_pixel_coordinates and 
; distort_irac_pixel_coordinates which are not used in the main procedure
; get_irac_ptg_offsets, but may prove useful.

;+
; NAME:
;      GET_IRAC_PTG_OFFSETS
;
; VERSION
;      1.0
;
; PURPOSE:
;      Determine row and column offsets for IRAC target position in 
;      Spot for a source to be on a given pixel position in the specified
;      channel.  If /ROWCOL is set, then return x, y BCD pixel position for 
;      input row and column offsets for specified channel.
;
; EXPLANATION:
;      Use IRAC astrometric information including distortion parameters to
;      determine pointing offsets for given pixel coordinates, or provide pixel 
;      coordinates for a given pointing offset.  The pixel coordinates are in 
;      BCD orientation, refer to the centers of the pixels and start at (1,1).
;      The procedure uses a set of IRAC BCD FITS headers oriented without
;      rotation in Equatorial coordinates and centered such that the IRAC 
;      pointing center is the coordinate origin. As a result, the sky 
;      coordinates are the angular offsets from the pointing centers.  
;      The standard astrolib procedures are used to calculate sky to pixel and
;      pixel to sky transformations from the image headers.  The approximation
;      that the longitude is in units of arc is used as it is practically true 
;      for the latitude range (+/- 2.5 arcminutes) covered.
;      
;
; CALLING SEQUENCE
;      GET_IRAC_PTG_OFFSETS, CHANNEL, X, Y, ROW, COL[, /SUB] [,/FOV_SUB]
;                          --- or ---
;      GET_IRAC_PTG_OFFSETS, CHANNEL, ROW, COL, X, Y, /ROWCOL_TO_PIXEL[, /SUB] [, /FOV_SUB] 
;     
; INPUTS:
;      CHANNEL: IRAC channel number 1-4
;      X: Scalar or array of x positions on the array in BCD
;         coordinates.  Pixel (1,1) is center of lower left
;         hand corner of array.
;      Y: Scalar of array of y positions on the array in BCD coordinates
;      --- or if /ROWCOL is set ---
;      ROW: Offset for input pixel position in arcseconds 
;          in row direction as defined in Spot
;      COL: Offset in arcseconds in column direction
;
;
; KEYWORD INPUT
;      /ROWCOL: If set, then inputs are ROW and COL and outputs are X and Y
;      /SUB: If set, then X and Y are relative to the subarray rather than 
;            the full array.  Full arrays are 256x256 pixels centered on 
;            (128.5,128.5).  The subarrays are 32x32 pixels with pointing 
;            centers located at
;            channel    x      y
;               1      24    232
;               2      24    232
;               3      24     24
;               4      24     24
;            in pixels relative to the full array BCD orientation.  In subarray
;            BCD orientation, the subarray pointing centers are at pixel (16,16)
;            for each array.  
;       /FOV_SUB: If set, then ROW and COL are relative to the subarray
;                   frame.  
;
; 
; OUTPUTS:
;      ROW: Offset for input pixel position in arcseconds 
;          in row direction as defined in Spot
;      COL: Offset in arcseconds in column direction
;      --- or if /ROWCOL is set ---
;      X: Scalar or array of x positions on the array in BCD
;         coordinates.  Pixel (1,1) is center of lower left
;         hand corner of array.
;      Y: Scalar of array of y positions on the array in BCD coordinates.
;
; EXAMPLE: 
;      Assume that you would like a full array observation at 4.5 and 8.0 um
;      of a star, but with the star at the center of the 8.0 um subarray for 
;      comparison with previous subarray observations.  You have the additional 
;      constraint that the 4.5 um observation should place the star as close as 
;      possible to the corner of a pixel as the star will be close to saturation 
;      if centered.  
;      IDL> ; find offsets corresponding to pixel (16,16) in ch4 subarray
;      IDL> get_irac_ptg_offsets, 4, 16.0, 16.0, row, col, /SUB
;      IDL> print, row, col 
;             124.67483      -125.03855
;      IDL> ; now find corresponding pixel position in channel 2
;      IDL> get_irac_ptg_offsets, 2, row, col, xpos2, ypos2, /ROWCOL_TO_PIXEL
;      IDL> print, xpos2, ypos2
;            24.575064       25.170789
;      IDL> ; refine xpos2, ypos2 so that star is at pixel corner
;      IDL> xp2 = 24.5 & yp2 = 25.5
;      IDL> ; now determine offsets for a fixed cluster target in Spot
;      IDL> get_irac_ptg_offsets, 2, xp2, yp2, orow, ocol
;      IDL>  print, orow, ocol
;              124.76713      -124.64341
; 
;      In Spot you will make a Fixed Cluster Target with the appropriate RA and
;      Dec, and use orow and ocol as the offsets.  Select offset coordinates in 
;      Array and "Observe the offsets only" to place the star at the channel 4 
;      subarray.
;     
;      Modulo pointing errors (rms 0.3 arcseconds), the star centroid should be
;      at
;      IDL> get_irac_ptg_offsets, 4, orow, ocol, xpos4, ypos4, /ROWCOL_TO_PIXEL
;      IDL> print, xpos4, ypos4
;             23.927502       24.330603
;      in the channel 4 BCD.
; 
; NOTES:
;      Tested only for IDL 7.0, 6.4
;      Compile with IDL> .run get_irac_ptg_offsets
;
; PROCEDURES CALLED:
;      GET_IRAC_ASTRO_HEADER_CH1, GET_IRAC_ASTRO_HEADER_CH2, 
;      GET_IRAC_ASTRO_HEADER_CH3, GET_IRAC_ASTRO_HEADER_CH4, 
;      XYAD, ADXY
;
; REVISION HISTORY:
;      Added /FOV_SUB                           JGI, 5 May 2011
;      Corrected orientation of subarray 1+2 BCDs with respect to
;      full array BCD (not flipped), found by SL, SJC 30 Sep 2008
;      Fixed 360 degree wrap around feature for row found by MDL,  
;         SJC 23 Sep 2008
;      First coding S. J. Carey, 22 Sep 2008
;-
pro get_irac_astro_header_ch1, header
; NAME:
;      GET_IRAC_ASTRO_HEADER_CH1
;
; PURPOSE:
;      Return astrometric information for channel 1 full array BCD.  This 
;      information is used to transform between pixel coordinates and angular
;      offsets from the 3.6/5.8 FOV.  
;
; EXPLANATION:
;      The image header is designed so that the 3.6/5.8 FOV pointing center
;      has celestial coordinates (0.0, 0.0) with a zero rotation angle.  As
;      the array is aligned with the coordinate system, xyad and adxy can be 
;      used to return the angular offsets in RA, Dec which are also the row
;      and column offsets for a given pixel or pixel position for a given 
;      input of angular offsets.  Since the array is only 5 arcminutes, the
;      offset between arc units and Right ascension is small (1.D-06) for the
;      edges of the array.  For the channel 1 array, the nominal pointing center 
;      for the 3.6/5.8 um FOV is pixel (131.0, 128.0)
;      
; CALLING SEQUENCE
;      GET_IRAC_ASTRO_HEADER_CH1, HEADER
;      
; OUTPUTS:
;      HEADER: String array containing FITS header with appropriate asrometric 
;              information
;
; NOTES:
;      These are the steps used to generate this header information.
;      1) Start with a BCD file, I used the S14.0 processed version of 
;         SPITZER_I[1-4]_6050560_0000_0000_4_bcd.fits
;      2) Find dx and dy from the CD matrix
;            dx1 = sqrt(sxpar(h1, 'CD1_1')^2.D + sxpar(h1, 'CD2_1')^2.)
;            dy1 = sqrt(sxpar(h1, 'CD1_2')^2.D + sxpar(h1, 'CD2_2')^2.)
;      3) Create a new CD matrix with just the diagonal elements
;            sxaddpar, h1, 'CD1_1', -dx
;            sxaddpar, h1, 'CD2_1', 0.0
;            sxaddpar, h1, 'CD1_2', 0.0
;            sxaddpar, h1, 'CD2_2', dy
;         this aligns the BCD in Equatorial coordinates
;      4) Change CRVAL1,2 and CRPIX1,2 so that the origin of the coordinate 
;         system is at the nominal frame pointing center
;            sxaddpar, h1, 'CRVAL1', 0.0
;            sxaddpar, h1, 'CRVAL2', 0.0
;            sxaddpar, h1, 'CRPIX1', x1
;            sxaddpar, h1, 'CRPIX2', x2
;         where x1 = 131.0 & y1 = 128.0
;               x2 = 128.0 & y2 = 129.0
;               x3 = 124.6 & y3 = 128.8
;               x4 = 127.1 & y4 = 127.5
;      5) Now as the SIP convention has been defined for CRPIX1 = CRPIX2 = 128.0
;         for IRAC, need to make equivalent headers. 
;            xyad, h1, 128.0, 128.0, ra1, dec1
;            sxaddpar, h1, 'CRPIX1', 128.0
;            sxaddpar, h1, 'CRPIX2', 128.0
;            sxaddpar, h1, 'CRVAL1', ra1
;            sxaddpar, h1, 'CRVAL2', dec1
;         this assumes a small error in the distortion correction from step 4) 
;         which is reasonable as the offset from the FOV center and the center 
;         of the array is less than 5 pixels for all arrays with a distortion 
;         correction of 0.02%.
;             
; REVISION HISTORY:
;      Initial coding S.J. Carey 22 Sep 2008
;-

; Return channel 1 full array header
header = ['BITPIX  =                  -32 / FOUR-BYTE SINGLE PRECISION FLOATING POINT', $
  'NAXIS   =                    2 / STANDARD FITS FORMAT', $
  'NAXIS1  =                  256', $
  'NAXIS2  =                  256', $
  'CHNLNUM =                    1 / 1 digit instrument channel number', $
  'CRVAL1  =    0.001019523620710 /', $
  'CRVAL2  =    6.66826192209E-09 /', $
  "RADESYS = 'ICRS    '           / International Celestial Reference Sys.",$
  'EQUINOX =                2000. / Equinox for ICRS celestial coord. system', $
  'CD1_1   =   -0.000339817306408 /', $
  'CD1_2   =        0.00000000000 /', $
  'CD2_1   =        0.00000000000 /', $
  'CD2_2   =    0.000339800986694 /', $
  "CTYPE1  = 'RA---TAN-SIP'       / RA---TAN with distortion in pixel space",$
  "CTYPE2  = 'DEC--TAN-SIP'       / DEC--TAN with distortion in pixel space",$
  'CRPIX1  =                128.0', $
  'CRPIX2  =                128.0', $
  'PXSCAL1 =    -1.22334117768332 / [arcsec/pix] Scale for axis 1 at CRPIX1,CRPIX2', $
  'PXSCAL2 =     1.22328355209902 / [arcsec/pix] Scale for axis 2 at CRPIX1,CRPIX2', $
  'A_ORDER =                    3 / poly order, axis 1, detector to sky', $
  'A_0_2   =           2.9656E-06 / distortion coefficient', $
  'A_0_3   =           3.7746E-09 / distortion coefficient', $
  'A_1_1   =           2.1886E-05 / distortion coefficient', $
  'A_1_2   =          -1.6847E-07 / distortion coefficient', $
  'A_2_0   =          -2.3863E-05 / distortion coefficient', $
  'A_2_1   =           -8.561E-09 / distortion coefficient', $
  'A_3_0   =          -1.4172E-07 / distortion coefficient', $
  'A_DMAX  =                1.394 / [pixel] maximum correction', $
  'B_ORDER =                    3 / poly order, axis 2, detector to sky', $
  'B_0_2   =             2.31E-05 / distortion coefficient', $
  'B_0_3   =          -1.6168E-07 / distortion coefficient', $
  'B_1_1   =          -2.4386E-05 / distortion coefficient', $
  'B_1_2   =          -5.7813E-09 / distortion coefficient', $
  'B_2_0   =           2.1197E-06 / distortion coefficient', $
  'B_2_1   =          -1.6583E-07 / distortion coefficient', $
  'B_3_0   =          -2.0249E-08 / distortion coefficient', $
  'B_DMAX  =                1.501 / [pixel] maximum correction', $
  'AP_ORDER=                    3 / poly order, axis 1, sky to detector', $
  'AP_0_1  =          -6.4275E-07 / distortion coefficient', $
  'AP_0_2  =          -2.9425E-06 / distortion coefficient', $
  'AP_0_3  =           -3.582E-09 / distortion coefficient', $
  'AP_1_0  =          -1.4897E-05 / distortion coefficient', $
  'AP_1_1  =           -2.225E-05 / distortion coefficient', $
  'AP_1_2  =           1.7195E-07 / distortion coefficient', $
  'AP_2_0  =           2.4146E-05 / distortion coefficient', $
  'AP_2_1  =            6.709E-09 / distortion coefficient', $
  'AP_3_0  =           1.4492E-07 / distortion coefficient', $
  'BP_ORDER=                    3 / poly order, axis 2, sky to detector', $
  'BP_0_1  =          -1.6588E-05 / distortion coefficient', $
  'BP_0_2  =          -2.3424E-05 / distortion coefficient', $
  'BP_0_3  =            1.651E-07 / distortion coefficient', $
  'BP_1_0  =          -2.6783E-06 / distortion coefficient', $
  'BP_1_1  =           2.4753E-05 / distortion coefficient', $
  'BP_1_2  =           3.8917E-09 / distortion coefficient', $
  'BP_2_0  =           -2.151E-06 / distortion coefficient', $
  'BP_2_1  =              1.7E-07 / distortion coefficient', $
  'BP_3_0  =           2.0482E-08 / distortion coefficient', $
  'END                                                    ']

return
end

pro get_irac_astro_header_ch2, header
; NAME:
;      GET_IRAC_ASTRO_HEADER_CH2
;
; PURPOSE:
;      Return astrometric information for channel 2 full array BCD.  This 
;      information is used to transform between pixel coordinates and angular
;      offsets from the 4.5/8.0 FOV.  
;
; EXPLANATION:
;      The image header is designed so that the 4.5/8.0 FOV pointing center
;      has celestial coordinates (0.0, 0.0) with a zero rotation angle.  As
;      the array is aligned with the coordinate system, xyad and adxy can be 
;      used to return the angular offsets in RA, Dec which are also the row
;      and column offsets for a given pixel or pixel position for a given 
;      input of angular offsets.  Since the array is only 5 arcminutes, the
;      offset between arc units and Right ascension is small (1.D-06) for the
;      edges of the array.  For the channel 2 array, the nominal pointing center 
;      for the 4.5/8.0 um FOV is pixel (128.0, 129.0)
;      
; CALLING SEQUENCE
;      GET_IRAC_ASTRO_HEADER_CH2, HEADER
;      
; OUTPUTS:
;      HEADER: String array containing FITS header with appropriate asrometric 
;              information
;
; REVISION HISTORY:
;      Initial coding S.J. Carey 22 Sep 2008
;-

; Return channel 2 full array header
header =['BITPIX  =                  -32 / FOUR-BYTE SINGLE PRECISION FLOATING POINT', $
  'NAXIS   =                    2 / STANDARD FITS FORMAT', $
  'NAXIS1  =                  256', $
  'NAXIS2  =                  256', $
  'CHNLNUM =                    2 / 1 digit instrument channel number', $
  'CRVAL1  =   -4.60431692773E-12 /', $
  'CRVAL2  =   -0.000337726314940 /', $
  "RADESYS = 'ICRS    '           / International Celestial Reference Sys.",$
  'EQUINOX =                2000. / Equinox for ICRS celestial coord. sys.',$
  'CD1_1   =   -0.000337893992738 /', $
  'CD1_2   =        0.00000000000 /', $
  'CD2_1   =        0.00000000000 /', $
  'CD2_2   =    0.000337737990776 /', $
  "CTYPE1  = 'RA---TAN-SIP'       / RA---TAN with distortion in pixel space",$
  "CTYPE2  = 'DEC--TAN-SIP'       / DEC--TAN with distortion in pixel space",$
  'CRPIX1  =                128.0', $
  'CRPIX2  =                128.0', $
  'PXSCAL1 =    -1.21641835430637 / [arcsec/pix] Scale for axis 1 at CRPIX1,CRPIX2', $
  'PXSCAL2 =     1.21585676679388 / [arcsec/pix] Scale for axis 2 at CRPIX1,CRPIX2', $
  'A_ORDER =                    3 / poly order, axis 1, detector to sky', $
  'A_0_2   =           1.0582E-08 / distortion coefficient', $
  'A_0_3   =          -3.0831E-09 / distortion coefficient', $
  'A_1_1   =           3.0188E-05 / distortion coefficient', $
  'A_1_2   =          -1.8817E-07 / distortion coefficient', $
  'A_2_0   =           1.9063E-05 / distortion coefficient', $
  'A_2_1   =           6.5975E-09 / distortion coefficient', $
  'A_3_0   =          -1.8531E-07 / distortion coefficient', $
  'A_DMAX  =                1.645 / [pixel] maximum correction', $
  'B_ORDER =                    3 / poly order, axis 2, detector to sky', $
  'B_0_2   =           3.4386E-05 / distortion coefficient', $
  'B_0_3   =          -1.7897E-07 / distortion coefficient', $
  'B_1_1   =           1.8781E-05 / distortion coefficient', $
  'B_1_2   =           3.7246E-09 / distortion coefficient', $
  'B_2_0   =           3.7734E-06 / distortion coefficient', $
  'B_2_1   =          -1.7747E-07 / distortion coefficient', $
  'B_3_0   =          -5.2103E-10 / distortion coefficient', $
  'B_DMAX  =                1.738 / [pixel] maximum correction', $
  'AP_ORDER=                    3 / poly order, axis 1, sky to detector', $
  'AP_0_1  =           5.0237E-07 / distortion coefficient', $
  'AP_0_2  =          -5.1645E-08 / distortion coefficient', $
  'AP_0_3  =           3.0939E-09 / distortion coefficient', $
  'AP_1_0  =          -2.1471E-05 / distortion coefficient', $
  'AP_1_1  =          -3.0796E-05 / distortion coefficient', $
  'AP_1_2  =           1.9379E-07 / distortion coefficient', $
  'AP_2_0  =          -1.9359E-05 / distortion coefficient', $
  'AP_2_1  =          -4.4162E-09 / distortion coefficient', $
  'AP_3_0  =           1.8929E-07 / distortion coefficient', $
  'BP_ORDER=                    3 / poly order, axis 2, sky to detector', $
  'BP_0_1  =           -1.997E-05 / distortion coefficient', $
  'BP_0_2  =           -3.491E-05 / distortion coefficient', $
  'BP_0_3  =           1.8428E-07 / distortion coefficient', $
  'BP_1_0  =             5.83E-07 / distortion coefficient', $
  'BP_1_1  =          -1.9129E-05 / distortion coefficient', $
  'BP_1_2  =          -1.2856E-09 / distortion coefficient', $
  'BP_2_0  =          -3.8713E-06 / distortion coefficient', $
  'BP_2_1  =            1.821E-07 / distortion coefficient', $
  'BP_3_0  =           7.0846E-10 / distortion coefficient', $
  'END                                                    ']

return
end

pro get_irac_astro_header_ch3, header
; NAME:
;      GET_IRAC_ASTRO_HEADER_CH3
;
; PURPOSE:
;      Return astrometric information for channel 3 full array BCD.  This 
;      information is used to transform between pixel coordinates and angular
;      offsets from the 3.6/5.8 FOV.  
;
; EXPLANATION:
;      The image header is designed so that the 3.6/5.8 FOV pointing center
;      has celestial coordinates (0.0, 0.0) with a zero rotation angle.  As
;      the array is aligned with the coordinate system, xyad and adxy can be 
;      used to return the angular offsets in RA, Dec which are also the row
;      and column offsets for a given pixel or pixel position for a given 
;      input of angular offsets.  Since the array is only 5 arcminutes, the
;      offset between arc units and Right ascension is small (1.D-06) for the
;      edges of the array.  For the channel 3 array, the nominal pointing center 
;      for the 3.6/5.8 um FOV is pixel (124.6, 128.8)
;      
; CALLING SEQUENCE
;      GET_IRAC_ASTRO_HEADER_CH3, HEADER
;      
; OUTPUTS:
;      HEADER: String array containing FITS header with appropriate asrometric 
;              information
;
; REVISION HISTORY:
;      Initial coding S.J. Carey 22 Sep 2008
;-

; Return channel 3 full array header
header = ['BITPIX  =                  -32 / FOUR-BYTE SINGLE PRECISION FLOATING POINT', $
    'NAXIS   =                    2 / STANDARD FITS FORMAT', $
    'NAXIS1  =                  256', $
    'NAXIS2  =                  256', $
  'CHNLNUM =                    3 / 1 digit instrument channel number', $
  'CRVAL1  =    -0.00115850232760 /', $
  'CRVAL2  =   -0.000271790202366 /', $
    "RADESYS = 'ICRS    '           / International Celestial Reference Sys.", $
    'EQUINOX =                2000. / Equinox for ICRS celestial coord. system', $
    'CD1_1   =   -0.000340765121720 /', $
    'CD1_2   =        0.00000000000 /', $
    'CD2_1   =        0.00000000000 /', $
    'CD2_2   =    0.000339716993039 /', $
    "CTYPE1  = 'RA---TAN-SIP'       / RA---TAN with distortion in pixel space", $
    "CTYPE2  = 'DEC--TAN-SIP'       / DEC--TAN with distortion in pixel space", $
    'CRPIX1  =                128.0', $
    'CRPIX2  =                128.0', $
    'PXSCAL1 =    -1.22673962032422 / [arcsec/pix] Scale for axis 1 at CRPIX1,CRPIX2', $
    'PXSCAL2 =     1.22298117494211 / [arcsec/pix] Scale for axis 2 at CRPIX1,CRPIX2', $
    'A_ORDER =                    3 / poly order, axis 1, detector to sky', $
    'A_0_2   =          -4.3447E-06 / distortion coefficient', $
    'A_0_3   =           -1.016E-09 / distortion coefficient', $
    'A_1_1   =           3.5897E-05 / distortion coefficient', $
    'A_1_2   =          -1.5883E-07 / distortion coefficient', $
    'A_2_0   =          -1.6032E-05 / distortion coefficient', $
    'A_2_1   =          -1.0378E-09 / distortion coefficient', $
    'A_3_0   =          -1.5738E-07 / distortion coefficient', $
    'A_DMAX  =                1.641 / [pixel] maximum correction', $
    'B_ORDER =                    3 / poly order, axis 2, detector to sky', $
    'B_0_2   =           2.5424E-05 / distortion coefficient', $
    'B_0_3   =          -1.6169E-07 / distortion coefficient', $
    'B_1_1   =           -9.977E-06 / distortion coefficient', $
    'B_1_2   =           7.6924E-09 / distortion coefficient', $
    'B_2_0   =          -7.8167E-06 / distortion coefficient', $
    'B_2_1   =          -1.6873E-07 / distortion coefficient', $
    'B_3_0   =          -1.1593E-08 / distortion coefficient', $
    'B_DMAX  =                1.184 / [pixel] maximum correction', $
    'AP_ORDER=                    3 / poly order, axis 1, sky to detector', $
    'AP_0_1  =          -2.3883E-07 / distortion coefficient', $
    'AP_0_2  =            4.406E-06 / distortion coefficient', $
    'AP_0_3  =           6.4348E-10 / distortion coefficient', $
    'AP_1_0  =          -1.5761E-05 / distortion coefficient', $
    'AP_1_1  =          -3.6428E-05 / distortion coefficient', $
    'AP_1_2  =             1.64E-07 / distortion coefficient', $
    'AP_2_0  =           1.6243E-05 / distortion coefficient', $
    'AP_2_1  =          -9.3393E-10 / distortion coefficient', $
    'AP_3_0  =           1.5989E-07 / distortion coefficient', $
    'BP_ORDER=                    3 / poly order, axis 2, sky to detector', $
    'BP_0_1  =          -1.6807E-05 / distortion coefficient', $
    'BP_0_2  =          -2.5772E-05 / distortion coefficient', $
    'BP_0_3  =           1.6546E-07 / distortion coefficient', $
    'BP_1_0  =          -8.8532E-07 / distortion coefficient', $
    'BP_1_1  =           1.0173E-05 / distortion coefficient', $
    'BP_1_2  =          -8.7895E-09 / distortion coefficient', $
    'BP_2_0  =           7.8383E-06 / distortion coefficient', $
    'BP_2_1  =           1.7089E-07 / distortion coefficient', $
    'BP_3_0  =           1.2114E-08 / distortion coefficient', $
    'END                                                    ']

return
end

pro get_irac_astro_header_ch4, header
; NAME:
;      GET_IRAC_ASTRO_HEADER_CH4
;
; PURPOSE:
;      Return astrometric information for channel 4 full array BCD.  This 
;      information is used to transform between pixel coordinates and angular
;      offsets from the 4.5/8.0 FOV.  
;
; EXPLANATION:
;      The image header is designed so that the 4.5/8.0 FOV pointing center
;      has celestial coordinates (0.0, 0.0) with a zero rotation angle.  As
;      the array is aligned with the coordinate system, xyad and adxy can be 
;      used to return the angular offsets in RA, Dec which are also the row
;      and column offsets for a given pixel or pixel position for a given 
;      input of angular offsets.  Since the array is only 5 arcminutes, the
;      offset between arc units and Right ascension is small (1.D-06) for the
;      edges of the array.  For the channel 4 array, the nominal pointing center 
;      for the 4.5/8.0 um FOV is pixel (127.1, 127.5)
;      
; CALLING SEQUENCE
;      GET_IRAC_ASTRO_HEADER_CH4, HEADER
;      
; OUTPUTS:
;      HEADER: String array containing FITS header with appropriate asrometric 
;              information
;
; REVISION HISTORY:
;      Initial coding S.J. Carey 22 Sep 2008
;-

; Return channel 4 full array header
header =['BITPIX  =                  -32 / FOUR-BYTE SINGLE PRECISION FLOATING POINT', $
    'NAXIS   =                    2 / STANDARD FITS FORMAT', $
    'NAXIS1  =                  256', $
    'NAXIS2  =                  256', $
  'CHNLNUM =                    4 / 1 digit instrument channel number', $
  'CRVAL1  =   -0.000306143198259 /', $
  'CRVAL2  =    0.000169316429913 /', $
    "RADESYS = 'ICRS    '           / International Celestial Reference Sys.", $
    'EQUINOX =                2000. / Equinox for ICRS celestial coord. system', $
    'CD1_1   =   -0.000340140939181 /', $
    'CD1_2   =        0.00000000000 /', $
    'CD2_1   =        0.00000000000 /', $
    'CD2_2   =    0.000338624988217 /', $
    "CTYPE1  = 'RA---TAN-SIP'       / RA---TAN with distortion in pixel space", $
    "CTYPE2  = 'DEC--TAN-SIP'       / DEC--TAN with distortion in pixel space", $
    'CRPIX1  =                128.0', $
    'CRPIX2  =                128.0', $
    'PXSCAL1 =     -1.2244968325831 / [arcsec/pix] Scale for axis 1 at CRPIX1,CRPIX2', $
    'PXSCAL2 =     1.21904995758086 / [arcsec/pix] Scale for axis 2 at CRPIX1,CRPIX2', $
    'A_ORDER =                    3 / poly order, axis 1, detector to sky', $
    'A_0_2   =           9.0886E-06 / distortion coefficient', $
    'A_0_3   =           4.8066E-09 / distortion coefficient', $
    'A_1_1   =           4.8146E-05 / distortion coefficient', $
    'A_1_2   =          -1.7096E-07 / distortion coefficient', $
    'A_2_0   =             2.82E-05 / distortion coefficient', $
    'A_2_1   =           3.3336E-08 / distortion coefficient', $
    'A_3_0   =          -1.8684E-07 / distortion coefficient', $
    'A_DMAX  =                2.146 / [pixel] maximum correction', $
    'B_ORDER =                    3 / poly order, axis 2, detector to sky', $
    'B_0_2   =           4.1248E-05 / distortion coefficient', $
    'B_0_3   =          -1.9016E-07 / distortion coefficient', $
    'B_1_1   =           1.4761E-05 / distortion coefficient', $
    'B_1_2   =           2.1973E-08 / distortion coefficient', $
    'B_2_0   =          -6.4708E-06 / distortion coefficient', $
    'B_2_1   =          -1.8188E-07 / distortion coefficient', $
    'B_3_0   =           1.0084E-10 / distortion coefficient', $
    'B_DMAX  =                1.606 / [pixel] maximum correction', $
    'AP_ORDER=                    3 / polynomial order, axis 1, sky to detector', $
    'AP_0_1  =           3.6698E-06 / distortion coefficient', $
    'AP_0_2  =          -9.1825E-06 / distortion coefficient', $
    'AP_0_3  =          -3.8909E-09 / distortion coefficient', $
    'AP_1_0  =          -2.0239E-05 / distortion coefficient', $
    'AP_1_1  =          -4.8946E-05 / distortion coefficient', $
    'AP_1_2  =           1.7951E-07 / distortion coefficient', $
    'AP_2_0  =          -2.8622E-05 / distortion coefficient', $
    'AP_2_1  =          -2.9553E-08 / distortion coefficient', $
    'AP_3_0  =           1.9119E-07 / distortion coefficient', $
    'BP_ORDER=                    3 / poly order, axis 2, sky to detector', $
    'BP_0_1  =          -2.1339E-05 / distortion coefficient', $
    'BP_0_2  =           -4.189E-05 / distortion coefficient', $
    'BP_0_3  =           1.9696E-07 / distortion coefficient', $
    'BP_1_0  =           2.8502E-06 / distortion coefficient', $
    'BP_1_1  =          -1.5089E-05 / distortion coefficient', $
    'BP_1_2  =          -2.0219E-08 / distortion coefficient', $
    'BP_2_0  =           6.4625E-06 / distortion coefficient', $
    'BP_2_1  =            1.849E-07 / distortion coefficient', $
    'BP_3_0  =          -7.6669E-10 / distortion coefficient', $
    'END                                                    ']

return
end

pro linearize_irac_pixel_coordinates, header, x, y, xl, yl
; NAME:
;      LINEARIZE_IRAC_PIXEL_COORDINATES
;
; PURPOSE:
;      Return pixel coordinates that are the linearized version of the input
;      coordinates
;
; EXPLANATION:
;      Procedure assumes that the distortion information is written in the 
;      Simple Imaging Polynomial (SIP) convention (Shupe et al. 2005, ADASS 
;      P.3.2.18)
;      
; CALLING SEQUENCE
;      LINEARIZE_IRAC_PIXEL_COORDINATES, HEADER, X, Y, XL, YL
; 
; INPUTS:
;      HEADER: String array containing image header which contains SIP 
;              distortion coefficients
;      X: Scalar or array of input (distorted) pixel x positions
;      Y: Scalar or array of input (distorted) pixel y positions
;
; OUTPUTS:
;      XL: Scalar or array containing linearized (undistorted) x positions on 
;          array
;      YL: Scalar or array containing linearized (undistorted) y positions on 
;          array
;
; REVISION HISTORY:
;      Initial coding S.J. Carey 22 Sep 2008
;-

  if (N_params() ne 5) then begin
    print, 'Syntax -- LINEARIZE_IRAC_PIXEL_COORDINATES, header, $'
    print, '              x, y, xlin, ylin'
    return
  endif
  
; For IRAC, a third order polynomial is used for both axes

; x axis
  a02 = sxpar(header, 'A_0_2', COUNT=ca02)
  a03 = sxpar(header, 'A_0_3', COUNT=ca03)
  a11 = sxpar(header, 'A_1_1', COUNT=ca11)
  a12 = sxpar(header, 'A_1_2', COUNT=ca12)
  a20 = sxpar(header, 'A_2_0', COUNT=ca20)
  a21 = sxpar(header, 'A_2_1', COUNT=ca21)
  a30 = sxpar(header, 'A_3_0', COUNT=ca30)
  
  dx = x - sxpar(header, 'CRPIX1')
  dy = y - sxpar(header, 'CRPIX2')

  if (ca02 * ca03 * ca11 * ca12 * ca20 * ca21 * ca30 gt 0) then $
    xl = a02 * dy * dy + a03 * dy * dy * dy + a11 * dx * dy + $
         a12 * dx * dy * dy + a20 * dx * dx + a21 * dx * dx * dy + $
         a30 * dx * dx * dx $
  else message, 'Header does not contain SIP distortion keywords'

; yaxis      
  b02 = sxpar(header, 'B_0_2', COUNT=cb02)
  b03 = sxpar(header, 'B_0_3', COUNT=cb03)
  b11 = sxpar(header, 'B_1_1', COUNT=cb11)
  b12 = sxpar(header, 'B_1_2', COUNT=cb12)
  b20 = sxpar(header, 'B_2_0', COUNT=cb20)
  b21 = sxpar(header, 'B_2_1', COUNT=cb21)
  b30 = sxpar(header, 'B_3_0', COUNT=cb30)
  
  if (cb02 * cb03 * cb11 * cb12 * cb20 * cb21 * cb30 gt 0) then $
    yl = b02 * dy * dy + b03 * dy * dy * dy + b11 * dx * dy + $
         b12 * dx * dy * dy + b20 * dx * dx + b21 * dx * dx * dy + $
         b30 * dx * dx * dx $
  else message, 'Header does not contain SIP distortion keywords'

; Add back reference pixel position 
  xl = xl + x
  yl = yl + y

return
end

pro distort_irac_pixel_coordinates, header, xl, yl, x, y
; NAME:
;      DISTORT_IRAC_PIXEL_COORDINATES
;
; PURPOSE:
;      Return pixel coordinates that are the distorted version of the input
;      coordinates.  The distorted coordinates are the positions as imaged
;      by IRAC.
;
; EXPLANATION:
;      Procedure assumes that the distortion information is written in the 
;      Simple Imaging Polynomial (SIP) convention (Shupe et al. 2005, ADASS 
;      P.3.2.18)
;      
; CALLING SEQUENCE
;      DISTORT_IRAC_PIXEL_COORDINATES, HEADER, XL, YL, X, Y
; 
; INPUTS:
;      HEADER: String array containing image header which contains SIP 
;              distortion coefficients
;      XL: Scalar or array containing linearized (undistorted) x positions on 
;          array
;      YL: Scalar or array containing linearized (undistorted) y positions on 
;          array
;
; OUTPUTS:
;      X: Scalar or array of distorted pixel x positions
;      Y: Scalar or array of distorted pixel y positions
;
; REVISION HISTORY:
;      Initial coding S.J. Carey 22 Sep 2008
;-

  if (N_params() ne 5) then begin
    print, 'Syntax -- DISTORT_IRAC_PIXEL_COORDINATES, header, $'
    print, '              xlin, ylin, x, y'
    return
  endif

; For IRAC, a third order polynomial is used for both axes
 
  dx = xl - sxpar(header, 'CRPIX1')
  dy = yl - sxpar(header, 'CRPIX2')

; x axis
  ap01 = sxpar(header, 'AP_0_1', COUNT=ca01)
  ap02 = sxpar(header, 'AP_0_2', COUNT=ca02)
  ap03 = sxpar(header, 'AP_0_3', COUNT=ca03)
  ap10 = sxpar(header, 'AP_1_0', COUNT=ca10)
  ap11 = sxpar(header, 'AP_1_1', COUNT=ca11)
  ap12 = sxpar(header, 'AP_1_2', COUNT=ca12)
  ap20 = sxpar(header, 'AP_2_0', COUNT=ca20)
  ap21 = sxpar(header, 'AP_2_1', COUNT=ca21)
  ap30 = sxpar(header, 'AP_3_0', COUNT=ca30)

  if (ca01 * ca02 * ca03 * ca10* ca11 * ca12 * ca20 * ca21 * ca30 gt 0) then $
    x = ap01 * dy + ap02 * dy * dy + ap03 * dy * dy * dy + ap10 * dx + $
         ap11 * dx * dy + ap12 * dx * dy * dy + ap20 * dx * dx + $
         ap21 * dx * dx * dy + ap30 * dx * dx * dx $
  else message, 'Header does not contain SIP distortion keywords'

; yaxis      
  bp01 = sxpar(header, 'BP_0_1', COUNT=cb01)
  bp02 = sxpar(header, 'BP_0_2', COUNT=cb02)
  bp03 = sxpar(header, 'BP_0_3', COUNT=cb03)
  bp10 = sxpar(header, 'BP_1_1', COUNT=cb10)
  bp11 = sxpar(header, 'BP_1_1', COUNT=cb11)
  bp12 = sxpar(header, 'BP_1_2', COUNT=cb12)
  bp20 = sxpar(header, 'BP_2_0', COUNT=cb20)
  bp21 = sxpar(header, 'BP_2_1', COUNT=cb21)
  bp30 = sxpar(header, 'BP_3_0', COUNT=cb30)
  
  if (cb01 * cb02 * cb03 * cb10 *cb11 * cb12 * cb20 * cb21 * cb30 gt 0) then $
    y = bp01 * dy + bp02 * dy * dy + bp03 * dy * dy * dy + bp10 * dy + $
         bp11 * dx * dy + bp12 * dx * dy * dy + bp20 * dx * dx + $
         bp21 * dx * dx * dy + bp30 * dx * dx * dx $
  else message, 'Header does not contain SIP distortion keywords'

; Add back reference pixel position
  x = x + xl
  y = y + yl
  
return
end

pro get_irac_ptg_offsets, channel, x, y, row, col, SUB=sub, $
                          ROWCOL_TO_PIXEL=rowcol_to_pixel,FOV_SUB=fov_sub

; Check number of parameters
	if (N_params() ne 5) then begin
		print, 'Syntax - GET_IRAC_PTG_OFFSETS, channel, x, y, row, col[, /SUB] [,/FOV_SUB]' 
		print, '                                -or-'
		print, '         GET_IRAC_PTG_OFFSETS, channel, row, col, x, y, $'
		print, '                               /ROWCOL_TO_PIXEL[, /SUB] [,/FOV_SUB]'
		print, ''
		print, 'If channel 1 or 3 is specified, it is assumed that the 3.6/5.8'
		print, 'FOV is desired.  For channels 2 or 4, the 4.5/8.0 FOV is used.'
		print, ''
		print, 'If /ROWCOL_TO_PIXEL is set then the transform is from Spot'
		print, 'offsets to BCD pixel positions.'
		print, ''
		print, 'If /SUB is set, then the pixel coordinates input or determined'
		print, 'will be from the subarray FOV for that channel; however, the '
		print, 'row/col offsets will be from the full array pair field of view'
		print, 'unless the keyword /FOV_SUB is set'
		return
	endif
; Error check first parameter, must be 1 through 4
	if (channel lt 1 or channel gt 4) then $
		message, 'Channel must be 1-4'

IF KEYWORD_SET(FOV_SUB) THEN BEGIN
;; Run the code recursively to get offsets relative to the subarray FOV
;; First determine the ROW,COL offsets (in the full array FOV) to get the target on the center of the subarray 
   get_irac_ptg_offsets,channel,16.,16.,row0,col0,SUB=sub
   IF KEYWORD_SET(ROWCOL_TO_PIXEL) THEN BEGIN
;; Here x,y are really input row,col in the subarray FOV
;; Determine the subarray pixel that the target will fall on if the row and column offsets are x+row0, y+row0, relative to Full array  
      get_irac_ptg_offsets,channel,x+row0,y+row0,row,col,SUB=sub,/ROWCOL_TO_PIXEL
   ENDIF ELSE BEGIN
;; Here x,y are input subarray pixel that the target falls on.  ROW, COL give the offsets relative to full array.
      get_irac_ptg_offsets,channel,x,y,row,col,SUB=sub
;; Compute the offsets relative to subarray.      
      row -= row0
      col -= col0
   ENDELSE
   RETURN
   
ENDIF

; Grab appropriate astrometry information
	case channel of
		1: get_irac_astro_header_ch1, header
		2: get_irac_astro_header_ch2, header
		3: get_irac_astro_header_ch3, header
		4: get_irac_astro_header_ch4, header
		else: message, 'Channel must be 1-4'
	endcase
		
; Determine if transformation is from pixel offsets to row/col offsets or
; vice-versa
	if (keyword_set(ROWCOL_TO_PIXEL)) then begin	
; Transform from angular offsets to pixel coordinates 
		adxy, header, x/3600., y/3600., row, col
		
; If subarray keyword is set, then row and col need to be transformed
; to subarray coordinates
		if (keyword_set(SUB)) then begin
; Subarray starts at x coordinate 9 for all channels
			row = row - 8.0D
			if (channel lt 3) then col = col - 216.D $
			else col = col - 8.0D
		endif
; Switch to FITS convention
		row = row + 1.0
		col = col + 1.0
	endif else begin
	
; Transform pixel coordinates to angular offsets
		if (keyword_set(SUB)) then begin
; Need to convert subarray coordinates to full array equivalents
; pixel coordinates need to be in IDL convention (-1 from FITS)
			if (channel lt 3) then xyad, header, x + 7., y + 215., row, col $
			else xyad, header, x + 7., y + 7., row, col
		endif else xyad, header, x - 1.0, y - 1.0, row, col
; Flip the sign of the column offset to match Spot orientation
; This actually seems to be wrong when visualizing!!!
;		col = -col

; Some of the RA-like coordinates get returned as 360 + row
		ptr = where(row gt 180., rcount)
		if (rcount gt 0.) then row[ptr] = row[ptr] - 360.

; Convert to arcseconds		
		row = row * 3600.
		col = col * 3600.
	endelse

return
end

