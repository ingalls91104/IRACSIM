FUNCTION prf_realize,channel,prf,x_prf,y_prf,xloc,yloc,dxstar=dxstar,dystar=dystar,flux=flux,nx=nx,ny=ny,$
                     r_aper=r_aper,sky_annnulus=sky_annulus,no_normalize=no_normalize,factor=factor,$
                     verbose=verbose,peak_star=peak_star,BG_IMAGE=bg_image,BG_VALUE=bg_value,prf_use=prf_use
;;;  Realize the image of a point source, given the following: 
;;;         CHANNEL: IRAC channel number (1 or 2)
;;;             PRF: the PRF table (or sets of tables for 5x5 positions around IRAC)
;;;   (X_PRF,Y_PRF): the X and Y coordinates at which the PRF table
;;;                  is sampled, in units of the native pixel grid
;;;                  (defined such that x_prf,y_prf=0,0 at the position of the actual point source,
;;;                   which may not be an integer PRF pixel).  
;;;                  This will be updated based on the PRF center positions tabulated herein.
;;; (XLOC,YLOC): the requested baseline location(s) of the star centroid(s)
;;;                  in the native pixel grid.  If XLOC and YLOC
;;;                  are input as NIMG-long arrays, the function will return a
;;;                  set of NIMG images (3-dimensional array with 3rd index
;;;                  being image number), each of which gives a
;;;                  realization based on the given coordinates.
;;;  Optional keyword inputs are as follows:
;;;   (DXSTAR,DYSTAR):  offset locations from XLOC,YLOC for each star (NSTAR elements) NOTE: the variable PRF does not
;;;                     currently account for multiple stars
;;;        BG_VALUE = A single background value that all pixels get
;;;        BG_IMAGE = An NX x NY image of values to give the pixels.  Supercedes BG_VALUE. 
;;;        PEAK_STAR:   peak value of prf for each star (NSTAR elements)
;;;              NX,NY: the number of pixels in the output image(s).
;;;                     Default is 256 x 256.
;;;              FLUX:  the star flux(es) in a sky-subtracted aperture centered on
;;;                     (xloc,yloc).  By default, the flux will be
;;;                     set to 1.0 (the stellar realization will be
;;;                     scaled by whatever factor gives this flux).  
;;;              R_APER: Aperture radius.  Default is 5 pixels.
;;;              SKY_ANNULUS: 2-element array giving the inner and
;;;                           outer radii over which to compute
;;;                           aperture photometry.  Default is [5,10]
;;;              /NO_NORMALIZE: Don't normalize the flux
;;;              FACTOR: multiply each realization by this number.  If given as an NIMG
;;;                      array, multiply each realization by the corresponding element of FACTOR.
;;;
;;; Ratio of distance in prf coordinates to the same distance in native pixel coordinates:
delta_x_pix_prf = 1./(x_prf[1,0]-x_prf[0,0])
delta_y_pix_prf = 1./(y_prf[0,1]-y_prf[0,0])

nimg = N_ELEMENTS(xloc) < N_ELEMENTS(yloc)
nstar = N_ELEMENTS(dxstar) < N_ELEMENTS(dystar) < N_ELEMENTS(peak_star)
IF NSTAR EQ 0 THEN BEGIN
   nstar = 1
   IF N_ELEMENTS(peak_star) EQ 0 THEN peak_star = 1.0
   IF N_ELEMENTS(dxstar) EQ 0 THEN dxstar = 0.0
   IF N_ELEMENTS(dystar) EQ 0 THEN dystar = 0.0
ENDIF
   
IF N_ELEMENTS(FLUX) EQ 0 then FLUX = MAKE_ARRAY(nimg,/FLOAT,VALUE=1.0) $
ELSE BEGIN
   IF N_ELEMENTS(FLUX) LT nimg THEN FLUX = MAKE_ARRAY(nimg,/FLOAT,VALUE=FLUX[0])
ENDELSE
IF N_ELEMENTS(NX) EQ 0 THEN nx = 256
IF N_ELEMENTS(NY) EQ 0 THEN ny = 256
IF nx GT 64 OR ny GT 64 THEN MISSING=0.0 ELSE MISSING=!NULL
IF N_ELEMENTS(R_APER) EQ 0 THEN r_aper = 3
IF N_ELEMENTS(SKY_ANNULUS) LT 2 THEN SKY_ANNULUS = [3,7]
CASE N_ELEMENTS(FACTOR) OF
    1: FACTOR = REPLICATE(factor,nimg)
    0: FACTOR = REPLICATE(1.0,nimg)
    ELSE:
ENDCASE
bg_img = DBLARR(nx,ny)
IF N_ELEMENTS(BG_IMAGE) NE 0 THEN bg_img = BG_IMAGE $
     ELSE BEGIN
       IF N_ELEMENTS(BG_VALUE) NE 0 THEN bg_img[*] = BG_VALUE
     ENDELSE 
;; Deal with multiple PRF's
sz = SIZE(prf)
nx_prf = sz[1]
ny_prf = sz[2]
nprfs = sz[0] EQ 3 ? sz[3] : 1
IF NPRFS GT 1 THEN BEGIN
   prf_rowcol = [25,77,129,181,233] -1 
   xave = MEAN(xloc + dxstar[0])
   yave = MEAN(yloc + dystar[0])
   dum = MIN(ABS(xave-prf_rowcol),i0)
   IF i0 EQ 4 THEN i0=3
   i1 = i0+1
   dum = MIN(ABS(yave-prf_rowcol),j0)
   IF j0 EQ 4 THEN j0=3
   j1 = j0+1
;; I checked these are very similar between warm and cold
   CASE channel OF
      1: BEGIN
      ;CH1
           prf_xcen = TRANSPOSE([ [64.9772,64.8530,64.8238,64.8502,64.9615],[64.6561,64.6596,64.6813,64.6414,64.6295],$
                                  [64.4804,64.5032,64.5190,64.4824,64.4404],[64.3060,64.2781,64.2536,64.2623,64.2713],$
                                  [64.1216,64.1126,64.0868,64.1009,64.0886] ])
           prf_ycen = TRANSPOSE([ [64.2023,63.9157,63.6696,63.3958,63.0523],[64.0970,63.9045,63.6680,63.4136,63.1648],$
                                  [64.0841,63.9514,63.6535,63.3601,63.1826],[64.0773,63.9507,63.6465,63.3461,63.1749],$
                                  [64.0674,63.8812,63.6430,63.3926,63.1533] ])
      END
      2: BEGIN
      ;CH2
           prf_xcen = TRANSPOSE([ [62.9576,62.9114,63.9345,62.9312,62.9576],[62.6333,63.6674,62.7097,62.6926,62.6625],$
                                [62.3564,62.3565,62.3573,62.3701,62.3878],[62.0746,62.0760,62.0496,62.0865,62.1259],$
                                [61.7113,61.8171,61.8369,61.8444,61.7963] ])
           prf_ycen = TRANSPOSE([ [64.2350,63.8873,63.5784,63.2649,62.9350],[64.1912,63.9294,63.5956,63.2360,62.9651],$
                                  [64.1964,63.9520,63.6119,63.2258,62.9685],[64.2055,63.9242,63.6099,63.2687,62.9873],$
                                  [64.2869,63.9169,63.6053,63.2966,62.9416] ])
      END
   ENDCASE
   ;; Create the interpolation weights
   a = (xave-prf_rowcol[i0])/(prf_rowcol[i1]-prf_rowcol[i0])
   b = (yave-prf_rowcol[j0])/(prf_rowcol[j1]-prf_rowcol[j0])
   c00 = 1 + a*(b-1) -b 
   c01 = b*(1-a)
   c10 = a*(1-b)
   c11 = a*b
   ;; Interpolate the center locations, and the PRF
   xcen = prf_xcen[i0,j0]*c00 + prf_xcen[i0,j1]*c01 + prf_xcen[i1,j0]*c10 + prf_xcen[i1,j1]*c11
   ycen = prf_ycen[i0,j0]*c00 + prf_ycen[i0,j1]*c01 + prf_ycen[i1,j0]*c10 + prf_ycen[i1,j1]*c11
   prf1 = TRANSPOSE(REFORM(prf,nx_prf,ny_prf,5,5),[0,1,3,2])
   prf_use = REFORM(prf1[*,*,i0,j0]*c00 + prf1[*,*,i0,j1]*c01 + prf1[*,*,i1,j0]*c10 + prf1[*,*,i1,j1]*c11) 
   x_prf = ( (dindgen(nx_prf) - xcen)/delta_x_pix_prf ) # (fltarr(ny_prf) + 1.0)
   y_prf =  (fltarr(nx_prf) + 1.0) # ( (dindgen(ny_prf) - ycen)/delta_y_pix_prf )
ENDIF ELSE BEGIN
   prf_use = prf
ENDELSE


;;; Initialize the star images with the background.
IF NIMG GT 1 THEN star_img = MAKE_ARRAY(nx,ny,nimg) ELSE star_img = MAKE_ARRAY(nx,ny)
FOR i = 0,nimg-1 DO star_img[*,*,i] = bg_img

maxprf = MAX(prf)
;;; The prf tabulates the star image at a given set of x and y
;;; coordinates wrt the pixel grid.  Compute the star image at the
;;; coordinates corresponding to the shifted position of the star
;;; centroid using interpolation.  For pixels outside the prf grid,
;;; set the star intensity to zero.
;;;
IF KEYWORD_SET(VERBOSE) EQ 1 THEN print,'Making '+STRNG(nimg)+' star realizations'
FOR I = 0,nimg-1 DO BEGIN
   IF KEYWORD_SET(VERBOSE) EQ 1 AND i mod 100 EQ 0 THEN print,'# '+STRNG(i)+'  ('+STRNG(xloc[i])+','+STRNG(yloc[i])+')'
   FOR J = 0,nstar-1 DO BEGIN
;;; Coordinates of each native pixel relative to the desired star position
      xgrid = findgen(nx) # (fltarr(ny) + 1.0) - xloc[i] - dxstar[j]
      ygrid = (fltarr(nx) + 1.0) # findgen(ny) - yloc[i] - dystar[j]
;;; Coordinates of each native pixel in prf pixel coordinates
      xgrid_prf = (xgrid - x_prf[0]) * delta_x_pix_prf
      ygrid_prf = (ygrid - y_prf[0]) * delta_y_pix_prf

;;; Individual Star realization.  
;;; Sample PRF at the set of interpolated pixel positions (XGRID_PRF,YGRID_PRF), using cubic convolution:   
     ;im = INTERPOLATE(PRF,XGRID_PRF,YGRID_PRF,CUBIC=-0.5,MISSING=0.0) * factor[i]
     ;; Remove MISSING keyword to set a constant value (the value at the edge)
     ;; for pixels outside the sampled region.
     im = INTERPOLATE(PRF_USE,XGRID_PRF,YGRID_PRF,MISSING=missing,CUBIC=-0.5) * factor[i]  ;; Turned off CUBIC=-0.5 for now to avoid possible use of multiple entries for the same phase JGI 4/16/14
;
;;  Sample PRF at the set of interpolated pixel positions (XGRID_PRF,YGRID_PRF), using bilinear interpolation:
   ;im = INTERPOLATE(PRF,XGRID_PRF,YGRID_PRF,MISSING=0.0) * factor[i]
   
;;; Normalize image so that the flux is as required by aperture
;;; photometry 
      IF KEYWORD_SET(NO_NORMALIZE) EQ 0 THEN BEGIN
         aper,im,xloc[i],yloc[i],aper_flux,$
              errflux,sky,skyerr,1.0,r_aper,sky_annulus,$
              /NAN,/FLUX,/EXACT,/silent,/MEANBACK
         star_img[*,*,i] += im * flux[i]/aper_flux[0]
      ENDIF ELSE star_img[*,*,i] = star_img[*,*,i] + im/maxprf * peak_star[j]   ;; normalize to the input peak value for the star
   ENDFOR   
ENDFOR

IF KEYWORD_SET(verbose) EQ 1 THEN PRINT,'Done making star realization'
RETURN,star_img
END
   
;REALIZING POINT SOURCE IMAGES USING AN IRAC PRF
;(one way to do it)
;
;      The PRF is stored as an "image" but is not a real point source image, rather a set 
;      of interleaved point source images on a given IRAC array, tabulated for a set of
;      pixel phases.  Interleaving the images can lead to confusion.  Many people think the PRF
;      is a "fully sampled" point source image, sampled on sub-pixel spacings, that has to be 
;      integrated to determine the value in each pixel -- actually it is a set of images already
;      integrated over each pixel.  The benefit of interleaving is that it also allows us to 
;      interpolate the images conveniently between pixel phases.
;       
;      The first thing to do is define a reference frame of the point source.  I define the "center" 
;      of the PRF by doing a center-of-light centroid on the PRF itself, (falsely) treating it as an image.  
;      This defines the position (xcnt_prf,ycnt_prf) on the PRF grid of the "center" of a point source, 
;      which doesn't have to be an integer.  I then determine, in native IRAC array pixels, the 
;      coordinates of all other "pixels" of the PRF relative to that.  Basically, this tells us 
;      how far away from the center of the point source a given PRF "pixel" lies, in native pixel units.
;
;      If prf_x_rsmp and prf_y_rsmp are the PRF resampling factors (the number of PRF samples per 
;      native pixel) then the X and Y coordinates of the PRF grid are
;
;  (1)             X_PRF = (DINDGEN(nx_prf) - xcnt_prf)/prf_x_rsmp
;                  Y_PRF = (DINDGEN(ny_prf) - ycnt_prf)/prf_y_rsmp
;
;      The code PRF_READ.PRO does this. The usage is:
;
;            IDL> PRF_READ,'IRAC1_col025_row233.fits',x_prf,y_prf,prf,/VERBOSE
;
;      I use the 5x oversampled PRF, not the 100x oversampled because the 100x is a linearly 
;      interpolated version of the 5x, and my code PRF_REALIZE.PRO does cubic convolution 
;      interpolation, which gives results closer to the "best" theoretical approach, sinc 
;      interpolation.
;
;      Given (X_PRF,Y_PRF), we can place the image of a point source at any point on an IRAC
;      array (XOFF,YOFF) by determining the locations of each native pixel on the IRAC array with 
;      respect to the center of the point source at that point. The locations are simply
;
;  (2)            XGRID = DINDGEN(nx) - XOFF
;                 YGRID = DINDGEN(ny) - YOFF
;
;      If we are using the subarray and the source is at the center, NX=NY=32 and XOFF=YOFF=15.5.  
;
;      To create the image of a point source, we interpolate the PRF onto this grid.  Remember that
;      the PRF is tabulated such that (X_PRF,Y_PRF) gives the native pixel distances from the center 
;      of the point source.  We thus have to interpolate the PRF from (X_PRF,Y_PRF) to (XGRID,YGRID).
;      IDL's INTERPOLATE routine requires the interpolated locations to be input relative to the grid 
;      of the input "image" to be interpolated.  So we convert the desired grid locations XGRID,YGRID 
;      into the subsampled array coordinates of the PRF (the bottom left corner of the PRF has coordinates 
;      (0,0) in this system) by subtracting off the native pixel location of the bottom left corner of the PRF 
;      (X_PRF[0],Y_PRF[0]), and scaling the native pixels to PRF "pixels":
;
;  (3)             XGRID_PRF = (XGRID - X_PRF[0]) * prf_x_rsmp
;                  YGRID_PRF = (YGRID - Y_PRF[0]) * prf_y_rsmp
;
;      Now we can create the realization of the point source:
;
;  (4)             image = INTERPOLATE(prf,xgrid_prf,ygrid_prf,CUBIC=-0.5,MISSING=0.0)

;      By setting the keyword MISSING in INTERPOLATE, we automatically set to 0 all pixels in the IRAC array 
;      which are outside the tabulated PRF, avoiding the need to truncate the PRF.
;
;The code PRF_REALIZE.pro does steps (2-4):
;
;IDL> star_image = PRF_REALIZE(prf,x_prf,y_prf,xoff,yoff,nx=32,ny=32,/verbose)


   
