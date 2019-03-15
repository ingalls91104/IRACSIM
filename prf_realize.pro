FUNCTION prf_realize,channel,prf,x_prf,y_prf,xloc,yloc,dxstar=dxstar,dystar=dystar,flux=flux,nx=nx,ny=ny,$
                     r_aper=r_aper,sky_annnulus=sky_annulus,no_normalize=no_normalize,factor=factor,linear=linear,$
                     verbose=verbose,peak_star=peak_star,BG_IMAGE=bg_image,BG_VALUE=bg_value,prf_use=prf_use,WARM=WARM
;;;  Realize the image of a point source, given the following: 
;;;         CHANNEL: IRAC channel number (1 or 2)
;;;             PRF: the PRF table (or sets of tables for 5x5 positions around IRAC)
;;;   (X_PRF,Y_PRF): the X and Y coordinates at which the PRF table
;;;                  is sampled, in units of the native pixel grid
;;;                  (defined such that x_prf,y_prf=0,0 at the position of the actual point source,
;;;                   which may not be an integer PRF pixel).  
;;;                  This will be updated based on the PRF center positions tabulated in PRF_CHOOSE.
;;; (XLOC,YLOC): the requested baseline location(s) of the star centroid(s)
;;;                  in the native pixel grid.  If XLOC and YLOC
;;;                  are input as NIMG-long arrays, the function will return a
;;;                  set of NIMG images (3-dimensional array with 3rd index
;;;                  being image number), each of which gives a
;;;                  realization based on the given coordinates.
;;;                  If multiple PRFs are input, each location (XLOC+DXSTAR,YLOC+DYSTAR)
;;;                  will have its realization produced from an average of the
;;;                  PRF set, weighted by distance. 
;;;  Optional keyword inputs are as follows:
;;;   (DXSTAR,DYSTAR):  offset locations from XLOC,YLOC for each star (NSTAR elements) 
;;;        BG_VALUE = A single background value that all pixels get
;;;        BG_IMAGE = An NX x NY image of values to give the pixels.  Supercedes BG_VALUE. 
;;;        PEAK_STAR:   peak value of prf for each star (NSTAR elements)
;;;        NX,NY: the number of pixels in the output image(s).
;;;               Default is 256 x 256.
;;;        FLUX:  the star flux(es) in a sky-subtracted aperture centered on
;;;               (xloc,yloc).  By default, the flux will be
;;;               set to 1.0 (the stellar realization will be
;;;               scaled by whatever factor gives this flux).  
;;;        R_APER: Aperture radius.  Default is 5 pixels.
;;;        SKY_ANNULUS: 2-element array giving the inner and
;;;                     outer radii over which to compute
;;;                     aperture photometry.  Default is [5,10]
;;;        /NO_NORMALIZE: Don't normalize the flux
;;;        FACTOR: multiply each realization by this number.  If given as an NIMG
;;;                array, multiply each realization by the corresponding element of FACTOR.
;;;        /LINEAR: use linear interpolation instead of cubic convolution to realize the stellar images
;;;                 in between grid points.  
;;;        /WARM:  set this if you are inputting multiple PRFs for the warm mission and will therefore use the tabulated centroids
;;;                when calling PRF_CHOOSE  
;;;
;;; Ratio of distance in prf coordinates to the same distance in native pixel coordinates:
;;;
;;;   13 Jan 2017: add keyword /WARM.  Move PRF weighted averaging to function PRF_CHOOSE. Allow for separate weighted averaging of
;;;                PRFs for each stellar realization.    
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
IF KEYWORD_SET(VERBOSE) EQ 1 THEN print,'Making '+STRNG(nimg)+' realizations'
FOR I = 0,nimg-1 DO BEGIN
   IF KEYWORD_SET(VERBOSE) EQ 1 AND i mod 100 EQ 0 THEN print,'# '+STRNG(i)+'  ('+STRNG(xloc[i])+','+STRNG(yloc[i])+')'
   FOR J = 0,nstar-1 DO BEGIN
      IF KEYWORD_SET(VERBOSE) EQ 1 and nstar GT 1 THEN  print,'   * '+STRNG(j)+'  ('+STRNG(xloc[i]+dxstar[j])+','+STRNG(yloc[i]+dystar[j])+'): '+STRNG(peak_star[j])
      ;;; Choose the appropriate PRF for each stellar location (will be a single PRF for subarray)
      prf_use = prf_choose(channel,prf,xloc[i]+dxstar[j],yloc[i]+dxstar[j],delta_x_pix_prf,delta_y_pix_prf,x_prf,y_prf,WARM=warm)
;;; Coordinates of each native pixel relative to the desired star position
      xgrid = findgen(nx) # (fltarr(ny) + 1.0) - xloc[i] - dxstar[j]
      ygrid = (fltarr(nx) + 1.0) # findgen(ny) - yloc[i] - dystar[j]
;;; Coordinates of each native pixel in prf pixel coordinates
      xgrid_prf = (xgrid - x_prf[0]) * delta_x_pix_prf
      ygrid_prf = (ygrid - y_prf[0]) * delta_y_pix_prf

;;; Individual Star realization.  
;;; Sample PRF at the set of interpolated pixel positions (XGRID_PRF,YGRID_PRF), using cubic convolution or linear interpolation:   
     ;; Remove MISSING keyword to set a constant value (the value at the edge)
     ;; for pixels outside the sampled region.
     ;; Set /LINEAR to turn off CUBIC=-0.5 and avoid possible use of multiple entries for the same phase
     im = KEYWORD_SET(LINEAR) ? INTERPOLATE(PRF_USE,XGRID_PRF,YGRID_PRF,MISSING=missing) * factor[i] : $
                                INTERPOLATE(PRF_USE,XGRID_PRF,YGRID_PRF,MISSING=missing,CUBIC=-0.5) * factor[i]  
;
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

IF KEYWORD_SET(verbose) EQ 1 THEN PRINT,'Done making images'
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
;      The first thing to do is define a reference frame of the point source. 
;      
;      --> INCORRECT NO LONGER DO THIS I define the "center" 
;      of the PRF by doing a center-of-light centroid on the PRF itself, (falsely) treating it as an image.  
;      
;      
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


   
