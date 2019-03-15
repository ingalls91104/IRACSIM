PRO prf_read,channel,x,y,prf,prf_x_rsmp,prf_y_rsmp,dir=dir,PRF_FILE=prf_file,WARM_MISSION=warm_mission,verbose=verbose,nocenter=nocenter,PRF_PEAK_INTEGRAL=prf_peak_integral
;;
;;   Read a point response function file and produce X,Y,PRF tables
;;   for making stellar realizations.  Find centroid of PRF.  Convert
;;   to native pixel coordinate units (X,Y), centered on the star.
;;   The arrays X and Y allow us to cast the prf in terms of
;;   a native pixel coordinate representation of the star.
;;
;;  2014 April 8 KJM: Iterative determination of best self-consistent PRF center using box_centroider and PRF realizations.
;;  2014 June 12 JGI: Allow the reading of multiple PRFs, and the ability to not iteratively center them (/NOCENTER)
;;                    PRF_REALIZE now contains the tabulated centers, and will interpolate between PRFs based on the position
;;                    on the array (for full array)  
;;  2016 April 8 JGI: Compute integral of peak PRF realization, in current PRF units, output in PRF_PEAK_INTEGRAL keyword.
;;                         
;;  
IF N_ELEMENTS(DIR) EQ 0 THEN BEGIN
  WHICH,'PRF_READ',PATH=this_file_loc,/SILENT
  base_dir = FILE_DIRNAME(this_file_loc,/MARK) 
  sub_dir = KEYWORD_SET(WARM_MISSION) ? '140915_warm_prfs' : $
                                    '070131_prfs'
  dir = base_dir + sub_dir                                  
ENDIF
prf_full_file = KEYWORD_SET(PRF_FILE) ? dir + path_sep() + prf_file[0] : $
                                   FILE_SEARCH(dir + path_sep() + 'IRAC'+strng(channel)+'_col*_row*.fits')
nfiles = N_ELEMENTS(prf_full_file)
head = HEADFITS(prf_full_file[0])
prf_x_rsmp = SXPAR(head,'PRFXRSMP')
prf_y_rsmp = SXPAR(head,'PRFYRSMP')
nx = SXPAR(head,'NAXIS1')
ny = SXPAR(head,'NAXIS2')
prf = DBLARR(nx,ny,nfiles)
prf_peak_integral_array = DBLARR(nfiles)
IF N_ELEMENTS(nocenter) EQ 0 THEN nocenter = nfiles GT 1
xcnt_array = DBLARR(nfiles)
ycnt_array = DBLARR(nfiles)
FOR i = 0,nfiles-1 DO BEGIN
   IF KEYWORD_SET(VERBOSE) THEN PRINT,'PRF file: '+prf_full_file[i]+'.'
   prf[*,*,i] = READFITS(prf_full_file[i],head,/SILENT)
   xcnt = nx/2;62 ;KJM: which *should* be the middle of the pixel at (15,15)
   ycnt = ny/2;62 ;KJM: which *should* be the middle of the pixel at (15,15)
   IF ~KEYWORD_SET(nocenter) THEN BEGIN
;verbose = 1 ;KJM

;;; PRF resampling factor (this number of prf pixels equals 1 native
;;; instrument pixel)

      search_x0 = nx/2.0
      search_y0 = ny/2.0
      geomean_rsmp = SQRT(prf_x_rsmp*prf_y_rsmp)
      boxhalfsize = 5 * geomean_rsmp
      backbox = [5,10] * geomean_rsmp    
;;; X and Y pixel arrays giving the fractional pixel locations of the
;;; prf grid, in native pixel coordinate units, centered on the
;;; star
;;;

;;KJM: good (for 070131_prfs/IRAC1_col025_row233.fits)
;xcnt = 62 ;JI
;ycnt = 62 ;JI

;;KJM: better (for 070131_prfs/IRAC1_col025_row233.fits)
;xcnt = 64 ;KJM: DS9 shows peak at (65,64) 
;ycnt = 63 ;KJM: ==> (64,63) [C/IDL zero-offset coordinates]

;;KJM: even better (for 070131_prfs/IRAC1_col025_row233.fits)
;;KJM: median DeltaX= -0.0458 px and median DeltaY= -0.0402 px 
;xcnt = 64.37 ;KJM: IRAF rimexam with radius=15, "a" command:
;ycnt = 63.25 ;KJM: center at (65.37,64.25) [IRAF/FITS one-offset coordinates]

;KJM: We can do a lot better through iteration
      jend = 9
;;KJM: best solution (for 070131_prfs/IRAC1_col025_row233.fits):
; For xcnt=62 and ycnt=62 and jend=9: 
; (xcnt,ycnt) = (64.9666,63.0539) gives a realization BOC_CENTROID center at (15.0000,15.0000)
      if keyword_set(verbose) then print,'BC: xcnt, ycnt:', xcnt, ycnt ;KJM
      for j = 0, jend do begin ;KJM
         x_provisional = ( (dindgen(nx) - xcnt[0])/prf_x_rsmp ) # (fltarr(ny) + 1.0)
         y_provisional =  (fltarr(nx) + 1.0) # ( (dindgen(ny) - ycnt[0])/prf_y_rsmp )
;; Realize a point source at (15,15) using the expected "center of pixel" PRF realization 
         realization = prf_realize(channel,REFORM(prf[*,*,i]),x_provisional,y_provisional,15,15,nx=32,ny=32,/no_normalize)
;; Determine the actual centroid position of the realization
         box_centroider,realization,replicate(1.0,nx,ny),15,15,3,6,3,$
                        xcnt_realiz,ycnt_realiz,f_box,f_bg,xerr,yerr,f_box_unc,f_bg_unc,ngood,ngood_bg,$
                        nnoise,xfwhm,yfwhm,/TWOPASS
         mnmx = minmax(realization)
        rmx = mnmx[1]
        rtot = total(realization)
        if keyword_set(verbose) then print,'BC: centroid, noisepx, flux:', xcnt_realiz, ycnt_realiz, nnoise, f_box, rmx, rtot, long(ngood), long(ngood_bg), j ;KJM
;; Where it goes from (15,15) specifies the actual point source center.
        xcnt += (xcnt_realiz[0]-15)*prf_x_rsmp
        ycnt += (ycnt_realiz[0]-15)*prf_y_rsmp
        prf_peak_integral = f_box
        if keyword_set(verbose) then print,'BC: xcnt, ycnt:', xcnt, ycnt ;KJM
        xcnt_array[i] = xcnt
        ycnt_array[i] = ycnt
        prf_peak_integral_array[i] = prf_peak_integral
      endfor ;KJM
      IF KEYWORD_SET(verbose) THEN PRINT,prf_full_file[i] +': PRF Centroid found at ('+STRNG(xcnt)+','+STRNG(ycnt)+').'
   ENDIF
ENDFOR

IF ~KEYWORD_SET(nocenter) AND KEYWORD_SET(VERBOSE) AND N_ELEMENTS(xcnt_array) EQ 25 THEN BEGIN
  PRINT,'X Centroid : [$'
  xcnt_array = REFORM(xcnt_array,5,5)
  FOR i = 0,4 DO PRINT,'['+STRJOIN(STRNG(xcnt_array[*,i],format='(f7.4)'),',')+'],$'
  PRINT,']'

  PRINT,'Y Centroid : [$'
  ycnt_array = REFORM(ycnt_array,5,5)
  FOR i = 0,4 DO PRINT,'['+STRJOIN(STRNG(ycnt_array[*,i],format='(f7.4)'),',')+'],$'
  PRINT,']'

  PRINT,'PRF Peak Integral : [$'
  prf_peak_integral_array = REFORM(prf_peak_integral_array,5,5)
  FOR i = 0,4 DO PRINT,'['+STRJOIN(STRNG(prf_peak_integral_array[*,i],format='(f7.4)'),',')+'],$'
  PRINT,']'
ENDIF
x = ( (dindgen(nx) - xcnt[0])/prf_x_rsmp ) # (fltarr(ny) + 1.0)
y =  (fltarr(nx) + 1.0) # ( (dindgen(ny) - ycnt[0])/prf_y_rsmp )

;; Obtain PRF realization that includes the peak, to estimate integral of normalized PRF
;max_prf = MAX(prf[*,*,0],imax,/NAN)
;PRF /= max_prf
;max_Indices = ARRAY_INDICES(REFORM(prf[*,*,0]),imax)
;x0 = max_Indices[0] mod prf_x_rsmp
;y0 = max_Indices[1] mod prf_y_rsmp
;xindices = (indgen(26) * 5 + x0) # MAKE_ARRAY(26,value=1)
;yindices = MAKE_ARRAY(26,value=1) # (indgen(26) * 5 + y0) 
;izero = WHERE(xindices GT 127 OR yindices GT 127,nzero)
;peak_realization = prf[xindices,yindices]
;if nzero NE 0 THEN peak_realization[izero] = 0
;prf_peak_integral = TOTAL(peak_realization,/DOUBLE,/NAN)

;cr = ''
;READ,cr,prompt='CR to continue'
RETURN 
END

