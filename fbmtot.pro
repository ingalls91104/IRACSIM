pro fbmtot,npix,beta,seed,image,amp,phase,npix_y=npix_y,not_pos=not_pos,$
           noview=noview
;
;  Distributed on an "as is" basis.  No guarantees.  Feel free to hack.
;
;  Inputs:
;   NPIX  number of pixels for square image
;   BETA  power law power spectrum index
;   SEED  random number generator seed
;   NPIX_Y   Optional number of pixels in Y dimension for non-square image
;            (Image will look "stretched" if NPIX NE NPIX_Y)
; 
;   NOT_POS  Optional keyword that says it's okay to have an image with
;            negative values, the natural result of the Fourier transform.
;            If set to zero, or unset, the square of the image will be 
;            returned (IMAGE^2 has the same power spectrum as IMAGE).
;   NOVIEW   Optional keyword that says you don't want to view the
;            final image
;
;  Outputs:
;   IMAGE resulting image
;   AMP   Amplitude image in Fourier domain
;   PHASE Phase image in Fourier domain
;
;   Note: the Fourier Transform of the resulting image is determined 
;   from the formula freq = complex(amp*cos(phase),amp*sin(phase))
;
;   Create a simulated 2D "cloud" using Fractional Brownian noise.
;   The image is output in IMAGE, a NPIX x NPIX array.  BETA is
;   a power law index for the input amplitudes as a function of
;   spatial frequency (Fourier plane).  The higher BETA is, the more 
;   structure there is on large scales, and the less clumpy the image.  
;;
;   The phases are randomized using a random number generator, which takes 
;   an input seed  SEED.  To get a 256 x 256 image which looks a lot like 
;;  an IRAS image, try "fbmtot,256,3.2,seed,image"
;;  Each time you execute this with a variable SEED, you'll get a new
;;  result.  Remember random number generators depend _deterministically_
;;  on the seed, so if you want to reproduce a former result, you
;;  should save the seed FIRST.  On exiting, the value you
;;  entered with is lost, replaced by a new seed.
;;  To initialize the sequence to a known place type "seed=[number]".
;;
;;  Last update January 2004
;;  
;;  Jim Ingalls
;;  Spitzer Science Center
;;  California Institute of Technology
;;  1200 E California Blvd 
;;  Mail Code 220-6
;;  Pasadena, CA 91125
;;
if n_params() eq 0 then begin
    print,'fbmtot,npix,beta,seed,image,amp,phase,npix_y=npix_y,not_pos=not_pos,noview=noview'
    return
endif

;;   Build the image in frequency space
npix_x = npix
if n_elements(NPIX_Y) EQ 0 then npix_y = npix

;d = shift(dist(npix_x,npix_y),-npix_x/2,-npix_y/2)
d = dist(npix_x,npix_y)
notzero=where(d ne 0)

;;; Set the power law amplitude behavior
amp = dblarr(npix_x,npix_y)
amp(notzero) = d(notzero)^(-beta/2d0)
if ~keyword_set(NOVIEW) then plot,d(notzero),amp(notzero)^2,/xlog,/ylog,psym=3

;;; Get random phases
phase = randomu(seed,npix_x,npix_y)*2d0*!dpi

;;; make sure the resulting phases are set for a hermitian matrix,
;;; i.e. the phases are odd:     phase(-x,-y) = -phase(x,y)
phase = phase - shift(rotate(phase,2),1,1) ;;; rotating phase matrix by 180 deg
;phase = phase - rotate(phase,2)

freq = dcomplex(amp*cos(phase),amp*sin(phase))
;fffreq = fft(freq,/inverse)
;image=sqrt(FLOAT(fffreq*conj(fffreq)))
;;;   Convert to spatial frame
image=DOUBLE(fft(freq,/inverse,/double))

if ~keyword_set(NOT_POS) then image=image^2
if ~keyword_set(NOVIEW) then tvscl,image   ; display image

return
end
