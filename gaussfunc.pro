function gaussfunc,x,center,fwhm,peak,normal=normal
;
;   return the evaluation of the Gaussian function at X given FWHM 
;   and PEAK intensity, centered on CENTER.  Normalize the integral
;   if keyword NORMAL is set
;
sigma = fwhm/2.354

if keyword_set(NORMAL) then f = exp(-( (x-center)^2 )/(2.*sigma^2) )$
          /(sigma*sqrt(2.*!pi)) else $
                   f = peak*exp(-( (x-center)^2 )/(2.*sigma^2) )

return,f

end
