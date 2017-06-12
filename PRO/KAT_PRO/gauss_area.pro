function gauss_area, a, error=error

;+
; NAME:
;	gaussfit_area
;
; Purpose:
;	Calculate the area of a Gaussian from the properties, formatted to accept the parameters
;       expelled by gaussfit.pro in the optional 'a' output
;	   a[0] = height
;	   a[1] = center position
;	   a[2] = sigma, where FWHM=2.*sqrt(2.*alog(2))*a[2]   
;	   a[3:5] = background fitting parameters. 
;	
; Input:
;	a - A array containing a minimum of 3 elements with the properties of a Gaussian.
;		a=[height, center position, sigma]
;           Note that the center position is irrelevant in the area calculation, but is 
;           included so the output from gaussfit.pro can easily be passed. 
;
; Result:
;	Area of a Gaussian, which is equal to the height*sigma width (not FWHM)*sqrt(2.0*!pi).
;
; By: Dr. Kat Barger Dec 2012
;-

;The width of the line should always be positive, 
;  but sometimes fitting routines declare it negitive due to a square in the funciton.
a[2]=abs(a[2])

area=a[0]*a[2]*sqrt(2.0*!pi)

if keyword_set(error) then begin
   ;All errors should be positive. 
   error=abs(error)
   
   area_err1=a[2]*error[0]
   area_err2=a[0]*error[2]
   area_error=sqrt(area_err1^2.0+area_err2^2.0)*sqrt(2.0*!pi)

   return, [area,area_error];
 
endif

return, area;

end