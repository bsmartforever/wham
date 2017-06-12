function rm_outliers, spectra

;Purpose: Clean ons and offs of spectra that deviate from the norm.
;
;Spectra is a structure containing multiple on or offs.

num=n_elements(spectra)
if num lt 5 then begin
   print,''
   print,'*** Only use this program for more than 5 spectra. ***'
   print,''
   return,spectra;
endif 

values=total(transpose(spectra.data),2)
sigma=stdev(total(transpose(spectra.data),2))
middle=median(values)
good_loc=where((values lt middle+sigma) and (values gt middle-sigma),good)
if good ne 0 then spectra=spectra[good_loc]
if good eq 0 then print, 'Skipping rm_outliers.pro'

   return,spectra;

end