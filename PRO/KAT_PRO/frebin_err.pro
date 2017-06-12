function frebin_err,array,length
;
; Purpose - To calculate the uncertainty propagation of data 
;           rebinned with frebin.pro.
;
; array - uncertainty array for data array.
; length - array size to rebin the uncertainty array. 
;
; Note:  
; See documentation for oploterror.pro, where they simply
; calculate propagated uncertainties for frebin data by 
; uncertainty = frebin(errors,new array size)/(old array size/new array size)

factor=double(n_elements(array))/double(length)

   return,frebin(array,length)/sqrt(factor)

end