function weight_stdev, data, data_err

;+
; Purpose - To calculate the weighted standard deviation. 
;           The typical standard deviation measures the 
;           deviation from the mean, this measures the 
;           deviation from the weighted average.
;
; CALLING SEQUENCE:
;       RESULT = WEIGHT_AVE(DATA, DATA_ERR)
;
; INPUTS:
;       DATA = A data array
;       DATA_Err = An array with the Errors of Data
;
; BY: Dr. Kat Barger April 2013
;
;-

if total(data_err) eq 0. then return,stdev(data);

data=float(data)
data_err=float(data_err)
weight=1.0/(data_err^2.0)

weight_stdev=fltarr(2)

;weighted average
weight_average=weight_ave(data,data_err)

tmp=where(weight ne 0,N_prime)
N_prime=float(N_prime)

top=total(weight*(data-weight_average[0])^2.0)
bottom=(N_prime-1)*total(weight)/N_prime
weight_stdev=sqrt(top/bottom)


return, weight_stdev;

end