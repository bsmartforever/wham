function weight_ave, data, data_err
;+
; NAME:
;       WEIGHT_AVE
; PURPOSE:
;       Returns a 2d array that contains the weighted average and its error.
;
; CALLING SEQUENCE:
;       RESULT = WEIGHT_AVE(DATA, DATA_ERR)
;
; INPUTS:
;       DATA = A data array
;       DATA_Err = An array with the Errors of Data
;
; BY: Kat Barger August 2010
;-

data=float(data)
data_err=float(data_err)

good=where((finite(data) eq 1) AND (finite(data_err) eq 1))

data=data[good]
data_err=data_err[good]

tmp=where(data_err eq 0.0,zeros)
if n_elements(data_err) eq zeros then return, [avg(data),0.0]

weight=1.0/(data_err^2.0)
weight_average=fltarr(2)

;weighted average
weight_average[0]=total(weight*data)/total(weight)
;error of weighted average
weight_average[1]=1.0/sqrt(total(weight))

return, weight_average

END