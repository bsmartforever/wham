function axis_line_up, loc, data, range1

; The purpose of this program is to calculate the axis range necessary to 
; line up two values on opposite axes when using the axis function to plot 
; to different data sets that are over different ranges and are scaled 
; differently. This is useful when comparing HI and H-alpha data such 
; that the baselines line up.
;
; Inputs:
; loc - A float that contains the location on the two axes that should 
;       line up.
; data - An array of the data to be plotted on this axis.
; range1 - The range of the first axis plotted (e.g. [min,max]). 
;         This range is easiest acquired by passing '!y.crange' 
;         or '!x.crange'. These are key word to the !y and !x 
;         structure that idl creates after a plot is produced. 
;
; Outputs:
; range2 - Range for new axis.

max_data=max(data)+(max(data)-min(data))/10.0
min_data=min(data)-(max(data)-min(data))/10.0

if range1(0) gt range1(1) then begin
   temp=range1(0)
   range1(0)=range1(1)
   range1(1)=range1(0)
endif

delta_max=range1(1)-loc
delta_min=loc-range1(0)
fraction_max=delta_max/(range1(1)-range1(0))
fraction_min=delta_min/(range1(1)-range1(0))

range2=fltarr(2)

If fraction_max lt 0.5 then begin
range2(1)=loc+(fraction_max*min_data-loc)/(fraction_max-1.0)
range2(0)=min_data
endif else begin
   range2(1)=max_data
   range2(0)=loc+(fraction_min*max_data-loc)/(fraction_min-1.0)
endelse

;read,'kill here: ',die

   return,range2;

end