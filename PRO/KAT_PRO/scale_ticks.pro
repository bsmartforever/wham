function scale_ticks, range, nticks

; The purpose of this program is to calculate the desired tick location 
; to be used in a simple plotting routine based on the range and the 
; requested number of ticks. This is especially useful for making multi
; plots where the ymax and ymin from each plot can overlap in a visually 
; unappealing way.
;
; Inputs:
; range - An array containing the range of axes that the ticks will 
;         be applied to (e.g. [min,max]). This range is easiest 
;         acquired by passing '!y.crange' or '!x.crange'. These 
;         are key word to the !y and !x structure that idl creates 
;         after a plot is produced. 
;            Step 1: Create plot with no axes labels 
;            Step 2: Calculate desired tick position
;            Step 3: Apply ticks to plot         
; nticks - The number of ticks to be applied to axis  
;
; Outputs:      
;	 Array with new tick labels
;
; Example:
;        temp=scale_ticks([1,5],4)
;        print,temp
;        1.80000      2.60000      3.40000      4.200

height=abs(range(1)-range(0))
;Check to see if the range passes through 0 or not. IDL likes to label 0 
;whether it is desirable or not
If ((range(0) ge 0) and (range(1) ge 0)) or ((range(0) le 0) and (range(1) le 0)) then begin
   delta_tick=(height)/(nticks+1.0)
   tickarr=range(0)+(indgen(nticks)+1.0)*delta_tick
endif else begin
   delta_tick=(height)/(nticks+1.0)
   tickarr=range(0)+(indgen(nticks)+1.0)*delta_tick   
   min_offset=min(abs(tickarr),min_loc)   
   for i=0, min_loc do tickarr(min_loc-i)=0-i*delta_tick
   if min_loc-1 ge 0 then begin
      for i=min_loc-1, nticks-min_loc-1 do tickarr(min_loc+i)=i*delta_tick
   endif else begin
      for i=min_loc, nticks-min_loc-1 do tickarr(min_loc+i)=i*delta_tick
   endelse  
endelse
   return,tickarr;

end