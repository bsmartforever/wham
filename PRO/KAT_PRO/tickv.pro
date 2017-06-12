pro tickv, range, nticks, tickname, tickv, padding=padding, factor=factor, $
    format=format, velocity=velocity, multiple=multiple, zero=zero

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;+
; Purpose: Determine tick values and the corresponding string names. 
;
; Input:
;   range - range of axis, expects [min, max]
;   nticks - how many ticks. The output might have a different number of ticks than this depending on which other keywords
;            are called, e.g., velocity, multiple, zero
;   padding - Amount of padding in data units to be split above and below the first tick labels.
;   factor  - Factor of the range to be split above and below first tick labels.
;   ticknames - an array containing the string names of the ticks
;   tickv  - an array containing the values of the ticks
;   format - specified the format of the tick names
;   velocity - keyword that specifies that the ticks are for a velocity axis. This will cause the program to automatically
;              search for the multiple that works best.
;   zero - forces zero to be one of the tick values
;
; Example:
;   yrange=[-1.,9]
;   nticks=10
;   tickv,yrange,nticks,ytickname,ytickv,padding=0.1,multiple=1,/zero
;   plot,[1,7,3,6,9],ystyle=1,yrange=yrange,yticks=nticks,ytickv=ytickv
;
;     ;Notice that nticks changed to 8, because multiple=1
;
;   yrange=[-1.,9]
;   nticks=10
;   tickv,yrange,nticks,ytickname,ytickv,padding=0.4,/zero
;   plot,[1,7,3,6,9],ystyle=1,yrange=yrange,yticks=nticks,ytickv=ytickv
;
;     ;In this case, nticks adjusted to 9 
;
;   yrange=[-1.,9]
;   nticks=10
;   tickv,yrange,nticks,ytickname,ytickv,padding=0.1,multiple=3,/zero
;   plot,[1,7,3,6,9],ystyle=3,yrange=yrange,yticks=nticks,ytickv=ytickv
;
;     ;Notice that nticks changed to 2, because multiple=3
;
; Created by Dr. Kat Barger
;   Last modified 11/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

    if (NOT keyword_set(padding)) then padding=0.0
    if keyword_set(factor) then begin
      if factor lt 1. then $
        padding=(range[1]-range[0])*factor $
      else begin
        print,''
        print,'*** Factor must be less than 1 ***'
        print,''
        return
      endelse
    endif

    delta=range[1]-range[0]-padding

    if keyword_set(velocity) then begin
       v_spacing=[5.,10.,20.,25.,50.,100.,200.,250.,500.] 
       nticks_tmp=delta/v_spacing 
       junk=min(abs(nticks - delta/v_spacing),loc_min) 
       nticks=nticks_tmp[loc_min]
       spacing=v_spacing[loc_min]
       values=((findgen(nticks+1))*spacing+delta*padding+range[0])
       values=round(values/spacing)*spacing
       multiple=spacing       
       nticks=n_elements(values)-1

    endif else begin
       spacing=delta/(nticks)
       values=((findgen(nticks+1))*spacing+range[0]+padding)
    endelse

       if (keyword_set(zero)) AND ((range[0] lt 0) AND (range[1] gt 0)) then begin 
          temp=min(abs(values),loc)
          values=values-values[loc]
          if values[0] lt range[0]+padding AND (n_elements(values) ge 2) AND (values[0] ne 0) then values=values[1:n_elements(values)-1]
          if (values[1] gt range[1]-padding) AND (n_elements(values) ge 3) AND (values[0] ne 0) then values=values[0:n_elements(values)-2]
       endif

    if keyword_set(multiple) then begin
       values=round(values/multiple)*multiple
    endif

    values=values[rem_dup(values)]
    indices=where((significant(values,-1) le significant(range[1]-delta*padding,-1)) AND $
                  (significant(values,-1) ge significant(range[0]+delta*padding,-1)),count)

    if keyword_set(zero) then $
    indices=where((significant(values,-1) le significant(range[1]-padding,-1)) AND $
                  (significant(values,-1) ge significant(range[0]+padding,-1)) OR $
                  (values eq 0),count)

    if count ne 0 then values=values[indices]    


    ;Don't know what this code was supposed to do!
    ;loc_zero=where(values eq 0)
    ;loc=where(values ne 0)  
    ;pause
    ;values=(indgen(n_elements(values))+loc_zero[0])*min(abs(values[loc]))
    ;Notice the absolute value sign. This is causing problems as the sign maters in the arithmatic


    tickv=values 
    nticks=n_elements(tickv)-1


    if (NOT keyword_set(format)) then tickname=FORMAT_AXIS_VALUES(tickv) $
    else begin
       tickname=strarr(nticks+1)
       for i=0, nticks do tickname[i]=strcompress(string(tickv[i],format=replicate(format,n_elements(tickv[i]))),/re)
    endelse

end