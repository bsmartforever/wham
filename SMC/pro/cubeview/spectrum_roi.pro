pro spectrum_roi, state

;  Written by David Tsung-Shiao Kung
;  This procedure (for use with WHAM) displays the spectrum when given 
;  a data cube, and a region of interest.  These variables are passed to the
;  procedure in the state variable (see wham.pro)

spectrum = fltarr(state.size(2))

;  Translate from coordinates on state.image to those on state.data
;  Note that state.conditions(5) is the scaling factor
;  the +.5's have to do with the way the function REBIN works
;  for a detailed explanation, ask Steve Tufte (I explained it to him)

x0=floor((state.conditions(7)/state.conditions(5))+.5)
y0=floor((state.conditions(8)/state.conditions(5))+.5)
x1=floor(((state.conditions(7)+state.conditions(9)-1)/state.conditions(5))+.5)
y1=floor(((state.conditions(8)+state.conditions(10)-1)/state.conditions(5))+.5)

;  Make sure the upper right corner is within the data cube
if x1 gt (state.size(0)-1) then x1 = state.size(0)-1
if y1 gt (state.size(1)-1) then y1 = state.size(1)-1

;  Make sure the lower right corner is within the data cube
if (x0 le state.size(0)-1) and (y0 le state.size(1)-1) then begin
	print, 'Lower Left Corner: ', x0, y0
	print, 'Upper Right Corner: ', x1, y1
	print, 'Number of Pixels: ', n_elements(state.data(x0:x1,y0:y1,0))
	for i = 0,state.size(2)-1 do  $
 	   spectrum(i)=avg(state.data(x0:x1,y0:y1,i))
	plot,state.vel_label,spectrum, psym=5
        end  $
    else $   ;comic relief
	print,'"You are being uncooperative, Dave"  "Sorry, Hal"'


return
end
