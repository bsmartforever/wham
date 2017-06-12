pro wham_curval, state,a,b,q		
;  written by Dave "Widget-man" Kung
;  This procedure takes coordinates a and b and button event q 
;  and puts the appropriate values into the appropriate fields.
;  It is much like curval, except that the displayed data value is 
;  real data from state.data, not from the byte-scaled state.image.

	a1 = string(a)
	b1 = string(b)
	if q eq 1 then state.conditions(4)=1
	if q eq 2 then state.conditions(4)=0


;Update the fields if the second button was the last one pressed, or
;if the first button generated this event
	if (state.conditions(4) eq 0) or $
	    (q eq 1) then begin

;Only update the fields if the cursor is in the image
	     if (a le state.imsize(0)-1) $
	     and (b le state.imsize(1)-1) $
		then begin
;Set the xposition field
		widget_control,state.widgetids(5),set_value=a1
		state.conditions(0)=a
;set the yposition field
		widget_control,state.widgetids(6),set_value=b1
		state.conditions(1)=b
;set the data value field
		datavalue =state.data(floor(a/state.conditions(5)+.5),$
			floor(b/state.conditions(5)+.5),	$
			state.conditions(2))
		datavalue1 = string(datavalue)
		widget_control,state.widgetids(8),   $
			set_value = datavalue1
	     end
	end
return
end
