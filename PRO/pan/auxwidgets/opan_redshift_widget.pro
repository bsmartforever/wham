; $Id: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_redshift,event
widget_control,event.top,get_uvalue = pState
case event.id of
(*pState).acceptButton: $
			begin
			widget_control,(*pState).redshift,get_value = redshift
			(*pState).out.redshift = redshift
			widget_control,event.top,/destroy
 			end
else:
endcase
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan_redshift_widget,group_leader = group_leader
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = 'Source Redshift',/tlb_frame_attr)
endif else begin
  tlb = widget_base(group_leader = group_leader, /col,/modal, $
        title = 'Source Redshift (km/s)',/tlb_frame_attr)
endelse
base = widget_base(tlb,/row)

base1 = widget_base(base,/col,/frame)
void = widget_label(base1,value = 'Enter source redshift (km/s)')
redshift = widget_text(base1,value = '0', /editable)

acceptButton = widget_button(tlb,value = 'OK', xsize=75, $  
   ACCELERATOR = "Return")

widget_control,tlb,/realize

out = {redshift:''}
state = {out:out, redshift:redshift, $
         acceptButton:acceptButton}

pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState
;widget_control,acceptButton,/set_button
xmanager,'opan_redshift_widget',tlb,event_handler = 'opan_redshift'

out = (*pState).out
ptr_free,pState
return,out
end
