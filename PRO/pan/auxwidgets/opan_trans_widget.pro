; $Id: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_tw,event
widget_control,event.top,get_uvalue = pState
case event.id of
(*pState).cancelButton:	$
			begin
			  (*pState).out.cancel = 1
			  widget_control,event.top,/destroy
			  device, get_screen_size=screen_size
			  message = dialog_message('File saved is empty', title='Cancel', dialog_parent=group_leader)
			end
(*pState).acceptButton: $
			begin
			widget_control,(*pState).trans,get_value = trans
			(*pState).out.trans = trans
			(*pState).out.cancel = 0
			widget_control,event.top,/destroy
 			end
else:
endcase
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan_trans_widget,group_leader = group_leader
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = 'Transition Name',/tlb_frame_attr)
endif else begin
  tlb = widget_base(group_leader = group_leader, /col,/modal, $
        title = 'Transition Name',/tlb_frame_attr)
endelse
base = widget_base(tlb,/row)

base1 = widget_base(base,/col,/frame)
void = widget_label(base1,value = 'Enter transition name, e.g. H_alpha = Ha :')
trans = widget_text(base1,value = 'Ha', /editable)

acceptButton = widget_button(tlb,value = 'OK', xsize=75)
cancelButton = widget_button(tlb,value = 'Cancel', xsize=75)

widget_control,tlb,/realize

out = {trans:'',cancel:1}
state = {out:out, trans:trans, $
         cancelButton:cancelButton, $
         acceptButton:acceptButton}

pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState
xmanager,'opan_trans_widget',tlb,event_handler = 'opan_tw'

out = (*pState).out
ptr_free,pState
return,out
end
