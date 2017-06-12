; $Id: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_gc,event
widget_control,event.top,get_uvalue = pState
state = *pState
case event.id of
(*pState).comField:	begin
					end
(*pState).accept:	begin
					widget_control,(*pState).comField,get_value = val
  					*(*pState).commentPtr = val
  					(*pState).cancelled = 0
  					widget_control,event.top,/destroy
					end

(*pState).cancel:	begin
					(*pState).cancelled = 1
					*(*pState).commentPtr = ''
					widget_control,event.top,/destroy
					end
else:
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan_getcomments,	group_leader = group_leader, $
        	               	cancelled = cancelled
title = 'Comments'
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = title,/tlb_frame_attr)
endif else begin
  tlb = widget_base(group_leader = group_leader,/col,/modal, $
        title = title,/tlb_frame_attr)
endelse


xsize = 40
ysize = 12
font = "Comic Sans MS*22*Bold"
comField = widget_text(tlb,xsize = xsize,ysize = ysize, $
           value = 'pan_log',font = font,/editable)
butBase = widget_base(tlb,/row)
accept = widget_button(butBase,value = 'Accept')
cancel = widget_button(butBase,value = 'Cancel')
widget_control,tlb,/realize
state = {dir_name:'', $
         comField:comField, $
         cancelled:0, $
         commentPtr:ptr_new(/allocate_heap), $
         accept:accept, $
         cancel:cancel}
pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState
xmanager,'opan_getcomments',tlb,event_handler = 'opan_gc'

cancelled = (*pState).cancelled
comments = *(*pState).commentPtr
ptr_free,(*pState).commentPtr
ptr_free,pState
return,comments
end