; $Id: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_gd,event
widget_control,event.top,get_uvalue = pState
state = *pState
case event.id of
(*pState).dirField:	begin
					end
(*pState).userField:begin
					end
(*pState).accept:	begin
					widget_control,(*pState).dirField,get_value = val
  					(*pState).dir_name = val[0]
  					cd,current = thisDir
					result = file_test((*pState).dir_name,/directory)
					if result[0] eq 1 then begin
					  strout = ['Directory already exists', $
					            ' Please enter a new directory name']
					  void = dialog_message(dialog_parent = event.top,strout)
					  return
					endif
					widget_control,(*pState).userField,get_value = userval
  					(*pState).user = userval[0]
					widget_control,(*pState).titleField,get_value = titleval
  					(*pState).title = titleval[0]
  					(*pState).cancelled = 0
  					widget_control,event.top,/destroy
					end

(*pState).cancel:	begin
					(*pState).cancelled = 1
					widget_control,event.top,/destroy
					end
else:
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan_getdirectory,group_leader = group_leader, $
        	               cancelled = cancelled, $
        	               workDir = workDir
title = 'Directory name'
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = title,/tlb_frame_attr)
endif else begin
  tlb = widget_base(group_leader = group_leader,/col,/modal, $
        title = title,/tlb_frame_attr)
endelse


xsize = 40
font = "Comic Sans MS*22*Bold"
dirField = cw_field(tlb,/col,title = 'Directory name',xsize = xsize, $
           value = 'pan_log',fieldFont = font)
userField = cw_field(tlb,/col,title = 'User name',xsize = xsize, $
           value = 'User name',fieldFont = font)
titleField = cw_field(tlb,/col,title = 'Log file title',xsize = xsize, $
             value = 'PAN Analysis Log',fieldFont = font)
butBase = widget_base(tlb,/row)
accept = widget_button(butBase,value = 'Accept')
cancel = widget_button(butBase,value = 'Cancel')
widget_control,tlb,/realize
state = {dir_name:'', $
         dirField:dirField, $
         cancelled:0, $
         userField:userField, $
         titleField:titleField, $
         title:'', $
         user:'', $
         accept:accept, $
         cancel:cancel}
pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState
xmanager,'opan_getdirectory',tlb,event_handler = 'opan_gd'

cancelled = (*pState).cancelled
dir_name = (*pState).dir_name
user = (*pState).user
title = (*pState).title
ptr_free,pState
return,[dir_name,user,title]
end