; $Id: $
; +
; NAME:
;  BOOTSTRAP_DIALOG
;
; PURPOSE:
;  Dialog which allows users to select their preferences
;  for performing the Bootstrap Monte-Carlo error estimation
;
;
; -
; ******************************************* ;
pro bootstrap_dialog_cleanup,tlb
widget_control,tlb,get_uvalue = pstate
ptr_free,pstate
end
; ******************************************* ;
; ******************************************* ;
pro bootstrap_dialog_event,event

end
; ******************************************* ;
pro bootstrap_dialog,   group_leader = group_leader,     $
                        notify_ids = notify_ids,         $
                        o_pan = o_pan


if n_elements(group_leader) eq 0 then group_leader = 0L
if n_elements(notify_ids) ne 2 then notify_ids = [0L,0L]
if n_elements(o_pan) eq 0 then o_pan = 0
if ~obj_valid(o_pan) then begin
   slider_sensitivity = 0
   slider_max = 1
endif else begin
   slider_sensitivity = 1
   o_pan->get_property()
endelse
title = 'Bootstrap Monte-Carlo Preferences'
tlb = widget_base(group_leader = group_leader, title = title,  $
      /col,/tlb_frame_attr)
group_id = cw_bgroup(tlb,['Current group','All groups'],/col,/no_release, $
   /exclusive,/frame)
num_iter_id = cw_field(tlb,value = '500',xsize = 6,title = '# Iterations')
row1 = widget_base(tlb,/row)
slider_id = widget_slider(row1,value = 0,minimum = 0,maximum = slider_max)

centertlb,tlb
widget_control,tlb,/realize
widget_control,group_id,set_value = 0
state = {group_id:group_id,num_iter_id:num_iter_id}
pstate = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pstate

xmanager,'bootstrap_dialog',tlb,/no_block,cleanup = 'bootstrap_dialog_cleanup'
end