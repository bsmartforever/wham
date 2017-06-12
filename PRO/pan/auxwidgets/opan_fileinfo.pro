; $Id: opan_fileinfo.pro,v 1.1 2002/09/19 21:30:20 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opanFICleanup,tlb
widget_control,tlb,get_uvalue = pState
if ((*pState).notifyIds)[0] ne (-1L) then begin
  s = size((*pState).notifyIDs)
  if s[0] eq 1 then count = 0 else count = s[2]-1
  for j = 0,count do begin
    fileInfo = {fileInfoEvent,$
                        		ID:(*pState).notifyIDs[0,j],$
                        		Top:(*pState).notifyIDs[1,j],$
                        		Handler:0l}
    if widget_info((*pState).notifyIDs[0,j],/valid_id) then begin $
      widget_control,(*pState).notifyIDs[0,j],send_event = fileInfo
    endif
  endfor
endif
ptr_free,pState
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opanFIQuit,event
widget_control,event.top,/destroy
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_fileInfo,	info, $
					group_leader = group_leader, $
                   	notifyIds = notifyIds
; Widget definition module
if n_elements(notifyIds) eq 0 then notifyIds = (-1L)
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = 'PAN File Information', $
        /base_align_center)
endif else begin
  tlb = widget_base(group_leader = group_leader, /col, $
        title = 'PAN File Information', $
        /base_align_center)
endelse
nlines = n_elements(info)
length = 0
for i = 0,nlines-1 do begin
  length = length > strlen(info[i])
endfor
text = widget_text(tlb,value = info,xsize = length,ysize = 25,/editable,/scroll)

void = widget_button(tlb,value = 'Dismiss',event_pro = 'opanFIQuit')

widget_control,tlb,/realize

state = {notifyIds:notifyIds}
pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState,/no_copy
xmanager,'opan_fileinfo',tlb,cleanup = 'opanFICleanup',/no_block
return
end