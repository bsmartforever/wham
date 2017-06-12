; $Id: eisfparmentry.pro,v 1.1 2002/09/19 21:30:20 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro eisf_event,event

widget_control,event.top,get_uvalue = pState
widget_control,event.id,get_value = val

widget_control,(*pState).elField,get_value = elText
elText = elText[0]
widget_control,(*pState).qeField,get_value = qeText
qeText = qeText[0]
el = opan_selectgroups(elText)
qe = opan_selectgroups(qeText)

case val[0] of
'ACCEPT':	begin
			  (*pState).canceled = 0
			  *(*pState).qePtr = qe
			  *(*pState).elPtr = el
			end
'CANCEL':	begin
			  (*pState).canceled = 1
			end
else:
endcase
widget_control,event.top,/destroy

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function eisfparmentry,group_leader = group_leader,canceled = canceled
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,/tlb_frame_attr,title = 'EISF parameter entry')
endif else begin
  tlb = widget_base(/col,/tlb_frame_attr,title = 'EISF parameter entry', $
        group_leader = group_leader)
endelse
elField = cw_field(tlb,/row,title = 'Elastic parameters (e.g. 1)',value = '1')
qeField = cw_field(tlb,/row,title = 'Quasielastic parameters (e.g. 1,2)', $
          value = '2,3')
rowBase = widget_base(tlb,/row)
void = widget_button(rowBase,value = 'ACCEPT')
void = widget_button(rowBase,value = 'CANCEL')

widget_control,tlb,/realize

state = {elPtr:ptr_new(/allocate_heap), $
         qePtr:ptr_new(/allocate_heap), $
         elField:elField, $
         qeField:qeField, $
         canceled:1 }

pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState
xmanager,'eisfparmentry',tlb,event_handler = 'eisf_event'

eisf = {el:(*(*pState).elPtr), $
        qe:(*(*pState).qePtr), $
        canceled:(*pState).canceled}

ptr_free,(*pState).elPtr,(*pState).qePtr
ptr_free,pState
return,eisf
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;