; $Id: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_rw,event
widget_control,event.top,get_uvalue = pState
case event.id of
(*pState).cancelButton:	$
			begin
			  (*pState).out.cancel = 1
			  widget_control,event.top,/destroy
			end
(*pState).acceptButton: $
			begin
			widget_control,(*pState).ndesired,get_value = nbins
			(*pState).out.nbins = fix(nbins[0])
			widget_control,(*pState).xdlo,get_value = xlo
			(*pState).out.xlo = float(xlo[0])
			widget_control,(*pState).xdhi,get_value = xhi
			(*pState).out.xhi = float(xhi[0])
			(*pState).out.cancel = 0
			widget_control,event.top,/destroy
 			end
else:
endcase
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan_rebin_widget,x,group_leader = group_leader
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = 'Rebinning utility',/tlb_frame_attr)
endif else begin
  tlb = widget_base(group_leader = group_leader, /col,/modal, $
        title = 'Rebinning utility',/tlb_frame_attr)
endelse
base = widget_base(tlb,/row)
xlo = min(x) & xhi = max(x) & nx = n_elements(x)

base1 = widget_base(base,/col,/frame)
void = widget_label(base1,value = 'Current Binning')
void = cw_field(base1,/row,value = string(nx),title = '# of channels',/noedit)
void = cw_field(base1,/row,value = string(xlo),title = 'Lower x limit',/noedit)
void = cw_field(base1,/row,value = string(xhi),title = 'Upper x limit',/noedit)

base2 = widget_base(base,/col,/frame)
void = widget_label(base2,value = 'Desired Binning')
ndesired = cw_field(base2,/row,value = string(nx),title = '# of channels')
xdlo = cw_field(base2,/row,value = string(xlo),title = 'Lower x limit')
xdhi = cw_field(base2,/row,value = string(xhi),title = 'Upper x limit')

acceptButton = widget_button(tlb,value = 'Accept')
cancelButton = widget_button(tlb,value = 'Cancel')

widget_control,tlb,/realize

out = {xlo:xlo,xhi:xhi,nbins:nx,cancel:1}
state = {out:out, $
         ndesired:ndesired, xdlo:xdlo, xdhi:xdhi, $
         cancelButton:cancelButton, $
         acceptButton:acceptButton}

pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState
xmanager,'opan_rebin_widget',tlb,event_handler = 'opan_rw'

out = (*pState).out
ptr_free,pState
return,out
end