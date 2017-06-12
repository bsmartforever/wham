pro CImWin_Gauss_Button_event, event

widget_control, event.top, get_uval=uval
widget_control, uval.wids.base_id, get_uval=base_uval
self=*base_uval.self_ptr
; remove the gaussian pointing mode
; if the box exists then remove it
pmode=*base_uval.pmode_ptr
if (pmode[0].pointingmode eq 'box') then begin
    if (base_uval.box_pres) then self->RemoveBox
endif

; if the first element is a circle, remove it since it will be redrawn
if (pmode[0].pointingmode eq 'aperture') then begin
    if ((pmode[0].type eq 'strehl' and (base_uval.circ_strehl_pres)) or $
        ((pmode[0].type eq 'phot' and (base_uval.circ_phot_pres)))) then begin
        self->RemoveCircle
    endif
endif

self->RmPntMode, 'peak fit'

; draw the current pointing mode box/circle if present
self->DrawImageBox_n_Circles

widget_control, event.top, /destroy

end
