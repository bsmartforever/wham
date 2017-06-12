pro CPlotWin_Close, base_id

widget_control, base_id, get_uval=uval
self=*uval.self_ptr

; change box mode
widget_control, self->GetParentBaseId(), get_uval=win_uval
imwin_self=*(win_uval.self_ptr)

; remove the box from the draw window
imwin_self=*(win_uval.self_ptr)
pmode=*win_uval.pmode_ptr

if (pmode[0].type ne 'diagonal') then begin 
    if (win_uval.box_pres) then begin
        imwin_self->RemoveBox
    endif
endif else begin
    if (win_uval.diag_pres) then begin
        imwin_self->RemoveDiagonalBox
    endif
endelse

; if the first element is a circle, remove it since it will be redrawn
if (pmode[0].pointingmode eq 'aperture') then begin
    if ((pmode[0].type eq 'strehl' and (win_uval.circ_strehl_pres)) or $
         ((pmode[0].type eq 'phot' and (win_uval.circ_phot_pres)))) then begin
        self->RemoveCircle
    endif
endif

; remove the top plot pointing mode from the stack
plot_type=self->GetPlotType()
imwin_self->RmPntMode, plot_type

; find the diagonal or other boxes modes in the stack and remove them too
widget_control, self->GetParentBaseId(), get_uval=win_uval
pmode=*(win_uval.pmode_ptr)

depth_pos=where(pmode.type[*] eq 'depth')
if (depth_pos[0] ne -1) then imwin_self->RmPntMode, 'depth'

horizontal_pos=where(pmode.type[*] eq 'horizontal')
if (horizontal_pos[0] ne -1) then imwin_self->RmPntMode, 'horizontal'

vertical_pos=where(pmode.type[*] eq 'vertical')
if (vertical_pos[0] ne -1) then imwin_self->RmPntMode, 'vertical'

diagonal_pos=where(pmode.type[*] eq 'diagonal')
if (diagonal_pos[0] ne -1) then imwin_self->RmPntMode, 'diagonal'

surface_pos=where(pmode.type[*] eq 'surface')
if (surface_pos[0] ne -1) then imwin_self->RmPntMode, 'surface'

contour_pos=where(pmode.type[*] eq 'contour')
if (contour_pos[0] ne -1) then imwin_self->RmPntMode, 'contour'

; draw the current pointing mode box/circle if present
imwin_self->DrawImageBox_n_Circles

obj_destroy, self
widget_control, base_id, /destroy

end
