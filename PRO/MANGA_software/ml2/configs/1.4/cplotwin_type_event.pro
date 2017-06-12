pro CPlotWin_Type_event, event

widget_control, event.top, get_uval=base_uval
self=*(base_uval.self_ptr)
widget_control, self->GetParentBaseId(), get_uval=imwin_uval
CImWinObj=*(imwin_uval.self_ptr)

; find the selection type
dlist_selection=widget_info(event.id, /droplist_select)
type_list=*base_uval.type_list_ptr
selection=type_list[dlist_selection]

; find the old plot type
old_plottype=self->GetPlotType()

case selection of
	'Depth Plot': begin	; Depth Plot 
            self->ChangePlotType, 'depth'
            if (old_plottype eq 'diagonal') then begin
                ; remove the diagonal box
                if (imwin_uval.diag_pres) then begin
                    CImWinObj->Draw_Diagonal_Box, imwin_uval.draw_diagonal_box_p0, imwin_uval.draw_diagonal_box_p1
                endif
                ; redraw the rectangular box if it exists
                if (imwin_uval.box_pres) then begin
                    CImWinObj->Draw_Box, imwin_uval.draw_box_p0, imwin_uval.draw_box_p1
                endif
            endif
	end
        'Horizontal Cut': begin	; Horizontal Cut 
            self->ChangePlotType, 'horizontal'
            if (old_plottype eq 'diagonal') then begin
                ; remove the diagonal box
                if (imwin_uval.diag_pres) then begin
                    CImWinObj->Draw_Diagonal_Box, imwin_uval.draw_diagonal_box_p0, imwin_uval.draw_diagonal_box_p1
                endif
                ; redraw the rectangular box if it exists
                if (imwin_uval.box_pres) then begin
                    CImWinObj->Draw_Box, imwin_uval.draw_box_p0, imwin_uval.draw_box_p1
                endif
            endif

        end
        'Vertical Cut': begin 	; Vertical Cut
            self->ChangePlotType, 'vertical'
            if (old_plottype eq 'diagonal') then begin
                ; remove the diagonal box
                if (imwin_uval.diag_pres) then begin
                    CImWinObj->Draw_Diagonal_Box, imwin_uval.draw_diagonal_box_p0, imwin_uval.draw_diagonal_box_p1
                endif
                ; redraw the rectangular box if it exists
                if (imwin_uval.box_pres) then begin
                    CImWinObj->Draw_Box, imwin_uval.draw_box_p0, imwin_uval.draw_box_p1
                endif
            endif
	end
	'Diagonal Cut': begin 	; Diagonal Cut
            self->ChangePlotType, 'diagonal'
            if (old_plottype ne 'diagonal') then begin
                ; remove the rectangular box if it exists
                if (imwin_uval.box_pres) then begin
                    CImWinObj->Draw_Box, imwin_uval.draw_box_p0, imwin_uval.draw_box_p1
                endif
                ; redraw the diagonal box
                if (imwin_uval.diag_pres) then begin
                    CImWinObj->Draw_Diagonal_Box, imwin_uval.draw_diagonal_box_p0, imwin_uval.draw_diagonal_box_p1
                endif
            endif
        end
	'Surface Plot': begin 	; Surface Plot
            self->ChangePlotType, 'surface'
            if (old_plottype eq 'diagonal') then begin
                ; remove the diagonal box
                if (imwin_uval.diag_pres) then begin
                    CImWinObj->Draw_Diagonal_Box, imwin_uval.draw_diagonal_box_p0, imwin_uval.draw_diagonal_box_p1
                endif
                ; redraw the rectangular box if it exists
                if (imwin_uval.box_pres) then begin
                    CImWinObj->Draw_Box, imwin_uval.draw_box_p0, imwin_uval.draw_box_p1
                endif
            endif
	end
	'Contour Plot': begin	; Contour Plot
            self->ChangePlotType, 'contour'
            if (old_plottype eq 'diagonal') then begin
                ; remove the diagonal box
                if (imwin_uval.diag_pres) then begin
                    CImWinObj->Draw_Diagonal_Box, imwin_uval.draw_diagonal_box_p0, imwin_uval.draw_diagonal_box_p1
                endif
                ; redraw the rectangular box if it exists
                if (imwin_uval.box_pres) then begin
                    CImWinObj->Draw_Box, imwin_uval.draw_box_p0, imwin_uval.draw_box_p1
                endif
            endif
	end
	else:
endcase

end
