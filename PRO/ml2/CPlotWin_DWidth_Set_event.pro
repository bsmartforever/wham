pro CPlotWin_DWidth_Set_event, event

widget_control, event.top, get_uval=uval
widget_control, uval.base_id, get_uval=imwin_uval

; get plot window object
self=*(uval.self_ptr)
imwin_self=*(imwin_uval.self_ptr)

; get the new diagonal width value
widget_control, uval.wids.data_dwidth, get_value=dwidth

; handle errors on user input (e.g. strings)
on_ioerror, dwidth_error
dwidth=fix(dwidth[0])
goto, no_dwidth_error
dwidth_error: dwidth=1
no_dwidth_error:

; set new diagonal width
self->SetDWidth, dwidth

end
