pro cimwin_tlb_event, event

if tag_names(event, /structure_name) eq 'WIDGET_KILL_REQUEST' then begin
    cimwin_close, event.top
endif else begin
    ; get uval
    widget_control, event.top, get_uval=uval

    ; find the new size of the window
    cimwin_resize_draw, event.top, event.x, event.y

    ; get uval again because parameters changed
    widget_control, event.top, get_uval=uval

    ; get self object
    self=*uval.self_ptr
    self->DrawImage

endelse

end 

