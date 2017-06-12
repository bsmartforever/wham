pro cfitshedit_quit_button_event, event
; ends program
  
  ; get uval struct
  widget_control, event.top, get_uval=uval
  widget_control, uval.base_id, get_uval=cimwin_uval

  if (uval.modified eq 1) then begin
      answer=dialog_message("File has been modified.  Do you wish to save before exiting?", dialog_parent=event.top, /question, /cancel)
      if (answer eq "Yes") then begin
          ; save file
          cfitshedit_saveas_button_event, event
          ; destroy widget
          cimwin_uval.exist.fitshedit=0L
          widget_control, uval.base_id, set_uval=cimwin_uval
          widget_control, event.top, /destroy
      endif else if (answer eq "No") then begin
          ; destroy widget
          cimwin_uval.exist.fitshedit=0L
          widget_control, uval.base_id, set_uval=cimwin_uval
          widget_control, event.top, /destroy
      endif
  endif else begin
      ; destroy widget
      cimwin_uval.exist.fitshedit=0L
      widget_control, uval.base_id, set_uval=cimwin_uval
      widget_control, event.top, /destroy
  endelse
end
