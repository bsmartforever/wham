;+
; This procedure sets the flag that indicates whether the xaxis is in
; absolute units or relative to the value at the reference channel.
;
; @param absrel {in}{required}{type=string} The string that determines
; the setting of the flag.  Recognized values are 'Abs' and 'Rel'.
;
; @version $Id: setabsrel.pro,v 1.5 2005/06/14 17:28:47 bgarwood Exp $
;-

pro setabsrel, absrel
    common gbtplot_common,mystate,xarray
    if n_elements(absrel) eq 0 then begin
        message,'Usage: setabsrel, absrel',/info
        return
    endif
    if (data_valid(*mystate.dc_ptr) le 0) then begin
        ; nothing has been plotted, just set it and return
        mystate.absrel = absrel
    endif else begin
        if (mystate.absrel ne absrel) then begin
            convertxstate, mystate.xunit, mystate.frame, mystate.veldef, absrel, mystate.voffset
            reshow
        endif
    endelse
    widget_control,mystate.absrel_id,set_value=absrel
end
