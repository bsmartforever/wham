pro CImWin_Zoom_event, event

; get uval
widget_control, event.top, get_uval=uval
; get self object
self=*uval.self_ptr

; get current scales
cur_xscl=self->GetXScale()
cur_yscl=self->GetYScale()

case event.handler of
    uval.wids.topzoomminus_button: begin
        self->SetXScale, cur_xscl/2.d
        self->SetYScale, cur_yscl/2.d			
    end
    uval.wids.topzoomplus_button: begin
        self->SetXScale, cur_xscl*2.d			
        self->SetYScale, cur_yscl*2.d			
    end
    uval.wids.topone2one_button: begin
        self->SetXScale, 1d
        self->SetYScale, 1d
    end
    uval.wids.topfit_button: begin
        tmp_xscl=self->GetXS() / double(self->GetDispIm_xs())
        tmp_yscl=self->GetYS()/ double(self->GetDispIm_ys())
        if (tmp_xscl le tmp_yscl) then begin
            new_xscl=tmp_xscl
            new_yscl=cur_yscl*(new_xscl/cur_xscl)
        endif else begin
            new_yscl=tmp_yscl
            new_xscl=cur_xscl*(new_yscl/cur_yscl)
        endelse
        self->SetXScale, new_xscl
        self->SetYScale, new_yscl
        ; recenter the image
        im_xs=self->GetDispIm_xs()
        im_ys=self->GetDispIm_ys()
        uval.tv_p0=[im_xs/2, im_ys/2]
    end
    uval.wids.xzoom_buttons: begin
        case event.value of
            ; divide x scale by 2
            ' - ': self->SetXScale, cur_xscl/2.d
            ; multiple x scale by 2
            ' + ': self->SetXScale, cur_xscl*2.d
            ; make x scale 1
            '1:1': self->SetXScale, 1d
            ; fit x scale so image fits to window size
            'Fit': begin 
                self->SetXScale, self->GetXS()/double(self->GetDispIm_xs())
                ; recenter the image
                im_xs=self->GetDispIm_xs()
                im_ys=self->GetDispIm_ys()
                uval.tv_p0=[im_xs/2, im_ys/2]
            end
        endcase
    end
    uval.wids.yzoom_buttons: begin
        case event.value of
            ; divide y scale by 2
            ' - ': self->SetYScale, cur_yscl/2.d
            ; multiple y scale by 2
            ' + ': self->SetYScale, cur_yscl*2.d
            ; make y scale 1
            '1:1': self->SetYScale, 1d
            ; fit y scale so image fits to window size
            'Fit': begin
                self->SetYScale, self->GetYS()/double(self->GetDispIm_ys())
                ; recenter the image
                im_xs=self->GetDispIm_xs()
                im_ys=self->GetDispIm_ys()
                uval.tv_p0=[im_xs/2, im_ys/2]
            end
        endcase
    end
endcase

widget_control, event.top, set_uval=uval

; update window text
self->UpdateText
; redraw image
self->DrawImage

end

