pro CPlotWin_Menu_event, event

widget_control, event.top, get_uval=uval
widget_control, event.id, get_value=selection

self=*uval.self_ptr

case selection of
    'Close': CPlotWin_Close, event.top
    'Save Plot As': self->SaveAs, event.top
    'Print...': if uval.exist.print eq 0 then $
      self->Print, event.top $
    else $
      widget_control, uval.exist.print, /show
    'Redraw': self->DrawPlot
    'Line Fit':  if uval.exist.gaussian eq 0 then $
      self->PlotGaussian, event.top $
    else $
      widget_control, uval.exist.gaussian, /show
    'Titles...': if uval.exist.titles eq 0 then $
      CPlotWin_Menu_Titles, event.top $
    else $
      widget_control, uval.exist.titles, /show
    'Axes...': if uval.exist.axes eq 0 then $
      CPlotWin_Menu_Axes, event.top $
    else $
      widget_control, uval.exist.axes, /show
    'Line...': if uval.exist.line eq 0 then $
      CPlotWin_Menu_Line, event.top $
    else $
      widget_control, uval.exist.line, /show
    else:
endcase

end
