; $Id: centertlb.pro,v 1.1 2002/04/24 20:38:14 jmspark Exp $
pro centertlb,tlb
; This procedure centers the entire widget (the
; top-level-base) on the screen (independent of
; the platform).
device,get_screen_size = screenSize
geom = widget_info(tlb,/geometry)
widget_control,tlb,xoff = (screenSize[0]/2)-(geom.scr_xsize/2),$
                   yoff = (screenSize[1]/2)-(geom.scr_ysize/2)

end