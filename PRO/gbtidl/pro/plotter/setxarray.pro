;+
; This procedure generates the xarray values used for the plotter.  
; This procedure is not intended to be called by users.
;
; @private_file
;
; @version $Id: setxarray.pro,v 1.7 2006/01/18 14:58:15 bgarwood Exp $
;-
pro setxarray
    compile_opt idl2
    common gbtplot_common,mystate,xarray
    
    xarray = makeplotx(*mystate.dc_ptr,type=mystate.xtype)
    setxtitle, mystate.xtype
end
