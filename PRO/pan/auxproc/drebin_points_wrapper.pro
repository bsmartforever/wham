; $Id: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function points_to_hist,xpts
nx = n_elements(xpts)
mid = 0.5*(xpts[0:nx-2]+xpts[1:nx-1])	; midpoint between each bin center
dx = mid[1:nx-2] - mid[0:nx-3]	; width of each bin
xbndi = mid[0] - 0.5*dx[0]
xbndf = mid[nx-2] + 0.5*dx[nx-3]
xhist = [xbndi,mid,xbndf]
return,xhist
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro drebin_points_wrapper,x_in,z_in,dz_in,x_out,z_out,dz_out, $
                          err = err, emsg = emsg
; Here we assume that all data are "POINT" type data.  This wrapper converts
; the appropriate quantities to "HISTOGRAM" type in order to use John Copley's
; rebinning routines.
yin = z_in
yerrin = dz_in
; Transform the output dependent variable from "POINT" to "HISTOGRAM"
xrebhist = points_to_hist(x_out)
; Transform the input dependent variable from "POINT" to "HISTOGRAM"
xhist = points_to_hist(x_in)
; Perform the rebinning
drebin,xhist,yin,yerrin,xrebhist,zout,dzout,err=err,emsg=emsg
z_out = zout & dz_out = dzout
return
end