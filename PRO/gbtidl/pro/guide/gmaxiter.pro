;+
; Sets the maximum iterations for fitting guassians: !g.gauss.maxiter
; 
; @param maxiter {in}{required}{type=long} maximum iterations for fitting gaussians
;
; @examples
; <pre>
; ; get some continuum data
; cont
; filein,'peaks.fits'
; get,index=1
; ; setup the gauss fit
; gregion,[60,80]
; ngauss,[1]
; gmaxiter,500
; gparamvalues, 0, 0, [400000.,70.,100.]
; ; find and show the fit
; gauss
; gshow
; </pre>
;
; @version $Id: gmaxiter.pro,v 1.3 2005/11/20 16:10:53 jbraatz Exp $
;-
pro gmaxiter, maxiter
    compile_opt idl2
 
    if n_elements(maxiter) eq 0 then begin
        message,'Usage: gmaxiter, maxiter',/info
        return
    endif
   
    !g.gauss.maxiter = maxiter

end    
