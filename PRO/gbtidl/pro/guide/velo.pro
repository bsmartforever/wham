;+
; Change the x-axis on the plotter to velocity.
;
; @examples
; <pre>
;   velo          ; x-axis is now velocity
;   chan          ; now it's channels
;   freq          ; now it's frequency
; </pre>
;
; @version $Id: velo.pro,v 1.9 2006/05/15 20:01:46 bgarwood Exp $
;-
pro velo
   if (!g.plotter_axis_type ne 2) then begin
       !g.plotter_axis_type=2
       if not !g.frozen and !g.line then reshow
   endif
   return
end
