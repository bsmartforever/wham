;+
; Change the x-axis on the plotter to channels.
;
; @examples
; <pre>
;   chan          ; x-axis is now channels
;   freq          ; now it's frequency
;   velo          ; now it's velocity
; </pre>
;
; @version $Id: chan.pro,v 1.9 2006/05/15 20:01:46 bgarwood Exp $
;-
pro chan
   if (!g.plotter_axis_type ne 0) then begin
       !g.plotter_axis_type=0
       if not !g.frozen then reshow
   endif
   return
end
