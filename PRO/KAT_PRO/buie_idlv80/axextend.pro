;+
; NAME:
;  axextend
; PURPOSE:   (one line only)
;  Adjust an axis plot range
; DESCRIPTION:
;  This function replicates the direct graphics option of extending the
;    axis plot range so that data do not touch the sides of the plotting
;    box or axis tick marks.  This is really only needed for the new plot
;    functions added in IDL v8.0
; CATEGORY:
;  Plotting
; CALLING SEQUENCE:
;  outrange=axextend(inrange)
; INPUTS:
;  inrange - two element vector with the input range, presumed to be the
;              exact data range for the plot.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  EXTEND - Fraction to extend the plot range by.  Default=0.05 (5%).
; OUTPUTS:
;  Return value is the extended plot range
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/01/07, Written by Marc W. Buie, Southwest Research Institute
;-
function axextend,inrange,EXTEND=extend

   self='axextend: '
   if badpar(inrange,[2,3,4,5],1,caller=self+'(inrange) ') then return,0

   if badpar(extend,[0,4,5],0,caller=self+'(EXTEND) ', $
                              default=0.05) then return,0

   meanval = (inrange[1]+inrange[0])/2.0
   valrange = inrange[1]-inrange[0]

   if valrange eq 0 then valrange=1.0

   newrange = (1.0+extend)*valrange
   outrange = [meanval-newrange/2.0,meanval+newrange/2.0]

   return,outrange

end
