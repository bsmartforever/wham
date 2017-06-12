;+
; NAME:
;  rescale
; PURPOSE:   (one line only)
;  Scale data for plotting from one range to another.
; DESCRIPTION:
; CATEGORY:
;  Plotting
; CALLING SEQUENCE:
;  res=rescale(data,inrange,outrange)
; INPUTS:
;  data     - Vector of data to be rescaled
;  inrange  - range of input data
;  outrange - range of output data
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  return value is a scaled copy of the input data
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  2011/02/07, Written by Marc W. Buie, Southwest Research Institute
;-
function rescale,data,ra1,ra2

   self='rescale: '
   if badpar(data,[2,3,4,5],1,caller=self+'(rescale) ') then return,0
   if badpar(ra1,[2,3,4,5],1,caller=self+'(inrange) ') then return,0
   if badpar(ra2,[2,3,4,5],1,caller=self+'(outrange) ') then return,0

   d = (data-ra1[0])/(ra1[1]-ra1[0])*(ra2[1]-ra2[0]) + ra2[0]

   return,d

end
