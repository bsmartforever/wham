;-------------------------------------------------------------
;+
; NAME:
;        MYCT
;
; PURPOSE:
;        load a color table and define the first 16 colors for 
;        drawing colors (white,black,red,green,blue,yellow,magenta,
;        lightblue,lightred,lightgreen,purple,black,85%grey,67%grey,
;        50%grey,33%grey,15%grey).
;
; CATEGORY:
;        color table manipulation
;
; CALLING SEQUENCE:
;        MYCT
;
; INPUTS:
;        TABLE --> [optional] number of the color table to be used
;               default is EOS-B (number 27)
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; SUBROUTINES:
;
; REQUIREMENTS:
;
; NOTES:
;        It is recommended to use the COLOR= keyword whenever possible
;        This will ensure correct colors on (hopefully) all devices.
;        In order to get 256 colors on a postcript printer use
;        DEVICE,/COLOR,BITS_PER_PIXEL=8 
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;        mgs, 06 Feb 1997: VERSION 1.00
;        mgs, 03 Aug 1997: added input parameter and template
;
;-
; Copyright (C) 1997, Martin Schultz, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author to arrange payment.
; Bugs and comments should be directed to mgs@io.harvard.edu
; with subject "IDL routine myct"
;-------------------------------------------------------------


pro myct,table
 
; loads colortable (default EOS-B) and modifies first entries:
;   color 0 becomes whitefor background
;   colors 1..10 become brilliant plot colors
;   colors 11..16 become grey shadings, beginning with black
;   the rest is unaltered

if (n_params() le 0) then table = 27
 
loadct, table
 
red  =[  255,  0,255,  0,  0,255,255,  0,255,127,127,0,62,98,172,200,232,255]
green=[  255,  0,  0,255,  0,255,  0,255,127,255,127,0,62,98,172,200,232,255]
blue =[  255,  0,  0,  0,255,  0,255,255,127,127,255,0,62,98,172,200,232,255]
; red  =[  255,  0,255,  0,  0,255,255,  0,255,127,127,0,42,85,128,170,212,255]
; green=[  255,  0,  0,255,  0,255,  0,255,127,255,127,0,42,85,128,170,212,255]
; blue =[  255,  0,  0,  0,255,  0,255,255,127,127,255,0,42,85,128,170,212,255]
 
TVLCT, red, green, blue
end
