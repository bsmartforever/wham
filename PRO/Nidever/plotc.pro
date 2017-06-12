pro plotc,x,y,z,min=min0,psym=psym,symsize=symsize,nocolorbar=nocolorbar,max=max0,_extra=extra,stp=stp,$
          trim=trim,format=format,overplot=overplot

;+
;
; Plot with each points' color specified
;
; INPUTS:
;  x             Array of X values
;  y             Array of Y values
;  z             Array of values to be used for color
;  min=          Min values for Z to be used for color. Z values
;                  less than min are set to min.
;  max=          Max values for Z to be used for color.  Z values
;                  greater than max are set to max.
;  /trim         Trim the ends (below min and above max).  Only plot
;                  points with (min<z<max)
;  psym=         Plot symbol
;  symsize=      Size of plot symbol
;  format=       Format for the colorbar
;  /nocolorbar   Do not plot a colorbar
;  /overplot     Overplot.  Don't erase previous plot.
;  /stp          Stop at the end of the program
;  ANY OTHER PLOT KEYWORDS CAN BE SET  (e.g. xrange, yrange, etc.)
;
; OUTPUTS:
;  Plot on the screen
;
; USAGE:
;  IDL>plotc,x,y,z,ps=1
;
; PROGRAMS USED:
;  SCALE.PRO
;  COLORBAR.PRO
;
; By D. Nidever   June 2007
;-

nx = n_elements(x)
ny = n_elements(y)
nz = n_elements(z)


; Not enough parameters input
if nx eq 0 or ny eq 0 then begin
  print,'Syntax - plotc,x,y,z,min=min,max=max,psym=psym,symsize=symsize,nocolorbar=nocolorbar,'
  print,'               other plot keywords'
endif

x0 = x
y0 = y

; Original plot
position = [0.08,0.08,0.95,0.85]
if keyword_set(nocolorbar) then position = [0.08,0.05,0.95,0.98]
if keyword_set(overplot) then noerase=1 else noerase=0
if nz eq 0 then position = [0.08,0.08,0.95,0.95]
if not keyword_set(overplot) then $
  PLOT,x,y,_extra=extra,/nodata,position=position,psym=psym,symsize=symsize,noerase=noerase

; Colors input
if nz gt 0 then begin

  if nz ne nx then begin
    print,'Z and X must have same number of elements'
    return
  endif

  z0 = z

  ; Get colors
  if n_elements(min0) eq 0 then min=min(z) else min=min0
  if n_elements(max0) eq 0 then max=max(z) else max=max0

  ; Trimming the ends
  if keyword_set(trim) then begin
    gd = where(z ge min and z le max,ngd)
    ; No points left
    if ngd eq 0 then begin
      print,'NO POINTS LEFT'
      return
    endif
    x0 = x0[gd]
    y0 = y0[gd]
    z0 = z0[gd]
  endif

  bottom = 50
  ncolors = 200
  color = (z0>min)<max
  color = SCALE( color, [min,max], [bottom, bottom+ncolors-1])

  ; Plot with colors
  PLOTS,x0,y0,color=color,noclip=0,psym=psym,symsize=symsize

  ; Colorbar
  if not keyword_set(nocolorbar) then begin
    colpos = [0.08,0.92,0.95,0.95]
    COLORBAR,minrange=min,maxrange=max,position=colpos,bottom=bottom,ncolors=ncolors,format=format
  endif

  ; Leave with the original coordinate system
  if not keyword_set(overplot) then $
  PLOT,x,y,_extra=extra,/nodata,position=position,psym=psym,symsize=symsize,/noerase

; No color input
endif else begin ; colors input

  oplot,x,y,_extra=extra,psym=psym,symsize=symsize

endelse

if keyword_set(stp) then stop

end
