; ----------------------------------------------------------------------

function krange,karray,kplot_type=kplot_type

karray = double(karray)

if (not keyword_set(kplot_type)) then begin
   kplot_type = 'i'
endif

kmm = dblarr(2)
kmm(1) = max(karray)
kmm(0) = min(karray)

if (kplot_type EQ 'o') then begin
   kmm = alog10(kmm)
   width = kmm(1) - kmm(0)
   return,[10^(kmm(0) - width*0.1),10^(kmm(1) + width*0.1)]
endif else begin
   width = kmm(1) - kmm(0)
   return,[kmm(0) - width*0.1,kmm(1) + width*0.1]
endelse

end

; ----------------------------------------------------------------------

pro get_new_krange,in_krange,in_karray,kplot_type=kplot_type

if (not keyword_set(kplot_type)) then begin
   kplot_type = 'i'
endif

krange2 = krange(in_karray,kplot_type=kplot_type)

new_krange = in_krange
new_krange(0) = min([in_krange(0),krange2(0)])
new_krange(1) = max([in_krange(1),krange2(1)])
in_krange = new_krange

end

; ----------------------------------------------------------------------

pro set_symbol,psym,tpsym

; psym = 0 ==> bin (solid)
;        1 ==> circle (open)
;        2 ==> circle (filled)
;        3 ==> box (open)
;        4 ==> box (filled)
;        5 ==> diamond (open)
;        6 ==> diamond (filled)
;        7 ==> triangle (open)
;        8 ==> triangle (filled)
;        9 ==> star (open)
;       10 ==> star (filled)
;       11 ==> upside down triangle (open)
;       12 ==> upside down triangle (filled)
;       13 ==> top diagonal half of square (open)
;       14 ==> top diagonal half of square (filled)
;       15 ==> bottom diagonal half of square (open)
;       16 ==> bottom diagonal half of square (filled)
;       17 ==> small point
;       99 ==> cross
;      100 ==> line (solid)

if (abs(psym) EQ 0) then begin
   tpsym = 10
endif
if (abs(psym) EQ 1) then begin
   a = fltarr(17)
   a(0:15) = findgen(16)*(!PI*2.0/16.0)
   a(16) = 0.0
   usersym,cos(a),sin(a)
   tpsym = 8
endif
if (abs(psym) EQ 2) then begin
   a = findgen(16)*(!PI*2.0/16.0)
   usersym,cos(a),sin(a),/fill
   tpsym = 8
endif
if (abs(psym) EQ 3) then begin
   usersym,[1.0,1.0,-1.0,-1.0,1.0],[1.0,-1.0,-1.0,1.0,1.0]
   tpsym = 8
endif
if (abs(psym) EQ 4) then begin
   usersym,[1.0,1.0,-1.0,-1.0,1.0],[1.0,-1.0,-1.0,1.0,1.0],/fill
   tpsym = 8
endif
if (abs(psym) EQ 5) then begin
   usersym,1.5*[0.0,1.0,0.0,-1.0,0.0],1.5*[1.0,0.0,-1.0,0.0,1.0]
   tpsym = 8
endif
if (abs(psym) EQ 6) then begin 
   usersym,1.5*[0.0,1.0,0.0,-1.0,0.0],1.5*[1.0,0.0,-1.0,0.0,1.0],/fill
   tpsym = 8
endif
if (abs(psym) EQ 7) then begin
   usersym,[1.0,-1.0,0.0,1.0],[-1.0,-1.0,1.0,-1.0]
   tpsym = 8
endif
if (abs(psym) EQ 8) then begin
   usersym,[1.0,-1.0,0.0,1.0],[-1.0,-1.0,1.0,-1.0],/fill
   tpsym = 8
endif
if (abs(psym) EQ 9) then begin
   usersym,[0.67,0.0,-0.67,1.0,-1.0,0.67],[-1.0,1.0,-1.0,0.33,0.33,-1.0]
   tpsym = 8
endif
if (abs(psym) EQ 10) then begin
   usersym,[0.67,0.0,-0.67,1.0,-1.0,0.67],[-1.0,1.0,-1.0,0.33,0.33,-1.0],/fill
   tpsym = 8
endif
if (abs(psym) EQ 11) then begin
   usersym,[1.0,-1.0,0.0,1.0],[1.0,1.0,-1.0,1.0]
   tpsym = 8
endif
if (abs(psym) EQ 12) then begin
   usersym,[1.0,-1.0,0.0,1.0],[1.0,1.0,-1.0,1.0],/fill
   tpsym = 8
endif
if (abs(psym) EQ 13) then begin
   usersym,[1.0,-1.0,-1.0,1.0],[1.0,-1.0,1.0,1.0]
   tpsym = 8
endif
if (abs(psym) EQ 14) then begin
   usersym,[1.0,-1.0,-1.0,1.0],[1.0,-1.0,1.0,1.0],/fill
   tpsym = 8
endif
if (abs(psym) EQ 15) then begin
   usersym,[1.0,1.0,-1.0,1.0],[1.0,-1.0,-1.0,1.0]
   tpsym = 8
endif
if (abs(psym) EQ 16) then begin
   usersym,[1.0,1.0,-1.0,1.0],[1.0,-1.0,-1.0,1.0],/fill
   tpsym = 8
endif
if (abs(psym) EQ 17) then begin
   usersym,[0.1,0.0,-0.1,0.0,0.1],[0.0,0.1,0.0,-0.1,0.0],/fill
   tpsym = 8
endif
if (abs(psym) GT 17) then begin
   tpsym = 1
endif
if (abs(psym) EQ 99) then begin
;   usersym,[0.5,0.5,0.5,0.0,0.1],[1.0,0.0,0.5,0.5,0.5]
   tpsym = 1
endif
if (abs(psym) EQ 100) then begin
   tpsym = 0
endif

if (psym LT 0) then begin
   tpsym = -1*tpsym
endif

end

; ----------------------------------------------------------------------

pro kplot,x,y,title=title,xtitle=xtitle,ytitle=ytitle,psym=psym,$
          xrange=xrange,yrange=yrange,position=position, $
          linestyle_in=linestyle_in,$
          charsize=charsize,kplot_type=kplot_type,error_bars=error_bars,$
          symsize=symsize,xstyle=xstyle,ystyle=ystyle,noerase=noerase, $
          color=color,xtickname=xtickname,ytickname=ytickname,  $
          xticks=xticks,yticks=yticks,background=background, $
          xerror=xerror,yerror=yerror,thick=thick, nodata = nodata, $
          ymargin = ymargin
  
if (not keyword_set(thick)) then begin
    thick = 1.5
    xthick = thick
    ythick = thick
    charthick = thick
endif else begin
;    thick = !P.thick
;    xthick = !P.xthick
;    ythick = !P.ythick
    charthick = !P.charthick
endelse

if (not keyword_set(xticks)) then xticks = 0
if (not keyword_set(yticks)) then yticks = 0
if (not keyword_set(xtickname)) then xtickname = ''
if (not keyword_set(ytickname)) then ytickname = ''

if (not keyword_set(color)) then begin
    color = !P.COLOR
endif else if (color EQ -1) then begin
    color = 0
endif

if (not keyword_set(background)) then background = !P.BACKGROUND
if (not keyword_set(kplot_type)) then kplot_type = 'ii'
if (not keyword_set(title)) then title = ''
if (not keyword_set(xtitle)) then xtitle = ''
if (not keyword_set(ytitle)) then ytitle = ''
if (not keyword_set(psym)) then psym = 0
if (not keyword_set(xstyle)) then xstyle = 1
if (not keyword_set(ystyle)) then ystyle = 1

if (not keyword_set(xrange)) then begin
   if ((kplot_type EQ 'oo') OR (kplot_type EQ 'oi')) then begin
      xrange = krange(x,kplot_type='o')
   endif else begin
      xrange = krange(x)
   endelse
endif

if (not keyword_set(yrange)) then begin
   if ((kplot_type EQ 'io') OR (kplot_type EQ 'oo')) then begin
      yrange = krange(y,kplot_type='o')
   endif else begin
      yrange = krange(y)
   endelse
endif

if (not keyword_set(position)) then begin
   ;position = [0.15, 0.15, 0.95, 0.95]
endif

if (not keyword_set(charsize)) then charsize = 1.4
if (not keyword_set(symsize)) then symsize = 1.5
if (not keyword_set(linestyle_in)) then linestyle_in = 0
linestyle = linestyle_in
if (not keyword_set(error_bars)) then error_bars = 'no'

set_symbol,psym,tpsym

linestyle = linestyle - 5*fix(float(linestyle)/5.0)

if (kplot_type EQ 'io') then begin
    plot_io,x,y,psym=tpsym,title=title,xtitle=xtitle,ytitle=ytitle,$
      position=position,xrange=xrange,yrange=yrange,$
      xstyle=xstyle,ystyle=ystyle,linestyle=linestyle,charsize=charsize,$
      symsize=symsize,noerase=noerase,color=color,background=background, $
      xtickname=xtickname,ytickname=ytickname,charthick=charthick, $
       xticks=xticks,yticks=yticks,thick=thick,xthick=xthick,ythick=ythick, nodata = nodata, $
       ymargin = ymargin
endif else if (kplot_type EQ 'oi') then begin
    plot_oi,x,y,psym=tpsym,title=title,xtitle=xtitle,ytitle=ytitle,$
      position=position,xrange=xrange,yrange=yrange,$
      xstyle=xstyle,ystyle=ystyle,linestyle=linestyle,charsize=charsize,$
      symsize=symsize,noerase=noerase,color=color,background=background, $
      xtickname=xtickname,ytickname=ytickname,charthick=charthick, $
      xticks=xticks,yticks=yticks,thick=thick,xthick=xthick,ythick=ythick, nodata = nodata, $
       ymargin = ymargin
endif else if (kplot_type EQ 'oo') then begin
    plot_oo,x,y,psym=tpsym,title=title,xtitle=xtitle,ytitle=ytitle,$
      position=position,xrange=xrange,yrange=yrange,$
      xstyle=xstyle,ystyle=ystyle,linestyle=linestyle,charsize=charsize,$
      symsize=symsize,noerase=noerase,color=color,background=background, $
      xtickname=xtickname,ytickname=ytickname,charthick=charthick, $
      xticks=xticks,yticks=yticks,thick=thick,xthick=xthick,ythick=ythick, nodata = nodata, $
       ymargin = ymargin
endif else begin
    plot,x,y,psym=tpsym,title=title,xtitle=xtitle,ytitle=ytitle,$
      position=position,xrange=xrange,yrange=yrange,$
      xstyle=xstyle,ystyle=ystyle,linestyle=linestyle,charsize=charsize,$
      symsize=symsize,noerase=noerase,color=color,background=background, $
      xtickname=xtickname,ytickname=ytickname,charthick=charthick, $
      xticks=xticks,yticks=yticks,thick=thick,xthick=xthick,ythick=ythick, nodata = nodata, $
       ymargin = ymargin
endelse
if (keyword_set(yerror)) then begin
    if (keyword_set(xerror)) then begin
        oploterr,x,y,xerror,yerror,psym=3;,errcolor=color,thick=thick
    endif else begin
        oploterr,x,y,yerror,psym=3,errcolor=color,thick=thick
    endelse
endif

end

; -----------------------------------------------------------------------

pro koplot,x,y,psym=psym,linestyle=linestyle,symsize=symsize,color=color, $
           xerror=xerror,yerror=yerror,thick=thick

if (not keyword_set(thick)) then thick = 1.5
if (not keyword_set(color)) then begin
    color = !P.COLOR
endif else if (color EQ -1) then begin
    color = 0
endif

if (not keyword_set(psym)) then begin
   psym = 0
endif

if (not keyword_set(symsize)) then begin
   symsize = 1.5
endif

if (not keyword_set(linestyle)) then begin
   linestyle = 0
endif

linestyle = linestyle - 5*fix(float(linestyle)/5.0)

set_symbol,psym,tpsym

oplot,x,y,psym=tpsym,linestyle=linestyle,symsize=symsize,color=color, $
  thick=thick
if (keyword_set(yerror)) then begin
    if (keyword_set(xerror)) then begin
        oploterr,x,y,xerror,yerror,psym=3,errcolor=color, $
          linestyle=linestyle,thick=thick
    endif else begin
        oploterr,x,y,yerror,psym=3,errcolor=color,thick=thick
    endelse
endif
    

end

; ----------------------------------------------------------------------

pro klegend,pos,ktype,klabel,charsize=charsize,twocol=twocol,$
            kplot_type=kplot_type,symsize=symsize,color=color, $
            line_color=line_color,thick=thick

if (not keyword_set(color)) then begin
    color = !P.COLOR
endif else if (color EQ -1) then begin
    color = 0
endif

if (not keyword_set(line_color)) then begin
    line_color = replicate(color,n_elements(klabel))
endif

if (not keyword_set(thick)) then thick = replicate(1,n_elements(klabel))

if (not keyword_set(charsize)) then begin
   charsize = 1.0
endif

if (not keyword_set(symsize)) then begin
   symsize = 1.5
endif

if (not keyword_set(twocol)) then begin
   twocol = 'no'
endif

if (not keyword_set(kplot_type)) then begin
   kplot_type = 'ii'
endif

kxrange = !x.crange
kyrange = !y.crange

npts = n_elements(klabel)

min_x = kxrange(0)
Dx = kxrange(1) - kxrange(0)
min_y = kyrange(0)
Dy = kyrange(1) - kyrange(0)

per_x = 0.03

;; Takes care of 'smashed' plots when position is set
;; Assumes most plots have a Y range of 80% of the screen

Y1 = !P.Position[1] &  Y2 = !P.Position[3]
IF Y1 EQ Y2 THEN BEGIN
   Y2 = .95 &  Y1 = .15  ;; from kplot
ENDIF
Extra_Delta_Y_Factor = 0.8/(Y2-Y1)

x_begin = pos(0)
delta_x = per_x + 0.02
y_begin = pos(1)
if (charsize LT 1.2) then begin
    delta_y = 0.04 * Extra_Delta_Y_Factor
endif else begin
    delta_y = 0.06 * Extra_Delta_Y_Factor
endelse
max_width = 0.0
yp = y_begin + (1.2)*delta_y
yp2 = y_begin + (1.0)*delta_y
for i = 0,(npts-1) do begin
   x = x_begin
   x = min_x + Dx*x
   yp = yp - delta_y
   y = min_y + Dy*yp

   beg_x = x - (per_x*Dx)
   end_x = x + (per_x*Dx)
   if ((kplot_type EQ 'oo') OR (kplot_type EQ 'oi')) then begin
      beg_x = 10^(x - (per_x*Dx))
      end_x = 10^(x + (per_x*Dx))
      x = 10^x
   endif
   if ((kplot_type EQ 'io') OR (kplot_type EQ 'oo')) then begin
      y = 10^y
   endif
   
   if (ktype(1,i) EQ 100) then begin
       koplot,[beg_x,end_x],[y,y],linestyle=abs(ktype(0,i)), $
         color=line_color(i),thick=thick[i]
   endif else if (ktype(0,i) LE 0) then begin
       koplot,[beg_x,end_x],[y,y],linestyle=abs(ktype(0,i)), $
         color=line_color(i),thick=thick[i]
      koplot,[x],[y],psym=ktype(1,i),symsize=symsize,color=line_color(i)
   endif else begin
       koplot,[x],[y],psym=ktype(1,i),symsize=symsize,color=line_color(i)
   endelse

   x = x_begin + delta_x
   x = min_x + Dx*x
   yp2 = yp2 - delta_y
   y2 = min_y + Dy*yp2

   if ((kplot_type EQ 'oo') OR (kplot_type EQ 'oi')) then begin
      x = 10^x
   endif
   if ((kplot_type EQ 'io') OR (kplot_type EQ 'oo')) then begin
      y2 = 10^y2
   endif
   
   xyouts,x,y2,klabel(i),width=tst_width,charsize=charsize,color=color
   max_width = max([max_width,tst_width])

   if ((twocol EQ 'yes') AND ((i+1) EQ fix(float(npts)/2.0))) then begin
      x_begin = x_begin + max_width + delta_x + 4.0*per_x
      yp = y_begin + (1.2)*delta_y
      yp2 = y_begin + (1.0)*delta_y
   endif

endfor

end

; ----------------------------------------------------------------------
PRO oploterr, x, y, xerr, yerr, NOHAT=hat, HATLENGTH=hln, ERRTHICK=eth, $
      ERRSTYLE=est, THICK = thick, NOCLIP=noclip, ERRCOLOR = ecol, $
      NSKIP = nskip, _EXTRA = pkey, ANONYMOUS_ = Dummy_
;+
; NAME:
;	OPLOTERR
; PURPOSE:
;	Over-plot data points with accompanying X or Y error bars.
; EXPLANATION:
;	For use instead of PLOTERR when the plotting system has already been
;	defined. 
;
; CALLING SEQUENCE:
;	oploterr, [ x,]  y, [xerr], yerr  [,/NOHAT, HATLENGTH= ,
;		 ERRTHICK= , ERRSTYLE=, ERRCOLOR = ]
; INPUTS:
;	X = array of abcissae, any datatype except string
;	Y = array of Y values, any datatype except string
;	XERR = array of error bar values (along X)
;       YERR = array of error bar values (along Y)
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;	NOHAT     = if specified and non-zero, the error bars are drawn
;	            without hats.
;	HATLENGTH = the length of the hat lines used to cap the error bars.
;	            Defaults to !D.X_VSIZE / 100).
;	ERRTHICK  = the thickness of the error bar lines.  Defaults to the
;	            THICK plotting keyword.
;	ERRSTYLE  = the line style to use when drawing the error bars.  Uses
;	            the same codes as LINESTYLE.
;	ERRCOLOR =  scalar integer (0 - !D.N_TABLE) specifying the color to
;			use for the error bars
;	NSKIP = Positive Integer specifying the error bars to be plotted.   
;		For example, if NSKIP = 2 then every other error bar is 
;		plotted; if NSKIP=3 then every third error bar is plotted.   
;		Default is to plot every error bar (NSKIP = 1)
;
; NOTES:
;       If only two parameters are input, they are taken as Y and YERR
;       If only three parameters are input, they will be taken as X, Y and
;       YERR respectively.
;
; EXAMPLE:
;       Suppose one has X and Y vectors with associated errors XERR and YERR
;	and that a plotting system has already been defined:
;
;       (1) Overplot Y vs. X with both X and Y errors and no lines connecting
;           the points
;                  IDL> oploterr, x, y, xerr, yerr, psym=3
;       (2) Like (1) but overplot only the Y errors bars and omits "hats"
;                  IDL> oploterr, x, y, yerr, psym=3, /NOHAT
;
; PROCEDURE:
;	A plot of X versus Y with error bars drawn from Y - YERR to Y + YERR
;	and optionally from X - XERR to X + XERR is written to the output device
;
; WARNING:
;	This an enhanced version of a procedure that already exists in the
;	standard IDL V4.0 distribution.   Any call to the standard IDL version
;	should also work with this version, but the reverse is not true.
;
; MODIFICATION HISTORY:
;	Adapted from the most recent version of PLOTERR.  M. R. Greason,
;		Hughes STX, 11 August 1992.
;       Removed spurious keywords for IDL V3.0.0  W. Landsman Jan. 1993 
;	Added ability to plot a single point W. Landsman   July 1993
;	Added COLOR keyword option to error bars W. Landsman   November 1993
;	Remove CHANNEL call for V4.0 compatibility W. Landsman June 1995
;	Add ERRCOLOR, use _EXTRA keyword,           W. Landsman, July 1995
;	Remove spurious call to PLOT_KEYWORDS     W. Landsman, August 1995
;	OPLOT more than 32767 error bars          W. Landsman, Feb 1996
;	Added NSKIP keyword                       W. Landsman, Dec 1996
;	Converted to IDL V5.0   W. Landsman   September 1997
;       fixed error in use of plots.pro     K. Gordon October 1997
;-
;			Check the parameters.
;
 On_error, 2
 np = N_params()
 IF (np LT 2) THEN BEGIN
	print, "OPLOTERR must be called with at least two parameters."
	print, "Syntax: oploterr, [x,] y, [xerr], yerr, [..oplot keywords... "
	print,'     /NOHAT, HATLENGTH = , ERRTHICK=, ERRSTLYE=, ERRCOLOR=]'
	RETURN
 ENDIF

; Error bar keywords (except for HATLENGTH; this one will be taken care of 
; later, when it is time to deal with the error bar hats).

 IF (keyword_set(hat)) THEN hat = 0 ELSE hat = 1
 if not keyword_set(THICK) then thick = !P.THICK
 IF (n_elements(eth) EQ 0) THEN eth = thick
 IF (n_elements(est) EQ 0) THEN est = 0
 IF (n_elements(ecol) EQ 0) THEN ecol = !P.COLOR
 if N_elements( NOCLIP ) EQ 0 THEN noclip = 0
 if not keyword_set(NSKIP) then nskip = 1
;
; If no X array has been supplied, create one.  Make sure the rest of the 
; procedure can know which parameter is which.
;
 IF np EQ 2 THEN BEGIN			; Only Y and YERR passed.
	yerr = abs(y)
	yy = x
	xx = indgen(n_elements(yy))
        xerr = make_array(size=size(xx))

 ENDIF ELSE IF np EQ 3 THEN BEGIN 	; X, Y, and YERR passed.
        yerr = abs(xerr)
        yy = y
        xx = x

 ENDIF ELSE BEGIN                        ; X, Y, XERR and YERR passed.
	yerr = abs(yerr)
	yy = y
        xerr = abs(xerr)
	xx = x
 ENDELSE
;
;			Determine the number of points being plotted.  This
;			is the size of the smallest of the three arrays
;			passed to the procedure.  Truncate any overlong arrays.
;

 n = N_elements(xx) < N_elements(yy)

 IF np GT 2 then n = n < N_elements(yerr)   
 IF np EQ 4 then n = n < N_elements(xerr)

 xx = xx[0:n-1]
 yy = yy[0:n-1]
 yerr = yerr[0:n-1]
 IF np EQ 4 then xerr = xerr[0:n-1]

 ylo = yy - yerr
 yhi = yy + yerr

 if Np EQ 4 then begin
     xlo = xx - xerr
     xhi = xx + xerr
 endif
;
;			Plot the positions.
;
 if n NE 1 then begin
     oplot, xx, yy, NOCLIP=noclip,THICK = thick,_EXTRA = pkey 
 endif else begin 
     oplot, xx, yy, NOCLIP=noclip,THICK = thick,_EXTRA = pkey
 endelse
;
;	Plot the error bars.   Compute the hat length in device coordinates
;       so that it remains fixed even when doing logarithmic plots.
;
    data_low = convert_coord(xx,ylo,/TO_DEVICE)
    data_hi = convert_coord(xx,yhi,/TO_DEVICE)
    if NP EQ 4 then begin
       x_low = convert_coord(xlo,yy,/TO_DEVICE)
       x_hi = convert_coord(xhi,yy,/TO_DEVICE)
    endif
    ycrange = !Y.CRANGE   &  xcrange = !X.CRANGE
    
FOR i = 0L, (n-1), Nskip DO BEGIN

    oplot, [xx[i],xx[i]], [ylo[i],yhi[i]], LINESTYLE=est,THICK=eth,  $
		NOCLIP = noclip, COLOR = ecol
;                                                         Plot X-error bars 
;
    if np EQ 4 then oplot, [xlo[i],xhi[i]],[yy[i],yy[i]],LINESTYLE=est, $
		THICK=eth, COLOR = ecol, NOCLIP = noclip
    IF (hat NE 0) THEN BEGIN
        IF (N_elements(hln) EQ 0) THEN begin
            if (!x.type EQ 0) then begin
                hln_y = (xcrange(1) - xcrange(0))/100.0
                exx1 = xx[i] - hln_y/2.
                exx2 = exx1 + hln_y
            endif else begin
                hln_y = (xcrange(1) - xcrange(0))/100.0
                exx1 = 10^(alog10(xx[i]) - hln_y/2)
                exx2 = 10^(alog10(exx1) + hln_y)
            endelse
        endif
        oplot, [exx1,exx2], [ylo[i],ylo[i]], $
          COLOR=ecol,LINESTYLE=est,THICK=eth, noclip = noclip
        oplot, [exx1,exx2], [yhi[i],yhi[i]], $
          COLOR = ecol,LINESTYLE=est,THICK=eth, noclip = noclip

        IF np EQ 4 THEN BEGIN
            IF (N_elements(hln) EQ 0) THEN begin
                if (!y.type EQ 0) then begin
                    hln_x = (ycrange(1) - ycrange(0))/100.0
                    eyy1 = yy[i] - hln_x/2.
                    eyy2 = eyy1 + hln_x
                endif else begin
                    hln_x = (ycrange(1) - ycrange(0))/100.0
                    eyy1 = 10^(alog10(yy[i]) - hln_x/2)
                    eyy2 = 10^(alog10(eyy1) + hln_x)
                endelse
            endif
            oplot, [xlo[i],xlo[i]], [eyy1,eyy2],COLOR = ecol, $
              LINESTYLE=est,THICK=eth, NOCLIP = noclip
            oplot, [xhi[i],xhi[i]], [eyy1,eyy2],COLOR = ecol, $
              LINESTYLE=est,THICK=eth, NOCLIP = noclip
        ENDIF
    ENDIF
    NOPLOT:
ENDFOR
;
RETURN
END
