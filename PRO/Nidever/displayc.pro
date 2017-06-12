pro displayc,im,xarr,yarr,color=color,nx=nx,ny=ny,$
          xtit=xtit,ytit=ytit,tit=tit,log=log,interpolate=interpolate,$
          noplot=noplot,save=save,file=file,xrange=xrange,yrange=yrange,$
          stp=stp,maxim=maxim,minim=minim,$
          nocolorbar=nocolorbar,position=position,$
          onlyim=onlyim,noerase=noerase,charsize=charsize,$
          xflip=xflip,yflip=yflip,$
          posim=posim,poscol=poscol,thick=thick,charthick=charthick,$
          framecolor=framecolor,background_color=background_color,$
          colcharsize=colcharsize,xstyle=xstyle,ystyle=ystyle,$
          invert=invert,center=center,xtickformat=xtickformat,$
          ytickformat=ytickformat,xticks=xticks,yticks=yticks,$
          out_posim=out_posim,out_poscol=out_poscol,xsize=xsize,$
          ysize=ysize

;+
;  INPUTS
;   im       2D image
;   x        Array of x-values
;   y        Array of y-values
;   /color   Plot in color
;   xtit     Title for x-axis on plot
;   ytit     Title for y-axis on plot
;   tit      Overall plot title
;   /log     Plot the logarithm
;   /interp  Interpolate between the points
;   /noplot  Don't plot anything
;   /save    Save the plot to a poscript file
;   file     Name of postscript file to save the plot to
;   xrange   X-axis plot range
;   yrange   Y-axis plot range
;   /stp     Stop at the end of the program
;   maxim    The largest value to plot
;   minim    The lowest value to plot
;   /nocolorbar   Don't overplot the colorbar
;   position The position to put the plot in, in normalized coordinates
;   /onlyim  Don't plot the colorbar and don't leave space for it.
;   /noerase Don't erase the screen before you plot the image
;   charsize Character size
;   colcharsize  Character size for the colorbar alone
;   posim    Position of image (in normalized coordinates)
;   poscol   Positin of color bar (in normalized coordinates)
;   thick    The thickness of lines
;   charthick The thickness of the annotations
;   framecolor  The color of the box and annotations
;   background_color The color of the background
;   invert   Black on white instead of white on black.
;   /center  The coordinates plotted should be the center of bins (default).
;
; Created by David Nidever October 2005
;-

nim = n_elements(im)

if (n_params() eq 0) or (nim eq 0) then begin
  print,'Syntax displayc,im,x,y,color=color,'
  print,'                xtit=xtit,ytit=ytit,tit=tit,log=log,interpolate=interpolate'
  print,'                noplot=noplot,save=save,file=file,xrange=xrange,yrange=yrange'
  print,'                stp=stp'
  return
endif

sz = size(im)
if sz(0) lt 2 or sz(0) gt 3 then begin
  print,'The image is not the right dimension
  return
endif

if sz(0) eq 2 then begin
  if n_elements(xarr) eq 0 then xarr=findgen(sz(1))
  if n_elements(yarr) eq 0 then yarr=findgen(sz(2))
endif else begin
  if n_elements(xarr) eq 0 then xarr=findgen(sz(2))
  if n_elements(yarr) eq 0 then yarr=findgen(sz(3))
endelse

orig_im = im
orig_xarr = xarr
orig_yarr = yarr

ymin = min(yarr)
ymax = max(yarr)
xmin = min(xarr)
xmax = max(xarr)

if keyword_set(xrange) then begin
  if (xrange(0) lt xmin) or (xrange(1) gt xmax) then begin
    xmin = xrange(0) < xmin
    xmax = xrange(1) > xmax
  endif
endif
if keyword_set(yrange) then begin
  if (yrange(0) lt ymin) or (yrange(1) gt ymax) then begin
    ymin = yrange(0) < ymin
    ymax = yrange(1) > ymax
  endif
endif

; MAXIM
if keyword_set(maxim) then begin
  tbd = where(im gt maxim,ntbd)
  if ntbd gt 0 then (im)(tbd) = maxim
endif

; MINIM
if keyword_set(minim) then begin
  bbd = where(im lt minim,nbbd)
  if nbbd gt 0 then (im)(bbd) = minim
endif


; Plotting
if not keyword_set(noplot) then begin
  if not keyword_set(file) then file='displayc'

  if keyword_set(save) then begin
    ps_open,file,color=color
    if keyword_set(color) then loadct,39

  endif else begin
    if (!d.name ne 'PS' and !d.name ne 'Z') then begin
      if keyword_set(color) then begin
        loadct,39
        ;device,decomposed=0
      endif ;else device,decomposed=1
    endif else begin
      loadct,39
    endelse
  endelse

  ; Using only a certain range
  if keyword_set(xrange) then begin
    dum = closest(xrange(0),xarr,ind=xlo)
    dum = closest(xrange(1),xarr,ind=xhi)
    imsz = size(im)
    if imsz(0) eq 3 then im = im(*,xlo:xhi,*) else im=im(xlo:xhi,*)
    xarr = xarr(xlo:xhi)
  endif
  if keyword_set(yrange) then begin
    dum = closest(yrange(0),yarr,ind=ylo)
    dum = closest(yrange(1),yarr,ind=yhi)
    imsz = size(im)
    if imsz(0) eq 3 then im = im(*,*,ylo:yhi) else im=im(*,ylo:yhi)
    yarr = yarr(ylo:yhi)
  endif

  ; Plotting it
  if not keyword_set(position) then position = [0.,0.,1.,1.]
  dx1 = position(2)-position(0)
  dy1 = position(3)-position(1)
  x0 = position(0)
  y0 = position(1)
  if not keyword_set(onlyim) then $
  pos = [0.08*dx1+x0,0.10*dy1+y0,0.95*dx1+x0,0.85*dy1+y0]
  if keyword_set(onlyim) then $
  pos = [0.08*dx1+x0,0.10*dy1+y0,0.95*dx1+x0,0.95*dy1+y0]
  if keyword_set(posim) then pos = posim

  pim = im
  if keyword_set(invert) then pim=-pim

  display,pim,xarr,yarr,interpolate=interpolate,log=log,xtit=xtit,$
          ytit=ytit,tit=tit,pos=pos,notop=notop,noerase=noerase,$
          charsize=charsize,xflip=xflip,yflip=yflip,thick=thick,$
          charthick=charthick,framecolor=framecolor,$
          background_color=background_color,color=color,$
          xstyle=xstyle,ystyle=ystyle,xtickformat=xtickformat,$
          ytickformat=ytickformat,xticks=xticks,yticks=yticks,$
          max=maxim,min=minim,xsize=xsize,ysize=ysize


  colpos = [0.08*dx1+x0,0.92*dy1+y0,0.95*dx1+x0,0.95*dy1+y0]
  ;colpos = [0.08,0.92,0.95,0.95]
  if keyword_set(poscol) then colpos = poscol

  ; The actual positions used
  out_posim = pos
  out_poscol = colpos

  ; Overplotting the colorbar
  if not keyword_set(nocolorbar) and not keyword_set(onlyim) then begin
    if keyword_set(color) then loadct,39

    if not keyword_set(charsize) then charsize=1.0
    ;minim = min(im)
    ;maxim = max(im)

    ; Setting the format of the colorbar annotation
    ; from hess.pro
    len = strlen(strtrim(long(maxim),2))
    if minim lt 0. then len = len+1         ; need room for the minus sign
    if maxim lt 100 then form = '(G'+strtrim(len+3,2)+'.2)'
    if maxim lt 0.1 then form = '(G8.2)'                  ; room for two sig figs, sign and exponent
    if maxim ge 100 then form = '(I'+strtrim(len,2)+')'
    if maxim gt 1e5 then form = '(G8.2)'                  ; room for two sig figs, sign and exponent
    if keyword_set(log) and minim lt 1.0 then form = '(G'+strtrim(len+4,2)+'.2)'
    div = abs(minim)
    if div eq 0 then div=1
    npow = alog10(abs(maxim)/div)
    if (npow gt 4) then form = '(G8.2)'                   ; for large ranges


    ;len = strlen(strtrim(long(maxim),2))
    ;if minim lt 0. then len = len+1
    ;if maxim lt 100 then form = '(F'+strtrim(len+3,2)+'.1)'
    ;if maxim ge 100 then form = '(I'+strtrim(len,2)+')'
    ;if not keyword_set(color) then loadct,0,/silent
    if keyword_set(log) and not keyword_set(avg) then begin
      xlog = 1
      minor = 9
      divisions = 0
    endif
    ;if minim eq 0. then minrange = 1. else minrange = minim
    minrange = minim
    if minim eq 0. and keyword_set(log) then minrange = 1.   ; check imgscl.pro, log=0.01
    maxrange= maxim

    ; Scale of colors
    if keyword_set(avg) then begin
      bottom=50
      ncolors=200
    endif else begin
      bottom=0       ;10
      ncolors=255    ;245
    endelse

    if not keyword_set(colcharsize) then colcharsize=charsize

    colorbar,position=colpos,minrange=minrange,maxrange=maxrange,$
              charsize=colcharsize,format=form,bottom=bottom,ncolors=ncolors,$
              xlog=xlog,minor=minor,divisions=divisions,thick=thick,$
              charthick=charthick,color=framecolor,xthick=thick,$
              ythick=thick,invertcolor=invert

  endif ; not /nocolorbar

  if keyword_set(save) then ps_close

endif

if keyword_set(stp) then stop

im = orig_im
xarr = orig_xarr
yarr = orig_yarr

end
