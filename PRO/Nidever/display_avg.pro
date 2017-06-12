pro display_avg,ima, imw, xs, ys, $
                Title=tit, XTitle=xtit, YTitle=ytit, $
                LOG=log_scaling, $
                LEVELS=l, HIST=hist,$
                bot=bot, top=top, $
                ASPECT=aspect, $
                INTERPOLATE=interp, $
                MASKVALUE=maskvalue, $
                PSFINE=psfine, $
                NO_EXPAND=no_expand, $
                NOERASE=noerase, $
                HELP=help,$
                charsize=charsize,$
                noframe=noframe,$
                xstyle=xstyle,$
                ystyle=ystyle,$
                xticks=xticks,$
                yticks=yticks,$
                framecolor=framecolor,$
                position=position,$
                decomp=decomp,$
                xrange=xrange,yrange=yrange,$
                notop=notop,xflip=xflip,yflip=yflip,$
                stp=stp,thick=thick,charthick=charthick,$
                background_color=background_color,$
                color=color,center=center,aitoff=aitoff,$
                iso=iso,orthographic=orthographic,$
                p0lat=p0lat,p0lon=p0lon,rotation=rotation,$
                rev=rev,xlog=xlog,ylog=ylog,square=square,$
                xminor=xminor,yminor=yminor,xticklen=xticklen,$
                yticklen=yticklen,xtickformat=xtickformat,$
                ytickformat=ytickformat,grid=grid,$
                noplot=noplot,save=save,file=file,$
                nocolorbar=nocolorbar,$
                onlyim=onlyim, minhue=minhue,maxhue=maxhue,minbright=minbright,$
                maxbright=maxbright, saturation=saturation,$
                posim=posim,poscol=poscol,im=im

;+
; This allows you to display an image that is weighted
; by another image
;
; INPUTS:
;  ima   The image
;  imw   The image to weight by
;  plus many more
;
; OUTPUTS:
;  im       The final 3-color image
;  Displays the image
;
; By D.Nidever  November 2006
;-

; Copied from hess_avg.pro

; Not enough inputs
if n_elements(ima) eq 0 or n_elements(imw) eq 0 then begin
  print,'Syntax - display_avg,ima, imw, xs, ys'
  print,'                Title=tit, XTitle=xtit, YTitle=ytit,'
  print,'                LOG=log_scaling, LEVELS=l, HIST=hist,'
  print,'                bot=bot, top=top, ASPECT=aspect,'
  print,'                INTERPOLATE=interp, MASKVALUE=maskvalue,'
  print,'                PSFINE=psfine,NO_EXPAND=no_expand,'
  print,'                NOERASE=noerase, HELP=help,'
  print,'                charsize=charsize,noframe=noframe,'
  print,'                xstyle=xstyle, ystyle=ystyle,'
  print,'                xticks=xticks, yticks=yticks,'
  print,'                framecolor=framecolor, position=position,'
  print,'                decomp=decomp, xrange=xrange,yrange=yrange,'
  print,'                notop=notop,xflip=xflip,yflip=yflip,'
  print,'                stp=stp,thick=thick,charthick=charthick,'
  print,'                background_color=background_color,'
  print,'                color=color,center=center,aitoff=aitoff,'
  print,'                iso=iso,orthographic=orthographic,'
  print,'                p0lat=p0lat,p0lon=p0lon,rotation=rotation,'
  print,'                rev=rev,xlog=xlog,ylog=ylog,square=square,'
  print,'                xminor=xminor,yminor=yminor,xticklen=xticklen,'
  print,'                yticklen=yticklen,xtickformat=xtickformat,'
  print,'                ytickformat=ytickformat,grid=grid,'
  print,'                noplot=noplot,save=save,file=file,'
  print,'                nocolorbar=nocolorbar,onlyim=onlyim,'
  print,'                minhue=minhue,maxhue=maxhue,minbright=minbright,'
  print,'                maxbright=maxbright, saturation=saturation,'
  print,'                posim=posim,poscol=poscol'
  return
endif

sz = size(ima)
nx = sz(1)
ny = sz(2)

imw2 = imw
ima2 = ima

; Max/Min and Log scaling of the weighting image
imw2 = ImgScl(imw, Min=bot, Max=top, Top=!D.Table_Size-1, $
		  	  Log=log_scaling, Levels=l, MaskValue=maskvalue)


;; TOP
;if keyword_set(top) then begin
;  tbd = where(imw2 gt top,ntbd)
;  if ntbd gt 0 then (imw2)(tbd) = top
;endif
;
;; BOT
;if keyword_set(bot) then begin
;  bbd = where(imw2 lt bot,nbbd)
;  if nbbd gt 0 then (imw2)(bbd) = bot
;endif


; convert image to RGB, using HLS
; hue is Average (IMA)   (0-360)
; 0-red, 120-green, 240-blue
; brightness is Total (im) (0-1)
ima2 = -ima2    ; (blue-green-red)
if n_elements(minhue) eq 0 then minhue = 0.
if n_elements(maxhue) eq 0 then maxhue = 240  ; 254
if n_elements(minbright) eq 0 then minbright = 0.10  ; 0.00
if n_elements(maxbright) eq 0 then maxbright = 0.70  ; 1.00
if n_elements(saturation) eq 0 then saturation = 0.9  ;1.0
hue = scale_vector(ima2,minhue,maxhue)
bright = scale_vector(imw2,minbright,maxbright)
sat = imw*0.+saturation

if max(imw2) eq min(imw2) then bright = 0.5 * imw2/max(imw2)

color_convert, hue, bright, sat, r, g, b, /HLS_RGB
;color_convert, hue, im*0.+1.0, bright, r, g, b, /HSV_RGB


; setting bottom to zero
;if keyword_set(bot) then begin
;  bbd = where(imw2 lt bot,nbbd)
;  if nbbd gt 0 then begin
;    (r)(bbd) = 0.
;    (g)(bbd) = 0.
;    (b)(bbd) = 0.
;  endif
;endif



; Interleaved image
im2 = bytarr(3,nx,ny)
im2(0,*,*) = r
im2(1,*,*) = g
im2(2,*,*) = b

; Final image to plot
im = im2

if not keyword_set(color) then loadct,0,/silent else loadct,39,/silent


; *** PLOTTING ***
if not keyword_set(file) then file='display_avg'

  if not keyword_set(noplot) then begin
    if keyword_set(save) then begin
      ps_open,file,color=color
      if keyword_set(color) then loadct,39,/silent
  
    endif else begin
      if (!d.name ne 'PS' and !d.name ne 'Z') then begin
        if keyword_set(color) then begin
          loadct,39,/silent
          device,decomposed=0
        endif else device,decomposed=1
      endif else begin
        loadct,39,/silent
      endelse
    endelse
  endif ; not noplot

  ; Axis titles
  if n_elements(xtit) eq 0 then xtit='X'
  if n_elements(ytit) eq 0 then ytit='Y'

  ; Plotting it
  if not keyword_set(position) then position = [0.,0.,1.,1.]
  dx1 = position[2]-position[0]
  dy1 = position[3]-position[1]
  x0 = position[0]
  y0 = position[1]
  if not keyword_set(onlyim) then $
  pos = [0.08*dx1+x0,0.10*dy1+y0,0.95*dx1+x0,0.85*dy1+y0]
  if keyword_set(onlyim) then $
  pos = [0.08*dx1+x0,0.10*dy1+y0,0.95*dx1+x0,0.95*dy1+y0]
  if keyword_set(posim) then pos = posim


  ; Display the image
  if not keyword_set(noplot) then $
  display,im,xs, ys, $
          Title=tit, XTitle=xtit, YTitle=ytit, $
          LOG=log_scaling, $
          LEVELS=l, HIST=hist,$
          ASPECT=aspect, $
          INTERPOLATE=interp, $
          MASKVALUE=maskvalue, $
          PSFINE=psfine, $
          NO_EXPAND=no_expand, $
          NOERASE=noerase, $
          HELP=help,$
          charsize=charsize,$
          noframe=noframe,$
          xstyle=xstyle,$
          ystyle=ystyle,$
          xticks=xticks,$
          yticks=yticks,$
          framecolor=framecolor,$
          position=pos,$
          decomp=decomp,$
          xrange=xrange,yrange=yrange,$
          notop=notop,xflip=xflip,yflip=yflip,$
          thick=thick,charthick=charthick,$
          background_color=background_color,$
          color=color,center=center,aitoff=aitoff,$
          iso=iso,orthographic=orthographic,$
          p0lat=p0lat,p0lon=p0lon,rotation=rotation,$
          rev=rev,xlog=xlog,ylog=ylog,square=square,$
          xminor=xminor,yminor=yminor,xticklen=xticklen,$
          yticklen=yticklen,xtickformat=xtickformat,$
          ytickformat=ytickformat,grid=grid


  colpos = [0.08*dx1+x0,0.92*dy1+y0,0.95*dx1+x0,0.95*dy1+y0]
  if keyword_set(poscol) then colpos = poscol

  ; Overplotting the colorbar
  if not keyword_set(noplot) then begin
  if not keyword_set(nocolorbar) and not keyword_set(onlyim) then begin
    if keyword_set(color) then loadct,39,/silent

    if (n_elements(charsize) eq 0) then charsize=1.0
    ;if n_elements(minim) eq 0 then minim = min(ima)
    ;if n_elements(maxim) eq 0 then maxim = max(ima)
    minim = min(ima)
    maxim = max(ima)

    ; Setting the format of the colorbar annotation
    len = strlen(strtrim(long(maxim),2))
    if minim lt 0. then len = len+1
    if maxim lt 100 then form = '(F'+strtrim(len+3,2)+'.1)'
    if maxim lt 0.1 then form = '(G8.2)'                  ; room for two sig figs, sign and exponent
    if maxim ge 100 then form = '(I'+strtrim(len,2)+')'
    if maxim gt 1e5 then form = '(G8.2)'                  ; room for two sig figs, sign and exponent
    if keyword_set(log) and minim lt 1.0 then form = '(G'+strtrim(len+4,2)+'.2)'
    div = abs(minim)
    if div eq 0 then div=1
    npow = alog10(abs(maxim)/div)
    if (npow gt 4) then form = '(G8.2)'                   ; for large ranges

    if not keyword_set(color) then loadct,0,/silent
    if keyword_set(log) and not keyword_set(avg) then begin
      xlog = 1
      minor = 9
      divisions = 0
    endif
    minrange = minim
    if minim eq 0. and keyword_set(log) then minrange = 1.
    maxrange= maxim


    bottom = 10
    ncolors = 245
    if keyword_set(avg) then begin
      bottom=50
      ncolors=200
    endif

    colorbar,position=colpos,minrange=minrange,maxrange=maxrange,$
             charsize=charsize,format=form,bottom=bottom,ncolors=ncolors,$
             xlog=xlog,minor=minor,divisions=divisions,thick=thick,$
             charthick=charthick,color=framecolor,xthick=thick,$
             ythick=thick

  endif ; not /nocolorbar
  endif ; not /noplot

if keyword_set(save) then ps_close

if keyword_set(stp) then stop

end
