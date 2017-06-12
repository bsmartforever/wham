pro selector,x,y,ind,sort=sort,silent=silent,xr=xr,yr=yr,$
             overplot=overplot,spherical=spherical,plotsym=plotsym,$
             color=color,symsize=symsize,thick=thick,backgd=backgd,$
             inpind=inpind,stp=stp,wt=wt,nowait=nowait

;+
;
; SELECTOR
;
; This program can be used to select points from a plot.
; Points can be selected and/or de-selected.
; Also see the CLICKER.PRO program.
;
; INPUT:
;  x           Array of x-values
;  y           Array of y-values
;  =inpind     Start with these pre-selected indices.
;  /sort       Sort the indices
;  /silent    Don't print anything
;  xr=xr       X-range for plotting window
;  yr=yr       Y-range for plotting window
;  /overplot   Don't replot the points
;  /spherical  We're using spherical coordinates, RA,DEC (degrees)
;  =thick      Thickness of the plot symbols
;  =plotsym    The plot symbol. Default plotsym=7       
;  =symsize    The symbol size. Default symsize=1.5
;  =color      The symbol color. Default color=255
;  =backgd     The background color. Default backgd=0.
;  =wt         How long to wait between cursor commands.  Default
;                wt=0.2 sec
;  /nowait     Don't wait between cursor commands.  Set if you want to
;                select many points.  Identical to wt=0.
;  /stp        Stop at the end of the program.
;
; OUTPUT:
;  ind         Array of indices of selected points.
;
; USAGE:
;  IDL>selector,x,y,ind
;
; Written by D.Nidever  December 2004
;-


; Not enough inputs
nx = n_elements(x)
ny = n_elements(y)
if nx eq 0 or ny eq 0 then begin
  print,'Syntax - selector,x,y,ind,sort=sort,silent=silent,xr=xr,yr=yr,'
  print,'                  overplot=overplot,spherical=spherical,plotsym=plotsym,'
  print,'                  color=color,symsize=symsize,thick=thick,backgd=backgd,'
  print,'                  inpind=inpind,stp=stp,wt=wt,nowait=nowait'
  return
endif

; Defaults
if n_elements(plotsym) eq 0 then ps=7 else ps=plotsym
if n_elements(color) eq 0 then color=255
if n_elements(symsize) eq 0 then symsize=1.5
if n_elements(wt) eq 0 then wt=0.2
if keyword_set(nowait) then wt=0.0
psym8
n = 0.

; Getting plotting region
if keyword_set(overplot) then begin
  pos = [ (convert_coord(!p.clip(0:1),/device,/to_data))[0:1],$
        (convert_coord(!p.clip(2:3),/device,/to_data))[0:1] ]
  xr = [pos(0),pos(2)]
  yr = [pos(1),pos(3)]
endif else begin
  dx = float(max(x)-min(x))
  dy = float(max(y)-min(y))
  fac=0.1
  if not keyword_set(xr) then xr=[min(x)-dx*fac,max(x)+dx*fac]
  if not keyword_Set(yr) then yr=[min(y)-dy*fac,max(y)+dy*fac]
  plot,x,y,xtit='X',ytit='Y',ps=8,symsize=0.5,xr=xr,yr=yr,xs=1,ys=1,thick=thick
endelse

; Scale factors for each dimension
dx = xr[1]-xr[0]
dy = yr[1]-yr[0]


; Starting index of stars are haven't been selected yet
leftind = lindgen(nx)


if not keyword_set(silent) then begin
  print,''
  print,'Click on a point to SELECT it'
  print,'Right-Click on a point to DE-SELECT it'
  print,'Click outside the box or hit the middle mouse button to stop selecting'
  print,''
endif

if not keyword_set(silent) then begin
  print,'----------------------------------------------------------'
  print,'      NUM       X           Y        IND                  '
  print,'----------------------------------------------------------'
endif

; Indices input
if n_elements(inpind) gt 0 then begin
  ind = long(inpind)
  n = n_elements(ind)
  oplot,[x[ind]],[y[ind]],ps=ps,color=color,symsize=symsize,thick=thick

  ; Print their information
  if not keyword_set(silent) then begin
    format = '(I8,F12.3,F12.3,I8,A17)'
    for i=0,n-1 do $
      print,format=format,i+1,float(x[ind[i]]),float(y[ind[i]]),ind[i],' PRE-SELECTED'
  endif

  ; Removing these from leftind
  if n lt nx then begin
    remove,ind,leftind
  endif else begin
    undefine,leftind
  endelse

endif


; Getting first click
CURSOR,curx,cury


;count = 0
count = n
while (curx le max(xr) and curx ge min(xr) and cury le max(yr) and cury ge min(yr)) $
      and !mouse.button ne 2 do begin

  CASE !mouse.button of

    ; No button, do nothing
    0:

    ; Regular buton, SELECT point
    1: begin


      ;; Getting the distance
      ;if not keyword_set(spherical) then begin
      ;  diff = sqrt( ((x-curx)/dx)^2. + ((y-cury)/dy)^2. )
      ;endif else begin  ; using spherical coordinates
      ;  diff = sphdist(x,y,curx,cury,/degree)
      ;endelse
      ;gd = minloc(diff,/first)

      ; The first one 
      if n eq 0 then begin

        ; Getting the distance
        if not keyword_set(spherical) then begin
          diff = sqrt( ((x-curx)/dx)^2. + ((y-cury)/dy)^2. )
        endif else begin  ; using spherical coordinates
          diff = sphdist(x,y,curx,cury,/degree)
        endelse
        gd = minloc(diff,/first)

        ind = gd[0]

        ; Remove from leftind
        nleftind = n_elements(leftind)
        if nleftind gt 1 then begin
          remove,gd[0],leftind
        endif else begin
          undefine,leftind
        endelse

        ; Printing and plotting
        format = '(I8,F12.3,F12.3,I8)'
        if not keyword_set(silent) then $
          print,format=format,count+1,float(x[gd]),float(y[gd]),gd[0]
        oplot,[x[gd]],[y[gd]],ps=ps,color=color,symsize=symsize,thick=thick


      ; Not the FIRST point selected
      ; Get the point closest that has not already been selected
      endif else begin

        ; We haven't selected all of the points yet
        if n_elements(leftind) gt 0 then begin

          ;leftind = lindgen(nx)
          ;REMOVE,ind,leftind

          ; Getting the distance
          if not keyword_set(spherical) then begin
            diff = sqrt( ((x[leftind]-curx)/dx)^2. + ((y[leftind]-cury)/dy)^2. )
          endif else begin  ; using spherical coordinates
            diff = sphdist(x[leftind],y[leftind],curx,cury,/degree)
          endelse
          leftgd = minloc(diff,/first)
          gd = leftind[leftgd]

          ; Add to ind
          ind = [ind,gd[0]]


          ; Remove from leftind
          nleftind = n_elements(leftind)
          if nleftind gt 1 then begin
            ;remove,gd[0],leftind
            remove,leftgd[0],leftind
          endif else begin
            undefine,leftind
          endelse

          ; Printing and plotting
          format = '(I8,F12.3,F12.3,I8)'
          if not keyword_set(silent) then $
            print,format=format,count+1,float(x[gd]),float(y[gd]),gd[0]
          oplot,[x[gd]],[y[gd]],ps=ps,color=color,symsize=symsize,thick=thick


        ; ALL of the points have been selected
        endif else begin
          print,'ALL points have been selected'
        endelse

       endelse ; not the first selected

     end ; normal button


   ; Center button, end
   2: begin
        ; Don't need to do anything
      end


   ; Right button, DE-SELECT point
   4: begin


      ; We have some selected points
      if n gt 0 then begin

        ; Getting the distance
        if not keyword_set(spherical) then begin
          diff = sqrt( ((x[ind]-curx)/dx)^2. + ((y[ind]-cury)/dy)^2. )
        endif else begin  ; using spherical coordinates
          diff = sphdist(x[ind],y[ind],curx,cury,/degree)
        endelse
        gd = minloc(diff,/first)

        ; Print out the information
        format = '(I8,F12.3,F12.3,I8,A17)'
        if not keyword_set(silent) then $
          print,format=format,count+1,float(x[ind[gd]]),float(y[ind[gd]]),ind[gd[0]],' DE-SELECTED'

        ; "Erase" the point from the plot window
        ;  Hopefully color=0 is the background
        if not keyword_set(backgd) then backgd=0
        oplot,[x[ind[gd]]],[y[ind[gd]]],ps=ps,color=backgd,symsize=symsize,thick=thick


        ; Put back into LEFTIND
        if n_elements(leftind) gt 0 then begin
          leftind = [leftind,ind[gd[0]]]
        endif else begin
          leftind = [ind[gd[0]]]
        endelse

        ; Remove the point from IND
        if n eq 1 then begin
          undefine,ind
        endif else begin
          remove,gd[0],ind
        endelse
        n = n_elements(ind)


      ; No points selected yet
      endif else begin
        ; Don't need to do anything
      endelse


     end  ; right button


    ; Multiple mouse buttons
    else: begin
            ; Don't do anything
          end

  ENDCASE

  ; Getting cursor position
  wait,wt
  CURSOR,curx,cury

  n = n_elements(ind)

  count = count+1

end

if not keyword_set(silent) then $
  print,'----------------------------------------------------------'
wait,wt

; sort the indices
if keyword_set(sort) then begin
  si = sort(ind)
  ind = ind[si]
endif

if keyword_set(stp) then stop

end
