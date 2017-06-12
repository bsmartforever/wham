
;+
;
; NAME:
;  CHART
;
; PURPOSE:
;  This is an IDL version of the IRAF CHART program to interactively view data
;
; INPUTS:
;  input  The data.  Can be in various formats: (1) Filename to the data.
;         (2) Structure. (3) 2D array.  The array will be converted to
;         a structure with field names like "FIELD0, FIELD1, ...".
;         Consider converting the 2D array to a structure first.
;  keys   A graph defintions file.
;  /stp   Stops at the end of the program
;
; OUTPUTS:
;  Data is plotted to the screen and can be manipulated interactively
;
; PROGRAMS:
;  chart_checksavefile   Checks if a file is an IDL save file or not
;  chart_getinput        This gets the input data
;  chart_getdata         Gets xarr/yarr for a panel from the "data" structure
;  chart_panelpositions  Sets up the panel positions
;  chart_definepanel     Defines a panel
;  chart_mkpstruc        Makes initial panel setup
;  chart_findclosest     Finds closest point to cursor position
;  chart_norm2data       Converts normalized coordinates to data coordinates
;                         and the panel number
;  chart_replot          Replots the data
;  chart_print           Print all the data for one point
;  chart_mousemode       Mouse mode
;  chart_keymode         Keyboard mode
;
; FUTURE IMPROVEMENTS:
;  varoius markers
;  various colors
;  write to postscript
;
; By D.Nidever   March 2007
;-


function chart_checksavefile,input

; Is the file ane ASCII or IDL save file:

catch,error_state

if error_state ne 0 then begin
  catch,/cancel
  return,0
endif

; Try restoring the data
restore,input

return,1

end

;---------------------------------

pro chart_getinput,input,data,header=header,stp=stp

; This is part of the IDL chart program
; It gets the input data

; Checking the input

sz = size(input,/structure)

; It's a structure
if sz.type eq 8 and sz.n_elements gt 1 then begin
  print,'STRUCTURE INPUT.  WE CAN START'
  data = input
endif

; It's a string, probably filename
if sz.type eq 7 and sz.n_elements eq 1 then begin

  print,'FILENAME INPUT'

  sav = chart_checksavefile(input)

  ; This is an IDL save file
  if (sav eq 1) then begin

    ; Restore the data
    restore,input

    ; Check the structures that we have now
    help,names='*',output=output
    vars = strsplitter(output,' ',/extract)
    vars = reform(vars[0,*])
    nvars = n_elements(vars)

    ; Check out the variables
    for i=0,nvars-1 do begin
      cmd = 'sz = size('+vars[i]+',/str)'
      dum = execute(cmd)
      if sz.n_elements gt 0 and sz.type eq 8 then push,structures,vars[i]
    end

    ; Remove the SZ structure from the list
    bd = where(structures eq 'SZ',nbd)
    if nbd gt 0 and n_elements(structures) eq 1 then undefine,structures
    if nbd gt 0 and n_elements(structures) gt 1 then remove,bd,structures
    nstr = n_elements(structures)

    ; No structures
    if (nstr eq 0) then begin
      print,'No structure restored. Quitting'
      data = -1
      return
    endif

    ; Too many structures, ask the user to decide
    if nstr gt 1 then begin
      print,'More than 1 structure restored'
     for i=0,nstr-1 do print,strtrim(i+1,2),structures[i]

      pick=''
      pickone:
      read,'Pick one: ',pick

      ; Is it valid
      if valid_num(pick) eq 0 then goto,pickone
      if long(pick) lt 1 or long(pick) gt nstr then goto,pickone
      pick = long(pick)-1   ;zero-indexed
      structures = structures[pick]

    endif

    ; Define the data structure
    print,'Using ',structures[0],' as the DATA structure' 
    cmd2 = 'data = '+structures[0]
    dum = execute(cmd2)

  ; This is an ASCII file
  endif else begin

    ; Try reading with IMPORTASCII.PRO
    print,'READING WITH IMPORTASCII.PRO.  CONSIDERING CONVERTING FIRST'
    data = IMPORTASCII(input,header=header)

  endelse

endif

; Array
if stregex(sz.type,'[345]',/boolean) eq 1 and sz.n_elements gt 1 then begin

  print,'ARRAY INPUT.  CONVERTING TO STRUCTURE WITH ARR2STR.PRO'
  print,'CONSIDERING CONVERTING FIRST'

  data = ARR2STR(input)

endif

if keyword_set(stp) then stop

end

;----------------------------------------------------

pro chart_getdata,panel,xarr,yarr,gd=gd,cut=cut,stp=stp

; This is part of the IDL chart program
; This gets the xdata/ydata

common chart,pstruc,data,marked,cutoffs,mstruc

tags = tag_names(data)
ntags = n_elements(tags)

i = panel

; Get the xdata/ydata
xdata = pstruc[i].xdata
ydata = pstruc[i].ydata
xdata2 = strupcase(xdata)
ydata2 = strupcase(ydata)

; Loop through the names to replace
for j=0,ntags-1 do begin
  xdata2 = repstr(xdata2,tags[j],'data.'+tags[j])
  ydata2 = repstr(ydata2,tags[j],'data.'+tags[j])
end

for j=0,10 do begin
  xdata2 = repstr(xdata2,'data.data.','data.')
  ydata2 = repstr(ydata2,'data.data.','data.')
end

; Make the arrays
xcmd = 'xarr='+xdata2
dum = execute(xcmd)
ycmd = 'yarr='+ydata2
dum = execute(ycmd)

; Making a default gd array
gd = findgen(n_elements(xarr))

; Using the cutoffs
if keyword_set(cut) and n_elements(cutoffs) gt 0 then begin

  ; Combine the cuts
  allcut = ''
  ncuts = n_elements(cutoffs)
  for i=0,ncuts-1 do begin
    
    if strtrim(cutoffs[i],2) ne '' then begin
      if strtrim(allcut,2) ne '' then allcut = allcut + ' and '
      allcut = allcut + ' ( '+ cutoffs[i] + ' ) '
    endif

  end

  ; Uppercase
  allcut2 = strupcase(allcut)

  ; Replace the letter operator with symbol operators before
  ; the tag replacement
  origname = ['AND','OR','LE','GE','EQ','GT','LT']
  replname = ['&&','||','<=','>=','==','>','<']
  nrepl = n_elements(origname)
  for i=0,nrepl-1 do allcut2 = repstr(allcut2,origname[i],replname[i])

  ; Replacing the tags with 'data.'+tags
  ; Loop through the names to replace
  for j=0,ntags-1 do $
    allcut2 = repstr(allcut2,tags[j],'data.'+tags[j])

  for j=0,10 do $
    allcut2 = repstr(allcut2,'data.data.','data.')

  ; Put back the letter operators
  origname = ['&&','||','<=','>=','==','>','<','=']
  replname = [' AND ',' OR ',' LE ',' GE ',' EQ ',' GT ',' LT ',' EQ ']
  nrepl = n_elements(origname)
  for i=0,nrepl-1 do allcut2 = repstr(allcut2,origname[i],replname[i])
  allcut2 = repstr(allcut2,'  ',' ')

  ; Do we have a cut?
  if strtrim(allcut2,2) ne '' then begin

    ; Get the good indices
    cmd = 'gd = where( '+allcut2+',ngd)'
    dum = execute(cmd)

    ; Get the good arrays
    ; Some good ones
    if ngd gt 0 then begin
      xarr = xarr[gd]
      yarr = yarr[gd]

    ; No good ones
    endif else begin
      undefine,xarr,yarr
    endelse

  endif  ; we have a cut

endif

if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart_panelpositions,stp=stp

; This is part of the IDL chart program
; This defines the panel positions in the pstruc structure

common chart,pstruc,data,marked,cutoffs,mstruc

npanel = n_elements(pstruc)

; Setting the positions
case 1 of

  ; 1-3 panels, all in a row
  (npanel lt 4): begin

                   x0 = 0.07    ; initial x position
                   y0 = 0.07    ; initial y position
                   x1 = 0.98    ; final x position
                   y1 = 0.96    ; final y position
                   xsep = 0.07  ; x separation b/w panels
                   ysep = 0.07  ; y separation b/w panels
                   dx = (x1-x0-(npanel-1)*xsep)/npanel  ; xsize of panels
                   ;dy = (1.0-y0-npanel*ysep)/npanel  ; ysize of panels
                   dy = (y1-y0)  ; ysize of panels

                   ; Looping through the panels
                   for i=0,npanel-1 do begin
                     pstruc[i].position[0:1] = [ x0+i*(dx+xsep), y0 ]
                     pstruc[i].position[2] = pstruc[i].position[0] + dx
                     pstruc[i].position[3] = pstruc[i].position[1] + dy
                   end

                 end

  ; 4 panels, in a 2x2 grid
  (npanel eq 4): begin


                   stop

                   ; THIS IS JUST COPIED FROM ABOVE

                   x0 = 0.05    ; initial x position
                   y0 = 0.05    ; initial y position
                   xsep = 0.02  ; x separation b/w panels
                   ysep = 0.02  ; y separation b/w panels
                   dx = (1.0-x0-(npanel-1)*xsep)/npanel  ; xsize of panels
                   dy = (1.0-y0-(npanel-1)*ysep)/npanel  ; ysize of panels

                   ; Looping through the panels
                   for i=0,npanel-1 do begin
                     pstruc[i].position[0,1] = [ x0+i*(dx+xsep), y0+i*(dy+ysep) ]
                     pstruc[i].position[2] = pstruc[i].position[0] + dx
                     pstruc[i].position[3] = pstruc[i].position[1] + dy
                   end


                 end

  ; Anything else
  else:          begin

                    stop

                 end

endcase

if keyword_set(stp) then stop

end


;-------------------------------------------------------------------------

pro chart_definepanel,indata

; This is part of the IDL chart program.
; This defines a panel

common chart,pstruc,data,marked,cutoffs,mstruc

undefine,xdata,ydata,xr,yr

print,'Define graph panel: panel# xdata ydata [ limits x0 x1 y0 y1 ]'

; Read data from input
if n_elements(indata) eq 0 then begin
  readindata:
  indata = ''
  read,indata
endif

arr = strsplit(strtrim(indata,2),' ',/extract)

; Not enough inputs
if n_elements(arr) lt 2 then begin
  print,'Not enough information'
  return
endif

; First element should be the panel number
panel = arr[0]
if valid_num(panel) eq 0 then begin
  print,'Not a valid panel number'
  goto,readindata
endif
panel = long(panel)
if panel lt 1 then begin
  print,'Not a valid panel number'
  goto,readindata
endif
panel = panel - 1  ; zero-indexed


; Is this a new panel?
if (panel gt (n_elements(pstruc)-1)) then begin

  ; Add to pstruc
  dum = {position:fltarr(4),xtit:'',ytit:'',xdata:'',ydata:'',plotsym:3L,xrange:fltarr(2),$
         yrange:fltarr(2)}
  push,pstruc,dum
  ;pstruc = [ pstruc, dum ]

endif

; The other stuff
arr2 = arr[1:*]

limind = where(strlowcase(arr2) eq 'limits',nlim)

; Limits input as well
if (nlim gt 0) then begin

  ; The data
  if (limind ge 2) then begin
    xdata = strtrim(arr2[0],2)
    ydata = strtrim(arr2[1],2)

    if xdata ne '' then begin
      pstruc[panel].xdata = xdata
      pstruc[panel].xtit = xdata
    endif

    if ydata ne '' then begin
      pstruc[panel].ydata = ydata
      pstruc[panel].ytit = ydata
    endif
  endif

  ; The limits
  limdata = arr2[limind:*]

  if n_elements(limdata) ge 3 then begin
    xr = [ float(limdata[1]), float(limdata[2]) ]
    pstruc[panel].xrange = xr
    print,'PANEL ',strtrim(panel+1,2),'  Xrange = [',strtrim(xr[0],2),$
           ', ',strtrim(xr[1],2),']'
  endif

  if n_elements(limdata) ge 5 then begin
    yr = [ float(limdata[3]), float(limdata[4]) ]
    pstruc[panel].yrange = yr
    print,'PANEL ',strtrim(panel+1,2),'  Yrange = [',strtrim(yr[0],2),$
            ', ',strtrim(yr[1],2),']'
  endif

; No limits
endif else begin

  if n_elements(arr2) ge 2 then begin
    xdata = strtrim(arr2[0],2)
    ydata = strtrim(arr2[1],2)

    if xdata ne '' then begin
      pstruc[panel].xdata = xdata
      pstruc[panel].xtit = xdata
    endif

    if ydata ne '' then begin
      pstruc[panel].ydata = ydata
      pstruc[panel].ytit = ydata
    endif
  endif

  ; Using the default xrange
  ; Default Xrange
  CHART_GETDATA,panel,xarr,yarr
  pstruc[panel].xrange[0] = min(xarr)-0.02*range(xarr)
  pstruc[panel].xrange[1] = max(xarr)+0.02*range(xarr)

  ; Default Yrange
  CHART_GETDATA,panel,xarr,yarr
  pstruc[panel].yrange[0] = min(yarr)-0.02*range(yarr)
  pstruc[panel].yrange[1] = max(yarr)+0.02*range(yarr)

endelse


;if n_elements(xdata) gt 0 then if xdata ne '' then pstruc[panel].xdata = xdata
;if n_elements(ydata) gt 0 then if ydata ne '' then pstruc[panel].ydata = ydata
;if n_elements(xdata) gt 0 then if xdata ne '' then pstruc[panel].xtit = xdata
;if n_elements(ydata) gt 0 then if ydata ne '' then pstruc[panel].ytit = ydata
;
;if n_elements(xr) gt 1 then pstruc[panel].xrange = xr
;if n_elements(yr) gt 1 then pstruc[panel].yrange = yr

; Default Xrange
if pstruc[panel].xrange[0] eq 0.0 and pstruc[panel].xrange[1] eq 0.0 then begin
  CHART_GETDATA,panel,xarr,yarr
  pstruc[panel].xrange[0] = min(xarr)-0.02*range(xarr)
  pstruc[panel].xrange[1] = max(xarr)+0.02*range(xarr)
endif

; Default Yrange
if pstruc[panel].yrange[0] eq 0.0 and pstruc[panel].yrange[1] eq 0.0 then begin
  CHART_GETDATA,panel,xarr,yarr
  pstruc[panel].yrange[0] = min(yarr)-0.02*range(yarr)
  pstruc[panel].yrange[1] = max(yarr)+0.02*range(yarr)
endif

; Set the panel positions
CHART_PANELPOSITIONS

;stop

if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart_mkpstruc,stp=stp

; This is part of the IDL CHART program.  This makes
; the PSTRUC plotting structure.

common chart,pstruc,data,marked,cutoffs,mstruc

dum = {position:fltarr(4),xtit:'',ytit:'',xdata:'',ydata:'',plotsym:0L,xrange:fltarr(2),$
       yrange:fltarr(2)}

npanel = ''
indata = ''
xdata = ''
ydata = ''
xtit = ''
ytit = ''

; We need to ask for the information

; How many panels
readpanel:
read,'How many panels? ',npanel
if valid_num(npanel) eq 0 then goto,readpanel
npanel = long(npanel)
if npanel le 0 then goto,readpanel

; Initializing the PSTRUC structure
pstruc = replicate(dum,npanel)
pstruc.plotsym = 3

; Looping through the panels
for i=0,npanel-1 do begin

  ; Define the panel
  CHART_DEFINEPANEL

end

; Define the Marker structure
mstruc = {color:250L,plotsym:7L,symsize:1.3}

if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart_findclosest,panel,xarr,yarr,xdat,ydat,ind,stp=stp

; This is part of the IDL chart program
; This finds the closest data point to a cursor click

common chart,pstruc,data,marked,cutoffs,mstruc


; Info for this panel
position = pstruc[panel].position
xdatar = pstruc[panel].xrange
ydatar = pstruc[panel].yrange
;xnormr = [position[0],position[2]]
;ynormr = [position[1],position[3]]


; Find the closest point
; Difference b/w points and cursor in data coordinates
delx = xarr-xdat
dely = yarr-ydat

; Difference in normalized panel coordinates
dxnorm = delx/range(xdatar)
dynorm = dely/range(ydatar)
;dxnorm = (delx-xnormr[0])/range(xnormr)
;dynorm = (dely-ynormr[0])/range(ynormr)

; Normalized distance
dist = sqrt(dxnorm^2.0 + dynorm^2.0)

; Get the point with the smallest distance
ind = where(dist eq min(dist),nbest)
ind = ind[0]

if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart_norm2data,x,y,panel,xdat,ydat,stp=stp

; This is part of the IDL chart program
; This converts normalized coordinates to 
; data coordinates.

common chart,pstruc,data,marked,cutoffs,mstruc

; If X/Y not input then use the X/Y in the !mouse structure
if n_elements(x) eq 0 or n_elements(y) eq 0 then begin

  ; !mouse.x/y are in device coordinates
  x = float(!mouse.x) / !d.x_size
  y = float(!mouse.y) / !d.y_size

endif

panel = -1
xdat = 999999.
ydat = 999999.

; Loop through the panels and figure out which one was clicked in
npanel = n_elements(pstruc)
for i=0,npanel-1 do begin

  position = pstruc[i].position
  xnormr = [position[0],position[2]]
  ynormr = [position[1],position[3]]
  xdatar = pstruc[i].xrange
  ydatar = pstruc[i].yrange

  ; Inside this panel
  if (x ge xnormr[0]) and (x le xnormr[1]) and $
     (y ge ynormr[0]) and (y le ynormr[1]) then begin

    panel = i

    ; Calculating X in data coordinates
    xdat = (x-xnormr[0])/range(xnormr)         ; normalized within the panel
    xdat = xdat*(xdatar[1]-xdatar[0]) + xdatar[0]

    ; Calculating Y in data coordinates
    ydat = (y-ynormr[0])/range(ynormr)         ; normalized within the panel
    ydat = ydat*(ydatar[1]-ydatar[0]) + ydatar[0]

  endif

end

if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart_replot,stp=stp

; This is part of the idl CHART program.
; It plots the data
; The plot information is in the PSTRUC common structure.

common chart,pstruc,data,marked,cutoffs,mstruc

erase   ; erase the screen

npanel = n_elements(pstruc)

for i=0,npanel-1 do begin

  ; Get the data
  CHART_GETDATA,i,xarr,yarr,/cut,gd=cutind

  xrange = pstruc[i].xrange
  yrange = pstruc[i].yrange

  ; Make the plot
  PLOT,dist(10),/nodata,position=pstruc[i].position,xtit=pstruc[i].xtit,$
       ytit=pstruc[i].ytit,ps=pstruc[i].plotsym,/noerase,xrange=xrange,$
       yrange=yrange,xstyle=1,ystyle=1,charsize=1.1


  ; Do we have points to plot?
  if n_elements(xarr) gt 0 then begin

    ; Make a color array from the color cuts and use PLOTS to plot the
    ; actual data.
    color = xarr*0.+255
    PLOTS,xarr,yarr,color=color,ps=pstruc[i].plotsym,noclip=0

    ; Overplot the marked points
    if n_elements(marked) gt 0 then begin

      ; Finding the intersection b/w MARKED and CUTIND
      ; CMARKED are indices of MARKED that are included in CUTIND
      ;  and indexed in terms of the CUTIND array
      unmarked = nonuniq(cutind,marked,indbl=cmarked)
      ngdcutmarked = 0   ; bad until proven okay
      if n_elements(cmarked) gt 0 then $
        gdcutmarked = where(cmarked ne -1,ngdcutmarked)

      ; We have some cut marked points
      if ngdcutmarked gt 0 then begin

        OPLOT,[xarr[cmarked]],[yarr[cmarked]],ps=mstruc.plotsym,color=mstruc.color,$
              symsize=mstruc.symsize

      endif  ; some cut marked points

    endif  ; some marked points

  end else begin

    ; No data points
    if i eq 0 then print,'No data points to plot'

  endelse

  ;stop

end

; XYOUTS the total number of points (after cuts) and the number of
; marked points.  Also plot the cut definitions.
allcut = ''
ncuts = n_elements(cutoffs)
for i=0,ncuts-1 do begin
  if strtrim(cutoffs[i],2) ne '' then begin
    if strtrim(allcut,2) ne '' then allcut=allcut+'  '
    allcut = allcut + 'cutoff'+strtrim(i+1,2)+'='+cutoffs[i]
  endif
end
if strtrim(allcut,2) ne '' then $
  xyouts,0.1,0.98,allcut,/normal

; Overplot the numbers
nxarr = n_elements(xarr)
nmarked = n_elements(marked)
out = 'N='+strtrim(nxarr,2)+'  Nmarked='+strtrim(nmarked,2)
xyouts,0.5,0.015,out,/normal,align=0.5

if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart_print,ind,stp=stp

; This is part of IDL chart program
; This prints out the data for one point

common chart,pstruc,data,marked,cutoffs,mstruc

print,''
print,' Data for point ',strtrim(ind,2)
print,''

tags = tag_names(data)
ntags = n_elements(tags)
len = strlen(tags)
maxlen = max(len)

; Get the array
arr = strarr(ntags)
for i=0,ntags-1 do arr[i]=strtrim(data[i].(i),2)
maxarr = max(strlen(arr))

; Looping through the tags
format = '(A1,A'+strtrim(maxlen,2)+',A3,A'+strtrim(maxarr,2)+')'
for i=0,ntags-1 do begin
  print,format=format,' ',strupcase(tags[i]),'=',arr[i]
end


if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart_mousemode,stp=stp

; This is part of the IDL CHART program
; This handles the mouse mode.

common chart,pstruc,data,marked,cutoffs,mstruc

print,'CENTER-CLICK FOR KEYBOARD INPUT'

; Loop until right-click
flag = 0
WHILE (flag ne 1) do begin

  ; Get the cursor position
  CURSOR,x,y,/nowait,/normal

  ; Convert to data coordinates
  CHART_NORM2DATA,x,y,panel,xdat,ydat

  ; Print position
  ; Inside a panel
  if (panel ge 0) then begin
    xtit = pstruc[panel].xtit
    ytit = pstruc[panel].ytit
    print,format='($,a,a,f10.4,a,f10.4,a)','Panel '+strtrim(panel+1,2)+'  ',strupcase(xtit)+' = ',$
          xdat,'  '+strupcase(ytit)+' = ',ydat,string("15b)

  ; Not inside a panel
  endif else begin
    ; Outside the graph window
    if (x lt 0.0 or x gt 1.0 or y lt 0.0 or y gt 1.0) then begin
      print,format='($,a,a30,a)','Off the graph window',' ',string("15b)

    ; Inside the graph window. Between panels
    endif else begin
      print,format='($,a,a30,a)','Between panels',' ',string("15b)
    endelse
  endelse

  t0 = systime(1)

  ; Plot the cursor position
  ; use CHART_NORM2DATA to convert
  ; and give panel #
  CASE !mouse.button of

    ; No button, do nothing
    0:

    ; Regular button, MARK point
    1:  begin

          ; Inside a panel
          if (panel ge 0) then begin

            ; The MARKED indices are in terms of the whole, original array

            ; Get the cut
            CHART_GETDATA,panel,xarr,yarr,/cut,gd=cutind
            ; Get the original data
            CHART_GETDATA,panel,xarr,yarr

            ; Finding the intersection b/w MARKED and CUTIND
            ; UNMARKED are the indices of points that
            ;  make the cut and are NOT marked
            ;  and indexed in terms of the original array
            unmarked = cutind
            if n_elements(marked) gt 0 then begin
              unmarkedind = nonuniq(cutind,marked,bad=unmarked)
            end
            ngdunmarked = 0   ; bad until proven okay
            if n_elements(unmarked) gt 0 then $
              gdunmarked = where(unmarked ne -1,ngdunmarked)

            ; Need to deal with the situation of there being
            ; NO unmarked points

            ;; Making unmarked array
            ;unmarked = findgen(n_elements(xarr))
            ;if n_elements(marked) gt 0 then remove,marked,unmarked

            ; We've got some unmarked points
            if ngdunmarked gt 0 then begin

              ; Get the closest point, xarr and yarr are original arrays
              CHART_FINDCLOSEST,panel,xarr[unmarked],yarr[unmarked],xdat,ydat,ind1
              ind = unmarked[ind1]

              ; Is this one marked already
              nalready = 0
              if n_elements(marked) gt 0 then $
                already = where(marked eq ind,nalready)

              ; NEW Marked point
              if (nalready eq 0) then begin
  
                ; Update MARKED array, add this point
                push,marked,ind
  
                ; Print to the screen
                print,format='(a,a,f10.4,a,f10.4,a)','Panel '+strtrim(panel+1,2)+'  ',strupcase(xtit)+' = ',$
                      xarr[ind],'  '+strupcase(ytit)+' = ',yarr[ind],'  POINT '+strtrim(long64(ind),2)+' MARKED'

                ; Replot
                CHART_REPLOT
  
              endif
  
              ; Wait a little
              if (systime(1)-t0 lt 0.001) then wait,0.1
  
            end  ; we've got some unmarked points

          end  ; panel>=0

        end  ; regular button

    ; Center button, keyboard mode
    2: begin
         flag=1
         print,''
       end

    ; Right button, UNMARK point
    4:  begin

          ; Inside a panel
          if (panel ge 0) then begin

            ; Get the cut
            CHART_GETDATA,panel,xarr,yarr,/cut,gd=cutind
            ; Get the original data
            CHART_GETDATA,panel,xarr,yarr

            ; Do we have any Marked points?
            if n_elements(marked) gt 0 then begin

              ; Finding the intersection b/w MARKED and CUTIND
              ; CUTMARKEDIND are indices of MARKED that are included in CUTIND
              ;  and indexed in terms of the MARKED array
              ; CUTMARKED are indiced in terms of the original array
              unmarked = nonuniq(cutind,marked,ind2dbl=cutmarkedind)
              ngdcutmarked = 0   ; bad until proven okay
              if n_elements(cutmarkedind) gt 0 then $
                gdcutmarked = where(cutmarkedind ne -1,ngdcutmarked)

              ; We have some marked points that passed the cut
              if ngdcutmarked gt 0 then begin

                cutmarked = marked[cutmarkedind]

                ; Get the closest point
                CHART_FINDCLOSEST,panel,xarr[cutmarked],yarr[cutmarked],xdat,ydat,ind1
                ind = cutmarked[ind1]

                ; Update MARKED array, remove this point
                bd = where(marked eq ind,nbd)
                if nbd gt 0 then begin
                  if n_elements(marked) gt 1 then $
                    remove,bd,marked $
                  else undefine,marked
                endif

                ; Print to the screen
                print,format='(a,a,f10.4,a,f10.4,a)','Panel '+strtrim(panel+1,2)+'  ',strupcase(xtit)+' = ',$
                      xarr[ind],'  '+strupcase(ytit)+' = ',yarr[ind],'  POINT '+strtrim(long64(ind),2)+' UNMARKED'
 
                ; Replot
                CHART_REPLOT

              endif ; have some cut marked points

            endif

            ; Wait a little
            if (systime(1)-t0 lt 0.001) then wait,0.1

            ;print,'Right-click'
            ;stop

          end ; panel>=0

        end ; right-click

  ENDCASE


ENDWHILE

if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart_keymode,quit=quit,stp=stp

; This is part of the IDL CHART program
; This handles the keyboard mode.

common chart,pstruc,data,marked,cutoffs,mstruc


lineread:
print,'Type ? for help'
line=''
read,line

inp = strlowcase(strtrim(line,2))
inp1 = strmid(inp,0,1)             ; first non-blank character

CASE inp1 of

  'a':  begin

        end

  ; Make a cut
  'c':  begin

          arr = strsplit(strtrim(line,2),' ',/extract)
          ;if n_elements(arr) eq 1 then begin
          ;  print,'No cutoff input'
          ;  goto,lineread
          ;end
          
          ; Getting the cutoff number
          tag = strlowcase(strtrim(arr[0],2))
          len = strlen(tag)
          ind = stregex(tag,'cutoff',/boolean)
          if ind gt 0 then cutind = strmid(tag,6,len-6) else $
            cutind = strmid(tag,1,len-1)

          ; Is this a valid cutoff number?
          if valid_num(cutind) eq 0 then begin
            print,'Invalid cut number'
            goto,lineread
          end
          cutind = long(cutind)
          if cutind lt 1 then begin
            print,'Invalid cut number. Must be >=1'
            goto,lineread
          endif

          ; Do we need to expand the CUTS array?
          if (cutind gt n_elements(cutoffs)-1) then begin

            ; Backup the original
            if n_elements(cutoffs) gt 0 then $
              cutoffs_orig = cutoffs

            ; Create new longer cutoff array
            cutoffs = strarr(cutind)

            ; Copy back the original cutoffs
            if n_elements(cutoffs_orig) gt 0 then $
              cutoffs[0] = cutoffs_orig

          endif

          ; Get the new cutoff
          if n_elements(arr) gt 1 then begin
            newcut = strjoin(arr[1:*],' ')
          endif else begin
            newcut = ''
          endelse

          if strtrim(newcut,2) eq '""' then newcut=''

          ;; Replace characters 
          ;newcut2 = newcut
          ;origname = ['&&','||','<=','>=','==','=','>','<']
          ;replname = [' and ',' or ',' le ',' ge ',' eq ',' eq ',' gt ',' lt ']
          ;nrepl = n_elements(origname)
          ;for i=0,nrepl-1 do newcut2 = repstr(newcut2,origname[i],replname[i])
          ;newcut2 = repstr(newcut2,'  ',' ')

          ; Add the new cutoff
          cutoffs[cutind-1] = newcut

          ; Replot
          CHART_REPLOT

        end

  ; Change plot settings
  'g':  begin

          ; Define panel
          CHART_DEFINEPANEL

          ; Replot
          CHART_REPLOT

        end

  ; Print data for a point
  'p':  begin

          ; Use the last mouse position
          ; !mouse.x/y are in device coordinates
          xnorm = float(!mouse.x) / !d.x_size
          ynorm = float(!mouse.y) / !d.y_size

          CHART_NORM2DATA,xnorm,ynorm,panel,xdat,ydat

          ; Get the data
          CHART_GETDATA,panel,xarr,yarr,/cut,gd=cutind

          ; Get the closest point
          CHART_FINDCLOSEST,panel,xarr,yarr,xdat,ydat,ind1
          ind = cutind[ind1]

          ; Print the data for this point
          CHART_PRINT,ind

        end

  ; QUITTING
  'q':  begin

          print,'QUIT'
          quit=1

        end

  ; Replot
  'r':  begin

          ; Replot
          CHART_REPLOT

        end

  ; Stop
  's':  stop

  ; Print help
  '?':   begin
          
           print,''
           print,'HELP'
           print,'c = cutoffs'
           print,'g = define graph panel'
           print,'p = print data for a point'
           print,'q = quit'
           print,'r = replot'
           print,'s = stop'
           print,'? = help'
           print,'marker_ps  = Set marker plot symbol [1-10]'
           print,'marker_col = Set marker color'
           print,'marker_sym = Set marker symbol size'
           print,'unmark = unmarks all points'
           print,''

         end

  else:  begin
 
           case 1 of

             ; Marker plot symbol command
             strmid(inp,0,9) eq 'marker_ps' OR strmid(inp,0,14) eq 'marker_plotsym':  begin

               arr = strsplit(strlowcase(line),' ',/extract)
               mps = arr[1]
               case check_num(mps,[1,10]) of
                 ; Fine
                 1:  begin
                       mstruc.plotsym = long(mps)
                       CHART_REPLOT
                     end
                 0:  begin
                       print,'Plot symbol out of range [1-10].'
                     end
                 ; Not a number
                 -1: print,'Not a number'

                 ; Not possible, don't do anything
                 else:
               endcase

             end  ; marker plot symbol

             ; Marker color
             strmid(inp,0,10) eq 'marker_col' OR strmid(inp,0,12) eq 'marker_color':  begin

               arr = strsplit(strlowcase(line),' ',/extract)
               mcol = arr[1]
               case check_num(mcol) of
                 ; Fine
                 1:  begin  
                       mstruc.color = float(mcol)
                       CHART_REPLOT
                     end
                 ; Fine as well
                 0:  begin  
                       mstruc.color = float(mcol)
                       CHART_REPLOT
                     end
                 ; Not a number
                 -1: print,'Not a number'

                 ; Not possible, don't do anything
                 else:
               endcase

             end  ; marker color

             ; Marker plot symbol size
             strmid(inp,0,10) eq 'marker_sym' OR strmid(inp,0,11) eq 'marker_size':  begin

               arr = strsplit(strlowcase(line),' ',/extract)
               msize = arr[1]
               case check_num(msize,[0.0]) of
                 ; Fine
                 1:  begin  
                       mstruc.symsize = float(msize)
                       CHART_REPLOT
                     end
                 ; Out of range
                 0:  begin  
                       print,'Marker symbol size out of range (>0)'
                     end
                 ; Not a number
                 -1: print,'Not a number'

                 ; Not possible, don't do anything
                 else:
               endcase

             end  ; marker symbol size

             ; Unmark all
             strmid(inp,0,6) eq 'unmark': begin
               undefine,marked
               CHART_REPLOT
             end

             else:  begin
                      print,line,' is currently not a supported option'
                    end

           endcase
 
  
         end

ENDCASE


if keyword_set(stp) then stop

end

;-------------------------------------------------------------------------

pro chart,input,keys=keys,header=header,stp=stp

; This is the main program of the IDL version of chart

common chart,pstruc,data,marked,cutoffs,mstruc

; Initialize the parameters
undefine,pstruc,data,marked,cutoffs,mstruc

; Not enough inputs
if n_elements(input) eq 0 then begin
  print,'Syntax - chart,input,/stp'
  return
endif

; Set the screen correctly
if !d.name ne 'PS' then device,decomp=0
print,'Setting DECOMPOSED=0'
loadct,39

; Get the input
CHART_GETINPUT,input,data,header=header
if size(data,/type) ne 8 then return



; Make a structure for the panels/plots.  One dimension each.
; position, x/y data definition, names, limits, etc.
; marked points, cuts, colors?

; Input graph keys definitions?
if n_elements(keys) gt 0 then begin

  ; This is probably an input file
  if size(keys,/type) eq 7 and n_elements(keys) eq 1 then begin
    readline,keys,lines,comment='#'
  endif

  ; Probably an array input
  if size(keys,/type) eq 7 and n_elements(keys) gt 1 then begin
    lines = keys
  endif

  for i=0,n_elements(lines)-1 do CHART_DEFINEPANEL,lines[i]


  ; Define the Marker structure
  mstruc = {color:250L,plotsym:7L,symsize:1.3}

; Define graph keys manually
endif else begin

  ; Make the initial PSTRUC plotting structure
  CHART_MKPSTRUC

endelse

; Make the initial plot
CHART_REPLOT

; LOOP until we quit
quit=0
WHILE (quit eq 0) do begin
  ; Mouse mode
  CHART_MOUSEMODE

  ; Keyword mode
  CHART_KEYMODE,quit=quit

ENDWHILE

if keyword_set(stp) then stop

end
