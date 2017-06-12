; $Id: opan_ploteisf.pro,v 1.1 2002/09/19 21:30:20 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotEISFCleanup,tlb
widget_control,tlb,get_uvalue = pState
if ((*pState).notifyIds)[0] ne (-1L) then begin
  s = size((*pState).notifyIDs)
  if s[0] eq 1 then count = 0 else count = s[2]-1
  for j = 0,count do begin
    plotEISFInfo = {plotEISFEvent,$
                        		ID:(*pState).notifyIDs[0,j],$
                        		Top:(*pState).notifyIDs[1,j],$
                        		Handler:0l}
    if widget_info((*pState).notifyIDs[0,j],/valid_id) then begin $
      widget_control,(*pState).notifyIDs[0,j],send_event = plotEISFInfo
    endif
  endfor
endif

; Now clean up all of the pointers and delete the pixmaps used in this module
ptr_free,(*pState).yPtr,(*pState).pPtr, (*pState).perrPtr, (*pState).ycleanPtr
wdelete,(*pState).winPix
ptr_free,pState
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotEISFSave,event
; Save the parameters in 3-column ascii format
widget_control,event.top,get_uvalue = pState
if n_elements(*(*pState).pPtr) eq 0 then return

workDir = (*pState).workDir
filename = dialog_pickfile(dialog_parent = event.top,/write,path = workDir, $
           title = 'Ascii file to store data',filter = '*.txt')
if filename eq '' or filename eq ' ' then return
filename = filename + '.txt'
x = *(*pState).yPtr
y = *(*pState).pPtr
yerr = *(*pState).perrPtr
nx = n_elements(x)

openw,lun,filename,/get_lun
strout = '# '+(*pState).xtitle+' '+(*pState).ytitle+' '+'Error'
printf,lun,strout
for i = 0,nx-1 do printf,lun,x[i],y[i],yerr[i]
free_lun,lun

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotEISFQuit,event
widget_control,event.top,/destroy
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_drawEISF,event
widget_control,event.top,get_uvalue = pState
x = *(*pState).yPtr
y = *(*pState).pPtr
yerr = *(*pState).perrPtr

if (*pState).autoscale eq 1 then begin
  dx = 0.2*(max(x)-min(x))
  (*pState).xrange = [min(x)-dx,max(x)+dx]
  dy = 0.5*(max(y)-min(y))
  (*pState).yrange = [min(y)-dy,max(y)+dy]
endif

plot,x,y,psym = 4,xrange = (*pState).xrange,yrange = (*pState).yrange, $
     xstyle = 1,ystyle = 1,xtitle = (*pState).xtitle, $
     ytitle = (*pState).ytitle,title = (*pState).title
errplot,x,y-yerr,y+yerr,width = 0.0

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotEISFAccept,event
widget_control,event.top,get_uvalue = pState
widget_control,(*pState).el,get_value = elText
widget_control,(*pState).qe,get_value = qeText
elText = elText[0]
qeText = qeText[0]
el = opan_selectgroups(elText)
qe = opan_selectgroups(qeText)

;val = fix(val[0])
fitObject = (*pState).fitObject
ngroups = n_elements(fitObject)
totalFits = 0
for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  ncurves = oc->count()
  chisq = oc->getchisq()
  if (ncurves gt 0) and (chisq ne (-1.0)) then begin
    totalFits = totalFits + 1
    if n_elements(okIndex) eq 0 then okIndex = i else okIndex = [okIndex,i]
  endif
endfor

ao = dblarr(totalFits)
aoerr = dblarr(totalFits)

for i = 0,totalFits-1 do begin

  oc = fitObject[okIndex[i]]
  p = oc->getparms()
  pe = oc->getParmError()
  nelastic = n_elements(el)
  nqe = n_elements(qe)
  x = 0 & sx = 0
  y = 0 & sy = 0

  for j = 0,nelastic-1 do begin
    ix = el[j]
    x = x + p[ix]
    sx = sqrt(sx^2+pe[ix]^2)
  endfor

  for j = 0,nqe-1 do begin
    iy = qe[j]
    y = y + p[iy]
    sy = sqrt(sy^2+pe[iy]^2)
  endfor
  y = y+x
  sy = sqrt(sx^2+sy^2)
  ao[i] = x/y
  aoerr[i] = ao[i]*sqrt((sx/x)^2+(sy/y)^2)

endfor

(*pState).ytitle = 'A!D0!N(Q)'

y = *(*pState).ycleanPtr
y = y[okIndex]
*(*pState).yPtr = y

*(*pState).pPtr = ao
*(*pState).perrPtr = aoerr

(*pState).title = 'EISF'

wset,(*pState).winPix
opan_drawEISF,event
wset,(*pState).winVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opanEISFDraw,event
widget_control,event.top,get_uvalue = pState
case event.type of
0:	begin		; button press
	  (*pState).mouse = event.press
	  if (*pState).mouse eq 4 then begin
	    (*pState).autoscale = 1

		wset,(*pState).winPix
		opan_drawEISF,event
		wset,(*pState).winVis
		device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]
	    return
	  endif
	  if (*pState).mouse eq 1 then begin
	    (*pState).xbox[0] = event.x
	    (*pState).ybox[0] = event.y
	    wset,(*pState).winVis
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]
	    empty
	    (*pState).autoscale = 0
	    widget_control,(*pState).win,/draw_motion_events
	  endif
	end
1:	begin	; button release
	 if (*pState).mouse eq 1 then begin
	  xll = (*pState).xbox[0] < (*pState).xbox[1]
	  yll = (*pState).ybox[0] < (*pState).ybox[1]
	  w = abs((*pState).xbox[1] - (*pState).xbox[0])
	  h = abs((*pState).ybox[1] - (*pState).ybox[0])
	  xur = xll + w
	  yur = yll + h
	  ll = convert_coord(xll,yll,/device,/to_data)
	  ur = convert_coord(xur,yur,/device,/to_data)
	  (*pState).xrange = [ll[0],ur[0]]
	  (*pState).yrange = [ll[1],ur[1]]
	  wset,(*pState).winPix
	  opan_drawEISF,event
	  wset,(*pState).winVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]
	  (*pState).mouse = 0B
	  widget_control,(*pState).win,draw_motion_events = 0
	 endif
	end
2:	begin	; mouse motion
	  if (*pState).mouse eq 1 then begin
	  	(*pState).xbox[1] = event.x
	  	(*pState).ybox[1] = event.y
	  	xc = [(*pState).xbox[0],event.x,event.x,$
	  	      (*pState).xbox[0],$
	  	      (*pState).xbox[0]]
	  	yc = [(*pState).ybox[0],(*pState).ybox[0],$
	  	      event.y,event.y,$
	  	      (*pState).ybox[0]]
	    wset,(*pState).winPix
	    opan_drawEISF,event
	    wset,(*pState).winVis
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]
	  	plots,xc,yc,/device

	  	empty
	  endif
	end
else:
endcase
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotEISFPrint,event
widget_control,event.top,get_uvalue = pState
keywords = PSConfig(Cancel=cancelled,group_leader = event.top,$
           filename = 'eisf.ps',color = 0)
IF cancelled THEN RETURN
thisDevice = !D.Name
Set_Plot, 'PS'
Device, _Extra=keywords
opan_drawEISF,event
Device, /Close_File
Set_Plot, thisDevice
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotEISFJPEG,event
widget_control,event.top,get_uvalue = pState
thisDevice = !D.Name
xsize = !d.x_size & ysize = !d.y_size
window,/free,/pixmap,xsize = xsize,ysize = ysize
winPix = !d.window
opan_drawEISF,event
device,get_visual_depth = thisDepth
if thisDepth eq 8 then begin
  tvlct,r,g,b,/get
  image2d = tvrd()
  s = size(image2d,/dimensions)
  image24 = bytarr(3,s[0],s[1])
  tvlct,r,g,b,/get
  image24[0,*,*] = r[image2d]
  image24[1,*,*] = g[image2d]
  image24[2,*,*] = b[image2d]
endif
if thisDepth gt 8 then begin
  device,decomposed = 1
  image24=tvrd(true = 1)
endif
wdelete,winPix
filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                           title = 'Enter output jpeg file name',$
                           /write,filter = '*.jpg')

if (filename eq '') or (filename eq ' ') then return
filename = filename + '.jpg'
s = Size(image24, /Dimensions)
newx = Round(300.0 * s[1] / 72)
newy = Round(300.0 * s[2] / 72)
highResImage = Congrid(image24, 3, newx, newy, /Interp)
write_jpeg,filename,255-highResImage,true = 1,quality = 100
Set_Plot, thisDevice
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro logEISF,event
widget_control,event.top,get_uvalue = pState
if n_elements(*(*pState).stringPtr) eq 0 then return
thisDevice = !D.Name
xsize = !d.x_size & ysize = !d.y_size
window,/free,/pixmap,xsize = xsize,ysize = ysize
winPix = !d.window
opan_drawEISF,event
device,get_visual_depth = thisDepth
if thisDepth eq 8 then begin
  tvlct,r,g,b,/get
  image2d = tvrd()
  s = size(image2d,/dimensions)
  image24 = bytarr(3,s[0],s[1])
  tvlct,r,g,b,/get
  image24[0,*,*] = r[image2d]
  image24[1,*,*] = g[image2d]
  image24[2,*,*] = b[image2d]
endif
if thisDepth gt 8 then begin
  device,decomposed = 1
  image24=tvrd(true = 1)
endif
wdelete,winPix
;filename = DIALOG_PICKFILE(dialog_parent = event.top,$
;                           title = 'Enter output jpeg file name',$
;                           /write,filter = '*.jpg')

;if (filename eq '') or (filename eq ' ') then return
;filename = filename + '.jpg'
;s = Size(image24, /Dimensions)
;newx = Round(300.0 * s[1] / 72)
;newy = Round(300.0 * s[2] / 72)
;highResImage = Congrid(image24, 3, newx, newy, /Interp)
;write_jpeg,filename,255-highResImage,true = 1,quality = 100
;Set_Plot, thisDevice






; Create the filename
counter = 0
res = 1
while res eq 1 do begin
  filename = 'panplot_'+strtrim(string(counter),2)+'.jpg'
;  testFile = filepath(filename, ROOT_DIR=(*pState).workDir,subdir=[(*pState).logDirectory])
  testFile = filepath(filename, ROOT_DIR=(*pState).logDirectory)
  res = file_test(testFile)
  counter = counter + 1
endwhile

s = Size(image24, /Dimensions)
newx = Round(150.0 * s[1] / 72)
newy = Round(150.0 * s[2] / 72)
highResImage = Congrid(image24, 3, newx, newy, /Interp)

imgFile = filename

write_jpeg,testFile,255-highResImage,true = 1,quality = 100
;write_jpeg,testFile,highResImage,true = 1,quality = 100
Set_Plot, thisDevice

; Ok, the file has been written.  Now we must add the plot to the HTML file.
strout = *(*pState).stringPtr
end_of_html = where(strout eq '</body>')
index = end_of_html[0] - 1
nstr = n_elements(strout)

read_jpeg,testFile,img
imgSize = size(img)
sx = imgSize[2] & sy = imgSize[3]
aspect2 = 1.0*sy/(1.0*sx)
outSx = 400 & outSy = fix(1.0*outSx*aspect2)
imgLine = '<img src="'+imgFile+'" width="'+ $
          strtrim(string(outSx),2)+ $
          '" height="'+strtrim(string(outSy),2)+'"><br>'

strout1 = [strout[0:index],'<p>',imgLine,strout[index:nstr-1],'<p>']
*(*pState).stringPtr = strout1
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_plotEISF_event,event
widget_control,event.top,get_uvalue = pState

; Handle the resize events
if tag_names(event,/structure_name) eq 'WIDGET_BASE' then begin
  base2geom = widget_info((*pState).base2,/geometry)
  base3geom = widget_info((*pState).base3,/geometry)
  xsize = event.x
  ysize = event.y

  ; New data window dimensions
  newxsize = xsize
  newysize = ysize - base2geom.ysize - base3geom.ysize

  widget_control,(*pState).win,draw_xsize = newxsize, $
                 draw_ysize = newysize
  wdelete,(*pState).winPix
  window,/free,/pixmap,xsize = newxsize,ysize = newysize
  (*pState).winPix = !d.window

  if n_elements(*(*pState).pPtr) eq 0 then return

  wset,(*pState).winPix
  opan_drawEISF,event
  wset,(*pState).winVis
  device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]

  return
endif

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_plotEisf,	fitObject, y, $
					group_leader = group_leader, $
                   	notifyIds = notifyIds, $
                   	xtitle = xtitle, $
                   	ytitle = ytitle, $
                   	title = title, $
                   	workDir = workDir, $
                   	logDirectory = logDirectory, $
                   	stringPtr = stringPtr
; Widget definition module
xsize = 5
if n_elements(workDir) eq 0 then workDir = ''
if n_elements(notifyIds) eq 0 then notifyIds = (-1L)
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = 'EISF plotting utility', $
        /base_align_center,/tlb_size_events)
endif else begin
  tlb = widget_base(group_leader = group_leader, /row, $
        title = 'EISF plotting utility', $
        /base_align_center,/tlb_size_events)
endelse

base1 = widget_base(tlb,/col)
base2 = widget_base(base1,/row)
fsize = 4

el = cw_field(base2,value = '5',/string,/return_events, $
               title = 'Elastic parameter(s)',xsize = fsize)
qe = cw_field(base2,value = '2,8',/string,/return_events, $
               title = 'QE parameter(s)',xsize = fsize)

void = widget_button(base2,value = 'Accept',event_pro = 'plotEISFAccept')

wxsize = 400 & wysize = 400
win = widget_draw(base1,xsize = wxsize,ysize = wysize,/button_events, $
      event_pro = 'opanEISFDraw')

base3 = widget_base(base1,/row)
void = widget_button(base3,value = 'Save',event_pro = 'plotEISFSave')
void = widget_button(base3,value = 'Print',event_pro = 'plotEISFPrint')
void = widget_button(base3,value = 'JPEG',event_pro = 'plotEISFJPEG')
if n_elements(*stringPtr) gt 0 then sensitive = 1 else sensitive = 0

void = widget_button(base3,value = 'Add plot to log file', $
       sensitive = sensitive,event_pro = 'logEISF')
void = widget_button(base3,value = 'Dismiss',event_pro = 'plotEISFQuit')

widget_control,tlb,/realize
widget_control,win,get_value = winVis
window,/free,/pixmap,xsize = wxsize,ysize = wysize
winPix = !d.window

state = {el:el, $
         qe:qe, $;parm2plot:parm2plot,$
;         info:info, $
         fitObject:fitObject, $
         yPtr:ptr_new(reform(y[0,*]),/no_copy), $
         ycleanPtr:ptr_new(reform(y[0,*]),/no_copy), $
         pPtr:ptr_new(/allocate_heap), $
         perrPtr:ptr_new(/allocate_heap), $
         workDir:workDir, $
         logDirectory:logDirectory, $
         stringPtr:stringPtr, $
         base2:base2, $
         base3:base3, $
         win:win, $
         winVis:winVis, $
         winPix:winPix, $
         mouse:0B, $
         xbox:[0.0,1.0], $
         ybox:[0.0,1.0], $
         autoscale:1, $
         xrange:[0.0,1.0], $
         yrange:[0.0,1.0], $
         xtitle:xtitle, $
         ytitle:ytitle, $
         title:title, $
         notifyIds:notifyIds}

pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState,/no_copy
xmanager,'opan_plotEISF',tlb,cleanup = 'plotEISFCleanup',/no_block
return
end