; $Id: opan_plotparms.pro,v 1.1 2002/09/19 21:30:20 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotParmsCleanup,tlb
widget_control,tlb,get_uvalue = pState
if ((*pState).notifyIds)[0] ne (-1L) then begin
  s = size((*pState).notifyIDs)
  if s[0] eq 1 then count = 0 else count = s[2]-1
  for j = 0,count do begin
    plotParmsInfo = {plotParmsEvent,$
                        		ID:(*pState).notifyIDs[0,j],$
                        		Top:(*pState).notifyIDs[1,j],$
                        		Handler:0l}
    if widget_info((*pState).notifyIDs[0,j],/valid_id) then begin $
      widget_control,(*pState).notifyIDs[0,j],send_event = plotParmsInfo
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
pro plotParmsSave,event
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
pro plotParmsQuit,event
widget_control,event.top,/destroy
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_drawParms,event
widget_control,event.top,get_uvalue = pState
x = *(*pState).yPtr
y = *(*pState).pPtr
yerr = *(*pState).perrPtr

if (*pState).autoscale eq 1 then begin
  dx = 0.2*(max(x)-min(x))
  (*pState).xrange = [min(x)-dx,max(x)+dx]
  dy = 0.5*(max(y)+max(yerr)-min(y)+max(yerr))
  (*pState).yrange = [min(y)-dy-max(yerr),max(y)+dy+max(yerr)]
endif

plot,x,y,psym = 4,xrange = (*pState).xrange,yrange = (*pState).yrange, $
     xstyle = 1,ystyle = 1,xtitle = (*pState).xtitle, $
     ytitle = (*pState).ytitle,title = (*pState).title
errplot,x,y-yerr,y+yerr,width = 0.0

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotParmsAccept,event
widget_control,event.top,get_uvalue = pState
widget_control,(*pState).parm2Plot,get_value = val
val = fix(val[0])
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
p = dblarr(totalFits)
perr = dblarr(totalFits)

for i = 0,totalFits-1 do begin
  oc = fitObject[okIndex[i]]
  p[i] = (oc->getparms())[val]
  perr[i] = (oc->getparmError())[val]
  parmName = (oc->getparmnames())[val]
  curveName = oc->getcurvename(val)
endfor


pos = strpos(curveName,'_')
dispName = strmid(strupcase(curveName),pos+1)

(*pState).ytitle = dispName+': '+parmName

y = *(*pState).ycleanPtr
y = y[okIndex]

*(*pState).yPtr = y
*(*pState).pPtr = p
*(*pState).perrPtr = perr

(*pState).title = '#'+strtrim(string(val),2)

wset,(*pState).winPix
opan_drawParms,event
wset,(*pState).winVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opanParmsDraw,event
widget_control,event.top,get_uvalue = pState
case event.type of
0:	begin		; button press
	  (*pState).mouse = event.press
	  if (*pState).mouse eq 4 then begin
	    (*pState).autoscale = 1

		wset,(*pState).winPix
		opan_drawParms,event
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
	  opan_drawParms,event
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
	    opan_drawParms,event
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
pro plotParmsPrint,event
widget_control,event.top,get_uvalue = pState
keywords = PSConfig(Cancel=cancelled,group_leader = event.top,$
           filename = 'groupdep.ps',color = 0)
IF cancelled THEN RETURN
thisDevice = !D.Name
Set_Plot, 'PS'
Device, _Extra=keywords
opan_drawParms,event
Device, /Close_File
Set_Plot, thisDevice
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro plotParmsJPEG,event
widget_control,event.top,get_uvalue = pState
thisDevice = !D.Name
xsize = !d.x_size & ysize = !d.y_size
window,/free,/pixmap,xsize = xsize,ysize = ysize
winPix = !d.window
opan_drawParms,event
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
pro logParms,event
widget_control,event.top,get_uvalue = pState
if n_elements(*(*pState).stringPtr) eq 0 then return
thisDevice = !D.Name
xsize = !d.x_size & ysize = !d.y_size
window,/free,/pixmap,xsize = xsize,ysize = ysize
winPix = !d.window
opan_drawParms,event
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

; Create the filename
counter = 0
res = 1
while res eq 1 do begin
  filename = 'panplot_'+strtrim(string(counter),2)+'.jpg'
  ;testFile = filepath(filename, ROOT_DIR=(*pState).workDir,subdir=[(*pState).logDirectory])
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
pro opan_plotParms_event,event
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
  opan_drawParms,event
  wset,(*pState).winVis
  device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pState).winPix]

  return
endif

if event.id eq (*pState).parm2plot then plotParmsAccept,event

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan_plotParms,	fitObject, y, $
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
  tlb = widget_base(/col,title = 'Parameter plotting utility', $
        /base_align_center,/tlb_size_events)
endif else begin
  tlb = widget_base(group_leader = group_leader, /row, $
        title = 'Parameter plotting utility', $
        /base_align_center,/tlb_size_events)
endelse

base1 = widget_base(tlb,/col)
base2 = widget_base(base1,/row)
fsize = 8
parm2plot = cw_field(base2,value = 1,/string, /return_events, $
               title = 'Parameter #',xsize = fsize)
void = widget_button(base2,value = 'Accept',event_pro = 'plotParmsAccept')

wxsize = 400 & wysize = 400
win = widget_draw(base1,xsize = wxsize,ysize = wysize,/button_events, $
      event_pro = 'opanParmsDraw')

base3 = widget_base(base1,/row)
void = widget_button(base3,value = 'Save',event_pro = 'plotParmsSave')
void = widget_button(base3,value = 'Print',event_pro = 'plotParmsPrint')
void = widget_button(base3,value = 'JPEG',event_pro = 'plotParmsJPEG')
if n_elements(*stringPtr) gt 0 then sensitive = 1 else sensitive = 0
void = widget_button(base3,value = 'Add plot to log file', $
       event_pro = 'logParms',sensitive = sensitive)
void = widget_button(base3,value = 'Dismiss',event_pro = 'plotParmsQuit')

widget_control,tlb,/realize
widget_control,win,get_value = winVis
window,/free,/pixmap,xsize = wxsize,ysize = wysize
winPix = !d.window

state = {parm2plot:parm2plot,$
;         info:info, $
         fitObject:fitObject, $
         ycleanPtr:ptr_new(reform(y[0,*]),/no_copy), $
         yPtr:ptr_new(reform(y[0,*]),/no_copy), $
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
xmanager,'opan_plotparms',tlb,cleanup = 'plotParmsCleanup',/no_block
return
end