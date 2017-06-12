;+
; NAME:
;       OPAN__DEFINE
;
; PURPOSE:
;
;       Object implementation of the program formerly known as PAN (Peak ANalysis).
;		Standalone version, not included in DAVE.
;
; AUTHOR:
;
;       Robert M. Dimeo, Ph.D.
;		NIST Center for Neutron Research
;       100 Bureau Drive
;		Gaithersburg, MD 20899
;       Phone: (301) 975-8135
;       E-mail: robert.dimeo@nist.gov
;       http://www.ncnr.nist.gov/staff/dimeo
;       
;       Modified (fiddled with) by MSW 2004-2008
;
; CATEGORY:
;
;       Objects, widgets, curve fitting
;
; CALLING SEQUENCE:
;
;       object = obj_new('OPAN')
;
;
; LIMITATIONS:
;
;		Due to a call to the MPFIT common block, simultaneous fitting cannot
;		be done if there is more than one instance of PAN running.  Otherwise
;		multiple instances of PAN should not conflict.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opanCleanup,tlb
widget_control,tlb,get_uvalue = self
if obj_valid(self) then begin
  s = size(self->getNotifyIds())
  if s[0] ne 0 then begin
    if s[0] eq 1 then count = 0 else count = s[2]-1
    for j = 0,count do begin
      OPANInfo = {OPANEvent,$
                            ID:(self->getNotifyIds())[0,j],$
                            Top:(self->getNotifyIds())[1,j],$
                            Handler:0l, $
                            object:self}
      if widget_info((self->getNotifyIds())[0,j],/valid_id) then begin $
        widget_control,(self->getNotifyIds())[0,j],send_event = OPANInfo
      endif
    endfor
  endif else begin
    obj_destroy,self
  endelse
endif
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::quit,event = event
if n_elements(*self.logStringPtr) ne 0 then begin
  self->closeHTMLFile,event = event
endif
widget_control,self.tlb,/destroy
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::aboutPAN,event = event
strout = ['PAN: Peak Analysis','Written by R.M.Dimeo', $
          'October 11, 2002','','NIST Center for Neutron Research', $
          '','Updated (RMD) May 11, 2005']
void = dialog_message(strout,/information,dialog_parent = event.top)
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::cleanup
tvlct,*self.rPtr,*self.gPtr,*self.bPtr
ptr_free,self.rPtr,self.gPtr,self.bPtr
ptr_free,self.pcor_ptr
ptr_free,self.curvenamesPtr, self.headerPtr
ptr_free,self.dataPtr,self.errorPtr
ptr_free,self.xvalsPtr,self.yvalsPtr
ptr_free,self.datxPtr,self.datyPtr
ptr_free,self.ocurrentPtr,self.prefs
ptr_free,self.grpArrayPtr,self.goodParmPtr
ptr_free,self.notifyIdPtr,self.logStringPtr
ptr_free,self.oerrorPtr,self.odataPtr,self.oxvalsPtr,self.oyvalsPtr
wdelete,self.resPix,self.datPix
; Since we have a pointer to an array of object containers, destroy
; the object container array first, then free the pointer.
if total(obj_valid(*self.ocurveGroup) ge 1) then obj_destroy,*self.ocurveGroup
ptr_free,self.ocurveGroup
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::flashMessage_create,msg,base
; This method simply flashes a widget with a message that remains present
; until the flashMessage_destroy method is invoked.
;
; Center it.
geom = widget_info(self.tlb, /geometry)
xpos = geom.xoffset + geom.xsize/2 - 100
ypos = geom.yoffset + geom.ysize/2 - 50

base = widget_base(title='Please wait:',/row,xoffset=xpos,yoffset=ypos, $
      tlb_frame_attr = 3)
void = widget_text(base,value = msg,xsize = strlen(msg),/editable)
widget_control,base,/realize
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::flashMessage_destroy,base
widget_control,base,/destroy
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro fitfunction,xlim,parms,yfit,_Extra = extra
; This is the function that can be used in curve fitting (using Markwardt's
; routines for instance).
x = *extra.xPtr
oc = extra.oc
oc->setparms,parms
oc->evaluate,x,yout = yout,_Extra = extra

wherevalid = where(x ge min(xlim) and x le max(xlim),newpts)
yfit = yout[wherevalid]
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::initCleanup, event = event
self->clearAllCurves
if n_elements(*self.dataPtr) gt 0 then begin
  if n_elements(*self.ocurveGroup) gt 0 then begin
     obj_destroy,(*self.ocurveGroup)
  endif
endif
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::initNewDataObjects, event = event
if n_elements(*self.dataPtr) eq 0 then return
datSize = size(*self.dataPtr)
if datSize[0] eq 2 then begin
  nq = (size(*self.dataPtr))[2]
  widget_control,self.groupSlider,sensitive = 1
endif else begin
  nq = 1
  widget_control,self.groupSlider,sensitive = 0
endelse
*self.ocurveGroup = objarr(nq)
for i = 0,nq-1 do begin
 (*self.ocurveGroup)[i] = obj_new("FUNC_CONT")
endfor
grpString = strtrim(string(1),2)+'-'+strtrim(string(nq),2)
widget_control,self.groupField,set_value = grpString
widget_control,self.groupSlider,set_slider_min = 1,set_slider_max = nq
x = (*self.xvalsPtr)
(*self.prefs).xfitlo = min(x)
(*self.prefs).xfithi = max(x)
*self.odataPtr = *self.dataPtr
*self.oxvalsPtr = *self.xvalsPtr
*self.oyvalsPtr = *self.yvalsPtr
*self.oerrorPtr = *self.errorPtr
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::clearAllCurves, event = event
; We can't just remove the objects from the container.  We actually have to
; free up each of the objects here...
ngroups = n_elements(*self.ocurveGroup)
if total(obj_valid(*self.ocurveGroup)) eq 0 then return
for j = 0,ngroups-1 do begin
  ncurves = (*self.ocurveGroup)[j]->count()
  if ncurves ne 0 then begin
    oall = (*self.ocurveGroup)[j]->get(/all)
    obj_destroy,oall
    (*self.ocurveGroup)[j]->remove,/all
  endif
endfor

wset,self.resPix
erase
wset,self.resVis
erase
wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

widget_control,self.curveSlider,set_slider_max = 2

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::clearCurrentCurves, event = event
; Clears the curves from those in the current slider position
widget_control,self.groupSlider,get_value = val
val = fix(val[0]) - 1
if n_elements(*self.dataPtr) eq 0 then return
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then val = 0
oc = (*self.ocurveGroup)[val]
ncurves = oc->count()
if ncurves eq 0 then return

; We can't just remove the objects from the container.  We actually have to
; free up each of the objects here...
oall = oc->get(/all)
obj_destroy,oall
oc->remove,/all

wset,self.resVis
erase
wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
widget_control,self.curveSlider,set_slider_max = 2

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::selectCurve, event = event

; Here I have used an unsatisfactory solution and implemented a modal
; widget dialog for parameter entry when the preferences are set
; such that the user can input only via a dialog or the type of
; curve demands it (i.e. it cannot be drawn with two clicks of the
; mouse or it is a user function).
ncurvetypes = n_elements(*self.curvenamesPtr)

; We begin by considering a user-defined function
if event.value eq (ncurvetypes-1) then begin
  if n_elements(*self.dataPtr) eq 0 then return

  out = enterfunction(group_leader = event.top,directory = self.workDir)
  if out.cancel eq 1 then return

  parmStr = getparmsfromexpr(out.expr)
  help,parmStr,output = output
  test = strpos(output,'STRING')
  if test[0] eq (-1L) then return

  nparms = n_elements(parmStr)
  parms = replicate(1d,nparms)
  name = 'pan_userfunction'

  ; Which group are we looking at?
  widget_control,self.groupSlider,get_value = val
  val = fix(val[0])-1
  datSize = size(*self.dataPtr)
  if datSize[0] eq 2 then begin
    dat = (*self.dataPtr)[*,val]
    daterr = (*self.errorPtr)[*,val]
    x = (*self.xvalsPtr)[*,val]
    y = (*self.yvalsPtr)[0,val]
  endif else begin
    dat = reform(*self.dataPtr)
    daterr = reform(*self.errorPtr)
    x = reform(*self.xvalsPtr)
    y = (*self.yvalsPtr)
    val = 0
  endelse
  ; Call the function here and see if it evaluates
  ; properly!

  test = pan_userfunction(x,parms,$
                    	  expr = out.expr, $
                    	  eval = eval)
  if eval ne 1 then return
  *self.ocurrentPtr = obj_new("func",name = name, $
                      xvalues = x, $
                      expr = out.expr, $
                      parms = parms)

  (*self.ocurveGroup)[val] -> add,*self.ocurrentPtr
  parmInfo = self->packageParmInfo()
endif else begin		; Now consider any of the library of functions

	name = strlowcase((*self.curvenamesPtr)[event.value])
	name = strtrim('pan_'+name,2)
	if n_elements(*self.dataPtr) eq 0 then return

	; Which group are we looking at?
	widget_control,self.groupSlider,get_value = val
	val = fix(val[0])-1
    datSize = size(*self.dataPtr)
    if datSize[0] eq 2 then begin
      dat = (*self.dataPtr)[*,val]
      daterr = (*self.errorPtr)[*,val]
      x = (*self.xvalsPtr)[*,val]
      y = (*self.yvalsPtr)[0,val]
    endif else begin
      dat = reform(*self.dataPtr)
      daterr = reform(*self.errorPtr)
      x = reform(*self.xvalsPtr)
      y = (*self.yvalsPtr)
      val = 0
    endelse

	*self.ocurrentPtr = obj_new("func",name = name,xvalues = x)
endelse

; Ok...now do we add the curve using a mouse or using a dialog?
initGuesses = (*self.prefs).initGuesses
if initGuesses eq 1 and (name ne 'pan_userfunction') then begin
; Use dialog input
  (*self.ocurveGroup)[val] -> add,*self.ocurrentPtr
  parmInfo = self->packageParmInfo()

  enterparminfo,  parmInfo, $
                  group_leader = event.top, $
                  notifyIds = [event.id,event.top], $
                  modal = 1, $
                  parmInfoOut = parmInfoOut

  dummy = self->updateParmInfo(parmInfoOut)

  wset,self.resPix
  self->plotResiduals
  wset,self.resVis
  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

  wset,self.datPix
  self->displaydata
  wset,self.datVis
  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

  return
endif

*self.ocurrentPtr->getProperty,canDraw = canDraw

if (initGuesses eq 0) and (canDraw eq 1) then begin
; Use mouse input
  self.addcurve = 1
  drawMessage = *self.ocurrentPtr->getDrawMessage()
  widget_control,self.info,set_value = drawMessage[0:1]
  return
endif

if (initGuesses eq 0) and (canDraw eq 0) then begin
; Use dialog input
  if name ne 'pan_userfunction' then begin
   	(*self.ocurveGroup)[val] -> add,*self.ocurrentPtr
    parmInfo = self->packageParmInfo()
  endif
  enterparminfo,  parmInfo, $
                  group_leader = event.top, $
                  notifyIds = [event.id,event.top], $
                  modal = 1, $
                  parmInfoOut = parmInfoOut

  dummy = self->updateParmInfo(parmInfoOut)

  wset,self.resPix
  self->plotResiduals
  wset,self.resVis
  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

  wset,self.datPix
  self->displaydata
  wset,self.datVis
  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
  return
endif

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::zoomEvents, event = event
case event.type of
0:	begin		; button press
	  self.mouse = event.press
	  if self.mouse eq 4 then begin
	    self.autoscale = 1

	    !x = *self.datxPtr
	    !y = *self.datyPtr
	    wset,self.datPix
	    self->displaydata
	    wset,self.datVis
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
		wset,self.resPix
		self->plotresiduals
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]
	  endif
	  if self.mouse eq 1 then begin
	    self.xbox[0] = event.x
	    self.ybox[0] = event.y
	    !x = *self.datxPtr
	    !y = *self.datyPtr
	    wset,self.datVis
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
	    wset,self.resPix
	    self->plotresiduals
	    wset,self.resVis
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]
	    empty
	    self.autoscale = 0
	    widget_control,self.datWin,/draw_motion_events
	  endif
	end
1:	begin	; button release
	 if self.mouse eq 1 then begin
	  xll = self.xbox[0] < self.xbox[1]
	  yll = self.ybox[0] < self.ybox[1]
	  w = abs(self.xbox[1] - self.xbox[0])
	  h = abs(self.ybox[1] - self.ybox[0])
	  xur = xll + w
	  yur = yll + h
	  !x = *self.datxPtr & !y = *self.datyPtr
	  ll = convert_coord(xll,yll,/device,/to_data)
	  ur = convert_coord(xur,yur,/device,/to_data)
	  self.xrange = [ll[0],ur[0]]
	  self.yrange = [ll[1],ur[1]]
	  wset,self.resPix
	  self->plotresiduals
	  wset,self.resVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]
	  !x = *self.datxPtr
	  !y = *self.datyPtr
	  wset,self.datPix
	  self->displaydata
	  wset,self.datVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
	  self.mouse = 0B
	  widget_control,self.datWin,draw_motion_events = 0
	 endif
	 if self.mouse eq 4 then begin
	  wset,self.resPix
	  self->plotresiduals
	  wset,self.resVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]
	  wset,self.datPix
	  self->displaydata
	  wset,self.datVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
	  self.mouse = 0B
	  widget_control,self.datWin,draw_motion_events = 0
	 endif
	end
2:	begin	; mouse motion
	  if self.mouse eq 1 then begin
	  	self.xbox[1] = event.x
	  	self.ybox[1] = event.y
	  	xc = [self.xbox[0],event.x,event.x,$
	  	      self.xbox[0],$
	  	      self.xbox[0]]
	  	yc = [self.ybox[0],self.ybox[0],$
	  	      event.y,event.y,$
	  	      self.ybox[0]]
		!x = *self.datxPtr & !y = *self.datyPtr
	  	wset,self.datVis
	  	device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
	  	plots,xc,yc,/device
	  	empty
	  endif
	end
else:
endcase
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::drawCurveEvents, event = event
widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then val = 0
xrange = self.xrange

case event.type of
0:	begin		; button press
	  self.mouse = event.press
	  if self.mouse eq 1 then begin
        dcoords = convert_coord(event.x,event.y,/device,/to_data)

	    if self.addcurve eq 1 then begin
	      (*self.ocurveGroup)[val] -> add,*self.ocurrentPtr
          *self.ocurrentPtr->changefirst,dcoords[0],dcoords[1],xrange
          widget_control,self.datWin,/draw_motion_events
	    endif
	    if self.addcurve eq 2 then begin
          *self.ocurrentPtr->changesecond,dcoords[0],dcoords[1],xrange
          widget_control,self.datWin,/draw_motion_events
	    endif
        wset,self.datVis
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

		(*self.ocurveGroup)[val]->draw,overplot = 1
	  endif
	end
1:	begin	; button release
		dcoords = convert_coord(event.x,event.y,/device,/to_data)
		if self.mouse eq 1 then begin
		  if self.addcurve eq 1 then begin
		    *self.ocurrentPtr->changefirst,dcoords[0],dcoords[1],xrange
			widget_control,self.datWin,draw_motion_events = 0
			drawMessage = *self.ocurrentPtr->getDrawMessage()
			widget_control,self.info,set_value = drawMessage[2:3]
	      endif
		  if self.addcurve eq 2 then begin
		    *self.ocurrentPtr->changesecond,dcoords[0],dcoords[1],xrange
		    widget_control,self.datWin,draw_motion_events = 0
		    widget_control,self.info,set_value = ''
	      endif
        endif
        wset,self.datVis
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
        (*self.ocurveGroup)[val]->draw,overplot = 1
	  self.mouse = 0B
	  self.addcurve = self.addcurve + 1
	  if self.addcurve eq 3 then begin
	    self.addcurve = 0
	    wset,self.datPix
		self->displaydata
		wset,self.datVis
		device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
		oc = (*self.ocurveGroup)[val]
		ncurves = oc->count()
	    widget_control,self.curveSlider,set_slider_max = (ncurves > 2)
	  endif
	end
2:	begin	; mouse motion
		dcoords = convert_coord(event.x,event.y,/device,/to_data)
		if self.mouse eq 1 then begin
		  if self.addcurve eq 1 then begin
		    *self.ocurrentPtr->changefirst,dcoords[0],dcoords[1],xrange
	      endif
		  if self.addcurve eq 2 then begin
		    *self.ocurrentPtr->changesecond,dcoords[0],dcoords[1],xrange
	      endif
        endif
        wset,self.datVis
	    device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
        (*self.ocurveGroup)[val]->draw,overplot = 1
	end
else:
endcase
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::drawEvents, event = event
if n_elements(*self.dataPtr) eq 0 then return
!x = *self.datxPtr
!y = *self.datyPtr
case self.addcurve of
0:	begin
	  self->zoomEvents,event = event
	end
1:	begin
	  self->drawCurveEvents, event = event
	end
2:	begin
	  self->drawCurveEvents, event = event
	end
else:
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::resize,event = event
ctrlgeom = widget_info(self.ctrlbase,/geometry)
infogeom = widget_info(self.infobase,/geometry)
xsize = event.x
ysize = event.y

; New data window dimensions
newxsize = xsize-ctrlgeom.xsize-infogeom.xsize
newysize = fix(ysize*(self.winRatio/(1.0+self.winRatio)))

widget_control,self.datWin,draw_xsize = newxsize, $
               draw_ysize = newysize
wdelete,self.datPix
window,/free,/pixmap,xsize = newxsize,ysize = newysize
self.datPix = !d.window

; New residual window dimensions
newysize = fix(ysize/(1.0+self.winRatio))

widget_control,self.resWin,draw_xsize = newxsize, $
               draw_ysize = newysize
wdelete,self.resPix
window,/free,/pixmap,xsize = newxsize,ysize = newysize
self.resPix = !d.window

wset,self.resPix
self->plotResiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opanEvents,event
if tag_names(event,/structure_name) eq 'WIDGET_BASE' then begin
  widget_control,event.top,get_uvalue = self
  self->resize,event = event
  return
endif
if tag_names(event,/structure_name) eq 'CORREL_EVENT' then return
widget_control,event.id,get_uvalue = cmd
call_method,cmd.method,cmd.object,event = event
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::displayFitParameters, event = event
widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
if n_elements(*self.dataPtr) eq 0 then return
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then val = 0
oc = (*self.ocurveGroup)[val]
oc->displayparms,output = output
if n_elements(output) ne 0 then begin
	widget_control,self.info,set_value = output
endif else begin
	widget_control,self.info,set_value = 'No curves selected'
endelse
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::displaydata,_Extra = extra
widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
if n_elements(*self.dataPtr) eq 0 then return
  datSize = size(*self.dataPtr)
  if datSize[0] eq 2 then begin
    dat = (*self.dataPtr)[*,val]
    daterr = (*self.errorPtr)[*,val]
    x = (*self.xvalsPtr)[*,val]
    y = (*self.yvalsPtr)[0,val]
  endif else begin
    dat = reform(*self.dataPtr)
    daterr = reform(*self.errorPtr)
    x = reform(*self.xvalsPtr)
    y = 0  ;MSW: 30/1/09 Need this to load a file with only 1 spectrum. Hopefull this works in all cases
    val = 0
  endelse

thisFormat = '(f5.0)'
self.title = '!6Spectrum No = '+strtrim(string(y+1,format = thisformat),2)

if self.autoscale eq 1 then begin
  dx = 0.1*(max(x,/nan)-min(x,/nan))
  self.xrange = [min(x,/nan)-dx,max(x,/nan)+dx]
  dy = 0.1*(max(dat,/nan)-min(dat,/nan))
  self.yrange = [min(dat-daterr,/nan)-dy,max(dat+daterr,/nan)+dy]
endif

if (*self.prefs).xenforce eq 1 then begin
  self.xrange = [(*self.prefs).xmin,(*self.prefs).xmax]
endif
if (*self.prefs).yenforce eq 1 then begin
  self.yrange = [(*self.prefs).ymin,(*self.prefs).ymax]
endif

;MSW ****REMOVED psym command in order to plot line not symbols for spectrum
plot,x,dat,/nodata,xtitle = self.xtitle,ytitle = self.ztitle,title = self.title, $
     xrange = self.xrange,yrange = self.yrange,xstyle = 1,ystyle = 1,_Extra = extra;,$
     ;psym = 4
errplot,x,dat-daterr,dat+daterr,width = 0.0,color=100
oplot,x,dat;,psym = 4     ;plot this way to get error bars under the plot

; Draw the fit range if in the viewable window
xfitLo = (*self.prefs).xfitlo
xfitHi = (*self.prefs).xfithi
xdispLo = (!x.crange)[0]
xdispHi = (!x.crange)[1]
if xfitLo gt xdispLo then $
  plots,[xfitLo,xfitLo],!y.crange,linestyle = 2,color=4
if xfitHi lt xdispHi then $
  plots,[xfitHi,xfitHi],!y.crange,linestyle = 2,color=4

if obj_valid((*self.ocurveGroup)[val]) then begin
  ncurves = (*self.ocurveGroup)[val]->count()
  ocurves = (*self.ocurveGroup)[val]
  ocurves->drawcomponents,linestyle = 2,thick = 1.5,color=1
  ocurves->evaluate,x,yout = yout

  if n_elements(yout) gt 1 then begin
    oplot,x,yout,linestyle = 0,thick = 1.5,color=1
  endif else begin
    if (!d.name ne 'PS') and (self.jpeg ne 1) then wset,self.resVis
    if self.jpeg eq 0 then erase
  endelse

endif
*self.datxPtr = !x
*self.datyPtr = !y

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::plotResiduals, event = event,_Extra = extra
if n_elements(*self.dataPtr) eq 0 then return
widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then val = 0
if not obj_valid((*self.ocurveGroup)[val]) then return

ncurves = (*self.ocurveGroup)[val]->count()
if ncurves eq 0 then begin
  erase
  return
endif
if datSize[0] eq 2 then begin
    dat = (*self.dataPtr)[*,val]
    daterr = (*self.errorPtr)[*,val]
    x = (*self.xvalsPtr)[*,val]
endif else begin
    dat = reform(*self.dataPtr)
    daterr = reform(*self.errorPtr)
    x = reform(*self.xvalsPtr)
endelse

(*self.ocurveGroup)[val]->evaluate,x,yout = yout

residuals = (yout-dat)/daterr

;MSW: it was skipping over blank spectra when fitting, but getting stuck re-displaying
;     them when reviewing the results - added catch here 11/11/09
if finite(total(residuals)) ne 1 then begin
;  print,'Residuals blank',val
  return 
endif

plot,x,residuals,psym = 0,xrange = self.xrange,xstyle = 1,xtitle = self.xtitle,$
     ytitle = '!6Residuals',_Extra = extra
plots,!x.crange,[1.0,1.0],/data,linestyle = 2,thick = 1.5,color=1
plots,!x.crange,[-1.0,-1.0],/data,linestyle = 2,thick = 1.5,color=1
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::rebinRestore,event = event
; Restore the original binning
*self.xvalsPtr = *self.oxvalsPtr
*self.yvalsPtr = *self.oyvalsPtr
*self.dataPtr = *self.odataPtr
*self.errorPtr = *self.oerrorPtr

; Recalculate the functions
data = *self.dataPtr
datSize = size(data)
if datSize[0] eq 1 then begin	; a single group
	x = *self.xvalsPtr
	oc = (*self.ocurveGroup)[0]
	ncurves = oc->count()
	if ncurves gt 0 then begin
  		oc->setxvalues,x,yout = yout
	endif
endif else begin
	ngrps = datSize[2]
	for i = 0,ngrps-1 do begin
		oc = (*self.ocurveGroup)[i]
		ncurves = oc->count()
		if ncurves gt 0 then begin
		  x = reform((*self.xvalsPtr)[*,i])
  			oc->setxvalues,x,yout = yout
		endif
	endfor
endelse


; Display the data
wset,self.resPix
self->plotResiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]
wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::rebinData,event = event
widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
if n_elements(*self.dataPtr) eq 0 then return
datSize = size(*self.dataPtr)
if datSize[0] eq 2 then begin
    dat = (*self.dataPtr)[*,val]
    daterr = (*self.errorPtr)[*,val]
    x = (*self.xvalsPtr)[*,val]
    y = (*self.yvalsPtr)[0,val]
endif else begin
    dat = reform(*self.dataPtr)
    daterr = reform(*self.errorPtr)
    x = reform(*self.xvalsPtr)
    y = (*self.yvalsPtr)
    val = 0
endelse
nx = n_elements(x)
result = opan_rebin_widget(x,group_leader = event.top)
if result.cancel then return
if result.nbins gt nx then begin
  strout = 'Number of desired bins cannot exceed the number of input bins'
  void = dialog_message(dialog_parent = event.top,strout)
  return
endif
; Ok, we've done the error checking...now do the rebinning
xlo = result.xlo & xhi = result.xhi
nbins = result.nbins
dx = (xhi-xlo)/(nbins-1.0)
x_out = xlo+dx*dindgen(nbins)

; Consider single group first
if datSize[0] eq 1 then begin
  x_in = x
  z_in = dat
  dz_in = daterr
  drebin_points_wrapper,x_in,z_in,dz_in,x_out,z_out,dz_out
  *self.dataPtr = z_out
  *self.errorPtr = dz_out
  *self.xvalsPtr = x_out
endif else begin
; Ok, now consider the general case of multiple groups
	ngrps = datSize[2]
	old = *self.dataPtr
	errold = *self.errorPtr
	err = dblarr(nbins,ngrps)
	new = dblarr(nbins,ngrps)
	xnew = dblarr(nbins,ngrps)

	for i = 0,ngrps-1 do begin
   	 	z_in = old[*,i]
    	dz_in = errold[*,i]
    	x_in = reform((*self.xvalsPtr)[*,i])
    	drebin_points_wrapper,x_in,z_in,dz_in,x_out,z_out,dz_out
		new[*,i] = z_out[*]
		err[*,i] = dz_out[*]
		xnew[*,i] = x_out[*]
		oc = (*self.ocurveGroup)[i]
  		oc->setxvalues,x_out,yout = yout
	endfor
	*self.dataPtr = new
	*self.errorPtr = err
	*self.xvalsPtr = xnew
endelse


wset,self.resPix
self->plotResiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]
wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::selectGroup, event = event
wset,self.resPix
self->plotResiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]
wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
if n_elements(*self.dataPtr) eq 0 then return
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then val = 0
oc = (*self.ocurveGroup)[val]
oc->displayparms,output = output
ncurves = oc->count()
if ncurves gt 0 then begin
  widget_control,self.curveSlider,set_slider_max = (ncurves > 2)
endif
if n_elements(output) eq 0 then begin
  output = 'No curves present'
endif
widget_control,self.info,set_value = output
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan_replaceSymbol,inText
outText = inText
invAngstromSym = '!3!sA!r!u!9 %!3!n!E-1!N'
omegaSym = '!4x!3'
if strpos(inText,'A-1') ne -1 then outText = invAngstromSym
if strpos(inText,'ueV') ne -1 then outText = '!4l!6eV'
if strpos(inText,'omega') ne -1 then outText = omegaSym
return,outText
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::loadasciidata, event = event
self->initCleanup

catch,theError
if theError ne 0 then begin
  catch,/cancel
  void = dialog_message(dialog_parent = event.top, $
         !error_state.msg+' Returning...')
  return
endif

filenames = dialog_pickfile(path = self.workDir,/read, $
           dialog_parent = event.top,filter = '*.txt', $
           get_path = thisPath)
if filenames[0] eq '' then return
length = strlen(thisPath)
self.printFileName = strmid(filenames[0], length)

openr,lun,filenames[0],/get_lun


nbig = 10000
x = FLTARR(nbig)
y = FLTARR(nbig)
yerr = FLTARR(nbig)
xx = 0.0
yy = 0.0
zz = 0.0
count = 0

dummy = ''
ncolon = 0
colPos = 0
while colPos ne (-1) do begin
  readf,lun,dummy
  colPos = strpos(dummy,'#')
  ncolon = ncolon + 1
endwhile

if ncolon ne 0 then begin
  reads,dummy, xx,yy,zz
  x[count] = float(xx)
  y[count] = float(yy)
  yerr[count] = float(zz)
  count = count + 1
endif

while (not(eof(lun))) do begin
  readf,lun,dummy
  reads,dummy, xx,yy,zz
  x[count] = float(xx)
  y[count] = float(yy)
  yerr[count] = float(zz)
  count = count + 1
endwhile
free_lun,lun

x = x[0:count-1]
y = y[0:count-1]
yerr = yerr[0:count-1]

; Sort the numbers!
xsort = sort(x)
x = x[xsort] & y = y[xsort] & yerr = yerr[xsort]

*self.dataPtr = y
*self.errorPtr = yerr
*self.xvalsPtr = x
*self.yvalsPtr = 0.0
*self.headerPtr = filenames[0]
self.xtitle = 'x'
self.ytitle = 'y'
self->initNewDataObjects

wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::loadgroupdata, event = event
self->initCleanup

catch,theError
if theError ne 0 then begin
  catch,/cancel
  void = dialog_message(dialog_parent = event.top, $
         !error_state.msg+' Returning...')
  return
endif

filename = dialog_pickfile(path = self.workDir,/read, $
           dialog_parent = event.top,filter = '*.txt',get_path = thisPath)
if filename eq '' then return

d = opan_readTextGroup(filename)
ux = 1+bytarr(n_elements(d.x))
uy = 1+bytarr(n_elements(d.y))

length = strlen(thisPath)
self.printFileName = strmid(filename, length)

*self.dataPtr = d.z
*self.errorPtr = d.zerr
*self.xvalsPtr = (d.x)#uy
*self.yvalsPtr = ux#(d.y)
*self.headerPtr = filename
self.xtitle = d.xlabel
self.ytitle = d.ylabel
self->initNewDataObjects

wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MSW
pro opan::loadfitsdata, event = event
self->initCleanup

; nice little if statement - more useful if it gave you the line in the code that 
; screwed up though!
catch,theError
if theError ne 0 then begin
  catch,/cancel
  void = dialog_message(dialog_parent = event.top, $
         !error_state.msg+' Returning...')
  return
endif

; get FITS file to read in
filename = dialog_pickfile(path = self.workDir,/read, $
           dialog_parent = event.top,filter = '*.fits',get_path = thisPath)
if filename eq '' then return
print,'Reading file: ',filename

;MSW 15/4/10 - added redshift option
result = opan_redshift_widget(group_leader=event.top)  ; call the redshift_widget to
						       ; read in a the redshift in km/s
redshift = result.redshift

d = opan_readFitsGroup(filename,redshift)

; Dunno what this means - but I think it needs to be here
length = strlen(thisPath)
self.printFileName = strmid(filename, length)

; need to create these arrays in order for the 'displaydata' procedure to work
ux = 1+bytarr(n_elements(d.x))
uy = 1+bytarr(n_elements(d.y))

; Set the pointers and initialise the objects
*self.dataPtr = d.z
*self.errorPtr = d.zerr
*self.xvalsPtr = (d.x)#uy
*self.yvalsPtr = ux#(d.y)
*self.headerPtr = filename
self.xtitle = '!6Wavelength (!6!sA!r!u!9 %!6!n!N)'
self.ytitle = 'Flux'
self->initNewDataObjects

; display the new data
wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::loadtestdata, event = event
self->initCleanup
; Create some fake data
nchan = 170
x = rmd_makepoints(xlo = -12d,xhi = 12d,npts = nchan)
ux = 1+bytarr(nchan)

nq = 20

q = rmd_makepoints(xlo = 0.25,xhi = 1.75,npts = nq)
ro = 1.73
ao = (5.0+4.0*beselj(q*ro,0))/9.0
uq = 1+bytarr(nq)

area = 4500.0 & bg = 0.05*area
yy = bg   + (pan_gaussian(x,[area,0d,1.0]))#ao + $
	 		0.5*(pan_lorentzian(x,[5.0*area,-3d,2d]))#(1.0-ao) + $
	 		0.5*(pan_lorentzian(x,[5.0*area,3d,2d]))#(1.0-ao)

datmat = yy

y = dblarr(nchan,nq)
for j = 0,nq-1 do begin
  for i = 0,nchan-1 do begin
    y[i,j] = randomn(s,1,poisson = datmat[i,j])
  endfor
endfor
yerr = sqrt(y)

; Now create a matrix out of all of the data variables
xmat = x#uq
ymat = ux#q
data = y
error = yerr
bigNum = 1.e7
*self.dataPtr = data/area
*self.errorPtr = error/area
*self.xvalsPtr = xmat
*self.yvalsPtr = ymat
self.printFileName = 'testdata.txt'
*self.headerPtr = ['Test data for demonstration purposes',$
                   'Simulated rotational tunneling']
self.xtitle = '!6Wavelength (!6!sA!r!u!9 %!6!n!N)'
self.ytitle = 'Q'
self.ztitle = '!6Intensity (arb units)'
self.title = '!6Neutron Spectrum'
(*self.prefs).xlabel = self.xtitle
(*self.prefs).ylabel = self.ztitle
self->initNewDataObjects
wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::loadtestres, event = event
; Create some fake data
nchan = 170
x = rmd_makepoints(xlo = -12d,xhi = 12d,npts = nchan)
ux = 1+bytarr(nchan)

nq = 20

q = rmd_makepoints(xlo = 0.25,xhi = 1.75,npts = nq)
ro = 1.73
ao = 0.5*(1.0+beselj(q*ro,0))
uq = 1+bytarr(nq)

area = 4500.0 & bg = 0.05*area
yy = bg   + (pan_gaussian(x,[area,0d,1.0]))#ao

datmat = yy

y = dblarr(nchan,nq)
for j = 0,nq-1 do begin
  for i = 0,nchan-1 do begin
    y[i,j] = randomn(s,1,poisson = datmat[i,j])
  endfor
endfor
yerr = sqrt(y)

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro pan_iterproc,fnc,p,iter,fnorm,dof = dof, stopBut = stopBut,_Extra = extra
if n_elements(stopBut) eq 0 then return
if widget_info(stopBut,/valid_id) eq 0 then return
event = widget_event(/nowait,stopBut)
evname = tag_names(event,/structure_name)
if evname eq '' or event.id eq 0 then return

if evname eq 'WIDGET_BUTTON' and event.select then begin
  event1 = widget_event(/nowait,stopBut)
  common mpfit_error,mperr
  mperr = -1
endif

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::fitOneGroup, event = event
; This method uses Craig Markwardt's suggestion for a curve-fit interrupt.
; This method is for single group fitting only.

widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
oc = (*self.ocurveGroup)[val]
if oc->count() ne 0 then begin

  parms = oc->getparms()
  parinfo = oc->createParInfo()
  datSize = size(*self.dataPtr)
  if datSize[0] eq 2 then begin
    y = (*self.dataPtr)[*,val]
    yerr = (*self.errorPtr)[*,val]
    x = (*self.xvalsPtr)[*,val]
  endif else begin
    y = reform(*self.dataPtr)
    yerr = reform(*self.errorPtr)
    x = reform(*self.xvalsPtr)
    val = 0
  endelse

xPtr = ptr_new(x)
  widget_control,self.fitstatfield,set_value = self.fitstatus[1]
  widget_control,self.interrupt,sensitive = 1
  widget_control,self.interrupt,/clear_events

; Try to fit it...
    w = 1.0/yerr^2
    limits = where((x ge (*self.prefs).xfitlo) and (x le (*self.prefs).xfithi))
    xlim = x[limits]
    ylim = y[limits]
    wlim = w[limits]
    iterargs = {stopBut:self.interrupt}
    iterProc = 'pan_iterproc'
    yf = mpcurvefit(xlim, ylim, wlim, parms, sigma, $
                      FUNCTION_NAME='fitfunction', $
                      status = status, $
                      /quiet, $
                      itmax = (*self.prefs).maxIter, $
                      chisq = chisq, $
                      parinfo = parinfo, $
                      iterargs = iterargs, $
                      iterProc = iterProc, $
                      functargs = {oc:oc,xPtr:xPtr}, $
                      covar = covar,	$
                      /autoderivative, $
                      ERRMSG=errmsg)
    ptr_free,xPtr
    widget_control,self.interrupt,sensitive = 0
    widget_control,self.fitstatfield,set_value = self.fitstatus[0]


;MSW: catch for if the spectrum is all zeros (i.e. blank) 
;MSW: added qualifier to mean "blank (for data or error arrays) within fit limits" 11/11/09
if (total(y[limits[0]:limits[size(limits,/dimensions)]]) eq 0.0) or $
   (total(yerr[limits[0]:limits[size(limits,/dimensions)]]) eq 0.0) then begin
;  print,'Spectrum blank',val
  return 
endif

; calculate the correlation matrix
	PCOR = COVAR * 0
	FOR i = 0, n_elements(parms)-1 DO FOR j = 0, n_elements(parms)-1 DO $
		PCOR(i,j) = COVAR(i,j)/sqrt(COVAR(i,i)*COVAR(j,j))
    oc->set_pcor,pcor

    ;if widget_info(tlb,/valid_id) then begin
    ;  widget_control,tlb,/destroy
    ;endif
	widget_control,self.interrupt,/clear_events
    if status le 0 then begin
      self.status = 0
      strout = 'Fit cancelled'
      widget_control,self.fitStatField,set_value = strout
    endif else begin
      self.status = 1
    endelse
    if n_elements(sigma) gt 0 then oc->setparmError,sigma
    dof = n_elements(xlim)-n_elements(parms)
	chi_reduced = chisq/dof
    oc->setchisq,chi_reduced
endif

; Plot the new fit with the data and plot the residuals
wset,self.resPix
self->plotresiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

; Display the parameters
self->displayFitParameters
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::fitAllGroups, event = event
; Which groups do we want to fit?
if n_elements(*self.dataPtr) eq 0 then return
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then nq = 1 else nq = datSize[2]

widget_control,self.groupField,get_value = groupString
groupString = groupString[0]
index = opan_selectgroups(groupString)
*self.grpArrayPtr = index

; Will we be using the same parameters for all of the groups?
if (*self.prefs).same eq 1 then begin
  ; Which of the current groups has at least an initial guess?
  ; Let's grab the first one...
  oc = -1L
  goodIndex = -1
  for i = 0,nq-1 do begin
    ncurves = (*self.ocurveGroup)[i]->count()
    if goodIndex eq -1 then begin
      if ncurves gt 0 then begin
        goodIndex = i
        oc = (*self.ocurveGroup)[goodIndex]
      endif
    endif
  endfor

  nindex = n_elements(index)

  for i = 0,nindex-1 do begin
    ind = index[i]-1
    if (ind ne goodIndex) then begin

      oall = oc->get(/all)	; oc is an instance of the FUNC_CONT object class.
      						; oall is an array of FUNC object references.
      ncurves = oc->count()
	  ; Remove any existing objects in oc and destroy them
	  o_old = (*self.ocurveGroup)[ind]
	  o_old_all = o_old->get(/all)
	  nold = o_old->count()
	  if nold gt 0 then begin
	    o_old->remove,/all
		for k = 0,nold-1 do begin
			obj_destroy,o_old_all[k]
		endfor
	  endif

      for j = 0,ncurves-1 do begin

		; Now extract the curve information
		oall[j]->getproperty,name = name, $
		                     xvalues = xvalues, $
		                     step = step, $
		                     parms = parms, $
		                     fixed = fixed, $
		                     fixvalues = fixvalues, $
		                     low = low, $
		                     lovalues = lovalues, $
		                     high = high, $
		                     hivalues = hivalues, $
		                     tied = tied, $
		                     parmError = parmError, $
		                     expr = expr
		; ...and make a copy of the object
		curveCopy = obj_new('FUNC',name = name, $
		                     xvalues = xvalues, $
		                     step = step, $
		                     parms = parms, $
		                     fixed = fixed, $
		                     fixvalues = fixvalues, $
		                     low = low, $
		                     lovalues = lovalues, $
		                     high = high, $
		                     tied = tied, $
		                     hivalues = hivalues, $
		                     expr = expr)

        curveCopy->setproperty,parmError = parmError,/calculate
        (*self.ocurveGroup)[ind]->add,curveCopy

      endfor
    endif

  endfor

endif

self.curIndex = 0
widget_control,self.groupSlider, $
               set_value = (*self.grpArrayPtr)[self.curIndex]

self.status = 1
while (self.status eq 1) and (self.curIndex le n_elements(*self.grpArrayPtr)-1) do begin
  self->fitOneGroup,event = event
  self.curIndex = self.curIndex + 1
  if (self.curIndex le n_elements(*self.grpArrayPtr)-1) then begin
    widget_control,self.groupSlider, $
                   set_value = (*self.grpArrayPtr)[self.curIndex]
  endif
endwhile

; Ok, now go through and clear any fits that have a chi-squared of -1.0
for i = 0,nq-1 do begin
  ncurves = (*self.ocurveGroup)[i]->count()
  if ncurves gt 0 then begin
    oc = (*self.ocurveGroup)[i]
    chisq = oc->getchisq()
    if chisq eq (-1.0) then begin	; remove it
		oall = oc->get(/all)
		obj_destroy,oall
		oc->remove,/all
    endif
  endif
endfor

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::sliderCurveSel,event = event
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::remSelCurve,event = event
widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
oc = (*self.ocurveGroup)[val]
ncurves = oc->count()
if ncurves eq 0 then return
oall = oc->get(/all)

widget_control,self.curveSlider,get_value = cval
cval = fix(cval[0])
cval = cval - 1	; now it goes from 0 to ncurves-1
if cval eq ncurves then return	; too big!
oc->remove,oall[cval]
obj_destroy,oall[cval]
widget_control,self.curveSlider,set_slider_max = (cval > 2)

wset,self.resPix
self->plotResiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

wset,self.datPix
self->displayData
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::plotEISF, event = event
; No data?->Get out.
if n_elements(*self.dataPtr) eq 0 then return

; Count how many groups of curves there are.  If there are less than two
; then get out.
fitObject = *self.ocurveGroup
ngroups = n_elements(fitObject)
totalFits = 0
for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  ncurves = oc->count()
  if ncurves gt 0 then begin
    totalFits = totalFits + 1
    if (n_elements(okIndex) eq 0) then $
       okIndex = i else okIndex = [okIndex,i]
  endif
endfor
if totalFits lt 2 then return

thisEvent = tag_names(event,/structure_name)
case thisEvent of

'WIDGET_BUTTON': $
  begin
    widget_control,self.logBase,sensitive = 0
    opan_plotEISF,	fitObject, $
                    *self.yvalsPtr, $
					group_leader = event.top, $
                   	notifyIds = [event.id,event.top], $
                   	xtitle = self.ytitle, $
                   	ytitle = 'Value', $
                   	title = '', $
                   	workDir = self.workDir, $
                   	logDirectory = self.logDirectory, $
                   	stringPtr = self.logStringPtr

  end
else:	    widget_control,self.logBase,sensitive = 1
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::plotFitParameter, event = event
; No data?->Get out.
if n_elements(*self.dataPtr) eq 0 then return

; Count how many groups of curves there are.  If there are less than two
; then get out.
fitObject = *self.ocurveGroup
ngroups = n_elements(fitObject)
totalFits = 0
for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  ncurves = oc->count()
  if ncurves gt 0 then begin
    totalFits = totalFits + 1
    if n_elements(okIndex) eq 0 then okIndex = i else okIndex = [okIndex,i]
  endif
endfor
if totalFits lt 2 then return

thisEvent = tag_names(event,/structure_name)
case thisEvent of

'WIDGET_BUTTON': $
  begin
    widget_control,self.logBase,sensitive = 0
    opan_plotParms,	fitObject, $
                    *self.yvalsPtr, $
					group_leader = event.top, $
                   	notifyIds = [event.id,event.top], $
                   	xtitle = self.ytitle, $
                   	ytitle = 'Value', $
                   	title = '', $
                   	workDir = self.workDir, $
                   	logDirectory = self.logDirectory, $
                   	stringPtr = self.logStringPtr

  end
;'PLOTPARMSEVENT': $
;  begin
;  end
else:    widget_control,self.logBase,sensitive = 1
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::saveParmsAsText,event = event
if n_elements(*self.dataPtr) eq 0 then return
thisPath = self.workDir
fitObject = *self.ocurveGroup
ngroups = n_elements(fitObject)
ncurves = 0
for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  ncurves = ncurves + (oc->count())
endfor
if ncurves eq 0 then return

filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                                              title = 'Enter parameter text file name',$
                                              /read,filter = '*.fit',$
                                              path = thisPath)
if filename eq '' then return
filename = filename + '.fit'
openw,lun,filename,/get_lun

for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  chisq = oc->getchisq()
  if chisq ne (-1.0) then begin
    ncurves = oc->count()
    if ncurves ne 0 then begin
      printf,lun,'###############################'
      printf,lun,'Group: ',strtrim(string(i+1),2)
      printf,lun,'###############################'
      oc->displayparms,output = output
      nlines = n_elements(output)
      for j = 0,nlines-1 do begin
        printf,lun,output[j]
      endfor
      printf,lun,'###############################'
    endif
  endif
endfor
free_lun,lun
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MSW
pro opan::saveParmsAsText_katrina,event = event
; procedure to write results into text file with format determined by 
;   func_cont::displayparms_katrina

if n_elements(*self.dataPtr) eq 0 then return
thisPath = self.workDir
fitObject = *self.ocurveGroup
ngroups = n_elements(fitObject)
ncurves = 0
for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  ncurves = ncurves + (oc->count())
endfor
if ncurves eq 0 then return

; first pick a file and open it for writing
filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                                              title = 'Enter parameter text file name',$
                                              /read,filter = '*.fit',$
                                              path = thisPath)
if filename eq '' then return
filename = filename + '.fit'
openw,lun,filename,/get_lun

; now printf the relevent info
; first print a comment line with column headings
printf,lun,'#C area error measwave error fwhm error spax_id line_id'
result = opan_trans_widget(group_leader=event.top)  ; call the opan_trans_widget to 
						    ; read in a value for the line_id 
						    ; transition name
trans = result.trans
if result.cancel then return
; for loop: loops around array called 'output' (passed in by displayparms_katrina) 
;   and reads each entry onto a separate line
for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  chisq = oc->getchisq()
  if chisq ne (-1.0) then begin
    ncurves = oc->count()
    if ncurves ne 0 then begin
      spax_number = i + fix((*self.yvalsPtr)[1])
      ; call displayparms_katrina procedure and pass the trans varaible and the group no
      oc->displayparms_katrina,output = output,trans = trans,spax_id = spax_number
      nlines = n_elements(output)
      for j=0,nlines-1 do begin   ; print out every entry of the output array onto a new line
        printf,lun,(output[j])
	print,output[j]
      endfor
    endif
  endif
endfor
free_lun,lun
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MSW
pro opan::saveParmsAsText_katrina_cont,event = event
; procedure to write results into text file with format determined by 
;   func_cont::displayparms_katrina

if n_elements(*self.dataPtr) eq 0 then return
thisPath = self.workDir
fitObject = *self.ocurveGroup
ngroups = n_elements(fitObject)
ncurves = 0
for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  ncurves = ncurves + (oc->count())
endfor
if ncurves eq 0 then return

; first pick a file and open it for writing
filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                                              title = 'Enter parameter text file name',$
                                              /read,filter = '*.fit',$
                                              path = thisPath)
if filename eq '' then return
filename = filename + '.fit'
openw,lun,filename,/get_lun

; now printf the relevent info
; first print a comment line with column headings
printf,lun,'#C area error measwave error fwhm error spax_id line_id'
result = opan_trans_widget(group_leader=event.top)  ; call the opan_trans_widget to 
						    ; read in a value for the line_id 
						    ; transition name
trans = result.trans
if result.cancel then return
; for loop: loops around array called 'output' (passed in by displayparms_katrina_cont) 
;   and reads each entry onto a separate line
for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  chisq = oc->getchisq()
  if chisq ne (-1.0) then begin
    ncurves = oc->count()
    if ncurves ne 0 then begin
      spax_number = i + fix((*self.yvalsPtr)[1]) + 1
      ; call displayparms_katrina procedure and pass the trans varaible and the group no
      oc->displayparms_katrina_cont,output = output,trans = trans,spax_id = spax_number
      nlines = n_elements(output)
      for j=0,nlines-1 do begin   ; print out every entry of the output array onto a new line
        printf,lun,(output[j])
	print,output[j]
      endfor
    endif
  endif
endfor
free_lun,lun
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::readParmsAsText,event = event
if n_elements(*self.dataPtr) eq 0 then return
thisPath = self.workDir

filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                                              title = 'Enter parameter text file name',$
                                              /read,filter = '*.fit',$
                                              path = thisPath)

if filename eq '' or filename eq ' ' then return
opanReadFitParms,filename,fitArray = fitArray,groups = groups, error = error

if error eq 1 then return
ngrps = n_elements(fitArray)

; How many groups are there in the current data set?
dat = *self.dataPtr
dsize = size(dat)
nx = dsize[1]
ny = dsize[2]
if ny lt ngrps then begin
  ; Clean up the pointers
  for i = 0,ngrps-1 do begin
    ptr_free,(fitArray[i]).namePtr
    ptr_free,(fitArray[i]).parmPtr
    ptr_free,(fitArray[i]).nparmPtr
    ptr_free,(fitArray[i]).exprPtr
  endfor
  return
endif


; Ok, now let's start building the fit objects
for i = 0,ngrps-1 do begin
  thisGroup = groups[i] - 1
  oc = (*self.ocurveGroup)[thisGroup]
  ; First, if necessary, free up any existing curves in the current group
  ncurves = oc->count()
  if ncurves ne 0 then begin
	oall = oc->get(/all)
	obj_destroy,oall
	oc->remove,/all
  endif

  names = *(fitArray[i]).namePtr
  parms = *(fitArray[i]).parmPtr
  nparms = *(fitArray[i]).nparmPtr
  exprArray = *(fitArray[i]).exprPtr
  ncurves = n_elements(names)
  parmcount = 0

  dat = (*self.dataPtr)[*,thisGroup]
  daterr = (*self.errorPtr)[*,thisGroup]
  x = (*self.xvalsPtr)[*,thisGroup]
  y = (*self.yvalsPtr)[0,thisGroup]

  for j = 0,ncurves-1 do begin
    ; curParms contains the parameters for the current curve
    curParms = parms[parmCount:parmCount+nparms[j]-1]
    parmCount = parmCount + nparms[j]
	name = strlowcase(names[j])
	expr = exprArray[j]
    if strupcase(name) ne 'PAN_USERFUNCTION' then begin
	    *self.ocurrentPtr = obj_new("func",name = name,xvalues = x)
    endif else begin	; Ok we have a USER-DEFINED FUNCTION

      parmStr = getparmsfromexpr(expr)
      ;numparms = n_elements(parmStr)
      test = pan_userfunction(x,curparms,expr = expr,eval = eval)
      if eval ne 1 then return
      *self.ocurrentPtr = obj_new("func",name = name, $
                          xvalues = x, $
                          expr = expr, $
                          parms = curparms)

    endelse
    oc -> add,*self.ocurrentPtr
    parmInfo = self->packageParmInfo()

  endfor
  oc->setparms,parms
endfor

wset,self.resPix
self->plotResiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

wset,self.datPix
self->displayData
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

; Clean up the pointers
for j = 0,ngrps-1 do begin
  ptr_free,(fitArray[j]).namePtr
  ptr_free,(fitArray[j]).parmPtr
  ptr_free,(fitArray[j]).nparmPtr
  ptr_free,(fitArray[j]).exprPtr
endfor

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::print2text,event = event
if n_elements(*self.dataPtr) eq 0 then return
thisPath = self.workDir
filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                                              title = 'Enter output text file name',$
                                              /read,filter = '*.txt',$
                                              path = thisPath)
filename = filename + '.txt'

fitObject = *self.ocurveGroup
ngroups = n_elements(fitObject)

openw,lun,filename,/get_lun

for i = 0,ngroups-1 do begin
  oc = fitObject[i]
  oall = oc->get(/all)
  ncurves = oc->count()

  dat = reform((*self.dataPtr)[*,i])
  daterr = reform((*self.errorPtr)[*,i])
  x = reform((*self.xvalsPtr)[*,i])
  nx = n_elements(x)

  strout1 = '# Group '+string(i+1)
  strout2 = '# x y yerr'
  str = ['#','x','yerr']
  if ncurves gt 0 then begin
    oc->evaluate,x,yout = yfit
    txt = replicate('  component: ',ncurves)
    txt = txt + strtrim(string(indgen(ncurves)+1),2)
    strout2 = strout2 + ' total fit '
    str = [str,'total fit']
    ycomp = fltarr(nx,ncurves)
    for j = 0,ncurves-1 do begin
      oall[j]->setproperty,xvalues = x,/calculate
      oall[j]->getproperty,yvalues = yvalues
      ycomp[*,j] = yvalues
      if j eq 0 then begin
        out = txt[j]
      endif else begin
		out = out + txt[j]
      endelse
      str = [str,txt[j]]
    endfor
    strout2 = strout2 + out
    printf,lun,strout1
    outFormat = '(A1,'+strtrim(string(3+ncurves),2)+'A20)'
    printf,lun,strout2;,format = outFormat

    thisFormat = '('+strtrim(string(4+ncurves),2)+'f15.5)'
    dataOut = fltarr(4+ncurves)
    for j = 0,nx-1 do begin
      dataOut[0:4+ncurves-1] = [x[j],dat[j],daterr[j],yfit[j],transpose(ycomp[j,0:ncurves-1])]
      printf,lun,dataOut[0:4+ncurves-1],format = thisFormat
    endfor

  endif else begin
    printf,lun,strout1
    outFormat = '(A1,'+strtrim(string(2),2)+'A20)'
    printf,lun,strout2,format = outFormat
    thisFormat = '('+strtrim(string(3),2)+'f15.5)'
    dataOut = fltarr(3)
    for j = 0,nx-1 do begin
      dataOut[0:2] = [x[j],dat[j],daterr[j]]
      printf,lun,dataOut[0:2],format = thisFormat
    endfor
  endelse
printf,lun,''
endfor

free_lun,lun
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan::packageParmInfo
; Get parameter information
widget_control,self.groupSlider,get_value = val
val = fix(val[0]) - 1
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then val = 0
oc = (*self.ocurveGroup)[val]
ncurves = oc->count()
if ncurves eq 0 then return,(-1L)
names = oc->getnames()
ncurves = n_elements(names)
step = oc->getstep()
fixed = oc->getfixed()
lovalues = oc->getlowvalues()
hivalues = oc->gethighvalues()
parms = oc->getparms()
parmnames = oc->getparmnames()
high = oc->gethigh()
low = oc->getlow()
tied = oc->gettied()
expr = oc->getExpr()

oall = oc->get(/all)
for i = 0,ncurves-1 do begin
 	oall[i]->getproperty,parmnames = parmnames
 	np = n_elements(parmnames)
 	if i eq 0 then begin
   		pn = parmnames
   		nparms = np
 	endif else begin
   		nparms = [nparms,np]
   		pn = [pn,parmnames]
 	endelse
endfor
parmInfo = {  $
			  nparms:nparms,names:names, step:step, fixed:fixed, $
    	      lovalues:lovalues, hivalues:hivalues, high:high, $
        	  low:low, parms:parms, parmnames:pn,tied:tied,expr:expr  $
        	  }
return,parmInfo
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan::updateParmInfo,newParmInfo
widget_control,self.groupSlider,get_value = val
val = fix(val[0]) - 1
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then val = 0
oc = (*self.ocurveGroup)[val]
oc->setparms,newParmInfo.parms
oc->setlow,newParmInfo.low
oc->sethigh,newParmInfo.high
oc->setlowvalues,newParmInfo.lovalues
oc->sethighvalues,newParmInfo.hivalues
oc->setfixed,newParmInfo.fixed
oc->settied,newParmInfo.tied
return,1
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::parameterInfo, event = event
if n_elements(*self.dataPtr) eq 0 then return
; Is there a curve present here?
widget_control,self.groupSlider,get_value = val
val = fix(val[0]) - 1
datSize = size(*self.dataPtr)
if datSize[0] eq 1 then val = 0
oc = (*self.ocurveGroup)[val]
ncurves = oc->count()
if ncurves eq 0 then return
; Get parameter information

thisEvent = tag_names(event,/structure_name)
case thisEvent of
'': $
  begin
    widget_control,event.top,sensitive = 0
    parmInfo = self->packageParmInfo()
	widget_control,self.modify,sensitive = 0
    enterparminfo,parmInfo, $
                  group_leader = event.top, $
                  notifyIds = [event.id,event.top]
  end
'WIDGET_BUTTON': $
  begin
    widget_control,event.top,sensitive = 0
	parmInfo = self->packageParmInfo()
	widget_control,self.modify,sensitive = 0
    enterparminfo,parmInfo, $
                  group_leader = event.top, $
                  notifyIds = [event.id,event.top]
  end
'PARMSEVENT': $
  begin
	  dummy = self->updateParmInfo(*event.parmInfo)

	  wset,self.datPix
	  self->displayData
	  wset,self.datVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

	  wset,self.resPix
	  self->plotResiduals
	  wset,self.resVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

  end
'PARMSQUITEVENT': $
  begin
	widget_control,self.modify,sensitive = 1
	widget_control,event.top,sensitive = 1
	; Empty placeholder.  We don't want to update any of the
	; parameter information.
  end
else:
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::showFileInfo,event = event
thisEvent = tag_names(event,/structure_name)
if n_elements(*self.headerPtr) eq 0 then begin
  output = ['No data currently loaded']
  void = dialog_message(dialog_parent = event.top,output)
  return
endif
case thisEvent of
'WIDGET_BUTTON': $
  begin
    opan_FileInfo,*self.headerPtr,group_leader = event.top, $
              notifyIds = [event.id,event.top]

  end
'FILEINFOEVENT': $	; This case not really necessary
  begin
  end
else:
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::preferences, event = event
thisEvent = tag_names(event,/structure_name)
case thisEvent of
'WIDGET_BUTTON': $
  begin
    enterPref,group_leader = event.top, $
              notifyIds = [event.id,event.top], $
              prefs = *self.prefs

  end
'PREFSEVENT': $
  begin
    *self.prefs = event.prefs
    self.xtitle = (*self.prefs).xlabel
    self.ztitle = (*self.prefs).ylabel
	  wset,self.resPix
	  self->plotResiduals
	  wset,self.resVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

	  wset,self.datPix
	  self->displayData
	  wset,self.datVis
	  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
  end
else:
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function html_header,title = title,username = username
if n_elements(title) eq 0 then title = 'PAN Log Output'
if n_elements(username) eq 0 then username = 'PAN User'

strout = ['<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"']
strout = [strout,'<html>']
strout = [strout,'<body>']
strout = [strout,'<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">']
strout = [strout,'<meta name="author" content="PAN">']
strout = [strout,'<title>'+title+'</title>']
strout = [strout,'</head>']
strout = [strout,'<body>']
strout = [strout,'<big><big><b>PAN Data Analysis Log</b></big></big><br>']
strout = [strout,'<br>']

strout = [strout,'User name:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'+username+'<br>']
time = systime()
strout = [strout,'Creation date:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'+time+'<br>']
strout = [strout,'<br>']

strout = [strout,'']
return,strout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function html_comments,comments = comments,str_in
if n_elements(comments) eq 0 then return,''
strout = [str_in,'<p>',comments,'<p>']
return,strout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function html_finish,str_in
strout = [str_in,'</body>','</html>']
return,strout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function html_open,filename = filename
if n_elements(filename) eq 0 then return,''
openr,lun,filename,/get_lun
dummy = ''
count = 0
while not(eof(lun)) do begin
  readf,lun,dummy
  if count eq 0 then strout = dummy else strout = [strout,dummy]
  count = count + 1L
endwhile
free_lun,lun
return,strout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function html_write,filename = filename,strout = strout
nstr = n_elements(strout)
openw,lun,filename,/get_lun
for i = 0,nstr-1 do begin
  printf,lun,strout[i]
endfor
free_lun,lun
return,0
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::logComment,event = event
if n_elements(*self.logStringPtr) eq 0 then return
strout = *self.logStringPtr
comments = opan_getcomments(group_leader = event.top,cancelled = cancel)
if cancel eq 1 then return

nc = n_elements(comments)

end_of_html = where(strout eq '</body>')
index = end_of_html[0] - 1
nstr = n_elements(strout)
strout1 = [strout[0:index],'<p>',comments[0:nc-1],'<p>',strout[index:nstr-1]]
*self.logStringPtr = strout1
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::logAddPlot,event = event
if n_elements(*self.dataPtr) eq 0 then return
if n_elements(*self.logStringPtr) eq 0 then return
self.jpeg = 1
thisDevice = !d.name
h = 11.0 & w = 8.0 & aspect1 = h/w
xsize = 600 & ysize = fix(aspect1*xsize)
window,/free,/pixmap,xsize = xsize,ysize = ysize
winPix = !d.window
wset,winPix
self->niceOutput,event = event

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
thisPath = self.workDir
; Create the filename
counter = 0
res = 1
while res eq 1 do begin
  filename = 'panplot_'+strtrim(string(counter),2)+'.jpg'
  testFile = filepath(filename, ROOT_DIR=self.logDirectory)
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
strout = *self.logStringPtr
end_of_html = where(strout eq '</body>')
index = end_of_html[0] - 1
nstr = n_elements(strout)

read_jpeg,testFile,img
imgSize = size(img)
sx = imgSize[2] & sy = imgSize[3]
aspect2 = 1.0*sy/(1.0*sx)
outSx = 500 & outSy = fix(1.0*outSx*aspect2)
imgLine = '<img src="'+imgFile+'" width="'+ $
          strtrim(string(outSx),2)+ $
          '" height="'+strtrim(string(outSy),2)+'"><br>'

strout1 = [strout[0:index],'<p>',imgLine,strout[index:nstr-1],'<p>']
*self.logStringPtr = strout1
self.jpeg = 0

wset,self.resPix
self->plotResiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

wset,self.datPix
self->displayData
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::newHTMLFile,event = event
; This method asks the user to provide a new directory
; Change directories to the user's working directory
cd,current = currentDir
cd,self.workDir
; Get the user to enter a directory name
result = opan_getdirectory(group_leader = event.top,cancelled = cancel)
if cancel then return
thisDir = result[0]
user = result[1]
title = result[2]
if thisDir eq '' or thisDir eq ' ' then return
; Create the directory
file_mkdir,thisDir
cd,currentDir
;self.logDirectory = thisDir
self.logDirectory = filepath('',root_dir = self.workDir,subdir = [thisDir])
; Create the log file name with a fully-qualified path
*self.logStringPtr = html_header(title = title,username = user)
*self.logStringPtr = html_finish(*self.logStringPtr)
self.logFile =filepath('index.html',ROOT_DIR=self.workDir,subdir=[thisDir])
widget_control,self.logFileDisplay,set_value = self.logFile
widget_control,self.logFileStatus,set_value = 'open'

; Sensitize or desensitize the appropriate buttons
widget_control,self.newHTML,sensitive = 0
widget_control,self.existingHTML,sensitive = 0
widget_control,self.closeHTML,sensitive = 1
widget_control,self.plotHTML,sensitive = 1
widget_control,self.commentHTML,sensitive = 1

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::existingHTMLFile,event = event
; This method reads in an existing log file and allows the user to append
; more information to it.
filename = dialog_pickfile(dialog_parent = event.top,file = 'index.html', $
           title = 'Select the HTML log file',filter = '*.html', $
           path = self.workDir,get_path = logDirectory)
self.logDirectory = logDirectory
if filename eq '' or filename eq ' ' or not(file_test(filename)) then begin
  strout = 'Not a valid file selection'
  void = dialog_message(dialog_parent = event.top,strout)
  return
endif
self.logFile = filename
*self.logStringPtr = html_open(filename = filename)
widget_control,self.logFileDisplay,set_value = self.logFile
widget_control,self.logFileStatus,set_value = 'open'

; Sensitize or desensitize the appropriate buttons
widget_control,self.newHTML,sensitive = 0
widget_control,self.existingHTML,sensitive = 0
widget_control,self.closeHTML,sensitive = 1
widget_control,self.plotHTML,sensitive = 1
widget_control,self.commentHTML,sensitive = 1

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::closeHTMLFile,event = event
if n_elements(*self.logStringPtr) eq 0 then return
; Write the html file
dummy = html_write(filename = self.logFile,strout = *self.logStringPtr)
ptr_free,self.logStringPtr
self.logStringPtr = ptr_new(/allocate_heap)
widget_control,self.logFileDisplay,set_value = ''
widget_control,self.logFileStatus,set_value = 'closed'

; Sensitize or desensitize the appropriate buttons
widget_control,self.newHTML,sensitive = 1
widget_control,self.existingHTML,sensitive = 1
widget_control,self.closeHTML,sensitive = 0
widget_control,self.plotHTML,sensitive = 0
widget_control,self.commentHTML,sensitive = 0
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::view_corr_matrix,event = event
thisEvent = tag_names(event,/structure_name)
case thisEvent of
'WIDGET_BUTTON':	$
	begin
; Which detector/fit is currently selected?
		widget_control,self.groupSlider,get_value = val
		val = fix(val[0])-1
		if n_elements(*self.dataPtr) eq 0 then return
		datSize = size(*self.dataPtr)
		if datSize[0] eq 1 then val = 0
		oc = (*self.ocurveGroup)[val]
		oc->displayparms,output = output
		ncurves = oc->count()
		if ncurves gt 0 then begin
			widget_control,self.curveSlider,set_slider_max = (ncurves > 2)
		endif
		if n_elements(output) eq 0 then begin
		  output = 'No curves present'
		endif
		oc->get_pcor,pcor
		if n_elements(pcor) eq 1 then return
		parmnames = oc->getparmnames()
		nparms = n_elements(parmnames)
		new_parmnames = strarr(nparms)
		for i = 0,nparms-1 do begin
			new_parmnames[i] = '#'+strtrim(string(i),2)+': '+parmnames[i]
		endfor
		bad_index = where(finite(pcor) ne 1, count_bad)
		if count_bad gt 0 then pcor[bad_index] = -1.e6

		rains_display_correlation,	pcor,						$
									new_parmnames,				$
									group_leader = event.top,	$
									notify_ids = [event.id,event.top]
	end
else:
endcase
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::mc_estimate,event = event
widget_control,self.ndata_field,get_value = val
self.ndata = fix(val) > 2
; Get the current data set
widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
if n_elements(*self.ocurvegroup) eq 0 then return
oc = (*self.ocurveGroup)[val]
parms = oc->getparms()
parinfo = oc->createparinfo()

if oc->count() eq 0 then return
datSize = size(*self.dataPtr)
if datSize[0] eq 2 then begin
  y = (*self.dataPtr)[*,val]
  yerr = (*self.errorPtr)[*,val]
  x = (*self.xvalsPtr)[*,val]
endif else begin
  y = reform(*self.dataPtr)
  yerr = reform(*self.errorPtr)
  x = reform(*self.xvalsPtr)
endelse
xptr = ptr_new(x)
; Get the current fitting range for the data
limits = where((x ge (*self.prefs).xfitlo) and (x le (*self.prefs).xfithi))
xlim = x[limits]
ylim = y[limits]
dylim = yerr[limits]
nx = n_elements(xlim)

msg = 'Estimating errors using the "bootstrap" Monte-Carlo method'
title = 'Progress'
progressBar = Obj_New("SHOWPROGRESS",message = msg,title = title, $
   event.top,/cancelbutton)
progressBar->Start
dy = dylim
w = 1./dy^2

iterargs = {stopBut:self.interrupt}
iterproc = 'pan_iterproc'
widget_control,self.fitstatfield,set_value = self.fitstatus[1]
widget_control,self.interrupt,sensitive = 1
widget_control,self.interrupt,/clear_events
np = n_elements(parms)
mc_parms = fltarr(np,self.ndata)

for i = 0L,self.ndata-1 do begin
   r_normal = randomn(s,nx)
   derr = dylim*r_normal
   y = ylim+derr
   yf = mpcurvefit(xlim,y,w,parms,sigma,                                $
         function_name = 'fitfunction', status = status,                $
         parinfo = parinfo, itmax = (*self.prefs).maxiter,              $
         /quiet,chisq = chisq,iterargs = iterargs,iterproc = iterproc,  $
         functargs = {oc:oc,xptr:xptr}, covar = covar,/autoderivative,  $
         errmsg = errmsg                                                )
   widget_control,self.interrupt,sensitive = 0
   widget_control,self.fitstatfield,set_value = self.fitstatus[0]
   mc_parms[*,i] = parms
   progressBar->update,(i+1)*100/self.ndata
   cancelled = progressBar->CheckCancel()
   if cancelled then begin
      progressBar->Destroy
      i = self.ndata-1
   endif
endfor

; Do the statistics on the parameters
presult = fltarr(np)
dpresult = fltarr(np)

for i = 0,np-1 do begin
   p = mc_parms[i,*]
   pstats = moment(p)
   presult[i] = pstats[0]
   dpresult[i] = sqrt(pstats[1])
endfor

; Put these results into the parameter
oc->setparmerror,dpresult

widget_control,self.interrupt,/clear_events
if status le 0 then begin
   self.status = 0
   strout = 'Fit cancelled'
   widget_control,self.fitstatfield,set_value = strout
endif else begin
   self.status = 1
endelse
ptr_free,xptr

progressBar->Destroy
obj_destroy, progressBar

oc->setparms,parms
oc->evaluate,x,yout = yfit
; Calculate the residuals
yfit = yfit[limits]
dof = n_elements(yfit) - np
chi_sq = (total(((yfit-ylim)/dy)^2))/dof
oc->setchisq,chi_sq

wset,self.resPix
self->plotresiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

wset,self.datPix
self->displaydata
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

; Display the parameters
self->displayFitParameters

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::createWidgets
; Widget creation module
if self.group_leader eq (-1L) then begin
  tlb = widget_base(/row, $
        title = 'PAN: Peak Analysis',mbar = bar,/tlb_size_events)
endif else begin
  tlb = widget_base(group_leader = self.group_leader,/row, $
        title = 'PAN: Peak Analysis',mbar = bar,/tlb_size_events)
endelse
self.tlb = tlb
filemenu = widget_button(bar,value = 'File',/menu)
printmenu = widget_button(bar,value = 'Print',/menu)
miscmenu = widget_button(bar,value = 'Miscellaneous',/menu)
void = widget_button(filemenu,value = 'Load 3-col ascii data', $
       uvalue = {object:self,method:'loadasciidata'})
void = widget_button(filemenu,value = 'Load grouped ascii data', $
       uvalue = {object:self,method:'loadgroupdata'})
void = widget_button(filemenu,value = 'Load FITS data', $
       uvalue = {object:self,method:'loadfitsdata'})
void = widget_button(filemenu,value = 'Load test data', $
       uvalue = {object:self,method:'loadtestdata'})
void = widget_button(filemenu,value = 'Save fit params',$
       uvalue = {object:self,method:'saveParmsAsText_katrina'})
void = widget_button(filemenu,value = 'Save continuum fit params',$
       uvalue = {object:self,method:'saveParmsAsText_katrina_cont'})
;void = widget_button(filemenu,value = 'Save fit parameters as text',$
;       uvalue = {object:self,method:'saveParmsAsText'})
void = widget_button(filemenu,value = 'Read in fit parameters',$
       uvalue = {object:self,method:'readParmsAsText'})
void = widget_button(filemenu,value = 'Display current file information',$
       uvalue = {object:self,method:'showFileInfo'})
void = widget_button(filemenu,value = 'Preferences',$
       uvalue = {object:self,method:'preferences'})
void = widget_button(filemenu,value = 'Quit',/separator,$
       uvalue = {object:self,method:'quit'})
void = widget_button(miscmenu,value = 'About PAN',$
       uvalue = {object:self,method:'aboutPAN'})

void = widget_button(printmenu,value = 'Print screen to .JPEG file', $
       uvalue = {object:self,method:'print2jpeg'})
void = widget_button(printmenu,value = 'Print to PS file', $
       uvalue = {object:self,method:'print2ps'})
void = widget_button(printmenu,value = 'Print data and fits to text file', $
       uvalue = {object:self,method:'print2text'})

ctrlBase = widget_base(tlb,/col)
self.ctrlBase = ctrlBase

curveNames = ['Select function',$
              'gaussian',$
  			  'lorentzian',$
  			  'lognormal', $
  			  'DHO', $
  			  'step', $
              'background', $
              'userfunction']

ncurves = n_elements(curveNames)
*self.curvenamesPtr = curveNames

desc = replicate({ flags:0, name:'' }, ncurves)
desc.flags = [1,0*indgen(ncurves-1)]
desc.name = curveNames

drop = cw_pdmenu(ctrlbase,desc,/return_index, $
       uvalue = {object:self,method:'selectCurve'})
void = widget_label(ctrlBase,value = '')
groupSlider = widget_slider(ctrlbase,value = 1,min = 1,max = 100, $
              title = 'Group selection', $
              uvalue = {object:self,method:'selectGroup'})

self.modify = widget_button(ctrlbase,value = 'Modify fit parameters', $
              uvalue = {object:self,method:'parameterInfo'})

butBase = widget_base(ctrlbase,/col,/frame)
void = widget_button(butbase,value = 'Clear current curves', $
       uvalue = {object:self,method:'clearCurrentCurves'})
void = widget_button(butbase,value = 'Clear all curves', $
       uvalue = {object:self,method:'clearAllCurves'})
void = widget_button(butbase,value = 'Remove selected curve', $
       uvalue = {object:self,method:'remSelCurve'})
self.curveSlider = widget_slider(self.ctrlBase,value = 1, $
                   min = 1,max = 2,title = 'Curve selection', $
                   uvalue = {object:self,method:'sliderCurveSel'})

void = widget_label(ctrlBase,value = '')

void = widget_button(ctrlbase,value = 'Fit current group', $
;       uvalue = {object:self,method:'startFitCurrent'})
       uvalue = {object:self,method:'fitOneGroup'})

void = widget_button(ctrlbase,value = 'Fit all groups', $
;       uvalue = {object:self,method:'startFitGroups'})
       uvalue = {object:self,method:'fitAllGroups'})
self.interrupt = widget_button(ctrlbase,value = 'Interrupt fit',sensitive = 0);, $
;            uvalue = {object:self,method:'interruptFitLoop'})

self.groupSlider = groupSlider
self.groupField = cw_field(ctrlbase,/col,value = 1,/string, $
                  title = 'Groups to fit')
self.fitStatField = cw_field(ctrlbase,/col,value = self.fitStatus[0],$
                    title = 'Fitting status',/string,/noedit)

;MSW - removed these buttons
;void = widget_button(ctrlbase,value = 'View CORR matrix', $
;       uvalue = {object:self,method:'view_corr_matrix'})

;void = widget_button(ctrlbase,value = 'Display fit parameters', $
;       uvalue = {object:self,method:'displayFitParameters'})

;void = widget_button(ctrlbase,value = 'Plot fit parameter', $
;       uvalue = {object:self,method:'plotFitParameter'})
;void = widget_button(ctrlbase,value = 'Plot EISF', $
;       uvalue = {object:self,method:'plotEISF'})


dispBase = widget_base(tlb,/col)
self.dispBase = dispBase

;MSW added screen-dependent sizing of plot area
device,get_screen_size = sz
sx = sz[0] & sy = sz[1]
datxsize = fix(0.4*sx) & datysize = fix(0.42*sy)
resxsize = fix(0.4*sx) & resysize = fix(0.18*sy)
;datxsize = 750 & datysize = 500
;resxsize = 750 & resysize = 250
self.winratio = float(datysize/resysize)
self.datwin = widget_draw(dispBase,xsize = datxsize,ysize = datysize,$
              /button_events,uvalue = {object:self,method:'drawEvents'})
self.reswin = widget_draw(dispBase,xsize = resxsize,ysize = resysize)

self.infoBase = widget_base(tlb,/col)
if !d.name eq 'WIN' then begin
  thisFont = "Comic Sans MS*16"
  self.info = widget_text(self.infoBase,font = thisFont, value = '', $
              xsize = 40,ysize = 20,/scroll)
endif else begin
  self.info = widget_text(self.infoBase,value = '', $
              xsize = 40,ysize = 20,/scroll)
endelse

; Now create a base for the HTML log controls
logBase = widget_base(self.infoBase,/frame,/col)
;MSW - moved these buttons over from left side
void = widget_button(logbase,value = 'Rebin data', $
       uvalue = {object:self,method:'rebinData'})
void = widget_button(logbase,value = 'Restore original data', $
       uvalue = {object:self,method:'rebinRestore'})

mc_base = widget_base(logbase,/frame,/col)
self.ndata_field = cw_field(mc_base,/col,value = strtrim(string(self.ndata),2), $
   title = '# of MC data sets')
if !version.release gt 6.0 then begin
   tooltip = 'Perform Bootstrap Monte-Carlo Error Estimate'
   void = widget_button(mc_base,value = 'Monte-Carlo error estimate', $
          uvalue = {object:self,method:'mc_estimate'},tooltip = tooltip)
endif else begin
   void = widget_button(mc_base,value = 'Monte-Carlo error estimate', $
          uvalue = {object:self,method:'mc_estimate'})
endelse

;MSW - removed all the HTML buttons which are pretty useless
;void = widget_label(logBase,value = 'HTML Log Controls')
;self.logBase = logBase
;self.newHTML = widget_button(logBase,value = 'Create new HTML log file', $
;       uvalue = {object:self,method:'newHTMLFile'})
;self.existingHTML = widget_button(logBase,value = 'Open existing HTML log file', $
;       uvalue = {object:self,method:'existingHTMLFile'})
;self.commentHTML = widget_button(logBase,value = 'Enter comment', $
;       uvalue = {object:self,method:'logComment'},sensitive = 0)
;self.plotHTML = widget_button(logBase,value = 'Add plot to log', $
;       uvalue = {object:self,method:'logAddPlot'},sensitive = 0)
;self.closeHTML = widget_button(logBase,value = 'Close HTML log file', $
;       uvalue = {object:self,method:'closeHTMLFile'},sensitive = 0)
;
;void = widget_label(logBase,value = 'Log file status:')
;self.logFileStatus = widget_text(logBase,value = 'closed')
;void = widget_label(logBase,value = 'Log file name')
;self.logFileDisplay = widget_text(logBase,value = self.logFile)

; Center the interface in the screen
geom = widget_info(self.tlb,/geometry)
device,get_screen_size = sz
sx = sz[0] & sy = sz[1]
xoff = fix(0.5*(sx-geom.xsize))
yoff = fix(0.25*(sy-geom.ysize)) ;MSW - changed from 0.5
widget_control,self.tlb,xoffset = xoff,yoffset = yoff
;widget_control,logBase,xoffset = xoff+fix(0.75*geom.xsize), $
;               yoffset = yoff+fix(0.7*geom.ysize)
widget_control,tlb,/realize
widget_control,logBase,/realize

widget_control,self.datwin,get_value = datVis
self.datVis = datVis
widget_control,self.reswin,get_value = resVis
self.resVis = resVis

window,/free,/pixmap,xsize = datxsize,ysize = datysize
self.datPix = !d.window
window,/free,/pixmap,xsize = resxsize,ysize = resysize
self.resPix = !d.window

widget_control,tlb,set_uvalue = self
; Note that we have to call xmanager twice here so that we can register
; both the top-level-base and the HTML LOG controls base with the same
; event handler.
xmanager,'opan::createWidgets',self.tlb,event_handler = 'opanEvents',$
         cleanup = 'opanCleanup', /no_block
;xmanager,'opan::createWidgets',logBase,event_handler = 'opanEvents',$
;         cleanup = 'opanCleanup', /no_block
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::niceOutput,event = event
deltay = 0.20
yreso = 0.25 & yresf = 0.40
ydato = 0.45 & ydatf = 0.75
pres = [0.15,yreso+deltay,0.9,yresf+deltay]
pdat = [0.15,ydato+deltay,0.9,ydatf+deltay]

widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
ncurves = (*self.ocurveGroup)[val]->count()

self->displaydata,position = pdat
if ncurves gt 0 then $
	self->plotresiduals,position = pres,/noerase

; Now annotate with the fit results
widget_control,self.groupSlider,get_value = val
val = fix(val[0])-1
oc = (*self.ocurveGroup)[val]
oc->displayparms,output = output
chsize = 0.75
nlines = n_elements(output)
if nlines eq 0 then return
if nlines lt 30 then begin
  ystart = 0.05 & yend = ydato-ystart
  dy = yend-ystart
  step = dy/30.0
  xstart = 0.05
  ypos = ystart+step*findgen(nlines)
  for i = 0,nlines-1 do begin
    xyouts,xstart,yend-ypos[i],output[i],/normal,charsize = chsize
  endfor
endif
if (nlines gt 29) and (nlines lt 59) then begin
  ; Print the first 30 lines
  ystart = 0.05 & yend = ydato-ystart
  dy = yend-ystart
  step = dy/30.0
  xstart = 0.05
  ypos = ystart+step*findgen(30)
  for i = 0,29 do begin
    xyouts,xstart,yend-ypos[i],output[i],/normal,charsize = chsize
  endfor
  ; Print the remainder
  ypos = ystart+step*findgen(30)
  ystart = 0.05 & yend = ydato-ystart
  xstart = 0.50
  ypos = ystart+step*findgen(30)
  count = 0
  for i = 30,nlines-1 do begin
    xyouts,xstart,yend-ypos[count],output[i],/normal,charsize = chsize
    count = count + 1
  endfor
endif
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::print2ps,event = event
if n_elements(*self.dataPtr) eq 0 then return
catch,theError
if theError ne 0 then begin
  catch,/cancel
  void = dialog_message(dialog_parent = event.top, $
         !error_state.msg+' Returning...')
  return
endif

myFileName = self.printFilename
ppos = strpos(myFileName,'.')
filename = strmid(myFileName,0,ppos)

myFile = filename+'.ps'
keywords = PSConfig(Cancel=cancelled,group_leader = event.top,$
           filename = myFile,color = 0,xsize = 8.0,ysize = 11.0, $
           directory = self.workDir,xoff = 0.0,yoff = 0.0)
IF cancelled THEN RETURN
thisDevice = !D.Name
Set_Plot, 'PS'
Device, _Extra=keywords

self->niceOutput,event = event

Device, /Close_File
Set_Plot, thisDevice
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan::print2jpeg,event = event
if n_elements(*self.dataPtr) eq 0 then return
self.jpeg = 1
thisDevice = !d.name
h = 11.0 & w = 8.0 & aspect = h/w
xsize = 600 & ysize = fix(aspect*xsize)
window,/free,/pixmap,xsize = xsize,ysize = ysize
winPix = !d.window
wset,winPix
self->niceOutput,event = event

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
thisPath = self.workDir

myFileName = self.printFilename
ppos = strpos(myFileName,'.')
filename = strmid(myFileName,0,ppos)
myFile = filename
filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                           title = 'Enter output jpeg file name',$
                           /write,filter = '*.jpg',$
                           path = self.workdir, $
                           file = myFile)
if (filename eq '') or (filename eq ' ') then return
filename = filename + '.jpg'
s = Size(image24, /Dimensions)
newx = Round(300.0 * s[1] / 72)
newy = Round(300.0 * s[2] / 72)
highResImage = Congrid(image24, 3, newx, newy, /Interp)
write_jpeg,filename,255-highResImage,true = 1,quality = 100
Set_Plot, thisDevice
self.jpeg = 0

wset,self.resPix
self->plotResiduals
wset,self.resVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.resPix]

wset,self.datPix
self->displayData
wset,self.datVis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan::getNotifyIds
return,*self.notifyIdPtr
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function opan::init,	group_leader = group_leader, $
						notifyId = notifyId, $
						data = data, $
						error = error, $
						xvalues = xvalues, $
						yvalues = yvalues, $
						xtitle = xtitle, $
						ytitle = ytitle, $
						ztitle = ztitle, $
						header = header, $
						title = title, $
						workDir = workDir, $
						dataDir = dataDir
tvlct,r,g,b,/get
self.rPtr = ptr_new(r)
self.gPtr = ptr_new(g)
self.bPtr = ptr_new(b)
loadct,0,/silent

red=  [0,1,0,0,0,1,1,1]*255
green=[0,0,1,0,1,0,1,1]*255
blue= [0,0,0,1,1,1,0,1]*255
tvlct,red,green,blue

self.ndata = 25
if n_elements(group_leader) eq 0 then group_leader = (-1L)
cd,current = curDir
if n_elements(workDir) eq 0 then self.workDir = curDir else self.workDir = workDir
if n_elements(dataDir) eq 0 then self.dataDir = '' else self.dataDir = dataDir

self.notifyIdPtr = ptr_new(/allocate_heap)
self.headerPtr = ptr_new(/allocate_heap)
self.dataPtr = ptr_new(/allocate_heap)
self.errorPtr = ptr_new(/allocate_heap)
self.xvalsPtr = ptr_new(/allocate_heap)
self.yvalsPtr = ptr_new(/allocate_heap)
self.pcor_ptr = ptr_new(/allocate_heap)

self.odataPtr = ptr_new(/allocate_heap)
self.oerrorPtr = ptr_new(/allocate_heap)
self.oxvalsPtr = ptr_new(/allocate_heap)
self.oyvalsPtr = ptr_new(/allocate_heap)

self.datxPtr = ptr_new(/allocate_heap)
self.datyPtr = ptr_new(/allocate_heap)
self.goodParmPtr = ptr_new(/allocate_heap)
self.curvenamesPtr = ptr_new(/allocate_heap)
self.ocurveGroup = ptr_new(/allocate_heap)
self.grpArrayPtr = ptr_new(/allocate_heap)
self.status = 5
self.fitloop = 0
self.duration = 1.e-3
self.curIndex = 0
self.info = 0L
self.groupField = 0L
self.fitStatus = ['Resting', 'Fitting']
self.fitStatField = 0L

self.printFileName = ''
self.logStringPtr = ptr_new(/allocate_heap)
self.logFile = ''
self.logDirectory = ''
self.jpeg = 0

;MSW: 30/1/09 Changes labels
self.xtitle = '!6Wavelength (!6!sA!r!u!9 %!6!n!N)'
self.ytitle = 'Flux'
self.ztitle = 'z'

self.prefs = ptr_new({preferences, $
                      same:1, xenforce:0,yenforce:0, $
                      xmin:0.0,xmax:1.0,ymin:0.0,ymax:1.0, $
                      xfitlo:0.0,xfithi:1.0,initGuesses:0,tied:0, $
                      xlabel:self.xtitle,ylabel:self.ytitle, $
                      maxIter:200})
self.iteration = 0L
self.fitgroup = 0
self.addcurve = 0
self.firstPoint = [0,0]
self.secondPoint = [0,0]
self.ocurrentPtr = ptr_new(/allocate_heap)

self.autoscale = 1
self.xbox = [0.0,1.0]
self.xrange = [0.0,1.0]
self.ybox = [0.0,1.0]
self.yrange = [0.0,1.0]

self.title = 'Data'
self.filename = ''

if n_elements(notifyId) ne 0 then *self.notifyIdPtr = notifyId
self.group_leader = group_leader

self->createWidgets

if n_elements(data) ne 0 then begin
  *self.dataPtr = data
  if n_elements(error) ne 0 then *self.errorPtr = error
  if n_elements(header) ne 0 then *self.headerPtr = header
  if n_elements(xvalues) ne 0 then *self.xvalsPtr = xvalues
  if n_elements(yvalues) ne 0 then *self.yvalsPtr = yvalues
  if n_elements(ztitle) ne 0 then begin
    self.ztitle = ztitle
    (*self.prefs).ylabel = ztitle
  endif
  if n_elements(xtitle) ne 0 then begin
    self.xtitle = xtitle
    (*self.prefs).xlabel = xtitle
  endif
  if n_elements(ytitle) ne 0 then begin
    self.ytitle = ytitle

    (*self.prefs).ylabel = ytitle
  endif
  if n_elements(title) ne 0 then self.title = title
  self->initNewDataObjects
  wset,self.datPix
  self->displayData
  wset,self.datVis
  device,copy = [0,0,!d.x_size,!d.y_size,0,0,self.datPix]
endif

return,1
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opan__define

define = 	{opan, $
          ndata:0L,  $
          ndata_field:0L,  $
			 dataDir:'', $
			 workDir:'', $
			 printFileName:'', $
			 jpeg:0, $
			 pcor_ptr:ptr_new(),	$
			 notifyIdPtr:ptr_new(), $
			 headerPtr:ptr_new(), $
			 prefs:ptr_new(), $
			 fitgroup:0, $
			 grpArrayPtr:ptr_new(), $
			 curIndex:0, $
			 goodParmPtr:ptr_new(), $
			 iteration:0L, $
			 filename:'', $
			 xtitle:'', $
			 ytitle:'', $
			 ztitle:'', $
			 title:'', $
			 group_leader:0L, $
			 fitStatus:strarr(3), $
			 fitStatField:0L, $
			 modify:0L, $
			 groupField:0L, $
			 interrupt:0L, $
			 info:0L, $
			 curveSlider:0L, $
			 ocurveGroup:ptr_new(), $
			 ocurrentPtr:ptr_new(), $
			 addcurve:0, $
			 curvenamesPtr:ptr_new(), $
			 dataPtr:ptr_new(), $
			 errorPtr:ptr_new(), $
			 xvalsPtr:ptr_new(), $
			 yvalsPtr:ptr_new(), $

			 odataPtr:ptr_new(), $
			 oerrorPtr:ptr_new(), $
			 oxvalsPtr:ptr_new(), $
			 oyvalsPtr:ptr_new(), $

			 autoscale:0, $
			 mouse:0B, $
			 xrange:fltarr(2), $
			 yrange:fltarr(2), $
			 xbox:fltarr(2), $
			 ybox:fltarr(2), $
			 firstPoint:intarr(2), $
			 secondPoint:intarr(2), $

			 datWin:0L, $
			 datVis:0L, $
			 datPix:0L, $
			 datxPtr:ptr_new(), $
			 datyPtr:ptr_new(), $

			 newHTML:0L, $
			 existingHTML:0L, $
			 commentHTML:0L, $
			 plotHTML:0L, $
			 closeHTML:0L, $
			 logStringPtr:ptr_new(), $
			 logDirectory:'', $
			 logFile:'', $
			 logBase:0L, $
			 logFileStatus:0L, $
			 logFileDisplay:0L, $

			 resWin:0L, $
			 resVis:0L, $
			 resPix:0L, $

			 winratio:0., $
			 groupSlider:0L, $
			 tlb:0L, $
			 ctrlbase:0L, $
			 dispBase:0L, $
			 infoBase:0L, $
			 duration:0.0, $
			 status:0, $
			 fitloop:0, $
			 rPtr:ptr_new(), $
			 gPtr:ptr_new(), $
			 bPtr:ptr_new()}

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro pan
o = obj_new('opan')
end
