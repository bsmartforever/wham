; $Id: enterpref.pro,v 1.1 2002/09/19 21:30:20 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function packagePrefs,pState
widget_control,(*pState).prefGroup,get_value = val
widget_control,(*pState).xgroup,get_value = xval
widget_control,(*pState).ygroup,get_value = yval
widget_control,(*pState).xminField,get_value = xmin
xmin = float(xmin[0])
widget_control,(*pState).xmaxField,get_value = xmax
xmax = float(xmax[0])
widget_control,(*pState).yminField,get_value = ymin
ymin = float(ymin[0])
widget_control,(*pState).ymaxField,get_value = ymax
ymax = float(ymax[0])
widget_control,(*pState).xfitlowField,get_value = xfitlo
xfitlo = float(xfitlo[0])
widget_control,(*pState).xfithighField,get_value = xfithi
xfithi = float(xfithi[0])

widget_control,(*pState).xlabel,get_value = xlabel
xlabel = xlabel[0]
widget_control,(*pState).ylabel,get_value = ylabel
ylabel = ylabel[0]
widget_control,(*pState).maxIterations,get_value = maxIter
maxIter = fix(maxIter[0])

(*pState).prefs.xlabel = xlabel
(*pState).prefs.ylabel = ylabel
(*pState).prefs.same = val[0]
(*pState).prefs.initGuesses = val[1]
(*pState).prefs.xenforce = xval[0]
(*pState).prefs.yenforce = yval[0]
(*pState).prefs.xmin = xmin
(*pState).prefs.xmax = xmax
(*pState).prefs.ymin = ymin
(*pState).prefs.ymax = ymax
(*pState).prefs.xfitlo = xfitlo
(*pState).prefs.xfithi = xfithi
(*pState).prefs.maxIter = maxIter
return,1
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro enterPrefCleanup,tlb
widget_control,tlb,get_uvalue = pState
if ((*pState).notifyIds)[0] ne (-1L) then begin
  s = size((*pState).notifyIDs)
  if s[0] eq 1 then count = 0 else count = s[2]-1
  for j = 0,count do begin
    prefsInfo = {prefsEvent,$
                        ID:(*pState).notifyIDs[0,j],$
                        Top:(*pState).notifyIDs[1,j],$
                        Handler:0l,$
                        prefs:(*pState).prefs}
    if widget_info((*pState).notifyIDs[0,j],/valid_id) then begin $
      widget_control,(*pState).notifyIDs[0,j],send_event = prefsInfo
    endif
  endfor
endif
ptr_free,pState
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro enterPrefAccept,event
widget_control,event.top,get_uvalue = pState
x = packagePrefs(pState)
if ((*pState).notifyIds)[0] ne (-1L) then begin
  s = size((*pState).notifyIDs)
  if s[0] eq 1 then count = 0 else count = s[2]-1
  for j = 0,count do begin
    prefsInfo = {prefsEvent,$
                        ID:(*pState).notifyIDs[0,j],$
                        Top:(*pState).notifyIDs[1,j],$
                        Handler:0l,$
                        prefs:(*pState).prefs}
    if widget_info((*pState).notifyIDs[0,j],/valid_id) then begin $
      widget_control,(*pState).notifyIDs[0,j],send_event = prefsInfo
    endif
  endfor
endif
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro enterPrefQuit,event
widget_control,event.top,get_uvalue = pState
x = packagePrefs(pState)
widget_control,event.top,/destroy
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro enterPref_event,event
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro enterPref,     group_leader = group_leader, $
                   prefs = prefs, $
                   notifyIds = notifyIds
; Widget definition module
xsize = 5
if n_elements(notifyIds) eq 0 then notifyIds = (-1L)
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = 'User Fitting Preferences', $
        /tlb_frame_attr,/base_align_center)
endif else begin
  tlb = widget_base(group_leader = group_leader, /col, $
        title = 'PAN-User Preferences',/tlb_frame_attr, $
        /base_align_center)
endelse

values = ['Fit all groups to same model', $
          'Use dialogs for parameter initial guesses']
if n_elements(prefs) eq 0 then $
   prefs = {preferences, $
            same:0, xenforce:0,yenforce:0, $
            xmin:0.0,xmax:1.0,ymin:0.0,ymax:1.0, $
            xfitlo:0.0, xfithi:1.0,initGuesses:0, $
            xlabel:'x',ylabel:'y',maxIter:200}

prefgroup = cw_bgroup(tlb, values, /col, /nonexclusive,$
                      label_top = 'User preferences', $
                      /frame,set_value = [prefs.same,prefs.initGuesses])

rangeBase = widget_base(tlb,/row,/base_align_center)
xBase = widget_base(rangeBase,/col,/base_align_center)
xgroup = cw_bgroup(xbase,['Enforce x-range'],/col,/nonexclusive, $
                   /frame,set_value = [prefs.xenforce])
xminField = cw_field(xBase,value = prefs.xmin,/string,title = 'min(x)',xsize=xsize)
xmaxField = cw_field(xBase,value = prefs.xmax,/string,title = 'max(x)',xsize=xsize)

yBase = widget_base(rangeBase,/col,/base_align_center)
ygroup = cw_bgroup(ybase,['Enforce y-range'],/col,/nonexclusive, $
                   /frame,set_value = [prefs.yenforce])
yminField = cw_field(yBase,value = prefs.ymin,/string,title = 'min(y)',xsize=xsize)
ymaxField = cw_field(yBase,value = prefs.ymax,/string,title = 'max(y)',xsize=xsize)

labelBase = widget_base(tlb,/row)
fsize = 8
xlabel = cw_field(labelbase,value = prefs.xlabel,/string, $
               title = 'x-axis label',xsize = 15)
ylabel = cw_field(labelbase,value = prefs.ylabel,/string, $
               title = 'y-axis label',xsize = 15)

ctrlBase = widget_base(tlb,/row)
xfitlowField = cw_field(ctrlbase,value = prefs.xfitlo,/string, $
               title = 'Lower fit limit',xsize = fsize)
xfithighField = cw_field(ctrlbase,value = prefs.xfithi,/string, $
               title = 'Upper fit limit',xsize = fsize)
maxIterations = cw_field(tlb,value = prefs.maxIter,/string, $
               title = 'Maximum fit iterations',xsize = fsize)
quitBase = widget_base(tlb,/row)
void = widget_button(quitBase,value = 'Accept',event_pro = 'enterPrefAccept')
void = widget_button(quitBase,value = 'Dismiss',event_pro = 'enterPrefQuit')

widget_control,tlb,/realize

state = {prefGroup:prefGroup,$
         xlabel:xlabel, $
         ylabel:ylabel, $
         xgroup:xgroup, $
         ygroup:ygroup, $
         xminField:xminField, $
         xmaxField:xmaxField, $
         yminField:yminField, $
         ymaxField:ymaxField, $
         xfitlowField:xfitlowField, $
         xfithighField:xfithighField, $
         maxIterations:maxIterations, $
         prefs:prefs, $
         notifyIds:notifyIds}

pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState,/no_copy
xmanager,'enterPref',tlb,cleanup = 'enterprefCleanup',/no_block
return
end