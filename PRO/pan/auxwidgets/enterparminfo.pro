; $Id: enterparminfo.pro,v 1.1 2002/09/19 21:30:20 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function getParmInfo,pState
; Ok, now pull all of the information out

ncurves = n_elements((*(*pState).parmInfoPtr).names)
nparms = total((*(*pState).parmInfoPtr).nparms)

; First pull out values from text fields

for i = 0,nparms-1 do begin
  widget_control,(*pState).valIds[i],get_value = valIds
  if i eq 0 then outValid = valIds[0] else outValid = [outValid,valIds[0]]
endfor
(*(*pState).parmInfoPtr).parms = outValid

for i = 0,nparms-1 do begin
  widget_control,(*pState).lowvalIds[i],get_value = lovalIds
  if i eq 0 then outlowvals = lovalIds[0] else outlowvals = [outlowvals,lovalids[0]]
endfor
(*(*pState).parmInfoPtr).lovalues = outlowvals

for i = 0,nparms-1 do begin
  widget_control,(*pState).hivalIds[i],get_value = hivalIds
  if i eq 0 then outhighvals = hivalIds[0] else outhighvals = [outhighvals,hivalids[0]]
endfor
(*(*pState).parmInfoPtr).hivalues = outhighvals

; Now pull out the checkbox settings
for i = 0,ncurves-1 do begin
  widget_control,(*pState).fixIds[i],get_value = fixed
  if i eq 0 then outfixed = fixed else outfixed = [outfixed,fixed]
endfor
(*(*pState).parmInfoPtr).fixed = outfixed

for i = 0,ncurves-1 do begin
  widget_control,(*pState).lowIds[i],get_value = low
  if i eq 0 then outlow = low else outlow = [outlow,low]
endfor
(*(*pState).parmInfoPtr).low = outlow

for i = 0,ncurves-1 do begin
  widget_control,(*pState).highIds[i],get_value = high
  if i eq 0 then outhigh = high else outhigh = [outhigh,high]
endfor
(*(*pState).parmInfoPtr).high = outhigh

for i = 0,ncurves-1 do begin
  widget_control,(*pState).tieIds[i],get_value = tied
  if i eq 0 then outtied = tied else outtied = [outtied,tied]
endfor
(*(*pState).parmInfoPtr).tied = outtied

return,1
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro parmInfoCleanup,tlb
widget_control,tlb,get_uvalue = pState
if ((*pState).notifyIds)[0] ne (-1L) then begin
  s = size((*pState).notifyIDs)
  if s[0] eq 1 then count = 0 else count = s[2]-1
  for j = 0,count do begin
    parmsInfo = {parmsQuitEvent,$
                        ID:(*pState).notifyIDs[0,j],$
                        Top:(*pState).notifyIDs[1,j],$
                        Handler:0l}
    if widget_info((*pState).notifyIDs[0,j],/valid_id) then begin $
      widget_control,(*pState).notifyIDs[0,j],send_event = parmsInfo
    endif
  endfor
endif
ptr_free,(*pState).parmInfoPtr
ptr_free,pState
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro parmInfoQuit,event
widget_control,event.top,/destroy
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro parmInfoQuitModal,event
widget_control,event.top,get_uvalue = pState
dummy = getParmInfo(pState)
widget_control,event.top,/destroy
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro parmInfoAccept,event
widget_control,event.top,get_uvalue = pState
dummy = getParmInfo(pState)
if ((*pState).notifyIds)[0] ne (-1L) then begin
  s = size((*pState).notifyIDs)
  if s[0] eq 1 then count = 0 else count = s[2]-1
  for j = 0,count do begin
    parmsInfo = {parmsEvent,$
                        ID:(*pState).notifyIDs[0,j],$
                        Top:(*pState).notifyIDs[1,j],$
                        Handler:0l,$
                        parmInfo:(*pState).parmInfoPtr}
    if widget_info((*pState).notifyIDs[0,j],/valid_id) then begin $
      widget_control,(*pState).notifyIDs[0,j],send_event = parmsInfo
    endif
  endfor
endif
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro enterparminfo_event,event
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro enterparminfo,inInfo, $
                  group_leader = group_leader, $
                  notifyIds = notifyIds, $
                  modal = modal, $
                  parmInfoOut = parmInfoOut

if n_elements(modal) eq 0 then modal = 0
if n_elements(notifyIds) eq 0 then notifyIds = (-1L)
; Widget definition module
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = 'PAN Parameter Information', $
        /tlb_frame_attr,/base_align_center)
endif else begin
  tlb = widget_base(group_leader = group_leader, /col, $
        title = 'PAN Parameter Information',/tlb_frame_attr, $
        /base_align_center,modal = modal)
endelse

xsize = 10

if n_params() eq 0 then begin
  ncurves = 2
  nparms = [3,2]
  totparms = total(nparms)
  names = ['pan_gaussian','pan_background']
  expr = replicate('',totparms)
  tied = replicate(0d,totparms)
  step = replicate(0d,totparms)
  fixed = replicate(0,totparms)
  lovalues = replicate(0d,totparms)
  low = replicate(0,totparms)
  high = replicate(0,totparms)
  hivalues = replicate(1d,totparms)
  parms = [1.0,0.0,1.0,0.0,0.0]
  parmnames = ['area','center','width','offset','slope']
  inInfo =   {names:names, nparms:nparms,step:step, fixed:fixed, $
              lovalues:lovalues, hivalues:hivalues, high:high, $
              low:low, parms:parms, parmnames:parmnames, $
              tied:tied,expr:expr}
endif

parminfo = {       names:inInfo.names, $
                   nparms:inInfo.nparms,$
                   step:inInfo.step, $
                   fixed:inInfo.fixed, $
                   lovalues:inInfo.lovalues, $
                   hivalues:inInfo.hivalues, $
                   high:inInfo.high, $
                   low:inInfo.low, $
                   parms:inInfo.parms, $
                   parmnames:inInfo.parmnames, $
                   tied:inInfo.tied, $
                   expr:inInfo.expr}


nparms = parminfo.nparms
ncurves = n_elements(parminfo.names)
totparms = total(nparms)

dispNames = strarr(ncurves)
for i = 0,ncurves-1 do begin
  pos = strpos((parminfo.names)[i],'_')
  dispNames[i] = strmid(strupcase((parminfo.names)[i]),pos+1)
endfor

button_labels = dispNames
base = cw_tabbase(tlb,button_labels,/droplist,bases = coltab,/frame,/col)
tab = lonarr(ncurves)
for i = 0,ncurves-1 do begin
  tab[i] = widget_base(coltab[i],/row)
endfor

bases = lonarr(ncurves)
subBases = lonarr(ncurves)
for i = 0,ncurves-1 do begin
  bases[i] = widget_base(tab[i],/row)
endfor

ctrlBase = widget_base(tlb,/row)
if modal eq 0 then begin
  void = widget_button(ctrlBase,value = 'Apply',event_pro = 'parmInfoAccept')
  void = widget_button(ctrlBase,value = 'Dismiss',event_pro = 'parmInfoQuit')
endif else begin
  void = widget_button(ctrlBase,value = 'Apply and Dismiss', $
         event_pro = 'parmInfoQuitModal')
endelse
valbase = lonarr(ncurves)
valIds = lonarr(totparms)
count = 0
for k = 0,ncurves-1 do begin
  valbase[k] = widget_base(bases[k],/col,/base_align_right)
  if (parminfo.expr)[k] ne '' then begin
    void = widget_label(coltab[k],value = '');(parmInfo.expr)[k])
  endif
  void = widget_label(valbase[k],value = 'Values')
  for j = 0,nparms[k]-1 do begin
      valIds[count] = cw_field(valbase[k],value = parminfo.parms[count],$
                        title = parminfo.parmnames[count],xsize = xsize)
      count = count + 1
  endfor
endfor

fixbase = lonarr(ncurves)
start = 0
finish = parminfo.nparms[0] - 1
for k = 1,ncurves-1 do begin
  start = [start,total(parminfo.nparms[0:k-1])]
  finish = [finish,total(parminfo.nparms[0:k])-1]
endfor

count = 0
fixIds = lonarr(ncurves)
for k = 0,ncurves-1 do begin
  ;;;;;;;;;
  fixbase[k] = widget_base(bases[k],/col)
  ;;;;;;;;;
  void = widget_label(fixbase[k],value = 'Fixed')
  fixIds[k] = cw_bgroup(fixbase[k],parminfo.parmnames[start[k]:finish[k]],$
              /col,/nonexclusive, $
              set_value = byte(parminfo.fixed[start[k]:finish[k]]))
endfor

lowbase = lonarr(ncurves)
lowIds = lonarr(ncurves)
for k = 0,ncurves-1 do begin
  lowbase[k] = widget_base(bases[k],/col)

  void = widget_label(lowbase[k],value = 'Set Low')
  lowIds[k] = cw_bgroup(lowbase[k],parminfo.parmnames[start[k]:finish[k]], $
              /col,/nonexclusive, $
              set_value = byte(parminfo.low[start[k]:finish[k]]))
endfor

lovalbase = lonarr(ncurves)
lowvalIds = lonarr(totparms)
count = 0
for i = 0,ncurves-1 do begin

  lovalbase[i] = widget_base(bases[i],/col,/base_align_right)
  void = widget_label(lovalbase[i],value = 'Lower Limit')
  for j = 0,nparms[i]-1 do begin
    lowvalIds[count] = cw_field(lovalbase[i],value = parminfo.lovalues[count],$
                       title = parminfo.parmnames[count],xsize = xsize)
    count = count + 1
  endfor
endfor

highbase = lonarr(ncurves)
highIds = lonarr(ncurves)

for i = 0,ncurves-1 do begin
  highbase[i] = widget_base(bases[i],/col)
  void = widget_label(highbase[i],value = 'Set High')
  highIds[i] = cw_bgroup(highbase[i],parminfo.parmnames[start[i]:finish[i]], $
               /col,/nonexclusive, $
               set_value = byte(parminfo.high[start[i]:finish[i]]))
endfor

hivalbase = lonarr(ncurves)
hivalIds = lonarr(totparms)
count = 0
for i = 0,ncurves-1 do begin
  hivalbase[i] = widget_base(bases[i],/col,/base_align_right)
  void = widget_label(hivalbase[i],value = 'Upper Limit')
  for j = 0,nparms[i]-1 do begin
    hivalIds[count] = cw_field(hivalbase[i],value = parminfo.hivalues[count],$
                      title = parminfo.parmnames[count],xsize = xsize)
    count = count + 1
  endfor
endfor

tiedbase = lonarr(ncurves)
tieIds = lonarr(ncurves)

for i = 0,ncurves-1 do begin
  tiedbase[i] = widget_base(bases[i],/col)
  void = widget_label(tiedbase[i],value = 'Tie')
  tieIds[i] = cw_bgroup(tiedbase[i],parminfo.parmnames[start[i]:finish[i]], $
               /col,/nonexclusive, $
               set_value = byte(parminfo.tied[start[i]:finish[i]]))
endfor


parmInfoOut = (-1L)

widget_control,tlb,/realize
state = {parminfoPtr:ptr_new(parmInfo,/no_copy),$
         lowvalIds:lowvalIds, $
         hivalIds:hivalIds, $
         lowIds:lowIds, $
         highIds:highIds, $
         fixIds:fixIds, $
         valIds:valIds, $
         tieIds:tieIds, $
         notifyIds:notifyIds}
pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState

if modal eq 0 then begin
  xmanager,'enterparminfo',tlb,/no_block,cleanup = 'parmInfoCleanup'
endif else begin
  xmanager,'enterparminfo',tlb
  parmInfoOut = *(*pState).parmInfoPtr
  ptr_free,(*pState).parmInfoPtr
  ptr_free,pState
endelse

return
end
