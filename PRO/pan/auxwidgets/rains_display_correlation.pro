; $Id: rains_display_correlation.pro,v 1.1 2003/08/19 19:50:20 dimeo Exp $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro rdc_cleanup,tlb
widget_control,tlb,get_uvalue = pstate
correl_info = {CORREL_EVENT,	$
					id:(*pstate).notify_ids[0],		$
					top:(*pstate).notify_ids[1],	$
					handler:0L						}
if widget_info((*pstate).notify_ids[0],/valid_id) then begin
	widget_control,(*pstate).notify_ids[1],send_event = correl_info
endif

widget_control,(*pstate).notify_ids[1],sensitive = 1
device,decomposed = 1
tvlct,*(*pstate).rptr,*(*pstate).gptr,*(*pstate).bptr
wdelete,(*pstate).winpix
heap_free,pstate
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro rdc_winevents,event
widget_control,event.top,get_uvalue = pstate
id = widget_info(event.top,find_by_uname = 'CORR_VALUE')
if (event.x gt !d.x_size-1) or (event.y gt !d.y_size-1) then return
;print,event.x
; calculate which value the cursor is on...
n_cor = n_elements((*pstate).parmnames)
x_index = fix(n_cor*(1.0*event.x/(!d.x_size)))
y_index = fix(n_cor*(1.0*event.y/(!d.y_size)))
widget_control,id,set_value = $
	strtrim(string((*(*pstate).prho)[x_index,y_index]),2)
id1 = widget_info(event.top,find_by_uname = 'PARM1')
id2 = widget_info(event.top,find_by_uname = 'PARM2')
widget_control,id1,set_value = (*pstate).parmnames[x_index]
widget_control,id2,set_value = (*pstate).parmnames[y_index]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro rdc_update,event
widget_control,event.top,get_uvalue = pstate
wset,(*pstate).winpix
disp_rho = bytscl(congrid(*(*pstate).prho,!d.x_size,!d.y_size),min = -1.0)
tv,disp_rho
wset,(*pstate).winvis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,(*pstate).winpix]
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro rdc_changeCT,event
widget_control,event.top,get_uvalue = pstate
thisEvent = Tag_Names(event, /Structure_Name)
CASE thisEvent OF
	'WIDGET_BUTTON':	BEGIN
							XColors, Group_Leader=event.top, $
							         NotifyID=[event.id, event.top],$
							         title = 'Colors for Correlation Matrix'
					 	ENDCASE

    'XCOLORS_LOAD': 	BEGIN
                 			rdc_update,event
                 		ENDCASE

ENDCASE

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro rdc_top_10,event
; Determine the top 10 parameters
widget_control,event.top,get_uvalue = pstate
pcor = *(*pstate).prho
parmnames = (*pstate).parmnames
nmax = 10 < n_elements(parmnames)

; Replace the main diagonal elements with sm_num
sm_num = -1.e6
acor = abs(pcor)
n = n_elements(parmnames)
for ii = 0,n-1 do acor[ii,ii] = sm_num
big = where(acor eq abs(sm_num),count_big)
if count_big gt 0 then acor[big] = sm_num
psort = UNIQ(acor, SORT(acor))
pcor_sorted = pcor[psort]
wheretomulti,pcor,psort,row,col
strout = strarr(nmax)
for i = 0,nmax-1 do begin
	strout[i] = parmnames[row[n_elements(pcor_sorted)-1-i]]+','+$
		parmnames[col[n_elements(pcor_sorted)-1-i]]+': '+ $
		strtrim(string(pcor_sorted[n_elements(pcor_sorted)-1-i]),2)
endfor
void = dialog_message(dialog_parent = event.top,strout,/information, $
	title = 'Top 10 Correlated Parameters')

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro rdc_event,event
widget_control,event.top,get_uvalue = pstate
uname = widget_info(event.id,/uname)

case uname of
'WINDOW':		rdc_winevents,event
'QUIT':			widget_control,event.top,/destroy
'CHANGE_CT':	rdc_changeCT,event
'TOP_10':		rdc_top_10,event
else:
endcase

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro rains_display_correlation,		rho,							$
									parmnames,						$
									group_leader = group_leader,	$
									notify_ids = notify_ids
; rho = correlation matrix
; parmnames = parameter names
;
; Widget definition module
modal = 1
; Desensitize RAINS
widget_control,notify_ids[1],sensitive = 0

tvlct,r,g,b,/get
device,decomposed = 0
loadct,15,/silent

tlb = widget_base(group_leader = group_leader,title = 'Correlation Matrix', $
	/col,tlb_frame_attr = 9)
xsize = 400 & ysize = 400
win = widget_draw(tlb,xsize = xsize,ysize = ysize,uname = 'WINDOW', $
	/motion_events)
rowbase = widget_base(tlb,/row)
corr_value = cw_field(rowbase,title = 'Correlation',value = 0.0,/string, $
	uname = 'CORR_VALUE')
rowbase1 = widget_base(tlb,/row)
p1 = cw_field(rowbase1,title = 'P1',value = '',/string, $
	uname = 'PARM1')
p2 = cw_field(rowbase1,title = 'P2',value = '',/string, $
	uname = 'PARM2')
rowbase2 = widget_base(tlb,/row)
void = widget_button(rowbase2,value = 'Top 10 Correlated Parameters',uname = 'TOP_10')
void = widget_button(rowbase,value = 'Change Color Table',uname = 'CHANGE_CT')
quit = widget_button(rowbase2,value = 'QUIT',uname = 'QUIT')

centertlb,tlb
widget_control,tlb,/realize
widget_control,win,get_value = winvis
window,/free,/pixmap,xsize = xsize,ysize = ysize
winpix = !d.window

prho = ptr_new(rho,/no_copy)

state = 	{	prho:prho,				$
				parmnames:parmnames,	$
				winvis:winvis,			$
				winpix:winpix,			$
				notify_ids:notify_ids,	$
				rptr:ptr_new(r),		$
				gptr:ptr_new(g),		$
				bptr:ptr_new(b)			}

pstate = ptr_new(state,/no_copy)
; Display the correlation matrix
gamma_ct,3.467
disp_rho = bytscl(congrid(*(*pstate).prho,xsize,ysize),min = -1.0,top = 182)
wset,winpix
tv,disp_rho
wset,winvis
device,copy = [0,0,!d.x_size,!d.y_size,0,0,winpix]
widget_control,tlb,set_uvalue = pstate
xmanager,'rdc',tlb,event_handler = 'rdc_event',cleanup = 'rdc_cleanup',/no_block

end
