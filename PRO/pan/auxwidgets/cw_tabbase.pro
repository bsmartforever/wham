;$Id: cw_tabbase.pro,v 1.2 2002/04/26 22:40:05 jmspark Exp $
;
; CW_TABBASE.PRO
;
; Class project for Application Development in IDL
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro cw_tabbase_cleanup,stash
; Kill_notify routine for the first child.
; Note that base is the first child of the
; cw's compound widget top-level base
compile_opt idl2,hidden
; Retrieve the state pointer and destroy it.
widget_control,stash,get_uvalue = pState
ptr_free,pState
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function cw_tabbase_event_func,event
compile_opt idl2,hidden
; Retrieve the state pointer
; event.handler is the id of the tlb of cw_tabbase
stash = widget_info(event.handler,/child)
widget_control,stash,get_uvalue = pState

; Unmap the previous base, map the new base
if keyword_set((*pState).droplist) then begin
	if event.id eq (*pState).drop then begin
		cw_tabbase_set_value,event.handler,event.index
		return,0
	endif else $
		return,event	; event from inside tab base "falls through" back to calling program
endif else begin
	ind = where((*pState).tabButtons eq event.id,count)
	if count ne 0 then begin
		cw_tabbase_set_value,event.handler,ind[0]
		return,0
	endif else $
		return,event	; event from inside tab base "falls through" back to calling program
endelse

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro cw_tabbase_event_pro,event
compile_opt idl2,hidden
; Retrieve the state pointer
; event.handler is the id of the tlb of cw_tabbase
stash = widget_info(event.handler,/child)
widget_control,stash,get_uvalue = pState

; Handle events.
ret = cw_tabbase_event_func(event)
if size(ret,/type) eq 8 then $
	call_procedure,(*pState).event_pro,ret

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function cw_tabbase_get_value,id
compile_opt idl2,hidden
; Retrieve the state pointer
stash = widget_info(id,/child)
widget_control,stash,get_uvalue = pState

; Return the index of the currently selected tab base
return,(*pState).currentBase
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro cw_tabbase_set_value,id,value
compile_opt idl2,hidden
; Retrieve the state pointer
stash = widget_info(id,/child)
widget_control,stash,get_uvalue = pState
; Hide the base, show the button as sensitive, hide label.
widget_control,((*pState).tabBases)[(*pState).currentBase],$
	map = 0

if ((*pState).droplist eq 0) and $
   ((*pState).exclusive eq 0) then $
   widget_control,((*pState).tabButtons)[(*pState).currentBase],sensitive = 1

if (*pState).exclusive then $
	widget_control,((*pState).tabButtons)[(*pState).currentBase],set_button = 0

if (*pState).no_label eq 0 then $
	widget_control,((*pState).labelBases)[(*pState).currentBase],map = 0

; Update the state pointer with the currently selected base.
(*pState).currentBase = value

; Show the new base,make the button insensitive, show label.
widget_control,((*pState).tabBases)[value],map = 1
if (*pState).droplist eq 0 and (*pState).exclusive eq 0 then $
	 widget_control,((*pState).tabButtons)[value],sensitive = 0
if (*pState).no_label eq 0 then $
	widget_control,((*pState).labelBases)[value],map = 1
if (*pState).droplist then $
	widget_control,(*pState).drop,set_droplist_select = value
if (*pState).exclusive then $
	widget_control,((*pState).tabButtons)[value],set_button = 1

end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function cw_tabbase,	base,labels,$
						bases = tabBases,$
						column = c,$
						row = r,$
						droplist = droplist,$	; specified for buttons at top of base
						exclusive = exclusive,$	; specified for buttons at top of base
						event_pro = event_pro,$
						font = font,$
						frame = frame,$
						no_label = no_label,$
						sidebar = sidebar		; returns widget id of column base
												; to right of tabbase

compile_opt idl2		; tells IDL compiler to subscript vector with square brackets,
						; promotes a scalar integer to a long integer


; Check for existence of an event_pro keyword.
if n_elements(event_pro) eq 0 then begin
	event_pro = ""
	epro = ""
	efunc = "cw_tabbase_event_func"
endif else begin
	epro = "cw_tabbase_event_pro"
	efunc = ""
endelse

; Perform parameter checking.
if n_elements(exclusive) eq 0 then exclusive = 0
if n_elements(droplist) eq 0 then droplist = 0
if n_elements(font) eq 0 then font = ""
if n_elements(frame) eq 0 then frame = 0
if n_elements(no_label) eq 0 then no_label = 0
if n_elements(sidebar) eq 0 then sidebar = 0

; Check for row or column layout
if keyword_set(r) and keyword_set(c) then $
	message,"Must be either row or column layout."

if keyword_set(r) then begin
	row = 1
	column = 0
endif

if keyword_set(c) then begin
	row = 0
	column = 1
endif

if (not keyword_set(c)) and (not keyword_set(r)) then begin
	row = 1
	column = 0
endif

; Define the number of tab bases.
ntabs = n_elements(labels)

; Make the top-level base. (Note: this is the tlb of the
; compound widget, not that of the widget program calling
; the compound widget).

tlb = 	widget_base(base,xpad = 0,ypad = 0,$
		row = row,column = column,$
		event_pro = epro,$
		event_func = efunc,$
		pro_set_value = "cw_tabbase_set_value",$
		func_get_value = "cw_tabbase_get_value")

; Create the tab buttons.
tabs = widget_base(tlb,column = column,row = row)	; tabs is our output keyword parameter

blankBase = widget_base(tabs,/column,space = 0)

buttons =	widget_base(tabs,column = row,row = column,$
			exclusive = exclusive,xpad = 0,ypad = 0)

if (row and not keyword_set(no_label)) then $
  blankLine = widget_label(blankBase,value = " ",font = font)

if keyword_set(droplist) then begin
  tabbuttons = 0
  drop = widget_droplist(tabs,value = labels,font = font)
endif else begin
  drop = 0
  tabbuttons = lonarr(ntabs)
  for i = 0,ntabs-1 do $
    tabbuttons[i] = widget_button(buttons,value = labels[i],font = font)
endelse

; Make a base to hold the label bases and tab bases.
col = widget_base(tlb,/column,xpad = 0,ypad = 0,space = 0)

; Build the label bases for the tabs.
if not keyword_set(no_label) then begin
	lbase = widget_base(col,space = 0)
	labelBases = lonarr(ntabs)
	for i = 0,ntabs-1 do begin
	  labelBases[i] = widget_base(lbase,/column,map = 0)
	  if not no_label then $
	    label = widget_label(labelBases[i],value = labels[i],/align_left,font = font)
	endfor
endif

; Build the actual tab bases
row = widget_base(col, /row,xpad = 0,ypad = 0)
bases = widget_base(row,frame = frame)
tabBases = lonarr(ntabs)
for i = 0,ntabs-1 do	$
	tabBases[i] = widget_base(bases,/column,map = 0)
; Build the sidebar
if arg_present(sidebar) then $
	sidebar = widget_base(row,/column,xpad = 0,ypad = 0)

; Map the first base
widget_control,tabBases[0], map = 1
if keyword_set(droplist) then $
	widget_control,drop,set_list_select = 0 $
else if keyword_set(exclusive) then $
	widget_control,tabButtons[0],/set_button $
else $
	widget_control,tabButtons[0],sensitive = 0

; Map the first label base if no_label is not set
if not keyword_set(no_label) then $
	widget_control,labelBases[0],map = 1

; Build a state structure.
state = { $
		droplist	:	droplist,$
		drop		:	drop,$
		exclusive	:	exclusive,$
		event_pro	:	event_pro,$
		tabBases	:	tabBases,$
		tabButtons	:	tabButtons,$
		labelBases	:	keyword_set(no_label) ? 0 : labelBases,$ ; note use of ternary operator
		no_label	:	keyword_set(no_label),$
		currentBase	:	0 $
		}

; Store the state structure in heap and access
; it with a pointer
pState = ptr_new(state,/no_copy)

; Store the state pointer in the user value of a child
; of the top-level base of the cw.
widget_control,widget_info(tlb,/child),$	; stuff it in the oldest child
	set_uvalue = pState,kill_notify = "cw_tabbase_cleanup"

; Return the top-level base widget id of the compound widget
return,tlb
end





