; $Id: enterfunction.pro,v 1.1 2002/09/19 21:30:20 dimeo Exp $
;
; ENTERFUNCTION.PRO
;
; This widget dialog allows the user to type in a fit expression, save it,
; and/or restore it.
;
; Written by R.M.Dimeo for OPAN__DEFINE.PRO
; Modified my MSW 30/12/2007 to restore equations for some common line doublets, etc.
; MSW 30/10/2008: added NaI doublet
; MSW 26/4/2010: modified HaNII_redshift, added SII_redshift
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro enterfunction_event,event
widget_control,event.top,get_uvalue = pState
widget_control,event.id,get_value = val
case strupcase(val[0]) of
'ACCEPT':	begin
			  ;(*pState).out.accept = 1
			  (*pState).out.cancel = 0
			  widget_control,(*pState).expression,get_value = expr
			  (*pState).out.expr = string(expr[0])
			  widget_control,event.top,/destroy
		end
'CANCEL':	begin
			  ;(*pState).out.accept = 0
			  (*pState).out.cancel = 1
			  (*pState).out.expr = ''
			  widget_control,event.top,/destroy
			end
'SAVE TO FILE':	$
			begin
			widget_control,(*pState).expression,get_value = expr
			thisExpr = string(expr[0])
			filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                       title = 'Enter file name for your equation',$
                       /write,filter = '*.eq',$
                       path = (*pState).workDir)
            if filename eq '' or filename eq ' ' then return
            filename = filename + '.eq'
			openw,lun,filename,/get_lun
			printf,lun,thisExpr
			free_lun,lun
			end
'RESTORE FROM FILE': $
			begin
			filename = DIALOG_PICKFILE(dialog_parent = event.top,$
                       title = 'Select equation to restore',$
                       /read,filter = '*.eq',$
                       path = (*pState).workDir)
            if filename eq '' or filename eq ' ' then return
            thisExpr = ''
			openr,lun,filename,/get_lun
			readf,lun,thisExpr
			free_lun,lun
			widget_control,(*pState).expression,set_value = thisExpr
			end

else:
endcase


;WIDGET_CONTROL,event.TOP,GET_UVALUE=sel_index
sel_index=0
if (TAG_NAMES(event, /STRUCTURE_NAME) eq 'WIDGET_DROPLIST') $
   then begin
     sel_index=event.index
     ;print,sel_index,event.index
endif

widget_control,event.top,get_uvalue = pState
case sel_index of
  0: begin
                        ;print,'selection 1'
                        filename = '/Users/msw/Documents/work/codes/idl/pan/functions/SII.eq'
            if filename eq '' or filename eq ' ' then return
            thisExpr = ''
                        openr,lun,filename,/get_lun
                        readf,lun,thisExpr
                        free_lun,lun
                        widget_control,(*pState).expression,set_value = thisExpr
     end
  1: begin
                        ;print,'selection 2'
                        filename = '/Users/msw/Documents/work/codes/idl/pan/functions/HaNII.eq'
            if filename eq '' or filename eq ' ' then return
            thisExpr = ''
                        openr,lun,filename,/get_lun
                        readf,lun,thisExpr
                        free_lun,lun
                        widget_control,(*pState).expression,set_value = thisExpr
     end
  2: begin
                        ;print,'selection 3'
                        filename = '/Users/msw/Documents/work/codes/idl/pan/functions/CaII.eq'
            if filename eq '' or filename eq ' ' then return
            thisExpr = ''
                        openr,lun,filename,/get_lun
                        readf,lun,thisExpr
                        free_lun,lun
                        widget_control,(*pState).expression,set_value = thisExpr
     end
  3: begin
                        ;print,'selection 4'
                        filename = '/Users/msw/Documents/work/codes/idl/pan/functions/NaI.eq'
            if filename eq '' or filename eq ' ' then return
            thisExpr = ''
                        openr,lun,filename,/get_lun
                        readf,lun,thisExpr
                        free_lun,lun
                        widget_control,(*pState).expression,set_value = thisExpr
     end
  4: begin
                        ;print,'selection 5'
                        filename = '/Users/msw/Documents/work/codes/idl/pan/functions/HaNII_redshift.eq'
			print,'p[0]=flux Ha'
			print,'p[1]=wavelength Ha'
			print,'p[2]=FWHM Ha'
			print,'p[3]=flux [NII]6583
			print,'p[4]=velocity of system (km/s)'
            if filename eq '' or filename eq ' ' then return
            thisExpr = ''
                        openr,lun,filename,/get_lun
                        readf,lun,thisExpr
                        free_lun,lun
                        widget_control,(*pState).expression,set_value = thisExpr
     end
  5: begin
                        ;print,'selection 6'
                        filename = '/Users/msw/Documents/work/codes/idl/pan/functions/SII_redshift.eq'
			print,'p[0]=flux [SII]6717'
			print,'p[1]=wavelength [SII]6717'
			print,'p[2]=FWHM [SII]6717'
			print,'p[3]=flux [SII]6731'
			print,'p[4]=recessional velocity of system (km/s)'
            if filename eq '' or filename eq ' ' then return
            thisExpr = ''
                        openr,lun,filename,/get_lun
                        readf,lun,thisExpr
                        free_lun,lun
                        widget_control,(*pState).expression,set_value = thisExpr
     end
else:
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function enterfunction,group_leader = group_leader,directory = directory
if n_elements(group_leader) eq 0 then begin
  tlb = widget_base(/col,title = 'User function entry',/tlb_frame_attr)
endif else begin
  tlb = widget_base(group_leader = group_leader, /col,/modal, $
        title = 'User function entry',/tlb_frame_attr)
endelse

;thisFont = "Comic Sans MS*22*Bold"
;expression = cw_field(tlb,title = 'e.g. p[0]+p[1]*x+p[2]*x^2', $
expression = cw_field(tlb,title = 'e.g. constrained double gaussian (p[0]=area, p[1]=cen, p[2]=width)', $
	;[SII] function
	;value = 'p[0]*exp(-((x-p[1])^2*4*alog(2))/p[2]^2) + (p[3])*exp(-((x-(p[1]+14.37))^2*4*alog(2))/(p[2])^2)',/col,xsize = 100, $
;	[OIII] function
	;value = 'p[0]*exp(-((x-p[1])^2*4*alog(2))/p[2]^2) + (p[3])*exp(-((x-(p[1]-47.92))^2*4*alog(2))/(p[2])^2)',/col,xsize = 100, $
;       Ha-[NII] function
        ;value = 'p[0]*exp(-((x-p[1])^2*4*alog(2))/p[2]^2) + (p[3])*exp(-((x-(p[1]+20.61))^2*4*alog(2))/(p[2])^2)',/col,xsize = 100, $
        ;test
        value = 'p[0]+p[1]*x+p[2]*x^2',$
	/col,xsize = 100, fieldfont = thisFont,font = thisFont,ysize = 3)
rowBase = widget_base(tlb,/row)

savef = widget_button(rowBase,value = 'Save to file')
loadf = widget_button(rowBase,value = 'Restore from file')
;** MODIFY here as well
selections = ['restore_SII','restore_HaNII','restore_CaII','restore_NaI','restore_HaNII_redshift','restore_SII_redshift']
sel_index=0
dlist = widget_droplist(rowBase,value=selections,title='')
widget_control,dlist,set_droplist_select=sel_index
accept = widget_button(rowBase,value = 'Accept')
cancel = widget_button(rowBase,value = 'Cancel')

widget_control,tlb,/realize

out = {expr:'',cancel:1}
state = {out:out,expression:expression,workDir:directory}
pState = ptr_new(state,/no_copy)
widget_control,tlb,set_uvalue = pState
xmanager,'enterfunction',tlb

out = (*pState).out
ptr_free,pState
return,out
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
