; $Id: launch_opan.pro,v 1.1 2002/09/19 21:30:49 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro launch_opan,event
widget_control,event.top,get_uvalue = pState
thisEvent = tag_names(event,/structure_name)
case thisEvent of
'WIDGET_BUTTON': $
  begin
	; Get the current DAVE pointer
	file_path = (*!dave_defaults).workDir
	if n_elements(*(*(*(*pState).davePtr).dataStrPtr).commonStr.histPtr) $
	   ne 0 then begin
	    davePtr = (*pState).davePtr
	    o = obj_new('opan',notifyId = [event.id,event.top], $
    	                   group_leader = event.top, $
        	               davePtr = davePtr, $
        	               workDir = file_path, $
        	               dataDir = file_path)
    endif else begin
	    o = obj_new('opan',notifyId = [event.id,event.top], $
    	                   group_leader = event.top, $
        	               workDir = file_path, $
        	               dataDir = file_path)
    endelse
    (*(*pState).extraPtr).panInfo.panContainer->add,o
  end
'OPANEVENT': $
  begin
    obj = event.object	; This is the object that just quit
    (*(*pState).extraPtr).panInfo.panContainer->remove,obj
	obj_destroy,obj
  end
else:
endcase
return
end