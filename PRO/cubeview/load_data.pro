pro load_data, state

; This procedure was written for use with WHAM.pro 
; Right now, this program does little except to get a few inputs from the user
;	Later, this routine might actually prompt the user for the file name
;	in some pre-determined format.
;
; What follows is copied from Kristin Tufte's whamfit program, and is 
; a simple way to read in information from the user.
;

;**************************************************************************
;EVERYTHING UNTIL THE NEXT SET OF ASTRISKS IS JUST FYI
;
;
; get file name
;fname = strarr(1)
;print,'File name to be read in'
;read,fname
;fname = fname(0)  ; converts to a simple string (scalar value) from an array

; print file name in text widget area
;widget_control,set_value = 'File: '+fname,mstate.fnamewdg


; get number of lines in file from the operating system
;command = 'wc -l ' + fname
;spawn,command,res
;ndp = fix(res)

;openr, 3, fname, ERROR = err
;if (err ne 0) then begin
;  widget_control,set_value='Error: Invalid File Name',mstate.errorwdg
;  return
;end
;
;a = dblarr(3)
;
; TODO  Should be changed to skip (and save) header lines - need to
; find out what standard header is and print some of that with the
; model widget
;temp = strarr(2)  ; skip first two lines of file
;readf,3,temp		
;ndp = ndp - 2     ; decrement ndp
;ndp = ndp(0)      ; convert to scalar
;print, 'Number of data points detected: ', ndp

; create a new state variable, now that I know the number of data points
;tempstate =    {initstruct1, 				$
;		sos:		1,			$	 
;	     	maxng:      	state.maxng,		$
;             	maxbk:    	state.maxbk, 		$ 
;	     	paramarr:	state.paramarr, 	$
;             	prparamarr:	state.prparamarr,	$
;		isused:	        state.isused,		$
;		sqztbl:		state.sqztbl,		$
;		invsqztbl:	state.invsqztbl,		$
;	     	sigmaa:		state.sigmaa,		$
;	     	ng: 		1,			$
;		bk: 		0,			$
;		ndp: 	       -1, 			$
;		fit: 		1, 			$
;	     	basewdg: 	state.basewdg, 		$
;		modelwdg: 	state.modelwdg, 	$ 
;             	paramwdg: 	state.paramwdg, 	$
;	     	function_name:	state.function_name, 	$
;	     	xArr: 		dblarr(ndp),		$
;		yArr: 		dblarr(ndp), 		$
;             	wArr: 		dblarr(ndp), 		$
;		result: 	dblarr(ndp)}
;
;
;state = tempstate
;
; read in data
;for i=0,(ndp-1) do begin
;  readf,3,a
;  state.xArr(i) = a(0)
;  state.yArr(i) = a(1)
;  state.wArr(i) = a(2)
;endfor	
;close,3

; convert variances to weights for curve fit
;state.warr = 1/state.warr
;
;************************************************************************
;
; change sos to 1 to indicate that we do now have the data array read in
state.sos = 1;


;Read in information from the user

filename = strarr(1)
print, 'Name of the file being analyzed.'
read,filename
filename = filename(0)
state.filename = filename + '                                  '
widget_control, state.widgetids(10), set_value = state.filename

print, 'Velocity of the first image, i.e. (*,*,0): '
read, v0
state.velocity(0)=v0
print, 'Change in velocity per image: '
read, dv
state.velocity(1)=dv

; Now initialize the state.vel_label array:

iden = findgen(state.size(2))
state.vel_label = v0 + iden * dv


return
end

