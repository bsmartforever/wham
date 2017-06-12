pro make_image, state
;
; This procedure produces the displayable array state.image from the
; data given in state.data

; Get display parameters
common draw_comm, draw_id
wset, draw_id

if state.sos eq 0 then begin
	print, 'ERROR:  Data must be read in first'
	return
	end


m = intarr(2)

for i = 0,1 do begin
;	d = 400 mod state.size(i)
;	m(i) = (400-d) / state.size(i) 
    m(i) = fix(400/state.size(i))
;	Print, 'Loop 1'
end
;Set rebin factor to the minimum of the two values.  
;This removes any distortion from the final image
factor = min(m)
state.conditions(5)=factor
; Remake the structure taking into account what we know now

state1 = {sos:		state.sos,	$
	widgetids:      state.widgetids,	$
	conditions: 	state.conditions,	$
	filename:	state.filename,	$
	velocity:	state.velocity,	$
	data:		state.data,		$
	size:		state.size,	$	
	image:		bytarr(factor*state.size(0),factor*state.size(1),$
			state.size(2)),	$
	imsize:		state.size,    	$
	vel_label:	state.vel_label}

;help,state
state= state1

state.imsize(0)=factor*state.size(0)
state.imsize(1)=factor*state.size(1)

;set up the image for display

;Print, 'Outside the loop'
minimum = min(state.data)
maximum = max(state.data)
image1 = intarr(state.size(0),state.size(1),state.size(2))
for i = 0,state.size(2)-1 do begin
	if state.conditions(6) eq 1 then begin
	     minimum = min(state.data(*,*,i))
	     maximum = max(state.data(*,*,i))
        end

	image1(*,*,i)= bytscl(state.data(*,*,i),min= minimum,max=maximum)
;	print, 'inside the loop (like Bush)'
	end
state.image = rebin(image1,factor*state.size(0),factor*state.size(1),   $
		state.size(2), /sample)
tv,state.image(*,*,state.conditions(2))
;Print, 'Exiting Display'
return
end
