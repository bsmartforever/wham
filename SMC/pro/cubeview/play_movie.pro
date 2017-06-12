pro play_movie, state
; This sets up and calls the procedure xinteranimate.

xinteranimate, set= state.imsize

for i=0,state.imsize(2)-1 do   $
	xinteranimate, image=reform(state.image(*,*,i)),frame=i

xinteranimate, group = state.widgetids(0)

return
end