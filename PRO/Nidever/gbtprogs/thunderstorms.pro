pro thunderstorms

; Make a plot of the GBT13B_068_03 data to show the
; effects of thunderstorms on our data.

str=mrdfits('../data/HVCAGrid1_ses3_s10-36.fits',1)

setdisp
!p.font = 0

file = 'thunderstorms_getfs'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=8.5,ysize=8.5

ind0 = where(str.plnum eq 0)
displayc,str[ind0].data,min=-2,max=2,xtit='Channels',ytit='Integrations',tit='getfs reduced spectra of HVCAGrid1'

ps_close
ps2jpg,file+'.eps',/eps

stop

restore,'../data/HVCAGrid1_ses3_s10-36_red2.dat'

file = 'thunderstorms_basesub'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=8.5,ysize=8.5

displayc,smooth(final.comb_corr,[50,50],/edge_truncate,/nan),min=-0.03,max=0.03,$
         xtit='Channels',ytit='Integrations',tit='Baseline subtracted spectra of HVCAGrid1'

ps_close
ps2jpg,file+'.eps',/eps

stop

end
