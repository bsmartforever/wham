pro make_plots_totcoldens

; Make total column density plot

fdir = '/Users/davidnidever/observing/gbt/GBT11B-082/'

fits_read,fdir+'data/grid_mstip_mag_pvsm.fits',cube,head
fits_arrays,head,mlon,mlat,vel
fits_read,fdir+'data/grid_mstip_mag_pvsm_rmsmap.fits',rmsmap,rmshead

step = median(slope(mlon))
dv = median(slope(vel))


tot3 = total(cube[*,*,90:250]>0.003,3)
bb = where(tot3 lt 0.6)
tot3[bb] = 0.0
col3 = tot3*dv*1.83e18

totcol = total(col3,2) * step

!p.font = 0

file = fdir+'plots/grid_mstip_mag_totcoldens'
ps_open,file,/color,thick=4,/encap

plot,mlon,totcol,xr=[-135,-170],/ylog,yr=[1e16,1e21],xtit='L!dMS!n',$
     ytit='N(HI) (deg atoms cm!u-2!n)',tit='Total Column Density',charsize=1.5
oplot,mlon,5.9e21*exp(mlon/19.3),co=250 

ps_close
ps2jpg,file+'.eps',/eps

stop

end
