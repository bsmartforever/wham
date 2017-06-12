smc_glon=302.0
smc_glat=-44.0

glon_min=smc_glon-10.
glon_max=smc_glon+10.
glat_min=smc_glat-10.
glat_max=smc_glat+10.

hi=exthi(glon_min,glon_max,glat_min,glat_max,/wham,/quiet)

subindex=where((Hi.glon le glon_max) $
           and (Hi.glon ge glon_min) $
           and (Hi.glat lt glat_max) $
           and (Hi.glat ge glat_min))
hi=hi(subindex)

hi_int=intmap(hi,vmin=75,vmax=300,/hi)* 1.8224e18

;forprint,hi.glon,hi.glat,hi_int,textout='hi.txt'

    zmin = 0 & zmax = 1.0e21
    whammap,hi,0,0,hi_int,/useimage,$  
    smgrid = 2.649166613817E-01,scale = 1.25,/cbottom,beamradius=0.3,$
    ymargin=[10.5,5.5], xmargin=[8,8], missing = missing, _extra = extra,$
    limits=[glon_min,glon_max,glat_min,glat_max],zmin = zmin, zmax = zmax, $
    smooth = 1,/nozero,/lin

curspect,map,hi, map2scale=.05


 end


