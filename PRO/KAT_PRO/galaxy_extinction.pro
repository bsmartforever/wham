function galaxy_extinction, glon, glat, radius=radius, vrange=vrange

;set to WHAM beam size if unset. 
if (NOT keyword_set(radius)) then radius=sqrt(1./!pi)
if (NOT keyword_set(vrange)) then vrange=[-150.,150]
vrange=[min(vrange),max(vrange)]

spectra=hi_spectra(glon,glat,radius=radius)

good_vel=where((spectra.vel ge vrange[0]) and (spectra.vel le vrange[1]))

column=int_tabulated(spectra.vel[good_vel],spectra.data[good_vel])*1.8224e18

Av=5.14e-22*column

return,exp(Av/2.5)

end