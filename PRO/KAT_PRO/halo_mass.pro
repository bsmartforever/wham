function halo_mass,mstar

if mstar lt 20 then mstar=10.^mstar

;From Moster et al. 2010, Table 2
M1=10.^11.899
m_Mo=0.02820
beta=1.068
gamma=0.611

M_halo=10^makearr(1000,7,20)

;From equation 2 in Moster et al. 2010

right_hand=mstar/M_halo
left_hand=2*m_Mo*((M_halo/M1)^(-beta)+(M_halo/M1)^(gamma))^(-1.)

tmp=min(abs(1-alog10(right_hand)/alog10(left_hand)),loc)
;plot,right_hand,/ylog
;oplot,left_hand
;vline,loc,linestyle=1
;print,loc

M_halo_final=M_halo[loc]

return,alog10(M_halo_final)

end