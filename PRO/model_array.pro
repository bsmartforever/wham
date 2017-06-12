pro model_array, smc, iha, los

;First model is based on the intensity. As you get farther out, flair up. 
;Assume the average of 4.5 kpc for the center, then with minor flairing

los = make_array(n_elements(iha), 1, VALUE=0)

los[where(iha ge 1)]= 4500

los[where(iha lt 1)]=3500

los[where( iha lt .45)]=2500

los[where( iha lt .3)]=1500

los[where( iha lt .15)]=1500

los[where( iha lt .01)]=0

;print, los
print, max(iha)
center=where(iha EQ max(iha))
print,where(iha EQ max(iha))

print ,center

loncent=smc[center].glon
latcent=smc[center].glat

print, loncent, latcent

; I wrote this a week ago what was I doing here? I think this centers it?
del_lon=61*(smc.glon-loncent)*!dtor
del_lat=61*(smc.glat-latcent)*!dtor

print, del_lon
print, del_lat


;h
scale_length= make_array(n_elements(smc.glon), 1, VALUE=0)
;z
scale_height= make_array(n_elements(smc.glon), 1, VALUE=0)

scale_length= SQRT(del_lon^2+del_lat^2)
scale_height= ATAN(del_lon/del_lat)

FOR i= 0, n_elements(smc.glon)-1 DO BEGIN

IF FINITE(scale_height[i]) EQ 0 THEN scale_height[i]=0

ENDFOR
print, del_lon[0], del_lat[0]

;print, scale_length
;print, scale_height
z=.820
r=.43

p= exp(-scale_length/r)*exp(-scale_height/z)


;cgScatter2D, smc.glat, p, /ylog



END