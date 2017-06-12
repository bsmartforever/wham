pro calc_phot

filename='/homelcl/osrseng/050330/SPEC/ORP/s050330_a011001_datset_Kn3_020_0.fits'
im=readfits(filename)
im2=im[50:399,*,*]
im_collapse=cmapply('USER:CMMEDIAN', im2, 1)

xcenter=39
ycenter=28

aper=15

aper_top=28+aper
aper_bottom=28-aper
aper_left=39-aper
aper_right=39

aper_xc = xcenter-aper_left			;Object's xcenter in subarray
aper_yc = ycenter-aper_bottom			;Object's ycenter in subarray

aper_xs = aper_right-aper_left+1		;Number of pixels X direction
aper_ys = aper_top-aper_bottom+1		;Number of pixels Y direction

;Extract subarray from image
aperbuf = im_collapse[aper_left:aper_right, aper_bottom:aper_top] 
skyarr = im_collapse[5:10, 5:10]

aper_dxsq = (dindgen(aper_xs)-aper_xc)^2
aper_rsq = dblarr(aper_xs, aper_ys)
for i=0, aper_ys-1 do aper_rsq[0,i]=aper_dxsq+(i-aper_yc)^2

aper_valid=where((aper_rsq le (double(aper)^2))) 

; get total value of pixels in aperture
counts=total(aper_valid, /double)
; get number of pixels in aperture
numpix_aper=n_elements(aper_valid)

; find mean sky value
mean_sky=mean(skyarr, /double)

; subtract sky from aperture counts
cor_counts=(counts-mean_sky*double(numpix_aper))

stop

end
