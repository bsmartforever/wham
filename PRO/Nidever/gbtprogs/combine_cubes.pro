pro combine_cubes

; Combine all of the Complex C cubes
; Verschuur & Nidever cube
; 3 Lockman cubes
; Chynoweth cube
; New GBT13B-068 cube

; Put all on same velocity grid
; interpolate onto final galactic grid.

head = headfits('~/observing/gbt/GBT13B-068/data/grid_HVCAGrid1_gal_bigcube.fits')
;CDELT1  =      0.0583333333333 /
;CRPIX1  =                   78 /
;CRVAL1  =        166.000000000 /
;CTYPE1  = 'GLON-TAN'           /
;CDELT2  =      0.0583333333333 /
;CRPIX2  =                   68 /
;CRVAL2  =        39.0000000000 /
;CTYPE2  = 'GLAT-TAN'           /
;CDELT3  =       0.161025620000 /
;CRPIX3  =                    1 /
;CRVAL3  =       -458.921530000 /
;CTYPE3  = 'VELO-LSR'           /

; velocity: -300, +100 at 0.8 km/s
sxaddpar,head,'CRPIX3',1L
sxaddpar,head,'CRVAL3',-300.0d0
sxaddpar,head,'CDELT3',0.8d0
sxaddpar,head,'NAXIS3',501L

; glon: 128-171
; glat: 18-53
sxaddpar,head,'CRPIX1',1L
sxaddpar,head,'CRVAL1',128.0d0
sxaddpar,head,'NAXIS1',738L
sxaddpar,head,'CRPIX2',1L
sxaddpar,head,'CRVAL2',18.0d0
sxaddpar,head,'NAXIS2',600L
fits_arrays,head,gl,gb,vel
nx = n_elements(gl)
ny = n_elements(gb)
nvel = n_elements(vel)


; 1. Verschuur & Nidever
;  0.48 km/s resolution
;fits_read,'~/observing/gbt/data/grid_gbt_ca_gal.fits',cube1,head1
;fits_arrays,head1,glon1,glat1,vel1
;sz1 = size(cube1)
;; bin 2x
;cube1 = rebin(cube1,sz1[1],sz1[2],sz1[3]/2)
;vel1 = rebin(vel1,sz1[3]/2)
;sxaddpar,head1,'CRVAL3',double(vel1[0])
;sxaddpar,head1,'CDELT3',double(vel1[1]-vel1[0])
;sxaddpar,head1,'NAXIS3',n_elements(vel1)
;fits_arrays,head1,glon1,glat1,vel1
;sz1 = size(cube1)
;cube1b = fltarr(sz1[1],sz1[2],nvel)
;; interpolate onto final velocity grid
;ind = where(vel ge min(vel1) and vel le max(vel1),nind)
;for i=0,sz1[1]-1 do begin
;  for j=0,sz1[2]-1 do begin
;    if max(cube1[i,j,*]) ne 0.0 then begin
;      nspec = spline(reverse(vel1),reverse(reform(cube1[i,j,*])),vel[ind])
;      cube1b[i,j,ind] = nspec
;    endif
;  endfor
;endfor
;head1b = head1
;sxaddpar,head1b,'CRVAL3',sxpar(head,'CRVAL3')
;sxaddpar,head1b,'CRPIX3',sxpar(head,'CRPIX3')
;sxaddpar,head1b,'CDELT3',sxpar(head,'CDELT3')
;sxaddpar,head1b,'NAXIS3',sxpar(head,'NAXIS3')
;fits_write,'../data/grid_gbt_ca_gal_velinterp.fits',cube1b,head1b
;fits_read,'../data/grid_gbt_ca_gal_velinterp.fits',cube1b,head1b
;fits_arrays,head1b,glon1,glat1
;sz1 = size(cube1b)
;
;; Interpolate onto final grid
;cube1c = fltarr(nx,ny,nvel)
;glon1b = glon1#replicate(1,sz1[2])
;glat1b = replicate(1,sz1[1])#glat1
;TRIANGULATE, glon1b, glat1b, tr, b
;for i=0,nvel-1 do begin
;  if i mod 20 eq 0 then print,i
;  limits = [min(gl), min(gb), max(gl), max(gb)]
;  steps = [sxpar(head,'CDELT1'), sxpar(head,'CDELT2')]
;  im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,/QUINTIC,missing=0.0)
;  cube1c[*,*,i] = im  
;endfor
;;fits_write,'../data/grid_gbt_ca_gal_final.fits',cube1c,head
;stop

; 2. Lockman cubes

; spc_gls.fits
; 0.8 km/s velocity resolution
; 3.5' spatial step

;fits_read,'~/observing/gbt/planck/spc_gls.fits',cube1,head1
;fits_arrays,head1,glon1,glat1,vel1
;vel1 /= 1e3
;sz1 = size(cube1)
;cube1b = fltarr(sz1[1],sz1[2],nvel)
;; interpolate onto final velocity grid
;ind = where(vel ge min(vel1) and vel le max(vel1),nind)
;for i=0,sz1[1]-1 do begin
;  for j=0,sz1[2]-1 do begin
;    if max(cube1[i,j,*]) ne 0.0 then begin
;      nspec = spline(reverse(vel1),reverse(reform(cube1[i,j,*])),vel[ind])
;      cube1b[i,j,ind] = nspec
;    endif
;  endfor
;endfor
;head1b = head1
;sxaddpar,head1b,'CTYPE3',sxpar(head,'CTYPE3')
;sxaddpar,head1b,'CRVAL3',sxpar(head,'CRVAL3')
;sxaddpar,head1b,'CRPIX3',sxpar(head,'CRPIX3')
;sxaddpar,head1b,'CDELT3',sxpar(head,'CDELT3')
;sxaddpar,head1b,'NAXIS3',sxpar(head,'NAXIS3')
;sxaddpar,head1b,'CUNIT3','KM/S'
;fits_write,'../data/spc_gls_velinterp.fits',cube1b,head1b
;stop
;fits_read,'../data/spc_gls_velinterp.fits',cube1b,head1b
;fits_arrays,head1b,glon1,glat1
;sz1 = size(cube1b)
;
;; Interpolate onto final grid
;cube1c = fltarr(nx,ny,nvel)
;xx = findgen(sz1[1])#replicate(1,sz1[2])
;yy = replicate(1,sz1[1])#findgen(sz1[2])
;xyad,head1b,xx,yy,glon1b,glat1b
;TRIANGULATE, glon1b, glat1b, tr, b
;for i=0,nvel-1 do begin
;  if i mod 20 eq 0 then print,i
;  limits = [min(gl), min(gb), max(gl), max(gb)]
;  steps = [sxpar(head,'CDELT1'), sxpar(head,'CDELT2')]
;  im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,/QUINTIC,missing=0.0)
;  bd = where(finite(im) eq 0,nbd)
;  if nbd gt 0 then im[bd]=0
;  cube1c[*,*,i] = im  
;endfor
;fits_write,'../data/spc_gls_final.fits',cube1c,head
;stop


; uma_gls.fits
; 0.8 km/s velocity resolution
; 3.5' spatial step
;fits_read,'~/observing/gbt/planck/uma_gls.fits',cube1,head1
;fits_arrays,head1,glon1,glat1,vel1
;vel1 /= 1e3
;sz1 = size(cube1)
;cube1b = fltarr(sz1[1],sz1[2],nvel)
;; interpolate onto final velocity grid
;ind = where(vel ge min(vel1) and vel le max(vel1),nind)
;for i=0,sz1[1]-1 do begin
;  for j=0,sz1[2]-1 do begin
;    if max(cube1[i,j,*]) ne 0.0 then begin
;      nspec = spline(reverse(vel1),reverse(reform(cube1[i,j,*])),vel[ind])
;      cube1b[i,j,ind] = nspec
;    endif
;  endfor
;endfor
;head1b = head1
;sxaddpar,head1b,'CTYPE3',sxpar(head,'CTYPE3')
;sxaddpar,head1b,'CRVAL3',sxpar(head,'CRVAL3')
;sxaddpar,head1b,'CRPIX3',sxpar(head,'CRPIX3')
;sxaddpar,head1b,'CDELT3',sxpar(head,'CDELT3')
;sxaddpar,head1b,'NAXIS3',sxpar(head,'NAXIS3')
;sxaddpar,head1b,'CUNIT3','KM/S'
;;fits_write,'../data/uma_gls_velinterp.fits',cube1b,head1b
;stop
;fits_read,'../data/uma_gls_velinterp.fits',cube1b,head1b
;fits_arrays,head1b,glon1,glat1
;sz1 = size(cube1b)
;
;; Interpolate onto final grid
;cube1c = fltarr(nx,ny,nvel)
;xx = findgen(sz1[1])#replicate(1,sz1[2])
;yy = replicate(1,sz1[1])#findgen(sz1[2])
;xyad,head1b,xx,yy,glon1b,glat1b
;TRIANGULATE, glon1b, glat1b, tr, b
;for i=0,nvel-1 do begin
;  if i mod 20 eq 0 then print,i
;  limits = [min(gl), min(gb), max(gl), max(gb)]
;  steps = [sxpar(head,'CDELT1'), sxpar(head,'CDELT2')]
;  ;im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,/QUINTIC,missing=0.0)
;  im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,missing=0.0)
;  bd = where(finite(im) eq 0,nbd)
;  if nbd gt 0 then im[bd]=0
;  cube1c[*,*,i] = im  
;endfor
;;fits_write,'../data/uma_gls_final.fits',cube1c,head
;stop


; umaeast_gls.fits
; 0.8 km/s velocity resolution
; 3.5' spatial step
;
;fits_read,'~/observing/gbt/planck/umaeast_gls.fits',cube1,head1
;fits_arrays,head1,glon1,glat1,vel1
;vel1 /= 1e3
;sz1 = size(cube1)
;cube1b = fltarr(sz1[1],sz1[2],nvel)
;; interpolate onto final velocity grid
;ind = where(vel ge min(vel1) and vel le max(vel1),nind)
;for i=0,sz1[1]-1 do begin
;  for j=0,sz1[2]-1 do begin
;    if max(cube1[i,j,*]) ne 0.0 then begin
;      nspec = spline(reverse(vel1),reverse(reform(cube1[i,j,*])),vel[ind])
;      cube1b[i,j,ind] = nspec
;    endif
;  endfor
;endfor
;head1b = head1
;sxaddpar,head1b,'CTYPE3',sxpar(head,'CTYPE3')
;sxaddpar,head1b,'CRVAL3',sxpar(head,'CRVAL3')
;sxaddpar,head1b,'CRPIX3',sxpar(head,'CRPIX3')
;sxaddpar,head1b,'CDELT3',sxpar(head,'CDELT3')
;sxaddpar,head1b,'NAXIS3',sxpar(head,'NAXIS3')
;sxaddpar,head1b,'CUNIT3','KM/S'
;;fits_write,'../data/umaeast_gls_velinterp.fits',cube1b,head1b
;stop
;fits_read,'../data/umaeast_gls_velinterp.fits',cube1b,head1b
;fits_arrays,head1b,glon1,glat1
;sz1 = size(cube1b)
;
;; Interpolate onto final grid
;cube1c = fltarr(nx,ny,nvel)
;xx = findgen(sz1[1])#replicate(1,sz1[2])
;yy = replicate(1,sz1[1])#findgen(sz1[2])
;xyad,head1b,xx,yy,glon1b,glat1b
;TRIANGULATE, glon1b, glat1b, tr, b
;for i=0,nvel-1 do begin
;  if i mod 20 eq 0 then print,i
;  limits = [min(gl), min(gb), max(gl), max(gb)]
;  steps = [sxpar(head,'CDELT1'), sxpar(head,'CDELT2')]
;  ;im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,/QUINTIC,missing=0.0)
;  im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,missing=0.0)
;  bd = where(finite(im) eq 0,nbd)
;  if nbd gt 0 then im[bd]=0
;  cube1c[*,*,i] = im  
;endfor
;;fits_write,'../data/umaeast_gls_final.fits',cube1c,head
;stop


; 3. Chynoweth cube
; vhelio, 5 km/s resolution
; 1.75' spatial resolution, RA/DEC--SIN
;fits_read,'~/observing/gbt/chynoweth/M81fil_compa.fits',cube1,head1
;fits_arrays,head1,glon1,glat1,vhelio1
;vhelio1 /= 1e3  ; convert to km/s
;bd = where(finite(cube1) eq 0,nbd)
;cube1[bd] = 0
;sz1 = size(cube1)
;cube1b = fltarr(sz1[1],sz1[2],nvel)
;; interpolate onto final velocity grid
;for i=0,sz1[1]-1 do begin
;  if i mod 20 eq 0 then print,i
;  for j=0,sz1[2]-1 do begin
;    if max(cube1[i,j,*]) ne 0.0 then begin
;      xyad,head1,i,j,ra,dec
;      helio2lsr,vhelio1[0],v1,ra=ra,dec=dec,/kinematic  ; WSRT in vhelio, convert to vlsr
;      voff = v1-vhelio1[0]
;      vlsr1 = vhelio1+voff
;      spec = reform(cube1[i,j,*])
;      ind = where(vel ge min(vlsr1) and vel le max(vlsr1),nind)
;      nspec = spline(reverse(vlsr1),reverse(spec),vel[ind])
;      cube1b[i,j,ind] = nspec
;    endif
;  endfor
;endfor
;head1b = head1
;sxaddpar,head1b,'CTYPE3',sxpar(head,'CTYPE3')
;sxaddpar,head1b,'CRVAL3',sxpar(head,'CRVAL3')
;sxaddpar,head1b,'CRPIX3',sxpar(head,'CRPIX3')
;sxaddpar,head1b,'CDELT3',sxpar(head,'CDELT3')
;sxaddpar,head1b,'NAXIS3',sxpar(head,'NAXIS3')
;sxaddpar,head1b,'CUNIT3','KM/S'
;;fits_write,'../data/chynoweth_velinterp.fits',cube1b,head1b
;stop
;fits_read,'../data/chynoweth_velinterp.fits',cube1b,head1b
;fits_arrays,head1b,glon1,glat1
;sz1 = size(cube1b)
;
;; Interpolate onto final grid
;cube1c = fltarr(nx,ny,nvel)
;xx = findgen(sz1[1])#replicate(1,sz1[2])
;yy = replicate(1,sz1[1])#findgen(sz1[2])
;xyad,head1b,xx,yy,ra,dec
;glactc,ra,dec,2000.0,glon1b,glat1b,1,/deg
;TRIANGULATE, glon1b, glat1b, tr, b
;limits = [min(gl), min(gb), max(gl), max(gb)]
;steps = [sxpar(head,'CDELT1'), sxpar(head,'CDELT2')]
;for i=0,nvel-1 do begin
;  if i mod 20 eq 0 then print,i
;  ;im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,/QUINTIC,missing=0.0)
;  im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,missing=0.0)
;  bd = where(finite(im) eq 0,nbd)
;  if nbd gt 0 then im[bd]=0
;  cube1c[*,*,i] = im  
;endfor
;;fits_write,'../data/chynoweth_final.fits',cube1c,head

; 4. GBT13B-068 cube

;  0.16 km/s resolution
;;fits_read,'../data/grid_HVCAGrid1_gal_bigcube.fits',cube1,head1
;fits_read,'../data/grid_complexa_gal.fits',cube1,head1
;fits_arrays,head1,glon1,glat1,vel1
;sz1 = size(cube1)
;; bin 5x
;cube1 = rebin(cube1,sz1[1],sz1[2],sz1[3]/5)
;vel1 = rebin(vel1,sz1[3]/5)
;sxaddpar,head1,'CRVAL3',double(vel1[0])
;sxaddpar,head1,'CDELT3',double(vel1[1]-vel1[0])
;sxaddpar,head1,'NAXIS3',n_elements(vel1)
;fits_arrays,head1,glon1,glat1,vel1
;sz1 = size(cube1)
;cube1b = fltarr(sz1[1],sz1[2],nvel)
;; interpolate onto final velocity grid
;ind = where(vel ge min(vel1) and vel le max(vel1),nind)
;for i=0,sz1[1]-1 do begin
;  if i mod 20 eq 0 then print,i
;  for j=0,sz1[2]-1 do begin
;    if max(cube1[i,j,*]) ne 0.0 then begin
;      nspec = spline(vel1,reform(cube1[i,j,*]),vel[ind])
;      cube1b[i,j,ind] = nspec
;    endif
;  endfor
;endfor
;head1b = head1
;sxaddpar,head1b,'CRVAL3',sxpar(head,'CRVAL3')
;sxaddpar,head1b,'CRPIX3',sxpar(head,'CRPIX3')
;sxaddpar,head1b,'CDELT3',sxpar(head,'CDELT3')
;sxaddpar,head1b,'NAXIS3',sxpar(head,'NAXIS3')
;fits_write,'../data/grid_complexa_gal_velinterp.fits',cube1b,head1b
;fits_write,'../data/HVCAGrid1_velinterp.fits',cube1b,head1b
;stop
;fits_read,'../data/grid_complexa_gal_velinterp.fits',cube1b,head1b
;fits_read,'../data/HVCAGrid1_velinterp.fits',cube1b,head1b
;fits_arrays,head1b,glon1,glat1
;sz1 = size(cube1b)
;
;; Interpolate onto final grid
;cube1c = fltarr(nx,ny,nvel)
;glon1b = glon1#replicate(1,sz1[2])
;glat1b = replicate(1,sz1[1])#glat1
;TRIANGULATE, glon1b, glat1b, tr, b
;for i=0,nvel-1 do begin
;  if i mod 20 eq 0 then print,i
;  limits = [min(gl), min(gb), max(gl), max(gb)]
;  steps = [sxpar(head,'CDELT1'), sxpar(head,'CDELT2')]
;  ;im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,/QUINTIC,missing=0.0)
;  im = TRIGRID(glon1b,glat1b,reform(cube1b[*,*,i]), tr, steps,limits,missing=0.0)
;  bd = where(finite(im) eq 0,nbd)
;  if nbd gt 0 then im[bd]=0
;  cube1c[*,*,i] = im  
;endfor
;fits_write,'../data/grid_complexa_gal_final.fits',cube1c,head
;fits_write,'../data/HVCAGrid1_final.fits',cube1c,head
;stop


; Combine ALL
;files = '../data/'+['grid_gbt_ca_gal','spc_gls','uma_gls','umaeast_gls','chynoweth','HVCAGrid1']+'_final.fits'
;files = '../data/'+['grid_gbt_ca_gal','spc_gls','uma_gls','umaeast_gls','HVCAGrid1']+'_final.fits'
;files = '../data/'+['grid_gbt_ca_gal','spc_gls','uma_gls','umaeast_gls','chynoweth','grid_complexa_gal']+'_final.fits'
files = '../data/'+['grid_gbt_ca_gal','spc_gls','uma_gls','umaeast_gls','grid_complexa_gal']+'_final.fits'
nfiles = n_elements(files)

for i=0,nfiles-1 do begin
  print,files[i]
  fits_read,files[i],cube1,head1
  if i eq 0 then begin
    cube = cube1*0
    head = head1
    sz = size(cube)
    nobs = lonarr(sz[1],sz[2])
  endif
  cube += cube1
  tot = total(cube1,3)
  nobs += long(tot ne 0.0)
endfor

; problem with Chynoweth data

; Add in Chynoweth data where we have NOTHING ELSE
fits_read,'../data/chynoweth_final.fits',cube1,head1
tot = total(cube1,3)
mask = long( (nobs eq 0) and (tot ne 0.0) )
nobs += mask
for i=0,sz[3]-1 do cube[*,*,i]+=cube1[*,*,i]*mask

; normalize by nobs
for i=0,sz[3]-1 do cube[*,*,i]/=(nobs>1)

; newest ones
;fits_write,'../data/combine_final.fits',cube,head

; older ones moved to data/combine_bak/
;fits_write,'../data/combine_final.fits',cube,head
;fits_write,'../data/combine_final_nochynoweth.fits',cube,head
;fits_write,'../data/combine_final_all.fits',cube,head

; Final map
tot = total(cube[*,*,0:274],3)
dvel = 0.8 ; km/s
col = tot*1.83e18*dvel
!p.font = 0
file = 'complexa_coldens'
ps_open,file,/color,thick=4,/encap
device,/inches,xsize=7.5,ysize=8.0
logcol = alog10(col>1)
bd = where(tot eq 0,nbd)
logcol[bd] = -1
displayc,logcol,gl,gb,/xflip,min=18,max=21,xtit='GLON',ytit='GLAT',tit='Column Density',maskv=-1,maskc=255
ps_close


stop

end
