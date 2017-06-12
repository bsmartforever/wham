Flux=FLTARR(102,100)
get_lun,lun

dir='$HOME/WHAM/ComplexA/photoionization/Radiaiton_Model/'
openr,lun,dir+'JBH_MW.dat'

readf,lun,flux

close,lun

imSize=SIZE(flux)
flux_kpc=rebin(flux,imSize[1]*2,imSize[2]*2)
size=200
flux_kpc=flux_kpc[0:size-1,0:size-1]

 ;tvimage,flux[0:999,0:999],/axis,xtitle='Galactic Radius [kpc]',ytitle='Galactic Height [kpc]'


flux_kpc_large=fltarr(size*2.0,size*2.0)
;bottom right
flux_kpc_large[0:size-1,0:size-1]=rotate(flux_kpc[0:size-1,0:size-1],2)
;top left
flux_kpc_large[0:size-1,size:size*2-1]=transpose(rotate(flux_kpc[0:size-1,0:size-1],3))
;bottom left
flux_kpc_large[size:size*2-1,0:size-1]=transpose(rotate(flux_kpc[0:size-1,0:size-1],1))
;top right
flux_kpc_large[size:size*2-1,size:size*2-1]=flux_kpc[0:size-1,0:size-1]


mw_flux=replicate({$
	flux:0.0,$
	x:0.0,$
	y:0.0,$
	z:0.0,$
	r:0.0,$
	glon:0.0,$
	glat:0.0,$
	mlon:0.0,$
	mlat:0.0,$
	d:0.0},$
	(size*2.)^2.)
R0=8.5
q=0L

for i=0, size*2.0-1 do begin
	for j=0, size*2.0-1 do begin
		;for k=0, size*2.0-1 do begin
			mw_flux[q].flux=flux_kpc_large[i,j]
			mw_flux[q].x=0; i-200.
			mw_flux[q].y=i-200. ;k-200.
			mw_flux[q].z=j-200.
			xyz2lbd,mw_flux[q].x,mw_flux[q].y,mw_flux[q].z,glon,glat,d
			mw_flux[q].r=sign(glon)*sqrt((i-200.)^2.0+(i-200.)^2.0)	
			;print,mw_flux[q].x,mw_flux[q].y,mw_flux[q].z,glon,glat,d
			;d=sqrt((mw_flux[q].x+R0)^2.0+mw_flux[q].y^2.0+mw_flux[q].z^2.0)
			;glat=asin(mw_flux[q].z/d)*180./!pi
			;glon=atan(mw_flux[q].y/(mw_flux[q].x+R0))*180./!pi
			gal2mag,glon,glat,mlon,mlat,wrap=0
			mw_flux[q].glon=glon
			mw_flux[q].glat=glat
			mw_flux[q].mlon=mlon
			mw_flux[q].mlat=mlat
			mw_flux[q].d=d	
	    	q=q+1
	    ;endfor
	endfor
endfor

;mw_flux[where(mw_flux.glon le 0)].glon=mw_flux[where(mw_flux.glon le 0)].glon+360.

;save,filename='mw_flux.sav',mw_flux

;slice=where((mw_flux.glat eq 0.))

;whammap,mw_flux[slice],0,0,mw_flux[slice].flux,/useimage,zmin=0.,zmax=5.0,smooth=1,loncenter=0.,/full


cgloadct,15,clip=[23,255],/reverse

zmin=0 & zmax=1e6
whammap,mw_flux,0,0,mw_flux.flux,/useimage,smooth=1,loncenter=0.,/full,/linear
whammap,mw_flux,0,0,mw_flux.flux,/useimage,smooth=1,loncenter=0.,/full,/linear,/magellanic,latcenter=-90

 colorbar, ncolors = !d.table_size-1, range = [zmin, zmax], $
   ;maxrange = 254, $
   position = [0.85, 0.35, 0.89, 0.82], $
   title = 'Intensity [R]', DIVISIONS=5,$
   format = '(f8.1)',VERTICAL=1,right=1,charsize=1.75




;works
triangulate,mw_flux.y,mw_flux.z,tri
result=trigrid(mw_flux.y,mw_flux.z,mw_flux.flux,tri,nx=400,ny=400)
tvimage,result


pause

lon=mw_flux.glon
lat=mw_flux.glat
f=mw_flux.flux

qhull, lon, lat, triangles, sphere = tsphere, /delaunay


              ;; Break out scale trimming since kringing can occassionally 
              ;; raise max pixels above input maximum. This is not a big
              ;; deal unless missing=255 (PS, white typcially) where
              ;; the missing then gets trimmed to 254 instead.
              
gd = griddata(lon, lat, f, /kriging, $
   tri = triangles, min_points = 10, missing = -1, $
   xout = xout, yout = yout, /grid, /sphere, /degrees) $
   < (!d.table_size-2-cbottom)




   Triangulate, lon, lat, tri, SPHERE=sTriangles, /DEGREES, FVALUE=f
   gridded = Trigrid(f, [0.5, 0.5], [0, -90, 360, 90], $
       SPHERE=sTriangles, XGRID=x, YGRID=y, /DEGREES)




   Triangulate, lon, lat, triangles
   gridded = GridData(lon, lat, f, /Sphere, Method='inversedistance', $
       /Degrees, Dimension=[400, 400], Triangles=triangles)



qhull, lon, lat, triangles, sphere = s, /delaunay
image=TRIGRID(lon,lat,f,sphere=s,$ 
   [-90,90,0,360], /DEGREES) 


f=mw_flux.flux
;TRIANGULATE, lon, lat, tr, 

gridData = GridData(lon, lat, f, $
	/kriging, triangles=triangles, $
	/grid,/sphere,/degrees,dim=[400,400])

gridData = Trigrid(lon, lat, f, $
	$ triangles, NX=400, NY=400, $
    XGrid=xgrid, YGrid=ygrid, Missing=!Values.F_NAN)


dEGREES 

nx=400
ny=400
result = trigrid(lon, lat, f, triangles,/sphere,/degrees,$
            nx=nx,ny=ny)


qhull, lon, lat, qtr, SPHERE = q_sphere, /DELAUNAY
TRIANGULATE, mw_flux.glon, mw_flux.glat, tr, SPHERE=s, FVALUE=mw_flux.flux, /DEGREES
triangulate,mw_flux.y,mw_flux.z,tri,sphere=test,fvalue=mw_flux.flux,/degrees
result=trigrid(mw_flux.flux,sphere=triangles,[2.,2.],[-180.,-90.,178.,90.],/degrees)
tvimage,result

end