pro plot3d,xarr,yarr,zarr,colorarr,rotate=rotate,npts=npts,$
           psize=psize,stp=stp,anim=anim,file=file,ax=ax,$
           az=az,top=top,nosphere=nosphere,xr=xr,yr=yr,zr=zr

; Plots data in 3D and you can have it rotate as well.

if n_elements(nosphere) eq 0 then nosphere=1
if not keyword_set(colorarr) then colorarr=xarr*0.+255.

if n_params() eq 0 then begin
  print,'Syntax - plot3d,xarr,yarr,zarr,colorarr,rotate=rotate,npts=npts,'
  print,'         psize=psize,stp=stp,anim=anim,file=file,ax=ax,'
  print,'         az=az'
  return
endif

if !d.name ne 'PS' then begin
  set_plot,'x'
  window,0,xsize=640,ysize=512
endif else begin
  device,xsize=6,ysize=6,/inches
endelse

  ; colors for my screen
  red = 250
  lred = 210
  green = 190000
  orange = 310000
  yellow = 450000
  blue = -25000
  lblue = -15000
  purple = -20000
  white = -1
  backgr = 0.
  coarr = [green,orange,yellow,blue,purple,lred,lblue]

  ; setting for postscript
  if keyword_set(save) then begin
    loadct,13
    black=0
    purple=30
    blue=60
    aqua=80
    green=155   ;135
    yellow=200
    orange=225
    white=0
    red=300
    lred=240
    ;backg=white
    backg=yellow
    coarr = [green,orange,yellow,blue,purple,lred,aqua]
  endif

  loadct,39
  !p.multi=0
  xd = max(xarr)-min(xarr)
  yd = max(yarr)-min(yarr)
  zd = max(zarr)-min(zarr)
  ;fac=0.5
  ;xr=[min(lxarr)-xd*fac,max(lxarr)+xd*fac]
  ;yr=[min(lyarr)-yd*fac,max(lyarr)+yd*fac]
  ;zr=[min(lzarr)-zd*fac*1.5,max(lzarr)+zd*fac]
  ;xr = [-100.,100.]
  ;yr = [-100.,100.]
  ;zr = [-100.,100.]

  if not keyword_set(xr) then xr = [min(xarr),max(xarr)]
  if not keyword_set(yr) then yr = [min(yarr),max(yarr)]
  if not keyword_set(zr) then zr = [min(zarr),max(zarr)]

  if not keyword_set(npts) then npts=10

  sphere,1.,sx,sy,sz,npts=10
  if not keyword_set(psize) then psize = 0.01
  sx = sx*xd*psize
  sy = sy*yd*psize
  sz = sz*zd*psize

colorarr2 = colorarr
if keyword_set(top) then begin
  bd = where(colorarr2 ge top,nbd)
  if nbd gt 0 then colorarr2(bd) = top
endif

if keyword_set(rotate) then nrot = 50 else nrot=1
n = n_elements(xarr)
if not keyword_set(colorarr) then colorarr = fltarr(n)+255.
;colorarr2 = colorarr
colorarr2 = colorarr2/max(colorarr2)*254.
;colorarr2 = colorarr2*200.+5.

for i=0,nrot-1 do begin

  if i/10 eq i/10. then print,i

  ;daz = 80./npts
  daz = 360./nrot
  az0 = 360.  ;330.  ; 275.  ;-45.

  if not keyword_set(ax) then ax =  20.   ;45

  if not keyword_set(rotate) then begin
    if not keyword_set(az) then az = 45.
  endif else begin
    az = daz*i+az0
  endelse

  ;ax=0
  ;az=0

  ;az = 45.
  ;charsize = 1d-10   ; 2.5
  surface,dist(10),/nodata,az=az,ax=ax,/save,xr=xr,yr=yr,zr=zr,$
    ; xtit=' ',ytit=' ',ztit=' ',charsize=2.5,xs=1,ys=1,zs=1,$
    ; xtickname=[''],xticks=1,yticks=1,zticks=1,charthick=1d-10
    ; xtit='X',ytit='Y',ztit='Z',charsize=2.5,xs=1,ys=1,zs=1
    ; xtit='X',ytit='Y',ztit='Z',charsize=2.5,xs=4,ys=4,zs=4
     xtit='X',ytit='Y',ztit='Z',charsize=2.5,xs=1,ys=1,zs=1

  ;stop

;  ; x-axes
;  plots,[-100,100],[-100,-100],[-100,-100],/t3d
;  plots,[-100,100],[100,100],[-100,-100],/t3d
;  plots,[-100,100],[-100,-100],[100,100],/t3d
;  plots,[-100,100],[100,100],[100,100],/t3d
;
;  ; y-axes
;  plots,[-100,-100],[-100,100],[-100,-100],/t3d
;  plots,[100,100],[-100,100],[-100,-100],/t3d
;  plots,[-100,-100],[-100,100],[100,100],/t3d
;  plots,[100,100],[-100,100],[100,100],/t3d
;
;  ; z-axes
;  plots,[100,100],[100,100],[-100,100],/t3d
;  plots,[-100,-100],[100,100],[-100,100],/t3d
;  plots,[100,100],[-100,-100],[-100,100],/t3d
;  plots,[-100,-100],[-100,-100],[-100,100],/t3d

  ;plots,xarr,yarr,zarr,/t3d,ps=3

  vec = [0.,1.,0.]
  rotate_xyz,vec,nvec,ax,az
  matr = transpose( [[xarr],[yarr],[zarr]] )

  s = reform( nvec#matr )
  si = reverse(sort(s))

  ;stop

  if not keyword_set(nosphere) then begin
    for j=0.,n-1 do plots,xarr(si(j))+sx,yarr(si(j))+sy,zarr(si(j))+sz,$
                          color=colorarr2(si(j)),/t3d
  endif else begin
    for j=0.,n-1 do plots,xarr(si(j)),yarr(si(j)),zarr(si(j)),ps=3,$
                          color=colorarr2(si(j)),/t3d
  endelse  


 ; for j=0.,n-1 do plots,xarr(j)+sx,yarr(j)+sy,zarr(j)+sz,color=colorarr2(j),/t3d

  if keyword_set(anim) then begin
    if not keyword_set(file) then file='plot3d'
    num = strtrim(long(i),2)
    if num lt 10 then num='0'+num
    if num lt 100 then num='0'+num
    filename = file+'_'+num+'.tiff'

    ; creating tiff image
    maketiff,filename
    ;tiff = tvrd(true=1)
    ;tiff = reverse(tiff,3)
    ;write_tiff,file+'/'+filename,tiff,1
  endif

  wait,0.2

  if keyword_set(stp) then stop
  ;stop

end

if keyword_set(stp) then stop

end
