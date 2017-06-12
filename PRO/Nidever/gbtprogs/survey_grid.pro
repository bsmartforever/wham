pro survey_grid,save=save


setdisp
loadcol,3
!p.font = 0

;file = 'survey_grid_060313c'
file = 'survey_grid_081313'
if keyword_set(save) then begin
  ps_open,file,/color,thick=4,/encap
  device,/inches,xsize=10.5,ysize=9.5
endif

;restore,'/Volumes/Data/net/halo/dln5q/doradus/research/gauss/hiall2.dat'
restore,'~/gauss/hiall2.dat'

;ghess,str,'glon','glat',dx=0.5,dy=0.5,xr=[110,170],yr=[10,55],$
ghess,str,'glon','glat',dx=0.5,dy=0.5,xr=[140,175],yr=[30,55],$
      /xflip,/column,/log,cut='cen lt -100',$
      tit='Column Density (VLSR<-100 km/s)'

;plot,[0],[0],/nodata,xr=[180,40],yr=[-90,90],xs=1,ys=1,$
;plot,[0],[0],/nodata,xr=[170,110],yr=[10,80],xs=1,ys=1,$
;     xtit='Galactic Longitude',ytit='Galactic Longitude'

loadct,39

; Lockman Planck data
arr = importascii('lockman_table.txt',/header)
narr = n_elements(arr)
gd = where(arr.glon ge 100 and arr.glon le 180 and $
           arr.glat ge 0 and arr.glat lt 60,ngd)
arr2 = arr[gd]
for i=0,ngd-1 do begin
  oplot,[-1,1,1,-1,-1]*0.5*arr2[i].lonsize+arr2[i].glon,$
        [-1,-1,1,1,-1]*0.5*arr2[i].latsize+arr2[i].glat,co=80 ;,linestyle=2
  ;polyfill,[-1,1,1,-1,-1]*0.5*arr2[i].lonsize+arr2[i].glon,$
  ;      [-1,-1,1,1,-1]*0.5*arr2[i].latsize+arr2[i].glat,co=80,$
  ;      /line_fill,spacing=0.1,orient=45
end

; Katie's GBT data
glon1 = [ 141.47242, 142.01068, 142.01068, 141.68772, 141.68772,$
          147.60854, 149.86922, 151.05338, 151.05338,  149.86922]
glat1 = [ 20.635816, 27.536885, 33.810584, 41.182181, 45.730613,$
          46.201140, 41.025339, 33.653742, 26.125303,  19.537919]
oplot,[glon1,glon1[0]],[glat1,glat1[0]],co=200
;polyfill,[glon1,glon1[0]],[glat1,glat1[0]],co=200,$
;         /line_fill,spacing=0.1,orient=-45

; Our GBT data
; 129.5 < GLON < 139.0
; 21.5 < GLAT < 29.0
oplot,[129.5,139.0,139.0,129.5,129.5],[21.5,21.5,29.0,29.0,21.5],$
      co=250
;polyfill,[129.5,139.0,139.0,129.5,129.5],[21.5,21.5,29.0,29.0,21.5],$
;      co=250,/line_fill,spacing=0.1,orient=-45

; Legend
al_legend,['Lockman','Verschuur & Nidever','Chynoweth'],textcolor=[80,250,200],$
       /top,/left

;; grid1
;; 150x130
;; 21.6667 hours for integration
;; 0.8666 hr for row changes
;; 0.2448 hr for (13) balances
;; total time = 22.774 hr
;
;nx = 150
;ny = 130
;glcen1 = 166.3
;gbcen1 = 39.0
;mkhdr,hd1,fltarr(nx,ny)
;sxaddpar,hd1,'CTYPE1','GLON-TAN'
;sxaddpar,hd1,'CRPIX1',nx/2
;sxaddpar,hd1,'CRVAL1',glcen1
;sxaddpar,hd1,'CDELT1',3.5/60.
;sxaddpar,hd1,'CTYPE2','GLON-TAN'
;sxaddpar,hd1,'CRPIX2',ny/2
;sxaddpar,hd1,'CRVAL2',gbcen1
;sxaddpar,hd1,'CDELT2',3.5/60.
;x = findgen(nx)#replicate(1,ny)
;y = replicate(1,nx)#findgen(ny)
;xyad,hd1,x,y,glon1,glat1
;oplot,glon1,glat1,ps=3,co=0



; I screwed up the script and we did 4' in GLAT!!!!!!!!!!!!

; grid1
; 150x115
; 21.6667 hours for integration
; 0.8666 hr for row changes
; 0.2448 hr for (13) balances
; total time = 22.774 hr

nx = 150
ny = 115
glcen1 = 166.3
gbcen1 = 39.0
mkhdr,hd1,fltarr(nx,ny)
sxaddpar,hd1,'CTYPE1','GLON-TAN'
sxaddpar,hd1,'CRPIX1',nx/2
sxaddpar,hd1,'CRVAL1',glcen1
sxaddpar,hd1,'CDELT1',3.5/60.
sxaddpar,hd1,'CTYPE2','GLAT-TAN'
sxaddpar,hd1,'CRPIX2',ny/2
sxaddpar,hd1,'CRVAL2',gbcen1
sxaddpar,hd1,'CDELT2',4.0/60.
x = findgen(nx)#replicate(1,ny)
y = replicate(1,nx)#findgen(ny)
xyad,hd1,x,y,glon1,glat1
oplot,glon1,glat1,ps=3,co=0


; Balance positions, along center, every 10 rows
xx = fltarr(13)+75
yy = findgen(13)*10
xyad,hd1,xx,yy,gl,gb
glactc,ra,dec,2000.,gl,gb,2,/deg
glactc,ra1,dec1,2000.,glcen1,gbcen1,2,/deg
print,'HVCAGrid1         '+strmid(ten2sexig(ra1/15.),0,10)+'     '+strmid(ten2sexig(dec1),0,10)+'  0.0'
printline,'Grid1Balance'+strtrim(indgen(13)+1,2)+'     '+strmid(ten2sexig(ra/15.),0,10)+'     '+strmid(ten2sexig(dec),0,10)+'  0.0'


; grid2
; 145x50
; 8.05 hours for integration
; 0.333 hr for row changes
; 0.094 hr for (5) balances
; total time = 8.477 hr

nx = 145
ny = 50
glcen2 = 155.2
gbcen2 = 41.3
mkhdr,hd2,fltarr(nx,ny)
sxaddpar,hd2,'CTYPE1','GLON-TAN'
sxaddpar,hd2,'CRPIX1',nx/2
sxaddpar,hd2,'CRVAL1',glcen2
sxaddpar,hd2,'CDELT1',3.5/60.
sxaddpar,hd2,'CTYPE2','GLAT-TAN'
sxaddpar,hd2,'CRPIX2',ny/2
sxaddpar,hd2,'CRVAL2',gbcen2
sxaddpar,hd2,'CDELT2',3.5/60.
x = findgen(nx)#replicate(1,ny)
y = replicate(1,nx)#findgen(ny)
xyad,hd2,x,y,glon2,glat2
oplot,glon2,glat2,ps=3,co=150

; Balance positions, along center, every 10 rows
xx = fltarr(ny/10)+nx/2
yy = findgen(ny/10)*10
xyad,hd2,xx,yy,gl,gb
print,'HVCAGrid2         '+strtrim(glcen2,2)+'     '+strtrim(gbcen2,2)+'  0.0'
printline,'Grid2Balance'+strtrim(indgen(13)+1,2)+'     '+strtrim(gl,2)+'     '+strtrim(gb,2)+'  0.0'


; grid3
; 200x130
; 28.889 hours for integration
; 0.8667 hr for row changes
; 0.358 hr for (19) balances
; total time = 30.11 hr

nx = 200  ; 195
ny = 130  ; 125
glcen3 = 155.8  ;155.7
gbcen3 = 46.3   ;46.2
mkhdr,hd3,fltarr(nx,ny)
sxaddpar,hd3,'CTYPE1','GLON-TAN'
sxaddpar,hd3,'CRPIX1',nx/2
sxaddpar,hd3,'CRVAL1',glcen3
sxaddpar,hd3,'CDELT1',3.5/60.
sxaddpar,hd3,'CTYPE2','GLAT-TAN'
sxaddpar,hd3,'CRPIX2',ny/2
sxaddpar,hd3,'CRVAL2',gbcen3
sxaddpar,hd3,'CDELT2',3.5/60.
x = findgen(nx)#replicate(1,ny)
y = replicate(1,nx)#findgen(ny)
xyad,hd3,x,y,glon3,glat3
oplot,glon3,glat3,ps=3,co=250



; Balance positions, along center, every 10 rows
xx = fltarr(ny/10)+nx/2
yy = findgen(ny/10)*10
xyad,hd2,xx,yy,gl,gb
print,'HVCAGrid3         '+strtrim(glcen3,2)+'     '+strtrim(gbcen3,2)+'  0.0'
printline,'Grid3Balance'+strtrim(indgen(13)+1,2)+'     '+strtrim(gl,2)+'     '+strtrim(gb,2)+'  0.0'


if keyword_set(save) then begin
  ps_close
  ps2jpg,file+'.eps',/eps
endif

; In the proposal I put down 18x4h sessions.
; If we have 18 sessions with 30min. setup each then that's
; another 9hr for setups.

; Total time
; grid1 = 22.77hr
; grid2 = 8.48hr
; grid3 = 30.11hr
; setup = 9hr
; total = 70.36

; 
; 24s for row change
; 1.13 min. for balance (roughly every hour)



; From notes 08/07/2010
; OVERHEAD
; Each row takes 12.73333 minutes and the balance takes 1.1333 min.
; So 5 rows with a balance take 64.8 minutes or 12.96 minutes per row
; The integration time for a row (10*74) is 12.3333 min. which means
; that there's an extra 0.40 min. or 24 sec. to move between rows.
; So changing the integration time to 9s per row brings down the
; time per row by 74 seconds.  So that saves us about 4.11 hours.
; We need about 7 hours to get another 30 rows in and finish the
; entire survey area.

; 1.13 min for balance
; 24 sec moving between rows

; BALANCING
; For the thin grids we can balance on their centers.
; For the big grid we'll need some balance points every degree
;   at the middle RA.  So need ~12-13 of them.

stop

end
