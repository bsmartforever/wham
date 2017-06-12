pro complexa_coverage2

; Plots Complex A coverage

setdisp
loadcol,3
!p.font = -1 ;0

;ps_open,'complexa_coverage2',/color,thick=1,/encap

restore,'~davidnidever/gauss/hiall2.dat'

ghess,str,'glon','glat',dx=0.5,dy=0.5,xr=[110,170],yr=[10,55],$
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
  polyfill,[-1,1,1,-1,-1]*0.5*arr2[i].lonsize+arr2[i].glon,$
        [-1,-1,1,1,-1]*0.5*arr2[i].latsize+arr2[i].glat,co=80,$
        /line_fill,spacing=0.1,orient=45
end

; Katie's GBT data
glon1 = [ 141.47242, 142.01068, 142.01068, 141.68772, 141.68772,$
          147.60854, 149.86922, 151.05338, 151.05338,  149.86922]
glat1 = [ 20.635816, 27.536885, 33.810584, 41.182181, 45.730613,$
          46.201140, 41.025339, 33.653742, 26.125303,  19.537919]
oplot,[glon1,glon1[0]],[glat1,glat1[0]],co=200
polyfill,[glon1,glon1[0]],[glat1,glat1[0]],co=200,$
         /line_fill,spacing=0.1,orient=-45

; Our GBT data
; 129.5 < GLON < 139.0
; 21.5 < GLAT < 29.0
oplot,[129.5,139.0,139.0,129.5,129.5],[21.5,21.5,29.0,29.0,21.5],$
      co=250
polyfill,[129.5,139.0,139.0,129.5,129.5],[21.5,21.5,29.0,29.0,21.5],$
      co=250,/line_fill,spacing=0.1,orient=-45

; Legend
legend,['Lockman','Verschuur & Nidever','Chynoweth'],textcolor=[80,250,200],$
       /top,/left


;ps_close

stop

end
