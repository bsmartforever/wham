pro jbh_mw, size, mcs=mcs, egb=egb, bw=bw, color=color, $
            levels=levels, clabels=clabels,$
            small=small,yflip=yflip, xflip=xflip,$
            top=top, bottom=bottom,left=left,right=right,$
            thick=thick,charthick=charthick,charsize=charsize

; size - The span of the Galactic Radius and the Galactic Height (kpc)
; egb - Display MW+EGB ionizing photons
; bw - Set color to black and white
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get and save current color table
tvlct, r, g, b, /get

p_temp=!p

if NOT keyword_set(size) then size=200.0
if size gt 200.0 then begin
   print,'*** Size must equal 200 kpc or less! ***'
   print,'   *** Setting size to 200 kpc. ***'
   size=200.0
endif

if keyword_set(bw) then begin
   cgloadct,0 ,/reverse
   GAMMA_CT,2.25
   !p.background=fsc_color('white')
endif

if keyword_set(color) AND (NOT keyword_set(mcs)) then $
   cgloadct,15,clip=[23,255],/reverse else $
if keyword_set(color) then $
   cgloadct,1

erase

!p.position=[0.15,0.1,0.925,0.95]
if (NOT keyword_set(charthick)) then charthick=2
if (NOT keyword_set(charsize)) then charsize=2
if !d.name eq 'PS' then begin
   !x.thick=5
   !y.thick=5
   if (NOT keyword_set(thick)) then thick=10
endif

if (NOT keyword_set(MCs)) then begin
  Flux=FLTARR(102,100)
  get_lun,lun
  
  dir='$HOME/PRO/KAT_PRO/Ionization_Processes/MW_MCs/'
  openr,lun,dir+'JBH_MW.dat'
  
  readf,lun,flux
  
  close,lun
  free_lun,lun

  imSize=SIZE(flux)
  flux_large=congrid(flux,imSize[1]*10,imSize[2]*10,/interp)
  flux_pc=congrid(flux,imSize[1]*2,imSize[2]*2,/interp)
  imSize=SIZE(flux_large)
  
  
  factor=1000./200.
  size=fix(size*factor) ;converts to integer
  
  
   ;tvimage,flux_large[0:999,0:999],/axis,xtitle='Galactic Radius (kpc)',ytitle='Galactic Height (kpc)'
  
  
  total_flux=fltarr(size*2.0,size*2.0)
  ;bottom right
  total_flux[0:size-1,0:size-1]=rotate(flux_large[0:size-1,0:size-1],2)
  ;top left
  total_flux[0:size-1,size:size*2-1]=transpose(rotate(flux_large[0:size-1,0:size-1],3))
  ;bottom left
  total_flux[size:size*2-1,0:size-1]=transpose(rotate(flux_large[0:size-1,0:size-1],1))
  ;top right
  total_flux[size:size*2-1,size:size*2-1]=flux_large[0:size-1,0:size-1]
  
endif else begin
  dir='$HOME/PRO/KAT_PRO/Ionization_Processes/MW_MCs/'
  datafile='Check3.tau1'

    data=fltarr(200,/nozero)
    image=fltarr(300,200,/nozero)
    OpenR, lun, dir+datafile, /Get_Lun, /F77_UNFORMATTED 
    i=0
    WHILE ~ EOF(lun) DO BEGIN 
       READU, lun, data
       image[i,*]=data
       i++
    ENDWHILE 
    CLOSE, lun
    FREE_LUN, lun
  
  flux=transpose(image[0:199,0:199])
  ;flux[WHERE(image LT 0)] = 0
  
  total_flux=congrid(flux,2000.,2000.,/interp)

if keyword_set(color) AND (NOT keyword_set(mcs)) then $
   cgloadct,15,clip=[23,255],/reverse else $
if keyword_set(color) then begin
   cgloadct,15,clip=[0,255],/reverse
   STRETCH, 170, 255
endif

  factor=1000./200.
  size=fix(size*factor) ;converts to integer

endelse


if keyword_set(top) then total_flux=rotate(total_flux[0:size*2-1,0:size*1-1],2)
if keyword_set(bottom) then total_flux=total_flux[0:size*2-1,0:size*1-1]
if keyword_set(left) then total_flux=total_flux[0:size-1,0:size*2-1]
if keyword_set(right) then total_flux=rotate(total_flux[0:size-1,0:size*2-1],2)

;Make background white
r[0]=!D.TABLE_SIZE-1
g[0]=!D.TABLE_SIZE-1
b[0]=!D.TABLE_SIZE-1

if NOT keyword_set(small) then begin

 tvscale,double(total_flux),/axis,$
         xrange=[-size/factor,size/factor],yrange=[-size/factor,size/factor],$
         position=!p.position,/save,$
         AXKEYWORDS={xminor:2,yminor:2,thick:!p.thick,charthick:2,$
         xtickname:replicate(' ',10),ytickname:replicate(' ',10)},Background=0
         
xyouts,!x.window[0]/2.,avg(!y.window),'Galactic Height (kpc)',/normal,$
  orientation=90,align=0.5,color=fsc_color('black'),charsize=charsize*1.15

xyouts,avg(!x.window),!y.window[0]/4.,'Galactic Radius (kpc)',/normal,$
  orientation=0,align=0.5,color=fsc_color('black'),charsize=charsize*1.15



endif else begin

flux_large=flux_large[0:size-1,0:size-1]
  
    x_range=[0,size/factor]
    y_range=[0,size/factor]

 if keyword_set(xflip) and (NOT keyword_set(yflip)) then begin
    flux_large=transpose(rotate(flux_large,3))
    x_range=-[size/factor,0]
    y_range=[0,size/factor]
 end
 if keyword_set(yflip) and (NOT keyword_set(xflip)) then begin
    flux_large=rotate(transpose(flux_large),3)
    y_range=-[size/factor,0]
    x_range=[0,size/factor]
endif
 if keyword_set(yflip) and keyword_set(xflip) then begin
    flux_large=rotate(flux_large,2)
    y_range=-[size/factor,0]
    x_range=-[size/factor,0]
endif

 tvscale,flux_large,/axis,xtitle='Galactic Radius (kpc)',ytitle='Galactic Height (kpc)',$
         xrange=x_range,yrange=y_range,$
         position=!p.position,/save,$
         AXKEYWORDS={xminor:2,yminor:2,thick:thick,charthick:2,$
         xtickname:replicate(' ',10),ytickname:replicate(' ',10)},Background=0
         

endelse

if keyword_set(levels) then begin

   levels=levels(sort(levels))
   levels=levels(rem_dup(levels))
   if (n_elements(clabels) eq 0) then c_annotation=strcompress(string(levels,format='(f8.1)'),/re) $
   else if (size(clabels,/type) eq 7) AND (n_elements(clabels) eq n_elements(levels)) then begin 
        c_annotation=clabels 
        c_labels=fltarr(n_elements(clabels))+1
        temp=where(c_annotation eq ' ',count)
        if count ne 0 then c_labels[temp]=0
   endif else c_annotation=replicate(' ',10)
   if keyword_set(egb) then levels=alog10(10^levels-10^4.51)
end 

if keyword_set(egb) and (NOT keyword_set(levels)) then begin 
    total_flux=alog10(10.0^total_flux+10^4.51)
    levels=[4.510001,4.6,4.8,5,6]
    C_ANNOTATION=['egb','4.6','4.8','5','6']
endif else if (NOT keyword_set(egb)) and (NOT keyword_set(levels)) then begin
    levels=[3,4,4.5,5]
    C_ANNOTATION=['3','4','4.5','5']
endif


if (NOT keyword_set(small)) then begin

 contour,double(total_flux),/noerase,xstyle=1,ystyle=1,position=!p.position,$
         levels=levels,C_ANNOTATION=C_ANNOTATION,C_CHARSIZE=charsize,C_CHARthick=charthick,$
         c_labels=c_labels,$
         xticks=1,yticks=1,xtickname=replicate(' ',2),ytickname=replicate(' ',2),$
         color=fsc_color('white'),c_thick=thick
         

         if (NOT keyword_set(top)) AND (NOT keyword_set(bottom)) AND $
            (NOT keyword_set(right)) AND (NOT keyword_set(left)) then begin
              x_range=[-size/factor,size/factor]
              y_range=[-size/factor,size/factor]
         endif else if keyword_set(top) then begin
              x_range=[-size/factor,size/factor]
              y_range=[0,size/factor]
         endif else if keyword_set(bottom) then begin   
              x_range=[-size/factor,size/factor]
              y_range=[-size/factor,0]
         endif else if keyword_set(right) then begin
              x_range=[0,size/factor]
              y_range=[-size/factor,size/factor]
         endif else if keyword_set(left) then begin              
              x_range=[-size/factor,0]
              y_range=[-size/factor,size/factor]
          endif    

axis,/xax,color=fsc_color('black'),xrange=x_range,$
     /save,xminor=2,charsize=charsize*.85,charthick=charthick,xstyle=1
axis,/yax,color=fsc_color('black'),yrange=y_range,$
     /save,yminor=2,charsize=charsize*.85,charthick=charthick,ystyle=1

axis,xax=0,color=fsc_color('black'),xrange=x_range,$
     /save,xminor=2,charsize=charsize*.85,charthick=charthick,xstyle=1
axis,yax=0,color=fsc_color('black'),yrange=y_range,$
     /save,yminor=2,charsize=charsize*.85,charthick=charthick,ystyle=1


endif else begin

  if keyword_set(egb) then flux_large=alog10(10.0^flux_large+10^4.51)

 contour,double(flux_large),/noerase,xstyle=1,ystyle=1,position=!p.position,$
         levels=levels,C_ANNOTATION=C_ANNOTATION,C_CHARSIZE=charsize,C_CHARthick=charthick,$
         xticks=1,yticks=1,xtickname=replicate(' ',2),ytickname=replicate(' ',2),$
         c_labels=c_labels,$
         color=fsc_color('white'),c_thick=thick

axis,/xax,color=fsc_color('black'),xrange=x_range,$
     /save,xminor=2,charsize=charsize*.85,charthick=charthick,xstyle=1
axis,/yax,color=fsc_color('black'),yrange=y_range,$
     /save,yminor=2,charsize=charsize*.85,charthick=charthick,ystyle=1

axis,xax=0,color=fsc_color('black'),xrange=x_range,$
     /save,xminor=2,charsize=charsize*.85,charthick=charthick,xstyle=1
axis,yax=0,color=fsc_color('black'),yrange=y_range,$
     /save,yminor=2,charsize=charsize*.85,charthick=charthick,ystyle=1


endelse

;restore original color table
tvlct, r, g, b

!p=p_temp

end