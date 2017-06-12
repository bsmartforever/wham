
pro plothilab_hvc, la,ba,v1,v2 


labspec,la,ba,x1,y1,li,bi,/notrim
d = sphdist(la, ba, li, bi,/degree)
print, 'separation in degrees', la,ba, li, bi, d

index = where(x1 ge -200 and x1 le 200)
out = int_tabulated(x1[index],y1[index]) 
n1 = alog10(1.823e18 * out) 

print,'-----------------'
print, 'log N(HI)', n1


 
ymax = max(y1[where(x1 ge v1 and x1 le v2)]) + 0.1 * max(y1[where(x1 ge v1 and x1 le v2)])


SET_PLOT, 'PS'
;
DEVICE, FILENAME='plothilab.eps', FONT_SIZE=11, /INCHES, XSIZE=8, YSIZE=6, $
	/ENCAPSULATED, /COLOR, /portrait

!P.MULTI = [0, 1, 1, 0, 0]
!P.CHARSIZE = 1.2
!p.psym=10
plot, x1, y1, thick=4,xr=[-399,399], yr=[-0.2,ymax], /ys,/xs,$
xtitle ='!6v!dLSR!n [km/s]', ytitle='!6T!dB!n [K]' , title = ''
plotzero

loadct,40

oplot, x1,y1 * 5, color = 210,thick = 0.3
oplot, x1,y1,thick = 4


restore,'CII1334.5i.save'
	ver,0,lines=1,color =50, thick = 3
	ver,100,lines=1,color =150, thick = 3	
	ver,-100,lines=1,color =150, thick = 3	
	ver,vstar,lines=2,color =90, thick = 3	


xyouts, v1-20, ymax/1.3, 'log N(H I)='+string(n1), size = 1.2 
DEVICE, /CLOSE

set_plot,'x'
cleanplot, /silent





end
