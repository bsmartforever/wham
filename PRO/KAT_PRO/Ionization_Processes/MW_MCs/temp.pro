
datafile='Check3.tau1'

   data=fltarr(200,/nozero)
   image=fltarr(300,200,/nozero)
   OpenR, lun, datafile, /Get_Lun, /F77_UNFORMATTED 
	i=0
	WHILE ~ EOF(lun) DO BEGIN 
	   READU, lun, data
	   image[i,*]=data
	   i++
	ENDWHILE 
	CLOSE, lun
	FREE_LUN, lun

image=transpose(image[0:199,0:199])
image[WHERE(image LT 0)] = 0

cgimage,image,minvalue=1,maxvalue=6

end