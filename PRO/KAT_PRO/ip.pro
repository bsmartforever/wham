function ip, mean, width, area, filename=filename

;This program produces WHAM instrument profiles in the format required by whamspect. 
;
;Input:
;   mean - An array containing the center location of the Gaussians,
;   width - An array containing the FWHM of the Gaussians. 
;   area - An array containing the area of each Gaussian in any units. 
;          Only the proportional size of the Gaussians compared to 
;          each other matters. 
;The program accepts the above parameters in the format outputted by whamspect in the fitting routine.  
;   filename - string containing the filename to output the determined instrument profile. Whamspect expects *.dat files for the instrument profile. This program does not check the extension of the output file. 
;
;Output:
;   The first line displays the number of Gaussians.   
;   Following the first line, properties of each Gaussian
;   gets outputted in groupings of three and gets repeated. 
;        line 2,5,8,etc.: Position of Gaussian centered about zero. 
;        line 3,6,9,etc.: FWHM of Gaussian
;        line 4,7,10,etc.: Normalized area of Gaussian such that the
;                          total of all areas equal 1 and such that the 
;                          relative size of each Gaussian remains in tacked.   

mean_norm=mean-avg(mean)
sigma=width/(2*sqrt(2*alog(2)))
area_norm=area/total(area)
height=area_norm/(sigma*sqrt(2*!pi))

if keyword_set(filename) then begin 
   openw,lun,filename,/get_lun
   printf,lun,n_elements(mean),format='(i-4)'
   for i=0,n_elements(mean)-1 do begin
       printf,lun,mean_norm(i),format='(f-12.6)'
       printf,lun,width(i),format='(f-12.6)'
       printf,lun,height(i),format='(f-12.6)'
    endfor
    close,/all
endif

print,n_elements(mean),format='(i-4)'
for i=0,n_elements(mean)-1 do begin
    print,mean_norm(i),format='(f-12.6)'
    print,width(i),format='(f-12.6)'
    print,height(i),format='(f-12.6)'
endfor

ip_str = REPLICATE({num:n_elements(mean),mean:mean_norm,width:width,height:height},1.0) 

return,ip_str;

end