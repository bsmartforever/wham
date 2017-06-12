pro prepfile_hst,z=z,vstar=vstar,file=file

  ; reducestis.pro is a file that co-adds multiple exposures written by Chris Howk.

    if not keyword_set(z) then z= 0.
    if not keyword_set(vstar) then vstar= -9999

  if keyword_set(file) then begin
  	 file=validate_extension(file,'fits')
  	 files=findfile(file,count=nfiles)
  endif else $
  files=findfile('*_x1d.fits',count=nfiles)

  if nfiles ne 0 then begin
 	 a=mrdfits(files[0],0,hdr0,/silent)
  endif else begin
  	print,''
  	print,'*** File does not exit. ***
  	print,''
  	return
  endelse

inst = strtrim(sxpar(hdr0,'INSTRUME'),2)
opt = strtrim(sxpar(hdr0,'OPT_ELEM'),2)
name = strtrim(sxpar(hdr0,'TARGNAME'),2)
ra = float(strtrim(sxpar(hdr0,'RA_TARG'),2))*1.d	;deg
dec = float(strtrim(sxpar(hdr0,'DEC_TARG'),2))*1.d	;deg

glactc,ra,dec,2000,gl,gb,1,/degree
lsrvel, gl, gb, vlsr, vmb,/silent

jk1 = where(strmatch(inst,'STIS'),test1)
jk2 = where(strmatch(inst,'COS'),test2)
jk3 = where(strmatch(opt,'E140H'),test3)
;Test to see if FUSE data
jk4 = where(strmatch(inst,'FUV'),test4)

	if test1 ne 0 then begin
 		if test3 eq 0 then begin 
      ;Tests to see if multiple spectrographs were used. 
 		  reducestis, name, files
 		endif else begin 
 		  for i = 0, n_elements(files) -1 do begin 
		    j=i+1
 		    reducestis,name+STRCOMPRESS(string(j),/remove_all),files(i)
 		  endfor 
 		  restore,name+'1_out.save'		
 		  wave1  = wave
		  flux1 = flux
		  err1 = err
 		  restore,name+'2_out.save'		
 		  wave2  = wave
		  flux2 = flux
		  err2 = err
		  wave=[wave1,wave2]
		  flux=[flux1,flux2]
		  err=[err1,err2]
		  save,fil=name+'_out.save',wave,flux,err
		  spawn,'mv  '+name+'1_out.save file1.save'
		  spawn,'mv  '+name+'2_out.save file2.save'
 		endelse
	endif 

	if test2 ne 0 then begin 
 		COADD_X1D,chan=3,method=2,/moderror,files=files,plot=2,savefile=name+'_out.save'
	endif 

restore,'*_out.save'
spawn, 'mkdir Analysis'

plotpreview,wave,flux,name
plotpreview,wave,flux,name


readcol,'$HOME/PRO/iNorm/lists/lineslls.dat',elem,w,gv,fv,format='A,D,D,D'

for i = 0,n_elements(fv) -1  do begin  
  wni = STRTRIM(string(w(i),'(f8.1)'),2)
  ion = STRTRIM(elem(i),2)
  wavc = w(i)
  fval = fv(i)
  gam = gv(i)
  redshift = z
  object = STRTRIM(name,2)
  
  vlsr1 = vlsr 
  if z gt 0 then vlsr1 =0 ; helio frame for z>0 absorbers
  
  if (wavc*(1+z) ge min(wave) and wavc*(1+z) le max(wave)) then begin
  	
    vel = ((wave - wavc)/wavc * 2.9979e5 -  2.9979e5 *z )/(1.+z) + vlsr1
     
    index = where(vel ge -2000 and vel le 2000,ct)
    if ct ne 0 then begin
      v= vel[index]
      f = flux[index]
      ef = err[index]
      save,fil=ion+wni+'i.save',v,f,ef,ion,wni,wavc,fval,gam,redshift,object,vlsr,name,ra,dec,gl,gb,vstar,z
    endif 
    
  endif else begin 
    v = 0.+fltarr(1)
    f =0.+fltarr(1)
    ef =0.+fltarr(1)
    save,fil=ion+wni+'i.save',v,f,ef,ion,wni,wavc,fval,gam,redshift,object,vlsr,name,ra,dec,gl,gb,vstar,z
  endelse 

endfor

;;plot 
spawn, 'mv *i.save Analysis'

;;plot 


cd,'Analysis' 

;plotstack
;plotstack
;plothilab_hvc, gl,gb,-300,300 
 
set_plot,'x'
cleanplot, /silent

cd,'../'

print,'Done......................'

end
