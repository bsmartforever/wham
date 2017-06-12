;+
;                               iNORMSAVE.PRO
;                               Version 1.0;
;Program Description:
; This procedure will create a list of inorm save files from 
; a list of ions. 
;
;Latest Update Comments:
;       04/15/13  NL: Version 1.0
;
;External Routines called:
;       None
;----------------------------------------------------------------------------
PRO iNORMSAVE,root=root,redshift=redshift, vlsr = vlsr, object=object

    if not keyword_set(object) then object=''
    if not keyword_set(redshift) then redshift=0
    if not keyword_set(vlsr) then vlsr=0
    if not keyword_set(root) then root=''

ion=''
vlsr = 0. 
wavc=0.
fval=0.
gam=0.
wni = ''
rext=''
        flgf = -1 
        if keyword_set(root) then begin
          root = STRTRIM(root,2)
          ; detemine file type   
          rext1=['.save','.sav','.fits','.txt','.dat','.asc']
          flgfile = [0,0,1,2,2,2]
          for j=0,n_elements(rext1)-1 do begin
            ;Tests to determine which file extension to use.
            test = file_test(root+rext1[j])
            ;Tests to see if root includes the file extenstion.
            test_exists=strmatch(root,'*'+rext1[j],/fold_case)
            if (test eq 1 ) then begin 
              rext = rext1[j]
              flgf = flgfile[j]
            endif else if test_exists eq 1 then begin
              root = strsplit(root,rext1[j],/extract,/regex,/fold_case)
              rext = rext1[j]
              flgf = flgfile[j]
            endif
          endfor 
        endif
          
          if flgf eq -1 then begin
                 PRINT,'iSAVE::  Enter  type of files'
                 PRINT,'iSAVE::  (s)ave, (x).fits, (a)scii or (q)uit'
                  kind = GET_KBRD(1)
                  rext = ''
                  if kind EQ 'q'  then RETURN
                  if kind EQ 's' then  flgf = 0
                  if kind eq 'x' then  flgf = 1
                  IF kind EQ 'a' THEN  flgf = 2
          endif           
;       
        if flgf eq 0 then begin 
          PRINT,'iNORM::  Reading: ',root+rext
          restore,root+rext
        endif 
        ;assume xidl fits file 
        if flgf eq 1 then begin 
           fluxi = file_search('*f.fits')
           fluxi=fluxi[0]
           erri = file_search('*e.fits')
           erri=erri[0]
           err=xmrdfits(erri,0,hdr,/silent)  
           flux=xmrdfits(fluxi,0,hdr,/silent)  
           object = sxpar(hdr,'TARGNAME')
           wave = 10^(sxpar(hdr,'CRVAL1') + findgen(n_elements(flux))*sxpar(hdr,'cdelt1'))
        endif
        if flgf eq 2 then begin
          PRINT,'iNORM::  Reading: ',root+rext
          readcol,root+rext,wave,flux,err,/silent
        endif

readcol,'$HOME/PRO/iNorm/lists/lineslls.dat',elem,w,gv,fv,format='A,D,D,D'

for i = 0,n_elements(fv) -1  do begin  
  wni = STRTRIM(string(w(i),'(f8.1)'),2)
  ion = STRTRIM(elem(i),2)
  wavc = w(i)
  fval = fv(i)
  gam = gv(i)
  z = redshift 
  
  vlsr1 = vlsr 
  if z gt 0 then vlsr1 =0 ; helio frame for z>0 absorbers
  
  if (wavc*(1+z) ge min(wave) and wavc*(1+z) le max(wave)) then begin
    	
    vel = ((wave - wavc)/wavc * 2.9979e5 -  2.9979e5 *z )/(1.+z) + vlsr1
     
    index = where(vel ge -2000 and vel le 2000,ct)
    if ct ne 0 then begin
      v= vel[index]
      f = flux[index]
      ef = err[index]
      save,fil=ion+wni+'i.save',v,f,ef,ion,wni,wavc,fval,gam,redshift,object,vlsr
    endif 
    
  endif else begin 
    v = 0.+fltarr(1)
    f =0.+fltarr(1)
    ef =0.+fltarr(1)
    save,fil=ion+wni+'i.save',v,f,ef,ion,wni,wavc,fval,gam,redshift,object,vlsr
  endelse 

endfor

;;plot 

print,'Done......................'

end
