function inormreadstr,root,quiet=quiet,COS=COS,STIS=STIS,FUSE=FUSE, instrument=instrument

; Purpose - Extract and calculate the flux, continuum, normalized flux, and 
;           apparent column density--along with other useful variables--from 
;           the *.dat and *.att files.
;
; root - Root of the file name of *.dat and *.att files
;             e.g., name.dat => 'name'. 
;        Will accept directory path + root, but the structure will 
;        save the whole directory path name.
;        Assumes that the *.dat and *.att have the same root.
;        Default is axis=0
; quiet - Turns off output messages of imread and imattr. 
;         Note that these programs need to be modified to accept this keyword.
; COS - Sets instrument tag to 'COS'. Default is to retrieve from *.dat file, 
;       but the instrument listed is sometimes inaccurate. 
; STIS - Sets instrument tag to 'STIS'. Default is to retrieve from *.dat file, 
;       but the instrument listed is sometimes inaccurate. 
;
; Output -  Structure containing:
;            name           root of file name, will contain directory path if supplied 
;            vel            velocity in km/s
;            wave           wavelength in angstroms 
;            flux	          flux		
;            flux_err       flux_err
;            norm           normalized flux
;            column         apparent column density as a function of velocity
;            fval           oscillator strength
;            instrument     name of instrument, e.g., COS or STIS
;            ion            name of ion
;            wavc           rest frame wavelength in angstroms 
;            continuum      continuum
;            lbar           continuum lower error bar array 
;            ubar	          continuum upper error bar array 
;          
; Requirements - Must have both *.dat and *.att files with the same root names to work.
;
;
;  Example: temp=imreadstr('$HOME/DATA_DIR/AlII1670im',/quiet) 
;
; Created 04/08/2013 by Dr. Kat Barger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!EXCEPT=0
;Turns off stupid "% Program caused arithmetic error: Floating illegal operand" error

int_root=root ;For some reason the imread program transforms the root to from dir + root => dir + dir + root
              ;This saves the 'initial' root passed so that it can be properly called by imattr
axis=0
root=int_root
smroot=strsplit(root,'im',/extract,/regex)+'o'
;imnorm used rooto.save and iNorm uses root_o.save for the output. 
if file_test(smroot+'save') eq 0 then smroot=strsplit(root,'im',/extract,/regex)+'_o'

restore,smroot+'.save'
if size(y,/type) eq 0 then y=flux
y_err=ef
if size(x,/type) eq 0 then x=vel

;This is the normalized flux, normalized by the continuum. 
;lbar and ubar are the upper and lower limits of the continuum. The continuum is just the average of these.
continuum=cont
continuum_err=econt
ubar=continuum+continuum_err
lbar=continuum-continuum_err
;normalized flux
norm=fnorm
norm_err = sqrt((y_err/y)^2.0+(continuum_err/continuum)^2.0)*norm

;Apparent column density as a function of velocity
a=3.768e14/(wavc*fval)
column=a*alog((continuum)/(y))
column_err=sqrt((a/alog(10)*1./y*y_err)^2.0+(a/alog(10)*1./continuum*continuum_err)^2.0)

stuff=where(y lt 0,count_stuff)
if count_stuff ne 0 then column[where(y lt 0)]=0
if count_stuff ne 0 then column_err[where(y lt 0)]=0

if keyword_set(COS) then instrument='COS' else $
if keyword_set(STIS) then instrument='STIS' else $
if keyword_set(FUSE) then instrument='FUSE' else $
if keyword_set(instrument) then instrument=instrument else $
instrument=' '

c=299792.458 ;km/s

wave=wavc*(c/x)/((c/x)-1.0)

   data_structure = REPLICATE({$
   	    name:root,$
            vel:x,$
            wave:wave,$
            flux:y,$
            flux_err:ef,$
            norm:norm,$
            norm_err:norm_err,$
            column:column,$
            column_err:column_err,$
            fval:fval,$
            instrument:instrument,$
            ion:ion,$
            wavc:wavc,$
            continuum:continuum,$
            lbar:lbar,$
            ubar:ubar,$
            continuum_err:continuum_err},1) 

   return,data_structure;

end