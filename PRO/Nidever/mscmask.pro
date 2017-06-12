pro mscmask,imagename,maskname,stp=stp

; Make a bad pixel mask for a MOSAIC file

if n_elements(imagename) eq 0 then begin
  print,"Syntax - maskmask,'image.fits','mask.fits'"
  return
endif

cd,current=curdir
irafdir = file_search('~/iraf/',/fully_qualify_path)

tempdir = maketemp('temp')
file_mkdir,tempdir

base = file_basename(imagename,'.fits')

if n_elements(maskname) eq 0 then maskname = base+'_bpm'
maskbase = file_basename(maskname,'.fits')

file_delete,maskname,/allow_non

test = file_test(base+'.fits')
if test eq 0 then begin
  print,'FILE ',base,'.fits DOES NOT EXIST'
  return
endif

head0 = headfits(base+'.fits',exten=0)

nextend = sxpar(head0,'NEXTEND')

fits_write,tempdir+'/'+maskbase+'_0.fits',0,head0

for i=1,nextend do begin

  fits_read,base+'.fits',im,head,exten=i
  head2 = headfits(base+'.fits',exten=i)

  nextend = sxpar(head,'NEXTEND')

  head3 = head2
  sxdelpar,head3,'XTENSION'
  sxdelpar,head3,'INHERIT'
  sxdelpar,head3,'PCOUNT'
  sxdelpar,head3,'GCOUNT'
  sxdelpar,head3,'EXTNAME'
  sxdelpar,head3,'EXTVER'

  sxaddpar,head3,'SIMPLE','T',before='BITPIX'
  sxaddpar,head3,'NEXTEND',nextend,after='NAXIS2'
  sxaddpar,head3,'EXTNM','im'+strtrim(i,2)

  ;bitpix = sxpar(head,'BITPIX')
  ;naxis = sxpar(head,'NAXIS')
  ;naxis1 = sxpar(head,'NAXIS1')
  ;naxis2 = sxpar(head,'NAXIS2')
  ;nextend = sxpar(head,'NEXTEND')
  ;
  ;undefine,head2
  ;sxaddpar,head2,'SIMPLE','T'
  ;sxaddpar,head2,'BITPIX',bitpix
  ;sxaddpar,head2,'NAXIS',naxis
  ;sxaddpar,head2,'NAXIS1',naxis1
  ;sxaddpar,head2,'NAXIS2',naxis2
  ;;sxaddpar,head2,'EXTEND','T'
  ;sxaddpar,head2,'NEXTEND',nextend
  ;sxaddpar,head2,'IMAGEID',i
  ;sxaddpar,head2,'EXTNM','im'+strtrim(i,2)
  ;;sxaddpar,head2,'EXTNAME','im'+strtrim(i,2)
  ;;sxdelpar,head2,'INHERIT'

  ;med = median(im)
  ;std = mad(im)
  ;mask = float(abs(im-med) gt 6.*std)
  mask = float(im le 10)

  ; kludge for #5 & 6 for Ds
  ;if i eq 5 then mask[599:601,3450:4095] = 1.0
  ;if i eq 6 then mask[130,3545:4095] = 1.0

  ; kludge for I
  ;if i eq 5 then mask[597:602,3450:4095] = 1.0
  ;if i eq 16 then mask[824:828,0:605] = 1.0
  ;if i eq 16 then mask[898:900,0:2750] = 1.0

  fits_write,tempdir+'/'+maskbase+'_'+strtrim(i,2)+'.fits',mask,head3

end

; Combining them

; Write IRAF script
undefine,cmd
push,cmd,'cd '+curdir+'/'+tempdir
push,cmd,'mscred'
push,cmd,'mscjoin("'+maskbase+'")'
push,cmd,'logout'
cmdfile = maketemp('temp','.cl')
WRITELINE,curdir+'/'+tempdir+'/'+cmdfile,cmd

; Goto the IRAF directory
CD,irafdir

; Running IRAF
undefine,out
SPAWN,'cl < '+curdir+'/'+tempdir+'/'+cmdfile,out,errout

; Return to original directory
cd,curdir

; Remove the CL file
file_delete,curdir+'/'+tempdir+'/'+cmdfile

; Move the mask from the temporary directory
file_move,curdir+'/'+tempdir+'/'+maskbase+'.fits',curdir,/overwrite

; Delete the individual files
files = file_search(curdir+'/'+tempdir+'/'+maskbase+'_*.fits')
file_delete,files,/allow

; Remove the temporary directory
file_delete,curdir+'/'+tempdir


if keyword_set(stp) then stop

end
