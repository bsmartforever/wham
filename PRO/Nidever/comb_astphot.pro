pro comb_astphot,field,all,dir=dir,cmd=cmd,stp=stp,save=save

;+
;
; COMB_ASTPHOT
;
; This combines all the photometry for a MOSAIC field, i.e. combines
; the different chips.  It uses the AST files which are output by
; GET_ASTROM.PRO.
; This program assumes that there are 16 files (i.e. 8 chips x 2 amps)
;
; INPUTS
;  field   The field name
;  =dir    The directory where the file are
;  /save   Save the output structure
;  /cmd    Plot a CMD
;  /stp    Stop at the end of the prgoram
; 
; OUTPUTS:
;  all   The final combined structure
;
; USAGE:
;  IDL>comb_astphot,'130L134a',all
;
; By D.Nidever   January 2007
;-

undefine,all

; Not enough inputs
if n_elements(field) eq 0 then begin
  print,'Syntax - pro comb_astphot,field,all,cmd=cmd,stp=stp,save=save'
  return
endif

; CD to the correct directory
cd,current=curdir
if n_elements(dir) eq 0 then dir='.'
cd,dir

fdir = field
;if n_elements(dir) eq 0 then dir='.'
dir2 = file_expand_path(fdir)

test = file_test(dir2,/directory)
if test eq 0 then begin
  print,'DIRECTORY ',dir2,' DOES NOT EXIST'
  cd,curdir
  return
endif

files = file_search(dir2+'/'+'*_*.ast',count=nfiles)
if nfiles eq 0 then begin
  print,'NO AST FILES FOR ',field
  return
endif
base = file_basename(files[0])
arr = strsplit(base,'_',/extract)
frame = arr[0]

; Loop through the images
for i=1,16 do begin

  amp = strtrim(i,2)  ; amplifier number

  ; Check the file
  ;astfile = dir2+'/'+field+'_'+strtrim(i,2)+'.ast'
  astfile = dir2+'/'+frame+'_'+strtrim(i,2)+'.ast'
  filebase = file_basename(astfile)
  test = file_test(astfile)
  if test eq 0 then begin
    print,'FILE ',astfile,' DOES NOT EXIST'
    goto,BOMB
  endif

  fieldnames = ['ID','X','Y','RA','DEC','I','Ierr','M','Merr','D','Derr','Chi','Sharp']
  fieldtypes = [7,4,4,5,5,4,4,4,4,4,4,4,4]
  str = importascii(astfile,/header,/noprint,fieldnames=fieldnames,fieldtypes=fieldtypes)
  nstr = n_elements(str)

  print,filebase,' Nstars = ',strtrim(nstr,2)

  ; Adding the extra tags
  add_tag,str,'xb',0.0,str
  add_tag,str,'yb',0.0,str
  add_tag,str,'xch',0.0,str
  add_tag,str,'ych',0.0,str

  ; The original X/Y are chip coordinates
  ; Now transfer those to XCH/YCH and change X/Y back to the original value
  str.xch = str.x
  str.ych = str.y
  if i mod 2 eq 0 then str.x=str.x-1024L   ; correcting the the X values

  ; Correcting the X/Y coordinates
  xoff = ((i-1) mod 8)*1024.
  yoff = ((i-1)/8)*4096.

  str.xb = str.x + xoff
  str.yb = str.y + yoff

  ; Changing the ID to reflect the field and amplifier
  id2 = field+'_'+amp+'.'+strtrim(str.id,2)
  str.id = id2

  ; Adding together with PHOT_OVERLAP.PRO to deal with overlaps
  ; at the edges of chips/amplifiers
  if (i gt 1) then begin
    ; Check for overlaps
    phot_overlap,all,str,outstr
    all = outstr
  endif else begin
    all = str 
  endelse

  BOMB:

end

print,'Nstars = ',strtrim(n_elements(all),2)

; Saving
if keyword_set(save) then begin
  outfile = dir2+'/'+field+'_ast.dat'
  print,''
  print,'SAVING the output structure to ',outfile
  save,all,file=outfile
endif

; Plotting
if keyword_set(cmd) and n_elements(all) gt 0 then begin
  window,0
  erase

  ; CMD
  !p.multi=[2,2,1]
  plot,all.m-all.i,all.m,ps=3,yr=[24,12],xr=[0,4],xs=1,ys=1,xtit='M-I',ytit='M',tit=field

  ; Color-Color diagram
  !p.multi=[1,2,1]
  plot,all.m-all.i,all.m-all.d,ps=3,yr=[-0.7,0.7],xr=[0,4],xs=1,ys=1,xtit='M-I',ytit='M-D',tit=field

  !p.multi=0
endif


; CD back to original directory
cd,curdir

if keyword_set(stp) then stop

end
