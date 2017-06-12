pro post_processing,input

; This does all of the post-processing of photometric data
; after they have been calibrated.  This program does the following:
; 1.) Finds a global astrometric solution
; 2.) Combines the photometry from all the separate chips
; 3.) Deredden the photometry
;
; INPUTS:
;  input    List of directories

;input =  ['/net/halo/dln5q/ctio4m/n1/255L032/']

;get_astrom_global    ; compiles the programs
;get_astrom_dir
;get_astrom

; Not enough inputs
if n_params() lt 1 then begin
  print,'Syntax - post_processing,list'
  return
endif

; A list file was input
if strmid(input[0],0,1) eq '@' then begin

  inp = strmid(input[0],1)

  ; Loading the files
  ;readcol,inp,list,format='A',/silent,comment='#'
  readline,inp,list,comment='#'
  nlist = n_elements(list)

endif else begin

  ; Probably an array of filenames
  if n_elements(input) gt 1 then begin
    list = input
    nlist = n_elements(list)

  ; A globbed list or only one file
  endif else begin
    list = file_search(input)
    nlist = n_elements(list)
  endelse

endelse


; Loop through the input list
FOR i=0,nlist-1 do begin

  fdir = list[i]
  fdir = file_expand_path(fdir)

  field = file_basename(fdir)

  print,''
  print,'#####################################'
  print,' POST-PROCESSING FOR FIELD ',field
  print,'#####################################'
  print,''


  ;##############################################
  ;#  STEP 1: Astrometry
  ;##############################################
  print,''
  print,'--------------------------'
  print,'Getting astrometry'
  print,'--------------------------'
  print,''
  undefine,fpar,fcenter,inpphot,inpusno

  ; Fields that need to be run a 2nd time b/c some chips weren't solved
  ;if field eq '214L154' then restore,'/net/halo/dln5q/ctio4m/n3/214L154/astrom/214L154_global_astrom.dat'
  ;if field eq '190L161a' then restore,'/net/halo/dln5q/ctio4m/n2/190L161a/astrom/190L161a_global_astrom.dat'
  ;if field eq '219L155' then restore,'/net/halo/dln5q/ctio4m/n3/219L155/astrom/219L155_global_astrom.dat'
  ;if keyword_set(phot2) then inpphot = phot2
  ;if keyword_set(usno2) then inpusno = usno2

  ; Get the global astrometric solution
  ;GET_ASTROM_GLOBAL,fdir,par,fcenter,usno2,phot2,inpphot=inpphot,inpusno=inpusno,/saveplot,psfile=fdir+'/'+field+'_resid'

  ;stop

  ; Save the solution
  outfile = fdir+'/astrom/'+field+'_global_astrom.dat'
  ;print,'Saving the Global astrometric solution to ',outfile
  ;save,par,fcenter,usno2,phot2,file=outfile
  restore,outfile


  ;##############################################
  ;#  STEP 2: Combine photometry from separate chips
  ;##############################################
  ; Most of this was taken from COMB_ASTPHOT.PRO
  print,''
  print,'--------------------------'
  print,'Combining the data'
  print,'--------------------------'
  print,''
  get_astrom_global

  ; Restore all of the separate photometry files  
  files = file_search(fdir+'/*.phot')
  nfiles = n_elements(files)

  dum = {id:'',x:0.0,y:0.0,ra:0.0d0,dec:0.0d0,I:0.0,Ierr:0.0,M:0.0,Merr:0.0,D:0.0,Derr:0.0,$
         chi:0.0,sharp:0.0,xb:0.0,yb:0.0,xch:0.0,ych:0.0}

  for j=0,nfiles-1 do begin

    ;amp = strtrim(j+1,2)  ; amplifier number
    base = file_basename(files[j],'.phot')
    tmp = strsplit(base,'_',/extract)
    amp = long(tmp[1])

    phot = importascii(files[j],/header,/noprint)
    nphot = n_elements(phot)

    ; Make new structure
    phot2 = replicate(dum,nphot)
    phot2.x = phot.x
    phot2.y = phot.y
    phot2.I = phot.I
    phot2.Ierr = phot.Ierr
    phot2.M = phot.M
    phot2.Merr = phot.Merr
    phot2.D = phot.D
    phot2.Derr = phot.Derr
    phot2.chi = phot.chi
    phot2.sharp = phot.sharp


    ; The original X/Y are AMP coordinates
    ; Get CHIP coordinates (xch/ych) and FIELD coordinates (xb/yb)
    phot2.xch = phot.x
    phot2.ych = phot.y
    ;if j mod 2 eq 1 then phot2.xch = phot2.xch+1024L   ; correcting the X values
    if amp mod 2 eq 0 then phot2.xch = phot2.xch+1024L   ; correcting the X values

    ; Adding chip offsets to FIELD coordinates the X/Y coordinates
    ;xoff = (j mod 8)*1024.
    ;yoff = (j/8)*4096.
    xoff = ((amp-1) mod 8)*1024.
    yoff = ((amp-1)/8)*4096.

    phot2.xb = phot2.x + xoff
    phot2.yb = phot2.y + yoff

    ; Changing the ID to reflect the field and amplifier
    id2 = field+'_'+strtrim(amp,2)+'.'+strtrim(phot.id,2)
    phot2.id = id2

    ; Getting coordinates
    mnra = fcenter[0]
    mndec = fcenter[1]
    ;chip = (phot2.x*0.0) + (j/2)+1
    chip = (phot2.x*0.0) + ((amp+1)/2)
    out = trans_coord(phot2.xch,phot2.ych,chip,par)
    zeta = reform(out[0,*])
    eta = reform(out[1,*])
    ROTSPHCEN,zeta,eta,mnra,mndec,nra,ndec,/gnomic,/reverse
    phot2.ra = nra
    phot2.dec = ndec

    ; Adding together with PHOT_OVERLAP.PRO to deal with overlaps
    ; at the edges of chips/amplifiers
    if (j gt 0) then begin
      ; Check for overlaps
      PHOT_OVERLAP,all,phot2,outphot
      all = outphot
    endif else begin
      all = phot2
    endelse

    ;print,format='(A35,I5,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2,F10.2)',file_basename(files[j]),amp,mean(chip),xoff,yoff,min(phot2.xch),$
    ;             max(phot2.xch),min(phot2.ych),max(phot2.ych)

    ;stop

  end

  ;stop


  ;##############################################
  ;#  STEP 3: Deredden the photometry
  ;##############################################
  print,''
  print,'--------------------------'
  print,'Dereddening the data'
  print,'--------------------------'
  print,''

  DEREDDEN,all,final


  ; Save the final structure
  fout = fdir+'/'+field+'_final.dat'
  print,'Saving final structure of ALL objects to ',fout
  save,final,file=fout

  ; Saving only stars
  gd = where(final.chi lt 1.5 and abs(final.sharp) lt 1.0,ngd)
  stars = final[gd]
  fout = fdir+'/'+field+'_stars.dat'
  print,'Saving final structure of STARS to ',fout
  save,stars,file=fout

  ; Also save to LMC halo directory
  halodir = '/local/dln5q/research/lmchalo/data/'
  save,final,file=halodir+field+'_final.dat'
  save,stars,file=halodir+field+'_stars.dat'

  ;stop

END ; for i

stop

end
