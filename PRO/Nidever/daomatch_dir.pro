pro daomatch_dir,dir

if n_elements(dir) eq 0 then dir='.'

print,'-------------------------------'
print,'  FIELD '+dir
print,'-------------------------------'


; Loop through the 16 frames of each image
for k=1,16 do begin

  files = file_search(dir+'/*_'+strtrim(k,2)+'.als')
  nfiles = n_elements(files)

  if nfiles gt 3 then begin
    print,'MORE THAN 3 BANDS, BOMBING'
    goto,bomb
  endif

  ;files = file_search(dirs[gd[j]]+'/*.als')
  ;g = where(stregex(files,'a.als',/boolean,/fold_case) eq 0,ng)
  ;files = files(g)

  files2 = file_basename(files,'.als')
  ;files3 = files2
  ;for k=0,n_elements(files2)-1 do files3[k]=(strsplit(files2[k],'_',/extract))(0)

  ;ui = uniq(files3,sort(files3))
  ;ufiles = files3(ui)
  ;nufiles = n_elements(ufiles)
   
  filtarr = strarr(nfiles)
  nrecords = fltarr(nfiles)
  for l=0,nfiles-1 do begin
    ;head = headfits(dirs[gd[j]]+'/'+ufiles(k)+'_1.fits')
    head = headfits(dir+'/'+files2(l)+'.fits')
    filter = sxpar(head,'FILTER')
    nrecords[l] = file_lines(dir+'/'+files2(l)+'.als')-3
    if stregex(filter,'M Washington',/boolean) eq 1 then filtarr[l]='M'
    if stregex(filter,'D51 DDO',/boolean) eq 1 then filtarr[l]='D'
    if stregex(filter,'I c6028',/boolean) eq 1 then filtarr[l]='I'
  end

  ;; Use the frame with the most stars as the reference
  ;si = reverse(sort(nrecords))
  ;files3 = files2(si)

  ; Use the I frame as the "reference"
  allind = lindgen(nfiles)
  mind = where(filtarr eq 'M',nmind)
  iind = where(filtarr eq 'I',niind)
  dind = where(filtarr eq 'D',ndind)

  undefine,files3

  ; If an "I" frame exists use it for the reference
  if (niind gt 0) then begin

    push,files3,files2[iind]

    if nmind gt 0 then push,files3,files2[mind]
    if ndind gt 0 then push,files3,files2[dind]

    ; Adding the rest
    left = nonuniq(files2,files3)
    if n_elements(left) gt 0 then begin
      if left[0] ne -1 then push,files3,files2[left]
    end

  ; NO "I" frame
  endif else begin

    ; Use M as the reference if it exists
    if (nmind gt 0) then begin

      ; M as the reference
      push,files3,files2[mind]

      ; Add the rest
      left = nonuniq(files2,files3,bad=bad,dbl=dbl,indbl=indbl,ind2dbl=ind2dbl)
      if n_elements(left) gt 0 then begin
        if left[0] ne -1 then push,files3,files2[left]
      end

    ; NO "I" or "M" frames
    endif else begin

      ; Use the frame with the most stars as the reference
      si = reverse(sort(nrecords))
      files3 = files2(si)
    endelse

  endelse

  ; Absolute paths
  cdir = file_dirname(files[0])+'/'
  files4 = cdir+files3

  ; Running daomatch
  daomatch,files4

  ; Getting total number of stars
  nrecords = file_lines(files4[0]+'.raw')-3
 
  ; Printing the results
  print,'FRAME '+strtrim(k,2)+' - ',strtrim(nrecords,2)+' STARS'

  ;stop

end

bomb:

;stop

end
