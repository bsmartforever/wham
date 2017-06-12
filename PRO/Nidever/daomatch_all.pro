pro daomatch_all

; This program runs the daomatch and daomaster
; scripts on one directory

dir = '/net/halo/dln5q/ctio4m/'

nights = ['n1','n2','n3','n4','n5']
nnights = n_elements(nights)

; Loop through the nights
for i=0,nnights-1 do begin

  print,'###############################'
  print,'  DOING NIGHT '+strtrim(i+1,2)
  print,'###############################'
  print,''

  dirs = file_search(dir+nights[i]+'/*',/test_directory)

  gd = where(stregex(dirs,'Raw',/boolean,/fold_case) eq 0 and $
             stregex(dirs,'twilight',/boolean,/fold_case) eq 0 and $
             stregex(dirs,'bad',/boolean,/fold_case) eq 0 and $
             stregex(dirs,'flat',/boolean,/fold_case) eq 0 and $
             stregex(dirs,'zero',/boolean,/fold_case) eq 0 and $
             stregex(dirs,'SA110',/boolean,/fold_case) eq 0 and $
             stregex(dirs,'SA98',/boolean,/fold_case) eq 0 and $
             stregex(dirs,'NGC3680',/boolean,/fold_case) eq 0,ngd)

  dirs = dirs(gd)
  ndirs = n_elements(dirs)

  dirs2 = file_basename(dirs)

  ; Loop through the fields/directories
  for j=0,ngd-1 do begin

    print,'-------------------------------'
    print,'  FIELD '+dirs2[j]
    print,'-------------------------------'

    ; Runnting daomatch on the directory
    daomatch_dir,dirs[j]

  end ; for j, directory loop

end ; for i, night loop

;stop

end
