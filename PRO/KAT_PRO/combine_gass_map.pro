pro combine_gass_map, file1, file2, combine_name, dir1=dir1, dir2=dir2, combine_dir=combine_dir

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Purpose: Combine GASS HI survey mini maps. 
;
; Inputs:
;
; 	file1 - File containing GASS HI mini map, file type *.sav. 
;	file2 - File containing GASS HI mini map, file type *.sav.
;	combine_name - Output file name, file type *.sav.
;
; Outputs:
;
;	Save file combine_name.sav that contains the combined GASS HI map.
;
; Options:
;
;	dir1 - directory of file1
;	dir2 - directory of file2
;	comine_dir - directory to save the combined map.
;
; Example:
;	combine_gass_map,'HE0226_4110.sav','HE0226_4110_near.sav','HE0266_4110_combined.sav'
;
;	Created by Dr. Kat Barger 05/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (NOT keyword_set(dir1)) then dir1='./'
if (NOT keyword_set(dir2)) then dir2='./'
if (NOT keyword_set(combine_dir)) then combine_dir='./'

dir1=validate_directory(dir1)
dir2=validate_directory(dir2)
combine_dir=validate_directory(combine_dir)

file1=validate_extension(file1,'sav')
file2=validate_extension(file2,'sav')
combine_name=validate_extension(combine_name,'sav')

restore,dir1+file1
restore,dir2+file2

file1_root=rm_ext(file1,'sav',/dash)
file2_root=rm_ext(file2,'sav',/dash)

map1=(SCOPE_VARFETCH(file1_root,/enter,level=0))
map2=(SCOPE_VARFETCH(file2_root,/enter,level=0))

n_tags_map1=n_tags(map1)
n_tags_map2=n_tags(map2)

if n_tags_map1 ne n_tags_map2 then begin
   print,'*** Number of tags not equal ***'
   print,'*** Removing Magellanic Coordinants ***'
   remove_tags,map1,['mlon','mlat'],map1,/quiet
   remove_tags,map2,['mlon','mlat'],map2,/quiet
endif

duplicates=where((map1.glon eq map2.glon) AND (map1.glat eq map2.glat),count)

if n_elements(duplicates) eq n_elements(map1) then begin
	print,'*** Must pass unique maps ***'
    print,'*** No map created ***'
    return
endif else if count ne 0 then remove,duplicates,map1

combine_map=replicate(map1[0],n_elements(map1)+n_elements(map2))

combine_map[0:n_elements(map1)-1]=map1
combine_map[n_elements(map1):n_elements(map1)+n_elements(map2)-1]=map2

(SCOPE_VARFETCH(rm_ext(combine_name,'sav',/dash),/enter,level=0))=combine_map

	save,filename=combine_dir+combine_name,(SCOPE_VARFETCH(rm_ext(combine_name,'sav',/dash),/enter,level=0))

end