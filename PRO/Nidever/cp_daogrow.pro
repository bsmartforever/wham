pro cp_daogrow

; Copy all the a.als files to the daogrow/ directory

dir = '/net/halo/dln5q/ctio4m/'

for i=1,5 do begin

  ; Get the list of files
  alsfiles = file_search(dir+'n'+strtrim(i,2)+'/','*_*a.als') 
  apfiles = file_search(dir+'n'+strtrim(i,2)+'/','*_*a.ap') 

  ; Copy the files
  file_copy,alsfiles,dir+'daogrow/n'+strtrim(i,2)+'/',/overwrite
  file_copy,apfiles,dir+'daogrow/n'+strtrim(i,2)+'/',/overwrite

end


stop

end
