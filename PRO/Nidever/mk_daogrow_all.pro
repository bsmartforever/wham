pro mk_daogrow_all

; This runs mk_daogrow on all the nights

dir='/net/halo/dln5q/ctio4m/'

for i=1,5 do begin
  print,'Night ',strtrim(i,2)
  mk_daogrow,dir+'n'+strtrim(i,2)+'/','n'+strtrim(i,2)
  ;stop
end

;inffiles = file_search(dir,'n?.inf')
;extfiles = file_search(dir,'n?.inf')

file_copy,dir+'n1.inf',dir+'daogrow/n1/',/overwrite
file_copy,dir+'n2.inf',dir+'daogrow/n2/',/overwrite
file_copy,dir+'n3.inf',dir+'daogrow/n3/',/overwrite
file_copy,dir+'n4.inf',dir+'daogrow/n4/',/overwrite
file_copy,dir+'n5.inf',dir+'daogrow/n5/',/overwrite
file_copy,dir+'n1.ext',dir+'daogrow/n1/',/overwrite
file_copy,dir+'n2.ext',dir+'daogrow/n2/',/overwrite
file_copy,dir+'n3.ext',dir+'daogrow/n3/',/overwrite
file_copy,dir+'n4.ext',dir+'daogrow/n4/',/overwrite
file_copy,dir+'n5.ext',dir+'daogrow/n5/',/overwrite

stop

end
