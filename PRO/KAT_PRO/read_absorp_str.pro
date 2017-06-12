function read_absorp_str, name, dir=dir, cos=cos, stis=stis, fuse=fuse, instrument=instrument, inorm=inorm, imnorm=imnorm

if (NOT keyword_set(imnorm)) then inorm=1

if (NOT keyword_set(name)) then name='' $
else begin
  if strmatch(name,'*_',/fold_case) eq 0 then name=name+'_' 
endelse
if (NOT keyword_set(dir)) then dir='' $
else begin
  if strmatch(dir,'*/',/fold_case) eq 0 then dir=dir+'/' 
endelse
if (NOT keyword_set(cos)) then cos=0
if (NOT keyword_set(stis)) then stis=0

if keyword_set(inorm) then files=findfile(dir+'*_o.save',count=count) else $
files=findfile(dir+'*att',count=count)

if count eq 0 then begin
   print,''
   print,'*** NO FiLES MATCH INPUT! ***'
   print,''  
   return,''
endif

root=files ;initialize root
for i=0, n_elements(files)-1 do begin
    junk=strsplit(files[i],'/',/extract,/regex,/fold_case)
    if keyword_set(inorm) then $
      junk=strsplit(junk[n_elements(junk)-1],'_o.save',/extract,/regex,/fold_case) else $
      junk=strsplit(junk[n_elements(junk)-1],'.att',/extract,/regex,/fold_case)
    root[i]=junk

    if (keyword_set(inorm)) then $
    (SCOPE_VARFETCH(name+root[i], /ENTER,level=1))=inormreadstr(dir+root[i],/quiet,cos=cos,stis=stis,fuse=fuse,instrument=instrument) else $
    (SCOPE_VARFETCH(name+root[i], /ENTER,level=1))=imreadstr(dir+root[i],/quiet,cos=cos,stis=stis,fuse=fuse,instrument=instrument)

endfor

   if (name ne '') then return, name+root $
   else return,root; 

end