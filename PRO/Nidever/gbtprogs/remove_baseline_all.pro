pro remove_baseline_all,redo=redo

; This runs "remove_baseline" on all data
; and makes an HTML page

dir = '/local/dln5q/research/observing/gbt/'
files = file_search(dir+'data/MS*_ses*.fits',count=nfiles)

;tag = '_nomed'
;tag = '_indiv0'
;tag = '_all2'
;tag = '_all3'
tag = '_all4'

; Getting number of scans, only keep Nscan>1
bases = file_basename(files,'.fits')
str = strsplitter(bases,'_',/extract)
scans = reform(str[2,*])
scans2 = strsplitter(scans,'-',/extract)
scan1 = reform(scans2[0,*])
scan1 = long(strmid(scan1,1,10))
scan2 = long(reform(scans2[1,*]))
nscans = scan2-scan1+1

;gd = where(nscans gt 1 and nscans le 37,ngd)
gd = where(nscans gt 1,ngd)
files = files[gd]
nfiles = n_elements(files)

undefine,lines
push,lines,'<HTML>'
push,lines,'<HEAD>'
push,lines,'<TITLE>'
push,lines,'GBT MS Data'
push,lines,'</TITLE>'
push,lines,'</HEAD>'
push,lines,''
push,lines,'<BODY>'
push,lines,'<H1>GBT MS Data</H1>'
push,lines,'<hr>'
push,lines,'<p>'
push,lines,'<table border=1>'

print,strtrim(nfiles,2),' FILES'

for i=0,nfiles-1 do begin

  source = file_basename(files[i],'.fits')
  print,strtrim(i,2),' ',source
  push,lines,'<tr><td>'+source+'</td>'

  file1 = source+'_intchan'+tag+'.gif'
  test1 = file_test(dir+'plots/'+file1)
  file2 = source+'_rfi'+tag+'.gif'
  test2 = file_test(dir+'plots/'+file2)
  ;file2 = source+'_map'+tag+'.gif'
  ;test2 = file_test(dir+'plots/'+file2)
  ;test2 = 1

  if test1 eq 0 or test2 eq 0 or keyword_set(redo) then begin
    REMOVE_BASELINE_INDIV,files[i],/save,tag=tag
;    REMOVE_BASELINE_INDIV0,files[i] ;,/save,tag=tag
;    REMOVE_BASELINE_INDIV,files[i],/save,tag=tag
    test1 = file_test(dir+'plots/'+file1)
    test2 = file_test(dir+'plots/'+file2)
  endif

  ;width = '400'
  height = '400'
  if test1 eq 1 then begin
    push,lines,'<td><a href="'+file1+'"><img src="'+file1+'" height='+height+'></a></td>'
    ;push,lines,'<td><a href="'+file1+'"><img src="'+file1+'" width='+width+'></a></td>'
  endif else push,lines,'<td>NO FILE</td>'

  if test2 eq 1 then begin
    push,lines,'<td><a href="'+file2+'"><img src="'+file2+'" height='+height+'></a></td>'
  endif else push,lines,'<td>NO FILE</td>'

  ;if test2 eq 1 then begin
  ;  push,lines,'<td><a href="'+file2+'"><img src="'+file2+'" width='+width+'></a></td>'
  ;endif else push,lines,'<td>NO FILE</td>'

  push,lines,'</tr>'

end

push,lines,'</table>'
push,lines,'</BODY>'
push,lines,'</HTML>'

WRITELINE,dir+'plots/gbtms'+tag+'.html',lines

stop

end
