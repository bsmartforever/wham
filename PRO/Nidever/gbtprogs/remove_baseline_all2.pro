pro remove_baseline_all2,redo=redo

; This runs "remove_baseline" on all data
; and makes an HTML page

;dir = '/net/halo/dln5q/doradus/research/observing/gbt/GBT11B-082/'
dir = '/Volumes/Data/net/halo/dln5q/doradus/research/observing/gbt/GBT11B-082/'
;tag1 = 'rfifix'
tag1 = '_rfifix2'
files1 = file_search(dir+'data/MS*_ses*'+tag1+'.fits',count=nfiles1)
files2 = file_search(dir+'data/grid1_missing1*'+tag1+'.fits',count=nfiles2)
files = [files1,files2]
nfiles = n_elements(files)

; Remove any "tp" files
bd = where(stregex(files,'_tp',/boolean) eq 1,nbd)
if nbd gt 0 then remove,bd,files

; Remove test scan "MSTIP-center_ses09_s9-9"
bd = where(stregex(files,'MSTIP-center_ses09_s9-9',/boolean) eq 1,nbd)
;bd = where(file_basename(files,'.fits') eq 'MSTIP-center_ses09_s9-9',nbd)
if nbd gt 0 then remove,bd,files

nfiles = n_elements(files)

;tag = '_nomed'
;tag = '_indiv0'
;tag = '_all2'
;tag = '_all3'
;tag = '_all2'
;tag = '_rfifix1'
;tag = '_rfifix2'
tag = '_rfifix3'

; Getting number of scans, only keep Nscan>1
bases = file_basename(files,'.fits')
;scans = strarr(nfiles)
nscans = lonarr(nfiles)
for i=0,nfiles-1 do begin
  dum = strsplit(bases[i],'_',/extract)
  ndum = n_elements(dum)
  scans = dum[ndum-2]
  scans2 = strsplitter(scans,'-',/extract)
  scan1 = reform(scans2[0,*])
  scan1 = long(strmid(scan1,1,10))
  scan2 = long(reform(scans2[1,*]))
  nscans[i] = scan2-scan1+1
endfor
;str = strsplitter(bases,'_',/extract)
;scans = reform(str[2,*])
;scans2 = strsplitter(scans,'-',/extract)
;scan1 = reform(scans2[0,*])
;scan1 = long(strmid(scan1,1,10))
;scan2 = long(reform(scans2[1,*]))
;nscans = scan2-scan1+1


;;gd = where(nscans gt 1 and nscans le 37,ngd)
;gd = where(nscans gt 1,ngd)
;files = files[gd]
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
;for i=28,nfiles-1 do begin

  ;source = file_basename(files[i],'.fits')
  ;source = file_basename(files[i],'_rfifix.fits')
  source = file_basename(files[i],tag1+'.fits')
  print,''
  print,'----------------------------------------'
  print,strtrim(i+1,2),'/',strtrim(nfiles,2),' ',source
  print,'----------------------------------------'

  push,lines,'<tr><td>'+source+'</td>'

  file1 = source+'_intchan'+tag+'.gif'
  test1 = file_test(dir+'plots/'+file1)
  file2 = source+'_rfi'+tag+'.gif'
  test2 = file_test(dir+'plots/'+file2)
  ;file2 = source+'_map'+tag+'.gif'
  ;test2 = file_test(dir+'plots/'+file2)
  ;test2 = 1
  if test1 eq 0 or keyword_set(redo) then begin
  ;if test1 eq 0 or test2 eq 0 or keyword_set(redo) then begin
    REMOVE_BASELINE_INDIV2,files[i],/save,tag=tag
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
