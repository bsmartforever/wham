pro reduce_session_getfs,number,project=project,smthoff=smthoff

; This runs getfs on all the data from one
; session and saves them in separate SDFITS
; files for each source.
;
; smthoff=8 is good to use. reduced the noise by sqrt(2)

if n_elements(number) eq 0 then begin
  print,'No Session Number'
  return
endif

if n_elements(smthoff) eq 0 then smthoff=16
print,'Using SMTHOFF=',strtrim(smthoff,2)

;dir = '/home/scratch/dnidever/'
;dir = userdir()+'observing/gbt/data/'
;dir = '/net/halo/dln5q/doradus/research/observing/gbt/data/'
;dir = '/net/halo/dln5q/doradus/research/observing/gbt/GBT11B-082/data/'
dir = '~/observing/gbt/GBT13B-068/data/'

;strnum = strtrim(long(number),2)
;if long(number) lt 10 then strnum='0'+strnum
strnum = strtrim(number,2)
if size(number,/type) ne 7 then strnum=strtrim(string(long(number),format='(I02)'),2)
if n_elements(project) eq 0 then project='GBT13B_068'
file = dir+'A'+project+'_'+strnum+'.raw.acs.fits'

fil = file_search(file)

; No file
if fil(0) eq '' then begin
  print,'File ',file,' NOT FOUND'
  print,'The sdfits file is NOT found'
  return
endif

; Load the file
filein,file

; Freeze the screen
freeze


; GETTING THE SCANS
; allscans is all scan numbers that you would see if you
; used the LIST command.  There are 4 scan numbers per integration
; (SIG OFF & CAL ON, SIG ON & CAL ON, SIG OFF & CAL OFF, 
;  and SIG ON  & CAL OFF), and 2 polarizations as well.
; There are also 37 for a RALONGMAP.  So there are many scan numbers.
;scans = !g.lineio->get_index_scans()
allscans = !g.lineio->get_index_values("SCAN")
ui = uniq(allscans,sort(allscans))
scans = allscans(ui)         ; actual "scans"
nscans = n_elements(scans)

; Getting integrations per scan
nint = lonarr(nscans)
sources = strarr(nscans)
for i=0,nscans-1 do begin
  scaninfo = scan_info(scans[i],/quiet)
  nint(i) = scaninfo.n_integrations
  ;ADD_TAG,scaninfo,'SOURCE',''
  sources[i] = !g.lineio->get_index_values('SOURCE',index=scaninfo.index_start)
  ;scaninfo.source = source
  PUSH,allscaninfo,scaninfo
end
sources = strtrim(sources,2)

; Only get RALongMAP ones
mapind = where(allscaninfo.procedure eq 'RALongMap',nmapind)
if nmapind eq 0 then begin
  print,'NO RALongMap scans for this session'
  return
endif
allscaninfo = allscaninfo[mapind]
sources = sources[mapind]

; Getting Source name and Subscan number
; subscan is the number that this is in the RALONGMAP, 0-36
;source = !g.lineio->get_index_values("SOURCE",index=ui)
;sub = !g.lineio->get_index_values("SUBSCAN",index=ui)

; Getting unique names
; UNIQ returns the last index of a contiguous chunk
;uis = uniq(source)  ;,sort(source))
uis = uniq(sources,sort(sources))
names = sources(uis)
nnames = n_elements(names)

print,''
print,strtrim(nnames,2),' SOURCES:'
print,names
print,''

;stop

; Loop through the sources
;for i=2,nnames-1 do begin
for i=0,nnames-1 do begin
  t0 = systime(1)

  indsc = where(sources eq names[i],nindsc)
  iscaninfo = allscaninfo[indsc]

  print,'SOURCE = ',names[i],'  NSCANS=',strtrim(nindsc,2)

  scan1 = min(iscaninfo.scan)
  scan2 = max(iscaninfo.scan)
  tag = '_s'+strtrim(scan1,2)+'-'+strtrim(scan2,2)
  ;if scan1 eq scan2 then tag='_'+strtrim(scan1,2)
  ;filebase = '/local/dln5q/research/observing/gbt/data/'+names[i]+'_ses'+strtrim(number,2)+tag
  ;filebase = '/net/halo/dln5q/doradus/research/observing/gbt/data/'+names[i]+'_ses'+strtrim(number,2)+tag
  ;filebase = '/net/halo/dln5q/doradus/research/observing/gbt/GBT11B-082/data/'+names[i]+'_ses'+strtrim(number,2)+tag
  filebase = '~/observing/gbt/GBT13B-068/data/'+names[i]+'_ses'+strtrim(number,2)+tag
  if FILE_TEST(filebase+'.fits') eq 1 then FILE_DELETE,filebase+'.fits'
  if FILE_TEST(filebase+'.index') eq 1 then FILE_DELETE,filebase+'.index'
  fileout,filebase+'.fits'

  ; Loop through the Scans
  for j=0,nindsc-1 do begin
    scinfo = iscaninfo[j]

    ; KLUDGES
    ;;if names[i] eq 'MS094.3-46.0' and scinfo.scan ge 78 and scinfo.scan le 92 then goto,bomb
    ;if names[i] eq 'MS100.3-40.0' and number eq 8 and scinfo.scan ge 215 and scinfo.scan le 218 then goto,bomb
    ;if names[i] eq 'MS090.3-38.0' and number eq 20 and scinfo.scan eq 75 then goto,bomb
    ;if names[i] eq 'MSTIP-survey' and number eq 1 and scinfo.scan eq 25 then goto,bomb
    ;if names[i] eq 'MSTIP-survey' and number eq 1 and scinfo.scan eq 26 then goto,bomb
    print,'  ',strtrim(scinfo.scan,2),'  ',strtrim(scinfo.n_integrations,2),' integrations'
    ;print,''

 
    ; This doesn't keep the polarizations together, but much faster
    ;   and that's how they are arranged in the raw SDFITS file
    getfs,scinfo.scan,plnum=0,/keepints,/quiet,smthoff=smthoff
    getfs,scinfo.scan,plnum=1,/keepints,/quiet,smthoff=smthoff

    ;; Loop through the integrations
    ;for k=0,scinfo.n_integrations-1 do begin
    ;  print,scinfo.scan,k,format='(I5,I5)'
    ;
    ;  ; Loop through the polarizations
    ;  for l=0,1 do begin
    ;
    ;    ;print,scinfo.scan,k,l,format='(I5,I5,I5)'
    ;    getfs,scinfo.scan,intnum=k,plnum=l,/quiet
    ;    keep
    ;
    ;    ; You could use "getfs,scan,/keepints",plnum=0
    ;    ;  but then the two polarizations won't be next to each other
    ;    ;  which might be okay, that's the way it is in the raw file
    ;
    ;    ;stop
    ;
    ;  end ; polarization loop
    ;
    ;   ;stop
    ;
    ;end ; integration loop

    ;stop

    BOMB:

  end ; scan loop

  ;print,'' 
  ;print,'dT = ',systime(1)-t0

  ;stop

end ; source loop

;stop

print,''
print,'DONE with SESSION ',strtrim(number,2)
print,''

unfreeze

;stop

end
