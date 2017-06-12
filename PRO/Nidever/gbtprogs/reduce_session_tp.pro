pro reduce_session_tp,number,project=project,smthoff=smthoff,remcont=remcont

; This runs getfs on all the data from one
; session and saves them in separate SDFITS
; files for each source.
;
; smthoff=8 is good to use. reduced the noise by sqrt(2)

if n_elements(number) eq 0 then begin
  print,'No Session Number'
  return
endif

if n_elements(project) eq 0 then project='GBT09A_046'
strnum = strtrim(long(number),2)
if number lt 10 then strnum='0'+strnum

;dir = '/Users/dnidever/observing/gbt/chynoweth/data/A'+project+'_'+strnum+'_SDFITS/'
;file = dir+'none.raw.acs.fits'

dir = '/Users/dnidever/observing/gbt/chynoweth/data/'
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
  filebase = dir+names[i]+'_ses'+strtrim(number,2)+tag+'_tp'
  if FILE_TEST(filebase+'.fits') eq 1 then FILE_DELETE,filebase+'.fits'
  if FILE_TEST(filebase+'.index') eq 1 then FILE_DELETE,filebase+'.index'
  fileout,filebase+'.fits'

  ; Loop through the Scans
  for j=0,nindsc-1 do begin
    scinfo = iscaninfo[j]
    print,'  ',strtrim(scinfo.scan,2),'  ',strtrim(scinfo.n_integrations,2),' integrations'

    ; Integration Loop
    For k=0,scinfo.n_integrations-1 do begin

      ; Polarization Loop
      For p=0,1 do begin
        if p eq 0 then pol='XX' else pol='YY'

        ;; Sig/Ref Loop
        ;For s=0,1 do begin
        ;  if s eq 0 then sig='T' else sig='F'
        sig = 'T'  ; no reference
        
          ; Get the data
          get,scan=scinfo.scan,pol=pol,ifnum=0,fdnum=0,int=k,sig=sig,cal='T'
          calon = getdata()
          dum = where(finite(calon) eq 1,ngdon)
          get,scan=scinfo.scan,pol=pol,ifnum=0,fdnum=0,int=k,sig=sig,cal='F'
          caloff = getdata()
          dum = where(finite(caloff) eq 1,ngdoff)
          nel = n_elements(calon)
          tcal = !g.s[0].mean_tcal
          ; Calculate Tsys
          ;  Try to deal with situations where one half is missing
          if ngdon gt 0 and ngdoff gt 0 then cs=1
          if ngdon gt 0 and ngdoff eq 0 then cs=2
          if ngdon eq 0 and ngdoff gt 0 then cs=3
          if ngdon eq 0 and ngdoff eq 0 then cs=4
          Case cs of
            ; Both okay
            1: begin
              frac = median(calon[0.1*nel:0.9*nel]/caloff[0.1*nel:0.9*nel])
              tsys = median(caloff[0.1*nel:0.9*nel])/median((calon-caloff)[0.1*nel:0.9*nel])*tcal+tcal*0.5
              ; Average the calon/caloff spectra and calibrate
              avgspec = (calon+caloff)*0.5
              calavgspec = avgspec * tsys   ; calibrate
             end
            ;  CALON okay
            2: begin
              print,'pol=',pol,' sig=',sig,' caloff bad'
              ; Use Tsys and FRAC from previous one
              fake_caloff = calon/frac
              ; Average the calon/caloff spectra and calibrate
              avgspec = (calon+fake_caloff)*0.5
              calavgspec = avgspec * tsys   ; calibrate
             end
            ; CALOFF okay
            3: begin
              print,'pol=',pol,' sig=',sig,' calon bad'
              ; Use Tsys and FRAC from previous one
              fake_calon = caloff*frac
              ; Average the calon/caloff spectra and calibrate
              avgspec = (fake_calon+caloff)*0.5
              calavgspec = avgspec * tsys   ; calibrate
             end
            ; Both BAD
            4: begin
               ; Nothing we can do
               print,'BOTH BAD'
               calavgspec = calon
             end
            else: stop
          ENDCASE

          ; Now get the "continuum" and remove it
          if keyword_set(remcont) then begin
            sm1 = GSMOOTH(calavgspec,100)
            med1 = median(calavgspec,100) ; med is slightly better
            x = findgen(nel)
            std = MAD(calavgspec-sm1,/zero)
           ; ; B-spline fit
           ; invvar = fltarr(n_elements(nel))+1.0/std^2
           ; ;if nbd gt 0 then invvar[bd] = 0
           ; nord = 3           ;4
           ; bkspace = 50
           ; ;invvar[0:59]=0
           ; ;invvar[280:*]=0
           ; dum = bspline_iterfit(x,calavgspec,invvar=invvar,nord=nord,bkspace=bkspace,yfit=model1)
; need to add "idlutils" to path
            ; Use a wider median for zero velocity
            med2 = median(calavgspec[7000L:15000L],1000)
            ; Continuum, paste together
            cont = med1
            cont[9000L:12000L] = med2[2000L:5000L]
            ; second pass, remove positive "outliers" with signal
            temp = calavgspec
            std2 = mad(calavgspec/cont-1)
            bd = where( (calavgspec/cont-1) gt 2*std2 and abs(x-10900.) lt 1000,nbd)
            if nbd gt 0 then begin
              temp[bd] = !values.f_nan
              med3 = median(temp[7000L:15000L],1000)
              cont[9000L:12000L] = med3[2000L:5000L]
            endif
            ; Calibrate
            calspec = calavgspec/cont-1
          endif
            
          ; Now stuff the data back in and update Tsys and units
          !g.s[0].tsys = tsys
          !g.s[0].units = 'Ta'
          if keyword_set(remcont) then (*!g.s[0].data_ptr)=calspec else $
            (*!g.s[0].data_ptr) = calavgspec

          keep ; save to file

          ;stop

        ;end ; sig/ref loop
      end ; polarization loop
    end  ; integration loop

    ;stop

    BOMB:

  end ; scan loop

  ;stop

end ; source loop

;stop

print,''
print,'DONE with SESSION ',strtrim(number,2)
print,''

unfreeze

;stop

end
