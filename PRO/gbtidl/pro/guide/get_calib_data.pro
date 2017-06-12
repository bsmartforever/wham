;+
; Used by the calibration routines to actually fetch the necessary
; data.
;
; This is not meant to be called directly by the user.  Error messages
; generated here are displayed using the prefix from the calling
; routine.  It is expected that some argument checking will have
; happened prior to this function being called.  The only checks done
; here are that the requested data (ifnum, plnum, fdnum, and intnum)
; are consistent with the given scan info.
;
; <p>The returned values are the array of data containers found in the
; current line data source that satisfy the request using the provided
; scan info structure to indentify the scan.
;
; <p>If there is a problem, the returned value is -1 and count is 0.
;
; @param info {in}{required}{type=structure} The scan_info
; structure that describes the scan.  Use
; <a href="find_scan_info.html">find_scan_info</a> to get this
; scan_info.
; @param ifnum {in}{required}{type=integer} The IF number to
; fetch
; @param plnum {in}{required}{type=integer} The polarization
; number to fetch.
; @param fdnum {in}{required}{type=integer} The feed number to
; fetch.  Ignored when twofeeds is set.
; @param sampler {in}{required}{type=intege} The sampler name, an
; alternative to ifnum, plnum and fdnum.  This is used (and the others
; are ignored) when it is not empty.
; @param count {out}{optional}{type=integer} The number of data
; containers returned.  This is 0 when there is a problem.
; @keyword intnum {in}{optional}{type=integer} The specific
; integration to fetch.  If not supplied then fetch data from all
; integrations that match the other parameters.
; @keyword useflag {in}{optional}{type=boolean or string}{default=true}
; Apply all or just some of the flag rules?
; @keyword skipflag {in}{optional}{type=boolean or string} Do not apply
; any or do not apply a few of the flag rules?
; @keyword twofeeds {in}{optional}{type=boolean} When set (1), then
; this data must contain 2 and only two feeds and all data from both
; feeds is returned by this call.  In that case, fdnum is ignored.
; @keyword sig_state {in}{optional}{type=integer} When -1, this
; keyword is ignored, when 0 then the reference state is selected,
; when 1 then the sig state is selected.
; @returns an array of spectral line data containers that satisfy this
; request.  Returns -1 on error (count will also be 0 in that case).
;
; @private_file
; 
; @version $Id: get_calib_data.pro,v 1.6 2011/05/10 19:36:30 bgarwood Exp $
;-
function get_calib_data,info, ifnum, plnum, fdnum, sampler, count, $
                        intnum=intnum, useflag=useflag, skipflag=skipflag, $
                        twofeeds=twofeeds, sig_state=sig_state
                        
    compile_opt idl2

    count = 0


    thisIF = ifnum
    thisPL = plnum
    thisFD = fdnum

    thisSig = -1
    sigVal = ''
    if n_elements(sig_state) ne 0 then begin
        thisSig = sig_state
        sigVal = 'T'
        if thisSig eq 0 then begin
            sigVal = 'F'
        endif
    endif

    if (strlen(sampler) gt 0) then begin
        if where(info.samplers eq sampler) lt 0 then begin
            message,'Illegal sampler name: ' + sampler + '.  Choose from : ' + strjoin(info.samplers,' '), level=-1,/info
            return,-1
        endif
        ; samplerinfo is too slow, select on sampler directly
        ; for twofeeds case, ifnum is the tracking if number, 
        ; it will be used eventually even here - check it now.
        if keyword_set(twofeeds) then begin
            if info.n_feeds ne 2 then begin
                message,"Scan "+strtrim(string(info.scan),2)+$
                        " does not have two feeds.  n_feeds = "+strtrim(string(info.n_feeds),2), /info
                return,-1
            endif
            if thisFD lt 0 or thisFD gt 1 then begin
                message,"Invalid feed: " + strcompress(string(thisFD),/remove_all) + ".  fdnum must be 0 or 1",/info
                return,-1
            endif
        endif

        if n_elements(intnum) eq 1 then begin
            if intnum le (info.n_integrations-1) then begin
                if thisSig ge 0 then begin
                    data = !g.lineio->get_spectra(count,scan=info.scan,sampler=sampler,$
                                                  int=intnum,sig=sigVal,$
                                                  file=info.file,timestamp=info.timestamp,$
                                                  useflag=useflag,skipflag=skipflag)
                endif else begin
                    data = !g.lineio->get_spectra(count,scan=info.scan,sampler=sampler,$
                                                  int=intnum, $
                                                  file=info.file,timestamp=info.timestamp,$
                                                  useflag=useflag,skipflag=skipflag)
                endelse
            endif else begin
                message,'Integration number out of range',level=-1,/info
                return,-1
            endelse
        endif else begin
            if thisSig ge 0 then begin
                data = !g.lineio->get_spectra(count,scan=info.scan,sampler=sampler,$
                                              sig=sigVal,$
                                              file=info.file,timestamp=info.timestamp,$
                                              useflag=useflag,skipflag=skipflag)
            endif else begin
                data = !g.lineio->get_spectra(count,scan=info.scan,sampler=sampler,$
                                              file=info.file,timestamp=info.timestamp,$
                                              useflag=useflag,skipflag=skipflag)
            endelse
        endelse
               
        if keyword_set(twofeeds) then begin
            ; must find the other data and select it as usuall
            thisIF = data[0].if_number
            thisPL = data[0].polarization_num
            thisFD = data[0].feed_num
        
            fdnum2 = (thisFD eq 0) ? 1 : 0

            thispol = info.polarizations[thisPL]
            thisfeed = info.feeds[fdnum2]
        
            if n_elements(intnum) eq 1 then begin
                if intnum le (info.n_integrations-1) then begin
                    if thisSig ge 0 then begin
                        data2 = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,$
                                                       ifnum=thisIF,pol=thispol,int=intnum,$
                                                       sig=sigVal,$
                                                       file=info.file,timestamp=info.timestamp,$
                                                       useflag=useflag,skipflag=skipflag)
                    endif else begin
                        data2 = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,$
                                                       ifnum=thisIF,pol=thispol,int=intnum,$
                                                       file=info.file,timestamp=info.timestamp,$
                                                       useflag=useflag,skipflag=skipflag)
                    endelse
                endif else begin
                    message,'Integration number out of range',level=-1,/info
                    return,-1
                endelse
            endif else begin
                if thisSig ge 0 then begin
                    data2 = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,ifnum=thisIF,$
                                                   pol=thispol,file=info.file,timestamp=info.timestamp,$
                                                   sig=sigVal,$
                                                   useflag=useflag,skipflag=skipflag)
                endif else begin
                    data2 = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,ifnum=thisIF,$
                                                   pol=thispol,file=info.file,timestamp=info.timestamp,$
                                                   useflag=useflag,skipflag=skipflag)
                endelse
            endelse

            if thisFD eq fdnum then begin
                data = [data,data2]
            endif else begin
                data = [data2,data]
            endelse
        endif

    endif else begin
        ; regular ifnum, plnum, fdnum selection
        if thisIF gt (info.n_ifs-1) then begin
            sifnum = strcompress(string(thisIF),/remove_all)
            snif = strcompress(string(info.n_ifs),/remove_all)
            message,'Illegal IF identifier: ' + sifnum + '.  This scan has ' + $
                    snif + ' IFs, zero-indexed.', level=-1, /info
            return,-1
        endif
        
        if max(info.plnums) eq (info.n_polarizations-1) then begin
                                ; must be a simple sequence of plnums
            if thisPL gt (info.n_polarizations-1) then begin
                spol = strcompress(string(thisPL),/remove_all)
                snpol = strcompress(string(info.n_polarizations),/remove_all)
                message, 'Invalid polarization identifier: ' + spol + $
                         '.  This scan has ' + snpol + $
                         ' polarizations, zero-indexed.', level=-1, /info
                return,-1
            endif
        endif else begin
                                ; not all possibly plnums present, search for this
                                ; one specifically
            if where(info.plnums eq thisPL) lt 0 then begin
                spol = strcompress(string(thisPL),/remove_all)
                message, 'Invalid poliarization identifier: ' + spol + $
                         '.  That plnum is not found in this scan',level=-1,/info
                return,-1
            endif
        endelse
        
        
        if keyword_set(twofeeds) then begin
            if info.n_feeds ne 2 then begin
                message,"Scan "+strtrim(string(info.scan),2)+$
                        " does not have two feeds.  n_feeds = "+strtrim(string(info.n_feeds),2), /info
                return,-1
            endif
            if thisFD lt 0 or thisFD gt 1 then begin
                message,"Invalid feed: " + strcompress(string(thisFD),/remove_all) + ".  fdnum must be 0 or 1",/info
                return,-1
            endif
            fdnum2 = (thisFD eq 0) ? 1 : 0
            thisfdnum = [thisFD,fdnum2]
        endif else begin
            if thisFD gt (info.n_feeds-1) then begin
                message,'Invalid feed: ' + strcompress(string(thisFD),/remove_all) + $
                        '. This scan has ' + strcompress(string(info.n_feeds),/remove_all) + $
                        ' feeds, zero-indexed.',level=-1,/info
                return,-1
            endif
            thisfdnum = thisFD
        endelse
        
        thispol = info.polarizations[thisPL]
        thisfeed = info.feeds[thisfdnum]
        
        if n_elements(intnum) eq 1 then begin
            if intnum le (info.n_integrations-1) then begin
                if thisSig ge 0 then begin
                    data = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,$
                                                  ifnum=thisIF,pol=thispol,int=intnum,$
                                                  sig=sigVal,$
                                                  file=info.file,timestamp=info.timestamp,$
                                                  useflag=useflag,skipflag=skipflag)
                endif else begin
                    data = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,$
                                                  ifnum=thisIF,pol=thispol,int=intnum,$
                                                  file=info.file,timestamp=info.timestamp,$
                                                  useflag=useflag,skipflag=skipflag)
                endelse
            endif else begin
                message,'Integration number out of range',level=-1,/info
                return,-1
            endelse
        endif else begin
            if thisSig ge 0 then begin
                data = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,ifnum=thisIF,$
                                              pol=thispol,file=info.file,timestamp=info.timestamp,$
                                              sig=sigVal,$
                                              useflag=useflag,skipflag=skipflag)
            endif else begin
                data = !g.lineio->get_spectra(count,scan=info.scan,feed=thisfeed,ifnum=thisIF,$
                                              pol=thispol,file=info.file,timestamp=info.timestamp,$
                                              useflag=useflag,skipflag=skipflag)
            endelse
        endelse
    endelse

    return,data
end
    
