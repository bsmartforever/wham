;+
; This procedure retrieves a pair of total power scans and calibrates the 
; spectrum.  
;
; <p>The signal and reference scans are identified separately and do 
; not need to be associated in a single observing procedure.  This
; procedure can be used as a template for the user who may wish to
; develop more tailored calibration schemes.  The spectrum is
; calibrated in Ta (K) by default.  Other recognized units are Ta* and
; Jy. 
;
; <p><b>Summary</b>
;   <ul><li>Data are selected for the two scans using sigscan, refscan,
;      ifnum, intnum, plnum and fdnum or, alternatively, sampler and
;      intnum if you know the specific sampler name (e.g. "A10").
;      The same sampler name is used for both sigscan and refscan.
;
;   <li>If the avgref option is not set (the default) and both scans
;     have the same number of integrations, then individual integrations
;     are processed separately.  The same integration number in
;     sigscan and refscan are processed together. If the avgref option
;     is set or both scans do not have the same number of integrations;     then the total power value from each integration in refscan is found
;     using <a href="../toolbox/dototalpower.html">dototalpower</a>
;     and the individual integrations are averaged together to produce
;     one reference spectrum that is used for each integration of sigscan.
;     Each integration is processed using <a href="../toolbox/dofullsigref.html">dofullsigref</a>
;     If there is no cal switching (TPnoCal) then there is no attempt
;     to determine TCal and the sig and ref values are used as is.
;
;   <li>The integrations are calibrated in Ta (K) by default.  If
;      units of Ta* or Jy are requested via the units keyword, then 
;      <a href="../toolbox/dcsetunits.html">dcsetunits</a> is used to convert to the desired units.
;
;   <li>Averaging of individual integrations is then done using 
;      <a href="../toolbox/dcaccum.html">dcaccum</a>.  By default, integrations are weighted as described in dcaccum.
;      If the eqweight keyword is set, then integrations are averaged
;      with an equal weight.
;
;   <li>The final average is left in the primary data container
;      (buffer 0), and a summary line is printed.  The printing of the
;      summary line can be suppressed by setting the quiet keyword.
;
;   <li>The individual integration results can be saved to the
;      currently opened output file by setting the keepints keyword.
;      The final average is still produced in that case.
;   </ul>
; <p><b>Parameters</b>
; <p>
; Arguments for sig and ref scan numbers are required.  Arguments to
; identify the IF number, polarization number, and feed number are
; optional.  The default feed number (0) is the lowest numbered FEED
; found in the data.  
; <p>
; <b>Tsys and Available Units</b>
; <p>
; The procedure calculates Tsys based on the Tcal values and the data
; in the reference scan.  The user can override this calculation by
; entering a zenith system temperature.  The procedure will then
; correct the user-supplied Tsys for the observed elevation of the
; reference scan. If the data are calibrated to Ta* or Jy, additional
; parameters are used.  A zenith tau value may be specified, and an
; aperture efficiency may be specified.  The user is strongly
; encouraged to enter values for these calibration parameters, but
; they will be estimated if none are provided.  The user can also
; supply a mean tcal using the tcal keyword.  That will override the
; tcal found in the data. 
; <p>
; <b>Smoothing the Reference Spectra</b>
; <p>
; A parameter called smthoff can be used to smooth the reference
; spectrum prior to calibration of each integration.  In certain cases
; this can improve the signal to noise ratio, but it may degrade
; baseline shapes and artificially emphasize spectrometer glitches.
; Use with care.   A value of smthoff=16 is often a good choice. 
; <p> 
; <b>Weighting of Integrations in Scan Average</b>
; <p> 
; By default, the averaging of integrations is weighted using tsys,
; exposure, and frequency_resolution as described in the <a href="../toolbox/dcaccum.html">dcaccum</a>
; documentation.  To give all integrations equal weight instead of the
; default weighting based on Tsys, use the /eqweight keyword.  This
; same weighting is used when averaging the reference scans if the
; avgref option is on or there are different numbers of integrations
; in each scan.
; <p>
; <b>Using or Ignoring Flags</b>
; <p>
; Flags (set via <a href="flag.html">flag</a>) can be selectively
; applied or ignored using the useflag and skipflag keywords.  Only
; one of those two keywords can be used at a time (it is an error to
; use both at the same time).  Both can be either a boolean (/useflag
; or /skipflag) or an array of strings.  The default is /useflag,
; meaning that all flag rules that have been previously set are
; applied when the data is fetched from disk, blanking data as
; described by each rule.  If /skipflag is set, then all of the flag
; rules associated with this data are ignored and no data will be
; blanked when fetched from disk (it may still contain blanked values
; if the actual values in the disk file have already been blanked by
; some other process).  If useflag is a string or array of strings,
; then only those flag rules having the same idstring value are used
; to blank the data.  If skipflag is a string or array of strings,
; then all flag rules except those with the same idstring value are
; used to blank the data. 
;
; <p>
; <b>Dealing With Duplicate Scan Numbers</b>
; <p>
; There are 3 ways to attempt to resolve ambiguities when the
; same scan number appears in the data source.  The instance keyword
; refers to the element of the returned array of scan_info structures
; that <a href="scan_info.html">scan_info</a> returns.  So, if scan 23
; appears 3 times then instance=1 refers to the second time that scan
; 23 appears as returned by scan_info.  The file keyword is useful if
; a scan is unique to a specific file and multiple files have been
; accessed using <a href="dirin.html">dirin</a>.  If file is specified and instance is also 
; specified, then instance refers to the instance of that scan just
; within that file (which may be different from its instance within 
; all opened files when dirin is used).  The timestamp keyword is
; another way to resolve ambiguous scan numbers.  The timestamp here
; is a string used essentially as a label by the monitor and control
; system and is unique to each scan.  The format of the timestamp
; string is "YYYY_MM_DD_HH:MM:SS".  When timstamp is given, scan and
; instance are ignored.  If more than one match is found, an error is 
; printed and this procedure will not continue.  These are specified
; independently for each of the two scans (sigscan and refscan).;
; @param sigscan {in}{required}{type=integer} M&C scan number for the
; signal scan
; @param refscan {in}{required}{type=integer} M&C scan number for the
; reference scan
; @keyword ifnum {in}{optional}{type=integer} IF number (starting with
; 0), defaults to 0.
; @keyword intnum {in}{optional}{type=integer} integration number,
; defaults to all integrations. 
; @keyword plnum {in}{optional}{type=integer} polarization number (0
; or 1), defaults to 0.
; @keyword fdnum {in}{optional}{type=integer} feed number, defaults
; to 0.
; @keyword sampler {in}{optional}{type=string} sampler name, this is
; an alternative way to specify ifnum,plnum, and fdnum.  When sampler
; name is given, ifnum, plnum, and fdnum must not be given.
; @keyword tau {in}{optional}{type=float} tau at zenith, if not
; supplied, it is estimated using <a href="../toolbox/get_tau.html">get_tau</a>
; tau is only used when the requested units are other than the default
; of Ta and when a user-supplied tsys value at zenith is to be used.
; @keyword tsys {in}{optional}{type=float} tsys at zenith, this is
; converted to a tsys at the observed elevation.  If not suppled, the
; tsys for each integration is calculated as described elsewhere.
; @keyword ap_eff {in}{optional}{type=float} aperture efficiency, if
; not suppled, it is estimated using <a href="../toolbox/get_ap_eff.html">get_ap_eff<a>
; ap_eff is only used when the requested units are Jy.
; @keyword smthoff {in}{optional}{type=integer} smooth factor for
; reference spectrum, default is no smoothing (1).
; @keyword units {in}{optional}{type=string} takes the value 'Jy',
; 'Ta', or 'Ta*', default is Ta.
; @keyword eqweight {in}{optional}{type=boolean} When set, all
; integrations are averaged with equal weight (1.0), default is unset.
; @keyword tcal {in}{optional}{type=float} Cal temperature (K) to use
; in the Tsys calculation.  If not supplied, the mean_tcal value from
; the header of the cal_off switching phase data in each integration
; is used.  This must be a scalar, vector tcal is not yet supported.
; The resulting data container will have it's mean_tcal header value
; set to this keyword when it is set by the user.
; @keyword avgref {in}{optional}{type=boolean}{default=unset} When set, the total
; power values for the individual integrations in refscan are averaged
; together using the current weighting option (using Tsys or equal
; weighting) to produce a single reference spectrum that is then used to
; calibrate each integration of sigscan.  This option will
; automatically be selected whenever the number of integrations in
; refscan is not the same as in sigscan.
; @keyword quiet {in}{optional}{type=boolean} When set, the normal
; status message on successful completion is not printed.  This will
; not affect error messages.  default is unset.
; @keyword avgref {in}{optional}{type=boolean} When set, the ref scan
; will first be processed to get the total power (if cal switching is
; on for that data) and then all integrations will be averaged to
; produce the reference spectrum to be used to calibrate the signal
; scan. If the two scans have an unequal number of integrations then
; this will be done no matter what the user-set value for this keyword
; is.  default is unset.
; @keyword keepints {in}{optional}{type=boolean} When set, the
; individual integrations are saved to the current output file
; (fileout).  This is ignored if a specific integration is requested
; using the intnum keyword.  Default is unset.
; @keyword useflag {in}{optional}{type=boolean or string}
; Apply all or just some of the flag rules?  Default is set.
; @keyword skipflag {in}{optional}{type=boolean or string} Do not apply
; any or do not apply a few of the flag rules?  Default is unset.
; @keyword siginstance {in}{optional}{type=integer} Which occurrence
; of sigscan should be used, default is 0.
; @keyword sigfile {in}{optional}{type=string} When specified, limit the search 
; for sigscan (and instance) to this specific file, default is all files.
; @keyword sigtimestamp {in}{optional}{type=string} The M&C timestamp associated
; with the desired signal scan. When supplied, sigscan and siginstance are ignored.
; @keyword refinstance {in}{optional}{type=integer} Which occurrence
; of refscan should be used, default is 0.
; @keyword reffile {in}{optional}{type=string} When specified, limit the search 
; for refscan (and instance) to this specific file, default is all files.
; @keyword reftimestamp {in}{optional}{type=string} The M&C timestamp associated
; with the desired reference scan. When supplied, refscan and refinstance are ignored.
; @keyword status {out}{optional}{type=integer} An output parameter to indicate
; whether the procedure finished as expected.  A value of 1 means there were
; no problems, a value of -1 means there were problems with the
; arguments before any data was processed, and a value of 0 means that
; some of the individual integrations were processed (and possibly
; saved to the output file if keepints was set) but there was a
; problem with the final average and buffer 0 likely
; contains just the result from the last integration processed. This
; keyword is primarily of use when using getsigref with another
; procedure or function.
;
; @examples
; Suppose scans 13 and 14 are a position switched pair and scans 21
; and 22 are also a position switched pair.  There was a problem with
; the off or reference scan in 13 and 14 (scan 13 in this example) so
; you want to use the other reference scan (scan 21) in it's place.
; Note that as this is currently written, for this to work scan 21
; must have the same number of integrations and switching phases as
; scan 13.  If that isn't the case, you may want to look at using the
; underlying dosigref procedure to work with each integration or
; an average of all integrations in each scan directly.
;
; <pre>
;   getsigref, 14, 21
;   ;
;   ; you could do this, uses dosigref directly
;   ;
;   gettp, 14        ; averages total power of all ints in scan 14
;   copy, 0, 10      ; remember for later use
;   gettp, 21        ; total power avg in scan 21
;   copy, 0, 11
;   dosigref, result, !g.s[10], !g.s[11]
;   set_data_container, result ; put result into buffer 0
;   data_free, result ; delete the pointer in result
; </pre>
; @uses <a href="../toolbox/accumave.html">accumave</a>
; @uses <a href="../toolbox/accumclear.html">accumclear</a>
; @uses <a href="../../devel/guide/calsummary.html">calsummary</a>
; @uses <a href="../../devel/guide/check_calib_args.html">check_calib_args</a>
; @uses <a href="../toolbox/data_free.html">data_free</a>
; @uses <a href="../toolbox/dcaccum.html">dcaccum</a>
; @uses <a href="../toolbox/dcscale.html">dcscale</a>
; @uses <a href="../toolbox/dcsetunits.html">dcsetunits</a>
; @uses <a href="../toolbox/dofullsigref.html">dofullsigref</a>
; @uses <a href="../../devel/guide/find_scan_info.html">find_scan_info</a>
; @uses <a href="../../devel/guide/get_calib_data.html">get_calib_data</a>
; @uses <a href="set_data_container.html">set_data_container</a>
;
; @version $Id: getsigref.pro,v 1.43 2011/01/25 17:29:15 bgarwood Exp $
;-

pro getsigref,sigscan,refscan,ifnum=ifnum,intnum=intnum,plnum=plnum,fdnum=fdnum,sampler=sampler,tau=tau,$
              tsys=tsys,ap_eff=ap_eff,smthoff=smthoff,units=units,eqweight=eqweight, $
              tcal=tcal,quiet=quiet,avgref=avgref,keepints=keepints,useflag=useflag,skipflag=skipflag,$
              siginstance=siginstance,sigfile=sigfile,sigtimestamp=sigtimestamp,$
              refinstance=refinstance,reffile=reffile,reftimestamp=reftimestamp,$
              status=status
    compile_opt idl2

    status = -1

    ; basic argument checks
    argsOK=check_calib_args(sigscan,refscan,ifnum=ifnum,intnum=intnum,plnum=plnum,fdnum=fdnum,sampler=sampler, $
                            eqweight=eqweight,units=units,quiet=quiet,keepints=keepints,useflag=useflag, $
                            skipflag=skipflag,instance=siginstance,file=sigfile,$
                            timestamp=sigtimestamp,refinstance=refinstance,reffile=reffile,$
                            reftimestamp=reftimestamp,tau=tau,ap_eff=ap_eff,ret=ret,/checkref)
    if not argsOK then return

    ; Get the scan_info for the signal scan
    siginfo = find_scan_info(sigscan,timestamp=sigtimestamp,instance=siginstance,file=sigfile)
    if size(siginfo,/type) ne 8 then return

    ; And the reference scan
    refinfo = find_scan_info(refscan,timestamp=reftimestamp,instance=refinstance,file=reffile)
    if size(refinfo,/type) ne 8 then return

    ; Check for appropriateness and compatibility
    if siginfo.n_channels ne refinfo.n_channels then begin
        message,string("sig and ref scans have different numbers of channels."),/info
        return
    endif

    ; do we need to average the reference scan first?
    ;       - user choice or forced
    doAvgRef = keyword_set(avgref) or siginfo.n_integrations ne refinfo.n_integrations

    ; cal switching type
    if siginfo.n_cal_states gt 2 then begin
        message,'The number of cal states in the sig scan is not 1 or 2, as needed for this procedure',/info
        return
    endif
    if refinfo.n_cal_states gt 2 then begin
        message,'The number of cal states in the ref scan is not 1 or 2, as needed for this procedure',/info
        return
    endif

    doSigCalSwitch = siginfo.n_cal_states eq 2
    doRefCalSwitch = refinfo.n_cal_states eq 2

    ; Get the requested data
    sigdata = get_calib_data(siginfo, ret.ifnum, ret.plnum, ret.fdnum, ret.sampler, sigcount, $
                             intnum=intnum, useflag=useflag, skipflag=skipflag)
    if sigcount le 0 then begin
        message,'No data found for signal scan, can not continue',/info
        return
    endif
    if doAvgRef then begin
        ; ignore intnum in this case
        refdata = get_calib_data(refinfo, ret.ifnum, ret.plnum, ret.fdnum, ret.sampler, refcount, $
                                 useflag=useflag, skipflag=skipflag)
    endif else begin
        refdata = get_calib_data(refinfo, ret.ifnum, ret.plnum, ret.fdnum, ret.sampler, refcount, $
                                 intnum=intnum, useflag=useflag, skipflag=skipflag)
    endelse

    if refcount le 0 then begin
        message,'No data found for reference scan, can not continue',/info
        data_free,sigdata
        return
    endif

    ; from this point on, sigdata and refdata contain data containers 
    ; that must be freed whenever this routine returns to avoid memory leaks.

   ; ignore any SIGREF switching states

    if doSigCalSwitch then begin
        sigwcal  = where(sigdata.cal_state eq 1, countSigwcal)
        sig = where(sigdata.cal_state eq 0, countSig)
    endif

    if doRefCalSwitch then begin
        refwcal  = where(refdata.cal_state eq 1, countRefwcal)
        ref = where(refdata.cal_state eq 0, countRef)
    endif

    ; Final sanity checks

    ; In this calibration, we calibrate each integration separately
    ; and then average the results.  We know that there are 1 or 2
    ; cal states per integration and we are ignoring 
    ; all sigstates.
    nSigIntegrations = (n_elements(intnum) eq 1) ? 1 : siginfo.n_integrations
    expectedSigCount = nSigIntegrations * siginfo.n_sig_states
    if doAvgRef then begin
        nRefIntegrations = refinfo.n_integrations
    endif else begin
        nRefIntegrations = (n_elements(intnum) eq 1) ? 1 : refinfo.n_integrations
    endelse
    expectedRefCount = nRefIntegrations * refinfo.n_sig_states

    if doSigCalSwitch then begin
        if (countSigwcal ne expectedSigCount or countSigwcal ne countSig) then begin
            message,"Unexpected number of spectra retrieved for some or all of the switching phases in the signal scan, can not continue.",/info
            data_free, sigdata
            data_free, refdata
            return
        endif
    endif
    if doRefCalSwitch then begin
        if (countRefwcal ne expectedRefCount or countRefwcal ne countRef) then begin
            message,"Unexpected number of spectra retrieved for some or all of the switching phases in the reference scan, can not continue.",/info
            data_free, sigdata
            data_free, refdata
            return
        endif
    endif

    status = 0
    missing = 0

    if keyword_set(eqweight) then weight = 1.0 ; else undefined and use default weight
    if not doRefCalSwitch and not keyword_set(tsys) then tsys = 1.0

    if doAvgRef then begin
        thisaccum = {accum_struct}
        ; average ref data, cal switching or not
        for i = 0,(nRefIntegrations-1) do begin
            if doRefCalSwitch then begin
                dototalpower,result,refdata[refwcal[i]],refdata[ref[i]],tcal=tcal
            endif else begin
                data_copy,refdata[i],result
                result.tsys = tsys
            endelse
            dcaccum,thisaccum,result,weight=weight
        endfor
        naccum = thisaccum.n
        if naccum le 0 then begin
            message,'Average reference is all blanked - probably all of the reference data were flagged or are bad.',/info
            message,'Can not continue',/info
            set_data_container,result
            ; clean up
            accumclear, thisaccum
            data_free, sigdata
            data_free, refdata
            return
        endif else begin
            ; refAvg must be freed on return
            accumave,thisaccum,refAvg,/quiet
            accumclear, thisaccum
        endelse
    endif

    thisaccum = {accum_struct}
    tauInts = fltarr(nSigIntegrations)
    apEffInts = tauInts
    for i = 0,(nSigIntegrations-1) do begin
        if doAvgRef then begin
            if doSigCalSwitch then begin
                dofullsigref,result,sigdata[sigwcal[i]],sigdata[sig[i]],$
                  refAvg,refAvg,smthoff,doAvgRef,$
                  tsys=tsys,tau=tau,tcal=tcal
            endif else begin
                ; tsys keyword already used, if necessary, above
                dosigref,result,sigdata[i],refAvg,smthoff
            endelse
        endif else begin
            if doSigCalSwitch and doRefCalSwitch then begin
                dofullsigref,result,sigdata[sigwcal[i]],sigdata[sig[i]],$
                  refdata[refwcal[i]],refdata[ref[i]],smthoff,$
                  tsys=tsys,tau=tau,tcal=tcal   
            endif else begin
                if not doRefCalSwitch then begin
                    refdata[i].tsys = tsys
                    dosigref,result,sigdata[i],refdata[i],smthoff
                 endif else begin
                    dofullsigref,result,unused,sigdata[i],$
                                 refdata[refwcal[i]],refdata[ref[i]],smthoff,$
                                 tsys=tsys,tau=tau,tcal=tcal,/signocal
                 endelse
            endelse
        endelse
        ; convert to the desired units
        dcsetunits,result,units,tau=tau,ap_eff=ap_eff,ret_tau=ret_tau,ret_ap_eff=ret_ap_eff
        ; these are only used in the status line at the end
        tauInts[i] = ret_tau
        apEffInts[i] = ret_ap_eff
        dcaccum,thisaccum,result,weight=weight
        if keyword_set(keepints) then begin
            ; re-use raw data containers to conserve space
            ; defer the actual keep until later
            ; takes 3 steps because of the nature of IDL
            ; data passing (value vs reference)
            if doSigCalSwitch then begin
                tmp = sigdata[sigwcal[i]]
                data_copy, result, tmp
                sigdata[sigwcal[i]] = tmp
            endif else begin
                tmp = sigdata[i]
                data_copy, result, tmp
                sigdata[i] = tmp
            endelse
        endif
    endfor

    if keyword_set(keepints) then begin
        if doSigCalSwitch then begin
            putchunk, sigdata[sigwcal]
        endif else begin
            putchunk, sigdata
        endelse
    endif

    naccum = thisaccum.n
    if (naccum le 0) then begin
        message,'Result is all blanked - probably all of the data were flagged',/info
        ; clean up
        ; result must therefor be all blanked, use it as the end result
        set_data_container, result
        data_free, result
        accumclear, thisaccum
        data_free, sigdata
        data_free, refdata
        if doAvgRef then data_free, refAvg
        return
    endif
    accumave,thisaccum,result,/quiet
    missing = nSigIntegrations ne naccum
    accumclear, thisaccum
    
    set_data_container, result
    status = 1
    if not keyword_set(quiet) then begin
        if missing then nmiss = nSigIntegrations-naccum
        calsummary, siginfo.scan, result.tsys, result.units, tauInts=tauInts,$
                    apEffInts=apEffInts,refScan=refinfo.scan,missingInts=nmiss
    endif
    
    data_free, result
    data_free, sigdata
    data_free, refdata
    if doAvgRef then data_free, refAvg
    
end

