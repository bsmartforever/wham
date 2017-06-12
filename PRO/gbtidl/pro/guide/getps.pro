;+
; Procedure getps retrieves and calibrates a total power, position
; switched scan pair.  
; <p>
; Position switched data are usually taken with the observing
; procedures "OnOff" or "OffOn".  This routine can be used as a
; template for the user who may wish to develop more tailored
; calibration schemes. The spectrum is calibrated in Ta (K) by
; default.  Other recognized units are Ta* and Jy.
;
; <p><b>Summary</b>
;   <ul><li>Data are selected using scan, ifnum, intnum, plnum and
;      fdnum or, alternatively, sampler and intnum if you know the
;      specific sampler name (e.g. "A10"). The other scan in the scan
;      pair is found using scan (see comments below).  The same
;      sampler name is used for both scans.
;
;   <li>Individual integrations are processed separately with the
;      same integration number in the two scans processed together.
;      Both scans must have the same number of integrations.  Each
;      integration is processed using <a href="../toolbox/dofullsigref.html">dofullsigref</a>
; 
;   <li>The integrations are calibrated in Ta (K).  If units of Ta* or
;      Jy are requested via the units keyword, then  <a href="../toolbox/dcsetunits.html">dcsetunits</a> is used to
;      convert to the desired units.
;
;   <li>Averaging of individual integrations is then done using 
;      <a href="../toolbox/dcaccum.html">dcaccum</a>.  By default, integrations are weighted as described in dcaccum.
;      If the eqweight keyword is set, then integrations are averaged with an
;      equal weight.
;
;   <li>The final average is left in the primary data container
;      (buffer 0), and a summary line is printed.  The printing of the
;      summary line can be suppressed by setting the quiet keyword. The
;      first Tsys displayed is that of the result. This all comes from
;      the "Off" scan integrations.  The second Tsys is weighted average
;      of the Tsys values from the "On" scan integrations.
;
;   <li>The individual integration results can be saved to the
;      currently opened output file by setting the keepints keyword.  The
;      final average is still produced in that case.
;   </ul>
; <p><b>Parameters</b>
; <p>
; The scan number is a required parameter.  Either scan in the sequence of two 
; total power scans can be given, and the paired scan is determined from the 
; header.  Arguments to identify the IF number, polarization number and feed 
; number are optional.
; <p>
; <b>Tsys and Available Units</b>
; <p>
; The procedure calculates Tsys based on the Tcal values and the data
; in the "Off" scan.  The user can override this calculation by
; entering a zenith system temperature.  The procedure will then
; correct the user-supplied Tsys for the observed elevation.  If the
; data are calibrated to Ta* or Jy,  additional parameters are used.
; A zenith opacity (tau) may be specified, and an aperture efficiency
; may be specified.  The user is strongly encouraged to enter values
; for these calibration parameters, but they will be estimated if none
; are  provided.  The user can also supply a mean tcal using the tcal
; keyword.  That will override the tcal found in the data.
; <p>
; <b>Smoothing the Reference Spectra</b>
; <p>
; A parameter called smthoff can be used to smooth the reference
; spectrum prior to calibration.  In certain cases this can improve
; the signal to noise ratio, but it may degrade baseline shapes and
; artificially emphasize spectrometer glitches.  Use with care.  A
; value of smthoff=16 is often a good choice. 
; <p> 
; <b>Weighting of Integrations in Scan Average</b>
; <p> 
; By default, the averaging of integrations is weighted using tsys,
; exposure, and frequency_resolution as described in the 
; <a href="../toolbox/dcaccum.html">dcaccum</a> documentation.
; To give all integrations equal weight instead of the default
; weighting based on Tsys, use the /eqweight keyword.
; <p>
; <b>Summary Information</b>
; <p>
; The scan number printed in the status line is that of the "On"
; scan.  The first Tsys printed is the tsys of the result (and they
; come only from the "Off" scan).  The second Tsys printed is a
; weighted average of the Tsys values associated with the "On" scan.
; <p>
; <b>Using or Ignoring Flags</b>
; <p>
; Flags (set via <a href="flag.html">flag</a>) can be selectively applied or ignored using 
; the useflag and skipflag keywords.  Only one of those two keywords
; can be used at a time (it is an error to use both at the same time).
; Both can be either a boolean (/useflag or /skipflag) or an array of
; strings.  The default is /useflag, meaning that all flag rules that
; have been previously set are applied when the data is fetched from
; disk, blanking data as described by each rule.  If /skipflag is set,
; then all of the flag rules associated with this data are ignored and
; no data will be blanked when fetched from disk (it may still contain
; blanked values if the actual values in the disk file have already
; been blanked by some other process).  If useflag is a string or
; array of strings, then only those flag rules having the same
; idstring value are used to blank the data.  If skipflag is a string
; or array of strings, then all flag rules except those with the same
; idstring value are used to blank the data. 
; <p>
; <b>Dealing With Duplicate Scan Numbers</b>
; <p>
; There are 3 ways to attempt to resolve ambiguities when the
; same scan number appears in the data source.  The instance keyword 
; refers to the element of the returned array of scan_info structures 
; that <a href="scan_info.html">scan_info</a> returns.  So, if scan 23
; appears 3 times then instance=1 refers to the second time that scan 23
; appears as returned by scan_info.  The file keyword is useful if a 
; scan is unique to a specific file and multiple files have been
; accessed using <a href="dirin.html">dirin</a>.  If file is specified and instance is also 
; specified, then instance refers to the instance of that scan just
; within that file (which may be different from its instance within 
; all opened files when dirin is used).  The timestamp keyword is
; another way to resolve ambiguous scan numbers.  The timestamp here
; is a string used essentially as a label by the monitor and control
; system and is unique to each scan.  The format of the timestamp
; string is "YYYY_MM_DD_HH:MM:SS".  When timstamp is given, scan and
; instance are ignored.  If more than one match is found, an error is 
; printed and this procedure will not continue.  
;
; <p>Once a unique match is found to the desired scan (using instance,
; file, or timestamp) then the scan paired with that scan necessary to
; finish this procedure is found.  The match must be found within the
; same file as the desired scan.  It must have the appropriate matching
; scan number (scan-1 if scan is the second scan in the procedure or
; scan+1 if scan is the first scan in the procedure).  If those two
; rules are not sufficient to find a unique match, the matching
; scan with the closest timestamp in the appropriate direction (before
; or after depending on which procseqn is associate with scan) is used.
; Finally, the matched pair must have the appropriate procseqn given the
; procseqn that scan is.
;
; @param scan {in}{required}{type=integer} M&C scan number
; @keyword ifnum {in}{optional}{type=integer} IF number (starting with
; 0), defaults to 0.
; @keyword intnum {in}{optional}{type=integer} Integration number
; (default=all}, defaults to all integrations.
; @keyword plnum {in}{optional}{type=integer} polarization number,
; defaults to 0.
; @keyword fdnum {in}{optional}{type=integer} feed number, defaults to
; 0.
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
; reference spectrum, defaults to 1 (no smoothing).
; @keyword units {in}{optional}{type=string} takes the value 'Jy',
; 'Ta', or 'Ta*', defaults is Ta.
; @keyword eqweight {in}{optional}{type=boolean} When set,
; all integrations are averaged with equal weight (1.0), defaults is unset.
; @keyword tcal {in}{optional}{type=float} Cal temperature (K) to use
; in the Tsys calculation.  If not supplied, the mean_tcal value from
; the header of the cal_off switching phase data in each integration
; is used.  This must be a scalar, vector tcal is not yet supported.
; The resulting data container will have it's mean_tcal header value
; set to this keyword when it is set by the user.
; @keyword quiet {in}{optional}{type=boolean} When set, the normal
; status message on successful completion is not printed.  This will
; not have any effect on error messages.  Default is unset.
; @keyword keepints {in}{optional}{type=boolean} When set, the
; individual integrations are saved to the current output file
; (fileout).  This option is ignored if a specific integration is requested
; using the intnum keyword.  Default is unset.
; @keyword useflag {in}{optional}{type=boolean or string}
; Apply all or just some of the flag rules?  Default is set.
; @keyword skipflag {in}{optional}{type=boolean or string} Do not apply
; any or do not apply a few of the flag rules?  Default is unset.
; @keyword instance {in}{optional}{type=integer} Which occurence
; of this scan should be used.  Default is 0.
; @keyword file {in}{optional}{type=string} When specified, limit the search 
; for this scan (and instance) to this specific file.  Default is all files.
; @keyword timestamp {in}{optional}{type=string} The M&C timestamp associated
; with the desired scan. When supplied, scan and instance are ignored.
; @keyword status {out}{optional}{type=integer} An utput parameter to indicate
; whether the procedure finished as expected.  A value of 1 means there were
; no problems, a value of -1 means there were problems with the
; arguments before any data was processed, and a value of 0 means that
; some of the individual integrations were processed (and possibly
; saved to the output file if keepints was set) but there was a
; problem with the final average and buffer 0 likely contains just the result 
; from the last integration processed. This keyword is primarily of use when
; getps is called within another procedure or function.
;
; @examples
; <pre>
;    ; average both polarizations from ifnum=1
;    sclear
;    getps, 76, ifnum=1, plnum=0
;    accum
;    getps, 76, ifnum=1, plnum=1
;    accum
;    ave
; </pre>
;
; @uses <a href="../toolbox/accumave.html">accumave</a>
; @uses <a href="../toolbox/accumclear.html">accumclear</a>
; @uses <a href="../../devel/guide/calsummary.html">calsummary</a>
; @uses <a href="../../devel/guide/check_calib_args.html">check_calib_args</a>
; @uses <a href="../toolbox/data_free.html">data_free</a>
; @uses <a href="../toolbox/dcaccum.html">dcaccum</a>
; @uses <a href="../toolbox/dcscale.html">dcscale</a>
; @uses <a href="../toolbox/dcsetunits.html">dcsetunits</a>
; @uses <a href="../toolbox/dofullsigref.html">dofullsigref</a>
; @uses <a href="find_paired_info.html">find_paired_info</a>
; @uses <a href="../../devel/guide/find_scan_info.html">find_scan_info</a>
; @uses <a href="../../devel/guide/get_calib_data.html">get_calib_data</a>
; @uses <a href="set_data_container.html">set_data_container</a>
;
; @version $Id: getps.pro,v 1.51 2008/02/12 22:28:42 bgarwood Exp $
;-

pro getps,scan,ifnum=ifnum,intnum=intnum,plnum=plnum,fdnum=fdnum,sampler=sampler,tau=tau,$
          tsys=tsys,ap_eff=ap_eff,smthoff=smthoff,units=units,eqweight=eqweight,$
          tcal=tcal,quiet=quiet, keepints=keepints, useflag=useflag, skipflag=skipflag, $
          instance=instance, file=file, timestamp=timestamp, status=status
    compile_opt idl2

    status=-1

    ; basic argument checks
    argsOK=check_calib_args(scan,ifnum=ifnum,intnum=intnum,plnum=plnum,fdnum=fdnum,sampler=sampler, $
                            eqweight=eqweight,units=units,quiet=quiet,keepints=keepints,useflag=useflag, $
                            skipflag=skipflag,instance=instance,file=file,$
                            timestamp=timestamp,tau=tau,ap_eff=ap_eff,ret=ret)
    if not argsOK then return

    ; Get the scan_info for scan
    onScanInfo = find_scan_info(scan,timestamp=timestamp,instance=instance,file=file)
    if size(onScanInfo,/type) ne 8 then return

    ; check for appropriate info in the scan found
    if onScanInfo.procedure ne 'OffOn' and onScanInfo.procedure ne 'OnOff' then begin
        message,"Cannot handle this scan: Procedure = " + strcompress(onScanInfo.procedure,/remove_all), /info
        return
    end
    ; it must be one of a pair
    if onScanInfo.procseqn gt 2 then begin
        sProcseqn = strcompress(string(onScanInfo.procseqn),/remove_all)
        message,"More than two scans in this procedure - can not continue.  At least :" + sProcseqn, /info
        return
    endif

    ; locate the paired scan
    offScanInfo = find_paired_info(onScanInfo)
    if size(offScanInfo,/type) ne 8 then begin
        message,'The other scan for this position switched procedure can not be found.',/info
        return
    endif
    
    ; make sure we know which is On and which is Off
    if onScanInfo.procedure eq 'OnOff' then begin
        ; lowest scan number is always On
        if onScanInfo.scan gt offScanInfo.scan then begin
            tmp = offScanInfo
            offScanInfo = onScanInfo
            onScanInfo = tmp
        endif
    endif else begin
        ; must be on OffOn - lowest scan number is always Off
        if offScanInfo.scan gt onScanInfo.scan then begin
            tmp = offScanInfo
            offScanInfo = onScanInfo
            onScanInfo = tmp
        endif
    endelse

    ; each scan must have 2 CAL states 
    if onScanInfo.n_cal_states ne 2 then begin
        message,'The number of cal states in the "On" scan is not 2, as needed for this procedure',/info
        return
    endif
    if offScanInfo.n_cal_states ne 2 then begin
        message,'The number of cal states in the "Off" scan is not 2, as needed for this procedure',/info
        return
    endif

    ; There can be no SIG states
    if onScanInfo.n_sig_states ne 1 then begin
        message,'There is more than 1 sig states in the "On" scan, can not continue',/info
        return
    endif
    if offScanInfo.n_sig_states ne 1 then begin
        message,'There is more than 1 sig states in the "Off" scan, can not continue',/info
        return
    endif

    ; Get the requested data
    onData = get_calib_data(onScanInfo, ret.ifnum, ret.plnum, ret.fdnum, ret.sampler, oncount, $
                            intnum=intnum, useflag=useflag, skipflag=skipflag)
    if oncount le 0 then begin
        message,'No data found for the "On" scan, can not continue',/info
        return
    endif

    offData = get_calib_data(offScanInfo, ret.ifnum, ret.plnum, ret.fdnum, ret.sampler, offcount, $
                             intnum=intnum, useflag=useflag, skipflag=skipflag)
    if offcount le 0 then begin
        message,'No data found for the "Off" scan, can not continue',/info
        data_free,onData
        return
    endif

    ; from this point on, sigdata and refdata contain data containers 
    ; that must be freed whenever this routine returns to avoid memory leaks.

    ; find the 4 types of data container
    onScan_off = where(onData.cal_state eq 0, countOnOff)
    onScan_on = where(onData.cal_state eq 1, countOnOn)
    offScan_off = where(offData.cal_state eq 0, countOffOff)
    offScan_on = where(offData.cal_state eq 1, countOffOn)

    ; final sanity checks
    
    ; In this calibration, we calibrate each integration separately
    ; and then average the results.  We know that there are 2 
    ; states per integration.  And we've already tested that the
    ; number of sig states is 1.  So, each type of data container
    ; should have 1 element for each integration
    expectedCount = (n_elements(intnum) eq 1) ? 1 : onScanInfo.n_integrations

    if (countOffOff ne expectedCount or countOnOff ne countOnOn or countOnOff ne countOffOff or countOnOff ne countOnOn) then begin 
        message,"Unexpected number of spectra retrieved for some or all of the switching phases, can not continue.",/info
        data_free, onData
        data_free, offData
        return
    endif

    status = 0
    missing = 0

    if keyword_set(eqweight) then weight = 1.0 ; else undefined and use default weight

    thisaccum = {accum_struct}
    tauInts = fltarr(expectedCount)
    apEffInts = tauInts
    sigTsysInts = tauInts
    for n_int = 0,(expectedCount-1) do begin
        dofullsigref,result,onData[onScan_on[n_int]],onData[onScan_off[n_int]],$
                     offData[offScan_on[n_int]],offData[offScan_off[n_int]], $
                     smthoff,tsys=tsys,tau=tau,tcal=tcal,retreftsys=retreftsys,retsigtsys=retsigtsys
        ; convert to the desired units
        dcsetunits,result,units,tau=tau,ap_eff=ap_eff,ret_tau=ret_tau,ret_ap_eff=ret_ap_eff
        ; these are only used in the status line at the end
        tauInts[n_int] = ret_tau
        apEffInts[n_int] = ret_ap_eff
        sigTsysInts[n_int] = retsigtsys
        
        dcaccum,thisaccum,result,weight=weight
        if keyword_set(keepints) then begin
            ; re-use raw data containers to conserve space
            ; defer the actual keep until later
            ; takes 3 steps because of the nature of IDL
            ; data passing (value vs reference)
           tmp = onData[onScan_on[n_int]]
           data_copy, result, tmp
           onData[onScan_on[n_int]] = tmp
       endif
    end
    if keyword_set(keepints) then putchunk, onData[onScan_on]
    naccum1 = thisaccum.n
    if naccum1 le 0 then begin
        message,'Result is all blanked - probably all of the data were flagged',/info
        ; clean up
        ; result must therefor be all blanked, use it as the end result
        set_data_container, result
        data_free, result
        accumclear, thisaccum
        data_free, onData
        data_free, offData
        return
    endif
    accumave,thisaccum,result,/quiet
    missing = naccum1 ne expectedCount
    accumclear, thisaccum

    set_data_container, result
    status = 1
    if not keyword_set(quiet) then begin
        if missing then nmiss = expectedCount-naccum1
        calsummary, onScanInfo.scan, result.tsys, result.units, $
                    tsysInts=sigTsysInts, tauInts=tauInts, $
                    apEffInts=apEffInts, missingInts=nmiss, eqweight=eqweight
    endif

    data_free, result
    data_free, onData
    data_free, offData

end

