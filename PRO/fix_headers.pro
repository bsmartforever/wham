FUNCTION get_calib_gl_gb, calib
    CASE calib OF
        'zeta_oph' : return, [3.42, 23.54]
        'zeta_oph_offa' : return, [4.5, 42.5]
        'zeta_oph_offb' : return, [16.5, 37.5]
        'l_ori' : return, [194.7, -11.8]
        'l_ori_off' : return, [200.0, -8.0]
        'Spica_HII' : return, [315.5, 48.9]
        'crab' : return, [184.57, -5.86]
        ELSE: message, 'Calibration source ' + calib + ' not found'
    ENDCASE
    
    message, "We shouldn't still be here!"

END

FUNCTION modifyfitsheader, header, headstr, newval, $
    format=format, truefalse=truefalse

;+
;NAME: modifyfitsheader
;PURPOSE: modify the value of the specified parameter in a FITS header
;SYNTAX: modified_hdr = modifyfitsheader(header, headstr, newval [,format=fmt]
;  'header' can be either the file name of a FITS file (in which case
;    readfitsheader will read the FITS file) or the header itself as an array
;    of strings
;
;OPTIONAL KEYWORD INPUTS:
;   format = format - format code for the value if newval is numerical.
;       NOTE: if the existing field is a string, format will be automatically
;           set to fit inside the quotation marks in the header; the keyword
;           parameter will be ignored
;       Default: '(F21.1)' if value to be replaced is numerical
;       Length of format should be 21 characters
;
;MODIFICATION HISTORY:
;   Written by Alex Hill 2009-10-30
;-

format = keyword_set(format) ? format : '(F21.1)'
IF n_elements(header) LE 1 THEN hdr = headfits(header) ELSE hdr = header

sub = where(strpos(hdr, headstr ) NE -1)

IF NOT array_equal(sub, -1) THEN BEGIN
	line = hdr[sub]
	line = line[0]
	pos = (strsplit(line, '=/'))[1]
	val = (strsplit(line, '=/', /extract))[1]
	IF keyword_set(truefalse) THEN $
	    strput, line, string( newval ? 'T' : 'F', format='(A21)'), pos $
	ELSE IF ( strmid(strtrim(val, 2), 0, 1) EQ "'" ) THEN BEGIN
	    beginquote = strpos(val, "'")
	    endquote = strpos(val, "'", beginquote+1)
	    format = '(A-' + strtrim(endquote - beginquote - 1,2) + ')'
	    strput, val, string(newval, format=format), beginquote + 1
	    strput, line, val, pos
	ENDIF ELSE strput, line, string(newval, format=format), pos
	hdr[sub] = line
	return, hdr
ENDIF ELSE print, 'MODIFYFITSHEADER: WARNING: Line "' + headstr + '" not found'

return, hdr

END

PRO replace_file, file, outfile, hdr, icslog, pcslog, date, gl, gb, FWSHNAME, FWLNGNAME, WAVELEN, exptime

    data=readfits(file, hdr, /silent)
    glactc, ra, dec, 2000, gl, gb, 2 
    eq2hor, ra*360./24, dec, date, alt, az, ha, obsname='ctio' 
    vlsr = vlsr(lon=gl, lat=gb, jd=date)

    hdr=modifyfitsheader(hdr, 'DATE-OBS', string(date, $
        format='(C(CYI,"-",CMOI02,"-",CDI02,"T",CHI02,":",CMI02,":",CSF4.1))'))
    hdr=modifyfitsheader(hdr, 'TIME-OBS', string(date, $
        format='(C(CHI02,":",CMI02,":",CSF4.1))'))
    hdr=modifyfitsheader(hdr, 'LDAT-OBS', string(date - 4./24, $
        format='(C(CYI,"-",CMOI02,"-",CDI02,"T",CHI02,":",CMI02,":",CSF4.1))'))
    hdr=modifyfitsheader(hdr, 'LTIM-OBS', string(date - 4./24, $
        format='(C(CHI02,":",CMI02,":",CSF4.1))'))
    ct2lst, lst, -70.815, dummy, date
    hdr=modifyfitsheader(hdr, 'LST-OBS', string(lst, $
        format='(C(CHI02,":",CMI02,":",CSF4.1))'))
    hdr=modifyfitsheader(hdr, 'WAVELEN', WAVELEN) 
    fmt='(F21.9)' 
;        hdr=modifyfitsheader(hdr, 'OBJECT', pntname) 
    hdr=modifyfitsheader(hdr, 'ZENITH_D', 90-alt, format=fmt) 
    hdr=modifyfitsheader(hdr, 'AIRMASS', zd2airmass(90.-alt), format=fmt)
    hdr=modifyfitsheader(hdr, '[hr] Hour angle', ha*24./360., format=fmt) 
    hdr=modifyfitsheader(hdr, 'VLSR', vlsr, format=fmt) 
    hdr=modifyfitsheader(hdr, 'ROTATION', -1000) 
    hdr=modifyfitsheader(hdr, 'SHDIST', -1000) 
    hdr=modifyfitsheader(hdr, 'SHHEIGHT', -1000) 
    hdr=modifyfitsheader(hdr, '[hr] Right ascension', ra, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Declination', dec, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Topocentric Elevation',alt,format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Topocentric Azimuth', az, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Galactic longitude', gl, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Galactic latitude', gb, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Ecliptic longitude', -1000) 
    hdr=modifyfitsheader(hdr, '[deg] Ecliptic latitude', -1000) 
    hdr=modifyfitsheader(hdr, '[deg] Siderostat longitude', -1000) 
    hdr=modifyfitsheader(hdr, '[deg] Siderostat latitude', -1000) 

    hdr=modifyfitsheader(hdr, '[hr] Demanded Right ascension', ra, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Declination', dec, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Topocentric Elevation',alt,format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Topocentric Azimuth', az, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Galactic longitude', gl, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Galactic latitude', gb, format=fmt) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Ecliptic longitude', -1000) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Ecliptic latitude', -1000) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Siderostat longitude', -1000) 
    hdr=modifyfitsheader(hdr, '[deg] Demanded Siderostat latitude', -1000) 

    hdr=modifyfitsheader(hdr, 'PACMD', pcslog.cha_setpoint) 
    hdr=modifyfitsheader(hdr, 'PBCMD', pcslog.chb_setpoint) 
    hdr=modifyfitsheader(hdr, 'PAMON', pcslog.cha_pres) 
    hdr=modifyfitsheader(hdr, 'PBMON', pcslog.chb_pres) 
    hdr=modifyfitsheader(hdr, 'PAERR', pcslog.cha_pres_error) 
    hdr=modifyfitsheader(hdr, 'PBERR', pcslog.chb_pres_error) 
    hdr=modifyfitsheader(hdr, 'PATEMP', pcslog.cha_temp) 
    hdr=modifyfitsheader(hdr, 'PBTEMP', pcslog.chb_temp) 
    hdr=modifyfitsheader(hdr, 'PBTEMP', pcslog.chb_temp) 
    hdr=modifyfitsheader(hdr, 'PBTEMP', pcslog.chb_temp) 
    hdr=modifyfitsheader(hdr, 'PALOOP', -1000) 
    hdr=modifyfitsheader(hdr, 'PBLOOP', -1000) 

    hdr=modifyfitsheader(hdr, 'FINVNUM', -1000) 
    hdr=modifyfitsheader(hdr, 'FSHNAME', fwshname) 
    hdr=modifyfitsheader(hdr, 'FLNGNAME', fwlngname) 
    hdr=modifyfitsheader(hdr, 'FCENTER', -1000) 

    hdr=modifyfitsheader(hdr, 'CCDTEMP', icslog.ccdtemp, format=fmt) 
    hdr=modifyfitsheader(hdr, 'IRISMON', -1000) 
    hdr=modifyfitsheader(hdr, 'FOCUSMON', -1000) 
    hdr=modifyfitsheader(hdr, 'ATMPRESS', icslog.baropres) 
    hdr=modifyfitsheader(hdr, 'ACITEMP', icslog.acin_temp) 
    hdr=modifyfitsheader(hdr, 'ACOTEMP', icslog.acout_temp) 
    hdr=modifyfitsheader(hdr, 'SF6WGHT', icslog.sf6weight) 
    hdr=modifyfitsheader(hdr, 'TEMP1',icslog.outside_temp) 
    hdr=modifyfitsheader(hdr, 'TEMP2',icslog.siderostat_temp) 
    hdr=modifyfitsheader(hdr, 'TEMP3',icslog.trailer_temp) 
    hdr=modifyfitsheader(hdr, 'TEMP4',icslog.spectrometer_temp) 
    hdr=modifyfitsheader(hdr, 'HUMID1', icslog.outside_humid) 
    hdr=modifyfitsheader(hdr, 'HUMID2', icslog.siderostat_humid) 
    hdr=modifyfitsheader(hdr, 'HUMID3', icslog.trailer_humid) 
    hdr=modifyfitsheader(hdr,'HUMID4',icslog.spectrometer_humid) 
    hdr=modifyfitsheader(hdr,'WINDSPD',icslog.windspeed) 
    hdr=modifyfitsheader(hdr,'WINDDIR',icslog.winddir) 
    hdr=modifyfitsheader(hdr,'HEATERS',fix(icslog.heater/0.1), format='(I21)') 
    hdr=modifyfitsheader(hdr, 'IRISCMD', -1000) 
    hdr=modifyfitsheader(hdr, 'FOCUSCMD', -1000) 
    hdr=modifyfitsheader(hdr,'CALAMON',icslog.callamp_A,/true) 
    hdr=modifyfitsheader(hdr,'CALBMON',icslog.callamp_B,/true) 
    hdr=modifyfitsheader(hdr,'CALCMON',icslog.callamp_C,/true) 
    hdr=modifyfitsheader(hdr,'CALDMON',icslog.callamp_D,/true) 
    hdr=modifyfitsheader(hdr, 'CALMIR', icslog.cm_beam ? 'BEAM' : icslog.cm_stow ? 'STOW' : 'ERROR') 
    hdr=modifyfitsheader(hdr, 'LENSCAR', icslog.lc_beam ? 'BEAM' : icslog.lc_stow ? 'STOW' : 'ERROR') 
    hdr=modifyfitsheader(hdr, 'STARMASK', -1000) 
    hdr=modifyfitsheader(hdr, 'FWPOS', icslog.fw_pos, format='(I02)') 
    hdr=modifyfitsheader(hdr, 'FWVALID', icslog.fw_valid, /true) 
    hdr=modifyfitsheader(hdr, 'FWMOVING', icslog.fw_moving_cw OR icslog.fw_moving_ccw, /true) 
    hdr=modifyfitsheader(hdr, 'IRMOVING', -1000) 
    hdr=modifyfitsheader(hdr, 'FCMOVING', -1000) 
    hdr=modifyfitsheader(hdr, 'LN2VALVE', icslog.ln2_fill,/true) 
    hdr=modifyfitsheader(hdr, 'SF6VALVE', -1000, format='(I21)') 

    hdr=modifyfitsheader(hdr, 'EXPTIME', exptime, format='(I21)') 

    writefits, outfile, data, hdr
    spawn, 'makeSpect -f ' + outfile + ' ' + outfile

END

PRO fix_headers, date, icslog, pcslog, hdr, dir=dir, fixeddir=fixeddir, $
    verbose=verbose, survey=survey

;+
;NAME: fix_headers
;
;PURPOSE: Fix headers in WHAM observations obtained when the telemetry was stale
;
;SYNTAX: fix_headers, date [,icslog [,pcslog [,hdr]]] $
;   [,dir=dir] [,fixeddir=fixeddir] [, /verbose] [, /survey]
;
;REQUIRED INPUT:
;   date - date in yymmdd format, e. g. '090727'. Can be string or integer.
;       Date can also be a three-element array in format [yy, mm, dd] or
;       [yyyy, mm, dd]
;
;OPTIONAL INPUT:
;   icslog and pcslog - already-read icslog and pcslogs. If empty, will be
;       outputs. If not provided, icslog and pcslog will be read automatically;
;       takes a few minutes.
;
;OPTIONAL KEYWORD INPUTS:
;   dir=dir - base directory containing the existing FITS files to modify
;       default: /d/aaron2/data-archive/pointed/
;   fixeddir=fixeddir - base directory to put the modified FITS files in
;       default: /d/aaron2/data-archive/pointed-fixed/
;       Note that appropriate subdirectories (one for the date plus 
;           ha/combo, sii/combo, etc) must already exist)
;   /survey - use survey as default for dir and survey-fixed as default for
;       fixeddir
;
;
;OPTIONAL OUTPUTS:
;   icslog and pcslog - icslog and pcslog read from /d/wham/log/
;   hdr - modified header of last observation read in specified directory.
;       Useful for debugging.
;
;NOTE: must run .compile icsday before running fix_headers to get the
;   searchfiles routine
;
;DETAILS:
;   fix_headers reads the FITS header of all observations specified in the
;   observing log file in the directory. The filter, observation date and time,
;   exposure time, and Galactic coordinates of each pointed observation are
;   collected from the observing log. RA, Dec, Az, El, and zenith distance are
;   calculated from the Galactic coordinates and observation date and time.
;   All other parameters are pulled from the ICS and PCS logs for the time 15
;   seconds after the beginning of each exposure.
;
;   The header is modified with this information and rewritten, along with the
;   data, to a new FITS file in fixeddir using the WRITEFITS routine.
;   fix_headers then runs makeSpect to recreate the RAWSPEC extension from the
;   image.
;
;MODIFICATION HISTORY:
;   2010 May 25 ASH: fix bug in which icslog.sysclock and pcslog.sysclock were
;       modified directly which ruined future runs in the same IDL session
;-

;.compile icsday

IF n_params() EQ 0 THEN $
    message, 'SYNTAX: fix_header, date [, optional parameters]'

verbose=keyword_set(verbose)

ymd = intarr(3)
date = string(date, format='(I06)')
IF n_elements(date) EQ 1 THEN BEGIN
    ymd[0] = fix(strmid(date, 0, 2))
    ymd[1] = fix(strmid(date, 2, 2))
    ymd[2] = fix(strmid(date, 4, 2))
ENDIF ELSE IF ymd[0] GE 2000 THEN ymd[0] -= 2000

d=julday(ymd[1], ymd[2], ymd[0] + 2000)
ld='/d/wham/log/'
IF n_elements(icslog) EQ 0 THEN $
    icslog=read_icslog(searchfiles(d-1,d+1,1,ld, 'icslog'))
IF n_elements(pcslog) EQ 0 THEN $
    pcslog=read_pcslog(searchfiles(d-1,d+1,1,ld, 'pcslog'))

;icslog.sysclock -= icslog[0].tz/24
;pcs_sysclock -= pcslog[0].tz/24
ics_sysclock = icslog.sysclock - icslog[0].tz/24
pcs_sysclock = pcslog.sysclock - pcslog[0].tz/24

dir=keyword_set(dir) ? dir : keyword_set(survey) ? '/d/aaron2/data-archive/survey/' : '/d/aaron2/data-archive/pointed/'
fixeddir=keyword_set(fixeddir) ? fixeddir : keyword_set(survey) ? '/d/aaron2/data-archive/survey-fixed/' : '/d/aaron2/data-archive/pointed-fixed/'

restore, '/d/wham/survey/idl/points.dat'

datestr = strjoin(string(ymd, format='(I02)'))
indir = dir + datestr + '/'
outdir = fixeddir + datestr + '/'
spawn, 'if [ ! -e ' + outdir + ' ]; then mkdir ' + outdir + '; fi'
logfile=indir + datestr + '.log'
readfmt, logfile, '(A10, X, A8, 4X, A50)', date, time, observation, /silent
exptime = 30
pnt_overhead = 6
FWLNGNAME = -1000

datearr=dblarr(n_elements(date))
FOR i=0, n_elements(date) - 1 DO BEGIN 
    dmy=fix(strsplit(date[i], '/', /extract)) 
    hms=fix(strsplit(time[i], ':', /extract)) 
    datearr[i]=double(julday(dmy[0],dmy[1],dmy[2],hms[0],hms[1],hms[2]+15))
    ; set time to 15 s after beginning of observation to improve chances of getting correct icslog and pcslog entry
    
    IF strpos(observation[i], 'Set Exposure Time') NE -1 THEN $
        exptime = (strsplit(observation[i], ":", /extract))[1] ELSE $
    IF strpos(observation[i], 'Setup:') NE -1 THEN BEGIN
        FWLNGNAME = strtrim((strsplit(observation[i], ':', /extract))[1], 2)
        CASE FWLNGNAME OF
            'Barr H-Alpha' : BEGIN
                FWSHNAME = 'ha'
                WAVELEN = 6567.0
            END
            'H-Beta' : BEGIN
                FWSHNAME = 'hb'
                WAVELEN = 4865.0
            END
            '[S II] 6716' : BEGIN
                FWSHNAME = 'sii'
                WAVELEN = 6720.0
            END
            '[N II] 6584' : BEGIN
                FWSHNAME = 'nii'
                WAVELEN = 6587.0
            END
            ELSE : BEGIN
                print, 'WARNING: filter ' + FWLNGNAME + ' not known'
                WAVELEN = 0.0
            END
        ENDCASE
        filterdir = outdir + FWSHNAME
        spawn,'if [ ! -e '+filterdir + ' ]; then mkdir ' + filterdir + '; fi'
        subdir = FWSHNAME + '/combo/'
        spawn,'if [ ! -e '+outdir+subdir+ ' ]; then mkdir '+outdir+subdir+'; fi'
        IF verbose THEN print, 'Filter = ' + FWLNGNAME + ' (' + FWSHNAME + ')'
    ENDIF ELSE IF (strpos(observation[i], 'Observation') NE -1) OR $
        (strpos(observation[i], 'Calibration') NE -1) THEN BEGIN 
        mindiff=min(abs(double(ics_sysclock) - datearr[i]), icsidx) 
        IF mindiff GT 2. / (24*3600) THEN print, $
            'WARNING: time in icslog differs from observation time by ' + $
            string(mindiff, format='(C(CHI4,":",CMI02,":",CDI02))')
        mindiff=min(abs(double(pcs_sysclock) - datearr[i]), pcsidx) 
        IF mindiff GT 2. / (24*3600) THEN print, $
            'WARNING: time in pcslog differs from observation time by ' + $
            string(mindiff, format='(C(CHI02,":",CMI02,":",CDI02))')
        pntname=(strsplit(observation[i], " ", /extract))[1] 
        filename=indir + subdir + pntname + '-' + strjoin(string(hms, format='(I02)')) + '.fts' 

        IF verbose THEN print, 'INFILE: ' + filename
        IF strpos(observation[i], 'Observation') NE -1 THEN BEGIN
            glgb=strsplit(observation[i], "( )", /extract) 
        gl=float(glgb[2]) & gb=float(glgb[3]) 
        ENDIF ELSE BEGIN
            print, 'Getting gl and gb from calibration: ' + pntname
            glgb = get_calib_gl_gb(pntname)
            gl = glgb[0] & gb = glgb[1]
        ENDELSE
        
        outfile = outdir + subdir + pntname + '-' + strjoin(string(hms, format='(I02)')) + '.fts'
        IF verbose THEN print, 'OUTFILE: ' + outfile
        
        replace_file, filename, outfile, hdr, icslog[icsidx], pcslog[pcsidx], $
            datearr[i], gl, gb, FWSHNAME, FWLNGNAME, WAVELEN, exptime
    ENDIF ELSE IF strpos(observation[i], 'Block #') NE -1 THEN BEGIN
        bnum = fix((strsplit(observation[i], '#', /extract))[1])
        print, 'Block #' + strtrim(bnum, 2)
        idx = where(points[2,*] EQ bnum)
        ptemp = points[*, idx]
        sortidx = sort(ptemp[1, *]) ; sort in increasing order of latitude
        latlist = ptemp[1, sortidx]
        lonlist = ptemp[0, sortidx]
        sortdescending=0
        k=0
        WHILE k LT n_elements(idx) - 1 DO BEGIN
            idx2 = where(latlist EQ latlist[k])
            sortidx = sort(lonlist[idx2])
            IF sortdescending THEN BEGIN
                sortidx = reverse(sortidx)
                IF verbose THEN print, 'SORTING DESCENDING'
                sortdescending = 0
            ENDIF ELSE sortdescending = 1
            lonlist[idx2] = lonlist[idx2[sortidx]]
            k += n_elements(idx2)
        ENDWHILE
        
        basefile = indir + subdir + 'b' + strtrim(bnum, 2) + '_'
        baseoutfile = outdir + subdir + 'b' + strtrim(bnum, 2) + '_'
        curtime = datearr[i]
                
        FOR j=0, npoints[bnum - 1] - 1 DO BEGIN
            mindiff=min(abs(double(ics_sysclock) - curtime), icsidx) 
            IF mindiff GT 2. / (24*3600) THEN print, $
                'WARNING: time in icslog differs from observation time by ' + $
                string(mindiff, format='(C(CHI02,":",CMI02,":",CDI02))')
            mindiff=min(abs(double(pcs_sysclock) - curtime), pcsidx) 
            IF mindiff GT 2. / (24*3600) THEN print, $
                'WARNING: time in pcslog differs from observation time by ' + $
                string(mindiff, format='(C(CHI02,":",CMI02,":",CDI02))')
            gl = lonlist[j] & gb = latlist[j]
            filename = basefile + strtrim(j+1, 2) + '.fts'
            outfile = baseoutfile + strtrim(j+1, 2) + '.fts'
            IF verbose THEN BEGIN
                print, 'INFILE: ' + filename + ' gl, gb = ' + string(gl) + string(gb)
                print, 'OUTFILE: ' + outfile
                print, 'Time of exposure: ' + string(curtime, format='(C())')
            ENDIF
            replace_file,filename,outfile,hdr,icslog[icsidx],pcslog[pcsidx], $
                curtime, gl, gb, FWSHNAME, FWLNGNAME, WAVELEN, exptime
            curtime = curtime + (exptime + pnt_overhead) / (3600*24.)
        ENDFOR
    ENDIF
ENDFOR

END
