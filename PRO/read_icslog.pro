FUNCTION gettz, str

tz = intarr(n_elements(str))
FOR i=0L, n_elements(str) - 1 DO tz[i] = strmid(str[i], strpos(str[i], 'UTC')+3, 5)

return, (float(tz)/100.)

END

FUNCTION sysclock, clock_s, tz

secPerDay = double(24L*60L*60L)
;spawn, 'date +%z', tz
;tz = (float(tz)/100)[0] ; covert, e. g., -0400 to -4.
IF n_params() EQ 1 THEN BEGIN
	print, 'WARNING: tz not specified; using UTC'
	help, /tr
	tz=0
ENDIF
day1 = julday(1,1,1970,0,0,0)
return, double(day1 + clock_s/secPerDay) + tz/24.

END

FUNCTION search_and_unzip, file, gzip, unzippedfile

nfiles = n_elements(file)
gzip = intarr(nfiles)
FOR i=0, nfiles - 1 DO $
	gzip[i] = (strsplit(file[i], '.', /extract))[n_elements(strsplit(file[i], '.'))-1] EQ 'gz'

unzippedfile = strarr(nfiles)
readfile = file
FOR i=0, nfiles - 1 DO $
	IF (gzip[i]) THEN BEGIN
		shortfilename = (strsplit(file[i], '/', /extract))[n_elements(strsplit(file[i], '/'))-1]
		unzippedfile[i] = '/tmp/' + strjoin ( $
			(strsplit(shortfilename, '.', /extract) )[0:n_elements(strsplit(shortfilename, '.')) - 2], '.')
		IF NOT file_test(unzippedfile[i]) THEN BEGIN
			unzipcommand = '/usr/bin/gunzip -c ' + file[i] + ' >' + unzippedfile[i]
			print, unzipcommand
			spawn, unzipcommand
		ENDIF ELSE $
			print, 'file ' + unzippedfile[i] + ' exists; using it without unzipping'
		readfile[i] = unzippedfile[i]
	ENDIF

return, readfile

END

PRO rmzipped, gzip, unzippedfile
idx = where(gzip)
IF NOT array_equal(idx, -1) THEN BEGIN
		rmcommand = '/bin/rm ' + strjoin(unzippedfile[idx], ' ')
		print, rmcommand
		spawn, rmcommand
ENDIF
END

FUNCTION volt, adu
return, adu/409.6    ; convert ADUs to volts
END

FUNCTION getbit, bit, pos
return, (fix(bit) / 2^pos) MOD 2
END

FUNCTION ccdtemp, adu
return, 25.*volt(adu) - 200.
END

FUNCTION tempconvert, adu
return, 0.25*(95*volt(adu) - 175.)
END

FUNCTION humidconvert, adu
return, 25*(volt(adu) - 1.)
END

FUNCTION windspeed, adu
return, 27.96*(volt(adu) - 1.)
END

FUNCTION winddir, adu
return, 90.*(volt(adu) - 1.)
END

FUNCTION heater, adu
return, 4*(volt(adu) - 1.)
END

FUNCTION baropres, adu
return, 71.5*volt(adu) + 455.5
END

FUNCTION actemp, adu
return, 18.75*volt(adu) - 36.53
END

FUNCTION sf6weight, adu
return, 100.*(volt(adu) - 1)
END

FUNCTION fw_pos, dio_a
pos = 0
FOR i=0, 3 DO $
	pos += 2^i * getbit(dio_a, i)
return, pos
END

FUNCTION read_icslog, file, keepunzip=keepunzip

;+
;NAME: read_icslog
;PURPOSE: read an ICS log file in the format output by wham_ics_read and
;	return an IDL structure with the data
;CALLING SEQUENCE: icslog = read_icslog(file)
;INPUT:
;	file: path to ICS log file(s) (string scalar or array)
;-

format='A,X,F,L,X,I,X,A,A,X,A,A,X,A,A,X,I,I,I,I,I,I,I,I,I,I,I,X,I,I,I,I,I,I,I,I,I,I,I,X,I,I,I,I,I,I,I,I'

readcol_multi, search_and_unzip(file, gzip, unzippedfile), $
	delimiter='|', format=format, /silent, prestr, $
    version, sysclock_s, sysstatus, dioa_in_temp, dioa_out_temp, $
    diob_in_temp, diob_out_temp, dioc_in_temp, dioc_out_temp, $
    adc1_0, adc1_1, adc1_2, adc1_3, adc1_4, adc1_5, adc1_6, adc1_7, adc1_8, $
    adc1_9, adc1_10, adc2_0, adc2_1, adc2_2, adc2_3, adc2_4, adc2_5, adc2_6, $
    adc2_7, adc2_8, adc2_9, adc2_10, dac1_0, dac1_1, dac1_2, dac1_3, dac1_4, $
    dac1_5, dac1_6, dac1_7
    
IF NOT keyword_set(keepunzip) THEN rmzipped, gzip, unzippedfile

IF n_elements(sysclock_s) EQ 0 THEN return, -1

dioa_in = lonarr(n_elements(dioa_in_temp))
dioa_out = dioa_in
diob_in = dioa_in
diob_out = dioa_in
dioc_in = dioa_in
dioc_out = dioa_in
FOR i=0L, n_elements(dioa_in) - 1 DO BEGIN
    reads, dioa_in_temp[i], out, format='(B16)'
    dioa_in[i] = long(out)
    reads, dioa_out_temp[i], out, format='(B8)'
    dioa_out[i] = long(out)
    reads, diob_in_temp[i], out, format='(B16)'
    diob_in[i] = long(out)
    reads, diob_out_temp[i], out, format='(B8)'
    diob_out[i] = long(out)
    reads, dioc_in_temp[i], out, format='(B16)'
    dioc_in[i] = long(out)
    reads, dioc_out_temp[i], out, format='(B8)'
    dioc_out[i] = long(out)
ENDFOR

icslog={icslog, sysclock_s:0L, sysclock:0D, tz:-3, sysstatus:0., ccdtemp:0., $
	ccd_pres:0., outside_temp:0.,outside_humid:0., siderostat_temp:0., $
	siderostat_humid:0.,trailer_temp:0.,trailer_humid:0.,spectrometer_temp:0.,$
    spectrometer_humid:0., windspeed:0., winddir:0., heater:0., baropres:0., $
    acin_temp:0., acout_temp:0., sf6weight:0., cha_pres:0., chb_pres:0., $
    cha_temp:0., chb_temp:0., lc_beam:0, lc_stow:0, $
    cm_beam:0, cm_stow:0, ln2_fill:0, ln2_trigger:0, ln2_valve:0, shutter:0, $
    callamp_a:0, callamp_b:0, callamp_c:0, callamp_d:0, pump_pres:0., dewar_pres: 0., $
    fw_pos: -1, fw_valid: -1, fw_moving_cw: -1, fw_moving_ccw: -1 }

icslog = replicate(icslog, n_elements(sysclock_s))

icslog.tz=gettz(prestr)
icslog.sysclock_s=sysclock_s
icslog.sysclock=sysclock(sysclock_s, icslog.tz)
icslog.sysstatus=sysstatus
icslog.ccdtemp=ccdtemp(adc1_0)
icslog.baropres=baropres(adc1_7)
icslog.acin_temp=actemp(adc1_8)
icslog.acout_temp=actemp(adc1_9)
icslog.sf6weight=sf6weight(adc1_10)
icslog.outside_temp=tempconvert(adc2_0)
icslog.outside_humid=humidconvert(adc2_1)
icslog.siderostat_temp=tempconvert(adc2_2)
icslog.siderostat_humid=humidconvert(adc2_3)
icslog.trailer_temp=tempconvert(adc2_4)
icslog.trailer_humid=humidconvert(adc2_5)
icslog.spectrometer_temp=tempconvert(adc2_6)
icslog.spectrometer_humid=humidconvert(adc2_7)
icslog.windspeed=windspeed(adc2_8)
icslog.winddir=winddir(adc2_9)
icslog.heater=heater(adc2_10)
icslog.lc_beam=getbit(dioa_in, 9)
icslog.lc_stow=getbit(dioa_in, 8)
icslog.cm_beam=getbit(dioa_in, 12)
icslog.cm_stow=getbit(dioa_in, 11)
icslog.ln2_fill=getbit(dioc_out, 2)
icslog.ln2_trigger=getbit(dioc_out, 3)
icslog.ln2_valve=getbit(dioc_in, 7)
icslog.shutter=getbit(dioa_in, 14)
icslog.callamp_a=getbit(diob_in, 2)
icslog.callamp_b=getbit(diob_in, 3)
icslog.callamp_c=getbit(diob_in, 4)
icslog.callamp_d=getbit(diob_in, 5)
icslog.fw_pos=fw_pos(dioa_in)
icslog.fw_valid=getbit(dioa_in, 4)
icslog.fw_moving_cw=getbit(dioa_in, 5)
icslog.fw_moving_ccw=getbit(dioa_in, 6)

return, icslog

END
