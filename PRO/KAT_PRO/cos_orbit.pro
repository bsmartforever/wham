function cos_orbit, flux_in, SN, galex2hst=galex2hst, quiet=quiet

; Purpose:
;	Rough orbit/exposure time calculator for HST/COS G130M and G160M
;	based on FUV flux. 
;
; Input:
;	flux_in   - FUV flux in units of erg cm^-2 s^-1 Angstrom^-1
;	SN        - Signal-to-Noise ratio [Default 10.]
;	galex2hst - Convert micro Janskys to (erg cm^-2 s^-1 Angstrom^-1)
;				Note: read_galex supplies FUV in micro Janskys
; Output:
; 	Structure containing the exposure time and number of orbits 
;	for G130M, G160M, and the two combined.
;	
; Example:
;   tmp=cos_orbit([0.084,0.7]*1e-14,20)
;
;	** S/N           20       
;	** G130M orbits  22.5    Exposure time (ks) 67.4      
;	** GM160 orbits  33.3    Exposure time (ks) 99.8      
;	** Total orbits  57.0    Exposure time (ks) 167.2     
;	
;	** S/N           20       
;	** G130M orbits  2.6     Exposure time (ks) 7.7       
;	** GM160 orbits  3.8     Exposure time (ks) 11.5      
;	** Total orbits  7.0     Exposure time (ks) 19.2 
;
;	help,tmp,/str
;	** Structure <165bd48>, 6 tags, length=24, data length=22, refs=1:
;	   G130_ORBIT      FLOAT           22.4668
;	   G130_TIME       FLOAT           67400.4
;	   G160_ORBIT      FLOAT           33.2831
;	   G160_TIME       FLOAT           99849.2
;	   ORBIT           INT             57
;	   TIME            FLOAT           167250.
;
; Created by Dr. Kat Barger 03/2014
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (NOT keyword_set(SN)) then SN=10.
if (keyword_set(galex2hst)) then flux_in=galex2hst(flux_in)

;The signal to noise scales as t^(1/2)
;so (t_new/t_old)^0.5=(SN_new/SN_old)

;exposure time calculator tools
;http://etc.stsci.edu/etc/
;S/N => exposure time: http://etc.stsci.edu/etcstatic/users_guide/1_2_2_time.html

;orbit=96 minutes
;orbit=96.*60. ;orbit time in seconds
orbit=50.*60.
total_time=0.

;But in the following example, they assume that the the orbit is 50 minutes 
;(ingnoring acquisition time)
;http://www.stsci.edu/hst/cos/documents/handbooks/current/ch07.ETC7.html#373677

;Convert Flux in micro Janskys (10^23 erg/(s cm^2 Hz)) to AB magnitude
;AB=23.9-2.5*alog10(Flux)
;These are the units the COS spectroscopic calulator uses for GALEX data

;;; G130M
	
	flux=[1.5e-13,1e-13,1.5e-14,1.0e-14,1.5e-15,1.0e-15]
	time=[84.8449,127.3281,855.7427,1289.6924,9286.8381,14538.0999]
	
	fit=linfit(alog10(flux),alog10(time))
	
	SN_old=10.
	t_old=10.^(fit[1]*alog10(flux_in)+fit[0])

	exptime_g130m=(SN/SN_old*sqrt(t_old))^2.
	
	;G130M flux calculator for S/N = 10, specified wavelength is 1310
	;http://etc.stsci.edu/etc/input/cos/spectroscopic/
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1310.00 Å (per resolution element)
	;gives: Time = 84.8449 seconds, Flux 1.5e-13 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1310.00 Å (per resolution element)
	;gives: Time = 127.3281 seconds, Flux 1e-13 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1310.00 Å (per resolution element)
	;gives: Time = 855.7427 seconds, Flux 1.5e-14 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1310.00 Å (per resolution element)
	;gives: Time = 1,289.6924 seconds, Flux 1e-14 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1310.00 Å (per resolution element)
	;gives: Time = 9,286.8381 seconds, Flux 1.5e-15 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1310.00 Å (per resolution element)
	;gives: Time = 14,538.0999 seconds, Flux 1e-15 flam at 1310.0 Å

	total_time=total_time+exptime_g130m


;;; GM160

	flux=[1.5e-13,1e-13,1.5e-14,1.0e-14,1.5e-15,1.0e-15]
	time=[128.8084,193.2827,1296.4941,1951.7492,13805.9126,21409.6788]
	
	fit=linfit(alog10(flux),alog10(time))

	SN_old=10.
	t_old=10.^(fit[1]*alog10(flux_in)+fit[0])

	exptime_g160m=(SN/SN_old*sqrt(t_old))^2.
	
	;G160M flux calculator for S/N = 10, specified wavelength is 1550
	;http://etc.stsci.edu/etc/input/cos/spectroscopic/
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1550.00 Å (per resolution element)
	;gives: Time = 128.8084 seconds, Flux 1.5e-13 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1550.00 Å (per resolution element)
	;gives: Time = 193.2827 seconds, Flux 1e-13 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1550.00 Å (per resolution element)
	;gives: Time = 1,296.4941 seconds, Flux 1.5e-14 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1550.00 Å (per resolution element)
	;gives: Time = 1,951.7492 seconds, Flux 1e-14 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1550.00 Å (per resolution element)
	;gives: Time = 13,805.9126 seconds, Flux 1.5e-15 flam at 1310.0 Å
	;
	;Requested Signal/Noise Ratio = 10.000 at wavelength 1550.00 Å (per resolution element)
	;gives: Time = 21,409.6788 seconds, Flux 1e-15 flam at 1310.0 Å

	total_time=total_time+exptime_g160m
	;Must round up
	total_orbits=ceil(exptime_g130m/orbit)+ceil(exptime_g160m/orbit)


if (NOT keyword_set(quiet)) then begin
	for i=0,n_elements(flux_in)-1 do begin
	print,''
	print,'** S/N', SN, format='(A-17,I-9)'
	print,'** G130M orbits', exptime_g130m[i]/orbit, 'Exposure time (ks)', exptime_g130m[i]*1e-3, format='(A-17,f-8.1,A-19,f-10.1)'
	print,'** GM160 orbits', exptime_g160m[i]/orbit, 'Exposure time (ks)', exptime_g160m[i]*1e-3, format='(A-17,f-8.1,A-19,f-10.1)'
	print,'** Total orbits', total_orbits[i], 'Exposure time (ks)',total_time[i]*1e-3, format='(A-17,f-8.1,A-19,f-10.1)'
	endfor
endif
	print,''

	struct={g130_orbit:exptime_g130m/orbit,g130_time:exptime_g130m,$
			g160_orbit:exptime_g160m/orbit,g160_time:exptime_g160m,$
			orbit:total_orbits,time:total_time}

    struct=struct_conv(struct)

	return,struct

end
