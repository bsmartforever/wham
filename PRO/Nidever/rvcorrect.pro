;+
; RVCORRECT -- Compute radial velocity correction
;
; INPUTS:
;  RA     Right Ascension of the star (decimal, in hours)
;  DEC    Declination of the star (decimal, in degrees)
;  EP     Equinox of the coordinates
;  YEAR   Year of the observation (4 digits)
;  MONTH  Month of the observation (1-12)
;  DAY    Day of the month of the observation
;  UT     The UT time of the observation
;  OBS    The name of the observatory
;
; OUTPUTS:
;  HJD    The heliocentric Julian day
;  VHELIO The heliocentric radial velocity correction
;  VLSR   The LSR radial velocity correction
;  =VROT  The diurnal rotation velocity
;  =VBARY The lunary velocity
;  =VORB  The annual velocity
;  =VSOL  The solar velocity
;
; Translated from IRAF by D.Nidever  Sept.2006
;-



;# AST_DATE_TO_JULDAY -- Convert date to Julian day.
;# This assumes dates after year 99.
;
function ast_date_to_julday, year, month, day, t

;int	year			# Year
;int	month			# Month (1-12)
;int	day			# Day of month
;double	t			# Time for date (mean solar day)
;
;double	jd
;int	y, m, d
;
;begin

        J2000  = 2000.0D0       ; J2000
        JD2000 = 2451545.0D0    ; J2000 Julian Date
        JYEAR  = 365.25D0       ; Julian year

	if (year lt 100) then begin
	    y = 1900.0 + year
        endif else begin
	    y = year
        endelse

	if (month gt 2) then begin
	    m = month + 1
	endif else begin
	    m = month + 13
	    y = y - 1
	endelse

	jd = long(JYEAR * y) + long(30.6001 * m) + day + 1720995.0d0
	if ( (day + 31.0 * (m + 12 * y)) ge 588829.0) then begin
	    d = long(y / 100.)
	    m = long(y / 400.)
	    jd = jd + 2.0 - d + m
	endif

	jd = jd - 0.5 + long(t * 360000.0 + 0.5) / 360000.0 / 24.0

	return,jd
end


;-------------------------------------------------


;# AST_DATE_TO_EPOCH -- Convert Gregorian date and solar mean time to
;# a Julian epoch.  A Julian epoch has 365.25 days per year and 24
;# hours per day.

pro ast_date_to_epoch, year, month, day, ut, epoch

;int	year			# Year
;int	month			# Month (1-12)
;int	day			# Day of month
;double	ut			# Universal time for date (mean solar day)
;double	epoch			# Julian epoch
;
;double	jd, ast_date_to_julday()
;
;begin

        J2000  = 2000.0D0       ; J2000
        JD2000 = 2451545.0D0    ; J2000 Julian Date
        JYEAR  = 365.25D0       ; Julian year

	jd = ast_date_to_julday(year, month, day, ut)
	epoch = J2000 + (jd - JD2000) / JYEAR
end


;-------------------------------------------------


;# AST_EPOCH_TO_DATE -- Convert a Julian epoch to year, month, day, and time.

pro ast_epoch_to_date, epoch, year, month, day, ut

;double	epoch			# Julian epoch
;int	year			# Year
;int	month			# Month (1-12)
;int	day			# Day of month
;double	ut			# Universal time for date
;
;double	jd
;
;begin

        J2000  = 2000.0D0       ; J2000
        JD2000 = 2451545.0D0    ; J2000 Julian Date
        JYEAR  = 365.25D0       ; Julian year

	jd = JD2000 + (epoch - J2000) * JYEAR
	ast_julday_to_date,jd, year, month, day, ut
end


;-------------------------------------------------


;# AST_JULDAY -- Convert epoch to Julian day.
;
function ast_julday, epoch

;double	epoch			# Epoch
;
;double	jd

;begin

        J2000  = 2000.0D0       ; J2000
        JD2000 = 2451545.0D0    ; J2000 Julian Date
        JYEAR  = 365.25D0       ; Julian year

	jd = JD2000 + (epoch - J2000) * JYEAR
	return,jd

end


;-------------------------------------------------


;# AST_JULDAY_TO_DATE -- Convert Julian date to calendar date.
;# This is taken from Numerical Receipes by Press, Flannery, Teukolsy, and
;# Vetterling.

pro ast_julday_to_date, j, year, month, day, t

;double	j			# Julian day
;int	year			# Year
;int	month			# Month (1-12)
;int	day			# Day of month
;double	t			# Time for date (mean solar day)
;
;int	ja, jb, jc, jd, je
;
;begin

	ja = long(j)
	t = 24. * (j - ja + 0.5)

	if (ja ge 2299161) then begin
	    jb = long(((ja - 1867216) - 0.25) / 36524.25)
	    ja = ja + 1 + jb - long(jb / 4)
	endif

	jb = ja + 1524
	jc = long(6680. + ((jb - 2439870) - 122.1) / JYEAR)
	jd = 365 * jc + long(jc / 4)
	je = long((jb - jd) / 30.6001)
	day = jb - jd - long(30.6001 * je)
	month = je - 1
	if (month gt 12) then begin
	    month = month - 12
        endif

	year = jc - 4715

	if (month gt 2) then begin
	    year = year - 1
        endif
	if (year lt 0) then begin
	    year = year - 1
        endif

end


;-------------------------------------------------


;# AST_MST -- Mean sidereal time of the epoch at the given longitude.
;# This procedure may be used to optain Greenwich Mean Sidereal Time (GMST)
;# by setting the longitude to 0.

function ast_mst, epoch, longitude

;double	epoch		# Epoch
;double	longitude	# Longitude in degrees
;
;double	jd, ut, t, st
;double	ast_julday()
;
;begin
	;# Determine JD and UT, and T (JD in centuries from J2000.0).
	jd = ast_julday(epoch)
	ut = (jd - long(jd) - 0.5) * 24.0
	t = (jd - 2451545.d0) / 36525.d0

	;# The GMST at 0 UT in seconds is a power series in T.
	st = 24110.54841d0 + $
	    t * (8640184.812866d0 + t * (0.093104d0 - t * 6.2d-6))

	;# Correct for longitude and convert to standard hours.
	st = (st / 3600. + ut - longitude / 15.) mod 24.0D0

	if (st lt 0) then begin
	    st = st + 24
        endif

	return,st
end

;-------------------------------------------------

; AST_PRECESS -- Precess coordinates from epoch1 to epoch2.
;
; The method used here is based on the new IAU system described in the
; supplement to the 1984 Astronomical Almanac.  The precession is
; done in two steps; precess epoch1 to the standard epoch J2000.0 and then
; precess from the standard epoch to epoch2.  The precession between
; any two dates is done this way because the rotation matrix coefficients
; are given relative to the standard epoch.

pro ast_precess, ra1, dec1, epoch1, ra2, dec2, epoch2

;double	ra1, dec1, epoch1		# First coordinates
;double	ra2, dec2, epoch2		# Second coordinates
;
;double	r0[3], r1[3], p[3, 3]
;bool	fp_equald()
;
;begin

        r0 = dblarr(3)
        r1 = dblarr(3)
        p = dblarr(3,3)

	; If the input epoch is 0 or undefined then assume the input epoch
	; is the same as the output epoch.  If the two epochs are the same
	; then return the coordinates from epoch1.

	if ((epoch1 eq 0.) OR not keyword_set(epoch1) OR (epoch1 eq epoch2)) then begin
	    ra2 = ra1
	    dec2 = dec1
	    return
	end

	; Rectangular equitorial coordinates (direction cosines).
	ra2 = (ra1 * 15.)/!radeg
	dec2 = dec1/!radeg

	r0[0] = cos(ra2) * cos(dec2)
	r0[1] = sin(ra2) * cos(dec2)
	r0[2] = sin(dec2)

	; If epoch1 is not the standard epoch then precess to the standard
	; epoch.

	if (epoch1 ne 2000.) then begin
	    ast_rotmatrix, epoch1, p

	    ; Note that we multiply by the inverse of p which is the
	    ; transpose of p.

	    r1[0] = p[0, 0] * r0[0] + p[0, 1] * r0[1] + p[0, 2] * r0[2]
	    r1[1] = p[1, 0] * r0[0] + p[1, 1] * r0[1] + p[1, 2] * r0[2]
	    r1[2] = p[2, 0] * r0[0] + p[2, 1] * r0[1] + p[2, 2] * r0[2]
	    r0[0] = r1[0]
	    r0[1] = r1[1]
	    r0[2] = r1[2]
	end

	; If epoch2 is not the standard epoch then precess from the standard
	; epoch to the desired epoch.

	if (epoch2 ne 2000.) then begin
	    ast_rotmatrix, epoch2, p
	    r1[0] = p[0, 0] * r0[0] + p[1, 0] * r0[1] + p[2, 0] * r0[2]
	    r1[1] = p[0, 1] * r0[0] + p[1, 1] * r0[1] + p[2, 1] * r0[2]
	    r1[2] = p[0, 2] * r0[0] + p[1, 2] * r0[1] + p[2, 2] * r0[2]
	    r0[0] = r1[0]
	    r0[1] = r1[1]
	    r0[2] = r1[2]
	end

	; Convert from radians to hours and degrees.
	ra2 = (atan(r0[1], r0[0]) / 15.) * !radeg
	dec2 =(asin(r0[2])) * !radeg
	if (ra2 lt 0.) then begin
	    ra2 = ra2 + 24.0
        end

end

;-------------------------------------------------


; ROTMATRIX -- Compute the precession rotation matrix from the standard epoch
; J2000.0 to the specified epoch.

pro ast_rotmatrix, epoch, p

;double	epoch		# Epoch of date
;double	p[3, 3]		# Rotation matrix
;
;double	t, a, b, c, ca, cb, cc, sa, sb, sc
;double	ast_julday()
;
;begin

        p = dblarr(3,3)

	; The rotation matrix coefficients are polynomials in time measured
	; in Julian centuries from the standard epoch.  The coefficients are
	; in degrees.

	t = (ast_julday(epoch) - 2451545.0d0) / 36525d0

	a = t * (0.6406161d0 + t * (0.0000839d0 + t * 0.0000050d0))
	b = t * (0.6406161d0 + t * (0.0003041d0 + t * 0.0000051d0))
	c = t * (0.5567530d0 - t * (0.0001185d0 + t * 0.0000116d0))

	; Compute the cosines and sines once for efficiency.
	ca = cos(a/!radeg)
	sa = sin(a/!radeg)
	cb = cos(b/!radeg)
	sb = sin(b/!radeg)
	cc = cos(c/!radeg)
	sc = sin(c/!radeg)

	; Compute the rotation matrix from the sines and cosines.
	p[0, 0] = ca * cb * cc - sa * sb
	p[1, 0] = -sa * cb * cc - ca * sb
	p[2, 0] = -cb * sc
	p[0, 1] = ca * sb * cc + sa * cb
	p[1, 1] = -sa * sb * cc + ca * cb
	p[2, 1] = -sb * sc
	p[0, 2] = ca * sc
	p[1, 2] = -sa * sc
	p[2, 2] = cc

end


;-------------------------------------------------

; AST_COORD -- Convert spherical coordinates to new system.
;
; This procedure converts the longitude-latitude coordinates (a1, b1)
; of a point on a sphere into corresponding coordinates (a2, b2) in a
; different coordinate system that is specified by the coordinates of its
; origin (ao, bo).  The range of a2 will be from -pi to pi.

pro ast_coord, ao, bo, ap, bp, a1, b1, a2, b2

;double	ao, bo		# Origin of new coordinates (radians)
;double	ap, bp		# Pole of new coordinates (radians)
;double	a1, b1		# Coordinates to be converted (radians)
;double	a2, b2		# Converted coordinates (radians)
;
;double	sao, cao, sbo, cbo, sbp, cbp
;double	x, y, z, xp, yp, zp, temp
;
;begin
	x = cos(a1) * cos(b1)
	y = sin(a1) * cos(b1)
	z = sin(b1)
	xp = cos(ap) * cos(bp)
	yp = sin(ap) * cos(bp)
	zp = sin(bp)

	; Rotate the origin about z.
	sao = sin(ao)
	cao = cos(ao)
	sbo = sin(bo)
	cbo = cos(bo)
	temp = -xp * sao + yp * cao
	xp = xp * cao + yp * sao
	yp = temp
	temp = -x * sao + y * cao
	x = x * cao + y * sao
	y = temp

	; Rotate the origin about y.
	temp = -xp * sbo + zp * cbo
	xp = xp * cbo + zp * sbo
	zp = temp
	temp = -x * sbo + z * cbo
	x = x * cbo + z * sbo
	z = temp

	; Rotate pole around x.
	sbp = zp
	cbp = yp
	temp = y * cbp + z * sbp
	y = y * sbp - z * cbp
	z = temp

	; Final angular coordinates.
	a2 = atan(y, x)
	b2 = asin(z)
end


;-------------------------------------------------


; AST_HJD -- Helocentric Julian Day from Epoch

pro ast_hjd, ra, dec, epoch, ltim, hjd

;double	ra		# Right ascension of observation (hours)
;double	dec		# Declination of observation (degrees)
;double	epoch		# Julian epoch of observation
;double	lt		# Light travel time in seconds
;double	hjd		# Helocentric Julian Day
;
;double	ast_julday()
;
;begin

	ast_jd_to_hjd, ra, dec, ast_julday(epoch), ltim, hjd

end

;-------------------------------------------------


; AST_JD_TO_HJD -- Helocentric Julian Day from UT Julian date

pro ast_jd_to_hjd, ra, dec, jd, ltim, hjd

;double	ra		# Right ascension of observation (hours)
;double	dec		# Declination of observation (degrees)
;double	jd		# Geocentric Julian date of observation
;double	lt		# Light travel time in seconds
;double	hjd		# Helocentric Julian Day
;
;double	t, manom, lperi, oblq, eccen, tanom, slong, r, d, l, b, rsun

;begin
	; JD is the geocentric Julian date.
	; T is the number of Julian centuries since J1900.

	t = (jd - 2415020d0) / 36525d0

	; MANOM is the mean anomaly of the Earth's orbit (degrees)
	; LPERI is the mean longitude of perihelion (degrees)
	; OBLQ is the mean obliquity of the ecliptic (degrees)
	; ECCEN is the eccentricity of the Earth's orbit (dimensionless)

	manom = 358.47583d0 + $
	    t * (35999.04975d0 - t * (0.000150d0 + t * 0.000003d0))
	lperi = 101.22083d0 + $
	    t * (1.7191733d0 + t * (0.000453d0 + t * 0.000003d0))
	oblq = 23.452294d0 - $
	    t * (0.0130125d0 + t * (0.00000164d0 - t * 0.000000503d0))
	eccen = 0.01675104d0 - t * (0.00004180d0 + t * 0.000000126d0)

	; Convert to principle angles
	manom = (manom mod 360.0D0)
	lperi = (lperi mod 360.0D0)

	; Convert to radians
	r = (ra * 15)/!radeg
	d = dec/!radeg
	manom = manom/!radeg
	lperi = lperi/!radeg
	oblq = oblq/!radeg

	; TANOM is the true anomaly (approximate formula) (radians)
	tanom = manom + (2.0 * eccen - 0.25 * eccen^3.0) * sin(manom) + $
	    1.25 * eccen^2.0 * sin(2.0 * manom) + $
	    13./12. * eccen^3.0 * sin(3.0 * manom)

	; SLONG is the true longitude of the Sun seen from the Earth (radians)
	slong = lperi + tanom + !dpi

	; L and B are the longitude and latitude of the star in the orbital
	; plane of the Earth (radians)

	ast_coord, double (0.), double (0.), double (-!dpi/2.0),$
	    (!dpi/2.0) - oblq, r, d, l, b

	; R is the distance to the Sun.
	rsun = (1.0 - eccen^2.0) / (1.0 + eccen * cos(tanom))

	; LT is the light travel difference to the Sun.
	ltim = -0.005770d0 * rsun * cos(b) * cos(l - slong)
	hjd = jd + ltim
end


;-------------------------------------------------


; AST_VR -- Project a velocity vector in radial velocity along line of sight.

pro ast_vr, ra1, dec1, v1, ra2, dec2, v2

;double  ra1             # Right ascension of velocity vector (hours)
;double  dec1            # Declination of velocity vector (degrees)
;double  v1              # Magnitude of velocity vector
;double  ra2             # Right ascension of observation (hours)
;double  dec2            # Declination of observation (degrees)
;double  v2              # Radial velocity along direction of observation
;
;double  vx, vy, vz, cc, cs, s

;begin
        ; Cartisian velocity components of the velocity vector.
        vx = v1 * cos( (15. * ra1)/!radeg) * cos(dec1/!radeg)
        vy = v1 * sin( (15. * ra1)/!radeg) * cos(dec1/!radeg)
        vz = v1 * sin(dec1/!radeg)

        ; Direction cosines along the direction of observation.
        cc = cos(dec2/!radeg) * cos( (15. * ra2)/!radeg)
        cs = cos(dec2/!radeg) * sin( (15. * ra2)/!radeg)
        s  = sin(dec2/!radeg)

        ; Project velocity vector along the direction of observation.
        v2 = (vx * cc + vy * cs + vz * s)


end


;-------------------------------------------------

; AST_VORBIT -- Radial velocity component of the Earth-Moon barycenter
; relative to the Sun.

pro ast_vorbit, ra, dec, epoch, v

;double  ra              # Right ascension of observation (hours)
;double  dec             # Declination of observation (degrees)
;double  epoch           # Julian epoch of observation
;double  v               # Component of orbital velocity (km/s)
;
;double  t, manom, lperi, oblq, eccen, tanom, slong, r, d, l, b, vorb
;double  ast_julday()
;
;begin
        ; T is the number of Julian centuries since J1900.
        t = (ast_julday(epoch) - 2415020d0) / 36525.

        ; MANOM is the mean anomaly of the Earth's orbit (degrees)
        ; LPERI is the mean longitude of perihelion (degrees)
        ; OBLQ is the mean obliquity of the ecliptic (degrees)
        ; ECCEN is the eccentricity of the Earth's orbit (dimensionless)

        manom = 358.47583d0 + $
            t * (35999.04975d0 - t * (0.000150d0 + t * 0.000003d0))
        lperi = 101.22083d0 + $
            t * (1.7191733d0 + t * (0.000453d0 + t * 0.000003d0))
        oblq = 23.452294d0 - $
            t * (0.0130125d0 + t * (0.00000164d0 - t * 0.000000503d0))
        eccen = 0.01675104d0 - t * (0.00004180d0 + t * 0.000000126d0)

        ; Convert to principle angles
        manom = (manom mod 360.0D0)
        lperi = (lperi mod 360.0D0)

        ; Convert to radians
        r = (ra * 15)/!radeg
        d = dec/!radeg
        manom = manom/!radeg
        lperi = lperi/!radeg
        oblq = oblq/!radeg

        ; TANOM is the true anomaly (approximate formula) (radians)
        tanom = manom + (2.0 * eccen - 0.25 * eccen^3.0) * sin(manom) + $
            1.25 * eccen^2.0 * sin(2.0 * manom) + $
            13./12. * eccen^3.0 * sin(3.0 * manom)

        ; SLONG is the true longitude of the Sun seen from the Earth (radians)
        slong = lperi + tanom + !dpi

        ; L and B are the longitude and latitude of the star in the orbital
        ; plane of the Earth (radians)

        ast_coord, double(0.), double(0.), double(-!dpi/2.0),$
            (!dpi/2.0) - oblq, r, d, l, b

        ; VORB is the component of the Earth's orbital velocity perpendicular
        ; to the radius vector (km/s) where the Earth's semi-major axis is
        ; 149598500 km and the year is 365.2564 days.

        vorb = ((2.0*!dpi / 365.2564d0) * $
            149598500.d0 / sqrt(1.0 - eccen^2)) / 86400.d0

        ; V is the projection onto the line of sight to the observation of
        ; the velocity of the Earth-Moon barycenter with respect to the
        ; Sun (km/s).

        v = vorb * cos(b) * (sin(slong - l) - eccen * sin(lperi - l))
end


;-------------------------------------------------


; AST_VBARY -- Radial velocity component of center of the Earth relative to
; to the barycenter of the Earth-Moon system.

pro ast_vbary, ra, dec, epoch, v

;double  ra              # Right ascension of observation (hours)
;double  dec             # Declination of observation (degrees)
;double  epoch           # Julian epoch of observation
;double  v               # Component of orbital velocity (km/s)
;
;double  t, oblq, omega, llong, lperi, inclin, em, anom, vmoon
;double  r, d, l, b, lm, bm, ast_julday()
;
;begin
        ; T is the number of Julian centuries since J1900.
        t = (ast_julday(epoch) - 2415020) / 36525.

        ; OBLQ is the mean obliquity of the ecliptic
        ; OMEGA is the longitude of the mean ascending node
        ; LLONG is the mean lunar longitude (should be 13.1763965268)
        ; LPERI is the mean lunar longitude of perigee
        ; INCLIN is the inclination of the lunar orbit to the ecliptic
        ; EM is the eccentricity of the lunar orbit (dimensionless)
        ; All quantities except the eccentricity are in degrees.

        oblq = 23.452294d0 - $
            t * (0.0130125d0 + t * (0.00000164d0 - t * 0.000000503d0))
        omega = 259.183275d0 - $
            t * (1934.142008d0 + t * (0.002078d0 + t * 0.000002d0))
        llong = 270.434164d0 + $
            t * (481267.88315d0 + t * (-0.001133d0 + t * 0.0000019d0)) - omega
        lperi = 334.329556d0 + $
            t * (4069.034029d0 - t * (0.010325d0 + t * 0.000012d0)) - omega
        em = 0.054900489d0
        inclin = 5.1453964d0

        ; Determine true longitude.  Compute mean anomaly, convert to true
        ; anomaly (approximate formula), and convert back to longitude.
        ; The mean anomaly is only approximate because LPERI should
        ; be the true rather than the mean longitude of lunar perigee.

        lperi = lperi/!radeg
        llong = llong/!radeg
        anom = llong - lperi
        anom = anom + (2.0 * em - 0.25 * em^3.0) * sin(anom) + 1.25 * em^2.0 * $
            sin(2.0 * anom) + 13./12. * em^3.0 * sin(3.0 * anom)
        llong = anom + lperi

        ; L and B are the ecliptic longitude and latitude of the observation.
        ; LM and BM are the lunar longitude and latitude of the observation
        ; in the lunar orbital plane relative to the ascending node.

        r = (ra * 15)/!radeg
        d = dec/!radeg
        omega = omega/!radeg
        oblq = oblq/!radeg
        inclin = inclin/!radeg

        ast_coord, double(0.), double(0.), double(-!dpi/2.0),$
            (!dpi/2.0) - oblq, r, d, l, b
        ast_coord, omega, double(0.), omega-(!dpi/2.0), (!dpi/2.0)-inclin,$
            l, b, lm, bm

        ; VMOON is the component of the lunar velocity perpendicular to the
        ; radius vector.  V is the projection onto the line of sight to the
        ; observation of the velocity of the Earth's center with respect to
        ; the Earth-Moon barycenter.  The 81.53 is the ratio of the Earth's
        ; mass to the Moon's mass.

        vmoon = (2.0*!dpi / 27.321661d0) * $
            384403.12040d0 / sqrt (1.0 - em^2.0) / 86400.
        v = vmoon * cos(bm) * (sin(llong - lm) - em * sin(lperi - lm))
        v = v / 81.53
end


;-------------------------------------------------


; AST_VROTATE -- Radial velocity component of the observer relative to
; the center of the Earth due to the Earth's rotation.

pro ast_vrotate, ra, dec, epoch, latitude, longitude, altitude, v

;double  ra              # Right Ascension of observation (hours)
;double  dec             # Declination of observation (degrees)
;double  epoch           # Epoch of observation (Julian epoch)
;double  latitude        # Latitude (degrees)
;double  longitude       # Latitude (degrees)
;double  altitude        # Altitude (meters)
;double  v               # Velocity (km / s)
;
;double  lat, dlat, r, vc, lmst, ast_mst()
;
;begin
        ; LAT is the latitude in radians.
        lat = latitude/!radeg

        ; Reduction of geodetic latitude to geocentric latitude (radians).
        ; Dlat is in arcseconds.

        dlat = -(11.0 * 60.0 + 32.743000d0) * sin(2.0 * lat) + $
                1.163300d0 * sin(4.0 * lat) -0.002600d0 * sin(6.0 * lat)
        lat = lat + (dlat / 3600.) / !radeg

        ; R is the radius vector from the Earth's center to the observer
        ; (meters).  Vc is the corresponding circular velocity
        ; (meters/sidereal day converted to km / sec).
        ; (sidereal day = 23.934469591229 hours (1986))

        r = 6378160.0d0 * (0.998327073d0 + 0.00167643800d0 * cos(2.0 * lat) - $
            0.00000351d0 * cos(4.0 * lat) + 0.000000008d0 * cos(6.0 * lat)) + $
            altitude
        vc = 2.0*!dpi * (r / 1000.)  / (23.934469591229d0 * 3600.0)

        ; Project the velocity onto the line of sight to the star.
        lmst = ast_mst(epoch, longitude)
        v = vc * cos(lat) * cos(dec/!radeg) * $
            sin ( ((ra - lmst) * 15.)/!radeg )
end


;-------------------------------------------------

; RVCORRECT -- Compute the radial velocities.

pro rvcorrect, ra, dec, ep, year, month, day, ut, obs, hjd, vhelio, vlsr,$
               vrot=vrot, vbary=vbary, vorb=vorb, vsol=vsol

;double	ra, dec, ep			# Coordinates of observation
;int	year, month, day		# Date of observation
;double	ut				# Time of observation
;double	hjd				# Helocentric Julian Day
;double	vrot, vbary, vorb, vsol		# Returned velocity components
;
;double	epoch, ra_obs, dec_obs, ra_vsun, dec_vsun, t
;
;include	"rvcorrect.com"
;
;begin

; Not enough inputs
if n_elements(ra) eq 0 or n_elements(dec) eq 0 or n_elements(ep) eq 0 or $
   n_elements(year) eq 0 or n_elements(month) eq 0 or n_elements(day) eq 0 or $
   n_elements(ut) eq 0 or n_elements(obs) eq 0 then begin

   print,'Syntax - rvcorrect, ra, dec, ep, year, month, day, ut, obs, hjd, vhelio, vlsr'
   return
endif

;ra = 0.0
;dec = 0.0
;ep = 2000.0
;year = 2006
;month = 9
;day = 18
;ut = 12.0

; Getting observatory information
observatory,obs,str
latitude = str.latitude
longitude = str.longitude
altitude = str.altitude

; SOLAR INFORMATION
;	vs = clgetd ("vsun")
;	ras = clgetd ("ra_vsun")
;	decs = clgetd ("dec_vsun")
;	eps = clgetd ("epoch_vsun")
; FROM THE RVCORRECT HELP
; FOR LSR 
vs = 20.0
ras = 18.0    ;"18:00:00"
decs = 30.0   ;"30:00:00"
eps = 1900.0

; More than one star input
nra = n_elements(ra)
if (nra gt 1) and n_elements(dec) eq nra and n_elements(ep) eq nra and n_elements(year) eq nra and $
   n_elements(month) eq nra and n_elements(day) eq nra and n_elements(ut) eq nra and n_elements(obs) eq nra then begin

 hjd = dblarr(nra)
 vhelio = dblarr(nra)
 vlsr = dblarr(nra)

  ; Loop through the stars
  for i=0,nra-1 do begin
    rvcorrect, ra[i], dec[i], ep[i], year[i], month[i], day[i], ut[i], obs[i], hjd1, vhelio1, vlsr1
    hdj[i] = hdj1
    vhelio[i] = vhelio1
    vlsr[i] = vlsr1
  end

end


	; Determine epoch of observation and precess coordinates.
	ast_date_to_epoch, year, month, day, ut, epoch

	ast_precess, ra, dec, ep, ra_obs, dec_obs, epoch

        ;ra_obs = ra*15.0
        ;dec_obs = dec
        ;precess,ra_obs,dec_obs,ep,epoch
        ;ra_obs = ra_obs/15.0

	ast_precess, ras, decs, eps, ra_vsun, dec_vsun, epoch

        ;ra_vsun = ras*15.0
        ;dec_vsun = decs
        ;precess,ra_vsun,dec_vsun,eps,epoch
        ;ra_vsun = ra_vsun/15.0

	ast_hjd, ra_obs, dec_obs, epoch, t, hjd



	; Determine velocity components.
	ast_vr, ra_vsun, dec_vsun, vs, ra_obs, dec_obs, vsol
	ast_vorbit, ra_obs, dec_obs, epoch, vorb
	ast_vbary, ra_obs, dec_obs, epoch, vbary
	ast_vrotate, ra_obs, dec_obs, epoch, latitude, longitude,$
	    altitude, vrot


	;# Record velocities in the parameter file.
	;call clputd ("hjd", hjd)
	;call clputd ("vhelio", vobs+vrot+vbary+vorb)
	;call clputd ("vlsr", vobs+vrot+vbary+vorb+vsol)

        ; Computing final velocities
        vobs = 0.0
        vhelio = vobs+vrot+vbary+vorb
        vlsr = vobs+vrot+vbary+vorb+vsol

;print,'hjd = ',hjd
;print,'vhelio = ',vhelio
;print,'vlsr = ',vlsr
;print,'vrot = ',vrot
;print,'vbary = ',vbary
;print,'vorb = ',vorb
;print,'vsol = ',vsol

;stop

end
