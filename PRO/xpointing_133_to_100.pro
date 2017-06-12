FUNCTION xpointing_133_to_100, mapin

;+
;NAME: xpointing_133_to_100
;PURPOSE: convert an extended XPOINTING structure to an XPOINTING_100 structure
;	Sets map.vel = mapin.vel[33:132] and equivalent for map.data and map.var
;SYNTAX: map_100 = xpointing_133_to_100(map_133)
;HISTORY: 2013 January: written by Alex Hill
;-

map = replicate({xpointing_100, $
	name: string(''), $
	vel: fltarr(100), $
	data: fltarr(100), $
	var: fltarr(100), $
	glon: float(0), $
	glat: float(0), $
	vlsr: float(0), $
	zd: float(0), $
	az: float(0), $
	date: string(''), $
	day: long(0), $
	time: float(0), $
	pacmd: float(0), $
	pbcmd: float(0), $
	pamon: float(0), $
	pbmon: float(0), $
	ccdtemp: float(0), $
	etemp: float(0), $
	chi2: float(0), $
	border: fix(0), $
	ngauss: fix(0), $
	bkg: fltarr(4), $
	bkgsd: fltarr(4), $
	mean: fltarr(10), $
	meansd: fltarr(10), $
	width: fltarr(10), $
	widthsd: fltarr(10), $
	area: fltarr(10), $
	areasd: fltarr(10), $
	atmos: float(0), $
	a75r: float(0) $
}, n_elements(mapin))

map.name = mapin.name
map.vel = mapin.vel[33:132]
map.data = mapin.data[33:132]
map.var = mapin.var[33:132]

map.glon = mapin.glon
map.glat = mapin.glat
map.vlsr = mapin.vlsr
map.zd = mapin.zd
map.az = mapin.az
map.date = mapin.date
map.day = mapin.day
map.time = mapin.time
map.pacmd = mapin.pacmd
map.pbcmd = mapin.pbcmd
map.pamon = mapin.pamon
map.pbmon = mapin.pbmon
map.ccdtemp = mapin.ccdtemp
map.etemp = mapin.etemp
map.chi2 = mapin.chi2
map.border = mapin.border
map.ngauss = mapin.ngauss
map.bkg = mapin.bkg
map.bkgsd = mapin.bkgsd
map.mean = mapin.mean
map.meansd = mapin.meansd
map.width = mapin.width
map.widthsd = mapin.widthsd
map.area = mapin.area
map.areasd = mapin.areasd
map.atmos = mapin.atmos
map.a75r = mapin.a75r

return, map

END
