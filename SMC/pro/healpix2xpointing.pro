FUNCTION healpix2xpointing, filename, unitnum

;+
;NAME: healpix2xpointing
;PURPOSE: read a HEALPix FITS file and put into an XPOINTING array
;SYNTAX: map = healpix2xpointing(filename [, unitnum])
;
;REQUIRED INPUT: filename: name of FITS bintable file to read
;
;OPTIONAL INPUT: unitnum: number of unit to read as data. Default: 0
;   Maximum value: number of fields in each row of data file (minus 1)
;
;NOTES: MUST be run in HIDL (HEALPix IDL)
;
;MODIFICATION HISTORY: 
;   2010-7-26: Written by Alex Hill
;-

read_fits_map, filename, v1, hdr, exthdr

IF n_params() LT 2 THEN unitnum = 0

pix2ang_nest, fix(readfitsheader(hdr, 'NSIDE')), $
    lindgen(readfitsheader(hdr, 'LASTPIX')+1), theta, phi

struct = replicate({HEALPIX_POINTING, data:0., vel:0., glon:0., glat:0.}, $
    readfitsheader(hdr, 'LASTPIX')+1)
struct.data = v1[*, unitnum]
print, 'read ' + readfitsheader(exthdr, 'TTYPE' + strtrim(unitnum + 1, 2))

struct.glon = phi * !radeg
struct.glat = 90 - (theta * !radeg)

return, struct

END
