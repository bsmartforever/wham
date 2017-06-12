PRO cor_spec, _extra = extra

;+
;NAME: cor_spec
;WRAPPER for COR_SPEC_CTIO and COR_SPEC_KPNO
;
;Uses cor_spec_ctio if TELESCOP value in FITS header is WHAM-SOUTH
;	cor_spec_kpno if TELESCOP is WHAM
;
;USAGE: typically: cor_spec, /chop
;   Note that cor_spec will find FITS files with the following command:
;   SPAWN, "find ./ -name '*.fts' -print | grep 'combo/' > list.txt"
;
;
;Written by Alex Hill 2009 June 19
;Modified by ASH 2009-8-19 to check to use CTIO or KPNO cor_spec
;-

SPAWN, "find ./ -name '*.fts' -print | grep 'combo/' > list.txt"
OPENR, unit,  'list.txt', /get_lun
  fn = ''

READF, unit, fn
telescop = readfitsheader(fn, 'TELESCOP')
close, unit

IF telescop EQ 'WHAM-SOUTH' THEN BEGIN
	print, 'Telescope is WHAM-SOUTH; calling cor_spec_ctio'
	cor_spec_ctio, _extra = extra
ENDIF ELSE IF telescop EQ 'WHAM' THEN BEGIN
	print, 'Telescope is WHAM; calling cor_spec_kpno'
	cor_spec_kpno, _extra = extra
ENDIF ELSE message, 'telescope (' + strtrim(telescop, 2) +') not known'

END
