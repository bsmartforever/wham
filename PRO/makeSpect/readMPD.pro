;+
; :Description:
;
;-
function readMPD, filename
  compile_opt idl2

  params = {rmax: 0.0, area: 0.0, xoff: 0.0, yoff: 0.0, e : 0.0, $
    bval: 0.0, xc  : 0.0, yc  : 0.0, v0  : 0.0, v1  : 0.0, $
    v2  : 0.0}
  
  if ~isa(filename) then filename = '/d/wham/lib/makeSpect/makeSpect.mpd'
  
  if ~FILE_TEST(filename, /read) then begin
    message, 'WARNING: MPD file unreadable, no parameters loaded: ' + filename, /info
    return, params
  endif
  
  READCOL, filename, Par, ParVal, COMMENT = '#', DELIMITER = '=', format = 'A,F', /silent
  
  Params.xc = ParVal[WHERE(STRMATCH(Par, 'xc*') EQ 1)]
  Params.yc = ParVal[WHERE(STRMATCH(Par, 'yc*') EQ 1)]
  Params.rmax = ParVal[WHERE(STRMATCH(Par, 'rmax*') EQ 1)]
  Params.area = ParVal[WHERE(STRMATCH(Par, 'area*') EQ 1)]
  Params.xoff = ParVal[WHERE(STRMATCH(Par, 'xoff*') EQ 1)]
  Params.yoff = ParVal[WHERE(STRMATCH(Par, 'yoff*') EQ 1)]
  Params.e = ParVal[WHERE(STRMATCH(Par, 'e*') EQ 1)]
  Params.bval = ParVal[WHERE(STRMATCH(Par, 'bval*') EQ 1)]
  Params.v0 = ParVal[WHERE(STRMATCH(Par, 'v0*') EQ 1)]
  Params.v1 = ParVal[WHERE(STRMATCH(Par, 'v1*') EQ 1)]
  Params.v2 = ParVal[WHERE(STRMATCH(Par, 'v2*') EQ 1)]

  return, params
end
