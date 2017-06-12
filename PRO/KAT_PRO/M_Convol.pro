Function M_Convol, far, gar, nocenter = noc, reverse = rev, clip = cli, $
    edge_val = edv, edge_truncate = edt

;+
; NAME:
;	M_CONVOL
; VERSION:
;	3.0
; PURPOSE:
;	Calculates the convolution of two functions represented by arrays.
; CATEGORY:
;	Mathematical, array function.
; CALLING SEQUENCE:
;	Result = M_CONVOL (FAR, GAR [, keywords])
; INPUTS:
;    FAR
;	Array, numeric, otherwise arbitrary.
;    GAR
;	Ditto.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    /NOCENTER
;	Switch.  Prevents centering one array over the other (corresponds to
;	the CENTER = 0 setting in IDL CONVOL.  The default is CENTERED.
;    /REVERSE
;	Switch.  Reverses the direction, i.e instead of f(x-x')g(x') the
;	integrand becomes f(x+x')g(x').
;    /CLIP
;	Switch.  If set, the edges of the result (which cannot be fully 
;	convolved) are set to zero, same as the default behavior of IDL CONVOL.
;	The default behaviour of M_CONVOL corresponds to the IDL CONVOL 
;	EDGE_WRAP SETTING.
;    EDGE_VAL
;	Accepts a value to be used for all the "beyond the edge " elements.
;	if EDGE_VAL is provided, /CLIP is ignored.  EDGE_VAL and EDGE_TRUNCATE
;	are mutually exclusive
;    /EDGE_TRUNCATE
;	Switch.  If set, the "beyond the edge" elements are obtained by a
;	propagation of the edge elements of FAR.  EDGE_TRUNCATE and EDGE_VAL
;	are mutually exclusive
; OUTPUTS:
;	Returns the result of the convolution with dimensions corresponding to 
;	the bigger of the two arrays.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses FFT.  Faster then the IDL CONVOL, and differs from it in few 
;	significant aspects, as follows:
;
;	1) The two arrays are treated symmetrically, i.e. the result stays the 
;	same if they are exchanged (with the exception that when /REVERSE is
;	set, it is always the second array that's reversed.
;	2) The problem with IDL CONVOL, where the centered convolution is 
;	calculated in a reversed direction (versus standard mathematical 
;	practice) has been corrected.
;	3) The arrays don't have to be of the same dimensionality.  If they're 
;	not they are imbedded in an array big enough to contained both and 
;	padded with zeroes (default), a constant value (if EDGE_VAL is used)
;	or the edge elements of FAR (if EDGE_TRUNCATE is used).
;
;	Calls CAST, EXTEND_ARRAY, FPU_FIX and TYPE from MIDL.
; MODIFICATION HISTORY:
;	Created 15-NOV-1996 by Mati Meron.
;	Modified 20-JAN-1997 by Mati Meron.  Streamlined (through the use of
;	EXTEND_ARRAY and added keyword EDGE_TRUNCATE.
;	Modified 15-SEP-1998 by Mati Meron.  Underflow filtering added.
;-

    ndm = 7
    if keyword_set(noc) then cefl = 0 else cefl = 1
    if keyword_set(rev) then refl = 1 else refl = 0
    clfl = keyword_set(cli)
    edfl = keyword_set(edt) or n_elements(edv) ne 0
    typ = Type(far) > Type(gar)

    sif = size([far])
    sig = size([gar])
    ndf = sif(0)
    ndg = sig(0)
    if ndf lt ndm then nf=[replicate(1l,ndm-ndf),sif(1:ndf)] else nf=sif(1:ndf)
    if ndg lt ndm then ng=[replicate(1l,ndm-ndg),sig(1:ndg)] else ng=sig(1:ndg)

    p = nf > ng
    q = nf < ng
    if edfl then begin
	clfl = 1
	l = ((1 - refl)*(q - 1) + refl*cefl*q)/(1 + cefl)
	p = p + q - 1
    endif else l = replicate(0l,ndm)

    prod = 1l
    for i = 0, ndm - 1 do prod = prod*p(i)
    sir = [ndm,p,typ,prod]

    wfar = Extend_array(far,l,newsize=sir,value=edv,edge=edt)
    wgar = Extend_array(gar,newsize=sir)

    if refl then res = conj(fft(conj(wgar))) else res = fft(wgar)
    res = reform(FPU_fix(prod*fft(fft(wfar)*res,/inverse)),sir(1:ndm))

    s = (2*refl - 1)*(q/2)
    if cefl then res = shift(res,s(0),s(1),s(2),s(3),s(4),s(5),s(6))

    if clfl then begin
	l = ((1 - refl)*(q - 1) + refl*cefl*q)/(1 + cefl)
	h = l + p - q

	res = res(l(0):h(0),l(1):h(1),l(2):h(2),l(3):h(3), $
		l(4):h(4),l(5):h(5),l(6):h(6))
	if not edfl then res = Extend_array(res,l,newsize=sir)
    endif

    return, Cast(reform(res),typ,typ)
end