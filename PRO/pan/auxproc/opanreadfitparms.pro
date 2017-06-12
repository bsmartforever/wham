; $Id: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro opanReadFitParms,filename,fitArray = fitArray,groups = groups, error = error
; This routine reads in a parameter file that was generated in
; OPAN.PRO.
;
error = 0
if n_params() eq 0 then begin
  fitArray = (-1)
  error = 1
  return
endif

catch,theError
if theError ne 0 then begin
  catch,/cancel
  error = 1
  fitArray = (-1)
  return
endif

openr,lun,filename,/get_lun
s = ''
;readf,lun,s
ngrps = 0
nlines = 0

repeat begin

    nlines = 0

    while strpos(s,'Chi-squared:') ne 0 do begin
		readf,lun,s
		nlines = nlines + 1
        if (nlines eq 1) and (ngrps eq 0) then begin
			strout = s
        endif else begin
            strout = [strout,s]
        endelse
    endwhile

    if ngrps eq 0 then grpArray = nlines else grpArray = [grpArray,nlines]
    ngrps = ngrps + 1
	readf,lun,s

endrep until eof(lun)
free_lun,lun

; Ok we now have all of the fit information in the form of strings
; Sort it all out

for j = 0,ngrps-1 do begin

  fitStr = {namePtr:ptr_new(/allocate_heap),  $
            parmPtr:ptr_new(/allocate_heap),  $
            nparmPtr:ptr_new(/allocate_heap), $
            exprPtr:ptr_new(/allocate_heap) }

  if j eq 0 then fitArray = fitStr else fitArray = [fitArray,fitStr]
  parmFlag = 0
  group = j
  start = group*grpArray[group]
  finish = (group+1)*grpArray[group]-1
  thisStrout = strout[start:finish]

  colPos = strpos(thisStrout[1],':')
  grpId = fix(strmid(thisStrout[1],colPos+1))

  ; Determine how many curves there are...
  cI = where(thisStrout eq '-----------------',ncurves)
  nameIndex = cI+1
  names = strarr(ncurves)
  nparms = intarr(ncurves)
  exprArray = strarr(ncurves)
  ; What are the names of the individual curves?  What are the parameters
  ; in each of the curves?
  for inames = 0,ncurves-1 do begin

    nPos = strpos(thisStrout[nameIndex[inames]],':')
    names[inames] = strtrim('PAN_'+strtrim(strmid(thisStrout[nameIndex[inames]],nPos+1),2),2)
    if strupcase(names[inames]) eq 'PAN_USERFUNCTION' then begin
      exprArray[inames] = thisStrout[nameIndex[inames]+1]
      n1 = nameIndex[inames]+2
    endif else begin
      n1 = nameIndex[inames]+1		; where the parameters begin
    endelse
      if inames ne ncurves-1 then begin
        n2 = nameIndex[inames+1]-2	; where the parameters end
      endif else begin
        n2 = (finish-start) - 2		; where the parameters end
      endelse
      nparms[inames] = n2-n1+1

      for jp = 0,nparms[inames]-1 do begin
        len = strlen(thisStrout[n1+jp])
        col1Pos = strpos(thisStrout[n1+jp],': ')
        col2Pos = col1Pos + strpos(thisStrout[n1+jp],': ',col1Pos+1)
        addPos = strpos(thisStrout[n1+jp],'+')
        curParms = float(strmid(thisStrout[n1+jp],col2Pos-1,addPos-col2Pos))

        if (parmFlag eq 0) then begin
          outParms = curParms
          parmFlag = 1
        endif else begin
          outParms = [outParms,curParms]
        endelse
      endfor
  endfor

  *(fitArray[j]).namePtr = names
  *(fitArray[j]).parmPtr = outParms
  *(fitArray[j]).nparmPtr = nparms
  *(fitArray[j]).exprPtr = exprArray
  if j eq 0 then groups = grpId else groups = [groups,grpId]
endfor


; Clean up the pointers
;for j = 0,ngrps-1 do begin
;  ptr_free,(fitArray[j]).namePtr
;  ptr_free,(fitArray[j]).parmPtr
;  ptr_free,(fitArray[j]).nparmPtr
;endfor

return
end