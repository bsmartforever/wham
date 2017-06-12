; This is exactly like STRPLIT.PRO except that you can input
; arrays as well

function strsplitter, stringIn, pattern, _ref_extra=extra

    ;ON_ERROR, 2  ; return to caller

    nstr = n_elements(stringIn)
    ncol = 1
    outarr = strarr(ncol,nstr)


    ; Looping through the array
    for i=0.,nstr-1 do begin

      ; Splitting the string
      if n_elements(pattern) eq 0 then  out = STRTOK(stringIn[i], _STRICT_EXTRA=extra)  $
         else out = STRTOK(stringIn[i], pattern, _STRICT_EXTRA=extra)

      nout = n_elements(out)
     
      ; Do we need to make the array larger?
      if (nout gt ncol) then begin
        old = outarr
        outarr = strarr(nout,nstr)
        outarr[0,0] = old    ; fill in what we already had
        
        ncol = nout
      endif

      ; Fill in the split string
      outarr[0,i] = out

    end ; for loop

return,outarr

end
