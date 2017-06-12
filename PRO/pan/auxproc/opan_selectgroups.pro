; $Id: opan_selectgroups.pro,v 1.1 2002/09/19 21:30:49 dimeo Exp $
;==========================================
function opan_selectgroups,strdets
;
; Input: 	string containing detector numbers separated either by
;        	commas or - (hyphens).  A hyphen indicates to include the
;        	two detectors and all detectors in between.  A comma simply
;        	means to append that detector to the current list.
;
; Output:	a pointer to an array containing all of the detectors to be
;			summed together.
;
; Catch errors in input string
;nerror = strmid(strdets,0)
nerror = strmid(strdets,0,1)
if (nerror[0] eq '-') or (nerror[0] eq ',') or (nerror[0] eq ' ') then begin
  void = dialog_message(parent = event.top,'Not a valid input field',$
           dialog_parent=event.top)
  return,0
endif
strdets = strcompress(strdets)
test1 = strpos(strdets,'-,')
test2 = strpos(strdets,',-')
if test1[0] ne -1 or test2[0] ne -1 then begin
  void = dialog_message(parent = event.top,'Not a valid input field',$
           dialog_parent=event.top)
  return,0
endif

; count the number of commas occurring in the string
i = 0
inum = 0
cnt = 0
commacnt = intarr(50)
while (inum ne -1) do begin
  i = strpos(strdets,',',i)
  inum = i[0]
  if (inum ne -1) then begin
    commacnt[cnt] = inum
    cnt = cnt+1
    inum = inum + 1
    i = inum
  endif
endwhile
ncommas = cnt
if ncommas gt 0 then begin
  commaPos = intarr(ncommas)
  commaPos = commacnt[0:ncommas-1]
endif
;commaPos = [-1,commaPos[0:ncommas-1]]

; if there are no commas then there are two possibilities
; either there is a single detector present.....
ndash = strpos(strdets,'-')
if (ncommas eq 0) and (ndash[0] eq -1) then begin
  detnum = fix(strdets)
  ndets = 1
  dets = detnum
endif

; or there are a sum of detectors present
if (ncommas eq 0) and (ndash[0] ne -1) then begin
  pos = strpos(strdets,'-')
  len = strlen(strdets)
  numstr1 = strmid(strdets,0,pos[0])
  numstr2 = strmid(strdets,pos[0]+1,len[0]-1)
  num1 = fix(numstr1)
  num2 = fix(numstr2)
  ndets = num2[0] - num1[0] + 1
  dets = intarr(ndets)
  dets = num1[0] + INDGEN(ndets)
endif

; are there one or more commas?
if (ncommas gt 0) then begin
  dets = intarr(100)
  totlen=strlen(strdets)		; total length of the input string
  smallLen = intarr(ncommas+1)	; array defining the length of all substrings in string
								; variable strdets
  smallLen[0] = commaPos[0]
  for i=1,ncommas-1 do begin
      smallLen[i] = commaPos[i]-commaPos[i-1]-1
  endfor
  smallLen[ncommas] = totlen-commaPos[ncommas-1]-1

  substrings = strarr(ncommas+1)	; string array representing string form of detectors to sum
  substrings[0]=strmid(strdets,0,smallLen[0])
  for i = 1,ncommas do begin
    substrings[i]=strmid(strdets,commaPos[i-1]+1,smallLen[i])
  endfor

  ; convert substrings to an array of numbers
  count = 0
  for i = 0,ncommas do begin

    dashpresent = strpos(substrings[i],'-')
    dash = dashpresent[0]

    if dash eq -1 then begin
      detnum = fix(substrings[i])
      ndets = 1
      dets[count] = detnum[0]
      count = count+1
    endif

    if dash ne -1 then begin

      pos = strpos(substrings[i],'-')
      len = strlen(substrings[i])
      numstr1 = strmid(substrings[i],0,pos[0])
      numstr2 = strmid(substrings[i],pos[0]+1,len[0]-1)
      num1 = fix(numstr1)
      num2 = fix(numstr2)
      ndets = num2[0] - num1[0] + 1
      singledets = intarr(ndets)
      singledets = num1[0] + INDGEN(ndets)
      dets[count:count+ndets-1] = singledets[0:ndets-1]
      count = count+ndets
    endif
  endfor

ndets = count
tempdets = intarr(ndets)
tempdets[0:ndets-1] = dets[0:ndets-1]
dets = intarr(ndets)
dets[0:ndets-1] = tempdets[0:ndets-1]
endif
RETURN,dets
END