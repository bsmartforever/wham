pro phot_overlap,str1,str2,outstr,dcr=dcr,stp=stp,silent=silent,$
    s1=s1,s2=s2

;+
;
; This combines overlapping photometry
;
; INPUTS:
;  str1   First photometry structure
;  str2   Second photometry structure
;  =dcr   Critical matchup radius.  Stars closer than are combined
;  /stp   Stop at end of program
;  /silent Don't print anything
; 
; OUTPUTS:
;  outstr  The final combined photometry structure
;
; USAGE:
;  IDL>phot_overlap,str1,str2,outstr
;
; By D.Nidever Jan 2007
;-


nstr1 = n_elements(str1)
nstr2 = n_elements(str2)

; Not enough inputs
if nstr1 eq 0 or nstr2 eq 0 then begin
  print,'Syntax - phot_overlap,str1,str2,outstr,stp=stp'
  return
endif

; Checking that they are structures
type1 = size(str1,/type)
type2 = size(str2,/type)
if type1 ne 8 then begin
  print,'str1 IS NOT A STRUCTURE'
  return
endif
if type2 ne 8 then begin
  print,'str2 IS NOT A STRUCTURE'
  return
endif

; Checking that the structures are the same
tags1 = tag_names(str1)
ntags1 = n_elements(tags1)
tags2 = tag_names(str2)
ntags2 = n_elements(tags2)
if ntags1 ne ntags2 then begin
  print,'DIFFERENT DATA STRUCTURES'
  return
endif

alltags = [tags1,tags2]
ui = uniq(alltags,sort(alltags))
nui = n_elements(ui)
if nui ne ntags1 then begin
  print,'DIFFERENT DATA STRUCTURES'
  return
endif


;#####################################
;# STARTING THE COMBINATION PROCESS

; First find the overlap region
rar1 = minmax(str1.ra)
decr1 = minmax(str1.dec)
rar2 = minmax(str2.ra)
decr2 = minmax(str2.dec)

ind1 = where(str1.ra ge rar2[0] and str1.ra le rar2[1] and $
             str1.dec ge decr2[0] and str1.dec le decr2[1],nind1)
ind2 = where(str2.ra ge rar1[0] and str2.ra le rar1[1] and $
             str2.dec ge decr1[0] and str2.dec le decr1[1],nind2)

; No overlap, just concatenate
if nind1 eq 0 or nind2 eq 0 then begin
  outstr = [str1,str2]
  if not keyword_set(silent) then print,'NO OVERLAP'
  return
endif


; Find the matches
if n_elements(dcr) eq 0 then dcr=0.5
;SRCOR,str1[ind1].ra,str1[ind1].dec,str2[ind2].ra,str2[ind2].dec,dcr,mind1,mind2,opt=1,sph=2
SRCMATCH,str1[ind1].ra,str1[ind1].dec,str2[ind2].ra,str2[ind2].dec,dcr,mind1,mind2,/sph

; No matches
if mind1[0] eq -1 then begin
  outstr = [str1,str2]
  if not keyword_set(silent) then print,'NO MATCHES'
  return
endif

; Matched indices
match1 = ind1[mind1]
match2 = ind2[mind2]

; Non-matched indices
left1 = lindgen(nstr1)
remove,match1,left1
left2 = lindgen(nstr2)
remove,match2,left2

; Combine the non-matched elements
outstr = [str1[left1],str2[left2]]

nmatch = n_elements(match1)

filters = ['I','M','D']
nfilters = n_elements(filters)
magindarr = lonarr(nfilters)

for i=0,nfilters-1 do begin
  dum = where(tags1 eq filters[i],nmagind)
  magindarr[i] = dum[0]  ; take the first one
end

if not keyword_set(silent) then print,'Combining ',strtrim(nmatch,2),' Stars'

; Combine the data

; Structures of the two observations
s1 = str1[match1]
s2 = str2[match2]

; Starting the output structure
out = s1

; Leave from the first structure
; ID, X, Y, CHI, SHARP, RA0, DEC0

; Average the coordinates
out.ra = (s1.ra+s2.ra)*0.5
out.dec = (s1.dec+s2.dec)*0.5

; Combine the magnitudes properly, convert to flux, use the errors as weight
; check magma for example
;filters = ['I','M','D']
;nfilters = n_elements(filters)

; Looping through the filters
for j=0,nfilters-1 do begin

  ; Filter index
  ;magind = where(tags1 eq filters[j],nmagind)
  ;magind = magind[0]    ; take the first one
  magind = magindarr[j]
  errind = magind+1    ; assume the error is the next column

  ; We've got this one
  if nmagind gt 0 then begin

    bothbad = where(s1.(magind) gt 50. and s2.(magind) gt 50.,nbothbad)
    onegood1 = where(s1.(magind) lt 50. and s2.(magind) gt 50.,nonegood1)
    onegood2 = where(s1.(magind) gt 50. and s2.(magind) lt 50.,nonegood2)
    bothgood = where(s1.(magind) lt 50. and s2.(magind) lt 50.,nbothgood)


    ; Both bad
    if (nbothbad gt 0) then begin
      out[bothbad].(magind) = 99.9999
      out[bothbad].(errind) = 9.9999
    endif

    ; Only one good (first one)
    if (nonegood1 gt 0) then begin
      out[onegood1].(magind) = s1[onegood1].(magind)
      out[onegood1].(errind) = s1[onegood1].(errind)
    endif

    ; Only one good (second one)
    if (nonegood2 gt 0) then begin
      out[onegood2].(magind) = s2[onegood2].(magind)
      out[onegood2].(errind) = s2[onegood2].(errind)
    endif

    ; Both good
    if (nbothgood gt 0) then begin
      flux1 = 2.511864^s1[bothgood].(magind)
      flux2 = 2.511864^s2[bothgood].(magind)
      err1 = s1[bothgood].(errind)
      err2 = s2[bothgood].(errind)
      wt1 = 1.0/(err1^2.0)
      wt2 = 1.0/(err2^2.0)
      totalwt = wt1 + wt2
      totalflux = (flux1*wt1 + flux2*wt2)
      totalerr = (err1^2.0)*wt1 + (err2^2.0)*wt2
      newflux = totalflux/totalwt
      newmag = 2.50*alog10(newflux)
      newerr = sqrt(1.0/totalwt)

      out[bothgood].(magind) = newmag
      out[bothgood].(errind) = newerr
    endif


  endif else begin ; we've got this filter
    print,'NO ',filters[j],' FILTER'
  endelse 
end  ; filter loop


; Add to final output structure
outstr = [outstr,out]

if keyword_set(stp) then stop

end
