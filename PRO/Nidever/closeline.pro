function closeline,line,arr,ind=ind,below=below

; This function treats arr as an array of points
; and tries to find the point where it crosses
; an horizontal line at "line".

undefine,ind

nline = n_elements(line)
narr = n_elements(arr)

if nline eq 0 or narr eq 0 then begin
  print,'Syntax - gd = closeline(line,arr,ind=ind,below=below)
  return,-1
end

gup = where(arr ge line,ngup)  ; elements above line
gdn = where(arr le line,ngdn)  ; elements below line

if ngup eq 0 or ngdn eq 0 then goto,BOMB

if not keyword_set(below) then begin
  dum = nonuniq(gup,gdn-1,dbl=ind1)
  dum = nonuniq(gup,gdn+1,dbl=ind2)
endif else begin
  dum = nonuniq(gdn,gup-1,dbl=ind1)
  dum = nonuniq(gdn,gup+1,dbl=ind2)
endelse

if n_elements(ind) gt 0 then undefine,ind

if ind1(0) ne -1 then push,ind,ind1
if ind2(0) ne -1 then push,ind,ind2

BOMB:

if n_elements(ind) eq 0 then ind=-1
if ind(0) ne -1 then y = arr(ind) else y=-1

if y(0) eq -1 then begin
  ;print,'ARRAY does not cross ',strtrim(line,2)
end

;stop

return,y

end
