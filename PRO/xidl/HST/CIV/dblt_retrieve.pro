;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dblt_find.pro                
; Author: Kathy Cooksey                      Date: 4 Nov 2005
; Project: 
; Description: 
; Input: 
; Optional:
; Output: 
; Example:
; History:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function dblt_retrieve,dblt_name

name = strlowcase(strtrim(dblt_name,2))

neviii = { doubletstrct }
neviii.ion = 'NeVIII'
neviii.wvI = 770.409
neviii.fI = 0.1030
neviii.wvII = 780.324
neviii.fII = 0.0505

ovi = { doubletstrct }
ovi.ion = 'OVI'
ovi.wvI = 1031.9261
ovi.fI = 0.13290
ovi.wvII = 1037.6167
ovi.fII = 0.06609 

nv = { doubletstrct }
nv.ion = 'NV'
nv.wvI = 1238.821
nv.fI = 0.1570
nv.wvII = 1242.804
nv.fII = 0.078230 

siiv = { doubletstrct }
siiv.ion = 'SiIV'
siiv.wvI = 1393.755 
siiv.fI = 0.5280
siiv.wvII = 1402.770
siiv.fII = 0.262 


civ = { doubletstrct }
civ.ion = 'CIV'
civ.wvI = 1548.195
civ.fI = 0.19080
civ.wvII = 1550.770
civ.fII = 0.095220 

lya = { doubletstrct }
if name eq 'lya' then lya.ion = 'Lya' $
else lya.ion = 'HI'
lya.wvI = 1215.6701
lya.fI = 0.41640 
lya.wvII = 1025.7223
lya.fII = 0.079120

ciii = { doubletstrct }
ciii.ion = 'CIII'
ciii.wvII = 1215.6701
ciii.fII = 0.41640
ciii.wvI = 977.020
ciii.fI = 0.7620

case name of 
   'neviii': return,neviii
    'ovi': return,ovi
    'nv': return,nv
    'siiv': return,siiv
    'civ': return,civ
    'lya': return,lya
    'hi': return,lya
    'ciii': return, ciii
    else: return,{ doubletstrct }
endcase

end
