function find_ions_order, variable, ions=ions, wave=wave, standard=standard, noairglow=noairglow, low=low, high=high
;
; Purpose - To sort through active spectra structures and extract the indices that matchs 
;           the ordering of the inputed ions and wave input. This is to make plotting easier. 
;            This program is similar to find_ions.pro, but that program only locates them 
;            and does not place them in a desired order. 
;
;           This is useful because read_absorp_str.pro reads data and attribute spectra, but 
;           doesn't return the names of those new structure variables in any particular order. 
;           Find_ions_order.pro can take the output of read_absorp_str.pro and order 
;           the structure names. 
;
; variable - String array containing the names of active spectral structure variables. 
;            e.g., ['HD33133_OI1302im', 'HD33133_CII1334im']
;            IDL> help,HD33133_OI1302im,/str
;            ** Structure <285c608>, 23 tags, length=26792, data length=26786, refs=1:
;              NAME            STRING    '/Users/Kat/COS/LMC_star_quasar/COS-LMC/hd33133/OI1302im'
;              VEL             DOUBLE    Array[498]
;              WAVE            DOUBLE    Array[498]
;              FLUX            DOUBLE    Array[498]
;              NORM            DOUBLE    Array[498]
;              COLUMN          DOUBLE    Array[498]
;              FVAL            FLOAT         0.0488700
;              INSTRUMENT      STRING    'COS'
;              ION             STRING    'OI'
;              WAVC            FLOAT           1302.17
;              CONTINUUM       FLOAT     Array[498]
;              LBAR            FLOAT     Array[498]
;              UBAR            FLOAT     Array[498]
;              XARRAY          FLOAT     Array[96]
;              YARRAY          FLOAT     Array[96]
;              STORE           FLOAT     Array[2, 3]
;              COEFF           FLOAT     Array[3]
;              SIGMA           FLOAT       6.20832e-14
;              BSIGMA          FLOAT           0.00000
;              COMMENTS        STRING    ''
;              ORDER           INT             -3
;              RFLAGS          INT              0
;              UPDATES         INT              1
;
; ions     - String array of ion names to search for, e.g., ['OI','CII']
; wave     - Array of wavelengths, e.g., wave=[1320.17,1334.2,1250.58,1253.81]
;            
; Number of elements in ions and wave must be equal and must agree with each other, 
; such that a given ions corresponds to a given wave. e.g., 'OI' -> 1302.
;
; standard - Allows user to find the order of variable using a preset ions and wave lists.
; noairglow - same as standard, but excludes the OI1302 and SiII1304 ions, contaminated by airglow.
; low      - Same as standard, but only searches for the low ions.
; high     - Same as low, but for the high ions. 
;           
;        
;           e.g., HD33133=read_absorp_str('HD33133',dir='hd33133')
;                 HD33133_ordered=HD33133[find_ions_order(HD33133,/low)]
;                 multiplot_spectra,HD33133_ordered,dim=[3,3],xrange=[-200,600]
;
;           IDL> print,HD33133
;           HD33133_AlII1670im HD33133_CII1334im HD33133_CIIs1335im 
;           HD33133_CIV1548im HD33133_CIV1550im HD33133_FeII1608im 
;           HD33133_FeII1611im HD33133_NV1238im HD33133_NV1242im 
;           HD33133_NiII1317im HD33133_NiII1370im HD33133_OI1302im 
;           HD33133_SII1250im, HD33133_SII1253im HD33133_SII1259im 
;           HD33133_SiII1190im HD33133_SiII1193im HD33133_SiII1260im 
;           HD33133_SiII1304im HD33133_SiII1526im HD33133_SiIII1206im
;           HD33133_SiIV1393im HD33133_SiIV1402im
;           
;           Take HD33133 and look for only the low ions and order them:
;           HD33133_ordered=HD33133[find_ions_order(HD33133,/low)]
;           print,HD33133_ordered
;           HD33133_OI1302im HD33133_CII1334im HD33133_SII1250im 
;           HD33133_SII1253im HD33133_SII1259im HD33133_SiII1193im 
;           HD33133_SiII1260im HD33133_SiII1304im HD33133_SiII1526im
;
;        
; Created by Dr. Kat Barger 04/2013         
;           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if keyword_set(standard) then begin
   ions=['OI','CII','SII','SII','SII','SiII','SiII','SiII','SiII','SiIII',$
         'AlII','FeII','NiII','NiII','SiIV','SiIV','CIV','CIV','NV','NV']
   wave=[1302,1334,1250,1253,1259,1193,1260,1304,1526,1206,$
         1670,1608,1317,1370,1393,1403,1548,1550,1238,1242]
endif

if keyword_set(noairglow) then begin
   ions=['CII','SII','SII','SII','SiII','SiII','SiII','SiIII',$
         'AlII','FeII','NiII','NiII','SiIV','SiIV','CIV','CIV','NV','NV']
   wave=[1334,1250,1253,1259,1193,1260,1526,1206,$
         1670,1608,1317,1370,1393,1403,1548,1550,1238,1242]
endif

if keyword_set(low) then begin
   ions=['OI','CII','SII','SII','SII','SiII','SiII','SiII','SiII']
   wave=[1302,1334,1250,1253,1259,1193,1260,1304,1526]
endif else low = 0

if keyword_set(high) then begin
   ions=['SiIII',$
         'AlII','FeII','NiII','NiII','SiIV','SiIV','CIV','CIV','NV','NV']
   wave=[1206,$
         1670,1608,1317,1370,1393,1403,1548,1550,1238,1242]
endif else high =0

if keyword_set(ions) then ions=strarr(n_elements(ions))+ions $
   else begin
     print,'*** Need to define your ions ***'
     return,0
   endelse
if keyword_set(wave) then wave=fltarr(n_elements(wave))+float(wave)  $
   else begin
     print,'*** Need to define your wavelengths ***'
     return,0
   endelse

if n_elements(wave) ne n_elements(ions) then begin
     print,'*** Number of elements in ions and wave must be equal ***'
     return,0
endif

num=n_elements(ions)
low=0
high=0
flag=0

indices=intarr(num)
for i=0, num-1 do begin
    indices[i]=find_ions(variable, ions=ions[i], wave=wave[i], low=low, high=high, flag=flag)
endfor

return, indices;

end

