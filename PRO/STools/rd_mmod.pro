pro rd_mmod,teff,logg,feh,model,header,tail

;
; RD_MMOD
;
; Extracts a model atmosphere from the MARCS grid and
; stores it into the (7,ntau) array "model"
; MARCS models are "standard composition"
;
; IR - Carnegie'10
;

if feh ge 0 then sign_feh='+' else sign_feh='-'
if feh ge +0.00 then alfa='+0.00'
if feh eq -0.25 then alfa='+0.10'
if feh eq -0.50 then alfa='+0.20'
if feh eq -0.75 then alfa='+0.30'
if feh le -1.00 then alfa='+0.40'

path='$WORK/marcs/'
if logg lt 3.0 then begin
   tt='s'   ;spherical model
   mm='1.0' ; mass
endif else begin
   tt='p'   ;plane-parallel
   mm='0.0'
endelse
root=tt+string(teff,format=('(I4)'))+$
     '_g+'+string(logg,format=('(F3.1)'))+$
     '_m'+mm+'_t02_st_z'+sign_feh+string(abs(feh),format=('(F4.2)'))+$
     '_a'+alfa+'_c+0.00_n+0.00_o'+alfa+'_r+0.00_s+0.00'

xmod=path+root+'.mod'
xkrz=path+root+'.krz'

model=fltarr(7,56)
close,1 & openr,1,xmod
a=strarr(25) & readf,1,a
a=fltarr(9,56) & readf,1,a
model(1,*)=a(4,*) & model(2,*)=a(6,*)
model(6,*)=a(2,*) ; log tau5000 is on last column
a='' & readf,1,a
a=fltarr(8,56) & readf,1,a
model(4,*)=a(2,*) & model(0,*)=a(7,*)
close,1 & openr,1,xkrz
a=strarr(3) & readf,1,a
a=fltarr(10,10) & readf,1,a
close,5 & openw,5,'rd_mmod.tmp'
printf,5,'TEFF',teff,'GRAVITY',logg,'LTE',format=('(A4,F8.0,2X,A7,F8.5,1X,A3)')
printf,5,'TITLE  [',feh,'] xxx VTURB=2 xxx MARCS',format=('(A8,F5.2,A23)')
printf,5,' OPACITY xxx'
printf,5,' CONVECTION xxx'
printf,5,'ABUNDANCE SCALE',10^feh,'ABUNDANCE CHANGE 1',a(0,0),'2 ',a(1,0),$
       format=('(A15,F10.5,1X,A18,F8.5,A2,F8.5)')
printf,5,' ABUNDANCE CHANGE',' 3',a(2,0),' 4',a(3,0),' 5',a(4,0),' 6',a(5,0),' 7',a(6,0),' 8',a(7,0),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE',' 9',a(8,0),'10',a(9,0),'11',a(0,1),'12',a(1,1),'13',a(2,1),'14',a(3,1),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','15',a(4,1),'16',a(5,1),'17',a(6,1),'18',a(7,1),'19',a(8,1),'20',a(9,1),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','21',a(0,2),'22',a(1,2),'23',a(2,2),'24',a(3,2),'25',a(4,2),'26',a(5,2),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','27',a(6,2),'28',a(7,2),'29',a(8,2),'30',a(9,2),'31',a(0,3),'32',a(1,3),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','33',a(2,3),'34',a(3,3),'35',a(4,3),'36',a(5,3),'37',a(6,3),'38',a(7,3),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','39',a(8,3),'40',a(9,3),'41',a(0,4),'42',a(1,4),'43',a(2,4),'44',a(3,4),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','45',a(4,4),'46',a(5,4),'47',a(6,4),'48',a(7,4),'49',a(8,4),'50',a(9,4),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','51',a(0,5),'52',a(1,5),'53',a(2,5),'54',a(3,5),'55',a(4,5),'56',a(5,5),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','57',a(6,5),'58',a(7,5),'59',a(8,5),'60',a(9,5),'61',a(0,6),'62',a(1,6),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','63',a(2,6),'64',a(3,6),'65',a(4,6),'66',a(5,6),'67',a(6,6),'68',a(7,6),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','69',a(8,6),'70',a(9,6),'71',a(0,7),'72',a(1,7),'73',a(2,7),'74',a(3,7),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','75',a(4,7),'76',a(5,7),'77',a(6,7),'78',a(7,7),'79',a(8,7),'80',a(9,7),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','81',a(0,8),'82',a(1,8),'83',a(2,8),'84',a(3,8),'85',a(4,8),'86',a(5,8),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','87',a(6,8),'88',a(7,8),'89',a(8,8),'90',a(9,8),'91',a(0,9),'92',a(1,9),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','93',a(2,9),'94',a(3,9),'95',a(4,9),'96',a(5,9),'97',a(6,9),'98',a(7,9),$
       format=('(A17,6(1X,A2,F7.2))')
printf,5,' ABUNDANCE CHANGE','99',a(8,9),format=('(A17,1(1X,A2,F7.2))')
printf,5,'READ DECK6 56 RHOX,T,P,XNE,ABROSS,ACCRAD,VTURB, FLXCNV,VCONV,VELSND'
if logg lt 3.0 then begin
   a=fltarr(6,56) & readf,1,a
endif else begin
   a=fltarr(5,56) & readf,1,a
endelse
model(3,*)=a(2,*)
printf,5,'PRADK xxx'
printf,5,'BEGIN xxx'
close,5
header=strarr(23) & tail=strarr(2)
close,1 & openr,1,'rd_mmod.tmp'
readf,1,header & readf,1,tail
close,1
spawn,"rm rd_mmod.tmp"

end
