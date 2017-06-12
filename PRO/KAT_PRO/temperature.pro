function temperature,Ha=Ha,err_Ha=err_Ha,SII=SII,err_SII=err_SII,output=output,help=help

; Purpose: To calculate the electron temperature using 
; the H-alpha and [SII] line widths. 

If keyword_set(help) then begin
   print,' '
   print,"Input Ha and [SII] widths in km/s to calculate the temperature and non-thermal width contribution (vp)"
   print,"result=temperature(Ha=Ha,err_Ha=err_Ha,SII=SII,err_SII=err_SII,output=output,help=help)"
   print,"Output is a structure with units of km/s or 10^4 K, depending on the quantity"
   print," "
   return,0;
endif

If NOT keyword_set(Ha) then begin
   print,"*** Must define Ha ***"
   return,0 ;
endif

elements=n_elements(Ha)

If keyword_set(SII) then begin
   if ((n_elements(SII) ne elements)) then begin
      print,"*** All quantities must have an equal number of elements ***"
   return,0 ;
   endif 
endif

If keyword_set(err_Ha) and keyword_set(SII) and NOT keyword_set(err_SII) then begin
      print,"*** Please define uncertainties for both H-alpha and [SII] widths  ***"
      return,0 ;
endif

If keyword_set(err_ha) and keyword_set(SII) and keyword_set(err_SII) then begin
   if ((n_elements(err_Ha) ne elements) and (n_elements(SII) ne elements) and (n_elements(err_SII) ne elements)) then begin
      print,"*** All quantities must have an equal number of elements ***"
      return,0 ;
   endif 
endif

if NOT keyword_set(err_Ha) then err_Ha=Ha*0
if NOT keyword_set(SII) then SII=Ha*0
if NOT keyword_set(err_SII) then err_SII=Ha*0

If elements eq 1 then begin
   Ha=[Ha]
   err_Ha=[err_Ha]
   SII=[SII]
   err_SII=[err_SII]
endif

junk=where(SII ge Ha,bad_count)
if bad_count ne 0 then begin
   print,'*** Invalid. [SII] width must be less than H-alpha width ***'
   return,0;
endif

vp=((SII/1.64)^2.0-(Ha/9.28)^2.0+0.372)^0.5
err_vp1=0.0372*(SII*err_SII)^2.0/(SII^2.0-0.031*(Ha^2.0-32.0))
err_vp2=-0.012*(Ha*err_Ha)^2.0/(Ha^2.0-32.0*(SII^2.0+1.0))
err_vp=sqrt(err_vp1+err_vp2)*vp ;each term has already been squared

junk=WHERE(FINITE(vp, /NAN, SIGN=-1),bad)
if bad ne 0 then vp(WHERE(FINITE(vp, /NAN, SIGN=-1)))=0.0
junk=WHERE(FINITE(err_vp, /NAN, SIGN=-1),bad)
if bad ne 0 then err_vp(WHERE(FINITE(err_vp, /NAN, SIGN=-1)))=0.0

T=(Ha/21.1)^2.0-(SII/21.1)^2.0-0.072
err_T=sqrt((0.002255*2.0*Ha*err_Ha)^2.0+(0.002273*2.0*SII*err_SII)^2.0)*T

if keyword_set(output) then begin
   print,"Ha width: ",Ha,format='(A-20,f8.2)'
   print,"Ha width error: ",err_Ha,format='(A-20,f8.2)'
   print,"SII width: ",SII,format='(A-20,f8.2)'
   print,"SII width error: ",err_SII,format='(A-20,f8.2)'
   print,"vp: ",vp,format='(A-20,f8.2)'
   print,"vp error: ",err_vp,format='(A-20,f8.2)'
   print,"T: ",T,format='(A-20,f8.2)'
   print,"T error: ",err_T,format='(A-20,f8.2)'
   ;help,Ha,err_Ha,SII,err_SII,vp,err_vp,T,err_T,format='(A-20,f8.2)'
endif

values=replicate({Ha:Ha,err_Ha:err_Ha,SII:SII,err_SII:err_SII,T:T,err_T:err_T,vp:vp,err_vp:err_vp},1)

   return,values;

end