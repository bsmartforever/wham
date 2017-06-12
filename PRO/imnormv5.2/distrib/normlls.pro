PRO normlls, v, f, err, SHIFT=shift, CUT=cut_off

   
 IF n_params(0) EQ 0 THEN BEGIN
   
   print
   print
   print,  '  Calling Sequence:'
   print,  '  normerrp, specname, v, f, err, SHIFT=shift, CUT=cut_off'
   print
   retall   
END  

   IF NOT keyword_set(cut_off) THEN cut_off = [-400, 400]
   IF NOT keyword_set(shift) THEN shift = 0.
 
 readcol,'fileim.dat',specname,format='A'  
   
   specname2 =  string(specname);+'im'

for i = 0, n_elements(specname) - 1 do begin 

	specname1 = specname2(i)
      restore,specname(i)+'o.save'
   imread,specname1,vel,flux,obj,comm,ion,wavc,mapi,ord,r_flags
   imattr,specname1,lbar,ubar,xfit,yfit,store,coeff,sigma,bsigma,cofl,ebfl,ftfl

   x =  max(abs(store))
   fit =  polyleg(vel/x, coeff)  
   snr = avg(fit)/sigma
   errhigh = ubar/fit
   errlow  = lbar/fit
   
     ;; Calculate error spectrum assuming Poisson statistics unless /FPN
   ;; is requested:
   IF keyword_set(fixedpattern) $
   THEN sigma0 = flux*0.+snr ELSE $
      sigma0 = sqrt((flux/fit)*snr^2.0)/snr^2.0
   
 
   nanindex = where((finite(sigma0) eq 0), countnan)
   IF countnan NE 0 THEN sigma0(nanindex) = 1.0e-5
   normal  = flux/fit
   errhigh = ubar/fit
   errlow  = lbar/fit 
   errout =  sqrt(((errhigh-errlow)/2)^2.0 + sigma0^2.0)
   errf = errout
   f = normal
	   
 	vel = vel + shift
	v = vel


   index = where(v gt -200 and v le 200)
   snr = avg(1/errf[index])
   imsavenorm, specname(i)+'out', v, f, errf, obj,comm,ion,wavc
   save,file=specname(i)+'out.save',v,f,errf,snr,wavc,ion,fv

 endfor
 


   return
END






