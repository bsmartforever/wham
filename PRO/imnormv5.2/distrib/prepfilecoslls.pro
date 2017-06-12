pro prepfilecoslls, z, outdir

; create imnorm and save file

red = string(z)


spawn,'mkdir '+string(outdir)
file = file_search('*t.save')
;restore,file(1)
restore,file

err = sigma_up

readcol,'$HOME/PRO/imnormv5.2/lines.dat',elem,wn,w,gam,fval,format='A,A,D,D,D'


for i = 0,n_elements(wn) -1  do begin  ;n_elements(wc) -1
wc = w(i)
el = elem(i)
wni = wn(i)
fv = fval(i)
gv = gam(i)

if (wc*(1+z) ge min(wave) and wc*(1+z) le max(wave)) then begin
	
vel = ((wave - wc)/wc * 2.9979e5 -  2.9979e5 *z )/(1.+z)
 
index = where(vel ge -2000 and vel le 2000,ct)
if ct ne 0 then begin
v= vel[index]
f = flux[index]
ef = err[index]
save,file=el+wni+'o.save',v,f,ef,el,wni,wc,fv,gv
imsave,el+wni,v,f,'COS',' ',el,wc,-3,0,[-1,0,0]
spawn, 'mv *.dat '+string(outdir)
spawn, 'mv *o.save '+string(outdir)
endif 

endif

endfor

end
