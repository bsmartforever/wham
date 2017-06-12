; ilines.dat is the line list. 
; Note when gam is not known it is either 1e8 or 1e7
; for H2 gam is not gamma but log(lam*fval)

pro ireadlinelist

readcol,'ilines.dat',ion,wavc,gam,fval,format='a,f,f,f'

save,file='ilines.save',ion,wavc,fval,gam

end
