pro writeTextGroup
filename = 'c:\dimeo\IDLROUTINES\pan\analysis\pan\grpdata.txt'

; Create some fake data
nchan = 5
x = rmd_makepoints(xlo = -2d,xhi = 2d,npts = nchan)
ux = 1+bytarr(nchan)

nq = 3

q = rmd_makepoints(xlo = 0.25,xhi = 1.75,npts = nq)
ro = 1.73
ao = (5.0+4.0*beselj(q*ro,0))/9.0
uq = 1+bytarr(nq)

area = 4500.0 & bg = 0.05*area
yy = bg   + (pan_gaussian(x,[area,0d,1.0]))#ao + $
	 		0.0*(pan_lorentzian(x,[5.0*area,-3d,2d]))#(1.0-ao) + $
	 		0.0*(pan_lorentzian(x,[5.0*area,3d,2d]))#(1.0-ao)

datmat = yy

y = dblarr(nchan,nq)
for j = 0,nq-1 do begin
  for i = 0,nchan-1 do begin
    y[i,j] = randomn(s,1,poisson = datmat[i,j])
  endfor
endfor
yerr = sqrt(y)
openw,lun,filename,/get_lun

printf,lun,'# Number of x-values'
printf,lun,strtrim(string(nchan),2)
printf,lun,'# Number of y-values'
printf,lun,strtrim(string(nq),2)
;printf,lun,'# xlabel: ','E (meV)'
;printf,lun,'# ylabel: ','Detector angle'
;printf,lun,'# zlabel: ','Intensity'
printf,lun,'# xvalues: '
for i = 0,nchan-1 do printf,lun,strtrim(string(x[i]),2)
printf,lun,'# yvalues: '
for i = 0,nq-1 do printf,lun,strtrim(string(q[i]),2)

for j = 0,nq-1 do begin
  printf,lun,'# Group: ',strtrim(string(j),2)
  for i = 0,nchan-1 do begin
    strout = strtrim(string(y[i,j]),2)+' '+strtrim(string(yerr[i,j]),2)
    printf,lun,strout
  endfor
endfor

free_lun,lun
return
end