pro testmoments, moment = moment
;I need to make sure my moment caluclations are not screwed up so here we go



A = CREATE_STRUCT('data', float(0), 'vel', float(0))
;make the array filled with values 1 to 1000, will emulate a window of data with velocities from 1 to 1000
pointings=REPLICATE(A,100)
for k=0,99 do begin
  pointings[k].vel=k
endfor

var =3.14
mu=50

;pointings.data=1/sqrt(2*!pi*var^2)*exp(-((pointings.vel-mu)^2)/(2*var^2))
for i=0,99 do begin
  xmu=pointings[i].vel-mu
  pointings[i].data=1/sqrt(2*!PI*var^2)*exp(-(xmu)^2/(2*var^2))
  ;pointings[i].data=exp(-(pointings[i].vel-mu)^2/(2*var^2))
  ;print, pointings[i].data
endfor
  ;zmom= intspect(pointings, 0, 99, moment=0)
  ;print, zmom

  zmom=int_tabulated(pointings.vel, pointings.data)

  print, zmom

  d = keyword_set(moment) ? (pointings.data * pointings.vel^ 1) : (pointings.data)
  fmom= int_tabulated(pointings.vel, d)/zmom
  print, fmom
  e = keyword_set(moment) ? (pointings.data * (pointings.vel - fmom )^2) : (pointings.data)
  smom=int_tabulated(pointings.vel, e)/zmom^2
  print, sqrt(smom)

  plot, pointings.vel, pointings.data

end
