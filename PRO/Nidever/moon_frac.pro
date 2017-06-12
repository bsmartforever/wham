pro moon_frac,month1,day1,year1,month2,day2,year2,stp=stp

; +
; Calculate the moon illumination fraction over a long period of time
;
; INPUTS:
;  month1  The starting month.  The current date by default
;  day1    The starting day
;  year1   The starting year
;  month2  The ending month.   Dec 31st of this year by default.
;  day2    The ending day
;  year2   The ending year
;
; OUTPUTS:
;  A plot of the moon illumination fraction
; 
; Written by D.Nidever October 2006
;-

; No parameters input
if n_params() eq 0 then begin
  print,'Syntax - moon_frac,month1,day1,year1,month2,day2,year2'

  ; Using current date
  jd1 = systime(/julian)
  caldat,jd1,month1,day1,year1,hour1

  ; Using Dec 31st for ending date
  month2 = 12
  day2 = 31
  year2 = year1
endif

; Only first date input
if n_elements(month2) eq 0 then begin
  month2 = 12
  day2 = 31
  year2 = year1
endif

; Julian Date at midnight
jd1 = julday(month1,day1,year1,24,0,0)      ; JD at midnight
jd2 = julday(month2,day2,year2,24,0,0)      ; JD at midnight

n = long(jd2-jd1)
jdarr = scale_vector(dindgen(n),jd1,jd2)

sunpos,jdarr,sunra,sundec   ; in degrees

; GETTING MOON INFO, position, rise and set times
moonpos,jdarr,moonra,moondec   ; in degrees

;; THIS MOON POSITION DOES NOT AGREE WITH IRAF'S AIRCHART
;; I THINK IRAF IS WRONG.  I CHECKED AGAINST THE CTIO ENVIRONMENTAL WEBSITE:
;; http://www.ctio.noao.edu/environ/environ.html
;
;
; CALCULATE MOON ILLUMINATION
; using the distance b/w the sun and moon on the sky
; This angle will also be the angle of the terminator
; angle = 0   -> illfrac = 0.0
; angle = 90  -> illfrac = 0.5
; angle = 180 -> illfrac = 1.0
angdist = dblarr(n)
for i=0,n-1 do angdist(i) = sphtrigdist(moonra(i),moondec(i),sunra(i),sundec(i))

bd = where(angdist gt 180,nbd)
if nbd gt 0 then angdist(bd) = 360-angdist(bd)
;if angdist gt 180 then angdist=360-angdist
illfrac = angdist/180.
strillfrac = strmid(strtrim(illfrac,2),0,4)

; THIS ILLUMINATION FRACTION DOES NOT AGREE WITH THE CTIO ENVIRONMENTAL WEBSITE
; BUT IT CHECKS OUT WITH THE USNO WEBSITE:
; http://aa.usno.navy.mil/data/docs/MoonFraction.html


jd0 = julday(1,1,year1,24,0,0)  ; January 1st of starting year


; Plotting
xr = minmax(jdarr-jd0)
yr = [0.0,1.0]
xtit = 'Month'
ytit = 'Moon Illumination Fraction'
stryear = strtrim(long(year1),2)
if year2 ne year1 then stryear = stryear+'/'+strtrim(long(year2),2)
tit = 'Moon Illumination Fraction for '+stryear

plot,jdarr-jd0,strillfrac,xr=xr,yr=yr,xtickunits='Month',xs=1,ys=1,xtit=xtit,ytit=ytit,$
     tit=tit,xminor=15
;,xs=9,xminor=2,xtickinterval=1,$ ; xticks=12
;     xtit=xtit,ytit=ytit,charsize=charsize,position=pos
oplot,xr,[0.70,0.70],linestyle=2
oplot,xr,[0.30,0.30],linestyle=2
xyouts,mean(xr),0.85,'BRIGHT TIME',align=0.5
xyouts,mean(xr),0.5,'GREY TIME',align=0.5
xyouts,mean(xr),0.15,'DARK TIME',align=0.5

if keyword_set(stp) then stop

end
