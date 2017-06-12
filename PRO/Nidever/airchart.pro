pro airchart,obsname,month,day,year,file=file,ra=ra,dec=dec,equinox=equinox,$
             name=name,tzone=tzone,save=save,psfile=psfile,color=color,stp=stp,$
             amout=amout,timeout=timeout,lstout=lstout,noplot=noplot

;+
; This is an IDL version of the IRAF airchart program
; that creates a plot of the airmass of an object over a night
; You can input the coordinates from a file or from the command line
;
; CALLING SEQUENCE:
;   airchart,obsname,month,day,year,file=file,ra=ra,dec=dec,
;                 equinox=equinox,name=name,tzone=tzone,save=save,
;                 psfile=psfile,color=color,stp=stp
;
; INPUT:
;  obsname       Name of the observatory
;  month         Month of the year (1-12)
;  day           Day of the month (1-31)
;  year          Year
;  =file         A filename from which to read the coordinates
;                in format: NAME RA DEC EQUINOX
;  =ra           Right ascension of the object (in sexigesimal or float format)
;                IN HOURS
;  =dec          Declination of the object (in sexigesimal or float format)
;                IN DEGREES
;  =equinox      The equinox of the coordinates
;  =name         The name of the object (optional)
;  =tzone        The time zone   (Local time = UT-tzone)
;  /save         Save the airmass chart to a postscript file
;  =psfile       The name of the postscript file (default psfile='airchart')
;  /color        In color
;  /noplot       Don't plot
;  /stp          Stop at the end
;
; OUTPUT:
;  Airmass chart
;  =amout        The 2D airmass array
;  =timeout      The time array
;  =lstout       The LST array
;
; EXAMPLE:
;   airchart,'ctio',9,30,2006,ra='03:20:41.201',dec='-75:05:15.335',equinox=2000
;   airchart,'ctio',9,30,2006,file='airchart.txt',/color,/save
;
; PROGRAMS USED:  (not in Astrolib or Mousse)
;   SEXIG2TEN.PRO
;   TEN2SEXIG.PRO
;   SIGNS.PRO
;   SCALE_VECTOR.PRO  (Fanning)
;   AIRMASS.PRO       (Buie)
;   BADPAR.PRO        (Buie)
;   HANGLE.PRO        (Buie)
;   LSIDTIME.PRO      (Buie)
;   REFRAC.PRO        (Buie)
;   AIRINDEX.PRO      (Buie)
;   SLOPE.PRO
;   SPHTRIGDIST.PRO
;   MAXLOC.PRO
;   MINLOC.PRO
;   FSC_COLOR.PRO     (Fanning)
;   PS_OPEN.PRO
;   PS_CLOSE.PRO
;
; By David Nidever   Feb.2006
;-


; TO ADD:
;   IMPROVE THE LABELING
;   COLOR-CODE THE DIFFERENT AIRMASS CURVES AND NAME LABELS

; No parameters input
if n_params() lt 4 then begin
  print,'Syntax - airchart,obsname,month,day,year,file=file,ra=ra,dec=dec,'
  print,'                  equinox=equinox,name=name,tzone=tzone,save=save,'
  print,'                  psfile=psfile,color=color,stp=stp'
  return
endif

; Default parameters
if not keyword_set(month) then begin

  ; Get current date
  caldat,systime(/julian),month,day,year
  print,"Using today's date"
endif

; No observatory input
if not keyword_set(obsname) then obsname = 'ctio'

; No coordinates input
if n_elements(ra) eq 0 or n_elements(dec) eq 0 then begin
  print,'Using LMC center coordinates'

; Stellar LMC CM position in ra,dec
  ra = ten(05,27.6,0)*15.  ; deg
  dec = ten(-69,52,0)   ; deg
  equinox = 2000.0
endif

; No equinox input
if n_elements(equinox) eq 0 then begin
  print,'No equinox input - Using J2000.0'
  equinox = ra*0.+2000.0
endif

; Making internal copies
alpha = ra
delta = dec
equi = equinox

; Checking the sizes of inputs
nra = n_elements(ra)
ndec = n_elements(dec)
nequi = n_elements(equinox)
; RA and DEC not the same size
if nra ne ndec then begin
  print,'RA and DEC arrays must have the same number of elements'
  return
endif
; Equinox is scalar
if nra gt 1 and nequi eq 1 then begin
  print,'Using Equinox=',strtrim(equinox,2),' for all stars'
  equi = ra*0. + equinox
endif 


; Julian Date at midnight
jd = julday(month,day,year,24,0,0)      ; JD at midnight

; Get Observatory information
observatory,obsname,obs
tzone = obs.tz   ; time zone
;lon_rad = obs.longitude/!radeg
;lat_rad = obs.latitude/!radeg
lon = obs.longitude
lat = obs.latitude

; Number of points for our time array
n = 1000.

; Array of JDs for the time we're interested in
;jdarr = scale_vector(dindgen(n),-0.5,0.5)+jd
jdarr = scale_vector(dindgen(n),-0.8,0.8)+jd


; Reading coordinates from the file
if n_elements(file) gt 0 then begin
  readcol,file,name,alpha,delta,equi,format='A,A,A,A',comment='#',/silent
end

nstars = n_elements(alpha)

; Initializing the airmass array
amarr = fltarr(nstars,n)

; LOOP through the stars
FOR i=0,nstars-1 DO BEGIN

  ; Getting this stars's coordinates
  sra = alpha[i]
  sdec = delta[i]
  sequinox = equi[i]

  ; Converting RA/DEC from sexigesimal to decimal
  fra = sra
  fdec = sdec
  if size(sra,/type) eq 7 then fra = sexig2ten(sra)
  if size(sdec,/type) eq 7 then fdec = sexig2ten(sdec)
  fra = fra[0]     ; make sure it's a scalar
  fdec = fdec[0]  

  ; Precessing the coordinates to date for which the
  ; airmass is desired
  fequinox = sequinox
  if size(sequinox,/type) eq 7 then fequinox = float(sequinox)
  jd0 = julday(1,1,year,24,0,0)
  equinox2 = year + (jd-jd0)/365.0

  pra = fra
  pdec = fdec
  precess,pra,pdec,fequinox,equinox2

  ; Converting to radians
  ;ra_deg = pra*15.0             ; converting RA from hours to degrees
  ;ra_rad = ra_deg/!radeg
  ;dec_rad = pdec/!radeg

  ; Making array of coordinates, must be same dimension as jdarr
  ;raarr = fltarr(n)+ra_rad
  ;decarr = fltarr(n)+dec_rad
  raarr = fltarr(n)+pra*15.0d0
  decarr = fltarr(n)+pdec

  ; Get the airmass
  ;am = airmass(jdarr,raarr,decarr,lat_rad,lon_rad,lst=lst_rad,lha=lha_rad)
  am = airmass(jdarr,raarr,decarr,lat,lon,lst=lst,lha=lha)
  lst = lst/15.0   ; convert to hours

  ; Are there some bad ones?
  bd = where(am le 0.0,nbd)
  if nbd gt 0 then am[bd]=99.99

  ; Converting
  ;lst = lst_rad*!radeg/15.     ; from radians-hours
  ;lha = lha_rad*!radeg/15.

  ; UT time and Local time
  ut = (jdarr-jd)*24.+24.
  ltime = ut-tzone    ;5.0
  xltime = -0.5+ltime/24.0  ; time in days, IDL's "hours" units

  ; Inserting into the array
  amarr(i,*) = am

END  ; star loop


; GETTING SUN INFO, rise and set times
sunpos,jd,sunra,sundec   ; in degrees
;sunra_rad = sunra/!radeg
;sundec_rad = sundec/!radeg
;sunraarr = fltarr(n)+sunra_rad
;sundecarr = fltarr(n)+sundec_rad
;sunam = airmass(jdarr,sunraarr,sundecarr,lat_rad,lon_rad,lst=sunlst,lha=sunlha_rad,alt=sunalt_rad)
;sunlha = sunlha_rad*!radeg
;sunalt = sunalt_rad *!radeg
sunraarr = fltarr(n)+sunra
sundecarr = fltarr(n)+sundec
sunam = airmass(jdarr,sunraarr,sundecarr,lat,lon,lst=sunlst,lha=sunlha,alt=sunalt)


; find where sunalt is less than -18
sundegree = 18.0    ; astronomical twilight begins at -18 degrees
sunsetind = (where(ltime gt 12 and ltime lt 24 and sunalt lt -sundegree))(0)
sunriseind = (where(ltime gt 24 and sunalt gt -sundegree))(0)

sunsettime = ltime(sunsetind) mod 24
sunrisetime = ltime(sunriseind) mod 24

strsunsettime = strmid(ten2sexig(sunsettime),0,5)
strsunrisetime = strmid(ten2sexig(sunrisetime),0,5)
if strmid(strsunrisetime,0,1) eq '0' then strsunrisetime = strmid(strsunrisetime,1,4)


; GETTING MOON INFO, position, rise and set times
moonpos,jdarr,moonra,moondec   ; in degrees
;moonra_rad = moonra/!radeg
;moondec_rad = moondec/!radeg
;moonam = airmass(jdarr,moonra_rad,moondec_rad,lat_rad,lon_rad,lst=moonlst,lha=moonlha_rad,alt=moonalt_rad)
;moonlha = moonlha_rad*!radeg
;moonalt = moonalt_rad *!radeg
moonam = airmass(jdarr,moonra,moondec,lat,lon,lst=moonlst,lha=moonlha,alt=moonalt)

; find where moonalt is lt 0.0
moondegree = 0.0
slpmoonalt = slope(moonalt)
slpmoonalt = [slpmoonalt,slpmoonalt[n-2]]
moonriseind = (where(slpmoonalt gt 0.0 and moonalt gt -moondegree))(0)
moonsetind = (where(slpmoonalt lt 0.0 and moonalt lt -moondegree))(0)

moonrisetime = ltime(moonriseind) mod 24
moonsettime = ltime(moonsetind) mod 24

strmoonrisetime = strmid(ten2sexig(moonrisetime),0,5)
strmoonsettime = strmid(ten2sexig(moonsettime),0,5)
if strmid(strmoonsettime,0,1) eq '0' then strmoonsettime = strmid(strmoonsettime,1,4)
strmoonra = strmid(ten2sexig(moonra[n/2]/15.0),0,5)
strmoondec = ten2sexig(moondec[n/2])
len = strlen(strmoondec)
strmoondec = strmid(strmoondec,0,len-7)

; THIS MOON POSITION DOES NOT AGREE WITH IRAF'S AIRCHART
; I THINK IRAF IS WRONG.  I CHECKED AGAINST THE CTIO ENVIRONMENTAL WEBSITE:
; http://www.ctio.noao.edu/environ/environ.html


; CALCULATE MOON ILLUMINATION
; using the distance b/w the sun and moon on the sky
; This angle will also be the angle of the terminator
; angle = 0   -> illfrac = 0.0
; angle = 90  -> illfrac = 0.5
; angle = 180 -> illfrac = 1.0
angdist = sphtrigdist(moonra[n/2],moondec[n/2],sunra,sundec)
if angdist gt 180 then angdist=360-angdist
illfrac = angdist/180.
strillfrac = strmid(strtrim(illfrac,2),0,4)

; THIS ILLUMINATION FRACTION DOES NOT AGREE WITH THE CTIO ENVIRONMENTAL WEBSITE
; BUT IT CHECKS OUT WITH THE USNO WEBSITE:
; http://aa.usno.navy.mil/data/docs/MoonFraction.html



; PLOTTING
; noon=0.0, midnight=0.5
xr = [sunsettime-12-0.5,sunrisetime+12+0.5]/24.0

; Getting positions for the names
xpos = fltarr(nstars)-1
ypos = fltarr(nstars)-1

; Looping through the stars
for i=0,nstars-1 do begin

  ; Getting the indices inside the plot
  gd = where(xltime ge xr[0] and xltime lt xr[1] and reform(amarr[i,*]) lt 2.4,ngd)
  ;gd = where(xltime ge 0.25 and xltime lt 0.75 and reform(amarr[i,*]) lt 2.4,ngd)

  ; It's on the plot
  if (ngd gt 0) then begin

   ; Not the first one
   if (i gt 0) then begin

     ; First try to have it not overlap in y
     gd2 = gd

     ; Looping through all stars in front of this one to check
     ; that the positions don't overlap
     for j=0,i-1 do begin
       bdx = where( abs(reform(amarr[i,gd2])-ypos[j]) lt 0.05 or xltime[gd2] gt xr[1]-0.1 ,nbdx)
       ;bdx = where( abs(reform(amarr[i,gd2])-ypos[j]) lt 0.05 or xltime[gd2] gt 0.65 ,nbdx)
     
       ; Removing all of the remaining elements
       if nbdx eq n_elements(gd2) then begin
         gd2 = -1
         break
       endif else begin
         if nbdx gt 0 then remove,bdx,gd2
       endelse
     end

     ; Getting the final position
     if (gd2[0] ne -1) then begin
       ; even, go high
       if (i mod 2 eq 0) then begin
         ind = first_el(maxloc(amarr[i,gd2]))
       ; odd, go low
       endif else begin
         ind = first_el(minloc(amarr[i,gd2]))
       endelse
       xpos[i] = xltime[gd2[ind]]
       ypos[i] = amarr[i,gd2[ind]] 
     endif

     ; Just make sure it doesn't overlap another label
     if (gd2[0] eq -1) then begin

       ; Looping through all stars in front of this one to check
       ; that the positions don't overlap
       gd2 = gd
       for j=0,i-1 do begin
         bd = where( abs(xltime[gd2]-xpos[j]) lt 0.1 and abs(reform(amarr[i,*])-ypos[j]) lt 0.1,nbd)

         ; Removing all of the remaining elements
         if nbd eq n_elements(gd2) then begin
           gd2 = -1
           break
         endif else begin
           if nbd gt 0 then remove,bd,gd2
         endelse
       end

       ; Getting the final position
       if (gd2[0] ne -1) then begin
         xpos[i] = xltime[gd2[0]]
         ypos[i] = amarr[i,gd2[0]] 
       endif else begin
         ; Can't find one that doesn't overlap, just put it at the top
         xpos[i] = xltime[gd[0]]
         ypos[i] = amarr[i,gd[0]] 
       endelse

      endif  ; just make sure it doesn't overlap

    ; The first one
    endif else begin
      xpos[i] = xltime[gd[0]]
      ypos[i] = amarr[i,gd[0]]
    endelse

  endif ; it's on the plot

  ;if xpos[i] eq -1.0 then stop

end  ; looping through the stars


; Plotting
IF not keyword_set(noplot) then begin

; Saving the plot to postscript
if keyword_set(save) then begin
  orig_p = !p
  orig_x = !x
  orig_y = !y

  if n_elements(psfile) eq 0 then psfile='airchart'
  ps_open,psfile,color=color
  charsize = 1.2

  !p.thick = 3
  !p.charthick = 3
  !x.thick = 3
  !y.thick = 3
endif else begin
  charsize = 1.2
endelse

;xr = [6,18]/24.0
yr = [1.0,2.5]
xtit = 'Zone Time'
ytit = 'Airmass'
;pos = [0.13,0.091,0.961,0.954]   ; original
pos = [0.13,0.091,0.961,0.80]
;xltime = -0.5+ltime/24.0

; Use xtickunits='hours'
; it's really in units of days, 0-1 = 24 hours
;           0 = 12 noon
;           0.5 = midnight

plot,xltime,am,/nodata,xr=xr,yr=yr,xtickunits='Hours',xs=9,xminor=2,xtickinterval=1,$ ; xticks=12
     xtit=xtit,ytit=ytit,charsize=charsize,position=pos


colors = fsc_color(['yellow','magenta','cyan','green','red','powder blue','orange','wheat','purple'])
;colors = fsc_color(['yellow','magenta','cyan','green','red','blue','orange','wheat','purple'])

; Looping through the stars
for i=0,nstars-1 do begin

  ; Color
  if keyword_set(color) then begin
    oplot,xltime,reform(amarr[i,*]),color=colors[i mod 9]
  ; B/W
  endif else begin
    oplot,xltime,reform(amarr[i,*])
  endelse

  ; Overplotting the name
  if n_elements(name) gt 0 then begin
    ; On the plot
    if xpos[i] ne -1 then begin
      ; Color
      if keyword_set(color) then begin
        xyouts,xpos[i],ypos[i],name[i],/data,color=colors[i mod 9]
      endif else begin
        xyouts,xpos[i],ypos[i],name[i],/data
      endelse
    endif
  end

end

; Overplot the airmass = 2.0 line
oplot,[0,30],[2.0,2.0],linestyle=2

; Overplot twilight lines
oplot,[1,1]*xltime(sunsetind),[0,2.5],linestyle=2
oplot,[1,1]*xltime(sunriseind),[0,2.5],linestyle=2

; Hour Angle < 5 hr
;gd = where(abs(lha) le 5.0,ngd)
;oplot,xltime(gd),am(gd),thick=3


; Overplot the LST axis on top
; noon = 0.0, midnight=0.5
diff = median(lst-ltime)
axis,xr=xr+diff/24.0,xaxis=1,xtickunits='Hours',xtickinterval=1,xminor=2,xs=1,$
    charsize=charsize
xyouts,0.5,0.85,'Local Sidereal Time (LST)',/normal,charsize=1.0,align=0.5

; ANNOTATIONS
; xyouts of the input data (date, ra, dec, lon, lat, sunset, sunrise) on top
dat = month_cnv(month,/short)+' '+strtrim(day,2)+', '+strtrim(year,2)
wkday = weekday(year,month,day)
wkday = strupcase(strmid(wkday,0,3))
out = 'Airmass chart for '+wkday+' '+dat+' at '+strupcase(obsname)+' (time zone='+strtrim(long(tzone),2)+')'
xyouts,0.5,0.96,out,/normal,charsize=1.0,align=0.5
xyouts,0.5,0.93,'evening ast. twilight = '+strsunsettime+'   morning ast. twilight = '+strsunrisetime,$
       /normal,charsize=1.0,align=0.5
moonout = 'moon: ra = '+strmoonra+'  dec = '+strmoondec+'  rise = '+strmoonrisetime
moonout = moonout+'  set = '+strmoonsettime+'  ill. frac = '+strillfrac
xyouts,0.5,0.90,moonout,/normal,charsize=1.0,align=0.5

if keyword_set(save) then begin
  ps_close
  !p = orig_p
  !x = orig_x
  !y = orig_y
endif

ENDIF   ; /noplot


; Making output arrays
; Getting the indices inside the plot
gd = where(xltime ge xr[0] and xltime lt xr[1],ngd)
amout = amarr[*,gd]
timeout = (xltime[gd]+0.5)*24.0
lstout = lst[gd]
;xltime = -0.5+ltime/24.0


if keyword_set(stp) then stop

end
