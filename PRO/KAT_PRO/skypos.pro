;+
; NAME:
;   SKYPOS
;
; PURPOSE:
;   This program is designed to show how a given object (i.e. RA, DEC)
;   tracks across the sky during a specified night.  The program gives
;   two forms of output: 1) a table listing Zenith Distance, Airmass,
;   Hour Angle, UT, local time, and positions of the moon as well during
;   a specified night; and 2) a plot of how the object and the moon
;   track across the sky.  Alternately, the data can be printed for a
;   single specified UT.
;
; CALLING SEQEUNCE:
;   SkyPos,[/noplot]
;
; INPUT:
;   None.  All required information is prompted for.
;
; OUTPUT:
;   A table and a plot.  See description above
;
; EXAMPLE:
;
; HISTORY:
;   17-FEB-94 Version 1 written by Eric W. Deutsch
;-

pro get_dbl,prompt1,dblval

GD_AGAIN:
  inp=' '
  if (n_elements(dblval) eq 0) then dblval=0.0d
  dblval=dblval*1.0d
  prompt=prompt1+': ['+strn(dblval)+']'
  read,prompt+'  ',inp
  if (inp eq '') then return
  
  if (strpos(inp,':') ne -1) then begin
    inp1=''
    for ii=0,strlen(inp)-1 do begin
      ch=strmid(inp,ii,1)
      if (ch eq ':') then ch=' '
      inp1=inp1+ch
      endfor
    nums=getopt(inp1,'F') & sec=0.0
    case (n_elements(nums)) of
      2: begin & hr=nums(0) & min=nums(1) & end
      3: begin & hr=nums(0) & min=nums(1) & sec=nums(2) & end
      else: begin & print,'  ** Invalid **' & goto,GD_AGAIN & end
      endcase
    dblval=hr + min/60d + sec/3600d
    return
    endif

  if (strnumber(inp) eq 0) then begin
    print,'  ** Invalid number ** ' & goto,GD_AGAIN & endif
  dblval=double(inp)
  return

end


; =========================================================================
; The calculations are from "Astronomical Photometry" by Henden & Kaitchuck

pro calc_ZDHA,lst,latitude,longitude,ra,dec,ZD,HA,AZ

  H = 15.0d * ( lst - ra/15.0d )

  sin_h1 = sin(latitude/!radeg) * sin(dec/!radeg) + $
	   cos(latitude/!radeg) * cos(dec/!radeg) * cos(H/!radeg)
  h1 = asin(sin_h1)*!radeg

  cos_A = ( sin(dec/!radeg) - sin(latitude/!radeg) * sin(h1/!radeg) ) / $
	  ( cos(latitude/!radeg) * cos(h1/!radeg) )
  AZ = acos(cos_A)*!radeg

  HA=H/15 & if (HA lt -12) then HA=HA+24
  if (HA gt 12) then HA=HA-24

  if ( HA gt 0 ) then AZ = 360.0 - AZ

  ZD = 90 - h1

  ZDc=ZD & if (ZD gt 85) then ZDc=85d
  ZD=ZD-0.00452*800*tan(ZDc/!radeg)/(273+10d)

  return
end

; ==========================================================================

pro skypos,dummy,noplot=noplot

  if (n_elements(noplot) eq 0) then noplot=0

  COMMON AST_POS_COMM,latitude,longitude,timezone,year,month,day,radec,equinox,stepsize, $
    specUT,starttime,endtime,targname

  if (n_elements(year) eq 0) then begin
    spawn,'date',datestr & datestr=datestr(0) & print,datestr
    year=double(strmid(datestr,24,4))
    months=['Urk','Jan','Feb','Mar','Apr','May','Jun','Jul','Aug', $
      'Sep','Oct','Nov','Dec']
    tmp1=strmid(datestr,4,3)
    tmp2=where(tmp1 eq months)
    if (tmp2(0) eq -1) then month=1.0d else month=tmp2(0)*1.0d
    day=double(strmid(datestr,8,2))
    if (strmid(datestr,21,1) eq 'S') then timezone=-7d else timezone=-6d
    endif

  if (n_elements(latitude) eq 0) then latitude=32.78036d
  if (n_elements(longitude) eq 0) then longitude=-105.82042d
  if (n_elements(timezone) eq 0) then timezone=-6.0d
  if (n_elements(year) eq 0) then year=1997.0d
  if (n_elements(month) eq 0) then month=1.0d
  if (n_elements(day) eq 0) then day=1.0d
  if (n_elements(radec) eq 0) then radec='0.00 0.00'
  if (n_elements(equinox) eq 0) then equinox=2000.0d
  if (n_elements(stepsize) eq 0) then stepsize=60d
  if (n_elements(specUT) eq 0) then specUT=0d
  if (n_elements(starttime) eq 0) then starttime=17d
  if (n_elements(endtime) eq 0) then endtime=7d
  if (n_elements(targname) eq 0) then targname=''


; ========= Select Observatory =====================================================
  print,'Selected Observatories: (from The Astronomical Almanac)'
  print,'  MRO              lat=  46.952   lon= -120.723    timezone= -7 (PDT)  -8 (PST)'
  print,'  DAO              lat=  48.520   lon= -123.417    timezone= -7 (PDT)  -8 (PST)'
  print,'  APO              lat=  32.787   lon= -105.820    timezone= -6 (MDT)  -7 (MST)'
  print,'  KPNO             lat=  31.963   lon= -111.600    timezone=      -7 (MST)'
  print,'  CTIO             lat= -30.165   lon=  -70.815    timezone= -3 (CDT)  -4 (CST)'
  print,'  CFHT             lat= +19.828   lon= -155.472    timezone=     -10 (HST)'
  print,'  AAO              lat= -31.273   lon= +149.062    timezone=-14 (ADT) -13 (AST)'
  print,'                                                  (xDT is ~Apr - ~Oct in north)'
  print,' '

  get_dbl,'Enter Latitude',latitude
  get_dbl,'Enter Longitude',longitude
  get_dbl,'Enter Timezone',timezone
  get_dbl,'Enter Year',year
  get_dbl,'Enter Month(1-12)',month
  msg1='Enter UT Date BEGINNING during Obs. Night(1-31)'
  if (timezone le -12) then msg1='Enter UT Date BEGINNING in the morning(1-31)'
  get_dbl,msg1,day
  get_dbl,'Enter stepsize for calculation in minutes (0 for a single time)',stepsize
  if (stepsize eq 0) then $
    get_dbl,'Enter specific UT',specUT $
  else begin
    get_dbl,'Enter starting LOCAL time (0-23.99)',starttime
    get_dbl,'Enter ending LOCAL time (0-23.99)',endtime
    endelse

RADECAGAIN:
  inp=radec
  print,'You may use or Sexigesimal or Decimal format'
  read,'  Enter RA and DEC: ['+strn(radec)+']  ',inp
  if (inp ne '') then radec=inp
  if (strpos(radec,':') eq -1) then radec1=radec $
  else begin
    radec1=''
    for ii=0,strlen(radec)-1 do begin
      ch=strmid(radec,ii,1)
      if (ch eq ':') then ch=' '
      radec1=radec1+ch
      endfor
    endelse
  Coord=getopt(radec1,'F')
  case (n_elements(Coord)) of
    2: begin & ra=Coord(0) & dec=Coord(1) & end
    6: begin & stringad,radec,ra,dec & end
    else: begin & print,'  ** Invalid **' & goto,RADECAGAIN & end
    endcase
  ra=ra*1.0d & dec=dec*1.0d


  get_dbl,'Enter Equinox of these coordinates',equinox
  eqcur=year+(month-1)/12+day/365
  precess,ra,dec,equinox,eqcur


  inp=targname
  read,'Enter target name: ['+strn(targname)+']  ',inp
  if (inp ne '') then targname=inp


  jdcnv,year,month,day,0.0d,jul_date


  print,' ' & print,'-----------------  '+targname+'  ----------------------' & print,' '
  print,'UT date yyyy-mm-dd = '+strn(fix(year))+'-'+strn(fix(month))+'-'+strn(fix(day))+ $
    '    latitude='+strn(latitude)+', longitude='+strn(longitude)
  print,'Julian date at 0 UT:  ',strn(jul_date,format='(f15.4)')
  moonpos,jul_date+4/24.0d,mra,mdec
  mphase,jul_date+4/24.0d,frac
  print,'Moon at   '+adstring(mra,mdec,2)+'   (Equinox J'+strn(eqcur,format='(f15.4)')+') at UT=4.00'
  print,'Lunar illumination: ',strn(frac*100,format='(f20.1)'),'%'

  print,'Object at '+adstring(ra,dec,2)+'   (Equinox J'+strn(eqcur,format='(f15.4)')+')'
  jra=ra & jdec=dec & precess,jra,jdec,eqcur,2000.0
  print,'Object at '+adstring(jra,jdec,2)+'   (Equinox J2000.0)'

  gcirc,0,mra/!radeg,mdec/!radeg,ra/!radeg,dec/!radeg,dist
  print,'Distance between object and moon: ',strn(dist*!radeg,format='(f20.2)'),' degrees at UT=4.00'

  print,'Sun ZDs: sunset:90.8  --  about dark enough:101  --  astronomical twilight:108'
  print,' '

  print,'Lcl Tim    UT       ST     Zen Dst  Airms  Hr Angle  |  Moon ZD  Moon HA  SunZD'
  print,'-------  -------  -------  -------  -----  --------  +  -------  -------  -----'
; print,'12.4567  12.4567  12.4567  123.567  1.345  123.5678  |  123.567  123.567  123.5'


  posdata=fltarr(4,500) & i=0

  starttime1=starttime
  endtime1=endtime
  if (starttime1 gt endtime1) then starttime1=starttime1-24
  if (starttime1 gt 12) then starttime1=starttime1-24
  if (endtime1 gt 12) then endtime1=endtime1-24
  if (starttime1 gt endtime1) then endtime1=endtime1+24
  localtime=starttime1
  stopflag=0

  while (localtime le endtime1) and (stopflag eq 0) do begin
    UT=localtime-timezone

    if (stepsize eq 0) then begin
      UT=specUT
      localtime=UT+timezone
      stopflag=1
      endif

;    ct2lst,lst,-1.0*longitude,timezone,jul_date+UT/24.0d
    ct2lst,lst,longitude,timezone,jul_date+UT/24.0d
    calc_ZDHA,lst,latitude,longitude,ra,dec,ZD,HA,AZ
    sec_z=1/cos((ZD<87d)/!radeg)
    AM = sec_z - 0.0018167d * ( sec_z - 1 ) - 0.002875 * ( sec_z - 1 )^2 - $
	 0.0008083d * ( sec_z - 1 )^3

    moonpos,jul_date+UT/24.0d,mra,mdec
    calc_ZDHA,lst,latitude,longitude,mra,mdec,mZD,mHA,mAZ
    sunpos,jul_date+UT/24.0d,sra,sdec
    calc_ZDHA,lst,latitude,longitude,sra,sdec,sZD,sHA,sAZ

    lt1=localtime & if (lt1 lt 0) then lt1=24+lt1
    print,format='(f7.4,2x,f7.4,2x,f7.4,2x,f7.3,2x,f5.3,2x,f8.4,a,f7.3,2x,f7.3,f7.1)', $
      lt1,UT,lst,ZD,AM<9.999,HA,'  |  ',mZD,mHA,sZD

    posdata(*,i)=[ZD,AZ,mZD,mAZ] & i=i+1

    localtime=localtime+stepsize/60d
    endwhile

  if (noplot eq 1) or (stepsize eq 0) or (i lt 2) then return

  posdata=posdata(*,0:i-1)

  if (!d.name eq 'PS') then begin
    !p.font=0					; select hardware fonts
    device,/helv,/isolatin1			; Helvetica ISOLatin fontset
  endif else begin			; If screen or other output mode
    !p.font=-1					; select Hershey fonts
    xyouts,0,0,/norm,'!17'			; Set to Triplex Roman font
    endelse

  map_set,90,0,180,/grid,/azi,limit=[0,0,90,360]
  plotsym,0,/fill
  mZD=posdata(2,*) & mAZ=posdata(3,*)
  good=where(mZD lt 90)
  if (good(0) ne -1) then plots,mAZ(good),(90-mZD(good))>2,psym=8
  plots,[.04],[.088],/norm,psym=8

  plotsym,3,/fill
  ZD=posdata(0,*) & AZ=posdata(1,*)
  good=where(ZD lt 90)
  if (good(0) ne -1) then plots,AZ(good),(90-ZD(good))>2,psym=8,symsize=1.5
  plots,[.04],[.058],/norm,psym=8,symsize=1.5

  xyouts,.05,.05,/norm,'Object'
  xyouts,.05,.08,/norm,'Moon'

  plots,[0],[latitude],psym=4
  plots,[.04],[.118],/norm,psym=4
  xyouts,.05,.11,/norm,'Pole'

  xyouts,.5,.96,/norm,'N',align=.5,charsize=2
  xyouts,.01,.475,/norm,'E',charsize=2

  i=findgen(20)/5
  x=.08+sin(i)/50 & y=.90+cos(i)/50
  plots,x,y,/norm
  arrow,x(1),y(1),x(0),y(0),/norm


  return

end