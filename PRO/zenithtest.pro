FUNCTION filter_long2short, filter

CASE strtrim(filter,2) OF 
	'Barr H-Alpha' : return, 'ha'
	'Omega H-Alpha' : return, 'ha'
	'Blue H-Alpha' : return, 'ha'
	'Red H-Alpha' : return, 'ha'
	'MC H-Alpha' : return, 'ha'
	'H-Beta' : return, 'hb'
	'[S II] 6716' : return, 'sii'
	'MC [S II] 6716' : return, 'sii'
	'[N I] 5200' : return, 'ni'
	'[N II] 6584' : return, 'nii'
	'[N II] 5755' : return, 'nii_blue'
	'Na I 5890' : return, 'nai'
	'Fe X 6374' : return, 'fex'
	'[O I] 6300' : return, 'oi'
	'[O II] 7320' : return, 'oii'
	'[O III] 5007' : return, 'oiii'
	'He I 5876' : return, 'hei'
ENDCASE

return, -1

END

pro zenithtest, starttime = starttime, planfile = planfile, exptime = exptime, $
              startatdark = startatdark

;+
;NAME: plantime
;
;SYNTAX: plantime [, starttime = starttime] [, planfile = planfile] $
;	[, exptime = exptime] [, /startatdark]
;
;OPTIONAL KEYWORD INPUTS:
;
; - starttime = starttime: startimg time JD (UT). e.g. for 2006-4-24, 20:25 CLT:
;	plantime, starttime = julday ( 4, 24, 2006, 20, 25 ) + 4./24
; - /startatdark: set starttime to tonight's dark time if it's after noon CLT
; - exptime = exptime: initial exposure time (default: 30)
;
;NOTE: use Chilean time (CLT) all year; never uses Chilean Summer Time (CLST)
;	Zenith distance reported for a block is the maximum zenith distance
;		of any pointing in the block
;
;EDIT NOTE: This is an alteration of plantime to try and work with the MS and planning out the survey. I am going to try and alter this 
;so it writes out a visual map with thezenith distances and prints the top 10 blocks, like I have in bestairmass.
;
;
;
;-
@colplstart
;filen=STRING(STRTRIM(year-2000,1),STRING(month,FORMAT='(I02)'),STRING(day,FORMAT='(I02)'),STRING(hour,FORMAT='(I02)'),'.ps')
filen=STRING(starttime,'.ps')

device, file = filen
  ;; timing constants (in seconds)... tweak at will 


  ;; Block overhead:

  ;; Overall, it's pretty consistantly 6-7 seconds per pointing. If
  ;; you just divide the whole overhead by the number of points, you
  ;; get 6-7 seconds per pointing. But some of that is the time to get
  ;; the block started as well. A simple linear regression on a
  ;; collection of observed blocks with different numbers of pointings
  ;; breaks down the overhead nicely.

  ;; Average block 'per pointing' overhead: 

  pointing_overhead = 6.2

  ;; Average block startup overhead:

  block_overhead = 18
  
  ;; Average time for a non-block pointing observation

  ;; Lots of scatter here, but typically 20-40 seconds. It could
  ;; probably be estimated better by knowing the path the siderostat
  ;; is taking on the sky, but let's try this static estimate first.

  obs_overhead = 40

  ;; Average time to set up a wavelength:

  ;; This ranges between 1-2 minutes and depends on how far the filter
  ;; wheel has to travel and how much the pressures are changing. The
  ;; former usually dominates when changing lines, though. Again, this
  ;; could be estimated since we do know the filter positions, but
  ;; let's try a simple upper limit and see how good we can do with
  ;; it.

  setup_overhead = 120
  
  zero = -1000.0	; initialize to zero; gets set in every Setup command
  loadct,10
  ;; Calibration direction information

 calib_name=['G194_0', 'G144_0', 'G300_0', 'Lockman', 'Spica_HII', 'Crab', 'NAN', 'l_ori', 'l_ori_off', 'zeta_oph', 'zeta_oph_offa', 'zeta_oph_offb', 'south_gal_pole','faint_89_-71','faint_60_-67']

  calib = replicate({calibration, name:'', glon:0.0, glat:0.0}, n_elements(Calib_name))

  calib.name = calib_name
  calib.glon = [     194,      144,      300,     148.5,       315.5, 184.67, 85.60, 194.7,    200.0,     3.42,     4.5,           16.5, 0.0, 88.6,60.5]
  calib.glat = [       0,        0,        0,      53.0,        48.9,  -5.86, -0.71, -11.8,    -8.0,      23.54,    42.6,          37.5, -90.0,-71.3,-67.4]

  restore, '/d/wham/lib/viewplan/points.dat'

  ;; 2015 - Chile staying on CLST (daylight savings time) year-round
  observatory, 'ctio', obs
  obs.tz = 3.0

  if n_elements(planfile) eq 0 then planfile = '/d/wham/bsmart/allblocks'
  if n_elements(exptime) eq 0 then exptime = 30
  if n_elements(starttime) eq 0 then $
    starttime = systime(/jul, /ut)

  n_lines = file_lines(planfile)
  lines = strarr(n_lines)

  openr, unit, planfile, /get_lun
  readf, unit, lines
  free_lun, unit

  dark_times = get_dark_times(starttime - obs.tz/24.0)
  
  IF keyword_set(startatdark) THEN starttime = dark_times[0]
  
  print, "Action", "NP", "St. Time", "Dur.", "Max ZD", 'min v', 'max v', 'v_term', $
    format = '(A, T40,A4,A10,A7,A7," |",A7,A8,A8)' 
  print, strjoin(replicate('-', 93))
 
  print, systime(/utc, /julian) - obs.tz/24.0, format = '("Current time is ", C(CHI2.2, ":", CMI2.2, ":", CSI2.2), " CLST")'
  print, starttime - obs.tz/24.0, format = '("Observation time: ", C(CHI2.2, ":", CMI2.2, ":", CSI2.2), " CLST", /)'
  plan = 0.0D  ;; plan accumulator in units of day(s)

  print, "**** START DARK TIME (CLST UT-" + string(obs.tz, format='(I02)') + $
  	") ****", $
  string(dark_times[0]-obs.tz/24.0,format='(C(CHI2.2,":",CMI2.2,":",CSI2.2))'),$
    format = '(A, T40, 4X, 2X, A8)'
  print, "**** NIGHT OF ", string(dark_times[0]-0.5, format='(C(cmoi2.2,"/",cdi2.2,"/",cyi4.2))'), " ****", $
  	format = "(A, A, A,/)"
  date=string(dark_times[0]-0.5, format='(C(cmoi2.2,"/",cdi2.2,"/",cyi4.2))')
  stime=string(starttime - obs.tz/24.0, format = '("    ", C(CHI2.2, ":", CMI2.2, ":", CSI2.2)/)')

  time = 0D
  labels=MAKE_ARRAY(n_elements(lines),3)
  bestarray=MAKE_ARRAY(2,n_elements(lines))
  nul1=[0]
  nul2=[0]
;***Start of plotting****
  plot, nul1, nul2, xrange=[5,-125],ytitle='b',charsize=1.5,title=date+stime,$
  xtitle='l',yrange=[-25,25],background=255,color=0, psym=1



;****************Here we are in a for loop that is going through all the blocks*********
  for i = 0, n_elements(lines)-1 do begin 
    case 1 of 
      stregex(lines[i], '^Block', /bool): begin
        bnum = (stregex(lines[i], '^Block #([0-9]+)', /extract, /subexpr))[1]
        time = (npoints[bnum-1] * (exptime + pointing_overhead) + $
                block_overhead) / 60.0

        pointings = where(points[2, *] eq bnum)

; Calculate zenith distance for each pointing individually and report max
;	Using mean or median glon and glat doesn't effectively represent the block
;	when block wraps around l=0/360
        glon = total(points[0, pointings], 1)
        glat = total(points[1, pointings], 1)
        obstime = ( indgen(npoints[bnum-1]) * (exptime+pointing_overhead) + $
        	block_overhead ) / (60.0*60.0*24) ; (in days)

;******************This gets me my zd calculator. I should now have all the ZD and the coordinates in galactic after this *******
        euler, glon, glat, ra, dec, 2
	eulerplus, glon, glat, mclon, mclat, 7
        eq2hor, ra, dec, starttime, alt, az, obsname = 'ctio'
        zd = 90-alt
        
		  vrange = zero EQ -1000. ? [0,0] : $
		  	[-50,150] - vlsr(lon=glon, lat=glat, jd=starttime+plan)-zero	

;******* Sets colors for block depending on ZD

			if (max(zd) GT 70)THEN BEGIN
			c=0
			ENDIF
			if (max(zd) GT 50) && (max(zd) LT 70) THEN BEGIN
			c=50
			ENDIF
			if (max(zd) GT 35) && (max(zd) LT 50)  THEN BEGIN
			c=100
			ENDIF
			if (max(zd) GT 25) && (max(zd) LT 35)  THEN BEGIN
			c=150
			ENDIF
			if(max(zd) LT 25)  THEN BEGIN
			c=215
			ENDIF

if max(zd) LT 60 THEN BEGIN

        print, lines[i], npoints[bnum-1], starttime, $
          time, max(zd), median(mclon-360), median(mclat), median(term_vel(glon, glat)), $
          format = '(A, T40, I4, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), F7.2,F7.2, "°|", F+7.2, 2F+8.2)'
	 bestarray=[[bestarray],[i+7000.0,max(zd)]]
ENDIF 
		
        plan = plan + time/60.0/24.0

;*******More plotting here. Sets colors for block depending on ZD

	OPLOT, [mclon[0]-360-.5,mclon[-1]-360+.5], [mclat[0]-.5,mclat[0]-.5],  color=c
	OPLOT, [mclon[0]-360-.5,mclon[-1]-360+.5], [mclat[-1]+.5,mclat[-1]+.5],  color=c
	OPLOT, [mclon[-1]-360+.5,mclon[-1]-360+.5], [mclat[0]-.5,mclat[-1]+.5],  color=c
	OPLOT, [mclon[0]-360-.5,mclon[0]-360-.5], [mclat[0]-.5,mclat[-1]+.5],  color=c
	sym=1
	c=0
	labels(i,0)=avg(mclon)
	labels(i,1)=avg(mclat)
	labels(i,2)=i


      end

      stregex(lines[i], '^(Calibration:|Geocorona:)', /bool): begin
        time = (obs_overhead + exptime) / 60.0

        c = stregex(lines[i], '^Calibration: (.*)', /extract, /subexpr)
        if (c[1] ne '') then begin
          cw = where(strmatch(calib.name, c[1]) eq 1)
          
          euler, calib[cw].glon, calib[cw].glat, ra, dec, 2
          eq2hor, ra, dec, starttime, alt, az, obsname = 'ctio'
          zd = 90-alt
                    
          vrange = zero EQ -1000. ? [0,0] : $
          	[-50,150] - vlsr(lon=calib[cw].glon, lat=calib[cw].glat, $
          	jd=starttime+plan)-zero

          print, lines[i], 1, starttime, time, zd, $
          	vrange[0], vrange[1], term_vel(calib[cw].glon, calib[cw].glat), $
            format = '(A, T40, I4, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), 2F7.2, "°|", F+7.2, 2F+8.2)'

        endif else begin 
          print, lines[i], starttime - obs.tz/24.0, time, $
            format = '(A, T40, 4X, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), F7.2)'
        endelse

        plan = plan + time/60.0/24.0
      end

      stregex(lines[i], '^Observation:', /bool): begin
        time = (obs_overhead + exptime) / 60.0

        o = stregex(lines[i], '^Observation: .* \(([0-9.-]+) ([0-9.-]+)\)', $
                    /extract, /subexpr)

        euler, o[1], o[2], ra, dec, 2
        eq2hor, ra, dec, starttime, alt, az, obsname = 'ctio'
        zd = 90-alt

		vrange = zero EQ -1000. ? [0,0] : $
		  	[-50,150] - vlsr(lon=o[1], lat=o[2], jd=starttime)-zero	

        print, lines[i], 1, starttime, time, zd, $
        	vrange[0], vrange[1], term_vel(o[1], o[2]), $
               format = '(A, T40, I4, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), F7.2, F7.2, "°|", F+7.2, 2F+8.2)'
               
        plan = plan + time/60.0/24.0
      END 

      stregex(lines[i], '^Setup', /bool): begin
        time = setup_overhead / 60.0
        print, lines[i], starttime, time, $
          format = '(A, T40, 4X, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), F7.2)'
        plan = plan + time/60.0/24.0
        arr = strsplit(lines[i], ':', /extract)
        lfilter = arr[1]
        pressures = float(strsplit(arr[2], ' ', /extract))
        zero = predict_vel_calib3(pressures, filter_long2short(lfilter), /silent)
      end 

      stregex(lines[i], '^Set Exposure', /bool): begin
        exptime = (stregex(lines[i], '^Set Exposure Time: ([0-9]+)', $
                           /extract, /subexpr))[1]
        print, lines[i], format = '(A, T40)'
      end 

      else: print, "(", lines[i], ")"
    endcase
  endfor 

  print, "****** END DARK TIME ******", $
  string(dark_times[1]-obs.tz/24.0,format='(C(CHI2.2,":",CMI2.2,":",CSI2.2))'),$
    format = '(/, A, T40, 4X, 2X, A8)'


  print, "****** TOTAL PLAN HOURS ******", (plan)*24.0,format='(A-41,f8.1)'
  print, "****** TOTAL DARK HOURS ******",(dark_times(1)-dark_times(0))*24.0,format='(A-41,f8.1)'

for l=0, n_elements(lines)-1 DO BEGIN

    xyouts, labels(l,0)-360+4, labels(l,1), fix(labels(l,2)), CHARSIZE=.3
ENDFOR

;xyouts, 0, 0, 'pleasework'

device,/close
end
