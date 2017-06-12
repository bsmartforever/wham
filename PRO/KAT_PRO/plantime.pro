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
        'MC [N II] 6584' : return, 'nii'
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

pro plantime, starttime = starttime, planfile = planfile, exptime = exptime, $
              startatdark = startatdark

;+
;NAME: plantime
;
;SYNTAX: plantime [, starttime = starttime] [, planfile = planfile] $
;       [, exptime = exptime] [, /startatdark]
;
;OPTIONAL KEYWORD INPUTS:
;
; - starttime = starttime: startimg time JD (UT). e.g. for 2006-4-24, 20:25 CLT:
;       plantime, starttime = julday ( 4, 24, 2006, 20, 25 ) + 4./24
; - /startatdark: set starttime to tonight's dark time if it's after noon CLT
; - exptime = exptime: initial exposure time (default: 30)
;
; Modifications:
; - Fixed the degree symbol for the zenith distance. -Kat 09/2013
; - Read the block information from /d/wham/observing/pointing.dat
;   instead of restoring /d/wham/lib/viewplan/points.dat. This way
;   all currently allowed blocks will load instead of only up to 1234.
;   Note that this fix is quisi sloppy as points and npoints are returned
;   to the main level so that the file is only read once if plantime is 
;   called multiple times. -Kat 09/2013
; - If zd is greater than 80 degrees, print in red.
;
;NOTE: use Chilean time (CLT) all year; never uses Chilean Summer Time (CLST)
;       Zenith distance reported for a block is the maximum zenith distance
;               of any pointing in the block
;-

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
  
  zero = -1000.0        ; initialize to zero; gets set in every Setup command

  deg = '"'+String("260B)+'"'

  ;; Calibration direction information

 calib_name=['G194_0', 'G144_0', 'G300_0', 'Lockman', 'Spica_HII', 'Crab', 'NAN', 'l_ori', 'l_ori_off', 'zeta_oph', 'zeta_oph_offa', 'zeta_oph_offb', 'south_gal_pole','faint_89_-71','faint_60_-67']

  calib = replicate({calibration, name:'', glon:0.0, glat:0.0}, n_elements(Calib_name))

  calib.name = calib_name
  calib.glon = [     194,      144,      300,     148.5,       315.5, 184.67, 85.60, 194.7,    200.0,     3.42,     4.5,           16.5, 0.0, 88.6,60.5]
  calib.glat = [       0,        0,        0,      53.0,        48.9,  -5.86, -0.71, -11.8,    -8.0,      23.54,    42.6,          37.5, -90.0,-71.3,-67.4]

;This only has up to 1234 blocks in it
;restore, '/d/wham/lib/viewplan/points.dat

;This loads all the blocks in the pointing.dat file. 
;This will only load this file once and return the 
;values of npoints and points to the main level.
;If these values exist in the main level, e.g., 
;plantime was already ran once before, the program
;will extract those values from the main leve instead of
;reading them from the pointing.dat file. This is really 
;done to save time. Though it's a sloppy way to code, it is
;much less frustrating that waiting for the file to be read 
;off the mounted /d/wham/observing directory.
if (NOT keyword_set((SCOPE_varfetch('npoints', LEVEL=1,/enter)))) OR $
   (NOT keyword_set((SCOPE_varfetch('points', LEVEL=1,/enter)))) then begin

      readcol,'/d/wham/observing/pointings.dat',glon,glat,bnum,/silent
      points=fltarr(3,n_elements(bnum))
      points[0,*]=glon
      points[1,*]=glat
      points[2,*]=bnum
      npoints=fltarr(bnum[n_elements(bnum)-1])
      for i=1, n_elements(npoints)-1 do begin $
        junk=where(bnum eq i,num) & $
        npoints[i]=num & end

      (SCOPE_varfetch('npoints', LEVEL=1,/enter))=npoints
      (SCOPE_varfetch('points', LEVEL=1,/enter))=points

endif else begin

      npoints=(SCOPE_varfetch('npoints', LEVEL=1,/enter))
      points=(SCOPE_varfetch('points', LEVEL=1,/enter))

endelse

observatory, 'ctio', obs

  if n_elements(planfile) eq 0 then planfile = '/d/wham/observing/currentplan'
  if n_elements(exptime) eq 0 then exptime = 30
  if n_elements(starttime) eq 0 then $
    starttime = systime(/jul, /ut)

  n_lines = file_lines(planfile)
  lines = strarr(n_lines)

  openr, unit, planfile, /get_lun
  readf, unit, lines
  free_lun, unit

  dark_times = get_dark_times(starttime - obs.tz/24.0)
  
  IF keyword_set(startatdark) THEN starttime = darktime()
  
  print, starttime - obs.tz/24.0, format = '("Start time: ", C(CHI2.2, ":", CMI2.2, ":", CSI2.2), " CLT", /)'

  print, "Action", "NP", "St. Time", "Dur.", "Max ZD", 'min v', 'max v', 'v_term', $
    format = '(A, T40,A4,A10,A7,A7," |",A7,A8,A8)' 
  print, strjoin(replicate('-', 93))
 
  plan = 0.0D  ;; plan accumulator in units of day(s)

  print, "**** START DARK TIME (CLT UT-" + string(obs.tz, format='(I02)') + $
        ", not CLST) ****", $
  string(dark_times[0]-obs.tz/24.0,format='(C(CHI2.2,":",CMI2.2,":",CSI2.2))'),$
    format = '(A, T40, 4X, 2X, A8)'
  print, "**** NIGHT OF ", string(dark_times[0]-0.5, format='(C(cmoi2.2,"/",cdi2.2,"/",cyi4.2))'), " ****", $
        format = "(A, A, A,/)"


  time = 0D
  for i = 0, n_elements(lines)-1 do begin 


;spawn,'echo -en "\033]11;?\033\\"',background
;background=strsplit(background,':,\',/extract)
;spawn,'echo -en "\033]10;?\033\\",foreground

;echo -ne "\033]11;#000000\007"
;echo -ne "\033]10;#ffffff\007" 



    ;set default terminal text to black. 
    spawn,'echo -ne "\033[30m"'
    case 1 of 
      stregex(lines[i], '^Block', /bool): begin
        bnum = (stregex(lines[i], '^Block #([0-9]+)', /extract, /subexpr))[1]
        pointings = where(points[2, *] eq bnum,num)
        time = (num * (exptime + pointing_overhead) + $
                block_overhead) / 60.0

; Calculate zenith distance for each pointing individually and report max
;       Using mean or median glon and glat doesn't effectively represent the block
;       when block wraps around l=0/360
        glon = total(points[0, pointings], 1)
        glat = total(points[1, pointings], 1)
        obstime = ( indgen(num) * (exptime+pointing_overhead) + $
                block_overhead ) / (60.0*60.0*24) ; (in days)

        euler, glon, glat, ra, dec, 2
        eq2hor, ra, dec, starttime+plan+obstime, alt, az, obsname = 'ctio'
        zd = 90-alt
        
                  vrange = zero EQ -1000. ? [0,0] : $
                        [-50,150] - vlsr(lon=glon, lat=glat, jd=starttime+plan)-zero    

        if (max(zd) ge 80.) then spawn,'echo -ne "\033[31m"' else spawn,'echo -ne "\033[30m"'
        print, lines[i], num, starttime+plan - obs.tz/24.0, $
          time, max(zd), vrange[0], vrange[1], median(term_vel(glon, glat)), $
          format = '(A, T40, I4, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), F7.2,F7.2,'+deg+', F+7.2, 2F+8.2)'
                
        plan = plan + time/60.0/24.0
        
      end

      stregex(lines[i], '^(Calibration:|Geocorona:)', /bool): begin
        time = (obs_overhead + exptime) / 60.0

        c = stregex(lines[i], '^Calibration: (.*)', /extract, /subexpr)
        if (c[1] ne '') then begin
          cw = where(strmatch(calib.name, c[1]) eq 1)
          
          euler, calib[cw].glon, calib[cw].glat, ra, dec, 2
          eq2hor, ra, dec, starttime+plan, alt, az, obsname = 'ctio'
          zd = 90-alt
                    
          vrange = zero EQ -1000. ? [0,0] : $
                [-50,150] - vlsr(lon=calib[cw].glon, lat=calib[cw].glat, $
                jd=starttime+plan)-zero

        if (max(zd) ge 80.) then spawn,'echo -ne "\033[31m"' else spawn,'echo -ne "\033[30m"'
        print, lines[i], 1, starttime+plan - obs.tz/24.0, time, zd, $
                vrange[0], vrange[1], term_vel(calib[cw].glon, calib[cw].glat), $
            format = '(A, T40, I4, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), 2F7.2, '+deg+', F+7.2, 2F+8.2)'

        endif else begin 
          print, lines[i], starttime+plan - obs.tz/24.0, time, $
            format = '(A, T40, 4X, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), F7.2)'
        endelse

        plan = plan + time/60.0/24.0
      end

      stregex(lines[i], '^Observation:', /bool): begin
        time = (obs_overhead + exptime) / 60.0

        o = stregex(lines[i], '^Observation: .* \(([0-9.-]+) ([0-9.-]+)\)', $
                    /extract, /subexpr)

        euler, o[1], o[2], ra, dec, 2
        eq2hor, ra, dec, starttime+plan, alt, az, obsname = 'ctio'
        zd = 90.-alt

                vrange = zero EQ -1000. ? [0,0] : $
                        [-50,150] - vlsr(lon=o[1], lat=o[2], jd=starttime+plan)-zero    

        if (max(zd) ge 80.) then spawn,'echo -ne "\033[31m"' else spawn,'echo -ne "\033[30m"'
        print, lines[i], 1, starttime+plan - obs.tz/24.0, time, zd, $
                vrange[0], vrange[1], term_vel(o[1], o[2]), $
               format = '(A, T40, I4, 2X, C(CHI2.2, ":", CMI2.2, ":", CSI2.2), F7.2, F7.2, '+deg+', F+7.2, 2F+8.2)'

        plan = plan + time/60.0/24.0
      END 

      stregex(lines[i], '^Setup', /bool): begin
        time = setup_overhead / 60.0
        print, lines[i], starttime+plan - obs.tz/24.0, time, $
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

  spawn,'echo -ne "\033[30m"'
  print, "****** END DARK TIME ******", $
  string(dark_times[1]-obs.tz/24.0,format='(C(CHI2.2,":",CMI2.2,":",CSI2.2))'),$
    format = '(/, A, T40, 4X, 2X, A8)'


  print, "****** TOTAL PLAN HOURS ******", (plan)*24.0,format='(A-41,f8.1)'
  print, "****** TOTAL DARK HOURS ******",(dark_times(1)-dark_times(0))*24.0,format='(A-41,f8.1)'

end