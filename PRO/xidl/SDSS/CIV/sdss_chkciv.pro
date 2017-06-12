;+ 
; NAME:
; sdss_chkciv
;    Version 1.0
;
; PURPOSE:
;   Visually check SDSS CIV absorption detected with sdss_fndciv
;
; CALLING SEQUENCE:
;   
;   sdss_chkciv, 
;
; INPUTS:
;
; RETURNS:
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;   XSIZE      - Size of gui in screen x-pixels (default = 1000)
;   YSIZE      - Size of gui in screen y-pixels (default = 600)
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   sdss_chkciv, x, maskid, expsr
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   19-Dec-2003 Written by GEP/SHF
;-
;------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

pro sdss_chkciv_icmmn, qalfil, civfil, badcivfil

  common sdss_chkciv_cmm, $
    npix, $
    fx, $
    wv, $
    sig, $
    conti, $
    fit, $
    qalstr, $
    civstr, $
    badciv

  ;; QALSTR
  qalstr = xmrdfits(qalfil, 2, /silent)

  ;; MGII str
  if x_chkfil(civfil, /silent) EQ 1 then $
    civstr = xmrdfits(civfil, 1, /silent) $
  else begin
    civstr = { sdssmgiistrct }
  endelse
  if x_chkfil(badcivfil, /silent) EQ 1 then $
    badciv = xmrdfits(badcivfil, 1, /silent) $
  else begin
    badciv = { sdssmgiistrct }
  endelse

  return
end
  

;;;;
; Events
;;;;

pro sdss_chkciv_event, ev

  common sdss_chkciv_cmm

  WIDGET_CONTROL, ev.top, get_uvalue = state, /no_copy
  WIDGET_CONTROL, ev.id, get_uvalue = uval

  case uval of
      'SPLT': x_specplot, fx, sig, wave=wv, zin=state.zabs, /qal, /block, $
        inflg=4
      'DEFINITE': begin
          sdss_chkciv_setciv, state, /definite
          sdss_chkciv_next, state
          sdss_chkciv_setup, state
          sdss_chkciv_update, state
      end
      'GOOD': begin
          sdss_chkciv_setciv, state, /good
          sdss_chkciv_next, state
          sdss_chkciv_setup, state
          sdss_chkciv_update, state
      end
      'MAYBE': begin
          sdss_chkciv_setciv, state, /maybe
          sdss_chkciv_next, state
          sdss_chkciv_setup, state
          sdss_chkciv_update, state
      end
      'BAD': begin
          sdss_chkciv_setbad, state
          sdss_chkciv_next, state
          sdss_chkciv_setup, state
          sdss_chkciv_update, state
      end
      'DONE' : begin
          sdss_chkciv_svciv, state
          widget_control, ev.top, /destroy
          return
      end
      'SAVE' : begin
          sdss_chkciv_svciv, state
      end
      else :
  endcase

  WIDGET_CONTROL, state.base_id, set_uvalue = state, /no_copy
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;  Metals Plot
;;;;;;;;;;;;;;;;;;;;


pro sdss_chkciv_Metals, state
  
  common sdss_chkciv_cmm

  ;; Set plot window
  if state.psfile NE 1 then begin
      widget_control, state.mdraw_id, get_value=wind
      wset, wind
  endif

  clr = getcolor(/load)
  
  gdlin = where((state.velplt.flg MOD 2) EQ 1, ngd)

  ny = ngd / 2 + (ngd MOD 2 EQ 1)
;  ny = ngd 

  ;; Good lines
  !p.multi = [0,2,ny,0,1]
  for j=0L,ngd-1 do begin
      i = gdlin[j]
      pixmin = state.all_pmnx[0,i]
      pixmax = state.all_pmnx[1,i]

      ;; Plot
      if (j NE ny-1) AND (j NE ngd-1 ) then begin
          spaces = replicate('!17 ',30)
          plot, state.all_velo[0:state.all_pmnx[2,i],i], $
            fx[pixmin:pixmax]/conti[pixmin:pixmax], xrange=state.vmnx, $
            yrange=state.velplt[i].ymnx, xtickn=spaces, xmargin=[9,3], $
            ymargin=[0,0], $
            charsize=1.8, psym=10, background=clr.white, color=clr.black, $
            xstyle=1, ystyle=1
      endif else begin
          plot, state.all_velo[0:state.all_pmnx[2,i],i], $
            fx[pixmin:pixmax]/conti[pixmin:pixmax], xrange=state.vmnx, $
            yrange=state.velplt[i].ymnx, xmargin=[9,3], ymargin=[3,0], $
            charsize=1.8, psym=10, background=clr.white, color=clr.black, $
            xstyle=1, ystyle=1
      endelse

      ;; Labels
      xyouts, 0.07*(state.vmnx[1]-state.vmnx[0])+state.vmnx[0], $
        state.velplt[i].ymnx[0]+ $
        (state.velplt[i].ymnx[1]-state.velplt[i].ymnx[0])*0.05, $
        strtrim(state.velplt[i].name,2), $
        color=clr.black, charsize=1.5
      
      ;; Lines
      oplot, [0., 0.], state.velplt[i].ymnx, color=clr.blue, linestyle=2
      oplot, [-10000., 10000.], [0.,0.], color=clr.green, linestyle=3
      oplot, [-10000., 10000.], [1.,1.], color=clr.green, linestyle=3
  endfor

  !p.multi = [0,1,1]
  widget_control, state.tdraw_id, get_value=wind
  wset, wind
  gd = where(wv GT 1400*(1.+state.zabs) AND wv LT (1.+state.zabs)*1700)
  plot, wv[gd]/(1.+state.zabs), fx[gd]/conti[gd], xrange=[1480., 1620.], $
    yrange=[-0.2,1.4], xmargin=[9,3], ymargin=[3,0], $
    charsize=1.8, psym=10, background=clr.white, color=clr.black, $
    xstyle=1, ystyle=1
  oplot, wv[gd]/(1+state.zabs), sig[gd]/conti[gd], color=clr.red, psym=10
  oplot, 1548.19*[1.,1], [-9,9.], color=clr.blue, linestyle=1
  oplot, 1550.77*[1.,1], [-9,9.], color=clr.blue, linestyle=1

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
; UPDATE LYA+METALS
pro sdss_chkciv_update, state
  sdss_chkciv_updinfo, state
  sdss_chkciv_Metals, state
  return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
; Setup data
pro sdss_chkciv_setup, state
  common sdss_chkciv_cmm
    
stop
  ingood = where(qalstr[state.curqso].qso_name EQ civstr.qso_name AND $
                 qalstr[state.curqso].zabs EQ civstr.zabs, nig)
  inbad = where(qalstr[state.curqso].qso_name EQ badciv.qso_name AND $
                 qalstr[state.curqso].zabs EQ badciv.zabs, nib)

  while(nig NE 0 OR nib NE 0) do begin
    state.curqso = state.curqso + 1
    if state.curqso EQ state.nqal then begin
      sdss_chkciv_svciv, state
      widget_control, ev.top, /destroy
      return
    endif
    ingood = where(qalstr[state.curqso].qso_name EQ civstr.qso_name AND $
                   qalstr[state.curqso].zabs EQ civstr.zabs, nig)
    inbad = where(qalstr[state.curqso].qso_name EQ badciv.qso_name AND $
                   qalstr[state.curqso].zabs EQ badciv.zabs, nib)
  endwhile

  ;; Read data
  file_name = getenv('SDSSPATH')+strtrim(qalstr[state.curqso].sdss_obs[0],2)
;  strput, file_name, getenv('SDSSPATH')
  parse_sdss, file_name, fx, wv, conti, SIG=sig, NPIX=npix, CDIR=state.con_dir

  ;; Set zabs
  state.zabs = qalstr[state.curqso].zabs
  state.zqso = qalstr[state.curqso].z_qso

  ;; Set EW
  state.ew = qalstr[state.curqso].ew[1]

  ;; xymnx
  state.xymnx[0] = 1215.6701*(1.+state.zabs) - 200.
  state.xymnx[2] = 1215.6701*(1.+state.zabs) + 200.
  gd = where(wv GT state.xymnx[0] AND wv LT state.xymnx[2], ngd)
  
  if ngd GT 1 then begin
      srt = sort(fx[gd])
      ymd = fx[gd[srt[round(0.9*ngd)<(ngd-1)]]]
  endif else ymd = 0.
  state.xymnx[1] = -1.
  state.xymnx[3] = ymd*1.5
  state.svxymnx = state.xymnx

  ;; MGII 
  state.civ_lin.N = 20.3
  state.civ_lin.b = 30.
  state.civ_lin.zabs = state.zabs

  ;; Fit
  state.civ_conti = ymd
  fit = replicate(state.civ_conti, npix)

  ;; Set flg
  gd = where(state.velplt.wrest*(state.zabs+1) GT min(wv) AND $
             state.velplt.wrest*(state.zabs+1) GT (state.zqso+1.)*1215.6701 AND $
             state.velplt.wrest*(state.zabs+1) LT max(wv), na)
  if na EQ 0 then stop
  state.velplt[*].flg = 0
  state.nplt = na
  state.velplt[gd].flg = 1

  ;; Just do the plots
  state.all_velo[*,gd] = x_allvelo(wv, state.zabs, $
                                   state.velplt[gd].wrest,$
                                   state.vmnx, $
                                   all_pmnx=all_pmnx, NPIX=5000L)
  state.all_pmnx[*,gd] = all_pmnx


  return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set Lines
pro sdss_chkciv_llist, state

  llist = getenv('XIDL_DIR')+'/SDSS/CIV/sdss_civ.lst'
  lines = x_setllst(llist, 0)
  state.ntrans = n_elements(lines)
  state.velplt[0:state.ntrans-1].wrest = lines.wave
  state.velplt[0:state.ntrans-1].name = lines.name
  delvarx, lines
  state.velplt[0:state.ntrans-1].ymnx = [-0.11, 1.39]

  weak = where(abs(state.velplt.wrest - 1808.0130d) LT 0.01 OR $
               abs(state.velplt.wrest - 1611.2005d) LT 0.01 OR $
               abs(state.velplt.wrest - 2026.136d) LT 0.01 OR $
               abs(state.velplt.wrest - 2260.7805d) LT 0.01, nwk)
  
  if nwk NE 0 then state.velplt[weak].ymnx = [0.7, 1.1]
               

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Next
pro sdss_chkciv_next, state
  common sdss_chkciv_cmm

  state.curqso = state.curqso + 1 

  if state.curqso EQ state.nqal then begin
    sdss_chkciv_svciv, state
    widget_control, ev.top, /destroy
    return
  endif

  ingood = where(qalstr[state.curqso].qso_name EQ civstr.qso_name AND $
                 qalstr[state.curqso].zabs EQ civstr.zabs, nig) 
  inbad = where(qalstr[state.curqso].qso_name EQ badciv.qso_name AND $
                 qalstr[state.curqso].zabs EQ badciv.zabs, nib) 

  while(nig NE 0 OR nib NE 0) do begin
    state.curqso = state.curqso + 1 
    if state.curqso EQ state.nqal then begin
      sdss_chkciv_svciv, state
      widget_control, ev.top, /destroy
      return
    endif
    ingood = where(qalstr[state.curqso].qso_name EQ civstr.qso_name AND $
                   qalstr[state.curqso].zabs EQ civstr.zabs, nig) 
    inbad = where(qalstr[state.curqso].qso_name EQ badciv.qso_name AND $
                   qalstr[state.curqso].zabs EQ badciv.zabs, nib) 
  endwhile

  return

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Next
pro sdss_chkciv_updinfo, state
  common sdss_chkciv_cmm

  ;; Namej
  widget_control, state.name_id, $
    set_value=strtrim(qalstr[state.curqso].qso_name,2)

  ;; RA, DEC
  widget_control, state.mag_id, set_value=qalstr[state.curqso].rmag
  widget_control, state.ra_id, set_value=qalstr[state.curqso].ra
  widget_control, state.dec_id, set_value=qalstr[state.curqso].dec
  widget_control, state.ew_id, set_value=qalstr[state.curqso].ew[1]

  ;; zabs
  widget_control, state.zabs_id, set_value=state.zabs

  return

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set BAD MGII structure
pro sdss_chkciv_setbad, state

  common sdss_chkciv_cmm

  if badciv[0].zabs NE 0 then begin
    tmp = { sdssmgiistrct }
    badciv = [badciv, tmp]
  endif
  nciv = n_elements(badciv)
  state.curciv = nciv-1
  state.flg_new = 1
  ;; Name, etc
  badciv[state.curciv].qso_name = qalstr[state.curqso].qso_name
  badciv[state.curciv].ra = qalstr[state.curqso].ra
  badciv[state.curciv].dec = qalstr[state.curqso].dec
  badciv[state.curciv].rmag = qalstr[state.curqso].rmag
  badciv[state.curciv].sdss_obs = qalstr[state.curqso].sdss_obs
  badciv[state.curciv].z_qso = qalstr[state.curqso].z_qso
  badciv[state.curciv].zabs = qalstr[state.curqso].zabs
  badciv[state.curciv].ew = qalstr[state.curqso].ew
  badciv[state.curciv].sigew = qalstr[state.curqso].sigew
  badciv[state.curciv].wrest = qalstr[state.curqso].wrest

  return

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set MGII structure
pro sdss_chkciv_setciv, state, GOOD=good, DEFINITE=definite, MAYBE=maybe

  common sdss_chkciv_cmm

  if keyword_set(DEFINITE) then flg_civ = 3
  if keyword_set(GOOD) then flg_civ = 2
  if keyword_set(MAYBE) then flg_civ = 1
  if not keyword_set(flg_civ) then stop

  if civstr[0].zabs NE 0 then begin
    tmp = { sdssmgiistrct }
    civstr = [civstr, tmp]
endif

  nciv = n_elements(civstr)
  state.curciv = nciv-1
  state.flg_new = 1

  ;; Name, etc
  civstr[state.curciv].qso_name = qalstr[state.curqso].qso_name
  civstr[state.curciv].ra = qalstr[state.curqso].ra
  civstr[state.curciv].dec = qalstr[state.curqso].dec
  civstr[state.curciv].rmag = qalstr[state.curqso].rmag
  civstr[state.curciv].sdss_obs = qalstr[state.curqso].sdss_obs
  civstr[state.curciv].z_qso = qalstr[state.curqso].z_qso
  civstr[state.curciv].zabs = qalstr[state.curqso].zabs
  civstr[state.curciv].ew = qalstr[state.curqso].ew
  civstr[state.curciv].sigew = qalstr[state.curqso].sigew
  civstr[state.curciv].wrest = qalstr[state.curqso].wrest

  ;; Flag
  civstr[state.curciv].ew[49] = flg_civ

  return

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro sdss_chkciv_svciv, state

  common sdss_chkciv_cmm

  mwrfits, civstr, state.civfil, /create
  mwrfits, badciv, state.badcivfil, /create
  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; MAIN PROGRAM ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sdss_chkciv, 'sdss_DR3_CIV.fits', 'civ_good.fits', 'civ_bad.fits',
; getenv('SDSSPATH')+'DR3_QSO/ABSLIN/'
pro sdss_chkciv, qalfil, civfil, badcivfil, con_dir, IQSO=iqso, FNEW=fnew, $
              XSIZE=xsize, L_YSIZE=i_ysize, M_YSIZE=s_ysize, $
              YSIZE=ysize, OBJNM=objnm, ZIN=zin, XMAX=xmax, LLIST=llist

  common sdss_chkciv_cmm
;
  if  N_params() LT 3  then begin 
    print,'Syntax - ' + $
      'sdss_chkciv, qalfil, civfil, badcivfil, con_dir, /FNEW'
    print, '        I_YSIZE=, S_YSIZE= [v1.0]'
    return
  endif 

;  Optional Keywords

  device, get_screen_size=ssz
  if not keyword_set( XSIZE ) then    xsize = ssz[0]-200
  if not keyword_set( YSIZE ) then    ysize = ssz[1]-100
  if not keyword_set( MINVAL ) then minval = 6.
  if not keyword_set( IQSO ) then iqso = 0L

; Initialize the common blcok
  sdss_chkciv_icmmn, qalfil, civfil, badcivfil

  tmp = { velpltstrct }
  tmp2 = { abslinstrct }

; STATE

  state = {             $
            nqal: 0L, $
            curqso: iqso, $
            curqal: 0, $
            curciv: 0L, $
            civfil: civfil, $
            badcivfil: badcivfil, $
            flg_new: 0, $
            zabs: 0., $
            zqso: 0., $
            con_dir: '', $
            minval: minval, $
            ntrans: 0L, $       ; PLOTTING LINES
            vmnx: [-800., 800.], $
            nplt: 0, $
            civ_lin: tmp2, $
            civ_conti: 0., $
            all_velo: dblarr(5000, 300), $  
            all_pmnx: lonarr(3, 300), $  
            velplt: replicate(tmp, 300), $
            xpos: 0.0, $
            ypos: 0.0, $
            ipress: 0L, $
            pos: [0.1,0.1,0.95,0.95], $ ; Plotting
            flg_zoom: 0, $
            psfile: 0, $
            help: strarr(50), $
            svxymnx: fltarr(4), $
            xymnx: fltarr(4), $
            tmpxy: fltarr(4), $
            xcurs: 0., $
            ycurs: 0., $
            size: lonarr(2), $
            base_id: 0L, $      ; Widgets
            ldraw_id: 0L, $    ; Lya
            ltext_id: 0L, $    ; 
            ldrawbase_id: 0L, $
            fxval_id: 0L, $
            iwvval_id: 0L, $
            mdraw_id: 0L, $       ; Spec Window
            mdrawbase_id: 0L, $
            swvval_id: 0L, $
            zabs_id: 0L, $
            xmax_id: 0L, $
            name_id: 0L, $
            nspec_id: 0L, $
            pmin_id: 0L, $
            pmax_id: 0L, $
            lines_id: 0L, $
            top_id: 0L, $
            tdraw_id: 0L, $
            bottom_id: 0L, $
            rhs_id: 0L, $
            info_id: 0L, $
            quality_id: 0L, $
            scr1_id: 0L, $
            scr2_id: 0L, $
            hits_id: 0L, $
            NHI_id: 0L, $
            NHIb_id: 0L, $
            mtl_id: 0L, $
            stat_id: 0L, $
            ra_id: 0L, $
            dec_id: 0L, $
            mag_id: 0L, $
            ew_id: 0L, $
            help_text_id: 0L, $
	    ew: 0L$
          }

;;;;;;;;;;;;;;
; SETUP LINES
  sdss_chkciv_llist, state
  state.civ_lin = x_setline(1215.6701d)

; Other setup
  state.nqal = n_elements(qalstr)
  state.con_dir = con_dir

;    WIDGET
  base = WIDGET_BASE( title = 'sdss_chkciv: Check spectra', /column, $
                    UNAME='BASE', /tlb_size_events, xoffset=200L)
  state.base_id = base
  

  state.top_id = WIDGET_BASE( state.base_id, /column, $
                              /base_align_center,/align_center, $
                              xsize=xsize, ysize=round(1*ysize/3.), $
                              uvalue='TOP_BASE', frame=2)
  state.tdraw_id = widget_draw(state.top_id, xsize=xsize, $
                              ysize=round(ysize/3), /frame, retain=2, $
                              uvalue='TDRAW')
  state.bottom_id = WIDGET_BASE( state.base_id, /row, $
                              /base_align_center,/align_center, $
                              xsize=xsize, ysize=round(2*ysize/3.), $
                              uvalue='TOP_BASE', frame=2)

;;;;;; Info window ;;;;;;;;;;;
  state.info_id = $
    WIDGET_BASE( state.bottom_id, /column, /base_align_center,/align_center, $
                 uvalue='INFO_BASE', frame=2, xsize=xsize/3.)
;               ysize=round(ysize/3.))
  ;; Info
  civinf = widget_base(state.info_id, /row, /align_center, frame=2)
  state.name_id = cw_field(civinf, title='Obj ', value=' ', xsize=18)
  state.zabs_id = cw_field(civinf, title='zabs: ', value=state.zabs, xsize=7)
  state.ew_id = cw_field(civinf, title='EW: ', value=state.ew, xsize=7)

  ;; RA, DEC
  radeci = widget_base(state.info_id, /row, /align_center, frame=2)
  state.mag_id = cw_field(radeci, title='MAG: ', value=0., xsize=10)
  state.ra_id = cw_field(radeci, title='RA: ', value=0., xsize=10)
  state.dec_id = cw_field(radeci, title='DEC: ', value=0., xsize=10)

  ;; Lya
  lyainf = widget_base(state.info_id, /row, /align_center, frame=2)

;      BUTTONS
  butbase = widget_base(state.info_id, /row, /align_center, frame=2)
  defi = WIDGET_BUTTON(butbase, value='DEFINITE',uvalue='DEFINITE')
  good = WIDGET_BUTTON(butbase, value='GOOD',uvalue='GOOD')
  maybe = WIDGET_BUTTON(butbase, value='MAYBE',uvalue='MAYBE')
  bad  = WIDGET_BUTTON(butbase, value='BAD', uvalue='BAD')
  butbase2 = widget_base(state.info_id, /row, /align_center, frame=2)
  save = WIDGET_BUTTON(butbase2, value='SAVE', uvalue='SAVE')
  done = WIDGET_BUTTON(butbase2, value='DONE',uvalue='DONE')
  splt = WIDGET_BUTTON(butbase2, value='SPLT',uvalue='SPLT')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      Metals DRAW
  state.mdrawbase_id = $
    WIDGET_BASE( state.bottom_id, /row, /base_align_center,/align_center, $
               uvalue='SDRAW_BASE', frame=2)

  state.mdraw_id = widget_draw(state.mdrawbase_id, xsize=round(xsize*2./3), $
                              ysize=round(2.*ysize/3), /frame, retain=2, $
                              uvalue='SDRAW')


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;        Help
  strhelp = strarr(50)
  strhelp = ['   Help Menu   ',$
             'LMB - Truncate/Extend trace', $ 
             'RMB - Contrast/Brightness', $
             'CMB/CMB - Zoom' $ 
             ]
;  help_text_id = widget_text(toolbar, value=strhelp, xsize=30, ysize=4,$
;                             /scroll)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Realize
  WIDGET_CONTROL, base, /realize

  ; Set qso and qal
  if qalstr[0].zabs[0] EQ 0. then sdss_chkciv_next, state

  ; Load data
  sdss_chkciv_setup, state

  ; PLOT
  sdss_chkciv_update, state
  
  WIDGET_CONTROL, base, set_uvalue = state, /no_copy

; Send to the xmanager
  xmanager, 'sdss_chkciv', base
  delvarx, fx, wv, npix, sig, qalstr

  return
end
	
