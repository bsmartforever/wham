;+ 
; NAME:
; sdss_fixciv
;    Version 1.0
;
; PURPOSE:
;   Visually check SDSS CIV absorption detected with sdss_fndciv
;   Edit EW and continuum as desired
;
; CALLING SEQUENCE:
;   
;   sdss_fixciv, 
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
;   sdss_fixciv 
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   1-Dec-2008 Written by JXP
;-
;------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;

pro sdss_fixciv_icmmn, civfil, ROOT=root

  common sdss_fixciv_cmm, $
    npix, $
    fx, $
    wv, $
    sig, $
    sv_conti, $
    conti, $
    fit_px, $
    fit_fx, $
    fit_wv, $
    dlastr, $
    civstr

  civstr = xmrdfits(civfil,1)

  return
end
  

;;;;
; Events
;;;;

pro sdss_fixciv_event, ev

  common sdss_fixciv_cmm

  WIDGET_CONTROL, ev.top, get_uvalue = state, /no_copy
  WIDGET_CONTROL, ev.id, get_uvalue = uval

  case uval of
      'IVALU' : begin
          widget_control, state.ivalu_id, get_value=tmp
          state.cursi2 = tmp
          sdss_fixciv_update, state, FLG=flg
      end
      'HSEW' : begin
          widget_control, state.hsew_id, get_value=tmp
          state.sighandEW = tmp
      end
      'HEW' : begin
          widget_control, state.hew_id, get_value=tmp
          state.handEW = tmp
      end
      'SPLT': x_specplot, fx, sig, wave=wv, zin=state.zabs, /lls, /block, $
        inflg=4
      'BACK': sdss_fixciv_back, state
      'BOX': sdss_fixciv_next, state, /BOX
      'GAUSS': sdss_fixciv_next, state, /GAUSS
      'HAND': sdss_fixciv_next, state, /HAND
      'NG': sdss_fixciv_next, state, /NG
      'DONE' : begin
          sdss_fixciv_svciv, state
          widget_control, ev.top, /destroy
          return
      end
      'SAVE' : begin
          print, 'sdss_fixciv: Saving..'
          sdss_fixciv_svciv, state
          print, 'sdss_fixciv: Done!'
      end
      'DRAW_BASE' : begin
          if ev.enter EQ 0 then begin ; Turn off keyboard
              widget_control, state.text_id, sensitive = 0
          endif
          WIDGET_CONTROL, state.base_id, set_uvalue = state, /no_copy
          return
      end
      'SI2DRAW' : begin
          widget_control, state.text_id, /sensitive, /input_focus ; Turn on keyb
          case ev.type of
              0 : begin ; Button press
                  mn = min(abs(wv- xgetx_plt(state, /strct)),imn)
                  mn = min(abs(wv-(state.zabs+1)*1548.195),icen)
                  case ev.press of
                      1 : begin
                          state.boxlim[0]= (imn < (icen-2))
                          civstr[state.cursi2].ew[39] = state.boxlim[0]
                      end
;                      2 : begin
;                          state.zabs = xgetx_plt(state, /strct)),imn)
;                          civstr[state.cursi2].sigew[39] = state.boxlim[1]
;                      end
                      4 : begin
                          state.boxlim[1]= (imn > (icen+2))
                          civstr[state.cursi2].sigew[39] = state.boxlim[1]
                      end
                      else: 
                  endcase
                  sdss_fixciv_update, state, flg=1, /noset
              end
              1 : begin ; Button Release
                  WIDGET_CONTROL, state.base_id, set_uvalue = state,  /no_copy
                  return
              end
              2 : begin ; Motion event
                  state.xcurs = ev.x
                  state.ycurs = ev.y
                  state.xpos = xgetx_plt(state, /strct)
                  state.ypos = xgety_plt(state, /strct)
                  WIDGET_CONTROL, state.base_id, set_uvalue = state, /no_copy
                  return
              end
          endcase
      end
      'TEXT' : begin
          eventch = string(ev.ch)
          case eventch of
              'b': state.xymnx[1] = xgety_plt(state, /strct) ; bottom
              'Z': state.xymnx[1] = 0.0 ; Set ymin to 0.0
              'l': state.xymnx[0] = xgetx_plt(state, /strct) ; left
              'r': state.xymnx[2] = xgetx_plt(state, /strct) ; right
              't': state.xymnx[3] = xgety_plt(state, /strct) ; top
              'T': state.xymnx[3] = 1.1 ; Set ymax to 1.1
              ;; Continuum
              '3': sdss_fixciv_continuum, state, 1L ;; Add point
              '4': sdss_fixciv_continuum, state, 2L ;; Move a point
              ; ZOOMING
              'i': state.flg_zoom = (state.flg_zoom - 1) > 0
              'o': state.flg_zoom = (state.flg_zoom + 1) 
              'w': state.xymnx = state.svxymnx ; Reset the screen
              ;; Continuum
;              'c': conti = replicate(xgety_plt(state, /strct), npix)
              ; Set EW value
              'g': sdss_fixciv_next, state, /GAUSS
              'B': sdss_fixciv_next, state, /BOX
              'n': sdss_fixciv_next, state, /NG
              'L': sdss_fixciv_next, state, /LIMIT
              'O': sdss_fixciv_next, state, /OI
              ;;
              ;; Reset redshift by the Gaussian
              'z': begin
                  widget_control, /hourglass
                  state.zabs = state.zgauss
                  sdss_fixciv_update, state, flg=1, /NOZ
              end
              ;; Specplot
              'p': x_specplot, fx, sig, wave=wv, zin=state.zabs, /lls, $
                               /block, inflg=4
              'P': x_specplot, fx, sig, wave=wv, zin=state.z2, /lls, $
                               /block, inflg=4  ;; Plot assuming OI
              else:  print, 'sdss_fixciv: Not a valid key!' ; Nothing
          endcase
          sdss_fixciv_update, state, flg=1, /noset
      end


      else :
  endcase

  WIDGET_CONTROL, state.base_id, set_uvalue = state, /no_copy
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;  Metals Plot -- At z = zabs for CIV
;;;;;;;;;;;;;;;;;;;;


pro sdss_fixciv_Metals, state
  
  common sdss_fixciv_cmm

  ;; Set plot window
  if state.psfile NE 1 then begin
      widget_control, state.mdraw_id, get_value=wind
      wset, wind
  endif

  clr = getcolor(/load)
  
  gdlin = where((state.velplt.flg MOD 2) EQ 1, ngd)

;  ny = ngd / 2 + (ngd MOD 2 EQ 1)
  ny = ngd 

  ;; Good lines
;  !p.multi = [0,2,ny,0,1]
  !p.multi = [0,1,ny,0,1]
  for j=0L,ngd-1 do begin
      i = gdlin[j]
      pixmin = state.all_pmnx[0,i]
      pixmax = state.all_pmnx[1,i]

      ;; Plot
;      if (j NE ny-1) AND (j NE ngd-1 ) then begin
      if (j NE ny-1) then begin
          spaces = replicate('!17 ',30)
          plot, state.all_velo[0:state.all_pmnx[2,i],i], $
            fx[pixmin:pixmax]/sv_conti[pixmin:pixmax], xrange=state.vmnx, $
            yrange=state.velplt[i].ymnx, xtickn=spaces, xmargin=[9,3], $
            ymargin=[0,0], $
            charsize=1.8, psym=10, background=clr.white, color=clr.black, $
            xstyle=1, ystyle=1
      endif else begin
          plot, state.all_velo[0:state.all_pmnx[2,i],i], $
            fx[pixmin:pixmax]/sv_conti[pixmin:pixmax], xrange=state.vmnx, $
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

  ;; CIV Plot
  !p.multi = [0,1,1]
  widget_control, state.si2_id, get_value=wind
  wset, wind
  wcen = 1548.195 * (1+state.zabs)
  wcen2 = 1550.770 * (1+state.zabs)
  wcen3 = 1548.195 * (1+state.zqso)
  mn = min(abs(wv-wcen),cpix)
  npp = 41*(state.flg_zoom + 1)
  ppx = lindgen(npp) + cpix - (npp-1)/2
  state.xymnx[0] = min(wv[ppx],max=mx)
  state.xymnx[2] = mx
  plot, wv[ppx], fx[ppx], color=clr.black, background=clr.white, psym=10, $
    position=state.pos, xrange=[state.xymnx[0],state.xymnx[2]], $
    yrange=[state.xymnx[1],state.xymnx[3]], xstyle=1, ystyle=1
  oplot, wv[ppx], conti[ppx], color=clr.red, linesty=1, thick=3

  oplot, wv[fit_px], (1.-fit_fx)*conti[fit_px], color=clr.blue

  oplot, replicate(wcen,2), [-9e9,9e9], color=clr.black, linestyle=2, thick=3
  oplot, replicate(wcen2,2), [-9e9,9e9], color=clr.black, linestyle=3, thick=1
  oplot, replicate(wcen3,2), [-9e9,9e9], color=clr.green, linestyle=2, thick=3

  ;; Continuum
  ;; Points
  if state.cstr.npts NE 0 then begin
      gdc = where(state.cstr.msk EQ 1)
      oplot, [state.cstr.xval[gdc]], [state.cstr.yval[gdc]], psym=1, $
        color=clr.cyan, symsize=5, thick=3
  endif

  ;; CIV
;  contam = [1548.195, 1550.770, 2796.352, 2803.531]
;  ncon = n_elements(contam)
;  for ss=0L,(ncon/2)-1,2 do begin
;      ;; Lower
;      zc = wcen / contam[ss] - 1
;      w2 = (1+zc)*contam[ss+1]
;      oplot, replicate(w2,2), [-9e9,9e9], color=clr.orange, linestyle=2, thick=3
      ;; Upper
;      zc = wcen / contam[ss+1] - 1
;      w2 = (1+zc)*contam[ss]
;      oplot, replicate(w2,2), [-9e9,9e9], color=clr.orange, linestyle=2, thick=3
;  endfor

  oplot, replicate(wv[state.boxlim[0]],2), [-9e9,9e9], $
         color=clr.orange, linestyle=2, thick=3
  oplot, replicate(wv[state.boxlim[1]],2), [-9e9,9e9], $
         color=clr.orange, linestyle=2, thick=3
  ;; EW
  xyouts, wcen+3., (min(fit_fx)-0.5)>0., 'EW: '+$
          string(state.gaussEW, format='(f7.3)')+' '+$
          string(state.boxEW, format='(f7.3)')+' '+$
          string(civstr[state.cursi2].EWflg,format='(i2)'), color=clr.black, $
          charsiz=2.

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;  Metals Plot -- At z = zabs for OI/SiII
;;;;;;;;;;;;;;;;;;;;


pro sdss_fixciv_FPMetals, state
  
  common sdss_fixciv_cmm

  ;; Set plot window
  if state.psfile NE 1 then begin
      widget_control, state.fpdraw_id, get_value=wind
      wset, wind
  endif

  clr = getcolor(/load)
  
  gdlin = where((state.fp_velplt.flg MOD 2) EQ 1, ngd)
  if ngd EQ 0 then return
  ngd = ngd < 2

;  ny = ngd / 2 + (ngd MOD 2 EQ 1)
  ny = ngd 

  ;; Good lines
;  !p.multi = [0,2,ny,0,1]
  !p.multi = [0,1,ny,0,1]
  for j=0L,ngd-1 do begin
      i = gdlin[j]
      pixmin = state.fp_all_pmnx[0,i]
      pixmax = state.fp_all_pmnx[1,i]

      ;; Plot
;      if (j NE ny-1) AND (j NE ngd-1 ) then begin
;      if (j NE ny-1) then begin
          spaces = replicate('!17 ',30)
          plot, state.fp_all_velo[0:state.fp_all_pmnx[2,i],i], $
            fx[pixmin:pixmax]/sv_conti[pixmin:pixmax], xrange=state.vmnx, $
            yrange=state.fp_velplt[i].ymnx, xtickn=spaces, xmargin=[6,1], $
            ymargin=[0,0], $
            charsize=1.4, psym=10, background=clr.white, color=clr.black, $
            xstyle=1, ystyle=1
;      endif else begin
;          plot, state.fp_all_velo[0:state.fp_all_pmnx[2,i],i], $
;            fx[pixmin:pixmax]/conti[pixmin:pixmax], xrange=state.vmnx, $
;            yrange=state.fp_velplt[i].ymnx, xmargin=[9,3], ymargin=[3,0], $
;            charsize=1.8, psym=10, background=clr.white, color=clr.black, $
;            xstyle=1, ystyle=1
;      endelse

      ;; Labels
      xyouts, 0.07*(state.vmnx[1]-state.vmnx[0])+state.vmnx[0], $
        state.fp_velplt[i].ymnx[0]+ $
        (state.fp_velplt[i].ymnx[1]-state.fp_velplt[i].ymnx[0])*0.05, $
        strtrim(state.fp_velplt[i].name,2), $
        color=clr.black, charsize=1.5
      
      ;; Lines
      oplot, [0., 0.], state.fp_velplt[i].ymnx, color=clr.blue, linestyle=2
      oplot, [-10000., 10000.], [0.,0.], color=clr.green, linestyle=3
      oplot, [-10000., 10000.], [1.,1.], color=clr.green, linestyle=3
  endfor
  !p.multi = [0,1,1]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;  Continuum
;;;;;;;;;;;;;;;;;;;;

pro sdss_fixciv_continuum, state, flg

  common sdss_fixciv_cmm
  if not keyword_set( flg ) then return

; CASE

  case flg of 
      0: stop ;  RESET to 1/2
      1: begin ; Set point
          if state.cstr.npts EQ 9 then return
          i = state.cstr.npts 
          state.cstr.xval[i] = state.xpos
          state.cstr.yval[i] = state.ypos
          state.cstr.msk[i] = 1L
          state.cstr.npts =  state.cstr.npts  + 1
      end
      2: begin ; Move point
          if state.cstr.npts EQ 0 then return
          gd = where(state.cstr.msk EQ 1)
          mn = min(abs(state.cstr.xval[gd]-state.xpos), imn)
          state.cstr.yval[gd[imn]] = state.ypos
          state.cstr.xval[gd[imn]] = state.xpos
      end
      3: ; Do nothing just respline
      else: stop
  endcase

  ;; Save
  civstr[state.cursi2].EW[40+lindgen(state.cstr.npts)] = $
    state.cstr.xval[lindgen(state.cstr.npts)]
  civstr[state.cursi2].sigEW[40+lindgen(state.cstr.npts)] = $
    state.cstr.yval[lindgen(state.cstr.npts)]

  ;; Redo the continuum
  if state.cstr.npts NE 0 then begin
      gdmsk = where(state.cstr.msk EQ 1, nmsk)
      if nmsk LT 3 then conti[*] = mean(state.cstr.yval[gdmsk]) $
      else begin ;; SPLINE
          mn = min(state.cstr.xval[gdmsk], imn)
          ;; Low
          low = where(wv LE mn, nlow)
          if nlow NE 0 then conti[low] = state.cstr.yval[gdmsk[imn]]
          ;; High
          mx = max(state.cstr.xval[gdmsk], imx)
          high = where(wv GE mx, nhigh)
          if nhigh NE 0 then conti[high] = state.cstr.yval[gdmsk[imx]]
          ;; Interp
          gd = where(wv GT mn AND wv LT mx, ngd)
          if ngd NE 0 then begin
              ;; SORT
              srt = sort(state.cstr.xval[gdmsk])
              swv = sort(wv[gd])
              twv = wv[gd[swv]]
              conti[gd[swv]] = xspline(state.cstr.xval[gdmsk[srt]], $
                                             state.cstr.yval[gdmsk[srt]], $
                                             twv)
              ;; Update Fit
;         if state.nlin NE 0 then x_fitline_updfit, state, EXACT=state.exact
          endif
      endelse   
  endif
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
; UPDATE LYA+METALS
pro sdss_fixciv_update, state, FLG=flg, NOSET=noset, NOZ=noz
  if not keyword_set(NOSET) then sdss_fixciv_setup, state, flg, NOZ=noz
  if flg EQ 1 then begin
      sdss_fixciv_fitciv, state
      sdss_fixciv_boxcar, state
      sdss_fixciv_updinfo, state
      sdss_fixciv_Metals, state
      sdss_fixciv_FPMetals, state
  endif
  return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
; Setup data
pro sdss_fixciv_setup, state, flg, NOZ=noz

  common sdss_fixciv_cmm
    
  if not keyword_set(NOZ) then state.zabs = civstr[state.cursi2].zabs

  ;; Read in data
  plate = civstr[state.cursi2].plate
  fibid = civstr[state.cursi2].fiber
  sdss_objinf, [plate, fibid], filnm=datfil, FLGSDSS=flgsdss
  if FLGSDSS EQ 0 then begin
      print, 'You will need to get this one yourself (not DR5)!', plate, fibid
      print, 'sdss_objinf, [plate, fibid], filnm=datfil, /dr1'
      print, 'And set cdir! e.g. '
      print, 'cdir = getenv(''SDSSPATH'')+''DR3_QSO/ABSLIN/'
      stop
  endif else cdir = state.con_dir

  ;; Read
  parse_sdss, datfil, fx, wv, conti, sig=sig, ZQSO=zem, CDIR=cdir, NPIX=npix
  sv_conti = conti
  state.zqso = zem

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Set flg for other metals
  gd = where(state.velplt.wrest*(state.zabs+1) GT min(wv) AND $
             state.velplt.wrest*(state.zabs+1) GT (state.zqso+1.)*1215.6701 AND $
             state.velplt.wrest*(state.zabs+1) LT max(wv), na)
  flg = 0
  if na EQ 0 then return
  state.velplt[*].flg = 0
  state.nplt = na
  state.velplt[gd].flg = 1

  ;; Just do the plots
  state.all_velo[*,gd] = x_allvelo(wv, state.zabs, $
                                   state.velplt[gd].wrest,$
                                   state.vmnx, $
                                   all_pmnx=all_pmnx, NPIX=5000L)
  state.all_pmnx[*,gd] = all_pmnx


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Boxcar
  mn = min(abs(wv-(state.zabs+1)*1548.195),imn)
  state.boxlim[0] = imn-4
  state.boxlim[1] = imn+4

  ;; xymnx
  state.xymnx[0] = wv[(imn-20)>0]
  state.xymnx[2] = wv[(imn+20)<(npix-1)]
  gd = where(wv GT state.xymnx[0] AND wv LT state.xymnx[2], ngd)
  
  ;; Flush/fix continuum points
  state.cstr.npts = 0
  state.cstr.xval = 0.
  state.cstr.yval = 0.
  state.cstr.msk = 0
  cpts = where(civstr[state.cursi2].EW[40+lindgen(9)] GT 0., nconti)
  if nconti GT 0 then begin
      state.cstr.xval[lindgen(nconti)] = civstr[state.cursi2].EW[40+cpts]
      state.cstr.yval[lindgen(nconti)] = civstr[state.cursi2].sigEW[40+cpts]
      state.cstr.msk[lindgen(nconti)] = 1
      state.cstr.npts = nconti
      sdss_fixciv_continuum, state, 3L ;; Update the continuum
  endif

  ;; Recover fit limits
  if civstr[state.cursi2].ew[39] GT 0. then $
    state.boxlim[0] = civstr[state.cursi2].ew[39] 
  if civstr[state.cursi2].sigew[39] GT 0. then $
    state.boxlim[1] = civstr[state.cursi2].sigew[39] 

  if ngd GT 1 then begin
      srt = sort(fx[gd])
      ymd = fx[gd[srt[round(0.9*ngd)<(ngd-1)]]]
  endif else ymd = 0.
  state.xymnx[1] = -1.
  state.xymnx[3] = ymd*1.5
  state.svxymnx = state.xymnx

  flg = 1

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Set flg for other metals
  z2 = (state.zabs+1)*1548.195 / 1302.1685 - 1
  state.z2 = z2
  gd = where(state.fp_velplt.wrest*(z2+1) GT min(wv) AND $
             state.fp_velplt.wrest*(z2+1) GT $
             (state.zqso+1.)*1215.6701 AND $
             state.fp_velplt.wrest*(z2+1) LT max(wv), na)
  if z2 GT (state.zqso + 0.1) then na = 0
  state.fp_nplt = na
  if na EQ 0 then return
  state.fp_velplt[*].flg = 0
  state.fp_nplt = na
  state.fp_velplt[gd].flg = 1

  ;; Just do the plots
  state.fp_all_velo[*,gd] = x_allvelo(wv, z2, $
                                   state.fp_velplt[gd].wrest,$
                                   state.vmnx, $
                                   all_pmnx=all_pmnx, NPIX=5000L)
  state.fp_all_pmnx[*,gd] = all_pmnx
  return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Set Lines
pro sdss_fixciv_llist, state

  llist = getenv('XIDL_DIR')+'/SDSS/CIV/sdss_civ.lst'
  lines = x_setllst(llist, 0)

  ;; 
  state.ntrans = n_elements(lines)
  state.velplt[0:state.ntrans-1].wrest = lines[0:state.ntrans-1].wave
  state.velplt[0:state.ntrans-1].name = lines[0:state.ntrans-1].name
  state.velplt[0:state.ntrans-1].ymnx = [-0.11, 1.39]

  weak = where(abs(state.velplt.wrest - 1808.0130d) LT 0.01 OR $
               abs(state.velplt.wrest - 1611.2005d) LT 0.01 OR $
               abs(state.velplt.wrest - 2026.136d) LT 0.01 OR $
               abs(state.velplt.wrest - 2260.7805d) LT 0.01, nwk)
  
  if nwk NE 0 then state.velplt[weak].ymnx = [0.7, 1.1]

  ;; (Skip CIV) Setup for OI/SiII false-positive
  gdlin = [2,5,6,7,8,9,10]
  nt = n_elements(gdlin)
  state.fp_velplt[0:nt-1].wrest = lines[gdlin].wave
  state.fp_velplt[0:nt-1].name = lines[gdlin].name
  state.fp_velplt[0:nt-1].ymnx = [-0.11, 1.39]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Next
pro sdss_fixciv_next, state, BOX=box, GAUSS=gauss, HAND=hand, NG=ng, $
  NEW=new, LIMIT=limit, OI=OI

  common sdss_fixciv_cmm

  widget_control, /hourglass

  iion = 0L
  ;;;;; 1548 ;;;;;

  ;; Boxcar
  if keyword_set(BOX) then begin
      civstr[state.cursi2].EWflg = 1
      civstr[state.cursi2].EW[iion] = state.boxEW
      civstr[state.cursi2].sigEW[iion] = state.sigboxEW
  endif

  ;; Gaussian
  if keyword_set(GAUSS) then begin
      civstr[state.cursi2].EWflg = 2
      civstr[state.cursi2].EW[iion] = state.gaussEW
      civstr[state.cursi2].sigEW[iion] = state.siggaussEW
      civstr[state.cursi2].zabs= state.zgauss
  endif

  ;; Hand
  if keyword_set(HAND) then begin
      civstr[state.cursi2].EWflg = 3
      civstr[state.cursi2].EW[iion] = state.handEW
      civstr[state.cursi2].sigEW[iion] = state.sighandEW
  endif

  ;; Lower LIMIT
  if keyword_set(LIMIT) then begin
      civstr[state.cursi2].EWflg = 4
      civstr[state.cursi2].EW[iion] = state.boxEW
      civstr[state.cursi2].sigEW[iion] = state.sigboxEW
  endif

  ;; OI/SiII
  if keyword_set(OI) then civstr[state.cursi2].EWflg = -2

  ;; NG
  if keyword_set(NG) then civstr[state.cursi2].EWflg = -1

  ;; Match?
  if keyword_set(NEW) then begin
      mt = where(civstr.EWflg EQ 0, nmt)
      if nmt EQ 0 then begin
          stop
          sdss_fixciv_svsi2, state
          stop
      endif
      state.cursi2 = mt[0]
  endif else state.cursi2 = (state.cursi2+1) < (n_elements(civstr)-1)

  ;; Proceed
  sdss_fixciv_update, state, FLG=flg

  return

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Back 
pro sdss_fixciv_back, state
  common sdss_fixciv_cmm

  ;; Match?
  state.cursi2 = (state.cursi2-1) > 0
  sdss_fixciv_update, state, FLG=flg

  return

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Next
pro sdss_fixciv_updinfo, state
  common sdss_fixciv_cmm

  ;; Name
;  widget_control, state.name_id, $
;    set_value=strtrim(dlastr[state.curdla].qso,2)

  ;; zabs
  widget_control, state.zabs_id, set_value=state.zabs
  widget_control, state.zqso_id, set_value=state.zqso
  widget_control, state.ivalu_id, set_value=state.cursi2
  
  ;; EW
  widget_control, state.ew_id, $
                  set_value=string(civstr[state.cursi2].ew[1],format='(f7.3)')+ $
                  ' '+string(civstr[state.cursi2].sigew[1],format='(f5.3)')
  ;; Box EW
  widget_control, state.boxew_id, $
                  set_value=string(state.boxew,format='(f7.3)')+' '+$
                  string(state.sigboxEW,format='(f5.3)')

  return

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Next
pro sdss_fixciv_boxcar, state
  common sdss_fixciv_cmm

  ;; Simple sum
  dwv = wv-shift(wv,1)

  np = state.boxlim[1] - state.boxlim[0] + 1
  sumpix = state.boxlim[0] + lindgen(np)
  if np NE 0 then begin
      dwv = wv[sumpix[np-1]+1] - wv[sumpix[np-1]]
      dwv = abs(dwv)
      state.boxEW = total(1.-(fx[sumpix]>0.)/conti[sumpix])*dwv / $
                    (1.+state.zabs)
      ;; ERROR
      sumvar = total((sig[sumpix]/conti[sumpix])^2)
      state.sigboxEW = sqrt(sumvar)*dwv/(1.+state.zabs)
  endif

  return

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro sdss_fixciv_svciv, state

  common sdss_fixciv_cmm

  widget_control, /hourglass
  mwrfits, civstr, state.civfil, /create
  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro sdss_fixciv_fitciv, state

  common sdss_fixciv_cmm


  rwv = 1548.195 
;  mn = min(abs(wv-(state.zabs+1)*rwv),cpix)
;  px = lindgen(15) + cpix - 7
;  subpx = lindgen(9) + cpix - 4
  px = long(state.boxlim[0]) + lindgen(abs(state.boxlim[1]-state.boxlim[0])+1)
  cpix = round(mean(state.boxlim))
  subpx = px
  fit_px = px

  if cpix EQ (npix-1) then begin
      print, 'Redshift too high! ', state.zabs
      return
  endif

  ;; Profile
  gprof = -1.*(fx[px] / conti[px] - 1.)

  wcen = wv[cpix]
  dwv = abs(wv[cpix]-wv[cpix+1])
  gsssig = 2.

  ;; FIT
  fit_wv = wv[px]
  fit_fx = x_gaussfit(fit_wv, gprof, acoeff, $
                    estimates=[max(gprof), wcen, gsssig], $
                    sigma=sigma, nterms=3, COVAR=covar, $
                    measure_errors=sig[px]/conti[px])

  ;; Save EW
  ewval = acoeff[0] * acoeff[2] * sqrt(!pi*2.) ; A
  sigew1 = sqrt(total( ((sig[subpx]/conti[subpx])*dwv)^2)) ; A
  sigew2 = sqrt(!pi*2.) * sqrt( (acoeff[0]*sigma[2])^2 + $
                                (acoeff[2]*sigma[0])^2 )
  state.zgauss = acoeff[1]/1548.195 - 1.

;      sigew = sigew1 
;      sigew = sigew2 
;      sigew = sigew1 > sigew2
  sigew = sigew1 < sigew2

  ;; Error checking (for a bad fit)
  ew_chk = total((1. - fx[px]/conti[px])*abs(dwv[px])) ;; Obs Ang
  if abs(ew_chk-ewval) GT 5.*sigew then begin
      print, 'Bad EW value!', ew_chk, ewval
      print, 'Probably a bad Gaussian fit'
      print, 'Setting EW to the lower value', ew_chk, ewval
      ewval = (ewval < ew_chk) > 0.
  endif

  state.gaussEW = ewval / (1.+state.zabs)
  state.siggaussEW = sigew / (1.+state.zabs)

;  if ewval LT -10 then stop
  print, civstr[state.cursi2].ew[1],  civstr[state.cursi2].sigew[1]
      
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
;; sdss_fixciv, 'dr7_civ_vetted.fits', /FNEW
pro sdss_fixciv, civfil, IQSO=iqso, FNEW=fnew, NOSTOP=nostop, $
              XSIZE=xsize, YSIZE=ysize, ROOT=root 

  common sdss_fixciv_cmm
;
  if  N_params() LT 1  then begin 
    print,'Syntax - ' + $
      'sdss_fixciv, civfil, /FNEW I_YSIZE=, S_YSIZE= [v1.0]'
    return
  endif 

  ;; Warning
  print, 'sdss_fixciv:  We are going to overwrite your original file!'
  print, 'sdss_fixciv:  Continue as you wish..'
  if not keyword_set(NOSTOP) then stop

;  Optional Keywords
  if not keyword_set(ROOT) then root = getenv('SDSSPATH')+'DR7_QSO/CIV/'
  if not keyword_set(CDIR) then cdir = getenv('SDSSPATH')+'DR7_QSO/ABSLIN/'
  if not keyword_set( PATH ) then path = getenv('SDSSPATH')+'/DR7_QSO/' 
  if not keyword_set( DATDIR ) then datdir = path+'spectro/1d_26/'

  device, get_screen_size=ssz
  if not keyword_set( XSIZE ) then    xsize = ssz[0]-200
  if not keyword_set( YSIZE ) then    ysize = ssz[1]-100
;  if not keyword_set( MINVAL ) then minval = 6.
  if not keyword_set( IQSO ) then iqso = 0L

; Initialize the common blcok
  sdss_fixciv_icmmn, civfil, ROOT=root

  tmp = { velpltstrct }
  tmp2 = { abslinstrct }

  tmp3 = { contistrct }

; STATE

  state = {             $
          nqal: 0L, $
          cursi2: iqso-1, $  ;; This will be incremented
          zabs: 0., $
          z2: 0., $ ;; False-positives of OI/SiII
          zqso: 0., $
          civfil: civfil, $
          boxlim: fltarr(2), $
          gaussEW: 0., $
          siggaussEW: 0., $
          boxEW: 0., $
          sigboxEW: 0., $
          handEW: 0., $
          sighandEW: 0., $
          con_dir: cdir, $
          datdir: datdir, $
          ntrans: 0L, $         ; PLOTTING LINES
          vmnx: [-800., 800.], $
          nplt: 0, $
          cstr: tmp3, $
          flg_redo: keyword_set(redo), $
          civ_lin: tmp2, $
          civ_conti: 0., $
          all_velo: dblarr(5000, 300), $  
          all_pmnx: lonarr(3, 300), $  
          velplt: replicate(tmp, 300), $
          fp_velplt: replicate(tmp, 30), $
          fp_all_velo: dblarr(5000, 30), $  
          fp_all_pmnx: lonarr(3, 30), $  
          fp_nplt: 0, $
          zgauss: 0., $
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
          base_id: 0L, $        ; Widgets
          ldraw_id: 0L, $       ; Lya
          text_id: 0L, $       ; 
          draw_base_id: 0L, $
          fxval_id: 0L, $
          iwvval_id: 0L, $
          mdraw_id: 0L, $       ; Spec Window
          fpdraw_id: 0L, $       ; Spec Window 2
          mdrawbase_id: 0L, $
          swvval_id: 0L, $
          zabs_id: 0L, $
          zqso_id: 0L, $
          xmax_id: 0L, $
          name_id: 0L, $
          nspec_id: 0L, $
          lines_id: 0L, $
          si2_id: 0L, $
          tdraw_id: 0L, $
          left_id: 0L, $
          right_id: 0L, $
          rhs_id: 0L, $
          info_id: 0L, $
          quality_id: 0L, $
          ew_id: 0L, $
          ivalu_id: 0L, $
          boxew_id: 0L, $
          hew_id: 0L, $
          hsew_id: 0L, $
          help_text_id: 0L $
          }

;;;;;;;;;;;;;;
; SETUP LINES
  sdss_fixciv_llist, state

;    WIDGET
  base = WIDGET_BASE( title = 'sdss_fixciv: Check spectra', /row, $
                    UNAME='BASE', /tlb_size_events, xoffset=200L)
  state.base_id = base
  

  state.left_id = WIDGET_BASE( state.base_id, /column, $
                              /base_align_center,/align_center, $
                              xsize=round(3*xsize/4), $
                              ysize=ysize, $
                              uvalue='LEFT)BASE', frame=2)
  state.draw_base_id = widget_base(state.left_id, /column, /base_align_left, $
                                   xsize=round(3*xsize/4), $
                                   ysize=round(4*ysize/5), $
                                   uvalue='DRAW_BASE', frame=2, $
                                   /tracking_events)
  state.si2_id = widget_draw(state.draw_base_id, xsize=round(3*xsize/4.), $
                              ysize=round(4*ysize/5), /frame, retain=2, $
                              uvalue='SI2DRAW', /button_even, /motion_events)
  state.right_id = WIDGET_BASE( state.base_id, /column, $
                              /base_align_top,/align_top, $
                              xsize=round(xsize/4.), ysize=ysize, $
                              uvalue='TOP_BASE', frame=2)
  state.mdraw_id = widget_draw(state.right_id, xsize=round(xsize/4.), $
                               ysize=4*ysize/5., /frame, retain=2, $
                               uvalue='MDRAW')
  state.fpdraw_id = widget_draw(state.right_id, xsize=round(xsize/4.), $
                               ysize=ysize/5., /frame, retain=2, $
                               uvalue='FPDRAW')
  state.text_id = widget_text(state.draw_base_id, $
                              /all_events, $
                              scr_xsize = 1, $
                              scr_ysize = 1, $
                              units = 0, $
                              uvalue = 'TEXT', $
                              value = '')
  state.size[0] = round(3*xsize/4)
  state.size[1] = round(4*ysize/5)

;;;;;; Info window ;;;;;;;;;;;
  state.info_id = $
    WIDGET_BASE( state.left_id, /row, /base_align_center,/align_center, $
                 uvalue='INFO_BASE', frame=2, xsize=xsize/2.)
;               ysize=round(ysize/3.))
  ;; Info
  civinf = widget_base(state.info_id, /column, /align_center, frame=2)
;  state.name_id = cw_field(civinf, title='Obj ', value=' ', xsize=18)
  zinf = widget_base(civinf, /row, /align_center, frame=2)
  state.zabs_id = cw_field(zinf, title='zabs: ', value=state.zabs, xsize=7)
  state.zqso_id = cw_field(zinf, title='zqso: ', value=state.zqso, xsize=7)
  state.ew_id = cw_field(civinf, title='EW: ', value=' ', xsize=13)
  state.boxew_id = cw_field(civinf, title='Box EW: ', value=' ', xsize=13)
  handinf = widget_base(state.info_id, /column, /align_center, frame=2)
  state.hew_id = cw_field(handinf, title='Hand:', value=state.handEW, $
                          /floating, $
                          /column, xsize=5, /return_events, uvalue='HEW')
  state.hsew_id = cw_field(handinf, title='SHand:', value=state.sighandEW, $
                           /floating, $
                           /column, xsize=5, /return_events, uvalue='HSEW')

  ;; BUTTONS
  butbase = widget_base(state.info_id, /column, /align_center, frame=2)
  good = WIDGET_BUTTON(butbase, value='BOX',uvalue='BOX')
  maybe = WIDGET_BUTTON(butbase, value='GAUSS',uvalue='GAUSS')
  bad  = WIDGET_BUTTON(butbase, value='HAND', uvalue='HAND')
  bad  = WIDGET_BUTTON(butbase, value='NG', uvalue='NG')

  ;; Indexing
  index = widget_base(state.info_id, /column, /align_center, frame=2)
  state.ivalu_id = cw_field(index, title='Indx: ', value=state.cursi2, $
                            xsize=6, /return_events, uvalue='IVALU')
  back  = WIDGET_BUTTON(index, value='BACK', uvalue='BACK')

  ;; Saving
  butbase2 = widget_base(state.info_id, /row, /align_center, frame=2)
  save = WIDGET_BUTTON(butbase2, value='SAVE', uvalue='SAVE')
  done = WIDGET_BUTTON(butbase2, value='DONE',uvalue='DONE')
  splt = WIDGET_BUTTON(butbase2, value='SPLT',uvalue='SPLT')


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

  ;; PLOT
  sdss_fixciv_next, state, NEW=FNEW
  
  WIDGET_CONTROL, base, set_uvalue = state, /no_copy

; Send to the xmanager
  xmanager, 'sdss_fixciv', base
  delvarx, fx, wv, npix, sig, qalstr

  return
end
	
