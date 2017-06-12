PRO lsrvel, long, lat, vlsr_out, vmb_out, SILENT=silent
   
;;**********************************************************************
;   This program calculates the projection of the velocity
;      vector of the local standard of rest on the sightline
;      specified by (l,b)=(long,lat).  
;   
;      Assumes v(LSR) = 20   km/sec to (l,b)=(56, 22) or 
;              v(LSR) = 16.5 km/sec to (l,b)=(53, 25)
;                              from Mihalas & Binney
;
;      Created by Howk 9/27/99
;;**********************************************************************   
   

   IF n_params(0) EQ 0 THEN BEGIN
   
      print
      print
      print,  '  Calling Sequence:'
      print,  '      lsrvel, long, lat, vlsr, vlsr_MB, /silent'
      print
      print,  '        long, lat = Input Galactic longitude and latitude'
      print,  '        vlsr      = Output for local standard of rest velocity'
      print,  '        vlsr_MB   = Output for local standard of rest velocity from Mihalas & Binney'
      print,  '        /silent   = Suppress printing of results'
      retall   
   END

   
   
   
   llsr =  56.
   blsr =  22.
   vlsr =  20.0
   
   lmb   =  53.
   bmb   =  25.
   vmb   =  16.5 
   
   
   dlsr =  sphdist(llsr, blsr, long, lat, /degrees)
   dmb   =  sphdist(lmb, bmb, long, lat, /degrees)

   ;;print, dlsr
   
   dlsr =  2.*!pi*dlsr/360.
   dmb   =  2.*!pi*dmb/360.

   vlsr_out =  vlsr*cos(dlsr)
   vmb_out   =  vmb*cos(dmb)
   
   
   IF NOT keyword_set(silent) THEN BEGIN 
   
      ;; Print output...
      print, " "
      print, " LSR Correction: "
      print, vlsr_out, $
       format='("     LSR     = ",f6.2," km/s")'
      print, vmb_out, $
       format='("     LSR(MB) = ",f6.2," km/s.")'
      print, " "
      print, " v(LSR) = v(helio) + LSR"
      print, " "
   ENDIF 
END 
