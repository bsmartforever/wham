;+
; Retrieves data from the output data file and places it in the
; primary data container (buffer 0).  
;
; <p>Use the selection parameters to specify which data to 
; retrieve.  If the selection returns more than one data container, 
; only the first data container is used. If you need to retrieve one
; of the other records you must first refine the selection criteria.
; A better approach might be to use <a href="select.html">select</a> to get the list of 
; records that satisfy that selection and then use <a href="kgetrec.html">kgetrec</a> to get the 
; individual records.
;
; <p>Only spectral line data can currently be fetched from a keep file.
;
; <p><em>Note :</em> all of the data satisfying the selection criteria
; (or lack of criteria if none is given) are extracted from the file
; before this routine copies the first one found and puts it into
; buffer 0.  There is no protection against running out of memory by
; grabbing too much data.
;
; <p>See the output of <a href="listcols.html">listcols</a> for a complete list of columns 
; that can be selected.
;
; <p>See the discussion on "Select" in the <a href="http://wwwlocal.gb.nrao.edu/GBT/DA/gbtidl/users_guide/node50.html" TARGET="_top">User's Guide</a> 
; for a summary of selection syntax.
;
; <p>The selection criteria are passed directly to the io class's
; get_spectra or get_continua function via the _EXTRA parameter. 
; <p>
; Flags (set via <a href="flag.html">flag</a>) can be selectively applied or ignored 
; using the useflag and skipflag keywords.  Only one of those two
; keywords can be used at a time (it is an error to use both at the
; same time).  Both can be either a boolean (/useflag or /skipflag) 
; or an array of strings.  The default is /useflag, meaning that all
; flag rules that have been previously set are applied when the data
; is fetched from disk, blanking data as described by each rule.  If
; /skipflag is set, then all of the flag rules associated with this
; data are ignored and no data will be blanked when fetched from disk
; (it may still contain blanked values if the actual values in the
; disk file have already been blanked by some other process).  If
; useflag is a string or array of strings, then only those flag rules
; having the same idstring value are used to blank the data.  If
; skipflag is a string or array of strings, then all flag rules except
; those with the same idstring value are used to blank the data.
;
; @keyword useflag {in}{optional}{type=boolean or string}{default=true}
; Apply all or just some of the flag rules?
; @keyword skipflag {in}{optional}{type=boolean or string} Do not apply
; any or do not apply a few of the flag rules?
; @keyword _EXTRA {in}{optional}{type=extra keywords} These are
; selection parameters passed to the data source to limit the amount
; of data returned. 
;
; @uses <a href="set_data_container.html">set_data_container</a>
; @examples
; <pre>
;   getnod,30
;   fileout,'mysave.fits'
;   keep
;   getnod, 32
;   keep
;   kget,index=0   ; retrieves the first record in the keep file
;   kget,index=0,/skipflag ; same record, ignore all flags
;   kget.index=0,useflag='wind' ; same record, apply just the 'wind' flag.
; </pre>
;
; @version $Id: kget.pro,v 1.15 2007/05/02 19:21:34 bgarwood Exp $
;-
pro kget, useflag=useflag, skipflag=skipflag, _EXTRA=ex
   compile_opt idl2

    if n_elements(useflag) gt 0 and n_elements(skipflag) gt 0 then begin
        message,'Useflag and skipflag can not be used at the same time',/info
        return
    endif

   if (!g.line) then begin
       if !g.lineoutio->is_data_loaded() eq 0 then begin
           message,'No keep file has been set or the keep file is empty',/info
           return
       endif

       dc = !g.lineoutio->get_spectra(useflag=useflag,skipflag=skipflag,_EXTRA=ex)

       if (data_valid(dc) gt 0) then begin
           if (n_elements(dc) gt 1) then begin
               message,'More than one item fetched - ignoring all by the first',$
                   /info
               dc = dc[0]
           endif
           nblanks = count_blanks(dc[0],ntot)
           if nblanks eq ntot then begin
               message,'All the data in the item fetched is blanked.',/info
           endif
           set_data_container, dc[0]
           data_free, dc
       endif else begin
           message, 'Fetched data appears to be empty or invalid',/info
       endelse
   endif else begin
       message, 'Continuum data can not be fetched from a keep file, sorry.',/info
   endelse

end

