;+ 
; NAME:
; vpparse
;   Version 1.1
;
; PURPOSE:
;   parses a vpfit fort.26 output file and puts data into a structure of type
;   'vpstrct' which holds the relavent data.
; 
;
; CALLING SEQUENCE:
;   vpparse, vpfil, VPSTR=vpstr
;
; INPUTS:
;
; RETURNS:
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   Written by GEP
;-
;------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro g_vpparse, vpfil, VPSTR=vpstr

    vpstr = {vpstrct}
    nlin = numlines(vpfil)
    openr, 1, vpfil

    dumc=''
    nreg = 0
    nion = 0

    for i=0,nlin-1 do begin
        readf, 1, dumc
        prs = strsplit(dumc,' ', /extract)
        if prs[0] EQ '%%' then begin          ;  a Region line
            vpstr.fluxfil[nreg] = prs[1]
            vpstr.reg_beg[nreg] = double(prs[3])
            vpstr.reg_end[nreg] = double(prs[4])
            nreg = nreg + 1
        endif else begin
            if strlen(prs[0]) EQ 1 then begin  ; the ion is like 'H I'
                vpstr.ion[nion] = prs[0]+' '+prs[1]
                vpstr.z[nion] = double(prs[2])
                vpstr.zerr[nion] = double(prs[3])
                vpstr.z_str[nion] = prs[2]
                vpstr.b[nion] = double(prs[4])
                vpstr.berr[nion] = double(prs[5])
                vpstr.b_str[nion] = prs[4]
                vpstr.n[nion] = double(prs[6])
                vpstr.nerr[nion] = double(prs[7])
                vpstr.n_str[nion] = prs[6]
            endif else begin                       ; the ion is like 'SiIV'
                vpstr.ion[nion] = prs[0]
                vpstr.z[nion] = double(prs[1])
                vpstr.zerr[nion] = double(prs[2])
                vpstr.z_str[nion] = prs[1]
                vpstr.b[nion] = double(prs[3])
                vpstr.berr[nion] = double(prs[4])
                vpstr.b_str[nion] = prs[3]
                vpstr.n[nion] = double(prs[5])
                vpstr.nerr[nion] = double(prs[6])
                vpstr.n_str[nion] = prs[5]
            endelse
            nion = nion + 1
        endelse                            ;  a transition line
    endfor
    close, 1

    vpstr.nreg = nreg
    vpstr.nion = nion

end
