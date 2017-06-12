;+
; NAME:
;  uniqueid
; PURPOSE:   (one line only)
;  Create a unique identifying string
; DESCRIPTION:
;  This routine will construct a 6-character string (base 32 number)
;    that is close to being a unique string.  The odds getting an identical
;    string in close temporal proximity is low but not zero.  You need to
;    take extra steps to make sure it is unique before counting on that
;    property.  This tool is mostly designed to help support parallel
;    processing.
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  id=uniqueid()
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
;   mwb_uniqueid_com - used to hold a persistent random number seed for
;     successive calls.  This is not designed to be used by any other
;     program.
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Southwest Research Institute, 2011/07/19
;-
function uniqueid

   common mwb_uniqueid_com,seed

   type=size(seed,/type)

   rval = strb36(trimrank(randomu(seed,1,/long)),pad=6)

   return,rval

end
