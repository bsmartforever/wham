;+
; Convert from day of month, month, and year to daynumber of year.
; It also works with arrays. 
;
; This code came from 
; <a href="http://www.naic.edu/~phil/">Phil Perillat</a> at Arecibo.
; Local changes:
; <UL>
; <LI> modify this documentation for use by idldoc.
; </UL>
; 
;  @param day {in}{required}{type=long integer} day of month
;  @param mon {in}{required}{type=long integer} month of year 1..12
;  @param year {in}{required}{type=long integer} 4 digit year
;
; @returns daynum, int/long  daynumber of year. First day of year is 1.
;
; @version $Id: dmtodayno.pro,v 1.2 2009/12/01 14:36:25 bgarwood Exp $
;-
function dmtodayno,day,mon,year
    dayNoDat=[0,0,31,59,90,120,151,181,212,243,273,304,334,$
              0,0,31,60,91,121,152,182,213,244,274,305,335]

    ind=where(mon lt 1,count)
    if count gt 1 then mon[ind]=1
    ind=where(mon gt 12,count)
    if count gt 1 then mon[ind]=12
    index=isleapyear(year)*13
    return,dayNoDat[index + mon] + day
end
