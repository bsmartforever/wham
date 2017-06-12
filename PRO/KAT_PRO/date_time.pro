function date_time, time=time
;
; Purpose: To create a string containing yymmdd
; 
; 	Input: None
;		   [time] - also return the hhmmss, such that yymmdd_hh:mm:ss
;
;	Output: string containing yymmdd
;
;	Example:
;		IDL> print, date()
;				150607
;		IDL> print, date(/time)
;				150607_19:06:52
;
;  Created by Dr. Kat Barger 06/2015
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    time_all = Systime(UTC=Keyword_Set(utc))
    day = Strmid(time_all, 0, 3)
    date = String(StrMid(time_all, 8, 2), Format='(I2.2)') ; Required because UNIX and Windows differ in time_all format.
    month = Strmid(time_all, 4, 3)
    year = Strmid(time_all, 22, 2)
    hhmmss = Strmid(time_all, 11, 8)
    months = ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
    m = (Where(months EQ StrUpCase(month))) + 1
    month=String(m, FORMAT='(I2.2)')

    if (keyword_set(time)) then return,year+month+date+'_'+hhmmss $
    	else return,year+month+date
end