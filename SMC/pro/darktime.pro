FUNCTION darktime, human = human, light = light

;+
;FUNCTION darktime
;PURPOSE: Returns astronomical twilight at CTIO for tonight
;SYNTAX: darktime = darktime( [,/human] [, /light] )
;
;OPTIONAL KEYWORD INPUT PARAMETERS:
;	/human: human readable output. By default, return JD in UT
;   /light: return light time (end of night) instead of dark time
;-

idx = keyword_set(light) ? 1 : 0
return, (get_dark_times( systime(/jul, /ut), human=keyword_set(human) ))[idx]

END
