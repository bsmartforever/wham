function validate_numeric, value
compile_opt strictarr

on_ioerror, badValue

if n_elements(value) ne 0 then f = float(value) else goto, badValue
return, 1B

badValue: return, 0B
end
