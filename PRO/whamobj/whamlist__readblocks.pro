; docformat = 'rst rst'

;+
; Class method to load a collection of WHAM pointings.
; 
;
; :Author: Matt Haffner
;-

compile_opt idl2, logical_predicate

pro WHAMList__ReadSingleBlock, g, path, _extra = _extra

  files = file_search(path + '_[0-9]*.fts', count = count)
  
  if count eq 0 then begin
    message, 'No files found for block path: ' + path, /info
    return
  endif
    
  b = objarr(count)
  for i = 0, count-1 do $
    b[i] = WHAMPointing(files[i], _extra = _extra)

  g.add, b, /pointings

end

function WHAMList__ReadBlocks, path, blocks, quiet = quiet, _extra = _extra

  ;; Create an empty group
  g = WHAMList(name = 'Blocks')
  
  if N_PARAMS() eq 1 then begin 
    ;; User passed a list containing blocks to read

    if ~file_test(path, /read) then begin 
      message, 'No block list file found: ' + path, /info
      return, g
    endif
    
    openr, unit, path, /get_lun
    line = ''
  
    blocks = [ ]
    while not eof(unit) do begin 
      readf, unit, line
      if strpos(line, "#") ne 0 then blocks = [blocks, line]
    endwhile
    close, unit
  
    foreach b, blocks do begin
      if ~KEYWORD_SET(quiet) then print, b
      WHAMList__ReadSingleBlock, g, b, _extra = _extra
    endforeach

    g.name = g.name + ': ' + path

;; old code that included averaging... need to reimplement.
;  
;    i = 0
;    REPEAT BEGIN 
;      print, "Reading Block:" + blocks[i]
;      readblock, blocks[i], nmap, /ext, /quiet, ftsext = ext
;      
;      avgblocks = [blocks[i]]
;      WHILE ++i LT N_ELEMENTS(blocks) && strmid(blocks[i], 0, 1) EQ '+' DO $
;        avgblocks = [avgblocks, strmid(blocks[i], 1)]
;      IF N_ELEMENTS(avgblocks) GT 1 THEN create_avg_block, avgblocks, nmap, $
;        ext = ext
;  
;      bmap = N_ELEMENTS(bmap) EQ 0 ? nmap : [bmap, nmap]
;    ENDREP UNTIL i GE N_ELEMENTS(blocks) 
  
  endif else begin
    ;; User passed a directory and an array of blocks to read
  
    if ~(isa(blocks, 'INT') or isa(blocks, 'LONG') or isa(blocks, 'UINT') or isa(blocks, 'ULONG')) then $
      message, 'Second argument must be integers'
  
    foreach b, blocks do begin
      if ~KEYWORD_SET(quiet) then print, 'Block ' + strtrim(b,2)
      WHAMList__ReadSingleBlock, g, path + '/combo/b' + strtrim(b, 2), _extra = _extra
      g.name = g.name + " " + strtrim(b, 2)
    endforeach
    
  endelse
  
  return, g
  
end