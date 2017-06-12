;+
; This class extends the rows_index_section class to properly manage the rows section 
; for a spectral line index file; that is, an index file where each row line represents
; a spectrum.
;
; @file_comments
; This class extends the rows_index_section class to properly manage the rows section 
; for a spectral line index file; that is, an index file where each row line represents
; a spectrum.
;
; @inherits rows_index_section
;
; @private_file
;-
PRO line_index_section__define

    ris = { LINE_INDEX_SECTION, $ 
            inherits rows_index_section $
    }

END

;+
; Class Constructor
; Here the formats for the rows are determined: how to list them verbosly and
; quietly.
; @private
;-
FUNCTION LINE_INDEX_SECTION::init, filename

    r = self->ROWS_INDEX_SECTION::init(filename)

    ; all floats and doubles have the same format
    self.float_format = 'e16.9'
    flt = self.float_format
    
    ; array that contains all info needed for printing out info + header
    ; ***NOTE: this order must follow the order of {row_line_info_strct}
    ;i val, l val, column name, i head, l head
    ; i=index, l=listing
    *self.frmt = [ $
    ['i6','i6',  '#INDEX',      'a6','a6', 'integer,string'], $
    ['a16','a16','PROJECT',     'a16','a16', 'string'], $ 
    ['a64','file_name','FILE',  'a64','a32', 'string'], $
    ['i3','i3',  'EXTENSION',   'a3','a3', 'integer,string'], $
    ['i6','i6',  'ROW',         'a6','a6', 'integer,string'], $
    ['a32','source_name','SOURCE','a32','a16', 'string'], $
    ['a9','a9',  'PROCEDURE',   'a9','a9', 'string'], $
    ['a32','a32','OBSID',       'a32','a32', 'string'], $
    ['i5','i5',  'E2ESCAN',     'a5','a5', 'integer,string'], $
    ['i5','i5',  'PROCSEQN',    'a5','a5', 'integer,string'], $
    ['i10','i10','SCAN',        'a10','a10', 'integer,string'], $
    ['a3','a3',  'POLARIZATION','a3','a3', 'string'], $
    ['i5','i5',  'PLNUM',       'a5','a5', 'integer,string'], $
    ['i5','i5',  'IFNUM',       'a5','a5', 'integer,string'], $
    ['i5','i5',  'FEED',        'a5','a5', 'integer,string'], $
    ['i5','i5',  'FDNUM',       'a5','a5', 'integer,string'], $
    ['i10','i10','INT',         'a10','a10', 'integer,string'], $
    ['i6','i6',  'NUMCHN',      'a6','a6', 'integer,string'], $
    ['a3','a3',  'SIG',         'a3','a3', 'string'], $
    ['a3','a3',  'CAL',         'a3','a3', 'string'], $
    ['a7','a7',  'SAMPLER',     'a7','a7', 'string'], $
    [flt,'F5.1',    'AZIMUTH',     'a16','a5', 'float,string'], $
    [flt,'F4.1',    'ELEVATION',   'a16','a4', 'float,string'], $
    [flt,'sexigesimal_ra','LONGITUDE',   'a16','a12', 'float,string'], $
    [flt,'sexigesimal','LATITUDE',    'a16','a12', 'float,string'], $
    [flt,'sexigesimal','TRGTLONG',    'a16','a12', 'float,string'], $
    [flt,'sexigesimal','TRGTLAT' ,    'a16','a12', 'float,string'], $
    ['i3','i3',  'SUBREF',      'a3', 'a3', 'integer,string'], $
    [flt,'sex_degrees','LST',         'a16','a12', 'float,string'], $
    [flt,flt,    'CENTFREQ',    'a16','a16', 'float,string'], $
    [flt,flt,    'RESTFREQ',    'a16','a16', 'float,string'], $
    [flt,flt,    'VELOCITY',    'a16','a16', 'float,string'], $
    [flt,flt,    'FREQINT',     'a16','a16', 'float,string'], $
    [flt,flt,    'FREQRES',     'a16','a16', 'float,string'], $
    ['a22','dateobs','DATEOBS',     'a22','a23', 'string'], $
    ['a22','a22','TIMESTAMP',     'a22','a22', 'string'], $
    [flt,flt,    'BANDWIDTH',   'a16','a16', 'float,string'], $
    [flt,'F8.1',    'EXPOSURE',    'a16','a8', 'float,string'], $
    [flt,flt,    'TSYS',        'a16','a16', 'float,string'], $
    ['i10','i10','NSAVE',       'a10','a10', 'integer,string'] $
    ]
    
    ; indicies into above array showing what to print when NOT in verbose mode
    ; These should match up to the following:
    ; INDEX
    ; SOURCE
    ; SCAN
    ; PROCEDURE
    ; POLARIZATION
    ; IFNUM
    ; FDNUM
    ; INT
    ; SIG
    ; CAL
    *self.frmt_quiet = [0,5,10,6,11,13,15,16,18,19]
    
    ; use the above arrays to create format strings
    self->create_formats

    return,1
    
END

;+
; Returns the specail structure needed for spectal line data
; @returns line_row_info_strct structure
; @private
; -
FUNCTION LINE_INDEX_SECTION::get_row_info_strct

    @line_row_info
    return, {line_row_info_strct}

END

;+
; Trims whitespace from structure string fields that represent columns in an
; index file
; @param rows {in}{required}{type=array} string array of index column values
;-
PRO LINE_INDEX_SECTION::trim_row_whitespace, rows
    compile_opt idl2, hidden
    
    rows.project = strtrim(rows.project,2)
    rows.file = strtrim(rows.file,2)
    rows.source = strtrim(rows.source,2)
    rows.procedure = strtrim(rows.procedure,2)
    rows.polarization = strtrim(rows.polarization,2)
    rows.cal_state = strtrim(rows.cal_state,2)
    rows.sig_state = strtrim(rows.sig_state,2)
    rows.sampler = strtrim(rows.sampler,2)
    rows.timestamp = strtrim(rows.timestamp,2)
    rows.obsid = strtrim(rows.obsid,2)

END
