;This file is used to replace the filter header keywords in the even that they are incorrectly
;labeled as "Empty". To fix the headers, run this file in both the combo and raw directories. 
;After this file is ran and the directory named "Empty" is changed to "nii", you can now run 
;the cor_spec_ctio,/chop command in the date directory. If this problem happens for other filters, 
;copy this file and modify the desired keywords accordingly.  


Result = FILE_SEARCH('*fts')
            for i=0,n_elements(result)-1 do begin
                   fits_open,Result(i),io,/update    ;Faster to explicity open
                   fits_read,io,0,h,/header_only,exten_no=0,/No_PDU ;Get header     
                   date= sxpar(h,'OBSDATE')         ;Save keyword value
                   sxdelpar,h,'WAVELEN'             ;Delete bad keyword
                   sxdelpar,h,'FINVNUM'             ;Delete bad keyword
                   sxaddpar,h,'FINVNUM',206,before='FSHNAME' 
                   sxdelpar,h,'FSHNAME'             ;Delete bad keyword
                   sxdelpar,h,'FLNGNAME'            ;Delete bad keyword
                   sxdelpar,h,'FCENTER'            ;Delete bad keyword
                   sxdelpar,h,'FWIDTH'            ;Delete bad keyword
                   sxaddpar,h,'WAVELEN',6587.00,after='OBJECT' 
                   sxaddpar,h,'FSHNAME','nii_red',after='FINVNUM' 
                   sxaddpar,h,'FLNGNAME','[N II] 6584',after='FSHNAME' 
                   sxaddpar,h,'FCENTER',6587.00,after='FLNGNAME' 
                   sxaddpar,h,'FWIDTH',0.0,after='FCENTER' 
                   modfits,io,0,h,exten_no=0        ;Update header
                   fits_close,io
            endfor
end