pro jpeg2fits,input,true=true,outdir=outdir,verbose=verbose,stp=stp

; +
;
; JPEG2FITS
;
; This program converts JPEG files to FITS files
; It checks that the input files exist and that they
; are valid JPEG files.
;
; The JPEG files is always converted to grayscale
; (even if it is an RGB image) unless the /TRUE
; keyword is set.
;
; INPUTS:
;  input    A list of filenames.  The input can be:
;            (1) an array list of files
;            (2) a globbed list, i.e. "*.txt"
;            (3) the name of a file that contains a list, i.e. "list.txt"
;  /true    Do NOT convert to grayscale.
;  =outdir  Output directory.  The default is to put the 
;             FITS files in the same directory as the JPG files
;  /verbose Print out information about the copying.
;  /stp     Stop at the end of the program.
;
; OUTPUTS:
;  The JPEG files are converted to FITS files with the same name
;  except with a ".fits" extension.
;
; By D.Nidever  August 2007
;-

; No input
if n_elements(input) eq 0 then begin
  print,'Syntax - jpeg2fits,list'
  return
endif

; Get the list of files
loadinput,input,list
nlist = n_elements(list)


; Looping through the files
for i=0,nlist-1 do begin

  file = list[i]

  ; Checking if it's a valid JPEG file
  status = QUERY_JPEG(file)

  ; Going through the cases
  CASE status of

    ; OKAY
    1:   begin

      ; Read in the JPEG
      grayscale = 1
      if keyword_set(true) then grayscale=0
      READ_JPEG,file,im,grayscale=grayscale

      ; Get the FITS name
      dir = file_dirname(file)
      file = file_basename(file)
      dum = strsplit(file,'.',/extract)
      base = dum[0]
      fitsname = base+'.fits'
      if keyword_set(outdir) then odir=outdir else odir=dir
      outname = odir+'/'+fitsname

      ; Test that we can write to this file
      info = FILE_TEST(odir,/write)

      ; Do NOT have permissions to write to this directory
      if (info eq 0) then begin
        print,'ERROR: CANNOT WRITE TO THIS DIRECTORY'

      ; DO have permission
      endif else begin

        ; Write the FITS file
        FITS_WRITE,outname,im

        if keyword_set(verbose) then $
          print,file,' -> ',outname

      endelse

    end  ; okay

    ; NOT VALID
    0:   begin

      if keyword_set(verbose) then $
        print,'ERROR: ',file,' IS NOT A VALID JPEG FILE'

    end

    ; Doesn't exist
    else: begin

      if keyword_set(verose) then $
        print,'ERROR: ',file,' DOES NOT EXIST'

    end

  ENDCASE

end

if keyword_set(stp) then stop

end
