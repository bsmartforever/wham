pro filldaophot,psfinfo,band,stp=stp

;+
;
; FILLDAOPHOT
;
; IDL version of Steve's filldaophot.cl IRAF script to make
; skawdphot input files.
;
; The photometry needs to be in *night* order otherwise
; skawdphot.pro crashes.  It should do this correctly now.
;
; Chris Palma wrote a similar program called PHOTINFO.PRO
;
;
; INPUTS:
;  psfinfo  The psfinfo.X file.
;  band     The name of the passpand (e.g. 'M').
;
; OUTPUTS:
;  It outputs the photometry data that skawdphot.pro uses.
;  The name is X.data, where X is "band".
;
; USAGE:
;  IDL>filldaophot,'psfinfo.M','M'
;
; By D. Nidever  Dec 2006 (copy of filldaophot.cl)
;-

; Not enough inputs
if n_elements(psfinfo) eq 0 or n_elements(band) eq 0 then begin
  print,'Syntax - filldaophot,psfinfo,band,stp=stp'
  return
endif

iinfile = psfinfo
iband = band
runnum = 0
tot = 0
holdnight=1

; Opening the output file
outfile = strupcase(strtrim(band,2))+'.data'
openw,unit,/get_lun,outfile

;print(" ALL MAG FILES MUST BE .mag.1 AND IN THIS DIRECTORY")
;print(" TEMPLATE FILES MUST BE IN THIS DIRECTORY.")
;print(" (Make sure templates nondegenerate and with correct colors and mags.")
;print(" ***Make sure psfinfo file has two initial blank lines***")
;print(" ***NOTE: This version of program allows E format in output weights! ***")
;print("           (may need to edit to fix E's)                                ")

list = iinfile

; Read in the data from the psfinfo.X file
format = 'A,A,A,I,A,F,A,F'
READCOL,iinfile,ccdframe,image,xband,night,fieldname,airmass,ut,exptime,format=format,/silent

; Put the data in NIGHT order
si = sort(night)
ccdframe = ccdframe[si]
image = image[si]
xband = xband[si]
night = night[si]
fieldname = fieldname[si]
airmass = airmass[si]
ut = ut[si]
exptime = exptime[si]


;format:
;image name       image name  filt night template airmass  UT           exposure
;-------------------------------------------------------------------------------
;ccd217_final    ccd217_final    V  1    RU149V  1.305   08:23:37         5.00

nframe = n_elements(ccdframe)

; Loop through the frames
for i=0,nframe-1 do begin

  ; The right band
  if strtrim(xband[i],2) eq iband then begin

    ; Read the mag file
    READCOL,strtrim(image[i])+'.mag',name,x,y,s1,merr,sky,m1,off,iter,format='I,F,F,F,F,F,F,F,I',/silent
    nstars = n_elements(name)

    ; Read the template file
    template = strtrim(fieldname[i])+'.template'
    READCOL,template,starid,rmag,rcolor,magerr,colerr,name,format='I,F,F,F,F,A',/silent

    ; Loop through the stars in the list
    for j=0,nstars-1 do begin

      starid1 = starid[j]
      rmag1 = rmag[j]
      rcolor1 = rcolor[j]
      magerr1 = magerr[j]
      colerr1 = colerr[j]
      name1 = name[j]
      s11 = s1[j]
      merr1 = merr[j]
      
      exp = float(exptime[i])
      s11 = s11 + 2.5*alog10(exp)
      pier = 0

      ; Good photometry
      if (pier eq 0 and merr1 lt 8.99 and name1 ne 'psfstar') then begin


        if (night[i] eq holdnight) then begin
          runnum = runnum + 1
        endif else begin
          runnum = 1
          holdnight = night[i]
        endelse


        ; Evaluate the weight as inverse of quadrature addition of measured error
        ; and quoted error in magnitudes.  The function is normalized so that the
        ; highest possible wt = 1.00
        if (merr1 lt 0.001) then merr1 = 0.001
        if (magerr1 lt 0.001) then magerr1 = 0.001
        wt = 0.0014142135624 / sqrt(merr1^2. + magerr1^2.)

        ; For formatting purposes
        ;if (runnum le 9) then seqnum = '  '+strtrim(runnum,2)
        ;if (runnum ge 10 and runnum le 99) then begin
        ;  seqnum = ' '+strtrim(runnum,2)
        ;endif
        ;if (runnum ge 100) then seqnum = strtrim(runnum,2)
        seqnum = runnum

        tot = tot + 1
        bad = ' '
        if (pier ne 0) then bad='B'

        out1=' '+strtrim(ccdframe[i],2)+'-'+strtrim(starid1,2)+string(bad)

        ; PRINTING OUT THE INFORMATION
        ;print (out1," ",rmag," ",rcolor," ",airmass," ",s1," ",night,seqnum," ",ut," ",wt)
        ;print,out1,' ',rmag1,' ',rcolor1,' ',airmass[i],' ',s11,' ',night[i],seqnum,' ',ut[i],' ',wt
        format='(A15,F12.5,F12.5,F12.5,F12.5,I5,I5,A12,F12.5)'
        printf,unit,format=format,out1,rmag1,rcolor1,airmass[i],s11,night[i],seqnum,ut[i],wt

        ; THIS IS THE READ STATEMENT FROM SKAWDPHOT.PRO
        ;readcol,ddfile,starname,realmag,realcol,secz,instmag,night,num,UT,wt,$
        ;  format='a,d,d,d,d,i,i,a,d'

        ;stop

      end

    end  ; loop through the stars

    ;stop

  end  ; the right band

end  ; for i

; Closing the output file
close,unit
free_lun,unit

;print(" ")
;print(" total number of objects = ", tot) 
;print,' '
print,'Total number of objects = ',strtrim(tot,2)


if keyword_set(stp) then stop

end


;procedure fillphot(infile, band)
;#  Steve Majewski - Yerkes Observatory - May 1989
;
;string	infile {prompt='information file '}
;string	band	{prompt='band '}
;struct  *alist
;
;begin
;string	iinfile, iband, template, junk, image, xband, fieldname, out1, bad
;string 	cerror, tempo, temp3, shit, barf, name
;int	night, holdnight , mmm, nnn, runnum, tot, pier
;string	airmass, sigma, fwhmpsf, rmag, rcolor, starid, seqnum, ccdframe 
;real	magerr, colerr, wt, merr, ut, exptime, s1
;
;
;iinfile=infile
;iband=band
;runnum = 0
;tot = 0
;holdnight=1
;print(" ALL MAG FILES MUST BE .mag.1 AND IN THIS DIRECTORY")
;print(" TEMPLATE FILES MUST BE IN THIS DIRECTORY.")
;print(" (Make sure templates nondegenerate and with correct colors and mags.")
;print(" ***Make sure psfinfo file has two initial blank lines***")
;print(" ***NOTE: This version of program allows E format in output weights! ***")
;print("           (may need to edit to fix E's)                                ")
;
;list=iinfile
;#nnn=fscan(list, junk, junk)
;#nnn=fscan(list,junk,junk,junk,junk,junk,junk,junk,junk,junk)
;while (fscan (list, ccdframe, image, xband, night, fieldname, airmass, ut, exptime) != EOF) {
;	if (xband == iband) {
;		#print("eat")
;		tempo=mktemp("tmp")
;#		txdump (str(image)+".mag.1", 
;#		"ID,MAG[2],MERR[2],PIER[2]", "yes", headers=no, parameters=yes, >tempo)
;
;                fields(str(image)+".mag","1,4,5", lines="1-", quit_if_miss=no, print_file_n=no, > tempo)
;
;		template=str(fieldname)+".template"		
;		temp3=mktemp("tmp")
;		#print("shit")
;		#join (template, tempo, > temp3)  old iraf
;		join (template, tempo, output=temp3, delim=" ", missing="Missing", maxchars=161,
;shortest=yes, verbose=yes)
;		#print("die")
;		alist=temp3
;		#print(template)
;		nnn=0
;	 	while (fscan(alist,starid,rmag,rcolor,magerr,colerr,name,shit,s1,merr,pier) !=EOF) {
;		        
;#                       print(log10(exptime))
;                       exptime=real(exptime)
;#                       print(s1,merr)
;                       s1=s1+2.5*log10(exptime)
;                       pier=0
;			if (pier==0 && merr <8.99 && name!="psfstar") {
;				if(night == holdnight) 
;					runnum += 1
;				else {
;					runnum = 1
;					holdnight = night
;				}
;#evaluate the weight as inverse of quadrature addition of measured error
;#and quoted error in magnitudes.  The function is normalized so that the
;#highest possible wt = 1.00
;				if (merr < 0.001) merr = 0.001
;				if (magerr < 0.001) magerr = 0.001
;				wt = 0.0014142135624 / sqrt(merr**2 + magerr**2)
;#
;#for formatting purposes
;				if (runnum <= 9)  seqnum = "  "+str(runnum)
;				if (runnum >= 10 && runnum <= 99) {
;					seqnum = " "+str(runnum)
;					}
;				if (runnum >= 100) seqnum = str(runnum)
;				tot += 1				
;			        bad=" "
;			        if (pier != 0) bad="B"
;			        out1=" "+str(ccdframe)+"-"+str(starid)+str(bad)
;				print (out1," ",rmag," ",rcolor," ",airmass," ",s1," ",night,seqnum," ",ut," ",wt)
;			}
;		}
;	delete (tempo, go_ahead=yes)
;	delete (temp3, go_ahead=yes)
;	}
;}
;print(" ")
;print(" total number of objects = ", tot) 
;end
