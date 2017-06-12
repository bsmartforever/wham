;+
; NAME:
;  siftstar
; PURPOSE:   (one line only)
;  Generate filtered lists of stars from a single image tagged by crowding
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  siftstar,fn
; INPUTS:
;  fn - filename of the image (string). Do not include path to the file
;          in this input. (example: '100704.032')
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  REDDIR - Path to the data products associate with this image.
;            (default='/net/frakir/raid/buie/Reduced/')  Do not include the
;            date name in the directory, that part is automatically generated
;            from the input file name.
;  KEYLIST  - Name of a file containing a correspondence list. This list
;                associates a set of standard names with the actual keyword
;                names found in a FITS file header. If this keyword is
;                omitted, a default list is used, as if a file with the
;                following contents had been supplied:
;                   AIRMASS   K  AIRMASS
;                   DATE      K  DATE-OBS
;                   DATETMPL  T  DD-MM-YYYY
;                   EXPDELTA  V  0.0
;                   EXPTIME   K  EXPTIME
;                   FILTER    K  FILTERS
;                   FILENAME  K  CCDFNAME
;                *  OBJECT    K  OBJECT
;                   UT        K  UT 
;                *  RA        K  RA
;                *  DEC       K  DEC
;                *  EPOCH     K  EPOCH
;                The middle column is a flag. It may be K, for Keyword,
;                T, for Template, or V, for Value. If it is V, the contents
;                of the third field on that line should make sense for the
;                name in the first field.  Only those fields marked with '*'
;                are actually used by this program.
; OUTPUTS:
;  Writes two file files to REDDIR+'Src/'
;    fn+'.sr1' - data for large photometric aperture sources
;    fn+'.sr2' - data for small photometric aperture sources
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
;  Get source list from rdir+root+'/Src/'+root+fn+'.srd
;     need x,y,ra,dec
;  Get photometric aperture size from rdir+root+'/reduc.inf'
;     use rdreduc, this will get objrad,sky1,sky2,gain,rdnoise,ddir
;  Grab the image and its header from ddir+root+'/cal/'+fn
;     need the image array and exposure time (info.exptime)
;  Call photiso with big aperture, saving the sky --> list1
;  Call photiso with small aperture, reusing the sky --> list2
;  Remove list1 from list2 (intrsect)
;  Save results (for each list)
;      x,y,fwhm,mag,err,ra,dec,snr   (mag goes with aperture for the list)
;      make sure to augment the header to save things like apertures
; MODIFICATION HISTORY:
;  2011/01/13, Written by Erin R. George, Southwest Research Institute
;  2011/02/10, ERG, added numtoflist call
;  2011/02/11, ERG, changed input to be complete filename string
;  2011/03/08, MWB, incorporated into the master library, minor fix on the
;                     contents of the sr2 files.
;-
pro siftstar,fn,REDDIR=reddir,KEYLIST=keylist

   self='siftstar: '
   if badpar(fn,[7],0,CALLER=self+'(fn): ') then return

   if badpar(reddir,[0,7],0,CALLER=self+'(REDDIR): ', $
                  DEFAULT='/net/frakir/raid/buie/Reduced/') then return
   if badpar(keylist,[0,7],0,CALLER=self+'(KEYLIST) ', $
                  DEFAULT='nodefault') then return

   reddir=addslash(reddir)
   if not exists(reddir) then begin
      print,self,'Error: ',reddir,' does not exist.'
      return
   endif
   root=strmid(fn,0,6)
   rdir=reddir+addslash(root)
   if not exists(rdir) then begin
      print,self,'Error: ',rdir,' does not exist.'
      return
   endif
   sdir=rdir+'Src/'

   infofile='reduc.inf'
   if not exists(rdir+infofile) then begin
      print,self,'Error: ',rdir+infofile,' does not exist.'
      return
   endif
   rdreduc,rdir+infofile,inst,ddir,rundate,radius,sky1,sky2,gain,rdnoise

   fnsrd=fn+'.srd'
   fnsr1=fn+'.sr1'
   fnsr2=fn+'.sr2'

   loadkeys,keylist,hdrlist,FOUNDIT=foundkey
   if not foundkey then begin
      print,self,'Keylist ',keylist,' could not be loaded. Aborting.'
      return
   endif

   if not exists(sdir+fnsrd) then begin
      print,self,'Error: ',sdir+fnsrd,' does not exist.'
      return
   endif

   ; read the raw data, all we need from the file is the raw position and
   ;   the FWHM of the source, keep the data array for later modification
   ;   and use.
   data=readfits(sdir+fnsrd,hdr1)
   xloc=trimrank(data[*,0])
   yloc=trimrank(data[*,1])
   fwhm=trimrank(data[*,2])
   robomean,fwhm,3.0,0.5,avgfwhm

   cdir=ddir+addslash(root)+'cal/'
   if not exists(cdir+fn) then begin
      print,self,'Error: ',cdir+fn,' does not exist. Line 128.'
      return
   endif

   ; Read in the original image, we need to extract the photometry with
   ;   a different aperture than used for the src/srd file.
   image=readfits(cdir+fn,hdr)
   parsekey,hdr,hdrlist,hdrinfo
   exptime = hdrinfo.exptime

   ; This first call will extract photometry with the same aperture used
   ;   for all-sky absolute photometry on this night (as found in reduc.inf
   ;   file).  The list of good stuff will be objects that are cleanly
   ;   separated from contaminating sources within the large photometric
   ;   aperture.
   photiso,gain,image,exptime,xloc,yloc,radius,sky1,sky2,ngood,zgood, $
      BOXMRAD=boxmrad,PSCALE=pscale,RDNOISE=rdnoise, $
      ERR=err,FLERR=flerr,FLUX=flux,FWHM=fwhm1,MAG=mag,XCEN=xcen,YCEN=ycen, $
      SKYMEAN=skymean,SKYERR=skyerr,VERBOSE=verbose

   if ngood eq 0 then begin
      print,self,'Error: The number of good stars is 0. Uh oh!'
      return
   endif

   ; Take the photometry results and put it in the data array
   data[*,0]=xcen
   data[*,1]=ycen
   data[*,3]=mag
   data[*,4]=err
   data[*,7]=flux/flerr

   sxaddpar,hdr1,'OBJRAD',radius
   sxaddpar,hdr1,'SKY1',sky1
   sxaddpar,hdr1,'SKY2',sky2
   sxaddpar,hdr1,'AVGFWHM',avgfwhm

   ; save the data to an sr1 file, this has the same format as an srd file
   writefits,sdir+fnsr1,data[zgood,*],hdr1
   ;the data written in is 0=xcen,1=ycen,2=fwhm,3=instrumental magnitude,
   ;4=uncertainty of instrumental magnitude,5=ra,6=dec,7=snr,8=standard mag

   ; second call, this time get a list of everything in the clear in a small
   ;   aperture.  Here we use a radius equal to the average FWHM.  The list
   ;   of good stuff here will be a superset of the first list.
   photiso,gain,image,exptime,xloc,yloc,avgfwhm,skymean,-1.0*skyerr, $
      ngood2,zgood2, $
      BOXMRAD=boxmrad,PSCALE=pscale,RDNOISE=rdnoise,VERBOSE=verbose, $
      ERR=err,FLERR=flerr,FLUX=flux,FWHM=fwhm2,MAG=mag,XCEN=xcen,YCEN=ycen

   if ngood2 eq 0 then begin
      print,self,'Error: The number of good stars is 0 for the second aperture!'
      return
   endif

   ; Take the second pass photometry results and put it in the data array
   data[*,0]=xcen
   data[*,1]=ycen
   data[*,3]=mag
   data[*,4]=err
   data[*,7]=flux/flerr

   ; This step removes the first set from the second, leaving only those
   ;   objects that are isolated in the small aperture and not in the big
   ;   aperture
   intrsect,zgood,zgood2,good,/nnot

   sxaddpar,hdr1,'OBJRAD',avgfwhm

   writefits,sdir+fnsr2,data[good,*],hdr1

end
