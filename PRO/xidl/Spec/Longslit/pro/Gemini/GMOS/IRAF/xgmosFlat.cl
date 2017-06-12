procedure xgmosFlat(list,rawpath,superflat,superbias,order)

string  list          {prompt="e.g. @gmos_R150_flats.lst"}
string  rawpath       {prompt="e.g. /Users/jhennawi/RAW_DATA/GEMINI_apr23_2003/"}
string  superflat     {prompt="e.g. N20040423_flat400"}
string  superbias     {prompt="e.g. N20040423_bias"}
int	order = 7    {prompt="order of polynomial for flat field"}

begin

string	combname, tmpfil

#tmpfil = mktemp("tmpoutfil")
#tmpfil = "test1"

gemini
gmos



# set up the logfile for this reduction
#gmos.logfile="gmosFlat.log"

# Make the flat with gsflat
gsflat(list,superflat,order=order,rawpath=rawpath,bias=superbias, fl_fixpix=no, fl_dete=yes)

###################################
#  These lines were a mistake...  Saving to remind me of this!
#Mosaic
#gmosaic(tmpfil, outimag=superflat, fl_past=no, fl_vard=no, fl_clea=yes, fl_fixp=no, geointe="nearest", gap=37)
#imdelete(tmpfil, verif-)

end  
