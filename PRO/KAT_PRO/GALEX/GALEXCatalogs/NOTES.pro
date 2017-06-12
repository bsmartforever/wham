default_target=GALEXCatalogs
default_target=GALEXGR6Plus7
;add the following the bash_profile:
alias casjobs='java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar'
;make casjobs a global command:
;sudo ln -s "java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar" /bin/casjobs
mv or copy the casjobs.config to $HOME


select top 10 objid, ra, dec, fuv_mag as fuv, nuv_mag as nuv
	from bcscat_mis
	order by nuv_mag
	
java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select * from fgetnearbyobjeq(14.97208,31.82694, 5/*arcmins*/)"
java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select nb.objid,nb.distance as dstArcMin,p.ra,p.dec,p.glon,p.glat,p.nuv_flux,p.fuv_flux, p.e_bv from fgetnearbyobjeq(14.97208,31.82694, 1/*arcmins*/) as nb, photoobjall as p, photoextract as pe where nb.objid=p.objid and p.photoextractid=pe.photoextractid and p.band=1 /* band = NUV */ order by dstArcMin"  
java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select nb.objid,nb.distance as dstArcMin,p.ra,p.dec,p.glon,p.glat,p.nuv_flux,p.fuv_flux, p.e_bv from fgetnearbyobjeq(14.9721, 31.8269,  1/*arcmins*/) as nb, photoobjall as p, photoextract as pe where nb.objid=p.objid and p.photoextractid=pe.photoextractid and p.band=1 /* band = NUV */ order by dstArcMin"


java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select nb.objid,pe.mpstype as survey,nb.distance as dstArcMin, p.ra,p.dec,p.band,p.glon,p.glat,p.nuv_mag,p.nuv_magerr,p.fuv_mag, p.fuv_magerr, p.e_bv from fgetnearbyobjeq(116.58073,42.66339, 5/*arcmins*/) as nb, photoobjall as p, photoextract as pe where nb.objid=p.objid and p.photoextractid=pe.photoextractid and p.band=1 /* band = NUV */ order by dstArcMin"  

;[objid]:Integer,[survey]:String,[dstArcMin]:Float,[ra]:Float,[dec]:Float,[band]:Integer,[glon]:Float,[glat]:Float,[nuv_mag]:Float,[nuv_magerr]:Float,[fuv_mag]:Float,[fuv_magerr]:Float,[e_bv]:Float
java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select nb.objid,pe.mpstype as survey,nb.distance as dstArcMin,p.ra,p.dec,p.band,p.glon,p.glat,p.nuv_mag,p.nuv_magerr,p.fuv_mag, p.fuv_magerr, p.e_bv from fgetnearbyobjeq(116.58073,42.66339, 5/*arcmins*/) as nb, photoobjall as p, photoextract as pe where nb.objid=p.objid and p.photoextractid=pe.photoextractid and p.band=1 /* band = NUV */ order by dstArcMin" 


java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select ra, dec, glon, glat, fuv_mag as fuv, nuv_mag as nuv from fgetnearbyobjeq(14.97208,31.82694, 5/*arcmins*/)"


casjobs execute "select top 10 objid, ra, dec, glon, glat, fuv_flux as fuv, nuv_flux as nuv from bcscat_mis"


casjobs execute "select top 10 objid, ra, dec, fuv_mag as fuv, nuv_mag as nuv from bcscat_mis where ra<44"

casjobs execute "select ra, dec, fuv_mag as fuv, nuv_mag as nuv from bcscat_mis where ra BETWEEN 180 AND 181 AND dec BETWEEN -0.5 AND 0.5" > outfile

casjobs execute "select ra, dec, fuv_mag as fuv, nuv_mag as nuv from bcscat_mis WHERE ra>43. AND ra<45. AND dec<-13. AND dec>-14."

casjobs execute "select ra, dec, fuv_mag as fuv, nuv_mag as nuv from bcscat_mis where ra BETWEEN 43. AND 45. AND dec BETWEEN -14. AND -13."

[objid]:Integer,[ra]:Float,[dec]:Float,[fuv]:Float,[nuv]:Float
2923471813389328156,44.1187117871552,-13.7677986051839,24.06133,23.0541
2923471813389328155,44.101426827521,-13.7796205087347,22.69102,21.87123
2923471813389328153,44.1681610713735,-13.795153207323,20.60951,20.46795
2923471813389328152,43.9198135251599,-13.7681876523342,23.94459,23.50661
2923471813389328150,43.5546268663964,-13.7687125239351,23.36254,22.81282
2923471813389328149,43.750220832609,-13.7663951333241,23.25093,23.37295
2923471813389328148,44.0462674126264,-13.7769992114639,20.90003,20.20241
2923471813389328147,43.8298583786796,-13.7709735373319,21.94251,21.94915
2923471813389328146,43.6631048672595,-13.7675297108627,23.4276,23.13043
2923471813389328141,44.4272028243504,-13.7749662153229,23.52501,23.11166







java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select nb.distance as dstArcMin,p.ra,p.dec,p.glon,p.glat,p.nuv_flux,p.fuv_flux, p.e_bv from fgetnearbyobjeq(18.05417,35.37194,  1/*arcmins*/) as nb, bcscat_mis as p /* band = NUV */ order by dstArcMin"

java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select nb.distance as dstArcMin from fgetnearbyobjeq(18.05417,35.37194,  1/*arcmins*/) as nb"
java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select top 10 p.ra, p.dec from bcscat_mis as p"





java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select objid, ra, dec, fuv_mag as fuv, nuv_mag as nuv from bcscat_ais where ra BETWEEN 18.0 AND 18.1 AND dec BETWEEN 35.2 AND 35.4"






java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select ra, dec, fuv_mag as fuv, nuv_mag as nuv from bcscat_mis where ra BETWEEN 17.9 AND 18.2 AND dec BETWEEN 35.0 AND 35.5"


java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select nb.distance as dstArcMin, p.objid, p.ra, p.dec, p.fuv_mag as fuv, p.nuv_mag as nuv from bcscat_ais as p, fgetnearbyobjeq(18.05417,35.37194,  1/*arcmins*/) as nb"
java -jar $HOME/PRO/KAT_PRO/GALEX/casjobs.jar execute "select nb.distance as dstArcMin from fgetnearbyobjeq(18.05417,35.37194,  1/*arcmins*/) as nb"