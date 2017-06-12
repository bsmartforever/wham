pro lab_gauss_decomp

readcol,'/d/wham3/hi/Nidever/msfinal2.txt',index,mlon,mlat,glon,glat,rms,noise,height,vcen,width,height_err,vcen_err,width_err

num=n_elements(index)

lab=replicate({glon:0.0,glat:0.0,mlon:0.0,mlat:0.0,rms:0.0,noise:0.0,area:0.0,area_err:0.0,height:0.0,height_err:0.0,vcen:0.0,vcen_err:0.0,width:0.0,width_err:0.0},num)

lab.glon=glon
lab.glat=glat
lab.mlon=mlon
lab.mlat=mlat
lab.rms=rms
lab.noise=noise
lab.area=height*width*sqrt(2.0*!pi)
	area_err1=width*height_err
	area_err2=lab.area*width_err
lab.area_err=sqrt(area_err1^2.0+area_err2^2.0)*sqrt(2.0*!pi)
lab.height=height
lab.height_err=height_err
lab.vcen=vcen
lab.vcen_err=vcen_err
lab.width=width
lab.width_err=width_err


end