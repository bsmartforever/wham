function rebin_spectra,spectra,new_npoints

num_spectra=n_elements(spectra)

    ;Remove the vel, data, and var tags from structure
    temp=struct_trimtags(spectra,except_tags=['vel','data','var'])
    name_of_tags=tag_names(temp)
    ;Add the vel, data, and var tags back to the structure with the altered length.
    temp_add_tags_back=replicate({vel:fltarr(new_npoints),data:fltarr(new_npoints),var:fltarr(new_npoints)},num_spectra)
    rebin_spectra=struct_addtags(temp,temp_add_tags_back)

for i=0, num_spectra-1 do begin
    x=spectra[i].vel
    y=spectra[i].data
    y_err=sqrt(spectra[i].var)

    xnew=frebin(x,new_npoints)
    ynew=interpol(y,x,xnew)
    ynew_err=linear_interpol_err(x,xnew,y,y_err)

    rebin_spectra[i].vel=xnew
    rebin_spectra[i].data=ynew
    rebin_spectra[i].var=(ynew_err)^2.0

endfor

return,rebin_spectra;

end