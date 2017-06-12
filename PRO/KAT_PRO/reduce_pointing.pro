pro reduce_pointing, dir_base_data,dir_data,line_list,on_files,$
  save_dir,off_files=off_files,save_name=save_name, $
  ftsext=ftsext, scale_velocity=scale_velocity

;ThAr_files='thcalib'

if keyword_set(save_dir)-1 then save_dir=validate_directory(save_dir)

    name=save_dir+on_files+'.ps'
    if keyword_set(save_name) then name=save_name

    entry_device=!d.name
    set_plot,'PS'
    !p.font=0
    device,filename=name,bits_per_pixel=16,color=1
    device,/portrait,encapsulated=0, /helvetica
    device,ysize=10,yoffset=(11.0-10)*0.5,/inches

!p.charsize=1.25
!p.thick=5
!x.thick=5
!y.thick=5

!p.position=[0.05,0.05,0.95,0.95]

if (NOT keyword_set(ftsext)) then ftsext='PROCSPEC'

for j=0,n_elements(dir_data)-1 do begin
   path=dir_base_data+dir_data[j]
   for k=0,n_elements(line_list)-1 do begin

       test=file_which(path,line_list[k])

       if test ne '' then begin
          line=line_list[k]
          dir=dir_base_data+dir_data[j]+'/'+line

          result = FILE_TEST(dir+'/combo/'+on_files+'*fts')
          if result ne 1 then begin
            print,''
            print,'*** File does not exist ***'
            print,'File: ',dir+'/combo/'+on_files+'*fts'
            print,''
            return
          endif

          count=0
          On=readobs(on_files,dir,ftsext=ftsext,/ext,count=count,/quiet)
          good_t=where(abs(on.ccdtemp+101.2) lt .2,count_t)
          if count_t ne 0 then on=on[good_t] else on=on
          good_zd=where(on.zd le 65.,count_zd)
          if count_zd ne 0 then on=on[good_zd] else on=on
          if line eq 'ha_blue' then begin
	      ftsext='RAWSPEC'
	      On=readobs(on_files,dir,ftsext='RAWSPEC',/ext,count=count,/quiet)
	      if count eq 0 then goto,next
	      exptime=hdr_fits_keyword(dir+'/raw/'+on_files+'*','EXPTIME')
 	      On.data=On.data/exptime
	      print,' '
	      print,' *** NO FLAT FIELDING ***'
	      print,'  *** USING  RAWSPEC ***'
	      print,'*** DIVIDED BY EXPTIME ***
              print,' '
	  endif
          if count eq 0 then goto, next 
          On=rm_outliers(On)         

          if total(on[n_elements(on)-1].data) eq 0 then begin
 
             print,''
             print,'*** FTS EXTENSION DOES NOT EXIST ***'
             print,'   *** RUN COR_SPEC_CTIO,/CHOP ***' 
             print,'     *** IN DIRECTORY ' + DIR_DATA[J] +' ***'
             goto,exit  

          endif

          off_list=strarr(3)
	  if (NOT keyword_set(off_files)) then off_list=on_files+['_off1*','_off2*','_off3*'] $
	  else off_list[0:n_elements(off_files)-1]=off_files+'*'

          test_arr=strarr(3)
          for m=0, 3-1 do begin
              test_arr[m]=file_which(path+'/'+line_list[k]+'/combo',off_list[m])
          endfor

          off_list=strarr(3)
          off_list=on_files+['_off1','_off2','_off3']

	  if (NOT keyword_set(off_files)) then off_list=on_files+['_off1','_off2','_off3'] $
	  else off_list[0:n_elements(off_files)-1]=off_files

             if test_arr[0] ne '' then off1_files=off_list[0]
             if test_arr[1] ne '' then off2_files=off_list[1]
             if test_arr[2] ne '' then off3_files=off_list[2]

          if test_arr[0] ne '' then begin
             result = FILE_TEST(dir+'/combo/'+off1_files+'*fts')
             if result ne 1 then begin
               print,''
               print,'*** File does not exist ***'
               print,'File: ',dir+'/combo/'+off1_files+'*fts'
               print,''
               return
             endif
             off1=readobs(off1_files,dir,ftsext=ftsext,/ext,count=count,/quiet)
             ccdtemp=hdr_fits_keyword(dir+'/raw/'+off1_files+'*','CCDTEMP')
             good_t=where(abs(off1.ccdtemp+101.2) lt .2,count_t)
             if count_t ne 0 then off1=off1[good_t] else off1=off1   
             good_zd=where(off1.zd le 65.,count_zd)
             if count_zd ne 0 then off1=off1[good_zd] else off1=off1          
             if line eq 'ha_blue' then begin
	               off1=readobs(off1_files,dir,ftsext='RAWSPEC',/ext,count=count,/quiet)
		             exptime=hdr_fits_keyword(dir+'/raw/'+off1_files+'*','EXPTIME')
 		             off1.data=off1.data/exptime
                 ccdtemp=hdr_fits_keyword(dir+'/raw/'+off1_files+'*','CCDTEMP')
                 good_t=where(abs(off1.ccdtemp+101.2) lt .2,count_t)
                 if count_t ne 0 then off1=off1[good_t] else off1=off1 
                good_zd=where(off1.zd le 65.,count_zd)
                 if count_zd ne 0 then off1=off1[good_zd] else off1=off1
	     endif
             off1=rm_outliers(off1) 
          endif
          if test_arr[1] ne '' then begin
             result = FILE_TEST(dir+'/combo/'+off2_files+'*fts')
             if result ne 1 then begin
               print,''
               print,'*** File does not exist ***'
               print,'File: ',dir+'/combo/'+off2_files+'*fts'
               print,''
               return
             endif
             off2=readobs(off2_files,dir,ftsext=ftsext,/ext,count=count,/quiet)
             if line eq 'ha_blue' then begin
	              off2=readobs(off2_files,dir,ftsext='RAWSPEC',/ext,count=count,/quiet)
		             exptime=hdr_fits_keyword(dir+'/raw/'+off2_files+'*','EXPTIME')
 		             off2.data=off2.data/exptime
                 ccdtemp=hdr_fits_keyword(dir+'/raw/'+off2_files+'*','CCDTEMP')
                 good_t=where(abs(off2.ccdtemp+101.2) lt .2,count_t)
                 if count_t ne 0 then off2=off2[good_t] else off2=off2 
                 good_zd=where(off2.zd le 65.,count_zd)
                 if count_zd ne 0 then off2=off2[good_zd] else off2=off2 
   		endif
             off2=rm_outliers(off2) 
          endif
          if test_arr[2] ne '' then begin
             result = FILE_TEST(dir+'/combo/'+off3_files+'*fts')
             if result ne 1 then begin
               print,''
               print,'*** File does not exist ***'
               print,'File: ',dir+'/combo/'+off3_files+'*fts'
               print,''
               return
             endif
             off3=readobs(off3_files,dir,ftsext=ftsext,/ext,count=count,/quiet)
             if line eq 'ha_blue' then begin
	               off3=readobs(off3_files,dir,ftsext='RAWSPEC',/ext,count=count,/quiet)
	             	 exptime=hdr_fits_keyword(dir+'/raw/'+off3_files+'*','EXPTIME')
 		             off3.data=off3.data/exptime
                 ccdtemp=hdr_fits_keyword(dir+'/raw/'+off3_files+'*','CCDTEMP')
                 good_t=where(abs(off3.ccdtemp+101.2) lt .2,count_t)
                 if count_t ne 0 then off3=off3[good_t] else off3=off3
                 good_zd=where(off3.zd le 65.,count_zd)
                 if count_zd ne 0 then off3=off3[good_zd] else off3=off3 
	     endif
             off3=rm_outliers(off3) 
          endif

          ;result = FILE_TEST(dir+'/combo/'+on_files+'*fts')
          ;if result eq 1 then begin
          ;   ThAr_calib=readobs(ThAr_files,dir,ftsext='PROCSPEC',/ext,count=count,/quiet)
          ;  if count ne 0 then ThAr_calib=ThAr_calib[0]
          ;endif


	  if (ftsext eq 'RAWSPEC') or (ftsext eq 'PROCSPEC') or keyword_set(scale_velocity) then scale_velocity=1 else scale_velocity=0
          if test_arr[0] ne '' then on_off1_ave=on_off(on,off1,/bin,scale_velocity=scale_velocity)
          if test_arr[1] ne '' then on_off2_ave=on_off(on,off2,/bin,scale_velocity=scale_velocity)
          if test_arr[2] ne '' then on_off3_ave=on_off(on,off3,/bin,scale_velocity=scale_velocity)

          if test_arr[0] ne '' then $
             writespe,save_dir+on_files+'_'+off1_files+'_'+dir_data[j]+'_'+line+'.spe',$
             On_Off1_ave.vel,On_Off1_ave.data,$
             On_Off1_ave.var,$
             n_elements(On_Off1_ave.vel)
          if test_arr[1] ne '' then $
             writespe,save_dir+on_files+'_'+off2_files+'_'+dir_data[j]+'_'+line+'.spe',$
             On_Off2_ave.vel,On_Off2_ave.data,$
             On_Off2_ave.var,$
             n_elements(On_Off2_ave.vel)
          if test_arr[2] ne '' then $
             writespe,save_dir+on_files+'_'+off3_files+'_'+dir_data[j]+'_'+line+'.spe',$
             On_Off3_ave.vel,On_Off3_ave.data,$
             On_Off3_ave.var,$
             n_elements(On_Off3_ave.vel)

          !p.position=[0.05,0.525,0.95,0.95]

	  ;If no off1, then define a on_off1_ave with elements equal to 0 for plotting.
          ;To avoid error with the normal function, data[0]=1
          if test_arr[0] eq '' then begin
             if test_arr[1] ne '' then begin
                on_off1_ave=on_off2_ave
                on_off1_ave.vel=on_off2_ave.vel
                on_off1_ave.data=on_off2_ave.data*0.0
                on_off1_ave.data[0]=1.0
                on_off1_ave.var=on_off2_ave.var*0.0
                on_off1_ave.name=''
             endif

             if test_arr[2] ne '' then begin
                on_off1_ave=on_off3_ave
                on_off1_ave.vel=on_off3_ave.vel
                on_off1_ave.data=on_off3_ave.data*0.0
                on_off1_ave.data[0]=1.0
                on_off1_ave.var=on_off3_ave.var*0.0
                on_off1_ave.name=''
             endif
          endif
          plot,on_off1_ave.vel,normal(on_off1_ave.data),$
               ytitle='Normalized Intensity [ADU/(km/s)]',$
               title=dir_data[j]+' '+line+' '+on_off1_ave.name,$
               yrange=[0,1]
          if test_arr[1] ne '' then oplot,on_off2_ave.vel,normal(on_off2_ave.data),linestyle=3
          if test_arr[2] ne '' then oplot,on_off3_ave.vel,normal(on_off3_ave.data),linestyle=2

          junk=where(test_arr ne '',num_off_files)
          if num_off_files eq 1 then legend,['On-Off1'],linestyle=[0],box=0,/right $
          else if num_off_files eq 2 then legend,['On-Off1','On-Off2'],linestyle=[0,3],box=0,/right $
          else if num_off_files eq 3 then legend,['On-Off1','On-Off2','On-Off3'],linestyle=[0,3,2],box=0,/right

          legend,['On-Off1','On-Off2'],linestyle=[0,3],box=0,/right
   

          !p.position=[0.05,0.05,0.95,0.475]

          plot,on[0].vel,on[0].data,/noerase,$
               xtitle='LSR velocity [km/s]',$
               ytitle='Ons and Offs [ADU/(km/s)]'
             for z=1,n_elements(on)-1 do oplot,on[z].vel,on[z].data
          if test_arr[0] ne '' then begin
             oplot,off1[0].vel,off1[0].data,linestyle=0,color=fsc_color('red')
             for z=1,n_elements(off1)-1 do oplot,off1[z].vel,off1[z].data,linestyle=0,color=fsc_color('red')
          endif
          if test_arr[1] ne '' then begin
             oplot,off2[0].vel,off2[0].data,linestyle=0,color=fsc_color('blue')
             for z=1,n_elements(off2)-1 do oplot,off2[z].vel,off2[z].data,linestyle=0,color=fsc_color('blue')
          endif
          if test_arr[2] ne '' then begin
             oplot,off3[0].vel,off3[0].data,linestyle=0,color=fsc_color('orchid')
             for z=1,n_elements(off3)-1 do oplot,off3[z].vel,off3[z].data,linestyle=0,color=fsc_color('orchid')
          endif

          junk=where(test_arr ne '',num_off_files)
          if (test_arr[0] ne '') then legend,['On','Off1'],linestyle=[0,0],box=0,/right, $
               color=[fsc_color('black'),fsc_color('red')] $
          else if (test_arr[1] ne '') then legend,['On','Off1','Off2'],linestyle=[0,0,0],box=0,/right, $
               color=[fsc_color('black'),fsc_color('red'),fsc_color('blue')] $
          else if (test_arr[2] ne '') then legend,['On','Off1','Off2','Off3'],linestyle=[0,0,0,0],box=0,/right,$
               color=[fsc_color('black'),fsc_color('red'),fsc_color('blue'),fsc_color('orchid')]

          set_plot,'x'

                 !p.position=[0.05,0.525,0.95,0.95]

          plot,on_off1_ave.vel,normal(on_off1_ave.data),$
               ytitle='Normalized Intensity [ADU/(km/s)]',$
               title=dir_data[j]+' '+line+' '+on_off1_ave.name
          if test_arr[1] ne '' then oplot,on_off2_ave.vel,normal(on_off2_ave.data),linestyle=3
          if test_arr[2] ne '' then oplot,on_off3_ave.vel,normal(on_off3_ave.data),linestyle=2

          junk=where(test_arr ne '',num_off_files)
          if num_off_files eq 1 then legend,['On-Off1'],linestyle=[0],box=0,/right $
          else if num_off_files eq 2 then legend,['On-Off1','On-Off2'],linestyle=[0,3],box=0,/right $
          else if num_off_files eq 3 then legend,['On-Off1','On-Off2','On-Off3'],linestyle=[0,3,2],box=0,/right

          legend,['On-Off1','On-Off2'],linestyle=[0,3],box=0,/right
   

          !p.position=[0.05,0.05,0.95,0.475]

          plot,on[0].vel,on[0].data,/noerase,$
               xtitle='LSR velocity [km/s]',$
               ytitle='Ons and Offs [ADU/(km/s)]',color=fsc_color('white')
             for z=1,n_elements(on)-1 do oplot,on[z].vel,on[z].data
          if test_arr[0] ne '' then begin
             oplot,off1[0].vel,off1[0].data,linestyle=0,color=fsc_color('red')
             for z=1,n_elements(off1)-1 do oplot,off1[z].vel,off1[z].data,linestyle=0,color=fsc_color('red')
          endif
          if test_arr[1] ne '' then begin
             oplot,off2[0].vel,off2[0].data,linestyle=0,color=fsc_color('blue')
             for z=1,n_elements(off2)-1 do oplot,off2[z].vel,off2[z].data,linestyle=0,color=fsc_color('blue')
          endif
          if test_arr[2] ne '' then begin
             oplot,off3[0].vel,off3[0].data,linestyle=0,color=fsc_color('orchid')
             for z=1,n_elements(off3)-1 do oplot,off3[z].vel,off3[z].data,linestyle=0,color=fsc_color('orchid')
          endif

          junk=where(test_arr ne '',num_off_files)
          if (test_arr[0] ne '') then legend,['On','Off1'],linestyle=[0,0],box=0,/right, $
               color=[fsc_color('white'),fsc_color('red')] $
          else if (test_arr[1] ne '') then legend,['On','Off1','Off2'],linestyle=[0,0,0],box=0,/right, $
               color=[fsc_color('white'),fsc_color('red'),fsc_color('blue')] $
          else if (test_arr[2] ne '') then legend,['On','Off1','Off2','Off3'],linestyle=[0,0,0,0],box=0,/right,$
               color=[fsc_color('white'),fsc_color('red'),fsc_color('blue'),fsc_color('orchid')]


          set_plot,'ps'
          pause

       endif
       next:
   endfor
endfor

exit:

    !p.multi=0
    !p.region=0
    !p.position=1
    !p.thick=1
    !x.thick=1
    !y.thick=1
    !p.color=fsc_color('black')

    device,/close_file
    set_plot,entry_device
    Close, /All

    set_plot,'x'

end