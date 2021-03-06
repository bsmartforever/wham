;+
; NAME:
;   ISEDFIT_MODELS
;
; PURPOSE:
;   Synthesize photometry on a grid of redshift for all the
;   models generated by BUILD_MONTEGRIDS.
;
; INPUTS:
;   isedfit_paramfile - iSEDfit parameter file which specifies the
;     filter, the redshift range, the cosmological model (which
;     affects the luminosity distance), and which specifies whether or
;     not to include IGM attenuation
;
; OPTIONAL INPUTS:
;   params - data structure with the same information contained in
;     ISEDFIT_PARAMFILE (over-rides ISEDFIT_PARAMFILE)
;   supergrid_paramfile - file describing the supergrid parameters 
;   thissupergrid - if SUPERGRID_PARAMFILE contains multiple
;     supergrids then build this SUPERGRID (may be a vector)
;   isedfit_dir - base pathname for iSEDfit files; the Monte Carlo
;     grids can be found in ISEDFIT_DIR/MONTEGRIDS, or in
;     MONTEGRIDS_DIR
;   montegrids_dir - override ISEDFIT_DIR+'/'+MONTEGRIDS
;   use_redshift - use this redshift array instead of constructing the
;     redshift array from the parameters given in the
;     ISEDFIT_PARAMFILE parameter file; useful for when you have a
;     relatively sample of objects with well-determined redshifts
;     spanning a wide redshift range [NZZ]
;
; KEYWORD PARAMETERS:
;   clobber - overwrite existing files of the same name (the default
;     is to check for existing files and if they exist to exit
;     gracefully)  
;
; OUTPUTS:
;   Binary FITS tables containing the convolved photometry are written
;   to ISEDFIT_DIR, but these should be transparent to most users.
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;   Should include a bit more info about the output data tables. 
;
; EXAMPLES:
;
; MODIFICATION HISTORY:
;   J. Moustakas, 2011 Sep 01, UCSD - I began writing iSEDfit in 2005
;     while at the U of A, adding updates off-and-on through 2007;
;     however, the code has evolved so much that the old modification
;     history became obsolete!  Future changes to the officially
;     released code will be documented here.
;   jm13jan13siena - documentation rewritten and updated to reflect
;     many major changes
;
; Copyright (C) 2011, 2013, John Moustakas
; 
; This program is free software; you can redistribute it and/or modify 
; it under the terms of the GNU General Public License as published by 
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
; 
; This program is distributed in the hope that it will be useful, but 
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details. 
;-

pro isedfit_models, isedfit_paramfile, params=params, supergrid_paramfile=supergrid_paramfile, $
  thissupergrid=thissupergrid, isedfit_dir=isedfit_dir, montegrids_dir=montegrids_dir, $
  use_redshift=use_redshift, clobber=clobber

    if n_elements(isedfit_paramfile) eq 0 and n_elements(params) eq 0 then begin
       doc_library, 'isedfit_models'
       return
    endif

; read the parameter file; parse to get the relevant path and
; filenames
    if (n_elements(isedfit_dir) eq 0) then isedfit_dir = './'
    if (n_elements(montegrids_dir) eq 0) then montegrids_dir = isedfit_dir+'montegrids/'
    if (n_elements(params) eq 0) then params = $
      read_isedfit_paramfile(isedfit_paramfile,use_redshift=use_redshift)

; read the SUPERGRID parameter file
    if n_elements(supergrid_paramfile) eq 0 then begin
       splog, 'SUPERGRID parameter file required'
       return
    endif
    
    super = read_supergrid_paramfile(supergrid_paramfile,supergrid=thissupergrid)
    if n_elements(thissupergrid) eq 0 then thissupergrid = super.supergrid
       
; fit each SUPERGRID separately
    nsuper = n_elements(thissupergrid)
    if nsuper gt 1 then begin
       for ii = 0, nsuper-1 do begin
          isedfit_models, params=params, supergrid_paramfile=supergrid_paramfile, $
            thissupergrid=thissupergrid[ii], isedfit_dir=isedfit_dir, $
            montegrids_dir=montegrids_dir, clobber=clobber
       endfor
       return
    endif 

    fp = isedfit_filepaths(params,supergrid_paramfile=supergrid_paramfile,$
      thissupergrid=thissupergrid,isedfit_dir=isedfit_dir,montegrids_dir=montegrids_dir)
    if (file_test(fp.modelspath,/dir) eq 0) then begin
       splog, 'Creating directory '+fp.modelspath
       spawn, 'mkdir -p '+fp.modelspath, /sh
    endif

    chunkfile = fp.modelspath+fp.isedfit_models_chunkfiles[0] ; check the first file
    if file_test(chunkfile+'.gz',/regular) and $
      (keyword_set(clobber) eq 0) then begin
       splog, 'First ChunkFile '+chunkfile+' exists; use /CLOBBER'
       return
    endif

    splog, 'SYNTHMODELS='+super.synthmodels+', '+$
      'REDCURVE='+strtrim(super.redcurve,2)+', IMF='+$
      super.imf+', '+'SFHGRID='+$
      string(super.sfhgrid,format='(I2.2)')

; filters and redshift grid
    filterlist = strtrim(params.filterlist,2)
    nfilt = n_elements(filterlist)

    splog, 'Synthesizing photometry in '+$
      string(nfilt,format='(I0)')+' bandpasses:'
    niceprint, replicate('  ',nfilt), filterlist

    redshift = params.redshift
    nredshift = n_elements(redshift)
    splog, 'Redshift grid: '
    splog, '  Nz   = '+string(nredshift,format='(I0)')
    splog, '  zmin = '+strtrim(string(min(redshift),format='(F12.3)'),2)
    splog, '  zmax = '+strtrim(string(max(redshift),format='(F12.3)'),2)

    if (im_double(min(redshift)) le 0D) then begin
       splog, 'REDSHIFT should be positive and non-zero'
       return
    endif
    
    pc10 = 3.085678D19 ; fiducial distance [10 pc in cm]
    dlum = pc10*10D^(lf_distmod(redshift,omega0=params.omega0,$ ; [cm]
      omegal0=params.omegal)/5D)/params.h100 
;   dlum = dluminosity(redshift,/cm) ; luminosity distance [cm]

; IGM attenuation    
    if params.igm then begin
       splog, 'Reading IGM attenuation lookup table'
       igmgrid = mrdfits(getenv('IMPRO_DIR')+'/etc/igmtau_grid.fits.gz',1)
    endif else begin
       splog, 'Neglecting IGM absorption'
    endelse 

; now loop on each "chunk" of models and build modelmaggies
    nchunk = n_elements(fp.sfhgrid_chunkfiles)
    splog, 'NCHUNK = '+string(nchunk,format='(I0)')
    t1 = systime(1)
    mem1 = memory(/current)
    for ichunk = 0, nchunk-1 do begin
       t0 = systime(1)
       mem0 = memory(/current)
       splog, 'Reading '+fp.sfhgrid_chunkfiles[ichunk]
       chunk = mrdfits(fp.sfhgrid_chunkfiles[ichunk],1,/silent)
       nage = n_elements(chunk[0].age)
       ndim = size(chunk.flux,/n_dim)
       sz = size(chunk.flux,/dim)
       npix = sz[0]
       if (nage eq 1) then begin
          if (ndim eq 1) then nmodel = 1L else nmodel = sz[1]
       endif else begin
          if (ndim eq 2) then nmodel = 1L else nmodel = sz[2]
       endelse
       distfactor = rebin(reform((pc10/dlum)^2.0,nredshift,1),nredshift,nmodel)
; initialize the output structure
       isedfit_models = struct_trimtags(chunk,except=['WAVE','FLUX'])
       isedfit_models = struct_addtags(temporary(isedfit_models),$
         replicate({modelmaggies: fltarr(nfilt,nage,nredshift)},nmodel))
; build the IGM absorption vector
       if (params.igm eq 1) then begin
          igm = fltarr(npix,nredshift)
          for iz = 0, nredshift-1 do begin
             zwave = chunk[0].wave*(1.0+redshift[iz])
             windx = findex(igmgrid.wave,zwave)
             zindx = findex(igmgrid.zgrid,redshift[iz])
             igm[*,iz] = interpolate(igmgrid.igm,windx,zindx,/grid,missing=1.0)
;            plot, zwave, igm[*,iz], xr=[0,9000], xsty=3, ysty=3
;            get_element, igmgrid.zgrid, redshift[iz], jj                                   
;            plot, igmgrid.wave, igmgrid.igm[*,jj], ysty=3
          endfor
       endif
       nwave = n_elements(chunk[0].wave)
       wave_edges = k_lambda_to_edges(chunk[0].wave)
       for iage = 0, nage-1 do begin
          if ((iage mod 5) eq 0) then print, format='("Chunk ",I0,"/",I0,", '+$
            'Age ",I0,"/",I0,A10,$)', ichunk+1, nchunk, $
            iage, nage, string(13b)
; jm09may16nyu - added IGM attenuation
; project the filters onto each SED, including IGM attenuation, and
; scale by the distance ratio
          flux = chunk.flux[*,iage]
; fast code, but doesn't include IGM attenuation
          if (params.igm eq 0) then begin
             k_projection_table, rmatrix, flux, wave_edges, redshift, filterlist, /silent
             for ff = 0, nfilt-1 do rmatrix[*,*,ff] = rmatrix[*,*,ff]*distfactor
             isedfit_models.modelmaggies[*,iage,*] = $
               reform(transpose(rmatrix,[2,0,1]),nfilt,1,nredshift,nmodel)
          endif else begin
; reasonably fast code that includes IGM absorption
;            for iz = 207, 207 do begin
             for iz = 0, nredshift-1 do begin
                bigigm = rebin(igm[*,iz],nwave,nmodel)
                k_projection_table, rmatrix1, flux*bigigm, wave_edges, $
                  redshift[iz], filterlist, /silent
                isedfit_models.modelmaggies[*,iage,iz] = $
                  reform(transpose(rmatrix1*(pc10/dlum[iz])^2.0,[2,0,1]))
                endfor
          endelse
;; very slow!
;         for iz = 0, nredshift-1 do begin
;            for it = 0, ngood-1 do begin
;               zwave = chunk[0].wave*(1.0+redshift[iz])
;               igm = interpolate(igmgrid.igm,findex(igmgrid.wave,zwave),$
;                 findex(igmgrid.zgrid,redshift[iz]),/grid)
;               isedfit_models[good[it]].modelmaggies[*,iage,iz] = $
;                 k_project_filters(k_lambda_to_edges(zwave),$
;                 igm*flux[*,it]/(1.0+redshift[iz]),filterlist=filterlist,$
;                 /silent)*(pc10/dlum[iz])^2.0
;            endfor
;         endfor
; test that I'm doing the distance ratio right
;         zindx = 10 & mindx = 5
;         ff = flux[*,mindx]/(1.0+redshift[zindx])*distfactor[zindx]
;         ww = k_lambda_to_edges(chunk[0].wave)*(1.0+redshift[zindx])
;         mm = k_project_filters(ww,ff,filterlist=filterlist)
;         niceprint, mm, rmatrix[zindx,mindx,*]
       endfor 
       if (ichunk eq 0) then begin
          splog, 'First chunk = '+string((systime(1)-t0)/60.0,format='(G0)')+$
            ' minutes, '+strtrim(string((memory(/high)-mem0)/1.07374D9,format='(F12.3)'),2)+' GB'
       endif
       outfile = fp.modelspath+fp.isedfit_models_chunkfiles[ichunk]
       im_mwrfits, isedfit_models, outfile, /clobber
    endfor
    splog, 'All chunks = '+string((systime(1)-t1)/60.0,format='(G0)')+$
      ' minutes, '+strtrim(string((memory(/high)-mem1)/1.07374D9,format='(F12.3)'),2)+' GB'

return
end
