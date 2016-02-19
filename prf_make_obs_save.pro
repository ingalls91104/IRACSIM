PRO PRF_MAKE_OBS_FITSWRITE,header,image_cube_write,readmode,label,framtime,clocktick,channel,fovid,expid,sclk_obs,gain,readnoise,exptime,bunit,fluxconv,$
                            pxscal1,pxscal2,fn,wt,ra0,dec0,xloc,yloc,xframe,yframe,sclk_aintbeg,sclk_atimeend,rundate,reqkey,out_dir,file_type,path_unit,$
                            full_header,SILENT=Silent,OBJECT=OBJECT,HEADER_COMMENTS=header_comments
     IF STRCOMPRESS(readmode, /REMOVE_ALL) EQ 'SUB' THEN clocktick_string='0.01' ELSE clocktick_string='0.2'     
     IF N_ELEMENTS(HEADER) EQ 0 THEN mkhdr,header,image_cube_write
     IF N_ELEMENTS(OBJECT) EQ 0 THEN object=label
     SCLK_COLD = 746236800.000D0  ;; Cold mission start
     SCLK_WARM = 1061468046.437D0  ;; Warm mission, for calculating AINTBEG
     IF SCLK_OBS LT SCLK_WARM then sclk0 = sclk_cold ELSE sclk0 = sclk_warm
     aintbeg = sclk_aintbeg - sclk0
     atimeend = sclk_atimeend - sclk0 
     
     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','*******************THESE ARE SIMULATED DATA*******************',''],header,/COMMENT,LOCATION='CHNLNUM'
     SXADDPAR,header,'ORIGIN','Spitzer Science Center (FAKE DATA)','Organization generating this FITS file',BEFORE='CHNLNUM'
     SXADDPAR,header,'TELESCOP','Spitzer (FAKE DATA)','SPITZER Space Telescope',BEFORE='CHNLNUM'
     SXADDPAR,header,'INSTRUME','IRAC (FAKE DATA)','SPITZER Space Telescope instrument ID',BEFORE='CHNLNUM'
     SXADDPAR,header,'EXPTYPE','sci','Exposure Type',AFTER='CHNLNUM'
     SXADDPAR,header,'REQTYPE','AOR','Request type (AOR, IER, or SER)',AFTER='EXPTYPE'
     SXADDPAR,header,'AOT_TYPE','IracMapPC','Observation template type',AFTER='REQTYPE'
     SXADDPAR,header,'AORLABEL',label,'AOR Label',AFTER='AOT_TYPE'
     SXADDPAR,header,'FOVID',fovid,'Field of View ID',AFTER='AORLABEL'
     SXADDPAR,header,'FOVNAME',SPITZER_FOV(fovid),'Field of View Name',AFTER='FOVID'
     loc = 'CRVAL1'
     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','          / TIME AND EXPOSURE INFORMATION',''],header,/BLANK,LOCATION=loc
     mjd = time_sclk(sclk_obs,/SCLK_TO_MJD)
     dt = time_sclk(sclk_obs,/SCLK_TO_DAY_TIME)
     daytimestring = STRING(dt[2],dt[0],dt[1],dt[3],dt[4],dt[5],format='(i4,"-",i02,"-",i02,"T",i02,":",i02,":",f06.3)')
     SXADDPAR,header,'DATE_OBS',daytimestring,'Date & time (UTC) at DCE start',BEFORE=loc
     SXADDPAR,header,'MJD_OBS',mjd,'[days] MJD in UTC at DCE start (,JD-2400000.5)',BEFORE=loc
     SXADDPAR,header,'HMJD_OBS',mjd,'[days] Corresponding Helioc. Mod. Julian Date',BEFORE=loc
     SXADDPAR,header,'BMJD_OBS',mjd,'[days] Solar System Barycenter Mod. Julian Date',BEFORE=loc
     SXADDPAR,header,'SCLK_OBS',sclk_obs,'[sec] SCLK time (since 1/1/1980) at DCE start',BEFORE=loc
     SXADDPAR,header,'AORTIME',FLOAT(framtime),'[sec] Frameset selected in IRAC AOT' ,BEFORE=loc
     SXADDPAR,header,'SAMPTIME',FLOAT(clocktick_string),'[sec] Sample integration time',BEFORE=loc
     SXADDPAR,header,'FRAMTIME',FLOAT(framtime),'[sec] Time spent integrating (whole array)',BEFORE=loc
     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,'Photons in Well = Flux[photons/sec/pixel] * FRAMTIME',header,/COMMENT,LOCATION=loc
     SXADDPAR,header,'EXPTIME',exptime,'[sec] Effective integration time per pixel',BEFORE=loc
     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,'DN per pixel = Flux[photons/sec/pixel] / GAIN * EXPTIME',header,/COMMENT,LOCATION=loc
     SXADDPAR,header,'AINTBEG',aintbeg,'[Secs since IRAC turn-on] Time of integ. start',BEFORE=loc
     SXADDPAR,header,'ATIMEEND',atimeend,'[Secs since IRAC turn-on] Time of integ. end',BEFORE=loc
     SXADDPAR,header,'AFOWLNUM',fn,'Fowler number',BEFORE=loc
     SXADDPAR,header,'AWAITPER',wt,'['+clocktick_string+' sec] Wait period',BEFORE=loc                          
     SXADDPAR,header,'AREADMOD',FIX(STRMATCH(readmode,'SUB*')),'Full (0) or subarray (1)',BEFORE=loc
     SXADDPAR,header,'HDR_MODE','F','DCE taken in High Dynamic Range mode',BEFORE=loc

     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','          / TARGET AND POINTING INFORMATION',''],header,/BLANK,LOCATION=loc

     SXADDPAR,header,'OBJECT',Object,'Target Name',BEFORE=loc
     hms = SIXTY(SXPAR(header,'CRVAL1')/15.)
     hms_string = STRNG(hms,format='(i02,"h",i02,"m",f04.1,"s")')
     dms = SIXTY(SXPAR(header,'CRVAL2'))
     dms_string = STRNG(ROUND(dms),format='(i03,"d",i02,"m",i02,"s")')
     SXADDPAR,header,'RA_HMS',hms_string,'[hh:mm:ss.s] CRVAL1 as sexagesimal',AFTER='CRVAL2'
     SXADDPAR,header,'DEC_HMS',dms_string,'[dd:mm:ss] CRVAL2 as sexagesimal',AFTER='RA_HMS' 
     SXADDPAR,header,'RA_RQST',ra0,'[deg] Requested RA at CRPIX1, CRPIX2',AFTER='PA'
     SXADDPAR,header,'DEC_RQST',dec0,'[deg] Requested Dec at CRPIX1, CRPIX2',AFTER='RA_RQST'

     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','          / DISTORTION KEYWORDS',''],header,/BLANK,LOCATION='A_ORDER'
     
     SXADDPAR,header,'YLOCAT',yloc,'Y Position of Star 0 (simulated, average)',AFTER='BP_3_0'
     loc = 'YLOCAT'
     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','          / PHOTOMETRY',''],header,/BLANK,LOCATION=loc
     SXADDPAR,header,'BUNIT',bunit,'Units of image data',BEFORE=loc
     SXADDPAR,header,'FLUXCONV',fluxconv,'Flux Conv. factor (MJy/sr per DN/sec)',BEFORE=loc
     SXADDPAR,header,'GAIN',GAIN,'e/DN conversion',BEFORE=loc
     SXADDPAR,header,'RONOISE',readnoise,'[Electrons] Readout Noise from Array',BEFORE=loc
     
     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','          / GENERAL MAPPING KEYWORDS',''],header,/BLANK,LOCATION=loc
     SXADDPAR,header,'CYCLENUM',1,'Current cycle number',BEFORE=loc
     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','          / IRAC MAPPING KEYWORDS',''],header,/BLANK,LOCATION=loc
     SXADDPAR,header,'READMODE',readmode,'Readout mode (full or sub)',BEFORE=loc

     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','          / DATA FLOW KEYWORDS',''],header,/BLANK,LOCATION=loc
     SXADDPAR,header,'AORKEY',reqkey,'AOR or IER key. Astrnmy Obs Req/Instr Eng Req ',BEFORE=loc
     SXADDPAR,header,'EXPID',expid,'Exposure ID (0-9999)',BEFORE=loc
     SXADDPAR,header,'DCENUM',0,'DCE number (0-9999)',BEFORE=loc

     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,['','          / SIMULATION INFORMATION',''],header,/BLANK,LOCATION=loc
     IF N_ELEMENTS(HEADER_COMMENTS) NE 0 THEN SXADDHIST,header_comments,header,/COMMENT,LOCATION=loc
     SXADDPAR,header,'XLOCAT',xloc,'X Position of Star 0 (simulated, average)',BEFORE=loc
     FOR i=0,N_ELEMENTS(xframe)-1 DO SXADDPAR,header,'XYMEAN'+STRNG(i+1,format='(i02)'),'('+STRNG(xframe[i],format='(f10.6)')+','+STRNG(yframe[i],format='(f10.6)')+')','(X,Y) Position of Star 0 on subframe '+STRNG(i+1)
     hist = ['PRF_MAKE_OBS Ran '+rundate]
     IF ~KEYWORD_SET(full_header) THEN SXADDHIST,hist,header
     
     out_dir_full = out_dir + PATH_SEP() + 'r'+STRNG(reqkey)+PATH_SEP()+'ch'+STRNG(channel)+PATH_SEP()+path_unit
     IF ~FILE_TEST(out_dir_full,/DIRECTORY) THEN FILE_MKDIR,out_dir_full
     outfile = out_dir_full + PATH_SEP() + 'SPITZER_I'+STRNG(channel)+'_'+STRNG(reqkey)+'_'+STRNG(expid,format='(i04)')+'_0000_1_'+file_type
     WRITEFITS,outfile,image_cube_write,header
     IF ~KEYWORD_SET(SILENT) THEN PRINT,'Saving '+outfile 
     IF ~KEYWORD_SET(full_header) THEN full_header=1
RETURN
END

PRO PRF_MAKE_OBS_SAVE,header,image_cube,noise_cube,framtime,clocktick,fn, wt,channel,readmode,fovid,expid,sclk_obs,gain,readnoise,exptime,fluxconv,$
                      pxscal1,pxscal2,ra0,dec0,xloc,yloc,xframe,yframe,sclk_aintbeg,sclk_atimeend,label,reqkey,out_dir,$
                      SURFACE_BRIGHTNESS=surface_brightness,DN=dn,VERBOSE=verbose,SILENT=silent,OBJECT=object,HEADER_COMMENTS=header_comments
     rundate = systime()
     ;;; image_cube is input in electrons
     image_cube_elec = image_cube
     noise_cube_elec = noise_cube   
     full_header = 0    
     SWITCH 1 of 
        KEYWORD_SET(SURFACE_BRIGHTNESS): BEGIN
           image_cube_surf = FLOAT(image_cube_elec * FLUXCONV / GAIN / exptime)
           bunit = 'MJy/sr' 
           path_unit = 'bcd'
           file_type = 'bcd.fits'
           noise_cube_write = FLOAT(noise_cube_elec * FLUXCONV / GAIN / exptime)
           file_type_unc = 'bunc.fits'
           PRF_MAKE_OBS_FITSWRITE,header,image_cube_surf,readmode,label,framtime,clocktick,channel,fovid,expid,sclk_obs,gain,readnoise,exptime,bunit,fluxconv,$
                            pxscal1,pxscal2,fn,wt,ra0,dec0,xloc,yloc,xframe,yframe,sclk_aintbeg,sclk_atimeend,rundate,reqkey,out_dir,file_type,path_unit,full_header,$
                            SILENT=silent,OBJECT=OBJECT,HEADER_COMMENTS=header_comments
           PRF_MAKE_OBS_FITSWRITE,header,noise_cube_write,readmode,label,framtime,clocktick,channel,fovid,expid,sclk_obs,gain,readnoise,exptime,bunit,fluxconv,$
                            pxscal1,pxscal2,fn,wt,ra0,dec0,xloc,yloc,xframe,yframe,sclk_aintbeg,sclk_atimeend,rundate,reqkey,out_dir,file_type_unc,path_unit,full_header,$
                            SILENT=silent,OBJECT=OBJECT,HEADER_COMMENTS=header_comments
        END
        KEYWORD_SET(DN): BEGIN
           image_cube_dn =  ROUND( image_cube_elec / GAIN ) 
           bunit = 'DN'
           path_unit = 'raw'
           file_type = 'dce.fits'
           PRF_MAKE_OBS_FITSWRITE,header,image_cube_dn,readmode,label,framtime,clocktick,channel,fovid,expid,sclk_obs,gain,readnoise,exptime,bunit,fluxconv,$
                            pxscal1,pxscal2,fn,wt,ra0,dec0,xloc,yloc,xframe,yframe,sclk_aintbeg,sclk_atimeend,rundate,reqkey,out_dir,file_type,path_unit,full_header,$
                            SILENT=silent,OBJECT=OBJECT,HEADER_COMMENTS=header_comments
        END
        ELSE: BEGIN
           bunit = 'e-'
           path_unit='electron'
           file_type = 'elec.fits'
           PRF_MAKE_OBS_FITSWRITE,header,image_cube_elec,readmode,label,framtime,clocktick,channel,fovid,expid,sclk_obs,gain,readnoise,exptime,bunit,fluxconv,$
                            pxscal1,pxscal2,fn,wt,ra0,dec0,xloc,yloc,xframe,yframe,sclk_aintbeg,sclk_atimeend,rundate,reqkey,out_dir,file_type,path_unit,full_header,$
                            SILENT=silent,OBJECT=OBJECT,HEADER_COMMENTS=header_comments
        END
     ENDSWITCH
RETURN
END

