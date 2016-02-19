FUNCTION SPITZER_FOV,fovnum
;
;   Return the Spitzer FOV name when input the FOV number.  Currently only filled for
;   the Warm Mission.
;   
;   
frame_table = HASH($
      '0','Undefined',$
      '1','Telescope_Boresight',$
      '2','CTA_frame',$
      '4','PCRS_1_A',$
      '5','PCRS_1_B',$
      '8','PCRS_2_A',$
      '9','PCRS_2_B',$
      '11','STA1_boresight',$
      '12','STA2_boresight',$
      '13','HGA_boresight',$
      '14','Plus_Y_LGA_boresight',$
      '15','Minus_Y_LGA_boresight',$
      '67','IRAC_Center_of_3.6&5.8umArray',$
      '68','IRAC_Center_of_3.6umArray',$
      '69','IRAC_Center_of_5.8umArray',$
      '70','IRAC_Center_of_3.6umSub-array',$
      '72','IRAC_Center_of_5.8umSub-array',$
      '74','IRAC_Center_of_4.5&8.0umArray',$
      '75','IRAC_Center_of_4.5umArray',$
      '76','IRAC_Center_of_8.0umArray',$
      '77','IRAC_Center_of_4.5umSub-Array',$
      '79','IRAC_Center_of_8.0umSub-Array',$
      '81','IRAC_FOV_bwn_3.6&5.8_and_4.5&8.0_for_mapping'$
      )
      
      IF frame_table.HasKey(STRNG(fovnum)) THEN RETURN,frame_table[STRNG(fovnum)] ELSE RETURN,''
END