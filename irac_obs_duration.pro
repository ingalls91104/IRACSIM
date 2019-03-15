PRO IRAC_OBS_DURATION,framtime,NREPEAT=nrepeat,DURATION=duration,SUB=sub,INTEGRATION_TIME=integration_time,FN=fn,WT=wt,EXPTIME=exptime,CLOCKTICK=clocktick,$
                      TLATENCY=tlatency,TRESET=treset
                      
treset = 8d-3;; Make it divisible by 1e-3 -- actually is 8.221e-3   ;;; Time before first pedestal read after start of DCE
tlatency = 0.0; 0.4d0 ;;; Time between command being issued by IRAC and received by the observatory  -- for now set to zero 
fownum = HASH('0.02S',1,'0.1S',2,'0.4S',4,'2S',8,'0.4F',1,'2F',4,'6F',8,'12F',8,'30F',32,'100F',32)
wait_tick = HASH('0.02S',0,'0.1S',6,'0.4S',32,'2S',184,'0.4F',0,'2F',2,'6F',14,'12F',44,'30F',86,'100F',436)
  
IF KEYWORD_SET(SUB) THEN BEGIN
    clocktick = 0.01d0
    readstring = 'S'
    nz = 64
ENDIF ELSE BEGIN 
    clocktick = 0.2d0
    readstring = 'F'
    nz = 1
ENDELSE
  
framestring = framtime+readstring                      
 
fn = fownum[framestring]
wt = wait_tick[framestring]
exptime = clocktick * (FN + WT)
integration_time = clocktick * (2.d0*FN + WT)
  
IF KEYWORD_SET(duration) THEN nrepeat = ROUND( (duration-integration_time) / (treset + CEIL(nz*integration_time) + 1 + tlatency) ) + 1 $
    ELSE IF ~KEYWORD_SET(nrepeat) THEN nrepeat = 1L
;; Compute the actual duration:  
duration = (nrepeat-1) * (treset + CEIL(nz * integration_time) + 1. + tlatency)  + integration_time

RETURN
END

 