FUNCTION time_sclk,t1,t2,t3,hh,mm,ss,JULIAN_TO_SCLK=julian_to_sclk,MJD_TO_SCLK=mjd_to_sclk,DAY_TIME_TO_SCLK=day_time_to_sclk,$
                                     SCLK_TO_JULIAN=sclk_to_julian,SCLK_TO_MJD=sclk_to_mjd,SCLK_TO_DAY_TIME=sclk_to_day_time,$
                                     MJD_TO_DAY_TIME=mjd_to_day_time,DAY_TIME_TO_MJD=day_time_to_mjd
;;
;; Convert between various time systems and Spitzer Spacecraft Clock (SCLK).  Inputs can be a scalar or vector of values.  Output will
;; have the same number of values.
;
;  Due to the variations between the actual spacecraft clock and UTC, and other issues due to spacecraft velocity and different gravitation
;  etc, these times might be as much as a few minutes off.  This should be fine for modeling. 
;;
;; Time systems to convert are given by the keyword:
;     /JULIAN_TO_SCLK : t1 is a julian date.  Return the sclk time.  
;     /MJD_TO_SCLK: t1 is a modified julian date.  Return the sclk time.  
;     /DAY_TIME_TO_SCLK: t1,t2,t3 is the month (number), day, year and hh,mm,ss, is the time (hours:minutes:seconds), in UTC.
;                        Return the sclk time. [DEFAULT]
;     /SCLK_TO_JULIAN: t1 is a sclk value.  Return the Julian date.
;     /SCLK_TO_MJD: t1 is a sclk.  Return the modified julian date.
;     /SCLK_TO_DAY_TIME: t1 is a sclk. Return the UTC day and time as an array containing [month (number), day, year, hour, minutes, seconds]
;     
;     Non-sclk related (because I need them too):
;     /DAY_TIME_TO_MJD: t1,t2,t3 is the month (number), day, year and hh,mm,ss, is the time (hours:minutes:seconds), in UTC.
;                        Return the modified julian date.
;     /MJD_TO_DAY_TIME: t1 is a modified Julian date.  Return the UTC day and time as an array containing [month (number), day, year, hour, minutes, seconds]
;     
;Julian Day at SCLK=0
julday_sclk0 = JULDAY(1,1,1980,0,0,0)
;Julian Day at MJD=0
julday_mjd0 = 2400000.5D0
 
CASE 1 of 
   KEYWORD_SET(JULIAN_TO_SCLK): toutput = (t1-julday_sclk0) * 86400.d0
   KEYWORD_SET(MJD_TO_SCLK): toutput = (t1+julday_mjd0-julday_sclk0) * 86400.d0
   KEYWORD_SET(SCLK_TO_JULIAN): toutput = (t1 / 86400.d0) + julday_sclk0
   KEYWORD_SET(SCLK_TO_MJD): toutput = (t1 / 86400.d0) + julday_sclk0 - julday_mjd0
   KEYWORD_SET(SCLK_TO_DAY_TIME): BEGIN
      julianday = (t1 / 86400.d0) + julday_sclk0
      CALDAT,julianday,mon,day,yr,hr,minits,sec
      toutput = [[mon],[day],[yr],[hr],[minits],[sec]]
   END
   KEYWORD_SET(DAY_TIME_TO_MJD): BEGIN
      julianday = JULDAY(t1,t2,t3,hh,mm,ss)
      toutput = (julianday-julday_mjd0) 
   END
   KEYWORD_SET(MJD_TO_DAY_TIME): BEGIN
      julianday = t1 + julday_mjd0
      CALDAT,julianday,mon,day,yr,hr,minits,sec
      toutput = [[mon],[day],[yr],[hr],[minits],[sec]]
   END
   ELSE: BEGIN
   ;; Date/time to sclk
      julianday = JULDAY(t1,t2,t3,hh,mm,ss)
      toutput = (julianday-julday_sclk0) * 86400.d0
   END
ENDCASE

RETURN,toutput
END

;2003-11-27T01:53:21.820 UT -> 754365238.202 sclk -> 52970.0787248 MJD