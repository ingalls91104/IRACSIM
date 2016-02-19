FUNCTION get_exoplanet_start,p_orbit,mjd_transit,START_HR_BEFORE_TRANSIT=start_hr_before_transit,START_PHASE=start_phase,PRE_AOR=pre_aor

;; Set /PRE_AOR if the inputs are for the start of the actual observation, but we need to find the start of a 30-minute pre-AOR before the "real" start.
;; 
now = systime(/JULIAN) - 2400000.0   ;;; Current MJD
norbits = (now-mjd_transit)/p_orbit  ;; decimal number of orbits since MJD_TRANSIT 
frac_since_last = norbits MOD 1 ;; Fraction of an orbit since the last transit
frac_till_next = 1-frac_since_last ;; Fraction of an orbit till the next transit
hr_till = P_ORBIT * frac_till_next * 24. ;; Number of hours until the next transit
IF N_ELEMENTS(mjd_transit) EQ 0 THEN mjd_start = now  $;;; Default to current time if we don't have transit info
   ELSE BEGIN
      CASE 1 OF 
         (N_ELEMENTS(START_HR_BEFORE_TRANSIT) NE 0): BEGIN  
            mjd_start = now + (hr_till  - start_hr_before_transit)/24d0  ;; start observations at the next transit minus START_HR_BEFORE_TRANSIT
         END
         (N_ELEMENTS(START_PHASE) NE 0): BEGIN
          ;;; NOTE:  the input START_PHASE can have an absolute value greater than 0.5.  In that case, make the observations start that many 
          ;;;        orbits before or after transit.
            start_phase_hr = start_phase * P_ORBIT*24 ;; Number of hours before (negative phase) or after (positive phase) transit corresponding to the input phase
            mjd_start = now + (hr_till + start_phase_hr)/24d0 ;; start observations at the next transit plus START_PHASE_HR
         END
         ELSE: BEGIN
            mjd_start = mjd_transit - p_orbit/2.   ;;; Start observations at phase of -0.5
         END
      ENDCASE
ENDELSE

IF KEYWORD_SET(PRE_AOR) THEN mjd_start -= 30./(60D0*24)  ;; Subtract 30 minutes from the time implied by the other inputs.

RETURN,mjd_start
END
