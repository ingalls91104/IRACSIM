FUNCTION PHASEIFY,t,ttransit,porbit
;; Convert times T in an orbit to fraction of orbital period PORBIT,
;; ranging from -0.5 to 0.5, where 0 occurs at TTRANSIT
phase0 = (t-ttransit)/porbit mod 1
ineg = WHERE(phase0 GT 0.5,nneg)
IF nneg NE 0 THEN phase0[ineg] -= 1

RETURN,phase0
END