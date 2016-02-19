FUNCTION PHASEIFY,t,ttransit,porbit

phase0 = (t-ttransit)/porbit mod 1
ineg = WHERE(phase0 GT 0.5,nneg)
IF nneg NE 0 THEN phase0[ineg] -= 1

RETURN,phase0
END