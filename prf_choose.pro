FUNCTION prf_choose,channel,prf,xloc,yloc,delta_x_pix_prf,delta_y_pix_prf,x_prf,y_prf,WARM=warm
;; Choose best PRF image for the location (XLOC,YLOC) to be realized.    
  sz = SIZE(prf)
  nx_prf = sz[1]
  ny_prf = sz[2]
  nprfs = sz[0] EQ 3 ? sz[3] : 1
  IF NPRFS GT 1 THEN BEGIN
    prf_rowcol = [25,77,129,181,233] -1
    dum = MIN(ABS(xloc-prf_rowcol),i0)
    IF i0 EQ 4 THEN i0=3
    i1 = i0+1
    dum = MIN(ABS(yloc-prf_rowcol),j0)
    IF j0 EQ 4 THEN j0=3
    j1 = j0+1


;;; (PRF_XCEN,PRF_YCEN): Locations, in PRF image coordinates, whose realizations give center-of-light centroids
;;; at the center of an array pixel (pixel phase zero).  All nonzero pixel phases should be computed relative 
;;; to these positions.   These values are tabulated with respect to the 5x5 array locations on the IRAC full array.
    CASE channel OF
      1: BEGIN
        ;CH1
        IF ~KEYWORD_SET(WARM) THEN BEGIN
          prf_xcen = TRANSPOSE([[64.9798,64.8700,64.8432,64.8670,64.9666],$
            [64.6898,64.6947,64.7154,64.6763,64.6637],$
            [64.5203,64.5446,64.5599,64.5223,64.4781],$
            [64.3401,64.3116,64.2844,64.2929,64.3014],$
            [64.1385,64.1287,64.0990,64.1145,64.1005]$
            ])
          prf_ycen = TRANSPOSE([[64.2161,63.9259,63.6962,63.4129,63.0540],$
            [64.1054,63.9165,63.6966,63.4341,63.1706],$
            [64.0916,63.9583,63.6826,63.3780,63.1894],$
            [64.0839,63.9573,63.6750,63.3620,63.1809],$
            [64.0730,63.8948,63.6701,63.4098,63.1576]$
            ])
        ENDIF ELSE BEGIN
          prf_xcen = TRANSPOSE([$
            [64.7235,64.6079,64.5781,64.6009,64.7039],$
            [64.4161,64.4163,64.4333,64.3972,64.3893],$
            [64.2410,64.2604,64.2733,64.2394,64.2027],$
            [64.0635,64.0314,64.0036,64.0145,64.0299],$
            [63.8640,63.8507,63.8203,63.8399,63.8323]$
            ])
          prf_ycen = TRANSPOSE([$
            [64.0981,63.7953,63.5653,63.3129,62.9836],$
            [63.9867,63.7808,63.5590,63.3251,63.0917],$
            [63.9709,63.8204,63.5436,63.2731,63.1066],$
            [63.9620,63.8183,63.5369,63.2602,63.1002],$
            [63.9523,63.7584,63.5369,63.3067,63.0815]$
            ])
        ENDELSE
        ;     prf_xcen = TRANSPOSE([ [64.9772,64.8530,64.8238,64.8502,64.9615],[64.6561,64.6596,64.6813,64.6414,64.6295],$
        ;                            [64.4804,64.5032,64.5190,64.4824,64.4404],[64.3060,64.2781,64.2536,64.2623,64.2713],$
        ;                            [64.1216,64.1126,64.0868,64.1009,64.0886] ])
        ;     prf_ycen = TRANSPOSE([ [64.2023,63.9157,63.6696,63.3958,63.0523],[64.0970,63.9045,63.6680,63.4136,63.1648],$
        ;                            [64.0841,63.9514,63.6535,63.3601,63.1826],[64.0773,63.9507,63.6465,63.3461,63.1749],$
        ;                            [64.0674,63.8812,63.6430,63.3926,63.1533] ])
      END
      2: BEGIN
        ;CH2
        IF ~KEYWORD_SET(WARM) THEN BEGIN
          prf_xcen = [$
            [62.9595,62.9152,62.9374,62.9344,62.9596],$
            [62.6425,62.6768,62.7190,62.7021,62.6724],$
            [62.3634,62.3639,62.3649,62.3780,62.3959],$
            [62.0761,62.0778,62.0509,62.0887,62.1287],$
            [61.7133,61.8193,61.8392,61.8467,61.7987]$
            ]
          prf_ycen = [$
            [64.2414,63.8913,63.5857,63.2681,62.9358],$
            [64.1966,63.9319,63.6027,63.2388,62.9653],$
            [64.2022,63.9536,63.6191,63.2285,62.9687],$
            [64.2120,63.9272,63.6176,63.2722,62.9873],$
            [64.2945,63.9198,63.6126,63.3002,62.9422]$
            ]

        ENDIF ELSE BEGIN
          prf_xcen = [$
            [62.0991,62.0551,62.0821,62.0779,62.1042],$
            [61.7912,61.8255,61.8702,61.8551,61.8257],$
            [61.5106,61.5125,61.5120,61.5280,61.5458],$
            [61.2156,61.2186,61.1860,61.2299,61.2658],$
            [60.8522,60.9547,60.9764,60.9794,60.9284]$
            ]
          prf_ycen = [$
            [63.4911,63.1392,62.8459,62.5336,62.1962],$
            [63.4509,63.1840,62.8660,62.5045,62.2269],$
            [63.4623,63.2100,62.8833,62.4958,62.2318],$
            [63.4769,63.1842,62.8857,62.5462,62.2510],$
            [63.5649,63.1802,62.8800,62.5720,62.2056]$
            ]

        ENDELSE

        ;           prf_xcen = TRANSPOSE([ [62.9576,62.9114,63.9345,62.9312,62.9576],[62.6333,63.6674,62.7097,62.6926,62.6625],$
        ;                                [62.3564,62.3565,62.3573,62.3701,62.3878],[62.0746,62.0760,62.0496,62.0865,62.1259],$
        ;                                [61.7113,61.8171,61.8369,61.8444,61.7963] ])
        ;           prf_ycen = TRANSPOSE([ [64.2350,63.8873,63.5784,63.2649,62.9350],[64.1912,63.9294,63.5956,63.2360,62.9651],$
        ;                                  [64.1964,63.9520,63.6119,63.2258,62.9685],[64.2055,63.9242,63.6099,63.2687,62.9873],$
        ;                                  [64.2869,63.9169,63.6053,63.2966,62.9416] ])
      END
      3: BEGIN
        prf_xcen = [$
          [65.1483,65.0515,65.0197,65.0236,65.1150],$
          [64.8031,64.8130,64.8183,64.7755,64.7539],$
          [64.5687,64.5798,64.5786,64.5464,64.5156],$
          [64.3247,64.2785,64.2548,64.2696,64.2906],$
          [64.0767,64.0638,64.0331,64.0517,64.0482]$
          ]
        prf_ycen = [$
          [64.5029,64.1677,63.8933,63.6223,63.2713],$
          [64.4090,64.1694,63.8933,63.6296,63.3753],$
          [64.4080,64.2150,63.8884,63.5839,63.3871],$
          [64.3965,64.1991,63.8876,63.5892,63.3866],$
          [64.4006,64.1484,63.8861,63.6267,63.3659]$
          ]

      END
      4: BEGIN
       prf_xcen = [$
          [62.7287,62.6868,62.7036,62.7134,62.7599],$
          [62.3607,62.4021,62.4496,62.4446,62.4344],$
          [62.0562,62.0743,62.0954,62.1153,62.1423],$
          [61.7563,61.7740,61.7662,61.8081,61.8572],$
          [61.3581,61.4854,61.5322,61.5469,61.5053]$
          ]
        prf_ycen = [$
          [64.5459,64.1918,63.8597,63.5326,63.1803],$
          [64.4997,64.2060,63.8749,63.5135,63.2224],$
          [64.5003,64.2274,63.8868,63.5067,63.2256],$
          [64.5088,64.2023,63.8855,63.5416,63.2351],$
          [64.5794,64.1945,63.8832,63.5658,63.1932]$
          ]

      END
    ENDCASE
    ;; Create the interpolation weights
    a = (xloc-prf_rowcol[i0])/(prf_rowcol[i1]-prf_rowcol[i0])
    b = (yloc-prf_rowcol[j0])/(prf_rowcol[j1]-prf_rowcol[j0])
    c00 = 1 + a*(b-1) -b
    c01 = b*(1-a)
    c10 = a*(1-b)
    c11 = a*b
    ;; Interpolate the center locations, and the PRF
    xcen = prf_xcen[i0,j0]*c00 + prf_xcen[i0,j1]*c01 + prf_xcen[i1,j0]*c10 + prf_xcen[i1,j1]*c11
    ycen = prf_ycen[i0,j0]*c00 + prf_ycen[i0,j1]*c01 + prf_ycen[i1,j0]*c10 + prf_ycen[i1,j1]*c11
    prf1 = TRANSPOSE(REFORM(prf,nx_prf,ny_prf,5,5),[0,1,3,2])
    prf_use = REFORM(prf1[*,*,i0,j0]*c00 + prf1[*,*,i0,j1]*c01 + prf1[*,*,i1,j0]*c10 + prf1[*,*,i1,j1]*c11)
    x_prf = ( (dindgen(nx_prf) - xcen)/delta_x_pix_prf ) # (fltarr(ny_prf) + 1.0)
    y_prf =  (fltarr(nx_prf) + 1.0) # ( (dindgen(ny_prf) - ycen)/delta_y_pix_prf )
  ENDIF ELSE BEGIN
    prf_use = prf
  ENDELSE

RETURN,prf_use
END