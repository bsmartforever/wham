;$Id: meanabsdev.pro,v 1.3 1998/03/02 18:02:38 slett Exp $
;
; Copyright (c) 1997-1998, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.
;+
; NAME:
;       MeanAbsDev
;
; PURPOSE:
;       MeanAbsDev computes the mean absolute deviation (average
;       deviation) of an N-element vector.
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = MeanAbsDev(X)
;
; INPUTS:
;       X:      An N-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
;
;       DOUBLE: If set to a non-zero value, MEANABSDEV performs its
;               computations in double precision arithmetic and returns
;               a double precision result. If not set to a non-zero value,
;               the computations and result depend upon the type of the
;               input data (integer and float data return float results,
;               while double data returns double results). This has no
;               effect if the Median keyword is set.
;
;       MEDIAN: If set to a non-zero value, meanabsdev will return
;               the average deviation from the median, rather than
;               the mean.  If Median is not set, meanabsdev will return
;               the average deviation from the mean.
;
;       NAN:    If set, treat NaN data as missing.
;
; EXAMPLES:
;       Define the N-element vector of sample data.
;         x = [1, 1, 1, 2, 5]
;       Compute the average deviation from the mean.
;         result = MeanAbsDev( x )
;       The result should be:
;       1.20000
;
;       Compute the average deviation from the median.
;         result = MeanAbsDev( x, /median )
;       The result should be:
;       1.00000
;
; PROCEDURE:
;       MeanAbsDev calls the IDL function MEAN.
;      
;       MeanAbsDev calls the IDL function MEDIAN if the Median
;       keyword is set to a nonzero value.
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Written by:  GSL, RSI, August 1997
;-
FUNCTION MeanAbsDev, X, Double = Double, Median = Median, NaN = NaN

  ON_ERROR, 2
  IF keyword_set( Median ) THEN $
      middle = median( X, /even) $
  ELSE $
      middle = mean( X, Double=Double, NaN = NaN )
 
  RETURN, mean( abs( X - middle ), NaN = NaN) 
END
