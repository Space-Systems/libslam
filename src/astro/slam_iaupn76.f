      module slam_iaupn76
      
      contains

      SUBROUTINE iau_PREC76 ( DATE01, DATE02, DATE11, DATE12,
     :                        ZETA, Z, THETA )
*+
*  - - - - - - - - - - -
*   i a u _ P R E C 7 6
*  - - - - - - - - - - -
*
*  IAU 1976 precession model.
*
*  This routine forms the three Euler angles which implement general
*  precession between two dates, using the IAU 1976 model (as for
*  the FK5 catalog).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE01,DATE02  d   TDB starting date (Note 1)
*     DATE11,DATE12  d   TDB ending date (Note 1)
*
*  Returned:
*     ZETA           d   1st rotation: radians clockwise around z
*     Z              d   3rd rotation: radians clockwise around z
*     THETA          d   2nd rotation: radians counterclockwise around y
*
*  Notes:
*
*  1) The dates DATE01+DATE02 and DATE11+DATE12 are Julian Dates,
*     apportioned in any convenient way between the arguments DATEn1 and
*     DATEn2.  For example, JD(TDB)=2450123.7 could be expressed in any
*     of these ways, among others:
*
*           DATEn1         DATEn2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in cases
*     where the loss of several decimal digits of resolution is
*     acceptable.  The J2000 method is best matched to the way the
*     argument is handled internally and will deliver the optimum
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*     The two dates may be expressed using different methods, but at
*     the risk of losing some resolution.
*
*  2) The accumulated precession angles zeta, z, theta are expressed
*     through canonical polynomials which are valid only for a limited
*     time span.  In addition, the IAU 1976 precession rate is known to
*     be imperfect.  The absolute accuracy of the present formulation is
*     better than 0.1 arcsec from 1960AD to 2040AD, better than 1 arcsec
*     from 1640AD to 2360AD, and remains below 3 arcsec for the whole of
*     the period 500BC to 3000AD.  The errors exceed 10 arcsec outside
*     the range 1200BC to 3900AD, exceed 100 arcsec outside 4200BC to
*     5600AD and exceed 1000 arcsec outside 6800BC to 8200AD.
*
*  3) The three angles are returned in the conventional order, which
*     is not the same as the order of the corresponding Euler rotations.
*     The precession matrix is R_3(-z) x R_2(+theta) x R_3(-zeta).
*
*  Reference:
*
*     Lieske, J.H., 1979, Astron.Astrophys. 73, 282.
*      equations (6) & (7), p283.
*
*  This revision:  2013 November 19
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE01, DATE02, DATE11, DATE12, ZETA, Z, THETA

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T0, T, TAS2R, W

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and start date (JC).
      T0 = ( ( DATE01-DJ00 ) + DATE02 ) / DJC

*  Interval over which precession required (JC).
      T = ( ( DATE11-DATE01 ) + ( DATE12-DATE02 ) ) / DJC

*  Euler angles.
      TAS2R = T * DAS2R
      W = 2306.2181D0 + (
     :       1.39656D0
     :     - 0.000139D0 * T0 ) * T0

      ZETA = ( W + ( ( 0.30188D0
     :               - 0.000344D0 * T0 )
     :               + 0.017998D0 * T ) * T ) * TAS2R

      Z = ( W + ( ( 1.09468D0
     :            + 0.000066D0 * T0 )
     :            + 0.018203D0 * T ) * T ) * TAS2R

      THETA = ( ( 2004.3109D0 + (
     :             - 0.85330D0
     :             - 0.000217D0 * T0 ) * T0 ) + ( (
     :             - 0.42665D0
     :             - 0.000217D0 * T0 )
     :             - 0.041833D0 * T ) * T ) * TAS2R

*  Finished.
      END SUBROUTINE

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      SUBROUTINE iau_NUT80 ( DATE1, DATE2, DPSI, DEPS )

*+
*  - - - - - - - - - -
*   i a u _ N U T 8 0
*  - - - - - - - - - -
*
*  Nutation, IAU 1980 model.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2     d      TT as a 2-part Julian Date (Note 1)
*
*  Returned:
*     DPSI            d      nutation in longitude (radians)
*     DEPS            d      nutation in obliquity (radians)
*
*  Notes:
*
*  1) The DATE DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TDB)=2450123.7 could be expressed in any of these ways,
*     among others:
*
*            DATE1          DATE2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 method is best matched to the way
*     the argument is handled internally and will deliver the
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*
*  2) The nutation components are with respect to the ecliptic of
*     date.
*
*  Called:
*     iau_ANPM     normalize angle into range +/- pi
*
*  Reference:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P. Kenneth Seidelmann (ed), University Science Books (1992),
*     Section 3.222 (p111).
*
*  This revision:  2009 December 15
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, DPSI, DEPS

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

*  Units of 0.1 milliarcsecond to radians
      DOUBLE PRECISION U2R
      PARAMETER ( U2R = DAS2R/1D4 )

*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T, EL, ELP, F, D, OM, DP, DE, ARG, S, C
      INTEGER I, J

      !DOUBLE PRECISION iau_ANPM

*  ------------------------------------------------
*  Table of multiples of arguments and coefficients
*  ------------------------------------------------
*
*  The coefficient values are in 0.1 mas units and the rates of change
*  are in mas per Julian millennium.

      REAL X(9,106)

*                Multiple of            Longitude        Obliquity
*           L    L'   F    D  Omega   coeff. of sin    coeff. of cos
*                                         1       t        1     t

      DATA ((X(I,J),I=1,9),J=1,10) /
     :      0.,  0.,  0.,  0.,  1., -171996., -1742.,  92025.,  89.,
     :      0.,  0.,  0.,  0.,  2.,    2062.,     2.,   -895.,   5.,
     :     -2.,  0.,  2.,  0.,  1.,      46.,     0.,    -24.,   0.,
     :      2.,  0., -2.,  0.,  0.,      11.,     0.,      0.,   0.,
     :     -2.,  0.,  2.,  0.,  2.,      -3.,     0.,      1.,   0.,
     :      1., -1.,  0., -1.,  0.,      -3.,     0.,      0.,   0.,
     :      0., -2.,  2., -2.,  1.,      -2.,     0.,      1.,   0.,
     :      2.,  0., -2.,  0.,  1.,       1.,     0.,      0.,   0.,
     :      0.,  0.,  2., -2.,  2.,  -13187.,   -16.,   5736., -31.,
     :      0.,  1.,  0.,  0.,  0.,    1426.,   -34.,     54.,  -1. /
      DATA ((X(I,J),I=1,9),J=11,20) /
     :      0.,  1.,  2., -2.,  2.,    -517.,    12.,    224.,  -6.,
     :      0., -1.,  2., -2.,  2.,     217.,    -5.,    -95.,   3.,
     :      0.,  0.,  2., -2.,  1.,     129.,     1.,    -70.,   0.,
     :      2.,  0.,  0., -2.,  0.,      48.,     0.,      1.,   0.,
     :      0.,  0.,  2., -2.,  0.,     -22.,     0.,      0.,   0.,
     :      0.,  2.,  0.,  0.,  0.,      17.,    -1.,      0.,   0.,
     :      0.,  1.,  0.,  0.,  1.,     -15.,     0.,      9.,   0.,
     :      0.,  2.,  2., -2.,  2.,     -16.,     1.,      7.,   0.,
     :      0., -1.,  0.,  0.,  1.,     -12.,     0.,      6.,   0.,
     :     -2.,  0.,  0.,  2.,  1.,      -6.,     0.,      3.,   0. /
      DATA ((X(I,J),I=1,9),J=21,30) /
     :      0., -1.,  2., -2.,  1.,      -5.,     0.,      3.,   0.,
     :      2.,  0.,  0., -2.,  1.,       4.,     0.,     -2.,   0.,
     :      0.,  1.,  2., -2.,  1.,       4.,     0.,     -2.,   0.,
     :      1.,  0.,  0., -1.,  0.,      -4.,     0.,      0.,   0.,
     :      2.,  1.,  0., -2.,  0.,       1.,     0.,      0.,   0.,
     :      0.,  0., -2.,  2.,  1.,       1.,     0.,      0.,   0.,
     :      0.,  1., -2.,  2.,  0.,      -1.,     0.,      0.,   0.,
     :      0.,  1.,  0.,  0.,  2.,       1.,     0.,      0.,   0.,
     :     -1.,  0.,  0.,  1.,  1.,       1.,     0.,      0.,   0.,
     :      0.,  1.,  2., -2.,  0.,      -1.,     0.,      0.,   0. /
      DATA ((X(I,J),I=1,9),J=31,40) /
     :      0.,  0.,  2.,  0.,  2.,   -2274.,    -2.,    977.,  -5.,
     :      1.,  0.,  0.,  0.,  0.,     712.,     1.,     -7.,   0.,
     :      0.,  0.,  2.,  0.,  1.,    -386.,    -4.,    200.,   0.,
     :      1.,  0.,  2.,  0.,  2.,    -301.,     0.,    129.,  -1.,
     :      1.,  0.,  0., -2.,  0.,    -158.,     0.,     -1.,   0.,
     :     -1.,  0.,  2.,  0.,  2.,     123.,     0.,    -53.,   0.,
     :      0.,  0.,  0.,  2.,  0.,      63.,     0.,     -2.,   0.,
     :      1.,  0.,  0.,  0.,  1.,      63.,     1.,    -33.,   0.,
     :     -1.,  0.,  0.,  0.,  1.,     -58.,    -1.,     32.,   0.,
     :     -1.,  0.,  2.,  2.,  2.,     -59.,     0.,     26.,   0./
      DATA ((X(I,J),I=1,9),J=41,50) /
     :      1.,  0.,  2.,  0.,  1.,     -51.,     0.,     27.,   0.,
     :      0.,  0.,  2.,  2.,  2.,     -38.,     0.,     16.,   0.,
     :      2.,  0.,  0.,  0.,  0.,      29.,     0.,     -1.,   0.,
     :      1.,  0.,  2., -2.,  2.,      29.,     0.,    -12.,   0.,
     :      2.,  0.,  2.,  0.,  2.,     -31.,     0.,     13.,   0.,
     :      0.,  0.,  2.,  0.,  0.,      26.,     0.,     -1.,   0.,
     :     -1.,  0.,  2.,  0.,  1.,      21.,     0.,    -10.,   0.,
     :     -1.,  0.,  0.,  2.,  1.,      16.,     0.,     -8.,   0.,
     :      1.,  0.,  0., -2.,  1.,     -13.,     0.,      7.,   0.,
     :     -1.,  0.,  2.,  2.,  1.,     -10.,     0.,      5.,   0. /
      DATA ((X(I,J),I=1,9),J=51,60) /
     :      1.,  1.,  0., -2.,  0.,      -7.,     0.,      0.,   0.,
     :      0.,  1.,  2.,  0.,  2.,       7.,     0.,     -3.,   0.,
     :      0., -1.,  2.,  0.,  2.,      -7.,     0.,      3.,   0.,
     :      1.,  0.,  2.,  2.,  2.,      -8.,     0.,      3.,   0.,
     :      1.,  0.,  0.,  2.,  0.,       6.,     0.,      0.,   0.,
     :      2.,  0.,  2., -2.,  2.,       6.,     0.,     -3.,   0.,
     :      0.,  0.,  0.,  2.,  1.,      -6.,     0.,      3.,   0.,
     :      0.,  0.,  2.,  2.,  1.,      -7.,     0.,      3.,   0.,
     :      1.,  0.,  2., -2.,  1.,       6.,     0.,     -3.,   0.,
     :      0.,  0.,  0., -2.,  1.,      -5.,     0.,      3.,   0. /
      DATA ((X(I,J),I=1,9),J=61,70) /
     :      1., -1.,  0.,  0.,  0.,       5.,     0.,      0.,   0.,
     :      2.,  0.,  2.,  0.,  1.,      -5.,     0.,      3.,   0.,
     :      0.,  1.,  0., -2.,  0.,      -4.,     0.,      0.,   0.,
     :      1.,  0., -2.,  0.,  0.,       4.,     0.,      0.,   0.,
     :      0.,  0.,  0.,  1.,  0.,      -4.,     0.,      0.,   0.,
     :      1.,  1.,  0.,  0.,  0.,      -3.,     0.,      0.,   0.,
     :      1.,  0.,  2.,  0.,  0.,       3.,     0.,      0.,   0.,
     :      1., -1.,  2.,  0.,  2.,      -3.,     0.,      1.,   0.,
     :     -1., -1.,  2.,  2.,  2.,      -3.,     0.,      1.,   0.,
     :     -2.,  0.,  0.,  0.,  1.,      -2.,     0.,      1.,   0. /
      DATA ((X(I,J),I=1,9),J=71,80) /
     :      3.,  0.,  2.,  0.,  2.,      -3.,     0.,      1.,   0.,
     :      0., -1.,  2.,  2.,  2.,      -3.,     0.,      1.,   0.,
     :      1.,  1.,  2.,  0.,  2.,       2.,     0.,     -1.,   0.,
     :     -1.,  0.,  2., -2.,  1.,      -2.,     0.,      1.,   0.,
     :      2.,  0.,  0.,  0.,  1.,       2.,     0.,     -1.,   0.,
     :      1.,  0.,  0.,  0.,  2.,      -2.,     0.,      1.,   0.,
     :      3.,  0.,  0.,  0.,  0.,       2.,     0.,      0.,   0.,
     :      0.,  0.,  2.,  1.,  2.,       2.,     0.,     -1.,   0.,
     :     -1.,  0.,  0.,  0.,  2.,       1.,     0.,     -1.,   0.,
     :      1.,  0.,  0., -4.,  0.,      -1.,     0.,      0.,   0. /
      DATA ((X(I,J),I=1,9),J=81,90) /
     :     -2.,  0.,  2.,  2.,  2.,       1.,     0.,     -1.,   0.,
     :     -1.,  0.,  2.,  4.,  2.,      -2.,     0.,      1.,   0.,
     :      2.,  0.,  0., -4.,  0.,      -1.,     0.,      0.,   0.,
     :      1.,  1.,  2., -2.,  2.,       1.,     0.,     -1.,   0.,
     :      1.,  0.,  2.,  2.,  1.,      -1.,     0.,      1.,   0.,
     :     -2.,  0.,  2.,  4.,  2.,      -1.,     0.,      1.,   0.,
     :     -1.,  0.,  4.,  0.,  2.,       1.,     0.,      0.,   0.,
     :      1., -1.,  0., -2.,  0.,       1.,     0.,      0.,   0.,
     :      2.,  0.,  2., -2.,  1.,       1.,     0.,     -1.,   0.,
     :      2.,  0.,  2.,  2.,  2.,      -1.,     0.,      0.,   0. /
      DATA ((X(I,J),I=1,9),J=91,100) /
     :      1.,  0.,  0.,  2.,  1.,      -1.,     0.,      0.,   0.,
     :      0.,  0.,  4., -2.,  2.,       1.,     0.,      0.,   0.,
     :      3.,  0.,  2., -2.,  2.,       1.,     0.,      0.,   0.,
     :      1.,  0.,  2., -2.,  0.,      -1.,     0.,      0.,   0.,
     :      0.,  1.,  2.,  0.,  1.,       1.,     0.,      0.,   0.,
     :     -1., -1.,  0.,  2.,  1.,       1.,     0.,      0.,   0.,
     :      0.,  0., -2.,  0.,  1.,      -1.,     0.,      0.,   0.,
     :      0.,  0.,  2., -1.,  2.,      -1.,     0.,      0.,   0.,
     :      0.,  1.,  0.,  2.,  0.,      -1.,     0.,      0.,   0.,
     :      1.,  0., -2., -2.,  0.,      -1.,     0.,      0.,   0. /
      DATA ((X(I,J),I=1,9),J=101,106) /
     :      0., -1.,  2.,  0.,  1.,      -1.,     0.,      0.,   0.,
     :      1.,  1.,  0., -2.,  1.,      -1.,     0.,      0.,   0.,
     :      1.,  0., -2.,  2.,  0.,      -1.,     0.,      0.,   0.,
     :      2.,  0.,  0.,  2.,  0.,       1.,     0.,      0.,   0.,
     :      0.,  0.,  2.,  4.,  2.,      -1.,     0.,      0.,   0.,
     :      0.,  1.,  0.,  1.,  0.,       1.,     0.,      0.,   0. /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and given date (JC).
      T = ( ( DATE1-DJ00 ) + DATE2 ) / DJC

*
*  FUNDAMENTAL ARGUMENTS in the FK5 reference system
*

*  Mean longitude of the Moon minus mean longitude of the Moon's
*  perigee.
      EL = iau_ANPM ( ( 485866.733D0 + ( 715922.633D0 +
     :                  ( 31.310D0 + 0.064D0 * T ) * T ) * T ) * DAS2R
     :                + MOD(1325D0*T, 1D0) * D2PI )

*  Mean longitude of the Sun minus mean longitude of the Sun's perigee.
      ELP = iau_ANPM ( ( 1287099.804D0 + ( 1292581.224D0 +
     :                   ( -0.577D0 -0.012D0 * T ) * T ) * T ) * DAS2R
     :                 + MOD(99D0*T, 1D0) * D2PI )

*  Mean longitude of the Moon minus mean longitude of the Moon's node.
      F = iau_ANPM ( ( 335778.877D0 + ( 295263.137D0 +
     :                 ( -13.257D0 + 0.011D0 * T ) * T ) * T ) * DAS2R
     :               + MOD(1342D0*T, 1D0) * D2PI )

*  Mean elongation of the Moon from the Sun.
      D = iau_ANPM ( ( 1072261.307D0 + ( 1105601.328D0 +
     :                 ( -6.891D0 + 0.019D0 * T ) * T ) * T ) * DAS2R
     :               + MOD(1236D0*T, 1D0) * D2PI )

*  Longitude of the mean ascending node of the lunar orbit on the
*  ecliptic, measured from the mean equinox of date.
      OM = iau_ANPM( ( 450160.280D0 + ( -482890.539D0 +
     :                 ( 7.455D0 + 0.008D0 * T ) * T ) * T ) * DAS2R
     :               + MOD( -5D0*T, 1D0) * D2PI )

*  ---------------
*  Nutation series
*  ---------------

*  Change time argument from centuries to millennia.
      T = T / 10D0

*  Initialize nutation components.
      DP = 0D0
      DE = 0D0

*  Sum the nutation terms, ending with the biggest.
      DO 1 J=106,1,-1

*     Form argument for current term.
         ARG = DBLE(X(1,J)) * EL
     :       + DBLE(X(2,J)) * ELP
     :       + DBLE(X(3,J)) * F
     :       + DBLE(X(4,J)) * D
     :       + DBLE(X(5,J)) * OM

*     Accumulate current nutation term.
         S = DBLE(X(6,J)) + DBLE(X(7,J)) * T
         C = DBLE(X(8,J)) + DBLE(X(9,J)) * T
         IF ( S .NE. 0D0 ) DP = DP + S * SIN(ARG)
         IF ( C .NE. 0D0 ) DE = DE + C * COS(ARG)

*     Next term.
 1    CONTINUE

*  Convert results from 0.1 mas units to radians.
      DPSI = DP * U2R
      DEPS = DE * U2R

*  Finished.
      END SUBROUTINE

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION iau_ANPM ( A )
*+
*  - - - - - - - - -
*   i a u _ A N P M
*  - - - - - - - - -
*
*  Normalize angle into the range -pi <= A < +pi.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     A          d       angle (radians)
*
*  Returned:
*     iau_ANPM   d       angle in range +/-pi
*
*  This revision:  2000 November 25
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION A

*  Pi
      DOUBLE PRECISION DPI
      PARAMETER ( DPI = 3.141592653589793238462643D0 )

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

      DOUBLE PRECISION W

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      W = MOD(A,D2PI)
      IF ( ABS(W) .GE. DPI ) W = W - SIGN(D2PI,A)
      iau_ANPM = W

*  Finished.
      END FUNCTION

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION iau_GMST82 ( DJ1, DJ2 )
*+
*  - - - - - - - - - - -
*   i a u _ G M S T 8 2
*  - - - - - - - - - - -
*
*  Universal Time to Greenwich Mean Sidereal Time (IAU 1982 model).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DJ1, DJ2     d      UT1 Julian Date (see note)
*
*  Returned:
*     iau_GMST82   d      Greenwich mean sidereal time (radians)
*
*  Notes:
*
*  1) The UT1 epoch DJ1+DJ2 is a Julian Date, apportioned in any
*     convenient way between the arguments DJ1 and DJ2.  For example,
*     JD(UT1)=2450123.7 could be expressed in any of these ways,
*     among others:
*
*             DJ1            DJ2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 and MJD methods are good compromises
*     between resolution and convenience.  The date & time method is
*     best matched to the algorithm used:  maximum accuracy (or, at
*     least, minimum noise) is delivered when the DJ1 argument is for
*     0hrs UT1 on the day in question and the DJ2 argument lies in the
*     range 0 to 1, or vice versa.
*
*  2) The algorithm is based on the IAU 1982 expression.  This is always
*     described as giving the GMST at 0 hours UT1.  In fact, it gives the
*     difference between the GMST and the UT, the steady 4-minutes-per-day
*     drawing-ahead of ST with respect to UT.  When whole days are ignored,
*     the expression happens to equal the GMST at 0 hours UT1 each day.
*
*  3) In this routine, the entire UT1 (the sum of the two arguments DJ1
*     and DJ2) is used directly as the argument for the standard formula,
*     the constant term of which is adjusted by 12 hours to take account
*     of the noon phasing of Julian Date.  The UT1 is then added, but
*     omitting whole days to conserve accuracy.
*
*  4) The result is returned in the range 0 to 2pi.
*
*  Called:
*     iau_ANP      normalize angle into range 0 to 2pi
*
*  References:
*
*     Transactions of the International Astronomical Union,
*     XVIII B, 67 (1983).
*
*     Aoki et al., Astron. Astrophys. 105, 359-361 (1982).
*
*  This revision:  2010 August 12
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DJ1, DJ2

*  Seconds of time to radians
      DOUBLE PRECISION DS2R
      PARAMETER ( DS2R = 7.272205216643039903848712D-5 )

*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

*  Seconds per day, days per Julian century
      DOUBLE PRECISION D2S, DJC
      PARAMETER ( D2S = 86400D0, DJC = 36525D0 )

*  Coefficients of IAU 1982 GMST-UT1 model
      DOUBLE PRECISION A, B, C, D
      PARAMETER ( A = 24110.54841D0 - D2S/2D0,
     :            B = 8640184.812866D0,
     :            C = 0.093104D0,
     :            D = -6.2D-6 )

*  Note: the first constant, A, has to be adjusted by 12 hours because
*  the UT1 is supplied as a Julian date, which begins at noon.

      DOUBLE PRECISION D1, D2, T, F

      !DOUBLE PRECISION iau_ANP

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Julian centuries since fundamental epoch.
      IF ( DJ1 .LT. DJ2 ) THEN
         D1 = DJ1
         D2 = DJ2
      ELSE
         D1 = DJ2
         D2 = DJ1
      END IF
      T = ( D1 + ( D2-DJ00 ) ) / DJC

*  Fractional part of JD(UT1), in seconds.
      F = D2S * ( MOD(D1,1D0) + MOD(D2,1D0) )

*  GMST at this UT1.
      iau_GMST82 = iau_ANP ( DS2R * ( (A+(B+(C+D*T)*T)*T) + F ) )

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END FUNCTION

      DOUBLE PRECISION FUNCTION iau_ANP ( A )
*+
*  - - - - - - - -
*   i a u _ A N P
*  - - - - - - - -
*
*  Normalize angle into the range 0 <= A < 2pi.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     A          d       angle (radians)
*
*  Returned:
*     iau_ANP    d       angle in range 0-2pi
*
*  This revision:  2000 December 15
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION A

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

      DOUBLE PRECISION W

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      W = MOD(A,D2PI)
      IF ( W .LT. 0D0 ) W = W + D2PI
      iau_ANP = W

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END FUNCTION

      DOUBLE PRECISION FUNCTION iau_EQEQ94 ( DATE1, DATE2 )
*+
*  - - - - - - - - - - -
*   i a u _ E Q E Q 9 4
*  - - - - - - - - - - -
*
*  Equation of the equinoxes, IAU 1994 model.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2     d         TDB date (Note 1)
*
*  Returned:
*     iau_EQEQ94      d         equation of the equinoxes (Note 2)
*
*  Notes:
*
*  1) The TDB date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TDB)=2450123.7 could be expressed in any of these ways, among
*     others:
*
*            DATE1          DATE2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 method is best matched to the way
*     the argument is handled internally and will deliver the
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*
*  2) The result, which is in radians, operates in the following sense:
*
*        Greenwich apparent ST = GMST + equation of the equinoxes
*
*  Called:
*     iau_ANPM     normalize angle into range +/- pi
*     iau_NUT80    nutation, IAU 1980
*     iau_OBL80    mean obliquity, IAU 1980
*
*  References:
*
*     IAU Resolution C7, Recommendation 3 (1994)
*
*     Capitaine, N. & Gontier, A.-M., Astron. Astrophys., 275,
*     645-650 (1993)
*
*  This revision:  2013 August 31
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER (D2PI = 6.283185307179586476925287D0)

*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T, OM, DPSI, DEPS, EPS0
      !DOUBLE PRECISION iau_ANPM, iau_OBL80

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and given date (JC).
      T = ( ( DATE1-DJ00 ) + DATE2 ) / DJC

*  Longitude of the mean ascending node of the lunar orbit on the
*  ecliptic, measured from the mean equinox of date.
      OM = iau_ANPM( ( 450160.280D0 + ( -482890.539D0 +
     :                 ( 7.455D0 + 0.008D0 * T ) * T ) * T ) * DAS2R
     :               + MOD(-5D0*T,1D0) * D2PI )

*  Nutation components and mean obliquity.
      CALL iau_NUT80 ( DATE1, DATE2, DPSI, DEPS )
      EPS0 = iau_OBL80 ( DATE1, DATE2 )

*  Equation of the equinoxes.
      iau_EQEQ94 = DPSI * COS(EPS0) + DAS2R * ( 0.00264D0 * SIN(OM) +
     :                                          0.000063D0 * SIN(OM+OM))

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END FUNCTION
     
      DOUBLE PRECISION FUNCTION iau_OBL80 ( DATE1, DATE2 )
*+
*  - - - - - - - - - -
*   i a u _ O B L 8 0
*  - - - - - - - - - -
*
*  Mean obliquity of the ecliptic, IAU 1980 model.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2     d      TT as a 2-part Julian Date (Note 1)
*
*  Returned:
*     iau_OBL80       d      obliquity of the ecliptic (radians, Note 2)
*
*  Notes:
*
*  1) The date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TDB)=2450123.7 could be expressed in any of these ways,
*     among others:
*
*             DATE1         DATE2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 method is best matched to the way
*     the argument is handled internally and will deliver the
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*
*  2) The result is the angle between the ecliptic and mean equator of
*     date DATE1+DATE2.
*
*  Reference:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P. Kenneth Seidelmann (ed), University Science Books (1992),
*     Expression 3.222-1 (p114).
*
*  This revision:  2009 December 15
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and given date (JC).
      T = ( ( DATE1-DJ00 ) + DATE2 ) / DJC

*  Mean obliquity of date.
      iau_OBL80 = DAS2R * ( 84381.448D0 +
     :                      ( -46.8150D0 +
     :                       ( -0.00059D0 +
     :                          0.001813D0 * T ) * T ) * T )

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END FUNCTION 
      
      SUBROUTINE iau_NUMAT ( EPSA, DPSI, DEPS, RMATN )
*+
*  - - - - - - - - - -
*   i a u _ N U M A T
*  - - - - - - - - - -
*
*  Form the matrix of nutation.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     EPSA          d       mean obliquity of date (Note 1)
*     DPSI,DEPS     d       nutation (Note 2)
*
*  Returned:
*     RMATN       d(3,3)    nutation matrix (Note 3)
*
*  Notes:
*
*  1) The supplied mean obliquity EPSA, must be consistent with the
*     precession-nutation models from which DPSI and DEPS were obtained.
*
*  2) The caller is responsible for providing the nutation components;
*     they are in longitude and obliquity, in radians and are with
*     respect to the equinox and ecliptic of date.
*
*  3) The matrix operates in the sense V(true) = RMATN * V(mean),
*     where the p-vector V(true) is with respect to the true
*     equatorial triad of date and the p-vector V(mean) is with
*     respect to the mean equatorial triad of date.
*
*  Called:
*     iau_IR       initialize r-matrix to identity
*     iau_RX       rotate around X-axis
*     iau_RZ       rotate around Z-axis
*
*  Reference:
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P. Kenneth Seidelmann (ed), University Science Books (1992),
*     Section 3.222-3 (p114).
*
*  This revision:  2006 November 13
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION EPSA, DPSI, DEPS, RMATN(3,3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Build the rotation matrix.
      CALL iau_IR ( RMATN )
      CALL iau_RX ( EPSA, RMATN )
      CALL iau_RZ ( -DPSI, RMATN )
      CALL iau_RX ( -(EPSA+DEPS), RMATN )

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END SUBROUTINE

      SUBROUTINE iau_RX ( PHI, R )
*+
*  - - - - - - -
*   i a u _ R X
*  - - - - - - -
*
*  Rotate an r-matrix about the x-axis.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     PHI      d         angle (radians)
*
*  Given and returned:
*     R        d(3,3)    r-matrix, rotated
*
*  Notes:
*
*  1) Calling this routine with positive PHI incorporates in the
*     supplied r-matrix R an additional rotation, about the x-axis,
*     anticlockwise as seen looking towards the origin from positive x.
*
*  2) The additional rotation can be represented by this matrix:
*
*         (  1        0            0      )
*         (                               )
*         (  0   + cos(PHI)   + sin(PHI)  )
*         (                               )
*         (  0   - sin(PHI)   + cos(PHI)  )
*
*  This revision:  2012 April 3
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION PHI, R(3,3)

      DOUBLE PRECISION S, C, A21, A22, A23, A31, A32, A33

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      S = SIN(PHI)
      C = COS(PHI)

      A21 =   C*R(2,1) + S*R(3,1)
      A22 =   C*R(2,2) + S*R(3,2)
      A23 =   C*R(2,3) + S*R(3,3)
      A31 = - S*R(2,1) + C*R(3,1)
      A32 = - S*R(2,2) + C*R(3,2)
      A33 = - S*R(2,3) + C*R(3,3)

      R(2,1) = A21
      R(2,2) = A22
      R(2,3) = A23
      R(3,1) = A31
      R(3,2) = A32
      R(3,3) = A33

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END SUBROUTINE

      SUBROUTINE iau_RZ ( PSI, R )
*+
*  - - - - - - -
*   i a u _ R Z
*  - - - - - - -
*
*  Rotate an r-matrix about the z-axis.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     PSI      d         angle (radians)
*
*  Given and returned:
*     R        d(3,3)    r-matrix, rotated
*
*  Notes:
*
*  1) Calling this routine with positive PSI incorporates in the
*     supplied r-matrix R an additional rotation, about the z-axis,
*     anticlockwise as seen looking towards the origin from positive z.
*
*  2) The additional rotation can be represented by this matrix:
*
*         (  + cos(PSI)   + sin(PSI)     0  )
*         (                                 )
*         (  - sin(PSI)   + cos(PSI)     0  )
*         (                                 )
*         (       0            0         1  )
*
*  This revision:  2012 April 3
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION PSI, R(3,3)

      DOUBLE PRECISION S, C, A11, A12, A13, A21, A22, A23

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      S = SIN(PSI)
      C = COS(PSI)

      A11 =   C*R(1,1) + S*R(2,1)
      A12 =   C*R(1,2) + S*R(2,2)
      A13 =   C*R(1,3) + S*R(2,3)
      A21 = - S*R(1,1) + C*R(2,1)
      A22 = - S*R(1,2) + C*R(2,2)
      A23 = - S*R(1,3) + C*R(2,3)

      R(1,1) = A11
      R(1,2) = A12
      R(1,3) = A13
      R(2,1) = A21
      R(2,2) = A22
      R(2,3) = A23

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END SUBROUTINE

      SUBROUTINE iau_IR ( R )
*+
*  - - - - - - -
*   i a u _ I R
*  - - - - - - -
*
*  Initialize an r-matrix to the identity matrix.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Returned:
*     R        d(3,3)    r-matrix
*
*  This revision:  2012 April 3
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION R(3,3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      R(1,1) = 1D0
      R(1,2) = 0D0
      R(1,3) = 0D0
      R(2,1) = 0D0
      R(2,2) = 1D0
      R(2,3) = 0D0
      R(3,1) = 0D0
      R(3,2) = 0D0
      R(3,3) = 1D0

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END SUBROUTINE

      SUBROUTINE iau_PMAT76 ( DATE1, DATE2, RMATP )
*+
*  - - - - - - - - - - -
*   i a u _ P M A T 7 6
*  - - - - - - - - - - -
*
*  Precession matrix from J2000.0 to a specified date, IAU 1976 model.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DATE1,DATE2    d       ending date, TT (Note 1)
*
*  Returned:
*     RMATP          d(3,3)  precession matrix, J2000.0 -> DATE1+DATE2
*
*  Notes:
*
*  1) The ending date DATE1+DATE2 is a Julian Date, apportioned
*     in any convenient way between the arguments DATE1 and DATE2.
*     For example, JD(TT)=2450123.7 could be expressed in any of
*     these ways, among others:
*
*            DATE1          DATE2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 method is best matched to the way
*     the argument is handled internally and will deliver the
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*
*  2) The matrix operates in the sense V(date) = RMATP * V(J2000),
*     where the p-vector V(J2000) is with respect to the mean
*     equatorial triad of epoch J2000.0 and the p-vector V(date)
*     is with respect to the mean equatorial triad of the given
*     date.
*
*  3) Though the matrix method itself is rigorous, the precession
*     angles are expressed through canonical polynomials which are
*     valid only for a limited time span.  In addition, the IAU 1976
*     precession rate is known to be imperfect.  The absolute accuracy
*     of the present formulation is better than 0.1 arcsec from
*     1960AD to 2040AD, better than 1 arcsec from 1640AD to 2360AD,
*     and remains below 3 arcsec for the whole of the period
*     500BC to 3000AD.  The errors exceed 10 arcsec outside the
*     range 1200BC to 3900AD, exceed 100 arcsec outside 4200BC to
*     5600AD and exceed 1000 arcsec outside 6800BC to 8200AD.
*
*  Called:
*     iau_PREC76   accumulated precession angles, IAU 1976
*     iau_IR       initialize r-matrix to identity
*     iau_RZ       rotate around Z-axis
*     iau_RY       rotate around Y-axis
*     iau_CR       copy r-matrix
*
*  References:
*
*     Lieske, J.H., 1979, Astron.Astrophys. 73, 282.
*      equations (6) & (7), p283.
*
*     Kaplan, G.H., 1981, USNO circular no. 163, pA2.
*
*  This revision:  2009 December 18
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, RMATP(3,3)

*  Reference epoch (J2000.0), JD
      DOUBLE PRECISION DJ00
      PARAMETER ( DJ00 = 2451545D0 )

      DOUBLE PRECISION ZETA, Z, THETA, WMAT(3,3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Precession Euler angles, J2000.0 to specified date.
      CALL iau_PREC76 ( DJ00, 0D0, DATE1, DATE2, ZETA, Z, THETA )

*  Form the rotation matrix.
      CALL iau_IR ( WMAT )
      CALL iau_RZ ( -ZETA, WMAT )
      CALL iau_RY ( THETA, WMAT )
      CALL iau_RZ ( -Z, WMAT )
      CALL iau_CR ( WMAT, RMATP )

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END SUBROUTINE

      SUBROUTINE iau_RY ( THETA, R )
*+
*  - - - - - - -
*   i a u _ R Y
*  - - - - - - -
*
*  Rotate an r-matrix about the y-axis.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     THETA    d         angle (radians)
*
*  Given and returned:
*     R        d(3,3)    r-matrix, rotated
*
*  Notes:
*
*  1) Calling this routine with positive THETA incorporates in the
*     supplied r-matrix R an additional rotation, about the y-axis,
*     anticlockwise as seen looking towards the origin from positive y.
*
*  2) The additional rotation can be represented by this matrix:
*
*         (  + cos(THETA)     0      - sin(THETA)  )
*         (                                        )
*         (       0           1           0        )
*         (                                        )
*         (  + sin(THETA)     0      + cos(THETA)  )
*
*  This revision:  2012 April 3
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION THETA, R(3,3)

      DOUBLE PRECISION S, C, A11, A12, A13, A31, A32, A33

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      S = SIN(THETA)
      C = COS(THETA)

      A11 = C*R(1,1) - S*R(3,1)
      A12 = C*R(1,2) - S*R(3,2)
      A13 = C*R(1,3) - S*R(3,3)
      A31 = S*R(1,1) + C*R(3,1)
      A32 = S*R(1,2) + C*R(3,2)
      A33 = S*R(1,3) + C*R(3,3)

      R(1,1) = A11
      R(1,2) = A12
      R(1,3) = A13
      R(3,1) = A31
      R(3,2) = A32
      R(3,3) = A33

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END SUBROUTINE

      SUBROUTINE iau_CR ( R, C )
*+
*  - - - - - - -
*   i a u _ C R
*  - - - - - - -
*
*  Copy an r-matrix.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     R        d(3,3)    r-matrix to be copied
*
*  Returned:
*     C        d(3,3)    copy
*
*  Called:
*     iau_CP       copy p-vector
*
*  This revision:  2000 November 25
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION R(3,3), C(3,3)

      INTEGER I

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      DO 1 I=1,3
         CALL iau_CP ( R(1,I), C(1,I) )
 1    CONTINUE

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END SUBROUTINE

      SUBROUTINE iau_CP ( P, C )
*+
*  - - - - - - -
*   i a u _ C P
*  - - - - - - -
*
*  Copy a p-vector.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     P        d(3)     p-vector to be copied
*
*  Returned:
*     C        d(3)     copy
*
*  This revision:  2000 November 25
*
*  SOFA release 2015-02-09
*
*  Copyright (C) 2015 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION P(3), C(3)

      INTEGER I

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      DO 1 I=1,3
         C(I) = P(I)
 1    CONTINUE

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2015
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END SUBROUTINE

      END MODULE
