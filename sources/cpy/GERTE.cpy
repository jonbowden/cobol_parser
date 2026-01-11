      * GERTE.cpyrk
      *****************************************************************
      *                        AMENDMENT HISTORY                      *
      *****************************************************************
      * NUMBER  DATE      BY    DESCRIPTION                           *
      * GH1SDF  04/10/2002 TMPSDF GLOBAL HUBBING:                     *
      *                        AMEND FIELD LENGHT SIZE/TYPE OF -      *
      *                        -WK-N-GERTE-BNKENTTY FROM S9(1)        *
      *                        TO X(02).                              *
      *****************************************************************
      *----------------- COPYBOOK FOR CALLING TRFGERTE - 25/09/89 ------
      -    ---*
       01 WK-C-GERTE-RECORD.
           05 WK-C-GERTE-INPUT.
           10 WK-C-GERTE-CUYVCD    PIC X(03).
           10 WK-C-GERTE-RTE-TYP   PIC X(02).
           10 WK-N-GERTE-BNKENTTY  PIC S9(01).
           10 WK-N-GERTE-BNKENTTY  PIC X(02).
           05 WK-C-GERTE-OUTPUT.
           10 WK-C-GERTE-INVALID-OUTPUT.
           15 WK-C-GERTE-ERROR-CD PIC X(07).
           15 WK-C-GERTE-COM0026.
           20 WK-C-GERTE-FILE PIC X(08).
           20 WK-C-GERTE-MODE PIC X(06).
           20 WK-C-GERTE-KEY  PIC X(20).
           20 WK-C-GERTE-FS   PIC X(02).
           10 WK-C-GERTE-VALID-OUTPUT.
           15 WK-N-GERTE-EXCH-RTE PIC S9(04)V9(06).
           15 WK-N-GERTE-EXCH-RTE PIC S9(09)V9(07).
           15 WK-N-GERTE-FXRATEUT PIC S9(05).
           15 WK-N-GERTE-TOLERNCE PIC S9(02)V9(02).
