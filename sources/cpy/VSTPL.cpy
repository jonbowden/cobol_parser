      *=================================================================
      -    ============*
      * FILE NAME        : VSTPC
      -    *
      * FILE DESCRIPTION : COPYBOOK FOR TRFVSTPC OF STP BY CURRENCY
      -    *
      * DATE CREATED     : 15 FEBRUARY 2020
      -    *
      * CREATED BY       : ACCENTURE
      -    *
      *=================================================================
      -    ============*
      * HISTORY OF MODIFICATION:
      -    *
      *=================================================================
      -    ============*
      * MOD.#   INIT   DATE        DESCRIPTION
      -    *
      * ------  -----  ----------  -------------------------------------
      -    ----------  *
      * GP4D00  ACNDUS 15/02/2020 - CASH MANAGEMENT ROAD MAP - P19 GPI
      -    DAY4         *
      *                      - INITIAL VERSION
      -    *
      *=================================================================
      -    ============*
      *------------------  COPYBOOK FOR CALLING TRFVSTPC  --------------
      -    -----------*
       01 WK-C-VSTPC-RECORD.
           05 WK-C-VSTPC-INPUT.
           10 WK-C-VSTPC-I-IMSGTYPE PIC X(01).
           10 WK-C-VSTPC-I-CUYCD   PIC X(03).
           10 WK-C-VSTPC-I-AMT     PIC S9(15)V9(2).
           05 WK-C-VSTPC-OUTPUT.
           10 WK-C-VSTPC-INVALID-OUTPUT.
           15 WK-C-VSTPC-ERROR-CD  PIC X(07).
           15 WK-C-VSTPC-COM02086.
           20 WK-C-VSTPC-FILE  PIC X(08).
           20 WK-C-VSTPC-MODE  PIC X(06).
           20 WK-C-VSTPC-KEY   PIC X(20).
           20 WK-C-VSTPC-FS    PIC X(02).
           10 WK-C-VSTPC-VALID-OUTPUT.
           15 WK-C-VSTPC-STATUS   PIC X(02).
           10 WK-C-VSTPC-O-FILLA1    PIC X(35).
           10 WK-C-VSTPC-O-FILLA2    PIC X(35).
           10 WK-C-VSTPC-O-FILLA3    PIC X(35).
           10 WK-C-VSTPC-O-FILLA4    PIC X(35).
           10 WK-C-VSTPC-O-FILLA5    PIC X(35).
           10 WK-C-VSTPC-O-FILLN1    PIC S9(15)V9(2).
           10 WK-C-VSTPC-O-FILLN2    PIC S9(15)V9(2).
           10 WK-C-VSTPC-O-FILLN3    PIC S9(15)V9(2).
           10 WK-C-VSTPC-O-FILLN4    PIC S9(15)V9(2).
           10 WK-C-VSTPC-O-FILLN5    PIC S9(15)V9(2).
