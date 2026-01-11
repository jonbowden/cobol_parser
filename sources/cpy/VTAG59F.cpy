      *=================================================================*
      *Copybook Name    : VTAG59F                                      *
      *Copybook Description : REM Copybook for TRFVTAG59F Program      *
      *Date Created     : 26 October 2019                              *
      *Created by       : Accenture                                    *
      *=================================================================*
      * HISTORY OF MODIFICATION:                                       *
      *=================================================================*
      * MOD.#   INIT   DATE        DESCRIPTION                         *
      * ------  ------ ----------  ----------------------------------- *
      * GP3600 - ACNDUS - 26/10/19 - CASH MANAGEMENT ROAD MAP - P19    *
      *                         - GPI Day4 (Retro from GPI Day3 HO)    *
      *                         - Initial Version.                     *
      *=================================================================*

       01  WK-C-VTAG59F-RECORD.
           05  WK-C-VTAG59F-INPUT.
               10  WK-C-VTAG59F-REMIND          PIC X(01).
               10  WK-C-VTAG59F-I-BENE-CUST.
                   15  WK-C-VTAG59F-I-BENE      PIC X(35) OCCURS 5.
               10  WK-C-VTAG59F-I-FILLER.
                   15  WK-C-VTAG59F-I-FILLA1    PIC X(35).
                   15  WK-C-VTAG59F-I-FILLA2    PIC X(35).
                   15  WK-C-VTAG59F-I-FILLA3    PIC X(35).
                   15  WK-C-VTAG59F-I-FILLN1    PIC S9(13)V9(2).
                   15  WK-C-VTAG59F-I-FILLN2    PIC S9(13)V9(2).
                   15  WK-C-VTAG59F-I-FILLN3    PIC S9(13)V9(2).
           05  WK-C-VTAG59F-OUTPUT.
               10  WK-C-VTAG59F-INVALID-OUTPUT.
                   15  WK-C-VTAG59F-ERROR-CD    PIC X(07).
                   15  WK-C-VTAG59F-COM02806.
                       20  WK-C-VTAG59F-FILE   PIC X(08).
                       20  WK-C-VTAG59F-MODE   PIC X(06).
                       20  WK-C-VTAG59F-KEY    PIC X(20).
                       20  WK-C-VTAG59F-FS     PIC X(02).
               10  WK-C-VTAG59F-VALID-OUTPUT.
                   15  WK-C-VTAG59F-O-BENE-CUST.
                       20  WK-C-VTAG59F-O-BENE-NME PIC X(100).
                       20  WK-C-VTAG59F-O-BENE-ADR PIC X(35) OCCURS 5.
                   15  WK-C-VTAG59F-I-FILLER.
                       20  WK-C-VTAG59F-O-FILLA1 PIC X(35).
                       20  WK-C-VTAG59F-O-FILLA2 PIC X(35).
                       20  WK-C-VTAG59F-O-FILLA3 PIC X(35).
                       20  WK-C-VTAG59F-O-FILLN1 PIC S9(13)V9(2).
                       20  WK-C-VTAG59F-O-FILLN2 PIC S9(13)V9(2).
                       20  WK-C-VTAG59F-O-FILLN3 PIC S9(13)V9(2).