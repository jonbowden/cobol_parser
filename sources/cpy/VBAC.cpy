      *=================================================================*
      *Copybook Name    : VBACU                                        *
      *Copybook Description : REM Copybook for Input/Output of TRFVBACU *
      *Date Created     : 03 April 2019                                *
      *Created by       : Accenture                                    *
      *=================================================================*
      * HISTORY OF MODIFICATION:                                       *
      *=================================================================*
      * MOD.#   INIT   DATE        DESCRIPTION                         *
      * ------  ------ ----------  ----------------------------------- *
      * G2BL00 - ACNJR - 03/04/19 - CASH MANAGEMENT ROAD MAP - P19    *
      *                        - GPI Day4 (Retrofit from GPI Day2b HO)*
      *                        - Initial Version.                     *
      *=================================================================*
       01  WK-C-VBACU-RECORD.
           05  WK-C-VBACU-INPUT.
               10  WK-C-VBACU-BANKID         PIC X(11).
               10  WK-C-IN-FILLA1            PIC X(20).
               10  WK-C-IN-FILLA2            PIC X(20).
               10  WK-C-IN-FILLA3            PIC X(20).
               10  WK-C-IN-FILLA4            PIC X(20).
               10  WK-C-IN-FILLA5            PIC X(20).
               10  WK-C-IN-FILLN1            PIC S9(13)V9(2).
               10  WK-C-IN-FILLN2            PIC S9(13)V9(2).
               10  WK-C-IN-FILLN3            PIC S9(13)V9(2).
               10  WK-C-IN-FILLN4            PIC S9(13)V9(2).
               10  WK-C-IN-FILLN5            PIC S9(13)V9(2).
           05  WK-C-VBACU-OUTPUT.
               10  WK-C-VBACU-INVALID-OUTPUT.
                   15  WK-C-VBACU-ERROR-CD   PIC X(07).
               15  WK-C-VBACU-COM00206.
                   20  WK-C-VBACU-FILE       PIC X(08).
                   20  WK-C-VBACU-MODE       PIC X(06).
                   20  WK-C-VBACU-KEY        PIC X(20).
                   20  WK-C-VBACU-FS         PIC X(02).
               10  WK-C-VBACU-VALID-OUTPUT.
                   15  WK-C-VBACU-UOBBRH     PIC X(01).
                   15  WK-C-VBACU-CNTRYINCO  PIC X(02).
                   15  WK-C-VBACU-CNTRYCD    PIC X(02).
                   15  WK-C-VBACU-LOCATNCD   PIC X(02).
               10  WK-C-OUT-FILLA1           PIC X(20).
               10  WK-C-OUT-FILLA2           PIC X(20).
               10  WK-C-OUT-FILLA3           PIC X(20).
               10  WK-C-OUT-FILLA4           PIC X(20).
               10  WK-C-OUT-FILLA5           PIC X(20).
               10  WK-C-OUT-FILLN1           PIC S9(13)V9(2).
               10  WK-C-OUT-FILLN2           PIC S9(13)V9(2).
               10  WK-C-OUT-FILLN3           PIC S9(13)V9(2).
               10  WK-C-OUT-FILLN4           PIC S9(13)V9(2).
               10  WK-C-OUT-FILLN5           PIC S9(13)V9(2).
