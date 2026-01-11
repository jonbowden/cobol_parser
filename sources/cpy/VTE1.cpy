      *=====================================================================*
      * HISTORY OF MODIFICATION:                                           *
      *---------------------------------------------------------------------*
      * G2BL00 - ACNRJR  - 25/03/2019 - CASH MANAGEMENT ROAD MAP - P19     *
      *                               - GPI Day4 (Retrofit from GPI Day2b HO)*
      *                               - ADDED NEW FIELD FOR PMODE          *
      *---------------------------------------------------------------------*
      *----------------- COPYBOOK FOR CALLING TRFVTE1 - 28/07/2011 -----------------*
       01 WK-VTE1.
           03 WK-VTE1-INPUT.
               05 WK-VTE1-PARALNO       PIC 9(08).
               05 WK-VTE1-SEQNUM        PIC 9(02).
               05 WK-VTE1-DR-PMODE      PIC X(08).
           03 WK-VTE1-OUTPUT.
               05 WK-VTE1-ERROR-FOUND   PIC X(01).
               05 WK-VTE1-DATAE1        PIC X(20).
           05 WK-VTE1-ACT.
               07 WK-VTE1-ACT1          PIC X(01).
               07 WK-VTE1-ACT2          PIC X(01).
               07 WK-VTE1-ACT3          PIC X(01).
               05 WK-VTE1-SPTTYP        PIC X(04).
               05 WK-VTE1-PMODE         PIC X(08).
               05 WK-VTE1-BANKID        PIC X(11).
               05 WK-VTE1-INTEMBNKID    PIC X(11).
               05 WK-VTE1-ACBNKID       PIC X(11).
               05 WK-VTE1-BANKAC        PIC X(11).
               05 WK-VTE1-INTEMBNKACC   PIC X(11).
               05 WK-VTE1-ACBNKACC      PIC X(11).
               05 WK-VTE1-ACBNKNM       PIC X(35).
               05 WK-VTE1-ACBNKADR1     PIC X(35).
               05 WK-VTE1-ACBNKADR2     PIC X(35).
               05 WK-VTE1-ACBNKADR3     PIC X(35).
               05 WK-VTE1-SHIFTNO       PIC X(04).
               05 WK-VTE1-BANKACTYP     PIC X(01).
               05 WK-VTE1-ACUDBUI       PIC X(01).