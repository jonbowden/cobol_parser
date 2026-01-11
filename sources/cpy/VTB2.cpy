      * HISTORY OF MODIFICATION:                                           *
      *--------------------------------------------------------------------*
      * G2BL00 - ACNRJR  - 25/03/2019 - CASH MANAGEMENT ROAD MAP - P19     *
      *                         GPI Day4 (Retrofit from GPI Day2b HO)      *
      *                         - ADDED NEW FIELD FOR PMODE                *
      *--------------------------------------------------------------------*
      *----------- COPYBOOK FOR CALLING TRFVTB2 - 28/07/2011 --------------*
       01 WK-VTB2.
           03 WK-VTB2-INPUT.
               05 WK-VTB2-PARALNO       PIC 9(08).
               05 WK-VTB2-SERQNUM       PIC 9(02).
               05 WK-VTB2-BR-PMODE      PIC X(08).
           03 WK-VTB2-OUTPUT.
               05 WK-VTB2-ERROR-FOUND   PIC X(01).
               05 WK-VTB2-DATAB2        PIC X(20).
           05 WK-VTB2-ACT.
               07 WK-VTB2-ACT1          PIC X(01).
               07 WK-VTB2-ACT2          PIC X(01).
               07 WK-VTB2-ACT3          PIC X(01).
               07 WK-VTB2-ACT4          PIC X(01).
               07 WK-VTB2-ACT5          PIC X(01).
               07 WK-VTB2-ACT6          PIC X(01).
           05 WK-VTB2-STPTYP            PIC X(04).
           05 WK-VTB2-PMODE             PIC X(08).
           05 WK-VTB2-BANKID            PIC X(11).
           05 WK-VTB2-ACBNKID           PIC X(11).
           05 WK-VTB2-BENBKID           PIC X(11).
           05 WK-VTB2-BANKAC            PIC X(11).
           05 WK-VTB2-ACBNKACC          PIC X(11).
      SM1   05 WK-VTB2-BENBKACC         PIC X(35).
           05 WK-VTB2-BENBKNM           PIC X(35).
           05 WK-VTB2-BENBKADR1         PIC X(35).
           05 WK-VTB2-BENBKADR2         PIC X(35).
           05 WK-VTB2-BENBKADR3         PIC X(35).
           05 WK-VTB2-SHIFTNO           PIC X(04).
           05 WK-VTB2-BANKACTYP         PIC X(01).
           05 WK-VTB2-ACUDBUI           PIC X(01).