      *  HISTORY OF MODIFICATION:                                           *
      *---------------------------------------------------------------------*
      *  G2BL00 - ACNRJR  -  25/03/2019 - CASH MANAGEMENT ROAD MAP - P19    *
      *                             - GPI Day4 (Retrofit from GPI Day2b HO) *
      *                             - ADDED NEW FIELD FOR PMODE             *
      *---------------------------------------------------------------------*
      *  7Q1EM1 - TMPEYIM -  22/09/2016 - REM Q1 2017 RELEASE               *
      *                             - e-Reg 47511 Refinement of             *
      *                             - Duplicate checking for Inw           *
      *                             - Added new fields                     *
      *---------------------------------------------------------------------*
      *------- COPYBOOK FOR CALLING TRVF1F1A - 28/07/2011 ------------------*
       01 WK-VTF1A.                                                         
           05 WK-VTF1A-INPUT.                                              
               10 WK-VTF1A-PARALNO      PIC 9(08).                         
               10 WK-VTF1A-SQBNUM       PIC 9(02).                         
SM1TY1         10 WK-VTF1A-RBK-IND      PIC X(01).                         
7Q1EM1         10 WK-VTF1A-TNRREF       PIC X(16).                         
G2BL00         10 WK-VTF1A-DR-PMODE     PIC X(08).                         
           05 WK-VTF1A-OUTPUT.                                             
               10 WK-VTF1A-ERROR-FOUND  PIC X(01).                         
               10 WK-VTF1A-DATAFIA      PIC X(20).                         
               10 WK-VTF1A-STPTYP       PIC X(04).                         
               10 WK-VTF1A-PMODE        PIC X(08).                         
               10 WK-VTF1A-ACCCUV       PIC X(03).                         
               10 WK-VTF1A-BANKID       PIC X(11).                         
               10 WK-VTF1A-ACBNKID      PIC X(11).                         
               10 WK-VTF1A-BENBNKID     PIC X(11).                         
               10 WK-VTF1A-BANKAC       PIC X(11).                         
               10 WK-VTF1A-ACBNKACC     PIC X(11).                         
               10 WK-VTF1A-BENBKACC     PIC X(11).                         
               10 WK-VTF1A-BENEACC      PIC X(11).                         
               10 WK-VTF1A-BENENAME     PIC X(35).                         
               10 WK-VTF1A-BENEADR1     PIC X(35).                         
               10 WK-VTF1A-BENEADR2     PIC X(35).                         
               10 WK-VTF1A-BENEADR3     PIC X(35).                         
               10 WK-VTF1A-BENEADR4     PIC X(35).                         
               10 WK-VTF1A-BENEADR5     PIC X(35).                         
               10 WK-VTF1A-BENEADR6     PIC X(35).                         
               10 WK-VTF1A-AOCD         PIC X(04).                         
               10 WK-VTF1A-RESCD        PIC 9(02).                         
               10 WK-VTF1A-DOMBRCH      PIC 9(03).                         
ID1VKE*        10 WK-VTF1A-DOMBRCH      PIC 9(04).                         
ID1VKE         10 WK-VTF1A-HOLDCD1      PIC 9(02).                         
               10 WK-VTF1A-HOLDCD2      PIC 9(02).                         
               10 WK-VTF1A-HOLDCD3      PIC 9(02).                         
               10 WK-VTF1A-BANKACTYP    PIC X(01).                         
               10 WK-VTF1A-ACUBDUI      PIC X(01).                         
               10 WK-VTF1A-BENEFLG      PIC X(01).                         
               10 WK-VTF1A-NO910        PIC X(01).                         