      *  HISTORY OF MODIFICATION:                                           *
      *---------------------------------------------------------------------*
      *  G2BL00 - ACNRJR  - 25/03/2019 - CASH MANAGEMENT ROAD MAP - P19     *
      *                             GPI Day4 (Retrofit from GPI Day2b HO)   *
      *                             - Added new field for PMODE             *
      *---------------------------------------------------------------------*
      *  7Q1EM1 - TMPEYM  - 25/11/2016 - REM Q1 2017 RELEASE                *
      *                             - e-Reg 47511 Refinement of             *
      *                             - Duplicate checking for Inw            *
      *                             - Added new fields                      *
      *---------------------------------------------------------------------*
      *----------------- COPYBOOK FOR CALLING TRFVTC1 - 28/07/2011 -----------------*
       01 WK-VTC1.                                                              
           05 WK-VTC1-INPUT.                                                    
              10 WK-VTC1-PARALNO      PIC 9(08).                               
              10 WK-VTC1-SEQNUM       PIC 9(02).                               
              10 WK-VTC1-IBK-IND      PIC X(01).                               
              10 WK-VTC1-TRNRFF       PIC X(16).                               
              10 WK-VTC1-DR-PMODE     PIC X(08).                               
           05 WK-VTC1-OUTPUT.                                                   
              10 WK-VTC1-ERROR-FOUND  PIC X(01).                               
              10 WK-VTC1-DATAC1       PIC X(20).                               
              10 WK-VTC1-SPTYP        PIC X(04).                               
              10 WK-VTC1-PMODE        PIC X(08).                               
              10 WK-VTC1-ACCCUV       PIC X(03).                               
              10 WK-VTC1-BANKID       PIC X(11).                               
              10 WK-VTC1-ACBNKID      PIC X(11).                               
              10 WK-VTC1-BENBNKID     PIC X(11).                               
              10 WK-VTC1-BANKAC       PIC X(11).                               
              10 WK-VTC1-ACBNKACC     PIC X(11).                               
              10 WK-VTC1-BENBKACC     PIC X(35).                               
              10 WK-VTC1-BENACC       PIC X(11).                               
              10 WK-VTC1-BENNAME      PIC X(35).                               
              10 WK-VTC1-BENEADR1     PIC X(35).                               
              10 WK-VTC1-BENEADR2     PIC X(35).                               
              10 WK-VTC1-BENEADR3     PIC X(35).                               
              10 WK-VTC1-BENEADR4     PIC X(35).                               
              10 WK-VTC1-BENEADR5     PIC X(35).                               
              10 WK-VTC1-BENEADR6     PIC X(35).                               
              10 WK-VTC1-AQCD         PIC X(04).                               
              10 WK-VTC1-RESCD        PIC 9(02).                               
      ID1VKE* 10 WK-VTC1-DOMBRCH      PIC 9(03).                               
      ID1VKE   10 WK-VTC1-DOMBRCH     PIC 9(04).                               
              10 WK-VTC1-HOLDCD1      PIC 9(02).                               
              10 WK-VTC1-HOLDCD2      PIC 9(02).                               
              10 WK-VTC1-HOLDCD3      PIC 9(02).                               
              10 WK-VTC1-BANKACTYP    PIC X(01).                               
              10 WK-VTC1-ACUDUBUI     PIC X(01).                               
              10 WK-VTC1-BENEFLG      PIC X(01).                               
              10 WK-VTC1-NO910        PIC X(01).                               