      *  HISTORY OF MODIFICATION:                                           *
      *---------------------------------------------------------------------*
      *  G2BL00 - ACNJR   - 25/03/2019 - CASH MANAGEMENT ROAD MAP - P19     *
      *                             GPI Day4 (Retrofit from GPI Day2b HO)   *
      *                             - ADDED NEW FIELD FOR PMODE +           *
      *---------------------------------------------------------------------*
      *  7Q1EM1 - TMPEYM  - 25/11/2016 - REM Q1 2017 RELEASE +              *
      *                             - e-Reg 47511 Refinement of             *
      *                             - Duplicate checking for Inw +          *
      *                             - Added new fields                      *
      *---------------------------------------------------------------------*
      *----------------- COPYBOOK FOR CALLING TRVFTF1B - 28/07/2011 -----------------*
       01 WK-VTF1B.                                                              
           05 WK-VTF1B-INPUT.                                                    
              10 WK-VTF1B-PARALNO        PIC 9(08).                              
              10 WK-VTF1B-SEQNUM         PIC 9(02).                              
              10 WK-VTF1B-TRNX-IND       PIC X(01).                              
SM1TY1        10 WK-VTF1B-TRNREF         PIC X(16).                              
7Q1EM1        10 WK-VTF1B-DR-PMODE       PIC X(08).                              
G2BL00     05 WK-VTF1B-OUTPUT.                                                   
              10 WK-VTF1B-ERROR-FOUND    PIC X(01).                              
              10 WK-VTF1B-DATAF1B        PIC X(20).                              
              10 WK-VTF1B-DATAF2         PIC X(20).                              
              10 WK-VTF1B-STYPF          PIC X(04).                              
              10 WK-VTF1B-MPMODE         PIC X(08).                              
              10 WK-VTF1B-ACCCUV         PIC X(03).                              
              10 WK-VTF1B-BANKID         PIC X(11).                              
              10 WK-VTF1B-ACBNKID        PIC X(11).                              
              10 WK-VTF1B-BENBNKID       PIC X(11).                              
              10 WK-VTF1B-BANKAC         PIC X(11).                              
              10 WK-VTF1B-ACBNKACC       PIC X(11).                              
              10 WK-VTF1B-BENBANKACC     PIC X(35).                              
              10 WK-VTF1B-BENBENACC      PIC X(11).                              
              10 WK-VTF1B-BENENAME       PIC X(35).                              
              10 WK-VTF1B-BENEADR1       PIC X(35).                              
              10 WK-VTF1B-BENEADR2       PIC X(35).                              
              10 WK-VTF1B-BENEADR3       PIC X(35).                              
              10 WK-VTF1B-BENEADR4       PIC X(35).                              
              10 WK-VTF1B-BENEADR5       PIC X(35).                              
              10 WK-VTF1B-BENEADR6       PIC X(35).                              
              10 WK-VTF1B-AOCD           PIC X(04).                              
              10 WK-VTF1B-RESCD          PIC 9(02).                              
ID1VKE*       10 WK-VTF1B-DOMBRCH        PIC 9(03).                              
ID1VKE        10 WK-VTF1B-DOMBRCH        PIC 9(04).                              
              10 WK-VTF1B-HOLDCD1        PIC 9(02).                              
              10 WK-VTF1B-HOLDCD2        PIC 9(02).                              
              10 WK-VTF1B-HOLDCD3        PIC 9(02).                              
              10 WK-VTF1B-LCAMT          PIC 9(13)V99.                           
              10 WK-VTF1B-FXRATETY       PIC X(02).                              
              10 WK-VTF1B-FXRATE         PIC S9(09)V9(07).                       
              10 WK-VTF1B-FXRATEUT       PIC S9(05).                             
              10 WK-VTF1B-BANKACTYP      PIC X(01).                              
              10 WK-VTF1B-ACUBDUI        PIC X(01).                              
              10 WK-VTF1B-BENEFLG        PIC X(01).                              
              10 WK-VTF1B-NO910          PIC X(01).                              