      *  HISTORY OF MODIFICATION:                                              *
      *-----------------------------------------------------------------------*
      *  G2BL00 - ACNRJR  -  25/03/2019 - CASH MANAGEMENT ROAD MAP - P19      *
      *                           GPI Day4 (Retrofit from GPI Day2b HO) *     *
      *                           - ADDED NEW FIELD FOR PMODE *               *
      *-----------------------------------------------------------------------*
      *----------- COPYBOOK FOR CALLING TRVFTE2 - 28/07/2011 -----------------*
       01 WK-VTE2.                                                            
           05 WK-VTE2-INPUT.                                                  
              10 WK-VTE2-PARALNO         PIC 9(08).                           
              10 WK-VTE2-SERQNM          PIC 9(02).                           
              10 WK-VTE2-PR-PMODE        PIC X(08).                           
       G2BL00 05 WK-VTE2-OUTPUT.                                             
              10 WK-VTE2-ERROR-FOUND     PIC X(01).                           
              10 WK-VTE2-DATAE2          PIC X(20).                           
              10 WK-VTE2-ACT             PIC X(01).                           
              10 WK-VTE2-SPTYP           PIC X(04).                           
              10 WK-VTE2-PMODE           PIC X(08).                           
              10 WK-VTE2-BANKID          PIC X(11).                           
              10 WK-VTE2-ACCNKID         PIC X(11).                           
              10 WK-VTE2-BENBNKID        PIC X(11).                           
              10 WK-VTE2-BANKAC          PIC X(11).                           
              10 WK-VTE2-ACBNKACCC       PIC X(11).                           
       SM1    10 WK-VTE2-BENBKACC        PIC X(35).                           
              10 WK-VTE2-BENBNKMN        PIC X(35).                           
              10 WK-VTE2-BENBKADR1       PIC X(35).                           
              10 WK-VTE2-BENBKADR2       PIC X(35).                           
              10 WK-VTE2-BENBKADR3       PIC X(35).                           
              10 WK-VTE2-SHIFTNO         PIC X(04).                           
              10 WK-VTE2-BANKACTYP       PIC X(01).                           
              10 WK-VTE2-ACUDBUI         PIC X(01).                           