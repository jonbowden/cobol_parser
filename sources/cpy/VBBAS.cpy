      * VBBAS.cpybk
      ***************************************************************
      *      AMENDMENT HISTORY
      -    *
      ***************************************************************
      *      NUMBER  DATE        BY      DESCRIPTION
      -    *
      *      -----------------------------------------------------------
      -    *
      *      GH1MBA  29/11/2002  MBAVILES -EXPAND SHIFTNO FIELD
      -    *
      ***************************************************************
      *--------- COPYBOOK FOR CALLING TRFVBBAS - 14/10/89 ---------*

       01  WK-C-VBBAS-RECORD.
           05  WK-C-VBBAS-INPUT.
           10  WK-C-VBBAS-BANKID      PIC X(11).
           05  WK-C-VBBAS-OUTPUT.
           10  WK-C-VBBAS-INVALID-OUTPUT.
           15  WK-C-VBBAS-ERROR-CD     PIC X(07).
           15  WK-C-VBBAS-COM00206.
           20  WK-C-VBBAS-FILE        PIC X(08).
           20  WK-C-VBBAS-MODE        PIC X(06).
           20  WK-C-VBBAS-KEY         PIC X(20).
           20  WK-C-VBBAS-FS          PIC X(02).
           10  WK-C-VBBAS-VALID-OUTPUT.
           15  WK-C-VBBAS-BNKSSNAME    PIC X(11).
           15  WK-C-VBBAS-BNKFNAME     PIC X(35).
           15  WK-C-VBBAS-SWFTADRI     PIC X(01).
           15  WK-C-VBBAS-SHIFTNO      PIC X(04).
           15  WK-C-VBBAS-SHIFTNO      PIC X(10).
           15  WK-C-VBBAS-CHIPNO       PIC X(06).
           15  WK-C-VBBAS-TELEXNO      PIC X(15).
           15  WK-C-VBBAS-ADDR1        PIC X(35).
           15  WK-C-VBBAS-ADDR2        PIC X(35).
           15  WK-C-VBBAS-ADDR3        PIC X(35).
