      * VBANO.cpybk
      ******************************************************************
      * HISTORY OF MODIFICATION:
      ******************************************************************
      * GH1NVB - NVUBOT - 27/09/2002 - GLOBAL HUBBING.
      *            1. CHANGE WK-C-VBANO-ACCNO
      *               X(11) TO X(15).
      *            2. CHANGE WK-N-VBANO-BNKENTTY
      *               S9(1) TO X(2).
      *----------------------------------------------------------------*
      *-------- COPYBOOK FOR CALLING TRFVBANO - 22/11/89 ---------*
       01  WK-C-VBANO-RECORD.
           05  WK-C-VBANO-INPUT.
              GH1NVB*       10  WK-N-VBANO-BNKENTTY  PIC S9(01).
GH1NVB     10  WK-N-VBANO-BNKENTTY  PIC X(02).
           10  WK-C-VBANO-BANKID   PIC X(11).
           10  WK-C-VBANO-CUYCD    PIC X(03).
              GH1NVB*       10  WK-C-VBANO-ACCNO    PIC X(11).
GH1NVB     10  WK-C-VBANO-ACCNO    PIC X(15).
           05  WK-C-VBANO-OUTPUT.
           10  WK-C-VBANO-INVALID-OUTPUT.
           15  WK-C-VBANO-ERROR-CD PIC X(07).
           15  WK-C-VBANO-COM0026.
           20  WK-C-VBANO-FILE PIC X(08).
           20  WK-C-VBANO-MODE PIC X(06).
           20  WK-C-VBANO-KEY PIC X(20).
           20  WK-C-VBANO-FS PIC X(02).
           10  WK-C-VBANO-VALID-OUTPUT.
           15  WK-C-VBANO-ACCTYP PIC X(01).
           15  WK-C-VBANO-ACUDUBI PIC X(01).
           15  WK-C-VBANO-AGTCOMIS PIC 9(13)V99.
