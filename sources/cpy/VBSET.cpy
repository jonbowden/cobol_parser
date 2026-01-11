      * VBSET.cpybk
      *****************************************************************
      * HISTORY OF MODIFICATION:
      *****************************************************************
      * GH1NVB - NVBUOT - 04/10/2002 - GLOBAL HUBBING.
      * 1. CHANGE WK-N-VBSET-BNKENTTY
      * S9(1) TO X(2).
      *----------------------------------------------------------------
      *-------- COPYBOOK FOR CALLING TRFVBSET - 11/10/89 ---------*
       01  WK-C-VBSET-RECORD.
           05  WK-C-VBSET-INPUT.
           10  WK-N-VBSET-BNKENTTY  PIC S9(01).
           10  WK-N-VBSET-BNKENTTY  PIC X(02).
           10  WK-C-VBSET-BANKID    PIC X(11).
           10  WK-C-VBSET-CUVYCD    PIC X(03).
           10  WK-C-VBSET-STLTMBNK  PIC X(11).
           05  WK-C-VBSET-OUTPUT.
           10  WK-C-VBSET-INVALID-OUTPUT.
           15  WK-C-VBSET-ERROR-CD  PIC X(07).
           15  WK-C-VBSET-COM0026.
           20  WK-C-VBSET-FILE  PIC X(08).
           20  WK-C-VBSET-MODE  PIC X(06).
           20  WK-C-VBSET-KEY   PIC X(20).
           20  WK-C-VBSET-FS    PIC X(02).
