      * VCFA.cpyrk
      *=============================================================
      * HISTORY OF MODIFICATION:
      *=============================================================
      * 626JTC - DEE JANE - 07/05/2007 - added fields starting from
      *           WK-C-VCFA-SEG-CODE upto
      *           WK-C-VCFA-ACCT-ADDR4.
      *-------------------------------------------------------------
      * GH1NVB - NVBUOT - 02/10/2002 - GLOBAL HUBBING.
      *           1. CHANGE WK-N-VCCA-DOMBRCH
      *              S9(3) TO X(4).
      *           2. CHANGE WK-C-VCFA-CA-NO
      *              X(11) TO X(15).
      *-------------------------------------------------------------
      *---------- COPYBOOK FOR CALLING TRFVCFA - 12/10/89 ---------*
       01 WK-C-VCFA-RECORD.
           05 WK-C-VCFA-INPUT.
              GH1NVB*      10 WK-C-VCFA-FCCA       PIC X(11).
GH1NVB     10 WK-C-VCFA-FCCA       PIC X(15).
      *UNCOBH FCCA:START
           10 WK-C-VCFA-CUY         PIC X(03).
      *UNCOBH FCCA:END
           05 WK-C-VCFA-OUTPUT.
           10 WK-C-VCFA-INVALID-OUTPUT.
           15 WK-C-VCFA-ERROR-CD   PIC X(07).
           15 WK-C-VCFA-COM0206.
           20 WK-C-VCFA-FILE   PIC X(08).
           20 WK-C-VCFA-MODE   PIC X(06).
           20 WK-C-VCFA-KEY    PIC X(20).
           20 WK-C-VCFA-FS     PIC X(02).
           10 WK-C-VCFA-VALID-OUTPUT.
           15 WK-C-VCFA-CUSTFNAM  PIC X(35).
           15 WK-C-VCFA-ADDR1     PIC X(35).
           15 WK-C-VCFA-ADDR2     PIC X(35).
           15 WK-C-VCFA-ADDR3     PIC X(35).
           15 WK-C-VCFA-ADDR4     PIC X(35).
           15 WK-C-VCFA-ADDR5     PIC X(35).
           15 WK-C-VCFA-ADDR6     PIC X(35).
           15 WK-N-VCFA-RESCD     PIC S9(02).
           15 WK-N-VCFA-HOLDCD1   PIC S9(02).
           15 WK-N-VCFA-HOLDCD2   PIC S9(02).
           15 WK-N-VCFA-HOLDCD3   PIC S9(02).
           15 WK-C-VCFA-AOCD      PIC X(04).
           15 WK-N-VCFA-STAFFID   PIC S9(02).
              GH1NVB*         15 WK-N-VCFA-DOMBRCH   PIC S9(03).
GH1NVB     15 WK-N-VCFA-DOMBRCH   PIC X(04).
           15 WK-N-VCFA-AVAIL-BAL PIC S9(13)V99.
           15 WK-N-VCFA-CURR-BAL  PIC S9(13)V99.
      * start of 626JTC
           15 WK-C-VCFA-SEG-CODE  PIC X(01).
           15 WK-C-VCFA-CTO-CODE  PIC X(10).
           15 WK-C-VCFA-REG-ADDR1 PIC X(35).
           15 WK-C-VCFA-REG-ADDR2 PIC X(35).

           15  WK-C-VCFA-REG-ADDR3       PIC X(35).
           15  WK-C-VCFA-REG-ADDR4       PIC X(35).
           15  WK-C-VCFA-REG-BLK         PIC X(07).
           15  WK-C-VCFA-REG-STOREY      PIC X(04).
           15  WK-C-VCFA-REG-UNIT        PIC X(07).
           15  WK-C-VCFA-REG-PO-BOX      PIC X(06).
           15  WK-C-VCFA-REG-BUILD       PIC X(45).
           15  WK-C-VCFA-REG-STREET      PIC X(32).
           15  WK-C-VCFA-REG-STATE       PIC X(20).
           15  WK-N-VCFA-REG-POSTAL      PIC 9(09).
           15  WK-C-VCFA-REG-CTY-CODE    PIC X(03).
           15  WK-C-VCFA-REG-ADDR-TYPE   PIC X(01).
           15  WK-C-VCFA-REG-ADDR-FORMAT PIC X(01).
           15  WK-C-VCFA-REG-FOREIGN-IND PIC X(01).
           15  WK-C-VCFA-REG-VERIFY-IND  PIC X(01).
           15  WK-C-VCFA-HOLDMAIL-IND    PIC X(01).
           15  WK-C-VCFA-ACCT-ADDR-FORMAT PIC X(01).
           15  WK-C-VCFA-PRI-ID-NUM      PIC X(15).
           15  WK-C-VCFA-PRI-ID-TYPE     PIC X(02).
           15  WK-C-VCFA-PRI-OWN-CTY-CODE PIC X(03).
           15  WK-N-VCFA-PRI-DOB         PIC 9(08).
           15  WK-C-VCFA-PRI-CTIZEN-CTY-CODE PIC X(03).
           15  WK-C-VCFA-ACCT-ADDR1      PIC X(35).
           15  WK-C-VCFA-ACCT-ADDR2      PIC X(35).
           15  WK-C-VCFA-ACCT-ADDR3      PIC X(35).
           15  WK-C-VCFA-ACCT-ADDR4      PIC X(35).
      * end of 626JTC *
