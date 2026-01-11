      * VCCA.cpybk
      *=================================================================
      * HISTORY OF MODIFICATION:
      *=================================================================
      * 626JTC - DEE JANE - 07/05/2007 - added fields starting from
      *       WK-C-VCCA-SEG-CODE upto
      *       WK-C-VCCA-ACCT-ADDR4.
      *-----------------------------------------------------------------
      * GHRMBA - MBAVILES - 23/06/2003 - INCLUDE CURRENCY CODE
      *-----------------------------------------------------------------
      * GH1NVB - NVBUOT - 02/10/2002 - GLOBAL HUBBING.
      *       1. CHANGE WK-N-VCCA-DOMBRCH
      *          S9(3) TO X(4).
      *       2. CHANGE WK-C-VCCA-CA-NO
      *          X(11) TO X(15).
      *-----------------------------------------------------------------
      *--------- COPYBOOK FOR CALLING TRFVCCA - 12/10/89 ---------*

       01  WK-C-VCCA-RECORD.
           05  WK-C-VCCA-INPUT.
              GH1NVB*       10  WK-C-VCCA-CA-NO         PIC X(11).
GH1NVB     10  WK-C-VCCA-CA-NO         PIC X(15).
GHRMBA     10  WK-C-VCCA-CA-CUY        PIC X(03).
           05  WK-C-VCCA-OUTPUT.
           10  WK-C-VCCA-INVALID-OUTPUT.
           15  WK-C-VCCA-ERROR-CD PIC X(07).
           15  WK-C-VCCA-COM0206.
           20  WK-C-VCCA-FILE PIC X(08).
           20  WK-C-VCCA-MODE PIC X(06).
           20  WK-C-VCCA-KEY  PIC X(20).
           20  WK-C-VCCA-FS   PIC X(02).
           10  WK-C-VCCA-VALID-OUTPUT.
           15  WK-C-VCCA-CUSTFNAM PIC X(35).
           15  WK-C-VCCA-ADDR1   PIC X(35).
           15  WK-C-VCCA-ADDR2   PIC X(35).
           15  WK-C-VCCA-ADDR3   PIC X(35).
           15  WK-C-VCCA-ADDR4   PIC X(35).
           15  WK-C-VCCA-ADDR5   PIC X(35).
           15  WK-C-VCCA-ADDR6   PIC X(35).
           15  WK-N-VCCA-RESCD   PIC S9(02).
           15  WK-N-VCCA-HOLDCD1 PIC S9(02).
           15  WK-N-VCCA-HOLDCD2 PIC S9(02).
           15  WK-N-VCCA-HOLDCD3 PIC S9(02).
           15  WK-C-VCCA-AOCD    PIC X(04).
           15  WK-C-VCCA-STAFFIND PIC S9(02).
           15  WK-N-VCCA-DOMBRCH PIC S9(03).
              GH1NVB*            15  WK-N-VCCA-DOMBRCH PIC X(04).
GH1NVB     15  WK-N-VCCA-AVAIL-BAL PIC S9(13)V99.
           15  WK-N-VCCA-CURR-BAL PIC S9(13)V99.
      * start of 626JTC *
           15  WK-C-VCCA-SEG-CODE PIC X(01).
           15  WK-C-VCCA-CTO-CODE PIC X(10).
           15  WK-C-VCCA-REG-ADDR1 PIC X(35).
           15  WK-C-VCCA-REG-ADDR2 PIC X(35).

           15  WK-C-VCCA-REG-ADDR3         PIC X(35).
           15  WK-C-VCCA-REG-ADDR4         PIC X(35).
           15  WK-C-VCCA-REG-BLK           PIC X(07).
           15  WK-C-VCCA-REG-STOREY        PIC X(04).
           15  WK-C-VCCA-REG-UNIT          PIC X(07).
           15  WK-C-VCCA-REG-PO-BOX        PIC X(06).
           15  WK-C-VCCA-REG-BUILD         PIC X(45).
           15  WK-C-VCCA-REG-STREET        PIC X(32).
           15  WK-C-VCCA-REG-STATE         PIC X(20).
           15  WK-N-VCCA-REG-POSTAL        PIC 9(09).
           15  WK-C-VCCA-REG-CTY-CODE      PIC X(03).
           15  WK-C-VCCA-REG-ADDR-TYPE     PIC X(01).
           15  WK-C-VCCA-REG-ADDR-FORMAT   PIC X(01).
           15  WK-C-VCCA-REG-FOREIGN-IND   PIC X(01).
           15  WK-C-VCCA-REG-VERIFY-IND    PIC X(01).
           15  WK-C-VCCA-HOLDMAIL-IND      PIC X(01).
           15  WK-C-VCCA-ACCT-ADDR-FORMAT  PIC X(01).
           15  WK-C-VCCA-PRI-ID-NUM        PIC X(15).
           15  WK-C-VCCA-PRI-ID-TYPE       PIC X(02).
           15  WK-C-VCCA-PRI-OWN-CTY-CODE  PIC X(03).
           15  WK-N-VCCA-PRI-DOB           PIC 9(08).
           15  WK-C-VCCA-PRI-CTIZEN-CTY-CODE PIC X(03).
           15  WK-C-VCCA-ACCT-ADDR1        PIC X(35).
           15  WK-C-VCCA-ACCT-ADDR2        PIC X(35).
           15  WK-C-VCCA-ACCT-ADDR3        PIC X(35).
           15  WK-C-VCCA-ACCT-ADDR4        PIC X(35).
      * end of 626JTC *
