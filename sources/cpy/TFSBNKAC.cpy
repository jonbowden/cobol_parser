      * TFSBNKAC.cpybk
      ******************************************************************
      * HISTORY OF MODIFICATION:
      ******************************************************************
      * GH1NVB - NVBUOT - 03/10/2002 - GLOBAL HUBBING.
      * 1. CHANGE TFSBNKAC-BNKENTTY
      * S9(1) TO X(2)
      * 2. CHANGE TFSBNKAC-BNKACNO
      * X(11) TO X(15)
           -------------------------------------------------------------
      -    -----
           05 TFSBNKAC-RECORD          PIC X(76).
           05 TFSBNKAC-RECORD          PIC X(81).
      * I-O FORMAT:TFSBNKACR FROM FILE TFSBNKAC OF LIBRARY COMDB
      * bank account number record
           05 TFSBNKACR                REDEFINES TFSBNKAC-RECORD.
           06 TFSBNKAC-BNKENTTY    PIC S9(1).
           06 TFSBNKAC-BNKENTTY    PIC X(2).
      * bank entity
           06 TFSBNKAC-BANKID      PIC X(11).
      * bank id
           06 TFSBNKAC-CUYCD       PIC X(3).
      * currency code
           06 TFSBNKAC-PRIORITY    PIC S9(2).
      * priority
           06 TFSBNKAC-BNKACNO     PIC X(11).
           06 TFSBNKAC-BNKACNO     PIC X(15).
      * bank account no
           06 TFSBNKAC-ACUDBUI     PIC X(1).
      * acu dbu indicator
           06 TFSBNKAC-ACCTYP      PIC X(1).
      * account type
           06 TFSBNKAC-AGTCOMIS    PIC S9(13)V9(2).
      * agent commission
           06 TFSBNKAC-ADVAMT      PIC S9(13)V9(2).
      * MT100 advice amount
           06 TFSBNKAC-SETUPDTE    PIC S9(8).
      * set up date
           06 TFSBNKAC-LSTUPDTE    PIC S9(8).
      * last update date
