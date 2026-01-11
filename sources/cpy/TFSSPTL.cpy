       05  TFSSPTL-RECORD               PIC X(1499).
SM1TY1*  05  TFSSPTL-RECORD               PIC X(1504).
      *  I-O FORMAT:TFSSPTLR  FROM FILE TFSSPTL   OF LIBRARY COMLIB
      *
           05  TFSSPTLR  REDEFINES TFSSPTL-RECORD.
               06  TFSSPTL-PARALNO       PIC 9(08).
      *                                PARA FILE LAST RUNNING NO
               06  TFSSPTL-SEQNUM        PIC 9(02).
      *                                SEQUENCE NUMBER
               06  TFSSPTL-INTDTE        PIC 9(08).
      *                                INTERFACE DATE
               06  TFSSPTL-INTTIM        PIC 9(08).
      *                                INTERFACE TIME
               06  TFSSPTL-SWFMTGTY      PIC 9(03).
      *                                SWIFT MSG TYP
               06  TFSSPTL-IMSGTYPE      PIC X(01).
      *                                INWARD MESSAGE TYPE
               06  TFSSPTL-TRNSMTMD      PIC X.
      *                                TRANSMIT MTD
SM1TY1    06  TFSSPTL-SSNO          PIC X(04).
SM1TY1*   06  TFSSPTL-OSNNO         PIC X(06).
      *                                SESSION NO
      *                                OUT NO
               06  TFSSPTL-ISNNO         PIC 9(06).
      *                                IN NO
               06  TFSSPTL-PROCUNIT      PIC X(03).
      *                                PROCESSING UNIT
               06  TFSSPTL-BNKENTITY     PIC X(02).
      *                                BANK ENTITY NO.
               06  TFSSPTL-SENBNKID      PIC X(11).
      *                                SEND BANK ID
               06  TFSSPTL-PRCIND        PIC X.
      *                                PROCESS IND
               06  TFSSPTL-STRIND        PIC X.
      *                                STP IND
               06  TFSSPTL-CUYCD         PIC X(03).
      *                                CURRENCY CODE
               06  TFSSPTL-AMT           PIC S9(15)V9(2) COMP-3.
      *                                AMOUNT
               06  TFSSPTL-LCAMT         PIC S9(15)V9(2) COMP-3.
      *                                LOCAL EQUIVALENT AMOUNT
               06  TFSSPTL-TAG53.
                   08  TFSSPTL-TAG53-OPT PIC X(04).
                   08  TFSSPTL-TAG53-VAL.
                       10  TFSSPTL-TAG53-V1.
                           12  TFSSPTL-TAG53-V1A PIC X(02).
                           12  TFSSPTL-TAG53-V1B PIC X(35).
                       10  TFSSPTL-TAG53-V2 PIC X(35) OCCURS 4 TIMES.
      *                                TAG 53 OPTION
               06  TFSSPTL-TAG54.
                   08  TFSSPTL-TAG54-OPT PIC X(04).
                   08  TFSSPTL-TAG54-VAL.
                       10  TFSSPTL-TAG54-V1.