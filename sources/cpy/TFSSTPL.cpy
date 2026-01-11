      * TFSSPTL.cpybk
           05 TFSSPTL-RECORD      PIC X(1499).
              SM1TY1* 05 TFSSPTL-RECORD      PIC X(1504).
      * I-O FORMAT:TFSSPTLR  FROM FILE TFSSPTL   OF LIBRARY COMLIB
      *
           05 TFSSPTLR  REDEFINES TFSSPTL-RECORD.
           06 TFSSPTL-PARALNO        PIC 9(08).
      *                        PARA FILE LAST RUNNING NO
           06 TFSSPTL-SEQNUM         PIC 9(02).
      *                        SEQUENCE NUMBER
           06 TFSSPTL-INTDTE         PIC 9(08).
      *                        INTERFACE DATE
           06 TFSSPTL-INTTIM         PIC 9(08).
      *                        INTERFACE TIME
           06 TFSSPTL-SWFTMGTY       PIC 9(03).
      *                        SWIFT MSG TYP
           06 TFSSPTL-IMSGTYPE       PIC X(01).
      *                        INWARD MESSAGE TYPE
           06 TFSSPTL-TRNSMTMD       PIC X.
      *                        TRANSMIT MTD
SM1TY1     06 TFSSPTL-SSNO           PIC X(04).
              SM1TY1*  06 TFSSPTL-OSNNO          PIC X(06).
      *                        OUT NO
           06 TFSSPTL-ISNNO          PIC 9(06).
      *                        IN NO
           06 TFSSPTL-PROCUNIT       PIC X(03).
      *                        PROCESSING UNIT
           06 TFSSPTL-BNKENTITY      PIC X(02).
      *                        BANK ENTITY NO.
           06 TFSSPTL-SENBNKID       PIC X(11).
      *                        SEND BANK ID
           06 TFSSPTL-PRCIND         PIC X.
      *                        PROCESS IND
           06 TFSSPTL-STRIND         PIC X.
      *                        STP IND
           06 TFSSPTL-CUYCD          PIC X(03).
      *                        CURRENCY CODE
           06 TFSSPTL-AMT            PIC S9(15)V9(2) COMP-3.
      *                        AMOUNT
           06 TFSSPTL-LCAMT          PIC S9(15)V9(2) COMP-3.
      *                        LOCAL EQUIVALENT AMOUNT
           06 TFSSPTL-TAG53.
           08 TFSSPTL-TAG53-OPT   PIC X(04).
           08 TFSSPTL-TAG53-VAL.
           10 TFSSPTL-TAG53-V1.
           12 TFSSPTL-TAG53-V1A  PIC X(02).
           12 TFSSPTL-TAG53-V1B  PIC X(35).
           10 TFSSPTL-TAG53-V2  PIC X(35) OCCURS 4 TIMES.
      *                        TAG 53 OPTION
           06 TFSSPTL-TAG54.
           08 TFSSPTL-TAG54-OPT   PIC X(04).
           08 TFSSPTL-TAG54-VAL.
           10 TFSSPTL-TAG54-V1.

      * TAG 54 OPTION
           06 TFSSPTL-TAG56.
           08 TFSSPTL-TAG56-OPT  PIC X(04).
           08 TFSSPTL-TAG56-VAL.
           10 TFSSPTL-TAG56-V1.
           12 TFSSPTL-TAG56-V1A PIC X(02).
           12 TFSSPTL-TAG56-V1B PIC X(35).
           10 TFSSPTL-TAG56-V2 PIC X(35) OCCURS 4 TIMES.
      * TAG 56 OPTION
           06 TFSSPTL-TAG57.
           08 TFSSPTL-TAG57-OPT  PIC X(04).
           08 TFSSPTL-TAG57-VAL.
           10 TFSSPTL-TAG57-V1.
           12 TFSSPTL-TAG57-V1A PIC X(02).
           12 TFSSPTL-TAG57-V1B PIC X(35).
           10 TFSSPTL-TAG57-V2 PIC X(35) OCCURS 4 TIMES.
      * TAG 57 OPTION
           06 TFSSPTL-TAG58.
           08 TFSSPTL-TAG58-OPT  PIC X(04).
           08 TFSSPTL-TAG58-VAL.
           10 TFSSPTL-TAG58-V1.
           12 TFSSPTL-TAG58-V1A PIC X(02).
           12 TFSSPTL-TAG58-V1B PIC X(35).
           10 TFSSPTL-TAG58-V2 PIC X(35) OCCURS 4 TIMES.
      * TAG 58 OPTION
           06 TFSSPTL-TAG59.
           08 TFSSPTL-TAG59-OPT  PIC X(03).
           08 TFSSPTL-TAG59-VAL.
           10 TFSSPTL-TAG59-V1 PIC X(35).
           10 TFSSPTL-TAG59-V2 PIC X(35) OCCURS 4 TIMES.
           08 TFSSPTL-TAG59-OPCH PIC X(01).
           08 TFSSPTL-TAG59-FILL PIC X(02).
      * TAG 59 OPTION
           06 TFSSPTL-DATAAREA.
           08 TFSSPTL-DATAREA1   PIC X(20).
      * TABLE A IND
           08 TFSSPTL-DATAREB1   PIC X(20).
      * TABLE B1 IND
           08 TFSSPTL-DATAREB2   PIC X(20).
      * TABLE B2 IND
           08 TFSSPTL-DATAREB3   PIC X(20).
      * TABLE B3 IND
           08 TFSSPTL-DATAREC1   PIC X(20).
      * TABLE C IND
           08 TFSSPTL-DATARED1   PIC X(20).
      * TABLE D1 IND
           08 TFSSPTL-DATARED2   PIC X(20).
      * TABLE D2 IND
           08 TFSSPTL-DATAREE1   PIC X(20).
      * TABLE E1 IND
           08 TFSSPTL-DATAREE2   PIC X(20).

      *      TABLE E2 IND
           08 TFSSPTL-DATAREE3    PIC X(20).
      *      TABLE E3 IND
           08 TFSSPTL-DATARF1A    PIC X(20).
      *      TABLE F1A IND
           08 TFSSPTL-DATARF1B    PIC X(20).
      *      TABLE F1B IND
           08 TFSSPTL-DATARF2     PIC X(20).
      *      TABLE F2 IND
           06 TFSSPTL-ACTA.
           08 TFSSPTL-ACTA1       PIC X.
      *      ACTION INDICATOR A1
           08 TFSSPTL-ACTA2       PIC X.
      *      ACTION INDICATOR A2
           06 TFSSPTL-ACTB1.
           08 TFSSPTL-ACTB11      PIC X.
      *      ACTION INDICATOR B11
           08 TFSSPTL-ACTB12      PIC X.
      *      ACTION INDICATOR B12
           08 TFSSPTL-ACTB13      PIC X.
      *      ACTION INDICATOR B13
           08 TFSSPTL-ACTB14      PIC X.
      *      ACTION INDICATOR B14
           08 TFSSPTL-ACTB15      PIC X.
      *      ACTION INDICATOR B15
           08 TFSSPTL-ACTB16      PIC X.
      *      ACTION INDICATOR B16
           06 TFSSPTL-ACTB2.
           08 TFSSPTL-ACTB21      PIC X.
      *      ACTION INDICATOR B21
           08 TFSSPTL-ACTB22      PIC X.
      *      ACTION INDICATOR B22
           08 TFSSPTL-ACTB23      PIC X.
      *      ACTION INDICATOR B23
           06 TFSSPTL-ACTB3.
           08 TFSSPTL-ACTB31      PIC X.
      *      ACTION INDICATOR B31
           08 TFSSPTL-ACTB32      PIC X.
      *      ACTION INDICATOR B32
           08 TFSSPTL-ACTB33      PIC X.
      *      ACTION INDICATOR B33
           08 TFSSPTL-ACTB34      PIC X.
      *      ACTION INDICATOR B34
           06 TFSSPTL-ACTD1.
           08 TFSSPTL-ACTD11      PIC X.
      *      ACTION INDICATOR D11
           08 TFSSPTL-ACTD12      PIC X.
      *      ACTION INDICATOR D12
           06 TFSSPTL-ACTE1.
           08 TFSSPTL-ACTE11      PIC X.
      *      ACTION INDICATOR E11
           08 TFSSPTL-ACTE12      PIC X.
      *      ACTION INDICATOR E12
           08 TFSSPTL-ACTE13      PIC X.

      *      ACTION INDICATOR E13
           06 TFSSTPL-ACTE2       PIC X.
      *      ACTION INDICATOR E2
           06 TFSSTPL-ACTE3       PIC X.
      *      ACTION INDICATOR E3
           06 TFSSTPL-ERRT72      PIC X.
      *      TAG72 ERROR INDICATOR
           06 TFSSTPL-ERRCDT      PIC X.
      *      DATE ERROR INDICATOR
           06 TFSSTPL-ERRCCY      PIC X.
      *      CURRENCY ERROR INDICATOR
           06 TFSSTPL-ERRAMT      PIC X.
      *      AMOUNT ERROR INDICATOR
           06 TFSSTPL-ERRTMD      PIC X.
      *      TELEX INDICATOR
           06 TFSSTPL-IRSWERR     PIC X.
      *      IRSWAMT ERROR
           06 TFSSTPL-FUNCTID     PIC X(08).
      *      FUNCTION ID
           06 TFSSTPL-FFUNCTID    PIC X(08).
      *      FINAL FUNCTION ID
           06 TFSSTPL-STPTYP      PIC X(05).
      *      STP TYPE
           06 TFSSTPL-FSTPTYP     PIC X(05).
      *      FINAL STP TYPE
           06 TFSSTPL-DRMMODE     PIC X(08).
      *      DEBIT PAYMENT MODE
           06 TFSSTPL-CRMODE      PIC X(08).
      *      CREDIT PAYMENT MODE
           06 TFSSTPL-FXRATETY    PIC X(02).
      *      CUSTOMER FX RATE
