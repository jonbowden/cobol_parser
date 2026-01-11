       IDENTIFICATION DIVISION.
       PROGRAM-ID. SWIFTMER.
      *      WORKING STORAGE FORMATS FOR MSGBODY WITH SWIFT MT101 FOR
      *      MERCURIA. IT WILL BE USED IN INTERFACE PROGRAM
      *****************************************************************
      * MODIFICATION HISTORY                                          *
      *****************************************************************
      * TAG   DATE    DEV   DESCRIPTION                               *
      *------ ------- ------ ---------------------------------------- *
      * GQ2RV1 06/04/16 TMPRVD - REM 2016 Q2 RELEASE                  *
      *                      - E-REQUEST# 46332                       *
      *                      - MERCURIA PHASE II                      *
      *                      - Expanded WK-C-MSGBDY1 from             *
      *                        4000 to 10000.                         *
      *---------------------------------------------------------------*
      * GQ2JM1 01/02/16 TMPJP6 - REM 2016 Q2 RELEASE                  *
      *                      - E-REQUEST# 46332                       *
      *                      - MERCURIA PHASE II                      *
      *                      - Expand to have 2 instances of tag      *
      *                        IDS on Sequence B.                     *
      *---------------------------------------------------------------*
      * GQ2JM1 01/02/16 TMPJP6 - REM 2016 Q2 RELEASE                  *
      *                      - E-REQUEST# 46332                       *
      *                      - MERCURIA PHASE II                      *
      *                      - INITIAL VERSION                        *
      *****************************************************************
       01 WK-C-SWIFTMER.
           05 WK-C-MSGBDY1           PIC X(4000).
           05 WK-C-MSGBDY1           PIC X(10000).
           05 WK-C-INF-101 REDEFINES WK-C-MSGBDY1.
      * TAG 20 - MANDATORY
           10 INF-101-TAG20-G.
           15 INF-101-TAG20          PIC X(02).
           15 INF-101-TAG20-COL      PIC X(01).
           10 INF-101-SENDERS-REF    PIC X(16).
      * TAG 21R - OPTIONAL
           10 INF-101-TAG21R-G.
           15 INF-101-TAG21R         PIC X(03).
           15 INF-101-TAG21R-COL     PIC X(01).
           10 INF-101-CUST-SPEC-REF  PIC X(16).
      * TAG 28D - MANDATORY
           10 INF-101-TAG28D-G.
           15 INF-101-TAG28D         PIC X(03).
           15 INF-101-TAG28D-COL     PIC X(01).
           10 INF-101-MSG-IDXTOT.
           15 INF-101-MSG-IDX        PIC 9(05).
           15 INF-101-MSG-SLASH      PIC X(01).
           15 INF-101-MSG-TOT        PIC 9(05).
      * TAG 50A - OPTIONAL (INSTRUCTING PARTY S1)
           10 INF-101-TAG50-S1-IC-G.
           15 INF-101-TAG50-S1-IC    PIC X(02).
           15 INF-101-TAG50-S1-IC-OPT PIC X(01).
           15 INF-101-TAG50-S1-IC-COL PIC X(01).
           10 INF-101-INST-PARTY-S1  PIC X(35).
      * TAG 50A - OPTIONAL (ORDERING CUSTOMER S1)
           10 INF-101-TAG50-S1-OC-G.
           15 INF-101-TAG50-S1-OC    PIC X(02).
           15 INF-101-TAG50-S1-OC-OPT PIC X(01).
           15 INF-101-TAG50-S1-OC-COL PIC X(01).
           10 INF-101-ORD-CUST-DTL-S1.
           15 INF-101-S1-ORD-CUST1   PIC X(35).
           15 INF-101-S1-ORD-CUST2   PIC X(35) OCCURS 4.

      * TAG 52A - OPTIONAL S1
           10 INF-101-TAG52-S1-G.
           15 INF-101-TAG52-S1        PIC X(02).
           15 INF-101-TAG52-S1-OPT    PIC X(01).
           15 INF-101-TAG52-S1-COL    PIC X(01).
           10 INF-101-ACCT-SRV-INST-S1.
           15 INF-101-S1-ACCT-SRV-INST1 PIC X(37).
           15 INF-101-S1-ACCT-SRV-INST2 PIC X(35).
      * TAG 51A - OPTIONAL
           10 INF-101-TAG51A-G.
           15 INF-101-TAG51A-NO       PIC X(03).
           15 INF-101-TAG51A-COL      PIC X(01).
           10 INF-101-SNDINST.
           15 INF-101-SNDINST-L1      PIC X(37).
           15 INF-101-SNDINST-L2      PIC X(35).
      * TAG 30 - MANDATORY
           10 INF-101-TAG30-G.
           15 INF-101-TAG30-NO        PIC X(02).
           15 INF-101-TAG30-COL       PIC X(01).
           10 INF-101-REQ-EXDTE           PIC 9(06).
      * TAG 25 - OPTIONAL
           10 INF-101-TAG25-G.
           15 INF-101-TAG25-NO        PIC X(02).
           15 INF-101-TAG25-COL       PIC X(01).
           10 INF-101-AUTHORISATION       PIC X(35).
      * TAG 21 - MANDATORY
           10 INF-101-TAG21-G.
           15 INF-101-TAG21-NO        PIC X(02).
           15 INF-101-TAG21-COL       PIC X(01).
           10 INF-101-TRAN-REF            PIC X(16).
      * TAG 21F - OPTIONAL
           10 INF-101-TAG21F-G.
           15 INF-101-TAG21F-NO       PIC X(03).
           15 INF-101-TAG21F-COL      PIC X(01).
           10 INF-101-FX-DEAL-REF         PIC X(16).
      * TAG 23B - OPTIONAL
           10 INF-101-TAG23B-G.
           15 INF-101-TAG23B-NO       PIC X(03).
           15 INF-101-TAG23B-COL      PIC X(01).
           10 INF-101-INS-CODE.
           15 INF-101-INS-CODE1       PIC X(04).
           15 INF-101-INS-CODE2       PIC X(30).
      * TAG 32B - MANDATORY
           10 INF-101-TAG32B-G.
           15 INF-101-TAG32B-NO       PIC X(03).
           15 INF-101-TAG32B-COL      PIC X(01).
           10 INF-101-TXN-CCY-AMT.
           15 INF-101-TXNCCY          PIC X(03).
           15 INF-101-TXNAMT          PIC X(15).
      * TAG 50A - OPTIONAL (INSTRUCTING PARTY S2)
           10 INF-101-TAG50-S2-IC-G.
           15 INF-101-TAG50-S2-IC     PIC X(02).
           15 INF-101-TAG50-S2-IC-OPT PIC X(01).
           15 INF-101-TAG50-S2-IC-COL PIC X(01).

      * TAG 50A - OPTIONAL (ORDERING CUSTOMER S2)
           10 INF-101-TAG50-S2-OC-G.
           15 INF-101-TAG50-S2-OC       PIC X(02).
           15 INF-101-TAG50-S2-OC-OPT   PIC X(01).
           15 INF-101-TAG50-S2-OC-COL   PIC X(01).
           10 INF-101-ORD-CUST-DTL-S2.
           15 INF-101-S2-ORD-CUST1      PIC X(35).
           15 INF-101-S2-ORD-CUST2      PIC X(35) OCCURS 4.
      * TAG 52A - OPTIONAL S2
           10 INF-101-TAG52-S2-G.
           15 INF-101-TAG52-S2          PIC X(02).
           15 INF-101-TAG52-S2-OPT      PIC X(01).
           15 INF-101-TAG52-S2-COL      PIC X(01).
           10 INF-101-ACCT-SRV-INST-S2.
           15 INF-101-S2-ACCT-SRV-INST1 PIC X(37).
           15 INF-101-S2-ACCT-SRV-INST2 PIC X(35).
      * TAG 56A - OPTIONAL
           10 INF-101-TAG56-G.
           15 INF-101-TAG56             PIC X(02).
           15 INF-101-TAG56-OPT         PIC X(01).
           15 INF-101-TAG56-COL         PIC X(01).
           10 INF-101-INTERMEDIARY.
           15 INF-INTERMEDIARY-1        PIC X(37).
           15 INF-INTERMEDIARY-2        PIC X(35) OCCURS 4.
      * TAG 57A - OPTIONAL
           10 INF-101-TAG57-G.
           15 INF-101-TAG57             PIC X(02).
           15 INF-101-TAG57-OPT         PIC X(01).
           15 INF-101-TAG57-COL         PIC X(01).
           10 INF-101-ACCT-WT-INST.
           15 INF-ACCT-WT-INST-1        PIC X(37).
           15 INF-ACCT-WT-INST-2        PIC X(35) OCCURS 4.
      * TAG 59A - MANDATORY
           10 INF-101-TAG59-G.
           15 INF-101-TAG59             PIC X(2).
           15 INF-101-TAG59-OPT         PIC X(1).
           15 INF-101-TAG59-COL         PIC X(1).
           10 INF-101-BENE.
           15 INF-101-BENE-CUST         PIC X(35) OCCURS 5.
      * TAG 70 - OPTIONAL
           10 INF-101-TAG70-G.
           15 INF-101-TAG70             PIC X(2).
           15 INF-101-TAG70-COL         PIC X(1).
           10 INF-101-REMINFO.
           15 INF-101-REM-INFO          PIC X(35) OCCURS 4.
      * TAG 77B - OPTIONAL
           10 INF-101-TAG77B-G.
           15 INF-101-TAG77B-1          PIC X(2).
           15 INF-101-TAG77B-OPTION     PIC X(1).
           15 INF-101-TAG77B-COL        PIC X(1).
           10 INF-101-REGURPT.
           15 INF-101-REGURPT1          PIC X(35).
           15 INF-101-REGURPT2          PIC X(35).
           15 INF-101-REGURPT3          PIC X(35).
      * TAG 33B - OPTIONAL
           10 INF-101-TAG33B-G.
           15 INF-101-TAG33B-NO         PIC X(03).

           15  INF-101-TAG33B-COL            PIC X(01).
           10  INF-101-ORG-CCY-AMT.
           15  INF-101-ORGCCY                PIC X(03).
           15  INF-101-ORGAMT                PIC X(15).

      *    TAG 71A - MANDATORY
           10  INF-101-TAG71A-G.
           15  INF-101-TAG71A.
           20  INF-101-TAG71A-1             PIC X(2).
           20  INF-101-TAG71A-OPTION        PIC X(1).
           15  INF-101-TAG71A-COL           PIC X(1).
           10  INF-101-DETL-OF-CHRGS             PIC X(210).

      *    TAG 25A - OPTIONAL
           10  INF-101-TAG25-G.
           15  INF-101-TAG25                PIC X(02).
           15  INF-101-TAG25-OPT            PIC X(01).
           15  INF-101-TAG50-COL            PIC X(01).
           10  INF-101-CHARGES-ACCT              PIC X(35).

      *    TAG 36 - OPTIONAL
           10  INF-101-TAG36-G.
           15  INF-101-TAG36-NO             PIC X(2).
           15  INF-101-TAG36-COL            PIC X(1).
           10  INF-101-EXCHRATE                  PIC X(12).

      *    TAG 21 - MANDATORY
           10  INF-101-TAG21-G2.
           15  INF-101-TAG21-NO2            PIC X(02).
           15  INF-101-TAG21-COL2           PIC X(01).
           10  INF-101-TRAN-REF2                 PIC X(16).
