       IDENTIFICATION DIVISION.
      **********************
       PROGRAM-ID.     TRFVTF1B.
       AUTHOR.         TYK.
       DATE-WRITTEN.   JUN 04.
       
      *DESCRIPTION :   TABLE F1B VALIDATION.
      *                SUBROUTINE - CREDIT BENE CHECKING FIELD 58/59 FOR
      *                INCOMING SWIFT LCY
      *
      *================================================================
      * HISTORY OF MODIFICATION:
      *================================================================
      * VASA03 - VENUFQ    - 12/08/2025 - PROJ#JW02 MYBN SG VASA
      *                                  - Added close for TFSICLSA2
      *----------------------------------------------------------------
      * VASA02 - VENUFQ    - 30/07/2025 - PROJ#JW02 MYBN SG VASA
      *                                  - Add bene name matching using
      *                                    MA name of VASA subaccount
      *                                  - Add checking from A/C
      *                                    re-routing table
      *                                  - Add checking from name
      *                                    variation table
      *----------------------------------------------------------------
      * VASA01 - VENUFQ    - 16/06/2025 - PROJ#JW02 MYBN SG VASA
      *                                  - Add beneficiary name matching
      *                                    using subaccount name
      *----------------------------------------------------------------
      * OFX2A3 - VENSIC    - 23/02/2021 - PROJ#BW24 - GEBNG ONLINE FX R2
      *                                  - JIRA GEBNEXTGEN-123653
      *                                  - Store XMS converted amount in
      *                                    local data area
      *----------------------------------------------------------------
      * OFX2A2 - VENSIC    - 11/01/2021 - PROJ#BW24 - GEBNG ONLINE FX R2
      *                                  - JIRA GEBNEXTGEN-112050
      *                                  - Rectify logic to check
      *                                    debiting account segment for
      *                                    MT101 transaction
      *----------------------------------------------------------------
      * OFX2A1 - VENSIC    - 10/10/2020 - PROJ#BW24 - GEBNG ONLINE FX R2
      *                                  - JIRA GEBNEXTGEN-82761
      *                                  - Perform XMS enquiry before
      *                                    calling TRFVTF2 to check on
      *                                    rate limit, rate type etc
      *                                  - XMS rate and "OF" rate type
      *                                    will be stored into local data
      *                                    area after successful enquiry
      *----------------------------------------------------------------
      * GP3M02 - VENTEH    - 08/10/2020 - CASH MANAGEMENT ROAD MAP
      *                                  - P19 SWIFT GPI DAY 4 POST PI
      *                                  - PCRMAPKGPI-2364
      *                                  - Bypass STP limit for:
      *                                    MT202 Ddebit NOSTRO Credit
      *                                    VOSTRO without T53/54
      *----------------------------------------------------------------
      * GP3M01 - VENADG    - 18/03/2020 - CASH MANAGEMENT ROAD MAP
      *                                  - P19 SWIFT GPI DAY 3
      *                                  - PCRMAPKGPI-1331
      *                                  - To add "MAS" Mode pay on
      *                                    the evaluation of DR NOSTRO.
      *                                  - "MAS" PMODE will be passed from
      *                                    TRFVTD1 only when:
      *                                    1. Incoming MEP5
      *                                    2. Doesnt have Tag53/54
      *                                    3. Sending BIC is MEP5
      *----------------------------------------------------------------
      * GP3C01 - ACNESQ    - 06/12/2019 - CASH MANAGEMENT ROAD MAP
      *                                  - P19 SWIFT GPI DAY 3
      *                                  - PCRMAPKGPI-1050
      *                                    Ensure only line 1 and 2 of
      *                                    Tag 57 C/D are being validated
      *                                    against Tag57 Validation Table
      *----------------------------------------------------------------
      * GP3K01 - ACNMIB    - 05/12/2019 - CASH MANAGEMENT ROAD MAP
      *                                  - P19 SWIFT GPI DAY 3
      *                                  - PCRMAPKGPI-1041 (Enh)
      *                                  - Txn with tag 59 opt = F
      *                                    will be routed to repair when
      *                                    Tag 59F validation tech switch
      *                                    is OFF.
      *                                  - Ensure WK-C-TAG59F-SW is used
      *                                    for all tag 59F changes.
      *----------------------------------------------------------------
      * GP3M00 - ACNESQ    - 29/10/2019 - CASH MANAGEMENT ROAD MAP
      *                                  - P19 SWIFT GPI DAY 3
      *                                  - Inward serial payment Bypass
      *                                    STP Limit for Nostro (Item5a)
      *                                  - MT103: Bypass STP Limit if
      *                                    Dr Leg = NOSTRO, CR Leg = CASA
      *                                    and both Tag 53/54 not present
      *                                  - MT202/C: Do not Bypass STP Limit
      *                                    if Dr Leg = VOSTRO, CR Leg = CASA
      *----------------------------------------------------------------
      * GP3C00 - VENADG    - 14/08/2019 - CASH MANAGEMENT ROAD MAP
      *                                  - P19 SWIFT GPI DAY 3
      *                                  - Tag57 Enhancement (Item5b)
      *                                  - To check Tag57 C/D Lines 1-5
      *                                    againts Tag Validation Table.
      *                                    This is to enable such tags to
      *                                    further proceed with STP processing
      *                                    if exact matches.
      * GP3K00 - VENADG    - 20/08/2019 - Tag 59F Enhancement (Structured
      *                                    Beneficiary details)
      *                                  - To enable handling of Tag59F
      *                                    Structured format.
      *----------------------------------------------------------------
      * SGX206 - ACNGOC    - 09/09/2019 - SGX PTS2 PHASE 2
      *                                  - JIRA PSGXPTSONE-762
      *                                    Initialize WS-SUB
      *----------------------------------------------------------------
      * SGX205 - ACNDOA    - 05/09/2019 - SGX PTS2 PHASE 2
      *                                  - JIRA PSGXPTSONE-760
      *                                    GL account validation will only
      *                                    be applicable for tag59 with
      *                                    prefix of "G" and with succeeding
      *                                    less than or equal 8 numeric
      *                                    bytes
      *----------------------------------------------------------------
      * GPI202 - ACNRJR    - 20/06/2019 - CASH MANAGEMENT ROAD MAP
      *                                    P19 SWIFT GPI DAY 2B
      *                                  - PCRMAPKGPI-786
      *                                  - Previously when txn encounter
      *                                    Bene A/C validation error, the
      *                                    STP Type was not populated
      *                                    properly that cause the txn
      *                                    NOT to be routed to 1STP/Repr.
      *                                  - Rectified to handle the scenario
      *                                    and that the STP will be always
      *                                    populated.
      *----------------------------------------------------------------
      * SGX204 - ACNGOC    - 30/05/2019 - SGX PTS2 PHASE 2
      *                                  - JIRA PSGXPTSONE-720
      *                                    Ensure 8-digit GL Accno is
      *                                    being passed to Beneficiary
      *                                    detail.
      *----------------------------------------------------------------
      * SGX203 - ACNESQ    - 28/01/2019 - SGX PTS2 PHASE 2
      *                                    Credit/Debit via GL Account
      *                                  - JIRA PSGXPTSONE-685
      *                                    PSTP/1STP/2STP STP Limit
      *                                    validation should not be
      *                                    bypassed for MT103 credit
      *                                    GL Account.
      *                                  - Considered STP enhancement
      *                                    under P19 SWIFT GPI DAY 2B,
      *                                    where in STP Limit validation
      *                                    is fully bypassed for certain
      *                                    conditions.
      *----------------------------------------------------------------
      * SGX202 - ACNSAT    - 30/04/2019 - SGX PTS2 PHASE 2
      *                                    JIRA PSGXPTSONE-657
      *                                  - Replace Technical switch
      *                                    with multiple values
      *----------------------------------------------------------------
      * GPI201 - ACNDCH    - 04/04/2019 - CASH MANAGEMENT ROAD MAP
      *                                  - P19 SWIFT GPI DAY 2B
      *                                  - PCRMAPKGPI-517
      *                                  - Add call of TRFVBACU to
      *                                    validate if crediting Nostro
      *                                    BIC is a BNK branch. If BNK
      *                                    branch, then bypass STP limit
      *                                  - Bypass if debitting leg is
      *                                    VOSTRO and if cover is already
      *                                    received for 103
      *----------------------------------------------------------------
      * SGX201 - ACNESQ    - 28/01/2019 - SGX PTS2 PHASE 2
      *                                  - Enhancement to cater crediting
      *                                    to GL account for Inward MT103
      *                                    receipt. Following validation
      *                                    will be skipped :
      *                                    + Account Re-routing
      *                                    + STP Limit by CIF/SEG/Accno
      *                                    + Tag59 Benename
      *----------------------------------------------------------------
      * REM269   TMPSRK    - 06/04/2017 - JIRA LOG REM-269
      *                                    STANDARDIZATION OF PROGRAM TO
      *                                    RETRIEVE CURRENCY AND COUNTRY
      *                                    CODE FROM SYSTEM PARAMETER FI LE.
      * 7Q1EM1 - TMPEYM    - 21/10/2016 - REM Q1 2017 RELEASE
      *                                  - e-Req 47511 Refinement  of
      *                                    Duplicate checking for Inw
      *                                  - Passed new field for checking.
      * 7Q1EM2              - 11/11/2016 - Passed value of SWIFT MSGTYP to
      *                                    local data area.
      * 7Q1EM3              - 16/12/2016 - JIRA PQR-15882
      *                                  - Initialize L-C-TRN-NO field
      *                                    before calling TREVDUPL pgm.
      *----------------------------------------------------------------*
      * CMP3A3 - ACNESQ    - 30/09/2016 - CASH MANAGEMENT PROJECT 3
      *                                    USE ORDERING CUSTOMER (TAG 50H)
      *                                    WHEN VALIDATING STP LIMIT
      *                                    FOR MT101
      *================================================================
      * CMP3A2 - CMPESQ    - 05/08/2016 - CASH MANAGEMENT PROJECT 3
      *                                    JIRA: PCSHMGMTS6-190
      *                                    UPDATE STP LIMIT VALIDATION
      *================================================================
      * CMP3A1 - CMPESQ    - 01/07/2016 - CASH MANAGEMENT PROJECT 3
      *                                  - INCLUDE REM INDICATOR
      *================================================================
      * CMP3X1 - CMPESQ    - 14/06/2016 - CASH MANAGEMENT PROJECT RELEASE 3
      * CMP3X2              -            - JIRA: PCSHMGMTS6-109,
      *                                           PCSHMGMTS6-110
      *                                  - FIX STP LIMIT PROCESS
      *================================================================
      * CMP3FL - VENAF2    - 07/01/2016 - CASH MANAGEMENT PROJECT RELEASE 3
      *                                  - STP Limit by Account, CIF
      *                                    and Segment
      *================================================================
      * 5Q2JE2 - TMPJAE    - 13/04/2015 - 14HOREM024 14HOREM028 14HOREM029
      *                                  - NON-STP REASON ENHANCEMENT
      *                                    Correct the repair reason when
      *                                    the STP validation encountered
      *                                    an error in SA/CA/FCCA programs
      *================================================================
      * 5Q2JE1 - TMPJAE    - 25/02/2015 - 14HOREM024 14HOREM028 14HOREM029
      *                                    NON PSTP REASON ENHANCEMENT
      *                                  - PQR-2719/2579
      *                                    Remove line of codes that can
      *                                    cause COM0024-FCCA NO ERROR
      *================================================================
      * 5Q1RD1 - TMPRVD    - 23/01/2015 - 14HOREM024/14HOREM029/14HOREM028
      *                                  - PQR-2577 NON-STP Reason PRJ
      *                                    Add Reason for Unmatched A/C
      *                                    Name
      *================================================================
      * 5Q1LN2 - UNCCNL    - 06/01/2015 - 14HOREM024/14HOREM029/14HOREM028
      *                                    JIRA LOG PQR-2867
      *                                    Set Non-STP reason when WS-OKAY
      *                                    flag is never switched to N or Y
      *================================================================
      * 5Q1ARV - TMPARV    - 10/11/2014 - 14HOREM024/14HOREM029/14HOREM028
      * 5Q1JE1                             Modified to determine the
      * 5Q1LN1                             repair reason and create an
      *                                    entry on the new file (RFTRRSN)
      *================================================================
      * HOJE03 - TMPJAE   - 22/09/2014 - 14HOREM011 RE-ENGINEERING PROJECT
      *                                   TO SORT THE SALUTAION LIST WHEN
      *                                   STORING TO TABLE SO THAT THE SYSTEM
      *                                   CAN REMOVE SALUTATIONS WITH LONGER
      *                                   LENGTH FIRST.
      *----------------------------------------------------------------
      * HOJE02 - TMPJAE   - 25/08/2014 - 14HOREM011 RE-ENGINEERING PROJECT
      *                                   ADDED VALIDATION FOR FCCA ACCOUNT
      *                                   TYPE
      *----------------------------------------------------------------
      * HOJE01 - TMPJAE   - 22/07/2014 - 14HOREM011 RE-ENGINEERING PROJECT
      *                                   ACCOUNT NAME MATCHING
      *----------------------------------------------------------------
      * SMITY1 - TMPTY1   - 09/09/2005 - ADD DUPL MSG CHECKING
      *                  - 08/11/2005 - CORRECT THE VALIDATION FOR NAME
      *                  - 22/11/2005 - SKIP ACC VERIFY WHEN AMT> IRSWAMT
      *----------------------------------------------------------------      
       ENVIRONMENT DIVISION.
      **********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES.   LOCAL-DATA IS LOCAL-DATA-AREA
       	       I-O-FEEDBACK IS I-O-FEEDBACK-AREA
       	       UPSI-0 IS UPSI-SWITCH-0
       		 ON  STATUS IS U0-ON
       		 OFF STATUS IS U0-OFF
       	       UPSI-1 IS UPSI-SWITCH-1
       		 ON  STATUS IS U0-ON
       		 OFF STATUS IS U0-OFF
       	       UPSI-2 IS UPSI-SWITCH-2
       		 ON  STATUS IS U0-ON
       		 OFF STATUS IS U0-OFF
       	       UPSI-3 IS UPSI-SWITCH-3
       		 ON  STATUS IS U0-ON
       		 OFF STATUS IS U0-OFF.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
         SELECT TFSSTPL  ASSIGN TO DATABASE-TFSSTPL
       	 ORGANIZATION      IS INDEXED
       	 ACCESS MODE       IS DYNAMIC
       	 RECORD KEY        IS EXTERNALLY-DESCRIBED-KEY
       	 FILE STATUS       IS WK-C-FILE-STATUS.
       
         SELECT TFSCLSYS ASSIGN TO DATABASE-TFSCLSYS
       	 ORGANIZATION      IS SEQUENTIAL
       	 FILE STATUS       IS WK-C-FILE-STATUS.

GPI201      SELECT UFIMIJCON ASSIGN TO DATABASE-UFIMIJCON       
GPI201             ORGANIZATION      IS INDEXED       
GPI201             ACCESS MODE       IS RANDOM       
GPI201             RECORD KEY        IS EXTERNALLY-DESCRIBED-KEY       
GPI201                                  WITH DUPLICATES       
GPI201             FILE STATUS       IS WK-C-FILE-STATUS.       
       
VASA01      SELECT TFSICLCA2 ASSIGN TO DATABASE-TFSICLCA2       
VASA01             ORGANIZATION    IS INDEXED       
VASA01             ACCESS MODE     IS DYNAMIC       
VASA01             RECORD KEY      IS EXTERNALLY-DESCRIBED-KEY       
VASA01             WITH    DUPLICATES       
VASA01             FILE STATUS     IS WK-C-FILE-STATUS.       
       
VASA01      SELECT TFSICLSA2 ASSIGN TO DATABASE-TFSICLSA2       
VASA01             ORGANIZATION    IS INDEXED       
VASA01             ACCESS MODE     IS DYNAMIC       
VASA01             RECORD KEY      IS EXTERNALLY-DESCRIBED-KEY       
VASA01             WITH    DUPLICATES       
VASA01             FILE STATUS     IS WK-C-FILE-STATUS.       

       DATA DIVISION.
       FILE SECTION.
      **************
       FD  TFSSTPL
       LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFSSTPL.
       01  WK-C-TFSSTPL.
       COPY DDS-ALL-FORMATS OF TFSSTPL.
       01  WK-C-TFSSTPL-1.
       COPY TFSSTPL.

       FD  TFSCLSYS
       LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSCLSYS-REC.
       01  TFSCLSYS-REC.
       COPY DDS-ALL-FORMATS OF TFSCLSYS.
       01  TFSCLSYS-REC-1.
       COPY TFSCLSYS.
       

GPI201 FD  UFIMIJCON       
GPI201     LABEL RECORDS ARE OMITTED       
GPI201     DATA RECORD IS WK-C-UFIMIJCON.       
GPI201 01  WK-C-UFIMIJCON.       
GPI201     COPY DDS-ALL-FORMATS OF UFIMIJCON.       
GPI201 01  UFIMIJCON-REC.       
GPI201     COPY UFIMIJCON.       
       
VASA01 FD  TFSICLCA2       
VASA01     LABEL RECORDS ARE OMITTED       
VASA01     DATA RECORD IS TFSICLCA2-REC.       
VASA01 01  TFSICLCA2-REC.       
VASA01     COPY DDS-ALL-FORMATS OF TFSICLCA2.       
VASA01     COPY ISLCAVQ.       
       
VASA01 FD  TFSICLSA2       
VASA01     LABEL RECORDS ARE OMITTED       
VASA01     DATA RECORD IS TFSICLSA2-REC.       
VASA01 01  TFSICLSA2-REC.       
VASA01     COPY DDS-ALL-FORMATS OF TFSICLSA2.       
VASA01     COPY ISLSAVQ.       

       WORKING-STORAGE SECTION.
      *************************
       01  WK-C-COMMON.
       COPY ASCMWS.
       
CMP3FL 01  WK-C-LINK-LIMIT.       
CMP3FL     05  WK-C-LINK-AREA-INPUT.       
CMP3FL         10  WS-LINK-BNKENTTY            PIC S9(1).       
CMP3FL         10  WS-LINK-ACCNO               PIC X(11) VALUE 0.       
CMP3FL         10  WS-LINK-CCY                 PIC X(03) VALUE SPACES.       
CMP3FL         10  WS-LINK-AMT                 PIC S9(13)V99 VALUE 0.       
CMP3A1         10  WS-LINK-REMIND              PIC X(01).       
CMP3FL     05  WK-C-LINK-AREA-OUTPUT.       
CMP3FL         10  WS-LINK-STATUS              PIC X(02) VALUE SPACES.       
       
       01  TAG57-FORMAT.
       05  TAG57-LINE-1.
       07  TAG57-FIL1                  PIC X(2).
       07  TAG57-OPT                   PIC X(1).
       07  TAG57-FIL2                  PIC X(1).
       07  TAG57-PTID.
         09  TAG57-PTID-1            PIC X(02).
         09  TAG57-PTID-2            PIC X(35).
       05  TAG57-LINE-2                   PIC X(35).
       05  TAG57-BIC  REDEFINES TAG57-LINE-2.
       07  TAG57A-SUB1                 PIC X(4).
       07  TAG57A-SUB2                 PIC X(2).
       07  TAG57A-SUB3                 PIC X(2).
       07  TAG57A-SUB4                 PIC X(3).
       07  TAG57A-FILLER               PIC X(24).
       05  TAG57-LOC  REDEFINES TAG57-LINE-2
       			      PIC X(35).
       05  TAG57-NAME REDEFINES TAG57-LINE-2
       			      PIC X(35).
       05  TAG57-LINE-3                   PIC X(35).
       05  TAG57-LINE-4                   PIC X(35).
       05  TAG57-LINE-5                   PIC X(35).
       
       01  TAG58-FORMAT.
       05  TAG58-LINE-1.
       07  TAG58-FIL1                  PIC X(2).
       07  TAG58-OPT                   PIC X(1).
       07  TAG58-FIL2                  PIC X(1).
       07  TAG58-PTID.
         09  TAG58-PTID-1            PIC X(02).
         09  TAG58-PTID-2            PIC X(35).
       05  TAG58-LINE-2                   PIC X(35).
       05  TAG58-BIC  REDEFINES TAG58-LINE-2.
       07  TAG58-SUB1                  PIC X(4).
       07  TAG58-SUB2                  PIC X(2).
       07  TAG58-SUB3                  PIC X(2).
       07  TAG58-SUB4                  PIC X(3).
       07  TAG58-FILLER                PIC X(24).
       05  TAG58-LOC  REDEFINES TAG58-LINE-2
       			      PIC X(35).
       05  TAG58-NAME REDEFINES TAG58-LINE-2
       			      PIC X(35).
       05  TAG58-LINE-3                   PIC X(35).
       05  TAG58-LINE-4                   PIC X(35).
       05  TAG58-LINE-5                   PIC X(35).
       
       01  TAG59-FORMAT.
       05  TAG59-LINE-1.
       07  TAG59-FIL1                  PIC X(2).
       07  TAG59-FIL2                  PIC X(1).
       07  TAG59-PTID.
         09  TAG59-PTID-1            PIC X(02).
         09  TAG59-PTID-2            PIC X(33).
       05  TAG59-LINE-2                   PIC X(35).
       05  TAG59-BIC  REDEFINES TAG59-LINE-2.
       07  TAG59A-SUB1                 PIC X(4).
       07  TAG59A-SUB2                 PIC X(2).
       07  TAG59A-SUB3                 PIC X(2).
       07  TAG59A-SUB4                 PIC X(3).
       07  TAG59A-FILLER               PIC X(24).
       05  TAG59-LOC  REDEFINES TAG59-LINE-2
       			      PIC X(35).
       05  TAG59-NAME REDEFINES TAG59-LINE-2
       			      PIC X(35).
       05  TAG59-LINE-3                   PIC X(35).
       05  TAG59-LINE-4                   PIC X(35).
       05  TAG59-LINE-5                   PIC X(35).
       05  TAG59-OPT                       PIC X(1).
       05  TAG59-FILLER                    PIC X(2).
       
GP3M00 01  TAG53-FORMAT.       
GP3M00     05  TAG53-LINE-1.       
GP3M00         07  TAG53-FIL1                PIC X(2).       
GP3M00         07  TAG53-OPT                 PIC X(1).       
GP3M00         07  TAG53-FIL2                PIC X(1).       
GP3M00         07  TAG53-PTID.       
GP3M00             09  TAG53-PTID-1          PIC X(02).       
GP3M00             09  TAG53-PTID-2          PIC X(35).       
GP3M00     05  TAG53-LINE-2                 PIC X(35).       
GP3M00     05  TAG53-BIC  REDEFINES TAG53-LINE-2.       
GP3M00         07  TAG53-BIC-SUB1            PIC X(4).       
GP3M00         07  TAG53-BIC-SUB2            PIC X(2).       
GP3M00         07  TAG53-BIC-SUB3            PIC X(2).       
GP3M00         07  TAG53-BIC-SUB4            PIC X(3).       
GP3M00         07  TAG53-BIC-FILLER          PIC X(24).       
GP3M00     05  TAG53-LOC  REDEFINES TAG53-LINE-2       
GP3M00                                       PIC X(35).       
GP3M00     05  TAG53-NAME REDEFINES TAG53-LINE-2       
GP3M00                                       PIC X(35).       
GP3M00     05  TAG53-LINE-3                 PIC X(35).       
GP3M00     05  TAG53-LINE-4                 PIC X(35).       
GP3M00     05  TAG53-LINE-5                 PIC X(35).       
GP3M00       
GP3M00 01  TAG54-FORMAT.       
GP3M00     05  TAG54-LINE-1.       
GP3M00         07  TAG54-FIL1                PIC X(2).       
GP3M00         07  TAG54-OPT                 PIC X(1).       
GP3M00         07  TAG54-FIL2                PIC X(1).       
GP3M00         07  TAG54-PTID.       
GP3M00             09  TAG54-PTID-1          PIC X(02).       
GP3M00             09  TAG54-PTID-2          PIC X(35).       
GP3M00     05  TAG54-LINE-2                 PIC X(35).       
GP3M00     05  TAG54-BIC  REDEFINES TAG54-LINE-2.       
GP3M00         07  TAG54-BIC-SUB1            PIC X(4).       
GP3M00         07  TAG54-BIC-SUB2            PIC X(2).       
GP3M00         07  TAG54-BIC-SUB3            PIC X(2).       
GP3M00         07  TAG54-BIC-SUB4            PIC X(3).       
GP3M00         07  TAG54-BIC-FILLER          PIC X(24).       
GP3M00     05  TAG54-LOC  REDEFINES TAG54-LINE-2       
GP3M00                                       PIC X(35).       
GP3M00     05  TAG54-NAME REDEFINES TAG54-LINE-2       
GP3M00                                       PIC X(35).       
GP3M00     05  TAG54-LINE-3                 PIC X(35).       
GP3M00     05  TAG54-LINE-4                 PIC X(35).       
GP3M00     05  TAG54-LINE-5                 PIC X(35).       
GP3M00       
       01  TABLE-ARRAY.
       05  TAB-VAL  OCCURS 20 TIMES      PIC X  VALUE "X".
       
       01  TABLE-ARR2.
       05  TAB-VL2  OCCURS 20 TIMES      PIC X  VALUE "X".
       
       01  PATH-P1                            PIC X(20)
              VALUE "NXYXXXXNNYXXXXXXXXX".
       01  PATH-P2                            PIC X(20)
              VALUE "NXYXXXXXNNNXXXXXXXXXX".
       01  PATH-P3                            PIC X(20)
              VALUE "NXNNXXXYNNYXXXXXXXXXX".
       01  PATH-P4                            PIC X(20)
              VALUE "NXNNNXXYNNNNXXXXXXXXX".
       01  PATH-P5                            PIC X(20)
              VALUE "NXNYYNYNNYXXXXXXXXXX".
       01  PATH-P6                            PIC X(20)
              VALUE "NXNYYNYNNNXXXXXXXXXX".
       01  PATH-P7                            PIC X(20)
              VALUE "NXNYYYYXNNYXXXXXXXXXX".
       01  PATH-P8                            PIC X(20)
              VALUE "NXNYYYYXNNNXXXXXXXXXX".
       01  PATH-P9                            PIC X(20)
              VALUE "NYYXXXXNNXXXXXXXXXX".
       01  PATH-P10                           PIC X(20)
              VALUE "XXYXXXXNNYXXXXXXXXXX".
       01  PATH-P11                           PIC X(20)
              VALUE "XXYXXXXXXNNXNNYXXXXXX".
       01  PATH-P12                           PIC X(20)
              VALUE "XXYXXXXXXNNXNNNXXXXXX".
       01  PATH-P13                           PIC X(20)
              VALUE "XXNYYYYXNNXYXXXXXXXXX".
       01  PATH-P14                           PIC X(20)
              VALUE "XXNYYYYXNNXNYXXXXXXXX".
       01  PATH-P15                           PIC X(20)
              VALUE "XXNYYNYNNXYXXXXXXXXX".
       01  PATH-P16                           PIC X(20)
              VALUE "XXNYYNYNNXNNYXXXXXXX".
       01  PATH-P17                           PIC X(20)
              VALUE "XXNYYNYNXNNNXXXXXXXX".
       01  PATH-P18                           PIC X(20)
              VALUE "XXNNXXYNNXYXXXXXXXXX".
       01  PATH-P19                           PIC X(20)
              VALUE "XXNNXXYNNXNNYXXXXXXXX".
       01  PATH-P20                           PIC X(20)
              VALUE "XXNNXXYNNXNNNYXXXXXX".
       
       01  WK-C-PARADATA.
       05  WK-C-PARAVALU                  PIC  X(20).
       05  WK-N-PARAVALU                  REDEFINES WK-C-PARAVALU
       			      PIC  9(13)V99.
       05  WK-N-IRMPSTP                   PIC  9(13)V99.
       05  WK-N-IRM1STP                   PIC  9(13)V99.
       
       01  WK-C-ACCNO.
       10  FILLER                          PIC X(03).
       10  WK-C-ACCNO-4                   PIC X(01).
       10  FILLER                          PIC X(30).
       
HOJE01 01  WK-C-ACCNAME.       
HOJE01     05  WK-C-CON-NAME                  PIC X(35)     VALUE SPACES.       
HOJE01     05  WK-C-CON-SA-NAME               PIC X(35)     VALUE SPACES.       
       
HOJE01     05  WK-C-CON-CA-NAME               PIC X(35)     VALUE SPACES.       
HOJE02     05  WK-C-CON-FA-NAME               PIC X(35)     VALUE SPACES.       
HOJE01     05  WK-C-STRING                    PIC X(35)     VALUE SPACES.       
HOJE01     05  WK-C-WORK-STRING               PIC X(35)     VALUE SPACES.       
HOJE01     05  WK-C-UNSTR-SALU.       
HOJE03         10 WK-C-SORT-SALUT              PIC X(20)     VALUE SPACES.       
HOJE01         10 WK-C-UNSTR-SALU01            PIC X(20)     VALUE SPACES.       
HOJE01         10 WK-C-UNSTR-SALU02            PIC X(20)     VALUE SPACES.       
HOJE01         10 WK-C-UNSTR-SALU03            PIC X(20)     VALUE SPACES.       
HOJE01         10 WK-C-UNSTR-SALU04            PIC X(20)     VALUE SPACES.       
HOJE01         10 WK-C-UNSTR-SALU05            PIC X(20)     VALUE SPACES.       
HOJE01         10 WK-C-UNSTR-SALU06            PIC X(20)     VALUE SPACES.       
HOJE01         10 WK-C-UNSTR-SALU07            PIC X(20)     VALUE SPACES.       
HOJE01     05  WK-C-HLD-SALU.       
HOJE01         10 WK-C-HLD-SALU01              PIC X(01)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU02              PIC X(02)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU03              PIC X(03)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU04              PIC X(04)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU05              PIC X(05)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU06              PIC X(06)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU07              PIC X(07)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU08              PIC X(08)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU09              PIC X(09)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU10              PIC X(10)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU11              PIC X(11)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU12              PIC X(12)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU13              PIC X(13)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU14              PIC X(14)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU15              PIC X(15)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU16              PIC X(16)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU17              PIC X(17)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU18              PIC X(18)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU19              PIC X(19)     VALUE SPACES.       
HOJE01         10 WK-C-HLD-SALU20              PIC X(20)     VALUE SPACES.       
HOJE03*HOJE01     05  WK-C-ARR-SALUTATION1 OCCURS 60 TIMES.       
HOJE03*HOJE01         10 WK-C-ARR-SALU        PIC X(20).       
HOJE01     05  WK-C-TEMP-SALU                  PIC X(20)     VALUE SPACES.       
HOJE01     05  WK-C-SPC-CHAR                   PIC X(40)     VALUE SPACES.       
HOJE01     05  WK-C-SPC-CHAR1                  PIC X(01)     VALUE SPACES.       
HOJE01     05  WK-C-CHK-LEN                    PIC X(02)     VALUE ZEROES.       
HOJE01     05  WK-N-CHK-LEN  REDEFINES WK-C-CHK-LEN       
				PIC 9(02).       
HOJE01     05  WK-N-CNT-PARA                   PIC 9(02)     VALUE ZEROES.       
HOJE01     05  WK-N-CNT                        PIC 9(02)     VALUE ZEROES.       
HOJE01     05  WK-N-COL                        PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-ROW                        PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT1                       PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT2                       PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT3                       PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT4                       PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT5                       PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT6                       PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT7                       PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT8                       PIC 9(02)     VALUE ZEROES.       
       
HOJE03     05  WK-N-CNT9                       PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT10                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT11                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT12                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT13                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT14                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT15                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT16                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT17                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT18                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT19                      PIC 9(02)     VALUE ZEROES.       
HOJE03     05  WK-N-CNT20                      PIC 9(02)     VALUE ZEROES.       
HOJE01     05  WK-N-CTR                        PIC 9(02)     VALUE ZEROES.       
HOJE01     05  WK-N-TALLY                      PIC 9(02)     VALUE ZEROES.       
       
HOJE03 01  WK-C-SALU-TABLE.       
HOJE03     05  WK-C-ARR-SALUTATION1 OCCURS 20 TIMES INDEXED BY WS-CTR1.       
HOJE03         10  WK-C-ARR-SALUTATION2 OCCURS 60 TIMES       
			    INDEXED BY WS-CTR2.       
HOJE03             15 WK-C-ARR-SALU        PIC X(20).       
       
       01  WK-C-WORK-AREA.
           05  FIRST-TIME                      PIC X(01)     VALUE "Y".
       	   05  WS-FLAG1                        PIC X(01)     VALUE SPACE.
       	   05  WS-FLAG2                        PIC X(01)     VALUE SPACE.
       	   05  WS-FXCONV                       PIC X(01)     VALUE SPACE.
       	   05  WS-OKAY                         PIC X(01)     VALUE SPACE.
       	   05  WS-ROUTE                        PIC X(01)     VALUE SPACE.
       	   05  WS-JUMP                         PIC 9(02)     VALUE ZEROS.
       	   05  WS-STPTYP                       PIC X(04)     VALUE SPACES.
       	   05  WS-NAME                         PIC X(35)     VALUE SPACES.
       	   05  WS-ADDR1                        PIC X(35)     VALUE SPACES.
       	   05  WS-ADDR2                        PIC X(35)     VALUE SPACES.
       	   05  WS-ADDR3                        PIC X(35)     VALUE SPACES.
       	   05  WS-ACCCUY                       PIC X(03)     VALUE SPACES.
       	   05  WS-ACCCUY-ORG                   PIC X(03)     VALUE SPACES.
       	   05  WS-ACCNO                        PIC X(11)     VALUE SPACES.
       	   05  WS-ACCNO-ORG                    PIC X(11)     VALUE SPACES.
       	   05  WS-ACBNKACC                     PIC X(11)     VALUE SPACES.
       	   05  WS-BENBENKACC                   PIC X(35)     VALUE SPACES.
       	   05  WS-BENEACC                      PIC X(11)     VALUE SPACES.
       	   05  WS-BANKID                       PIC X(11)     VALUE SPACES.
       	   05  WS-ACBNKID                      PIC X(11)     VALUE SPACES.
       	   05  WS-BENBNKID                     PIC X(11)     VALUE SPACES.
       	   05  WS-ACUDBUI                      PIC X(01)     VALUE "D".
       	   05  WS-ACCTYP                       PIC X(01)     VALUE SPACE.
GPI201     05  WK-C-GPI-SW                     PIC X(01)     VALUE SPACE.       
GPI201     05  WK-C-BYPASS-LMT-IND             PIC X(01)     VALUE SPACE.       
GPI201     05  WK-C-DR-PMODE                   PIC X(08)     VALUE SPACE.       
GPI201     05  WK-C-COV-SW                     PIC X(01)     VALUE SPACE.       
GP3C00     05  WK-C-GPI3-SW                    PIC X(01)     VALUE SPACE.       
GP3C00     05  WK-C-TAG57-CD-SW                PIC X(01)     VALUE SPACE.       
GP3C00     05  WK-C-TAG59F-SW                  PIC X(01)     VALUE SPACE.       
GP3M00     05  WK-C-NSLMT-SW                   PIC X(01)     VALUE SPACE.       
       
VASA01     05  WK-C-VASA-SW                    PIC X(01)     VALUE SPACE.       
VASA01     05  WK-C-CA-EXIST                   PIC X(01)     VALUE SPACE.       
       
GPI201 01  WK-C-LIT-GPI.       
GPI201     05 WK-C-Y                           PIC X(01)     VALUE "Y".       
GPI201     05 WK-C-A                           PIC X(01)     VALUE "A".       
GPI201     05 WK-C-GPI-SW-PARCD                PIC X(10)       
GPI201                                         VALUE "GPISWITCH2".       
GPI201     05 WK-C-STP-SW-PARCD                PIC X(10)       
GPI201                                         VALUE "GPISTPSW".       
GP3M00     05 WK-C-NSLMT-PARCD                 PIC X(10)       
GP3M00                                         VALUE "GPI3NSLMT".       
       
GP3C00     05 WK-C-GPI3-SW-PARCD               PIC X(10)       
GP3C00                                         VALUE "GPISWITCH3".       
GP3C00     05 WK-C-TAG57-SW-PARCD              PIC X(10)       
GP3C00                                         VALUE "GPI3T57SW".       
GP3C00     05 WK-C-TAG57-MT-PARCD              PIC X(10)       
GP3C00                                         VALUE "GPI3T57MT".       
GP3K00     05 WK-C-TAG59F-SW-PARCD             PIC X(10)       
GP3K00                                         VALUE "GPI3T59FSW".       
       
GP3C00 01  WK-C-MT-TAG57-TBL                   PIC X(18)     VALUE SPACES.       
GP3C00     05  WK-C-MT-TAG57                   PIC X(03)     OCCURS 6 TIMES.       
       
5Q1ARV 01  WK-C-RPRRN-AREA.       
5Q1ARV     05  WK-C-SEGCDE                      PIC X(01)     VALUE SPACE.       
5Q1ARV     05  WK-N-STAFFIND                    PIC S9(02)    VALUE ZEROS.       
5Q1ARV     05  WK-C-ACCNO-RPR                   PIC X(11)     VALUE SPACE.       
5Q1ARV     05  WK-C-ACCNAME-RPR                 PIC X(35)     VALUE SPACE.       
5Q1ARV     05  WK-C-QRATE                       PIC X(02)     VALUE SPACE.       
5Q1ARV     05  WK-C-RPRCODE                     PIC X(07)     VALUE SPACE.       
5Q1ARV     05  WK-C-TRNNO                       PIC X(12)     VALUE SPACE.       
5Q1ARV     05  WK-C-FUNCTID                     PIC X(08)     VALUE SPACE.       
5Q1ARV 01  WK-N-SYSDTE                          PIC S9(08)    VALUE ZEROS.       
5Q1ARV 0I  WK-C-RPRPGM                          PIC X(10)     VALUE       
5Q1ARV     "TRFVTF1B".       
CMP3A3 0I  WK-101-TAG50H-ACCNO                   PIC X(11)     VALUE SPACE.       
7Q1EM2 01  WK-C-SWFTMGTY                        PIC X(03)     VALUE SPACES.       
7Q1EM3 01  WK-C-TRN-NO                          PIC X(12)     VALUE SPACES.       
REM269 01  WK-C-LCUYCD                          PIC X(03).       
       
SGX201 01  WK-C-SGX2-PARAM.       
SGX202*SGX201     05  WK-C-SGX2-TECH-SW.       
SGX202*SGX201         10  WK-SGX-CUTTTM-SW       PIC X(01)     VALUE SPACES.       
SGX202*SGX201         10  WK-SGX-DALMT-SW        PIC X(01)     VALUE SPACES.       
SGX202*SGX201         10  WK-SGX-GLACT-SW        PIC X(01)     VALUE SPACES.       
SGX202*SGX201         10  WK-SGX-PRTPAYM-SW      PIC X(01)     VALUE SPACES.       
SGX202*SGX201         10  FILLER                  PIC X(06)     VALUE SPACES.       
SGX202     05  WK-SGX-GLACT-SW                 PIC X(01) VALUE SPACES.       
SGX201     05  WK-C-GLACT-MTMSG-TBL            PIC X(24) VALUE SPACES.       
SGX201         10  WK-C-GLACT-MTMSG            PIC X(06) OCCURS 4 TIMES.       
SGX201         15 WK-C-GLACT-MTMSG1            PIC X(03).       
SGX201         15 WK-C-GLACT-MTMSG2            PIC X(03).       
       
SGX201 01  WK-C-SGX2-VAR.       
SGX201     05  WK-C-GLACT-PREFIX               PIC X(01).       
SGX201     05  WK-C-TAG59-G-IND                PIC X(01) VALUE SPACES.       
SGX201     05  WK-C-GLACT-VALID-MSG            PIC X(01) VALUE SPACE.       
SGX201     05  WK-V-VALID-GLACNO               PIC X(01) VALUE SPACES.       
SGX203     05  WK-C-CR-GLACNO                  PIC X(01) VALUE SPACES.       
SGX201     05  WK-C-GLACT-CRMOD                PIC X(08) VALUE SPACES.       
SGX201     05  WK-C-GLACT-RPRCODE              PIC X(07) VALUE SPACES.       
SGX201     05  WK-SGX-DAY2-SW                  PIC X(01) VALUE SPACES.       
SGX201     05  WK-N-GLACNO                     PIC 9(08).       
SGX201     05  WK-ACCNO-JUST-RT                PIC X(18).       
SGX201         10  WK-ACCNO-JR-1-10            PIC X(10).       
SGX201         10  WK-ACCNO-JR-11-18           PIC X(08).       
SGX205     05  WS-SUB                          PIC 9(02) VALUE ZEROS.       
       
OFX2A1 01  WK-C-OFX-TCH-SW2                    PIC X(01) VALUE SPACE.       
OFX2A1 01  WK-N-XMS-RETRY-LMT                  PIC 9(03) VALUE ZEROS.       
OFX2A1 01  WK-C-OFX-VARS.       
OFX2A1     05  WK-C-OFX-SUBSCRIBED              PIC X(01) VALUE SPACE.       
OFX2A1     05  WK-C-SEGCODE                     PIC X(01) VALUE SPACE.       
OFX2A1     05  WK-C-TMP-PARALNO-SEQNUM.       
OFX2A1         10  WK-C-TMP-PARALNO             PIC 9(08) VALUE ZEROS.       
OFX2A1         10  WK-C-TMP-SEQNUM              PIC 9(02) VALUE ZEROS.       
OFX2A1 01  WK-N-OFX-VARS.       
OFX2A1     05  WK-N-XMS-RETRY-CTR               PIC 9(03) VALUE ZEROS.       
       
VASA01 01  WK-C-VASA-PARAM.       
VASA01     05  WK-C-CON-SA-NAME-SUB             PIC X(35) VALUE SPACES.       
VASA01     05  WK-C-CON-CA-NAME-SUB             PIC X(35) VALUE SPACES.       
VASA01 01  WK-C-CA-NO-EXPAND                    PIC X(18).       
       
REM269 COPY XGSPA.       
       COPY VCCA.
       COPY VCSA.
       COPY VCFA.
       COPY VSTPL.
       COPY VTF2.
       COPY VBAC.
       COPY VBANO.
       COPY XPARA.
       COPY NSTP.
       COPY ACMN.
       COPY ACRO.
       COPY NMVR.
       COPY LOGG.
       COPY GERTE.
       SMITY1 COPY VDUPL.
       5Q1ARV COPY RRSN.
       7Q1EM2 COPY TRFLDA.
       SGX201 COPY VGLAC.
       GPI201 COPY GPISTPSW.
       GPI201 COPY VBACU.
       GP3C00 COPY VTAG57.
       GP3K00 COPY VTAG59F.
       
       OFX2A1 COPY IXMSQ.
       OFX2A1 COPY CHID.
       OFX2A1 COPY CSEG.
       OFX2A1 COPY CUYP.
       
       LINKAGE SECTION.
      ****************
       COPY VTF1B.
       PROCEDURE DIVISION USING WK-VTF1B.
       ***********************************
       MAIN-MODULE.
       
7Q1EM2      ACCEPT  L-C-LOCAL-DATA-AREA   FROM  LOCAL-DATA-AREA.
       
CMP3A3      MOVE SPACES                    TO    WK-101-TAG50H-ACCNO.       
CMP3A3      IF   WK-VTF1B-BANKAC NOT = SPACES       
CMP3A3          MOVE  WK-VTF1B-BANKAC TO       WK-101-TAG50H-ACCNO       
CMP3A3          MOVE  SPACES          TO       WK-VTF1B-BANKAC       
CMP3A3      END-IF.       
       
       INITIALIZE WK-VTF1B-OUTPUT
             WK-LOGG
             WK-C-WORK-AREA.
       MOVE ALL "X"    TO TABLE-ARRAY.
       MOVE ALL "X"    TO TABLE-ARR2.
       MOVE "Y"        TO FIRST-TIME.
       
5Q1ARV MOVE ZEROS      TO WK-C-RRSN-QUENUM       
5Q1ARV                    WK-C-RRSN-QUESUF       
5Q1ARV                    WK-C-RRSN-STAFFIND       
5Q1ARV                    WK-C-RRSN-SEQNUM       
5Q1ARV                    WK-C-RRSN-RPRDTE.       
       
OFX2A1 INITIALIZE WK-C-OFX-VARS       
OFX2A1            WK-N-OFX-VARS.       
       
GPI201 MOVE WK-VTF1B-DR-PMODE          TO    WK-C-DR-PMODE.       
       
GPI201 IF FIRST-TIME = "Y"
          OPEN     INPUT TFSSTPL
       IF  NOT WK-C-SUCCESSFUL
       AND WK-C-FILE-STATUS NOT = "41"
          DISPLAY "TRFVTF1B - OPEN FILE ERROR - TFSSTPL"
          DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF
       OPEN     INPUT TFSCLSYS
       IF  NOT WK-C-SUCCESSFUL
       AND WK-C-FILE-STATUS NOT = "41"
          DISPLAY "TFSCLSYS - OPEN FILE ERROR - TFSCLSYS"
          DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF
GPI201 OPEN     INPUT UFIMIJCON               
GPI201 IF  NOT WK-C-SUCCESSFUL               
GPI201 AND WK-C-FILE-STATUS NOT = "41"               
GPI201     DISPLAY "UFIMIJCON - OPEN FILE ERROR - UFIMIJCON"               
GPI201     DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS               
GPI201 END-IF               
VASA01 OPEN     INPUT TFSICLCA2               
VASA01 IF  NOT WK-C-SUCCESSFUL               
VASA01 AND WK-C-FILE-STATUS NOT = "41"               
VASA01     DISPLAY "TFSICLCA2 - OPEN FILE ERROR - TFSICLCA2"               
VASA01     DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS               
VASA01 END-IF               
VASA01 OPEN     INPUT TFSICLSA2               
VASA01 IF  NOT WK-C-SUCCESSFUL               
VASA01 AND WK-C-FILE-STATUS NOT = "41"               
VASA01     DISPLAY "TFSICLSA2 - OPEN FILE ERROR - TFSICLSA2"               
VASA01     DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS               
VASA01 END-IF               
       END-IF.
       
       MOVE      WK-VTF1B-PARALNO    TO     TFSSTPL-PARALNO.
       MOVE      WK-VTF1B-SEQNUM     TO     TFSSTPL-SEQNUM.
       
       READ      TFSSTPL
            KEY IS EXTERNALLY-DESCRIBED-KEY.
       
       IF  WK-C-SUCCESSFUL
       MOVE TFSSTPL-TAG57     TO  TAG57-FORMAT
       MOVE TFSSTPL-TAG58     TO  TAG58-FORMAT
       MOVE TFSSTPL-TAG59     TO  TAG59-FORMAT
       MOVE TFSSTPL-CUYCD     TO  WS-ACCCUY
       PERFORM A100-INITIAL-SUBROUTINE
         THRU A199-INITIAL-SUBROUTINE-EX
       PERFORM A200-MOVE-TAG-VALUES
         THRU A299-MOVE-TAG-VALUES-EX
       PERFORM B100-PATH-CHOICE  THRU B199-PATH-CHOICE-EX
       END-IF.
       GO TO    Z000-END-PROGRAM.
       
       A100-INITIAL-SUBROUTINE.
       *----------------------------------------------------------------*
       *     GET DATA FROM "TFSCLSYS" TABLE                            *
       *----------------------------------------------------------------*
       READ      TFSCLSYS.
       IF        NOT WK-C-SUCCESSFUL
            DISPLAY "TRFVTF1B    - READ TFSCLSYS ERROR"
            DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
            GO TO    Z000-END-PROGRAM.
       
5Q1ARV      MOVE      TFSCLSYS-SYSDTE           TO      WK-N-SYSDTE.       
       
REM269*Retrieve local currency code       
|      INITIALIZE  WK-C-XGSPA-RECORD.       
|      MOVE "RSYCTLLCUY"                    TO      WK-C-XGSPA-GHPARCD.       
|      CALL "TRFXGSPA"                      USING   WK-C-XGSPA-RECORD.       
       
|      IF  WK-C-XGSPA-ERROR-CD        =     SPACES       
|          MOVE WK-C-XGSPA-GHPARVAL     TO       
|      ELSE       
|          MOVE SPACES       
|         
REM269 END-IF.       
       
SGX202*SGX201*Retrieve SGX Phase 2 Technical Switch       
SGX202*SGX201 INITIALIZE      WK-C-XGSPA-RECORD            
SGX202*SGX201                 WK-SGX2-TECH-SW.            
SGX202*SGX201 MOVE "SGX2TECHSW"                   TO        WK-C-XGSPA-GHPARCD.            
SGX202*SGX201 CALL "TRFXGSPA"                     USING     WK-C-XGSPA-RECORD.            
SGX202*SGX201 IF  WK-C-XGSPA-ERROR-CD       =      SPACES            
SGX202*SGX201     MOVE WK-C-XGSPA-GHPARVAL(1:10)            
SGX202*SGX201                                   TO        WK-C-SGX2-TECH-SW            
SGX202*SGX201 ELSE            
SGX202*SGX201     MOVE ALL "N"                  TO        WK-C-SGX2-TECH-SW            
SGX202*SGX201 END-IF.            
       
SGX201** CHECK FOR SGX DAY2 SWITCH IN TFSGSYSPA **       
SGX201      MOVE SPACES                           TO       WK-C-SGX-DAY2-SW.       
SGX201      INITIALIZE                                      WK-C-XGSPA-RECORD.       
SGX201      MOVE "SGX2MAINSW"                     TO       WK-C-XGSPA-GHPARCD.       
SGX201      CALL "TRFXGSPA"                      USING     WK-C-XGSPA-RECORD.       
SGX201      IF WK-C-XGSPA-ERROR-CD                =        SPACES       
SGX201          MOVE WK-C-XGSPA-GHPARVAL          TO       WK-SGX-DAY2-SW       
SGX201      END-IF.       
       
SGX202*-- Retrieve SGX Phase 2 - GL ACCOUNT TECH SWITCH       
SGX202      INITIALIZE                                      WK-C-XGSPA-RECORD.       
SGX202      MOVE "N"                              TO       WK-SGX-GLACT-SW       
SGX202      MOVE "SGX2GLACSW"                     TO       WK-C-XGSPA-GHPARCD.       
SGX202      CALL "TRFXGSPA"                      USING     WK-C-XGSPA-RECORD.       
SGX202      IF  WK-C-XGSPA-ERROR-CD         =     SPACES       
SGX202          AND WK-C-XGSPA-GHPARVAL(1:1) = "Y"       
SGX202                MOVE WK-C-XGSPA-GHPARVAL(1:1)       
SGX202                                              TO       WK-SGX-GLACT-SW       
SGX202      END-IF.       
       
SGX201*--> Retrieve List of eligible MT types for CR/DR GL Account       
SGX201      INITIALIZE                                      WK-C-XGSPA-RECORD       
SGX201                                                      WK-C-GLACT-MTMSG-TBL       
SGX201      MOVE "GLACTMT"                        TO       WK-C-XGSPA-GHPARCD.       
SGX201      CALL "TRFXGSPA"                      USING     WK-C-XGSPA-RECORD.       
SGX201      IF  WK-C-XGSPA-GHPARVAL NOT EQUAL SPACES       
SGX201          MOVE WK-C-XGSPA-GHPARVAL          TO       WK-C-GLACT-MTMSG-TBL       
       
SGX201      END-IF.       
       
GPI201*----------------------------------------------------------------*       
GPI201* RETRIEVE GPI TECHNICAL AND STP SWITCH FROM SYSTEM PARAMETER  *       
GPI201*----------------------------------------------------------------*       
GPI201      INITIALIZE                            WK-C-XGSPA-RECORD       
GPI201                                            SW-STP-LMT-SKP.       
GPI201       
GPI201      MOVE WK-C-STP-SW-PARCD           TO   WK-C-XGSPA-GHPARCD.       
GPI201      CALL "TRFXGSPA"                  USING WK-C-XGSPA-RECORD.       
       
GPI201      IF WK-C-XGSPA-ERROR-CD = SPACES       
GPI201          MOVE WK-C-XGSPA-GHPARVAL(2:1)       
GPI201                                        TO   SW-STP-LMT-SKP       
GPI201      END-IF.       
GPI201      END-IF.       
GPI201      INITIALIZE                            WK-C-XGSPA-RECORD       
GPI201                                            WK-C-GPI-SW.       
GPI201      MOVE WK-C-GPI-SW-PARCD          TO   WK-C-XGSPA-GHPARCD.       
GPI201      CALL "TRFXGSPA"                 USING WK-C-XGSPA-RECORD.       
GPI201       
GPI201      IF WK-C-XGSPA-ERROR-CD = SPACES       
GPI201          MOVE WK-C-XGSPA-GHPARVAL(1:1)       
GPI201                                       TO   WK-C-GPI-SW       
GPI201      END-IF.       
       
GP3C00*-->Retrieve GPI Day 3 Technical Switch       
GP3C00      INITIALIZE                           WK-C-XGSPA-RECORD       
GP3C00                                           WK-C-GPI3-SW.       
GP3C00      MOVE WK-C-GPI3-SW-PARCD         TO   WK-C-XGSPA-GHPARCD.       
GP3C00      CALL "TRFXGSPA"                 USING WK-C-XGSPA-RECORD.       
GP3C00       
GP3C00      IF   WK-C-XGSPA-ERROR-CD = SPACES       
GP3C00           MOVE WK-C-XGSPA-GHPARVAL(1:1)       
GP3C00                                       TO   WK-C-GPI3-SW       
GP3C00      END-IF.       
       
GP3C00*-->Retrieve GPI Day3 Tag57 C/D Enhancement Switch       
GP3C00      INITIALIZE                           WK-C-XGSPA-RECORD       
GP3C00                                           WK-C-TAG57-CD-SW.       
GP3C00      MOVE WK-C-TAG57-SW-PARCD        TO   WK-C-XGSPA-GHPARCD.       
GP3C00      CALL "TRFXGSPA"                 USING WK-C-XGSPA-RECORD.       
GP3C00      IF   WK-C-XGSPA-ERROR-CD = SPACES       
GP3C00           MOVE WK-C-XGSPA-GHPARVAL(1:1)       
GP3C00                                       TO   WK-C-TAG57-CD-SW       
GP3C00      END-IF.       
       
GP3C00*-->Retrieve GPI Day3 Tag57 C/D Enhancement Eligable MT Types       
GP3C00      INITIALIZE                           WK-C-XGSPA-RECORD       
GP3C00                                           WK-C-MT-TAG57-TBL.       
GP3C00      MOVE WK-C-TAG57-MT-PARCD        TO   WK-C-XGSPA-GHPARCD.       
GP3C00      CALL "TRFXGSPA"                 USING WK-C-XGSPA-RECORD.       
GP3C00      IF   WK-C-XGSPA-ERROR-CD = SPACES       
GP3C00           MOVE WK-C-XGSPA-GHPARVAL       
GP3C00                                       TO   WK-C-MT-TAG57-TBL       
GP3C00      END-IF.       
       
GP3K00*-->Retrieve GPI Day3 Tag59F handling Enhancement Switch       
       
GP3K00      INITIALIZE                           WK-C-XGSPA-RECORD       
GP3K00                                           WK-C-TAG59F-SW.       
GP3K00      MOVE WK-C-TAG59F-SW-PARCD       TO   WK-C-XGSPA-GHPARCD.       
GP3K00      CALL "TRFXGSPA"                 USING WK-C-XGSPA-RECORD.       
GP3K00      IF   WK-C-XGSPA-ERROR-CD = SPACES       
GP3K00           MOVE WK-C-XGSPA-GHPARVAL(1:1)       
GP3K00                                       TO   WK-C-TAG59F-SW       
GP3K00      END-IF.       
       
GP3M00*_>Retrieve GPI Day3 Nostro Bypass STP Limit Enhancement       
GP3M00      INITIALIZE                           WK-C-XGSPA-RECORD       
GP3M00                                           WK-C-NSLMT-SW.       
GP3M00      MOVE WK-C-NSLMT-PARCD           TO   WK-C-XGSPA-GHPARCD.       
GP3M00      CALL "TRFXGSPA"                USING WK-C-XGSPA-RECORD.       
GP3M00      IF   WK-C-XGSPA-ERROR-CD = SPACES       
GP3M00           MOVE WK-C-XGSPA-GHPARVAL(1:1)       
GP3M00                                       TO   WK-C-NSLMT-SW       
GP3M00      END-IF.       
       
OFX2A1*-->Retrieve OFX R2 technical switch.       
OFX2A1      INITIALIZE                           WK-C-XGSPA-RECORD.       
OFX2A1      MOVE "N"                         TO   WK-C-OFX-TCH-SW2.       
OFX2A1      MOVE "FXONLTCHS2"                TO   WK-C-XGSPA-GHPARCD.       
OFX2A1      CALL "TRFXGSPA"                USING WK-C-XGSPA-RECORD.       
OFX2A1      IF   WK-C-XGSPA-ERROR-CD       =       
OFX2A1           MOVE WK-C-XGSPA-GHPARVAL(1:1)       
OFX2A1                                       TO   WK-C-OFX-TCH-SW2       
OFX2A1      END-IF.       
       
OFX2A1*-->Retrieve OFX R2 technical switch.       
OFX2A1      INITIALIZE                           WK-C-XGSPA-RECORD       
OFX2A1                                           WK-N-XMS-RETRY-LMT.       
OFX2A1      MOVE "XMSRTRYLMT"                TO   WK-C-XGSPA-GHPARCD.       
OFX2A1      CALL "TRFXGSPA"                USING WK-C-XGSPA-RECORD.       
OFX2A1      IF   WK-C-XGSPA-ERROR-CD       =      SPACES       
OFX2A1      AND  WK-C-XGSPA-GHPARVAL(1:3) IS     NUMERIC       
OFX2A1           MOVE WK-C-XGSPA-GHPARVAL(1:3)       
OFX2A1                                       TO    WK-N-XMS-RETRY-LMT       
OFX2A1      END-IF.       
       
VASA01*--> Check VASA switch       
VASA01      MOVE SPACES                      TO WK-C-VASA-SW.       
VASA01      INITIALIZE                           WK-C-XGSPA-RECORD.       
VASA01      MOVE "VASASW"                    TO WK-C-XGSPA-GHPARCD.       
VASA01      CALL "TRFXGSPA"                USING WK-C-XGSPA-RECORD.       
VASA01      IF WK-C-XGSPA-ERROR-CD            = SPACES       
VASA01          MOVE WK-C-XGSPA-GHPARVAL TO WK-C-VASA-SW       
VASA01      END-IF.       
       
----------------------------------------------------------------*      *
SGX201       
SGX201       END-IF.       
SGX201       
SGX201*--> Retrieve Prefix Indicator of GL Account       
SGX201       INITIALIZE                      WK-C-XGSPA-RECORD       
SGX201                                       WK-C-GLACT-PREFIX       
SGX201       MOVE "GLACTIND"            TO   WK-C-XGSPA-GHPARCD.       
SGX201       CALL "TRFXGSPA"         USING   WK-C-XGSPA-RECORD.       
SGX201       IF   WK-C-XGSPA-GHPARVAL NOT EQUAL SPACES       
SGX201            MOVE WK-C-XGSPA-GHPARVAL(1:1)       
SGX201                                  TO   WK-C-GLACT-PREFIX       
SGX201       END-IF.       
SGX201       
SGX201 A159-INITIALIZE-SGX-FLDS-EX.       
SGX201       EXIT.       
      
       A200-MOVE-TAG-VALUES.       
          IF   TAG57-BIC NOT= SPACES       
          AND TFSSTPL-SWFTMGTY = "200"       
               MOVE TAG57-BIC        TO   WS-ACBNKID       
                                          WS-BANKID       
               MOVE TAG57-PTID       TO   WS-ACBNKACC       
                                          WS-ACCNO       
          END-IF.       
      
GP3C00*-->GPI Day3 Tag57 C/D Enhancement       
GP3C00 IF   WK-C-GPI3-SW = "Y"             
GP3C00 AND WK-C-TAG57-CD-SW = "Y"             
GP3C00      IF    TAG57-OPT = "C" OR "D"             
GP3C00           AND (TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(1)             
GP3C00            OR  TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(2)             
GP3C00            OR  TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(3)             
GP3C00            OR  TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(4)             
GP3C00            OR  TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(5)             
GP3C00            OR  TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(6))             
GP3C00                PERFORM D600-EVAL-TAG57-CD             
GP3C00                   THRU D699-EVAL-TAG57-CD-EX             
GP3C00           END-IF             
GP3C00      END-IF.             
      
            IF   TAG58-BIC NOT = SPACES     
            AND TFSSTPL-SWFTMGTY = "202" OR "203"     
                 MOVE TAG58-BIC        TO   WS-BENBNKID     
                                            WS-BANKID     
                 MOVE TAG58-PTID       TO   WS-BENBKACC     
                                            WS-ACCNO     
      
            END-IF.     
            IF   TAG59-BIC NOT = SPACES
CMP3FL*     AND TFSSTPL-SWFTMGTY = "103"       
CMP3FL      AND (TFSSTPL-SWFTMGTY = "103"       
CMP3FL      OR   TFSSTPL-SWFTMGTY = "101")       
               MOVE TAG59-BIC        TO   WS-BANKID       
SGX201*SGX205  IF   WK-SGX-GLACT-SW   = "Y"       
SGX201*SGX205  AND  WK-C-SGX-DAY2-SW = "Y"       
SGX201*SGX205       PERFORM Y100-VALIDATE-TAG59       
SGX201*SGX205            THRU Y199-VALIDATE-TAG59-EX       
SGX201*SGX205  ELSE       
               MOVE TAG59-PTID       TO   WS-BENEACC       
                                          WS-ACCNO       
SGX205      IF   WK-SGX-GLACT-SW   = "Y"       
SGX205      AND  WK-C-SGX-DAY2-SW = "Y"       
SGX205            PERFORM Y100-VALIDATE-TAG59       
SGX205                 THRU Y199-VALIDATE-TAG59-EX       
SGX205       
SGX201      END-IF       
       END-IF.       
      
SGX201 IF   WK-SGX-GLACT-SW   = "Y"             
SGX201 IF   WK-C-SGX-DAY2-SW = "Y"             
SGX201      IF   WK-C-TAG59-G-IND = "Y"             
SGX201           MOVE "D"              TO   WS-ACUDBUI             
SGX201           MOVE SPACES           TO   WS-ACCTYP             
SGX201           GO TO A201-MOVE-TAG-VALUES             
SGX201      END-IF             
SGX201 END-IF.             
      
       MOVE TFSSTPL-BNKENTTY     TO WK-N-VBAC-BNKENTTY.          
       MOVE WS-BANKID            TO WK-C-VBAC-BANKID.          
       MOVE WS-ACCCUY            TO WK-C-VBAC-CUYCD.          
       CALL "TRFVBAC"  USING WK-C-VBAC-RECORD.          
       IF   WK-C-VBAC-ERROR-CD        = SPACES          
            MOVE WK-C-VBAC-ACUDBUI   TO WS-ACUDBUI          
            MOVE WK-C-VBAC-ACCTYP    TO WS-ACCTYP          
            IF   WS-ACCNO  = SPACES          
                 MOVE WK-C-VBAC-BNKACNO  TO WS-ACCNO          
            END-IF          
       ELSE          
            MOVE "D"                 TO WS-ACUDBUI          
       END-IF.          
SGX201 A201-MOVE-TAG-VALUES.
       IF   TAG58-NAME    NOT = SPACES   
       AND TAG58-OPT          = "D"          
            MOVE SPACES          TO   WS-BANKID          
                                       WS-BENBNKID          
                                       WS-ACBNKID          
            MOVE TAG58-PTID       TO   WS-BENBKACC          
                                       WS-ACCNO          
            MOVE TAG58-NAME       TO   WS-NAME          
            MOVE TAG58-LINE-3     TO   WS-ADDR1          
            MOVE TAG58-LINE-4     TO   WS-ADDR2          
            MOVE TAG58-LINE-5     TO   WS-ADDR3          
       END-IF.          
       IF   TAG59-NAME    NOT = SPACES          
       AND TAG59-OPT          = SPACE          
            MOVE SPACES          TO   WS-BANKID          
                                       WS-BENBNKID          
                                       WS-ACBNKID          
SGX201*SGX205  IF   WK-SGX-GLACT-SW   = "Y"       
SGX201*SGX205  AND  WK-C-SGX-DAY2-SW = "Y"       
SGX201*SGX205       PERFORM Y100-VALIDATE-TAG59       
SGX201*SGX205            THRU Y199-VALIDATE-TAG59-EX       
SGX201*SGX205  ELSE       
               MOVE TAG59-PTID       TO   WS-BENEACC       
                                          WS-ACCNO       
SGX205         IF   WK-SGX-GLACT-SW   = "Y"              
SGX205 	       AND  WK-C-SGX-DAY2-SW = "Y"             
SGX205 	            PERFORM Y100-VALIDATE-TAG59             
SGX205                   THRU Y199-VALIDATE-TAG59-EX           
SGX205 	             
SGX201 	            END-IF             
                        MOVE TAG59-NAME       TO   WS-NAME     
                        MOVE TAG59-LINE-3     TO   WS-ADDR1    
                        MOVE TAG59-LINE-4     TO   WS-ADDR2    
                        MOVE TAG59-LINE-5     TO   WS-ADDR3    
       	           END-IF.      
      
GP3K00 	       IF   WK-C-GPI3-SW       = "Y"            
GP3K00 	       AND WK-C-TAG59F-SW  = "Y"           
GP3K00 	           IF   TAG59-NAME   NOT = SPACES           
GP3K00 	           AND TAG59-OPT        = "F"           
GP3K00                    PERFORM Y400-REFORMAT-TAG59F         
GP3K00                    THRU Y499-REFORMAT-TAG59F-EX         
GP3K00 	           END-IF           
GP3K00 	       END-IF.           
      
       A299-MOVE-TAG-VALUES-EX.    
             EXIT.    
        EJECT    
      	      
       B100-PATH-CHOICE.
             MOVE "Y"        TO WS-FLAG1.
      
             IF   TFSSTPL-SWFTMGTY = "103"
             AND TAG59-OPT         = "A"
             AND TAG59-PTID    NOT = SPACES
             AND TAG59-BIC     NOT = SPACES
CMP3FL       OR  TFSSTPL-SWFTMGTY = "101"       
CMP3FL       AND TAG59-OPT         = "A"       
CMP3FL       AND TAG59-PTID    NOT = SPACES       
CMP3FL       AND TAG59-BIC     NOT = SPACES       
              OR  TFSSTPL-SWFTMGTY = "202"
             AND TAG58-OPT         = "A"    
             AND TAG58-PTID    NOT = SPACES    
             AND TAG58-BIC     NOT = SPACES    
              OR  TFSSTPL-SWFTMGTY = "203"    
             AND TAG58-OPT         = "A"    
             AND TAG58-PTID    NOT = SPACES    
             AND TAG58-BIC     NOT = SPACES    
              OR  TFSSTPL-SWFTMGTY = "200"    
             AND TAG57-OPT         = "A"    
             AND TAG57-PTID    NOT = SPACES    
             AND TAG57-BIC     NOT = SPACES    
                  PERFORM C100-VALIDATION-PART    
                     THRU C199-VALIDATION-PART-EX    
             END-IF.    
      
             IF   TFSSTPL-SWFTMGTY = "103"
             AND TAG59-OPT         = "A"
             AND TAG59-PTID        = SPACES
             AND TAG59-BIC     NOT = SPACES
CMP3FL        OR  TFSSTPL-SWFTMGTY = "101"
CMP3FL       AND TAG59-OPT         = "A"
CMP3FL       AND TAG59-PTID        = SPACES
CMP3FL       AND TAG59-BIC     NOT = SPACES
              OR  TFSSTPL-SWFTMGTY = "202"
             AND TAG58-OPT         = "A"
             AND TAG58-PTID        = SPACES
             AND TAG58-BIC     NOT = SPACES
              OR  TFSSTPL-SWFTMGTY = "203"
             AND TAG58-OPT         = "A"
             AND TAG58-PTID        = SPACES
             AND TAG58-BIC     NOT = SPACES
                  PERFORM C200-VALIDATION-PART
                     THRU C299-VALIDATION-PART-EX
             END-IF.
      
             IF   TFSSTPL-SWFTMGTY = "103"
             AND TAG59-OPT         = SPACES
             AND TAG59-PTID    NOT = SPACES
             AND TAG59-NAME    NOT = SPACES
CMP3FL        OR  TFSSTPL-SWFTMGTY = "101"
CMP3FL       AND TAG59-OPT         = SPACES
CMP3FL       AND TAG59-PTID    NOT = SPACES
CMP3FL       AND TAG59-NAME    NOT = SPACES
              OR  TFSSTPL-SWFTMGTY = "202"
             AND TAG58-OPT         = "D"
             AND TAG58-PTID    NOT = SPACES
             AND TAG58-BIC     NOT = SPACES
             AND TAG58-NAME    NOT = SPACES
              OR  TFSSTPL-SWFTMGTY = "203"
             AND TAG58-OPT         = "D"
             AND TAG58-PTID    NOT = SPACES
             AND TAG58-BIC     NOT = SPACES
             AND TAG58-NAME    NOT = SPACES
                      PERFORM C300-VALIDATION-PART
                         THRU C399-VALIDATION-PART-EX
             END-IF.
      
GP3K00*-->To enable handling of structured Tag59F
GP3K00       IF   WK-C-GPI3-SW = "Y"
GP3K01*GP3K00  AND WK-C-TAG57-CD-SW = "Y"
GP3K01       AND WK-C-TAG59F-SW    = "Y"
GP3K00            IF (TFSSTPL-SWFTMGTY = "103"
GP3K00                OR TFSSTPL-SWFTMGTY = "101")
GP3K00                AND TAG59-OPT        = "F"
GP3K00                AND TAG59-PTID   NOT = SPACES
GP3K00                AND TAG59-NAME   NOT = SPACES
GP3K00                     PERFORM C300-VALIDATION-PART
GP3K00                        THRU C399-VALIDATION-PART-EX
GP3K00            END-IF
GP3K00       END-IF.

          PERFORM  D100-VALIDATION  THRU D199-VALIDATION-EX.
          PERFORM  D200-VALIDATION  THRU D299-VALIDATION-EX.

     B199-PATH-CHOICE-EX.
          EXIT.

     C100-VALIDATION-PART.
          MOVE WS-BANKID            TO WK-NSTP-ACCTBIC.
          CALL "TRFNSTP"      USING WK-NSTP.
          IF   WK-NSTP-NONSTPCR = "Y"
               MOVE "N" TO  WS-OKAY
               MOVE "Y" TO  TAB-VAL(01)
               MOVE 0   TO  WS-JUMP
SQ1ARV         INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV         MOVE "RSN0038" TO WK-C-RPRCODE
SQ1ARV         PERFORM D400-PROCESS-RPRRSN
SQ1ARV               THRU D499-PROCESS-RPRRSN-EX
          ELSE
               MOVE "Y" TO  WS-OKAY
               MOVE "N" TO  TAB-VAL(01)
               MOVE 3   TO  WS-JUMP
          END-IF.
          PERFORM D300-LOGGING  THRU D399-LOGGING-EX.
      
              IF   WS-OKAY = "Y" AND WS-JUMP = 3
SM1TY1        IF   WK-VTF1B-RBK-IND NOT = "Y"
SGX201            IF    WK-SGX-GLACT-SW  = "Y"
SGX201                  AND  WK-C-TAG59-G-IND = "Y"
SGX201                  AND  WK-C-SGX-DAY2-SW = "Y"
SGX201                       PERFORM Y400-VALIDATE-GLACNO
SGX201                          THRU Y499-VALIDATE-GLACNO-EX
SGX201                       GO TO C101-VALIDATE-JUMP-4
SGX201            END-IF
                  MOVE WS-ACCNO         TO WK-C-VCFA-FCCA
               	  MOVE WS-ACCCUY        TO WK-C-VCFA-CUY
               	  CALL "TRFVCFA"     USING WK-C-VCFA-RECORD
               	  IF   WK-C-VCFA-ERROR-CD NOT = SPACES
               	       MOVE "N" TO  TAB-VAL(03)
               	       MOVE 4   TO  WS-JUMP
SQ2JE1*SQ1LN1  	             INITIALIZE WK-C-RPRRSN-AREA
SQ2JE1*SQ1LN1  	             PERFORM D600-PROCESS-ACC-ERR
SQ2JE1*SQ1LN1  	                  THRU D699-PROCESS-ACC-ERR-EX
                  ELSE
                       MOVE "Y" TO  TAB-VAL(03)
                                    WK-VTF1B-BENEFLG
                       MOVE 8   TO  WS-JUMP
                       MOVE WK-C-VCFA-CUSTFNAM TO WK-VTF1B-BENENAME
                       MOVE WK-C-VCFA-ADDR1     TO WK-VTF1B-BENEADR1
                       MOVE WK-C-VCFA-ADDR2     TO WK-VTF1B-BENEADR2
                       MOVE WK-C-VCFA-ADDR3     TO WK-VTF1B-BENEADR3
                       MOVE WK-C-VCFA-ADDR4     TO WK-VTF1B-BENEADR4
                       MOVE WK-C-VCFA-ADDR5     TO WK-VTF1B-BENEADR5
                       MOVE WK-C-VCFA-ADDR6     TO WK-VTF1B-BENEADR6
                       MOVE WK-C-VCFA-AOCD      TO WK-VTF1B-AOCD
                       MOVE WK-N-VCFA-RESCD      TO WK-VTF1B-RESCD
                       MOVE WK-N-VCFA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                       MOVE WK-N-VCFA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                       MOVE WK-N-VCFA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                       MOVE WK-N-VCFA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                       MOVE "FCCA"               TO WK-VTF1B-PMODE
OFX2A1                 MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCODE       
                  END-IF       
SM1TY1        ELSE       
SM1TY1            MOVE "N" TO  WS-OKAY       
SM1TY1            MOVE "X" TO  TAB-VAL(03)       
SM1TY1            MOVE 0   TO  WS-JUMP       
SM1TY1            INITIALIZE WK-C-RPRRSN-AREA       
SQ1JE1            MOVE "RSN0035" TO WK-C-RPRCODE       
SQ1ARV            PERFORM D400-PROCESS-RPRRSN       
SQ1ARV                 THRU D499-PROCESS-RPRRSN-EX       
SM1TY1        END-IF       
              PERFORM D300-LOGGING  THRU D399-LOGGING-EX       
           END-IF.
      
SGX201 C101-VALIDATE-JUMP-4.
          IF   WS-OKAY = "Y" AND WS-JUMP = 4
SM1TY1        IF   WK-VTF1B-RBK-IND NOT = "Y"
               MOVE WS-ACCNO         TO WK-ACRO-ACCNO
                                        WS-ACCNO-ORG
                                        WK-C-ACCNO
               MOVE WS-ACCCUY         TO WK-ACRO-CUYCD
                                         WS-ACCCUY-ORG
               CALL "TRFACRO"  USING WK-ACRO
               IF   WK-ACRO-RACIND NOT = "Y"
                OR  WK-ACRO-MCUYCD = SPACES
                    MOVE "N" TO  TAB-VAL(04)
                                 WS-ROUTE
                    MOVE 7   TO  WS-JUMP
               ELSE
                    IF   WK-C-ACCNO-4 = "1"
                         MOVE WS-ACCNO        TO WK-C-VCSA-SA-NO
                         CALL "TRFVCSA"  USING WK-C-VCSA-RECORD
                    END-IF
                    IF   WK-C-ACCNO-4 = "3"
                         MOVE WS-ACCNO        TO WK-C-VCCA-CA-NO
                         CALL "TRFVCCA"  USING WK-C-VCCA-RECORD
                    END-IF
                    IF   WK-C-ACCNO-4 NOT = "1"
                    AND WK-C-ACCNO-4 NOT = "3"
                         MOVE WS-ACCNO        TO WK-C-VCFA-FCCA
                         MOVE WK-ACRO-MCUYCD TO WK-C-VCFA-CUY
                         CALL "TRFVCFA"  USING WK-C-VCFA-RECORD
                    END-IF
                    IF   (WK-C-ACCNO-4              = "1"
                         AND WK-C-VCSA-ERROR-CD NOT = SPACES)
                     OR  (WK-C-ACCNO-4              = "3"
                         AND WK-C-VCCA-ERROR-CD NOT = SPACES)
                     OR  (WK-C-ACCNO-4              = "9"
                         AND WK-C-VCFA-ERROR-CD NOT = SPACES)
                         MOVE "N" TO  WS-OKAY  WS-ROUTE  TAB-VAL(05)
                         MOVE "Y" TO  TAB-VAL(04)
                         MOVE 0   TO  WS-JUMP
SQ1ARV                    INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                    IF   WK-C-ACCNO-4        = "1"
SQ2JE2                         MOVE WK-C-VCSA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                    END-IF
SQ2JE2                    IF   WK-C-ACCNO-4        = "3"
SQ2JE2                         MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                    END-IF
SQ2JE2                    IF   WK-C-ACCNO-4        = "9"
SQ2JE2                         MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                    END-IF
SQ2JE2                    MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                    PERFORM D400-PROCESS-RPRRSN
SQ2JE2                         THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV                  PERFORM D600-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                       THRU D699-PROCESS-ACC-ERR-EX
                    ELSE
                         MOVE "Y" TO  TAB-VAL(04)  TAB-VAL(05)
                                      WS-ROUTE
                         MOVE WK-ACRO-RACCNO     TO WS-ACCNO
                                                    WK-C-ACCNO
                         MOVE 6   TO  WS-JUMP
                    END-IF
               END-IF
SM1TY1        ELSE
SM1TY1            MOVE "N" TO  WS-OKAY
SM1TY1            MOVE "X" TO  TAB-VAL(04)
SM1TY1            MOVE 0   TO  WS-JUMP
SQ1ARV            INITIALIZE WK-C-RPRRSN-AREA
SQ1JE1            MOVE "RSN0035" TO WK-C-RPRCODE
SQ1ARV            PERFORM D400-PROCESS-RPRRSN
SQ1ARV                 THRU D499-PROCESS-RPRRSN-EX
SM1TY1        END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

          IF   WS-OKAY = "Y" AND WS-JUMP = 6
SM1TY1        IF   WK-VTF1B-RBK-IND NOT = "Y"
               MOVE WS-ACCNO         TO WK-C-VCFA-FCCA
               MOVE WS-ACCCUY        TO WK-C-VCFA-CUY
               CALL "TRFVCFA"     USING WK-C-VCFA-RECORD
               IF   WK-C-VCFA-ERROR-CD NOT = SPACES
                    MOVE "N" TO  TAB-VAL(06)
                    MOVE WS-ACCNO-ORG  TO WS-ACCNO
                    MOVE WS-ACCCUY-ORG TO WS-ACCCUY
                    MOVE 7   TO  WS-JUMP
SQ2JE1*SQ1LN1             INITIALIZE WK-C-RPRRSN-AREA
SQ2JE1*SQ1LN1             PERFORM D600-PROCESS-ACC-ERR
SQ2JE1*SQ1LN1                  THRU D699-PROCESS-ACC-ERR-EX
          ELSE
                    MOVE "Y" TO  TAB-VAL(06)
                                 WK-VTF1B-BENEFLG
                    MOVE 8   TO  WS-JUMP
                    MOVE WK-C-VCFA-CUSTFNAM TO WK-VTF1B-BENENAME
                    MOVE WK-C-VCFA-ADDR1     TO WK-VTF1B-BENEADR1
                    MOVE WK-C-VCFA-ADDR2     TO WK-VTF1B-BENEADR2
                    MOVE WK-C-VCFA-ADDR3     TO WK-VTF1B-BENEADR3
                    MOVE WK-C-VCFA-ADDR4     TO WK-VTF1B-BENEADR4
                    MOVE WK-C-VCFA-ADDR5     TO WK-VTF1B-BENEADR5
                    MOVE WK-C-VCFA-ADDR6     TO WK-VTF1B-BENEADR6
                    MOVE WK-C-VCFA-AOCD       TO WK-VTF1B-AOCD
                    MOVE WK-N-VCFA-RESCD      TO WK-VTF1B-RESCD
                    MOVE WK-N-VCFA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                    MOVE WK-N-VCFA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                    MOVE WK-N-VCFA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                    MOVE WK-N-VCFA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                    MOVE "FCCA"               TO WK-VTF1B-PMODE
                    MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCODE
                 END-IF
SM1TY1           ELSE
SM1TY1                   MOVE "N" TO  WS-OKAY
SM1TY1                   MOVE "X" TO  TAB-VAL(06)
SM1TY1                   MOVE 0   TO  WS-JUMP
SQ1ARV                   INITIALIZE WK-C-RPRRSN-AREA
SQ1JE1                   MOVE "RSN0035" TO WK-C-RPRCODE
SQ1ARV                   PERFORM D400-PROCESS-RPRRSN
SQ1ARV                        THRU D499-PROCESS-RPRRSN-EX
SM1TY1               END-IF
                      PERFORM D300-LOGGING  THRU D399-LOGGING-EX
                 END-IF.
      
                 IF   WS-OKAY = "Y" AND WS-JUMP = 7
SM1TY1               IF   WK-VTF1B-RBK-IND NOT = "Y"
                      MOVE WS-ACCNO         TO WK-C-ACCNO
                      IF   WK-C-ACCNO-4 = "1"
                           MOVE WS-ACCNO        TO WK-C-VCSA-SA-NO
                           CALL "TRFVCSA"  USING WK-C-VCSA-RECORD
                           IF   WK-C-VCSA-ERROR-CD  = SPACES
                                MOVE WK-C-VCSA-CUSTFNAM TO WK-VTF1B-BENENAME
                                MOVE WK-C-VCSA-ADDR1     TO WK-VTF1B-BENEADR1
                                MOVE WK-C-VCSA-ADDR2     TO WK-VTF1B-BENEADR2
                                MOVE WK-C-VCSA-ADDR3     TO WK-VTF1B-BENEADR3
                                MOVE WK-C-VCSA-ADDR4     TO WK-VTF1B-BENEADR4
                                MOVE WK-C-VCSA-ADDR5     TO WK-VTF1B-BENEADR5
                                MOVE WK-C-VCSA-ADDR6     TO WK-VTF1B-BENEADR6
                                MOVE WK-C-VCSA-AOCD      TO WK-VTF1B-AOCD
                                MOVE WK-N-VCSA-RESCD      TO WK-VTF1B-RESCD
                                MOVE WK-N-VCSA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                                MOVE WK-N-VCSA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                                MOVE WK-N-VCSA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                                MOVE WK-N-VCSA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                                MOVE "SA"                 TO WK-VTF1B-PMODE
OFX2A1                          MOVE WK-C-VCSA-SEG-CODE TO WK-C-SEGCODE
                      END-IF
                      ELSE
                           MOVE WS-ACCNO        TO WK-C-VCCA-CA-NO
                           CALL "TRFVCCA"  USING WK-C-VCCA-RECORD
                           IF   WK-C-VCCA-ERROR-CD  = SPACES
                                MOVE WK-C-VCCA-CUSTFNAM TO WK-VTF1B-BENENAME
                                MOVE WK-C-VCCA-ADDR1     TO WK-VTF1B-BENEADR1
                                MOVE WK-C-VCCA-ADDR2     TO WK-VTF1B-BENEADR2
                                MOVE WK-C-VCCA-ADDR3     TO WK-VTF1B-BENEADR3
                                MOVE WK-C-VCCA-ADDR4     TO WK-VTF1B-BENEADR4
                                MOVE WK-C-VCCA-ADDR5     TO WK-VTF1B-BENEADR5
                                MOVE WK-C-VCCA-ADDR6     TO WK-VTF1B-BENEADR6
                                MOVE WK-C-VCCA-AOCD      TO WK-VTF1B-AOCD
                                MOVE WK-N-VCCA-RESCD      TO WK-VTF1B-RESCD
                                MOVE WK-N-VCCA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                                MOVE WK-N-VCCA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                                MOVE WK-N-VCCA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                                MOVE WK-N-VCCA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                                MOVE "CA"                 TO WK-VTF1B-PMODE
OFX2A1                          MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCODE
                           END-IF
                      END-IF
                      IF (WK-C-ACCNO-4                = "1" AND
                          WK-C-VCSA-ERROR-CD NOT = SPACES) OR
                         (WK-C-ACCNO-4         NOT = "1" AND
                          WK-C-VCCA-ERROR-CD NOT = SPACES)
                           MOVE "N" TO  WS-OKAY
                           MOVE "N" TO  TAB-VAL(07)
                           MOVE 0   TO  WS-JUMP
SQ1ARV                 INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                 IF   WK-C-ACCNO-4        = "1"
SQ2JE2                      MOVE WK-C-VCSA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 END-IF
SQ2JE2                 IF   WK-C-ACCNO-4 NOT = "1"
SQ2JE2                      MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 END-IF
SQ2JE2                 MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                 PERFORM D400-PROCESS-RPRRSN
SQ2JE2                      THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV               PERFORM D500-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                    THRU D599-PROCESS-ACC-ERR-EX
               ELSE
REM269****               MOVE TFSCLSYS-LCUYCD    TO WS-ACCCUY
REM269                   MOVE WK-C-LCUYCD        TO WS-ACCCUY
REM269                   MOVE "Y" TO  TAB-VAL(07)
                                      WS-FXCONV
                                      WK-VTF1B-BENEFLG
                    MOVE 8   TO  WS-JUMP
               END-IF
SM1TY1        ELSE
SM1TY1            MOVE "N" TO  WS-OKAY
SM1TY1            MOVE "X" TO  TAB-VAL(07)
SM1TY1            MOVE 0   TO  WS-JUMP
SQ1ARV            INITIALIZE WK-C-RPRRSN-AREA
SQ1JE1            MOVE "RSN0035" TO WK-C-RPRCODE
SQ1ARV            PERFORM D400-PROCESS-RPRRSN
SQ1ARV                 THRU D499-PROCESS-RPRRSN-EX
SM1TY1        END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

          IF   WS-OKAY = "Y" AND WS-JUMP = 8
SGX201       IF   WK-SGX-GLACT-SW    = "Y"
SGX201       AND WK-C-VALID-GLACNO = "Y"
SGX201       AND WK-C-SGX-DAY2-SW  = "Y"
SGX201            MOVE "N" TO  TAB-VAL(08)
SGX201            MOVE 9   TO  WS-JUMP
SGX201       ELSE
SGX201       IF   WS-ACCCUY  = TFSCLSYS-LCUYCD
REM269****   IF   WS-ACCCUY  = WK-C-LCUYCD
REM269       IF   WS-ACCCUY  = WK-C-LCUYCD
               IF (WK-C-ACCNO-4                = "1" AND
                   WK-C-VCSA-ERROR-CD NOT = SPACES) OR
                  (WK-C-ACCNO-4         NOT = "1" AND
                   WK-C-VCCA-ERROR-CD NOT = SPACES)
                    MOVE "N" TO  WS-OKAY
                    MOVE "Y" TO  TAB-VAL(08)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                 INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                 IF   WK-C-ACCNO-4        = "1"
SQ2JE2                      MOVE WK-C-VCSA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 END-IF
SQ2JE2                 IF   WK-C-ACCNO-4 NOT = "1"
SQ2JE2                      MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 END-IF
SQ2JE2                 MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                 PERFORM D400-PROCESS-RPRRSN
SQ2JE2                      THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV               PERFORM D500-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                    THRU D599-PROCESS-ACC-ERR-EX
               ELSE
                    MOVE "N" TO  TAB-VAL(08)
                    MOVE 9   TO  WS-JUMP
               END-IF
          ELSE
               IF   WK-C-VCFA-ERROR-CD  NOT = SPACES
                    MOVE "N" TO  WS-OKAY
                    MOVE "Y" TO  TAB-VAL(08)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                 INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                 MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                 PERFORM D400-PROCESS-RPRRSN
SQ2JE2                      THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV               PERFORM D600-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                    THRU D699-PROCESS-ACC-ERR-EX
               ELSE
                    MOVE "N" TO  TAB-VAL(08)
                    MOVE 9   TO  WS-JUMP
               END-IF
          END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.
SGX201       END-IF

          IF   WS-OKAY = "Y" AND WS-JUMP = 9
          MOVE WS-ACCNO-ORG         TO WK-NSTP-ACCTBIC
          CALL "TRFNSTP"     USING WK-NSTP
          IF   WK-NSTP-NONSTPCR = "Y"
               MOVE "N" TO  WS-OKAY
               MOVE "Y" TO  TAB-VAL(09)
               MOVE 0   TO  WS-JUMP
SQ1ARV           INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV           MOVE "RSN0034" TO WK-C-RPRCODE
SQ1ARV           PERFORM D400-PROCESS-RPRRSN
SQ1ARV                THRU D499-PROCESS-RPRRSN-EX
          ELSE
               MOVE WS-ACCNO         TO WK-NSTP-ACCTBIC
               CALL "TRFNSTP"     USING WK-NSTP
               IF   WK-NSTP-NONSTPCR = "Y"
                    MOVE "N" TO  WS-OKAY
                    MOVE "Y" TO  TAB-VAL(09)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV                MOVE WS-ACCNO  TO WK-C-ACCNO-RPR
SQ1ARV                MOVE "RSN0034" TO WK-C-RPRCODE
SQ1ARV                PERFORM D400-PROCESS-RPRRSN
SQ1ARV                     THRU D499-PROCESS-RPRRSN-EX
               ELSE
                    MOVE WS-BANKID         TO WK-NSTP-ACCTBIC
                    CALL "TRFNSTP"     USING WK-NSTP
                    IF   WK-NSTP-NONSTPCR = "Y"
                         MOVE "N" TO  WS-OKAY
                         MOVE "Y" TO  TAB-VAL(09)
                         MOVE 0   TO  WS-JUMP
SQ1ARV                     INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV                     MOVE "RSN0038" TO WK-C-RPRCODE
SQ1ARV                     PERFORM D400-PROCESS-RPRRSN
SQ1ARV                          THRU D499-PROCESS-RPRRSN-EX
                    ELSE
                         MOVE "N" TO  TAB-VAL(09)
                         MOVE 14  TO  WS-JUMP
                    END-IF
               END-IF
          END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

     C199-VALIDATION-PART-EX.
          EXIT.
     EJECT

     C200-VALIDATION-PART.
          MOVE WS-BANKID            TO WK-NSTP-ACCTBIC.
          CALL "TRFNSTP"      USING WK-NSTP.
          IF   WK-NSTP-NONSTPCR = "Y"
               MOVE "N" TO  WS-OKAY
               MOVE "Y" TO  TAB-VAL(01)
               MOVE 0   TO  WS-JUMP
SQ1ARV           INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV           MOVE "RSN0007" TO WK-C-RPRCODE
SQ1ARV           PERFORM D400-PROCESS-RPRRSN
SQ1ARV                THRU D499-PROCESS-RPRRSN-EX
          ELSE
               MOVE "N" TO  TAB-VAL(01)
               MOVE "Y" TO  WS-OKAY
               MOVE 2   TO  WS-JUMP
          END-IF.
          PERFORM D300-LOGGING  THRU D399-LOGGING-EX.

          IF   WS-OKAY = "Y" AND WS-JUMP = 2
          MOVE TFSSTPL-BNKENTTY  TO WK-N-VBAC-BNKENTTY
          MOVE WS-BANKID         TO WK-C-VBAC-BANKID
          MOVE WS-ACCCUY         TO WK-C-VBAC-CUYCD
          CALL "TRFVBAC"   USING WK-C-VBAC-RECORD
          IF   WK-C-VBAC-ERROR-CD = SPACES
               MOVE "Y" TO  TAB-VAL(02)
               MOVE 3   TO  WS-JUMP
               MOVE WK-C-VBAC-ACUDBUI TO WS-ACUDBUI
               MOVE WK-C-VBAC-ACCTYP  TO WS-ACCTYP
               MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
          ELSE
               MOVE "N" TO  WS-OKAY
               MOVE "N" TO  TAB-VAL(02)
               MOVE 0   TO  WS-JUMP
SQ1ARV           INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV           MOVE WK-C-VBAC-ERROR-CD  TO WK-C-RPRCODE
SQ1ARV           PERFORM D400-PROCESS-RPRRSN
SQ1ARV                THRU D499-PROCESS-RPRRSN-EX
          END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

          IF   WS-OKAY = "Y" AND WS-JUMP = 3
SM1TY1        IF   WK-VTF1B-RBK-IND NOT = "Y"
               MOVE WS-ACCNO         TO WK-C-VCFA-FCCA
               MOVE WS-ACCCUY        TO WK-C-VCFA-CUY
               CALL "TRFVCFA"     USING WK-C-VCFA-RECORD
               IF   WK-C-VCFA-ERROR-CD NOT = SPACES
                    MOVE "N" TO  WS-OKAY
                    MOVE "N" TO  TAB-VAL(03)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                PERFORM D400-PROCESS-RPRRSN
SQ2JE2                     THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV              PERFORM D600-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                   THRU D699-PROCESS-ACC-ERR-EX
               ELSE
                    MOVE "Y" TO  TAB-VAL(03)
                                 WK-VTF1B-BENEFLG
                    MOVE 8   TO  WS-JUMP
                    MOVE WK-C-VCFA-CUSTFNAM TO WK-VTF1B-BENENAME
                    MOVE WK-C-VCFA-ADDR1     TO WK-VTF1B-BENEADR1
                    MOVE WK-C-VCFA-ADDR2     TO WK-VTF1B-BENEADR2
                    MOVE WK-C-VCFA-ADDR3     TO WK-VTF1B-BENEADR3
                    MOVE WK-C-VCFA-ADDR4     TO WK-VTF1B-BENEADR4
                    MOVE WK-C-VCFA-ADDR5     TO WK-VTF1B-BENEADR5
                    MOVE WK-C-VCFA-ADDR6     TO WK-VTF1B-BENEADR6
                    MOVE WK-C-VCFA-AOCD       TO WK-VTF1B-AOCD
                    MOVE WK-N-VCFA-RESCD      TO WK-VTF1B-RESCD
                    MOVE WK-N-VCFA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                    MOVE WK-N-VCFA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                    MOVE WK-N-VCFA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                    MOVE WK-N-VCFA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                    MOVE "FCCA"               TO WK-VTF1B-PMODE
               END-IF
SM1TY1        ELSE
SM1TY1            MOVE "N" TO  WS-OKAY
SM1TY1            MOVE "X" TO  TAB-VAL(03)
SM1TY1            MOVE 0   TO  WS-JUMP
SQ1ARV            INITIALIZE WK-C-RPRRSN-AREA
SQ1JE1            MOVE "RSN0035" TO WK-C-RPRCODE
SQ1ARV            PERFORM D400-PROCESS-RPRRSN
SQ1ARV                 THRU D499-PROCESS-RPRRSN-EX
SM1TY1        END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

          IF   WS-OKAY = "Y" AND WS-JUMP = 8
          IF   WK-C-VCFA-ERROR-CD NOT = SPACE
               MOVE "N" TO  WS-OKAY
               MOVE "Y" TO  TAB-VAL(08)
               MOVE 0   TO  WS-JUMP
SQ1ARV           INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2           MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2           MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2           PERFORM D400-PROCESS-RPRRSN
SQ2JE2                THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV         PERFORM D600-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV              THRU D699-PROCESS-ACC-ERR-EX
          ELSE
               MOVE "N" TO  TAB-VAL(08)
               MOVE 9   TO  WS-JUMP
          END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

          IF   WS-OKAY = "Y" AND WS-JUMP = 9
          MOVE WS-ACCNO-ORG         TO WK-NSTP-ACCTBIC
          CALL "TRFNSTP"     USING WK-NSTP
          IF   WK-NSTP-NONSTPCR = "Y"
               MOVE "N" TO  WS-OKAY
               MOVE "Y" TO  TAB-VAL(09)
               MOVE 0   TO  WS-JUMP
SQ1ARV           INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV           MOVE "RSN0034" TO WK-C-RPRCODE
SQ1ARV           PERFORM D400-PROCESS-RPRRSN
SQ1ARV                THRU D499-PROCESS-RPRRSN-EX
          ELSE
               MOVE WS-ACCNO         TO WK-NSTP-ACCTBIC
               CALL "TRFNSTP"     USING WK-NSTP
               IF   WK-NSTP-NONSTPCR = "Y"
                    MOVE "N" TO  WS-OKAY
                    MOVE "Y" TO  TAB-VAL(09)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV                MOVE WS-ACCNO  TO WK-C-ACCNO-RPR
SQ1ARV                MOVE "RSN0038" TO WK-C-RPRCODE
SQ1ARV                PERFORM D400-PROCESS-RPRRSN
SQ1ARV                     THRU D499-PROCESS-RPRRSN-EX
                      ELSE
                           MOVE WS-BANKID         TO WK-NSTP-ACCTBIC
                           CALL "TRFNSTP"     USING WK-NSTP
                           IF   WK-NSTP-NONSTPCR = "Y"
                                MOVE "N" TO  WS-OKAY
                                MOVE "Y" TO  TAB-VAL(09)
                                MOVE 0   TO  WS-JUMP
SQ1ARV                     INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV                     MOVE "RSN0038" TO WK-C-RPRCODE
SQ1ARV                     PERFORM D400-PROCESS-RPRRSN
SQ1ARV                          THRU D499-PROCESS-RPRRSN-EX
                           ELSE
                                MOVE "N" TO  TAB-VAL(09)
                                MOVE 14  TO  WS-JUMP
                           END-IF
                      END-IF
                 END-IF
                      PERFORM D300-LOGGING  THRU D399-LOGGING-EX
                 END-IF.
      
            C299-VALIDATION-PART-EX.
                 EXIT.
            EJECT
      
            C300-VALIDATION-PART.
SM1TY1       IF   WK-VTF1B-RBK-IND NOT = "Y"
SM1TY1*      MOVE WS-ACCNO         TO WK-C-VCFA-FCCA.
SM1TY1*      MOVE WS-ACCCUY        TO WK-C-VCFA-CUY.
SM1TY1*      CALL "TRFVCFA"     USING WK-C-VCFA-RECORD.
SGX201       IF   WK-SGX-GLACT-SW  = "Y"
SGX201       AND  WK-C-TAG59-G-IND = "Y"
SGX201       AND  WK-C-SGX-DAY2-SW = "Y"
SGX201            PERFORM Y400-VALIDATE-GLACNO
SGX201                 THRU Y499-VALIDATE-GLACNO-EX
SGX201            GO TO C301-VALIDATE-JUMP-4
SGX201       END-IF
SM1TY1       MOVE WS-ACCNO         TO WK-C-VCFA-FCCA
SM1TY1       MOVE WS-ACCCUY        TO WK-C-VCFA-CUY
SM1TY1       CALL "TRFVCFA"     USING WK-C-VCFA-RECORD
                    IF   WK-C-VCFA-ERROR-CD NOT = SPACES
                         MOVE "Y" TO  WS-OKAY
                         MOVE "N" TO  TAB-VAL(03)
                         MOVE 4   TO  WS-JUMP
SQ2JE1*SQ1LN1          INITIALIZE WK-C-RPRRSN-AREA
SQ2JE1*SQ1LN1          PERFORM D600-PROCESS-ACC-ERR
SQ2JE1*SQ1LN1               THRU D699-PROCESS-ACC-ERR-EX
             ELSE
                         MOVE "Y" TO  WS-OKAY
                                      TAB-VAL(03)
                                      WK-VTF1B-BENEFLG
                         MOVE 8   TO  WS-JUMP
                         MOVE WK-C-VCFA-CUSTFNAM TO WK-VTF1B-BENENAME
                         MOVE WK-C-VCFA-ADDR1     TO WK-VTF1B-BENEADR1
                         MOVE WK-C-VCFA-ADDR2     TO WK-VTF1B-BENEADR2
                         MOVE WK-C-VCFA-ADDR3     TO WK-VTF1B-BENEADR3
                         MOVE WK-C-VCFA-ADDR4     TO WK-VTF1B-BENEADR4
                         MOVE WK-C-VCFA-ADDR5     TO WK-VTF1B-BENEADR5
                         MOVE WK-C-VCFA-ADDR6     TO WK-VTF1B-BENEADR6
                         MOVE WK-C-VCFA-AOCD       TO WK-VTF1B-AOCD
                         MOVE WK-N-VCFA-RESCD      TO WK-VTF1B-RESCD
                         MOVE WK-N-VCFA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                         MOVE WK-N-VCFA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                         MOVE WK-N-VCFA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                         MOVE WK-N-VCFA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                         MOVE "FCCA"               TO WK-VTF1B-PMODE
 OFX2A1            MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCODE
              END-IF
 SM1TY1       ELSE
 SM1TY1            MOVE "N" TO  WS-OKAY
 SM1TY1            MOVE "X" TO  TAB-VAL(03)
 SM1TY1            MOVE 0   TO  WS-JUMP
 SQ1ARV            INITIALIZE WK-C-RPRRSN-AREA
 SQ1JE1            MOVE "RSN0035" TO WK-C-RPRCODE
 SQ1ARV            PERFORM D400-PROCESS-RPRRSN
 SQ1ARV                 THRU D499-PROCESS-RPRRSN-EX
              END-IF.
              PERFORM D300-LOGGING  THRU D399-LOGGING-EX.

 SGX201 C301-VALIDATE-JUMP-4.
           IF   WS-OKAY = "Y" AND WS-JUMP = 4
 SM1TY1        IF   WK-VTF1B-RBK-IND NOT = "Y"
                MOVE WS-ACCNO         TO WK-ACRO-ACCNO
                                               WS-ACCNO-ORG
                                               WK-C-ACCNO
                      MOVE WS-ACCCUY         TO WK-ACRO-CUYCD
                                                WS-ACCCUY-ORG
                      CALL "TRFACRO"  USING WK-ACRO
                      IF   WK-ACRO-RACIND NOT = "Y"
                       OR  WK-ACRO-MCUYCD = SPACES
                           MOVE "N" TO  TAB-VAL(04)
                                        WS-ROUTE
                           MOVE 7   TO  WS-JUMP
                      ELSE
                           IF   WK-C-ACCNO-4 = "1"
                                MOVE WS-ACCNO        TO WK-C-VCSA-SA-NO
                                CALL "TRFVCSA"  USING WK-C-VCSA-RECORD
                           END-IF
                           IF   WK-C-ACCNO-4 = "3"
                                MOVE WS-ACCNO        TO WK-C-VCCA-CA-NO
                                CALL "TRFVCCA"  USING WK-C-VCCA-RECORD
                           END-IF
                           IF   WK-C-ACCNO-4 NOT = "1"
                           AND WK-C-ACCNO-4 NOT = "3"
                                MOVE WS-ACCNO        TO WK-C-VCFA-FCCA
                                MOVE WK-ACRO-MCUYCD TO WK-C-VCFA-CUY
                                CALL "TRFVCFA"  USING WK-C-VCFA-RECORD
                           END-IF
                           IF   (WK-C-ACCNO-4              = "1"
                                AND WK-C-VCSA-ERROR-CD NOT = SPACES)
                            OR  (WK-C-ACCNO-4              = "3"
                                AND WK-C-VCCA-ERROR-CD NOT = SPACES)
                            OR  (WK-C-ACCNO-4              = "9"
                                AND WK-C-VCFA-ERROR-CD NOT = SPACES)
                                MOVE "N" TO  WS-OKAY  WS-ROUTE  TAB-VAL(05)
                                MOVE "Y" TO  TAB-VAL(04)
                                MOVE 0   TO  WS-JUMP
SQ2JE2                    INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                    IF   WK-C-ACCNO-4        = "1"
SQ2JE2                         MOVE WK-C-VCSA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                    END-IF
SQ2JE2                    IF   WK-C-ACCNO-4        = "3"
SQ2JE2                         MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                    END-IF
SQ2JE2                    IF   WK-C-ACCNO-4        = "9"
SQ2JE2                         MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                    END-IF
SQ2JE2                    MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                    PERFORM D400-PROCESS-RPRRSN
SQ2JE2                         THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV                  PERFORM D600-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                       THRU D699-PROCESS-ACC-ERR-EX
                    ELSE
                         MOVE "Y" TO  TAB-VAL(04)  TAB-VAL(05)
                                      WS-ROUTE
                         MOVE WK-ACRO-RACCNO     TO WS-ACCNO
                                                    WK-C-ACCNO

                         MOVE 6   TO  WS-JUMP
                    END-IF
               END-IF
SM1TY1        ELSE
SM1TY1            MOVE "N" TO  WS-OKAY
SM1TY1            MOVE "X" TO  TAB-VAL(04)
SM1TY1            MOVE 0   TO  WS-JUMP
SQ1ARV            INITIALIZE WK-C-RPRRSN-AREA
SQ1JE1            MOVE "RSN0035" TO WK-C-RPRCODE
SQ1ARV            PERFORM D400-PROCESS-RPRRSN
SQ1ARV                 THRU D499-PROCESS-RPRRSN-EX
SM1TY1        END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

          IF   WS-OKAY = "Y" AND WS-JUMP = 6
SM1TY1        IF   WK-VTF1B-RBK-IND NOT = "Y"
               MOVE WS-ACCNO         TO WK-C-VCFA-FCCA
               MOVE WS-ACCCUY        TO WK-C-VCFA-CUY
               CALL "TRFVCFA"     USING WK-C-VCFA-RECORD
               IF   WK-C-VCFA-ERROR-CD NOT = SPACES
                    MOVE "N" TO  TAB-VAL(06)
                    MOVE WS-ACCNO-ORG  TO WS-ACCNO
                    MOVE WS-ACCCUY-ORG TO WS-ACCCUY
                    MOVE 7   TO  WS-JUMP
SQ2JE1*SQ1LN1             INITIALIZE WK-C-RPRRSN-AREA
SQ2JE1*SQ1LN1             PERFORM D600-PROCESS-ACC-ERR
SQ2JE1*SQ1LN1                  THRU D699-PROCESS-ACC-ERR-EX
               ELSE
                    MOVE "Y" TO  TAB-VAL(06)
                                 WK-VTF1B-BENEFLG
                    MOVE 8   TO  WS-JUMP
                    MOVE WK-C-VCFA-CUSTFNAM TO WK-VTF1B-BENENAME
                    MOVE WK-C-VCFA-ADDR1     TO WK-VTF1B-BENEADR1
                    MOVE WK-C-VCFA-ADDR2     TO WK-VTF1B-BENEADR2
                    MOVE WK-C-VCFA-ADDR3     TO WK-VTF1B-BENEADR3
                    MOVE WK-C-VCFA-ADDR4     TO WK-VTF1B-BENEADR4
                    MOVE WK-C-VCFA-ADDR5     TO WK-VTF1B-BENEADR5
                    MOVE WK-C-VCFA-ADDR6     TO WK-VTF1B-BENEADR6
                    MOVE WK-C-VCFA-AOCD       TO WK-VTF1B-AOCD
                    MOVE WK-N-VCFA-RESCD      TO WK-VTF1B-RESCD
                    MOVE WK-N-VCFA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                    MOVE WK-N-VCFA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                    MOVE WK-N-VCFA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                    MOVE WK-N-VCFA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                    MOVE "FCCA"               TO WK-VTF1B-PMODE
OFX2A1                MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCODE
               END-IF
SM1TY1        ELSE
SM1TY1            MOVE "N" TO  WS-OKAY
SM1TY1            MOVE "X" TO  TAB-VAL(06)
SM1TY1            MOVE 0   TO  WS-JUMP
SQ1ARV            INITIALIZE WK-C-RPRRSN-AREA
SQ1JE1            MOVE "RSN0035" TO WK-C-RPRCODE
SQ1ARV            PERFORM D400-PROCESS-RPRRSN
SQ1ARV                 THRU D499-PROCESS-RPRRSN-EX
SM1TY1        END-IF
               PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

          IF   WS-OKAY = "Y" AND WS-JUMP = 7
SM1TY1        IF   WK-VTF1B-RBK-IND NOT = "Y"
               MOVE WS-ACCNO         TO WK-C-ACCNO
               IF   WK-C-ACCNO-4 = "1"
                    MOVE WS-ACCNO        TO WK-C-VCSA-SA-NO
                    CALL "TRFVCSA"     USING WK-C-VCSA-RECORD
                    IF   WK-C-VCSA-ERROR-CD  = SPACES
                         MOVE WK-C-VCSA-CUSTFNAM TO WK-VTF1B-BENENAME
                         MOVE WK-C-VCSA-ADDR1     TO WK-VTF1B-BENEADR1
                         MOVE WK-C-VCSA-ADDR2     TO WK-VTF1B-BENEADR2
                         MOVE WK-C-VCSA-ADDR3     TO WK-VTF1B-BENEADR3
                         MOVE WK-C-VCSA-ADDR4     TO WK-VTF1B-BENEADR4
                         MOVE WK-C-VCSA-ADDR5     TO WK-VTF1B-BENEADR5
                         MOVE WK-C-VCSA-ADDR6     TO WK-VTF1B-BENEADR6
                         MOVE WK-C-VCSA-AOCD      TO WK-VTF1B-AOCD
                         MOVE WK-N-VCSA-RESCD      TO WK-VTF1B-RESCD
                         MOVE WK-N-VCSA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                         MOVE WK-N-VCSA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                         MOVE WK-N-VCSA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                         MOVE WK-N-VCSA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                         MOVE "SA"                 TO WK-VTF1B-PMODE
OFX2A1                    MOVE WK-C-VCSA-SEG-CODE TO WK-C-SEGCODE
                    END-IF
               ELSE
                    MOVE WS-ACCNO        TO WK-C-VCCA-CA-NO
                    CALL "TRFVCCA"  USING WK-C-VCCA-RECORD
                    IF   WK-C-VCCA-ERROR-CD  = SPACES
                         MOVE WK-C-VCCA-CUSTFNAM TO WK-VTF1B-BENENAME
                         MOVE WK-C-VCCA-ADDR1     TO WK-VTF1B-BENEADR1
                         MOVE WK-C-VCCA-ADDR2     TO WK-VTF1B-BENEADR2
                         MOVE WK-C-VCCA-ADDR3     TO WK-VTF1B-BENEADR3
                         MOVE WK-C-VCCA-ADDR4     TO WK-VTF1B-BENEADR4
                         MOVE WK-C-VCCA-ADDR5     TO WK-VTF1B-BENEADR5
                         MOVE WK-C-VCCA-ADDR6     TO WK-VTF1B-BENEADR6
                         MOVE WK-C-VCCA-AOCD      TO WK-VTF1B-AOCD
                         MOVE WK-N-VCCA-RESCD      TO WK-VTF1B-RESCD
                         MOVE WK-N-VCCA-DOMBRCH    TO WK-VTF1B-DOMBRCH
                         MOVE WK-N-VCCA-HOLDCD1    TO WK-VTF1B-HOLDCD1
                         MOVE WK-N-VCCA-HOLDCD2    TO WK-VTF1B-HOLDCD2
                         MOVE WK-N-VCCA-HOLDCD3    TO WK-VTF1B-HOLDCD3
                         MOVE "CA"                 TO WK-VTF1B-PMODE
OFX2A1                    MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCODE
                    END-IF
               END-IF
               IF (WK-C-ACCNO-4                = "1" AND
                   WK-C-VCSA-ERROR-CD NOT = SPACES) OR
                  (WK-C-ACCNO-4         NOT = "1" AND
                   WK-C-VCCA-ERROR-CD NOT = SPACES)
                    MOVE "N" TO  WS-OKAY
                    MOVE "N" TO  TAB-VAL(07)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                 INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                 IF   WK-C-ACCNO-4        = "1"
SQ2JE2                      MOVE WK-C-VCSA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 END-IF
SQ2JE2                 IF   WK-C-ACCNO-4 NOT = "1"
SQ2JE2                      MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 END-IF
SQ2JE2                 MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                 PERFORM D400-PROCESS-RPRRSN
SQ2JE2                      THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV               PERFORM D500-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                    THRU D599-PROCESS-ACC-ERR-EX
               ELSE
REM269****               MOVE TFSCLSYS-LCUYCD    TO WS-ACCCUY
REM269                   MOVE WK-C-LCUYCD        TO WS-ACCCUY
REM269                   MOVE "Y" TO  TAB-VAL(07)
                                      WS-FXCONV
                                      WK-VTF1B-BENEFLG
                    MOVE 8   TO  WS-JUMP
               END-IF
SM1TY1        ELSE
SM1TY1            MOVE "N" TO  WS-OKAY
SM1TY1            MOVE "X" TO  TAB-VAL(07)
SM1TY1            MOVE 0   TO  WS-JUMP
SQ1ARV            INITIALIZE WK-C-RPRRSN-AREA
SQ1JE1            MOVE "RSN0035" TO WK-C-RPRCODE
SQ1ARV            PERFORM D400-PROCESS-RPRRSN
SQ1ARV                 THRU D499-PROCESS-RPRRSN-EX
SM1TY1        END-IF
                PERFORM D300-LOGGING  THRU D399-LOGGING-EX
           END-IF.
 
           IF   WS-OKAY = "Y" AND WS-JUMP = 8
SGX201       IF   WK-SGX-GLACT-SW    = "Y"
SGX201       AND WK-C-VALID-GLACNO = "Y"
SGX201       AND WK-C-SGX-DAY2-SW  = "Y"
SGX201            MOVE "N" TO  TAB-VAL(08)
SGX201            MOVE 9   TO  WS-JUMP
SGX201       ELSE
SGX201       IF   WS-ACCCUY  = TFSCLSYS-LCUYCD
REM269****   IF   WS-ACCCUY  = WK-C-LCUYCD
REM269       IF   WS-ACCCUY  = WK-C-LCUYCD
                IF (WK-C-ACCNO-4                = "1" AND
                   WK-C-VCSA-ERROR-CD NOT = SPACES) OR
                  (WK-C-ACCNO-4         NOT = "1" AND
                   WK-C-VCCA-ERROR-CD NOT = SPACES)
                    MOVE "N" TO  WS-OKAY
                    MOVE "Y" TO  TAB-VAL(08)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                 INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                 IF   WK-C-ACCNO-4        = "1"
SQ2JE2                      MOVE WK-C-VCSA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 END-IF
SQ2JE2                 IF   WK-C-ACCNO-4 NOT = "1"
SQ2JE2                      MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 END-IF
SQ2JE2                 MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                 PERFORM D400-PROCESS-RPRRSN
SQ2JE2                      THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV               PERFORM D500-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                    THRU D599-PROCESS-ACC-ERR-EX
               ELSE
                    MOVE "N" TO  TAB-VAL(08)
                    MOVE 9   TO  WS-JUMP
               END-IF
          ELSE
               IF   WK-C-VCFA-ERROR-CD  NOT = SPACES
                    MOVE "N" TO  WS-OKAY
                    MOVE "Y" TO  TAB-VAL(08)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                 INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                 MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                 MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                 PERFORM D400-PROCESS-RPRRSN
SQ2JE2                      THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV               PERFORM D600-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                    THRU D699-PROCESS-ACC-ERR-EX
               ELSE
                     MOVE "N" TO  TAB-VAL(08)
                     MOVE 9   TO  WS-JUMP
                END-IF
           END-IF
SGX201       END-IF
                PERFORM D300-LOGGING  THRU D399-LOGGING-EX
           END-IF.
 
           IF   WS-OKAY = "Y" AND WS-JUMP = 9
           IF   WK-SGX-GLACT-SW    = "Y"
           AND WK-C-VALID-GLACNO = "Y"
           AND WK-C-SGX-DAY2-SW  = "Y"
                MOVE "Y"   TO  TAB-VAL(09)
                MOVE 10   TO  WS-JUMP
           ELSE
           MOVE WS-ACCNO-ORG         TO WK-NSTP-ACCTBIC
           CALL "TRFNSTP"     USING WK-NSTP
           IF   WK-NSTP-NONSTPCR = "Y"
                MOVE "N" TO  WS-OKAY
                MOVE "Y" TO  TAB-VAL(09)
                MOVE 0   TO  WS-JUMP
SQ1ARV           INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV           MOVE "RSN0034" TO WK-C-RPRCODE
SQ1ARV           PERFORM D400-PROCESS-RPRRSN
SQ1ARV                THRU D499-PROCESS-RPRRSN-EX
           ELSE
                MOVE WS-ACCNO        TO WK-NSTP-ACCTBIC
                CALL "TRFNSTP"     USING WK-NSTP
                IF   WK-NSTP-NONSTPCR = "Y"
                     MOVE "N" TO  WS-OKAY
                     MOVE "Y" TO  TAB-VAL(09)
                     MOVE 0   TO  WS-JUMP
SQ1ARV                INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV                MOVE WS-ACCNO  TO WK-C-ACCNO-RPR
SQ1ARV                MOVE "RSN0034" TO WK-C-RPRCODE
SQ1ARV                PERFORM D400-PROCESS-RPRRSN
SQ1ARV                     THRU D499-PROCESS-RPRRSN-EX
                ELSE
                     MOVE "N" TO  TAB-VAL(09)
                     MOVE 14   TO  WS-JUMP
                END-IF
           END-IF
                PERFORM D300-LOGGING  THRU D399-LOGGING-EX
           END-IF
           END-IF
 
SGX201       END-IF
          PERFORM D300-LOGGING  THRU D399-LOGGING-EX
          END-IF.

          IF   WS-OKAY = "Y" AND WS-JUMP = 10
SGX201       IF   WK-SGX-GLACT-SW    = "Y"
SGX201       AND WK-C-VALID-GLACNO = "Y"
SGX201       AND WK-C-SGX-DAY2-SW  = "Y"
SGX201            MOVE "Y"   TO  TAB-VAL(10)
SGX201            MOVE "N"   TO WK-VTF1B-N0910
SGX201            MOVE 14    TO WS-JUMP
SGX201            MOVE SPACE     TO WS-ACCTYP
SGX201       ELSE
SGX201       MOVE TFSSTPL-BNKENTTY  TO WK-N-VBANO-BNKENTTY
SGX201       MOVE WS-BANKID         TO WK-C-VBANO-BANKID
SGX201       MOVE WS-ACCCUY         TO WK-C-VBANO-CUYCD
SGX201       MOVE WS-ACCNO          TO WK-C-VBANO-ACCNO
              CALL "TRFVBANO"  USING WK-C-VBANO-RECORD
              IF   WK-C-VBANO-ERROR-CD NOT = SPACES
                   MOVE "N" TO  TAB-VAL(10)
                   MOVE "Y" TO  WK-VTF1B-N0910
                   MOVE 15  TO  WS-JUMP
SQ1ARV                INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV                MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ1ARV                MOVE WK-C-VBANO-ERROR-CD TO WK-C-RPRCODE
SQ1ARV                PERFORM D400-PROCESS-RPRRSN
SQ1ARV                     THRU D499-PROCESS-RPRRSN-EX
              ELSE
                   MOVE "Y" TO  TAB-VAL(10)
                   MOVE "N" TO  WK-VTF1B-N0910
                   MOVE 14  TO  WS-JUMP
                   MOVE WK-C-VBANO-ACUDBUI TO WS-ACUDBUI
                   MOVE WK-C-VBANO-ACCTYP  TO WS-ACCTYP
              END-IF
SGX201       END-IF
              PERFORM D300-LOGGING  THRU D399-LOGGING-EX
           END-IF.
 
     C199-VALIDATION-PART-EX.
          EXIT.
     EJECT

     C200-VALIDATION-PART.
          MOVE WS-BANKID            TO WK-NSTP-ACCTBIC.
          CALL "TRFNSTP"      USING WK-NSTP.
          IF   WK-NSTP-NONSTPCR = "Y"
               MOVE "N" TO  WS-OKAY
               MOVE "Y" TO  TAB-VAL(01)
               MOVE 0   TO  WS-JUMP
SQ1ARV           INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV           MOVE "RSN0007" TO WK-C-RPRCODE
SQ1ARV           PERFORM D400-PROCESS-RPRRSN
SQ1ARV                THRU D499-PROCESS-RPRRSN-EX
           ELSE
                MOVE "N" TO  TAB-VAL(01)
                MOVE "Y" TO  WS-OKAY
                MOVE 2   TO  WS-JUMP
           END-IF.
           PERFORM D300-LOGGING  THRU D399-LOGGING-EX.
 
           IF   WS-OKAY = "Y" AND WS-JUMP = 2
           MOVE TFSSTPL-BNKENTTY  TO WK-N-VBAC-BNKENTTY
           MOVE WS-BANKID         TO WK-C-VBAC-BANKID
           MOVE WS-ACCCUY         TO WK-C-VBAC-CUYCD
           CALL "TRFVBAC"   USING WK-C-VBAC-RECORD
           IF   WK-C-VBAC-ERROR-CD = SPACES
                MOVE "Y" TO  TAB-VAL(02)
                MOVE 3   TO  WS-JUMP
                MOVE WK-C-VBAC-ACUDBUI TO WS-ACUDBUI
                MOVE WK-C-VBAC-ACCTYP  TO WS-ACCTYP
                MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
           ELSE
                MOVE "N" TO  WS-OKAY
                MOVE "N" TO  TAB-VAL(02)
                MOVE 0   TO  WS-JUMP
SQ1ARV           INITIALIZE WK-C-RPRRSN-AREA
SQ1ARV           MOVE WK-C-VBAC-ERROR-CD  TO WK-C-RPRCODE
SQ1ARV           PERFORM D400-PROCESS-RPRRSN
SQ1ARV                THRU D499-PROCESS-RPRRSN-EX
           END-IF
                PERFORM D300-LOGGING  THRU D399-LOGGING-EX
           END-IF.
 
          IF   WS-OKAY = "Y" AND WS-JUMP = 3
SM1TY1       IF   WK-VTF1B-RBK-IND NOT = "Y"
               MOVE WS-ACCNO         TO WK-C-VCFA-FCCA
               MOVE WS-ACCCUY        TO WK-C-VCFA-CUY
               CALL "TRFVCFA"     USING WK-C-VCFA-RECORD
               IF   WK-C-VCFA-ERROR-CD NOT = SPACES
                    MOVE "N" TO  WS-OKAY
                    MOVE "N" TO  TAB-VAL(03)
                    MOVE 0   TO  WS-JUMP
SQ1ARV                INITIALIZE WK-C-RPRRSN-AREA
SQ2JE2                MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
SQ2JE2                MOVE WS-ACCNO        TO WK-C-ACCNO-RPR
SQ2JE2                PERFORM D400-PROCESS-RPRRSN
SQ2JE2                     THRU D499-PROCESS-RPRRSN-EX
SQ2JE2*SQ1ARV              PERFORM D600-PROCESS-ACC-ERR
SQ2JE2*SQ1ARV                   THRU D699-PROCESS-ACC-ERR-EX
               ELSE
                    MOVE "Y" TO  TAB-VAL(03)
                                 WK-VTF1B-BENEFLG
                    MOVE 8   TO  WS-JUMP
                    MOVE WK-C-VCFA-CUSTFNAM TO WK-VTF1B-BENENAME
                    MOVE WK-C-VCFA-ADDR1     TO WK-VTF1B-BENEADR1
                    MOVE WK-C-VCFA-ADDR2     TO WK-VTF1B-BENEADR2
                    MOVE WK-C-VCFA-ADDR3     TO WK-VTF1B-BENEADR3
                    MOVE WK-C-VCFA-ADDR4     TO WK-VTF1B-BENEADR4
 
 		    MOVE "Y" TO  TAB-VAL(08)
 		    MOVE 0   TO  WS-JUMP
5Q1ARV     INITIALIZE WK-C-RPRRSN-AREA
5Q2JE2     MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
5Q2JE2     MOVE WS-ACCNO           TO WK-C-ACCNO-RPR
5Q2JE2     PERFORM D400-PROCESS-RPRRSN
5Q2JE2     THRU D499-PROCESS-RPRRSN-EX
5Q2JE2*5Q1ARV        PERFORM D600-PROCESS-ACC-ERR
5Q2JE2*5Q1ARV        THRU D699-PROCESS-ACC-ERR-EX
	   ELSE
	       MOVE "N" TO  TAB-VAL(08)
	       MOVE 9   TO  WS-JUMP
	   END-IF
       END-IF
SGX201     END-IF
	   PERFORM D300-LOGGING  THRU D399-LOGGING-EX
	   END-IF.

	   IF  WS-OKAY = "Y" AND WS-JUMP = 9
SGX201     IF  WK-SGX-GLACT-SW   = "Y"
SGX201     AND WK-C-VALID-GLACNO = "Y"
SGX201     AND WK-C-SGX-DAY2-SW  = "Y"
SGX201         MOVE "N" TO  TAB-VAL(09)
SGX201         MOVE 11  TO  WS-JUMP
SGX201     ELSE
	       MOVE WS-ACCNO-ORG      TO WK-NSTP-ACCTBIC
	       CALL "TRFNSTP"   USING WK-NSTP
	       IF  WK-NSTP-NONSTPCR = "Y"
		   MOVE "N" TO  WS-OKAY
		   MOVE "Y" TO  TAB-VAL(09)
		   MOVE 0   TO  WS-JUMP
5Q1ARV             INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV             MOVE "RSN0034" TO WK-C-RPRCODE
5Q1ARV             PERFORM D400-PROCESS-RPRRSN
5Q1ARV             THRU D499-PROCESS-RPRRSN-EX
       ELSE
	   MOVE WS-ACCNO        TO WK-NSTP-ACCTBIC
	   CALL "TRFNSTP"  USING WK-NSTP
	   IF  WK-NSTP-NONSTPCR = "Y"
	       MOVE "N" TO  WS-OKAY
		       MOVE "Y" TO  TAB-VAL(09)
		       MOVE 0   TO  WS-JUMP
5Q1ARV                 INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV                 MOVE WS-ACCNO  TO WK-C-ACCNO-RPR
5Q1ARV                 MOVE "RSN0034" TO WK-C-RPRCODE
5Q1ARV                 PERFORM D400-PROCESS-RPRRSN
5Q1ARV                 THRU D499-PROCESS-RPRRSN-EX
		   ELSE
		       MOVE WS-BANKID       TO WK-NSTP-ACCTBIC
		       CALL "TRFNSTP"  USING WK-NSTP
		       IF  WK-NSTP-NONSTPCR = "Y"
			   MOVE "N" TO  WS-OKAY
			   MOVE "Y" TO  TAB-VAL(09)
			   MOVE 0   TO  WS-JUMP

5Q1ARV                     INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV                     MOVE "RSN0038" TO WK-C-RPRCODE
5Q1ARV                     PERFORM D400-PROCESS-RPRRSN
5Q1ARV                     THRU D499-PROCESS-RPRRSN-EX
		       ELSE
			   MOVE "N" TO  TAB-VAL(09)
			   MOVE 11  TO  WS-JUMP
		       END-IF
		   END-IF
	       END-IF
SGX201     END-IF
	   PERFORM D300-LOGGING  THRU D399-LOGGING-EX
	   END-IF.

VASA01     IF WK-C-VASA-SW = "Y"
VASA01         IF WK-C-VCSA-CUSTFNAM NOT = SPACES
VASA01             PERFORM R300-READ-TFSICL5A2
VASA01             THRU R399-READ-TFSICL5A2-EX
VASA01         ELSE
VASA01             IF WK-C-VCCA-CUSTFNAM NOT = SPACES
VASA01                 PERFORM R200-READ-TFSICL1CA2
VASA01                 THRU R299-READ-TFSICL1CA2-EX
VASA01         END-IF
VASA01     END-IF.

	   IF  WS-OKAY = "Y" AND WS-JUMP = 11
SM1TY1         MOVE WS-ACCNO          TO WK-C-ACCNO
SM1TY1*        IF (WK-C-VCCA-CUSTFNAM = WS-NAME
SM1TY1*        OR  WK-C-VCSA-CUSTFNAM = WS-NAME
SM1TY1*        OR  WK-C-VCFA-CUSTFNAM = WS-NAME)
SM1TY1*        AND WS-NAME         NOT = SPACES
SM1TY1         IF  WS-NAME         NOT = SPACES
SM1TY1         AND ((WK-C-ACCNO-4      = "1"
SM1TY1         AND WK-C-VCSA-CUSTFNAM = WS-NAME)
SM1TY1         OR (WK-C-ACCNO-4        = "3"
SM1TY1         AND WK-C-VCCA-CUSTFNAM = WS-NAME)
SM1TY1         OR (WK-C-ACCNO-4        = "9"
SM1TY1         AND WK-C-VCFA-CUSTFNAM = WS-NAME))
SGX201         OR (WK-SGX-GLACT-SW     = "Y"
SGX201         AND WK-C-SGX-DAY2-SW    = "Y"
SGX201         AND WK-C-VALID-GLACNO   = "Y" )
		   MOVE "Y" TO  TAB-VAL(11)
		   MOVE 14  TO  WS-JUMP
	       ELSE
HOJE01             MOVE SPACES            TO  WK-C-CON-NAME
HOJE01                                        WK-C-CON-SA-NAME
HOJE01                                        WK-C-CON-CA-NAME
HOJE02                                        WK-C-CON-FA-NAME
HOJE01             IF  WS-NAME         NOT =  SPACES
HOJE01                 MOVE WS-NAME         TO  WK-C-STRING
HOJE01                 PERFORM C500-ACCNAME-VALIDATION
HOJE01                     THRU C599-ACCNAME-VALIDATION-EX
HOJE01                 MOVE WK-C-STRING     TO  WK-C-CON-NAME
HOJE01             END-IF

HOJE01             IF  WK-C-VCSA-CUSTFNAM   NOT =  SPACES
HOJE01                 MOVE WK-C-VCSA-CUSTFNAM TO  WK-C-STRING
HOJE01                 PERFORM C500-ACCNAME-VALIDATION
HOJE01                     THRU C599-ACCNAME-VALIDATION-EX
HOJE01                 MOVE WK-C-STRING       TO  WK-C-CON-SA-NAME
HOJE01             END-IF
HOJE01             IF  WK-C-VCCA-CUSTFNAM   NOT =  SPACES
HOJE01                 MOVE WK-C-VCCA-CUSTFNAM TO  WK-C-STRING
HOJE01                 PERFORM C500-ACCNAME-VALIDATION
HOJE01                     THRU C599-ACCNAME-VALIDATION-EX
HOJE01                 MOVE WK-C-STRING       TO  WK-C-CON-CA-NAME
HOJE01             END-IF
HOJE02             IF  WK-C-VCFA-CUSTFNAM   NOT =  SPACES
HOJE02                 MOVE WK-C-VCFA-CUSTFNAM TO  WK-C-STRING
HOJE02                 PERFORM C500-ACCNAME-VALIDATION
HOJE02                     THRU C599-ACCNAME-VALIDATION-EX
HOJE02                 MOVE WK-C-STRING       TO  WK-C-CON-FA-NAME
HOJE02             END-IF
HOJE02*HOJE01         IF (WK-C-ACCNO-4       = "1" AND
HOJE02*HOJE01             WK-C-CON-SA-NAME   = WK-C-CON-NAME) OR
HOJE02*HOJE01            (WK-C-ACCNO-4    NOT = "1" AND
HOJE02*HOJE01             WK-C-CON-CA-NAME   = WK-C-CON-NAME)
HOJE02             IF  WK-C-CON-NAME    NOT = SPACES
HOJE02             AND ((WK-C-ACCNO-4        = "1"
HOJE02             AND   WK-C-CON-SA-NAME   = WK-C-CON-NAME)
HOJE02             OR    (WK-C-ACCNO-4       = "3"
HOJE02             AND   WK-C-CON-CA-NAME   = WK-C-CON-NAME)
HOJE02             OR    (WK-C-ACCNO-4       = "9"
HOJE02             AND   WK-C-CON-FA-NAME   = WK-C-CON-NAME))
HOJE01                 MOVE "Y" TO  TAB-VAL(11)
HOJE01                 MOVE 14  TO  WS-JUMP
HOJE01             ELSE
VASA01*VASA02             IF WK-C-VASA-SW = "Y" AND
VASA01*VASA02                (VERIADDI OF TFSICL1CA2 = "Y" OR
VASA01*VASA02                 VERIADDI OF TFSICL5A2 = "Y")
VASA01*VASA02             IF WK-C-VCSA-CUSTFNAM NOT = SPACES
VASA01*VASA02                 MOVE ACCTM1 OF TFSICL5A2 TO WK-C-STRING
VASA01*VASA02                 PERFORM C500-ACCNAME-VALIDATION
VASA01*VASA02                     THRU C599-ACCNAME-VALIDATION-EX
VASA01*VASA02                 MOVE WK-C-STRING TO WK-C-CON-SA-NAME-SUB
VASA01*VASA02             END-IF
VASA01*VASA02             IF WK-C-VCCA-CUSTFNAM NOT = SPACES
VASA01*VASA02                 MOVE ACCTM1 OF TFSICL1CA2 TO WK-C-STRING
VASA01*VASA02                 PERFORM C500-ACCNAME-VALIDATION
VASA01*VASA02                     THRU C599-ACCNAME-VALIDATION-EX
VASA01*VASA02                 MOVE WK-C-STRING TO WK-C-CON-CA-NAME-SUB
VASA01*VASA02             END-IF
VASA01*VASA02             IF (WK-C-ACCNO-4 = "1" AND
VASA01*VASA02                 WK-C-CON-NAME = WK-C-CON-SA-NAME-SUB)
VASA01*VASA02             OR (WK-C-ACCNO-4 NOT = "1"
VASA01*VASA02             AND WK-C-CON-NAME = WK-C-CON-CA-NAME-SUB)
VASA01*VASA02                 MOVE "Y" TO TAB-VAL(11)
VASA01*VASA02                             WK-VTF1B-BENEFLG
VASA01*VASA02                 MOVE 14  TO WS-JUMP

VASA01*VASA02             END-IF
VASA02                 IF WK-C-VASA-SW = "Y"
VASA02                     PERFORM R400-VASA-NAME-CHECK
VASA02                     THRU R499-VASA-NAME-CHECK-EX
VASA01                 ELSE
HOJE01                     MOVE "N" TO  TAB-VAL(11)
HOJE01                     MOVE 12  TO  WS-JUMP
HOJE01                 END-IF
HOJE01*                MOVE "N" TO  TAB-VAL(11)
HOJE01*                MOVE 12  TO  WS-JUMP
	       END-IF
	   END-IF
	   PERFORM D300-LOGGING  THRU D399-LOGGING-EX
       END-IF.

	   IF  WS-OKAY = "Y" AND WS-JUMP = 12
	   IF  WS-ROUTE NOT = "Y"
	       MOVE "N" TO  TAB-VAL(12)
	       MOVE 13  TO  WS-JUMP
	   ELSE
	       MOVE "Y" TO  TAB-VAL(12)
	       MOVE 15  TO  WS-JUMP
5Q1RD1         INITIALIZE WK-C-RPRRSN-AREA
5Q1RD1         MOVE "RSN0107" TO WK-C-RPRCODE
5Q1RD1         PERFORM D400-PROCESS-RPRRSN
5Q1RD1         THRU D499-PROCESS-RPRRSN-EX
	   END-IF
	   PERFORM D300-LOGGING  THRU D399-LOGGING-EX
	   END-IF.

	   IF  WS-OKAY = "Y" AND WS-JUMP = 13
	   MOVE WS-ACCCUY       TO WK-NMVR-CUYCD
	   MOVE WS-ACCNO        TO WK-NMVR-ACCNO
	   MOVE WS-NAME         TO WK-NMVR-ACCNM
	   CALL "TRFNMVR"  USING WK-NMVR
	   IF  WK-NMVR-INDIC  = "Y"
	       MOVE "Y" TO  TAB-VAL(13)
	       MOVE 14  TO  WS-JUMP
	   ELSE
	       MOVE "N" TO  TAB-VAL(13)
	       MOVE 15  TO  WS-JUMP
5Q1RD1         INITIALIZE WK-C-RPRRSN-AREA
5Q1RD1         MOVE "RSN0107" TO WK-C-RPRCODE
5Q1RD1         PERFORM D400-PROCESS-RPRRSN
5Q1RD1         THRU D499-PROCESS-RPRRSN-EX
	   END-IF
	   PERFORM D300-LOGGING  THRU D399-LOGGING-EX
	   END-IF.

       C399-VALIDATION-PART-EX.
	   EXIT.
       EJECT

HOJE01 C400-GET-PARAMETER.
HOJE01     MOVE 1                   TO  WK-N-CNT-PARA

HOJE03                                      WK-N-CNT1    WK-N-CNT11
HOJE03                                      WK-N-CNT2    WK-N-CNT12
HOJE03                                      WK-N-CNT3    WK-N-CNT13
HOJE03                                      WK-N-CNT4    WK-N-CNT14
HOJE03                                      WK-N-CNT5    WK-N-CNT15
HOJE03                                      WK-N-CNT6    WK-N-CNT16
HOJE03                                      WK-N-CNT7    WK-N-CNT17
HOJE03                                      WK-N-CNT8    WK-N-CNT18
HOJE03                                      WK-N-CNT9    WK-N-CNT19
HOJE03                                      WK-N-CNT10   WK-N-CNT20
HOJE01                                      WK-N-CNT.
HOJE01 C410-GET-SALUTATION.
HOJE01     INITIALIZE
HOJE01                                      WK-C-XPARA-RECORD
HOJE01                                      WK-C-UNSTR-SALU.
HOJE01     EVALUATE WK-N-CNT-PARA
HOJE01     WHEN 1
HOJE01         MOVE "ANSALU01"          TO  WK-C-XPARA-PARACD
HOJE01     WHEN 2
HOJE01         MOVE "ANSALU02"          TO  WK-C-XPARA-PARACD
HOJE01     WHEN 3
HOJE01         MOVE "ANSALU03"          TO  WK-C-XPARA-PARACD
HOJE01     WHEN 4
HOJE01         MOVE "ANSALU04"          TO  WK-C-XPARA-PARACD
HOJE01     WHEN 5
HOJE01         MOVE "ANSALU05"          TO  WK-C-XPARA-PARACD
HOJE01     WHEN 6
HOJE01         MOVE "ANSALU06"          TO  WK-C-XPARA-PARACD
HOJE01     WHEN 7
HOJE01         MOVE "ANSALU07"          TO  WK-C-XPARA-PARACD
HOJE01     WHEN 8
HOJE01         MOVE "ANSALU08"          TO  WK-C-XPARA-PARACD
HOJE01     WHEN OTHER
HOJE01         GO TO C420-GET-SPECIAL-CHAR
HOJE01     END-EVALUATE.
HOJE01
HOJE01     CALL "TRFXPARA"       USING  WK-C-XPARA-RECORD.
HOJE01     UNSTRING WK-C-XPARA-PARAVALU
HOJE01              DELIMITED BY ";"
HOJE01              INTO WK-C-UNSTR-SALU01
HOJE01                   WK-C-UNSTR-SALU02
HOJE01                   WK-C-UNSTR-SALU03
HOJE01                   WK-C-UNSTR-SALU04
HOJE01                   WK-C-UNSTR-SALU05
HOJE01                   WK-C-UNSTR-SALU06
HOJE01                   WK-C-UNSTR-SALU07.
HOJE01
HOJE01     IF  WK-C-UNSTR-SALU01      NOT =  SPACES
HOJE03*HOJE01  MOVE  WK-C-UNSTR-SALU01     TO  WK-C-ARR-SALU(WK-N-CNT)
HOJE03*HOJE01  ADD   1                      TO  WK-N-CNT
HOJE03         MOVE  WK-C-UNSTR-SALU01     TO  WK-C-SORT-SALUT
HOJE03         PERFORM C600-SORT-SALUTATION
HOJE03             THRU C699-SORT-SALUTATION-EX
HOJE01     END-IF.
HOJE01     IF  WK-C-UNSTR-SALU02       NOT=  SPACES

HOJE03*HOJE01  MOVE  WK-C-UNSTR-SALU02     TO  WK-C-ARR-SALU(WK-N-CNT)
HOJE03*HOJE01  ADD   1                      TO  WK-N-CNT
HOJE03         MOVE  WK-C-UNSTR-SALU02     TO  WK-C-SORT-SALUT
HOJE03         PERFORM C600-SORT-SALUTATION
HOJE03             THRU C699-SORT-SALUTATION-EX
HOJE03
HOJE01     END-IF.
HOJE01     IF  WK-C-UNSTR-SALU03       NOT =  SPACES
HOJE03*HOJE01  MOVE  WK-C-UNSTR-SALU03     TO  WK-C-ARR-SALU(WK-N-CNT)
HOJE03*HOJE01  ADD   1                      TO  WK-N-CNT
HOJE03         MOVE  WK-C-UNSTR-SALU03     TO  WK-C-SORT-SALUT
HOJE03         PERFORM C600-SORT-SALUTATION
HOJE03             THRU C699-SORT-SALUTATION-EX
HOJE01     END-IF.
HOJE01     IF  WK-C-UNSTR-SALU04       NOT =  SPACES
HOJE03*HOJE01  MOVE  WK-C-UNSTR-SALU04     TO  WK-C-ARR-SALU(WK-N-CNT)
HOJE03*HOJE01  ADD   1                      TO  WK-N-CNT
HOJE03         MOVE  WK-C-UNSTR-SALU04     TO  WK-C-SORT-SALUT
HOJE03         PERFORM C600-SORT-SALUTATION
HOJE03             THRU C699-SORT-SALUTATION-EX
HOJE01     END-IF.
HOJE01     IF  WK-C-UNSTR-SALU05       NOT =  SPACES
HOJE03*HOJE01  MOVE  WK-C-UNSTR-SALU05     TO  WK-C-ARR-SALU(WK-N-CNT)
HOJE03*HOJE01  ADD   1                      TO  WK-N-CNT
HOJE03         MOVE  WK-C-UNSTR-SALU05     TO  WK-C-SORT-SALUT
HOJE03         PERFORM C600-SORT-SALUTATION
HOJE03             THRU C699-SORT-SALUTATION-EX
HOJE01     END-IF.
HOJE01     IF  WK-C-UNSTR-SALU06       NOT =  SPACES
HOJE03*HOJE01  MOVE  WK-C-UNSTR-SALU06     TO  WK-C-ARR-SALU(WK-N-CNT)
HOJE03*HOJE01  ADD   1                      TO  WK-N-CNT
HOJE03         MOVE  WK-C-UNSTR-SALU06     TO  WK-C-SORT-SALUT
HOJE03         PERFORM C600-SORT-SALUTATION
HOJE03             THRU C699-SORT-SALUTATION-EX
HOJE01     END-IF.
HOJE01     IF  WK-C-UNSTR-SALU07       NOT =  SPACES
HOJE03*HOJE01  MOVE  WK-C-UNSTR-SALU07     TO  WK-C-ARR-SALU(WK-N-CNT)
HOJE03*HOJE01  ADD   1                      TO  WK-N-CNT
HOJE03         MOVE  WK-C-UNSTR-SALU07     TO  WK-C-SORT-SALUT
HOJE03         PERFORM C600-SORT-SALUTATION
HOJE03             THRU C699-SORT-SALUTATION-EX
HOJE01     END-IF.
HOJE01
HOJE01     ADD 1 TO WK-N-CNT-PARA.
HOJE01     GO TO C410-GET-SALUTATION.
HOJE01
HOJE01 C420-GET-SPECIAL-CHAR.
HOJE01
HOJE01     INITIALIZE                       WK-C-XPARA-RECORD
HOJE01                                      WK-C-SPC-CHAR.
HOJE01     MOVE "ANINVCH1"             TO  WK-C-XPARA-PARACD.
HOJE01     CALL "TRFXPARA"        USING  WK-C-XPARA-RECORD.
HOJE01     MOVE WK-C-XPARA-PARAVALU    TO  WK-C-SPC-CHAR.
HOJE01
HOJE01     INITIALIZE                       WK-C-XPARA-RECORD.

HOJE01     MOVE "ANINVCH2"             TO  WK-C-XPARA-PARACD.
HOJE01     CALL "TRFXPARA"        USING  WK-C-XPARA-RECORD.
HOJE01
HOJE01     STRING WK-C-SPC-CHAR,
HOJE01            WK-C-XPARA-PARAVALU
HOJE01            DELIMITED BY SPACES
HOJE01            INTO WK-C-SPC-CHAR.
HOJE01
HOJE01 C430-GET-CHECK-LENGTH.
HOJE01     INITIALIZE                       WK-C-XPARA-RECORD
HOJE01                                      WK-N-CHK-LEN.
HOJE01     MOVE "ANCHKLEN"             TO  WK-C-XPARA-PARACD.
HOJE01     CALL "TRFXPARA"        USING  WK-C-XPARA-RECORD.
HOJE01     MOVE   WK-C-XPARA-PARAVALU   TO  WK-C-CHK-LEN.
HOJE01
HOJE01 C499-GET-PARAMETER-EX.
HOJE01     EXIT.
HOJE01
HOJE01 C500-ACCNAME-VALIDATION.
HOJE03*HOJE01     MOVE 1                       TO  WK-N-CNT.
HOJE03     MOVE 1                       TO  WK-N-COL.
HOJE03     MOVE 20                      TO  WK-N-ROW.
HOJE01
HOJE01 C510-NEXT-SALUTATION.
HOJE03*HOJE01     MOVE WK-C-ARR-SALU(WK-N-CNT)  TO  WK-C-TEMP-SALU.
HOJE03     IF WK-N-ROW < 1
HOJE03         GO TO C520-REMOVE-SPECIAL-CHAR
HOJE03     END-IF
HOJE03     MOVE WK-C-ARR-SALU(WK-N-ROW, WK-N-COL) TO  WK-C-TEMP-SALU.
HOJE03
HOJE01     IF  WK-C-TEMP-SALU = SPACES
HOJE03*HOJE01         GO TO C520-REMOVE-SPECIAL-CHAR
HOJE03         SUBTRACT 1 FROM WK-N-ROW
HOJE03         MOVE     1 TO WK-N-COL
HOJE03         GO TO C510-NEXT-SALUTATION
HOJE01     END-IF.
HOJE01
HOJE03*HOJE01     MOVE LENGTH OF WK-C-TEMP-SALU TO  WK-N-TALLY.
HOJE03*HOJE01     PERFORM UNTIL WK-C-TEMP-SALU(WK-N-TALLY:1) NOT = SPACE
HOJE03*HOJE01         SUBTRACT 1              FROM  WK-N-TALLY
HOJE03*HOJE01     END-PERFORM.
HOJE01
HOJE01*** REPLACING ALL SALUTATIONS WITH SPACES
HOJE03*HOJE01     EVALUATE WK-N-TALLY
HOJE03     EVALUATE WK-N-ROW
HOJE01     WHEN 1
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU01
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU01 BY SPACE
HOJE01     WHEN 2
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU02
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU02 BY SPACES
HOJE01     WHEN 3

HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU03
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU03 BY SPACES
HOJE01     WHEN 4
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU04
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU04 BY SPACES
HOJE01     WHEN 5
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU05
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU05 BY SPACES
HOJE01     WHEN 6
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU06
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU06 BY SPACES
HOJE01     WHEN 7
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU07
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU07 BY SPACES
HOJE01     WHEN 8
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU08
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU08 BY SPACES
HOJE01     WHEN 9
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU09
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU09 BY SPACES
HOJE01     WHEN 10
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU10
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU10 BY SPACES
HOJE01     WHEN 11
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU11
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU11 BY SPACES
HOJE01     WHEN 12
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU12
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU12 BY SPACES
HOJE01     WHEN 13
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU13
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU13 BY SPACES
HOJE01     WHEN 14
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU14
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU14 BY SPACES
HOJE01     WHEN 15
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU15
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU15 BY SPACES
HOJE01     WHEN 16
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU16
HOJE01         INSPECT WK-C-STRING REPLACING ALL

HOJE01                 WK-C-HLD-SALU16 BY SPACES
HOJE01     WHEN 17
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU17
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU17 BY SPACES
HOJE01     WHEN 18
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU18
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU18 BY SPACES
HOJE01     WHEN 19
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU19
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU19 BY SPACES
HOJE01     WHEN 20
HOJE01         MOVE WK-C-TEMP-SALU        TO  WK-C-HLD-SALU20
HOJE01         INSPECT WK-C-STRING REPLACING ALL
HOJE01                 WK-C-HLD-SALU20 BY SPACES
HOJE01     END-EVALUATE.
HOJE01
HOJE03*HOJE01     ADD 1                        TO  WK-N-CNT.
HOJE03*HOJE01     IF  WK-N-CNT > 60
HOJE03*HOJE01         OR WK-C-ARR-SALU(WK-N-CNT) =  SPACES
HOJE03*HOJE01         NEXT SENTENCE
HOJE03*HOJE01     ELSE
HOJE03*HOJE01         GO TO C510-NEXT-SALUTATION
HOJE03*HOJE01     END-IF.
HOJE03     ADD 1                        TO  WK-N-COL.
HOJE03     IF  WK-N-COL  > 60
HOJE03         SUBTRACT  1              FROM  WK-N-ROW
HOJE03         MOVE      1              TO   WK-N-COL
HOJE03         GO TO C510-NEXT-SALUTATION
HOJE03     ELSE
HOJE03         GO TO C510-NEXT-SALUTATION
HOJE03     END-IF.
HOJE01
HOJE01 C520-REMOVE-SPECIAL-CHAR.
HOJE01*** REPLACING ALL SPECIAL CHARACTERS WITH SPACES
HOJE01     PERFORM VARYING WK-N-TALLY FROM 1 BY 1
HOJE01         UNTIL WK-N-TALLY > LENGTH OF WK-C-SPC-CHAR
HOJE01         MOVE WK-C-SPC-CHAR(WK-N-TALLY:1) TO WK-C-SPC-CHAR1
HOJE01         IF  WK-C-SPC-CHAR1 NOT = SPACE
HOJE01             INSPECT WK-C-STRING REPLACING ALL
HOJE01                     WK-C-SPC-CHAR1 BY SPACE
HOJE01         END-IF
HOJE01     END-PERFORM.
HOJE01
HOJE01*** LEFT-JUSTIFIED THE VALUE OF STRING
HOJE01     MOVE SPACES                 TO  WK-C-WORK-STRING.
HOJE01     MOVE WK-C-STRING            TO  WK-C-WORK-STRING.
HOJE01     MOVE SPACES                 TO  WK-C-STRING.
HOJE01     MOVE 1                      TO  WK-N-CTR
HOJE01                                     WK-N-TALLY.
HOJE01     PERFORM VARYING WK-N-TALLY FROM 1 BY 1
HOJE01         UNTIL WK-N-TALLY > LENGTH OF WK-C-WORK-STRING

HOJE01         IF  WK-C-WORK-STRING(WK-N-TALLY:1) NOT = SPACE
HOJE01             MOVE WK-C-WORK-STRING(WK-N-TALLY:1)
HOJE01                                  TO  WK-C-STRING(WK-N-CTR:1)
HOJE01             ADD 1                TO  WK-N-CTR
HOJE01         END-IF
HOJE01     END-PERFORM.
HOJE01
HOJE01     MOVE SPACES                 TO  WK-C-WORK-STRING.
HOJE01     MOVE WK-C-STRING(1:WK-N-CHK-LEN) TO WK-C-WORK-STRING.
HOJE01     MOVE WK-C-WORK-STRING       TO  WK-C-STRING.
HOJE01
HOJE01 C599-ACCNAME-VALIDATION-EX.
HOJE01     EXIT.

HOJE03 C600-SORT-SALUTATION.
HOJE03
HOJE03     MOVE LENGTH OF WK-C-SORT-SALUT    TO  WK-N-TALLY
HOJE03     PERFORM UNTIL WK-C-SORT-SALUT(WK-N-TALLY:1) NOT = SPACE
HOJE03         SUBTRACT 1               FROM  WK-N-TALLY
HOJE03     END-PERFORM.
HOJE03
HOJE03     EVALUATE WK-N-TALLY
HOJE03     WHEN 01
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT1)
HOJE03         ADD    1                 TO  WK-N-CNT1
HOJE03     WHEN 02
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT2)
HOJE03         ADD    1                 TO  WK-N-CNT2
HOJE03     WHEN 03
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT3)
HOJE03         ADD    1                 TO  WK-N-CNT3
HOJE03     WHEN 04
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT4)
HOJE03         ADD    1                 TO  WK-N-CNT4
HOJE03     WHEN 05
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT5)
HOJE03         ADD    1                 TO  WK-N-CNT5
HOJE03     WHEN 06
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT6)
HOJE03         ADD    1                 TO  WK-N-CNT6
HOJE03     WHEN 07
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT7)
HOJE03         ADD    1                 TO  WK-N-CNT7
HOJE03     WHEN 08
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT8)
HOJE03         ADD    1                 TO  WK-N-CNT8

HOJE03     WHEN 09
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT9)
HOJE03         ADD    1                 TO  WK-N-CNT9
HOJE03     WHEN 10
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT10)
HOJE03         ADD    1                 TO  WK-N-CNT10
HOJE03     WHEN 11
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT11)
HOJE03         ADD    1                 TO  WK-N-CNT11
HOJE03     WHEN 12
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT12)
HOJE03         ADD    1                 TO  WK-N-CNT12
HOJE03     WHEN 13
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT13)
HOJE03         ADD    1                 TO  WK-N-CNT13
HOJE03     WHEN 14
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT14)
HOJE03         ADD    1                 TO  WK-N-CNT14
HOJE03     WHEN 15
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT15)
HOJE03         ADD    1                 TO  WK-N-CNT15
HOJE03     WHEN 16
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT16)
HOJE03         ADD    1                 TO  WK-N-CNT16
HOJE03     WHEN 17
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT17)
HOJE03         ADD    1                 TO  WK-N-CNT17
HOJE03     WHEN 18
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT18)
HOJE03         ADD    1                 TO  WK-N-CNT18
HOJE03     WHEN 19
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT19)
HOJE03         ADD    1                 TO  WK-N-CNT19
HOJE03     WHEN 20
HOJE03         MOVE WK-C-SORT-SALUT TO
HOJE03              WK-C-ARR-SALU(WK-N-TALLY, WK-N-CNT20)
HOJE03         ADD    1                 TO  WK-N-CNT20
HOJE03     END-EVALUATE.
HOJE03
HOJE03 C699-SORT-SALUTATION-EX.
HOJE03     EXIT.

       D100-VALIDATION.

GP3K01*--Route transaction to repair if Tag 59 opt = F and
GP3K01*--Tag 59F validation tech switch is OFF
GP3K01     IF   TAG59-OPT = "F"
GP3K01         IF  (WK-C-GPI3-SW  = "Y"
GP3K01         AND WK-C-TAG59F-SW = "Y")
GP3K01             CONTINUE
GP3K01         ELSE
GP3K01             INITIALIZE                   WK-C-RPRRSN-AREA
GP3K01             MOVE "RSN0368"          TO   WK-C-RPRCODE
GP3K01             PERFORM D400-PROCESS-RPRRSN
GP3K01                 THRU D499-PROCESS-RPRRSN-EX
GP3K01         END-IF
GP3K01     END-IF.

	  *CALCULATE LOCAL EQUIVALENT BASED ON 'BT' RATF
		  MOVE TFSSTPL-CUYCD        TO  WK-C-GERTE-CUYCD.
		  MOVE TFSSTPL-BNKENTTY     TO  WK-N-GERTE-BNKENTTY.
		  MOVE "BT"                 TO  WK-C-GERTE-RTE-TYP.
		  CALL "TRFGERTE"      USING  WK-C-GERTE-RECORD.
		  IF  WK-C-GERTE-ERROR-CD      = SPACES
		  AND WK-N-GERTE-EXCH-RTE NOT = ZEROS
		      COMPUTE WK-VTF1B-LCAMT ROUNDED =  TFSSTPL-AMT
					       *  WK-N-GERTE-EXCH-RTE
					       /  WK-N-GERTE-FXRATEUT
		      MOVE WK-C-GERTE-RTE-TYP   TO  WK-VTF1B-FXRATETY
		      MOVE WK-N-GERTE-EXCH-RTE  TO  WK-VTF1B-FXRATE
		      MOVE WK-N-GERTE-FXRATEUT  TO  WK-VTF1B-FXRATEUT
		  ELSE
		      COMPUTE WK-VTF1B-LCAMT =  ZEROS
		      MOVE "N"               TO  WS-OKAY
5Q1ARV             INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV             MOVE WK-C-GERTE-ERROR-CD TO WK-C-RPRCODE
5Q1ARV             PERFORM D400-PROCESS-RPRRSN
5Q1ARV                 THRU D499-PROCESS-RPRRSN-EX
		  END-IF.

SGX201     IF   WK-SGX-GLACT-SW   = "Y"
SGX201     AND  WK-C-SGX-DAY2-SW  = "Y"
SGX203         MOVE "N"                    TO  WK-C-CR-GLACNO
SGX201         IF   WK-C-TAG59-G-IND  = "Y"
SGX201         AND  WK-C-VALID-GLACNO = "Y"
SGX201         AND  WS-ACCNO NOT = SPACES
SGX203*SGX201        MOVE "A0"                TO  WS-LINK-STATUS
SGX203             MOVE "A1"                TO  WS-LINK-STATUS
SGX203             MOVE "Y"                 TO  WK-C-CR-GLACNO
SGX203             IF  WK-C-GPI-SW = WK-C-Y
SGX203             AND SW-STP-LMT-SKP-Y
SGX203                 CONTINUE
SGX203             ELSE
SGX201                 GO TO D101-VALIDATE-JUMP-14
SGX203             END-IF
SGX201         END-IF
SGX201     END-IF.
SGX201

GPI201     IF   WK-C-GPI-SW = WK-C-Y
GPI201         IF     SW-STP-LMT-SKP-Y
GPI201         AND   (TFSSTPL-SWFTMGTY = "103"
GPI201         OR     TFSSTPL-SWFTMGTY = "202")
GPI201                PERFORM D110-VALIDATE-STP-BYPASS
GPI201                    THRU D119-VALIDATE-STP-BYPASS-EX
GPI201         ELSE
GPI201                MOVE SPACES TO WK-C-BYPASS-LMT-IND
GPI201         END-IF
GPI201         IF WK-C-BYPASS-LMT-IND = WK-C-Y
SGX203         OR (WK-SGX-GLACT-SW  = "Y"
SGX203         AND WK-C-SGX-DAY2-SW = "Y"
SGX203         AND WK-C-CR-GLACNO   = "Y")
GPI201             GO TO D101-VALIDATE-JUMP-14
GPI201         END-IF
GPI201     END-IF.

CMP3X1*CMP3A2  MOVE SPACES  TO  WS-LINK-STATUS.
CMP3A2     MOVE "+A1"    TO  WS-LINK-STATUS.
CMP3FL     IF  WS-ACCNO NOT = SPACES
CMP3A3         OR (WK-101-TAG50H-ACCNO NOT = SPACES AND
CMP3A3             TFSSTPL-SWFTMGTY = "101")
CMP3FL         INITIALIZE WK-C-RPRRSN-AREA
CMP3FL         INITIALIZE WK-C-LINK-LIMIT
CMP3FL         MOVE  TFSSTPL-BNKENTTY       TO  WS-LINK-BNKENTTY
CMP3FL*CMP3A3  MOVE  WS-ACCNO               TO  WS-LINK-ACCNO
CMP3A3         IF    TFSSTPL-SWFTMGTY = "101"
CMP3A3             MOVE WK-101-TAG50H-ACCNO
CMP3A3                                      TO  WS-LINK-ACCNO
CMP3A3         ELSE
CMP3A3             MOVE  WS-ACCNO           TO  WS-LINK-ACCNO
CMP3A3         END-IF
CMP3FL         MOVE  TFSSTPL-CUYCD          TO  WS-LINK-CCY
CMP3FL         MOVE  TFSSTPL-AMT            TO  WS-LINK-AMT
CMP3A1         MOVE  "I"                    TO  WS-LINK-REMIND
CMP3FL         CALL "TRFVLMT" USING WK-C-LINK-LIMIT
CMP3FL         EVALUATE WS-LINK-STATUS
CMP3FL         WHEN "XX"
CMP3FL             MOVE "N"        TO  WS-OKAY
CMP3FL             MOVE "RSN0311"      TO WK-C-RPRCODE
CMP3FL             PERFORM D400-PROCESS-RPRRSN
CMP3FL                 THRU D499-PROCESS-RPRRSN-EX
CMP3FL*CMP3X1  WHEN "AA"
CMP3FL*CMP3X1      MOVE "N"        TO  WS-OKAY
CMP3FL*CMP3X1      MOVE "RSN0312"      TO WK-C-RPRCODE
CMP3FL*CMP3X1      PERFORM D400-PROCESS-RPRRSN
CMP3FL*CMP3X1          THRU D499-PROCESS-RPRRSN-EX
CMP3FL*CMP3X1  WHEN "AC"
CMP3FL*CMP3X1      MOVE "N"        TO  WS-OKAY
CMP3FL*CMP3X1      MOVE "RSN0313"      TO WK-C-RPRCODE
CMP3FL*CMP3X1      PERFORM D400-PROCESS-RPRRSN
CMP3FL*CMP3X1          THRU D499-PROCESS-RPRRSN-EX
CMP3FL*CMP3X1  WHEN "AS"
CMP3FL*CMP3X1      MOVE "N"        TO  WS-OKAY

CMP3FL*CMP3X1      MOVE "RSN0314"      TO WK-C-RPRCODE
CMP3FL*CMP3X1      PERFORM D400-PROCESS-RPRRSN
CMP3FL*CMP3X1          THRU D499-PROCESS-RPRRSN-EX
CMP3FL         END-EVALUATE
CMP3FL*CMP3X1END-IF
CMP3X1     END-IF.

SGX201 D101-VALIDATE-JUMP-14.
REM269**** IF  WS-ACCCUY  = TFSCL-SYS-1-CUYCD
REM269     IF  WS-ACCCUY  = WK-C-LCUYCD
	   AND WS-OKAY    = "Y"
OFX2A1         IF   WK-C-OFX-TCH-SW2 = "Y"
OFX2A1         AND (TFSSTPL-SWFTMGTY = "103"
OFX2A1         OR   TFSSTPL-SWFTMGTY = "202"
OFX2A1         OR   TFSSTPL-SWFTMGTY = "101")
OFX2A1         AND (WK-VTF1B-PMODE   = "CA"
OFX2A1         OR   WK-VTF1B-PMODE   = "SA"
OFX2A1         OR   WK-VTF1B-PMODE   = "FCCA")
OFX2A1             PERFORM D700-VALIDATE-OFX
OFX2A1                 THRU D799-VALIDATE-OFX-EX
OFX2A1             IF   WK-C-OFX-SUBSCRIBED = "Y"
OFX2A1                 PERFORM D800-XMS-ENQUIRY
OFX2A1                     THRU D899-XMS-ENQUIRY-EX
OFX2A1             END-IF
OFX2A1         END-IF
OFX2A1         IF    L-C-G-FX-RATE-TYPE = "OF"
OFX2A1             CONTINUE
OFX2A1         ELSE
		   MOVE WK-VTF1B-PARALNO     TO  WK-VTF2-PARALNO
		   MOVE WK-VTF1B-SEQNUM      TO  WK-VTF2-SEQNUM
		   MOVE TFSSTPL-BNKENTTY     TO  WK-VTF2-BNKENTTY
		   MOVE TFSSTPL-AMT          TO  WK-VTF2-AMOUNT
		   MOVE WS-ACCNO             TO  WK-VTF2-ACCNO
		   MOVE TFSSTPL-CUYCD        TO  WK-VTF2-CUYCD
		   CALL "TRFVTF2"       USING WK-VTF2
		   MOVE WK-VTF2-NO-ERROR     TO  WS-OKAY
		   MOVE WK-VTF2-DATAF2       TO  WK-VTF1B-DATAF2
		   MOVE WK-VTF2-FXRATETY     TO  WK-VTF1B-FXRATETY
		   MOVE WK-VTF2-FXRATE       TO  WK-VTF1B-FXRATE
		   MOVE WK-VTF2-FXRATEUT     TO  WK-VTF1B-FXRATEUT
OFX2A1         END-IF
	   END-IF.

	   IF  WS-JUMP = 14 AND WS-OKAY = "Y"
CMP3X1         IF  WS-LINK-STATUS = "A0"
GPI201         OR (WK-C-GPI-SW = WK-C-Y
GPI201         AND  SW-STP-LMT-SKP-Y
GPI201         AND  WK-C-BYPASS-LMT-IND = WK-C-Y)
CMP3X1         MOVE 16               TO   WS-JUMP
CMP3X1         MOVE "N"              TO   TAB-VAL(14)
CMP3X1         ELSE
		   IF  WK-VTF1B-LCAMT  <=   WK-N-IRMPSTP
CMP3A2         AND WS-LINK-STATUS   =   "A1"
SM1TY1*            MOVE "PSTP"       TO   WS-STPTYP

SM1TY1             MOVE 16           TO   WS-JUMP
		   MOVE "N"          TO   TAB-VAL(14)
	       ELSE
		   MOVE 15           TO   WS-JUMP
		   MOVE "Y"          TO   TAB-VAL(14)
	       END-IF
CMP3X1     END-IF
	   END-IF
	   PERFORM D300-LOGGING  THRU D399-LOGGING-EX
       END-IF.
       IF  WS-JUMP = 15 AND WS-OKAY = "Y"
GPI202     IF  (WK-C-GPI-SW = WK-C-Y
GPI202     AND  SW-STP-LMT-SKP-Y
GPI202     AND  WK-C-BYPASS-LMT-IND = WK-C-Y)
GPI202*--TAB-VAL(10) equal "N" - BENE  A/C not found in Bank Account Table
GPI202*--TAB-VAL(12) equal "Y" - BENE  A/C Name not match
GPI202*--TAB-VAL(13) equal "N" - BENE  A/C Name not match
GPI202        IF TAB-VAL(10)        = "N"
GPI202        OR TAB-VAL(12)        = "Y"
GPI202        OR TAB-VAL(13)        = "N"
GPI202            MOVE "1STP"       TO   WS-STPTYP
GPI202            MOVE "N"          TO   TAB-VAL(15)
GPI202        END-IF
GPI202     ELSE
	      IF  WK-VTF1B-LCAMT  <=   WK-N-IRM1STP
		  MOVE "1STP"       TO   WS-STPTYP
		  MOVE "N"          TO   TAB-VAL(15)
	      IF  WK-VTF1B-LCAMT  >    WK-N-IRM1STP
5Q1RD1        OR  (WS-LINK-STATUS   =   "AA"
CMP3A2        OR   WS-LINK-STATUS   =   "AC"
CMP3A2        OR   WS-LINK-STATUS   =   "AS")
5Q1ARV            INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV            MOVE "RSN0028"  TO WK-C-RPRCODE
5Q1ARV            PERFORM D400-PROCESS-RPRRSN
5Q1ARV                THRU D499-PROCESS-RPRRSN-EX
5Q1RD1        END-IF
	      ELSE
		  MOVE "2STP"       TO   WS-STPTYP
		  MOVE "Y"          TO   TAB-VAL(15)
5Q1ARV            INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV            MOVE "RSN0023"  TO WK-C-RPRCODE
5Q1ARV            PERFORM D400-PROCESS-RPRRSN
5Q1ARV                THRU D499-PROCESS-RPRRSN-EX
	      END-IF
GPI202     END-IF
	   END-IF
	   PERFORM D300-LOGGING  THRU D399-LOGGING-EX
       END-IF.
SM1TY1     IF  WS-JUMP = 16 AND WS-OKAY = "Y"
CMP3X1*CMP3X2IF  WS-LINK-STATUS = "A0"
CMP3X1*CMP3X2  MOVE "PSTP"       TO   WS-STPTYP
CMP3X1*CMP3X2  MOVE "N"          TO   TAB-VAL(16)
CMP3X1*CMP3X2ELSE
      |     MOVE WS-ACCNO         TO   WK-C-VDUPL-ACCNO
      |     MOVE WS-ACCCUY        TO   WK-C-VDUPL-REMCUY
      |     MOVE TFSSTPL-AMT      TO   WK-N-VDUPL-REMAMT

7Q1EM1      MOVE WK-VTF1B-TRNREF TO   WK-C-VDUPL-TRNREF
7Q1EM2      MOVE SPACES          TO   WK-C-SWFTMGTY
7Q1EM3      MOVE SPACES          TO   WK-C-TRN-NO
7Q1EM2      MOVE L-C-G-MSGTYP    TO   WK-C-SWFTMGTY
7Q1EM3      MOVE L-C-TRN-NO      TO   WK-C-TRN-NO
7Q1EM2      MOVE TFSSTPL-SWFTMGTY TO  L-C-G-MSGTYP
7Q1EM3      MOVE SPACES           TO  L-C-TRN-NO
7Q1EM2      DISPLAY L-C-LOCAL-DATA-AREA      UPON  LOCAL-DATA-AREA
      |     CALL "TREVDUPL"   USING  WK-C-VDUPL-RECORD
7Q1EM2      MOVE WK-C-SWFTMGTY    TO  L-C-G-MSGTYP
7Q1EM3      MOVE WK-C-TRN-NO      TO  L-C-TRN-NO
7Q1EM2      DISPLAY L-C-LOCAL-DATA-AREA      UPON  LOCAL-DATA-AREA
      |     IF   WK-C-VDUPL-DPTRNNO NOT = SPACE
      |          MOVE "1STP"       TO   WS-STPTYP
      |          MOVE "Y"          TO   TAB-VAL(16)
5Q1ARV           INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV           MOVE "RSN0003"  TO WK-C-RPRCODE
5Q1ARV           PERFORM D400-PROCESS-RPRRSN
5Q1ARV                  THRU D499-PROCESS-RPRRSN-EX
      |ELSE
      |          MOVE "PSTP"       TO   WS-STPTYP
      |          MOVE "N"          TO   TAB-VAL(16)
      |     END-IF
CMP3X1*CMP3X2END-IF
	   PERFORM D300-LOGGING  THRU D399-LOGGING-EX
SM1TY1     END-IF.
       D199-VALIDATION-EX.
	   EXIT.
       EJECT

GPI201 D110-VALIDATE-STP-BYPASS.
GPI201*-------------------------------------------------------------*
GPI201* THIS WILL CALL TRFVBACU TO CHECK IF THE BNK BRANCH IND= Y  *
GPI201*-------------------------------------------------------------*
GPI201
GPI201     MOVE SPACES              TO  WK-C-BYPASS-LMT-IND.
GPI201
GPI201*--Bypass STP Limit if Debit Leg is a VOSTRO account
GP3M00*--Additional Validation:
GP3M00*-- - MT202/C: Do not bypass STP Limit if Dr Leg= VOSTRO
GP3M00*--           and CR Leg= CASA
GP3M00     IF      WK-C-GPI3-SW  = WK-C-Y
GP3M00     AND     WK-C-NSLMT-SW = WK-C-Y
GP3M00     AND     TFSSTPL-SWFTMGTY = "202"
GP3M00     AND    (WK-C-DR-PMODE  = "CA"
GP3M00     OR      WK-C-DR-PMODE  = "FCCA")
GP3M00     AND    (WK-VTF1B-PMODE = "CA"
GP3M00     OR      WK-VTF1B-PMODE = "SA"
GP3M00     OR      WK-VTF1B-PMODE = "FCCA")
GP3M00         CONTINUE
GP3M00     ELSE
GPI201         IF   WK-C-DR-PMODE   = "CA"
GPI201         OR   WK-C-DR-PMODE   = "FCCA"
GPI201             MOVE WK-C-Y           TO WK-C-BYPASS-LMT-IND

GPI201             GO TO D119-VALIDATE-STP-BYPASS-EX
GP3M00         END-IF
GP3M00     END-IF.
GPI201
GPI201
GP3M00*--MT103: Bypass STP Limit if Dr Leg= NOSTRO, CR Leg = CASA
GP3M00*--       and both Tag 53/54 not present
GP3M02*--MT202: Adding MT202 to by pass STP limit
GP3M00     IF     WK-C-GPI3-SW  = WK-C-Y
GP3M00     AND    WK-C-NSLMT-SW = WK-C-Y
GP3M00         IF    TFSSTPL-SWFTMGTY = "103"
GP3M02         OR    TFSSTPL-SWFTMGTY = "202"
GP3M00             MOVE SPACES           TO  TAG53-FORMAT
GP3M00                                      TAG54-FORMAT
GP3M00             MOVE TFSSTPL-TAG53    TO  TAG53-FORMAT
GP3M00             MOVE TFSSTPL-TAG54    TO  TAG54-FORMAT
GP3M01*GP3M00         IF   WK-C-DR-PMODE  = "NOSTRO"
GP3M01             IF  (WK-C-DR-PMODE  = "NOSTRO"
GP3M01             OR WK-C-DR-PMODE  = "MAS")
GP3M00             AND (WK-VTF1B-PMODE = "CA"
GP3M00             OR   WK-VTF1B-PMODE = "SA"
GP3M00             OR   WK-VTF1B-PMODE = "FCCA")
GP3M00             AND (TAG53-OPT = SPACES
GP3M00             AND  TAG53-BIC = SPACES
GP3M00             AND  TAG54-OPT = SPACES
GP3M00             AND  TAG54-BIC = SPACES)
GP3M00                 MOVE WK-C-Y       TO WK-C-BYPASS-LMT-IND
GP3M00                 GO TO D119-VALIDATE-STP-BYPASS-EX
GP3M00             END-IF
GP3M00         END-IF
GP3M00     END-IF.
GP3M00
GPI201*--Check the cover received indicator
GPI201     IF    TFSSTPL-SWFTMGTY  = "103"
GPI201         MOVE SPACE           TO  WK-C-COV-SW
GPI201         PERFORM R100-READ-UFIMIJCON
GPI201             THRU R199-READ-UFIMIJCON-EX
GPI201         IF    WK-C-COV-SW  = WK-C-Y
GPI201             MOVE WK-C-Y      TO  WK-C-BYPASS-LMT-IND
GPI201             GO TO D119-VALIDATE-STP-BYPASS-EX
GPI201         END-IF
GPI201     END-IF.
GPI201
GPI201*--Check if Sending BankID is a Nostro - BNK Branch
GPI201     INITIALIZE WK-C-VBACU-RECORD.
GPI201
GPI201     MOVE TFSSTPL-SENBNKID    TO WK-C-VBACU-BANKID.
GPI201     CALL "TRFVBACU"  USING WK-C-VBACU-RECORD.
GPI201
GPI201     IF  WK-C-VBACU-ERROR-CD = SPACES
GPI201         IF  WK-C-VBACU-BNKBRH = WK-C-Y
GPI201             MOVE WK-C-Y      TO  WK-C-BYPASS-LMT-IND
GPI201         ELSE
GPI201             MOVE SPACES      TO  WK-C-BYPASS-LMT-IND
GPI201         END-IF

GPI201     END-IF.
GPI201
GPI201 D119-VALIDATE-STP-BYPASS-EX.
GPI201     EXIT.
GPI201 EJECT

       D200-VALIDATION.
		MOVE WS-BANKID        TO  WK-VTF1B-BANKID.
		MOVE WS-ACBNKID       TO  WK-VTF1B-ACBNKID.
		MOVE WS-BENBNKID      TO  WK-VTF1B-BENBNKID.
		MOVE WS-ACCNO         TO  WK-VTF1B-BANKAC.
		MOVE WS-ACBNKACC      TO  WK-VTF1B-ACBNKACC.
		MOVE WS-BENBKACC      TO  WK-VTF1B-BENBKACC.
		MOVE WS-BENEACC       TO  WK-VTF1B-BENEACC.
		MOVE WS-ACCTYP        TO  WK-VTF1B-BANKACTYP.
		MOVE WS-ACCCUY        TO  WK-VTF1B-ACCCUY.
		MOVE WS-ACUDBUI       TO  WK-VTF1B-ACUDBUI.
		MOVE TABLE-ARRAY      TO  WK-VTF1B-DATAF1B.

		IF  TAG58-OPT = "A"
		AND (TFSSTPL-SWFTMGTY = "202"
		OR   TFSSTPL-SWFTMGTY = "203")
		    MOVE  SPACES  TO  WK-VTF1B-BENBKACC
		END-IF.
		IF  WS-OKAY   = "Y"
		    MOVE  "N"          TO  WK-VTF1B-ERROR-FOUND
		    MOVE  WS-STPTYP    TO  WK-VTF1B-STPTYP
		ELSE
		    MOVE  "Y"          TO  WK-VTF1B-ERROR-FOUND
		    MOVE  SPACES       TO  WK-VTF1B-STPTYP
5Q1LN2             IF WS-OKAY = SPACE
5Q1LN2                 INITIALIZE WK-C-RPRRSN-AREA
5Q1LN2                 MOVE "RSN0224" TO WK-C-RPRCODE
5Q1LN2                 PERFORM D400-PROCESS-RPRRSN
5Q1LN2                     THRU D499-PROCESS-RPRRSN-EX
5Q1LN2             END-IF
		END-IF.
		MOVE  "N"       TO WS-FLAG1.
		PERFORM D300-LOGGING       THRU D399-LOGGING-EX.
       D299-VALIDATION-EX.
	   EXIT.
       EJECT

       D300-LOGGING.
	   MOVE WK-VTF1B-PARALNO     TO WK-LOGG-PARALNO.
	   MOVE WK-VTF1B-SEQNUM      TO WK-LOGG-SEQNUM.
	   MOVE TABLE-ARRAY          TO WK-LOGG-DATAF1B.
	   MOVE "F1B"                TO WK-LOGG-TABTYP.
	   MOVE WK-VTF1B-LCAMT       TO WK-LOGG-LCAMT.
	   CALL "TRFLOGGCL"  USING WK-LOGG
				   WS-FLAG1
				   WS-FLAG2.
	   IF WK-LOGG-ERROR-FOUND = "Y"
	      GO TO D399-LOGGING-EX

	   END-IF.
       D399-LOGGING-EX.
	   EXIT.

5Q1ARV D400-PROCESS-RPRRSN SECTION.
5Q1ARV D400-ENTRY.
5Q1ARV
5Q1ARV     MOVE WK-VTF1B-PARALNO    TO WK-C-RRSN-QUENUM.
5Q1ARV     MOVE WK-VTF1B-SEQNUM     TO WK-C-RRSN-QUESUF.
5Q1ARV     MOVE WK-C-TRNNO          TO WK-C-RRSN-TRNNO.
5Q1ARV     MOVE WK-C-FUNCTID        TO WK-C-RRSN-FUNCTID.
5Q1ARV     MOVE WK-C-SEGCDE         TO WK-C-RRSN-SEGCDE.
5Q1ARV     MOVE SPACES              TO WK-C-RRSN-SEGDESC.
5Q1ARV     MOVE WK-N-STAFFIND       TO WK-C-RRSN-STAFFIND.
5Q1ARV     MOVE WK-C-ACCNO-RPR      TO WK-C-RRSN-ACCNO.
CMP3A3     IF WK-101-TAG50H-ACCNO NOT = SPACES AND
CMP3A3        TFSSTPL-SWFTMGTY = "101"
CMP3A3        MOVE WK-101-TAG50H-ACCNO      TO  WK-C-RRSN-ACCNO
CMP3A3     END-IF.
5Q1ARV     MOVE WK-C-ORATE          TO WK-C-RRSN-ORATE.
5Q1ARV     MOVE WK-N-SYSDTE         TO WK-C-RRSN-RPRDTE.
5Q1JE1*5Q1ARV     MOVE WK-C-RPRCODE     TO WK-C-RRSN-RSNCDE
5Q1JE1     IF   WK-C-RPRCODE =  SPACE
5Q1JE1         MOVE "RSN9999"       TO WK-C-RRSN-RSNCDE
5Q1JE1     ELSE
5Q1JE1         MOVE WK-C-RPRCODE  TO WK-C-RRSN-RSNCDE
5Q1JE1     END-IF.
5Q1JE1
5Q1ARV     MOVE SPACES              TO WK-C-RRSN-RSNDESC.
5Q1ARV     MOVE WK-C-RPRPGM         TO WK-C-RRSN-RPRPGM.
5Q1ARV     CALL "TRFGRRSN"  USING WK-C-RRSN-RECORD.
5Q1ARV
5Q1ARV D499-PROCESS-RPRRSN-EX.
5Q1ARV     EXIT.
5Q1ARV
GPI201 R100-READ-UFIMIJCON.
GPI201
GPI201     INITIALIZE UFIMIJCON-REC WK-C-UFIMIJCON.
GPI201
GPI201     MOVE WK-VTF1B-PARALNO    TO UFIMIJCON-QUENUM
GPI201     MOVE WK-VTF1B-SEQNUM     TO UFIMIJCON-QUESUF
GPI201
GPI201     READ UFIMIJCON KEY IS EXTERNALLY-DESCRIBED-KEY
GPI201
GPI201     IF NOT WK-C-SUCCESSFUL
GPI201         GO TO R199-READ-UFIMIJCON-EX
GPI201     END-IF.
GPI201
GPI201*-- Turn ON the switch if COVER is already received.
GPI201     IF      UFIMIJCON-STATUS EQUAL WK-C-A
GPI201             MOVE WK-C-Y       TO WK-C-COV-SW
GPI201     END-IF.
GPI201
GPI201 R199-READ-UFIMIJCON-EX.

GPI201     EXIT.
GPI201
      *================================================================*
GP3C00 D600-EVAL-TAG57-CD.
      *================================================================*
GP3C00*--This routine will check Tag57 C/D Lines 1-2 if it exact matches
GP3C00*--Tag Validation table. If Match, treat it as Tag57A w/ our Own BIC
GP3C00*--(MYBNS6S6XXX - parameterized) to further proceed with STP processing.
GP3C00*--E.g Raw Tag57D Line1:/123456789
GP3C00*--               Line2:MYBNK INTERNATIONAL BANK
GP3C00*--               Line3:SINGAPORE
GP3C00*--               Line4:KANTONG
GP3C00*--               Line5:SG
GP3C00*--If Line 2 "MYBNK INTERNATIONAL BANK" exact matches Tag validation table
GP3C00*--system will treat this as Tag57A Line1: *blank
GP3C00*--                                  Line2: MYBNS6S6XXX
GP3C00*--                                  Line3: *blank
GP3C00*--                                  Line4: *blank
GP3C00*--                                  Line5: *blank
GP3C00*--and proceed with BAU STP processing.
GP3C00
GP3C00     INITIALIZE                   WK-C-VTAG57-RECORD.
GP3C00     MOVE TAG57-OPT          TO   WK-C-VTAG57-OPTION.
GP3C00
GP3C00*--Tag57C:
GP3C00     IF   TAG57-OPT = "C"
GP3C00         IF  TAG57-PTID = SPACES
GP3C00             GO TO D699-EVAL-TAG57-CD-EX
GP3C00         ELSE
GP3C00             MOVE TAG57-PTID      TO     WK-C-VTAG57-INFO(1)
GP3C00         END-IF
GP3C00     END-IF.
GP3C00
GP3C00*--Tag57D:
GP3C00     IF   TAG57-OPT = "D"
GP3C00         IF  TAG57-PTID = SPACES
GP3C00         AND TAG57-NAME = SPACES
GP3C00             GO TO D699-EVAL-TAG57-CD-EX
GP3C00         ELSE
GP3C01*GP3C00         MOVE TAG57-PTID      TO     WK-C-VTAG57-INFO(1)
GP3C00             MOVE TAG57-NAME      TO     WK-C-VTAG57-INFO(2)
GP3C01*GP3C00         MOVE TAG57-LINE-3    TO     WK-C-VTAG57-INFO(3)
GP3C01*GP3C00         MOVE TAG57-LINE-4    TO     WK-C-VTAG57-INFO(4)
GP3C01*GP3C00         MOVE TAG57-LINE-5    TO     WK-C-VTAG57-INFO(5)
GP3C00         END-IF
GP3C00     END-IF.
GP3C00
GP3C00     MOVE TFSSTPL-BNKENTTY       TO     WK-C-VTAG57-I-BNKENTTY.
GP3C00
GP3C00*--Check Tag57 if either Lines 1-2 matches Tag validation table.
GP3C00     CALL   "TRFVTAG57"      USING      WK-C-VTAG57-RECORD.
GP3C00     CANCEL "TRFVTAG57".
GP3C00

      *================================================================*
GP3C00 D699-EVAL-TAG57-CD-EX.
      *================================================================*
GP3C00     EXIT.
      *=================================================================*
GP3C00*--This routine will check Tag57 C/D Lines 1-2 if it exact matches
GP3C00*--Tag Validation table. If Match, treat it as Tag57A w/ our Own BIC
GP3C00*--(MYBNS6S6XXX - parameterized) to further proceed with STP processing.
      *=================================================================*
       EJECT

GPI201 D110-VALIDATE-STP-BYPASS.
GPI201*-------------------------------------------------------------*
GPI201* THIS WILL CALL TRFVBACU TO CHECK IF THE BNK BRANCH IND= Y  *
GPI201*-------------------------------------------------------------*
GPI201
GPI201     MOVE SPACES              TO  WK-C-BYPASS-LMT-IND.
GPI201
GPI201*--Bypass STP Limit if Debit Leg is a VOSTRO account
GP3M00*--Additional Validation:
GP3M00*-- - MT202/C: Do not bypass STP Limit if Dr Leg= VOSTRO
GP3M00*--           and CR Leg= CASA
GP3M00     IF      WK-C-GPI3-SW  = WK-C-Y
GP3M00     AND     WK-C-NSLMT-SW = WK-C-Y
GP3M00     AND     TFSSTPL-SWFTMGTY = "202"
GP3M00     AND    (WK-C-DR-PMODE  = "CA"
GP3M00     OR      WK-C-DR-PMODE  = "FCCA")
GP3M00     AND    (WK-VTF1B-PMODE = "CA"
GP3M00     OR      WK-VTF1B-PMODE = "SA"
GP3M00     OR      WK-VTF1B-PMODE = "FCCA")
GP3M00         CONTINUE
GP3M00     ELSE
GPI201         IF   WK-C-DR-PMODE   = "CA"
GPI201         OR   WK-C-DR-PMODE   = "FCCA"
GPI201             MOVE WK-C-Y           TO WK-C-BYPASS-LMT-IND
GPI201             GO TO D119-VALIDATE-STP-BYPASS-EX
GP3M00         END-IF
GP3M00     END-IF.
GPI201
GPI201*--Check the cover received indicator
GPI201     IF    TFSSTPL-SWFTMGTY  = "103"
GPI201         MOVE SPACE           TO  WK-C-COV-SW
GPI201         PERFORM R100-READ-UFIMIJCON
GPI201             THRU R199-READ-UFIMIJCON-EX
GPI201         IF    WK-C-COV-SW  = WK-C-Y
GPI201             MOVE WK-C-Y      TO  WK-C-BYPASS-LMT-IND
GPI201             GO TO D119-VALIDATE-STP-BYPASS-EX
GPI201         END-IF
GPI201     END-IF.
GPI201
GPI201*--Check if Sending BankID is a Nostro - BNK Branch
GPI201     INITIALIZE WK-C-VBACU-RECORD.
GPI201
GPI201     MOVE TFSSTPL-SENBNKID    TO WK-C-VBACU-BANKID.
GPI201     CALL "TRFVBACU"  USING WK-C-VBACU-RECORD.
GPI201
GPI201     IF  WK-C-VBACU-ERROR-CD = SPACES
GPI201         IF  WK-C-VBACU-BNKBRH = WK-C-Y
GPI201             MOVE WK-C-Y      TO  WK-C-BYPASS-LMT-IND
GPI201         ELSE
GPI201             MOVE SPACES      TO  WK-C-BYPASS-LMT-IND
GPI201         END-IF
GPI201     END-IF.

GPI201 D119-VALIDATE-STP-BYPASS-EX.
GPI201 EXIT.
*******************************************************************
 Fourth
*******************************************************************
===============================================================*
 GP3C00 - Tag57 Validation Procedures
===============================================================*
GP3C00      IF  WK-C-VTAG57-ERROR-CD = SPACES
GP3C00          CONTINUE
GP3C00      ELSE
GP3C00          GO TO D699-EVAL-TAG57-CD-EX
GP3C00      END-IF.
GP3C00
GP3C00*--If it match, overide w/ Tag57A:<Own BIC> (parameterized)
GP3C00      IF  WK-C-VTAG57-VALID = "Y"
GP3C00          MOVE SPACES           TO      TAG57-PTID
GP3C00                                        WS-ACBNKACC
GP3C00                                        WS-ACCNO
GP3C00          MOVE "A"              TO      TAG57-OPT
GP3C00          MOVE WK-C-VTAG57-BIC  TO      TAG57-BIC
GP3C00                                        WS-ACBNKID
GP3C00                                        WS-BANKID
GP3C00      END-IF.
   *===============================================================*
GP3C00 D699-EVAL-TAG57-CD-EX.
   *===============================================================*
GP3C00      EXIT.

OFX2A1*---------------------------------------------------------------*
OFX2A1 D700-VALIDATE-OFX.
OFX2A1*---------------------------------------------------------------*
OFX2A1      MOVE "N"                  TO    WK-C-OFX-SUBSCRIBED.

OFX2A1*--> Checking for Channel ID found
OFX2A1      MOVE "ITT"                TO    WK-C-CHID-PROD.
OFX2A1      MOVE TFSSTPL-PARALNO      TO    WK-N-CHID-PARALNO.
OFX2A1      MOVE TFSSTPL-SEQNUM       TO    WK-N-CHID-SEQNUM.
OFX2A1      CALL "TRFVCHID"         USING   WK-C-CHID-RECORD.
OFX2A1      IF  WK-C-CHID-ERROR-CD        = SPACES
OFX2A1      AND WK-C-CHID-FOUND           = "Y"
OFX2A1          CONTINUE
OFX2A1      ELSE
OFX2A1          GO TO D799-VALIDATE-OFX-EX
OFX2A1      END-IF.

OFX2A1*--> Checking for Segment Code found
OFX2A1      INITIALIZE                      WK-C-CSEG-RECORD
OFX2A1      MOVE WK-C-SEGCODE         TO    WK-C-CSEG-SEGCODE
OFX2A2      IF  TFSSTPL-SWFTMGTY = "101"
OFX2A2          MOVE SPACES           TO    WK-C-CSEG-SEGCODE
OFX2A2          MOVE TFSSTPL-CUYCD    TO    WK-C-CSEG-DRCCY
OFX2A2          MOVE WK-101-TAG50H-ACCNO TO  WK-C-CSEG-DRACCNO
OFX2A2      END-IF.
OFX2A1      CALL "TRFVCSEG"         USING   WK-C-CSEG-RECORD
OFX2A1      IF  WK-C-CSEG-ERROR-CD        = SPACES
OFX2A1      AND WK-C-CSEG-FOUND           = "Y"
OFX2A1          CONTINUE
OFX2A1      ELSE
OFX2A1          GO TO D799-VALIDATE-OFX-EX
OFX2A1      END-IF.

OFX2A1*--> Checking for Currency Pairing found
OFX2A1      INITIALIZE                      WK-C-CUYP-RECORD.
OFX2A1      MOVE TFSSTPL-CUYCD        TO    WK-C-CUYP-MAJOR.
OFX2A1      MOVE WS-ACCCUY            TO    WK-C-CUYP-MINOR.
OFX2A1      CALL "TRFVCUYP"         USING   WK-C-CUYP-RECORD.
OFX2A1      IF  WK-C-CUYP-ERROR-CD        = SPACES
OFX2A1      AND WK-C-CUYP-FOUND           = "Y"
OFX2A1          CONTINUE
OFX2A1      ELSE
OFX2A1          GO TO D799-VALIDATE-OFX-EX
OFX2A1      END-IF.
OFX2A1
OFX2A1      MOVE "Y"                  TO    WK-C-OFX-SUBSCRIBED.
OFX2A1
OFX2A1*---------------------------------------------------------------*
OFX2A1 D799-VALIDATE-OFX-EX.
OFX2A1*---------------------------------------------------------------*
OFX2A1      EXIT.

OFX2A1*---------------------------------------------------------------*
OFX2A1 D800-XMS-ENQUIRY.
OFX2A1*---------------------------------------------------------------*
OFX2A1*--> This routine will interface to XMS for OFX Rate Enquiry
OFX2A1      INITIALIZE                      WK-IXMS
OFX2A1                                      WK-C-TMP-PARALNO-SEQNUM.
OFX2A1      MOVE TFSSTPL-PARALNO      TO    WK-C-TMP-PARALNO.
OFX2A1      MOVE TFSSTPL-SEQNUM       TO    WK-C-TMP-SEQNUM.
OFX2A1      MOVE WK-C-TMP-PARALNO-SEQNUM
OFX2A1                                TO    LK-IXMS-PCBNKRF.
OFX2A1      MOVE WK-VTF1B-BENENAME    TO    LK-IXMS-CUSTNAME.
OFX2A1      MOVE TFSSTPL-BNKENTTY     TO    LK-IXMS-BANKGP.
OFX2A1      MOVE TFSSTPL-CUYCD        TO    LK-IXMS-REMCUY.
OFX2A1      MOVE TFSSTPL-AMT          TO    LK-IXMS-REMAMT.
OFX2A1      MOVE SPACES               TO    LK-IXMS-DRACCCUY.
OFX2A2      IF  TFSSTPL-SWFTMGTY = "101"
OFX2A2          MOVE TFSSTPL-CUYCD    TO    LK-IXMS-DRACCCUY
OFX2A2      END-IF.
OFX2A1      EVALUATE WK-C-ACCNO-4
OFX2A1          WHEN "9"
OFX2A1              MOVE WS-ACCCUY    TO    LK-IXMS-CRACCCUY
OFX2A1          WHEN OTHER
OFX2A1              MOVE WK-C-LCUYCD  TO    LK-IXMS-CRACCCUY
OFX2A1      END-EVALUATE.
OFX2A1      MOVE SPACES               TO    LK-IXMS-DRACCTNO.
OFX2A2      IF  TFSSTPL-SWFTMGTY = "101"
OFX2A2          MOVE WK-101-TAG50H-ACCNO
OFX2A2                                TO    LK-IXMS-DRACCTNO
OFX2A2      END-IF.
OFX2A1      MOVE WS-ACCNO             TO    LK-IXMS-CRACCTNO.
OFX2A1      MOVE "ITT"                TO    LK-IXMS-PRDCD.
OFX2A1      CALL "TRFIFXMSQ"        USING   WK-IXMS.
OFX2A1      CANCEL "TRFIFXMSQ".
OFX2A1
OFX2A1      ADD  1                    TO    WK-N-XMS-RETRY-CTR.

OFX2A1      IF  LK-IXMS-ERRCDE      = SPACES
OFX2A1      AND LK-IXMS-EXCHGRTE    > 0
OFX2A1          MOVE LK-IXMS-EXCHGRTE TO    L-N-G-FX-RATE
OFX2A1          MOVE "OF"             TO    L-C-G-FX-RATE-TYPE
OFX2A3          MOVE LK-IXMS-CONVAMT  TO    L-N-G-P-AMT
OFX2A1          DISPLAY L-C-LOCAL-DATA-AREA
OFX2A1                  UPON          LOCAL-DATA-AREA
OFX2A1      ELSE
OFX2A1          EVALUATE LK-IXMS-REMHIND
OFX2A1              WHEN "I"
OFX2A1*--> For Interface Error, retry 3X before dropping the txn.
OFX2A1                  IF  WK-N-XMS-RETRY-CTR >= WK-N-XMS-RETRY-LMT
OFX2A1                      GO TO D899-XMS-ENQUIRY-EX
OFX2A1                  ELSE
OFX2A1                      GO TO D800-XMS-ENQUIRY
OFX2A1                  END-IF
OFX2A1          END-EVALUATE
OFX2A1      END-IF.
OFX2A1
OFX2A1*---------------------------------------------------------------*
OFX2A1 D899-XMS-ENQUIRY-EX.
OFX2A1*---------------------------------------------------------------*
OFX2A1      EXIT.

VASA01*===============================================================*
VASA01 R200-READ-TFSICLCA2.
VASA01*===============================================================*
VASA01      INITIALIZE TFSICLCA2-RECORD.
VASA01      MOVE  WS-ACCCUY           TO ACCCUY  OF TFSICLCA2R.
VASA01      MOVE  2                   TO ACCTYPE OF TFSICLCA2R.
VASA01      MOVE  WS-ACCNO            TO WK-C-CA-NO-EXPAND.
VASA01      CALL "SRFGRJTFY"        USING WK-C-CA-NO-EXPAND.
VASA01      INSPECT WK-C-CA-NO-EXPAND REPLACING ALL SPACE BY ZERO.
VASA01      MOVE WK-C-CA-NO-EXPAND    TO ACCNO OF TFSICLCA2R.
VASA01      MOVE  SPACE               TO WK-C-CA-EXIST.
VASA01      READ TFSICLCA2.
VASA01
VASA01      IF  WK-C-RECORD-NOT-FOUND
VASA01      OR  NOT WK-C-SUCCESSFUL
VASA01          MOVE    "N"           TO    WK-C-CA-EXIST
VASA01      END-IF.
VASA01
VASA01*===============================================================*
VASA01 R299-READ-TFSICLCA2-EX.
VASA01*===============================================================*

VASA01*===============================================================*
VASA01 R300-READ-TFSICLSA2.
VASA01*===============================================================*
VASA01      INITIALIZE TFSICLSA2-RECORD.
VASA01      MOVE  WS-ACCCUY           TO ACCCUY  OF TFSICLSA2R.
VASA01      MOVE  1                   TO ACCTYPE OF TFSICLSA2R.
VASA01      MOVE  WS-ACCNO            TO WK-C-CA-NO-EXPAND.
VASA01      CALL "SRFGRJTFY"        USING WK-C-CA-NO-EXPAND.

VASA01      INSPECT WK-C-CA-NO-EXPAND REPLACING ALL SPACE BY ZERO.
VASA01      MOVE WK-C-CA-NO-EXPAND    TO ACCNO OF TFSICLSA2R.
VASA01      MOVE  SPACE               TO WK-C-CA-EXIST.
VASA01      READ TFSICLSA2.
VASA01
VASA01      IF  WK-C-RECORD-NOT-FOUND
VASA01      OR  NOT WK-C-SUCCESSFUL
VASA01          MOVE    "N"           TO    WK-C-CA-EXIST
VASA01      END-IF.
VASA01
VASA01*===============================================================*
VASA01 R399-READ-TFSICLSA2-EX.
VASA01*===============================================================*

VASA02*===============================================================*
VASA02 R400-VASA-NAME-CHECK.
VASA02*===============================================================*
VASA02      IF WK-C-VASA-SW = "Y" AND
VASA02         (VERIADDI OF TFSICLCA2 = "Y" OR
VASA02          VERIADDI OF TFSICLSA2 = "Y")
VASA02      IF WK-C-VCSA-CUSTFNAM NOT = SPACES
VASA02          MOVE ACCTM1 OF TFSICLSA2 TO WK-C-STRING
VASA02          PERFORM C500-ACCNAME-VALIDATION
VASA02              THRU C599-ACCNAME-VALIDATION-EX
VASA02          MOVE WK-C-STRING TO WK-C-CON-SA-NAME-SUB
VASA02      END-IF
VASA02      IF WK-C-VCCA-CUSTFNAM NOT = SPACES
VASA02          MOVE ACCTM1 OF TFSICLCA2 TO WK-C-STRING
VASA02          PERFORM C500-ACCNAME-VALIDATION
VASA02              THRU C599-ACCNAME-VALIDATION-EX
VASA02          MOVE WK-C-STRING TO WK-C-CON-SA-NAME-SUB
VASA02      END-IF
VASA02      IF WK-C-ACCNO-4 = "1" AND
VASA02         WK-C-CON-NAME = WK-C-CON-SA-NAME-SUB)
VASA02      OR (WK-C-ACCNO-4 NOT = "1"
VASA02      AND WK-C-CON-NAME = WK-C-CON-CA-NAME-SUB)
VASA02          MOVE "Y" TO TAB-VAL(11)
VASA02                      WK-VTF1B-BENEFLG
VASA02          MOVE 14  TO WS-JUMP
VASA02          GO TO R499-VASA-NAME-CHECK-EX
VASA02      ELSE
VASA02          IF WK-C-VCSA-CUSTFNAM NOT = SPACES
VASA02              MOVE MACTN1 OF TFSICLSA2 TO WK-C-STRING
VASA02              PERFORM C500-ACCNAME-VALIDATION
VASA02                  THRU C599-ACCNAME-VALIDATION-EX
VASA02              MOVE WK-C-STRING TO WK-C-CON-SA-NAME-SUB
VASA02          END-IF
VASA02          IF WK-C-VCCA-CUSTFNAM NOT = SPACES
VASA02              MOVE MACTN1 OF TFSICLCA2 TO WK-C-STRING
VASA02              PERFORM C500-ACCNAME-VALIDATION
VASA02                  THRU C599-ACCNAME-VALIDATION-EX
VASA02              MOVE WK-C-STRING TO WK-C-CON-CA-NAME-SUB
VASA02          END-IF
VASA02          IF (WK-C-ACCNO-4 = "1" AND

VASA02              WK-C-CON-NAME = WK-C-CON-SA-NAME-SUB)
VASA02          OR (WK-C-ACCNO-4 NOT = "1"
VASA02          AND WK-C-CON-NAME = WK-C-CON-CA-NAME-SUB)
VASA02              MOVE "Y" TO TAB-VAL(11)
VASA02                          WK-VTF1B-BENEFLG
VASA02              MOVE 14  TO WS-JUMP
VASA02              GO TO R499-VASA-NAME-CHECK-EX
VASA02          ELSE
VASA02              MOVE WS-ACCNO         TO WK-ACRO-ACCNO
VASA02                                       WS-ACCNO-ORG
VASA02              MOVE WS-ACCCUY        TO WK-ACRO-CUYCD
VASA02              CALL "TRFACRO"  USING WK-ACRO
VASA02              IF  WK-ACRO-RACIND NOT = "Y"
VASA02              OR  WK-ACRO-MCUYCD = SPACES
VASA02                  MOVE "N" TO  WS-OKAY
VASA02                               WS-ROUTE
VASA02                  MOVE "N" TO  TAB-VAL(04)
VASA02                  MOVE 0   TO  WS-JUMP
VASA02                  INITIALIZE WK-C-RPRRSN-AREA
VASA02                  MOVE "RSN0107" TO WK-C-RPRCODE
VASA02                  PERFORM D400-PROCESS-RPRRSN
VASA02                      THRU D499-PROCESS-RPRRSN-EX
VASA02              ELSE
VASA02                  MOVE "Y" TO  WS-OKAY
VASA02                  MOVE 13  TO  WS-JUMP
VASA02              END-IF
VASA02          END-IF
VASA02      END-IF
VASA02      ELSE
VASA02          GO TO R499-VASA-NAME-CHECK-EX
VASA02      END-IF.
VASA02
VASA02*===============================================================*
VASA02 R499-VASA-NAME-CHECK-EX.
VASA02*===============================================================*

SGX201 Y100-VALIDATE-TAG59.
SGX206      INITIALIZE   WS-SUB.
SGX201      PERFORM Y200-VALIDATE-SWFTMGTY
SGX201          THRU Y299-VALIDATE-SWFTMGTY-EX.
SGX201      IF  WK-C-GLACT-VALID-MSG  = "Y"
SGX201      AND WK-C-GLACT-PREFIX NOT = SPACES
SGX201      AND TAG59-PTID-1(1:1)     = WK-C-GLACT-PREFIX
SGX205      AND TAG59-PTID(10:)       = SPACES
SGX205          PERFORM VARYING WS-SUB FROM 2 BY 1 UNTIL WS-SUB > 9
SGX205              IF TAG59-PTID(WS-SUB:1) IS    NUMERIC
SGX205              OR TAG59-PTID(WS-SUB:1) EQUAL SPACES
SGX205                  CONTINUE
SGX205              ELSE
SGX205                  GO TO Y199-VALIDATE-TAG59-EX
SGX205              END-IF
SGX205          END-PERFORM
SGX201          MOVE "Y"              TO    WK-C-TAG59-G-IND
SGX201          MOVE TAG59-PTID(2:)   TO    WS-BENEACC

SGX201                                       WS-ACCNO
SGX204          PERFORM Y150-FORMAT-ACCOUNT
SGX204              THRU Y199-FORMAT-ACCOUNT-EX
SGX201*SGX205     ELSE
SGX201*SGX205     MOVE TAG59-PTID           TO    WS-BENEACC
SGX201*SGX205                                      WS-ACCNO
SGX201      END-IF.
SGX201 Y199-VALIDATE-TAG59-EX.
SGX201      EXIT.
SGX201
SGX204 Y150-FORMAT-ACCOUNT.
SGX204      INITIALIZE                       WK-ACCNO-JUST-RT.
SGX204      MOVE WS-ACCNO             TO    WK-ACCNO-JUST-RT.
SGX204      CALL "TRFGRJTFY"        USING   WK-ACCNO-JUST-RT
SGX204      INSPECT WK-ACCNO-JUST-RT
SGX204              REPLACING ALL SPACE BY ZERO.
SGX204
SGX204      IF  WK-ACCNO-JR-1-10    = ZEROES
SGX204      AND WK-ACCNO-JR-11-18 IS NUMERIC
SGX204          MOVE WK-ACCNO-JR-11-18   TO    WS-BENEACC
SGX204                                         WS-ACCNO
SGX204      END-IF.
SGX204 Y199-FORMAT-ACCOUNT-EX.
SGX204      EXIT.
SGX204
SGX201 Y200-VALIDATE-SWFTMGTY.
SGX201*--> Validate if msgtype is eligible for CR GL Account
SGX201      MOVE "N"                  TO    WK-C-GLACT-VALID-MSG.
SGX201      IF  TFSSTPL-SWFTMGTY NOT = SPACES
SGX201      AND (TFSSTPL-SWFTMGTY EQUAL WK-C-GLACT-MTMSG1(1)
SGX201      OR  TFSSTPL-SWFTMGTY EQUAL WK-C-GLACT-MTMSG1(2)
SGX201      OR  TFSSTPL-SWFTMGTY EQUAL WK-C-GLACT-MTMSG1(3)
SGX201      OR  TFSSTPL-SWFTMGTY EQUAL WK-C-GLACT-MTMSG1(4))
SGX201          MOVE "Y"              TO    WK-C-GLACT-VALID-MSG
SGX201      END-IF.
SGX201
SGX201 Y299-VALIDATE-SWFTMGTY-EX.
SGX201      EXIT.
SGX201
SGX201 Y300-CHECK-TAG59-GLACT.
SGX201*--> Check if Beneficiary (Tag 59 has prefix 'G') is
SGX201*-- a valid GL Number. If valid, retrieve CR Payment Mode
SGX201*-- else route to repair. If no CR Payment Mode retrieve, route
SGX201*-- to repair.
SGX201      MOVE "N"                  TO    WK-C-VALID-GLACNO.
SGX201      MOVE SPACES               TO    WK-C-GLACT-RPRCODE.
SGX201      MOVE ZEROES               TO    WK-N-GLACNO.
SGX201
SGX201      PERFORM Y350-FORMAT-ACCOUNT
SGX201              THRU Y359-FORMAT-ACCOUNT-EX.
SGX201
SGX201      IF  WK-N-GLACNO IS NUMERIC
SGX201      AND WK-N-GLACNO NOT = ZEROES
SGX201          INITIALIZE                   WK-C-VGLAC-RECORD

SGX201          MOVE WK-N-GLACNO          TO    WK-N-VGLAC-GLNO6
SGX201          MOVE "3"                  TO    WK-N-VGLAC-OPTION
SGX201          CALL "TRFVGLAC"         USING   WK-C-VGLAC-RECORD
SGX201          IF  WK-C-VGLAC-GLIND = "Y"
SGX201              MOVE "Y"              TO    WK-C-VALID-GLACNO
SGX201              IF  WK-C-VGLAC-PYIND    = "Y"
SGX201              AND WK-C-VGLAC-O-PMODE NOT = SPACES
SGX201                  MOVE WK-C-VGLAC-O-PMODE
SGX201                                    TO    WK-C-GLACT-CRMOD
SGX201              ELSE
SGX201                  MOVE "RSN0358"    TO    WK-C-GLACT-RPRCODE
SGX201              END-IF
SGX201          ELSE
SGX201              MOVE "RSN0357"        TO    WK-C-GLACT-RPRCODE
SGX201          END-IF
SGX201      ELSE
SGX201          MOVE "RSN0357"            TO    WK-C-GLACT-RPRCODE
SGX201      END-IF.
SGX201
SGX201 Y399-CHECK-TAG59-GLACT-EX.
SGX201      EXIT.
SGX201
SGX201*---------------------------------------------------------------*
SGX201 Y350-FORMAT-ACCOUNT.
SGX201*---------------------------------------------------------------*
SGX201      INITIALIZE                       WK-ACCNO-JUST-RT.
SGX201      MOVE WS-ACCNO             TO    WK-ACCNO-JUST-RT.
SGX201      CALL "TRFGRJTFY"        USING   WK-ACCNO-JUST-RT
SGX201      INSPECT WK-ACCNO-JUST-RT
SGX201              REPLACING ALL SPACE BY ZERO.
SGX201
SGX201      IF  WK-ACCNO-JR-1-10    = ZEROES
SGX201      AND WK-ACCNO-JR-11-18 IS NUMERIC
SGX201          MOVE WK-ACCNO-JR-11-18   TO    WK-N-GLACNO
SGX201      END-IF.
SGX201
SGX201 Y359-FORMAT-ACCOUNT-EX.
SGX201      EXIT.
SGX201
SGX201 Y400-VALIDATE-GLACNO.
SGX201      PERFORM Y300-CHECK-TAG59-GLACT
SGX201          THRU Y399-CHECK-TAG59-GLACT-EX.
SGX201      IF  WK-C-VALID-GLACNO NOT = "Y"
SGX201          MOVE "N"              TO    WS-OKAY
SGX201          MOVE "Y"              TO    TAB-VAL(03)
SGX201          MOVE 0                TO    WS-JUMP
SGX201          INITIALIZE WK-C-RPRRSN-AREA
SGX201          MOVE WK-C-GLACT-RPRCODE  TO    WK-C-RPRCODE
SGX201          MOVE WS-ACCNO            TO    WK-C-ACCNO-RPR
SGX201          PERFORM D400-PROCESS-RPRRSN
SGX201              THRU D499-PROCESS-RPRRSN-EX
SGX201      ELSE
SGX201          MOVE "Y"              TO    WS-OKAY
SGX201                                       TAB-VAL(03)

SGX201                                       WK-VTF1B-BENEFLG
SGX201          MOVE 8                TO    WS-JUMP
SGX201          MOVE WK-C-VGLAC-FULNAME TO    WK-VTF1B-BENENAME
SGX201          MOVE WK-C-VGLAC-ADDR1   TO    WK-VTF1B-BENEADR1
SGX201          MOVE WK-C-VGLAC-ADDR2   TO    WK-VTF1B-BENEADR2
SGX201          MOVE WK-C-VGLAC-ADDR3   TO    WK-VTF1B-BENEADR3
SGX201          MOVE SPACE              TO    WK-VTF1B-BENEADR4
SGX201                                        WK-VTF1B-BENEADR5
SGX201                                        WK-VTF1B-BENEADR6
SGX201                                        WK-VTF1B-AOCD
SGX201          MOVE ZEROES             TO    WK-VTF1B-DOMBRCH
SGX201                                        WK-VTF1B-RESCD
SGX201                                        WK-VTF1B-HOLDCD1
SGX201                                        WK-VTF1B-HOLDCD2
SGX201                                        WK-VTF1B-HOLDCD3
SGX201          IF  WK-N-VGLAC-DOMBRCH IS NUMERIC
SGX201              MOVE WK-N-VGLAC-DOMBRCH
SGX201                                TO    WK-VTF1B-DOMBRCH
SGX201          END-IF
SGX201          MOVE WK-C-GLACT-CRMOD   TO    WK-VTF1B-PMODE
SGX201      END-IF.
SGX201
SGX201      PERFORM D300-LOGGING THRU D399-LOGGING-EX.
SGX201
SGX201 Y499-VALIDATE-GLACNO-EX.
SGX201      EXIT.
SGX201
   *===============================================================*
GP3K00 Y400-REFORMAT-TAG59F.
   *===============================================================*
GP3K00*--This routine will evaluate Tag59F's structured Lines 1-5 based on
GP3K00*--prefixes (e.g "1/" for Bene name etc..), remove prefixes and string
GP3K00*--multiple occurence of prefixes to be later used for Account Name check
GP3K00      INITIALIZE                      WK-C-VTAG59F-RECORD.
GP3K00
GP3K00      MOVE TAG59-PTID           TO    WS-BENEACC
GP3K00                                       WS-ACCNO.
GP3K00      MOVE TAG59-NAME           TO    WK-C-VTAG59F-I-BENE(1).
GP3K00      MOVE TAG59-LINE-3         TO    WK-C-VTAG59F-I-BENE(2).
GP3K00      MOVE TAG59-LINE-4         TO    WK-C-VTAG59F-I-BENE(3).
GP3K00      MOVE TAG59-LINE-5         TO    WK-C-VTAG59F-I-BENE(4).
GP3K00      MOVE "I"                  TO    WK-C-VTAG59F-REMIND.
GP3K00
GP3K00*--Call Utility pgm to remove prefixes "1/", "2/" etc from Tag59F
GP3K00      CALL  "TRFVTAG59F"      USING   WK-C-VTAG59F-RECORD.
GP3K00      CANCEL "TRFVTAG59F".
GP3K00
GP3K00      IF  WK-C-VTAG59F-ERROR-CD = SPACES
GP3K00*--------Move formatted values
GP3K00          MOVE WK-C-VTAG59F-O-BENE-NME      TO WS-NAME
GP3K00                                                WK-C-ACCNAME-RPR
GP3K00          MOVE WK-C-VTAG59F-O-BENE-ADR(1)  TO WS-ADDR1
GP3K00          MOVE WK-C-VTAG59F-O-BENE-ADR(2)  TO WS-ADDR2
GP3K00          MOVE WK-C-VTAG59F-O-BENE-ADR(3)  TO WS-ADDR3

GP3K00      ELSE
GP3K00*--------Move unformatted values
GP3K00          MOVE TAG59-NAME                   TO WS-NAME
GP3K00                                                WK-C-ACCNAME-RPR
GP3K00          MOVE TAG59-LINE-3                 TO WS-ADDR1
GP3K00          MOVE TAG59-LINE-4                 TO WS-ADDR2
GP3K00          MOVE TAG59-LINE-5                 TO WS-ADDR3
GP3K00      END-IF.
   *===============================================================*
GP3K00 Y499-REFORMAT-TAG59F-EX.
   *===============================================================*
GP3K00      EXIT.
   /
5Q2JE2*5Q1ARV D500-PROCESS-ACC-ERR SECTION.
5Q2JE2*5Q1ARV D500-ENTRY.
5Q2JE2*5Q1ARV
5Q2JE2*5Q1ARV      EVALUATE TRUE
5Q2JE2*5Q1ARV          WHEN WK-C-VCSA-ERROR-CD NOT = SPACES
5Q2JE2*5Q1ARV              MOVE WK-C-VCSA-ERROR-CD TO WK-C-RPRCODE
5Q2JE2*5Q1ARV              MOVE WK-C-VCSA-SEG-CODE TO WK-C-SEGCDE
5Q2JE2*5Q1ARV              MOVE WK-N-VCSA-STAFFIND TO WK-N-STAFFIND
5Q2JE2*5Q1ARV              MOVE SPACES             TO WK-C-QRATE
5Q2JE2*5Q1ARV          WHEN WK-C-VCCA-ERROR-CD NOT = SPACES
5Q2JE2*5Q1ARV              MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
5Q2JE2*5Q1ARV              MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCDE
5Q2JE2*5Q1ARV              MOVE WK-N-VCCA-STAFFIND TO WK-N-STAFFIND
5Q2JE2*5Q1ARV              MOVE SPACES             TO WK-C-QRATE
5Q2JE2*5Q1ARV      END-EVALUATE
5Q2JE2*5Q1ARV      MOVE WS-ACCNO             TO WK-C-ACCNO-RPR
5Q2JE2*5Q1ARV      PERFORM D400-PROCESS-RPRRSN
5Q2JE2*5Q1ARV              THRU D499-PROCESS-RPRRSN-EX.
5Q2JE2*5Q1ARV
5Q2JE2*5Q1ARV D599-PROCESS-ACC-ERR-EX.
5Q2JE2*5Q1ARV      EXIT.
5Q2JE2*5Q1ARV
5Q2JE2*5Q1ARV D600-PROCESS-ACC-ERR SECTION.
5Q2JE2*5Q1ARV D600-ENTRY.
5Q2JE2*5Q1ARV      EVALUATE TRUE
5Q2JE2*5Q1ARV          WHEN WK-C-VCSA-ERROR-CD NOT = SPACES
5Q2JE2*5Q1ARV              MOVE WK-C-VCSA-ERROR-CD TO WK-C-RPRCODE
5Q2JE2*5Q1ARV              MOVE WK-C-VCSA-SEG-CODE TO WK-C-SEGCDE
5Q2JE2*5Q1ARV              MOVE WK-N-VCSA-STAFFIND TO WK-N-STAFFIND
5Q2JE2*5Q1ARV              MOVE SPACES             TO WK-C-QRATE
5Q2JE2*5Q1ARV          WHEN WK-C-VCCA-ERROR-CD NOT = SPACES
5Q2JE2*5Q1ARV              MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
5Q2JE2*5Q1ARV              MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCDE
5Q2JE2*5Q1ARV              MOVE WK-N-VCCA-STAFFIND TO WK-N-STAFFIND
5Q2JE2*5Q1ARV              MOVE SPACES             TO WK-C-QRATE
5Q2JE2*5Q1ARV          WHEN WK-C-VCFA-ERROR-CD NOT = SPACES
5Q2JE2*5Q1ARV              MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
5Q2JE2*5Q1ARV              MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCDE
5Q2JE2*5Q1ARV              MOVE WK-N-VCFA-STAFFIND TO WK-N-STAFFIND
5Q2JE2*5Q1ARV              MOVE SPACES             TO WK-C-QRATE
5Q2JE2*5Q1ARV      END-EVALUATE

5Q2JE2*5Q1ARV      MOVE WS-ACCNO             TO WK-C-ACCNO-RPR
5Q2JE2*5Q1ARV      PERFORM D400-PROCESS-RPRRSN
5Q2JE2*5Q1ARV              THRU D499-PROCESS-RPRRSN-EX.
5Q2JE2*5Q1ARV
5Q2JE2*5Q1ARV
5Q2JE2*5Q1ARV D699-PROCESS-ACC-ERR-EX.
5Q2JE2*5Q1ARV      EXIT.
                   EJECT

              Z000-END-PROGRAM.
                   CLOSE  TFSSTPL
GPI201                    UFIMIJCON
VASA01                    TFSICLCA2
VASA03                    TFSICLSA2
       		                 TFSCLSYS.
                     EXIT PROGRAM .
