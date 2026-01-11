*======================================================================
* AMENDMENT LOG:
*======================================================================
* SEARCH KEY     DATE        DESCRIPTION
*----------------------------------------------------------------------
* VASA03 - VENUFQ  - 03/07/2025  - PROJ#JW02 UOVB SG VASA
*                                  Update reg addr field length
*                                  Update position of filler field for
*                                  master account
*----------------------------------------------------------------------
* VASA02 - VENUFQ  - 12/06/2025  - PROJ#JW02 UOVB SG VASA
*                                  Additional field and re-mapping of
*                                  VASA fields.
*----------------------------------------------------------------------
* VASA01 - VENUFQ  - 26/05/2025  - PROJ#JW02 UOVB SG VASA
*                                  Initial version.
*----------------------------------------------------------------------
       01  WK-ISLCAVQ.
           05  WK-ISLCAVQ-INPUT.
               10  WK-I-ISLCAVQ-MQ-HEADER.
                   15  WK-I-ISLCAVQ-APPLI-SYS        PIC X(05).
      *                TRM for REMITTANCE and TRD for TRADE
                   15  WK-I-MQ-SER-NO                 PIC S9(10).
      *                Generated and used by MQ
                   15  WK-I-RTN-DATAQ-NAME            PIC X(10).
                   15  WK-I-MQ-RTN-CODE               PIC X(04).
                   15  FILLER                         PIC X(03).
               10  WK-I-ISLCAVQ-MSG-HEADER.
                   15  WK-I-ISLCAVQ-TRAN-CODE         PIC S9(04).
                   15  WK-I-ISLCAVQ-TELLER-ID         PIC X(04).
                   15  WK-I-ISLCAVQ-TELLER-TRANS-SEQNO PIC S9(04).
                   15  WK-I-ISLCAVQ-REVERSE-IND       PIC X(01).
      *                N - normal transaction
                   15  WK-I-ISLCAVQ-PTIME             PIC S9(04).
                   15  WK-I-ISLCAVQ-CCY-CODE-HDR      PIC X(03).
                   15  WK-I-ISLCAVQ-ACC-TYPE-HDR      PIC S9(02).
      *                02 - CA
                   15  WK-I-ISLCAVQ-ACC-NO-HDR        PIC S9(18).
                   15  WK-I-ISLCAVQ-TRNNO             PIC X(12).
                   15  WK-I-ISLCAVQ-TERM-ID           PIC X(04).
                   15  WK-I-ISLCAVQ-SUPOVR-ID         PIC X(08).
                   15  FILLER                         PIC X(36).
                   15  WK-I-ISLCAVQ-REQ-FUNC-CODE     PIC S9(02).
      *                02 - Get CA information
               10  WK-I-ISLCAVQ-INPUT-DETAILS.
                   15  WK-I-ISLCAVQ-CCY-CODE          PIC X(03).
                   15  WK-I-ISLCAVQ-ACC-TYPE          PIC S9(02).
                   15  WK-I-ISLCAVQ-ACC-NO            PIC S9(18).
           05  WK-ISLCAVQ-OUTPUT.
               10  WK-O-ISLCAVQ-MQ-HEADER.
                   15  WK-O-ISLCAVQ-APPLI-SYS        PIC X(05).
      *                TRM for REMITTANCE and TRD for TRADE
                   15  WK-O-MQ-SER-NO                 PIC S9(10).
      *                Generated and used by MQ
                   15  WK-O-RTN-DATAQ-NAME            PIC X(10).
                   15  WK-O-MQ-RTN-CODE               PIC X(04).
                   15  FILLER                         PIC X(03).
               10  WK-O-ISLCAVQ-MSG-HEADER.
                   15  WK-O-ISLCAVQ-TRAN-CODE         PIC S9(04).
                   15  WK-O-ISLCAVQ-TELLER-ID         PIC X(04).
                   15  WK-O-ISLCAVQ-TELLER-TRANS-SEQNO PIC S9(04).
                   15  WK-O-ISLCAVQ-REVERSE-IND       PIC X(01).
      *                N - normal transaction
                   15  WK-O-ISLCAVQ-PTIME             PIC S9(04).
                   15  WK-O-ISLCAVQ-CCY-CODE-HDR      PIC X(03).
                   15  WK-O-ISLCAVQ-ACC-TYPE-HDR      PIC S9(02).
      *                02 - CA
                   15  WK-O-ISLCAVQ-ACC-NO-HDR        PIC S9(18).
                   15  WK-O-ISLCAVQ-TRNNO             PIC X(12).
                   15  WK-O-ISLCAVQ-TERM-ID           PIC X(04).
                   15  WK-O-ISLCAVQ-SUPOVR-ID         PIC X(08).
                   15  FILLER                         PIC X(36).
                   15  WK-O-ISLCAVQ-REQ-FUNC-CODE     PIC S9(02).
      *                02 - Get CA information
               10  WK-O-ISLCAVQ-OUTPUT-DETAILS.
                   15  WK-O-ISLCAVQ-MSG-RET-CODE      PIC X(01).
                   15  WK-O-ISLCAVQ-ERROR-CODE        PIC S9(04).
                   15  WK-O-ISLCAVQ-ERROR-TEXT        PIC X(30).
                   15  WK-O-ISLCAVQ-STATUS            PIC S9(02).
      *                00 - NORMAL
      *                02 - DORMANT
      *                03 - CLOSED
                   15  WK-O-ISLCAVQ-HOLD-A            PIC S9(02).
                   15  WK-O-ISLCAVQ-HOLD-B            PIC S9(02).
                   15  WK-O-ISLCAVQ-HOLD-C            PIC S9(02).
                   15  WK-O-ISLCAVQ-SHORT-NAME        PIC X(20).
                   15  WK-O-ISLCAVQ-NAME-ADDR1        PIC X(40).
                   15  WK-O-ISLCAVQ-NAME-ADDR2        PIC X(40).
                   15  WK-O-ISLCAVQ-NAME-ADDR3        PIC X(40).
                   15  WK-O-ISLCAVQ-NAME-ADDR4        PIC X(40).
                   15  WK-O-ISLCAVQ-NAME-ADDR5        PIC X(40).
                   15  WK-O-ISLCAVQ-NAME-ADDR6        PIC X(40).
                   15  WK-O-ISLCAVQ-NAME-ADDR7        PIC X(40).
                   15  WK-O-ISLCAVQ-POSTAL-ID         PIC X(10).
                   15  WK-O-ISLCAVQ-OL-NO-IDS         PIC S9(01).
      *                # of ID
                   15  WK-O-ISLCAVQ-IDA-CDE           PIC X(01).
      *                1st ID prefix code
                   15  WK-O-ISLCAVQ-IDA-NUM           PIC X(30).
      *                1st ID number
                   15  WK-O-ISLCAVQ-IDB-CDE           PIC X(01).
      *                2nd ID prefix code
                   15  WK-O-ISLCAVQ-IDB-NUM           PIC X(30).
      *                2nd ID number
                   15  WK-O-ISLCAVQ-STAFF-CODE        PIC S9(02).
      *                00 - Non-staff
      *                01 - Non-staff
                   15  WK-O-ISLCAVQ-RES-CODE          PIC S9(02).
      *                Not used
                   15  WK-O-ISLCAVQ-INFO-RACE         PIC S9(02).
                   15  WK-O-ISLCAVQ-BIRTH-DATE        PIC S9(08).
                   15  WK-O-ISLCAVQ-INFO-SEX          PIC X(01).
                   15  WK-O-ISLCAVQ-INFO-RES-CTRY     PIC S9(04).
                   15  WK-O-ISLCAVQ-USER-CD-CUST-TYPE PIC S9(03).
      *                Customer Type
                   15  WK-O-ISLCAVQ-IL-LIMIT-1        PIC S9(13)V99.
                   15  WK-O-ISLCAVQ-IL-LIMIT-2        PIC S9(13)V99.
                   15  WK-O-ISLCAVQ-IL-LIMIT-REST     PIC S9(13)V99.
                   15  WK-O-ISLCAVQ-OL-AVAIL-BAL-SIGN PIC X(01).
                   15  WK-O-ISLCAVQ-OL-AVAIL-BAL      PIC S9(13)V99.
      *                Available Balance
                   15  WK-O-ISLCAVQ-OL-CUR-BAL-SIGN   PIC X(01).
                   15  WK-O-ISLCAVQ-OL-CUR-BAL        PIC S9(13)V99.
      *                Current Balance
                   15  WK-O-ISLCAVQ-CCY-CODE          PIC X(03).
                   15  WK-O-ISLCAVQ-PROD-TYPE         PIC X(02).
                   15  WK-O-ISLCAVQ-CIFNO             PIC X(19).
                   15  WK-O-ISLCAVQ-OFFICER-CODE      PIC X(10).
                   15  WK-O-ISLCAVQ-BANK-CODE         PIC S9(02).
                   15  WK-O-ISLCAVQ-BRANCH-CODE       PIC S9(03).
                   15  WK-O-ISLCAVQ-ID-ISS-CTY-CODE-1 PIC X(03).
      *                Issue Country Code 1
                   15  WK-O-ISLCAVQ-ID-ISS-CTY-CODE-2 PIC X(03).
      *                Issue Country Code 2
      * start of 6263TC *
                   15  WK-O-ISLCAVQ-SEG-CODE          PIC X(01).
                   15  WK-O-ISLCAVQ-CTO-CODE          PIC X(10).
      VASA02*      15  WK-O-ISLCAVQ-REG-ADDR1         PIC X(35).
      VASA02*      15  WK-O-ISLCAVQ-REG-ADDR2         PIC X(35).
      VASA02*      15  WK-O-ISLCAVQ-REG-ADDR3         PIC X(35).
      VASA02*      15  WK-O-ISLCAVQ-REG-ADDR4         PIC X(35).
      VASA03*VASA02 15  WK-O-ISLCAVQ-REG-ADDR1        PIC X(40).
      VASA03*VASA02 15  WK-O-ISLCAVQ-REG-ADDR2        PIC X(40).
      VASA03*VASA02 15  WK-O-ISLCAVQ-REG-ADDR3        PIC X(40).
      VASA03*VASA02 15  WK-O-ISLCAVQ-REG-ADDR4        PIC X(40).
      VASA03       15  WK-O-ISLCAVQ-REG-ADDR1         PIC X(35).
      VASA03       15  WK-O-ISLCAVQ-REG-ADDR2         PIC X(35).
      VASA03       15  WK-O-ISLCAVQ-REG-ADDR3         PIC X(35).
      VASA03       15  WK-O-ISLCAVQ-REG-ADDR4         PIC X(35).
                   15  WK-O-ISLCAVQ-REG-BLK           PIC X(07).
                   15  WK-O-ISLCAVQ-REG-STOREY        PIC X(04).
                   15  WK-O-ISLCAVQ-REG-UNIT          PIC X(07).
                   15  WK-O-ISLCAVQ-REG-PO-BOX        PIC X(06).
                   15  WK-O-ISLCAVQ-REG-BUILD         PIC X(45).
                   15  WK-O-ISLCAVQ-REG-STREET        PIC X(32).
                   15  WK-O-ISLCAVQ-REG-STATE         PIC X(20).
                   15  WK-O-ISLCAVQ-REG-POSTAL        PIC 9(09).
                   15  WK-O-ISLCAVQ-REG-CTY-CODE      PIC X(03).
                   15  WK-O-ISLCAVQ-REG-ADDR-TYPE     PIC X(01).
                   15  WK-O-ISLCAVQ-REG-ADDR-FORMAT   PIC X(01).
                   15  WK-O-ISLCAVQ-REG-FOREIGN-IND   PIC X(01).
                   15  WK-O-ISLCAVQ-REG-VERIFY-IND    PIC X(01).
                   15  WK-O-ISLCAVQ-HOLDMAIL-IND      PIC X(01).
                   15  WK-O-ISLCAVQ-ACCT-ADDR-FORMAT  PIC X(01).
                   15  WK-O-ISLCAVQ-PRI-ID-NUM        PIC X(30).
                   15  WK-O-ISLCAVQ-PRI-ID-TYPE       PIC X(02).
                   15  WK-O-ISLCAVQ-PRI-OWN-CTY-CODE  PIC X(03).
                   15  WK-O-ISLCAVQ-PRI-DOB           PIC 9(08).
                   15  WK-O-ISLCAVQ-PRI-CTZEN-CTY-CODE PIC X(03).
                   15  WK-O-ISLCAVQ-ENG-ACC-NAME-1    PIC X(40).
                   15  WK-O-ISLCAVQ-ENG-ACC-NAME-2    PIC X(40).
                   15  WK-O-ISLCAVQ-ACC-NAME-COUNT    PIC X(01).
      * end of 6263TC *
                   15  WK-O-ISLCAVQ-ACCT-NRA          PIC X(03).
                   15  WK-O-ISLCAVQ-HOLD-TXT-A        PIC X(30).
                   15  WK-O-ISLCAVQ-HOLD-TXT-B        PIC X(30).
                   15  WK-O-ISLCAVQ-HOLD-TXT-C        PIC X(30).
                   15  WK-O-ISLCAVQ-VER-ADD-IND       PIC X(01).
      * Start of VASA fields
      VASA03       15  FILLER                         PIC X(01).
                   15  WK-O-ISLCAVQ-SMACT             PIC 9(18).
      VASA03*VASA02 15  FILLER                        PIC X(01).
                   15  WK-O-ISLCAVQ-SMPURP            PIC X(03).
      VASA02*      15  WK-O-ISLCAVQ-SMRAD1            PIC X(35).
      VASA02*      15  WK-O-ISLCAVQ-SMRAD2            PIC X(35).
      VASA02*      15  WK-O-ISLCAVQ-SMRAD3            PIC X(35).
      VASA02*      15  WK-O-ISLCAVQ-SMRAD4            PIC X(35).
      VASA02*      15  WK-O-ISLCAVQ-SMRBLK            PIC X(07).
      VASA02*      15  WK-O-ISLCAVQ-SMRSTO            PIC X(04).
      VASA02*      15  WK-O-ISLCAVQ-SMRUNT            PIC X(07).
      VASA02*      15  WK-O-ISLCAVQ-SMRPOB            PIC X(06).
      VASA02*      15  WK-O-ISLCAVQ-SMRBLD            PIC X(45).
      VASA02*      15  WK-O-ISLCAVQ-SMRSTR            PIC X(32).
      VASA02*      15  WK-O-ISLCAVQ-SMRSTA            PIC X(20).
      VASA02*      15  WK-O-ISLCAVQ-SMRZIP            PIC S9(09).
      VASA02*      15  WK-O-ISLCAVQ-SMRCOU            PIC X(03).
      VASA02*      15  WK-O-ISLCAVQ-SMRATY            PIC X(01).
      VASA02*      15  WK-O-ISLCAVQ-SMRADF            PIC X(01).
      VASA02*      15  WK-O-ISLCAVQ-SMRFOR            PIC X(01).
      VASA02*      15  WK-O-ISLCAVQ-BADACA            PIC X(01).
      VASA02*      15  WK-O-ISLCAVQ-HLDMCA            PIC X(01).
      VASA02*      15  WK-O-ISLCAVQ-ADFHCA            PIC X(01).
      VASA02*      15  WK-O-ISLCAVQ-SMADR1            PIC X(40).
      VASA02*      15  WK-O-ISLCAVQ-SMADR2            PIC X(40).
      VASA02*      15  WK-O-ISLCAVQ-SMADR3            PIC X(40).
      VASA02*      15  WK-O-ISLCAVQ-SMADR4            PIC X(40).
      VASA02*      15  WK-O-ISLCAVQ-SMADR5            PIC X(40).
      VASA02*      15  WK-O-ISLCAVQ-SMADR6            PIC X(40).
      VASA02*      15  WK-O-ISLCAVQ-SMADR7            PIC X(40).
      VASA02*      15  WK-O-ISLCAVQ-SMAZIP            PIC X(10).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD1            PIC S9(02).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD2            PIC S9(02).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD3            PIC S9(02).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD4            PIC S9(02).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD5            PIC S9(02).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD6            PIC S9(02).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD7            PIC S9(02).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD8            PIC S9(02).
      VASA02*      15  WK-O-ISLCAVQ-SMHCD9            PIC S9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD1            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD2            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD3            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD4            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD5            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD6            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD7            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD8            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCD9            PIC 9(02).
      VASA02       15  WK-O-ISLCAVQ-SMHCN1            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-SMHCN2            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-SMHCN3            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-SMHCN4            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-SMHCN5            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-SMHCN6            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-SMHCN7            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-SMHCN8            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-SMHCN9            PIC X(30).
      VASA02       15  WK-O-ISLCAVQ-ACCTM1            PIC X(35).
      VASA02       15  WK-O-ISLCAVQ-ACCTM2            PIC X(35).
      VASA02       15  WK-O-ISLCAVQ-ACCTLN            PIC X(140).
      VASA02       15  WK-O-ISLCAVQ-CUSTN1            PIC X(35).
      VASA02       15  WK-O-ISLCAVQ-CUSTN2            PIC X(35).
      VASA02       15  WK-O-ISLCAVQ-CUSTLN            PIC X(140).
      VASA02       15  WK-O-ISLCAVQ-MACTN1            PIC X(35).
      VASA02       15  WK-O-ISLCAVQ-MACTN2            PIC X(35).
      VASA02       15  WK-O-ISLCAVQ-MACTLN            PIC X(140).
      VASA02       15  WK-O-ISLCAVQ-ADD1MA            PIC X(40).
      VASA02       15  WK-O-ISLCAVQ-ADD2MA            PIC X(40).
      VASA02       15  WK-O-ISLCAVQ-ADD3MA            PIC X(40).
      VASA02       15  WK-O-ISLCAVQ-ADD4MA            PIC X(40).
      VASA02       15  WK-O-ISLCAVQ-ABNMMA            PIC X(07).
      VASA02       15  WK-O-ISLCAVQ-ASTYMA            PIC X(04).
      VASA02       15  WK-O-ISLCAVQ-AUNMMA            PIC X(07).
      VASA02       15  WK-O-ISLCAVQ-ABOXMA            PIC X(06).
      VASA02       15  WK-O-ISLCAVQ-ABDNMA            PIC X(45).
      VASA02       15  WK-O-ISLCAVQ-ASTNMA            PIC X(32).
      VASA02       15  WK-O-ISLCAVQ-CITYMA            PIC X(20).
      VASA02       15  WK-O-ISLCAVQ-ZIPMA             PIC 9(09).
      VASA02       15  WK-O-ISLCAVQ-COUNMA            PIC X(03).
      VASA02       15  WK-O-ISLCAVQ-ADTPMA            PIC X(01).
      VASA02       15  WK-O-ISLCAVQ-ADFMMA            PIC X(01).
      VASA02       15  WK-O-ISLCAVQ-FORNMA            PIC X(01).
      VASA02       15  WK-O-ISLCAVQ-BADAMA            PIC X(01).
      VASA02       15  WK-O-ISLCAVQ-HLDMMA            PIC X(01).
      VASA02       15  WK-O-ISLCAVQ-ADFHMA            PIC X(01).
      VASA02       15  WK-O-ISLCAVQ-SSPI              PIC X(10).
      VASA02       15  WK-O-ISLCAVQ-LCKI              PIC X(10).
      VASA03       15  WK-O-ISLCAVQ-STFFLG            PIC 9(02).
      VASA02       15  FILSUB                         PIC X(628).
