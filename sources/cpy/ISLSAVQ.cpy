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
       01  WK-ISLSAVQ.
           05  WK-ISLSAVQ-INPUT.
               10  WK-I-ISLSAVQ-MQ-HEADER.
                   15  WK-I-ISLSAVQ-APPLI-SYS        PIC X(05).
      *                TRM for REMITTANCE and TRD for TRADE
                   15  WK-I-MQ-SER-NO                 PIC S9(10).
      *                Generated and used by MQ
                   15  WK-I-RTN-DATAQ-NAME            PIC X(10).
                   15  WK-I-MQ-RTN-CODE               PIC X(04).
                   15  FILLER                         PIC X(03).
               10  WK-I-ISLSAVQ-MSG-HEADER.
                   15  WK-I-ISLSAVQ-TRAN-CODE         PIC S9(04).
                   15  WK-I-ISLSAVQ-TELLER-ID         PIC X(04).
                   15  WK-I-ISLSAVQ-TELLER-TRANS-SEQNO PIC S9(04).
                   15  WK-I-ISLSAVQ-REVERSE-IND       PIC X(01).
      *                N - normal transaction
                   15  WK-I-ISLSAVQ-PTIME             PIC S9(04).
                   15  WK-I-ISLSAVQ-CCY-CODE-HDR      PIC X(03).
                   15  WK-I-ISLSAVQ-ACC-TYPE-HDR      PIC S9(02).
      *                02 - CA
                   15  WK-I-ISLSAVQ-ACC-NO-HDR        PIC S9(18).
                   15  WK-I-ISLSAVQ-TRNNO             PIC X(12).
                   15  WK-I-ISLSAVQ-TERM-ID           PIC X(04).
                   15  WK-I-ISLSAVQ-SUPOVR-ID         PIC X(08).
                   15  FILLER                         PIC X(36).
                   15  WK-I-ISLSAVQ-REQ-FUNC-CODE     PIC S9(02).
      *                02 - Get CA information
               10  WK-I-ISLSAVQ-INPUT-DETAILS.
                   15  WK-I-ISLSAVQ-CCY-CODE          PIC X(03).
                   15  WK-I-ISLSAVQ-ACC-TYPE          PIC S9(02).
                   15  WK-I-ISLSAVQ-ACC-NO            PIC S9(18).
           05  WK-ISLSAVQ-OUTPUT.
               10  WK-O-ISLSAVQ-MQ-HEADER.
                   15  WK-O-ISLSAVQ-APPLI-SYS        PIC X(05).
      *                TRM for REMITTANCE and TRD for TRADE
                   15  WK-O-MQ-SER-NO                 PIC S9(10).
      *                Generated and used by MQ
                   15  WK-O-RTN-DATAQ-NAME            PIC X(10).
                   15  WK-O-MQ-RTN-CODE               PIC X(04).
                   15  FILLER                         PIC X(03).
               10  WK-O-ISLSAVQ-MSG-HEADER.
                   15  WK-O-ISLSAVQ-TRAN-CODE         PIC S9(04).
                   15  WK-O-ISLSAVQ-TELLER-ID         PIC X(04).
                   15  WK-O-ISLSAVQ-TELLER-TRANS-SEQNO PIC S9(04).
                   15  WK-O-ISLSAVQ-REVERSE-IND       PIC X(01).
      *                N - normal transaction
                   15  WK-O-ISLSAVQ-PTIME             PIC S9(04).
                   15  WK-O-ISLSAVQ-CCY-CODE-HDR      PIC X(03).
                   15  WK-O-ISLSAVQ-ACC-TYPE-HDR      PIC S9(02).
      *                02 - CA
                   15  WK-O-ISLSAVQ-ACC-NO-HDR        PIC S9(18).
                   15  WK-O-ISLSAVQ-TRNNO             PIC X(12).
                   15  WK-O-ISLSAVQ-TERM-ID           PIC X(04).
                   15  WK-O-ISLSAVQ-SUPOVR-ID         PIC X(08).
                   15  FILLER                         PIC X(36).
                   15  WK-O-ISLSAVQ-REQ-FUNC-CODE     PIC S9(02).
      *                01 - Get SA information
               10  WK-O-ISLSAVQ-OUTPUT-DETAILS.
                   15  WK-O-ISLSAVQ-MSG-RET-CODE      PIC X(01).
                   15  WK-O-ISLSAVQ-ERROR-CODE        PIC S9(04).
                   15  WK-O-ISLSAVQ-ERROR-TEXT        PIC X(30).
                   15  WK-O-ISLSAVQ-NAME-ADDR1        PIC X(40).
                   15  WK-O-ISLSAVQ-NAME-ADDR2        PIC X(40).
                   15  WK-O-ISLSAVQ-NAME-ADDR3        PIC X(40).
                   15  WK-O-ISLSAVQ-NAME-ADDR4        PIC X(40).
                   15  WK-O-ISLSAVQ-NAME-ADDR5        PIC X(40).
                   15  WK-O-ISLSAVQ-NAME-ADDR6        PIC X(40).
                   15  WK-O-ISLSAVQ-NAME-ADDR7        PIC X(40).
                   15  WK-O-ISLSAVQ-POSTAL-ID         PIC X(10).
                   15  WK-O-ISLSAVQ-CUST-TYPE         PIC S9(02).
      *                Customer Type
                   15  WK-O-ISLSAVQ-STATUS            PIC S9(02).
      *                00 - NORMAL
      *                02 - DORMANT
      *                03 - CLOSED
                   15  WK-O-ISLSAVQ-DC-SHLD-A         PIC S9(02).
                   15  WK-O-ISLSAVQ-DC-SHLD-B         PIC S9(02).
                   15  WK-O-ISLSAVQ-DC-SHLD-C         PIC S9(02).
                   15  WK-O-ISLSAVQ-DC-SSHRT-NME      PIC X(20).
                   15  WK-O-ISLSAVQ-NO-IDS            PIC S9(01).
      *                # of ID
                   15  WK-O-ISLSAVQ-IDA-CDE           PIC X(01).
      *                1st ID prefix code
                   15  WK-O-ISLSAVQ-IDA-NUM           PIC X(30).
      *                1st ID number
                   15  WK-O-ISLSAVQ-IDB-CDE           PIC X(01).
      *                2nd ID prefix code
                   15  WK-O-ISLSAVQ-IDB-NUM           PIC X(30).
      *                2nd ID number
                   15  WK-O-ISLSAVQ-DC-SSTAFF-CD      PIC S9(02).
      *                00 - Non-Staff
      *                01 - Non-staff
                   15  WK-O-ISLSAVQ-DC-SRES-CD        PIC S9(02).
      *                Not used
                   15  WK-O-ISLSAVQ-DC-SRACE-CD       PIC S9(02).
                   15  WK-O-ISLSAVQ-DC-SBIRTH-DATE    PIC S9(08).
                   15  WK-O-ISLSAVQ-DC-SSEX-CD        PIC X(01).
                   15  WK-O-ISLSAVQ-DC-SRES-CTRY-CD   PIC S9(04).
                   15  WK-O-ISLSAVQ-CURRENT-BAL       PIC S9(13)V99.
      *                Current Balance
                   15  WK-O-ISLSAVQ-AVAIL-BAL         PIC S9(13)V99.
      *                Available Balance
                   15  WK-O-ISLSAVQ-PROD-TYPE         PIC X(02).
                   15  WK-O-ISLSAVQ-CIFNO             PIC X(19).
                   15  WK-O-ISLSAVQ-OFFICER-CODE      PIC X(10).
                   15  WK-O-ISLSAVQ-BANK-CODE         PIC S9(02).
                   15  WK-O-ISLSAVQ-BRANCH-CODE       PIC S9(03).
                   15  WK-O-ISLSAVQ-ID-ISS-CTY-CODE-1 PIC X(03).
      *                Issue Country Code 1
                   15  WK-O-ISLSAVQ-ID-ISS-CTY-CODE-2 PIC X(03).
      *                Issue Country Code 2
      * Start of 6263TC *
                   15  WK-O-ISLSAVQ-SEG-CODE          PIC X(01).
                   15  WK-O-ISLSAVQ-CTO-CODE          PIC X(10).
      VASA02*      15  WK-O-ISLSAVQ-REG-ADDR1         PIC X(35).
      VASA02*      15  WK-O-ISLSAVQ-REG-ADDR2         PIC X(35).
      VASA02*      15  WK-O-ISLSAVQ-REG-ADDR3         PIC X(35).
      VASA02*      15  WK-O-ISLSAVQ-REG-ADDR4         PIC X(35).
      VASA03*VASA02 15  WK-O-ISLSAVQ-REG-ADDR1        PIC X(40).
      VASA03*VASA02 15  WK-O-ISLSAVQ-REG-ADDR2        PIC X(40).
      VASA03*VASA02 15  WK-O-ISLSAVQ-REG-ADDR3        PIC X(40).
      VASA03*VASA02 15  WK-O-ISLSAVQ-REG-ADDR4        PIC X(40).
      VASA03       15  WK-O-ISLSAVQ-REG-ADDR1         PIC X(35).
      VASA03       15  WK-O-ISLSAVQ-REG-ADDR2         PIC X(35).
      VASA03       15  WK-O-ISLSAVQ-REG-ADDR3         PIC X(35).
      VASA03       15  WK-O-ISLSAVQ-REG-ADDR4         PIC X(35).
                   15  WK-O-ISLSAVQ-REG-BLK           PIC X(07).
                   15  WK-O-ISLSAVQ-REG-STOREY        PIC X(04).
                   15  WK-O-ISLSAVQ-REG-UNIT          PIC X(07).
                   15  WK-O-ISLSAVQ-REG-PO-BOX        PIC X(06).
                   15  WK-O-ISLSAVQ-REG-BUILD         PIC X(45).
                   15  WK-O-ISLSAVQ-REG-STREET        PIC X(32).
                   15  WK-O-ISLSAVQ-REG-STATE         PIC X(20).
                   15  WK-O-ISLSAVQ-REG-POSTAL        PIC 9(09).
                   15  WK-O-ISLSAVQ-REG-CTY-CODE      PIC X(03).
                   15  WK-O-ISLSAVQ-REG-ADDR-TYPE     PIC X(01).
                   15  WK-O-ISLSAVQ-REG-ADDR-FORMAT   PIC X(01).
                   15  WK-O-ISLSAVQ-REG-FOREIGN-IND   PIC X(01).
                   15  WK-O-ISLSAVQ-REG-VERIFY-IND    PIC X(01).
                   15  WK-O-ISLSAVQ-HOLDMAIL-IND      PIC X(01).
                   15  WK-O-ISLSAVQ-ACCT-ADDR-FORMAT  PIC X(01).
                   15  WK-O-ISLSAVQ-PRI-ID-NUM        PIC X(30).
                   15  WK-O-ISLSAVQ-PRI-ID-TYPE       PIC X(02).
                   15  WK-O-ISLSAVQ-PRI-OWN-CTY-CODE  PIC X(03).
                   15  WK-O-ISLSAVQ-PRI-DOB           PIC 9(08).
                   15  WK-O-ISLSAVQ-PRI-CTZEN-CTY-CDE PIC X(03).
                   15  WK-O-ISLCAVQ-ENG-ACC-NAME-1    PIC X(40).
                   15  WK-O-ISLCAVQ-ENG-ACC-NAME-2    PIC X(40).
                   15  WK-O-ISLCAVQ-ACC-NAME-COUNT    PIC X(01).
      * end of 6263TC *
                   15  WK-O-ISLSAVQ-ACCT-NRA          PIC X(03).
                   15  WK-O-ISLSAVQ-DC-SHLD-TXT-A     PIC X(30).
                   15  WK-O-ISLSAVQ-DC-SHLD-TXT-B     PIC X(30).
                   15  WK-O-ISLSAVQ-DC-SHLD-TXT-C     PIC X(30).
                   15  WK-O-ISLSAVQ-VER-ADD-IND       PIC X(01).
      * Start of VASA fields
      VASA03       15  FILLER                         PIC X(01).
                   15  WK-O-ISLSAVQ-SMACT             PIC 9(18).
      VASA03*VASA02 15  FILLER                        PIC X(01).
                   15  WK-O-ISLSAVQ-SMPURP            PIC X(03).
      VASA02*      15  WK-O-ISLSAVQ-SMRAD1            PIC X(35).
      VASA02*      15  WK-O-ISLSAVQ-SMRAD2            PIC X(35).
      VASA02*      15  WK-O-ISLSAVQ-SMRAD3            PIC X(35).
      VASA02*      15  WK-O-ISLSAVQ-SMRAD4            PIC X(35).
      VASA02*      15  WK-O-ISLSAVQ-SMRBLK            PIC X(07).
      VASA02*      15  WK-O-ISLSAVQ-SMRSTO            PIC X(04).
      VASA02*      15  WK-O-ISLSAVQ-SMRUNT            PIC X(07).
      VASA02*      15  WK-O-ISLSAVQ-SMRPOB            PIC X(06).
      VASA02*      15  WK-O-ISLSAVQ-SMRBLD            PIC X(45).
      VASA02*      15  WK-O-ISLSAVQ-SMRSTR            PIC X(32).
      VASA02*      15  WK-O-ISLSAVQ-SMRSTA            PIC X(20).
      VASA02*      15  WK-O-ISLSAVQ-SMRZIP            PIC S9(09).
      VASA02*      15  WK-O-ISLSAVQ-SMRCOU            PIC X(03).
      VASA02*      15  WK-O-ISLSAVQ-SMRATY            PIC X(01).
      VASA02*      15  WK-O-ISLSAVQ-SMRADF            PIC X(01).
      VASA02*      15  WK-O-ISLSAVQ-SMRFOR            PIC X(01).
      VASA02*      15  WK-O-ISLSAVQ-BADACA            PIC X(01).
      VASA02*      15  WK-O-ISLSAVQ-HLDMCA            PIC X(01).
      VASA02*      15  WK-O-ISLSAVQ-ADFHCA            PIC X(01).
      VASA02*      15  WK-O-ISLSAVQ-SMADR1            PIC X(40).
      VASA02*      15  WK-O-ISLSAVQ-SMADR2            PIC X(40).
      VASA02*      15  WK-O-ISLSAVQ-SMADR3            PIC X(40).
      VASA02*      15  WK-O-ISLSAVQ-SMADR4            PIC X(40).
      VASA02*      15  WK-O-ISLSAVQ-SMADR5            PIC X(40).
      VASA02*      15  WK-O-ISLSAVQ-SMADR6            PIC X(40).
      VASA02*      15  WK-O-ISLSAVQ-SMADR7            PIC X(40).
      VASA02*      15  WK-O-ISLSAVQ-SMAZIP            PIC X(10).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD1            PIC S9(02).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD2            PIC S9(02).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD3            PIC S9(02).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD4            PIC S9(02).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD5            PIC S9(02).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD6            PIC S9(02).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD7            PIC S9(02).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD8            PIC S9(02).
      VASA02*      15  WK-O-ISLSAVQ-SMHCD9            PIC S9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD1            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD2            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD3            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD4            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD5            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD6            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD7            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD8            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCD9            PIC 9(02).
      VASA02       15  WK-O-ISLSAVQ-SMHCN1            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-SMHCN2            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-SMHCN3            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-SMHCN4            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-SMHCN5            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-SMHCN6            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-SMHCN7            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-SMHCN8            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-SMHCN9            PIC X(30).
      VASA02       15  WK-O-ISLSAVQ-ACCTM1            PIC X(35).
      VASA02       15  WK-O-ISLSAVQ-ACCTM2            PIC X(35).
      VASA02       15  WK-O-ISLSAVQ-ACCTLN            PIC X(140).
      VASA02       15  WK-O-ISLSAVQ-CUSTN1            PIC X(35).
      VASA02       15  WK-O-ISLSAVQ-CUSTN2            PIC X(35).
      VASA02       15  WK-O-ISLSAVQ-CUSTLN            PIC X(140).
      VASA02       15  WK-O-ISLSAVQ-MACTN1            PIC X(35).
      VASA02       15  WK-O-ISLSAVQ-MACTN2            PIC X(35).
      VASA02       15  WK-O-ISLSAVQ-MACTLN            PIC X(140).
      VASA02       15  WK-O-ISLSAVQ-ADD1MA            PIC X(40).
      VASA02       15  WK-O-ISLSAVQ-ADD2MA            PIC X(40).
      VASA02       15  WK-O-ISLSAVQ-ADD3MA            PIC X(40).
      VASA02       15  WK-O-ISLSAVQ-ADD4MA            PIC X(40).
      VASA02       15  WK-O-ISLSAVQ-ABNMMA            PIC X(07).
      VASA02       15  WK-O-ISLSAVQ-ASTYMA            PIC X(04).
      VASA02       15  WK-O-ISLSAVQ-AUNMMA            PIC X(07).
      VASA02       15  WK-O-ISLSAVQ-ABOXMA            PIC X(06).
      VASA02       15  WK-O-ISLSAVQ-ABDNMA            PIC X(45).
      VASA02       15  WK-O-ISLSAVQ-ASTNMA            PIC X(32).
      VASA02       15  WK-O-ISLSAVQ-CITYMA            PIC X(20).
      VASA02       15  WK-O-ISLSAVQ-ZIPMA             PIC 9(09).
      VASA02       15  WK-O-ISLSAVQ-COUNMA            PIC X(03).
      VASA02       15  WK-O-ISLSAVQ-ADTPMA            PIC X(01).
      VASA02       15  WK-O-ISLSAVQ-ADFMMA            PIC X(01).
      VASA02       15  WK-O-ISLSAVQ-FORNMA            PIC X(01).
      VASA02       15  WK-O-ISLSAVQ-BADAMA            PIC X(01).
      VASA02       15  WK-O-ISLSAVQ-HLDMMA            PIC X(01).
      VASA02       15  WK-O-ISLSAVQ-ADFHMA            PIC X(01).
      VASA02       15  WK-O-ISLSAVQ-SSPI              PIC X(10).
      VASA02       15  WK-O-ISLSAVQ-LCKI              PIC X(10).
      VASA03       15  WK-O-ISLSAVQ-STFFLG            PIC 9(02).
      VASA02       15  FILSUB                         PIC X(628).
