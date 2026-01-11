      *=====================================================================
      * AMANDMENT HISTORY:
      *=====================================================================
      * MY1CR1 - CRISTINA  14/03/2013 - UOBM REM PROJECT SIT JIRA LOG 6974
      *           - New field L-C-BNM-TAGX(08)
      *             For CSI screen
      *
      * MY1AT1 - ANGELITA  26/02/2013 - UOBM REM PROJECT JIRA LOG 6741
      *           - New field L-C-RBK-IBL-MSG X(35)
      *             For RBK response msg
      *
      * ID1DV1 - DANEILV  30/08/2011 - UOBI REM PROJECT
      *           - New field L-C-USERBNKBR x(04)
      *             for User branch code
      *
      * GH1SDF - SPENCER - 30/09/2002 - AMEND THE FOLLOWING FIELD'S
      *           LENGTH SIZE FOR GLOBAL HUBBING:
      *
      *           - L-C-PYBNKAC FROM X(11) TO X(15)
      *           - L-C-SETBNKAC FROM X(11) TO X(15)
      *           - L-N-G-PU-BNK-ENTITY FROM 9(01) TO 9(02)
      *           - L-C-G-CUST-AC-NO FROM X(11) TO X(15)
      *           - L-C-APP-ACCNO FROM X(12) TO X(15)
      *           - L-C-APP-BNKBR FROM X(03) TO X(04)
      *
      *           FIELD WITH NEW LENGTH SIZE AND
      *           TYPE:
      *           - L-C-G-BNKBR X(04)
      *           - L-N-SYSLEV1 FROM 9(13)V99 TO X(15)
      *           - L-N-G-PU-BNK-ENTITY FROM 9(1) TO X(2)
      *           - L-C-BASECD X(1)
      *           - FILLER X(79)
      *
      *           FOLLOWING FIELDS ARE NOT USED :
      *           ANYMORE:
      *           - L-N-G-DAY-R-W
      *           - L-N-G-DAY-R-M
      *           - L-N-G-BNKBR-1
      *           - L-N-G-BNKBR-23
      *           - L-N-G-OD-P
      *           - L-N-G-PAY-TOLRN
      *           - L-N-G-COR-CHG-AMT-2
      *           - L-N-G-COR-CHG-AMT-3
      *           - L-N-G-COR-CHG-AMT-4
      *           - L-C-G-CUST-S-NM
      *           - L-C-G-CUST-POSTAL
      *
      *=====================================================================
      * GH1LC2 - DESMOND  - 25/09/2002 - GLOBAL HUBBING FOR HK
      *=====================================================================
      * MP2MBA - MBAVILES - 20/11/2001 - MEPS PHASE II MODS - TO RE-USE
      *           L-N-QUENUM AS NEW INDICATOR
MP2MBA*RE-USE L-N-QUENUM AS NEW INDICATOR -
       01  L-C-LOCAL-DATA-AREA.
           05  L-C-USERID               PIC X(08).
           05  L-C-FUNCTID              PIC X(08).
           05  L-C-G-PG-ID              PIC X(08).
           05  L-C-G-ERROR-CD           PIC X(07).
           05  L-C-PFKEY                PIC X(02).
           05  L-C-PROCUNIT             PIC X(03).
           05  L-C-WORK-G.
               10  L-C-SUPPRESS-MSG     PIC X(01).
               10  L-C-TREEIRRS.
                   15  L-N-QUENUM       PIC 9(05).
               15  L-C-NEW-INDICATOR.
                   20  L-C-RCHK-OVRRIDE PIC X(01).
                       *    INDIC VALUES: R = RECHECK
                       *                 O = OVERRIDE
                   20  L-C-MGTQ-IND     PIC X(01).
                   20  L-C-UNUSED1      PIC X(01).
                   20  L-C-UNUSED2      PIC X(01).
                   20  L-C-UNUSED3      PIC X(01).
           15  L-N-QUESUF               PIC 9(02).
           15  L-C-QUENAM               PIC X(03).
           *    10  FILLER              PIC X(49).
               10  L-C-PYBNKID          PIC X(11).
GHF *       10  L-C-PYBNKAC             PIC X(11).
               10  L-C-PYBNKAC          PIC X(15).
               10  L-C-SETBNKID         PIC X(11).
GH1SDF*     10  L-C-SETBNKAC            PIC X(11).
               10  L-C-SETBNKAC         PIC X(15).
               10  L-C-AC-CUY           PIC X(3).
               10  L-C-RP               PIC X(1).
               10  FILLER               PIC X(1).
alice *     10  L-C-DSP-GL-SRC          PIC X(1).
       05  L-C-PROUNT-TAB-G REDEFINES L-C-WORK-G.
           10  L-C-PROUNT-TAB OCCURS 20 TIMES.
               15  L-C-PROUNT           PIC X(03).
GH1SDF      10  FILLER                 PIC X(08).
       05  L-C-APROVLWT                PIC X(17).
       05  L-C-APROVLMT                PIC X(17).
CHB *  05  L-C-OPERATOR-ID             PIC X(08).
CHB      05  L-C-G-RTGS               PIC X(01).
CHB      05  L-C-PRINT-169            PIC X(01).
CHB      05  L-C-irr-next-day         PIC X(01).
NLH      05  L-C-FPERATOR-ID-LEFT     PIC X(05).
         05  L-C-PRODC                PIC X(03).
GH1LC2*  RELEASE L-N-SYSLEV1 FIELD FOR OTHER USAGE.
|  *     05  L-N-SYSLEV1              PIC 9(13)V99.
GH1SDF*  05  L-N-SYSLEV1              PIC 9(13)V99.
|        05  L-N-SYSLEV1              PIC X(15).
chb      05  L-C-CHG-PAYMENT          PIC X(01).
chb      05  L-C-EARMK-IND            PIC X(01).
alice    05  L-C-TAGS6                PIC X(01).
chb      05  L-C-SEND-MT910           PIC X(01).
chb      05  L-C-SEND-MT195           PIC X(01).
alice    05  L-C-G-MSGTYP             PIC X(03).
chb      05  L-C-GRY-M95-no           PIC X(02).
alice *  05  L-C-USERFILE             PIC X(10).
         05  L-C-PTRNNO               PIC X(12).
alice    05  L-C-ENQ-NO               PIC X(12).
alice    05  L-C-G-TRN-PU-MD          PIC X(01).
alice *** S -SINGLE ; A - ALL
alice *  05  L-N-PADMNO               PIC 9(02).
alice *  05  L-C-PFNID                PIC X(08).
alice *  05  L-C-PROCOUNT             PIC X(03).
*---------------------------------------------------------------------*
         05  L-N-PLECAMT              PIC S9(13)V99.
         05  L-N-G-SYSDTE             PIC 9(08).
         05  L-C-G-L-CNTRYCD          PIC X(02).
         05  L-C-G-L-CUYCD            PIC X(03).
         05  L-C-G-US-CUYCD           PIC X(03).
GH1DSF*. 05  L-N-G-PU-BNK-ENTITY      PIC 9(01).
|        05  L-N-G-PU-BNK-ENTITY      PIC X(02).
|  *     05  L-N-G-DAY-R-W            PIC 9(02).
|  *     05  L-N-G-DAY-R-M            PIC 9(02).
|        05  L-C-G-BNKBR              PIC X(04).
|  *     10  L-N-G-BNKBR-1            PIC 9(01).
|  *     10  L-N-G-BNKBR-23           PIC 9(02).
|  *     05  L-N-G-OD-P               PIC 9(02).
         05  L-C-G-FX-RATE-IND        PIC X(01).
NLH *    05  L-C-G-FORM-LOA           PIC X(14).
NLH *    05  L-C-G-PARA-LOA           PIC X(14).
         05  L-N-G-NO-MENU            PIC 9(02).
         05  L-C-G-MENUID-TAB         OCCURS 20 TIMES.
         10  L-C-G-MENUID             PIC X(06).
         05  L-C-G-FUN-DESCP          PIC X(20).
GH1SDF*
       05  L-C-G-TRN-MD                PIC X(01).
       05  L-C-G-ACTION                PIC X(01).
       05  L-C-G-PROC-CD               PIC X(06).
GH1SDF*  05  L-N-G-PAY-TOLRN            PIC S9(13)V99.
       05  L-C-RDS                     PIC X(01).
           88  RDS                     VALUE "Y".
           88  NO-RDS                  VALUE "N".
       05  L-C-REM-DEO-PG-ID           PIC X(08).
       05  L-C-G-VER-IND               PIC X(01).
       05  L-C-G-RCVRYMOD              PIC X(01).
       05  L-C-REMIND                 PIC X(01).
       05  L-N-G-ZEROES.
           10  L-N-G-P-AMT             PIC S9(13)V99.
NLH    *  10  L-N-G-FX-RATE            PIC S9(04)V9(06).
NLH    *  10  L-N-G-FX-RATE            PIC S9(09)V9(07).
           10  L-N-G-PAYBAMT           PIC S9(13)V99.
           10  L-N-G-DAYS              PIC 9(03).
           10  L-N-G-COR-CHG-AMT-1     PIC S9(13)V99.
GH1SDF*  10  L-N-G-COR-CHG-AMT-2     PIC S9(13)V99.
|      *  10  L-N-G-COR-CHG-AMT-3     PIC S9(13)V99.
|      *  10  L-N-G-COR-CHG-AMT-4     PIC S9(13)V99.
           10  L-N-G-VALUEDTE          PIC 9(08).
           10  L-N-G-REPECNT           PIC 9(02).
       05  L-C-G-SPACES.
           10  L-C-TRN-NO              PIC X(12).
           10  L-C-G-CUST-NO           PIC X(11).
GH1SDF*  10  L-C-G-CUST-S-NM           PIC X(11).
           10  L-C-G-CUST-L-NM         PIC X(35).
           10  L-C-G-CUST-ADD1         PIC X(35).
           10  L-C-G-CUST-ADD2         PIC X(35).
           10  L-C-G-CUST-ADD3         PIC X(35).
           10  L-C-G-CUST-ADD4         PIC X(35).
GH1SDF*  10  L-C-G-CUST-POSTAL        PIC X(10).
           10  L-C-G-P-CUY             PIC X(03).
           10  L-C-G-REL-TRNNO         PIC X(12).
           10  L-C-G-TRN-STATUS        PIC X(01).
           10  L-C-G-PAYST             PIC X(01).
           10  L-C-G-CHRGST            PIC X(01).
           10  L-C-G-SKIP-MAS          PIC X(01).
           10  L-C-G-PAY-IND           PIC X(01).
           10  L-C-G-CHG-IND           PIC X(01).
           10  L-C-G-CHG-IND-NONSR     PIC X(01).
           10  L-C-G-EARMK-IND         PIC X(01).
           10  L-C-G-REM-EARMK-IND     PIC X(01).
GH15DF*
      10  L-C-G-EARMK-STATUS  PIC X(01).
      10  L-C-G-DR-IND       PIC X(01).
      10  L-C-G-CR-IND       PIC X(01).
      10  L-C-G-CNTRYCD      PIC X(02).
      10  L-C-G-FX-RATE-TYPE PIC X(02).
      10  L-C-G-DR-PAYMODE   PIC X(06).
      10  L-C-G-DR-REF-NO    PIC X(15).
      10  L-C-G-DR-PAYM-CUY  PIC X(03).
      10  L-C-G-DR-CUSTBNK   PIC X(01).
      10  L-C-G-CR-PAYMODE   PIC X(06).
      10  L-C-G-CR-REF-NO    PIC X(15).
      10  L-C-G-CR-PAYM-CUY  PIC X(03).
      10  L-C-G-CR-CUSTBNK   PIC X(01).
      10  L-C-G-ADV-IND      PIC X(01).
      10  L-C-G-PAYBCUY      PIC X(03).
95831***  950830 REMITTANCE ENHANCEMENT ----
95831***  L-C-G-LOAN-CUY-IND is used for temp. parameter to
95831***  pass the status of the transaction(deo/ver/app) to the
95831***  following routine --> TREEDPGL, TRFMCCIN, TRFXDMSG
95831***  "V" for ver, "A" for app.
95831***  if l-c-g-loan-cuy-ind = "S", this is to indicate the
95831***  transaction is in TREEIRR/TREEIRM function and TRFMCCIN
95831***  should provide F12 function key.
      10  L-C-G-LOAN-CUY-IND PIC X(01).
GH15DF*      10  L-C-G-CUST-AC-NO  PIC X(11).
      10  L-C-G-CUST-AC-NO  PIC X(15).
      10  L-C-G-H-C-IND     PIC X(01).
      10  L-C-G-FX-C-IND    PIC X(01).
      10  L-C-G-CHGBRNBY    PIC X(01).
      10  L-C-G-T-CNTRYCD   PIC X(02).
      10  L-C-G-T-TYPE      PIC X(01).
      10  L-C-G-P-CNTRYCD   PIC X(02).
      10  L-C-G-P-TYPE      PIC X(01).
      10  L-C-G-COR-CHG-CUY-1 PIC X(03).
      10  L-C-G-COR-CHG-CUY-2 PIC X(03).
      10  L-C-G-COR-CHG-CUY-3 PIC X(03).
      10  L-C-G-COR-CHG-CUY-4 PIC X(03).
      10  L-C-G-IRPNO       PIC X(06).
      10  L-C-G-CODDNO      PIC X(15).
      10  L-C-G-SONO        PIC X(12).
      10  L-C-G-DRCRNO      PIC X(03).
      10  L-C-G-HC-PAY-IND  PIC X(01).
      10  L-C-G-RPNO        PIC X(06).
      10  L-C-G-NO-TLX-IND  PIC X(01).
      10  L-C-G-CUST-NONAC  PIC X(35).
      10  L-C-G-T1-CNTRYCD  PIC X(02).
      10  L-C-G-RFUND-TYP   PIC X(01).
 05  L-C-APPROVAL.
GH15DF*      10  L-C-APP-ACCNO   PIC X(12).
      10  L-C-APP-ACCNO   PIC X(15).