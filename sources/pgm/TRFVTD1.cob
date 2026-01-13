       IDENTIFICATION DIVISION.
      **************************
       PROGRAM-ID.    TRFVTD1.
       AUTHOR.        TYK.
       DATE-WRITTEN.  JUN 04.
      *DESCRIPTION :  TABLE D1 VALIDATION.
      *               SUBROUTINE - DEBIT PARTY CHECKING FIELD 53/54 AND
      *               SENDING BANK FOR INCOMING SWIFT FCY/RTGS
      *               THIS ROUTINE WILL INITIATE TRFVTD2 PGM
      *================================================================
      * HISTORY OF MODIFICATION:
      *================================================================
      * P14B00 - ACNRJR - 09/11/2020 - CASH MANAGEMENT ROAD MAP        *
      *                              - P14 - Upgrade GLMS CR5 (Str2)   *
      *                              - PCRMAPDLMC-506                  *
      *                              - Recompiled due to chng in cpybk *
      *----------------------------------------------------------------*
      * GP4A05 - VENADG - 23/10/2020 - CASH MANAGEMENT ROAD MAP - P19
      *                              - GPI Day4 (POST IMPEM IMPROVEMENT)
      *                                STP #1 (HK req) Inward SWIFT & RTGS
      *                                JIRA PCRMAPKGPI-2395
      *                              - BAU Bugfix
      *                              - Rectified hardcoded "MAS"
      *                                when crediting to RTGS
      *                                Utilize TRFVDRTGS utility module
      *                                to retrieve corresponding modepay.
      *----------------------------------------------------------------*
      * GP4A04 - VENDU5 - 07/09/2020 - CASH MANAGEMENT ROAD MAP - P19
      *                              - PCRMAPKGPI-2246
      *                              - Move "N" to Account Type for
      *                                GL accounts.
      *                              - Use GPI4GLENH SW for this Enh.
      * ---------------------------------------------------------------*
      * GP3A03 - VENDU5 - 07/07/2020 - CASH MANAGEMENT ROAD MAP - P19
      *                              - SIT JIRA: PCRMAPKGPI-1757
      *                              - Previously when Tag54B has an
      *                                account and name, the name is being
      *                                moved to bankid.
      *                              - Fix to move sending bank id to
      *                                bankid instead of name
      *                              - Previously when Tag54D 10 byte
      *                                account is being used to call
      *                                NSTP module.
      *                              - Fix is to use the correct 13 byte
      *                                account when calling NSTP program.
      * ---------------------------------------------------------------*
      * GP3A02 - VENDU5 - 02/07/2020 - CASH MANAGEMENT ROAD MAP - P19
      *                              - SIT JIRA: PCRMAPKGPI-1736
      *                              - Previously when Tag53B has an
      *                                account and BIC, the account was
      *                                used directly to check for acc
      *                                mandate which has excess 3 bytes.
      *                              - Fix to remove the first 3 bytes
      *                                before calling mandate table
      * ---------------------------------------------------------------*
      * GP4A00 - VENADG - 16/03/2020 - CASH MANAGEMENT ROAD MAP - P19
      *                              - GPI Day4 (In-Country Req)
      *                                STP #1 (HK req) Inward SWIFT & RTGS
      *                              - Previously system is assigning
      *                                "MAS" DR Modepay for Inward RTGS
      *                                txn and retrieving corresponding GL
      *                                from TFSBNKET (MASNOSTR).
      *                              - Rectified to utilize utility
      *                                TRFVDRTGS to retrieve RTGS modepay
      *                                and read UFMGLPAY retrieve
      *                                corresponding GLNO.
      *                                (Copied from GH2ABA from TREEEDT)
      *----------------------------------------------------------------*
      * GP4C00 - VENJ08 - 12/02/2020 - CASH MANAGEMENT ROAD MAP - P19
      *                              - GPI Day4 (In-Country Req)
      *                                STP #4: Ability to setup DR Limit
      *                                on STP Limit by ACC/CIF/SEG
      *                              - VALIDATE DR VOSTRO STP LIMIT
      *----------------------------------------------------------------*
      * GP3A01 - VENADG - 17/01/2020 - BAU Bugfix
      *                              - PT JIRA: PCRMAPKGPI-1238
      *                              - Previously when Tag53/54
      *                                is a BIC and is defined w/
      *                                account (1st Line), System
      *                                is not able to pass the correct
      *                                ACCNO prior to caling TRFVBANO.
      *                              - Rectified the existing BAU BUG
      *                                to evaluate first if ACCNO retrieved
      *                                from Bank Account Table has Routing
      *                                Code (e.g 103) already or not to
      *                                avoid incorrectly appending
      *----------------------------------------------------------------*
      * G2BM00 - VENADG - 11/08/2019 - CASH MANAGEMENT ROAD MAP - P19
      *                                GPI Day4 (Retro from GPI Day2b HO)
      *                              - Enable STP processing for
      *                                TAG53/54 option B/D when
      *                                TAG32 is not local currency
      *                              - Removed dependency on Day2b Main
      *                                technical switch. Introduced new
      *                                switch for IAFT FCY STP enh instead.
      *----------------------------------------------------------------*
      * G2BM01 - VENADG - 22/11/2019 - GPI Day4 (Retro from GPI Day2b HO)
      *                                POST IMPLEM CR
      *                              - To allow Non-STP check for
      *                                FCY txn. Check will be done
      *                                prior to Account Mandate Chk
      *----------------------------------------------------------------*
      * CMR4J1 - TMPJC5 - 11/06/2018 - CASH MANAGEMENT PROJECT4
      *                              - JIRA # PCRMAPUPAY-611 from HO
      *                              - To modify the sequence of the blacklist
      *                                table program routine validation,
      *                                in order to pass the correct repair
      *                                reason code.
      *----------------------------------------------------------------*
      * REM269 - TMPSRK - 07/04/2017 - JIRA LOG REM-269
      *                              - STANDARDIZATION OF PROGRAM TO
      *                                RETRIEVE CURRENCY AND COUNTRY
      *                                CODE FROM SYSTEM PARAMETER FILE.
      *----------------------------------------------------------------*
      * CMP3A2 - CMPFEN - 21/03/2017 - CASH MANAGEMENT PROJECT REL. 3
      *                                FIX ACCT NUMBER LENGTH           *
      * ---------------------------------------------------------------*
      * CMP3A1 - CMPFEN - 14/02/2017 - CASH MANAGEMENT PROJECT REL. 3
      *                                ALLOW MT101 STP PROCESSING       *
      * ---------------------------------------------------------------*
      * 7Q1EM1 - TMPEYM - 25/11/2016 - REM Q1 2017 RELEASE
      *                              - e-Req 47511 Refinement of
      *                                Duplicate checking for Inw
      *                              - Recompiled due to changes made in
      *                                VSTPL copy book.
      * ---------------------------------------------------------------*
      *6Q3LN1 - UNCCNL - 09/05/2016 - REM 2016 Q3 RELEASE
      *                              - E-REQUEST# 46441
      *                              - INCOMING BAHTNET STP
      *                                To bypass F53 & F54 validation
      *                                for RTGS message.
      *----------------------------------------------------------------*
      * STPGB1 - TMPGCB - 29/09/2015 - UOBM OTT STP PROJECT
      *                                RECOMPILED THE PROGRAM DUE TO
      *                                THE CHANGES ON NSTP COPYBOOK.
      *----------------------------------------------------------------*
      *5Q1JM1 - TMPJ2M - 23/12/2014 - 14HOREM024/14HOREM029/14HOREM028
      *                                Retrofit NON PSTP Reason
      *                                Enhancement Project
      *----------------------------------------------------------------*
      * ID1VK1 - KESAVAN - 24/04/2012 - REM UOBI PROJECT
      *                                 Include Routing number in
      *                                 Account Number.
      *----------------------------------------------------------------*
      * ID1VKE - KESAVAN - 24/04/2012 - REM UOBI PROJECT
      *                                 This program copied from SG
      *                                 based on regional logic done
      *                                 some changes.
      *----------------------------------------------------------------*
      * 202VKE - KESAVAN - 09/06/2009 - If TAG54/53 BANKID is found
      *                                 in TFS202V file then rout to
      *                                 repair.
      *----------------------------------------------------------------*
      * CURRENT PATH :
      * - IF TAG 54A, THEN VALIDATE 54A BIC AGAINST TFS202V TBL
      * - IF TAG 54 NOT EXIST, AND 53A EXIST, VALIDATE 53A BIC AGAINST
      *----------------------------------------------------------------*
      * T55YTW - TECKWAI - 15/08/2008 - For incoming MT103 that has
      *                                 non-empty Tag 55, rout to
      *                                 repair.
      * LOG#: 08HOREM028
      *----------------------------------------------------------------*
      * SM1YTW - TECKWAI - 02/05/2007 - To call new routine TRFVBBASM WAMT
      *                                 for validation of RTGS bank
      *                                 instead of TRFVBBAS
      *----------------------------------------------------------------*
      * SM1TY1 - TMPTY1 - 22/11/2005 - SKIP ACC VERIFY WHEN AMT > IRSWAMT
      *
       ENVIRONMENT DIVISION.
      **********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.     IBM-AS400.
       OBJECT-COMPUTER.     IBM-AS400.
       SPECIAL-NAMES.       LOCAL-DATA IS LOCAL-DATA-AREA
                            I-O-FEEDBACK IS I-O-FEEDBACK-AREA
                            UPSI-0 IS UPSI-SWITCH-0
                              ON STATUS IS U0-ON
                              OFF STATUS IS U0-OFF
                            UPSI-1 IS UPSI-SWITCH-1
                              ON STATUS IS U0-ON
                              OFF STATUS IS U0-OFF
                            UPSI-2 IS UPSI-SWITCH-2
                              ON STATUS IS U0-ON
                              OFF STATUS IS U0-OFF
                            UPSI-3 IS UPSI-SWITCH-3
                              ON STATUS IS U0-ON
                              OFF STATUS IS U0-OFF.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TFSSTPL ASSIGN TO DATABASE-TFSSTPL
                          ORGANIZATION    IS INDEXED
                          ACCESS MODE     IS DYNAMIC
                          RECORD KEY      IS EXTERNALLY-DESCRIBED-KEY
                          FILE STATUS     IS WK-C-FILE-STATUS.

           SELECT TFSCLSYS ASSIGN TO DATABASE-TFSCLSYS
                           ORGANIZATION   IS SEQUENTIAL
                           FILE STATUS    IS WK-C-FILE-STATUS.

           SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
                           ORGANIZATION   IS INDEXED
                           ACCESS MODE    IS RANDOM
                           RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY
                           FILE STATUS    IS WK-C-FILE-STATUS.

202VKE SELECT TFS202V ASSIGN TO DATABASE-TFS202V
202VKE         ORGANIZATION   IS INDEXED
202VKE         ACCESS MODE    IS DYNAMIC
202VKE         RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY
202VKE         FILE STATUS    IS WK-C-FILE-STATUS.

CMP3A1 SELECT UFIMIJ ASSIGN TO DATABASE-UFIMIJ
CMP3A1         ORGANIZATION   IS INDEXED
CMP3A1         ACCESS MODE    IS DYNAMIC
CMP3A1         RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY
CMP3A1         FILE STATUS    IS WK-C-FILE-STATUS.

GP4A00 SELECT UFMGLPAY ASSIGN TO DATABASE-UFMGLPAY
GP4A00         ORGANIZATION   IS INDEXED
GP4A00         ACCESS MODE    IS RANDOM
GP4A00         RECORD KEY     IS EXTERNALLY-DESCRIBED-KEY
GP4A00         FILE STATUS    IS WK-C-FILE-STATUS.

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

       FD  TFSBNKET
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS TFSBNKET-REC.
       01  TFSBNKET-REC.
           COPY DDS-ALL-FORMATS OF TFSBNKET.
       01  TFSBNKET-REC-1.
           COPY TFSBNKET.

202VKE FD TFS202V
  |         LABEL RECORDS ARE OMITTED
  |         DATA RECORD IS WK-C-TFS202V.
  |    01  WK-C-TFS202V.
  |        COPY DDS-ALL-FORMATS OF TFS202V.
  |    01  WK-C-TFS202V-1.
202VKE   COPY TFS202V.

CMP3A1 FD  UFIMIJ
CMP3A1     LABEL RECORDS ARE OMITTED
CMP3A1     DATA RECORD IS UFIMIJ-REC.
CMP3A1 01  UFIMIJ-REC.
CMP3A1     COPY DDS-ALL-FORMATS OF UFIMIJ.
CMP3A1 01  UFIMIJ-REC-1.
CMP3A1     COPY UFIMIJ.

GP4A00 FD  UFMGLPAY
GP4A00     LABEL RECORDS ARE OMITTED
GP4A00     DATA RECORD IS UFMGLPAY-REC.
GP4A00 01  UFMGLPAY-REC.
GP4A00     COPY DDS-ALL-FORMATS OF UFMGLPAY.
GP4A00 01  UFMGLPAY-REC-1.
GP4A00     COPY UFMGLPAY.

       WORKING-STORAGE SECTION.
      *************************
       01  WK-C-COMMON.
           COPY ASCMWS.

       01  TAG53-FORMAT.
           05  TAG53-LINE-1.
               07  TAG53-FIL1              PIC X(2).
               07  TAG53-OPT               PIC X(1).
               07  TAG53-FIL2              PIC X(1).
               07  TAG53-PTID.
                   09  TAG53-PTID-1        PIC X(02).
                   09  TAG53-PTID-2        PIC X(35).
           05  TAG53-LINE-2                PIC X(35).
           05  TAG53-BIC REDEFINES TAG53-LINE-2.
               07  TAG53-BIC-SUB1          PIC X(4).
               07  TAG53-BIC-SUB2          PIC X(2).
               07  TAG53-BIC-SUB3          PIC X(2).
               07  TAG53-BIC-SUB4          PIC X(3).
               07  TAG53-BIC-FILLER        PIC X(24).
           05  TAG53-LOC REDEFINES TAG53-LINE-2
                                           PIC X(35).
           05  TAG53-NAME REDEFINES TAG53-LINE-2
                                           PIC X(35).
           05  TAG53-LINE-3                PIC X(35).
           05  TAG53-LINE-4                PIC X(35).
           05  TAG53-LINE-5                PIC X(35).

       01  TAG54-FORMAT.
           05  TAG54-LINE-1.
               07  TAG54-FIL1              PIC X(2).
               07  TAG54-OPT               PIC X(1).
               07  TAG54-FIL2              PIC X(1).
               07  TAG54-PTID.
                   09  TAG54-PTID-1        PIC X(02).
                   09  TAG54-PTID-2        PIC X(35).
           05  TAG54-LINE-2                PIC X(35).
           05  TAG54-BIC REDEFINES TAG54-LINE-2.
               07  TAG54-BIC-SUB1          PIC X(4).
               07  TAG54-BIC-SUB2          PIC X(2).
               07  TAG54-BIC-SUB3          PIC X(2).
               07  TAG54-BIC-SUB4          PIC X(3).
               07  TAG54-BIC-FILLER        PIC X(24).
           05  TAG54-LOC REDEFINES TAG54-LINE-2
                                           PIC X(35).
           05  TAG54-NAME REDEFINES TAG54-LINE-2
                                           PIC X(35).
           05  TAG54-LINE-3                PIC X(35).
           05  TAG54-LINE-4                PIC X(35).
           05  TAG54-LINE-5                PIC X(35).

T55YTW 01  TAG55-OPT                 PIC X(1).

       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 TIMES     PIC X VALUE "X".

       01  TABLE-ARR2.
           05  TAB-VL2 OCCURS 20 TIMES     PIC X VALUE "X".

       01  PATH-P1                         PIC X(20)
                            VALUE "YXXYXXXXXXXXXXXXXXXX".

       01  PATH-P2                         PIC X(20)
                            VALUE "XYXYXXXXXXXXXXXXXXXX".

       01  PATH-P3                         PIC X(20)
                            VALUE "YXXYXXXXXXXXXXXXXXXX".

       01  PATH-P4                         PIC X(20)
                            VALUE "XYXYXXXXXXXXXXXXXXXX".

       01  PATH-P5                         PIC X(20)
                            VALUE "XXYYXXXXXXXXXXXXXXXX".

       01  PATH-P6                         PIC X(20)
                            VALUE "XXYNYNXXXXXXXXXXXXXX".

       01  PATH-P7                         PIC X(20)
                            VALUE "XXXXXXYXXXXXXXXXXXXX".

       01  WK-C-WORK-AREA.
           05  FIRST-TIME                  PIC X(01) VALUE "Y".
           05  SHIFT-IND                   PIC X(01) VALUE SPACE.
           05  WS-FLAG1                    PIC X(01) VALUE SPACE.
           05  WS-FLAG2                    PIC X(01) VALUE SPACE.
           05  WS-ACT1                     PIC X(01) VALUE SPACE.
           05  WS-ACT2                     PIC X(01) VALUE SPACE.
           05  WS-OKAY                     PIC X(01) VALUE SPACE.
GP3A01*    05  WS-ACCNO              PIC X(11) VALUE SPACES.
GP3A01     05  WS-ACCNO              PIC X(15) VALUE SPACES.
CMP3A1*CMP3A2 05 WS-T50-ACCNO        PIC X(11) VALUE SPACE.
CMP3A2     05  WS-T50-ACCNO          PIC X(15) VALUE SPACE.
CMP3A2     05  WK-N-ACCTLEN          PIC S9(02) VALUE ZEROES.
           05  WS-BANKID                   PIC X(11) VALUE SPACES.
           05  WS-RECBNKID                 PIC X(11) VALUE SPACE.
           05  WS-SNDCBNKID                PIC X(11) VALUE SPACE.
202VKE     05  WS-SENCBNKID          PIC X(11) VALUE SPACE.
           05  WS-PMODE                    PIC X(08) VALUE SPACES.
           05  WS-ACUDBUI                  PIC X(01) VALUE "D".
           05  WS-ACCTYP                   PIC X(01) VALUE SPACE.

GP4C00 01  WK-C-GPI4STPLSW           PIC X(01) VALUE SPACE.
GP4C00 01  WK-C-LINK-LIMIT.
GP4C00     05  WK-C-LINK-AREA-INPUT.
GP4C00         10  WS-LINK-BNKENTTY  PIC X(02).
GP4C00         10  WS-LINK-ACCNO     PIC X(15) VALUE 0.
GP4C00         10  WS-LINK-CCY       PIC X(03) VALUE SPACES.
GP4C00         10  WS-LINK-AMT       PIC S9(13)V99 VALUE 0.
GP4C00         10  WS-LINK-REMIND    PIC X(01).
GP4C00     05  WK-C-LINK-AREA-OUTPUT.
GP4C00         10  WS-LINK-STATUS    PIC X(02) VALUE SPACES.

ID1VK1 01  WK-N-ACCLEN               PIC 9(02) VALUE ZEROES.

ID1VKE 01  WK-C-NEW-ACCNO.
ID1VKE     15  WK-C-NEW-ACCNO1       PIC X(03).
ID1VKE     15  WK-C-NEW-ACCNO2       PIC X(11).

5Q1JM1 01  WK-C-RPRRSN-AREA.
5Q1JM1     05  WK-C-SEGCDE           PIC X(01) VALUE SPACE.
5Q1JM1     05  WK-N-STAFFIND         PIC S9(02) VALUE ZEROS.
5Q1JM1     05  WK-C-QRATE            PIC X(02) VALUE SPACE.
5Q1JM1     05  WK-C-RPRCODE          PIC X(07) VALUE SPACE.
5Q1JM1     05  WK-C-TRNNO            PIC X(12) VALUE SPACE.
5Q1JM1     05  WK-C-ACCNAME          PIC X(35) VALUE SPACE.
5Q1JM1     05  WK-C-FUNCTID          PIC X(08) VALUE SPACE.
5Q1JM1 01  WK-N-SYSDTE               PIC S9(08) VALUE ZEROS.
5Q1JM1 01  WK-C-RPRPGM               PIC X(10) VALUE "TRFVTD1".
6Q3LN1 01  L-STPRTGS                 PIC X(1) VALUE SPACES.
6Q3LN1 01  WK-RTGS-BNKAC-EXIST       PIC X(01) VALUE SPACE.
CMP3A1 01  WS-C-M101STPIND           PIC X(01) VALUE "N".
REM269 01  WK-C-LCUYCD               PIC X(03).

G2BM00 01  WK-GPI2B-VAR.
G2BM00     05  WS-C-GPI-SW           PIC X(01) VALUE SPACE.
G2BM00     05  WS-N-ACCLEN           PIC 9(02) VALUE ZEROS.
G2BM00     05  WK-C-VALID-MTMSG      PIC X(01) VALUE SPACE.
G2BM00     05  WK-C-ACMN-CHECK       PIC X(01) VALUE SPACE.
G2BM01 01  WS-C-FCY-NSTP-SW          PIC X(01) VALUE SPACE.
GP4A00 01  WS-C-SWF-RTGS-SW          PIC X(01) VALUE SPACE.
GP4A05 01  WK-C-MAS-RTGS-SW          PIC X(01) VALUE "N".
GP4A00 01  WK-N-MAS                  PIC S9(8) VALUE ZERO.
GP4A04 01  WK-C-GPI4-GL-SW           PIC X(01) VALUE SPACE.

           COPY VCCA.
           COPY VCFA.
           COPY VSTPL.
           COPY VBAC.
           COPY VBBAS.
           COPY VBANO.
           COPY VTD2.
           COPY NSTP.
           COPY ACMN.
           COPY BLKB.
           COPY LOGG.
T55YTW COPY GTAG.
ID1VKE COPY XGSPA.
5Q1JM1 COPY RRSN.
6Q3LN1 COPY IRTGSSWTC.
6Q3LN1 COPY XPARA.
CMP3A1 COPY SWIFTMER.
CMP3A1 COPY CUPF.
CMP3A1 COPY GSDTS.
GP4A00 COPY VDRTGS.

       LINKAGE SECTION.
      *****************
           COPY VTD1.

       PROCEDURE DIVISION USING WK-VTD1.
      **********************************
       MAIN-MODULE.

ID1VKE     INITIALIZE                WK-C-XGSPA-RECORD.
  |          MOVE "RSYACCROUT"         TO WK-C-XGSPA-GHPARCD.
  |          CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
  |          IF WK-C-XGSPA-ERROR-CD  = SPACES
  |              MOVE WK-C-XGSPA-GHPARVAL TO WK-C-NEW-ACCNO1
  |          ELSE
  |              MOVE ZEROS            TO WK-C-NEW-ACCNO1
  |          END-IF.
ID1VKE
ID1VK1*GET THE LENGTH OF THE ROUTING ACCOUNT CODE
  |          INITIALIZE WK-C-XGSPA-RECORD.
  |          MOVE "RSYACCLEN"          TO WK-C-XGSPA-GHPARCD.
  |          CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
  |          MOVE WK-C-XGSPA-GHPARVAL(1:2) TO WK-N-ACCLEN.
ID1VK1

6Q3LN1* GET STPFLAG FROM PARAMETER TABLE FOR RTGS
6Q3LN1     INITIALIZE L-STPRTGS.
6Q3LN1     INITIALIZE WK-C-XGSPA-RECORD.
6Q3LN1     MOVE "RTGSTPFL"           TO WK-C-XGSPA-GHPARCD.
6Q3LN1     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
6Q3LN1     IF WK-C-XGSPA-ERROR-CD = SPACES
6Q3LN1         MOVE WK-C-XGSPA-GHPARVAL TO L-STPRTGS
6Q3LN1     END-IF.

GP4C00     INITIALIZE WK-C-XGSPA-RECORD.
GP4C00     MOVE "GPI4STPLSW"         TO WK-C-XGSPA-GHPARCD.
GP4C00     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
GP4C00     IF WK-C-XGSPA-ERROR-CD    = SPACES
GP4C00         MOVE WK-C-XGSPA-GHPARVAL TO WK-C-GPI4STPLSW
GP4C00     ELSE
GP4C00         MOVE "N"              TO WK-C-GPI4STPLSW
GP4C00     END-IF.

6Q3LN1     INITIALIZE WK-RTGS-SWITCHES.
6Q3LN1     IF L-STPRTGS = "Y"
6Q3LN1         MOVE "IRTGSWTC"       TO WK-C-XPARA-PARACD
6Q3LN1         CALL "TRFXPARA"       USING WK-C-XPARA-RECORD
6Q3LN1         IF WK-C-XPARA-ERROR-CD = SPACES
6Q3LN1             MOVE WK-C-XPARA-PARAVALU(1:20) TO WK-RTGS-SWITCHES
6Q3LN1         END-IF
6Q3LN1     END-IF.

CMP3A1* GET MT101 STP INDICATOR
CMP3A1     INITIALIZE WK-C-XGSPA-RECORD.
CMP3A1     MOVE "M101STPIND"         TO WK-C-XGSPA-GHPARCD.
CMP3A1     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
CMP3A1     IF WK-C-XGSPA-ERROR-CD    = SPACES
CMP3A1         MOVE WK-C-XGSPA-GHPARVAL TO WS-C-M101STPIND
CMP3A1     ELSE
CMP3A1         MOVE SPACES           TO WS-C-M101STPIND
CMP3A1     END-IF.

      *----------------------------------------------------------------*
      *    GET SYSTEM PARAMETERS FOR LOCAL CURRENCY CODE.              *
      *----------------------------------------------------------------*
           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "RSYCTLLCUY"               TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"                 USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD          = SPACES
               MOVE WK-C-XGSPA-GHPARVAL    TO WK-C-LCUYCD
           ELSE
               MOVE SPACES                 TO WK-C-LCUYCD
REM269 END-IF.

G2BM00*Retrieve GPI Day2b to Allow for Debit FCY Acc Mandate Table check.
G2BM00     INITIALIZE WK-C-XGSPA-RECORD
G2BM00                WS-C-GPI-SW.
G2BM00*    MOVE "GPISWITCH2"         TO WK-C-XGSPA-GHPARCD.
G2BM00     MOVE "GPIFCYACMN"         TO WK-C-XGSPA-GHPARCD.
G2BM00     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
G2BM00     IF WK-C-XGSPA-ERROR-CD    = SPACES
G2BM00         MOVE WK-C-XGSPA-GHPARVAL(1:1)
G2BM00                                TO WS-C-GPI-SW
G2BM00     ELSE
G2BM00         MOVE "N"              TO WS-C-GPI-SW
G2BM00     END-IF.

G2BM00*Retrieve account length.
G2BM00     INITIALIZE WK-C-XGSPA-RECORD
G2BM00                WS-N-ACCLEN.
G2BM00     MOVE "RSYACCLEN"          TO WK-C-XGSPA-GHPARCD.
G2BM00     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
G2BM00     IF WK-C-XGSPA-ERROR-CD    = SPACES
G2BM00         MOVE WK-C-XGSPA-GHPARVAL(1:2)
G2BM00                                TO WS-N-ACCLEN
G2BM00     ELSE
G2BM00         MOVE 10               TO WS-N-ACCLEN
G2BM00     END-IF.

G2BM01*Retrieve IAFT FCY NSTP Check switch.
G2BM01     INITIALIZE WK-C-XGSPA-RECORD
G2BM01                WS-C-FCY-NSTP-SW.
G2BM01     MOVE "GPIFCYNSTP"         TO WK-C-XGSPA-GHPARCD.
G2BM01     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
G2BM01     IF WK-C-XGSPA-ERROR-CD    = SPACES
G2BM01         MOVE WK-C-XGSPA-GHPARVAL(1:1)
G2BM01                                TO WS-C-FCY-NSTP-SW
G2BM01     ELSE
G2BM01         MOVE "N"              TO WS-C-FCY-NSTP-SW
G2BM01     END-IF.

GP4A00*Retrieve SWIFT RTGS STP Switch.
GP4A00     INITIALIZE WK-C-XGSPA-RECORD
GP4A00                WS-C-SWF-RTGS-SW.
GP4A00     MOVE "SWFRTGSIND"         TO WK-C-XGSPA-GHPARCD.
GP4A00     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
GP4A00     IF WK-C-XGSPA-ERROR-CD    = SPACES
GP4A00         MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP4A00                                TO WS-C-SWF-RTGS-SW
GP4A00     ELSE
GP4A00         MOVE "N"              TO WS-C-SWF-RTGS-SW
GP4A00     END-IF.

GP4A05*-->Retrieve GPI Day 4 Replace MAS w/ RTGS Payment Mode
GP4A05     INITIALIZE                WK-C-XGSPA-RECORD
GP4A05                               WK-C-MAS-RTGS-SW.
GP4A05
GP4A05     MOVE "GPI4MASRTG"         TO WK-C-XGSPA-GHPARCD.
GP4A05     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
GP4A05
GP4A05     IF WK-C-XGSPA-ERROR-CD = SPACES
GP4A05         MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP4A05                                TO WK-C-MAS-RTGS-SW
GP4A05     END-IF.

GP4A04*Retrieve GPI Day 4 HK Enhancement Switch
GP4A04     INITIALIZE                WK-C-XGSPA-RECORD
GP4A04                               WK-C-GPI4-GL-SW.
GP4A04     MOVE "GPI4GLENH"          TO WK-C-XGSPA-GHPARCD.
GP4A04     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
GP4A04     IF WK-C-XGSPA-ERROR-CD    = SPACES
GP4A04         MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP4A04                                TO WK-C-GPI4-GL-SW
GP4A04     ELSE
GP4A04         MOVE "N"              TO WK-C-GPI4-GL-SW
GP4A04     END-IF.

           INITIALIZE WK-VTD1-OUTPUT
                      WK-LOGG
                      WK-C-WORK-AREA.
           MOVE ALL "X"                    TO TABLE-ARRAY.
           MOVE ALL "X"                    TO TABLE-ARR2.
           MOVE "Y"                        TO FIRST-TIME.
6Q3LN1     MOVE "Y"                  TO WK-RTGS-BNKAC-EXIST.

5Q1JM1     MOVE ZEROS                    TO WK-C-RRSN-QUENUM
5Q1JM1                                      WK-C-RRSN-QUESUF
5Q1JM1                                      WK-C-RRSN-STAFFIND
5Q1JM1                                      WK-C-RRSN-SEQNUM
5Q1JM1                                      WK-C-RRSN-RPRDTE.

G2BM00      MOVE "N"                      TO WK-C-VALID-MTMSG
G2BM00                                       WK-C-ACMN-CHECK.

           IF FIRST-TIME = "Y"
               OPEN     INPUT TFSSTPL
               IF NOT WK-C-SUCCESSFUL
               AND WK-C-FILE-STATUS NOT = "41"
                   DISPLAY "TRFVTD1 - OPEN FILE ERROR - TFSSTPL"
                   DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
               END-IF
               OPEN     INPUT TFSCLSYS
               IF NOT WK-C-SUCCESSFUL
               AND WK-C-FILE-STATUS NOT = "41"
                   DISPLAY "TRFVTD1 - OPEN FILE ERROR - TFSCLSYS"
                   DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
               END-IF
               OPEN     INPUT TFSBNKET
               IF NOT WK-C-SUCCESSFUL
               AND WK-C-FILE-STATUS NOT = "41"
                   DISPLAY "TRFVTD1 - OPEN FILE ERROR - TFSBNKET"
                   DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
               END-IF
202VKE     OPEN     INPUT TFS202V
              IF NOT WK-C-SUCCESSFUL
              AND WK-C-FILE-STATUS NOT = "41"
                  DISPLAY "TRFVTD1 - OPEN FILE ERROR - TFS202V"
                  DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
202VKE     END-IF
CMP3A1     OPEN     INPUT UFIMIJ
CMP3A1     IF NOT WK-C-SUCCESSFUL
CMP3A1     AND WK-C-FILE-STATUS NOT = "41"
CMP3A1         DISPLAY "TRFVTD1 - OPEN FILE ERROR - UFIMIJ "
CMP3A1         DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
CMP3A1     END-IF
GP4A00     OPEN     INPUT UFMGLPAY
GP4A00     IF NOT WK-C-SUCCESSFUL
GP4A00     AND WK-C-FILE-STATUS NOT = "41"
GP4A00         DISPLAY "TRFVTD1 - OPEN FILE ERROR - UFMGLPAY"
GP4A00         DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
GP4A00     END-IF
           END-IF.

           MOVE WK-VTD1-PARALNO            TO TFSSTPL-PARALNO.
           MOVE WK-VTD1-SEQNUM             TO TFSSTPL-SEQNUM.

           READ TFSSTPL
                KEY IS EXTERNALLY-DESCRIBED-KEY.

           IF WK-C-SUCCESSFUL
               MOVE TFSSTPL-TAG53          TO TAG53-FORMAT
               MOVE TFSSTPL-TAG54          TO TAG54-FORMAT
G2BM00     IF WS-C-GPI-SW = "Y"
G2BM00         IF TFSSTPL-SWFTMGTY = "103" OR "202"
G2BM00             MOVE "Y"          TO WK-C-VALID-MTMSG
G2BM00         END-IF
G2BM00     END-IF
           PERFORM A100-MOVE-TAG-VALUES
             THRU A199-MOVE-TAG-VALUES-EX
           PERFORM A200-INITIAL-SUBROUTINE
             THRU A299-INITIAL-SUBROUTINE-EX
           PERFORM B100-PATH-CHOICE THRU B199-PATH-CHOICE-EX
           END-IF.

           GO TO Z000-END-PROGRAM.

       A100-MOVE-TAG-VALUES.
           MOVE TFSSTPL-SENBNKID           TO WS-BANKID.
202VKE   IF TAG53-OPT                  NOT = SPACES
  |      AND TAG53-BIC                 NOT = SPACES
  |          MOVE TAG53-BIC            TO WS-SENCBNKID
202VKE   END-IF.
         IF TFSSTPL-IMSGTYPE NOT = "M"
               IF TAG54-OPT                NOT = SPACES
                   IF TAG54-BIC            NOT = SPACES
                       MOVE TAG54-BIC      TO WS-RECBNKID
                                              WS-BANKID
                   END-IF
                   MOVE TAG54-PTID         TO WS-ACCNO
               ELSE
                   IF TAG53-OPT            NOT = SPACES
                       IF TAG53-BIC        NOT = SPACES
                           MOVE TAG53-BIC  TO WS-SNDCBNKID
                                              WS-BANKID
                       END-IF
                       MOVE TAG53-PTID     TO WS-ACCNO
                   END-IF
               END-IF
           END-IF.

       A199-MOVE-TAG-VALUES-EX.
           EXIT.
       EJECT

       A200-INITIAL-SUBROUTINE.
      *----------------------------------------------------------------*
      *    GET DATA FROM "TFSCLSYS" TABLE                              *
      *----------------------------------------------------------------*
           READ TFSCLSYS.
           IF   NOT WK-C-SUCCESSFUL
                DISPLAY "TRFVTD1     - READ TFSCLSYS ERROR"
                DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
                GO TO   Z000-END-PROGRAM.

5Q1JM1 MOVE TFSCLSYS-SYSDTE          TO WK-N-SYSDTE.

       A299-INITIAL-SUBROUTINE-EX.
           EXIT.

       B100-PATH-CHOICE.
           MOVE "Y"                        TO WS-FLAG1.
6Q3LN1    IF TFSSTPL-IMSGTYPE = "M"
6Q3LN1       IF SW-RTGS-BYPASS-F53-F54-Y
6Q3LN1           PERFORM D500-RTGS-VALIDATION
6Q3LN1              THRU D599-RTGS-VALIDATION-EX
6Q3LN1       ELSE
                   IF TAG53-OPT            = SPACES
                   AND TAG53-PTID          = SPACES
                   AND TAG53-BIC           = SPACES
                   AND TAG54-OPT           = SPACES
                   AND TAG54-PTID          = SPACES
                   AND TAG54-BIC           = SPACES
                       PERFORM C100-VALIDATION-PART
                           THRU C199-VALIDATION-PART-EX
                   END-IF
6Q3LN1        END-IF
           ELSE
T55YTW
T55YTW**** call routine to obtain incoming msg's tag 55 if any.
T55YTW**** For an incoming msg with tag 55, currently possible for
T55YTW**** swift MT103, it should be routed to repair.
T55YTW
T55YTW     PERFORM E001-GET-TAG55 THRU E001-GET-TAG55-EX
T55YTW     EVALUATE TRUE
T55YTW         WHEN TAG55-OPT NOT = SPACES
T55YTW             PERFORM C500-VALIDATION-PART
T55YTW                 THRU C599-VALIDATION-PART-EX
T55YTW         WHEN NOT(TAG54-OPT = SPACES
T55YTW              AND TAG54-PTID = SPACES
T55YTW              AND TAG54-BIC  = SPACES)
T55YTW             PERFORM C200-VALIDATION-PART
T55YTW                 THRU C299-VALIDATION-PART-EX
T55YTW         WHEN OTHER
T55YTW             PERFORM C300-VALIDATION-PART
T55YTW                 THRU C399-VALIDATION-PART-EX
T55YTW     END-EVALUATE
           END-IF.

           PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.

CMP3A1 IF TFSSTPL-SWFTMGTY = 101
CMP3A1 AND WS-C-M101STPIND = "Y"
CMP3A1     PERFORM C600-VALIDATION-PART
CMP3A1         THRU C699-VALIDATION-PART-EX
CMP3A1 END-IF.

           PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.
           EXIT.

       C100-VALIDATION-PART.
           PERFORM D400-SHIFT-VALIDATION.
           IF SHIFT-IND = "Y"
               MOVE PATH-P7                TO TABLE-ARRAY
           END-IF.
       C199-VALIDATION-PART-EX.
           EXIT.
       EJECT

       C200-VALIDATION-PART.
           IF TAG54-OPT                    = "A"
           AND TAG54-PTID                  NOT = SPACES
           AND TAG54-BIC                   NOT = SPACES
               MOVE PATH-P1                TO TABLE-ARRAY
G2BM00     IF WS-C-GPI-SW            = "Y"
G2BM00     AND WK-C-VALID-MTMSG = "Y"
G2BM00     AND TAG54-BIC             NOT = TFSSTPL-SENBNKID
G2BM00         MOVE "Y"              TO WK-C-ACMN-CHECK
G2BM00     END-IF
           END-IF.
           IF TAG54-OPT                    = "A"
           AND TAG54-PTID                  = SPACES
           AND TAG54-BIC                   NOT = SPACES
               MOVE PATH-P2                TO TABLE-ARRAY
G2BM00     IF WS-C-GPI-SW            = "Y"
G2BM00     AND WK-C-VALID-MTMSG = "Y"
G2BM00     AND TAG54-BIC             NOT = TFSSTPL-SENBNKID
G2BM00         MOVE "Y"              TO WK-C-ACMN-CHECK
G2BM00     END-IF
           END-IF.
G2BM00 IF (( TAG54-OPT               = "B"
G2BM00     OR TAG54-OPT              = "D")
G2BM00 AND TAG54-PTID                = SPACES
G2BM00 AND TAG54-BIC                 NOT = SPACES
G2BM00 AND WS-C-GPI-SW               = "Y"
G2BM00 AND WK-C-VALID-MTMSG          = "Y")
G2BM00     MOVE PATH-P2              TO TABLE-ARRAY
G2BM00     IF (TAG54-BIC NOT = SPACES
G2BM00     AND TAG54-BIC NOT = TFSSTPL-SENBNKID)
G2BM00         MOVE "Y"              TO WK-C-ACMN-CHECK
G2BM00     END-IF
G2BM00 END-IF.
G2BM00 IF WS-C-GPI-SW                = "Y"
G2BM00 AND WK-C-VALID-MTMSG          = "Y"
G2BM00*G2BM00 IF TAG54-OPT           = "B" OR "D"
G2BM00     IF (TAG54-OPT             = "B"
G2BM00     OR  TAG54-OPT             = "D")
G2BM00     AND TAG54-PTID            NOT = SPACES
G2BM00*        IF TAG54-PTID(1:WS-N-ACCLEN) IS NUMERIC
G2BM00         IF WS-ACCNO NOT = SPACES
G2BM00             MOVE PATH-P1      TO TABLE-ARRAY
G2BM00             IF TAG54-OPT      = "B"
G2BM00             AND (TAG54-BIC NOT = SPACES
G2BM00             AND TAG54-BIC NOT = TFSSTPL-SENBNKID)
G2BM00                 MOVE "Y"      TO WK-C-ACMN-CHECK
G2BM00             END-IF
G2BM00             IF TAG54-OPT      = "D"
G2BM00                 MOVE TFSSTPL-SENBNKID
G2BM00                                TO WS-BANKID
G2BM00             END-IF
G2BM00         ELSE
G2BM00             INITIALIZE WK-C-RPRRSN-AREA
G2BM00             MOVE "RSN0095"    TO WK-C-RPRCODE
G2BM00             PERFORM E002-PROCESS-RPRRSN
G2BM00                 THRU E002-PROCESS-RPRRSN-EX
G2BM00         END-IF
G2BM00     END-IF
G2BM00 END-IF.
       C299-VALIDATION-PART-EX.
           EXIT.
       EJECT

       C300-VALIDATION-PART.
           IF TAG53-OPT                    = "A"
           AND TAG53-PTID                  NOT = SPACES
           AND TAG53-BIC                   NOT = SPACES
               MOVE PATH-P3                TO TABLE-ARRAY
G2BM00     IF WS-C-GPI-SW            = "Y"
G2BM00     AND WK-C-VALID-MTMSG = "Y"
G2BM00     AND TAG53-BIC             NOT = TFSSTPL-SENBNKID
G2BM00         MOVE "Y"              TO WK-C-ACMN-CHECK
G2BM00     END-IF
          END-IF.
           IF TAG53-OPT                    = "A"
           AND TAG53-PTID                  = SPACES
           AND TAG53-BIC                   NOT = SPACES
               MOVE PATH-P4                TO TABLE-ARRAY
      G2BM00     IF WS-C-GPI-SW            = "Y"
      G2BM00     AND WK-C-VALID-MTMSG = "Y"
      G2BM00     AND TAG53-BIC             NOT = TFSSTPL-SENBNKID
      G2BM00         MOVE "Y"              TO WK-C-ACMN-CHECK
      G2BM00     END-IF
           END-IF.
G2BM00 IF (( TAG53-OPT               = "B"
G2BM00     OR TAG53-OPT              = "D")
G2BM00 AND TAG53-PTID                = SPACES
G2BM00 AND TAG53-BIC                 NOT = SPACES
G2BM00 AND WS-C-GPI-SW               = "Y"
G2BM00 AND WK-C-VALID-MTMSG          = "Y")
G2BM00     MOVE PATH-P4              TO TABLE-ARRAY
G2BM00     IF (TAG53-BIC NOT = SPACES
G2BM00     AND TAG53-BIC NOT = TFSSTPL-SENBNKID)
G2BM00         MOVE "Y"              TO WK-C-ACMN-CHECK
G2BM00     END-IF
G2BM00 END-IF.
G2BM00 IF WS-C-GPI-SW                = "Y"
G2BM00 AND WK-C-VALID-MTMSG          = "Y"
G2BM00     IF (TAG53-OPT             = "B"
G2BM00     OR  TAG53-OPT             = "D")
G2BM00     AND TAG53-PTID            NOT = SPACES
G2BM00*        IF TAG53-PTID(1:WS-N-ACCLEN) IS NUMERIC
G2BM00         IF WS-ACCNO NOT = SPACES
G2BM00             MOVE PATH-P3      TO TABLE-ARRAY
G2BM00             IF TAG53-OPT      = "B"
G2BM00             AND (TAG53-BIC NOT = SPACES
G2BM00             AND TAG53-BIC NOT = TFSSTPL-SENBNKID)
G2BM00                 MOVE "Y"      TO WK-C-ACMN-CHECK
G2BM00             END-IF
G2BM00             IF TAG53-OPT      = "D"
G2BM00                 MOVE TFSSTPL-SENBNKID
G2BM00                                TO WS-BANKID
G2BM00             END-IF
G2BM00         ELSE
G2BM00             INITIALIZE WK-C-RPRRSN-AREA
G2BM00             MOVE "RSN0095"    TO WK-C-RPRCODE
G2BM00             PERFORM E002-PROCESS-RPRRSN
G2BM00                 THRU E002-PROCESS-RPRRSN-EX
G2BM00         END-IF
G2BM00     END-IF
G2BM00 END-IF.
           IF TAG53-OPT                    = SPACES
           AND TAG53-PTID                  = SPACES
           AND TAG53-BIC                   = SPACES
           AND TAG54-OPT                   = SPACES
           AND TAG54-PTID                  = SPACES
           AND TAG54-BIC                   = SPACES
               MOVE PATH-P5                TO TABLE-ARRAY
           END-IF.
       C399-VALIDATION-PART-EX.
           EXIT.
       EJECT

       C400-TABLE-D2.
           MOVE WK-VTD1-PARALNO            TO WK-VTD2-PARALNO.
           MOVE WK-VTD1-SEQNUM             TO WK-VTD2-SEQNUM.
           CALL "TRFVTD2"                  USING WK-VTD2.
           MOVE WK-VTD2-NO-ERROR           TO WS-OKAY.
       C499-TABLE-D2-EX.
           EXIT.
       EJECT

T55YTW C500-VALIDATION-PART.
T55YTW**** This routine is performed for an incoming msg that has a
T55YTW**** tag 55. It is to be routed to repair straightaway.
T55YTW
T55YTW     MOVE "N"                  TO WS-OKAY.
5Q1JM1     INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1     MOVE "RSN0042"            TO WK-C-RPRCODE.
5Q1JM1     PERFORM E002-PROCESS-RPRRSN
5Q1JM1         THRU E002-PROCESS-RPRRSN-EX.
T55YTW
T55YTW C599-VALIDATION-PART-EX.
T55YTW     EXIT.

CMP3A1 C600-VALIDATION-PART.
CMP3A2     INITIALIZE WK-C-XGSPA-RECORD.
CMP3A2     MOVE "RSYACCLEN"          TO WK-C-XGSPA-GHPARCD.
CMP3A2     CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
CMP3A2     MOVE WK-C-XGSPA-GHPARVAL(1:2) TO WK-N-ACCTLEN.
CMP3A1**** This routine is performed for incoming MT101 messages,
CMP3A1**** retrieve debiting Account from Tag50 (Ordering Customer) instead
CMP3A1*-- Get Ordering Customer (Tag50) from UFIMIJ
CMP3A1     PERFORM R001-READ-UFIMIJ
CMP3A1         THRU R001-READ-UFIMIJ-EX.
CMP3A1
CMP3A1*-- Check Customer Processing Feature and WEE Hours.
CMP3A1     MOVE "Y" TO TAB-VL2(08).
CMP3A1     CALL "TRFGSDTS"           USING WK-C-GSDTS-RECORD.
CMP3A1
CMP3A1     INITIALIZE WK-C-CUPF-RECORD.
CMP3A1*CMP3A2 MOVE WS-T50-ACCNO(1:10) TO WK-C-CUPF-ACCNO.
CMP3A2     MOVE WS-T50-ACCNO(1:WK-N-ACCTLEN)
CMP3A2                                TO WK-C-CUPF-ACCNO.
CMP3A1     MOVE TFSSTPL-CUYCD        TO WK-C-CUPF-CUYCD.
CMP3A1     MOVE WS-BANKID            TO WK-C-CUPF-BANKID.
CMP3A1     MOVE "1"                  TO WK-C-CUPF-OPTION.
CMP3A1
CMP3A1     CALL "TRFCUPF" USING WK-C-CUPF-RECORD.
CMP3A1     CANCEL "TRFCUPF".
CMP3A1
CMP3A1     IF WK-C-CUPF-ERROR-CD NOT = SPACES
CMP3A1         MOVE "N" TO WS-OKAY
CMP3A1                     TAB-VL2(08)
CMP3A1         INITIALIZE WK-C-RPRRSN-AREA
CMP3A1         MOVE WK-C-CUPF-ERROR-CD TO WK-C-RPRCODE
CMP3A1         PERFORM E002-PROCESS-RPRRSN
CMP3A1             THRU E002-PROCESS-RPRRSN-EX
CMP3A1     END-IF.
CMP3A1
CMP3A1*-- Account must be eligible for Inward MT101 process.
CMP3A1     IF WK-C-CUPF-EFFDTE = ZEROES OR
CMP3A1        WK-C-CUPF-EFFDTE > WK-N-GSDTS-SYSDTE
CMP3A1         MOVE "N" TO WS-OKAY
CMP3A1                     TAB-VL2(08)
CMP3A1         INITIALIZE WK-C-RPRRSN-AREA
CMP3A1         MOVE "RSN0317"        TO WK-C-RPRCODE
CMP3A1         PERFORM E002-PROCESS-RPRRSN
CMP3A1             THRU E002-PROCESS-RPRRSN-EX
CMP3A1     END-IF.
CMP3A1
CMP3A1*-- Account must be eligible for WEE HOUR processing.
CMP3A1     IF WK-C-CUPF-WEEHR-PERIOD = "Y"
CMP3A1         IF WK-C-CUPF-WEEIND NOT = "Y"
CMP3A1             MOVE "N" TO WS-OKAY
CMP3A1                         TAB-VL2(08)
CMP3A1             INITIALIZE WK-C-RPRRSN-AREA
CMP3A1             MOVE "RSN0318"    TO WK-C-RPRCODE
CMP3A1             PERFORM E002-PROCESS-RPRRSN
CMP3A1                 THRU E002-PROCESS-RPRRSN-EX
CMP3A1         END-IF
CMP3A1     END-IF.
CMP3A1
CMP3A1*-- Check if Ordering Customer's Account No. is valid.
CMP3A1     IF WS-OKAY = "Y" AND TAB-VL2(08) = "Y"
CMP3A1         IF WK-VTD1-RBK-IND NOT = "Y"
CMP3A1             IF WK-C-CUPF-ACCTYP = "C"
CMP3A1             OR WK-C-CUPF-ACCTYP = "F"
CMP3A1*REM269          IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
REM269                 IF TFSSTPL-CUYCD = WK-C-LCUYCD
CMP3A1                     MOVE "CA"     TO WS-PMODE
CMP3A1*CMP3A2              MOVE WS-T50-ACCNO(1:10) TO WK-C-VCCA-CA-NO
CMP3A2                     MOVE WS-T50-ACCNO(1:WK-N-ACCTLEN)
CMP3A2                                    TO WK-C-VCCA-CA-NO
CMP3A1                     CALL "TRFVCCA" USING WK-C-VCCA-RECORD
CMP3A1                     IF WK-C-VCCA-ERROR-CD = SPACES
CMP3A1                         MOVE WK-C-VCCA-CUSTFNAM TO WK-VTD1-PAYRNAME
CMP3A1                         MOVE WK-C-VCCA-ADDR1  TO WK-VTD1-PAYRADR1
CMP3A1                         MOVE WK-C-VCCA-ADDR2  TO WK-VTD1-PAYRADR2
CMP3A1                         MOVE WK-C-VCCA-ADDR3  TO WK-VTD1-PAYRADR3
CMP3A1                         MOVE WK-C-VCCA-ADDR4  TO WK-VTD1-PAYRADR4
CMP3A1                         MOVE WK-C-VCCA-ADDR5  TO WK-VTD1-PAYRADR5
CMP3A1                         MOVE WK-C-VCCA-ADDR6  TO WK-VTD1-PAYRADR6
CMP3A1                         MOVE WK-C-VCCA-AOCD   TO WK-VTD1-AOCD
CMP3A1                         MOVE WK-N-VCCA-RESCD  TO WK-VTD1-RESCD
CMP3A1                         MOVE WK-N-VCCA-DOMBRCH TO WK-VTD1-DOMBRCH
CMP3A1                         MOVE WK-N-VCCA-HOLDCD1 TO WK-VTD1-HOLDCD1
CMP3A1                         MOVE WK-N-VCCA-HOLDCD2 TO WK-VTD1-HOLDCD2
CMP3A1                         MOVE WK-N-VCCA-HOLDCD3 TO WK-VTD1-HOLDCD3
CMP3A1                     END-IF
CMP3A1                 ELSE
CMP3A1                     MOVE "FCCA"   TO WS-PMODE
CMP3A1*CMP3A2              MOVE WS-T50-ACCNO(1:10) TO WK-C-VCFA-FCCA
CMP3A2                     MOVE WS-T50-ACCNO(1:WK-N-ACCTLEN)
CMP3A2                                    TO WK-C-VCFA-FCCA
CMP3A1                     MOVE TFSSTPL-CUYCD TO WK-C-VCFA-CUY
CMP3A1                     CALL "TRFVCFA" USING WK-C-VCFA-RECORD
CMP3A1                     IF WK-C-VCFA-ERROR-CD = SPACES
CMP3A1                         MOVE WK-C-VCFA-CUSTFNAM TO WK-VTD1-PAYRNAME
CMP3A1                         MOVE WK-C-VCFA-ADDR1  TO WK-VTD1-PAYRADR1
CMP3A1                         MOVE WK-C-VCFA-ADDR2  TO WK-VTD1-PAYRADR2
CMP3A1                         MOVE WK-C-VCFA-ADDR3  TO WK-VTD1-PAYRADR3
CMP3A1                         MOVE WK-C-VCFA-ADDR4  TO WK-VTD1-PAYRADR4
CMP3A1                         MOVE WK-C-VCFA-ADDR5  TO WK-VTD1-PAYRADR5
CMP3A1                         MOVE WK-C-VCFA-ADDR6  TO WK-VTD1-PAYRADR6
CMP3A1                         MOVE WK-C-VCFA-AOCD   TO WK-VTD1-AOCD
CMP3A1                         MOVE WK-N-VCFA-RESCD  TO WK-VTD1-RESCD
CMP3A1                         MOVE WK-N-VCFA-DOMBRCH TO WK-VTD1-DOMBRCH
CMP3A1                         MOVE WK-N-VCFA-HOLDCD1 TO WK-VTD1-HOLDCD1
CMP3A1                         MOVE WK-N-VCFA-HOLDCD2 TO WK-VTD1-HOLDCD2
CMP3A1                         MOVE WK-N-VCFA-HOLDCD3 TO WK-VTD1-HOLDCD3
CMP3A1                     END-IF
CMP3A1                 END-IF
CMP3A1*REM269          IF (TFSSTPL-CUYCD  = TFSCLSYS-LCUYCD
REM269                 IF (TFSSTPL-CUYCD  = WK-C-LCUYCD
CMP3A1                 AND WK-C-VCCA-ERROR-CD NOT = SPACES)
CMP3A1*REM269          OR (TFSSTPL-CUYCD  NOT = TFSCLSYS-LCUYCD
REM269                 OR (TFSSTPL-CUYCD  NOT = WK-C-LCUYCD
CMP3A1                 AND WK-C-VCFA-ERROR-CD NOT = SPACES)
CMP3A1                     MOVE "N"  TO WS-OKAY, TAB-VL2(09)
CMP3A1                     INITIALIZE WK-C-RPRRSN-AREA
CMP3A1                     IF WK-C-VCCA-ERROR-CD NOT = SPACES
CMP3A1                         MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
CMP3A1                         MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCDE
CMP3A1                         MOVE WK-N-VCCA-STAFFIND TO WK-N-STAFFIND
CMP3A1                         MOVE WK-C-VCCA-CUSTFNAM TO WK-C-ACCNAME
CMP3A1                     ELSE
CMP3A1                         MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
CMP3A1                         MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCDE
CMP3A1                         MOVE WK-N-VCFA-STAFFIND TO WK-N-STAFFIND
CMP3A1                         MOVE WK-C-VCFA-CUSTFNAM TO WK-C-ACCNAME
CMP3A1                     END-IF
CMP3A1                     PERFORM E002-PROCESS-RPRRSN
CMP3A1                         THRU E002-PROCESS-RPRRSN-EX
CMP3A1                 END-IF
CMP3A1             ELSE
CMP3A1                 MOVE "N" TO WS-OKAY, TAB-VL2(09)
CMP3A1                 INITIALIZE WK-C-RPRRSN-AREA
CMP3A1                 MOVE "RSN0091" TO WK-C-RPRCODE
CMP3A1                 PERFORM E002-PROCESS-RPRRSN
CMP3A1                     THRU E002-PROCESS-RPRRSN-EX
CMP3A1             END-IF
CMP3A1         ELSE
CMP3A1             MOVE "N"          TO WS-OKAY
CMP3A1             MOVE "X"          TO TAB-VL2(09)
CMP3A1             INITIALIZE WK-C-RPRRSN-AREA
CMP3A1             MOVE "RSN0035"    TO WK-C-RPRCODE
CMP3A1             PERFORM E002-PROCESS-RPRRSN
CMP3A1                 THRU E002-PROCESS-RPRRSN-EX
CMP3A1         END-IF
CMP3A1         PERFORM D300-LOGGING THRU D399-LOGGING-EX
CMP3A1     END-IF.
CMP3A1
CMP3A1 C699-VALIDATION-PART-EX.
CMP3A1     EXIT.

       D100-VALIDATION.
           IF TABLE-ARRAY NOT = ALL "X"
              MOVE "Y"                    TO WS-OKAY
           ELSE
6Q3LN1        IF TFSSTPL-IMSGTYPE       = "M"
6Q3LN1        AND SW-RTGS-BYPASS-F53-F54-Y
6Q3LN1            IF WK-RTGS-BNKAC-EXIST = "Y"
6Q3LN1               MOVE "Y"          TO WS-OKAY
6Q3LN1            END-IF
6Q3LN1        ELSE
                MOVE "N"                    TO WS-OKAY
5Q1JM1          INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1          MOVE "RSN0114"            TO WK-C-RPRCODE
5Q1JM1          PERFORM E002-PROCESS-RPRRSN
5Q1JM1            THRU E002-PROCESS-RPRRSN-EX
6Q3LN1        END-IF
           END-IF.

           IF ((TAG53-OPT                  = "A"
           AND TAG53-BIC NOT = SPACES)
           OR (TAG54-OPT                   = "A"
           AND TAG54-BIC NOT = SPACES))
           AND WS-OKAY                     = "Y"
               PERFORM C400-TABLE-D2 THRU C499-TABLE-D2-EX
           END-IF.

           IF TAB-VAL(01) NOT = "X" AND WS-OKAY = "Y"
               MOVE TAB-VAL(01)            TO TAB-VL2(01)
               MOVE TFSSTPL-BNKENTTY       TO WK-N-VBANO-BNKENTTY
               MOVE WS-BANKID              TO WK-C-VBANO-BANKID
               MOVE TFSSTPL-CUYCD          TO WK-C-VBANO-CUYCD
GP3A01     IF WS-ACCNO NOT = SPACES AND
GP3A01        WS-ACCNO(WK-N-ACCLEN:1) = SPACES
GP3A01         CONTINUE
GP3A01     ELSE
GP3A01         MOVE WS-ACCNO(4:) TO WS-ACCNO
GP3A01     END-IF
GP3A01     MOVE WS-ACCNO             TO WK-C-NEW-ACCNO2
GP3A01     MOVE WK-C-NEW-ACCNO       TO WK-C-VBANO-ACCNO
GP3A01*    MOVE WS-ACCNO             TO WK-C-VBANO-ACCNO
         CALL "TRFVBANO"             USING WK-C-VBANO-RECORD
         IF WK-C-VBANO-ERROR-CD NOT = SPACES
             MOVE "N" TO WS-OKAY, TAB-VL2(01)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE WK-C-VBANO-ERROR-CD TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
         ELSE
             MOVE WK-C-VBANO-ACUDBUI TO WS-ACUDBUI
             MOVE WK-C-VBANO-ACCTYP  TO WS-ACCTYP
         END-IF
202VKE     MOVE WS-RECBNKID          TO TFS202V-MT202VBK
  |          READ TFS202V
  |          NOT INVALID KEY
  |              MOVE "N" TO WS-OKAY, TAB-VL2(01)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "SUP0036"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
202VKE     END-READ
202VKE     MOVE WS-SENCBNKID         TO TFS202V-MT202VBK
  |          READ TFS202V
  |          NOT INVALID KEY
  |              MOVE "N" TO WS-OKAY, TAB-VL2(01)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "SUP0037"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
202VKE     END-READ
G2BM00*    Check DR BIC againts NSTP Table.
G2BM01     IF WS-C-GPI-SW = "Y"
G2BM01     AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
G2BM01     AND WS-C-FCY-NSTP-SW = "Y"
G2BM01     AND WS-OKAY               = "Y"
G2BM01         INITIALIZE            WK-NSTP
G2BM01         MOVE WS-BANKID        TO WK-NSTP-ACCTBIC
G2BM01         CALL "TRFNSTP"        USING WK-NSTP
G2BM01         IF WK-NSTP-NONSTPDR = "Y"
G2BM01             MOVE "N" TO WS-OKAY
G2BM01                         TAB-VL2(01)
G2BM01             INITIALIZE WK-C-RPRRSN-AREA
G2BM00* ------------> RSN0038: Bank ID found in NON STP TABLE
G2BM01             MOVE "RSN0038"    TO WK-C-RPRCODE
G2BM01             PERFORM E002-PROCESS-RPRRSN
G2BM01                 THRU E002-PROCESS-RPRRSN-EX
G2BM01         ELSE
G2BM00*            Check DR Account againts NSTP Table.
G2BM01             INITIALIZE        WK-NSTP
GP3A03*G2BM01      MOVE WS-ACCNO     TO WK-NSTP-ACCTBIC
GP3A03             MOVE WK-C-NEW-ACCNO TO WK-NSTP-ACCTBIC
G2BM01             CALL "TRFNSTP"    USING WK-NSTP
G2BM01             IF WK-NSTP-NONSTPDR = "Y"
G2BM01                 MOVE "N" TO WS-OKAY
G2BM01                             TAB-VL2(01)
G2BM01                 INITIALIZE WK-C-RPRRSN-AREA
G2BM00* ---------------> RSN0034: A/C number exist in Non STP Table
G2BM01                 MOVE "RSN0034" TO WK-C-RPRCODE
G2BM01                 PERFORM E002-PROCESS-RPRRSN
G2BM01                     THRU E002-PROCESS-RPRRSN-EX
G2BM01             END-IF
G2BM01         END-IF
G2BM01     END-IF
G2BM00*Check Account againts Account mandate Table.
G2BM00     IF WS-C-GPI-SW            = "Y"
G2BM00     AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
G2BM00     AND WK-C-ACMN-CHECK       = "Y"
G2BM00     AND WS-OKAY               = "Y"
G2BM00         IF WS-ACCTYP          = "N"
G2BM00             GO TO TAB-VAL2-CHECK
G2BM00         END-IF
G2BM00         INITIALIZE            WK-ACMN
G2BM00         MOVE TFSSTPL-BNKENTTY
G2BM00                                TO WK-ACMN-BANKCD
G2BM00         MOVE TFSSTPL-CUYCD
G2BM00                                TO WK-ACMN-CUYCD
GP3A02         IF WS-ACCNO NOT = SPACES AND
GP3A02            WS-ACCNO(WK-N-ACCLEN:1) = SPACES
G2BM00             MOVE WS-ACCNO     TO WK-ACMN-ACCNO
GP3A02         ELSE
GP3A02             MOVE WS-ACCNO(4:)
GP3A02                                TO WK-ACMN-ACCNO
GP3A02         END-IF
G2BM00         MOVE TFSSTPL-SENBNKID
G2BM00                                TO WK-ACMN-BANKID
G2BM00         CALL "TRFACMN" USING WK-ACMN
G2BM00         IF WK-ACMN-MANIND     = "N"
G2BM00             MOVE "N"          TO WS-OKAY
G2BM00                                  TAB-VL2(01)
G2BM00             INITIALIZE        WK-C-RPRRSN-AREA
G2BM00             MOVE "RSN0097"
G2BM00                                TO WK-C-RPRCODE
G2BM00             PERFORM E002-PROCESS-RPRRSN
G2BM00                 THRU E002-PROCESS-RPRRSN-EX
G2BM00         END-IF
G2BM00     END-IF
         PERFORM D300-LOGGING THRU D399-LOGGING-EX
           END-IF.

G2BM00 TAB-VAL2-CHECK.
           IF TAB-VAL(02) NOT = "X" AND WS-OKAY = "Y"
               MOVE TAB-VAL(02)            TO TAB-VL2(02)
               MOVE TFSSTPL-BNKENTTY       TO WK-N-VBAC-BNKENTTY
               MOVE WS-BANKID              TO WK-C-VBAC-BANKID
               MOVE TFSSTPL-CUYCD          TO WK-C-VBAC-CUYCD
               CALL "TRFVBAC"              USING WK-C-VBAC-RECORD
               IF WK-C-VBAC-ERROR-CD NOT = SPACES
                   MOVE "N" TO WS-OKAY, TAB-VL2(02)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
               ELSE
                   MOVE WK-C-VBAC-ACUDBUI  TO WS-ACUDBUI
                   MOVE WK-C-VBAC-ACCTYP   TO WS-ACCTYP
ID1VK1         IF WK-C-VBAC-BNKACNO NOT = SPACES AND
ID1VK1            WK-C-VBAC-BNKACNO(WK-N-ACCLEN:1) = SPACES
ID1VK1             MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
ID1VK1         ELSE
ID1VK1             MOVE WK-C-VBAC-BNKACNO(4:) TO WS-ACCNO
ID1VK1         END-IF
               END-IF
202VKE     MOVE WS-RECBNKID          TO TFS202V-MT202VBK
202VKE         READ TFS202V
202VKE         NOT INVALID KEY
202VKE             MOVE "N" TO WS-OKAY, TAB-VL2(01)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "RSN0036"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
202VKE     END-READ
202VKE     MOVE WS-SENCBNKID         TO TFS202V-MT202VBK
  |          READ TFS202V
  |          NOT INVALID KEY
  |              MOVE "N" TO WS-OKAY, TAB-VL2(02)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "RSN0037"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
202VKE     END-READ
G2BM00*    Check DR BIC againts NSTP Table.
G2BM01     IF WS-C-GPI-SW = "Y"
           IF ((TAG53-OPT                  = "A"
           AND TAG53-BIC                   NOT = SPACES)
           OR (TAG54-OPT                   = "A"
           AND TAG54-BIC                   NOT = SPACES))
           AND WS-OKAY                     = "Y"
               PERFORM C400-TABLE-D2 THRU C499-TABLE-D2-EX
           END-IF.

           IF TAB-VAL(01) NOT = "X" AND WS-OKAY = "Y"
               MOVE TAB-VAL(01)            TO TAB-VL2(01)
               MOVE TFSSTPL-BNKENTTY       TO WK-N-VBANO-BNKENTTY
               MOVE WS-BANKID              TO WK-C-VBANO-BANKID
               MOVE TFSSTPL-CUYCD          TO WK-C-VBANO-CUYCD
GP3A01     IF WS-ACCNO NOT = SPACES AND
GP3A01        WS-ACCNO(WK-N-ACCLEN:1) = SPACES
GP3A01         CONTINUE
GP3A01     ELSE
GP3A01         MOVE WS-ACCNO(4:) TO WS-ACCNO
GP3A01     END-IF
GP3A01     MOVE WS-ACCNO             TO WK-C-NEW-ACCNO2
GP3A01     MOVE WK-C-NEW-ACCNO       TO WK-C-VBANO-ACCNO
GP3A01*    MOVE WS-ACCNO             TO WK-C-VBANO-ACCNO
               CALL "TRFVBANO"             USING WK-C-VBANO-RECORD
               IF WK-C-VBANO-ERROR-CD NOT = SPACES
                   MOVE "N" TO WS-OKAY, TAB-VL2(01)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE WK-C-VBANO-ERROR-CD TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
               ELSE
                   MOVE WK-C-VBANO-ACUDBUI TO WS-ACUDBUI
                   MOVE WK-C-VBANO-ACCTYP  TO WS-ACCTYP
               END-IF
202VKE     MOVE WS-RECBNKID          TO TFS202V-MT202VBK
202VKE         READ TFS202V
202VKE         NOT INVALID KEY
202VKE             MOVE "N" TO WS-OKAY, TAB-VL2(01)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "SUP0036"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
202VKE     END-READ
202VKE     MOVE WS-SENCBNKID         TO TFS202V-MT202VBK
202VKE         READ TFS202V
202VKE         NOT INVALID KEY
202VKE             MOVE "N" TO WS-OKAY, TAB-VL2(01)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "SUP0037"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
202VKE     END-READ
G2BM00*    Check DR BIC againts NSTP Table.
G2BM01     IF WS-C-GPI-SW = "Y"
G2BM01     AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
G2BM01     AND WS-C-FCY-NSTP-SW = "Y"
G2BM01     AND WS-OKAY               = "Y"
G2BM01         INITIALIZE            WK-NSTP
G2BM01         MOVE WS-BANKID        TO WK-NSTP-ACCTBIC
G2BM01         CALL "TRFNSTP"        USING WK-NSTP
G2BM01         IF WK-NSTP-NONSTPDR = "Y"
G2BM01             MOVE "N" TO WS-OKAY
G2BM01                         TAB-VL2(01)
G2BM01             INITIALIZE WK-C-RPRRSN-AREA
G2BM00* ------------> RSN0038: Bank ID found in NON STP TABLE
G2BM01             MOVE "RSN0038"    TO WK-C-RPRCODE
G2BM01             PERFORM E002-PROCESS-RPRRSN
G2BM01                 THRU E002-PROCESS-RPRRSN-EX
G2BM01         ELSE
G2BM00*            Check DR Account againts NSTP Table.
G2BM01             INITIALIZE        WK-NSTP
GP3A03*G2BM01      MOVE WS-ACCNO     TO WK-NSTP-ACCTBIC
GP3A03             MOVE WK-C-NEW-ACCNO TO WK-NSTP-ACCTBIC
G2BM01             CALL "TRFNSTP"    USING WK-NSTP
G2BM01             IF WK-NSTP-NONSTPDR = "Y"
G2BM01                 MOVE "N" TO WS-OKAY
G2BM01                             TAB-VL2(01)
G2BM01                 INITIALIZE WK-C-RPRRSN-AREA
G2BM00* ---------------> RSN0034: A/C number exist in Non STP Table
G2BM01                 MOVE "RSN0034" TO WK-C-RPRCODE
G2BM01                 PERFORM E002-PROCESS-RPRRSN
G2BM01                     THRU E002-PROCESS-RPRRSN-EX
G2BM01             END-IF
G2BM01         END-IF
G2BM01     END-IF
G2BM00*Check Account againts Account mandate Table.
G2BM00     IF WS-C-GPI-SW            = "Y"
G2BM00     AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
G2BM00     AND WK-C-ACMN-CHECK       = "Y"
G2BM00     AND WS-OKAY               = "Y"
G2BM00         IF WS-ACCTYP          = "N"
G2BM00             GO TO TAB-VAL2-CHECK
G2BM00         END-IF
G2BM00         INITIALIZE            WK-ACMN
G2BM00         MOVE TFSSTPL-BNKENTTY
G2BM00                                TO WK-ACMN-BANKCD
G2BM00         MOVE TFSSTPL-CUYCD
G2BM00                                TO WK-ACMN-CUYCD
GP3A02         IF WS-ACCNO NOT = SPACES AND
GP3A02            WS-ACCNO(WK-N-ACCLEN:1) = SPACES
G2BM00             MOVE WS-ACCNO     TO WK-ACMN-ACCNO
GP3A02         ELSE
GP3A02             MOVE WS-ACCNO(4:)
GP3A02                                TO WK-ACMN-ACCNO
GP3A02         END-IF
G2BM00         MOVE TFSSTPL-SENBNKID
G2BM00                                TO WK-ACMN-BANKID
G2BM00         CALL "TRFACMN" USING WK-ACMN
G2BM00         IF WK-ACMN-MANIND     = "N"
G2BM00             MOVE "N"          TO WS-OKAY
G2BM00                                  TAB-VL2(01)
G2BM00             INITIALIZE        WK-C-RPRRSN-AREA
G2BM00             MOVE "RSN0097"
G2BM00                                TO WK-C-RPRCODE
G2BM00             PERFORM E002-PROCESS-RPRRSN
G2BM00                 THRU E002-PROCESS-RPRRSN-EX
G2BM00         END-IF
G2BM00     END-IF
               PERFORM D300-LOGGING THRU D399-LOGGING-EX
           END-IF.

G2BM00 TAB-VAL2-CHECK.
           IF TAB-VAL(02) NOT = "X" AND WS-OKAY = "Y"
               MOVE TAB-VAL(02)            TO TAB-VL2(02)
               MOVE TFSSTPL-BNKENTTY       TO WK-N-VBAC-BNKENTTY
               MOVE WS-BANKID              TO WK-C-VBAC-BANKID
               MOVE TFSSTPL-CUYCD          TO WK-C-VBAC-CUYCD
               CALL "TRFVBAC"              USING WK-C-VBAC-RECORD
               IF WK-C-VBAC-ERROR-CD NOT = SPACES
                   MOVE "N" TO WS-OKAY, TAB-VL2(02)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
               ELSE
                   MOVE WK-C-VBAC-ACUDBUI  TO WS-ACUDBUI
                   MOVE WK-C-VBAC-ACCTYP   TO WS-ACCTYP
ID1VK1         IF WK-C-VBAC-BNKACNO NOT = SPACES AND
ID1VK1            WK-C-VBAC-BNKACNO(WK-N-ACCLEN:1) = SPACES
ID1VK1             MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
ID1VK1         ELSE
ID1VK1             MOVE WK-C-VBAC-BNKACNO(4:) TO WS-ACCNO
ID1VK1         END-IF
               END-IF
202VKE     MOVE WS-RECBNKID          TO TFS202V-MT202VBK
  |          READ TFS202V
  |          NOT INVALID KEY
  |              MOVE "N" TO WS-OKAY, TAB-VL2(01)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "RSN0036"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
202VKE     END-READ
202VKE     MOVE WS-SENCBNKID         TO TFS202V-MT202VBK
  |          READ TFS202V
  |          NOT INVALID KEY
  |              MOVE "N" TO WS-OKAY, TAB-VL2(02)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "RSN0037"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
202VKE     END-READ
G2BM00*    Check DR BIC againts NSTP Table.
G2BM01     IF WS-C-GPI-SW = "Y"
G2BM01     AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
G2BM01     AND WS-C-FCY-NSTP-SW = "Y"
G2BM01     AND WS-OKAY               = "Y"
G2BM01         INITIALIZE            WK-NSTP
G2BM01         MOVE WS-BANKID        TO WK-NSTP-ACCTBIC
G2BM01         CALL "TRFNSTP"        USING WK-NSTP
G2BM01         IF WK-NSTP-NONSTPDR = "Y"
G2BM01             MOVE "N" TO WS-OKAY
G2BM01                         TAB-VL2(02)
G2BM01             INITIALIZE WK-C-RPRRSN-AREA
G2BM00* ------------> RSN0038: Bank ID found in NON STP TABLE
G2BM01             MOVE "RSN0038"    TO WK-C-RPRCODE
G2BM01             PERFORM E002-PROCESS-RPRRSN
G2BM01                 THRU E002-PROCESS-RPRRSN-EX
G2BM01         ELSE
G2BM00*            Check DR Account againts NSTP Table.
G2BM01             INITIALIZE        WK-NSTP
GP3A03             IF WS-ACCNO NOT = SPACES AND
GP3A03                WS-ACCNO(WK-N-ACCLEN:1) = SPACES
GP3A03                 CONTINUE
GP3A03             ELSE
GP3A03                 MOVE WS-ACCNO(4:) TO WS-ACCNO
GP3A03             END-IF
GP3A03             MOVE WS-ACCNO     TO WK-C-NEW-ACCNO2
GP3A03             MOVE WK-C-NEW-ACCNO TO WK-NSTP-ACCTBIC
GP3A03*G2BM01      MOVE WS-ACCNO     TO WK-NSTP-ACCTBIC
G2BM01             CALL "TRFNSTP"    USING WK-NSTP
G2BM01             IF WK-NSTP-NONSTPDR = "Y"
G2BM01                 MOVE "N" TO WS-OKAY
G2BM01                             TAB-VL2(02)
G2BM01                 INITIALIZE WK-C-RPRRSN-AREA
G2BM00* ---------------> RSN0034: A/C number exist in Non STP Table
G2BM01                 MOVE "RSN0034" TO WK-C-RPRCODE
G2BM01                 PERFORM E002-PROCESS-RPRRSN
G2BM01                     THRU E002-PROCESS-RPRRSN-EX
G2BM01             END-IF
G2BM01         END-IF
G2BM01     END-IF
G2BM00*--- Check account mandate table if tag BIC is not equal sending bic
G2BM00     IF WS-C-GPI-SW            = "Y"
G2BM00     AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
G2BM00     AND WK-C-ACMN-CHECK       = "Y"
G2BM00     AND WS-OKAY               = "Y"
G2BM00         IF WS-ACCTYP          = "N"
G2BM00             GO TO TAB-VAL3-CHECK
G2BM00         END-IF
G2BM00         PERFORM E003-ACC-MANDATE-CHECK
G2BM00             THRU E003-ACC-MANDATE-CHECK-EX
G2BM00     END-IF
               PERFORM D300-LOGGING THRU D399-LOGGING-EX
           END-IF.

G2BM00 TAB-VAL3-CHECK.
           IF TAB-VAL(03) NOT = "X" AND WS-OKAY = "Y"
               MOVE TAB-VAL(03)            TO TAB-VL2(03)
CMR4J1*-- Bank account table validation.
               MOVE TFSSTPL-BNKENTTY       TO WK-N-VBAC-BNKENTTY
               MOVE WS-BANKID              TO WK-C-VBAC-BANKID
CMR4J1*                                 WK-BLKB-BKID
               MOVE TFSSTPL-CUYCD          TO WK-C-VBAC-CUYCD
               CALL "TRFVBAC"              USING WK-C-VBAC-RECORD
CMR4J1*  CALL "TRFBLKB"              USING WK-BLKB
               IF WK-C-VBAC-ERROR-CD NOT = SPACES
CMR4J1*  OR WK-BLKB-INDIC            = "Y"
                   MOVE "N" TO WS-OKAY, TAB-VL2(03)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
               ELSE
                   MOVE WK-C-VBAC-ACUDBUI  TO WS-ACUDBUI
                   MOVE WK-C-VBAC-ACCTYP   TO WS-ACCTYP
ID1VK1         IF WK-C-VBAC-BNKACNO NOT = SPACES AND
ID1VK1            WK-C-VBAC-BNKACNO(WK-N-ACCLEN:1) = SPACES
ID1VK1             MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
ID1VK1         ELSE
ID1VK1             MOVE WK-C-VBAC-BNKACNO(4:) TO WS-ACCNO
ID1VK1         END-IF
               END-IF
CMR4J1*-- Blacklist table validation.
CMR4J1     INITIALIZE WK-BLKB
CMR4J1     MOVE WS-BANKID            TO WK-BLKB-BKID
CMR4J1     CALL "TRFBLKB" USING WK-BLKB
CMR4J1     IF WK-BLKB-INDIC          = "Y"
CMR4J1         MOVE "N" TO WS-OKAY, TAB-VL2(03)
CMR4J1         INITIALIZE WK-C-RPRRSN-AREA
CMR4J1         MOVE "RSN0098"        TO WK-C-RPRCODE
CMR4J1         PERFORM E002-PROCESS-RPRRSN
CMR4J1             THRU E002-PROCESS-RPRRSN-EX
CMR4J1     END-IF
G2BM00*    Check DR BIC againts NSTP Table.
G2BM01     IF WS-C-GPI-SW = "Y"
G2BM01     AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
G2BM01     AND WS-C-FCY-NSTP-SW = "Y"
G2BM01     AND WS-OKAY               = "Y"
G2BM01         INITIALIZE            WK-NSTP
G2BM01         MOVE WS-BANKID        TO WK-NSTP-ACCTBIC
G2BM01         CALL "TRFNSTP"        USING WK-NSTP
G2BM01         IF WK-NSTP-NONSTPDR = "Y"
G2BM01             MOVE "N" TO WS-OKAY
G2BM01                         TAB-VL2(03)
G2BM01             INITIALIZE WK-C-RPRRSN-AREA
G2BM00* ------------> RSN0038: Bank ID found in NON STP TABLE
G2BM01             MOVE "RSN0038"    TO WK-C-RPRCODE
G2BM01             PERFORM E002-PROCESS-RPRRSN
G2BM01                 THRU E002-PROCESS-RPRRSN-EX
G2BM01         ELSE
G2BM00*            Check DR Account againts NSTP Table.
G2BM01             INITIALIZE        WK-NSTP
GP3A03             IF WS-ACCNO NOT = SPACES AND
GP3A03                WS-ACCNO(WK-N-ACCLEN:1) = SPACES
GP3A03                 CONTINUE
GP3A03             ELSE
GP3A03                 MOVE WS-ACCNO(4:) TO WS-ACCNO
GP3A03             END-IF
GP3A03             MOVE WS-ACCNO     TO WK-C-NEW-ACCNO2
GP3A03             MOVE WK-C-NEW-ACCNO TO WK-NSTP-ACCTBIC
GP3A03*G2BM01      MOVE WS-ACCNO     TO WK-NSTP-ACCTBIC
G2BM01             CALL "TRFNSTP"    USING WK-NSTP
G2BM01             IF WK-NSTP-NONSTPDR = "Y"
G2BM01                 MOVE "N" TO WS-OKAY
G2BM01                             TAB-VL2(03)
G2BM01                 INITIALIZE WK-C-RPRRSN-AREA
G2BM00* ---------------> RSN0034: A/C number exist in Non STP Table
G2BM01                 MOVE "RSN0034" TO WK-C-RPRCODE
G2BM01                 PERFORM E002-PROCESS-RPRRSN
G2BM01                     THRU E002-PROCESS-RPRRSN-EX
G2BM01             END-IF
G2BM01         END-IF
G2BM01     END-IF
               PERFORM D300-LOGGING THRU D399-LOGGING-EX
           END-IF.

           IF TAB-VAL(04) NOT = "X" AND WS-OKAY = "Y"
               MOVE TAB-VAL(04)            TO TAB-VL2(04)
               IF WS-ACCTYP                NOT = "C"
               AND WS-ACCTYP               NOT = "F"
G2BM00     OR (WS-C-GPI-SW           = "Y"
G2BM00     AND WK-C-VALID-MTMSG      = "Y"
G2BM00     AND TAB-VL2(03)           = "X")
G2BM00         IF (WS-ACCTYP         = "C"
G2BM00         OR  WS-ACCTYP         = "F")
G2BM00             EVALUATE WS-ACCTYP
G2BM00                 WHEN "C"
G2BM00                     MOVE "CA"
G2BM00                                TO WS-PMODE
G2BM00                 WHEN "F"
G2BM00                     MOVE "FCCA"
G2BM00                                TO WS-PMODE
G2BM00             END-EVALUATE
G2BM00             MOVE SPACE        TO WS-ACT1
G2BM00             MOVE "Y"          TO WS-ACT2
G2BM00             MOVE "Y"          TO TAB-VAL(05)
G2BM00             MOVE "N"          TO TAB-VAL(06)
G2BM00         ELSE
                   MOVE "NOSTRO"           TO WS-PMODE
                   IF TAB-VL2(03)          = "X"
                   AND TAB-VL2(04)         = "Y"
                       MOVE "Y"            TO WS-ACT1
                       MOVE SPACE          TO WS-ACT2
                   ELSE
                       IF TAB-VL2(03)      = "Y"
                       AND TAB-VL2(04)     = "Y"
                           MOVE SPACE      TO WS-ACT1
                           MOVE "Y"        TO WS-ACT2
                       ELSE
                           MOVE SPACE      TO WS-ACT1
                                              WS-ACT2
                       END-IF
                   END-IF
G2BM00         END-IF
G2BM00     END-IF
G2BM00 ELSE
                   MOVE "N"                TO TAB-VL2(04)
                   IF TAB-VL2(03)          = "Y"
                       MOVE PATH-P6        TO TABLE-ARRAY
                   ELSE
                       MOVE "N"            TO WS-OKAY
5Q1JM1             INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1             MOVE "RSN0223"    TO WK-C-RPRCODE
5Q1JM1             PERFORM E002-PROCESS-RPRRSN
5Q1JM1                 THRU E002-PROCESS-RPRRSN-EX
                   END-IF
               END-IF
               PERFORM D300-LOGGING THRU D399-LOGGING-EX
           END-IF.

           IF TAB-VAL(05) NOT = "X" AND WS-OKAY = "Y"
SM1TY1     IF WK-VTD1-RBK-IND NOT = "Y"
               MOVE TAB-VAL(05)            TO TAB-VL2(05)
               IF WS-ACCTYP                = "C"
               OR WS-ACCTYP                = "F"
REM269****   IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
REM269       IF TFSSTPL-CUYCD = WK-C-LCUYCD
                       MOVE "CA"           TO WS-PMODE
ID1VKE*            MOVE WS-ACCNO     TO WK-C-VCCA-CA-NO
ID1VKE             MOVE WS-ACCNO     TO WK-C-NEW-ACCNO2
ID1VKE             MOVE WK-C-NEW-ACCNO TO WK-C-VCCA-CA-NO
ID1VKE             MOVE TFSSTPL-CUYCD TO WK-C-VCCA-CA-CUY
                       CALL "TRFVCCA" USING WK-C-VCCA-RECORD
                       IF WK-C-VCCA-ERROR-CD = SPACES
GP4C00                 IF WK-C-GPI4STPLSW = "Y"
GP4C00                     PERFORM D100-STPLIMIT-VALIDATION
GP4C00                         THRU D199-STPLIMIT-VALIDATION-EX
GP4C00                 END-IF
                           MOVE WK-C-VCCA-CUSTFNAM TO WK-VTD1-PAYRNAME
                           MOVE WK-C-VCCA-ADDR1  TO WK-VTD1-PAYRADR1
                           MOVE WK-C-VCCA-ADDR2  TO WK-VTD1-PAYRADR2
                           MOVE WK-C-VCCA-ADDR3  TO WK-VTD1-PAYRADR3
                           MOVE WK-C-VCCA-ADDR4  TO WK-VTD1-PAYRADR4
                           MOVE WK-C-VCCA-ADDR5  TO WK-VTD1-PAYRADR5
                           MOVE WK-C-VCCA-ADDR6  TO WK-VTD1-PAYRADR6
                           MOVE WK-C-VCCA-AOCD   TO WK-VTD1-AOCD
                           MOVE WK-N-VCCA-RESCD  TO WK-VTD1-RESCD
                           MOVE WK-N-VCCA-DOMBRCH TO WK-VTD1-DOMBRCH
                           MOVE WK-N-VCCA-HOLDCD1 TO WK-VTD1-HOLDCD1
                           MOVE WK-N-VCCA-HOLDCD2 TO WK-VTD1-HOLDCD2
                           MOVE WK-N-VCCA-HOLDCD3 TO WK-VTD1-HOLDCD3
                       END-IF
                   ELSE
                       MOVE "FCCA"         TO WS-PMODE
ID1VKE*            MOVE WS-ACCNO     TO WK-C-VCFA-FCCA
ID1VKE             MOVE WS-ACCNO     TO WK-C-NEW-ACCNO2
ID1VKE             MOVE WK-C-NEW-ACCNO TO WK-C-VCFA-FCCA
                       MOVE TFSSTPL-CUYCD  TO WK-C-VCFA-CUY
                       CALL "TRFVCFA"      USING WK-C-VCFA-RECORD
                       IF WK-C-VCFA-ERROR-CD = SPACES
GP4C00                 IF WK-C-GPI4STPLSW = "Y"
GP4C00                     PERFORM D100-STPLIMIT-VALIDATION
GP4C00                         THRU D199-STPLIMIT-VALIDATION-EX
GP4C00                 END-IF
                           MOVE WK-C-VCFA-CUSTFNAM TO WK-VTD1-PAYRNAME
                           MOVE WK-C-VCFA-ADDR1  TO WK-VTD1-PAYRADR1
                           MOVE WK-C-VCFA-ADDR2  TO WK-VTD1-PAYRADR2
                           MOVE WK-C-VCFA-ADDR3  TO WK-VTD1-PAYRADR3
                           MOVE WK-C-VCFA-ADDR4  TO WK-VTD1-PAYRADR4
                           MOVE WK-C-VCFA-ADDR5  TO WK-VTD1-PAYRADR5
                           MOVE WK-C-VCFA-ADDR6  TO WK-VTD1-PAYRADR6
                           MOVE WK-C-VCFA-AOCD   TO WK-VTD1-AOCD
                           MOVE WK-N-VCFA-RESCD  TO WK-VTD1-RESCD
                           MOVE WK-N-VCFA-DOMBRCH TO WK-VTD1-DOMBRCH
                           MOVE WK-N-VCFA-HOLDCD1 TO WK-VTD1-HOLDCD1
                           MOVE WK-N-VCFA-HOLDCD2 TO WK-VTD1-HOLDCD2
                           MOVE WK-N-VCFA-HOLDCD3 TO WK-VTD1-HOLDCD3
                       END-IF
                   END-IF
REM269****   IF (TFSSTPL-CUYCD       = TFSCLSYS-LCUYCD
REM269       IF (TFSSTPL-CUYCD       = WK-C-LCUYCD
                   AND WK-C-VCCA-ERROR-CD NOT = SPACES)
REM269****   OR (TFSSTPL-CUYCD       NOT = TFSCLSYS-LCUYCD
REM269       OR (TFSSTPL-CUYCD       NOT = WK-C-LCUYCD
                   AND WK-C-VCFA-ERROR-CD NOT = SPACES)
                       MOVE "N"            TO WS-OKAY, TAB-VL2(05)
5Q1JM1             INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1             IF WK-C-VCCA-ERROR-CD NOT = SPACES
5Q1JM1                 MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
5Q1JM1                 MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCDE
5Q1JM1                 MOVE WK-N-VCCA-STAFFIND TO WK-N-STAFFIND
5Q1JM1                 MOVE WK-C-VCCA-CUSTFNAM TO WK-C-ACCNAME
5Q1JM1             ELSE
5Q1JM1                 MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
5Q1JM1                 MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCDE
5Q1JM1                 MOVE WK-N-VCFA-STAFFIND TO WK-N-STAFFIND
5Q1JM1                 MOVE WK-C-VCFA-CUSTFNAM TO WK-C-ACCNAME
5Q1JM1             END-IF
5Q1JM1             PERFORM E002-PROCESS-RPRRSN
5Q1JM1                 THRU E002-PROCESS-RPRRSN-EX
                   END-IF
               ELSE
                   MOVE "N"                TO WS-OKAY, TAB-VL2(05)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "RSN0091"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
               END-IF
SM1TY1     ELSE
SM1TY1         MOVE "N" TO WS-OKAY
SM1TY1         MOVE "X" TO TAB-VL2(05)
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "RSN0035"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
SM1TY1     END-IF
               PERFORM D300-LOGGING THRU D399-LOGGING-EX
           END-IF.

           IF TAB-VAL(06) NOT = "X" AND WS-OKAY = "Y"
               MOVE TAB-VAL(06)            TO TAB-VL2(06)
REM269****   IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
REM269       IF TFSSTPL-CUYCD = WK-C-LCUYCD
                   IF WK-C-VCCA-ERROR-CD   = SPACES
                       MOVE "Y"            TO WS-ACT2
                   ELSE
                       MOVE "Y"            TO TAB-VL2(06)
                       MOVE "N"            TO WS-OKAY
5Q1JM1             INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1             MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCDE
5Q1JM1             MOVE WK-N-VCCA-STAFFIND TO WK-N-STAFFIND
5Q1JM1             MOVE WK-C-VCCA-CUSTFNAM TO WK-C-ACCNAME
5Q1JM1             MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
5Q1JM1             PERFORM E002-PROCESS-RPRRSN
5Q1JM1                 THRU E002-PROCESS-RPRRSN-EX
                   END-IF
               ELSE
                   IF WK-C-VCFA-ERROR-CD   = SPACES
                       MOVE "Y"            TO WS-ACT2
                   ELSE
                       MOVE "Y"            TO TAB-VL2(06)
                       MOVE "N"            TO WS-OKAY
5Q1JM1             INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1             MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCDE
5Q1JM1             MOVE WK-N-VCFA-STAFFIND TO WK-N-STAFFIND
5Q1JM1             MOVE WK-C-VCFA-CUSTFNAM TO WK-C-ACCNAME
5Q1JM1             MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
5Q1JM1             PERFORM E002-PROCESS-RPRRSN
5Q1JM1                 THRU E002-PROCESS-RPRRSN-EX
                   END-IF
               END-IF
               PERFORM D300-LOGGING THRU D399-LOGGING-EX
           END-IF.

           IF TAB-VAL(07) NOT = "X" AND WS-OKAY = "Y"
               MOVE TAB-VAL(07)            TO TAB-VL2(07)
               MOVE "MAS"                  TO WS-PMODE
               PERFORM D300-LOGGING THRU D399-LOGGING-EX
GP4A05*GP4A00 IF TFSSTPL-IMSGTYPE    = "M"
GP4A05*GP4A00 AND SW-RTGS-BYPASS-F53-F54-N
GP4A05*GP4A00 AND WS-C-SWF-RTGS-SW = "Y"
GP4A05*GP4A04 AND WK-C-GPI4-GL-SW = "Y"
GP4A05     IF (TFSSTPL-IMSGTYPE      = "M"
GP4A05     AND SW-RTGS-BYPASS-F53-F54-N
GP4A05     AND WS-C-SWF-RTGS-SW      = "Y"
GP4A05     AND WK-C-GPI4-GL-SW       = "Y")
GP4A05     OR WK-C-MAS-RTGS-SW       = "Y"
GP4A00         PERFORM D600-RTGS-VALIDATION
GP4A00             THRU D699-RTGS-VALIDATION-EX
GP4A00     END-IF
           END-IF.

GP4C00     IF WK-C-GPI4STPLSW        = "Y"
GP4C00     AND (WS-ACCTYP = "C" OR "F")
GP4C00         PERFORM D100-STPLIMIT-VALIDATION
GP4C00             THRU D199-STPLIMIT-VALIDATION-EX
GP4C00     END-IF

6Q3LN1     IF TFSSTPL-IMSGTYPE = "M"
6Q3LN1     AND SW-RTGS-BYPASS-F53-F54-Y
6Q3LN1         MOVE "RTGS"           TO WS-PMODE
6Q3LN1         PERFORM D300-LOGGING THRU D399-LOGGING-EX
GP4A00         GO TO D199-VALIDATION-EX
6Q3LN1     END-IF.

       D199-VALIDATION-EX.
           EXIT.
       EJECT

GP4C00 D100-STPLIMIT-VALIDATION.
GP4C00
GP4C00     MOVE "A1"                 TO WS-LINK-STATUS.
GP4C00
GP4C00     IF WS-ACCNO NOT           = SPACES
GP4C00         INITIALIZE WK-C-RPRRSN-AREA
GP4C00         INITIALIZE WK-C-LINK-LIMIT
GP4C00
GP4C00         MOVE TFSSTPL-BNKENTTY TO WS-LINK-BNKENTTY
GP4C00         MOVE WS-ACCNO         TO WS-LINK-ACCNO
GP4C00         MOVE TFSSTPL-CUYCD    TO WS-LINK-CCY
GP4C00         MOVE TFSSTPL-AMT      TO WS-LINK-AMT
GP4C00         MOVE "I"              TO WS-LINK-REMIND
GP4C00         CALL "TRFVLMT2" USING WK-C-LINK-LIMIT
GP4C00
GP4C00         EVALUATE WS-LINK-STATUS
GP4C00             WHEN "XX"
GP4C00                 MOVE "N"      TO WS-OKAY
GP4C00* ------> RSN0311:STP LIMIT CHECKING HAS BEEN FAILED
GP4C00                 MOVE "RSN0311" TO WK-C-RPRCODE
GP4C00                 PERFORM E002-PROCESS-RPRRSN
GP4C00                     THRU E002-PROCESS-RPRRSN-EX
GP4C00             WHEN "AA"
GP4C00                 MOVE "N"      TO WS-OKAY
GP4C00* ------> RSN0312:TRANS. AMOUNT IS > ACCNT. STP LIMIT
GP4C00                 MOVE "RSN0312" TO WK-C-RPRCODE
GP4C00                 PERFORM E002-PROCESS-RPRRSN
GP4C00                     THRU E002-PROCESS-RPRRSN-EX
GP4C00             WHEN "AC"
GP4C00                 MOVE "N"      TO WS-OKAY
GP4C00* ------> RSN0313:TRANS. AMOUNT IS > CIF STP LIMIT
GP4C00                 MOVE "RSN0313" TO WK-C-RPRCODE
GP4C00                 PERFORM E002-PROCESS-RPRRSN
GP4C00                     THRU E002-PROCESS-RPRRSN-EX
GP4C00             WHEN "AS"
GP4C00                 MOVE "N"      TO WS-OKAY
GP4C00* ------> RSN0314:TRANS. AMOUNT IS > SEGMENT STP LIMIT
GP4C00                 MOVE "RSN0314" TO WK-C-RPRCODE
GP4C00                 PERFORM E002-PROCESS-RPRRSN
GP4C00                     THRU E002-PROCESS-RPRRSN-EX
GP4C00         END-EVALUATE
GP4C00     END-IF.
GP4C00
GP4C00 D199-STPLIMIT-VALIDATION-EX.
GP4C00     EXIT.

       D200-VALIDATION.
           MOVE WS-BANKID                  TO WK-VTD1-BANKID.
           MOVE WS-RECBNKID                TO WK-VTD1-RECBNKID.
           MOVE WS-SNDCBNKID               TO WK-VTD1-SNDCBNKID.
           MOVE WS-ACCNO                   TO WK-VTD1-BANKAC.
           MOVE WS-ACCTYP                  TO WK-VTD1-BANKACTYP.
           MOVE WS-ACUDBUI                 TO WK-VTD1-ACUDBUI.
           MOVE WS-PMODE                   TO WK-VTD1-PMODE.
           MOVE TABLE-ARR2                 TO WK-VTD1-DATAD1.
CMP3A1 IF TFSSTPL-SWFTMGTY           = 101
CMP3A1 AND WS-C-M101STPIND           = "Y"
CMP3A1     MOVE WK-C-CUPF-ACCTYP     TO WK-VTD1-BANKACTYP
CMP3A1     MOVE "D"                  TO WK-VTD1-ACUDBUI
CMP3A1     MOVE WS-T50-ACCNO         TO WK-VTD1-BANKAC
CMP3A1     MOVE SPACES               TO WK-VTD1-BANKID
CMP3A1 END-IF.

           IF WS-OKAY                      = "Y"
               MOVE WS-ACT1                TO WK-VTD1-ACT1
               MOVE WS-ACT2                TO WK-VTD1-ACT2
               MOVE "N"                    TO WK-VTD1-ERROR-FOUND
           ELSE
               MOVE SPACES                 TO WK-VTD1-ACT1
                                              WK-VTD1-ACT2
               MOVE "Y"                    TO WK-VTD1-ERROR-FOUND
           END-IF
           MOVE "N"                        TO WS-FLAG1.
           PERFORM D300-LOGGING            THRU D399-LOGGING-EX.

       D299-VALIDATION-EX.
           EXIT.
       EJECT

       D300-LOGGING.
           MOVE WK-VTD1-PARALNO            TO WK-LOGG-PARALNO.
           MOVE WK-VTD1-SEQNUM             TO WK-LOGG-SEQNUM.
           MOVE TABLE-ARR2                 TO WK-LOGG-DATAD1.
           MOVE "D1"                       TO WK-LOGG-TABTYP.
           MOVE WK-VTD1-ACT                TO WK-LOGG-ACTD1.
           CALL "TRFLOGGCL"                USING WK-LOGG
                                                 WS-FLAG1
                                                 WS-FLAG2.
           IF WK-LOGG-ERROR-FOUND          = "Y"
               GO TO D399-LOGGING-EX
           END-IF.

       D399-LOGGING-EX.
           EXIT.
       EJECT

       D400-SHIFT-VALIDATION.
           IF TFSSTPL-SENBNKID NOT = SPACES
               MOVE TFSSTPL-SENBNKID       TO WK-C-VBBAS-BANKID
SM1YTW     CALL "TRFVBBASM"          USING WK-C-VBBAS-RECORD
               IF WK-C-VBBAS-SHIFTNO NOT = SPACES
                   MOVE "Y"                TO SHIFT-IND
                   MOVE TFSSTPL-BNKENTTY   TO TFSBNKET-BNKENTTY
                   READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
                   INVALID KEY
                       MOVE "N"            TO SHIFT-IND
5Q1JM1             INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1             MOVE "RSN0089"    TO WK-C-RPRCODE
5Q1JM1             PERFORM E002-PROCESS-RPRRSN
5Q1JM1                 THRU E002-PROCESS-RPRRSN-EX
                   NOT INVALID KEY
                       MOVE TFSBNKET-MASNOSTR TO WS-ACCNO
                   END-READ
               ELSE
                   MOVE "N"                TO SHIFT-IND
5Q1JM1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1         MOVE "RSN0089"        TO WK-C-RPRCODE
5Q1JM1         PERFORM E002-PROCESS-RPRRSN
5Q1JM1             THRU E002-PROCESS-RPRRSN-EX
               END-IF
           END-IF.

       D499-SHIFT-VALIDATION-EX.
           EXIT.
       EJECT

6Q3LN1 D500-RTGS-VALIDATION.
6Q3LN1*    IF TFSSTPL-SENBNKID NOT = SPACES
6Q3LN1*        MOVE TFSSTPL-BNKENTTY TO TFSBNKET-BNKENTTY
6Q3LN1*        READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
6Q3LN1*        INVALID KEY
6Q3LN1*            MOVE "N"          TO SHIFT-IND
6Q3LN1*            INITIALIZE WK-C-RPRRSN-AREA
6Q3LN1*            MOVE "RSN0087"    TO WK-C-RPRCODE
6Q3LN1*            PERFORM E002-PROCESS-RPRRSN
6Q3LN1*                THRU E002-PROCESS-RPRRSN-EX
6Q3LN1*        NOT INVALID KEY
6Q3LN1*            MOVE TFSBNKET-MASNOSTR TO WS-ACCNO
6Q3LN1*        END-READ
6Q3LN1*    END-IF.
6Q3LN1
6Q3LN1     MOVE TFSSTPL-BNKENTTY     TO WK-N-VBAC-BNKENTTY.
6Q3LN1     MOVE TFSSTPL-SENBNKID     TO WK-C-VBAC-BANKID.
6Q3LN1     MOVE TFSSTPL-CUYCD        TO WK-C-VBAC-CUYCD.
6Q3LN1     CALL "TRFVBAC"            USING WK-C-VBAC-RECORD.
6Q3LN1     IF WK-C-VBAC-ERROR-CD = SPACES
6Q3LN1         MOVE WK-C-VBAC-ACUDBUI TO WS-ACUDBUI
6Q3LN1         MOVE WK-C-VBAC-ACCTYP TO WS-ACCTYP
6Q3LN1         IF WK-C-VBAC-BNKACNO NOT = SPACES AND
6Q3LN1            WK-C-VBAC-BNKACNO(WK-N-ACCLEN:1) = SPACES
6Q3LN1             MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
6Q3LN1         ELSE
6Q3LN1             MOVE WK-C-VBAC-BNKACNO(4:) TO WS-ACCNO
6Q3LN1         END-IF
6Q3LN1     ELSE
6Q3LN1         MOVE "N"              TO WS-OKAY
6Q3LN1                                  WK-RTGS-BNKAC-EXIST
6Q3LN1         INITIALIZE WK-C-RPRRSN-AREA
6Q3LN1         MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
6Q3LN1         PERFORM E002-PROCESS-RPRRSN
6Q3LN1             THRU E002-PROCESS-RPRRSN-EX
6Q3LN1     END-IF.
6Q3LN1
6Q3LN1 D599-RTGS-VALIDATION-EX.
6Q3LN1     EXIT.
6Q3LN1 EJECT

GP4A00 D600-RTGS-VALIDATION.
GP4A00*Retrieve RTGS Mode pay.
GP4A00     INITIALIZE WK-C-VDRTGS-RECORD.
GP4A00     MOVE TFSSTPL-PROCUNIT     TO WK-C-VDRTGS-PU.
GP4A00     MOVE TFSSTPL-CUYCD        TO WK-C-VDRTGS-CUY.
GP4A00     IF TFSSTPL-AMT IS NUMERIC
GP4A00         MOVE TFSSTPL-AMT      TO WK-C-VDRTGS-AMT
GP4A00     ELSE
GP4A00         MOVE ZERO             TO WK-C-VDRTGS-AMT
GP4A00     END-IF.
GP4A00
GP4A00     CALL "TRFVDRTGS"          USING WK-C-VDRTGS-RECORD.
GP4A00
GP4A00     IF WK-C-VDRTGS-RTGSCUYIND = "Y"
GP4A00         MOVE WK-C-VDRTGS-RTGSTYPE TO WS-PMODE
GP4A00*--------> Retrieve corresponding RTGS GL No.
GP4A00         INITIALIZE UFMGLPAYR OF UFMGLPAY-REC
GP4A00         MOVE ZEROES           TO WK-N-MAS
GP4A00         MOVE SPACES           TO WS-ACCNO
GP4A00         MOVE WS-PMODE         TO UFMGLPAY-PAYMODE
GP4A00         READ UFMGLPAY KEY IS EXTERNALLY-DESCRIBED-KEY
GP4A00         IF WK-C-SUCCESSFUL
GP4A00             MOVE UFMGLPAY-GLNO6 TO WK-N-MAS
GP4A00             MOVE WK-N-MAS     TO WS-ACCNO
GP4A04             MOVE "N"          TO WS-ACCTYP
GP4A00             PERFORM D300-LOGGING THRU D399-LOGGING-EX
GP4A00         ELSE
GP4A00             MOVE "N"          TO WS-OKAY
GP4A00*----------> RSN0376: Invalid RTGS GL Account.
GP4A00             INITIALIZE WK-C-RPRRSN-AREA
GP4A00             MOVE "RSN0376"    TO WK-C-RPRCODE
GP4A00             PERFORM E002-PROCESS-RPRRSN
GP4A00                 THRU E002-PROCESS-RPRRSN-EX
GP4A00         END-IF
GP4A00     END-IF.

GP4A00 D699-RTGS-VALIDATION-EX.
GP4A00     EXIT.

T55YTW E001-GET-TAG55.
T55YTW*** get the option of incoming msg Tag 55 by calling common
T55YTW*** routine TRFGTAG.
T55YTW
T55YTW     MOVE SPACES               TO TAG55-OPT.
T55YTW
T55YTW     INITIALIZE WK-C-GTAG-RECORD.
T55YTW     MOVE WK-VTD1-PARALNO      TO WK-N-GTAG-QUENUM.
T55YTW     MOVE WK-VTD1-SEQNUM       TO WK-N-GTAG-QUESUF.
T55YTW     MOVE "55T"                TO WK-C-GTAG-TAGNO.
T55YTW     CALL "TRFGTAG"            USING WK-C-GTAG-RECORD.
T55YTW     MOVE WK-C-GTAG-OUT1(1:1)  TO TAG55-OPT.
T55YTW
T55YTW E001-GET-TAG55-EX.
T55YTW     EXIT.

5Q1JM1 E002-PROCESS-RPRRSN SECTION.
5Q1JM1 E002-ENTRY.
5Q1JM1
5Q1JM1     MOVE WK-VTD1-PARALNO      TO WK-C-RRSN-QUENUM.
5Q1JM1     MOVE WK-VTD1-SEQNUM       TO WK-C-RRSN-QUESUF.
5Q1JM1     MOVE WK-C-TRNNO           TO WK-C-RRSN-TRNNO.
5Q1JM1     MOVE WK-C-FUNCTID         TO WK-C-RRSN-FUNCTID.
5Q1JM1     MOVE WK-C-SEGCDE          TO WK-C-RRSN-SEGCDE.
5Q1JM1     MOVE SPACES               TO WK-C-RRSN-SEGDESC.
5Q1JM1     MOVE WK-N-STAFFIND        TO WK-C-RRSN-STAFFIND.
5Q1JM1     MOVE WS-ACCNO             TO WK-C-RRSN-ACCNO.
CMP3A1     IF TFSSTPL-SWFTMGTY = 101
CMP3A1     AND WS-C-M101STPIND = "Y"
CMP3A1         MOVE WS-T50-ACCNO     TO WK-C-RRSN-ACCNO
CMP3A1     END-IF.
5Q1JM1     MOVE WK-C-ACCNAME         TO WK-C-RRSN-ACCNAME.
5Q1JM1     MOVE WK-C-QRATE           TO WK-C-RRSN-QRATE.
5Q1JM1     MOVE WK-N-SYSDTE          TO WK-C-RRSN-RPRDTE.
5Q1JM1*    MOVE WK-C-RPRCODE         TO WK-C-RRSN-RSNCDE.
5Q1JM1     IF WK-C-RPRCODE = SPACE
5Q1JM1         MOVE "RSN9999"        TO WK-C-RRSN-RSNCDE
5Q1JM1     ELSE
5Q1JM1         MOVE WK-C-RPRCODE     TO WK-C-RRSN-RSNCDE
5Q1JM1     END-IF.
5Q1JM1
5Q1JM1     MOVE SPACES               TO WK-C-RRSN-RSNDESC.
5Q1JM1     MOVE WK-C-RPRPGM          TO WK-C-RRSN-RPRPGM.
5Q1JM1     CALL "TRFGRRSN"           USING WK-C-RRSN-RECORD.
5Q1JM1
5Q1JM1 E002-PROCESS-RPRRSN-EX.
5Q1JM1     EXIT.

G2BM00 E003-ACC-MANDATE-CHECK.
G2BM00     INITIALIZE                WK-ACMN.
G2BM00     MOVE TFSSTPL-BNKENTTY     TO WK-ACMN-BANKCD.
G2BM00     MOVE TFSSTPL-CUYCD        TO WK-ACMN-CUYCD.
GP3A02*G2BM00 MOVE WS-ACCNO          TO WK-ACMN-ACCNO.
G2BM00     MOVE TFSSTPL-SENBNKID     TO WK-ACMN-BANKID.
GP3A02     IF WS-ACCNO NOT = SPACES AND
GP3A02        WS-ACCNO(WK-N-ACCLEN:1) = SPACES
G2BM00         MOVE WS-ACCNO         TO WK-ACMN-ACCNO
GP3A02     ELSE
GP3A02         MOVE WS-ACCNO(4:)
GP3A02                                TO WK-ACMN-ACCNO
GP3A02     END-IF.
G2BM00
G2BM00     CALL "TRFACMN" USING WK-ACMN.
G2BM00     IF WK-ACMN-MANIND         = "N"
G2BM00         MOVE "N"              TO WS-OKAY
G2BM00                                  TAB-VL2(02)
G2BM00         INITIALIZE            WK-C-RPRRSN-AREA
G2BM00         MOVE "RSN0097"        TO WK-C-RPRCODE
G2BM00         PERFORM E002-PROCESS-RPRRSN
G2BM00             THRU E002-PROCESS-RPRRSN-EX
G2BM00     END-IF.
G2BM00
G2BM00 E003-ACC-MANDATE-CHECK-EX.
G2BM00     EXIT.

CMP3A1 R001-READ-UFIMIJ.
CMP3A1     INITIALIZE WS-T50-ACCNO
CMP3A1                UFIMIJ-REC-1.
CMP3A1     MOVE WK-VTD1-PARALNO      TO UFIMIJ-PARALNO.
CMP3A1     MOVE WK-VTD1-SEQNUM       TO UFIMIJ-SEQNUM.
CMP3A1
CMP3A1     READ UFIMIJ KEY IS EXTERNALLY-DESCRIBED-KEY
CMP3A1         INVALID KEY
CMP3A1             GO TO R001-READ-UFIMIJ-EX
CMP3A1         NOT INVALID KEY
CMP3A1             MOVE UFIMIJ-IMSGBDY TO WK-C-MSGBDY1
CMP3A1     END-READ.
CMP3A1
CMP3A1     IF INF-101-S1-ORD-CUST1(1:1) = "/"
CMP3A1         MOVE INF-101-S1-ORD-CUST1(2:) TO WS-T50-ACCNO
CMP3A1     ELSE
CMP3A1         MOVE INF-101-S1-ORD-CUST1 TO WS-T50-ACCNO
CMP3A1     END-IF.
CMP3A1
CMP3A1 R001-READ-UFIMIJ-EX.
CMP3A1     EXIT.

       Z000-END-PROGRAM.
           CLOSE TFSSTPL
                 TFSBNKET
                 TFSCLSYS
202VKE     TFS202V.
CMP3A1     CLOSE UFIMIJ.
GP4A00     CLOSE UFMGLPAY.
           EXIT PROGRAM.
      *----------------------------------------------------------------*
      * END OF TRFVTD1 PROGRAM
      *----------------------------------------------------------------*
