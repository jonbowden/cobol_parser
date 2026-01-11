       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRFVTD1.
       AUTHOR. TYK.
       DATE-WRITTEN. JUN 04.
      *DESCRIPTION : TABLE D1 VALIDATION.
      *   SUBROUTINE - DEBIT PARTY CHECKING FIELD 53/54 AND
      *   SENDING BANK FOR INCOMING SWIFT FCY/RTGS
      *   THIS ROUTINE WILL INITIATE TRFVTD2 PGM
      *=================================================================
      * HISTORY OF MODIFICATION:
      *=================================================================
      * P14B00 - ACNRJR - 09/11/2020 - CASH MANAGEMENT ROAD MAP        *
      *   - P14 - Upgrade GLMS CR5 (Str2)                              *
      *   - PCRMAPDLMC-506                                            *
      *   - Recompiled due to chng in cpybk                           *
      *----------------------------------------------------------------*
      * GP4A05 - VENADG - 23/10/2020 - CASH MANAGEMENT ROAD MAP - P19 *
      *   - GPI Day4 (POST IMPEM IMPROVEMENT)                         *
      *     STP #1 (HK req) Inward SWIFT & RTGS                       *
      *     JIRA PCRMAPKGPI-2395                                      *
      *   - BAU Bugfix                                                *
      *   - Rectified hardcoded "MAS"                                 *
      *     when crediting to RTGS                                    *
      *     Utilize TRFVDTRGS utility module                          *
      *     to retrieve corresponding modepay.                        *
      *----------------------------------------------------------------*
      * GP4A04 - VENDUS - 07/09/2020 - CASH MANAGEMENT ROAD MAP - P19 *
      *   - PCRMAPKGPI-2246                                           *
      *   - Move "N" to Account Type for                              *
      *     GL accounts.                                              *
      *   - Use GPI4GLENH SW for this Enh.                            *
      *----------------------------------------------------------------*
      * GP3A03 - VENDUS - 07/07/2020 - CASH MANAGEMENT ROAD MAP - P19 *
      *   - SIT JIRA: PCRMAPKGPI-1757                                 *
      *   - Previously when Tag54B has an                             *
      *     account and name, the name is being                       *
      *     moved to bankid.                                          *
      *   - Fix to move sending bank id to                            *
      *     bannkid instead of name                                   *
      *   - Previously when Tag54D 10 byte                            *
      *     account is being used to call                             *
      *     NSTP module.                                              *
      *   - Fix is to use the correct 13 byte                         *
      *     account when calling NSTP program.                        *
      *----------------------------------------------------------------*
      * GP3A02 - VENDUS - 02/07/2020 - CASH MANAGEMENT ROAD MAP - P19 *
      *   - SIT JIRA: PCRMAPKGPI-1736                                 *
      *   - Previously when Tag53B has an                             *
      *     account and BIC, the account was                          *
      *     used directly to check for acc                            *
      *     mandate which has excess 3 bytes.                         *
      *   - Fix to remove the first 3 bytes                           *
      *     before calling mandate table                              *

      *---------------------------------------------------------------*
      * GP4A00 - VENADG  - 16/03/2020 - CASH MANAGEMENT ROAD MAP - P19
      * - GPI Day4 (In-Country Req)
      *   STP #1 (HK req) Inward SWIFT & RTGS
      * - Previously system is assigning
      *   "MAS" DR Modepay for Inward RTGS
      *   txn and retrieving corresponding GL
      *   from TFSBNKET (MASNOSTR).
      * - Rectified to utilize utility
      *   TRFVDORTGS to retrieve RTGS modepay
      *   and read UFMGLPAY retrieve
      *   corresponding GLNO.
      *   (Copied from GH2ABA from TREEEDT)
      *---------------------------------------------------------------
      * GP4C00 - VENJ08  - 12/02/2020 - CASH MANAGEMENT ROAD MAP - P19
      * - GPI Day4 (In-Country Req)
      * - STP #4: Ability to setup DR Limit
      *   on STP Limit by ACC/CIF/SEG
      * - VALIDATE DR VOSTRO STP LIMIT
      *---------------------------------------------------------------
      * GP3A01 - VENADG  - 17/01/2020 - BAU Bugfix
      * - PT JIRA: PCRMAPKGPI-1238
      * - Previously when Tag53/54
      *   is a BIC and is defined w/
      *   account (1st Line), System
      *   is not able to pass the correct
      *   ACCNO prior to calling TRFVBANO.
      * - Rectified the existing BAU BUG
      *   to evaluate first if ACCNO retrieved
      *   from Bank Account Table has Routing
      *   Code (e.g 103) already or not to
      *   avoid incorrectly appending
      *---------------------------------------------------------------
      * G2BM00 - VENADG  - 11/08/2019 - CASH MANAGEMENT ROAD MAP - P19
      * - GPI Day4 (Retro from GPI Day2b HO)
      * - Enable STP processing for
      *   TAG53/54 option B/D when
      *   TAG32 is not local currency
      * - Removed dependency on Day2b Main
      *   technical switch. Introduced new
      *   switch for IAFT FCY STP enh instead.
      * G2BM01 - VENADG  - 22/11/2019 - GPI Day4 (Retro from GPI Day2b
      -    HO)
      * - POST IMPLEM CR
      * - To allow Non-STP check for
      *   FCY txn. Check will be done
      *   prior to Account Mandate Chk
      *---------------------------------------------------------------
      * CMR4J1 - TMPJC5  - 11/06/2018 - CASH MANAGEMENT PROJECT4
      * - JIRA # PCRMAPUPAY-G11 from HO
      * - To modify the sequence of the blacklist
      *   table program routine validation,
      *   in order to pass the correct repair
      *   reason code.
      *---------------------------------------------------------------
      * REM269 - TMPSRK  - 07/04/2017 - JIRA LOG REM-269

      * - STANDARDIZATION OF PROGRAM TO
      *   RETRIEVE CURRENCY AND COUNTRY
      *   CODE FROM SYSTEM PARAMETER FILE.
      *----------------------------------------------------------------*
      * CMP3A2 - CMPFEN - 21/03/2017 - CASH MANAGEMENT PROJECT REL. 3
      *   FIX ACCT NUMBER LENGTH
      *----------------------------------------------------------------*
      * CMP3A1 - CMPFEN - 14/02/2017 - CASH MANAGEMENT PROJECT REL. 3
      *   ALLOW MT101 STP PROCESSING
      *----------------------------------------------------------------*
      * 7Q1EM1 - TMPEYM - 25/11 /2016 - REM Q1 2017 RELEASE
      *   - e-Req 47511 Refinement of
      *     Duplicate checking for Inw
      *   - Recompiled due to changes made in
      *     VSTPL copy book.
      *----------------------------------------------------------------*
      * 6Q3LN1 - UNCCNL - 09/05/2016 - REM 2016 Q3 RELEASE
      *   - E-REQUEST# 46441
      *   - INCOMING BAHTNET STP
      *     To bypass FS3 & FS4 validation
      *     for RTGS message.
      *----------------------------------------------------------------*
      * STPGB1 - TMPGCB - 29/09/2015 - UOBM OTT STP PROJECT
      *   RECOMPILED THE PROGRAM DUE TO
      *   THE CHANGES ON NSTP COPYBOOK.
      *----------------------------------------------------------------*
      * 5Q1JM1 - TMPJZM - 23/12/2014 - 14HOREM024/14HOREM029/14HOREM028
      *   Retrofit NON PSTP Reason
      *   Enhancement Project
      *----------------------------------------------------------------*
      * ID1VK1 - KESAVAN - 24/04/2012 - REM UOBI PROJECT
      *   Include Routing number in
      *   Account Number.
      *----------------------------------------------------------------*
      * ID1VKE - KESAVAN - 24/04/2012 - REM UOBI PROJECT .
      *   This program copied from SG
      *   based on regional logic done
      *   some changes.
      *----------------------------------------------------------------*
      * 202VKE - KESAVAN - 09/06/2009 - If TAG54/53 BANKID is found
      *     in TFS202V file then rout to
      *     repair.
      *
      * CURRENT PATH :
      * - IF TAG 54A, THEN VALIDATE 54A BIC AGAINST TFS202V TBL
      * - IF TAG 54 NOT EXIST, AND 53A EXIST, VALIDATE 53A BIC AGAINST
      *----------------------------------------------------------------*
      * T5SYTW - TECKWAI - 15/08/2008 - For incoming MT103 that has
      *   non-empty Tag 55, rout to
      *   repair.
      *----------------------------------------------------------------*
      * SM1YW - TECKWAI - 02/05/2007 - To call new routine TRFVBBASM
      -    WAMT
      *   for validation of RTGS bank
      *   instead of TRFVBBAS
      *----------------------------------------------------------------*

      * SM1TY1 - TMPTY1 - 22/11/2005 - SKIP ACC VERIFY WHEN AMT >
      -    IRSWAMT
      *
       ENVIRONMENT DIVISION.
      ********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. LOCAL-DATA IS LOCAL-DATA-AREA
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
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TFSCLSYS ASSIGN TO DATABASE-TFSCLSYS
              ORGANIZATION IS SEQUENTIAL
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TFS202V ASSIGN TO DATABASE-TFS202V
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT UFIM1J ASSIGN TO DATABASE-UFIM1J
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT UFMLGPLAY ASSIGN TO DATABASE-UFMLGPLAY
              ORGANIZATION IS INDEXED

       DATA DIVISION.
       FILE SECTION.
      ***************
       FD TFSSTPL
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFSSTPL.
       01  WK-C-TFSSTPL.
              COPY DDS-ALL-FORMATS OF TFSSTPL.
       01  WK-C-TFSSTPL-1.
              COPY TFSSTPL.

       FD TFSCLSYS
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSCLSYS-REC.
       01  TFSCLSYS-REC.
              COPY DDS-ALL-FORMATS OF TFSCLSYS.
       01  TFSCLSYS-REC-1.
              COPY TFSCLSYS.

       FD TFSBNKET
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSBNKET-REC.
       01  TFSBNKET-REC.
              COPY DDS-ALL-FORMATS OF TFSBNKET.
       01  TFSBNKET-REC-1.
              COPY TFSBNKET.

       FD TFS202V
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFS202V.
       01  WK-C-TFS202V.
              COPY DDS-ALL-FORMATS OF TFS202V.
       01  WK-C-TFS202V-1.
              COPY TFS202V.

       FD UFIMIJ
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS UFIMIJ-REC.
       01  UFIMIJ-REC.
              COPY DDS-ALL-FORMATS OF UFIMIJ.
       01  UFIMIJ-REC-1.
              COPY UFIMIJ.

       FD UFMGLPAY
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS UFMGLPAY-REC.
       01  UFMGLPAY-REC.
              COPY DDS-ALL-FORMATS OF UFMGLPAY.
       01  UFMGLPAY-REC-1.
              COPY UFMGLPAY.

       WORKING-STORAGE SECTION.
      ***********************
       01 WK-C-COMMON.
              COPY ASCWMS.

       01 TAG53-FORMAT.
           05 TAG53-LINE-1.
           07 TAG53-FIL1       PIC X(2).
           07 TAG53-OPT        PIC X(1).
           07 TAG53-FIL2       PIC X(1).
           07 TAG53-PTID.
           09 TAG53-PTID-1 PIC X(02).
           09 TAG53-PTID-2 PIC X(35).
           05 TAG53-LINE-2         PIC X(35).
           05 TAG53-BIC REDEFINES TAG53-LINE-2.
           07 TAG53-BIC-SUB1   PIC X(4).
           07 TAG53-BIC-SUB2   PIC X(2).
           07 TAG53-BIC-SUB3   PIC X(2).
           07 TAG53-BIC-SUB4   PIC X(3).
           07 TAG53-BIC-FILLER PIC X(24).
           05 TAG53-LOC REDEFINES TAG53-LINE-2
              PIC X(35).
           05 TAG53-NAME REDEFINES TAG53-LINE-2
              PIC X(35).
           05 TAG53-LINE-3         PIC X(35).
           05 TAG53-LINE-4         PIC X(35).
           05 TAG53-LINE-5         PIC X(35).

       01 TAG54-FORMAT.
           05 TAG54-LINE-1.
           07 TAG54-FIL1       PIC X(2).
           07 TAG54-OPT        PIC X(1).
           07 TAG54-FIL2       PIC X(1).
           07 TAG54-PTID.
           09 TAG54-PTID-1 PIC X(02).
           09 TAG54-PTID-2 PIC X(35).
           05 TAG54-LINE-2         PIC X(35).
           05 TAG54-BIC REDEFINES TAG54-LINE-2.
           07 TAG54-BIC-SUB1   PIC X(4).
           07 TAG54-BIC-SUB2   PIC X(2).
           07 TAG54-BIC-SUB3   PIC X(2).
           07 TAG54-BIC-SUB4   PIC X(3).
           07 TAG54-BIC-FILLER PIC X(24).
           05 TAG54-LOC REDEFINES TAG54-LINE-2
              PIC X(35).
           05 TAG54-NAME REDEFINES TAG54-LINE-2
              PIC X(35).
           05 TAG54-LINE-3         PIC X(35).
           05 TAG54-LINE-4         PIC X(35).
           05 TAG54-LINE-5         PIC X(35).

       01 TAG55-OPT                PIC X(1).

       01 TABLE-ARRAY.
           05 TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".

       01 TABLE-ARR2.
           05 TAB-VL2 OCCURS 20 TIMES PIC X VALUE "X".

       01 PATH-P1           PIC X(20)
              VALUE "YXXXXXXXXXXXXXXXXXXX".
       01 PATH-P2           PIC X(20)
              VALUE "XYXXXXXXXXXXXXXXXOXX".
       01 PATH-P3           PIC X(20)
              VALUE "YXXXXXXXXXXXXXXXOXXX".
       01 PATH-P4           PIC X(20)
              VALUE "XYXXXXXXXXXXXXOXXXXX".
       01 PATH-P5           PIC X(20)
              VALUE "XYXXXXXXXXXXXOXXXXXX".
       01 PATH-P6           PIC X(20)
              VALUE "XYXXXXXXXXXXOXXXXXXX".
       01 PATH-P7           PIC X(20)
              VALUE "XXYNYNXXXXXXXXXXXXXX".

       01 WK-C-WORK-AREA.
           05 FIRST-TIME    PIC X(01) VALUE "Y".
           05 SHIFT-IND     PIC X(01) VALUE SPACE.
           05 WS-FLAG1      PIC X(01) VALUE SPACE.
           05 WS-FLAG2      PIC X(01) VALUE SPACE.
           05 WS-ACT1       PIC X(01) VALUE SPACE.
           05 WS-ACT2       PIC X(01) VALUE SPACE.
           05 WS-OKAY       PIC X(01) VALUE SPACE.
           05 WS-ACCNO      PIC X(11) VALUE SPACES.
           05 WS-ACCNO      PIC X(15) VALUE SPACES.
           05 WS-T50-ACCNO  PIC X(11) VALUE SPACE.
           05 WS-T50-ACCNO  PIC X(15) VALUE SPACE.
           05 WK-N-ACCTLEN  PIC S9(02) VALUE ZEROES.
           05 WS-BANKID     PIC X(11) VALUE SPACES.
           05 WS-RECBNKID   PIC X(11) VALUE SPACE.
           05 WS-SNDCBNKID  PIC X(11) VALUE SPACE.
           05 WS-SENCBNKID  PIC X(11) VALUE SPACE.
           05 WS-PMODE      PIC X(08) VALUE SPACES.
           05 WS-ACDUBU     PIC X(01) VALUE "D".
           05 WS-ACCTYP     PIC X(01) VALUE SPACE.

           05 WK-C-GPI4STPLSW  PIC X(01) VALUE SPACE.
       01 WK-C-LINK-LIMIT.
           05 WK-C-LINK-AREA-INPUT.
           10 WS-LINK-BNKENTY PIC X(02).
           10 WS-LINK-ACCNO   PIC X(15) VALUE 0.
           10 WS-LINK-CCY     PIC X(03) VALUE SPACES.
           10 WS-LINK-AMT     PIC S9(13)V99 VALUE 0.
           10 WS-LINK-REMIND  PIC X(01).
           05 WK-C-LINK-AREA-OUTPUT.
           10 WS-LINK-STATUS  PIC X(02) VALUE SPACES.

       01 WK-N-ACCLEN      PIC 9(02) VALUE ZEROES.

       01  WK-C-NEW-ACCNO.
           15  WK-C-NEW-ACCNO1           PIC X(03).
           15  WK-C-NEW-ACCNO2           PIC X(11).
       01  WK-C-RPPRSN-AREA.
           05  WK-C-SEGCODE              PIC X(01) VALUE SPACE.
           05  WK-N-STAFFIND             PIC S9(02) VALUE ZEROS.
           05  WK-C-QRATE                PIC X(02) VALUE SPACE.
           05  WK-C-RPRCODE              PIC X(07) VALUE SPACE.
           05  WK-C-TRNNO                PIC X(12) VALUE SPACE.
           05  WK-C-ACCNAME              PIC X(35) VALUE SPACE.
           05  WK-C-FUNCTID              PIC X(08) VALUE SPACE.
       01  WK-N-SYSDFID              PIC S9(08) VALUE ZEROS.
       01  WK-C-RPRPGM               PIC X(10) VALUE "TRFVTID1".
       01  L-STRPTGS                    PIC X(1) VALUE SPACES.
       01  WK-RTGS-BNKAC-EXIST          PIC X(01) VALUE SPACE.
       01  WS-C-M101STIPND              PIC X(01) VALUE "N".
       01  WK-C-LCUYCD                  PIC X(03).
       01  WK-GPI2B-VAR.
           05  WS-C-GPI-SW               PIC X(01) VALUE SPACE.
           05  WS-N-ACCLEN               PIC 9(02) VALUE ZEROS.
           05  WK-C-VALID-MTMSG          PIC X(01) VALUE SPACE.
           05  WK-C-ACNM-CHECK           PIC X(01) VALUE SPACE.
           05  WS-C-FCY-NSTP-SW          PIC X(01) VALUE SPACE.
       01  WS-C-SWF-RTGS-SW             PIC X(01) VALUE SPACE.
       01  WK-C-MAS-RTGS-SW             PIC X(01) VALUE "N".
       01  WK-N-MAS                     PIC S9(8) VALUE ZERO.
       01  WK-C-GPI4-GL-SW              PIC X(01) VALUE SPACE.
              COPY VCCA.
              COPY VCFA.
              COPY VSTPL.
              COPY VBAC.
              COPY VBBAS.
              COPY VBANO.
              COPY VTD2.
              COPY NSTP.
              COPY ACNM.
              COPY BLKB.
              COPY LOGG.
              COPY GTAG.
              COPY XGSPA.
              COPY RRSN.
              COPY IRTGSSWTC.
              COPY XPARA.
              COPY SWIFTMER.
              COPY CUPF.
              COPY GSDSI.
              COPY VDRTGS.
       LINKAGE SECTION.

              COPY VTD1.

       PROCEDURE DIVISION USING WK-VTD1.
      ****************************************
       MAIN-MODULE.
           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "RSYACCROUT" TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL TO WK-C-NEW-ACCNO1
           ELSE
              MOVE ZEROS TO WK-C-NEW-ACCNO1
       END-IF.

           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "RSYACCLEN" TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
           MOVE WK-C-XGSPA-GHPARVAL(1:2) TO WK-N-ACCLEN.

           INITIALIZE L-STPRTGS.
           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "RTGSTPFL" TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL TO L-STPRTGS
       END-IF.

           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "GPI4STPLSW" TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL TO WK-C-GPI4STPLSW
           ELSE
              MOVE "N" TO WK-C-GPI4STPLSW
       END-IF.

           INITIALIZE WK-RTGS-SWITCHES.
           IF L-STPRTGS = "Y"
              MOVE "IRTGSWTC" TO WK-C-XPARA-PARACD
              CALL "TRFXPARA" USING WK-C-XPARA-RECORD
              IF WK-C-XPARA-ERROR-CD = SPACES
                 MOVE WK-C-XPARA-PARAVALU(1:20) TO WK-RTGS-SWITCHES
              END-IF
       END-IF.

           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "M101STPIND" TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL TO WS-C-M101STPIND

           ELSE
              MOVE SPACES           TO WS-C-M101STPIND
       END-IF.
      *----------------------------------------------------------------*
      * GET SYSTEM PARAMETERS FOR LOCAL CURRENCY CODE.                *
      *----------------------------------------------------------------*
           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "RSYCTLUCY"      TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL TO WK-C-LCUYCD
           ELSE
              MOVE SPACES       TO WK-C-LCUYCD
           END-IF
      *Retrieve GPI Day2b to Allow for Debit FCY Acc Mandate Table
      -    check.
           INITIALIZE WK-C-XGSPA-RECORD
       WS-C-GPI-SW.
           MOVE "GPISWTCH2"      TO WK-C-XGSPA-GHPARCD.
           MOVE "GPIFCYACMN"     TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL(1:1)
                 TO WS-C-GPI-SW
           ELSE
              MOVE "N"          TO WS-C-GPI-SW
       END-IF.
      *Retrieve account length.
           INITIALIZE WK-C-XGSPA-RECORD
       WS-N-ACCLEN.
           MOVE "RSYACCLEN"      TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL(1:2)
                 TO WS-N-ACCLEN
           ELSE
              MOVE 10           TO WS-N-ACCLEN
       END-IF.
      *Retrieve IAFT FCY NSTP Check switch.
           INITIALIZE WK-C-XGSPA-RECORD
       WS-C-FCY-NSTP-SW.
           MOVE "GPIFCYNSTP"     TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL(1:1)
                 TO WS-C-FCY-NSTP-SW
           ELSE
              MOVE "N"          TO WS-C-FCY-NSTP-SW
       END-IF.
      *Retrieve SWIFT RTGS STP Switch.

           INITIALIZE WK-C-XGSPA-RECORD
       WS-C-SWF-RTGS-SW.
           MOVE "SWFRTGSIND"        TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"          USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD  = SPACES
              MOVE WK-C-XGSPA-GHPARVAL(1:1)
                 TO WS-C-SWF-RTGS-SW
           ELSE
              MOVE "N"             TO WS-C-SWF-RTGS-SW
       END-IF.
      *-->Retrieve GPI Day 4 Replace MAS w/ RTGS Payment Mode
           INITIALIZE WK-C-XGSPA-RECORD
       WK-C-MAS-RTGS-SW.
           MOVE "GPI4MASRTG"        TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"          USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD  = SPACES
              MOVE WK-C-XGSPA-GHPARVAL(1:1)
                 TO WK-C-MAS-RTGS-SW
       END-IF.
      *Retrieve GPI Day 4 HK Enhancement Switch
           INITIALIZE WK-C-XGSPA-RECORD
       WK-C-GPI4-GL-SW.
           MOVE "GPI4GLENH"         TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"          USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD  = SPACES
              MOVE WK-C-XGSPA-GHPARVAL(1:1)
                 TO WK-C-GPI4-GL-SW
           ELSE
              MOVE "N"             TO WK-C-GPI4-GL-SW
       END-IF.
           INITIALIZE WK-VTD1-OUTPUT
              WK-LOGG
       WK-C-WORK-AREA.
           MOVE ALL "X"             TO TABLE-ARRAY.
           MOVE ALL "X"             TO TABLE-ARR2.
           MOVE "Y"                 TO FIRST-TIME.
           MOVE "Y"                     TO WK-RTGS-BNKAC-EXIST.
           MOVE ZEROS                   TO WK-C-RRSN-QUENUM
              WK-C-RRSN-QUESUF
              WK-C-RRSN-STAFFIND
              WK-C-RRSN-SEQNUM
       WK-C-RRSN-RPRDTE.
           MOVE "N"                     TO WK-C-VALID-MTMSG
       WK-C-ACMN-CHECK.
           IF FIRST-TIME = "Y"
              OPEN INPUT TFSSTPL

              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVD1 - OPEN FILE ERROR - TFSSTPL"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT TFSCLSYS
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVD1 - OPEN FILE ERROR - TFSCLSYS"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT TFSBNKET
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVD1 - OPEN FILE ERROR - TFSBNKET"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT TFS202V
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVD1 - OPEN FILE ERROR - TFS202V"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT UFIMIJ
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVD1 - OPEN FILE ERROR - UFIMIJ "
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT UFMGLPAY
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVD1 - OPEN FILE ERROR - UFMGLPAY"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
       END-IF.
           MOVE WK-VTD1-PARALNO TO TFSSTPL-PARALNO.
           MOVE WK-VTD1-SEQNUM TO TFSSTPL-SEQNUM.
           READ TFSSTPL
              KEY IS EXTERNALLY-DESCRIBED-KEY.
           IF WK-C-SUCCESSFUL
              MOVE TFSSTPL-TAG53 TO TAG53-FORMAT
              MOVE TFSSTPL-TAG54 TO TAG54-FORMAT
              IF WS-C-GPI-SW = "Y"
                 IF TFSSTPL-SWFTMTGY = "103" OR "202"
                    MOVE "Y" TO WK-C-VALID-MTMSG
                 END-IF
              END-IF
              PERFORM A100-MOVE-TAG-VALUES
                 THRU A199-MOVE-TAG-VALUES-EX
              PERFORM A200-INITIAL-SUBROUTINE

                 THRU A299-INITIAL-SUBROUTINE-EX
              PERFORM B100-PATH-CHOICE THRU B199-PATH-CHOICE-EX
       END-IF.
           GO TO Z000-END-PROGRAM.

       A100-MOVE-TAG-VALUES.
           MOVE TFSSTPL-SENBNKID TO WS-BANKID.
202VKE     IF TAG53-OPT NOT = SPACES
                 AND TAG53-BIC NOT = SPACES
              MOVE TAG53-BIC TO WS-SENCBNKID
202VKE     END-IF.
           IF TFSSTPL-IMSGTYPE NOT = "M"
              IF TAG54-OPT NOT = SPACES
                 IF TAG54-BIC NOT = SPACES
                    MOVE TAG54-BIC TO WS-RECBNKID
                       WS-BANKID
                 END-IF
                 MOVE TAG54-PTID TO WS-ACCNO
              ELSE
                 IF TAG53-OPT NOT = SPACES
                    IF TAG53-BIC NOT = SPACES
                       MOVE TAG53-BIC TO WS-SNDCBNKID
                          WS-BANKID
                    END-IF
                    MOVE TAG53-PTID TO WS-ACCNO
                 END-IF
              END-IF
       END-IF.

       A199-MOVE-TAG-VALUES-EX.
       EXIT.
              EJECT

       A200-INITIAL-SUBROUTINE.
      *----------------------------------------------------------------*
      *  GET DATA FROM "TFSCLSYS" TABLE                               *
      *----------------------------------------------------------------*
           READ TFSCLSYS.
           IF NOT WK-C-SUCCESSFUL
              DISPLAY "TRFVTDI  - READ TFSCLSYS ERROR"
              DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
              GO TO Z000-END-PROGRAM.

5Q1JM1        MOVE TFSCLSYS-SYSDTE TO WK-N-SYSDTE.

       A299-INITIAL-SUBROUTINE-EX.
       EXIT.

       B100-PATH-CHOICE.
              MOVE "Y" TO WS-FLAG1.
              IF TFSSTPL-IMSGTYPE = "M"
6Q3LN1           IF SW-RTGS-BYPASS-F53-F54-Y
6Q3LN1              PERFORM D500-RTGS-VALIDATION
6Q3LN1                 THRU D599-RTGS-VALIDATION-EX

                    ELSE
                    IF TAG53-OPT    = SPACES
                          AND TAG53-PTID  = SPACES
                          AND TAG53-BIC   = SPACES
                          AND TAG54-OPT   = SPACES
                          AND TAG54-PTID  = SPACES
                          AND TAG54-BIC   = SPACES
                       PERFORM C100-VALIDATION-PART
                          THRU C199-VALIDATION-PART-EX
                       END-IF
                    END-IF

      **** call routine to obtain incoming msg's tag 55 if any.
      **** For an incoming msg with tag 55, currently possible for
      **** swift MT103, it should be routed to repair.
                 PERFORM E001-GET-TAG55 THRU E001-GET-TAG55-EX
                 EVALUATE TRUE
                    WHEN TAG55-OPT NOT = SPACES
                    PERFORM C500-VALIDATION-PART
                       THRU C599-VALIDATION-PART-EX
                    WHEN NOT(TAG54-OPT = SPACES
                       AND TAG54-PTID = SPACES
                       AND TAG54-BIC = SPACES)
                    PERFORM C200-VALIDATION-PART
                       THRU C299-VALIDATION-PART-EX
                    WHEN OTHER
                    PERFORM C300-VALIDATION-PART
                       THRU C399-VALIDATION-PART-EX
                    END-EVALUATE
       END-IF.

              PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.
              IF TFSSTPL-SWFTMGTY = 101
                    AND WS-C-M101STPND = "Y"
                 PERFORM C600-VALIDATION-PART
                    THRU C699-VALIDATION-PART-EX
       END-IF.
              PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.
       EXIT.

       C100-VALIDATION-PART.
              PERFORM D400-SHIFT-VALIDATION.
              IF SHIFT-IND = "Y"
                 MOVE PATH-P7 TO TABLE-ARRAY
       END-IF.
       C199-VALIDATION-PART-EX.
       EXIT.

                 EJECT

       C200-VALIDATION-PART.
              IF TAG54-OPT    = "A"
                    AND TAG54-PTID  NOT = SPACES
                    AND TAG54-BIC   NOT = SPACES
                 MOVE PATH-P1 TO TABLE-ARRAY
                 IF WS-C-GPI-SW = "Y"
                       AND WK-C-VALID-MTMSG = "Y"
                       AND TAG54-BIC NOT = TFSSTPL-SENBNKID
                    MOVE "Y" TO WK-C-ACMN-CHECK
                 END-IF
       END-IF.
              IF TAG54-OPT    = "A"
                    AND TAG54-PTID  = SPACES
                    AND TAG54-BIC   NOT = SPACES
                 MOVE PATH-P2 TO TABLE-ARRAY
                 IF WS-C-GPI-SW = "Y"
                       AND WK-C-VALID-MTMSG = "Y"
                       AND TAG54-BIC NOT = TFSSTPL-SENBNKID
                    MOVE "Y" TO WK-C-ACMN-CHECK
                 END-IF
       END-IF.
              IF ((    TAG54-OPT    = "B"
                    OR TAG54-OPT    = "D")
                    AND TAG54-PTID  = SPACES
                    AND TAG54-BIC   NOT = SPACES
                    AND WS-C-GPI-SW = "Y"
                    AND WK-C-VALID-MTMSG = "Y")
                 MOVE PATH-P2 TO TABLE-ARRAY
                 IF (TAG54-BIC NOT = SPACES
                       AND TAG54-BIC NOT = TFSSTPL-SENBNKID)
                    MOVE "Y" TO WK-C-ACMN-CHECK
                 END-IF
       END-IF.
              IF WS-C-GPI-SW = "Y"
                    AND WK-C-VALID-MTMSG = "Y"
                 IF TAG54-OPT    = "B" OR "D"
                    IF (TAG54-OPT    = "B"
                          OR TAG54-OPT    = "D")
                          AND TAG54-PTID  NOT = SPACES
                       IF TAG54-PTID(1:WS-N-ACCLEN) IS NUMERIC
                          IF WS-ACCNO NOT = SPACES
                             MOVE PATH-P1 TO TABLE-ARRAY
                             IF TAG54-OPT    = "B"
                                   AND (TAG54-BIC NOT = SPACES
                                   AND TAG54-BIC NOT = TFSSTPL-SENBNKID)
                                MOVE "Y" TO WK-C-ACMN-CHECK
                             END-IF
                             IF TAG54-OPT    = "D"
                                MOVE TFSSTPL-SENBNKID
                                   TO WS-BANKID
                             END-IF
                          ELSE
                             INITIALIZE WK-C-RPRRSN-AREA
                             MOVE "RSN0095" TO WK-C-RPRCODE

                             PERFORM E002-PROCESS-RPRRSN
                                THRU E002-PROCESS-RPRRSN-EX
                             END-IF
                          END-IF
       C299-VALIDATION-PART-EX.
       EXIT.
       C300-VALIDATION-PART.
                       IF TAG53-OPT       = "A"
                             AND TAG53-PTID     NOT = SPACES
                             AND TAG53-BIC      NOT = SPACES
                          MOVE PATH-P3 TO TABLE-ARRAY
                          IF WS-C-GPI-SW = "Y"
                                AND WK-C-VALID-MTMSG = "Y"
                                AND TAG53-BIC NOT = TFSSTPL-SENBNKID
                             MOVE "Y" TO WK-C-ACMN-CHECK
                          END-IF
                       END-IF
                       IF TAG53-OPT       = "A"
                             AND TAG53-PTID     = SPACES
                             AND TAG53-BIC      NOT = SPACES
                          MOVE PATH-P4 TO TABLE-ARRAY
                          IF WS-C-GPI-SW = "Y"
                                AND WK-C-VALID-MTMSG = "Y"
                                AND TAG53-BIC NOT = TFSSTPL-SENBNKID
                             MOVE "Y" TO WK-C-ACMN-CHECK
                          END-IF
                       END-IF
                       IF (( TAG53-OPT    = "B"
                             OR TAG53-OPT       = "D")
                             AND TAG53-PTID     = SPACES
                             AND TAG53-BIC      NOT = SPACES
                             AND WS-C-GPI-SW    = "Y"
                             AND WK-C-VALID-MTMSG = "Y")
                          MOVE PATH-P4 TO TABLE-ARRAY
                          IF (TAG53-BIC NOT = SPACES
                                AND TAG53-BIC NOT = TFSSTPL-SENBNKID)
                             MOVE "Y" TO WK-C-ACMN-CHECK
                          END-IF
                       END-IF
                       IF WS-C-GPI-SW     = "Y"
                             AND WK-C-VALID-MTMSG = "Y"
                          IF ((TAG53-OPT     = "B"
                                OR TAG53-OPT       = "D")
                                AND TAG53-PTID     NOT = SPACES
                             IF TAG53-PTID(1:WS-N-ACCLEN) IS NUMERIC
                                IF WS-ACCNO NOT = SPACES
                                   MOVE PATH-P3 TO TABLE-ARRAY
                                   IF TAG53-OPT = "B"
                                         AND (TAG53-BIC NOT = SPACES
           AND TAG53-BIC NOT = TFSSTPL-SENBNKID)
                                      MOVE "Y" TO WK-C-ACMN-CHECK

                                   END-IF
                                   IF    TAG53-OPT       = "D"
                                      MOVE TFSSTPL-SENBNKID
                                         TO WS-BANKID
                                   END-IF
                                ELSE
                                   INITIALIZE WK-C-RPRRSN-AREA
                                   MOVE "RSN0095"       TO WK-C-RPRCODE
                                   PERFORM E002-PROCESS-RPRRSN
                                      THRU E002-PROCESS-RPRRSN-EX
                                   END-IF
       END-IF.
                             IF    TAG53-OPT       = SPACES
                                   AND TAG53-PTID      = SPACES
                                   AND TAG53-BIC       = SPACES
                                   AND TAG54-OPT       = SPACES
                                   AND TAG54-PTID      = SPACES
                                   AND TAG54-BIC       = SPACES
                                MOVE PATH-P5         TO TABLE-ARRAY
       END-IF.
       C399-VALIDATION-PART-EX.
       EXIT.
                                EJECT
                             MOVE WK-VTD1-PARALNO    TO WK-VTD2-PARALNO.
                             MOVE WK-VTD1-SEQNUM     TO WK-VTD2-SEQNUM.
                             CALL "TRFVTD2"          USING WK-VTD2.
                             MOVE WK-VTD2-NO-ERROR   TO WS-OKAY.
       C499-TABLE-D2-EX.
       EXIT.
                                EJECT
                             MOVE "N"            TO WS-OKAY.
                             INITIALIZE WK-C-RPRRSN-AREA
                             MOVE "RSN0042"       TO WK-C-RPRCODE.
                             PERFORM E002-PROCESS-RPRRSN
                                THRU E002-PROCESS-RPRRSN-EX.
       C599-VALIDATION-PART-EX.
       EXIT.
                             INITIALIZE WK-C-XGSPA-RECORD.
                             MOVE "RSYACCLEN"     TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA"      USING WK-C-XGSPA-RECORD.
           MOVE WK-C-XGSPA-GHPARVAL(1:2) TO WK-N-ACCTLEN.

      *-- Get Ordering Customer (Tag50) from UFIMIJ
                             PERFORM R001-READ-UFIMIJ
                                THRU R001-READ-UFIMIJ-EX.
      *-- Check Customer Processing Feature and WEE Hours.
                             MOVE "Y" TO TAB-VL2(08).
                             CALL "TRFGSDTS" USING WK-C-GSDTS-RECORD.
                             INITIALIZE WK-C-CUPF-RECORD.
                             MOVE WS-T50-ACCNO(1:10) TO WK-C-CUPF-ACCNO.
                             MOVE WS-T50-ACCNO(1:WK-N-ACCTLEN)
                                TO WK-C-CUPF-ACCNO.
                             MOVE TFSSTPL-CUYCD TO WK-C-CUPF-CUYCD.
                             MOVE WS-BANKID TO WK-C-CUPF-BANKID.
                             MOVE "1" TO WK-C-CUPF-OPTION.
                             CALL "TRFCUPF" USING WK-C-CUPF-RECORD.
                             CANCEL "TRFCUPF".
                             IF WK-C-CUPF-ERROR-CD NOT = SPACES
                                MOVE "N" TO WS-OKAY
                                   TAB-VL2(08)
                                INITIALIZE WK-C-RPRRSN-AREA
                                MOVE WK-C-CUPF-ERROR-CD TO WK-C-RPRCODE
                                PERFORM E002-PROCESS-RPRRSN
                                   THRU E002-PROCESS-RPRRSN-EX
       END-IF.
      *-- Account must be eligible for Inward MT101 process.
                             IF WK-C-CUPF-EFFDT = ZEROES OR
                                   WK-C-CUPF-EFFDT > WK-N-GSDTS-SYSTDE
                                MOVE "N" TO WS-OKAY
                                   TAB-VL2(08)
                                INITIALIZE WK-C-RPRRSN-AREA
                                MOVE "RSN0317" TO WK-C-RPRCODE
                                PERFORM E002-PROCESS-RPRRSN
                                   THRU E002-PROCESS-RPRRSN-EX
       END-IF.
      *-- Account must be eligible for WEE HOUR processing.
                             IF WK-C-CUPF-WEEHR-PERIOD = "Y"
                                IF WK-C-CUPF-WEEIND NOT = "Y"
                                   MOVE "N" TO WS-OKAY
                                      TAB-VL2(08)
                                   INITIALIZE WK-C-RPRRSN-AREA
                                   MOVE "RSN0318" TO WK-C-RPRCODE
                                   PERFORM E002-PROCESS-RPRRSN
                                      THRU E002-PROCESS-RPRRSN-EX
                                   END-IF
       END-IF.
      *-- Check if Ordering Customer's Account No. is valid.
                             IF WS-OKAY = "Y" AND TAB-VL2(08) = "Y"
                                IF WK-VTD1-RBK-IND NOT = "Y"

                                   IF WK-C-CUPF-ACCTYP = "C"
                                         OR WK-C-CUPF-ACCTYP = "F"
                                      IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
                                         IF TFSSTPL-CUYCD = WK-C-LCUYCD
           MOVE "CA"           TO WS-PMODE
           MOVE WS-T50-ACCNO(1:10) TO WK-C-VCCA-CA-NO
           MOVE WS-T50-ACCNO(1:WK-N-ACCTLEN)
                                               TO WK-C-VCCA-CA-NO
           CALL "TRFVCCA" USING WK-C-VCCA-RECORD
           IF WK-C-VCCA-ERROR-CD = SPACES
           MOVE WK-C-VCCA-CUSTFNAM TO WK-VTD1-PAYRNAME
           MOVE WK-C-VCCA-ADDR1 TO WK-VTD1-PAYRADR1
           MOVE WK-C-VCCA-ADDR2 TO WK-VTD1-PAYRADR2
           MOVE WK-C-VCCA-ADDR3 TO WK-VTD1-PAYRADR3
           MOVE WK-C-VCCA-ADDR4 TO WK-VTD1-PAYRADR4
           MOVE WK-C-VCCA-ADDR5 TO WK-VTD1-PAYRADR5
           MOVE WK-C-VCCA-ADDR6 TO WK-VTD1-PAYRADR6
           MOVE WK-C-VCCA-AOCD TO WK-VTD1-AOCD
           MOVE WK-N-VCCA-RESCD TO WK-VTD1-RESCD
           MOVE WK-N-VCCA-DOMBRCH TO WK-VTD1-DOMBRCH
           MOVE WK-N-VCCA-HOLCDCD1 TO WK-VTD1-HOLCDCD1
           MOVE WK-N-VCCA-HOLCDCD2 TO WK-VTD1-HOLCDCD2
           MOVE WK-N-VCCA-HOLCDCD3 TO WK-VTD1-HOLCDCD3
                                            END-IF
                                         ELSE
           MOVE "FCCA"           TO WS-PMODE
           MOVE WS-T50-ACCNO(1:10) TO WK-C-VCFA-FCCA
           MOVE WS-T50-ACCNO(1:WK-N-ACCTLEN)
                                               TO WK-C-VCFA-FCCA
           MOVE TFSSTPL-CUYCD TO WK-C-VCFA-CUY
           CALL "TRFVCFA" USING WK-C-VCFA-RECORD
           IF WK-C-VCFA-ERROR-CD = SPACES
           MOVE WK-C-VCFA-CUSTFNAM TO WK-VTD1-PAYRNAME
           MOVE WK-C-VCFA-ADDR1 TO WK-VTD1-PAYRADR1
           MOVE WK-C-VCFA-ADDR2 TO WK-VTD1-PAYRADR2
           MOVE WK-C-VCFA-ADDR3 TO WK-VTD1-PAYRADR3
           MOVE WK-C-VCFA-ADDR4 TO WK-VTD1-PAYRADR4
           MOVE WK-C-VCFA-ADDR5 TO WK-VTD1-PAYRADR5
           MOVE WK-C-VCFA-ADDR6 TO WK-VTD1-PAYRADR6
           MOVE WK-C-VCFA-AOCD TO WK-VTD1-AOCD
           MOVE WK-N-VCFA-RESCD TO WK-VTD1-RESCD
           MOVE WK-N-VCFA-DOMBRCH TO WK-VTD1-DOMBRCH
           MOVE WK-N-VCFA-HOLCDCD1 TO WK-VTD1-HOLCDCD1
           MOVE WK-N-VCFA-HOLCDCD2 TO WK-VTD1-HOLCDCD2
           MOVE WK-N-VCFA-HOLCDCD3 TO WK-VTD1-HOLCDCD3
                                            END-IF
                                         END-IF
           IF (TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
           OR (TFSSTPL-CUYCD = WK-C-LCUYCD
           AND WK-C-VCFA-ERROR-CD NOT = SPACES)
           OR (TFSSTPL-CUYCD NOT = TFSCLSYS-LCUYCD
           OR (TFSSTPL-CUYCD NOT = WK-C-LCUYCD
           AND WK-C-VCFA-ERROR-CD NOT = SPACES))
           MOVE "N"           TO WS-OKAY TAB-VL2(09)

                                            INITIALIZE WK-C-RPRRSN-AREA
           IF WK-C-VCCA-ERROR-CD NOT = SPACES
           MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
           MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCDE
           MOVE WK-N-VCCA-STAFFIND TO WK-N-STAFFIND
           MOVE WK-C-VCCA-CUSTFNAM TO WK-C-ACCNAME
                                            ELSE
           MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
           MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCDE
           MOVE WK-N-VCFA-STAFFIND TO WK-N-STAFFIND
           MOVE WK-C-VCFA-CUSTFNAM TO WK-C-ACCNAME
                                            END-IF
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX
                                            END-IF
                                         ELSE
           MOVE "N"         TO WS-OKAY TAB-VL2(09)
                                         INITIALIZE WK-C-RPRRSN-AREA
           MOVE "RSN0091"   TO WK-C-RPRCODE
                                         PERFORM E002-PROCESS-RPRRSN
                                            THRU E002-PROCESS-RPRRSN-EX
                                         END-IF
                                         ELSE
                                      MOVE "N"         TO WS-OKAY
                                      MOVE "X"         TO TAB-VL2(09)
                                      INITIALIZE WK-C-RPRRSN-AREA
                                      MOVE "RSN0035"   TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
                                      END-IF
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
       C699-VALIDATION-PART-EX.
       EXIT.
       D100-VALIDATION.
                                IF TABLE-ARRAY NOT = ALL "X"
                                   MOVE "Y"     TO WS-OKAY
                                ELSE
                                   IF TFSSTPL-IMSGTYPE = "M"
                                         AND SW-RTGS-BYPASS-F53-F54-Y
                                      IF WK-RTGS-BNKAC-EXIST = "Y"
                                         MOVE "Y"     TO WS-OKAY
                                      END-IF
                                   ELSE
                                      MOVE "N"     TO WS-OKAY
                                      INITIALIZE WK-C-RPRRSN-AREA
                                      MOVE "RSN0114" TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
                                      END-IF
       END-IF.

                                IF ((TAG53-OPT   = "A"
                                      AND TAG53-BIC    NOT = SPACES)
                                      OR (TAG54-OPT    = "A"
                                      AND TAG54-BIC    NOT = SPACES))
                                      AND WS-OKAY      = "Y"
           PERFORM C400-TABLE-D2 THRU C499-TABLE-D2-EX
       END-IF.

           IF TAB-VAL(01) NOT = "X" AND WS-OKAY = "Y"
                                   MOVE TAB-VAL(01)     TO TAB-VL2(01)
           MOVE TFSSTPL-BNKENTTY TO WK-N-VBANO-BNKENTTY
           MOVE WS-BANKID       TO WK-C-VBANO-BANKID
           MOVE TFSSTPL-CUYCD   TO WK-C-VBANO-CUYCD
GP3A01                             IF WS-ACCNO NOT = SPACES AND
           GP3A01                                   WS-ACCNO(WK-N-
      -    ACCLEN:1) = SPACES
GP3A01                                CONTINUE
GP3A01                             ELSE
GP3A01                                MOVE WS-ACCNO(4:) TO WS-ACCNO
GP3A01                             END-IF
GP3A01                             MOVE WS-ACCNO     TO WK-C-NEW-ACCNO2
           GP3A01                             MOVE WK-C-NEW-ACCNO TO WK-
      -    C-VBANO-ACCNO
GP3A01                             MOVE WS-ACCNO     TO WK-C-VBANO-ACCNO
           GP3A01* CALL "TRFVBANO" USING WK-C-VBANO-RECORD
                                   IF WK-C-VBANO-ERROR-CD NOT = SPACES
                                      MOVE "N" TO WS-OKAY TAB-VL2(01)
5Q1JM1                                INITIALIZE WK-C-RPRRSN-AREA
           MOVE WK-C-VBANO-ERROR-CD TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
                                      ELSE
           MOVE WK-C-VBANO-ACUDBUI TO WS-ACUDBUI
           MOVE WK-C-VBANO-ACCTYP TO WS-ACCTYP
                                      END-IF
202VKE                             MOVE WS-RECBNKID TO TFS202V-MT202VBK
                                   READ TFS202V
                                      NOT INVALID KEY
                                   MOVE "N" TO WS-OKAY TAB-VL2(01)
5Q1JM1                             INITIALIZE WK-C-RPRRSN-AREA
                                   MOVE "SUP0036" TO WK-C-RPRCODE
                                   PERFORM E002-PROCESS-RPRRSN
                                      THRU E002-PROCESS-RPRRSN-EX
202VKE                             END-READ
202VKE                          MOVE WS-SENCBNKID TO TFS202V-MT202VBK
                                READ TFS202V
                                   NOT INVALID KEY
                                MOVE "N" TO WS-OKAY TAB-VL2(01)
5Q1JM1                          INITIALIZE WK-C-RPRRSN-AREA
                                MOVE "SUP0037" TO WK-C-RPRCODE
                                PERFORM E002-PROCESS-RPRRSN
                                   THRU E002-PROCESS-RPRRSN-EX
202VKE                          END-READ
                                G2BM00* Check CR BIC againts NSTP Table.
G2BM01                       IF WS-C-GPI-SW = "Y"
G2BM01                             AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD

                                   AND WS-C-FCY-NSTP-SW = "Y"
                                   AND WS-OKAY = "Y"
                                INITIALIZE WK-NSTP
                                MOVE WS-BANKID TO WK-NSTP-ACCTBIC
                                CALL "TRFNSTP" USING WK-NSTP
                                IF WK-NSTP-NONSTPDR = "Y"
                                   MOVE "N" TO WS-OKAY
                                      TAB-VL2(01)
                                   INITIALIZE WK-C-RPPRSN-AREA
      *------------> RSN0038: Bank ID found in NON STP TABLE
                                   MOVE "RSN0038" TO WK-C-RPRCODE
                                   PERFORM E002-PROCESS-RPPRSN
                                      THRU E002-PROCESS-RPPRSN-EX
                                   ELSE
      * Check DR Account againts NSTP Table.
                                   INITIALIZE WK-NSTP
                                   MOVE WS-ACCNO TO WK-NSTP-ACCTBIC
           MOVE WK-C-NEW-ACCNO TO WK-NSTP-ACCTBIC
                                   CALL "TRFNSTP" USING WK-NSTP
                                   IF WK-NSTP-NONSTPDR = "Y"
                                      MOVE "N" TO WS-OKAY
                                         TAB-VL2(01)
                                      INITIALIZE WK-C-RPPRSN-AREA
      *----------------> RSN0034: A/C number exist in Non STP Table
                                      MOVE "RSN0034" TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPPRSN
                                         THRU E002-PROCESS-RPPRSN-EX
                                      END-IF
                                   END-IF
      *Check Account againts Account mandate Table.
                                IF WS-C-GPI-SW = "Y"
           AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
                                      AND WK-C-ACMN-CHECK = "Y"
                                      AND WS-OKAY = "Y"
                                   IF WS-ACCTYP = "N"
                                      GO TO TAB-VAL2-CHECK
                                   END-IF
                                   INITIALIZE WK-ACMN
                                   MOVE TFSSTPL-BNKENTTY
                                      TO WK-ACMN-BANKCD
                                   MOVE TFSSTPL-CUYCD
                                      TO WK-ACMN-CUYCD
                                   IF WS-ACCNO NOT = SPACES AND
           WS-ACCNO(WK-N-ACCLEN:1) = SPACES
                                      MOVE WS-ACCNO TO WK-ACMN-ACCNO
                                   ELSE
                                      MOVE WS-ACCNO(4:)
                                         TO WK-ACMN-ACCNO
                                   END-IF
                                   MOVE TFSSTPL-SENBNKID
                                      TO WK-ACMN-BANKID
                                   CALL "TRFACMN" USING WK-ACMN
                                   IF WK-ACMN-MANIND = "N"

                                      MOVE "N"           TO WS-OKAY
                                      MOVE               TAB-VL2(01)
           INITIALIZE         WK-C-RPRRSN-AREA
                                      MOVE "RSN0097"     TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
                                      END-IF
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(02) NOT = "X" AND WS-OKAY = "Y"
                                   MOVE TAB-VAL(02) TO TAB-VL2(02)
           MOVE TFSSTPL-BNKENTTY TO WK-C-VBAC-BNKENTTY
                                   MOVE WS-BANKID TO WK-C-VBAC-BANKID
                                   MOVE TFSSTPL-CUYCD TO WK-C-VBAC-CUYCD
                                   CALL "TRFVBAC" USING WK-C-VBAC-RECORD
                                   IF WK-C-VBAC-ERROR-CD NOT = SPACES
                                      MOVE "N" TO WS-OKAY TAB-VL2(02)
                                      INITIALIZE WK-C-RPRRSN-AREA
           MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
                                      ELSE
           MOVE WK-C-VBAC-ACUDUBUI TO WS-ACUDUBUI
                                      MOVE WK-C-VBAC-ACCTYP TO WS-ACCTYP
           IF WK-C-VBAC-BNKACNO NOT = SPACES AND
           WK-C-VBAC-BNKACNO(WK-N-ACCLEN:1) = SPACES
           MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
                                      ELSE
           MOVE WK-C-VBAC-BNKACNO(4:) TO WS-ACCNO
                                      END-IF
                                      END-IF
                                   MOVE WS-RECBNKID TO TFS202V-MT202VBK
                                   READ TFS202V
                                      NOT INVALID KEY
                                   MOVE "N" TO WS-OKAY TAB-VL2(01)
                                   INITIALIZE WK-C-RPRRSN-AREA
                                   MOVE "RSN0036" TO WK-C-RPRCODE
                                   PERFORM E002-PROCESS-RPRRSN
                                      THRU E002-PROCESS-RPRRSN-EX
                                   END-READ
                                MOVE WS-SENCBNKID TO TFS202V-MT202VBK
                                READ TFS202V
                                   NOT INVALID KEY
                                MOVE "N" TO WS-OKAY TAB-VL2(02)
                                INITIALIZE WK-C-RPRRSN-AREA
                                MOVE "RSN0037" TO WK-C-RPRCODE
                                PERFORM E002-PROCESS-RPRRSN
                                   THRU E002-PROCESS-RPRRSN-EX
                                END-READ
      * Check DR BIC againts NSTP Table.
                             IF WS-C-GPI-SW = "Y"

                                   AND  TFSTPL-CUYCD NOT = WK-C-LCUYCD
                                   AND  WS-C-FCY-NSTP-SW = "Y"
                                   AND  WS-OKAY = "Y"
                                INITIALIZE WK-NSTP
                                MOVE WS-BANKID TO WK-NSTP-ACCTBIC
                                CALL "TRFNSTP" USING WK-NSTP
                                IF WK-NSTP-NONSTPDR = "Y"
                                   MOVE "N" TO WS-OKAY
                                      TAB-VL2(02)
                                   INITIALIZE WK-C-RPPRSN-AREA
                                   MOVE "RSN0038" TO WK-C-RPRCODE
                                   PERFORM E002-PROCESS-RPPRSN
                                      THRU E002-PROCESS-RPPRSN-EX
                                   ELSE
                                   INITIALIZE WK-NSTP
                                   IF WS-ACCNO NOT = SPACES AND
           WS-ACCNO(WK-N-ACCLEN:1) = SPACES
                                      CONTINUE
                                   ELSE
                                      MOVE WS-ACCNO(4:) TO WS-ACCNO
                                   END-IF
                                   MOVE WS-ACCNO TO WK-C-NEW-ACCNO2
           MOVE WK-C-NEW-ACCNO TO WK-NSTP-ACCTBIC
                                   MOVE WS-ACCNO TO WK-NSTP-ACCTBIC
                                   CALL "TRFNSTP" USING WK-NSTP
                                   IF WK-NSTP-NONSTPDR = "Y"
                                      MOVE "N" TO WS-OKAY
                                         TAB-VL2(02)
                                      INITIALIZE WK-C-RPPRSN-AREA
                                      MOVE "RSN0034" TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPPRSN
                                         THRU E002-PROCESS-RPPRSN-EX
                                      END-IF
                                   END-IF
                                IF WS-C-GPI-SW = "Y"
                                      AND TFSTPL-CUYCD NOT = WK-C-LCUYCD
                                      AND WK-C-ACMN-CHECK = "Y"
                                      AND WS-OKAY = "Y"
                                   IF WS-ACCTYP = "N"
                                      GO TO TAB-VAL3-CHECK
                                   END-IF
                                   PERFORM E003-ACC-MANDATE-CHECK
                                      THRU E003-ACC-MANDATE-CHECK-EX
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
       TAB-VAL3-CHECK.
           IF TAB-VAL(03) NOT = "X" AND WS-OKAY = "Y"
                                   MOVE TAB-VAL(03) TO TAB-VL2(03)

      *-- Bank account table validation.
           MOVE TFSSTPL-BNKENTITY TO WK-N-VBAC-BNKENTITY
           MOVE WS-BANKID        TO WK-C-VBAC-BANKID
                                   MOVE WK-BLKB-BKID     TO WK-BLKB-BKID
           MOVE TFSSTPL-CUYCD    TO WK-C-VBAC-CUYCD
           CALL "TRFVBAC"        USING WK-C-VBAC-RECORD
                                   CALL "TRFBLKB"        USING WK-BLKB
                                   IF WK-C-VBAC-ERROR-CD NOT = SPACES
                                         OR WK-BLKB-INDIC = "Y"
                                      MOVE "N" TO WS-OKAY TAB-VL2(03)
                                      INITIALIZE WK-C-RPRRSN-AREA
           MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
                                      ELSE
           MOVE WK-C-VBAC-ACUDBUUI TO WS-ACUDBUUI
                                      MOVE WK-C-VBAC-ACCTYP TO WS-ACCTYP
           IF WK-C-VBAC-BNKACNO NOT = SPACES AND
           WK-C-VBAC-BNKACNO(WK-N-ACCLEN:1) = SPACES
           MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
                                      ELSE
           MOVE WK-C-VBAC-BNKACNO(4:) TO WS-ACCNO
                                      END-IF
                                      END-IF
      *-- Blacklist table validation.
                                   INITIALIZE WK-BLKB
                                   MOVE WS-BANKID TO WK-BLKB-BKID
                                   CALL "TRFBLKB" USING WK-BLKB
                                   IF WK-BLKB-INDIC = "Y"
                                      MOVE "N" TO WS-OKAY TAB-VL2(03)
                                      INITIALIZE WK-C-RPRRSN-AREA
                                      MOVE "RSN009" TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
                                      END-IF
      * Check DR BIC againts NSTP Table.
                                   IF WS-C-GPI-SW = "Y"
           AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD
                                         AND WS-C-FCY-NSTP-SW = "Y"
                                         AND WS-OKAY = "Y"
                                      INITIALIZE WK-NSTP
                                      MOVE WS-BANKID TO WK-NSTP-ACCTBIC
                                      CALL "TRFNSTP" USING WK-NSTP
                                      IF WK-NSTP-NONSTPDR = "Y"
                                         MOVE "N" TO WS-OKAY
                                            TAB-VL2(03)
                                         INITIALIZE WK-C-RPRRSN-AREA
      * --------------> RSN0038: Bank ID found in NON STP TABLE
                                         MOVE "RSN0038" TO WK-C-RPRCODE
                                         PERFORM E002-PROCESS-RPRRSN
                                            THRU E002-PROCESS-RPRRSN-EX
                                         ELSE
      * Check DR Account againts NSTP Table.
                                         INITIALIZE WK-NSTP

                                         IF WS-ACCNO NOT = SPACES AND
           WS-ACCNO(WK-N-ACCLEN:1) = SPACES
                                            CONTINUE
                                         ELSE
           MOVE WS-ACCNO(4:) TO WS-ACCNO
                                         END-IF
           MOVE WS-ACCNO TO WK-C-NEW-ACCNO2
           MOVE WK-C-NEW-ACCNO TO WK-NSTP-ACCTBIC
           MOVE WS-ACCNO TO WK-NSTP-ACCTBIC
                                         CALL "TRFNSTP" USING WK-NSTP
                                         IF WK-NSTP-NONSTPDR = "Y"
                                            MOVE "N" TO WS-OKAY
                                               TAB-VL2(03)
                                            INITIALIZE WK-C-RPRRSN-AREA
           MOVE "RSN0034" TO WK-C-RPRCODE
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX
                                            END-IF
                                         END-IF
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(04) NOT = "X" AND WS-OKAY = "Y"
                                      MOVE TAB-VAL(04) TO TAB-VL2(04)
                                      IF WS-ACCTYP NOT = "C"
                                            AND WS-ACCTYP NOT = "F"
                                            OR (WS-C-GPI-SW = "Y"
                                            AND WK-C-VALID-MTMSG = "Y"
                                            AND TAB-VL2(03) = "X")
                                         IF (WS-ACCTYP = "C"
                                               OR WS-ACCTYP = "F")
                                            EVALUATE WS-ACCTYP
                                               WHEN "C"
                                               MOVE "CA"
                                                  TO WS-PMODE
                                               WHEN "F"
                                               MOVE "FCCA"
                                                  TO WS-PMODE
                                               END-EVALUATE
                                            MOVE SPACE TO WS-ACT1
                                            MOVE "Y" TO WS-ACT2
                                            MOVE "Y" TO TAB-VAL(05)
                                            MOVE "N" TO TAB-VAL(06)
                                               ELSE
                                            MOVE "NOSTRO" TO WS-PMODE
                                            IF TAB-VL2(03) = "X"
                                                  AND TAB-VL2(04) = "Y"
                                               MOVE "Y" TO WS-ACT1
                                               MOVE SPACE TO WS-ACT2
                                            ELSE
                                               IF TAB-VL2(03) = "Y"
           AND TAB-VL2(04) = "Y"
                                                  MOVE SPACE TO WS-ACT1

           MOVE "Y"           TO WS-ACT2
                                               ELSE
           MOVE SPACE     TO WS-ACT1
                                                     WS-ACT2
                                               END-IF
G2BM00                                      END-IF
G2BM00                                         END-IF
                                            ELSE
                                         MOVE "N"       TO TAB-VL2(04)
                                         IF TAB-VL2(03) = "Y"
                                            MOVE PATH-P6 TO TABLE-ARRAY
                                         ELSE
                                            MOVE "N"   TO WS-OKAY
                                            INITIALIZE WK-C-RPRRSN-AREA
           MOVE "RSN2023" TO WK-C-RPRCODE
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX
                                            END-IF
                                         END-IF
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(05) NOT = "X" AND WS-OKAY = "Y"
SM1TY1                                IF WK-VTD1-RBK-IND NOT = "Y"
                                         MOVE TAB-VAL(05) TO TAB-VL2(05)
                                         IF WS-ACCTYP    = "C"
                                               OR WS-ACCTYP    = "F"
           REM269**** IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
           REM269                                      IF TFSSTPL-CUYCD
      -    = WK-C-LCUYCD
                                               MOVE "CA"   TO WS-PMMODE
           ID1VKE*    MOVE WS-ACCNO TO WK-C-VCCA-CA-NO
           ID1VKE                                         MOVE WS-ACCNO
      -    TO WK-C-NEW-ACCNO2
           ID1VKE                                         MOVE WK-C-NEW-
      -    ACCNO TO WK-C-VCCA-CA-NO
           ID1VKE                                         MOVE TFSSTPL-
      -    CUYCD TO WK-C-VCCA-CA-CUY
           CALL "TRFVCA" USING WK-C-VCCA-RECORD
           IF WK-C-VCCA-ERROR-CD = SPACES
           GP4C00                                            IF WK-C-
      -    GP1A5TPLSW = "Y"
           PERFORM D100-STPLIMIT-VALIDATION
           THRU D199-STPLIMIT-VALIDATION-EX
GP4C00                                               END-IF
           MOVE WK-C-VCCA-CUSTFNAM TO WK-VTD1-PAYRNAME
           MOVE WK-C-VCCA-ADDR1 TO WK-VTD1-PAYRADDR1
           MOVE WK-C-VCCA-ADDR2 TO WK-VTD1-PAYRADDR2
           MOVE WK-C-VCCA-ADDR3 TO WK-VTD1-PAYRADDR3
           MOVE WK-C-VCCA-ADDR4 TO WK-VTD1-PAYRADDR4
           MOVE WK-C-VCCA-ADDR5 TO WK-VTD1-PAYRADDR5
           MOVE WK-C-VCCA-ADDR6 TO WK-VTD1-PAYRADDR6
           MOVE WK-C-VCCA-AOCD TO WK-VTD1-AOCD
           MOVE WK-N-VCCA-RESCD TO WK-VTD1-RESCD
           MOVE WK-N-VCCA-DOMBRCH TO WK-VTD1-DOMBRCH
           MOVE WK-N-VCCA-HOLDCD1 TO WK-VTD1-HOLDCD1
           MOVE WK-N-VCCA-HOLDCD2 TO WK-VTD1-HOLDCD2
           MOVE WK-N-VCCA-HOLDCD3 TO WK-VTD1-HOLDCD3
                                                  END-IF
                                               ELSE

           MOVE "FCCA"            TO WS-PMODE
           MOVE WS-ACCNO          TO WK-C-VCFA-FCCA
           MOVE WS-ACCNO          TO WK-C-NEW-ACCNO2
           MOVE WK-C-NEW-ACCNO    TO WK-C-VCFA-FCCA
           MOVE TFSSTPL-CUYCD     TO WK-C-VCFA-CUY
           CALL "TRFYVCFA" USING WK-C-VCFA-RECORD
           IF WK-C-VCFA-ERROR-CD = SPACES
           IF WK-C-GP41STPLSW = "Y"
           PERFORM D100-STPLIMIT-VALIDATION
           THRU D199-STPLIMIT-VALIDATION-EX
                                                     END-IF
           MOVE WK-C-VCFA-CUSTFNAM TO WK-VTD1-PAYRNAME
           MOVE WK-C-VCFA-ADDR1   TO WK-VTD1-PAYRADDR1
           MOVE WK-C-VCFA-ADDR2   TO WK-VTD1-PAYRADDR2
           MOVE WK-C-VCFA-ADDR3   TO WK-VTD1-PAYRADDR3
           MOVE WK-C-VCFA-ADDR4   TO WK-VTD1-PAYRADDR4
           MOVE WK-C-VCFA-ADDR5   TO WK-VTD1-PAYRADDR5
           MOVE WK-C-VCFA-ADDR6   TO WK-VTD1-PAYRADDR6
           MOVE WK-C-VCFA-AOCD    TO WK-VTD1-AOCD
           MOVE WK-N-VCFA-RESCD   TO WK-VTD1-RESCD
           MOVE WK-N-VCFA-DOMBRCH TO WK-VTD1-DOMBRCH
           MOVE WK-N-VCFA-HOLDCD1 TO WK-VTD1-HOLDCD1
           MOVE WK-N-VCFA-HOLDCD2 TO WK-VTD1-HOLDCD2
           MOVE WK-N-VCFA-HOLDCD3 TO WK-VTD1-HOLDCD3
                                                  END-IF
                                               END-IF
           IF (TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
           OR TFSSTPL-CUYCD = WK-C-LCUYCD
           OR (TFSSTPL-CUYCD NOT = TFSCLSYS-LCUYCD
           OR TFSSTPL-CUYCD NOT = WK-C-LCUYCD)
           AND WK-C-VCFA-ERROR-CD NOT = SPACES)
           MOVE "N" TO WS-OKAY TAB-VL2(05)
           INITIALIZE WK-C-RPRRSN-AREA
           IF WK-C-VCCA-ERROR-CD NOT = SPACES
           MOVE WK-C-VCCA-ERROR-CD TO WK-C-RPRCODE
           MOVE WK-C-VCCA-SEG-CODE TO WK-C-SEGCDE
           MOVE WK-N-VCCA-STAFFIND TO WK-N-STAFFIND
           MOVE WK-C-VCCA-CUSTFNAM TO WK-C-ACCNAME
                                               ELSE
           MOVE WK-C-VCFA-ERROR-CD TO WK-C-RPRCODE
           MOVE WK-C-VCFA-SEG-CODE TO WK-C-SEGCDE
           MOVE WK-N-VCFA-STAFFIND TO WK-N-STAFFIND
           MOVE WK-C-VCFA-CUSTFNAM TO WK-C-ACCNAME
                                               END-IF
           PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX
                                               END-IF
           MOVE "N" TO WS-OKAY TAB-VL2(05)
                                            INITIALIZE WK-C-RPRRSN-AREA
           MOVE "RSN0091" TO WK-C-RPRCODE
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX

                                            END-IF
SM1TY1                                      ELSE
SM1TY1                                   MOVE "N"        TO WS-OKAY
SM1TY1                                   MOVE "X"        TO TAB-VL2(05)
5Q1JM1                                   INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1                                   MOVE "RSN0035"  TO WK-C-RPRCODE
5Q1JM1                                   PERFORM E002-PROCESS-RPRRSN
5Q1JM1                                      THRU E002-PROCESS-RPRRSN-EX
SM1TY1                                   END-IF
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(06) NOT = "X" AND WS-OKAY = "Y"
                                      MOVE TAB-VAL(06)   TO TAB-VL2(06)
           REM269**** IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
REM269                                IF TFSSTPL-CUYCD = WK-C-LCUYCD
                                         IF WK-C-VCCA-ERROR-CD = SPACES
                                            MOVE "Y"        TO WS-ACT2
                                         ELSE
           MOVE "Y"        TO TAB-VL2(06)
                                            MOVE "N"        TO WS-OKAY
5Q1JM1                                      INITIALIZE WK-C-RPRRSN-AREA
           5Q1JM1                                      MOVE WK-C-VCCA-
      -    SEG-CODE TO WK-C-SEGCDE
           5Q1JM1                                      MOVE WK-N-VCCA-
      -    STAFFIND TO WK-N-STAFFIND
           5Q1JM1                                      MOVE WK-C-VCCA-
      -    CUSTFNAM TO WK-C-ACCNAME
           5Q1JM1                                      MOVE WK-C-VCCA-
      -    ERROR-CD TO WK-C-RPRCODE
5Q1JM1                                      PERFORM E002-PROCESS-RPRRSN
           5Q1JM1                                         THRU E002-
      -    PROCESS-RPRRSN-EX
                                            END-IF
                                         ELSE
                                         IF WK-C-VCFA-ERROR-CD = SPACES
                                            MOVE "Y"        TO WS-ACT2
                                         ELSE
           MOVE "Y"        TO TAB-VL2(06)
                                            MOVE "N"        TO WS-OKAY
5Q1JM1                                      INITIALIZE WK-C-RPRRSN-AREA
           5Q1JM1                                      MOVE WK-C-VCFA-
      -    SEG-CODE TO WK-C-SEGCDE
           5Q1JM1                                      MOVE WK-N-VCFA-
      -    STAFFIND TO WK-N-STAFFIND
           5Q1JM1                                      MOVE WK-C-VCFA-
      -    CUSTFNAM TO WK-C-ACCNAME
           5Q1JM1                                      MOVE WK-C-VCFA-
      -    ERROR-CD TO WK-C-RPRCODE
5Q1JM1                                      PERFORM E002-PROCESS-RPRRSN
           5Q1JM1                                         THRU E002-
      -    PROCESS-RPRRSN-EX
                                            END-IF
                                         END-IF
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(07) NOT = "X" AND WS-OKAY = "Y"
                                      MOVE TAB-VAL(07)   TO TAB-VL2(07)
                                      MOVE "MAS"         TO WS-PMODE
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
           GP4A05*GP4A00 IF TFSSTPL-IMSGTYPE = "M"
           GP4A06*GP4A00 AND WS-RTGS-BYPASS-F53-F54-N
           GP4A06*GP4A00 AND WS-C-SWF-RTGS-SW = "Y"
           GP4A06*GP4A00 AND WK-C-GPI4-GL-SW = "Y"
GP4A05                                IF (TFSSTPL-IMSGTYPE = "M"

                                            AND SW-RTGS-BYPASS-F53-F54-N
                                            AND WS-C-SWF-RTGS-SW = "Y"
                                            AND WK-C-GPI4-GL-SW = "Y")
                                            OR WK-C-MAS-RTGS-SW = "Y"
                                         PERFORM D600-RTGS-VALIDATION
                                            THRU D699-RTGS-VALIDATION-EX
       END-IF.
                                      IF WK-C-GPI4STPLSW = "Y"
                                            AND (WS-ACCTYP = "C" OR "F")
           PERFORM D100-STPLIMIT-VALIDATION
           THRU D199-STPLIMIT-VALIDATION-EX
       END-IF.
                                      IF TFSSTPL-IMGSTYPE = "M"
                                            AND SW-RTGS-BYPASS-F53-F54-Y
           MOVE "RTGS"           TO WS-PMODE
           PERFORM D300-LOGGING  THRU D399-LOGGING-EX
                                         GO TO D199-VALIDATION-EX
       END-IF.
       D199-VALIDATION-EX.
       EXIT.
                                         EJECT
       D100-STPLIMIT-VALIDATION.
           MOVE "A1"            TO WS-LINK-STATUS
                                      IF WS-ACCNO NOT = SPACES
                                         INITIALIZE WK-C-RPRRSN-AREA
                                         INITIALIZE WK-C-LINK-LIMIT
           MOVE TFSSTPL-BNKENTTY TO WS-LINK-BNKENTTY
           MOVE WS-ACCNO       TO WS-LINK-ACCNO
           MOVE TFSSTPL-CUYCD  TO WS-LINK-CCY
           MOVE TFSSTPL-AMT    TO WS-LINK-AMT
           MOVE "I"            TO WS-LINK-REMIND
           CALL "TRFVLMT2" USING WK-C-LINK-LIMIT
                                         EVALUATE WS-LINK-STATUS
                                            WHEN "XX"
                                            MOVE "N" TO WS-OKAY
           MOVE "RSN0311" TO WK-C-RPRCODE
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX
                                            WHEN "AA"
                                            MOVE "N" TO WS-OKAY
           MOVE "RSN0312" TO WK-C-RPRCODE
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX

                                            WHEN "AC"
           MOVE "N"           TO WS-OKAY
      *-------> RSN0313:TRANS. AMOUNT IS > CIF STP LIMIT
           MOVE "RSN0313"     TO WK-C-RPRCODE
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX
                                            WHEN "AS"
           MOVE "N"           TO WS-OKAY
      *-------> RSN0314:TRANS. AMOUNT IS > SEGMENT STP LIMIT
           MOVE "RSN0314"     TO WK-C-RPRCODE
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX
                                            END-EVALUATE
       END-IF.
       D199-STPLIMIT-VALIDATION-EX.
       EXIT.

       D200-VALIDATION.
           MOVE WS-BANKID     TO WK-VTD1-BANKID.
           MOVE WS-RECBNKID   TO WK-VTD1-RECBNKID.
           MOVE WS-SNCDBNKID  TO WK-VTD1-SNCDBNKID.
           MOVE WS-ACCNO      TO WK-VTD1-BANKAC.
           MOVE WS-ACCTYP     TO WK-VTD1-BANKACTYP.
           MOVE WS-ACUDBUI    TO WK-VTD1-ACUDBUI.
           MOVE WS-PMODE      TO WK-VTD1-PMODE.
           MOVE TABLE-ARR2    TO WK-VTD1-DATAD1.
                                      IF TFSSTPL-SWFTMGTY = 101
                                            AND WS-C-M101STPIND = "Y"
           MOVE WK-C-CUPF-ACCTYP TO WK-VTD1-BANKACTYP
           MOVE "D"           TO WK-VTD1-ACUDBUI
           MOVE WS-T50-ACCNO  TO WK-VTD1-BANKAC
           MOVE SPACES        TO WK-VTD1-BANKID
       END-IF.

                                      IF WS-OKAY = "Y"
           MOVE WS-ACT1       TO WK-VTD1-ACT1
           MOVE WS-ACT2       TO WK-VTD1-ACT2
           MOVE "N"           TO WK-VTD1-ERROR-FOUND
                                      ELSE
           MOVE SPACES        TO WK-VTD1-ACT1
                                            TO WK-VTD1-ACT2
           MOVE "Y"           TO WK-VTD1-ERROR-FOUND
                                      END-IF
                                      MOVE "N"           TO WS-FLAG1.
           PERFORM D300-LOGGING THRU D399-LOGGING-EX.

       D299-VALIDATION-EX.
       EXIT.
                                         EJECT

       D300-LOGGING.
           MOVE WK-VTD1-PARALNO TO WK-LOGG-PARALNO.
           MOVE WK-VTD1-SEQNUM  TO WK-LOGG-SEQNUM.

           MOVE TABLE-ARR2         TO WK-LOGG-DATA1.
           MOVE "D1"               TO WK-LOGG-TABTYP.
           MOVE WK-VTD1-ACT        TO WK-LOGG-ACTD1.
                                      CALL "TRFLOGGCL" USING WK-LOGG
                                         WS-FLAG1
       WS-FLAG2.
                                      IF WK-LOGG-ERROR-FOUND = "Y"
                                         GO TO D399-LOGGING-EX
       END-IF.
       D399-LOGGING-EX.
       EXIT.
                                         EJECT
       D400-SHIFT-VALIDATION.
                                      IF TFSSTPL-SENBNKID NOT = SPACES
           MOVE TFSSTPL-SENBNKID TO WK-C-VBBAS-BANKID
           CALL "TRFVBBASM" USING WK-C-VBBAS-RECORD
           IF WK-C-VBBAS-SHIFTNO NOT = SPACES
                                            MOVE "Y" TO SHIFT-IND
           MOVE TFSSTPL-BNKENTTY TO TFSBNKET-BNKENTTY
           READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
                                               INVALID KEY
                                            MOVE "N" TO SHIFT-IND
                                            INITIALIZE WK-C-RPRRSN-AREA
           MOVE "RSN089" TO WK-C-RPRCODE
                                            PERFORM E002-PROCESS-RPRRSN
           THRU E002-PROCESS-RPRRSN-EX
                                               NOT INVALID KEY
           MOVE TFSBNKET-MASONSTR TO WS-ACCNO
                                            END-READ
                                         ELSE
                                         MOVE "N" TO SHIFT-IND
                                         INITIALIZE WK-C-RPRRSN-AREA
                                         MOVE "RSN089" TO WK-C-RPRCODE
                                         PERFORM E002-PROCESS-RPRRSN
                                            THRU E002-PROCESS-RPRRSN-EX
                                         END-IF
       END-IF.
       D499-SHIFT-VALIDATION-EX.
       EXIT.
                                      EJECT

      *      MOVE TFSBNKET-MASNOSTR TO WS-ACCNO
      *      END-READ
      *      END-IF.
           MOVE TFSSTPL-BNKENTTY TO WK-N-VBAC-BNKENTTY.
           MOVE TFSSTPL-SENBNKID TO WK-C-VBAC-BANKID.
           MOVE TFSSTPL-CUYCD TO WK-C-VBAC-CUYCD.
           CALL "TRFVBAC" USING WK-C-VBAC-RECORD.
                                   IF WK-C-VBAC-ERROR-CD = SPACES
           MOVE WK-C-VBAC-ACUDBUI TO WS-ACUDBUI
                                      MOVE WK-C-VBAC-ACCTYP TO WS-ACCTYP
           IF WK-C-VBAC-BNKACNO NOT = SPACES AND
           WK-C-VBAC-BNKACNO(WK-N-ACCLEN:1) = SPACES
           MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
                                      ELSE
           MOVE WK-C-VBAC-BNKACNO(4:) TO WS-ACCNO
                                      END-IF
                                   ELSE
                                      MOVE "N" TO WS-OKAY
                                         WK-RTGS-BNKAC-EXIST
                                      INITIALIZE WK-C-RPRRSN-AREA
           MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
       END-IF.
       D599-RTGS-VALIDATION-EX.
       EXIT.
                                      EJECT
       D600-RTGS-VALIDATION.
      *Retrieve RTGS Mode pay.
                                   INITIALIZE WK-C-VDRTGS-RECORD.
           MOVE TFSSTPL-PROCUINT TO WK-C-VDRTGS-PU.
           MOVE TFSSTPL-CUYCD TO WK-C-VDRTGS-CUY.
                                   IF TFSSTPL-AMT IS NUMERIC
           MOVE TFSSTPL-AMT TO WK-C-VDRTGS-AMT
                                   ELSE
                                      MOVE ZERO TO WK-C-VDRTGS-AMT
       END-IF.
           CALL "TRFVDRTGS" USING WK-C-VDRTGS-RECORD.
                                   IF WK-C-VDRTGS-RTGSCUYIND = "Y"
           MOVE WK-C-VDRTGS-RTGSTYPE TO WS-PMMODE
      *-------> Retrieve corresponding RTGS GL No.
           INITIALIZE UFMGLPAYR OF UFMGLPAY-REC
                                      MOVE ZEROES TO WK-N-MAS
                                      MOVE SPACES TO WS-ACCNO
                                      MOVE WS-PMMODE TO UFMGLPAY-PAYMODE
           READ UFMGLPAY KEY IS EXTERNALLY-DESCRIBED-KEY
                                      IF WK-C-SUCCESSFUL
                                         MOVE UFMGLPAY-GLNO6 TO WK-N-MAS
                                         MOVE WK-N-MAS TO WS-ACCNO

           MOVE "N"            TO WS-ACCTYP
           PERFORM D300-LOGGING THRU D399-LOGGING-EX
                                         ELSE
                                         MOVE "N"            TO WS-OKAY
      *RSN0376: Invalid RTGS GL Account.
                                         INITIALIZE WK-C-RPRRSN-AREA
           MOVE "RSN0376"      TO WK-C-RPRCODE
                                         PERFORM E002-PROCESS-RPRRSN
                                            THRU E002-PROCESS-RPRRSN-EX
                                         END-IF
       END-IF.
       D699-RTGS-VALIDATION-EX.
       EXIT.

       E001-GET-TAG55.
      * get the option of incoming msg Tag 55 by calling common
      * routine TRFGTAG.
                                   MOVE SPACES         TO TAG55-OPT.
                                   INITIALIZE WK-C-GTAG-RECORD.
           MOVE WK-VTD1-PARALNO TO WK-N-GTAG-QUENUM.
           MOVE WK-VTD1-SEQNUM TO WK-N-GTAG-QUESUF.
           MOVE "55T"          TO WK-C-GTAG-TAGNO.
           CALL "TRFGTAG"      USING WK-C-GTAG-RECORD.
           MOVE WK-C-GTAG-OUT1(1:1) TO TAG55-OPT.
       E001-GET-TAG55-EX.
       EXIT.

                                      E002-PROCESS-RPRRSN SECTION.
       E002-ENTRY.
           MOVE WK-VTD1-PARALNO TO WK-C-RRSN-QUENUM.
           MOVE WK-VTD1-SEQNUM TO WK-C-RRSN-QUESUF.
           MOVE WK-C-TRNNO     TO WK-C-RRSN-TRNNO.
           MOVE WK-C-FUNCTID   TO WK-C-RRSN-FUNCTID.
           MOVE WK-C-SEGCODE   TO WK-C-RRSN-SEGCODE.
           MOVE SPACES         TO WK-C-RRSN-SEGDESC.
           MOVE WK-N-STAFFIND  TO WK-C-RRSN-STAFFIND.
           MOVE WS-ACCNO       TO WK-C-RRSN-ACCNO.
                                   IF TFSSPL-SWFTMGTY = 101
                                         AND WS-C-M101STPIND = "Y"
           MOVE WS-T50-ACCNO   TO WK-C-RRSN-ACCNO
       END-IF.
           MOVE WK-C-ACCNAME   TO WK-C-RRSN-ACCNAME.
           MOVE WK-C-QRATE     TO WK-C-RRSN-QRATE.
           MOVE WK-N-SYSRTE    TO WK-C-RRSN-RPRDTE.
           MOVE WK-C-RPRCODE   TO WK-C-RRSN-RNSCDE.
                                   IF WK-C-RPRCODE = SPACE
           MOVE "RSN9999"      TO WK-C-RRSN-RNSCDE
                                   ELSE
           MOVE WK-C-RPRCODE   TO WK-C-RRSN-RNSCDE
       END-IF.

           MOVE SPACES           TO WK-C-RRSN-RSNDESC.
           MOVE WK-C-RPRPGM      TO WK-C-RRSN-RPRPGM.
           CALL "TRFGRRSN" USING WK-C-RRSN-RECORD.
       E002-PROCESS-RPRRSN-EX.
       EXIT.
       E003-ACC-MANDATE-CHECK.
                                   INITIALIZE       WK-ACMN.
           MOVE TFSSTPL-BNKENTTY TO WK-ACMN-BANKCD.
                                   MOVE TFSSTPL-CUYCD TO WK-ACMN-CUYCD.
           MOVE WS-ACCNO        TO WK-ACMN-ACCNO.
           MOVE TFSSTPL-SENBNKID TO WK-ACMN-BANKID.
                                   IF WS-ACCNO NOT = SPACES AND
           WS-ACCNO(WK-N-ACCLEN:1) = SPACES
                                      MOVE WS-ACCNO    TO WK-ACMN-ACCNO
                                   ELSE
                                      MOVE WS-ACCNO(4:) TO WK-ACMN-ACCNO
       END-IF.
                                   CALL "TRFACMN" USING WK-ACMN.
                                   IF WK-ACMN-MANIND    = "N"
                                      MOVE "N"         TO WS-OKAY
                                         TAB-VL2(02)
                                      INITIALIZE WK-C-RPRRSN-AREA
                                      MOVE "RSN0097"   TO WK-C-RPRCODE
                                      PERFORM E002-PROCESS-RPRRSN
                                         THRU E002-PROCESS-RPRRSN-EX
       END-IF.
       E003-ACC-MANDATE-CHECK-EX.
       EXIT.
       R001-READ-UFIMIJ.
                                   INITIALIZE WS-T50-ACCNO
       UFIMIJ-REC-1.
           MOVE WK-VTD1-PARALNO TO UFIMIJ-PARALNO.
                                   MOVE WK-VTD1-SEQNUM TO UFIMIJ-SEQNUM.
           READ UFIMIJ KEY IS EXTERNALLY-DESCRIBED-KEY
                                      INVALID KEY
                                   GO TO R001-READ-UFIMIJ-EX
                                      NOT INVALID KEY
                                   MOVE UFIMIJ-IMSGBDY TO WK-C-MSGBDY1
       END-READ.
                                IF INF-101-S1-ORD-CUST1(1:1) = "/"
           MOVE INF-101-S1-ORD-CUST1(2:) TO WS-T50-ACCNO
                                ELSE
           MOVE INF-101-S1-ORD-CUST1 TO WS-T50-ACCNO
       END-IF.

CMP3A1                             R001-READ-UFIMIJ-EX.
CMP3A1                          EXIT.

       Z000-END-PROGRAM.
                                CLOSE TFSSTPL
                                   TFSBNKET
                                   TFSCLSYS
       TFS202V.
CMP3A1                          CLOSE UFIMIJ.
GP4A00                          CLOSE UFMGLPAY.
                                EXIT PROGRAM.
