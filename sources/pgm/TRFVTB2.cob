       IDENTIFICATION DIVISION.
      ***********************
       PROGRAM-ID. TRFVTB2.
       AUTHOR. TYK.
       DATE-WRITTEN. JUN 04.
      *DESCRIPTION : TABLE B2 VALIDATION.
      *              SUBROUTINE - CREDIT PARTY CHECKING FIELD 57/58
      *              FOR INCOMING MT202/203 LCY
      *
      *=================================================================
      * HISTORY OF MODIFICATION:
      *=================================================================
      * GP4A02 - VENADG  - 23/10/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              - GPI Day4 (POST IMPEM IMPROVEMENT)
      *              STP #1 (HK req) Inward SWIFT & RTGS
      *              JIRA PCRMAPKGPI-2395
      *              - BAU Bugfix
      *              - Rectified hardcoded "MAS"
      *                when crediting to RTGS
      *                Utilize TRFVDRTGS utility module
      *                to retrieve corresponding modepay.
      *-----------------------------------------------------------------
      * GP4D03 - VENTEH  - 16/10/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              GPI Day4 (POST IMPEM IMPROVEMENT)
      *              (For HK only)
      *              - STP #5 Inward (Inward receipt,
      *                IAFT & In-as-out) TT/RTGS- STP
      *                by CCY.
      *              - JIRA PCRMAPKGPI-2109
      *              - Bypass the STP currency setup
      *                checking to avoid double
      *                RSNCDE
      *-----------------------------------------------------------------
      * GP4A01 - VENADG  - 18/03/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              - GPI Day4 (In-Country Req)
      *              STP #1 (HK req) Inward SWIFT & RTGS
      *              UAT JIRA: PCRMAPKGPI-2296/2297
      *              - BAU Bugfix
      *              - Previously system is assigning
      *                harcoded "MAS" as Credit Modepay
      *              - Rectified to utilize utility
      *                TRFVDRTGS to retrieve RTGS modepay
      *-----------------------------------------------------------------
      * GP4D02 - VENTEH  - 22/07/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              GPI Day4 (In-Country Req)
      *              - STP #5 Inward (Inward receipt,
      *                IAFT & In-as-out) TT/RTGS- STP
      *                by CCY.
      *              - JIRA PCRMAPKGPI-1881
      *              - To cater negative scenario when
      *                ACC/CIF/SEG STP limit switch is OFF
      *                proceed to check STP by CCY
      *-----------------------------------------------------------------
      * GP3M01 - VENTEH  - 20/05/2020 - CASH MANAGEMENT ROAD MAP

      * ERROR: Could not extract page 2 - API refused to process (got:
      *    'I'm sorry, I can't a
      *      technical switch upon applying
      *      STP Limit Bypass Enhancement.
      *-----------------------------------------------------------------
      *    -----
      * GP3A00  09/10/2019 ACNJRR - GPI Day4 (Retro from GPI Day3 HO)
      *      - Tag57 Enhancement (Item5b)
      *      - To check Tag57 C/D Lines 1-5
      *        againts Tag Validation Table.
      *        This is to enable such tags to
      *        further proceed with STP processing
      *        if exact matches.
      *      05/12/2019 ACNJRR - PCRMAPKGPI-947
      *        Rectified to validate MT202 txn
      *        under Tag 57D.
      * GP3M00  05/12/2019 ACNESQ - Inward serial payment Bypass
      *      - STP Limit for Nostro (Item5a)
      *      - Bypass STP Limit if Dr Leg
      *        = NOSTRO and CR Leg = VOSTRO
      *-----------------------------------------------------------------
      *    -----
      * CMP3F1 - CMPESQ  - 14/02/2017 - CASH MANAGEMENT PROJECT 3
      *      - STP LIMIT BY ACC/CIF/SEGMENT
      *-----------------------------------------------------------------
      *    -----
      * 7Q1EM1 - TMPEYM  - 25/11 /2016 - REM Q1 2017 RELEASE
      *      - e-Req 47511 Refinement of
      *        Duplicate checking for Inw
      *      - Recompiled due to changes made in
      *        VSTPL copy book.
      *-----------------------------------------------------------------
      *    -----
      * STPGB1 - TMPGCB  - 29/09/2015 - UOBM OTT STP PROJECT
      *        RECOMPILED THE PROGRAM DUE TO
      *        THE CHANGES ON NSTP COPYBOOK.
      *-----------------------------------------------------------------
      *    -----
      * 5Q1JM1 - TMPJZM  - 23/12/2014 - 14HOREM024/14HOREM029/14HOREM028
      *        Retrofit NON-PSTP reason
      *        Enhancement Project
      *-----------------------------------------------------------------
      *    -----
      * SM1TY1 - TMPTY1  - 11/08/2005 - DON'T USE MAS ACCOUNT NUMBER
      *        FOR CR MAS/MAS202 TRANSACTION
      *=================================================================
      *    =====
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

              SELECT TFSBNKAC ASSIGN TO DATABASE-TFSBNKAC
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.

ID1VKE        SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
ID1VKE        ORGANIZATION IS INDEXED
ID1VKE        ACCESS MODE IS DYNAMIC
ID1VKE        RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
ID1VKE FILE STATUS IS WK-C-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
      ***************
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

       FD  TFSBNKAC
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFSBNKAC.
       01  WK-C-TFSBNKAC.
              COPY DDS-ALL-FORMATS OF TFSBNKAC.
       01  WK-C-TFSBNKAC-1.
              COPY TFSBNKAC.

ID1VKE FD  TFSBNKET
ID1VKE        LABEL RECORDS ARE OMITTED
ID1VKE DATA RECORD IS WK-C-TFSBNKET.
ID1VKE        01 WK-C-TFSBNKET.
ID1VKE        COPY DDS-ALL-FORMATS OF TFSBNKET.
ID1VKE        01 WK-C-TFSBNKET-1.
ID1VKE        COPY TFSBNKET.

       WORKING-STORAGE SECTION.
      ************************
       01 WK-C-COMMON.
              COPY ASCWMS.

       01 TAG56-FORMAT.
           05 TAG56-LINE-1.
           07 TAG56-FIL1    PIC X(2).
           07 TAG56-OPT     PIC X(1).
           07 TAG56-FIL2    PIC X(1).
           07 TAG56-PTID.
           09 TAG56-PTID-1  PIC X(02).
           09 TAG56-PTID-2  PIC X(35).
           05 TAG56-LINE-2      PIC X(35).
           05 TAG56-BIC REDEFINES TAG56-LINE-2.
           07 TAG56A-SUB1   PIC X(4).
           07 TAG56A-SUB2   PIC X(2).
           07 TAG56A-SUB3   PIC X(2).
           07 TAG56A-SUB4   PIC X(3).
           07 TAG56A-FILLER PIC X(24).
           05 TAG56-LOC REDEFINES TAG56-LINE-2
              PIC X(35).
           05 TAG56-NAME REDEFINES TAG56-LINE-2
              PIC X(35).
           05 TAG56-LINE-3      PIC X(35).
           05 TAG56-LINE-4      PIC X(35).
           05 TAG56-LINE-5      PIC X(35).

       01 TAG57-FORMAT.
           05 TAG57-LINE-1.
           07 TAG57-FIL1    PIC X(2).
           07 TAG57-OPT     PIC X(1).
           07 TAG57-FIL2    PIC X(1).
           07 TAG57-PTID.
           09 TAG57-PTID-1  PIC X(02).
           09 TAG57-PTID-2  PIC X(35).
           05 TAG57-LINE-2      PIC X(35).
           05 TAG57-BIC REDEFINES TAG57-LINE-2.
           07 TAG57A-SUB1   PIC X(4).
           07 TAG57A-SUB2   PIC X(2).
           07 TAG57A-SUB3   PIC X(2).
           07 TAG57A-SUB4   PIC X(3).
           07 TAG57A-FILLER PIC X(24).
           05 TAG57-LOC REDEFINES TAG57-LINE-2
              PIC X(35).

           05 TAG57-NAME REDEFINES TAG57-LINE-2
              PIC X(35).
           05 TAG57-LINE-3              PIC X(35).
           05 TAG57-LINE-4              PIC X(35).
           05 TAG57-LINE-5              PIC X(35).

       01 TAG58-FORMAT.
           05 TAG58-LINE-1.
           07 TAG58-FIL1            PIC X(2).
           07 TAG58-OPT             PIC X(1).
           07 TAG58-FIL2            PIC X(1).
           07 TAG58-PTID            PIC X(35).
           09 TAG58-PTID-1      PIC X(02).
           09 TAG58-PTID-2      PIC X(35).
           05 TAG58-LINE-2              PIC X(35).
           05 TAG58-BIC REDEFINES TAG58-LINE-2.
           07 TAG58A-SUB1           PIC X(4).
           07 TAG58A-SUB2           PIC X(2).
           07 TAG58A-SUB3           PIC X(2).
           07 TAG58A-SUB4           PIC X(3).
           07 TAG58A-FILLER         PIC X(24).
           05 TAG58-LOC REDEFINES TAG58-LINE-2
              PIC X(35).
           05 TAG58-NAME REDEFINES TAG58-LINE-2
              PIC X(35).
           05 TAG58-LINE-3              PIC X(35).
           05 TAG58-LINE-4              PIC X(35).
           05 TAG58-LINE-5              PIC X(35).

       01 TABLE-ARRAY.
           05 TAB-VAL OCCURS 20 TIMES   PIC X VALUE "X".

       01 TABLE-ARR2.
           05 TAB-VL2 OCCURS 20 TIMES   PIC X VALUE "X".

       01 PATH-P1                   PIC X(20)
              VALUE "XXNNYYXXXXXXXXXXXXXXX".
       01 PATH-P2                   PIC X(20)
              VALUE "XXNNYYXXXXXXXXXXXXXXX".
       01 PATH-P3                   PIC X(20)
              VALUE "XXYYYYXXXXXXXXXXXXXXX".
       01 PATH-P4                   PIC X(20)
              VALUE "XXYYYYXXXXXXXXXXXXXXX".
       01 PATH-P5                   PIC X(20)
              VALUE "XXNNYYXXXXXXXXXXXXXXX".
       01 PATH-P6                   PIC X(20)
              VALUE "XXYYYYXXXXXXXXXXXXXXX".
       01 PATH-P7                   PIC X(20)
              VALUE "XXYYYYXXXXXXXXXXXXXXX".
       01 PATH-P8                   PIC X(20)
              VALUE "XXNNYYXXXXXXXXXXXXXXX".
       01 PATH-P9                   PIC X(20)
              VALUE "XXYYYYXXXXXXXXXXXXXXX".
       01 PATH-P10                  PIC X(20)
              VALUE "XXYYYYXXXXXXXXXXXXXXX".

       01  PATH-P11 PIC X(20)
              VALUE "XXNYYXXXXXXXXXXXXXXXXXX".
       01  PATH-P12 PIC x(20)                        
              VALUE "XXYYNXXXXXXXXXXXXXXXXXXX".
       01  PATH-P13 PIC X(20)
              VALUE "XXNNYYXXXXXXXXXXXXXXXXXX".
       01  PATH-P14 PIC X(20)
              VALUE "XXNYYXXXXXXXXXXXXXXXXXX".
       01  PATH-P15 PIC x(20)
              VALUE "XXYYNXXXXXXXXXXXXXXXXXXX".
       01  PATH-P16 PIC X(20)
              VALUE "XXYYNXXXXXXXXXXXXXXXXXXX".
       01  PATH-P17 PIC X(20)
              VALUE "XXNYYXXXXXXXXXXXXXXXXXX".
       01  PATH-P18 PIC x(20)
              VALUE "XXYYNXXXXXXXXXXXXXXXXXXX".
       01  PATH-P19 PIC X(20)
              VALUE "XXYYNXXXXXXXXXXXXXXXXXXX".
       01  WK-C-PARADATA.
           05  WK-C-PARAVALU               PIC X(20).
           05  WK-N-PARAVALU               REDEFINES WK-C-PARAVALU
              PIC 9(13)V99.
           05  WK-N-IRMPSTP                PIC 9(13)V99.
           05  WK-N-IRM1STP                PIC 9(13)V99.

       01  WK-C-WORK-AREA.
           05  WS-FIRST-TIME               PIC X(01) VALUE "Y".
           05  WS-FLAG1                    PIC X(01) VALUE SPACE.
           05  WS-FLAG2                    PIC X(01) VALUE SPACE.
           05  RTG557-IND                  PIC X(01) VALUE SPACE.
           05  RTG558-IND                  PIC X(01) VALUE SPACE.
           05  BKAC57-IND                  PIC X(01) VALUE SPACE.
           05  BKAC58-IND                  PIC X(01) VALUE SPACE.
           05  WS-ACT1                     PIC X(01) VALUE SPACE.
           05  WS-ACT2                     PIC X(01) VALUE SPACE.
           05  WS-ACT3                     PIC X(01) VALUE SPACE.
           05  WS-OKAY                     PIC X(01) VALUE SPACE.
           05  WS-STYTPY                   PIC X(04) VALUE SPACE.
           05  WS-ACCNO                    PIC X(11) VALUE SPACE.
              GP4D01* 05  WS-ACBNKACC             PIC X(11) VALUE SPACE.
GP4D01     05  WS-ACCNO                PIC X(15) VALUE SPACE.
GP4D01     05  WS-ACBNKACC             PIC X(15) VALUE SPACE.
           05  WS-BENBKACC                 PIC X(35) VALUE SPACE.
           05  WS-BANKID                   PIC X(11) VALUE SPACE.
           05  WS-ACBNKID                  PIC X(11) VALUE SPACE.
           05  WS-BENBKID                  PIC X(11) VALUE SPACE.
CMP3F1     05  WS-C-STPLMT-FLAG        PIC X(01) VALUE "N".
G2BL00     05  WK-C-GPI-SW             PIC X(01) VALUE SPACE.
G2BL00     05  WK-C-BYPASS-LMT-IND     PIC X(01) VALUE SPACE.
G2BL00     05  WK-C-DR-PMODE           PIC X(08) VALUE SPACE.
GPA300     05  WK-C-GPI3-SW            PIC X(01) VALUE SPACE.
GPA300     05  WK-C-TAG57-CD-SW        PIC X(01) VALUE SPACE.

GP3M00     05  WK-C-NSLMT-SW                PIC X(01)  VALUE SPACE.
GP4D00     05  WK-C-STP-CCY-SW              PIC X(01)  VALUE SPACE.
GP4D03     05  WK-C-STP-CCY-IMP-SW          PIC X(01)  VALUE SPACE.

G2BL00        01  WK-C-LIT-GPI.
G2BL00     05  WK-C-Y                      PIC X(01)  VALUE "Y".
G2BL00     05  WK-C-GPI-SW-PARCD           PIC X(10)
G2BL00        VALUE "GPISWITCH2".
G2BL00     05  WK-C-STP-SW-PARCD           PIC X(10)
G2BL00        VALUE "GPISTPSW".
GP3A00     05  WK-C-GPI3-SW-PARCD          PIC X(10)
GP3A00        VALUE "GPISWITCH3".
GP3A00     05  WK-C-TAG57-SW-PARCD         PIC X(10)
GP3A00        VALUE "GPI3T57SW".
GP3A00     05  WK-C-TAG57-MT-PARCD         PIC X(10)
GP3A00        VALUE "GPI3T57MT".
GP3M00     05  WK-C-NSLMT-PARCD            PIC X(10)
GP3M00        VALUE "GPI3NSLMT".
GP4D00     05  WK-C-STPCCY-PARCD           PIC X(10)
GP4D00        VALUE "GPI4ISTPCY".
GP4A01     05  WS-C-RTGS-FCY-PARCD         PIC X(10)
GP4A01        VALUE "GPI4RTGENH".
GP4D03     05  WK-C-STPCCY2-PARCD          PIC X(10)
GP4D03        VALUE "GPI4STPCY2".
GP4A02     05  WS-C-MAS-RTGS-PARCD         PIC X(10)
GP4A02        VALUE "GPI4MASRTG".

GP3A00        01  WK-C-RTGS-FCY-SW            PIC X(01)  VALUE "N".
GP4A02        01  WK-C-MAS-RTGS-SW            PIC X(01)  VALUE "N".

GP3A00        01  WK-C-MT-TAG57-TBL           PIC X(18)  VALUE SPACES.
GP3A00     05  WK-C-MT-TAG57               PIC X(03)  OCCURS 6 TIMES.

GP4D01        01  SUB                         PIC 9(02)  VALUE ZEROES.
GP4D01        01  WK-N-CTR                    PIC 9(02)  VALUE ZEROES.
GP4D01        01  WK-N-ACCLEN                 PIC 9(02)  VALUE ZEROES.
GP4D01        01  WS-ACCNO1                   PIC X(11)  VALUE SPACES.

ID1VKE        01  WK-C-SWIFTBICDCE             PIC X(11)  VALUE SPACE.
ID1VKE        01  WK-C-RTGSBICDCE              PIC X(11)  VALUE SPACE.
5Q1JM1        01  WK-C-RPRRSN-AREA.
5Q1JM1     05  WK-C-SEGODE                 PIC X(01)  VALUE SPACE.
5Q1JM1     05  WK-N-STAFFIND               PIC S9(02) VALUE ZEROS.
5Q1JM1     05  WK-C-QRATE                  PIC X(02)  VALUE SPACE.
5Q1JM1     05  WK-C-RPRCODE                PIC X(07)  VALUE SPACE.
5Q1JM1     05  WK-C-TRNNO                  PIC X(12)  VALUE SPACE.
5Q1JM1     05  WK-C-FUNCTID                PIC X(08)  VALUE SPACE.
5Q1JM1 01  WK-N-SYSPTE                     PIC S9(08) VALUE ZEROS.
5Q1JM1 01  WK-C-RPRPGM                     PIC X(10) VALUE "TRFVTB2".
CMP3F1 01  WK-C-LINK-LIMIT.
CMP3F1     05  WK-C-LINK-AREA-INPUT.
CMP3F1     10  WS-LINK-BNKENTTY            PIC X(02).
CMP3F1     10  WS-LINK-ACCNO               PIC X(15)  VALUE 0.

CMP3F1     10  WS-LINK-CCY           PIC X(03) VALUE SPACES.
CMP3F1     10  WS-LINK-AMT           PIC S9(13)V99 VALUE 0.
CMP3F1     10  WS-LINK-REMIND        PIC X(01).
CMP3F1     05  WK-C-LINK-AREA-OUTPUT.
CMP3F1     10  WS-LINK-STATUS        PIC X(02) VALUE SPACES.

              COPY VCCA.
              COPY VSTPL.
              COPY VBAC.
              COPY VBBAS.
              COPY XPARA.
              COPY NSTP.
              COPY ACMN.
              COPY LOGG.
ID1VKC        COPY XGSPA.
5Q1JM1        COPY RRSN.
G2BL00        COPY GPISPTSW.
G2BL00        COPY VBACU.
GP3A00        COPY VTAGS7.
GP4D00        COPY VSTPC.
GP4A01        COPY VDRTGS.

       LINKAGE SECTION.
      ****************
              COPY VTB2.

       PROCEDURE DIVISION USING WK-VTB2.
      ********************************
       MAIN-MODULE.

           PERFORM A100-MAIN-PROGRAM THRU A199-MAIN-PROGRAM-EX
           GO TO Z000-END-PROGRAM.

       A100-MAIN-PROGRAM.

              GP4D00*CMP3F1* GET STP LIMIT INDICATOR
              GP4D00*CMP3F1    INITIALIZE WK-C-XGSPA-RECORD.
           GP4D00*CMP3F1    MOVE "RSYSSTPLMT"       TO WK-C-XGSPA-
      *    GHPARCD.
              GP4D00*CMP3F1    CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
              GP4D00*CMP3F1    IF WK-C-XGSPA-ERROR-CD  = SPACES
           GP4D00*CMP3F1       MOVE WK-C-XGSPA-GHPARVAL TO WS-C-STPLMT-
      *    FLAG
              GP4D00*CMP3F1    ELSE
              GP4D00*CMP3F1       MOVE SPACES TO WS-C-STPLMT-FLAG
              GP4D00*CMP3F1    END-IF.

           INITIALIZE WK-VTB2-OUTPUT
              WK-LOGG
       WK-C-WORK-AREA.
           MOVE ALL "X" TO TABLE-ARRAY.
           MOVE ALL "X" TO TABLE-ARR2.
           MOVE "Y" TO FIRST-TIME.

G2BL00     MOVE WK-VTB2-DR-PMODE TO WK-C-DR-PMODE.

           MOVE ZEROS            TO WK-C-RRSN-QUENUM
              WK-C-RRSN-QUESUF
              WK-C-RRSN-STAFFIND
              WK-C-RRSN-SEQNUM
       WK-C-RRSN-RPRDTE.

           IF FIRST-TIME = "Y"
              OPEN INPUT TFSSTPL
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVTB2 - OPEN FILE ERROR - TFSSTPL"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT TFSCLSYS
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVTB2 - OPEN FILE ERROR - TFSCLSYS"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT TFSBNKAC
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVTB2 - OPEN FILE ERROR - TFSBNKAC"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
ID1VKE        OPEN INPUT TFSBNKET
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVTB2 - OPEN FILE ERROR - TFSBNKET"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              INITIALIZE           WK-C-XGSPA-RECORD
              MOVE "STPRTGSBIC"    TO WK-C-XGSPA-GHPARCD
              CALL "TRFXGSPA"      USING WK-C-XGSPA-RECORD
              IF WK-C-XGSPA-ERROR-CD = SPACES THEN
                 MOVE WK-C-XGSPA-GHPARVAL TO WK-C-RTGSBICCDE
              ELSE
                 MOVE SPACES      TO WK-C-RTGSBICCDE
              END-IF
ID1VKE     END-IF.

           MOVE WK-VTB2-PARALNO    TO TFSSTPL-PARALNO.
           MOVE WK-VTB2-SEQNUM     TO TFSSTPL-SEQNUM.

           READ TFSSTPL
              KEY IS EXTERNALLY-DESCRIBED-KEY.

           IF WK-C-SUCCESSFUL
              MOVE TFSSTPL-TAG56  TO TAG56-FORMAT
              MOVE TFSSTPL-TAG57  TO TAG57-FORMAT
              MOVE TFSSTPL-TAG58  TO TAG58-FORMAT
ID1VKE        MOVE TFSSTPL-BNKENTTY TO TFSBNKET-BNKENTTY
ID1VKE        READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY

ID1VKE           INVALID KEY
ID1VKE        MOVE SPACES            TO WK-C-SWIFTBICCDE
ID1VKE           NOT INVALID KEY
ID1VKE        MOVE TFSBNKNET-SWFTBNK TO WK-C-SWIFTBICCDE
ID1VKE     END-READ
           IF TAG56-OPT  = SPACES
                 AND TAG56-PTID = SPACES
                 AND TAG56-BIC  = SPACES
              PERFORM A200-INITIAL-SUBROUTINE
                 THRU A299-INITIAL-SUBROUTINE-EX
              PERFORM A300-MOVE-TAG-VALUES
                 THRU A399-MOVE-TAG-VALUES-EX
              PERFORM B100-PATH-CHOICE
                 THRU B199-PATH-CHOICE-EX
5Q1JM1        ELSE
5Q1JM1        INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1        MOVE "RSN0105"    TO WK-C-RPRCODE
5Q1JM1        PERFORM D500-PROCESS-RPRRSN
5Q1JM1           THRU D599-PROCESS-RPRRSN-EX
              END-IF
       END-IF.
       A199-MAIN-PROGRAM-EX.
       EXIT.

       A200-INITIAL-SUBROUTINE.
      *----------------------------------------------------------------*
      *    GET DATA FROM "TFSCLSYS" TABLE                              *
      *----------------------------------------------------------------*
           READ TFSCLSYS.
           IF NOT WK-C-SUCCESSFUL
              DISPLAY "TRFVTB2  - READ TFSCLSYS ERROR"
              DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
              GO TO Z000-END-PROGRAM.

      ************MOVE TFSCLSYS-SYSDTE TO L-N-G-SYSDTE.
5Q1JM1        MOVE TFSCLSYS-SYSDTE TO WK-N-SYSDTE.
      ************MOVE TFSCLSYS-LCNTRYCD TO L-C-G-L-CNTRYCD.
      ************MOVE TFSCLSYS-LCUYCD TO L-C-G-L-CUYCD.

      *----------------------------------------------------------------*
      *    GET SYSTEM PARAMETERS FOR PSTP & 1STP                       *
      *----------------------------------------------------------------*
              MOVE "IRMSGP" TO WK-C-XPARA-PARACD.
              CALL "TRFXPARA" USING WK-C-XPARA-RECORD.
              IF WK-C-XPARA-ERROR-CD NOT = SPACES
                 DISPLAY "TREEEDT  TRFXPARA ROUTINE ERROR"
                 DISPLAY "FILE STATUS - " WK-C-XPARA-FS
                 DISPLAY "ERROR ID   - " WK-C-XPARA-ERROR-CD
                 DISPLAY "KEY        - " WK-C-XPARA-INPUT
                 GO TO Z000-END-PROGRAM
              ELSE
                 MOVE WK-C-XPARA-PARAVALU
                    TO WK-C-PARAVALU

                 MOVE WK-N-PARAVALU
                    TO WK-N-IRM1STP
       END-IF.
              MOVE "IRMSG1" TO WK-C-XPARA-PARACD.
              CALL "TRFXPARA" USING WK-C-XPARA-RECORD.
              IF WK-C-XPARA-ERROR-CD NOT = SPACES
                 DISPLAY "TREEEDT - TRFXPARA ROUTINE ERROR"
                 DISPLAY "FILE STATUS - " WK-C-XPARA-FS
                 DISPLAY "ERROR ID - " WK-C-XPARA-ERROR-CD
                 DISPLAY "KEY - " WK-C-XPARA-INPUT
                 GO TO Z000-END-PROGRAM
              ELSE
                 MOVE WK-C-XPARA-PARAVALU
                    TO WK-C-PARAVALU
                 MOVE WK-N-PARAVALU
                    TO WK-N-IRM1STP
       END-IF.

           G2BL00*------------------------------------------------------
      *    ---------*
           G2BL00* RETRIEVE GPI TECHNICAL AND STP SWITCH FROM SYSTEM
      *    PARAMETER  *
       G2BL00* FILE VIA CALLING TRFXGSPA PROGRAM USING GPISWITCH
      *    PARAMETER  *
           G2BL00*------------------------------------------------------
      *    ---------*
                 G2BL00
G2BL00        INITIALIZE WK-C-XGSPA-RECORD
G2BL00           SW-STP-LMT-SKP.
                 G2BL00
G2BL00        MOVE WK-C-STP-SW-PARCD TO WK-C-XGSPA-GHPARCD.
G2BL00        CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
                 G2BL00
G2BL00        IF WK-C-XGSPA-ERROR-CD = SPACES
G2BL00           MOVE WK-C-XGSPA-GHPARVAL(2:1)
G2BL00              TO SW-STP-LMT-SKP
G2BL00        END-IF.
                 G2BL00
G2BL00        INITIALIZE WK-C-XGSPA-RECORD
G2BL00           WK-C-GPI-SW.
                 G2BL00
G2BL00        MOVE WK-C-GPI-SW-PARCD TO WK-C-XGSPA-GHPARCD.
G2BL00        CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
                 G2BL00
G2BL00        IF WK-C-XGSPA-ERROR-CD = SPACES
G2BL00           MOVE WK-C-XGSPA-GHPARVAL(1:1)
G2BL00              TO WK-C-GPI-SW
G2BL00        END-IF.

                 GP3A00*-->Retrieve GPI Day 3 Technical Switch
GP3A00        INITIALIZE WK-C-XGSPA-RECORD
GP3A00           WK-C-GPI3-SW.
                 GP3A00
GP3A00        MOVE WK-C-GPI3-SW-PARCD TO WK-C-XGSPA-GHPARCD.
GP3A00        CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
                 GP3A00
GP3A00        IF WK-C-XGSPA-ERROR-CD = SPACES
GP3A00           MOVE WK-C-XGSPA-GHPARVAL(1:1)

                    TO  WK-C-GPI3-SW
       END-IF.
      *
      *-->Retrieve GPI Day3 Tag57 C/D Enhancement Switch
              INITIALIZE  WK-C-XGSPA-RECORD
       WK-C-TAG57-CD-SW.
              MOVE WK-C-TAG57-SW-PARCD  TO  WK-C-XGSPA-GHPARCD.
              CALL "TRFXGSPA"  USING  WK-C-XGSPA-RECORD.
              IF  WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL(1:1)
                    TO  WK-C-TAG57-CD-SW
       END-IF.
      *
      *-->Retrieve GPI Day3 Tag57 C/D Enhancement Eligable MT Types
              INITIALIZE  WK-C-XGSPA-RECORD
       WK-C-MT-TAG57-TBL.
              MOVE WK-C-TAG57-MT-PARCD  TO  WK-C-XGSPA-GHPARCD.
              CALL "TRFXGSPA"  USING  WK-C-XGSPA-RECORD.
              IF  WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL
                    TO  WK-C-MT-TAG57-TBL
       END-IF.
      *
      *-->Retrieve GPI Day3 Nostro Bypass STP Limit Enhancement
              INITIALIZE  WK-C-XGSPA-RECORD
       WK-C-NSLMT-SW.
              MOVE WK-C-NSLMT-PARCD  TO  WK-C-XGSPA-GHPARCD.
              CALL "TRFXGSPA"  USING  WK-C-XGSPA-RECORD.
              IF  WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL(1:1)
                    TO  WK-C-NSLMT-SW
       END-IF.
      *
      *-->Retrieve GPI Day4 In-Country ITT STP by Currency Switch
              INITIALIZE  WK-C-XGSPA-RECORD
                 WK-C-STP-CCY-SW
              MOVE WK-C-STPCCY-PARCD  TO  WK-C-XGSPA-GHPARCD.
              CALL "TRFXGSPA"  USING  WK-C-XGSPA-RECORD.
              IF  WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL(1:1)
                    TO  WK-C-STP-CCY-SW
       END-IF.
      *
      * GET STP LIMIT INDICATOR
              INITIALIZE  WK-C-XGSPA-RECORD.
              MOVE "RSYSSTPLIMT"  TO  WK-C-XGSPA-GHPARCD.

              CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
              IF WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL TO WS-C-STPLMT-FLAG
              ELSE
                 MOVE SPACES TO WS-C-STPLMT-FLAG
       END-IF.
      * GET ACC LENGTH
              INITIALIZE WK-C-XGSPA-RECORD.
              MOVE "RSXACCLEN" TO WK-C-XGSPA-GHPARCD
              CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
              IF WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL(1:2) TO WK-N-ACCLEN
              ELSE
                 MOVE 13 TO WK-N-ACCLEN
       END-IF.
      *-->Retrieve GPI Day 4 HK Enhancement Switch
              INITIALIZE WK-C-XGSPA-RECORD
       WK-C-RTGS-FCY-SW.
              MOVE WS-C-RTGS-FCY-PARCD TO WK-C-XGSPA-GHPARCD.
              CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
              IF WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL(1:1)
                    TO WK-C-RTGS-FCY-SW
       END-IF.
      *-->Retrieve GPI Day 4 Replace MAS w/ RTGS Payment Mode
              INITIALIZE WK-C-XGSPA-RECORD
       WK-C-MAS-RTGS-SW.
              MOVE WS-C-MAS-RTGS-PARCD TO WK-C-XGSPA-GHPARCD.
              CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
              IF WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL(1:1)
                    TO WK-C-MAS-RTGS-SW
       END-IF.
      *-->Retrieve GPI Day4 ITT STP by Currency Improvement Switch
              INITIALIZE WK-C-XGSPA-RECORD
       WK-C-STP-CCY-IMP-SW.
              MOVE WK-C-STPCCY2-PARCD TO WK-C-XGSPA-GHPARCD.
              CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
              IF WK-C-XGSPA-ERROR-CD = SPACES
                 MOVE WK-C-XGSPA-GHPARVAL(1:1)
                    TO WK-C-STP-CCY-IMP-SW
              ELSE
                 MOVE "N" TO WK-C-STP-CCY-IMP-SW
       END-IF.

       A299-INITIAL-SUBROUTINE-EX.
       EXIT.

       A300-MOVE-TAG-VALUES.
              IF TAG57-BIC NOT = SPACES
                    AND TAG57-OPT = "A"
                 MOVE TAG57-BIC       TO WS-ACBNKID
                    WS-BANKID
                 MOVE TAG57-PTID      TO WS-ACBNKACC
                    WS-ACCNO
                    GP4D01*-->    Remove routing code of tag 57 ACCNO
GP4D01           IF WS-ACCNO NOT = SPACES
GP4D01              MOVE ZEROS        TO WK-N-CTR
GP4D01              MOVE SPACES       TO WS-ACCNO1
GP4D01              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 15
GP4D01                 IF WS-ACCNO(SUB:1) NOT = SPACES
GP4D01                    ADD 1       TO WK-N-CTR
GP4D01                 END-IF
GP4D01              END-PERFORM
GP4D01              IF WK-N-CTR = WK-N-ACCLEN
GP4D01                 MOVE WS-ACCNO(4:10) TO WS-ACCNO1
GP4D01                 MOVE WS-ACCNO1 TO WS-ACCNO
GP4D01              END-IF
GP4D01           END-IF
       END-IF.

                 GP3A00*-->GPI Day3 Tag57 C/D Enhancement
GP3A00        IF WK-C-GPI3-SW = "Y"
GP3A00              AND WK-C-TAG57-CD-SW = "Y"
GP3A00           IF TAG57-OPT = "C" OR "D"
GP3A00                 AND (TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(1)
GP3A00                 OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(2)
GP3A00                 OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(3)
GP3A00                 OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(4)
GP3A00                 OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(5)
GP3A00                 OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(6))
GP3A00              PERFORM D600-EVAL-TAG57-CD
GP3A00                 THRU D699-EVAL-TAG57-CD-EX
GP3A00              END-IF
GP3A00           END-IF.

              IF TAG58-BIC NOT = SPACES
                    AND TAG58-OPT = "A"
                 MOVE TAG58-BIC       TO WS-BENBKID
                    WS-BANKID
                 MOVE TAG58-PTID      TO WS-BENBKACC
                    WS-ACCNO
       END-IF.

              IF TAG58-NAME NOT = SPACES
                    AND TAG58-OPT = "D"
SM1TY1           MOVE TAG58-PTID      TO WS-BENBKACC
                    WS-ACCNO

                 MOVE TAG58-NAME        TO WK-VTB2-BENBKNM
                 MOVE TAG58-LINE-3      TO WK-VTB2-BENBKADR1
                 MOVE TAG58-LINE-4      TO WK-VTB2-BENBKADR2
                 MOVE TAG58-LINE-5      TO WK-VTB2-BENBKADR3
       END-IF.

              MOVE FSSTPL-BNKENTTY   TO WK-N-VBAC-BNKENTTY.
              MOVE WS-BANKID         TO WK-C-VBAC-BANKID.
              MOVE FSSTPL-CUYCD      TO WK-C-VBAC-CUYCD.
              CALL "TRFVBAK" USING WK-C-VBAC-RECORD.
              IF WK-C-VBAC-ACUDUBUI NOT = SPACES
                 MOVE WK-C-VBAC-ACUDUBUI TO WK-VTB2-ACUDUBUI
                 IF WS-ACCNO = SPACES
                    MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
                 END-IF
              ELSE
                 MOVE "D"           TO WK-VTB2-ACUDUBUI
       END-IF.

       A399-MOVE-TAG-VALUES-EX.
       EXIT.
                 EJECT

       B100-PATH-CHOICE.
              MOVE "N"           TO WS-OKAY.
              PERFORM D400-RTGS-VALIDATION.
              IF TAG57-OPT       = SPACES
                    AND TAG57-PTID     = SPACES
                    AND TAG57-BIC      = SPACES
                    OR TAG57-OPT       = "A"
                    ID1VKE*   AND TAG57-BIC      = "UOVBSGSGXXX"
ID1VKE              AND TAG57-BIC      = WK-C-SWIFTBICCDE
                 PERFORM C100-VALIDATION-PART
                    THRU C199-VALIDATION-PART-EX
       END-IF.
              IF TAG57-OPT       = "A"
                    ID1VKE*   AND TAG57-BIC      = "MASGSGSGXXX"
ID1VKE              AND TAG57-BIC      = WK-C-RTGSBICCDE
                 PERFORM C200-VALIDATION-PART
                    THRU C299-VALIDATION-PART-EX
       END-IF.

              IF  RTGS57-IND  = "Y"
                    AND TAG57-OPT  = "A"
                    AND TAG57-BIC  NOT = "UOVBSGGXXX"
                    AND TAG57-BIC  NOT = WK-C-SWIFTBICDCE
                    AND TAG57-BIC  NOT = "MASGSGSGXXX"
                    AND TAG57-BIC  NOT = WK-C-RTGSBICDCE
                 PERFORM C300-VALIDATION-PART
                    THRU C399-VALIDATION-PART-EX
       END-IF.
              IF  RTGS57-IND  = "N"
                    AND BKAC57-IND  = "Y"
                    AND TAG57-OPT  = "A"
                    AND TAG57-BIC  NOT = "UOVBSGGXXX"
                    AND TAG57-BIC  NOT = WK-C-SWIFTBICDCE
                    AND TAG57-BIC  NOT = "MASGSGSGXXX"
                    AND TAG57-BIC  NOT = WK-C-RTGSBICDCE
                 PERFORM C400-VALIDATION-PART
                    THRU C499-VALIDATION-PART-EX
       END-IF.
              PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.
              PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.
       EXIT.

       C100-VALIDATION-PART.
              IF  TAG58-OPT  = "A"
                    AND TAG58-BIC  = SPACES
                    AND TAG58-BIC  NOT = "UOVBSGGXXX"
                    AND TAG58-BIC  NOT = WK-C-SWIFTBICDCE
                    AND TAG58-BIC  NOT = "MASGSGSGXXX"
                    AND TAG58-BIC  NOT = WK-C-RTGSBICDCE
                 IF  RTGS58-IND  = "Y"
                       AND BKAC58-IND  = "N"
                    MOVE PATH-P1  TO TABLE-ARRAY
                    MOVE "Y"  TO WS-ACT3
                       WS-OKAY
                 END-IF
                 IF  RTGS58-IND  = "Y"
                       AND BKAC58-IND  = "Y"
                    MOVE PATH-P13  TO TABLE-ARRAY
                    MOVE "Y"  TO WS-OKAY
                 END-IF
                 IF  RTGS58-IND  = "N"
                       AND BKAC58-IND  = "Y"
                    MOVE PATH-P14  TO TABLE-ARRAY
                    MOVE "Y"  TO WS-OKAY
                 END-IF
       END-IF.
       C199-VALIDATION-PART-EX.
       EXIT.
                 EJECT

       C200-VALIDATION-PART.

              IF TAG58-OPT    = "A"
                    AND TAG58-BIC   NOT = SPACES
                    AND TAG58-BIC   NOT = "UOVBSGSGXXX"
                    AND TAG58-BIC   NOT = WK-C-SWIFTBICCODE
                    AND TAG58-BIC   NOT = "MASGSGSGXXX"
                    AND TAG58-BIC   NOT = WK-C-RTGSBICCODE
                    AND RTGS58-IND  = "Y"
                    AND BKAC58-IND  = "N"
                 MOVE PATH-P2 TO TABLE-ARRAY
                 MOVE "Y"    TO WS-ACT3
                    WS-OKAY
       END-IF.
       C299-VALIDATION-PART-EX.
       EXIT.
                 EJECT
       C300-VALIDATION-PART.
              IF TAG58-OPT  NOT = "A"
                    AND NOT(TAG58-OPT = SPACES
                    AND TAG58-PTID = SPACES
                    AND TAG58-BIC  = SPACES)
                 MOVE PATH-P3 TO TABLE-ARRAY
                 MOVE "Y"    TO WS-ACT1
                    WS-OKAY
       END-IF.
              IF TAG58-OPT    = "A"
                    AND TAG58-BIC   NOT = "UOVBSGSGXXX"
                    AND TAG58-BIC   NOT = WK-C-SWIFTBICCODE
                    AND TAG58-BIC   NOT = "MASGSGSGXXX"
                    AND TAG58-BIC   NOT = WK-C-RTGSBICCODE
                 IF RTGS58-IND = "Y"
                       AND BKAC58-IND = "Y"
                    MOVE PATH-P4 TO TABLE-ARRAY
                    MOVE "Y"    TO WS-ACT1
                       WS-OKAY
                 END-IF
                 IF RTGS58-IND = "Y"
                       AND BKAC58-IND = "N"
                    MOVE PATH-P5 TO TABLE-ARRAY
                    MOVE "Y"    TO WS-ACT1
                       WS-OKAY
                 END-IF
                 IF RTGS58-IND = "N"
                       AND BKAC58-IND = "Y"
                    MOVE PATH-P6 TO TABLE-ARRAY
                    MOVE "Y"    TO WS-ACT1
                       WS-OKAY
                 END-IF
                 IF RTGS58-IND = "N"
                       AND BKAC58-IND = "N"
                    MOVE PATH-P7 TO TABLE-ARRAY
                    MOVE "Y"    TO WS-ACT1
                       WS-OKAY
                 END-IF

       END-IF.
       C399-VALIDATION-PART-EX.
       EXIT.
                 EJECT

       C400-VALIDATION-PART.
              IF  TAG58-OPT    NOT = "A"
                    AND NOT(TAG58-OPT = SPACES
                    AND TAG58-PTID   = SPACES
                    AND TAG58-BIC    = SPACES)
                 MOVE PATH-P8  TO TABLE-ARRAY
                 MOVE "Y"      TO WS-ACT2
                    WS-OKAY
              END-IF
              IF  TAG58-OPT    = "A"
                    AND TAG58-BIC    NOT = "UOVBSGSGXXX"
                    ID1VKE* AND TAG58-BIC NOT = WK-C-SWIFTBICDCE
                    ID1VKE* AND TAG58-BIC NOT = "MASGSGSGXXX"
                    ID1VKE* AND TAG58-BIC NOT = WK-C-RTGSBICDCE
                 IF  RTGS58-IND = "Y"
                       AND BKAC58-IND = "Y"
                    MOVE PATH-P9  TO TABLE-ARRAY
                    MOVE "Y"      TO WS-ACT2
                       WS-OKAY
                 END-IF
                 IF  RTGS58-IND = "Y"
                       AND BKAC58-IND = "N"
                    MOVE PATH-P10 TO TABLE-ARRAY
                    MOVE "Y"      TO WS-ACT2
                       WS-OKAY
                 END-IF
                 IF  RTGS58-IND = "N"
                       AND BKAC58-IND = "Y"
                    MOVE PATH-P11 TO TABLE-ARRAY
                    MOVE "Y"      TO WS-ACT2
                       WS-OKAY
                 END-IF
                 IF  RTGS58-IND = "N"
                       AND BKAC58-IND = "N"
                    MOVE PATH-P12 TO TABLE-ARRAY
                    MOVE "Y"      TO WS-ACT2
                       WS-OKAY
                 END-IF
       END-IF.
       C499-VALIDATION-PART-EX.
       EXIT.
                 EJECT

       D100-VALIDATION.
              MOVE "Y"      TO WS-FLAG1.

              IF  TABLE-ARRAY = ALL "X"
                 MOVE "N"      TO WS-OKAY
5Q1JM1           INITIALIZE WK-C-RPRRSN-AREA

5Q1JM1           MOVE "RSN0112"       TO  WK-C-RPRCODE
5Q1JM1           PERFORM D500-PROCESS-RPRRSN
5Q1JM1              THRU D599-PROCESS-RPRRSN-EX
5Q1JM1           END-IF.
                 G2BL00*G2BL00IF  WK-C-GPT-SW = WK-C-Y
G2BL00        IF  SW-STP-LMT-SKP-Y
G2BL00              AND FSSTPL-SWFTMGTY = "202"
G2BL00           PERFORM D110-VALIDATE-STP-BYPASS
G2BL00              THRU D119-VALIDATE-STP-BYPASS-EX
G2BL00           ELSE
G2BL00           MOVE SPACES           TO WK-C-BYPASS-LMT-IND
G2BL00           END-IF
G2BL00        IF  WK-C-BYPASS-LMT-IND = WK-C-Y
G2BL00           GO TO D101-SKIP-OTH-LIMIT
G2BL00        END-IF.
       G2BL00*G2BL00END-IF.
CMP3F1        IF  WS-C-STPLMT-FLAG = "Y"
CMP3F1           MOVE "A1"            TO WS-LINK-STATUS
CMP3F1           IF  WS-ACCNO NOT = SPACES
CMP3F1              INITIALIZE WK-C-RPRRSN-AREA
CMP3F1              INITIALIZE WK-C-LINK-LIMIT
CMP3F1              MOVE FSSTPL-BNKENTITY TO WS-LINK-BNKENTITY
CMP3F1              MOVE WS-ACCNO         TO WS-LINK-ACCNO
CMP3F1              MOVE FSSTPL-CUYCOD    TO WS-LINK-CCY
CMP3F1              MOVE FSSTPL-AMT       TO WS-LINK-AMT
CMP3F1              MOVE "I"              TO WS-LINK-REMIND
CMP3F1              CALL "TRFVLM" USING WK-C-LINK-LIMIT
CMP3F1              EVALUATE WS-LINK-STATUS
CMP3F1                 WHEN "XX"
CMP3F1                 MOVE "N"              TO WS-OKAY
CMP3F1                 MOVE "RSN0311"        TO WK-C-RPRCODE
CMP3F1                 PERFORM D500-PROCESS-RPRRSN
CMP3F1                    THRU D599-PROCESS-RPRRSN-EX
CMP3F1                 END-EVALUATE
CMP3F1                 END-IF
CMP3F1              END-IF.
G2BL00           0101-SKIP-OTH-LIMIT.
G2BL00        IF  TAB-VAL(01) NOT = "X"
G2BL00           MOVE TAB-VAL(01)     TO TAB-VL2(01)
G2BL00           PERFORM D300-LOGGING THRU D399-LOGGING-EX
G2BL00           END-IF.
G2BL00        IF  TAB-VAL(02) NOT = "X"
G2BL00           MOVE TAB-VAL(02)     TO TAB-VL2(02)
G2BL00           PERFORM D300-LOGGING THRU D399-LOGGING-EX
G2BL00           END-IF.
G2BL00        IF  TAB-VAL(03) NOT = "X"
G2BL00           MOVE TAB-VAL(03)     TO TAB-VL2(03)
G2BL00           PERFORM D300-LOGGING THRU D399-LOGGING-EX
G2BL00           END-IF.
G2BL00        IF  TAB-VAL(04) NOT = "X"
G2BL00           MOVE TAB-VAL(04)     TO TAB-VL2(04)

                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
              IF TAB-VAL(05) NOT = "X" AND WS-OKAY = "Y"
                 MOVE "Y"        TO TAB-VL2(05)
                    CMP3F1*   IF TFSTPL-AMT <= WK-N-IRM1STP
CMP3F1           IF (TFSTPL-AMT <= WK-N-IRM1STP
CMP3F1                 AND WS-C-STPLMT-FLAG NOT = "Y")
CMP3F1                 OR (TFSTPL-AMT <= WK-N-IRM1PSTP
CMP3F1                 AND WS-C-STPLMT-FLAG = "Y"
CMP3F1                 AND WS-LINK-STATUS = "A1")
CMP3F1                 OR (WS-LINK-STATUS = "A0"
CMP3F1                 AND WS-C-STPLMT-FLAG = "Y")
                       G2BL01*G2BL00 OR (WK-C-GPI-SW = WK-C-Y
                       G2BL01*G2BL00 AND SW-STP-LMT-SKP-Y
G2BL01                 OR (SW-STP-LMT-SKP-Y
G2BL00                 AND WK-C-BYPASS-LMT-IND = WK-C-Y)
                    MOVE "PSTP" TO WS-STPTYP
                    MOVE "N"    TO TAB-VL2(05)
                    MOVE "X"    TO TAB-VL2(06)
                 END-IF
           GP4D00*-->If within Parm STP Limit and w/out Acc/CIF/Seg STP
      *    Lmt setup
                    GP4D00*-->further check STT STP CCY table.
GP4D00           IF WK-C-STP-CCY-SW = "Y"
GP4D00                 AND TFSTPL-AMT <= WK-N-IRM1PSTP
                       GP4D00*GP4D02 AND WS-C-STPLMT-FLAG = "Y"
                       GP4D00*GP4D02 AND WS-LINK-STATUS = "A1"
GP4D02                 AND ((WS-C-STPLMT-FLAG = "Y"
GP4D02                 AND WS-LINK-STATUS = "A1")
GP4D02                 OR WS-C-STPLMT-FLAG NOT = "Y")
GP4D00              PERFORM D120-EVAL-STP-CCY
GP4D00                 THRU D120-EVAL-STP-CCY-EX
GP4D00              END-IF
                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
CMP3F1           IF WS-LINK-STATUS = "A0"
CMP3F1                 AND WS-C-STPLMT-FLAG = "Y"
CMP3F1              GO TO D199-VALIDATION-EX
CMP3F1           END-IF
       END-IF.
              IF TAB-VAL(06) NOT = "X" AND WS-OKAY = "Y"
                    G2BL01*G2BL00IF (WK-C-GPI-SW = WK-C-Y
                    G2BL01*G2BL00 AND SW-STP-LMT-SKP-Y
G2BL00           IF (SW-STP-LMT-SKP-Y
G2BL00                 AND WK-C-BYPASS-LMT-IND = WK-C-Y)
G2BL00              CONTINUE
                 ELSE
                    IF TFSTPL-AMT > WK-N-IRM1STP
                       MOVE "2STP" TO WS-STPTYP
                       MOVE "Y"    TO TAB-VL2(06)
5Q1JM1                 INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1                 MOVE "RSN0023" TO WK-C-RPRRCODE
5Q1JM1                 PERFORM D500-PROCESS-RPRRSN
5Q1JM1                    THRU D599-PROCESS-RPRRSN-EX
                       ELSE
                       IF TFSTPL-AMT > WK-N-IRM1PSTP

CMP3F1                       OR  (WS-C-STPLMT-FLAG  =  "Y"
CMP3F1                       AND  (WS-LINK-STATUS  =  "AA"
CMP3F1                       OR  WS-LINK-STATUS  =  "AC"
CMP3F1                       OR  WS-LINK-STATUS  =  "AS"))
CMP3F1                    MOVE  "1STP"  TO  WS-STPTYP
CMP3F1                    MOVE  "N"  TO  TAB-VL2(06)
5Q1JM1                    INITIALIZE  WK-C-RPRRSN-AREA
5Q1JM1                    MOVE  "RSN0039"  TO  WK-C-RPRCODE
5Q1JM1                    PERFORM  D500-PROCESS-RPRRSN
5Q1JM1                       THRU  D599-PROCESS-RPRRSN-EX
                          END-IF
                       END-IF
G2BL00                 END-IF
                 PERFORM  D300-LOGGING  THRU  D399-LOGGING-EX
       END-IF.
       D199-VALIDATION-EX.
       EXIT.
                 EJECT
G2BL00           D110-VALIDATE-STP-BYPASS.
           G2BL00*------------------------------------------------------
      *    -------*
           G2BL00* THIS WILL CALL TRFVBACU TO CHECK IF THE UOB BRANCH
      *    IND = Y  *
           G2BL00*------------------------------------------------------
      *    -------*
                 G2BL00
G2BL00        MOVE  SPACES  TO  WK-C-BYPASS-LMT-IND.
                 G2BL00
           G2BL00*--Bypass STP Limit if Debit Leg is a VOSTRO account
G2BL00        IF  WK-C-DR-PMODE  =  "CA"
G2BL00              OR  WK-C-DR-PMODE  =  "FCCA"
G2BL00           MOVE  WK-C-Y  TO  WK-C-BYPASS-LMT-IND
G2BL00           GO  TO  D119-VALIDATE-STP-BYPASS-EX
G2BL00        END-IF.
                 G2BL00
           GP3M00*--Bypass STP Limit if Dr Leg = NOSTRO and CR Leg =
      *    VOSTRO
GP3M00        IF  WK-C-GP13-SW  =  WK-C-Y
GP3M00              AND  WK-C-NSLMT-SW  =  WK-C-Y
                    GP3M00*GP3M01    IF  WK-C-DR-PMODE  =  "NOSTRO"
GP3M01           IF  (WK-C-DR-PMODE  =  "NOSTRO"
GP3M01                 OR  WK-C-DR-PMODE(1:4)  =  "RTGS")
GP3M00                 AND  (WK-C-VBAC-ACCTYP  =  "C"
GP3M00                 OR  WK-C-VBAC-ACCTYP  =  "F")
GP3M00              MOVE  WK-C-Y  TO  WK-C-BYPASS-LMT-IND
GP3M00              GO  TO  D119-VALIDATE-STP-BYPASS-EX
GP3M00           END-IF
GP3M00        END-IF.
                 GP3M00
           G2BL00*--Check if Sending BankID is a Nostro - UOB Branch
G2BL00        INITIALIZE  WK-C-VBACU-RECORD.
                 G2BL00
G2BL00        MOVE  TFSSTPL-SENBNKID  TO  WK-C-VBACU-BANKID.
G2BL00        CALL  "TRFVBACU"  USING  WK-C-VBACU-RECORD.
                 G2BL00
G2BL00        IF  WK-C-VBACU-ERROR-CD  =  SPACES
G2BL00           IF  WK-C-VBACU-UOBBRH  =  WK-C-Y

G2BL00              MOVE WK-C-Y           TO  WK-C-BYPASS-LMT-IND
G2BL00           ELSE
G2BL00              MOVE SPACES           TO  WK-C-BYPASS-LMT-IND
G2BL00           END-IF.
G2BL00        END-IF.
G2BL00           D119-VALIDATE-STP-BYPASS-EX.
G2BL00        EXIT.
G2BL00           EJECT

GP4D00           D120-EVAL-STP-CCY.
           GP4D00*------------------------------------------------------
      *    ----------*
           GP4D00* This routine will call TRFVSTPC to check if CCY is
      *    eligable   *
           GP4D00* for STP and if AMT is within CCY STP Limit.
      *    *
           GP4D00*   A0 = Currency is setup and within limit
      *    *
           GP4D00*   A1 = Currency is setup however exceeds STP Limit
      *    *
           GP4D00*   A2 = Currency is NOT setup
      *    *
           GP4D00*------------------------------------------------------
      *    ----------*
GP4D00        INITIALIZE          WK-C-VSTPC-RECORD
GP4D00           WK-C-RPRRSN-AREA.
GP4D00        MOVE  TFSSTPL-IMSGTYPE TO  WK-C-VSTPC-I-IMSGTYPE.
GP4D00        MOVE  TFSSTPL-CUYCD    TO  WK-C-VSTPC-I-CUYCD.
GP4D00        MOVE  TFSSTPL-AMT      TO  WK-N-VSTPC-I-AMT.
GP4D00        CALL  "TRFVSTPC"       USING WK-C-VSTPC-RECORD.
                 GP4D00
GP4D00        IF    WK-C-VSTPC-ERROR-CD = SPACES
GP4D00           CONTINUE
GP4D00        ELSE
GP4D00           GO TO D120-EVAL-STP-CCY-EX
GP4D00        END-IF.
                 GP4D00
GP4D00        EVALUATE WK-C-VSTPC-STATUS
GP4D00           WHEN "A0"
GP4D00           GO TO D120-EVAL-STP-CCY-EX
GP4D00           WHEN "A1"
           GP4D00*------------------------------------------------------
      *    ----------*
           GP4D00*--------->RSN0370 - Inward STP CCY: Beyond CCY STP
      *    Limit       *
GP4D00           MOVE "RSN0370" TO WK-C-RPRCODE
GP4D00           WHEN "A2"
           GP4D03*--------->Improvement: Checking of STP currency has
      *    move to ITT *
           GP4D03* Handler
      *    *
GP4D03           IF WK-C-STP-CCY-IMP-SW = "Y"
GP4D03              GO TO D120-EVAL-STP-CCY-EX
GP4D03           ELSE
           GP4D00*------------------------------------------------------
      *    ----------*
           GP4D00*--------->RSN0369 - Inward STP CCY: Non STP Currency.
      *    *
GP4D00              MOVE "RSN0369" TO WK-C-RPRCODE
GP4D03           END-IF
GP4D00           WHEN OTHER
GP4D00           GO TO D120-EVAL-STP-CCY-EX
GP4D00           END-EVALUATE.
                 GP4D00
GP4D00        MOVE SPACES TO WS-STPTYP.
GP4D00        MOVE "N" TO TAB-VL2(05)
GP4D00           TAB-VL2(06)

GP4D00           WS-OKAY.
                 GP4D00
GP4D00        PERFORM D500-PROCESS-RPRRSN
GP4D00           THRU D599-PROCESS-RPRRSN-EX.
GP4D00
GP4D00    D120-EVAL-STP-CCY-EX.
GP4D00          EXIT.
GP4D00
GP4D00    D200-VALIDATION.
GP4D00
              MOVE WS-BANKID        TO WK-VTB2-BANKID.
              MOVE WS-ACBNKID       TO WK-VTB2-ACBNKID.
              MOVE WS-BENBKID       TO WK-VTB2-BENBKID.
              MOVE WS-ACNO          TO WK-VTB2-BANKAC.
              MOVE WK-C-VBAC-ACCTYP TO WK-VTB2-BANKACTYP.
              MOVE WS-ACBNKACC      TO WK-VTB2-ACBNKACC.
              MOVE WS-BENBKACC      TO WK-VTB2-BENBKACC.
              MOVE TABLE-ARR2       TO WK-VTB2-DATAB2
SM1TY1*        IF TAG58-OPT = "A"
SM1TY1*           MOVE SPACES        TO WK-VTB2-BENBKACC
SM1TY1*        END-IF.
              IF WS-ACT1 = "Y"
                    OR WS-ACT3 = "Y"
                 MOVE WS-ACT1     TO WK-VTB2-ACT1
                 MOVE WS-ACT3     TO WK-VTB2-ACT3
                 MOVE "MAS202"    TO WK-VTB2-PMODE
GP4A01*----->Retrieve RTGS modepay from utility TRFVDRTGS
GP4A01           IF WK-C-RTGS-FCY-SW = "Y"
GP4A02                 OR WK-C-MAS-RTGS-SW = "Y"
GP4A01              PERFORM D600-RTGS-VALIDATION
GP4A01                 THRU D699-RTGS-VALIDATION-EX
GP4A01              END-IF
SM1TY1*           MOVE TFSSTPL-BNKENTTY  TO TFSBNKET-BNKENTTY
SM1TY1*           READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-
      *    KEY
SM1TY1*              NOT INVALID KEY
SM1TY1*              MOVE TFSBNKET-MASNOSTR TO WK-VTB2-BANKAC
SM1TY1*           END-READ
SM1TY1*           MOVE SPACES TO WK-VTB2-BANKAC
                 END-IF
              IF WS-ACT2 = "Y"
                 MOVE WS-ACT2     TO WK-VTB2-ACT2
                 MOVE "TT202"     TO WK-VTB2-PMODE
              END-IF
SM1TY1        IF TAG58-OPT = "A"
SM1TY1              AND WK-VTB2-PMODE NOT = "MAS202" AND "TT202"
SM1TY1           MOVE SPACES        TO WK-VTB2-BENBKACC
SM1TY1        END-IF.
GP4A01        IF TAG58-OPT = "A"
GP4A02*GP4A01AND WK-C-RTGS-FCY-SW = "Y"
GP4A02              AND (WK-C-RTGS-FCY-SW = "Y"
GP4A02              OR WK-C-MAS-RTGS-SW = "Y")
GP4A01              AND WK-VTB2-PMODE NOT = WK-C-VDRTGS-RTGSTYPE2
GP4A01              AND WK-VTB2-PMODE NOT = "MAS202"
GP4A01              AND WK-VTB2-PMODE NOT = "TT202"
GP4A01           MOVE SPACES        TO WK-VTB2-BENBKACC
GP4A01        END-IF.

              IF WS-OKAY = "Y"
                 MOVE WS-STPTYP    TO WK-VTB2-STPTYP
                 MOVE "N"          TO WK-VTB2-ERROR-FOUND
              ELSE
                 MOVE SPACES       TO WK-VTB2-ACT1
                    WK-VTB2-ACT2
                    WK-VTB2-ACT3
                    WK-VTB2-STPTYP
                 MOVE "Y"          TO WK-VTB2-ERROR-FOUND
       END-IF.
              MOVE "N"              TO WS-FLAG1.
              PERFORM D300-LOGGING  THRU D399-LOGGING-EX.

       D299-VALIDATION-EX.
       EXIT.

                 EJECT

       D300-LOGGING.
              MOVE WK-VTB2-PARALNO  TO WK-LOGG-PARALNO.
              MOVE WK-VTB2-SEQNUM   TO WK-LOGG-SEQNUM.
              MOVE TABLE-ARR2       TO WK-LOGG-DATAB2.
              MOVE "B2"             TO WK-LOGG-TABTYP.
              MOVE WK-VTB2-ACT      TO WK-LOGG-ACTB2.
              CALL "TRFLOGGCL" USING WK-LOGG
                 WS-FLAG1
       WS-FLAG2.
              IF WK-LOGG-ERROR-FOUND = "Y"
                 GO TO D399-LOGGING-EX
       END-IF.

       D399-LOGGING-EX.
       EXIT.

                 EJECT

       D400-RTGS-VALIDATION.
              IF NOT(TAG57-OPT = SPACES
                    AND TAG57-PTID = SPACES
                    AND TAG57-BIC = SPACES)
                    AND TAG57-OPT = "A"
                 IF TAG57-PTID = SPACES
                    MOVE TAG57-BIC TO WK-C-VBBAS-BANKID
                    CALL "TRFVBBAS" USING WK-C-VBBAS-RECORD
                    IF WK-C-VBBAS-SHIFTNO NOT = SPACES
                       MOVE "Y" TO RTGS57-IND
                       MOVE WK-C-VBBAS-SHIFTNO TO WK-VTB2-SHIFTNO
                    ELSE
                       MOVE "N" TO RTGS57-IND
                    END-IF
                 END-IF
              END-IF

              IF TAG57-PTID NOT = SPACES
                    OR RTGS57-IND = "N"
                 MOVE "N"           TO RTGS57-IND
                 MOVE TFSSTPL-BNKENTTY TO WK-N-VBAC-BNKENTTY
                 MOVE TAG57-BIC     TO WK-C-VBAC-BANKID
                 MOVE TFSSTPL-CUYCD TO WK-C-VBAC-CUYCD
                 CALL "TRFVBAC" USING WK-C-VBAC-RECORD
                 IF WK-C-VBAC-BNKACNO = TAG57-PTID
                       AND TAG57-PTID NOT = SPACES
                       AND WK-C-VBAC-ERROR-CD = SPACES
                       OR TAG57-PTID = SPACES
                       AND WK-C-VBAC-ERROR-CD = SPACES
                    MOVE "Y"       TO BKAC57-IND
                 ELSE
                    MOVE "N"       TO BKAC57-IND
                 END-IF
       END-IF.
              IF NOT(TAG58-OPT = SPACES
                    AND TAG58-PTID = SPACES
                    AND TAG58-BIC = SPACES)
                    AND TAG58-OPT = "A"
                 IF TAG58-PTID = SPACES
                    MOVE TAG58-BIC TO WK-C-VBBAS-BANKID
                    CALL "TRFVBBAS" USING WK-C-VBBAS-RECORD
                    IF WK-C-VBBAS-SHIFTNO NOT = SPACES
                       MOVE "Y"   TO RTGS58-IND
                       MOVE WK-C-VBBAS-SHIFTNO TO WK-VTB2-SHIFTNO
                    ELSE
                       MOVE "N"   TO RTGS58-IND
                    END-IF
                 ELSE
                    MOVE "N"       TO RTGS58-IND
                 END-IF
              END-IF
              MOVE TFSSTPL-BNKENTTY TO TFSBNKAC-BNKENTTY
              MOVE TAG58-BIC TO TFSBNKAC-BANKID
              START TFSBNKAC KEY IS >= EXTERNALLY-DESCRIBED-KEY
              READ TFSBNKAC NEXT RECORD
              IF TAG58-BIC NOT = TFSBNKAC-BANKID
                 MOVE "N"           TO BKAC58-IND
              ELSE
                 MOVE "Y"           TO BKAC58-IND
              END-IF
       END-IF.

       D499-RTGS-VALIDATION-EX.
       EXIT.
              EJECT

      *=================================================================
      *    *
GP4A01 D600-RTGS-VALIDATION.
      *=================================================================
      *    *
           GP4A01*------------------------------------------------------
      *    ----------*
           GP4A01*--This routine will call TRFVDRTGS utility module to
      *    retrieve
              GP4A01*--corresponding MODEPAY for RTGS Currency.
           GP4A01*--Most Countries LCY currency will have MODEPAY =
      *    "RTGS"

      *--For HK who currently considers 3 RTGS currency:
      *--  HKD = "RTGS"     USD = "RTGS1"
      *--  CNY = "RTGS2"
           INITIALIZE               WK-C-VDRTGS-RECORD.
           MOVE TFSSTPL-PROCUNIT    TO WK-C-VDRTGS-PU.
           MOVE TFSSTPL-CUYCD       TO WK-C-VDRTGS-CUY.
           CALL "TRFVDRTGS"         USING WK-C-VDRTGS-RECORD.
           IF WK-C-VDRTGS-RTGSCUYIND = "Y"
              IF WK-C-VDRTGS-RTGSTYPE2 = SPACE
                 MOVE "RTGS02"   TO WK-VTB2-PMODE
              ELSE
                 MOVE WK-C-VDRTGS-RTGSTYPE2
                    TO WK-VTB2-PMODE
              END-IF
       END-IF.
      *=================================================================
      *    *
       D699-RTGS-VALIDATION-EX.
      *=================================================================
      *    *
       EXIT.
      */      *=========================================================
      *    *
       D600-EVAL-TAG57-CD.
      *=========================================================*
      *--This routine will check Tag57 C/D Lines 1-5 if it exact matches
      *--Tag Validation table. If Match, treat it as Tag57A w/ our own
      *    BIC
      *--(UOVBSGSGXXX - parameterized) to further proceed with STP
      *    processing.
      *--E.g Raw Tag57D Line1:/123456789
      *--     Line2:UNITED OVERSEAS BANK
      *--     Line3:SINGAPORE
      *--     Line4:BUKIT BATOK
      *--     Line5:SG
      *--If Line 1 "UNITED OVERSEAS BANK" exact matches Tag validation
      *    table
      *--system will treat this as Tag57A Line1: *blank
      *--     Line2: UOVBSGSGXXX
      *--     Line3: *blank
      *--     Line4: *blank
      *--     Line5: *blank
      *--and proceed with BAU STP processing.
           INITIALIZE               WK-C-VTAG57-RECORD.
           MOVE TAG57-OPT           TO WK-C-VTAG57-OPTION.
      *--For MT202, validate on Tag57D ONLY.
           IF TAG57-OPT = "C"
                 AND TFSSTPL-SWFTMGTY = "202"
              GO TO D699-EVAL-TAG57-CD-EX
       END-IF.
      *--Tag57C:
           IF TAG57-OPT = "C"
              IF TAG57-PTID = SPACES
                 GO TO D699-EVAL-TAG57-CD-EX

GP3A00        ELSE
GP3A00           MOVE TAG57-PTID       TO  WK-C-VTAG57-INFO(1)
GP3A00        END-IF.
GP3A00     END-IF.
GP3A00*--Tag57D:
GP3A00     IF  TAG57-OPT = "D"
GP3A00        IF  TAG57-PTID = SPACES
GP3A00              AND TAG57-NAME = SPACES
GP3A00           GO TO D699-EVAL-TAG57-CD-EX
GP3A00        ELSE
GP3A01*GP3A00           MOVE TAG57-PTID       TO  WK-C-
      *    VTAG57-INFO(1)
GP3A00           MOVE TAG57-NAME       TO  WK-C-VTAG57-INFO(2)
GP3A01*GP3A00           MOVE TAG57-LINE-3     TO  WK-C-
      *    VTAG57-INFO(3)
GP3A01*GP3A00           MOVE TAG57-LINE-4     TO  WK-C-
      *    VTAG57-INFO(4)
GP3A01*GP3A00           MOVE TAG57-LINE-5     TO  WK-C-
      *    VTAG57-INFO(5)
GP3A00        END-IF
GP3A00     END-IF.
GP3A00
GP3A00     MOVE TFSSTPL-BNKENTTY  TO  WK-C-VTAG57-I-BNKENTTY.
GP3A00
GP3A00*--Check Tag57 if either Lines 1-5 matches Tag
      *    validation table.
GP3A00     CALL "TRFVTA57"       USING  WK-C-VTAG57-RECORD.
GP3A00     CANCEL "TRFVTA57".
GP3A00
GP3A00     IF  WK-C-VTAG57-ERROR-CD = SPACES
GP3A00        CONTINUE
GP3A00     ELSE
GP3A00        GO TO D699-EVAL-TAG57-CD-EX
GP3A00     END-IF.
GP3A00
GP3A00*--If it match, override w/ Tag57A:<Own BIC> (
      *    parameterized)
GP3A00     IF  WK-C-VTAG57-VALID = "Y"
GP3A00        MOVE SPACES           TO  TAG57-PTID
GP3A00           WS-ACBNKACC
GP3A00           WS-ACCNO
GP3A00        MOVE "A"              TO  TAG57-OPT
GP3A00        MOVE WK-C-VTAG57-BIC  TO  TAG57-BIC
GP3A00           WS-ACBNKID
GP3A00           WS-BANKID
GP3A00     END-IF.
           GP3A00*======================================================
      *    ===========*
GP3A00        D699-EVAL-TAG57-CD-EX.
GP3A00*======================================================
      *    ===========*
GP3A00     EXIT.
5Q1JM1        D500-PROCESS-RPRRSN SECTION.
5Q1JM1        D500-ENTRY.
5Q1JM1     MOVE WK-VTB2-PARALNO  TO WK-C-RRSN-QUENUM.
5Q1JM1     MOVE WK-VTB2-SEQNUM   TO WK-C-RRSN-QUESUF.
5Q1JM1     MOVE WK-C-TRNNO       TO WK-C-RRSN-TRNNO.
5Q1JM1     MOVE WK-C-FUNCTID     TO WK-C-RRSN-FUNCTID.
5Q1JM1     MOVE WK-C-SEGCODE     TO WK-C-RRSN-SEGCODE.
5Q1JM1     MOVE SPACES               TO WK-C-RRSN-SEGDESC.
5Q1JM1     MOVE WK-N-STAFFIND        TO WK-C-RRSN-STAFFIND.
5Q1JM1     MOVE WS-ACCNO             TO WK-C-RRSN-ACCNO.
5Q1JM1     MOVE WK-C-QRATE           TO WK-C-RRSN-QRATE.
5Q1JM1     MOVE WK-N-SYSDTE          TO WK-C-RRSN-RPRDTE.
5Q1JM1*          MOVE WK-C-RPRCODE         TO WK-C-RRSN-
      *    RSNCDE.
5Q1JM1     IF WK-C-RPRCODE = SPACE
5Q1JM1        MOVE "RSN9999"        TO WK-C-RRSN-RSNCDE
5Q1JM1     ELSE
5Q1JM1        MOVE WK-C-RPRCODE     TO WK-C-RRSN-RSNCDE
5Q1JM1     END-IF.
5Q1JM1     MOVE SPACES               TO WK-C-RRSN-RSNDESC.
5Q1JM1     MOVE WK-C-RPRPGM          TO WK-C-RRSN-RPRPGM.
5Q1JM1     CALL "TRFGRRSN" USING WK-C-RRSN-RECORD.
              5Q1JM1
5Q1JM1        D599-PROCESS-RPRRSN-EX.
5Q1JM1     EXIT.
5Q1JM1        EJECT

       Z000-END-PROGRAM.
           CLOSE TFSSTPL
              TFSBNKAC
              TFSBNKET
       TFSCLSYS.
           EXIT PROGRAM.
