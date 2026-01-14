       IDENTIFICATION DIVISION.
      ***********************
       PROGRAM-ID. TRFVTB1.
       AUTHOR. TYK.
       DATE-WRITTEN. JUN 04.
      *DESCRIPTION : TABLE B1 VALIDATION.
      *              SUBROUTINE - CREDIT PARTY CHECKING FIELD 56/57 FOR
      *              INCOMING MT103 LCY
      *
      *==================================================================
      * HISTORY OF MODIFICATION:
      *==================================================================
      * GP3M01 - VENADG  - 18/03/2020 - CASH MANAGEMENT ROAD MAP
      *                      - P19 SWIFT GPI DAY 3
      *                      - PCRMAPKGPI-1331
      *                      - To add "MAS" Mode pay on
      *                        the evaluation of DR NOSTRO.
      *                      - "MAS" PMODE will be passed from
      *                        TRFVT D1 only when:
      *                        1. Incoming MEPS
      *                        2. Doesnt have Tag53/54
      *                        3. Sending BIC is MEPS
      *------------------------------------------------------------------
      * GP3C02 - ACNESQ  - 06/12/2019 - CASH MANAGEMENT ROAD MAP
      *                      - P19 SWIFT GPI DAY 3
      *                      - PCRMAPKGPI-1050
      *                      - Ensure only line 1 and 2 of
      *                        Tag 57 C/D are being validated
      *                        against Tag57 Validation Table
      *------------------------------------------------------------------
      * GP3M00 - ACNESQ  - 29/10/2019 - CASH MANAGEMENT ROAD MAP
      *                      - P19 SWIFT GPI DAY 3
      *                      - Inward serial payment Bypass
      *                      - STP Limit for Nostro (Item5a)
      *                      - Bypass STP Limit if Dr Leg
      *                        = NOSTRO and CR Leg = VOSTRO
      *------------------------------------------------------------------
      * GP3C01 - ACNJR   - 08/10/2019 - CASH MANAGEMENT ROAD MAP
      *                      - P19 SWIFT GPI DAY 3
      *                      - PCRMAPKGPI-932
      *                      - Tag57 Enhancement (Item5b)
      *                      - Previously Tag 57C/D validation
      *                        will proceed even if Tag56 has
      *                        a value.
      *                      - Rectified that Tag 57C/D vali-
      *                        dation will not proceed as long
      *                        as Tag56 has value.
      *------------------------------------------------------------------
      * GP3C00 - VENADG  - 14/18/2019 - CASH MANAGEMENT ROAD MAP
      *                      - P19 SWIFT GPI DAY 3
      *                      - Tag57 Enhancement (Item5b)
      *                      - To check Tag57 C/D Lines 1-5
      *                        againts Tag Validation Table.
      *                        This is to enable such tags to

      * further proceed with STP processing
      * if exact matches.
      *--------------------------------------------------------------
      * GPI201 - ACNDCH - 04/04/2019 - CASH MANAGEMENT ROAD MAP
      * - P19 SWIFT GPI DAY 2B
      * - PCRMAPKGPI-517
      * - Add call of TRFVBACU to
      *   validate if crediting Nostro
      *   BIC is a UOB branch. If UOB
      *   branch, then bypass STP limit
      * - Bypass if debitting leg is
      *   VOSTRO and if cover is already
      *   received for 103
      *--------------------------------------------------------------
      * CMR4A1 - ACNROO - 03/29/2018 - CASH MANAGEMENT ROAD MAP
      *   PROJECT 4
      *   USE BANK BIC FROM PARAMETER
      *   TABLE
      * - PCRMAPUPAY-273
      *--------------------------------------------------------------
      * 7Q1EM1 - TMPEYM - 20/10/2016 - REM Q1 2017 RELEASE
      * - e-Req 47511 Refinement of
      *   Duplicate checking for Inw
      * - Recompiled due to changes made in
      *   VSTPL copy book.
      *--------------------------------------------------------------*
      * CMP3A4 - ACNESQ - 30/09/2016 - CASH MANAGEMENT PROJECT 3
      *   USE ORDERING CUSTOMER (TAG 50H)
      *   WHEN VALIDATING STP LIMIT
      *   FOR MT101
      *==============================================================
      * CMP3A3 - VENAF2 - 18/08/2016 - CASH MANAGEMENT PROJECT 3
      *   JIRA: PCSHMGMTSG-191
      *   INCLUDE LOCAL SWIFT BIC
      *   FOR MT101 TRANSACTIONS
      *==============================================================
      * CMP3A2 - CMPESQ - 05/08/2016 - CASH MANAGEMENT PROJECT 3
      *   JIRA: PCSHMGMTSG-190
      *   UPDATE STP LIMIT VALIDATION
      *==============================================================
      * CMP3A1 - CMPESQ - 01/07/2016 - CASH MANAGEMENT PROJECT 3
      * - INCLUDE REM INDICATOR
      *==============================================================
      * CMP3X1 - CMPESQ - 14/06/2016 - CASH MANAGEMENT PROJECT RELEASE 3
      *   JIRA: PCSHMGMTSG-109,
      *   PCSHMGMTSG-110
      * - FIX STP LIMIT PROCESS
      *==============================================================
      * CMP3FL - VENAF2 - 07/01/2016 - CASH MANAGEMENT PROJECT RELEASE 3
      *   STP Limit by Account, CIF
      *   and Segment
      * 5Q1ARV - TMPARV - 10/11/2014 - 14HOREM024/14HOREM029/14HOREM028
      * 5Q1JE1
      *   Modified to determine the
      *   repair reason and create an

      *      entry on the new file (RFTFRRSN)
      *=================================================================
      * SM1TY1 - TMPTY1  - 11/08/2005 - DON'T USE MAS ACCOUNT NUMBER
      *                             FOR CR MAS/MAS202 TRANSACTION
      *=================================================================
       ENVIRONMENT DIVISION.
      ********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-AS400.
       OBJECT-COMPUTER.  IBM-AS400.
       SPECIAL-NAMES.  LOCAL-DATA IS LOCAL-DATA-AREA
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

       SM1TY1* SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
               SM1TY1*     ORGANIZATION IS INDEXED
               SM1TY1*     ACCESS MODE IS RANDOM
       SM1TY1*     RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       SM1TY1*     FILE STATUS IS WK-C-FILE-STATUS.

       CMP3A3  SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
               CMP3A3      ORGANIZATION IS INDEXED
               CMP3A3      ACCESS MODE IS RANDOM
       CMP3A3      RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       CMP3A3      FILE STATUS IS WK-C-FILE-STATUS.

       GP1201  SELECT UFIMJICON ASSIGN TO DATABASE-UFIMJICON
               GP1201      ORGANIZATION IS INDEXED
               GP1201      ACCESS MODE IS RANDOM
       GP1201      RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
               GP1201          WITH DUPLICATES
       GP1201      FILE STATUS IS WK-C-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
      **************
       FD  TFSSFTPL
       LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFSSFTPL.
       01  WK-C-TFSSFTPL.
               COPY DDS-ALL-FORMATS OF TFSSFTPL.
       01  WK-C-TFSSFTPL-1.
               COPY TFSSFTPL.

       FD  TFSCLSYS
       LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSCLSYS-REC.
       01  TFSCLSYS-REC.
               COPY DDS-ALL-FORMATS OF TFSCLSYS.
       01  TFSCLSYS-REC-1.
               COPY TFSCLSYS.

SM1TY1*FD  TFSBNKET
SM1TY1*    LABEL RECORDS ARE OMITTED
SM1TY1*    DATA RECORD IS TFSBNKET-REC.
SM1TY1*01  TFSBNKET-REC.
SM1TY1*    COPY DDS-ALL-FORMATS OF TFSBNKET.
SM1TY1*01  TFSBNKET-REC-1.
SM1TY1*    COPY TFSBNKET.
CMP3A3 FD  TFSBNKET
CMP3A3     LABEL RECORDS ARE OMITTED
CMP3A3     DATA RECORD IS TFSBNKET-REC.
CMP3A3 01  TFSBNKET-REC.
CMP3A3     COPY DDS-ALL-FORMATS OF TFSBNKET.
CMP3A3 01  TFSBNKET-REC-1.
CMP3A3     COPY TFSBNKET.

GPI201 FD  UFIMIJCON
GPI201     LABEL RECORDS ARE OMITTED
GPI201     DATA RECORD IS WK-C-UFIMIJCON.
GPI201 01  WK-C-UFIMIJCON.
GPI201     COPY DDS-ALL-FORMATS OF UFIMIJCON.
GPI201 01  UFIMIJCON-REC.
GPI201     COPY UFIMIJCON.

       WORKING-STORAGE SECTION.
      ************************
       01  WK-C-COMMON.
               COPY ASCWWS.

CMP3FL 01  WK-C-LINK-LIMIT.
CMP3FL     05  WK-C-LINK-AREA-INPUT.
CMP3FL     10  WS-LINK-BNKENTTY      PIC S9(1).
CMP3FL     10  WS-LINK-ACCNO         PIC X(11) VALUE 0.
CMP3FL     10  WS-LINK-CCY           PIC X(03) VALUE SPACES.
CMP3FL     10  WS-LINK-AMT           PIC S9(13)V99 VALUE 0.
CMP3A1     10  WS-LINK-REMIND        PIC X(01).

CMP3FL  05  WK-C-LINK-AREA-OUTPUT.
CMP3FL  10  WS-LINK-STATUS           PIC X(02) VALUE SPACES.

       01 TAG56-FORMAT.
               05 TAG56-LINE-1.
               07 TAG56-FIL1               PIC X(2).
               07 TAG56-OPT                PIC X(1).
               07 TAG56-FIL2               PIC X(1).
               07 TAG56-PTID.
               09 TAG56-PTID-1         PIC X(02).
               09 TAG56-PTID-2         PIC X(35).
               05 TAG56-LINE-2                 PIC X(35).
               05 TAG56-BIC REDEFINES TAG56-LINE-2.
               07 TAG56A-SUB1              PIC X(4).
               07 TAG56A-SUB2              PIC X(2).
               07 TAG56A-SUB3              PIC X(2).
               07 TAG56A-SUB4              PIC X(3).
               07 TAG56A-FILLER            PIC X(24).
               05 TAG56-LOC REDEFINES TAG56-LINE-2
               PIC X(35).
               05 TAG56-NAME REDEFINES TAG56-LINE-2
               PIC X(35).
               05 TAG56-LINE-3                 PIC X(35).
               05 TAG56-LINE-4                 PIC X(35).
               05 TAG56-LINE-5                 PIC X(35).

       01 TAG57-FORMAT.
               05 TAG57-LINE-1.
               07 TAG57-FIL1               PIC X(2).
               07 TAG57-OPT                PIC X(1).
               07 TAG57-FIL2               PIC X(1).
               07 TAG57-PTID.
               09 TAG57-PTID-1         PIC X(02).
               09 TAG57-PTID-2         PIC X(35).
               05 TAG57-LINE-2                 PIC X(35).
               05 TAG57-BIC REDEFINES TAG57-LINE-2.
               07 TAG57A-SUB1              PIC X(4).
               07 TAG57A-SUB2              PIC X(2).
               07 TAG57A-SUB3              PIC X(2).
               07 TAG57A-SUB4              PIC X(3).
               07 TAG57A-FILLER            PIC X(24).
               05 TAG57-LOC REDEFINES TAG57-LINE-2
               PIC X(35).
               05 TAG57-NAME REDEFINES TAG57-LINE-2
               PIC X(35).
               05 TAG57-LINE-3                 PIC X(35).
               05 TAG57-LINE-4                 PIC X(35).
               05 TAG57-LINE-5                 PIC X(35).

       01 TABLE-ARRAY.
               05 TAB-VAL OCCURS 20 TIMES      PIC X VALUE "X".

       01 TABLE-ARR2.

               05 TAB-VL2 OCCURS 20 TIMES PIC X VALUE "X".

       01 PATH-P1            PIC X(20)
               VALUE "XXXXYYYYXXXXXXXXXXXXXX".
       01 PATH-P2            PIC X(20)
               VALUE "XXXXXXXXXXOXXXXXXXXXXX".
       01 PATH-P3            PIC X(20)
               VALUE "XXXXYYYYXXOXXXXXXXXXXX".
       01 PATH-P4            PIC X(20)
               VALUE "XXXXYYYYXXOXXXXXXXXXXX".
       01 PATH-P5            PIC X(20)
               VALUE "XXXXXXXXXXOXXXXXXXXXXX".
       01 PATH-P6            PIC X(20)
               VALUE "XXXXYYYYXXOXXXXXXXXXXX".
       01 PATH-P7            PIC X(20)
               VALUE "XXXXYYYYXXOXXXXXXXXXXX".
       01 PATH-P8            PIC X(20)
               VALUE "XXXXXXXXXXOXXXXXXXXXXX".
       01 PATH-P9            PIC X(20)
               VALUE "XYXXXXYYXXOXXXXXXXXXXX".
       01 PATH-P10           PIC X(20)
               VALUE "XXXXYYYYXXOXXXXXXXXXXX".
       01 PATH-P11           PIC X(20)
               VALUE "XXXXXXXXXXOXXXXXXXXXXX".
       01 PATH-P12           PIC X(20)
               VALUE "XXXXXXXXXXOXXXXXXXXXXX".
       01 PATH-P13           PIC X(20)
               VALUE "XXXXXXNYXXOXXXXXXXXXXX".
       01 PATH-P14           PIC X(20)
               VALUE "XYXXXXYYXXOXXXXXXXXXXX".
       01 PATH-P15           PIC X(20)
               VALUE "XXXXXXXXYYXOXXXXXXXXXX".
       01 PATH-P16           PIC X(20)
               VALUE "XXXXXXNYXXOXXXXXXXXXXX".
       01 PATH-P17           PIC X(20)
               VALUE "XYXXXXOXXOXXXXXXXXXXXX".
       01 PATH-P18           PIC X(20)
               VALUE "XYXYXXOXXOXXXXXXXXXXXX".
       01 PATH-P19           PIC X(20)
               VALUE "XYXYXXXXXXXXXXXXXXXOXX".

       01 WK-C-PARADATA.
               05 WK-C-PARAVALU     PIC X(20).
               05 WK-N-PARAVALU     REDEFINES WK-C-PARAVALU
               PIC 9(13)V99.
               05 WK-N-IRMPSTP      PIC 9(13)V99.
               05 WK-N-IRMISTP      PIC 9(13)V99.

       01 WK-C-WORK-AREA.
               05 FIRST-TIME        PIC X(01) VALUE "Y".
               05 WS-FLAG1          PIC X(01) VALUE SPACE.
               05 WS-FLAG2          PIC X(01) VALUE SPACE.
               05 MEPS56-IND        PIC X(01) VALUE SPACE.
               05 MEPS57-IND        PIC X(01) VALUE SPACE.

               05  BKAC56-IND              PIC X(01) VALUE SPACE.
               05  BKAC57-IND              PIC X(01) VALUE SPACE.
               05  WS-ACT1                 PIC X(01) VALUE SPACE.
               05  WS-ACT2                 PIC X(01) VALUE SPACE.
               05  WS-ACT3                 PIC X(01) VALUE SPACE.
               05  WS-ACT4                 PIC X(01) VALUE SPACE.
               05  WS-ACT5                 PIC X(01) VALUE SPACE.
               05  WS-ACT6                 PIC X(01) VALUE SPACE.
               05  WS-OKAY                 PIC X(01) VALUE SPACE.
               05  WS-STPTYP               PIC X(04) VALUE SPACE.
               05  WS-ACCNO                PIC X(11) VALUE SPACE.
               05  WS-INTEMBNKACC          PIC X(11) VALUE SPACE.
               05  WS-ACBNKACC             PIC X(11) VALUE SPACE.
               05  WS-BANKID               PIC X(11) VALUE SPACE.
               05  WS-INTEMBNKID           PIC X(11) VALUE SPACE.
               05  WS-ACBNKID              PIC X(11) VALUE SPACE.
GPI201 05  WK-C-GPI-SW             PIC X(01) VALUE SPACE.
GPI201 05  WK-C-BYPASS-LMT-IND     PIC X(01) VALUE SPACE.
GPI201 05  WK-C-DR-PMODE           PIC X(08) VALUE SPACE.
GPI201 05  WK-C-COV-SW             PIC X(01) VALUE SPACE.
GP3C00 05  WK-C-GPI3-SW            PIC X(01) VALUE SPACE.
GP3C00 05  WK-C-TAG57-CD-SW        PIC X(01) VALUE SPACE.
GP3C01 05  WK-C-TAG56-SW           PIC X(01) VALUE SPACE.
GP3M00 05  WK-C-NSLMT-SW           PIC X(01) VALUE SPACE.
GPI201 01  WK-C-LIT-GPI.
GPI201 05  WK-C-Y                 PIC X(01) VALUE "Y".
GPI201 05  WK-C-A                 PIC X(01) VALUE "A".
GPI201 05  WK-C-GPI-SW-PARCD      PIC X(10)
GPI201                             VALUE "GPISWITCH2".
GPI201 05  WK-C-STP-SW-PARCD      PIC X(10)
GPI201                             VALUE "GPISWTSW".
GP3C00 05  WK-C-GPI3-SW-PARCD     PIC X(10)
GP3C00                             VALUE "GPISWITCH3".
GP3C00 05  WK-C-TAG57-SW-PARCD    PIC X(10)
GP3C00                             VALUE "GPI3T57SW".
GP3C00 05  WK-C-TAG57-MT-PARCD    PIC X(10)
GP3C00                             VALUE "GPI3T57MT".
GP3M00 05  WK-C-NSLMT-PARCD       PIC X(10)
GP3M00                             VALUE "GPI3NSLMT".

GP3C00 01  WK-C-MT-TAG57-TBL      PIC X(18) VALUE SPACES.
GP3C00 05  WK-C-MT-TAG57          PIC X(03) OCCURS 6 TIMES.
5Q1ARV 01  WK-C-RPPRSN-AREA.
5Q1ARV 05  WK-C-NQENUM            PIC S9(08) VALUE ZEROS.
5Q1ARV 05  WK-C-NQUESUF           PIC S9(02) VALUE ZEROS.
5Q1ARV 05  WK-C-SEGCDE            PIC X(01) VALUE SPACE.
5Q1JE1 05  WK-N-STAFFIND          PIC S9(02) VALUE ZEROS.
5Q1ARV 05  WK-C-QRATE             PIC X(02) VALUE SPACE.
5Q1ARV 05  WK-C-RPRCODE           PIC X(07) VALUE SPACE.
5Q1ARV 05  WK-C-TRNNNO            PIC X(12) VALUE SPACE.
5Q1ARV 05  WK-C-FUNCTID           PIC X(08) VALUE SPACE.
5Q1ARV 01  WK-N-SYSDTE            PIC S9(08) VALUE ZEROS.

5Q1ARV 01  WK-C-RPRPGM               PIC X(10)  VALUE "TRFVTB1".
CMP3A3 01  WK-C-SWIFTBICCODE         PIC X(11)  VALUE SPACE.
CMP3A4 01  WK-101-TAG50H-ACCNO       PIC X(11)  VALUE SPACE.

               COPY VCCA.
               COPY VSTPL.
               COPY VBBAS.
               COPY VBAC.
               COPY XPARA.
               COPY NSTP.
               COPY ACMN.
               COPY LOGG.
               5Q1ARV COPY RRSN.
               GPI201 COPY XGSPA.
               GPI201 COPY GPISTPSW.
               GPI201 COPY VBACU.
               GP3C00 COPY VTAG57.

       LINKAGE SECTION.
      *****************
               COPY VTB1.

       PROCEDURE DIVISION USING WK-VTB1.
      *********************************
       MAIN-MODULE.

CMP3A4       MOVE SPACES         TO  WK-101-TAG50H-ACCNO.
CMP3A4       IF  WK-VTB1-BANKAC NOT = SPACES
CMP3A4           MOVE WK-VTB1-BANKAC TO  WK-101-TAG50H-ACCNO
CMP3A4           MOVE SPACES         TO  WK-VTB1-BANKAC
CMP3A4       END-IF.

           INITIALIZE WK-VTB1-OUTPUT
               WK-LOGG
       WK-C-WORK-AREA.
           MOVE ALL "X"  TO TABLE-ARRAY.
           MOVE ALL "X"  TO TABLE-ARR2.
           MOVE "Y"      TO FIRST-TIME.

5Q1ARV       MOVE ZEROS     TO WK-C-RRSN-QUENUM
               WK-C-RRSN-QUESUF
               WK-C-RRSN-STAFFIND
               WK-C-RRSN-SEQNUM
       WK-C-RRSN-RPRDTE.

GPI201       MOVE WK-VTB1-DR-PMODE TO WK-C-DR-PMODE.

           IF FIRST-TIME = "Y"
               OPEN INPUT TFSSTPL
               IF NOT WK-C-SUCCESSFUL
               AND WK-C-FILE-STATUS NOT = "41"
                   DISPLAY "TRFSTPL - OPEN FILE ERROR - TFSSTPL"
                   DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
               END-IF

               OPEN INPUT TFSCLSYS
               IF NOT WK-C-SUCCESSFUL
               AND WK-C-FILE-STATUS NOT = "41"
                   DISPLAY "TRFVTB1 - OPEN FILE ERROR - TFSCLSYS"
                   DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
               END-IF
SM1Y1*         OPEN INPUT TFSBNKET
SM1Y1*         IF NOT WK-C-SUCCESSFUL
SM1Y1*         AND WK-C-FILE-STATUS NOT = "41"
SM1Y1*             DISPLAY "TRFVTB1 - OPEN FILE ERROR - TFSBNKET"
SM1Y1*             DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
SM1Y1*         END-IF
CMP3A3         OPEN INPUT TFSBNKET
CMP3A3         IF NOT WK-C-SUCCESSFUL
CMP3A3         AND WK-C-FILE-STATUS NOT = "41"
CMP3A3             DISPLAY "TRFVTB1 - OPEN FILE ERROR - TFSBNKET"
CMP3A3             DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
CMP3A3         END-IF
GPI201         OPEN INPUT UFIMIJCON
GPI201         IF NOT WK-C-SUCCESSFUL
GPI201         AND WK-C-FILE-STATUS NOT = "41"
GPI201             DISPLAY "UFIMIJCON - OPEN FILE ERROR - UFIMIJCON"
GPI201             DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
GPI201         END-IF
           END-IF.

           MOVE WK-VTB1-PARALNO TO TFSSTPL-PARALNO.
           MOVE WK-VTB1-SEQNUM TO TFSSTPL-SEQNUM.

           READ TFSSTPL
                KEY IS EXTERNALLY-DESCRIBED-KEY.

           IF WK-C-SUCCESSFUL
              MOVE "N" TO WS-OKAY
              MOVE TFSSTPL-TAG56 TO TAG56-FORMAT
              MOVE TFSSTPL-TAG57 TO TAG57-FORMAT
CMP3A3        MOVE TFSSTPL-BNKENTITY TO TFSBNKET-BNKENTITY
CMP3A3        READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
CMP3A3             INVALID KEY
CMP3A3                MOVE SPACES TO WK-C-SWIFTBICCODE
CMP3A3             NOT INVALID KEY
CMP3A3                MOVE TFSBNKET-SWFTBNK TO WK-C-SWIFTBICCODE
CMP3A3        END-READ
              PERFORM A100-INITIAL-SUBROUTINE
                THRU A199-INITIAL-SUBROUTINE-EX
              PERFORM A200-MOVE-TAG-VALUES
                THRU A299-MOVE-TAG-VALUES-EX
              PERFORM B100-PATH-CHOICE THRU B199-PATH-CHOICE-EX
       END-IF.
              GO TO 2000-END-PROGRAM.

       A100-INITIAL-SUBROUTINE.
      *---------------------------------------------------------------*
      * GET DATA FROM "TFSCLSYS" TABLE                                *

      *-----------------------------------------------------------------*
                   READ  TFSCLSYS.
                   IF  NOT WK-C-SUCCESSFUL
                       DISPLAY "TRFVTB1  -  READ TFSCLSYS ERROR"
                       DISPLAY "FILE STATUS  -  " WK-C-FILE-STATUS
                       GO TO  Z000-END-PROGRAM.
      **********MOVE  TFSCLSYS-SYSDTE  TO  L-N-G-SYSDTE.
5Q1ARV      MOVE  TFSCLSYS-SYSDTE  TO  WK-N-SYSDTE.
      **********MOVE  TFSCLSYS-LCNTRYCD  TO  L-C-G-L-CNTRYCD.
      **********MOVE  TFSCLSYS-LCUYCD  TO  L-C-G-L-CUYCD.

      *-----------------------------------------------------------------*
      *        GET SYSTEM PARAMETERS FOR PSTP & 1STP                   *
      *-----------------------------------------------------------------*
       MOVE  "IRMSGP"  TO  WK-C-XPARA-PARACD.
       CALL  "TRFXPARA"  USING  WK-C-XPARA-RECORD.
       IF  WK-C-XPARA-ERROR-CD NOT = SPACES
           DISPLAY  "TREEEDT  -  TRFXPARA ROUTINE ERROR"
           DISPLAY  "FILE STATUS  -  " WK-C-XPARA-FS
           DISPLAY  "ERROR ID  -  " WK-C-XPARA-ERROR-CD
           DISPLAY  "KEY       " WK-C-XPARA-INPUT
           GO TO  Z000-END-PROGRAM
       ELSE
           MOVE  WK-C-XPARA-PARAVALU
                               TO  WK-C-PARAVALU
           MOVE  WK-N-PARAVALU
                               TO  WK-N-IRMPSTP
       END-IF.
       MOVE  "IRMSG1"  TO  WK-C-XPARA-PARACD.
       CALL  "TRFXPARA"  USING  WK-C-XPARA-RECORD.
       IF  WK-C-XPARA-ERROR-CD NOT = SPACES
           DISPLAY  "TREEEDT  -  TRFXPARA ROUTINE ERROR"
           DISPLAY  "FILE STATUS  -  " WK-C-XPARA-FS
           DISPLAY  "ERROR ID  -  " WK-C-XPARA-ERROR-CD
           DISPLAY  "KEY       " WK-C-XPARA-INPUT
           GO TO  Z000-END-PROGRAM
       ELSE
           MOVE  WK-C-XPARA-PARAVALU
                 TO  WK-C-PARAVALU
           MOVE  WK-N-PARAVALU
                 TO  WK-N-IRM1STP
       END-IF.

GPI201*-----------------------------------------------------------------*
GPI201* RETRIEVE GPI TECHNICAL AND STP SWITCH FROM SYSTEM PARAMETER    *
GPI201* FILE VIA CALLING TRFXGSPA PROGRAM USING GPISWITCH PARAMETER    *
GPI201*-----------------------------------------------------------------*
GPI201
GPI201      INITIALIZE  WK-C-XGSPA-RECORD
GPI201                   SW-STP-LMT-SKP.
GPI201
GPI201      MOVE  WK-C-STP-SW-PARCD  TO  WK-C-XGSPA-GHPARCD.
GPI201      CALL  "TRFXGSPA"  USING  WK-C-XGSPA-RECORD.

GP1201
GP1201       IF WK-C-XGSPA-ERROR-CD = SPACES
GP1201           MOVE WK-C-XGSPA-GHPARVAL(2:1)
GP1201               TO SW-STP-LMT-SKP
GP1201       END-IF.
GP1201
GP1201       INITIALIZE WK-C-XGSPA-RECORD
GP1201                   WK-C-GPI-SW.
GP1201
GP1201       MOVE WK-C-GPI-SW-PARCD TO WK-C-XGSPA-GHPARCD.
GP1201       CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
GP1201
GP1201       IF WK-C-XGSPA-ERROR-CD = SPACES
GP1201           MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP1201               TO WK-C-GPI-SW
GP1201       END-IF.
GP3C00*-->Retrieve GPI Day 3 Technical Switch
GP3C00       INITIALIZE WK-C-XGSPA-RECORD
GP3C00                   WK-C-GPI3-SW.
GP3C00
GP3C00       MOVE WK-C-GPI3-SW-PARCD TO WK-C-XGSPA-GHPARCD.
GP3C00       CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
GP3C00
GP3C00       IF WK-C-XGSPA-ERROR-CD = SPACES
GP3C00           MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP3C00               TO WK-C-GPI3-SW
GP3C00       END-IF.
GP3C00*-->Retrieve GPI Day3 Tag57 C/D Enhancement Switch
GP3C00       INITIALIZE WK-C-XGSPA-RECORD
GP3C00                   WK-C-TAG57-CD-SW.
GP3C00
GP3C00       MOVE WK-C-TAG57-SW-PARCD TO WK-C-XGSPA-GHPARCD.
GP3C00       CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
GP3C00
GP3C00       IF WK-C-XGSPA-ERROR-CD = SPACES
GP3C00           MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP3C00               TO WK-C-TAG57-CD-SW
GP3C00       END-IF.
GP3C00*-->Retrieve GPI Day3 Tag57 C/D Enhancement Eligable MT Types
GP3C00       INITIALIZE WK-C-XGSPA-RECORD
GP3C00                   WK-C-MT-TAG57-TBL.
GP3C00
GP3C00       MOVE WK-C-TAG57-MT-PARCD TO WK-C-XGSPA-GHPARCD.
GP3C00       CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
GP3C00
GP3C00       IF WK-C-XGSPA-ERROR-CD = SPACES
GP3C00           MOVE WK-C-XGSPA-GHPARVAL
GP3C00               TO WK-C-MT-TAG57-TBL
GP3C00       END-IF.
GP3M00*-->Retrieve GPI Day3 Nostro Bypass STP Limit Enhancement
GP3M00       INITIALIZE               WK-C-XGSPA-RECORD
GP3M00                                WK-C-NSLMT-SW.
GP3M00
GP3M00       MOVE WK-C-NSLMT-PARCD    TO  WK-C-XGSPA-GHPARCD.
GP3M00       CALL "TRFXGSPA"          USING WK-C-XGSPA-RECORD.
GP3M00
GP3M00       IF  WK-C-XGSPA-ERROR-CD = SPACES
GP3M00           MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP3M00                                TO  WK-C-NSLMT-SW
GP3M00       END-IF.
GP3M00
       A199-INITIAL-SUBROUTINE-EX.
       EXIT.

       A200-MOVE-TAG-VALUES.
GP3C01       MOVE "N"                 TO  WK-C-TAG56-SW
             IF  TAG56-BIC NOT = SPACES
             AND TAG56-OPT = "A"
                 MOVE TAG56-BIC       TO  WS-INTEMBNKID
                                          WS-BANKID
                 MOVE TAG56-PTID      TO  WS-INTEMBNKACC
                                          WS-ACCNO
GP3C01           MOVE "Y"             TO  WK-C-TAG56-SW
             END-IF.
             IF  TAG57-BIC NOT = SPACES
             AND TAG57-OPT = "A"
                 MOVE TAG57-BIC       TO  WS-ACBNKID
                                          WS-BANKID
                 MOVE TAG57-PTID      TO  WS-ACBNKACC
                                          WS-ACCNO
             END-IF.

             IF  TAG57-NAME NOT = SPACES
             AND TAG57-OPT = "D"
                 MOVE TAG57-PTID      TO  WS-ACBNKACC
                                          WS-ACCNO
                 MOVE TAG57-NAME      TO  WK-VTB1-ACBNKNM
                 MOVE TAG57-LINE-3    TO  WK-VTB1-ACBNKADR1
                 MOVE TAG57-LINE-4    TO  WK-VTB1-ACBNKADR2
                 MOVE TAG57-LINE-5    TO  WK-VTB1-ACBNKADR3
             END-IF.
GP3C00*-->GPI Day3 Tag57 C/D Enhancement
GP3C00       IF  WK-C-GPI3-SW = "Y"
GP3C00       AND WK-C-TAG57-CD-SW = "Y"
GP3C01*-- Skip Tag 57C/D validation IF Tag56A switch is ON
GP3C01         IF  WK-C-TAG56-SW = "Y"
GP3C01             CONTINUE
GP3C01         ELSE
GP3C00             IF  TAG57-OPT = "C" OR "D"
GP3C00             AND (TFSSPTPL-SWFTMGTY = WK-C-MT-TAG57(1)
GP3C00             OR  TFSSPTPL-SWFTMGTY = WK-C-MT-TAG57(2)
GP3C00             OR  TFSSPTPL-SWFTMGTY = WK-C-MT-TAG57(3)
GP3C00             OR  TFSSPTPL-SWFTMGTY = WK-C-MT-TAG57(4)
GP3C00             OR  TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(5)
GP3C00             OR  TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(6))
GP3C00                 PERFORM D600-EVAL-TAG57-CD
GP3C00                     THRU D699-EVAL-TAG57-CD-EX
GP3C00             END-IF
GP3C01         END-IF
GP3C00       END-IF.

             MOVE TFSSTPL-BNKENTTY   TO WK-N-VBAC-BNKENTTY.
             MOVE WS-BANKID          TO WK-C-VBAC-BANKID.
             MOVE TFSSTPL-CUYCD      TO WK-C-VBAC-CUYCD.
             CALL "TRFVBAC" USING WK-C-VBAC-RECORD.
             IF  WK-C-VBAC-ACUDBUI NOT = SPACES
                 MOVE WK-C-VBAC-ACUDBUI TO WK-VTB1-ACUDBUI
                 IF  WS-ACCNO = SPACES
                     MOVE WK-C-VBAC-BNKACCNO TO WS-ACCNO
                 END-IF
             ELSE
                 MOVE "D"            TO WK-VTB1-ACUDBUI
             END-IF.
       A299-MOVE-TAG-VALUES-EX.
             EXIT.
             EJECT

       B100-PATH-CHOICE.
             PERFORM D400-MEPS-VALIDATION.
             IF  TAG56-OPT = SPACES
             AND TAG56-PTID = SPACES
             AND TAG56-BIC  = SPACES
                 PERFORM C100-VALIDATION-PART
                   THRU C199-VALIDATION-PART-EX
             ELSE
CMP3A3*         IF  TAG56-OPT = "A"
CMP3A3*         AND TAG56-BIC = "UOVBSGSGXXX"
CMP3A3          IF  (TAG56-OPT = "A" 
CMP3A3          AND    TAG56-BIC = "UOVBSGSGXXX")
CMP3A3          OR  (TAG56-OPT = "A" 
CMP3A3*CMR4A1 TFSSTPL-SWFTMGTY = 101 
CMP3A3          AND TAG56-BIC = WK-C-SWIFTBICCDE)
                  PERFORM C200-VALIDATION-PART
                     THRU C299-VALIDATION-PART-EX
                ELSE
                    IF  TAG56-OPT = "A"
                    AND TAG56-BIC = "MASGSGSXXX"
                      PERFORM C300-VALIDATION-PART
                        THRU C399-VALIDATION-PART-EX
                    ELSE
                         IF  TAG56-OPT = "A"
                         AND MEPS56-IND = "Y"
                             PERFORM C400-VALIDATION-PART
                               THRU C499-VALIDATION-PART-EX
                         ELSE
                              IF  MEPS56-IND = "N"
                              AND BKAC56-IND = "Y"
                                  PERFORM C500-VALIDATION-PART
                                    THRU C599-VALIDATION-PART-EX
                              END-IF
                         END-IF
                    END-IF
                END-IF
             END-IF
             PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.
             PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.
       EXIT.

       C100-VALIDATION-PART.
             IF TAG57-OPT = SPACES
             AND TAG57-PTID = SPACES
             AND TAG57-BIC = SPACES
                 MOVE PATH-P16 TO TABLE-ARRAY
                 MOVE "Y" TO WS-OKAY
             END-IF.
CMP3A3*      IF TAG57-OPT = "A"
CMP3A3*      AND TAG57-BIC = "UOVBSGSGXXX"
CMP3A3       IF (TAG57-OPT = "A" 
CMP3A3       AND TAG57-BIC = "UOVBSGSGXXX")
CMP3A3       OR (TAG57-OPT = "A" 
CMP3A3*CMRAA1 TFSSTPL-SWFTMGTY = 101 
CMP3A3       AND TAG57-BIC = WK-C-SWIFTBICCDE)
                 MOVE PATH-P17 TO TABLE-ARRAY
                 MOVE "Y" TO WS-OKAY
             ELSE
                 IF TAG57-OPT = "A"
                 AND TAG57-BIC NOT = SPACES
                 AND TAG57-BIC NOT = "MASGSGSGXXX"
                     IF MEPS57-IND = "Y"
                        MOVE PATH-P1 TO TABLE-ARRAY
                        MOVE "Y" TO WS-ACT4
                                    WS-OKAY
                     END-IF
                     IF MEPS57-IND = "N"
                     AND BKAC57-IND = "Y"
                         MOVE PATH-P3 TO TABLE-ARRAY
                         MOVE "Y" TO WS-ACT3
                                     WS-OKAY
                     END-IF
                 END-IF
             END-IF.
       C199-VALIDATION-PART-EX.
             EXIT.
             EJECT

       C200-VALIDATION-PART.
CMP3A3*      IF TAG57-OPT = "A"
CMP3A3*      AND TAG57-BIC = "UOVBSGSGXXX"
CMP3A3       IF (TAG57-OPT = "A" 
CMP3A3       AND TAG57-BIC    = "UOVBSGSGXXX")
CMP3A3       OR (TAG57-OPT = "A" 
CMP3A3*CMR4A1 TFSSTPL-SWFTMGTY = 101 AND
CMP3A3       AND TAG57-BIC    = WK-C-SWIFTBICCDE)
                 MOVE PATH-P18 TO TABLE-ARRAY
                 MOVE "Y" TO WS-OKAY
             ELSE
                 IF TAG57-OPT = "A"
                 AND TAG57-BIC NOT = SPACES
                 AND TAG57-BIC NOT = "MASGSGSGXXX"
                     IF MEPS57-IND = "Y"
                        MOVE PATH-P4 TO TABLE-ARRAY
                        MOVE "Y" TO WS-ACT4
                                    WS-OKAY
                     END-IF
                     IF MEPS57-IND = "N"
                     AND BKAC57-IND = "Y"
                         MOVE PATH-P6 TO TABLE-ARRAY
                         MOVE "Y" TO WS-ACT3
                                     WS-OKAY
                     END-IF
                 END-IF
             END-IF.
       C299-VALIDATION-PART-EX.
            EXIT.
            EJECT
       C300-VALIDATION-PART.
CMP3A3*   IF TAG57-OPT = "A"
CMP3A3*   AND TAG57-BIC = "UOVBSGSGXXX"
CMP3A3    IF (TAG57-OPT = "A" 
CMP3A3    AND TAG57-BIC    = "UOVBSGSGXXX")
CMP3A3    OR (TAG57-OPT = "A" 
CMP3A3*CMR4A1 TFSSTPL-SWFTMGTY = 101 AND
CMP3A3    AND TAG57-BIC    = WK-C-SWIFTBICCDE)
              MOVE PATH-P19 TO TABLE-ARRAY
              MOVE "Y" TO WS-OKAY
          ELSE
              IF TAG57-OPT = "A"
              AND TAG57-BIC NOT = SPACES
              AND TAG57-BIC NOT = "MASGSGSGXXX"
                  IF MEPS57-IND = "Y"
                     MOVE PATH-P7 TO TABLE-ARRAY
                     MOVE "Y" TO WS-ACT4
                                 WS-OKAY
                  END-IF
          END-IF.
       C399-VALIDATION-PART-EX.
          EXIT.
          EJECT
       C400-VALIDATION-PART.
          IF TAG57-OPT NOT = "A"
          AND NOT(TAG57-OPT = SPACES
          AND TAG57-PTID    = SPACES
          AND TAG57-BIC     = SPACES)
              MOVE PATH-P9      TO TABLE-ARRAY
              MOVE "Y"          TO WS-ACT1
                                   WS-OKAY
          END-IF.
CMP3A3*    IF TAG57-OPT     = "A"
CMP3A3*    AND TAG57-BIC    NOT = SPACES
CMP3A3*    AND TAG57-BIC    NOT = "UOVBSGSGXXX"
CMP3A3*    AND TAG57-BIC    NOT = "MASGSGSGXXX"
CMP3A3     IF (TAG57-OPT    = "A"           
CMP3A3     AND TAG57-BIC        NOT = SPACES    
CMP3A3     AND TAG57-BIC        NOT = "UOVBSGSGXXX" 
CMP3A3     AND TAG57-BIC        NOT = "MASGSGSGXXX")
CMP3A3     OR (TAG57-OPT    = "A"           
CMP3A3*CMRA1 AND TFSSTPL-SWFTMGTY = 101         
CMP3A3     AND TAG57-BIC        NOT = SPACES    
CMP3A3     AND TAG57-BIC        NOT = "MASGSGSGXXX" 
CMP3A3     AND TAG57-BIC        NOT = WK-C-SWFTBICCDE)
               IF MEP57-IND     = "Y"
                  MOVE PATH-P10    TO TABLE-ARRAY
                  MOVE "Y"         TO WS-ACT5
                                      WS-OKAY
               END-IF
               IF MEP57-IND     = "N"
               AND BKAC57-IND   = "N"
                   MOVE PATH-P12    TO TABLE-ARRAY
                   MOVE "Y"         TO WS-ACT5
                                       WS-OKAY
               END-IF
           END-IF.
       C499-VALIDATION-PART-EX.
           EXIT.
           EJECT

       C500-VALIDATION-PART.
           IF TAG57-OPT     NOT = "A"
           AND NOT(TAG57-OPT = SPACES
           AND TAG57-PTID    = SPACES
           AND TAG57-BIC     = SPACES)
               MOVE PATH-P13    TO TABLE-ARRAY
               MOVE "Y"         TO WS-ACT2
                                   WS-OKAY

           END-IF.
CMP3A3*    IF TAG57-OPT     = "A"
CMP3A3*    AND TAG57-BIC    NOT = SPACES
CMP3A3*    AND TAG57-BIC    NOT = "UOVBSGSGXXX"
CMP3A3*    AND TAG57-BIC    NOT = "MASGSGSGXXX"
CMP3A3     IF (TAG57-OPT    = "A"           
CMP3A3     AND TAG57-BIC        NOT = SPACES    
CMP3A3     AND TAG57-BIC        NOT = "UOVBSGSGXXX" 
CMP3A3     AND TAG57-BIC        NOT = "MASGSGSGXXX")
CMP3A3     OR (TAG57-OPT    = "A"           
CMRAA1     AND TFSSTPL-SWFTMGTY = "101"            
CMP3A3     AND   TAG57-BIC  NOT = SPACES            
CMP3A3     AND   TAG57-BIC  NOT = "MASGSGSGXXX"     
CMP3A3     AND   TAG57-BIC  NOT = WK-C-SWIFTBICCODE)
                 IF  MEPS57-IND = "N"
                 AND BKAC57-IND = "Y"
                     MOVE PATH-P14     TO TABLE-ARRAY
                     MOVE "Y"          TO WS-ACT6
                                          WS-OKAY
                 END-IF
                 IF  MEPS57-IND = "N"
                 AND BKAC57-IND = "N"
                     MOVE PATH-P15     TO TABLE-ARRAY
                     MOVE "Y"          TO WS-ACT6
                                          WS-OKAY
                 END-IF
           END-IF.
       C599-VALIDATION-PART-EX.
           EXIT.
           EJECT
       D100-VALIDATION.
           MOVE "Y"          TO WS-FLAG1.
           IF  TABLE-ARRAY   = ALL "X"
               MOVE "N"      TO WS-OKAY
               INITIALIZE WK-C-RPRRSN-AREA
               MOVE "RSN011" TO WK-C-RPRCODE
               PERFORM D500-PROCESS-RPRRSN
                   THRU D599-PROCESS-RPRRSN-EX
           END-IF.
GPI201     IF  WK-C-GPI-SW = WK-C-Y
GPI201     AND SW-STP-LMT-SKP-Y
GPI201     AND TFSSTPL-SWFTMGTY = "103"
GPI201          PERFORM D110-VALIDATE-STP-BYPASS
                  THRU D119-VALIDATE-STP-BYPASS-EX
GPI201      ELSE
GPI201          MOVE SPACES TO WK-C-BYPASS-LMT-IND
GPI201      END-IF
GPI201      IF  WK-C-BYPASS-LMT-IND = WK-C-Y
GPI201          GO TO D101-BYPASS-STP-LMT
GPI201      END-IF
GPI201  END-IF.
CMP3X1*CMP3A2 MOVE SPACES TO WS-LINK-STATUS.
CMP3A2        MOVE "A1" TO WS-LINK-STATUS.
CMP3FL        IF  WS-ACCNO NOT = SPACES
CMP3A4        OR (WK-101-TAG5H-ACCNO NOT = SPACES AND
CMP3A4        TFSSTPL-SWFTMGTY = "101")
CMP3FL        INITIALIZE WK-C-RPRRSN-AREA
CMP3FL        INITIALIZE WK-C-LINK-LIMIT
CMP3FL        MOVE TFSSTPL-BNKENTTY TO WS-LINK-BNKENTTY
CMP3FL*CMP3A4 MOVE WS-ACCNO TO WS-LINK-ACCNO
CMP3A4        IF  TFSSTPL-SWFTMGTY = "101"
CMP34A            MOVE WK-101-TAG50H-ACCNO
CMP34A                TO WS-LINK-ACCNO
CMP34A        ELSE
CMP34A            MOVE WS-ACCNO       TO WS-LINK-ACCNO
CMP34A        END-IF
CMP3FL        MOVE TFSSTPL-CUYCD     TO WS-LINK-CCY
CMP3FL        MOVE TFSSTPL-AMT       TO WS-LINK-AMT
CMP3A1        MOVE "I"               TO WS-LINK-REMIND
CMP3FL        CALL "TFRVLMT" USING WK-C-LINK-LIMIT
CMP3FL        EVALUATE WS-LINK-STATUS
CMP3FL           WHEN "XX"
CMP3FL                MOVE "N"           TO WS-OKAY
CMP3FL                MOVE "RSN0311"     TO WK-C-RPRCODE
CMP3FL                PERFORM D500-PROCESS-RPRRSN
CMP3FL                  THRU D599-PROCESS-RPRRSN-EX
CMP3FL*CMP3X1    WHEN "AA"
CMP3FL*CMP3X1        MOVE "N"       TO WS-OKAY
CMP3FL*CMP3X1        MOVE "RSN0312" TO WK-C-RPRCODE
CMP3FL*CMP3X1        PERFORM D500-PROCESS-RPRRSN
CMP3FL*CMP3X1        THRU D599-PROCESS-RPRRSN-EX
CMP3FL*CMP3X1    WHEN "AC"
CMP3FL*CMP3X1        MOVE "N"       TO WS-OKAY
CMP3FL*CMP3X1        MOVE "RSN0313" TO WK-C-RPRCODE
CMP3FL*CMP3X1        PERFORM D500-PROCESS-RPRRSN
CMP3FL*CMP3X1        THRU D599-PROCESS-RPRRSN-EX
CMP3FL*CMP3X1    WHEN "AS"
CMP3FL*CMP3X1        MOVE "N"       TO WS-OKAY
CMP3FL*CMP3X1        MOVE "RSN0314" TO WK-C-RPRCODE
CMP3FL*CMP3X1        PERFORM D500-PROCESS-RPRRSN
CMP3FL*CMP3X1        THRU D599-PROCESS-RPRRSN-EX
CMP3FL       END-EVALUATE
CMP3FL*CMP3X1END-IF
CMP3X1       END-IF.
GP1201       D101-BYPASS-STP-LMT.
              IF TAB-VAL(01) NOT = "X"
                 MOVE TAB-VAL(01) TO TAB-VL2(01)
                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(02) NOT = "X"
                 MOVE TAB-VAL(02) TO TAB-VL2(02)
                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(03) NOT = "X"
                 MOVE TAB-VAL(03) TO TAB-VL2(03)
                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(04) NOT = "X"
                 MOVE TAB-VAL(04) TO TAB-VL2(04)
                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(05) NOT = "X"
                 MOVE TAB-VAL(05) TO TAB-VL2(05)
                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(06) NOT = "X"
                 MOVE TAB-VAL(06) TO TAB-VL2(06)
                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(07) NOT = "X" AND WS-OKAY = "Y"
                 MOVE "Y" TO TAB-VL2(07)
CMP3A2*   IF TFSSTPL-AMT <= WK-N-IRM1STP
CMP3A2    IF (TFSSTPL-AMT <= WK-N-IRM1STP
CMP3A2    AND WS-LINK-STATUS = "A1")
CMP3X1    OR WS-LINK-STATUS = "A0"
GPI201    OR (WK-C-GPI-SW = WK-C-Y
GPI201    AND SW-STP-LMT-SKP-Y
GPI201    AND WK-C-BYPASS-LMT-IND = WK-C-Y)
              MOVE "PSTP" TO WS-STPTYP
              MOVE "N" TO TAB-VL2(07)
              MOVE "X" TO TAB-VL2(08)
          END-IF
          PERFORM D300-LOGGING THRU D399-LOGGING-EX
CMP3X1    IF WS-LINK-STATUS = "A0"
CMP3X1       GO TO D199-VALIDATION-EX
CMP3X1    END-IF
       END-IF.
       IF TAB-VAL(08) NOT = "X" AND WS-OKAY = "Y"
GPI201    IF (WK-C-GPI-SW = WK-C-Y
GPI201    AND SW-STP-LMT-SKP-Y
GPI201    AND WK-C-BYPASS-LMT-IND = WK-C-Y)
GPI201       CONTINUE
GPI201    ELSE
             IF TFSSTPL-AMT > WK-N-IRM1STP
                MOVE "2STP" TO WS-STPTYP
                MOVE "Y" TO TAB-VL2(08)
5Q1ARV          INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV          MOVE "RSN0023" TO WK-C-RPRCODE
5Q1ARV          PERFORM D500-PROCESS-RPRRSN
5Q1ARV            THRU D599-PROCESS-RPRRSN-EX
             ELSE
                IF TFSSTPL-AMT > WK-N-IRM1STP
CMP3A2          OR (WS-LINK-STATUS = "AA"
CMP3A2       OR WS-LINK-STATUS = "AC"
CMP3A2       OR WS-LINK-STATUS = "AS")
                MOVE "1STP" TO WS-STPTYP
                MOVE "N" TO TAB-VL2(08)
5Q1ARV           INITIALIZE WK-C-RPRRSN-AREA
5Q1ARV           MOVE "RSN0039" TO WK-C-RPRCODE
5Q1ARV           PERFORM D500-PROCESS-RPRRSN
5Q1ARV           THRU D599-PROCESS-RPRRSN-EX
             END-IF
           END-IF
GPI201   END-IF
         PERFORM D300-LOGGING THRU D399-LOGGING-EX
         END-IF.
       D199-VALIDATION-EX.
       EXIT.
       EJECT
GP1201  D110-VALIDATE-STP-BYPASS.
GP1201*----------------------------------------------------------------*
GP1201* THIS WILL CALL TRFVBACU TO CHECK IF THE UOB BRANCH IND = Y   *
GP1201*----------------------------------------------------------------*
GP1201
GP1201           MOVE SPACES        TO WK-C-BYPASS-LMT-IND.
GP1201
GP1201*----------------------------------------------------------------*
GP1201*--Bypass STP Limit if Debit Leg is a VOSTRO account
GP1201           IF  WK-C-DR-PMODE = "CA"
GP1201               OR WK-C-DR-PMODE = "FCCA"
GP1201               MOVE WK-C-Y    TO WK-C-BYPASS-LMT-IND
GP1201               GO TO D119-VALIDATE-STP-BYPASS-EX
GP1201           END-IF.
GP1201
GP3M00*----------------------------------------------------------------*
GP3M00*--Bypass STP Limit if Dr Leg = NOSTRO and CR Leg = VOSTRO
GP3M00           IF  WK-C-GP13-SW = WK-C-Y
GP3M00           AND WK-C-NSLMT-SW = WK-C-Y
GP3M01           IF (WK-C-DR-PMODE = "NOSTRO"
GP3M01               OR WK-C-DR-PMODE = "MAS")
GP3M00           AND (WK-C-VBAC-ACCTYP = "C"
GP3M00               OR WK-C-VBAC-ACCTYP = "F")
GP3M00               MOVE WK-C-Y    TO WK-C-BYPASS-LMT-IND
GP3M00               GO TO D119-VALIDATE-STP-BYPASS-EX
GP3M00           END-IF
GP3M00           END-IF.
GP3M00
GP1201*----------------------------------------------------------------*
GP1201*--Check the cover received indicator
GP1201           IF  TFSSTPL-SWFTMGTY = "103"
GP1201               MOVE SPACE     TO WK-C-COV-SW
GP1201               PERFORM R100-READ-UFMJICON
GP1201               THRU R199-READ-UFMJICON-EX
GP1201               IF  WK-C-COV-SW = WK-C-Y
GP1201                   MOVE WK-C-Y TO WK-C-BYPASS-LMT-IND
GP1201                   GO TO D119-VALIDATE-STP-BYPASS-EX
GP1201               END-IF
GP1201           END-IF.
GP1201
GP1201*----------------------------------------------------------------*
GP1201*--Check if Sending BankID is a Nostro - UOB Branch
GP1201           INITIALIZE WK-C-VBACU-RECORD.
GP1201
GP1201           MOVE TFSSTPL-SENBNKID TO WK-C-VBACU-BANKID.
GP1201           CALL "TRFVBACU" USING WK-C-VBACU-RECORD.
GP1201
GP1201           IF  WK-C-VBACU-ERROR-CD = SPACES
GP1201               IF  WK-C-VBACU-UOBBRH = WK-C-Y
GP1201                   MOVE WK-C-Y TO WK-C-BYPASS-LMT-IND
GP1201               ELSE
GP1201                   MOVE SPACES TO WK-C-BYPASS-LMT-IND
GP1201               END-IF
GP1201           END-IF.
GP1201
GPI201 D119-VALIDATE-STP-BYPASS-EX.
          EXIT.
GPI201    EJECT

       D200-VALIDATION.
          MOVE WS-BANKID            TO WK-VTB1-BANKID.
          MOVE WS-INTEMBNKID        TO WK-VTB1-INTEMBNKID.
          MOVE WS-ACBNKID           TO WK-VTB1-ACBNKID.
          MOVE WS-ACCNO             TO WK-VTB1-BANKAC.
          MOVE WK-C-VBAC-ACCTYP     TO WK-VTB1-BANKACTYP.
          MOVE WS-INTEMBNKACC       TO WK-VTB1-INTEMBNKACC.
          MOVE WS-ACBNKACC          TO WK-VTB1-ACBNKACC.
          MOVE TABLE-ARR2           TO WK-VTB1-DATAB1.

          IF   WS-ACT1 = "Y"
          OR   WS-ACT4 = "Y"
          OR   WS-ACT5 = "Y"
               MOVE WS-ACT1          TO WK-VTB1-ACT1
               MOVE WS-ACT4          TO WK-VTB1-ACT4
               MOVE WS-ACT5          TO WK-VTB1-ACT5
               MOVE "MAS"            TO WK-VTB1-PMODE
SM1TY1*    MOVE TFSSTPL-BNKENTTY  TO TFSBNKET-BNKENTTY
SM1TY1*    READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
SM1TY1*        NOT INVALID KEY
SM1TY1*    MOVE TFSBNKET-MASNOSTR TO WK-VTB1-BANKAC
SM1TY1*    END-READ
SM1TY1*    MOVE SPACES         TO WK-VTB1-BANKAC
          END-IF
          IF   WS-ACT2 = "Y"
          OR   WS-ACT3 = "Y"
          OR   WS-ACT6 = "Y"
               MOVE WS-ACT2          TO WK-VTB1-ACT2
               MOVE WS-ACT3          TO WK-VTB1-ACT3
               MOVE WS-ACT6          TO WK-VTB1-ACT6
               MOVE "TT"             TO WK-VTB1-PMODE
         END-IF

         IF   WS-OKAY = "Y"
              MOVE "N"              TO WK-VTB1-ERROR-FOUND
              MOVE WS-STPTYP        TO WK-VTB1-STPTYP
         ELSE
              MOVE SPACES           TO WK-VTB1-ACT1
                                       WK-VTB1-ACT2
                                       WK-VTB1-ACT3
                                       WK-VTB1-ACT4
                                       WK-VTB1-ACT5
                                       WK-VTB1-ACT6
                                       WK-VTB1-STPTYP
              MOVE "Y"              TO WK-VTB1-ERROR-FOUND
         END-IF.
         MOVE "N"                  TO WS-FLAG1.
         PERFORM D300-LOGGING      THRU D399-LOGGING-EX.

       D299-VALIDATION-EX.
         EXIT.
         EJECT

       D300-LOGGING.
         MOVE WK-VTB1-PARALNO     TO WK-LOGG-PARALNO.
         MOVE WK-VTB1-SEQNUM      TO WK-LOGG-SEQNUM.
         MOVE TABLE-ARR2          TO WK-LOGG-DATAB1.
         MOVE "B1"                TO WK-LOGG-TABTYP.
         MOVE WK-VTB1-ACT         TO WK-LOGG-ACTB1.
         CALL "TRFLOGGCL" USING WK-LOGG
                                WS-FLAG1
                                WS-FLAG2.
         IF WK-LOGG-ERROR-FOUND = "Y"
            GO TO D399-LOGGING-EX
         END-IF.
       D399-LOGGING-EX.
         EXIT.
         EJECT

       D400-MEPS-VALIDATION.
         IF NOT(TAG56-OPT = SPACES
         AND TAG56-PTID = SPACES
         AND TAG56-BIC  = SPACES)
         AND TAG56-OPT  = "A"
             IF TAG56-PTID = SPACES
                 MOVE TAG56-BIC      TO WK-C-VBBAS-BANKID
                 CALL "TRFVBBAS" USING WK-C-VBBAS-RECORD
                 IF WK-C-VBBAS-SHIFTNO NOT = SPACES
                    MOVE "Y"        TO MEPS56-IND
                    MOVE WK-C-VBBAS-SHIFTNO TO WK-VTB1-SHIFTNO
                 ELSE
                    MOVE "N"        TO MEPS56-IND
                 END-IF
             END-IF
             IF TAG56-PTID NOT = SPACES
             OR MEPS56-IND  = "N"
                MOVE "N"            TO MEPS56-IND
                MOVE TFSSTPL-BNKENTTY TO WK-N-VBAC-BNKENTTY
                MOVE TAG56-BIC      TO WK-C-VBAC-BANKID
                MOVE TFSSTPL-CUYCD  TO WK-C-VBAC-CUYCD
                CALL "TRFVBAC" USING WK-C-VBAC-RECORD
                IF WK-C-VBAC-BNKACNO = TAG56-PTID
                AND TAG56-PTID      NOT = SPACES
                AND WK-C-VBAC-ERROR-CD = SPACES
                OR TAG56-PTID       = SPACES
                AND WK-C-VBAC-ERROR-CD = SPACES
                    MOVE "Y"        TO BKAC56-IND
                ELSE
                    MOVE "N"        TO BKAC56-IND
                END-IF
             END-IF.
         END-IF.
         IF NOT(TAG57-OPT = SPACES
         AND TAG57-PTID        = SPACES
         AND TAG57-BIC         = SPACES)
         AND TAG57-OPT         = "A"
             IF  TAG57-PTID = SPACES
                 MOVE TAG57-BIC         TO WK-C-VBBAS-BANKID
                 CALL "TRFVBBAS" USING WK-C-VBBAS-RECORD
                 IF  WK-C-VBBAS-SHIFTNO NOT = SPACES
                     MOVE "Y"           TO MEPS57-IND
                     MOVE WK-C-VBBAS-SHIFTNO TO WK-VTB1-SHIFTNO
                 ELSE
                     MOVE "N"           TO MEPS57-IND
                 END-IF
             END-IF
             IF  TAG57-PTID NOT = SPACES
             OR  MEPS57-IND = "N"
                 MOVE "N"               TO MEPS57-IND
                 MOVE TFSSTPL-BNKENTITY TO WK-N-VBAC-BNKENTITY
                 MOVE TAG57-BIC         TO WK-C-VBAC-BANKID
                 MOVE TFSSTPL-CUYCD     TO WK-C-VBAC-CUYCD
                 CALL "TRFVBAC" USING WK-C-VBAC-RECORD
                 IF  WK-C-VBAC-BNKACNO  = TAG57-PTID
                 AND TAG57-PTID     NOT = SPACES
                 AND WK-C-VBAC-ERROR-CD = SPACES
                 OR  TAG57-PTID     = SPACES
                 AND WK-C-VBAC-ERROR-CD = SPACES
                     MOVE "Y"           TO BKAC57-IND
                 ELSE
                     MOVE "N"           TO BKAC57-IND
                 END-IF
             END-IF.

       D499-MEPS-VALIDATION-EX.
             EXIT.

GPI201 R100-READ-UFIMIJCON.
GPI201
GPI201          INITIALIZE UFIMIJCON-REC WK-C-UFIMIJCON.
GPI201
GPI201          MOVE WK-VTB1-PARALNO TO UFIMIJCON-QUENUM
GPI201          MOVE WK-VTB1-SEQNUM  TO UFIMIJCON-QUESUF
GPI201
GPI201          READ UFIMIJCON KEY IS EXTERNALLY-DESCRIBED-KEY
GPI201
GPI201          IF NOT WK-C-SUCCESSFUL
GPI201              GO TO R199-READ-UFIMIJCON-EX
GPI201          END-IF.
GPI201
GPI201*-- Turn ON the switch if COVER is already received.
GPI201          IF  UFIMIJCON-STATUS EQUAL WK-C-A
GPI201              MOVE WK-C-Y     TO WK-C-COV-SW
GPI201          END-IF.
GPI201
GPI201      R199-READ-UFIMIJCON-EX.
GPI201          EXIT.
GP1201
      *=================================================================
       GP3C00  D060-EVAL-TAG57-CD.
      *=================================================================*
GP3C00**--This routine will check Tag57 C/D Lines 1-2 if it exact matches
GP3C00**--Tag Validation table. If Match, treat it as Tag57A w/ our Own BIC
GP3C00**--(UOVBSGSGXXX - parameterized) to further proceed with STP processing.
GP3C00**--E.g Raw Tag57D Line1:/123456789
GP3C00**--        Line2:UNITED OVERSEAS BANK
GP3C00**--        Line3:SINGAPORE
GP3C00**--        Line4:BUKIT BATOK
GP3C00**--        Line5:SG
GP3C00**--If Line 2 "UNITED OVERSEAS BANK" exact matches Tag validation table
GP3C00**--system will treat this as Tag57A Line1: *blank
GP3C00**--        Line2: UOVBSGSGXXX
GP3C00**--        Line3: *blank
GP3C00**--        Line4: *blank
GP3C00**--        Line5: *blank
GP3C00**--and proceed with BAU STP processing.
GP3C00
GP3C00           INITIALIZE              WK-C-VTAG57-RECORD.
GP3C00           MOVE TAG57-OPT          TO WK-C-VTAG57-OPTION.
GP3C00
GP3C00**--Tag57C:
GP3C00           IF TAG57-OPT = "C"
GP3C00               IF TAG57-PTID = SPACES
GP3C00                   GO TO D699-EVAL-TAG57-CD-EX
GP3C00               ELSE
GP3C00                   MOVE TAG57-PTID TO WK-C-VTAG57-INFO(1)
GP3C00               END-IF
GP3C00           END-IF.
GP3C00
GP3C00**--Tag57D:
GP3C00           IF TAG57-OPT = "D"
GP3C00               IF TAG57-PTID = SPACES
GP3C00               AND TAG57-NAME = SPACES
GP3C00                   GO TO D699-EVAL-TAG57-CD-EX
GP3C00               ELSE
GP3C02*GP3C00           MOVE TAG57-PTID TO WK-C-VTAG57-INFO(1)
GP3C00                   MOVE TAG57-NAME TO WK-C-VTAG57-INFO(2)
GP3C02*GP3C00           MOVE TAG57-LINE-3 TO WK-C-VTAG57-INFO(3)
GP3C02*GP3C00           MOVE TAG57-LINE-4 TO WK-C-VTAG57-INFO(4)
GP3C02*GP3C00           MOVE TAG57-LINE-5 TO WK-C-VTAG57-INFO(5)
GP3C00               END-IF
GP3C00           END-IF.
GP3C00
GP3C00           MOVE TFSSTPL-BNKENTTY TO WK-C-VTAG57-I-BNKENTTY.
GP3C00
GP3C00**--Check Tag57 if either Lines 1-2 matches Tag validation table.
GP3C00           CALL "TRVFTAG57" USING WK-C-VTAG57-RECORD.
GP3C00           CANCEL "TRVFTAG57".
GP3C00
GP3C00           IF WK-C-VTAG57-ERROR-CD = SPACES
GP3C00               CONTINUE
GP3C00           ELSE
GP3C00           GO TO D699-EVAL-TAG57-CD-E
GP3C00           END-IF.
GP3C00*--If it match, override w/ Tag57A:<Own BIC> (parameterized)
GP3C00           IF  WK-C-VTAG57-VALID = "Y"
GP3C00               MOVE SPACES         TO TAG57-PTID
GP3C00                                      WS-ACBNKACC
GP3C00                                      WS-ACCNO
GP3C00                                      WK-VTB1-ACBNKNM
GP3C00                                      WK-VTB1-ACBNKADR1
GP3C00                                      WK-VTB1-ACBNKADR2
GP3C00                                      WK-VTB1-ACBNKADR3
GP3C00               MOVE "A"            TO TAG57-OPT
GP3C00               MOVE WK-C-VTAG57-BIC TO TAG57-BIC
GP3C00                                      WS-ACBNKID
GP3C00                                      WS-BANKID
GP3C00           END-IF.
GP3C00*=================================================================*
GP3C00      D699-EVAL-TAG57-CD-EX.
GP3C00*=================================================================*
GP3C00           EXIT.
5Q1ARV      D500-PROCESS-RPRRSN SECTION.
5Q1ARV      D500-ENTRY.
5Q1ARV           MOVE WK-VTB1-PARALNO   TO WK-C-RRSN-QUENUM.
5Q1ARV           MOVE WK-VTB1-SEQNUM    TO WK-C-RRSN-QUESUF.
5Q1ARV           MOVE WK-TRNNO          TO WK-C-RRSN-TRNNO.
5Q1ARV           MOVE WK-C-FUNCTID      TO WK-C-RRSN-FUNCTID.
5Q1ARV           MOVE WK-C-SEGCDE       TO WK-C-RRSN-SEGCDE.
5Q1ARV           MOVE SPACES            TO WK-C-RRSN-SEGDESC.
5Q1ARV           MOVE WK-N-STAFFIND     TO WK-C-RRSN-STAFFIND.
5Q1ARV           MOVE WS-ACCNO          TO WK-C-RRSN-ACCNO.
CMP3A4           IF WK-101-TAG50H-ACCNO NOT = SPACES AND
CMP3A4              TFSSPTL-SWFTMGTY = "101"
CMP3A4              MOVE WK-101-TAG50H-ACCNO TO WK-C-RRSN-ACCNO
CMP3A4           END-IF.
5Q1ARV           MOVE WK-C-QRATE        TO WK-C-RRSN-QRATE.
5Q1ARV           MOVE WK-N-SYSDTE       TO WK-C-RRSN-RPRDTE.
5Q1E1 *5Q1ARV     MOVE WK-C-RPRCODE      TO WK-C-RRSN-RSNCDE.
5Q1E1            IF  WK-C-RPRCODE = SPACE
5Q1E1               MOVE "RSN9999"      TO WK-C-RRSN-RSNCDE
5Q1E1            ELSE
5Q1E1               MOVE WK-C-RPRCODE   TO WK-C-RRSN-RSNCDE
5Q1E1            END-IF.
5Q1E1            MOVE SPACES            TO WK-C-RRSN-RSNDESC.
5Q1ARV           MOVE WK-C-RPRPGM       TO WK-C-RRSN-RPRPGM.
5Q1ARV           CALL "TRFGRRSN" USING WK-C-RRSN-RECORD.
5Q1ARV      D599-PROCESS-RPRRSN-EX.
5Q1ARV           EXIT.
5Q1ARV           EJECT
       Z000-END-PROGRAM.
              CLOSE TFSSTPL
SM1TY1* TFSBNKET
CMP3A3              TFSBNKET
GPI201              UFIMIJCON
                    TFSCLSYS.
              EXIT PROGRAM.
