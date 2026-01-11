       IDENTIFICATION DIVISION.
      ***********************
       PROGRAM-ID. TRFVTE2.
       AUTHOR. TYK.
       DATE-WRITTEN. JUN 04.
      *DESCRIPTION : TABLE E2 VALIDATION.
      *              SUBROUTINE - CREDIT PARTY CHECKING FIELD 56/57 FOR
      *              INCOMING SWIFT MT202/203 OR RTGS+ MT202/203 FCY
      *
      *=================================================================
      * GP4D03 - VENTEH  - 16/10/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              GPI Day4 (POST IMPEM IMPROVEMENT)
      *              (For HK only)
      *              - STP #5 Inward (Inward receipt,
      *                IAFT & In-as-out) TT/RTGS- STP
      *                by CCY.
      *              - JIRA PCRMAPKGPI-2109
      *              - Bypass the STP currency setup
      *                checking to avoid double
      *                RSNCODE
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
      * GP3M01 - VENTEH  - 26/05/2020 - CASH MANAGEMENT ROAD MAP
      *              - gpi DAY 4 (Retro from GPI Day3 HO)
      *              - PCRMAPKGPI-1331
      *              - To add "RTGS" Mode pay on
      *                the evaluation of DR NOSTRO.
      *              - "RTGS" PMODE will be passed from
      *                TRFVTD1 only when:
      *                1. Incoming RTGS
      *                2. Doesnt have Tag53/54
      *                3. Sending BIC is MEPS
      *-----------------------------------------------------------------
      * GP4D00 - VENADG  - 14/02/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              GPI Day4 (In-Country Req)
      *              - STP #5 Inward (Inward receipt,
      *                IAFT & In-as-out) TT/RTGS- STP
      *                by CCY.
      *              - To enable ITT STP CCY check
      *-----------------------------------------------------------------
      * GP4D01 07/05/2020 VENTEH  - CASH MANAGEMENT ROAD MAP - P19
      *              GPI Day4 (In-Country Req)
      *              - BAU Bug fix
      *              - Previously WS-ACCNO LENGTH is 11, when
      *                13 length accno passes to WS-ACCNO,it
      *                was truncated into 11 byte BNK Routing
      *                code
      *              - Rectified to expand WS-ACCNO and
      *                EVALUATE first IF ACCNO length in the
      *                incoming msg/ TFSBNKAC has Routing Code
      *                (e.g 101). IF yes remove the routing
      *                code in WS-ACCNO
      *---------------------------------------------------------------------*
      * 7Q1EM1  25/11/2016 TMPEYM  - REM Q1 2017 RELEASE
      *                             - e-Req 47511 Refinement of
      *                               Duplicate checking for Inw
      *                             - Recompiled due to changes made in
      *                               VSTPL copy book.
      *---------------------------------------------------------------------*
      * 5Q2JE1  04/03/2015 TMPJAE  - 14HOREM024 14HOREM028 14HOREM029
      *                               NON PSTP REASON ENHANCEMENT
      *                             - Remove line of code that don't
      *                               need a reason.
      *---------------------------------------------------------------------
      * 5Q1JM1  - TMPJZM   - 23/12/2014 - 14HOREM024/14HOREM029/14HOREM028
      *                             - Retrofit NON PSTP Reason
      *                               Enhancement Project
      *---------------------------------------------------------------------
       ENVIRONMENT DIVISION.
      ********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-AS400.
       OBJECT-COMPUTER.  IBM-AS400.
       SPECIAL-NAMES.    LOCAL-DATA IS LOCAL-DATA-AREA
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

1D1VKE     SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET

       ID1VKE ORGANIZATION IS INDEXED
       ID1VKE ACCESS MODE IS RANDOM
       ID1VKE RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       ID1VKE FILE STATUS IS WK-C-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
      ***************
           FD TFSSPTPL
              LABEL RECORDS ARE OMITTED
              DATA RECORD IS WK-C-TFSSPTPL.
          01 WK-C-TFSSPTPL.
             COPY DDS-ALL-FORMATS OF TFSSPTPL.
          01 WK-C-TFSSPTPL-1.
             COPY TFSSPTPL.

           FD TFSCLSYS
              LABEL RECORDS ARE OMITTED
              DATA RECORD IS TFSCLSYS-REC.
          01 TFSCLSYS-REC.
             COPY DDS-ALL-FORMATS OF TFSCLSYS.
          01 TFSCLSYS-REC-1.
             COPY TFSCLSYS.

           FD TFSBNKAC
              LABEL RECORDS ARE OMITTED
              DATA RECORD IS WK-C-TFSBNKAC.
          01 WK-C-TFSBNKAC.
             COPY DDS-ALL-FORMATS OF TFSBNKAC.
          01 WK-C-TFSBNKAC-1.
             COPY TFSBNKAC.

       ID1VKE FD TFSBNKET
       ID1VKE LABEL RECORDS ARE OMITTED
       ID1VKE DATA RECORD IS TFSBNKET-REC.
       ID1VKE 01 TFSBNKET-REC.
       ID1VKE COPY DDS-ALL-FORMATS OF TFSBNKET.
       ID1VKE 01 TFSBNKET-REC-1.
       ID1VKE COPY TFSBNKET.

           WORKING-STORAGE SECTION.
      ***********************
          01 WK-C-COMMON.
             COPY ASCMWS.

          01 TAGS6-FORMAT.
             05 TAGS6-LINE-1.
                07 TAGS6-FIL1 PIC X(2).
                07 TAGS6-OPT PIC X(1).
                07 TAGS6-FIL2 PIC X(1).
                07 TAGS6-PTID.
                   09 TAGS6-PTID-1 PIC X(02).
                   09 TAGS6-PTID-2 PIC X(35).
             05 TAGS6-LINE-2 PIC X(35).

      05  TAG56-BIC  REDEFINES TAG56-LINE-2.
          07  TAG56A-SUB1       PIC X(4).
          07  TAG56A-SUB2       PIC X(2).
          07  TAG56A-SUB3       PIC X(2).
          07  TAG56A-SUB4       PIC X(3).
          07  TAG56A-FILLER     PIC X(24).
      05  TAG56-LOC  REDEFINES TAG56-LINE-2
                         PIC X(35).
      05  TAG56-NAME REDEFINES TAG56-LINE-2
                         PIC X(35).
      05  TAG56-LINE-3         PIC X(35).
      05  TAG56-LINE-4         PIC X(35).
      05  TAG56-LINE-5         PIC X(35).

      01  TAG57-FORMAT.
      05  TAG57-LINE-1.
          07  TAG57-FIL1       PIC X(2).
          07  TAG57-OPT        PIC X(1).
          07  TAG57-FIL2       PIC X(1).
          07  TAG57-PTID.
              09  TAG57-PTID-1 PIC X(02).
              09  TAG57-PTID-2 PIC X(35).
      05  TAG57-LINE-2         PIC X(35).
      05  TAG57-BIC  REDEFINES TAG57-LINE-2.
          07  TAG57A-SUB1       PIC X(4).
          07  TAG57A-SUB2       PIC X(2).
          07  TAG57A-SUB3       PIC X(2).
          07  TAG57A-SUB4       PIC X(3).
          07  TAG57A-FILLER     PIC X(24).
      05  TAG57-LOC  REDEFINES TAG57-LINE-2
                         PIC X(35).
      05  TAG57-NAME REDEFINES TAG57-LINE-2
                         PIC X(35).
      05  TAG57-LINE-3         PIC X(35).
      05  TAG57-LINE-4         PIC X(35).
      05  TAG57-LINE-5         PIC X(35).

      01  TAG58-FORMAT.
      05  TAG58-LINE-1.
          07  TAG58-FIL1       PIC X(2).
          07  TAG58-OPT        PIC X(1).
          07  TAG58-FIL2       PIC X(1).
          07  TAG58-PTID.
              09  TAG58-PTID-1 PIC X(02).
              09  TAG58-PTID-2 PIC X(35).
      05  TAG58-LINE-2         PIC X(35).
      05  TAG58-BIC  REDEFINES TAG58-LINE-2.
          07  TAG58A-SUB1       PIC X(4).
          07  TAG58A-SUB2       PIC X(2).
          07  TAG58A-SUB3       PIC X(2).
          07  TAG58A-SUB4       PIC X(3).
          07  TAG58A-FILLER     PIC X(24).
      05  TAG58-LOC  REDEFINES TAG58-LINE-2
                         PIC X(35).

       05 TAG58-NAME REDEFINES TAG58-LINE-2
                         PIC X(35).
       05 TAG58-LINE-3   PIC X(35).
       05 TAG58-LINE-4   PIC X(35).
       05 TAG58-LINE-5   PIC X(35).

   01 TABLE-ARRAY.
       05 TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".

   01 TABLE-ARR2.
       05 TAB-VL2 OCCURS 20 TIMES PIC X VALUE "X".

   01 PATH-P1               PIC X(20)
                            VALUE "XYXXXXXXXXXXXXXXXXXXXX".
   01 PATH-P2               PIC X(20)
                            VALUE "YXYXXXXXXXXXXXXXXXXXXXX".
   01 PATH-P3               PIC X(20)
                            VALUE "XXYXXXXXXXXXXXXXXXXXXXX".
   01 PATH-P4               PIC X(20)
                            VALUE "XWNYXXXXXXXXXXXXXXXXXXXX".
   01 PATH-P5               PIC X(20)
                            VALUE "XNXYXXXXXXXXXXXXXXXXXXXX".
   01 PATH-P6               PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P7               PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P8               PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P9               PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P10              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P11              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P12              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P13              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P14              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P15              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P16              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P17              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P18              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".
   01 PATH-P19              PIC X(20)
                            VALUE "XXXXXXXXXXCXXXXXXXXXXXXX".

   01 WK-C-PARADATA.
       05 WK-C-PARAVALU    PIC X(20).
       05 WK-N-PARAVALU    REDEFINES WK-C-PARAVALU

      05  WK-N-TRNPSTP                PIC 9(13)V99.
      05  WK-N-IRM1STP                PIC 9(13)V99.

01  WK-C-WORK-AREA.
      05  FIRST-TIME                  PIC X(01) VALUE "Y".
      05  WS-FLAG1                    PIC X(01) VALUE SPACE.
      05  WS-FLAG2                    PIC X(01) VALUE SPACE.
      05  WS-ACT1                     PIC X(01) VALUE SPACE.
      05  WS-STPTYTP                  PIC X(04) VALUE SPACE.
      05  BKAC57-IND                  PIC X(01) VALUE SPACE.
      05  BKAC58-IND                  PIC X(01) VALUE SPACE.
      05  WS-OKAY                     PIC X(01) VALUE SPACE.
GP4D01*  05  WS-ACCNO                  PIC X(11) VALUE SPACE.
GP4D01*  05  WS-ACBNKACC               PIC X(11) VALUE SPACE.
GP4D01   05  WS-ACCNO                  PIC X(15) VALUE SPACE.
GP4D01   05  WS-ACBNKACC               PIC X(15) VALUE SPACE.
      05  WS-BENBNKACC                PIC X(35) VALUE SPACE.
      05  WS-BANKID                   PIC X(11) VALUE SPACE.
      05  WS-ACBNKID                  PIC X(11) VALUE SPACE.
      05  WS-BENBNKID                 PIC X(11) VALUE SPACE.
      05  WS-LCAMT                    PIC 9(13)V99 VALUE ZEROS.
G2BL00   05  WK-C-BYPASS-LMT-IND      PIC X(01) VALUE SPACE.
G2BL00   05  WK-C-GPI-SW              PIC X(01) VALUE SPACE.
G2BL00   05  WK-C-DR-PMODE            PIC X(08) VALUE SPACE.
GP3A00   05  WK-C-GPI3-SW             PIC X(01) VALUE SPACE.
GP3A00   05  WK-C-TAG57-CD-SW         PIC X(01) VALUE SPACE.
GP3M00   05  WK-C-NSLMT-SW            PIC X(01) VALUE SPACE.
GP4D00   05  WK-C-STP-CCY-SW          PIC X(01) VALUE SPACE.
GP4D03   05  WK-C-STP-CCY-IMP-SW      PIC X(01) VALUE SPACE.

G2BL00   01  WK-C-LIT-GPI.
G2BL00      05  WK-C-Y                PIC X(01) VALUE "Y".
GP3A00      05  WK-C-GPI3-SW-PARCD    PIC X(10)
GP3A00                                   VALUE "GPISWITCH3".
GP3A00      05  WK-C-TAG57-SW-PARCD   PIC X(10)
GP3A00                                   VALUE "GPI3T57SW".
GP3A00      05  WK-C-TAG57-MT-PARCD   PIC X(10)
GP3A00                                   VALUE "GPI3T57MT".
GP3M00      05  WK-C-NSLMT-PARCD      PIC X(10)
GP3M00                                   VALUE "GPI3NSLMT".
GP4D00      05  WK-C-STPCCY-PARCD     PIC X(10)
GP4D00                                   VALUE "GPI4ISTPCY".
GP4D03      05  WK-C-STPCCY2-PARCD    PIC X(10)
GP4D03                                   VALUE "GPI4ISTPCY2".
GP3A00      01  WK-C-MT-TAG57-TBL     PIC X(18) VALUE SPACES.
GP3A00      05  WK-C-MT-TAG57         PIC X(03) OCCURS 6 TIMES.

GP4D01 01  SUB                        PIC 9(02) VALUE ZEROES.
GP4D01 01  WK-N-CTR                   PIC 9(02) VALUE ZEROES.
GP4D01 01  WK-N-ACCLEN                PIC 9(02) VALUE ZEROES.
GP4D01 01  WS-ACCNO1                  PIC X(11) VALUE SPACES.

ID1VKE 01 WK-C-SWIFTBICCODE     PIC X(11)    VALUE SPACE.
ID1VKE 01 WK-C-RTGSBICCODE      PIC X(11)    VALUE SPACE.
5Q1JM1 01 WK-C-RPRRSN-AREA.
5Q1JM1    05 WK-C-SEGCODE       PIC X(01)    VALUE SPACE.
5Q1JM1    05 WK-N-STAFFIND      PIC S9(02)   VALUE ZEROS.
5Q1JM1    05 WK-C-ACCNO         PIC X(15)    VALUE SPACE.
5Q1JM1    05 WK-C-QRATE         PIC X(02)    VALUE SPACE.
5Q1JM1    05 WK-C-RPRRCODE      PIC X(07)    VALUE SPACE.
5Q1JM1    05 WK-C-TRNNO         PIC X(12)    VALUE SPACE.
5Q1JM1    05 WK-C-TRUCTID       PIC X(08)    VALUE SPACE.
5Q1JM1 01 WK-N-SYSDFE           PIC S9(08)   VALUE ZEROS.
5Q1JM1 01 WK-C-RPRPGM           PIC X(10)    VALUE "TRFVTE2".
CMP3F1 01 WS-C-STPLMT-FLAG      PIC X(01)    VALUE "N".
REM269 01 WK-C-LCLUYCD          PIC X(03).
CMP3F1 01 WK-C-LINK-LIMIT.
CMP3F1    05 WK-C-LINK-AREA-INPUT.
CMP3F1       10 WS-LINK-BNKENTTY PIC X(02).
CMP3F1       10 WS-LINK-ACCNO    PIC X(15) VALUE 0.
CMP3F1       10 WS-LINK-CCY      PIC X(03) VALUE SPACES.
CMP3F1       10 WS-LINK-AMT      PIC S9(13)V99 VALUE 0.
CMP3F1       10 WS-LINK-REMIND   PIC X(01).
CMP3F1    05 WK-C-LINK-AREA-OUTPUT.
CMP3F1       10 WS-LINK-STATUS   PIC X(02) VALUE SPACES.

           COPY VSTPL.
           COPY VBAC.
           COPY VBANO.
           COPY XPARA.
           COPY GERTE.
           COPY LOGG.
ID1VKE     COPY XGSPA.
5Q1JM1     COPY RRSN.
G2BL00     COPY GPISTPSW.
G2BL00     COPY VBACU.
GP3A00     COPY VTAG57.
GP4D00     COPY VSTPC.

           LINKAGE SECTION.
      ****************
           COPY VTE2.

           PROCEDURE DIVISION USING WK-VTE2.
      ********************************
           MAIN-MODULE.

               PERFORM A100-MAIN-PROGRAM THRU A199-MAIN-PROGRAM-EX.
               GO TO Z000-END-PROGRAM.

           A100-MAIN-PROGRAM.

CMP3F1* GET STP LIMIT INDICATOR

CMP3F1    INITIALIZE WK-C-XGSPA-RECORD.
CMP3F1    MOVE "RSYSSTPLMT"         TO WK-C-XGSPA-GHPARCD.
CMP3F1    CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
CMP3F1    IF WK-C-XGSPA-ERROR-CD    = SPACES
CMP3F1        MOVE WK-C-XGSPA-GHPARVAL TO WS-C-STPLMT-FLAG
CMP3F1    ELSE
CMP3F1        MOVE SPACES            TO WS-C-STPLMT-FLAG
CMP3F1    END-IF.

REM269*----------------------------------------------------------------*
REM269*    GET SYSTEM PARAMETERS FOR LOCAL CURRENCY CODE.             *
REM269*----------------------------------------------------------------*
         INITIALIZE WK-C-XGSPA-RECORD.
         MOVE "RSYTCLLCUY"         TO WK-C-XGSPA-GHPARCD.
         CALL "TRFXGSPA"           USING WK-C-XGSPA-RECORD.
         IF WK-C-XGSPA-ERROR-CD    = SPACES
             MOVE WK-C-XGSPA-GHPARVAL TO WK-C-LCUYCD
         ELSE
             MOVE SPACES            TO WK-C-LCUYCD
         END-IF

         INITIALIZE WK-VTE2-OUTPUT
                    WK-LOGG
                    WK-C-WORK-AREA.
         MOVE ALL "X"              TO TABLE-ARRAY.
         MOVE ALL "X"              TO TABLE-ARRAY2.
         MOVE "Y"                  TO FIRST-TIME.

5Q1JM1    MOVE ZEROS               TO WK-C-RRSN-QUENUM
5Q1JM1                            WK-C-RRSN-QUESUF
5Q1JM1                            WK-C-RRSN-STAFFIND
5Q1JM1                            WK-C-RRSN-SEQNUM
5Q1JM1                            WK-C-RRSN-RPRDTE.

G2BL00    MOVE WK-VTE2-DR-PMODE    TO WK-C-DR-PMODE.

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
                DISPLAY "TFSCLSYS - OPEN FILE ERROR - TFSCLSYS"
                DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
             END-IF
             OPEN I-O TFSBNKAC
             IF NOT WK-C-SUCCESSFUL
                AND WK-C-FILE-STATUS NOT = "41"
                DISPLAY "TFSBNKAC - OPEN FILE ERROR - TFSBNKAC"
                DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS

           END-IF
ID1VKE     OPEN     INPUT TFSBNKET
           IF NOT WK-C-SUCCESSFUL
           AND WK-C-FILE-STATUS NOT = "41"
               DISPLAY "TRFVFE1 - OPEN FILE ERROR - TFSBNKET"
               DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
           END-IF

           INITIALIZE           WK-C-XGSPA-RECORD
           MOVE "SPTRGSBIC"     TO WK-C-XGSPA-GHPARCD
           CALL "TRFXGSPA"      USING WK-C-XGSPA-RECORD
           IF WK-C-XGSPA-ERROR-CD = SPACES THEN
               MOVE WK-C-XGSPA-GHPARVAL TO WK-C-RTGSBICCDCE
           ELSE
               MOVE SPACES      TO WK-C-RTGSBICCDCE
           END-IF
ID1VKE

           END-IF.

           MOVE WK-VTE2-PARALNO TO TFSSTPL-PARALNO.
           MOVE WK-VTE2-SEQNUM  TO TFSSTPL-SEQNUM.

           READ TFSSTPL
               KEY IS EXTERNALLY-DESCRIBED-KEY.

           IF WK-C-SUCCESSFUL
               MOVE "N"         TO WS-OKAY
               MOVE TFSSTPL-TAG56 TO TAG56-FORMAT
               MOVE TFSSTPL-TAG57 TO TAG57-FORMAT
               MOVE TFSSTPL-TAG58 TO TAG58-FORMAT
ID1VKE         MOVE TFSSTPL-BNKENTITY TO TFSBNKET-BNKENTITY
ID1VKE         READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
ID1VKE         INVALID KEY
ID1VKE             MOVE SPACES TO WK-C-SWIFTBICCDCE
ID1VKE         NOT INVALID KEY
ID1VKE             MOVE TFSBNKET-SWFTBNK TO WK-C-SWIFTBICCDCE
ID1VKE         END-READ
               IF TAG56-OPT = SPACES
               AND TAG56-PTID = SPACES
               AND TAG56-BIC = SPACES
                   PERFORM A200-INITIAL-SUBROUTINE
                       THRU A299-INITIAL-SUBROUTINE-EX
                   PERFORM A300-MOVE-TAG-VALUES
                       THRU A399-MOVE-TAG-VALUES-EX
                   PERFORM B100-PATH-CHOICE THRU B199-PATH-CHOICE-EX
5Q1JM1      ELSE
5Q1JM1          INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1          MOVE "RSN0105" TO WK-C-RPRCODE
5Q1JM1          PERFORM D500-PROCESS-RPRRSN
5Q1JM1              THRU D599-PROCESS-RPRRSN-EX
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
               DISPLAY "TRFVTE2  - READ TFSCLSYS ERROR"
               DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
               GO TO Z000-END-PROGRAM.

      **********MOVE TFSCLSYS-SYSDTE   TO L-N-G-SYSDTE.
      5Q1JM1 MOVE TFSCLSYS-SYSDTE   TO WK-N-SYSDTE.
      **********MOVE TFSCLSYS-LCNTRYCD  TO L-C-G-L-CNTRYCD.
      **********MOVE TFSCLSYS-LCUYCD    TO L-C-G-L-CUYCD.

      *----------------------------------------------------------------*
      *    GET SYSTEM PARAMETERS FOR PSTP & 1STP                       *
      *----------------------------------------------------------------*
           IF TFSSTPL-IMSGTYPE = "M"
               MOVE "IRMMSGP"     TO WK-C-XPARA-PARACD
           ELSE
               MOVE "IRSMSGP"     TO WK-C-XPARA-PARACD
           END-IF.
           CALL "TRFXPARA"        USING WK-C-XPARA-RECORD.
           IF WK-C-XPARA-ERROR-CD NOT = SPACES
               DISPLAY "TREEEDT  - TRFXPARA ROUTINE ERROR"
               DISPLAY "FILE STATUS - " WK-C-XPARA-FS
               DISPLAY "ERROR ID  - " WK-C-XPARA-ERROR-CD
               DISPLAY "KEY       - " WK-C-XPARA-INPUT
               GO TO Z000-END-PROGRAM
           ELSE
               MOVE WK-C-XPARA-PARAVALU
                   TO WK-C-PARAVALU
               MOVE WK-N-PARAVALU
                   TO WK-N-IRMPSPTP
           END-IF.
           IF TFSSTPL-IMSGTYPE = "M"
               MOVE "IRMMSG1"     TO WK-C-XPARA-PARACD
           ELSE
               MOVE "IRSMSG1"     TO WK-C-XPARA-PARACD
           END-IF.
           CALL "TRFXPARA"        USING WK-C-XPARA-RECORD.
           IF WK-C-XPARA-ERROR-CD NOT = SPACES
               DISPLAY "TREEEDT  - TRFXPARA ROUTINE ERROR"
               DISPLAY "FILE STATUS - " WK-C-XPARA-FS
               DISPLAY "ERROR ID  - " WK-C-XPARA-ERROR-CD
               DISPLAY "KEY       - " WK-C-XPARA-INPUT
               GO TO Z000-END-PROGRAM
           ELSE
               MOVE WK-C-XPARA-PARAVALU
                   TO WK-C-PARAVALU
               MOVE WK-N-PARAVALU
                   TO WK-N-IRMPSPTP
           END-IF.

       TO  WK-N-IRM1STP
       END-IF.
G2BL00*---------------------------------------------------------------------*
G2BL00* RETRIEVE GPI TECHNICAL SWITCH FROM SYSTEM PARAMETER FILE            *
G2BL00*---------------------------------------------------------------------*
G2BL00
G2BL00      INITIALIZE                 WK-C-XGSPA-RECORD
G2BL00                                   WK-C-GPI-SW
G2BL00        REPLACING ALPHANUMERIC BY SPACES
G2BL00                  NUMERIC        BY ZEROS.
G2BL00
G2BL00      MOVE "GPISWITCH2"          TO  WK-C-XGSPA-GHPARCD.
G2BL00      CALL "TRFXGSPA"            USING WK-C-XGSPA-RECORD.
G2BL00
G2BL00      IF WK-C-XGSPA-ERROR-CD = SPACES
G2BL00          MOVE WK-C-XGSPA-GHPARVAL TO  WK-C-GPI-SW
G2BL00      END-IF.
G2BL00
G2BL00*---------------------------------------------------------------------*
G2BL00* RETRIEVE GPI STP SWITCH FROM SYSTEM PARAMETER FILE                  *
G2BL00*---------------------------------------------------------------------*
G2BL00
G2BL00      INITIALIZE                 WK-C-XGSPA-RECORD
G2BL00                                   SW-STP-LMT-SKP
G2BL00        REPLACING ALPHANUMERIC BY SPACES
G2BL00                  NUMERIC        BY ZEROS.
G2BL00
G2BL00      MOVE "GPISTPSW"            TO  WK-C-XGSPA-GHPARCD.
G2BL00      CALL "TRFXGSPA"            USING WK-C-XGSPA-RECORD.
G2BL00
G2BL00      IF WK-C-XGSPA-ERROR-CD = SPACES
G2BL00          MOVE WK-C-XGSPA-GHPARVAL(2:1)
G2BL00                                   TO  SW-STP-LMT-SKP
G2BL00      END-IF.
G2BL00
GP3A00*-->Retrieve GPI Day 3 Technical Switch
GP3A00      INITIALIZE                 WK-C-XGSPA-RECORD
GP3A00                                   WK-C-GPI3-SW.
GP3A00
GP3A00      MOVE WK-C-GPI3-SW-PARCD    TO  WK-C-XGSPA-GHPARCD.
GP3A00      CALL "TRFXGSPA"            USING WK-C-XGSPA-RECORD.
GP3A00
GP3A00      IF WK-C-XGSPA-ERROR-CD = SPACES
GP3A00          MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP3A00                                   TO  WK-C-GPI3-SW
GP3A00      END-IF.
GP3A00
GP3A00*-->Retrieve GPI Day3 Tag57 C/D Enhancement Switch
GP3A00      INITIALIZE                 WK-C-XGSPA-RECORD
GP3A00                                   WK-C-TAG57-CD-SW.
GP3A00
GP3A00      MOVE WK-C-TAG57-SW-PARCD   TO  WK-C-XGSPA-GHPARCD.
GP3A00      CALL "TRFXGSPA"            USING WK-C-XGSPA-RECORD.

GP3A00
GP3A00      IF  WK-C-XGSPA-ERROR-CD = SPACES
GP3A00          MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP3A00               TO  WK-C-TAG57-CD-SW
GP3A00      END-IF.
GP3A00
GP3A00*-->Retrieve GPI Day3 Tag57 C/D Enhancement Eligable MT Types
GP3A00          INITIALIZE            WK-C-XGSPA-RECORD
GP3A00                                 WK-C-MT-TAG57-TBL.
GP3A00
GP3A00          MOVE WK-C-TAG57-MT-PARCD  TO  WK-C-XGSPA-GHPARCD.
GP3A00          CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
GP3A00
GP3A00      IF  WK-C-XGSPA-ERROR-CD = SPACES
GP3A00          MOVE WK-C-XGSPA-GHPARVAL
GP3A00               TO  WK-C-MT-TAG57-TBL
GP3A00      END-IF.
GP3M00
GP3M00*-->Retrieve GPI Day3 Nostro Bypass STP Limit Enhancement
GP3M00          INITIALIZE            WK-C-XGSPA-RECORD
GP3M00                                 WK-C-NSLMT-SW.
GP3M00
GP3M00          MOVE WK-C-NSLMT-PARCD  TO  WK-C-XGSPA-GHPARCD.
GP3M00          CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
GP3M00
GP3M00      IF  WK-C-XGSPA-ERROR-CD = SPACES
GP3M00          MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP3M00               TO  WK-C-NSLMT-SW
GP3M00      END-IF.
GP4D00
GP4D00*-->Retrieve GPI Day4 In-Country ITT STP by Currency Switch
GP4D00          INITIALIZE            WK-C-XGSPA-RECORD
GP4D00                                 WK-C-STP-CCY-SW
GP4D00
GP4D00          MOVE WK-C-STPCCY-PARCD  TO  WK-C-XGSPA-GHPARCD.
GP4D00          CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
GP4D00
GP4D00      IF  WK-C-XGSPA-ERROR-CD = SPACES
GP4D00          MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP4D00               TO  WK-C-STP-CCY-SW
GP4D00      END-IF.
GP4D03
GP4D03*-->Retrieve GPI Day4 ITT STP by Currency Improvement Switch
GP4D03          INITIALIZE            WK-C-XGSPA-RECORD
GP4D03                                 WK-C-STP-CCY-IMP-SW.
GP4D03
GP4D03          MOVE WK-C-STPCCY2-PARCD  TO  WK-C-XGSPA-GHPARCD.
GP4D03          CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
GP4D03
GP4D03      IF  WK-C-XGSPA-ERROR-CD = SPACES
GP4D03          MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP4D03               TO  WK-C-STP-CCY-IMP-SW
GP4D03      ELSE
GP4D03          MOVE "N"              TO  WK-C-STP-CCY-IMP-SW

GP4D03           END-IF.

GP4D01*          GET ACC LENGTH
GP4D01           INITIALIZE WK-C-XGSPA-RECORD.
GP4D01           MOVE "RSYACCLEN"        TO WK-C-XGSPA-GHPARCD
GP4D01           CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
GP4D01           IF WK-C-XGSPA-ERROR-CD = SPACES
GP4D01               MOVE WK-C-XGSPA-GHPARVAL(1:2) TO WK-N-ACCLEN
GP4D01           ELSE
GP4D01               MOVE 13             TO WK-N-ACCLEN
GP4D01           END-IF.

                A299-INITIAL-SUBROUTINE-EX.
                    EXIT.

                A300-MOVE-TAG-VALUES.
                    IF TAG57-BIC NOT = SPACES
                       AND TAG57-OPT = "A"
                        MOVE TAG57-BIC TO WS-ACBNKID
                                         WS-BANKID
                        MOVE TAG57-PTID TO WS-ACBNKACC
                                         WS-ACCNO
GP4D01*-->      Remove routing code of tag 57 ACCNO
GP4D01           IF WS-ACCNO NOT = SPACES
GP4D01               MOVE ZEROS          TO WK-N-CTR
GP4D01               MOVE SPACES         TO WS-ACCNO1
GP4D01               PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 15
GP4D01                   IF WS-ACCNO(SUB:1) NOT = SPACES
GP4D01                       ADD 1       TO WK-N-CTR
GP4D01                   END-IF
GP4D01               END-PERFORM
GP4D01               IF WK-N-CTR = WK-N-ACCLEN
GP4D01                   MOVE WS-ACCNO(4:10) TO WS-ACCNO1
GP4D01                   MOVE WS-ACCNO1 TO WS-ACCNO
GP4D01               END-IF
GP4D01           END-IF
GP4D01           END-IF.

GP3A00*-->GPI Day3 Tag57 C/D Enhancement
GP3A00           IF WK-C-GPI3-SW = "Y"
GP3A00           AND WK-C-TAG57-CD-SW = "Y"
GP3A00               IF TAG57-OPT = "C" OR "D"
GP3A00                   AND (TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(1)
GP3A00                   OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(2)
GP3A00                   OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(3)
GP3A00                   OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(4)
GP3A00                   OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(5)
GP3A00                   OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(6))
GP3A00                   PERFORM D600-EVAL-TAG57-CD
GP3A00                       THRU D699-EVAL-TAG57-CD-EX
GP3A00               END-IF
GP3A00           END-IF.

                IF TAG58-BIC NOT = SPACES

       AND TAG58-OPT   = "A"
           MOVE TAG58-BIC         TO  WS-BENBNKID
                                  WS-BANKID
           MOVE TAG58-PTID        TO  WS-BENBKACC
                                  WS-ACCNO
       END-IF.

       IF TAG58-NAME NOT = SPACES
       AND TAG58-OPT   = "D"
           MOVE TAG58-PTID        TO  WS-BENBKACC
                                  WS-ACCNO
           MOVE TAG58-NAME        TO  WK-VTE2-BENBKNM
           MOVE TAG58-LINE-3      TO  WK-VTE2-BENBKADR1
           MOVE TAG58-LINE-4      TO  WK-VTE2-BENBKADR2
           MOVE TAG58-LINE-5      TO  WK-VTE2-BENBKADR3
       END-IF.

           MOVE TFSSTPL-BNKENTTY  TO WK-N-VBAC-BNKENTTY.
           MOVE WS-BANKID         TO WK-C-VBAC-BANKID.
           MOVE TFSSTPL-CUYCD     TO WK-C-VBAC-CUYCD.
           CALL "TRFVBAC" USING WK-C-VBAC-RECORD.
           IF WK-C-VBAC-ACUBDUI NOT = SPACES
               MOVE WK-C-VBAC-ACUBDUI TO WK-VTE2-ACUBDUI
               IF WS-ACCNO = SPACES
                   MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
               END-IF
       GP4D01*--> To properly evaluate 1st PRIO Bank Account No pulled
       GP4D01*--> if w/ Routing Code or not and assign accordingly.
       GP4D01       MOVE ZEROS         TO WK-N-CTR
       GP4D01       MOVE SPACES        TO WS-ACCNO1
       GP4D01       PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 15
       GP4D01           IF WS-ACCNO(SUB:1) NOT = SPACES
       GP4D01               ADD 1     TO  WK-N-CTR
       GP4D01           END-IF
       GP4D01       END-PERFORM
       GP4D01       IF WK-N-CTR = WK-N-ACCLEN
       GP4D01           MOVE WS-ACCNO(4:10) TO WS-ACCNO1
       GP4D01           MOVE WS-ACCNO1     TO WS-ACCNO
       GP4D01       END-IF
               ELSE
                   MOVE "D"           TO WK-VTE2-ACUBDUI
           END-IF.

           A399-MOVE-TAG-VALUES-EX.
               EXIT.
           EJECT

       B100-PATH-CHOICE.
           PERFORM D400-BKAC-VALIDATION.
           IF TAG57-OPT   = SPACES
           AND TAG57-PTID = SPACES
           AND TAG57-BIC  = SPACES
           OR  TAG57-OPT  = "A"
       ID1VKE* AND TAG57-BIC  = "UOVBSGSGXXX"

       ID1VKE     AND TAG57-BIC     = WK-C-SWIFTBICCDE
                  PERFORM C100-VALIDATION-PART
                          THRU C199-VALIDATION-PART-EX

                  END-IF.
                  IF BKAC57-IND     = "Y"
                  AND TAG57-OPT     = "A"
       ID1VKE*    AND TAG57-BIC NOT = "UOVBSGSGXXX"
       ID1VKE*    AND TAG57-BIC NOT = WK-C-SWIFTBICCDE
       ID1VKE*    AND TAG57-BIC NOT = "MASGSGSGXXX"
       ID1VKE     AND TAG57-BIC NOT = WK-C-RTGSBICCDE
                  PERFORM C200-VALIDATION-PART
                          THRU C299-VALIDATION-PART-EX

                  END-IF.
                  PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.
                  PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.
                  EXIT.

       C100-VALIDATION-PART.
                  IF TAG58-OPT     = "A"
                  AND TAG58-BIC    NOT = SPACES
       ID1VKE*    AND TAG58-BIC NOT = "UOVBSGSGXXX"
       ID1VKE     AND TAG58-BIC NOT = WK-C-SWIFTBICCDE
       ID1VKE*    AND TAG58-BIC NOT = "MASGSGSGXXX"
       ID1VKE     AND TAG58-BIC NOT = WK-C-RTGSBICCDE
                  AND BKAC58-IND   = "Y"
                  MOVE PATH-P1     TO TABLE-ARRAY
                  MOVE "Y"         TO WS-OKAY
                  END-IF.
       C199-VALIDATION-PART-EX.
                  EXIT.
                  EJECT

       C200-VALIDATION-PART.
                  IF TAG58-OPT     NOT = "A"
                  AND NOT(TAG58-OPT = SPACES
                  AND TAG58-PTID   = SPACES
                  AND TAG58-BIC    = SPACES)
       ID1VKE*    AND TAG58-BIC NOT = "UOVBSGSGXXX"
       ID1VKE     AND TAG58-BIC NOT = WK-C-SWIFTBICCDE
       ID1VKE*    AND TAG58-BIC NOT = "MASGSGSGXXX"
       ID1VKE     AND TAG58-BIC NOT = WK-C-RTGSBICCDE
                  MOVE PATH-P2     TO TABLE-ARRAY
                  MOVE "Y"         TO WS-ACT1
                                  WS-OKAY

                  END-IF.
                  IF TAG58-OPT     = "A"
                  AND TAG58-BIC    NOT = SPACES
       ID1VKE*    AND TAG58-BIC NOT = "UOVBSGSGXXX"
       ID1VKE     AND TAG58-BIC NOT = WK-C-SWIFTBICCDE
       ID1VKE*    AND TAG58-BIC NOT = "MASGSGSGXXX"
       ID1VKE     AND TAG58-BIC NOT = WK-C-RTGSBICCDE
                  AND BKAC58-IND   = "Y"

           MOVE PATH-P3         TO TABLE-ARRAY
           MOVE "Y"             TO WS-ACT1
                               WS-OKAY

           END-IF.
           IF  TAGS8-OPT        = "A"
           AND TAGS8-BIC        NOT = SPACES
           AND TAGS8-BIC        NOT = "UOVBSGSGXXX"
           AND TAGS8-BIC        NOT = WK-C-SWIFTBICCDE
           AND TAGS8-BIC        NOT = "MASGSGSGXXX"
           AND TAGS8-BIC        NOT = WK-C-RTGSBICCDE
           AND BKAC58-IND       = "N"
           MOVE PATH-P4         TO TABLE-ARRAY
           MOVE "Y"             TO WS-ACT1
                               WS-OKAY

           END-IF.
           C299-VALIDATION-PART-EX.
           EXIT.
           EJECT

      D100-VALIDATION.
           MOVE "Y"             TO WS-FLAG1.
           IF  TABLE-ARRAY      = ALL "X"
           MOVE "N"             TO WS-OKAY
           5Q1JM1 INITIALIZE WK-C-RPRRSN-AREA
           5Q1JM1 MOVE "RSN0117" TO WK-C-RPRCODE
           5Q1JM1 PERFORM D500-PROCESS-RPRRSN
           5Q1JM1 THRU D599-PROCESS-RPRRSN-EX

           END-IF.

G2BL00**If GPI 2B technical switch and GPI STP limit switch are ON,
G2BL00**check if crediting NOSTRO BIC is UOB branch. If it is, bypass
G2BL00**limit check. If not, continue with the BAU behavior.
G2BL01*G2BL00IF WK-C-GPI-SW = WK-C-Y
G2BL00    IF SW-STP-LMT-SKP-Y
G2BL00    AND FSSTPL-SWFTMGTY = "202"
G2BL00        PERFORM D110-VALIDATE-STP-BYPASS
G2BL00        THRU D119-VALIDATE-STP-BYPASS-EX
G2BL00    ELSE
G2BL00        MOVE SPACES TO WK-C-BYPASS-LMT-IND
G2BL00    END-IF
G2BL00    IF WK-C-BYPASS-LMT-IND = WK-C-Y
G2BL00        GO TO D101-SKIP-OTH-LIMT
G2BL00    END-IF.
G2BL01*G2BL00END-IF.

CMP3F1    IF WS-C-STPLMT-FLAG = "Y"
CMP3F1        MOVE "A1" TO WS-LINK-STATUS
CMP3F1        IF WS-ACCNO NOT = SPACES
CMP3F1            INITIALIZE WK-C-RPRRSN-AREA
CMP3F1            INITIALIZE WK-C-LINK-LIMIT
CMP3F1            MOVE FSSTPL-BNKENTITY TO WS-LINK-BNKENTITY
CMP3F1            MOVE WS-ACCNO TO WS-LINK-ACCNO
CMP3F1            MOVE FSSTPL-CUYCD TO WS-LINK-CCY
CMP3F1            MOVE FSSTPL-AMT TO WS-LINK-AMT

CMP3F1      MOVE "I"               TO WS-LINK-REMIND
CMP3F1      CALL "TRFVLMT" USING WK-C-LINK-LIMIT
CMP3F1      EVALUATE WS-LINK-STATUS
CMP3F1          WHEN "XX"
CMP3F1              MOVE "N"       TO WS-OKAY
CMP3F1              MOVE "RSN0311" TO WK-C-RPRCODE
CMP3F1              PERFORM D500-PROCESS-RPRRSN
CMP3F1                  THRU D599-PROCESS-RPRRSN-EX
CMP3F1      END-EVALUATE
CMP3F1      END-IF
CMP3F1      END-IF.

G2BL00 D101-SKIP-OTH-LIMT.
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

REM269**** IF TFSSTPL-CUYCD NOT = TFSCLSYS-LCUYCD
REM269     IF TFSSTPL-CUYCD NOT = WK-C-LCUYCD
              MOVE "BT"           TO WK-C-GERTE-RTE-TYP
              MOVE TFSSTPL-CUYCD  TO WK-C-GERTE-CUYCD
              MOVE TFSSTPL-BNKENTITY TO WK-N-GERTE-BNKENTITY
              CALL "TRFGERTE" USING WK-C-GERTE-RECORD
              COMPUTE WS-LCAMT ROUNDED = TFSSTPL-AMT
      * WK-N-GERTE-EXCH-RTE
                                     / WK-N-GERTE-FXRATEUT
          ELSE
              MOVE TFSSTPL-AMT    TO WS-LCAMT
          END-IF.
          IF TAB-VAL(04) NOT = "X" AND WS-OKAY = "Y"
              MOVE "Y"            TO TAB-VL2(04)
CMP3F1*         IF WS-LCAMT <= WK-N-IRMPTSP
CMP3F1          IF (WS-LCAMT <= WK-N-IRMPTSP
CMP3F1          AND WS-C-STPLMT-FLAG NOT = "Y")
CMP3F1          OR (WS-LCAMT <= WK-N-IRMPTSP
CMP3F1          AND WS-C-STPLMT-FLAG = "Y"
CMP3F1          AND WS-LINK-STATUS = "A1")
CMP3F1          OR (WS-LINK-STATUS = "A0"
CMP3F1          AND WS-C-STPLMT-FLAG = "Y")
G2BL00     OR (SW-STP-LMT-SKP-Y
G2BL01*G2BL00 AND WK-C-GPI-SW = WK-C-Y
G2BL00     AND WK-C-BYPASS-LMT-IND = WK-C-Y)
              MOVE "PSTP"         TO WS-STPTY
              MOVE "N"            TO TAB-VL2(04)
              MOVE "X"            TO TAB-VL2(05)
          END-IF

GP4D00*----------------------------------------------------------------*
GP4D00*>--If within Parm STP Limit and w/out Acc/CIF/Seg STP Limt setup
GP4D00*>--further check ITT STP CCY table.
GP4D00     IF WK-C-STP-CCY-SW  = "Y"
GP4D00     AND WS-LCAMT        <= WK-N-IRMSTP
GP4D00*GP4D02 AND WS-C-STPLMT-FLAG = "Y"
GP4D00*GP4D02 AND WS-LINK-STATUS = "A1"
GP4D02     AND ((WS-C-STPLMT-FLAG = "Y"
GP4D02     AND WS-LINK-STATUS = "A1")
GP4D02     OR  WS-C-STPLMT-FLAG NOT = "Y")
GP4D00         PERFORM D120-EVAL-STP-CCY
GP4D00         THRU D120-EVAL-STP-CCY-EX
GP4D00         END-IF
CMP3F1         PERFORM D300-LOGGING THRU D399-LOGGING-EX
CMP3F1         IF WS-LINK-STATUS = "A0"
CMP3F1         AND WS-C-STPLMT-FLAG = "Y"
CMP3F1             GO TO D199-VALIDATION-EX
CMP3F1         END-IF
GP4D00         END-IF.
              IF TAB-VAL(05) NOT = "X" AND WS-OKAY = "Y"
                  MOVE TAB-VAL(05)  TO TAB-VL2(05)
G2BL00            IF (SW-STP-LMT-SKP-Y
G2BL01*G2BL00 AND WK-C-GPI-SW = WK-C-Y
G2BL00            AND WK-C-BYPASS-LMT-IND = WK-C-Y)
G2BL00                CONTINUE
G2BL00            ELSE
                  IF WS-LCAMT  > WK-N-IRM1STP
                      MOVE "2STP" TO WS-STPTYP
                      MOVE "Y"   TO TAB-VL2(05)
5Q1JM1                INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1                MOVE "RSN0023" TO WK-C-RPRRCODE
5Q1JM1                PERFORM D500-PROCESS-RPRRSN
5Q1JM1                THRU D599-PROCESS-RPRRSN-EX
                  ELSE
                      IF WS-LCAMT  > WK-N-IRMSTP
CMP3F1                 OR (WS-C-STPLMT-FLAG = "Y"
CMP3F1                 AND (WS-LINK-STATUS = "AA"
CMP3F1                 OR WS-LINK-STATUS = "AC"
CMP3F1                 OR WS-LINK-STATUS = "AS"))
                          MOVE "1STP" TO WS-STPTYP
                          MOVE "N"    TO TAB-VL2(05)
5Q1JM1                    INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1                    MOVE "RSN0028" TO WK-C-RPRRCODE
5Q1JM1                    PERFORM D500-PROCESS-RPRRSN
5Q1JM1                    THRU D599-PROCESS-RPRRSN-EX
                      END-IF
                  END-IF
              END-IF.
              D199-VALIDATION-EX.
              EXIT.
              EJECT
G2BL00*----------------------------------------------------------------*

G2BL00* VALIDATE IF CREDITING NOSTRO BIC IS UOB BRANCH BY CALLING    *
G2BL00* TRFVBACU TO READ TFSBNKACF FILE.                             *
G2BL00*----------------------------------------------------------------*
G2BL00      D110-VALIDATE-STP-BYPASS.                                
G2BL00                                                                 
G2BL00           MOVE SPACES           TO WK-C-BYPASS-LMT-IND.        
G2BL00                                                                 
G2BL00**--Bypass STP Limit if Debit Leg is a VOSTRO account           
G2BL00           IF  WK-C-DR-PMODE = "CA"                             
G2BL00               OR  WK-C-DR-PMODE = "FCCA"                       
G2BL00               MOVE WK-C-Y        TO WK-C-BYPASS-LMT-IND        
G2BL00               GO TO D119-VALIDATE-STP-BYPASS-EX                
G2BL00           END-IF.                                              
G2BL00                                                                 
G3M00**--Bypass STP Limit if Dr Leg = NOSTRO and CR Leg = NOSTRO/VOSTRO
G3M00           IF  WK-C-GP13-SW = WK-C-Y                             
G3M00           AND WK-C-NSLMT-SW = WK-C-Y                            
G3M00               IF  WK-C-DR-PMODE = "NOSTRO"                      
G3M00+GP3M01          IF  (WK-C-DR-PMODE = "NOSTRO"                   
G3M01               OR WK-C-DR-PMODE(1:4) = "RTGS")                   
G3M01               AND (WK-C-VBAC-ACCTYP = "C"                       
G3M00               OR WK-C-VBAC-ACCTYP = "F"                         
G3M00               OR WK-C-VBAC-ACCTYP = "N"                         
G3M00               OR WK-C-VBAC-ACCTYP = "V")                        
G3M00                   MOVE WK-C-Y  TO WK-C-BYPASS-LMT-IND           
G3M00                   GO TO D119-VALIDATE-STP-BYPASS-EX             
G3M00               END-IF                                            
G3M00           END-IF.                                               
G3M00                                                                 
G2BL00**--Check if sending BankID is a Nostro - UOB Branch            
G2BL00           INITIALIZE           WK-C-VBACU-RECORD               
G2BL00               REPLACING NUMERIC BY ZEROS                       
G2BL00               ALPHANUMERIC BY SPACES.                          
G2BL00           MOVE TFSSTPL-SENBNKID TO WK-C-VBACU-BANKID.          
G2BL00           CALL "TRFVBACU"       USING WK-C-VBACU-RECORD.       
G2BL00                                                                 
G2BL00           IF  WK-C-VBACU-ERROR-CD = SPACES                     
G2BL00               IF WK-C-VBACU-UOBBRH = WK-C-Y                    
G2BL00                   MOVE WK-C-Y    TO WK-C-BYPASS-LMT-IND        
G2BL00               ELSE                                              
G2BL00                   MOVE SPACES TO WK-C-BYPASS-LMT-IND           
G2BL00               END-IF                                           
G2BL00           END-IF.                                              
G2BL00                                                                 
G2BL00      D119-VALIDATE-STP-BYPASS-EX.                              
G2BL00           EXIT.                                                
G2BL00           EJECT.                                               
                                                                      
G4P40D00      D120-EVAL-STP-CCY.                                      
G4P40D00*----------------------------------------------------------------*
G4P40D00* This routine will call TRFVSTPC to check if CCY is eligable  *
G4P40D00* for STP and if AMT is within CCY STP Limit.                  *
G4P40D00*    A0 = Currency is setup and within limit                   *
G4P40D00*    A1 = Currency is setup however exceeds STP Limit          *

GP4000* A2 = Currency is NOT setup                                           *
GP4000*---------------------------------------------------------------------*
GP4000      INITIALIZE                  WK-C-VSTPC-RECORD
GP4000                                   WK-C-RPRRSN-AREA.
GP4000
GP4000      MOVE  TFSSTPL-IMSGTYPE      TO  WK-C-VSTPC-I-IMSGTYPE.
GP4000      MOVE  TFSSTPL-CUYCOD        TO  WK-C-VSTPC-I-CUYCD.
GP4000      MOVE  TFSSTPL-AMT           TO  WK-N-VSTPC-I-AMT.
GP4000      CALL  "TRFVSTPC"            USING  WK-C-VSTPC-RECORD.
GP4000
GP4000      IF    WK-C-VSTPC-ERROR-CD = SPACES
GP4000          CONTINUE
GP4000      ELSE
GP4000          GO TO D120-EVAL-STP-CCY-EX
GP4000      END-IF.
GP4000
GP4000      EVALUATE  WK-C-VSTPC-STATUS
GP4000          WHEN "A0"
GP4000              GO TO D120-EVAL-STP-CCY-EX
GP4000          WHEN "A1"
GP4000*--------->RSN0370 - Inward STP CCY: Beyond CCY STP Limit
GP4000              MOVE "RSN0370"      TO WK-C-RPRCODE
GP4000          WHEN "A2"
GP4003*--------->Improvement: Checking of STP currency has move to ITT
GP4003*          Handler
GP4003          IF WK-C-STP-CCY-IMP-SW = "Y"
GP4003              GO TO D120-EVAL-STP-CCY-EX
GP4003          ELSE
GP4000*--------->RSN0369 - Inward STP CCY: Non STP Currency.
GP4000              MOVE "RSN0369"      TO WK-C-RPRCODE
GP4003          END-IF
GP4000          WHEN OTHER
GP4000              GO TO D120-EVAL-STP-CCY-EX
GP4000      END-EVALUATE.
GP4000
GP4000      MOVE SPACES                TO WS-STPTYP.
GP4000      MOVE "N"                   TO TAB-VL2(04)
GP4000                                 TAB-VL2(05)
GP4000                                 WS-OKAY.
GP4000
GP4000      PERFORM D500-PROCESS-RPRRSN
GP4000          THRU D599-PROCESS-RPRRSN-EX.
GP4000
GP4000  D120-EVAL-STP-CCY-EX.
GP4000      EXIT.
GP4000
/         D200-VALIDATION.
GP4000
           MOVE WS-BANKID              TO WK-VTE2-BANKID.
           MOVE WS-ACBNKID             TO WK-VTE2-ACBNKID.
           MOVE WS-BENBNKID            TO WK-VTE2-BENBNKID.
           MOVE WS-ACCNO               TO WK-VTE2-BANKAC.
           MOVE WK-C-VBAC-ACCTYP       TO WK-VTE2-BANKACTYP.
           MOVE WS-ACBNKACC            TO WK-VTE2-ACBNKACC.

       MOVE WS-BENBKACC        TO WK-VTE2-BENBKACC.
       MOVE TABLE-ARR2        TO WK-VTE2-DATA2.
       MOVE "TT202"           TO WK-VTE2-PMODE.

           IF TAG58-OPT = "A"
               MOVE SPACES    TO WK-VTE2-BENBKACC
           END-IF.
           IF WS-OKAY = "Y"
               MOVE WS-ACT1   TO WK-VTE2-ACT
               MOVE WS-STPTYP TO WK-VTE2-STPTYP
               MOVE "N"       TO WK-VTE2-ERROR-FOUND
           ELSE
               MOVE SPACES    TO WK-VTE2-ACT
                               WK-VTE2-STPTYP
               MOVE "Y"       TO WK-VTE2-ERROR-FOUND
           END-IF.
       MOVE "N"               TO WS-FLAG1.
       PERFORM D300-LOGGING   THRU D399-LOGGING-EX.

   D299-VALIDATION-EX.
       EXIT.
       EJECT

   D300-LOGGING.
       MOVE WK-VTE2-PARALNO   TO WK-LOGG-PARALNO.
       MOVE WK-VTE2-SEQNUM    TO WK-LOGG-SEQNUM.
       MOVE WK-VTE2-ACT       TO WK-LOGG-ACTE2.
       MOVE TABLE-ARR2        TO WK-LOGG-DATAE2.
       CALL "TRFLOGGCL" USING WK-LOGG
                               WS-FLAG1
                               WS-FLAG2.

           IF WK-LOGG-ERROR-FOUND = "Y"
               GO TO D399-LOGGING-EX
           END-IF.
   D399-LOGGING-EX.
       EXIT.
       EJECT

   D400-BKAC-VALIDATION.
           IF NOT(TAG57-OPT   = SPACES
           AND TAG57-PTID     = SPACES
           AND TAG57-BIC      = SPACES)
               MOVE TFSSTPL-BNKENTTY TO WK-N-VBAC-BNKENTTY
               MOVE TFSSTPL-CUYCD    TO WK-C-VBAC-CUYCD
               MOVE TAG57-BIC        TO WK-C-VBAC-BANKID
               CALL "TRFVBAC" USING WK-C-VBAC-RECORD
               IF WK-C-VBAC-ERROR-CD = SPACES
               AND TAG57-PTID        = SPACES
               OR WK-C-VBAC-ERROR-CD = SPACES
               AND TAG57-PTID        NOT = SPACES
               AND WK-C-VBAC-BNKACNO = TAG57-PTID
                   MOVE "Y"          TO BKAC57-IND
               ELSE
                   MOVE "N"          TO BKAC57-IND

 5Q2JE1*5Q1JM1      INITIALIZE WK-C-RPRRSN-AREA
 5Q2JE1*5Q1JM1      MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
 5Q2JE1*5Q1JM1      PERFORM D500-PROCESS-RPRRSN
 5Q2JE1*5Q1JM1           THRU D599-PROCESS-RPRRSN-EX

                    END-IF
                END-IF.

                IF NOT(TAG58-OPT    = SPACES
                AND TAG58-PTID     = SPACES
                AND TAG58-BIC      = SPACES)
                    INITIALIZE WK-C-TFSBNKAC
                    MOVE TFSSTPL-BNKENTITY TO TFSBNKAC-BNKENTITY
                    MOVE TAG58-BIC TO TFSBNKAC-BANKID
                    MOVE 1 TO TFSBNKAC-PRIORITY
                    START TFSBNKAC KEY IS >= EXTERNALLY-DESCRIBED-KEY
                    READ TFSBNKAC NEXT WITH NO LOCK
                    IF TAG58-BIC = TFSBNKAC-BANKID
                        MOVE "Y" TO BKAC58-IND
                    ELSE
                        MOVE "N" TO BKAC58-IND
                    END-IF
                END-IF.

            D499-BKAC-VALIDATION-EX.
                EXIT.
                EJECT

      *=================================================================*
GP3A00  D600-EVAL-TAG57-CD.
      *=================================================================*
GP3A00*----------------------------------------------------------------*
GP3A00*--This routine will check Tag57 C/D Lines 1-5 if it exact matches
GP3A00*--Tag Validation table. If Match, treat it as Tag57A w/ Our Own BIC
GP3A00*--(UOVBSGSGXXX - parameterized) to further proceed with STP processing.
GP3A00*--E.g Raw Tag57D Line1:/123456789
GP3A00*--                Line2:UNITED OVERSEAS BANK
GP3A00*--                Line3:SINGAPORE
GP3A00*--                Line4:BUKIT BATOK
GP3A00*--                Line5:SG
GP3A00*--If Line 1 "UNITED OVERSEAS BANK" exact matches Tag validation table
GP3A00*--system will treat this as Tag57A Line1: *blank
GP3A00*--                Line2: UOVBSGSGXXX
GP3A00*--                Line3: *blank
GP3A00*--                Line4: *blank
GP3A00*--                Line5: *blank
GP3A00*--and proceed with BAU STP processing.
GP3A00
GP3A00           INITIALIZE WK-C-VTAG57-RECORD.
GP3A00           MOVE TAG57-OPT TO WK-C-VTAG57-OPTION.
GP3A00
GP3A00*--For MT202, validate on Tag57D ONLY.
GP3A00           IF TAG57-OPT = "C"
GP3A00           AND TFSSTPL-SWFTMGTY = "202"
GP3A00               GO TO D699-EVAL-TAG57-CD-EX
GP3A00           END-IF.
GP3A00

GP3A00
GP3A00*----------------------------------------------------------------*
GP3A00*--Tag57C:
GP3A00       IF   TAG57-OPT = "C"
GP3A00           IF   TAG57-PTID = SPACES
GP3A00               GO TO D699-EVAL-TAG57-CD-EX
GP3A00           ELSE
GP3A00               MOVE TAG57-PTID     TO   WK-C-VTAG57-INFO(1)
GP3A00           END-IF
GP3A00       END-IF.
GP3A00
GP3A00*----------------------------------------------------------------*
GP3A00*--Tag57D:
GP3A00       IF   TAG57-OPT = "D"
GP3A00           IF   TAG57-PTID = SPACES
GP3A00           AND TAG57-NAME = SPACES
GP3A00               GO TO D699-EVAL-TAG57-CD-EX
GP3A00           ELSE
GP3A01*GP3A00       MOVE TAG57-PTID     TO   WK-C-VTAG57-INFO(1)
GP3A00               MOVE TAG57-NAME     TO   WK-C-VTAG57-INFO(2)
GP3A01*GP3A00       MOVE TAG57-LINE-3   TO   WK-C-VTAG57-INFO(3)
GP3A01*GP3A00       MOVE TAG57-LINE-4   TO   WK-C-VTAG57-INFO(4)
GP3A01*GP3A00       MOVE TAG57-LINE-5   TO   WK-C-VTAG57-INFO(5)
GP3A00           END-IF
GP3A00       END-IF.
GP3A00
GP3A00       MOVE TFSSTPL-BNKNENTTY     TO   WK-C-VTAG57-I-BNKNENTTY.
GP3A00
GP3A00*----------------------------------------------------------------*
GP3A00*--Check Tag57 if either Lines 1-5 matches Tag validation table.
GP3A00       CALL "TRFVTAG57" USING   WK-C-VTAG57-RECORD.
GP3A00       CANCEL "TRFVTAG57".
GP3A00
GP3A00       IF   WK-C-VTAG57-ERROR-CD = SPACES
GP3A00           CONTINUE
GP3A00       ELSE
GP3A00           GO TO D699-EVAL-TAG57-CD-EX
GP3A00       END-IF.
GP3A00
GP3A00*----------------------------------------------------------------*
GP3A00*--If it match, override w/ Tag57A:<Own BIC> (parameterized)
GP3A00       IF   WK-C-VTAG57-VALID = "Y"
GP3A00           MOVE SPACES          TO   TAG57-PTID
GP3A00                                   WS-ACBNKACC
GP3A00                                   WS-ACCNO
GP3A00           MOVE "A"             TO   TAG57-OPT
GP3A00           MOVE WK-C-VTAG57-BIC TO   TAG57-BIC
GP3A00                                   WS-ACBNKID
GP3A00                                   WS-BANKID
GP3A00       END-IF.
GP3A00*----------------------------------------------------------------*
GP3A00       GO TO D699-EVAL-TAG57-CD-EX.
GP3A00*----------------------------------------------------------------*
GP3A00       EXIT.
5Q1JM1 D500-PROCESS-RPRRSN SECTION.
5Q1JM1 D500-ENTRY.
5Q1JM1

      MOVE WK-VTE2-PARALNO     TO WK-C-RRSN-QUENUM.
      MOVE WK-VTE2-SEQMUN      TO WK-C-RRSN-QUESUF.
      MOVE WK-C-TRNNO          TO WK-C-RRSN-TRNNO.
      MOVE WK-C-FUNCTID        TO WK-C-RRSN-FUNCTID.
      MOVE WK-C-SEGCDE         TO WK-C-RRSN-SEGCDE.
      MOVE SPACES              TO WK-C-RRSN-SEGDESC.
      MOVE WK-N-STAFFIND       TO WK-C-RRSN-STAFFIND.
      MOVE WK-C-ACCNO          TO WK-C-RRSN-ACCNO.
      MOVE WK-C-QRATE          TO WK-C-RRSN-QRATE.
      MOVE WK-N-SYSDTE         TO WK-C-RRSN-RPRDTE.
      *     MOVE WK-C-RPRCODE        TO WK-C-RRSN-RSNCDE.
      IF WK-C-RPRCODE = SPACE
      MOVE "RSN9999"           TO WK-C-RRSN-RSNCDE
      ELSE
      MOVE WK-C-RPRCODE        TO WK-C-RRSN-RSNCDE
      END-IF.
      
      MOVE SPACES              TO WK-C-RRSN-RSNDESC.
      MOVE WK-C-RPRPGM         TO WK-C-RRSN-RPRPGM.
      CALL "TRFGRRSN" USING WK-C-RRSN-RECORD.
      
      D599-PROCESS-RPRRSN-EX.
      EXIT.
      EJECT
      
Z000-END-PROGRAM.
      CLOSE TFSSPL
            TFSBNKAC
ID1VKE      TFSBNKET
            TFSCLSYS.
      EXIT PROGRAM.