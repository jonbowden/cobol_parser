       IDENTIFICATION DIVISION.
      ***********************
       PROGRAM-ID. TRFVTE3.
       AUTHOR. TYK.
       DATE-WRITTEN. JUN 04.
      *DESCRIPTION : TABLE E3 VALIDATION.
      *              SUBROUTINE - CREDIT PARTY CHECKING FIELD 56/57 FOR
      *              INCOMING SWIFT MT200 FCY (RTGS)
      *
      *=================================================================
      * GP4D02 - VENTEH  - 16/10/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              GPI Day4 (POST IMPEM IMPROVEMENT)
      *              (For HK only)
      *              - JIRA PCRMAPKGPI-2109
      *              - STP #5 Inward (Inward receipt,
      *                IAFT & In-as-out) TT/RTGS- STP
      *                by CCY.
      *              - Bypass the STP currency setup
      *                checking to avoid double
      *                RSNCDE
      *-----------------------------------------------------------------
      * GP4D01 - VENTEH  - 22/07/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              GPI Day4 (In-Country Req)
      *              - STP #5 Inward (Inward receipt,
      *                IAFT & In-as-out) TT/RTGS- STP
      *                by CCY.
      *              - JIRA PCRMAPKGPI-1881
      *              - To cater negative scenario when
      *                ACC/CIF/SEG STP limit switch is OFF
      *                proceed to check STP by CCY
      *-----------------------------------------------------------------
      * GP4D00 - VENADG  - 14/02/2020 - CASH MANAGEMENT ROAD MAP - P19
      *              GPI Day4 (In-Country Req)
      *              - STP #5 Inward (Inward receipt,
      *                IAFT & In-as-out) TT/RTGS- STP
      *                by CCY.
      *              - To enable ITT STP CCY check
      *-----------------------------------------------------------------
      * REM269 - TMPSRK  - 07/04/2017 - JIRA LOG REM-269
      *              - STANDARDIZATION OF PROGRAM TO
      *                RETRIEVE CURRENCY AND COUNTRY
      *                CODE FROM SYSTEM PARAMETER FILE.
      *-----------------------------------------------------------------
      * CMP3F1 - ACNFEN  - 15/02/2017 - CASH MANAGEMENT PROJECT 3
      *              STP LIMIT BY ACC/CIF/SEGMENT
      *-----------------------------------------------------------------*
      * 7Q1EM1 - TMPFYM  - 25/11/2016 - REM Q1 2017 RELEASE
      *              - e-Req 47511 Refinement of
      *                Duplicate checking for Inw
      *              - Recompiled due to changes made in
      *                VSTPL copy book.
      *-----------------------------------------------------------------*
      * 5Q1JM1 - TMPJZM  - 23/12/2014 - 14HOREM024/14HOREM029/14HOREM028
      *              Retrofit NON PSTP Reason

      *     Enhancement Project
      *--------------------------------------------------------------
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
           SELECT TFSSPTPL ASSIGN TO DATABASE-TFSSPTPL
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
               FILE STATUS IS WK-C-FILE-STATUS.

           SELECT TFSCLSYS ASSIGN TO DATABASE-TFSCLSYS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WK-C-FILE-STATUS.

      ID1VKE SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
      ID1VKE     ORGANIZATION IS INDEXED
      ID1VKE     ACCESS MODE IS RANDOM
      ID1VKE     RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
      ID1VKE     FILE STATUS IS WK-C-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
      *************
       FD  TFSSPTPL
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS WK-C-TFSSPTPL.
       01  WK-C-TFSSPTPL.
           COPY DDS-ALL-FORMATS OF TFSSPTPL.
       01  WK-C-TFSSPTPL-1.
           COPY TFSSPTPL.

       FD  TFSCLSYS
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS TFSCLSYS-REC.

      01  TFSCLSYS-REC.
          COPY DDS-ALL-FORMATS OF TFSCLSYS.
      01  TFSCLSYS-REC-1.
          COPY TFSCLSYS.

      ID1VKE FD  TFSBNKET
      ID1VKE     LABEL RECORDS ARE OMITTED
      ID1VKE     DATA RECORD IS TFSBNKET-REC.
      ID1VKE 01  TFSBNKET-REC.
      ID1VKE     COPY DDS-ALL-FORMATS OF TFSBNKET.
      ID1VKE 01  TFSBNKET-REC-1.
      ID1VKE     COPY TFSBNKET.

          WORKING-STORAGE SECTION.
      ************************
      01  WK-C-COMMON.
          COPY ASCWMS.

      01  TAG56-FORMAT.
      05  TAG56-LINE-1.
          07  TAG56-FIL1       PIC X(2).
          07  TAG56-OPT        PIC X(1).
          07  TAG56-FIL2       PIC X(1).
          07  TAG56-PTID.
              09  TAG56-PTID-1 PIC X(02).
              09  TAG56-PTID-2 PIC X(35).
      05  TAG56-LINE-2         PIC X(35).
      05  TAG56-BIC  REDEFINES TAG56-LINE-2.
          07  TAG56A-SUB1      PIC X(4).
          07  TAG56A-SUB2      PIC X(2).
          07  TAG56A-SUB3      PIC X(2).
          07  TAG56A-SUB4      PIC X(3).
          07  TAG56A-FILLER    PIC X(24).
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
          07  TAG57A-SUB1      PIC X(4).
          07  TAG57A-SUB2      PIC X(2).
          07  TAG57A-SUB3      PIC X(2).

       07  TAG57A-SUB4                  PIC X(3).
       07  TAG57A-FILLER                PIC X(24).
       05  TAG57-LOC REDEFINES TAG57-LINE-2
                                       PIC X(35).
       05  TAG57-NAME REDEFINES TAG57-LINE-2
                                       PIC X(35).
       05  TAG57-LINE-3                 PIC X(35).
       05  TAG57-LINE-4                 PIC X(35).
       05  TAG57-LINE-5                 PIC X(35).

       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".

       01  TABLE-ARR2.
           05  TAB-VL2 OCCURS 20 TIMES PIC X VALUE "X".

       01  PATH-P1                      PIC X(20).
       01  PATH-P2                      VALUE "XXXXXXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P3                      VALUE "XYYXXXXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P4                      VALUE "XXXYYYXXXXXXXXXXXXXXX".
                                       PIC X(20)
                                       VALUE "YXXXYTYXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P5                      VALUE "XYXXXYTYCXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P6                      VALUE "XYXXXYTYCXXXXXXXXXXXXX".
                                       PIC X(20)
                                       VALUE "XXXYNYTYCXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P7                      VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P8                      VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P9                      VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P10                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P11                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P12                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P13                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P14                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P15                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P16                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P17                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P18                     VALUE "XXXXXYCXXXXXXXXXXXXXXX".
                                       PIC X(20)
       01  PATH-P19                     VALUE "XXXXXXXXXXXXXXXXXXXXXX".
                                       PIC X(20)

       01 PATH-P20                   PIC X(20)
                                      VALUE "XXXXXXXXXXXXXXXXXXXX".
       01 PATH-P21                   PIC X(20)
                                      VALUE "XXXXXXXXXXXXXXXXXXXX".
       01 PATH-P22                   PIC X(20)
                                      VALUE "XXXXXXXXXXXXXXXXXXXX".

       01 WK-C-PARADATA.
          05 WK-C-PARAVALU           PIC X(20).
          05 WK-N-PARAVALU           REDEFINES WK-C-PARAVALU
                                     PIC 9(13)V99.
          05 WK-N-IRMIPSTP           PIC 9(13)V99.
          05 WK-N-IRM1STP            PIC 9(13)V99.

       01 WK-C-WORK-AREA.
          05 FIRST-TIME              PIC X(01) VALUE "Y".
          05 WS-FLAG1                PIC X(01) VALUE SPACE.
          05 WS-FLAG2                PIC X(01) VALUE SPACE.
          05 WS-ACT1                 PIC X(01) VALUE SPACE.
          05 BKAC56-IND              PIC X(01) VALUE SPACE.
          05 BKAC57-IND              PIC X(01) VALUE SPACE.
          05 WS-OKAY                 PIC X(01) VALUE SPACE.
          05 WS-STPTYP               PIC X(04) VALUE SPACE.
          05 WS-ACCNO                PIC X(11) VALUE SPACE.
          05 WS-INTEMBNKACC          PIC X(11) VALUE SPACE.
          05 WS-ABNKNACC             PIC X(11) VALUE SPACE.
          05 WS-BANKID               PIC X(11) VALUE SPACE.
          05 WS-INTEMBNKID           PIC X(11) VALUE SPACE.
          05 WS-ACBNKID              PIC X(11) VALUE SPACE.
          05 WS-LCAMT                PIC 9(13)V99 VALUE ZEROS.

      ID1VKE 01 WK-C-SWIFTBICDCE     PIC X(11) VALUE SPACE.
      ID1VKE 01 WK-C-TRGSBICDCE      PIC X(11) VALUE SPACE.
      5Q1JM1 01 WK-C-RPRRSN-AREA.
      5Q1JM1    05 WK-C-SEGDE        PIC X(01) VALUE SPACE.
      5Q1JM1    05 WK-N-STAFFIND     PIC S9(02) VALUE ZEROS.
      5Q1JM1    05 WK-C-ACCNO        PIC X(15) VALUE SPACE.
      5Q1JM1    05 WK-C-QRATE        PIC X(02) VALUE SPACE.
      5Q1JM1    05 WK-C-RPRCODE      PIC X(07) VALUE SPACE.
      5Q1JM1    05 WK-C-TRNNO        PIC X(12) VALUE SPACE.
      5Q1JM1    05 WK-C-FUNCTID      PIC X(08) VALUE SPACE.
      5Q1JM1    01 WK-N-SYSPTE       PIC S9(08) VALUE ZEROS.
      5Q1JM1    01 WK-C-RPRPGM       PIC X(10) VALUE "TRFVTE3".
      CMP3F1 01 WS-C-STPLMT-FLAG     PIC X(01) VALUE "N".
      RME269 01 WK-C-LCUYCD          PIC X(03).
      CMP3F1 01 WK-C-LINK-LIMIT.
      CMP3F1    05 WK-C-LINK-AREA-INPUT.
      CMP3F1       10 WS-LINK-BNKENTTY PIC X(02).
      CMP3F1       10 WS-LINK-ACCNO   PIC X(15) VALUE 0.
      CMP3F1       10 WS-LINK-CCY     PIC X(03) VALUE SPACES.
      CMP3F1       10 WS-LINK-AMT     PIC S9(13)V99 VALUE 0.
      CMP3F1       10 WS-LINK-REMIND  PIC X(01).
      CMP3F1    05 WK-C-LINK-AREA-OUTPUT.
      CMP3F1       10 WS-LINK-STATUS  PIC X(02) VALUE SPACES.

      GP4D00 01  WK-C-STP-CCY-SW           PIC X(01) VALUE SPACE.
      GP4D02 01  WK-C-STP-CCY-IMP-SW       PIC X(01) VALUE SPACE.
      GP4D00 01  WK-C-LIT-GPI.
      GP4D00    05 WK-C-STPCCY-PARCD       PIC X(10)
      GP4D00                               VALUE "GPI4ISTPCY".
      GP4D02    05 WK-C-STPCCY2-PARCD      PIC X(10)
      GP4D02                               VALUE "GPI4STPCY2".

               COPY VSTPL.
               COPY VBAC.
               COPY VBANO.
               COPY XPARA.
               COPY GERTE.
               COPY LOGG.
               ID1VKE COPY XGSPA.
               5Q1JM1 COPY RRSN.
      GP4D00   COPY VSTPC.

               LINKAGE SECTION.
      *****************
               COPY VTE3.

               PROCEDURE DIVISION USING WK-VTE3.
      ********************************
               MAIN-MODULE.

CMP3F1*        GET STP LIMIT INDICATOR
CMP3F1         INITIALIZE WK-C-XGSPA-RECORD.
CMP3F1         MOVE "RSYSSTPLMT"      TO WK-C-XGSPA-GHPARCD.
CMP3F1         CALL "TRFXGSPA"        USING WK-C-XGSPA-RECORD.
CMP3F1         IF WK-C-XGSPA-ERROR-CD = SPACES
CMP3F1            MOVE WK-C-XGSPA-GHPARVAL TO WS-C-STPLMT-FLAG
CMP3F1         ELSE
CMP3F1            MOVE SPACES         TO WS-C-STPLMT-FLAG
CMP3F1         END-IF.

REM269*----------------------------------------------------------------*
      |        *    GET SYSTEM PARAMETERS FOR LOCAL CURRENCY CODE.    *
      |        *------------------------------------------------------*
      |           INITIALIZE WK-C-XGSPA-RECORD.
      |           MOVE "RSYTCLCUCY"      TO WK-C-XGSPA-GHPARCD.
      |           CALL "TRFXGSPA"        USING WK-C-XGSPA-RECORD.
      |           IF WK-C-XGSPA-ERROR-CD = SPACES
      |              MOVE WK-C-XGSPA-GHPARVAL TO WK-C-LCUYCD
      |           ELSE
      |              MOVE SPACES         TO WK-C-LCUYCD
      |           END-IF

               INITIALIZE WK-VTE3-OUTPUT
                          WK-LOGG
                          WK-C-WORK-AREA.
               MOVE ALL "X" TO TABLE-ARRAY.

       MOVE ALL "X"     TO TABLE-ARR2.
       MOVE "Y"         TO FIRST-TIME.

5Q1JM1 MOVE ZEROS      TO WK-C-RRSN-QUENUM
5Q1JM1                  WK-C-RRSN-QUESUF
5Q1JM1                  WK-C-RRSN-STAFFIND
5Q1JM1                  WK-C-RRSN-SEQNUM
5Q1JM1                  WK-C-RRSN-RPRDTIE.

       IF FIRST-TIME = "Y"
           OPEN INPUT TFSSTPL
           IF NOT WK-C-SUCCESSFUL
              AND WK-C-FILE-STATUS NOT = "41"
               DISPLAY "TRFTABLB2 - OPEN FILE ERROR - TFSSTPL"
               DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
           END-IF
           OPEN INPUT TFSCLSYS
           IF NOT WK-C-SUCCESSFUL
              AND WK-C-FILE-STATUS NOT = "41"
               DISPLAY "TFSCLSYS - OPEN FILE ERROR - TFSCLSYS"
               DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
           END-IF
ID1VKE     OPEN INPUT TFSBNKET
|          IF NOT WK-C-SUCCESSFUL
|             AND WK-C-FILE-STATUS NOT = "41"
|              DISPLAY "TRFVE1 - OPEN FILE ERROR - TFSBNKET"
|              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
|          END-IF
|
|          INITIALIZE    WK-C-XGSPA-RECORD
|          MOVE "SPTRGSBIC" TO WK-C-XGSPA-GHPARCD
|          CALL "TRFXGSPA"  USING WK-C-XGSPA-RECORD
|          IF WK-C-XGSPA-ERROR-CD = SPACES THEN
|              MOVE WK-C-XGSPA-GHPARVAL TO WK-C-RTGSBICCDE
|          ELSE
|              MOVE SPACES TO WK-C-RTGSBICCDE
|          END-IF
ID1VKE  END-IF.

       MOVE WK-VTE3-PARALNO TO TFSSTPL-PARALNO.
       MOVE WK-VTE3-SEQNUM  TO TFSSTPL-SEQNUM.

       READ TFSSTPL
           KEY IS EXTERNALLY-DESCRIBED-KEY.

       IF WK-C-SUCCESSFUL
           MOVE "N" TO WS-OKAY
           MOVE TFSSTPL-TAG56 TO TAG56-FORMAT
           MOVE TFSSTPL-TAG57 TO TAG57-FORMAT
ID1VKE     MOVE TFSSTPL-BNKENTITY TO TFSBNKET-BNKENTITY
ID1VKE     READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
ID1VKE     INVALID KEY
ID1VKE     MOVE SPACES TO WK-C-SWIFTBICCDE

      ID1VKE       NOT INVALID KEY
      ID1VKE       MOVE TFSBNKET-SWFTBNK  TO WK-C-SWIFTBICCDE
      ID1VKE       END-READ
                   PERFORM A100-INITIAL-SUBROUTINE
                          THRU A199-INITIAL-SUBROUTINE-EX
                   PERFORM A200-MOVE-TAG-VALUES
                          THRU A299-MOVE-TAG-VALUES-EX
                   PERFORM B100-PATH-CHOICE  THRU B199-PATH-CHOICE-EX
                   END-IF.
                   GO TO  Z000-END-PROGRAM.

      A100-INITIAL-SUBROUTINE.
      *-----------------------------------------------------------------*
      *    GET DATA FROM "TFSCLSYS" TABLE                               *
      *-----------------------------------------------------------------*
                   READ TFSCLSYS.
                   IF NOT WK-C-SUCCESSFUL
                      DISPLAY "TRFVET3  - READ TFSCLSYS ERROR"
                      DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
                      GO TO  Z000-END-PROGRAM.

      **********MOVE TFSCLSYS-SYSDTE  TO  L-N-G-SYSDTE.
      50J1JM1    MOVE TFSCLSYS-SYSDTE  TO  WK-N-SYSDTE.
      **********MOVE TFSCLSYS-LCNTRYCD TO  L-C-G-L-CNTRYCD.
      **********MOVE TFSCLSYS-LCUYCD   TO  L-C-G-L-CUYCD.

      *-----------------------------------------------------------------*
      *    GET SYSTEM PARAMETERS FOR PSTP & 1STP                        *
      *-----------------------------------------------------------------*
                   MOVE "IRSMSG"       TO  WK-C-XPARA-PARACD.
                   CALL "TRFXPARA"     USING WK-C-XPARA-RECORD.
                   IF WK-C-XPARA-ERROR-CD NOT = SPACES
                      DISPLAY "TREEEDT  - TRFXPARA ROUTINE ERROR"
                      DISPLAY "FILE STATUS - " WK-C-XPARA-FS
                      DISPLAY "ERROR ID    - " WK-C-XPARA-ERROR-CD
                      DISPLAY "KEY         - " WK-C-XPARA-INPUT
                      GO TO  Z000-END-PROGRAM
                   ELSE
                      MOVE WK-C-XPARA-PARAVALU
                           TO  WK-C-PARAVALU
                      MOVE WK-N-PARAVALU
                           TO  WK-N-IRMPSPTP
                   END-IF.
                   MOVE "IRSMSG1"      TO  WK-C-XPARA-PARACD.
                   CALL "TRFXPARA"     USING WK-C-XPARA-RECORD.
                   IF WK-C-XPARA-ERROR-CD NOT = SPACES
                      DISPLAY "TREEEDT  - TRFXPARA ROUTINE ERROR"
                      DISPLAY "FILE STATUS - " WK-C-XPARA-FS
                      DISPLAY "ERROR ID    - " WK-C-XPARA-ERROR-CD
                      DISPLAY "KEY         - " WK-C-XPARA-INPUT
                      GO TO  Z000-END-PROGRAM
                   ELSE
                      MOVE WK-C-XPARA-PARAVALU
                           TO  WK-C-PARAVALU

       MOVE WK-N-PARAVALU
           TO  WK-N-IRM1STP
       END-IF.

GP4D00*-->Retrieve GPI Day4 In-Country ITT STP by Currency Switch
GP4D00    INITIALIZE            WK-C-XGSPA-RECORD
GP4D00                            WK-C-STP-CCY-SW
GP4D00    MOVE WK-C-STPCCY-PARCD TO WK-C-XGSPA-GHPARCD.
GP4D00    CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
GP4D00    IF  WK-C-XGSPA-ERROR-CD = SPACES
GP4D00        MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP4D00                            TO  WK-C-STP-CCY-SW
GP4D00    END-IF.

GP4D02*-->Retrieve GPI Day4 ITT STP by Currency Improvement Switch
GP4D02    INITIALIZE            WK-C-XGSPA-RECORD
GP4D02                            WK-C-STP-CCY-IMP-SW.
GP4D02    MOVE WK-C-STPCCY2-PARCD TO WK-C-XGSPA-GHPARCD.
GP4D02    CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
GP4D02    IF  WK-C-XGSPA-ERROR-CD = SPACES
GP4D02        MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP4D02                            TO  WK-C-STP-CCY-IMP-SW
GP4D02    ELSE
GP4D02        MOVE "N"           TO  WK-C-STP-CCY-IMP-SW
GP4D02    END-IF.

       A199-INITAL-SUBROUTINE-EX.
           EXIT.

       A200-MOVE-TAG-VALUES.
           IF  TAG56-BIC NOT = SPACES
           AND TAG56-OPT = "A"
               MOVE TAG56-BIC     TO  WS-INTEMBNKID
                                     WS-BANKID
               MOVE TAG56-PTID    TO  WS-INTEMBNKACC
                                     WS-ACCNO
           END-IF.
           IF  TAG57-BIC NOT = SPACES
           AND TAG57-OPT = "A"
               MOVE TAG57-BIC     TO  WS-ACBNKID
                                     WS-BANKID
               MOVE TAG57-PTID    TO  WS-ACBNKACC
                                     WS-ACCNO
           END-IF.

           IF  TAG57-NAME NOT = SPACES
           AND TAG57-OPT = "D"
               MOVE TAG57-PTID    TO  WS-ACBNKACC
                                     WS-ACCNO
               MOVE TAG57-NAME    TO  WK-VTE3-ACBNKNM
               MOVE TAG57-LINE-3  TO  WK-VTE3-ACBNKADR1

            MOVE TAG57-LINE-4      TO  WK-VTE3-ACBNKADR2
            MOVE TAG57-LINE-5      TO  WK-VTE3-ACBNKADR3
            END-IF.

            MOVE TFSSTPL-BNKENTTY  TO WK-N-VBAC-BNKENTTY.
            MOVE WS-BANKID         TO WK-C-VBAC-BANKID.
            MOVE TFSSTPL-CUYCD     TO WK-C-VBAC-CUYCD.
            CALL "TFRVBAC" USING WK-C-VBAC-RECORD.
            IF   WK-C-VBAC-ACUDBUI NOT = SPACES
                 MOVE WK-C-VBAC-ACUDBUI TO WK-VTE3-ACUDBUI
                 IF  WS-ACCNO = SPACES
                     MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
                 END-IF
            ELSE
                 MOVE "D"           TO WK-VTE3-ACUDBUI
            END-IF.

       A299-MOVE-TAG-VALUES-EX.
            EXIT.
            EJECT

       B100-PATH-CHOICE.
            PERFORM D400-BKAC-VALIDATION.
            IF TAG56-OPT = SPACES
            AND TAG56-PTID = SPACES
            AND TAG56-BIC = SPACES
            OR  TAG56-OPT = "A"
       1D1VKE* AND TAG56-BIC = "UOVBSGGXXX"
       1D1VKE  AND TAG56-BIC = WK-C-SWIFTBICCODE
                 PERFORM C100-VALIDATION-PART
                         THRU C199-VALIDATION-PART-EX

            END-IF.
            IF TAG56-OPT = "A"
            AND TAG56-BIC NOT = SPACES
            AND BKAC56-IND = "Y"
       1D1VKE* AND TAG56-BIC NOT = "UOVBSGGXXX"
       1D1VKE  AND TAG56-BIC NOT = WK-C-SWIFTBICCODE
       1D1VKE* AND TAG56-BIC NOT = "MASGSGGXXX"
       1D1VKE  AND TAG56-BIC NOT = WK-C-RTGSBICCODE
                 PERFORM C200-VALIDATION-PART
                         THRU C299-VALIDATION-PART-EX

            END-IF.
            PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.
            PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.
            EXIT.

       C100-VALIDATION-PART.
            IF TAG57-OPT = "A"
       1D1VKE* AND TAG57-BIC = "UOVBSGGXXX"
       1D1VKE  AND TAG57-BIC = WK-C-SWIFTBICCODE
            AND TAG57-PTID NOT = SPACES
                 MOVE PATH-P2 TO TABLE-ARRAY

            MOVE "Y"         TO WS-OKAY
            END-IF.
            IF BKAC57-IND    = "Y"
               AND TAG57-OPT = "A"
      ID1VKE* AND TAG57-BIC  NOT = "UOVBSGSGXXX"
      ID1VKE  AND TAG57-BIC  NOT = WK-C-SWIFTBICCDE
      ID1VKE* AND TAG57-BIC  NOT = "MASGSGSGXXX"
      ID1VKE  AND TAG57-BIC  NOT = WK-C-RTGSBICCDE
               MOVE PATH-P3  TO TABLE-ARRAY
               MOVE "Y"      TO WS-ACT1
                              WS-OKAY
            END-IF.
            C199-VALIDATION-PART-EX.
               EXIT.
            EJECT

       C200-VALIDATION-PART.
            IF TAG57-OPT     = SPACES
               AND TAG57-PTID = SPACES
               AND TAG57-BIC = SPACES
               MOVE PATH-P4  TO TABLE-ARRAY
               MOVE "Y"      TO WS-ACT1
                              WS-OKAY
            END-IF.
            IF TAG57-OPT     NOT = "A"
               AND NOT(TAG57-OPT = SPACES
               AND TAG57-PTID = SPACES
               AND TAG57-BIC = SPACES)
      ID1VKE* AND TAG57-BIC  NOT = "UOVBSGSGXXX"
      ID1VKE  AND TAG57-BIC  NOT = WK-C-SWIFTBICCDE
      ID1VKE* AND TAG57-BIC  NOT = "MASGSGSGXXX"
      ID1VKE  AND TAG57-BIC  NOT = WK-C-RTGSBICCDE
               MOVE PATH-P5  TO TABLE-ARRAY
               MOVE "Y"      TO WS-ACT1
                              WS-OKAY
            END-IF.
            IF TAG57-OPT     = "A"
      ID1VKE* AND TAG57-BIC  NOT = "UOVBSGSGXXX"
      ID1VKE  AND TAG57-BIC  NOT = WK-C-SWIFTBICCDE
      ID1VKE* AND TAG57-BIC  NOT = "MASGSGSGXXX"
      ID1VKE  AND TAG57-BIC  NOT = WK-C-RTGSBICCDE
               IF BKAC57-IND = "Y"
                  MOVE PATH-P6 TO TABLE-ARRAY
                  MOVE "Y"    TO WS-ACT1
                              WS-OKAY
               ELSE
                  MOVE PATH-P7 TO TABLE-ARRAY
                  MOVE "Y"    TO WS-ACT1
                              WS-OKAY
               END-IF
            END-IF.
            C299-VALIDATION-PART-EX.
               EXIT.
            EJECT

      D100-VALIDATION.
           MOVE "Y"        TO WS-FLAG1.
           IF TABLE-ARRAY = ALL "X"
               MOVE "N"    TO WS-OKAY
               INITIALIZE WK-C-RPRRSN-AREA
               MOVE "RSN0118" TO WK-C-RPRCODE
               PERFORM D500-PROCESS-RPRRSN
                   THRU D599-PROCESS-RPRRSN-EX
           END-IF.
      CMP3F1 IF WS-C-STPLMT-FLAG = "Y"
           MOVE "A1"       TO WS-LINK-STATUS
           IF WS-ACCNO NOT = SPACES
               INITIALIZE WK-C-RPRRSN-AREA
               INITIALIZE WK-C-LINK-LIMIT
               MOVE TFSSTPL-BNKENTTY TO WS-LINK-BNKENTTY
               MOVE WS-ACCNO TO WS-LINK-ACCNO
               MOVE TFSSTPL-CUYCD TO WS-LINK-CCY
               MOVE TFSSTPL-AMT TO WS-LINK-AMT
               MOVE "I" TO WS-LINK-REMIND
               CALL "TRFVLMT" USING WK-C-LINK-LIMIT
               EVALUATE WS-LINK-STATUS
                   WHEN "XX"
                       MOVE "N" TO WS-OKAY
                       MOVE "RSN0311" TO WK-C-RPRCODE
                       PERFORM D500-PROCESS-RPRRSN
                           THRU D599-PROCESS-RPRRSN-EX
               END-EVALUATE
           END-IF
      CMP3F1 END-IF.

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
      REM269**** IF TFSSTPL-CUYCD NOT = TFSCLSYS-LCUYCD
      REM269     IF TFSSTPL-CUYCD NOT = WK-C-LCUYCD
                 Move "BT" TO WK-C-GERTE-RTE-TYP

            MOVE TFSSTPL-CUYCD        TO WK-C-GERTE-CUYCD
            MOVE TFSSTPL-BNKENTTY     TO WK-N-GERTE-BNKENTTY
            CALL "TRFGERTE"           USING WK-C-GERTE-RECORD
            COMPUTE WS-LCAMT ROUNDED  = TFSSTPL-AMT
      * WK-N-GERTE-EXCH-RTE
                                     / WK-N-GERTE-FXRATEUT
            ELSE
            MOVE TFSSTPL-AMT          TO WS-LCAMT
            END-IF.
            IF TAB-VAL(06) NOT = "X" AND WS-OKAY = "Y"
            MOVE "Y"                  TO TAB-VL2(06)
CMP3F1*     IF WS-LCAMT <= WK-N-IRMSTP
CMP3F1      IF (WS-LCAMT <= WK-N-IRMSTP
CMP3F1      AND WS-C-STPLMT-FLAG NOT = "Y")
CMP3F1      OR (WS-LCAMT <= WK-N-IRMSTP
CMP3F1      AND WS-C-STPLMT-FLAG = "Y"
CMP3F1      AND WS-LINK-STATUS = "A1")
CMP3F1      OR (WS-LINK-STATUS = "A0"
CMP3F1      AND WS-C-STPLMT-FLAG = "Y")
            MOVE "PSTP"               TO WS-STPTYP
            MOVE "N"                  TO TAB-VL2(06)
            MOVE "X"                  TO TAB-VL2(07)
            END-IF
GP4D00*-->If within Parm STP Limit and w/out Acc/CIF/Seg STP Limit setup
GP4D00*-->further check ITT STP CCY table.
GP4D00      IF WK-C-STP-CCY-SW = "Y"
GP4D00      AND WS-LCAMT <= WK-N-IRMSTP
GP4D00*GP4D01 AND WS-C-STPLMT-FLAG = "Y"
GP4D00*GP4D01 AND WS-LINK-STATUS = "A1"
GP4D01      AND ((WS-C-STPLMT-FLAG = "Y"
GP4D01      AND WS-LINK-STATUS = "A1")
GP4D01      OR WS-C-STPLMT-FLAG NOT = "Y")
GP4D00      PERFORM D120-EVAL-STP-CCY
GP4D00      THRU D120-EVAL-STP-CCY-EX
GP4D00      END-IF
            PERFORM D300-LOGGING THRU D399-LOGGING-EX
CMP3F1      IF WS-LINK-STATUS = "A0"
CMP3F1      AND WS-C-STPLMT-FLAG = "Y"
CMP3F1      GO TO D199-VALIDATION-EX
CMP3F1      END-IF
            END-IF.
            IF TAB-VAL(07) NOT = "X" AND WS-OKAY = "Y"
            IF WS-LCAMT > WK-N-IRM1STP
            MOVE "2STP"               TO WS-STPTYP
            MOVE "Y"                  TO TAB-VL2(07)
5QJ3M1      INITIALIZE WK-C-RPRRSN-AREA
5QJ3M1      MOVE "RSN0023"           TO WK-C-RPRPCODE
5QJ3M1      PERFORM D500-PROCESS-RPRRSN
5QJ3M1      THRU D599-PROCESS-RPRRSN-EX
            ELSE
            IF WS-LCAMT > WK-N-IRMSTP
CMP3F1      OR (WS-C-STPLMT-FLAG = "Y" AND
CMP3F1      (WS-LINK-STATUS = "AA" OR
CMP3F1      WS-LINK-STATUS = "AC" OR

      GP4000
      GP4000           MOVE SPACES           TO WS-STPTYP.
      GP4000           MOVE "N"              TO TAB-VL2(06)
      GP4000                                 TAB-VL2(07)
      GP4000                                 WS-OKAY.
      GP4000
      GP4000           PERFORM D500-PROCESS-RPRRSN
      GP4000             THRU D599-PROCESS-RPRRSN-EX.
      GP4000           D120-EVAL-STP-CCY-EX.
      GP4000           EXIT.
      GP4000       /
      D200-VALIDATION.
                   
                      MOVE WS-BANKID        TO WK-VTE3-BANKID.
                      MOVE WS-INTEMBNKID    TO WK-VTE3-INTEMBNKID.
                      MOVE WS-ACBNKID       TO WK-VTE3-ACBNKID.
                      MOVE WS-ACCNO         TO WK-VTE3-BANKAC.
                      MOVE WK-C-VBAC-ACCTYP TO WK-VTE3-BANKACCTYP.
                      MOVE WS-INTEMBNKACC   TO WK-VTE3-INTEMBNKACC.
                      MOVE WS-ACBNKACC      TO WK-VTE3-ACBNKACC.
                      MOVE TABLE-ARR2       TO WK-VTE3-DATAE3.
                   
                      IF WS-OKAY = "Y"
                          MOVE WS-ACT1      TO WK-VTE3-ACT
                          MOVE WS-STPTYP    TO WK-VTE3-STPTYP
                          MOVE "TT202"      TO WK-VTE3-PMODE
                          MOVE "N"          TO WK-VTE3-ERROR-FOUND
                      ELSE
                          MOVE SPACES       TO WK-VTE3-ACT
                                             WK-VTE3-STPTYP
                                             WK-VTE3-PMODE
                          MOVE "Y"          TO WK-VTE3-ERROR-FOUND
                      END-IF.
                      MOVE "N"              TO WS-FLAG1.
                      PERFORM D300-LOGGING  THRU D399-LOGGING-EX.
                   
      D299-VALIDATION-EX.
                      EXIT.
                      EJECT
      D300-LOGGING.
                      MOVE WK-VTE3-PARALNO  TO WK-LOGG-PARALNO.
                      MOVE WK-VTE3-SEQNUM   TO WK-LOGG-SEQNUM.
                      MOVE TABLE-ARR2       TO WK-LOGG-DATAE3.
                      MOVE "E3"             TO WK-LOGG-TABTYP.
                      MOVE WK-VTE3-ACT      TO WK-LOGG-ACTE3.
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
           IF  NOT(TAG56-OPT  = SPACES
           AND TAG56-PTID     = SPACES
           AND TAG56-BIC      = SPACES)
               MOVE TFSSTPL-BNKENTTY  TO WK-N-VBAC-BNKENTTY
               MOVE TFSSTPL-CUYCD     TO WK-C-VBAC-CUYCD
               MOVE TAG56-BIC         TO WK-C-VBAC-BANKID
               CALL 'TRFVBAC' USING WK-C-VBAC-RECORD
               IF  WK-C-VBAC-ERROR-CD = SPACES
               AND TAG56-PTID         = SPACES
               OR  WK-C-VBAC-ERROR-CD = SPACES
               AND TAG56-PTID NOT     = SPACES
               AND WK-C-VBAC-BNKACNO  = TAG56-PTID
                   MOVE "Y" TO BKAC56-IND
               ELSE
                   MOVE "N" TO BKAC56-IND
      5Q1JM1     INITIALIZE WK-C-RPRRSN-AREA
      5Q1JM1     MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
      5Q1JM1     PERFORM D500-PROCESS-RPRRSN
      5Q1JM1     THRU D599-PROCESS-RPRRSN-EX
           END-IF
           END-IF.

           IF  NOT(TAG57-OPT  = SPACES
           AND TAG57-PTID     = SPACES
           AND TAG57-BIC      = SPACES)
               MOVE TFSSTPL-BNKENTTY  TO WK-N-VBAC-BNKENTTY
               MOVE TFSSTPL-CUYCD     TO WK-C-VBAC-CUYCD
               MOVE TAG57-BIC         TO WK-C-VBAC-BANKID
               CALL 'TRFVBAC' USING WK-C-VBAC-RECORD
               IF  WK-C-VBAC-ERROR-CD = SPACES
               AND TAG57-PTID         = SPACES
               OR  WK-C-VBAC-ERROR-CD = SPACES
               AND TAG57-PTID NOT     = SPACES
               AND WK-C-VBAC-BNKACNO  = TAG57-PTID
                   MOVE "Y" TO BKAC57-IND
               ELSE
                   MOVE "N" TO BKAC57-IND
      5Q1JM1     INITIALIZE WK-C-RPRRSN-AREA
      5Q1JM1     MOVE WK-C-VBAC-ERROR-CD TO WK-C-RPRCODE
      5Q1JM1     PERFORM D500-PROCESS-RPRRSN
      5Q1JM1     THRU D599-PROCESS-RPRRSN-EX
           END-IF
           END-IF.
      D499-BKAC-VALIDATION-EX.
           EXIT.
           EJECT
      5Q1JM1 D500-PROCESS-RPRRSN SECTION.
      5Q1JM1 D500-ENTRY.

      5Q1JM1
      5Q1JM1           MOVE WK-VTE3-PARALNO     TO WK-C-RRSN-QUENUM.
      5Q1JM1           MOVE WK-VTE3-SEQNUM      TO WK-C-RRSN-QUESUF.
      5Q1JM1           MOVE WK-C-TRNNO          TO WK-C-RRSN-TRNNO.
      5Q1JM1           MOVE WK-C-FUNCTID        TO WK-C-RRSN-FUNCTID.
      5Q1JM1           MOVE WK-C-SEGCODE        TO WK-C-RRSN-SEGCODE.
      5Q1JM1           MOVE SPACES              TO WK-C-RRSN-SEGDESC.
      5Q1JM1           MOVE WK-N-STAFFIND       TO WK-C-RRSN-STAFFIND.
      5Q1JM1           MOVE WS-ACCNO            TO WK-C-RRSN-ACCNO.
      5Q1JM1           MOVE WK-C-QRATE          TO WK-C-RRSN-QRATE.
      5Q1JM1           MOVE WK-N-SYSDTE         TO WK-C-RRSN-RPRDTE.
      5Q1JM1*          MOVE WK-C-RPRCODE        TO WK-C-RRSN-RSNCDE.
      5Q1JE1           IF  WK-C-RPRCODE = SPACE
      5Q1JE1               MOVE "RSN9999"       TO WK-C-RRSN-RSNCDE
      5Q1JE1           ELSE
      5Q1JE1               MOVE WK-C-RPRCODE    TO WK-C-RRSN-RSNCDE
      5Q1JE1           END-IF.
      5Q1JE1
      5Q1JM1           MOVE SPACES              TO WK-C-RRSN-RSNDESC.
      5Q1JM1           MOVE WK-C-RPRPGM         TO WK-C-RRSN-RPRPGM.
      5Q1JM1           CALL "TRFGRRSN" USING WK-C-RRSN-RECORD.
      5Q1JM1
      5Q1JM1       D599-PROCESS-RPRRSN-EX.
      5Q1JM1           EXIT.
      5Q1JM1           EJECT
      5Q1JM1
                     Z000-END-PROGRAM.
                          CLOSE TFSSTPL
                     ID1VKE TFSBNKET
                          TFSCLSYS.
                          EXIT PROGRAM.