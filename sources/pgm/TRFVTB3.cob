       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRFVTB3.
       AUTHOR. TVK.
       DATE-WRITTEN. JUN 04.
      *DESCRIPTION : TABLE B3 VALIDATION.
      *   SUBROUTINE - CREDIT PARTY CHECKING FOR FIELD 56/57
      *   MT200 LCY
      *===========================================================
      * HISTORY OF MODIFICATION:
      *===========================================================
      * REM269 - TMPSRK  - 06/04/2017 - JIRA LOG REM-269
      *   STANDARDIZATION OF PROGRAM TO
      *   RETRIEVE CURRENCY AND COUNTRY
      *   CODE FROM SYSTEM PARAMETER FILE.
      * 7Q1EM1 - TMPFYM  - 20/10/2016 - REM Q1 2017 RELEASE
      *   - e-Req 47511 Refinement of
      *   Duplicate checking for Inw
      *   - Recompiled due to changes made
      *   in VSTPL copy book.
      *-----------------------------------------------------------*
      * CMP3A2 - CMPESQ  - 05/08/2016 - CASH MANAGEMENT PROJECT 3
      *   JIRA: PCSHGMGMTSG-190
      *   UPDATE STP LIMIT VALIDATION
      *===========================================================
      * CMP3A1 - CMPESQ  - 01/07/2016 - CASH MANAGEMENT PROJECT 3
      *   - INCLUDE REM INDICATOR
      *===========================================================
      * CMP3X1 - CMPESQ  - 14/06/2016 - CASH MANAGEMENT PROJECT RELEASE
      *    3
      *   JIRA: PCSHGMGMTSG-109,
      *   PCSHGMGMTSG-110
      *   - FIX STP LIMIT PROCESS
      *===========================================================
      * CMP3FL - VENAF2  - 07/01/2016 - CASH MANAGEMENT PROJECT RELEASE
      *    3
      *   STP Limit by Account, CIF
      *   and Segment
      *===========================================================
      * 5Q1ARV - TMPARV  - 10/11/2014 - 14HOREM024/14HOREM029/14HOREM028
      *   Modified to determine the
      *   repair reason and create an
      *   entry on the new file (RFTRRSN)
      *===========================================================
      * SM1TY1 - TMPTY1  - 11/08/2005 - DON'T USE MAS ACCOUNT NUMBER
      *   FOR CR MAS/MAS202 TRANSACTION
      *===========================================================
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. LOCAL-DATA IS LOCAL-DATA-AREA
              I-O-FEEDBACK IS I-O-FEEDBACK-AREA
              UPSI-0 IS UPSI-SWITCH-0

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

      *      SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
      *          ORGANIZATION IS INDEXED
      *          ACCESS MODE IS RANDOM
      *          RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
      *          FILE STATUS IS WK-C-FILE-STATUS.

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

      *FD  TFSBNKET
      *    LABEL RECORDS ARE OMITTED
      *    DATA RECORD IS TFSBNKET-REC.
      *01  TFSBNKET-REC.
      *    COPY DDS-ALL-FORMATS OF TFSBNKET.

       WORKING-STORAGE SECTION.
      ***********************
       01  WK-C-COMMON.
              COPY ASCMWS.

       01  WK-C-LINK-LIMIT.
           05  WK-C-LINK-AREA-INPUT.
           10  WS-LINK-BNKENTTY          PIC S9(1).
           10  WS-LINK-ACCNO             PIC X(11) VALUE 0.
           10  WS-LINK-CCY               PIC X(03) VALUE SPACES.
           10  WS-LINK-AMT               PIC S9(13)V99 VALUE 0.
           10  WS-LINK-REMIND            PIC X(01).
           05  WK-C-LINK-AREA-OUTPUT.
           10  WS-LINK-STATUS            PIC X(02) VALUE SPACES.

       01  TAG56-FORMAT.
           05  TAG56-LINE-1.
           07  TAG56-FIL1               PIC X(2).
           07  TAG56-OPT                PIC X(1).
           07  TAG56-FIL2               PIC X(1).
           07  TAG56-PTID.
           09  TAG56-PTID-1         PIC X(02).
           09  TAG56-PTID-2         PIC X(35).
           05  TAG56-LINE-2                 PIC X(35).
           05  TAG56-BIC REDEFINES TAG56-LINE-2.
           07  TAG56A-SUB1              PIC X(4).
           07  TAG56A-SUB2              PIC X(2).
           07  TAG56A-SUB3              PIC X(2).
           07  TAG56A-SUB4              PIC X(3).
           07  TAG56A-FILLER            PIC X(24).
           05  TAG56-LOC REDEFINES TAG56-LINE-2
              PIC X(35).
           05  TAG56-NAME REDEFINES TAG56-LINE-2
              PIC X(35).
           05  TAG56-LINE-3                 PIC X(35).
           05  TAG56-LINE-4                 PIC X(35).
           05  TAG56-LINE-5                 PIC X(35).

       01  TAG57-FORMAT.
           05  TAG57-LINE-1.
           07  TAG57-FIL1               PIC X(2).
           07  TAG57-OPT                PIC X(1).
           07  TAG57-FIL2               PIC X(1).
           07  TAG57-PTID.
           09  TAG57-PTID-1         PIC X(02).
           09  TAG57-PTID-2         PIC X(35).
           05  TAG57-LINE-2                 PIC X(35).
           05  TAG57-BIC REDEFINES TAG57-LINE-2.
           07  TAG57A-SUB1              PIC X(4).
           07  TAG57A-SUB2              PIC X(2).
           07  TAG57A-SUB3              PIC X(2).

           07  TAG57A-SUB4     PIC X(3).
           07  TAG57A-FILLER   PIC X(24).
           05  TAG57-LOC REDEFINES TAG57-LINE-2
              PIC X(35).
           05  TAG57-NAME REDEFINES TAG57-LINE-2
              PIC X(35).
           05  TAG57-LINE-3    PIC X(35).
           05  TAG57-LINE-4    PIC X(35).
           05  TAG57-LINE-5    PIC X(35).

       01  TABLE-ARRAY.
           05  TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".

       01  TABLE-ARR2.
           05  TAB-VL2 OCCURS 20 TIMES PIC X VALUE "X".

       01  PATH-P1         PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXX".
       01  PATH-P2         PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P3         PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXX".
       01  PATH-P4         PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P5         PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P6         PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P7         PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P8         PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P9         PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXX".
       01  PATH-P10        PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P11        PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P12        PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P13        PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXX".
       01  PATH-P14        PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXX".
       01  PATH-P15        PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXX".
       01  PATH-P16        PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P17        PIC X(20)
              VALUE "XXXXYYYYXXXXXXXXXXXX".
       01  PATH-P18        PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXX".
       01  PATH-P19        PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXX".

       01  PATH-P20                   PIC X(20)
              VALUE "XXXXXXNYYXXXXXXXXXXXXXXXX".
       01  PATH-P21                   PIC X(20)
              VALUE "XXXXXXXXXXXXXXXXXXXXXXXX".
       01  PATH-P22                   PIC X(20)
              VALUE "XYXXXXXXXXXXXXXXXXXXXXXX".

       01  WK-C-PARADATA.
           05  WK-C-PARAVALU          PIC X(20).
           05  WK-N-PARAVALU          REDEFINES WK-C-PARAVALU
              PIC 9(13)V99.
           05  WK-N-IRMPSTP           PIC 9(13)V99.
           05  WK-N-IRM1STP           PIC 9(13)V99.

       01  WK-C-WORK-AREA.
           05  FIRST-TIME             PIC X(01) VALUE "Y".
           05  WS-FLAG1               PIC X(01) VALUE SPACE.
           05  WS-FLAG2               PIC X(01) VALUE SPACE.
           05  WS-ACT1                PIC X(01) VALUE SPACE.
           05  WS-ACT2                PIC X(01) VALUE SPACE.
           05  WS-ACT3                PIC X(01) VALUE SPACE.
           05  WS-ACT4                PIC X(01) VALUE SPACE.
           05  MEPS56-IND             PIC X(01) VALUE SPACE.
           05  MEPS57-IND             PIC X(01) VALUE SPACE.
           05  BKAC56-IND             PIC X(01) VALUE SPACE.
           05  BKAC57-IND             PIC X(01) VALUE SPACE.
           05  WS-OKAY                PIC X(01) VALUE SPACE.
           05  WS-STPTYP              PIC X(04) VALUE SPACE.
           05  WS-ACCNO               PIC X(11) VALUE SPACE.
           05  WS-INTEMBNKACC         PIC X(11) VALUE SPACE.
           05  WS-ACBNKACC            PIC X(11) VALUE SPACE.
           05  WS-BANKID              PIC X(11) VALUE SPACE.
           05  WS-INTEMBNKID          PIC X(11) VALUE SPACE.
           05  WS-ACBNKID             PIC X(11) VALUE SPACE.

5Q1ARV        01  WK-C-RPPRSN-AREA.
5Q1ARV     05  WK-C-SEGCDE         PIC X(01) VALUE SPACE.
5Q1ARV     05  WK-N-STAFFIND       PIC S9(02) VALUE ZEROS.
5Q1ARV     05  WK-C-TRNN0          PIC X(12) VALUE SPACE.
5Q1ARV     05  WK-C-QRATE          PIC X(02) VALUE SPACE.
5Q1ARV     05  WK-C-RPRCODE        PIC X(07) VALUE SPACE.
5Q1ARV     05  WK-C-FUNCTID        PIC X(08) VALUE SPACE.
5Q1ARV     05  WK-N-RPRDTE         PIC S9(08) VALUE ZEROS.
5Q1ARV        01  WK-N-SYSDTE         PIC S9(08) VALUE ZEROS.
5Q1ARV        01  WK-C-RPRPGM         PIC X(10) VALUE "TRFVTB3".
REM269        01  WK-C-LCUYCD         PIC X(03).

              COPY VCCA.
              COPY VSTPL.
              COPY VBAC.
              COPY VBBAS.
              COPY XPARA.
              COPY NSTP.
              COPY ACMN.

       LINKAGE SECTION.
      ****************
              COPY VTB3.
       PROCEDURE DIVISION USING WK-VTB3.
      ********************************
       MAIN-MODULE.
           INITIALIZE WK-VTB3-OUTPUT
              WK-LOGG
       WK-C-WORK-AREA.
           MOVE ALL "X" TO TABLE-ARRAY.
           MOVE ALL "X" TO TABLE-ARR2.
           MOVE "Y" TO FIRST-TIME.
           MOVE ZEROS TO WK-C-RRSN-QUENUM
              WK-C-RRSN-QUESUF
              WK-C-RRSN-STAFFIND
              WK-C-RRSN-SEQNUM
       WK-C-RRSN-RPRDTE.
           IF FIRST-TIME = "Y"
              OPEN INPUT TFSSTPL
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVTB3 - OPEN FILE ERROR - TFSSTPL"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT TFSCLSYS
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVTB3 - OPEN FILE ERROR - TFSCLSYS"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
              OPEN INPUT TFSBNKET
              IF NOT WK-C-SUCCESSFUL
                    AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVTB3 - OPEN FILE ERROR - TFSBNKET"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              END-IF
       END-IF.
           MOVE WK-VTB3-PARALNO TO TFSSTPL-PARALNO.
           MOVE WK-VTB3-SEQNUM TO TFSSTPL-SEQNUM.
           READ TFSSTPL
              KEY IS EXTERNALLY-DESCRIBED-KEY.
           IF WK-C-SUCCESSFUL

              MOVE "N"               TO WS-OKAY
              MOVE TFSSTPL-TAG56     TO TAG56-FORMAT
              MOVE TFSSTPL-TAG57     TO TAG57-FORMAT
              PERFORM A100-INITIAL-SUBROUTINE
                 THRU A199-INITIAL-SUBROUTINE-EX
              PERFORM A200-MOVE-TAG-VALUES
                 THRU A299-MOVE-TAG-VALUES-EX
              PERFORM B100-PATH-CHOICE THRU B199-PATH-CHOICE-EX
       END-IF.
           GO TO Z000-END-PROGRAM.

       A100-INITIAL-SUBROUTINE.
      *----------------------------------------------------------------*
      *    GET DATA FROM "TFSCLSYS" TABLE                              *
      *----------------------------------------------------------------*
           READ TFSCLSYS.
           IF NOT WK-C-SUCCESSFUL
              DISPLAY "TRFVFB3 - READ TFSCLSYS ERROR"
              DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
              GO TO Z000-END-PROGRAM.

      **********MOVE TFSCLSYS-SYSDTE TO L-N-G-SYSDTE.
5Q1ARV        MOVE TFSCLSYS-SYSDTE TO WK-N-SYSDTE.
      **********MOVE TFSCLSYS-LCNTRYCD TO L-C-G-L-CNTRYCD.
      **********MOVE TFSCLSYS-LCUYCD TO L-C-G-L-CUYCD.

                 REM269*Retrieve local currency code
                 |    INITIALIZE WK-C-XGSPA-RECORD.
                 |    MOVE "RSYTCLLCUY" TO WK-C-XGSPA-GHPARCD.
                 |    CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
                 |    IF WK-C-XGSPA-ERROR-CD = SPACES
                 |        MOVE WK-C-XGSPA-GHPARVAL TO WK-C-LCUYCD
                 |    ELSE
                 |        MOVE SPACES TO WK-C-LCUYCD
REM269     END-IF.
      *----------------------------------------------------------------*
      *    GET SYSTEM PARAMETERS FOR PSTP & ISTP                       *
      *----------------------------------------------------------------*
           MOVE "IRSMSGP" TO WK-C-XPARA-PARACD.
           CALL "TRFXPARA" USING WK-C-XPARA-RECORD.
           IF WK-C-XPARA-ERROR-CD NOT = SPACES
              DISPLAY "TREEEDT = TRFXPARA ROUTINE ERROR"
              DISPLAY "FILE STATUS - " WK-C-XPARA-FS
              DISPLAY "ERROR ID - " WK-C-XPARA-ERROR-CD
              DISPLAY "KEY - " WK-C-XPARA-INPUT
              GO TO Z000-END-PROGRAM
           ELSE
              MOVE WK-C-XPARA-PARAVALU
                 TO WK-C-PARAVALU
              MOVE WK-N-PARAVALU
                 TO WK-N-IRMPSTP
       END-IF.
           MOVE "IRSMSG1" TO WK-C-XPARA-PARACD.
           CALL "TRFXPARA" USING WK-C-XPARA-RECORD.

           IF  WK-C-XPARA-ERROR-CD NOT = SPACES
              DISPLAY "TREEEDT  TRFXPARA ROUTINE ERROR"
              DISPLAY "FILE STATUS - " WK-C-XPARA-FS
              DISPLAY "ERROR ID  - " WK-C-XPARA-ERROR-CD
              DISPLAY "KEY       - " WK-C-XPARA-INPUT
              GO TO Z000-END-PROGRAM
           ELSE
              MOVE WK-C-XPARA-PARAVALU
                 TO  WK-C-PARAVALU
              MOVE WK-N-PARAVALU
                 TO  WK-N-IRM1STP
       END-IF.
       A199-INITIAL-SUBROUTINE-EX.
       EXIT.
       A200-MOVE-TAG-VALUES.
           IF TAG56-BIC NOT = SPACES
                 AND TAG56-OPT = "A"
              MOVE TAG56-BIC     TO  WS-INTEMBNKID
                 WS-BANKID
              MOVE TAG56-PTID    TO  WS-INTEMBNKACC
                 WS-ACCNO
       END-IF.
           IF TAG57-BIC NOT = SPACES
                 AND TAG57-OPT = "A"
              MOVE TAG57-BIC     TO  WS-ACBNKID
                 WS-BANKID
              MOVE TAG57-PTID    TO  WS-ACBNKACC
                 WS-ACCNO
       END-IF.
           IF TAG57-NAME NOT = SPACES
                 AND TAG57-OPT = "D"
              MOVE TAG57-PTID    TO  WS-ACBNKACC
                 WS-ACCNO
              MOVE TAG57-NAME    TO  WK-VTB3-ACBNKNM
              MOVE TAG57-LINE-3  TO  WK-VTB3-ACBNKADR1
              MOVE TAG57-LINE-4  TO  WK-VTB3-ACBNKADR2
              MOVE TAG57-LINE-5  TO  WK-VTB3-ACBNKADR3
       END-IF.
           MOVE TFSSTPL-BNKENTITY TO WK-N-VBAC-BNKENTITY.
           MOVE WS-BANKID         TO WK-C-VBAC-BANKID.
           MOVE TFSSTPL-CUYCD     TO WK-C-VBAC-CUYCD.
           CALL "TRFVBAC" USING WK-C-VBAC-RECORD.
           IF WK-C-VBAC-ACDUBUI NOT = SPACES
              MOVE WK-C-VBAC-ACDUBUI TO WK-VTB3-ACDUBUI
              IF WS-ACCNO = SPACES
                 MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
              END-IF
           ELSE
              MOVE "D" TO WK-VTB3-ACDUBUI
       END-IF.

       A299-MOVE-TAG-VALUES-EX.
       EXIT.
              EJECT

       B100-PATH-CHOICE.
           PERFORM D400-MEPS-VALIDATION.
           IF TAG56-OPT    = SPACES
                 AND TAG56-PTID  = SPACES
                 AND TAG56-BIC   = SPACES
                 OR TAG56-OPT    = "A"
                 AND TAG56-BIC   = "UOVBSSGGXXX"
              PERFORM C100-VALIDATION-PART
                 THRU C199-VALIDATION-PART-EX
       END-IF.
           IF MEPS56-IND   = "Y"
                 AND TAG56-BIC   NOT = "UOVBSSGGXXX"
                 AND TAG56-BIC   NOT = "MASGSSGGXXX"
              PERFORM C300-VALIDATION-PART
                 THRU C399-VALIDATION-PART-EX
       END-IF.
           IF MEPS56-IND   = "N"
                 AND BKAC56-IND  = "Y"
                 AND TAG56-BIC   NOT = "UOVBSSGGXXX"
                 AND TAG56-BIC   NOT = "MASGSSGGXXX"
              PERFORM C400-VALIDATION-PART
                 THRU C499-VALIDATION-PART-EX
       END-IF.
           PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.
           PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.
       EXIT.

       C100-VALIDATION-PART.
           IF TAG57-OPT    = "A"
                 AND TAG57-BIC   = "UOVBSSGGXXX"
                 AND TAG57-PTID  NOT = SPACES
              MOVE PATH-P22 TO TABLE-ARRAY
              MOVE "Y"     TO WS-OKAY
       END-IF.
           IF TAG57-OPT    = "A"
                 AND TAG57-BIC   NOT = "UOVBSSGGXXX"
                 AND TAG57-BIC   NOT = "MASGSSGGXXX"
              IF MEPS57-IND = "Y"
                 MOVE PATH-P2  TO TABLE-ARRAY
                 MOVE "Y"      TO WS-ACT3
                    WS-OKAY
              END-IF
              IF MEPS57-IND = "N"
                    AND BKAC57-IND = "Y"
                 MOVE PATH-P4  TO TABLE-ARRAY
                 MOVE "Y"      TO WS-ACT4
                    WS-OKAY

              END-IF
       END-IF.
       C199-VALIDATION-PART-EX.
       EXIT.
              EJECT
       C300-VALIDATION-PART.
           IF TAG57-OPT    = SPACES
                 AND TAG57-PTID  = SPACES
                 AND TAG57-BIC   = SPACES
              MOVE PATH-P9 TO TABLE-ARRAY
              MOVE "Y"    TO WS-ACT1
                 WS-OKAY
       END-IF.
           IF TAG57-OPT    NOT = "A"
                 AND NOT(TAG57-OPT = SPACES
                 AND TAG57-PTID  = SPACES
                 AND TAG57-BIC   = SPACES)
              MOVE PATH-P10 TO TABLE-ARRAY
              MOVE "Y"     TO WS-ACT1
                 WS-OKAY
       END-IF.
           IF TAG57-OPT    = "A"
                 AND TAG57-BIC   NOT = "UOVBSGSGXXX"
                 AND TAG57-BIC   NOT = "MASGSGSGXXX"
              IF MEPS57-IND = "Y"
                    AND TAG57-OPT = "A"
                 MOVE PATH-P11 TO TABLE-ARRAY
                 MOVE "Y"     TO WS-ACT1
                    WS-OKAY
              END-IF
              IF MEPS57-IND = "N"
                    AND BKAC57-IND = "Y"
                 MOVE PATH-P13 TO TABLE-ARRAY
                 MOVE "Y"     TO WS-ACT1
                    WS-OKAY
              END-IF
              IF MEPS57-IND = "N"
                    AND BKAC57-IND = "N"
                 MOVE PATH-P14 TO TABLE-ARRAY
                 MOVE "Y"     TO WS-ACT1
                    WS-OKAY
              END-IF
       END-IF.
       C399-VALIDATION-PART-EX.
       EXIT.
              EJECT
       C400-VALIDATION-PART.
           IF TAG57-OPT    = SPACES
                 AND TAG57-PTID  = SPACES
                 AND TAG57-BIC   = SPACES
              MOVE PATH-P15 TO TABLE-ARRAY
              MOVE "Y"     TO WS-ACT2

                 WS-OKAY
       END-IF.
           IF TAG57-OPT     NOT = "A"
                 AND NOT(TAG57-OPT = SPACES
                 AND TAG57-PTID   = SPACES
                 AND TAG57-BIC    = SPACES)
              MOVE PATH-P15 TO TABLE-ARRAY
              MOVE "Y"     TO WS-ACT2
                 WS-OKAY
       END-IF.
           IF TAG57-OPT     = "A"
                 AND TAG57-BIC    NOT = "UOWBSGSGXXX"
                 AND TAG57-BIC    NOT = "MASGSGSGXXX"
              IF MEPS57-IND = "Y"
                    AND TAG57-OPT = "A"
                 MOVE PATH-P17 TO TABLE-ARRAY
                 MOVE "Y" TO WS-ACT2
                    WS-OKAY
              END-IF
              IF MEPS57-IND = "N"
                    AND BKAC57-IND = "Y"
                    AND TAG57-OPT = "A"
                 MOVE PATH-P19 TO TABLE-ARRAY
                 MOVE "Y" TO WS-ACT2
                    WS-OKAY
              END-IF
              IF MEPS57-IND = "N"
                    AND BKAC57-IND = "N"
                    AND TAG57-OPT = "A"
                 MOVE PATH-P20 TO TABLE-ARRAY
                 MOVE "Y" TO WS-ACT2
                    WS-OKAY
              END-IF
       END-IF.
       C499-VALIDATION-PART-EX.
       EXIT.
              EJECT
       D100-VALIDATION.
           MOVE "Y" TO WS-FLAG1.
           IF TABLE-ARRAY NOT = ALL "X"
              MOVE "Y" TO WS-OKAY
5Q1JE1        INITIALIZE WK-C-RPRRSN-AREA
5Q1JE1        MOVE "RSN0113" TO WK-C-RPRCODE
5Q1JE1        PERFORM D500-PROCESS-RPRRSN
5Q1JE1           THRU D599-PROCESS-RPRRSN-EX
       END-IF.
CMP3X1*CMP3A2 MOVE SPACES TO WS-LINK-STATUS.
CMP3A2     MOVE "A1" TO WS-LINK-STATUS.
CMP3FL     IF WS-ACCNO NOT = SPACES
CMP3FL        INITIALIZE WK-C-RPRRSN-AREA
CMP3FL        INITIALIZE WK-C-LINK-LIMIT
CMP3FL        MOVE TFSSTPL-BNKENTITY TO WS-LINK-BNKENTITY

              MOVE WS-ACCNO           TO WS-LINK-ACCNO
              MOVE TFSSTPL-CUYCD      TO WS-LINK-CCY
              MOVE TFSSTPL-AMT        TO WS-LINK-AMT
              MOVE "I"                TO WS-LINK-REMIND
              CALL "TRFWLMT" USING WK-C-LINK-LIMIT
              EVALUATE WS-LINK-STATUS
                 WHEN "XX"
                 MOVE "N"            TO WS-OKAY
                 MOVE "RSN0311"      TO WK-C-RPRCODE
                 PERFORM D500-PROCESS-RPRRSN
                    THRU D599-PROCESS-RPRRSN-EX
                 WHEN "AA"
                 MOVE "N"            TO WS-OKAY
                 MOVE "RSN0312"      TO WK-C-RPRCODE
                 PERFORM D500-PROCESS-RPRRSN
                    THRU D599-PROCESS-RPRRSN-EX
                 WHEN "AC"
                 MOVE "N"            TO WS-OKAY
                 MOVE "RSN0313"      TO WK-C-RPRCODE
                 PERFORM D500-PROCESS-RPRRSN
                    THRU D599-PROCESS-RPRRSN-EX
                 WHEN "AS"
                 MOVE "N"            TO WS-OKAY
                 MOVE "RSN0314"      TO WK-C-RPRCODE
                 PERFORM D500-PROCESS-RPRRSN
                    THRU D599-PROCESS-RPRRSN-EX
                 END-EVALUATE
       END-IF.
           IF TAB-VAL(01) NOT = "X"
              MOVE TAB-VAL(01)    TO TAB-VL2(01)
              PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(02) NOT = "X"
              MOVE TAB-VAL(02)    TO TAB-VL2(02)
              PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(03) NOT = "X"
              MOVE TAB-VAL(03)    TO TAB-VL2(03)
              PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(04) NOT = "X"
              MOVE TAB-VAL(04)    TO TAB-VL2(04)
              PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(05) NOT = "X"
              MOVE TAB-VAL(05)    TO TAB-VL2(05)
              PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
           IF TAB-VAL(06) NOT = "X"
              MOVE TAB-VAL(06)    TO TAB-VL2(06)
              PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.

           IF TAB-VAL(07) NOT = "X" AND WS-OKAY = "Y"
              MOVE "Y"        TO TAB-VL2(07)
              IF WS-LINK-STATUS = "A0"
                 MOVE "PSTP"     TO WS-STPTYP
                 MOVE "N"        TO TAB-VL2(07)
                 MOVE "X"        TO TAB-VL2(08)
              ELSE
                 IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
                    IF TFSSTPL-CUYCD = WK-C-LCUYCD
                          AND TFSSTPL-AMT <= WK-N-IRM1PSTP
                          AND WS-LINK-STATUS = "A1"
                       MOVE "PSTP"     TO WS-STPTYP
                       MOVE "N"        TO TAB-VL2(07)
                       MOVE "X"        TO TAB-VL2(08)
                    END-IF
                 END-IF
                 PERFORM D300-LOGGING THRU D399-LOGGING-EX
                 IF WS-LINK-STATUS = "A0"
                    GO TO D199-VALIDATION-EX
       END-IF.
                 IF TAB-VL(08) NOT = "X" AND WS-OKAY = "Y"
                    IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
                       IF TFSSTPL-CUYCD = WK-C-LCUYCD
                             AND TFSSTPL-AMT > WK-N-IRM1STP
                          MOVE "2STP"     TO WS-STPTYP
                          MOVE "Y"        TO TAB-VL2(08)
                          INITIALIZE WK-C-RPRRSN-AREA
                          MOVE "RSN0023" TO WK-C-RPRRCODE
                          PERFORM D500-PROCESS-RPRRSN
                             THRU D599-PROCESS-RPRRSN-EX
                          ELSE
                          IF TFSSTPL-CUYCD = TFSCLSYS-LCUYCD
                             IF TFSSTPL-CUYCD = WK-C-LCUYCD
                                   AND TFSSTPL-AMT > WK-N-IRM1PSTP
                                   OR (WS-LINK-STATUS = "AA"
                                   OR WS-LINK-STATUS = "AC"
                                   OR WS-LINK-STATUS = "AS")
                                MOVE "1STP"     TO WS-STPTYP
                                MOVE "N"        TO TAB-VL2(08)
                                INITIALIZE WK-C-RPRRSN-AREA
                                MOVE "RSN0039" TO WK-C-RPRRCODE
                                PERFORM D500-PROCESS-RPRRSN
                                   THRU D599-PROCESS-RPRRSN-EX
                                END-IF
                             END-IF
                          PERFORM D300-LOGGING THRU D399-LOGGING-EX
       END-IF.
       D199-VALIDATION-EX.
       EXIT.
                          EJECT
       D200-VALIDATION.

                       MOVE WS-BANKID           TO WK-VTB3-BANKID.
                       MOVE WS-INTEMBNKID       TO WK-VTB3-INTEMBNKID.
                       MOVE WS-ACBNKID          TO WK-VTB3-ACBNKID.
                       MOVE WS-ACCNO            TO WK-VTB3-BANKAC.
                       MOVE WK-C-VBAC-ACCTYP    TO WK-VTB3-BANKACCTYP.
                       MOVE WS-INTEMBNKACC      TO WK-VTB3-INTEMBNKACC.
                       MOVE WS-ACBNKACC         TO WK-VTB3-ACBNKACC.
                       MOVE TABLE-ARR2          TO WK-VTB3-DATAB3

                       IF WS-ACT1 = "Y"
                             OR WS-ACT3 = "Y"
                          MOVE WS-ACT1         TO WK-VTB3-ACT1
                          MOVE WS-ACT3         TO WK-VTB3-ACT3
                          MOVE "MAS202"        TO WK-VTB3-PMODE
           SM1TY1*    MOVE TFSSTPL-BNKENTITY  TO TFSBNKET-BNKENTITY
           SM1TY1*    READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
                             SM1TY1*    NOT INVALID KEY
           SM1TY1*    MOVE TFSBNKET-MASNOSTR TO WK-VTB3-BANKAC
                             SM1TY1*    END-READ
                          MOVE SPACES         TO WK-VTB3-BANKAC
                       END-IF
                       IF WS-ACT2 = "Y"
                             OR WS-ACT4 = "Y"
                          MOVE WS-ACT2         TO WK-VTB3-ACT2
                          MOVE WS-ACT4         TO WK-VTB3-ACT4
                          MOVE "TT202"         TO WK-VTB3-PMODE
                       END-IF

                       IF WS-OKAY = "Y"
                          MOVE WS-STPTYP       TO WK-VTB3-STPTYP
                          MOVE "N"             TO WK-VTB3-ERROR-FOUND
                       ELSE
                          MOVE SPACES          TO WK-VTB3-ACT1
                             WK-VTB3-ACT2
                             WK-VTB3-ACT3
                             WK-VTB3-ACT4
                             WK-VTB3-STPTYP
                          MOVE "Y"             TO WK-VTB3-ERROR-FOUND
       END-IF.
                       MOVE "N"                 TO WS-FLAG1.
                       PERFORM D300-LOGGING     THRU D399-LOGGING-EX.

       D299-VALIDATION-EX.
       EXIT.
                          EJECT

       D300-LOGGING.
                       MOVE WK-VTB3-PARALNO     TO WK-LOGG-PARALNO.
                       MOVE WK-VTB3-SEQNUM      TO WK-LOGG-SEQNUM.
                       MOVE TABLE-ARR2          TO WK-LOGG-DATAB3.
                       MOVE "B3"                TO WK-LOGG-TABTYP.
                       MOVE WK-VTB3-ACT         TO WK-LOGG-ACTB3.
                       CALL "TRFLOGGCL" USING WK-LOGG
                          WS-FLAG1

                       IF WK-LOGG-ERROR-FOUND = "Y"
                          GO TO D399-LOGGING-EX
       END-IF.
       D399-LOGGING-EX.
       EXIT.
       EJECT.
       D400-MEPS-VALIDATION.
                       IF NOT(TAG56-OPT = SPACES
                             AND TAG56-PTID = SPACES
                             AND TAG56-BIC  = SPACES)
                             AND TAG56-OPT  = "A"
                          IF TAG56-PTID = SPACES
                             MOVE TAG56-BIC         TO WK-C-VBBAS-BANKID
                             CALL "TRFVBBAS" USING WK-C-VBBAS-RECORD
                             IF WK-C-VBBAS-SHIFTNO NOT = SPACES
                                MOVE "Y"          TO MEPS56-IND
           MOVE WK-C-VBBAS-SHIFTNO TO WK-VTB3-SHIFTNO
                             ELSE
                                MOVE "N"          TO MEPS56-IND
                             END-IF
                          END-IF
                          IF TAG56-PTID  NOT = SPACES
                                OR MEPS56-IND  = "N"
                             MOVE "N"          TO MEPS56-IND
           MOVE TFSSTPL-BNKENTITY TO WK-N-VBAC-BNKENTITY
                             MOVE TAG56-BIC         TO WK-C-VBAC-BANKID
                             MOVE TFSSTPL-CUYCD     TO WK-C-VBAC-CUYCD
                             CALL "TRFVBAC" USING WK-C-VBAC-RECORD
                             IF WK-C-VBAC-BNKACNO = TAG56-PTID
                                   AND TAG56-PTID  NOT = SPACES
                                   AND WK-C-VBAC-ERROR-CD = SPACES
                                   OR TAG56-PTID  = SPACES
                                   AND WK-C-VBAC-ERROR-CD = SPACES
                                MOVE "Y"          TO BKAC56-IND
                             ELSE
                                MOVE "N"          TO BKAC56-IND
                             END-IF
                          END-IF
       END-IF.
                       IF NOT(TAG57-OPT = SPACES
                             AND TAG57-PTID = SPACES
                             AND TAG57-BIC  = SPACES)
                             AND TAG57-OPT  = "A"
                          IF TAG57-PTID = SPACES
                             MOVE TAG57-BIC         TO WK-C-VBBAS-BANKID
                             CALL "TRFVBBAS" USING WK-C-VBBAS-RECORD
                             IF WK-C-VBBAS-SHIFTNO NOT = SPACES
                                MOVE "Y"          TO MEPS57-IND
           MOVE WK-C-VBBAS-SHIFTNO TO WK-VTB3-SHIFTNO
                             ELSE
                                MOVE "N"          TO MEPS57-IND
                             END-IF
                          END-IF

                       END-IF
                       IF  TAG57-PTID  NOT = SPACES
                             OR  MEPS57-IND  = "N"
                          MOVE "N"  TO MEPS57-IND
                          MOVE TFSSPTPL-BNKENTTY  TO WK-C-VBAC-BNKENTTY
                          MOVE TAG57-BIC  TO WK-C-VBAC-BANKID
                          MOVE TFSSPTPL-CUYCD  TO WK-C-VBAC-CUYCD
                          CALL "TFRVBAC" USING WK-C-VBAC-RECORD
                          IF  WK-C-VBAC-BNKACNO  = TAG57-PTID
                                AND TAG57-PTID  NOT = SPACES
                                AND WK-C-VBAC-ERROR-CD  = SPACES
                                OR  TAG57-PTID  = SPACES
                                AND WK-C-VBAC-ERROR-CD  = SPACES
                             MOVE "Y"  TO BKAC57-IND
                          ELSE
                             MOVE "N"  TO BKAC57-IND
                          END-IF
       END-IF.

       D499-MEPS-VALIDATION-EX.
       EXIT.

5Q1ARV                    D500-PROCESS-RPRRSN SECTION.
5Q1ARV                    D500-ENTRY.
                          5Q1ARV
5Q1ARV                 MOVE WK-VTB3-PARALNO  TO WK-C-RRSN-QUENUM.
5Q1ARV                 MOVE WK-VTB3-SEQNUM  TO WK-C-RRSN-QUESUF.
5Q1ARV                 MOVE WK-C-TRNNO  TO WK-C-RRSN-TRNNO.
5Q1ARV                 MOVE WK-C-FUNCTID  TO WK-C-RRSN-FUNCTID.
5Q1ARV                 MOVE WK-C-SEGCDE  TO WK-C-RRSN-SEGCDE.
5Q1ARV                 MOVE SPACES  TO WK-C-RRSN-SEGDESC.
5Q1ARV                 MOVE WK-N-STAFFIND  TO WK-C-RRSN-STAFFIND.
5Q1ARV                 MOVE WS-ACCNO  TO WK-C-RRSN-ACCNO.
5Q1ARV                 MOVE WK-C-QRATE  TO WK-C-RRSN-QRATE.
5Q1ARV                 MOVE WK-N-SYSDTE  TO WK-C-RRSN-RPRDTE.
5Q1JE1*5Q1ARV          MOVE WK-C-RPRCODE  TO WK-C-RRSN-RSNCDE.
5Q1JE1                 IF  WK-C-RPRCODE  = SPACE
5Q1JE1                    MOVE "RSN9999"  TO WK-C-RRSN-RSNCDE
5Q1JE1                 ELSE
5Q1JE1                    MOVE WK-C-RPRCODE  TO WK-C-RRSN-RSNCDE
5Q1JE1                 END-IF.
5Q1JE1 
5Q1ARV                 MOVE SPACES  TO WK-C-RRSN-RSNDESC.
5Q1ARV                 MOVE WK-C-RPRPGM  TO WK-C-RRSN-RPRPGM.
5Q1ARV                 CALL "TRFRGRRSN" USING WK-C-RRSN-RECORD.
5Q1ARV 
5Q1ARV                    D599-PROCESS-RPRRSN-EX.
5Q1ARV                 EXIT.
                          EJECT

       Z000-END-PROGRAM.
                       CLOSE  TFSSPTPL
                              TFSBNKET
                              TFSCLSYS.
                       EXIT PROGRAM.
       
