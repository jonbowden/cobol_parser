       IDENTIFICATION DIVISION.
      ***********************
       PROGRAM-ID. TRFVDT2.
       AUTHOR. TYK.
       DATE-WRITTEN. JUN 04.
      *DESCRIPTION : TABLE D2 VALIDATION.
      *             SUBROUTINE - COUNTRY & BANK RISK TABLE BASED ON
      *             SENDING BANK ID (INCOMING SWIFT FCY)
      *             THIS ROUTINE IS INITIATED BY TRFVDT1 PGM
      *______________________________________________________________________
      * HISTORY OF MODIFICATION:
      *======================================================================
      * 7Q1EM1  20/10/2016 TMPPYM  - REM Q1 2017 RELEASE
      *                          - e-Req 47511 Refinement of
      *                            Duplicate checking for Inw
      *                          - Recompiled due to changes made
      *                            in VSTPL copy book.
      *---------------------------------------------------------------------*
      * 5Q1JE2  12/11/2014 TMPJAE  - 14HOREM024/14HOREM029/14HOREM028
      *                          - PQR-2578 NON-STP REASON PRJ
      *                            Add reason for Country Code has
      *                            Risk Ind=Y
      *---------------------------------------------------------------------*
      * 5Q1ARV  12/11/2014 TMPARV  - 14HOREM024/14HOREM029/14HOREM028
      * 5Q1JE1                      NON PSTP REASON ENHANCEMENT
      * 5Q1LN1                      Modified the program to add the
      *                            new function F7=Repair Reason
      *======================================================================

       ENVIRONMENT DIVISION.
      *********************
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

       SELECT TFSCNTRY ASSIGN TO DATABASE-TFSCNTRY
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
       RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.

       SELECT TFSBANK ASSIGN TO DATABASE-TFSBANK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
       RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.

       5Q1ARV SELECT TFSCLSYS ASSIGN TO DATABASE-TFSCLSYS
               5Q1ARV ORGANIZATION IS SEQUENTIAL
       5Q1ARV FILE STATUS IS WK-C-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
      ***************
       FD TFSSTPL
       LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFSSTPL.
       01 WK-C-TFSSTPL.
               COPY DDS-ALL-FORMATS OF TFSSTPL.
       01 WK-C-TFSSTPL-1.
               COPY TFSSTPL.

       FD TFSCNTRY
       LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFSCNTRY.
       01 WK-C-TFSCNTRY.
               COPY DDS-ALL-FORMATS OF TFSCNTRY.
       01 WK-C-TFSCNTRY-1.
               COPY TFSCNTRY.

       FD TFSBANK
       LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFSBANK.
       01 WK-C-TFSBANK.
               COPY DDS-ALL-FORMATS OF TFSBANK.
       01 WK-C-TFSBANK-1.
               COPY TFSBANK.

       5Q1ARV FD TFSCLSYS
       5Q1ARV LABEL RECORDS ARE OMITTED
       5Q1ARV DATA RECORD IS TFSCLSYS-REC.
               5Q1ARV 01 TFSCLSYS-REC.
               5Q1ARV    COPY DDS-ALL-FORMATS OF TFSCLSYS.
               5Q1ARV 01 TFSCLSYS-REC-1.

               5Q1ARV    COPY TFSCLSYS.

       WORKING-STORAGE SECTION.
      ***********************
       01  WK-C-COMMON.
               COPY ASCMWS.

       01  TABLE-ARRAY.
               05  TAB-VAL OCCURS 20 TIMES PIC X VALUE "X".

       01  TABLE-ARR2.
               05  TAB-VL2 OCCURS 20 TIMES PIC X VALUE "X".

       01  PATH-P1                  PIC X(20)
               VALUE "YNYXXXXXXXXXXXXXXXXXXX".
       01  PATH-P2                  PIC X(20)
               VALUE "YNNXXXXXXXXXXXXXXXXXXX".
       01  PATH-P3                  PIC X(20)
               VALUE "YXXXOXXXXXXXXXXXXXXXXX".
       01  PATH-P4                  PIC X(20)
               VALUE "YNYXXXXXXXXXXXXXXXXXXX".

       01  WK-C-SENBNKID.
               05  WK-C-SENBNKID-1      PIC X(04).
               05  WK-C-SENBNKID-2      PIC X(07).

       01  WK-C-WORK-AREA.
               05  FIRST-TIME           PIC X(01) VALUE "Y".
               05  WS-OKAY              PIC X(01) VALUE SPACE.
               05  WS-FLAG1             PIC X(01) VALUE SPACE.
               05  WS-FLAG2             PIC X(01) VALUE SPACE.
               05  WS-FOUND             PIC X(01) VALUE SPACE.
               05  WS-RISKIND           PIC X(01) VALUE SPACE.
       05  WS-CNTRYCD           PIC X(02) VALUE SPACE.

5Q1ARV    01  WK-C-RPRRSN-AREA.
5Q1ARV        05  WK-C-SEGCODE       PIC X(01) VALUE SPACE.
5Q1ARV        05  WK-N-STAFFIND      PIC S9(02) VALUE ZEROS.
5Q1ARV        05  WK-C-ACCNO         PIC X(11) VALUE SPACE.
5Q1ARV        05  WK-C-ACCNAME       PIC X(35) VALUE SPACE.
5Q1ARV        05  WK-C-QRATE         PIC X(02) VALUE SPACE.
5Q1ARV        05  WK-C-RPRCODE       PIC X(07) VALUE SPACE.
5Q1ARV        05  WK-C-TRNNO         PIC X(12) VALUE SPACE.
5Q1ARV        05  WK-C-FUNCTID       PIC X(08) VALUE SPACE.
5Q1ARV    01  WK-N-SYSPTE            PIC S9(08) VALUE ZEROS.
5Q1ARV    01  WK-C-RPRPGM            PIC X(10) VALUE "TRFVTID2".

5Q1ARV    COPY RRSN.
          COPY VSTPL.
          COPY ACNT.
          COPY BLKB.
          COPY EXCB.
          COPY OECD.
          COPY LOGG.

       LINKAGE SECTION.
      ****************
          COPY VTD2.

       PROCEDURE DIVISION USING WK-VTD2.
      ********************************
       MAIN-MODULE.
           INITIALIZE WK-VTD2-OUTPUT
               WK-LOGG
       WK-C-WORK-AREA.
           MOVE ALL "X" TO TABLE-ARRAY.
           MOVE ALL "X" TO TABLE-ARR2.
           MOVE "Y" TO FIRST-TIME.

5Q1ARV    MOVE ZEROS    TO WK-C-RRSN-QUENUM
5Q1ARV                    WK-C-RRSN-QUESUF
5Q1ARV                    WK-C-RRSN-STAFFIND
5Q1ARV                    WK-C-RRSN-SEQNUM
5Q1ARV                    WK-C-RRSN-RPRDTE.

           IF FIRST-TIME = "Y"
               OPEN    INPUT TFSSTPL
               IF NOT WK-C-SUCCESSFUL
       AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVTD2 - OPEN FILE ERROR - TFSSTPL"
       DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                   GO TO 2000-END-PROGRAM
               END-IF
               OPEN    INPUT TFSCNTRY
               IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVTD2 - OPEN FILE ERROR - TFSCNTRY"
       DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                   GO TO 2000-END-PROGRAM
               END-IF
               OPEN    INPUT TFSBANK
               IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVTD2 - OPEN FILE ERROR - TFSBANK"
       DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                   GO TO 2000-END-PROGRAM
               END-IF
5Q1ARV        OPEN    INPUT TFSCLSYS
5Q1ARV        IF NOT WK-C-SUCCESSFUL
5Q1ARV            DISPLAY "TRFVTD2 - OPEN FILE ERROR - TFSCLSYS"
5Q1ARV            DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
5Q1ARV            GO TO 2000-END-PROGRAM
5Q1ARV        END-IF
       END-IF.

               MOVE WK-VTD2-PARALNO    TO TFSSTPL-PARALNO.
               MOVE WK-VTD2-SEQNUM     TO TFSSTPL-SEQNUM.

               READ TFSSTPL
                   KEY IS EXTERNALLY-DESCRIBED-KEY.

               IF WK-C-SUCCESSFUL
                   MOVE "N"            TO WS-OKAY
                   MOVE TFSSTPL-SENBNKID TO WK-C-SENBNKID
                   PERFORM A100-INITIAL-SUBROUTINE
                       THRU A199-INITIAL-SUBROUTINE-EX
                   PERFORM B100-PATH-CHOICE THRU B199-PATH-CHOICE-EX
       END-IF.
                   GO TO Z000-END-PROGRAM.

      *-------------------------------------------------------------------------*
      *  GET SYSTEM VALUES                                                      *
      *-------------------------------------------------------------------------*
       A100-INITIAL-SUBROUTINE.

                   MOVE WK-C-SENBNKID TO TFSBANK-BANKID.
                   READ TFSBANK KEY IS EXTERNALLY-DESCRIBED-KEY
                       INVALID KEY
       MOVE SPACES TO WS-CNTRYCD
                       NOT INVALID KEY
       MOVE TFSBANK-CNTRYCD TO WS-CNTRYCD
       END-READ.
       MOVE WS-CNTRYCD TO TFSCNTRY-CNTRYCD.
                   READ TFSCNTRY KEY IS EXTERNALLY-DESCRIBED-KEY
                       INVALID KEY
                   MOVE "N" TO WS-FOUND
                   MOVE SPACE TO WS-RISKIND
                       NOT INVALID KEY
                   MOVE "Y" TO WS-FOUND
                   MOVE TFSCNTRY-RISKIND TO WS-RISKIND
       END-READ.
5Q1ARV   READ TFSCLSYS.
5Q1ARV   IF NOT WK-C-SUCCESSFUL
5Q1ARV       DISPLAY "TRFVTID2 - READ TFSCLSYS ERROR"
5Q1ARV       DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
                       5Q1ARV       GO TO Z000-END-PROGRAM.
5Q1ARV   MOVE TFSCLSYS-SYSDTE TO WK-N-SYSDTE.

       A199-INITIAL-SUBROUTINE-EX.
       EXIT.

       B100-PATH-CHOICE.
                   MOVE "Y" TO WS-FLAG1.
      *-------------------------------------------------------------------------*
      *  C1 - OECD TOP 200 BANK TABLE                                           *
      *-------------------------------------------------------------------------*
       MOVE WK-C-SENBNKID-1 TO WK-OECD-BKCD.
       CALL "TRFOECD" USING WK-OECD.
       IF WK-OECD-INDIC = "Y"
                       PERFORM C100-VALIDATION THRU C199-VALIDATION-EX
       END-IF.
      *-------------------------------------------------------------------------*

      * C2 - BLACKLISTED BANK TABLE                                      *
      *-----------------------------------------------------------------*
                       MOVE WK-C-SENBNKID     TO WK-BLKB-BKID.
                       CALL "TRFBLKB"         USING WK-BLKB.
       SM0TY1****IF WK-OECD-INDIC NOT = "Y"
                       IF WK-BLKB-INDIC       = "Y"
                         PERFORM C200-VALIDATION THRU C299-VALIDATION-EX
       END-IF.
      *-----------------------------------------------------------------*
      * C3 - EXCEPTIONAL BANK TABLE                                     *
      *-----------------------------------------------------------------*
                           MOVE WK-C-SENBNKID     TO WK-EXCB-BKID.
                           CALL "TRFEXCB"         USING WK-EXCB.
SM0TY1****IF WK-OECD-INDIC NOT = "Y"
SM0TY1****AND WK-BLKB-INDIC NOT = "Y"
SM0TY1****AND WK-EXCB-INDIC  = "Y"
SM0TY1    IF WK-EXCB-INDIC   = "Y"
SM0TY1    AND WK-BLKB-INDIC NOT = "Y"
              PERFORM C300-VALIDATION THRU C399-VALIDATION-EX
       END-IF.
      *-----------------------------------------------------------------*
      * C4 - OTHER BANKS                                                *
      *-----------------------------------------------------------------*
                           MOVE WK-C-SENBNKID     TO WK-EXCB-BKID.
                           CALL "TRFEXCB"         USING WK-EXCB.
       IF NOT(WK-OECD-INDIC = "Y"
       OR  WK-BLKB-INDIC = "Y"
       OR  WK-EXCB-INDIC = "Y")
           PERFORM C400-VALIDATION THRU C499-VALIDATION-EX
       END-IF.

       PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.
       PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.
       EXIT.

       C100-VALIDATION.
               IF WS-FOUND NOT = "Y"
                  MOVE "N" TO TAB-VL2(01)
               ELSE
                  MOVE "Y" TO TAB-VL2(01)
                  IF WS-RISKIND = "Y"
                     MOVE "Y" TO TAB-VL2(02)
5Q1JE2               INITIALIZE WK-C-RPRRSN-AREA
5Q1JE2               MOVE "RSN0094" TO WK-C-RPRCODE
5Q1JE2               PERFORM D400-PROCESS-RPRRSN
5Q1JE2                 THRU D499-PROCESS-RPRRSN-EX
                 ELSE
                      MOVE "N" TO TAB-VL2(02)
                      MOVE "Y" TO WS-OKAY
                      IF WS-FOUND = "Y"
                         MOVE WS-CNTRYCD TO WK-ACNT-CCTY
                         CALL "TRFACNT" USING WK-ACNT
                         MOVE WK-ACNT-INDIC TO TAB-VL2(03)
                         IF  WK-ACNT-INDIC = "Y"
                             MOVE PATH-P1 TO TABLE-ARR2
                         ELSE
                             MOVE PATH-P2 TO TABLE-ARR2
                         END-IF
                      END-IF
                 END-IF.
       C199-VALIDATION-EX.
            EXIT.
            EJECT

       C200-VALIDATION.
            MOVE "N"     TO WS-OKAY.
            MOVE ALL "X" TO TABLE-ARR2.
5Q1JE1      INITIALIZE WK-C-RPRRSN-AREA.
5Q1JE1      MOVE "RSN0098" TO WK-C-RPRCODE.
5Q1JE1      PERFORM D400-PROCESS-RPRRSN
5Q1JE1          THRU D499-PROCESS-RPRRSN-EX.
       C299-VALIDATION-EX.
            EXIT.
            EJECT
       C300-VALIDATION.
            IF  WS-FOUND NOT = "Y"
                MOVE "N" TO TAB-VL2(01)
            ELSE
                MOVE "Y" TO TAB-VL2(01)
                            WS-OKAY
                MOVE PATH-P3 TO TABLE-ARR2
            END-IF.
       C399-VALIDATION-EX.
            EXIT.
            EJECT

       C400-VALIDATION.
            IF  WS-FOUND NOT = "Y"
                MOVE "N" TO TAB-VL2(01)
            ELSE
                MOVE "Y" TO TAB-VL2(01)
                IF  WS-RISKIND = "Y"
                    MOVE "Y" TO TAB-VL2(02)
5Q1JE2              INITIALIZE WK-C-RPRRSN-AREA
5Q1JE2              MOVE "RSN0094" TO WK-C-RPRCODE
5Q1JE2              PERFORM D400-PROCESS-RPRRSN
5Q1JE2                THRU D499-PROCESS-RPRRSN-EX
                ELSE
                    MOVE "N" TO TAB-VL2(02)
                    IF  WS-FOUND = "Y"
                        MOVE WS-CNTRYCD      TO WK-ACNT-CCTY
                        CALL "TRFACNT" USING WK-ACNT
                        MOVE WK-ACNT-INDIC TO TAB-VL2(03)
                                              WS-OKAY

                        IF  WK-ACNT-INDIC  = "Y"
                            MOVE PATH-P4   TO TABLE-ARR2
                            MOVE "Y"       TO WS-OKAY
                        ELSE
                            MOVE "N"       TO WS-OKAY
5Q1ARV                      INITIALIZE WK-C-RPRRSN-AREA
5Q1LN1                      MOVE "RSN0119" TO WK-C-RPRCODE
5Q1ARV                      PERFORM D400-PROCESS-RPRRSN
5Q1ARV                         THRU D499-PROCESS-RPRRSN-EX
                        END-IF
                     END-IF
            END-IF.
       C499-VALIDATION-EX.
            EXIT.
            EJECT

       D100-VALIDATION.
            MOVE TABLE-ARR2   TO TABLE-ARRAY.
            IF TABLE-ARRAY = ALL "X"
               MOVE "N"       TO WS-OKAY
SQ1JE1         INITIALIZE WK-C-RPRRSN-AREA
5Q1JE1         MOVE "RSN0115" TO WK-C-RPRCODE
5Q1JE1         PERFORM D400-PROCESS-RPRRSN
5Q1JE1           THRU D499-PROCESS-RPRRSN-EX
             END-IF.
             IF TAB-VL2(01) NOT = "X"
                MOVE TAB-VL2(01) TO TAB-VAL(01)
                PERFORM D300-LOGGING THRU D399-LOGGING-EX
             END-IF.
             IF TAB-VL2(02) NOT = "X"
                MOVE TAB-VL2(02) TO TAB-VAL(02)
                PERFORM D300-LOGGING THRU D399-LOGGING-EX
             END-IF.
             IF TAB-VL2(03) NOT = "X"
                MOVE TAB-VL2(03) TO TAB-VAL(03)
                PERFORM D300-LOGGING THRU D399-LOGGING-EX
             END-IF.
       D199-VALIDATION-EX.
             EXIT.
             EJECT

       D200-VALIDATION.
             MOVE TABLE-ARRAY  TO WK-VTD2-DATAD2
             MOVE WS-OKAY      TO WK-VTD2-NO-ERROR
             MOVE "N"          TO WS-FLAG1.
             PERFORM D300-LOGGING THRU D399-LOGGING-EX.

       D299-VALIDATION-EX.
            EXIT.
            EJECT

       D300-LOGGING.
            MOVE WK-VTD2-PARALNO TO WK-LOGG-PARALNO.
            MOVE WK-VTD2-SEQNUM        TO WK-LOGG-SEQNUM.
            MOVE TABLE-ARRAY           TO WK-LOGG-DATAD2.
            MOVE "D2"                  TO WK-LOGG-TABTYP.
            CALL "TRFLOGGCL" USING WK-LOGG
                                   WS-FLAG1
                                   WS-FLAG2.
            IF WK-LOGG-ERROR-FOUND = "Y"
               GO TO D399-LOGGING-EX
            END-IF.
       D399-LOGGING-EX.
            EXIT.

5Q1ARV D400-PROCESS-RPRRSN SECTION.
5Q1ARV D400-ENTRY.
5Q1ARV
5Q1ARV      MOVE WK-VTD2-PARALNO   TO WK-C-RRSN-QUENUM.
5Q1ARV      MOVE WK-VTD2-SEQNUM    TO WK-C-RRSN-QUESUF.
5Q1ARV      MOVE WK-C-TRNNO        TO WK-C-RRSN-TRNNO.
5Q1ARV      MOVE WK-C-FUNCTID      TO WK-C-RRSN-FUNCTID.
5Q1ARV      MOVE WK-C-SEGCDE       TO WK-C-RRSN-SEGCDE.
5Q1ARV      MOVE SPACES            TO WK-C-RRSN-SEGDESC.
5Q1ARV      MOVE WK-N-STAFFIND     TO WK-C-RRSN-STAFFIND.
5Q1ARV      MOVE WK-C-ACCNO        TO WK-C-RRSN-ACCNO.
5Q1ARV      MOVE WK-C-QRATE        TO WK-C-RRSN-QRATE.
5Q1ARV      MOVE WK-N-SYSDTE       TO WK-C-RRSN-RPRDTE.
5Q1JE1*5Q1ARV  MOVE WK-C-RPRCODE   TO WK-C-RRSN-RSNCDE.
5Q1JE1      IF WK-C-RPRCODE = SPACE
5Q1JE1          MOVE "RSN9999"     TO WK-C-RRSN-RSNCDE
5Q1JE1      ELSE
5Q1JE1          MOVE WK-C-RPRCODE  TO WK-C-RRSN-RSNCDE
5Q1JE1      END-IF.
5Q1JE1
5Q1ARV      MOVE SPACES            TO WK-C-RRSN-RSNDESC.
5Q1ARV      MOVE WK-C-RPRPGM       TO WK-C-RRSN-RPRPGM.
5Q1ARV      CALL "TRFGRRSN" USING WK-C-RRSN-RECORD.
5Q1ARV
5Q1ARV D499-PROCESS-RPRRSN-EX.
5Q1ARV      EXIT.
            EJECT
       Z000-END-PROGRAM.
            CLOSE   TFSSTPL
                    TFSCNTRY
5Q1ARV              TFSCLSYS
                    TFSBANK.
            EXIT PROGRAM.
