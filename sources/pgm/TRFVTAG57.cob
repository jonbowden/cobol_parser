       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRFVTAG57.
       DATE-WRITTEN. 26 OCT 2019.
       AUTHOR. ACCENTURE.
      *DESCRIPTION : SUBROUTINE - TAG 57 VALIDATION.
      *=================================================================
      *
      * HISTORY OF MODIFICATION:
      *
      *=================================================================
      *
      * TAG  DEV   DATE        DESCRIPTION
      *
      *-----------------------------------------------------------------
      *
      * GP3A00 - ACNDUS - 26/10/2019 - CASH MANAGEMENT ROAD MAP - P19  *
      *                   - GPI Day4 (Retro from GPI Day3 HO)          *
      *                   - PCRMAKGPI-927                              *
      *                   - Ensure validation will continue            *
      *                     if Tag 57 D line 1 is spaces               *
      *                   - Initial version.                           *
      *-----------------------------------------------------------------
      *

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. LOCAL-DATA IS LOCAL-DATA-AREA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT RLSGTAG57 ASSIGN TO DATABASE-RLSGTAG57
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
              WITH DUPLICATES
       FILE STATUS IS WK-C-FILE-STATUS.

              SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  RLSGTAG57
              LABEL RECORDS ARE OMITTED
       DATA RECORDS IS RLSGTAG57-REC.
       01  RLSGTAG57-REC.
              COPY DDS-ALL-FORMATS OF RLSGTAG57.

       01  RLSGTAG57-REC-1.
              COPY RFSGTAG57.

       FD  TFSBNKET
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSBNKET-REC.
       01  TFSBNKET-REC.
              COPY DDS-ALL-FORMATS OF TFSBNKET.
       01  TFSBNKET-REC-1.
              COPY TFSBNKET.

       WORKING-STORAGE SECTION.
      ***********************
       01  FILLER                  PIC X(24) VALUE
              "** PROGRAM TRFVTAG57 **".

       01  WK-C-WORK-AREA.
           05  WK-C-DFLT-57BIC    PIC X(11) VALUE SPACES.
           05  WK-C-TAG57-NOT-FOUND PIC X(01) VALUE SPACE.
           05  WK-C-VAL-SPACES    PIC X(01) VALUE SPACE.

       01  WK-N-WORK-AREA.
           05  WK-N-CTR           PIC 9(01) VALUE ZERO.
           05  WK-N-OCCURENCE     PIC 9(01) VALUE 5.

      * -------------------- PROGRAM WORKING STORAGE -------------------
      *    -*
       01  WK-C-COMMON.
              COPY ASCWWS.
              COPY XGSPA.

      ****************
       LINKAGE SECTION.
      ****************
              COPY VTAG57.

              EJECT
      ****************************************
       PROCEDURE DIVISION USING WK-C-VTAG57-RECORD.
      ****************************************
       MAIN-MODULE.
           PERFORM A000-PROCESS-CALLED-ROUTINE
              THRU A999-PROCESS-CALLED-ROUTINE-EX.
           PERFORM B000-MAIN-PROCESSING
              THRU B999-MAIN-PROCESSING-EX.
           PERFORM Z000-END-PROGRAM-ROUTINE
              THRU Z999-END-PROGRAM-ROUTINE-EX.
           EXIT PROGRAM.

      *-----------------------------------------------------------------
      *    -*
       A000-PROCESS-CALLED-ROUTINE.
      *-----------------------------------------------------------------
      *    -*
           OPEN INPUT RLSGTAG57.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVTAG57 - OPEN FILE ERROR - RLSGTAG57"

              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.
           OPEN INPUT TFSBNKET.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVTAG57 - OPEN FILE ERROR - TFSBNKET"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.
           INITIALIZE TFSBNKET-REC-1.
           MOVE WK-C-VTAG57-I-BNKENTTY TO TFSBNKET-BNKENTTY
           READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
              INVALID KEY
           MOVE SPACES TO WK-C-DFLT-57BIC
              NOT INVALID KEY
           MOVE TFSBNKET-SWFTBNK TO WK-C-DFLT-57BIC
       END-READ.
       A999-PROCESS-CALLED-ROUTINE-EX.
       EXIT.
      *----------------------------------------------------------------*
       B000-MAIN-PROCESSING.
      *----------------------------------------------------------------*
           INITIALIZE RLSGTAG57-REC-1
       WK-C-VTAG57-OUTPUT.
           MOVE ZERO TO WK-N-CTR.
           MOVE "N" TO WK-C-VAL-SPACES
       WK-C-TAG57-NOT-FOUND.
           IF WK-C-VTAG57-VALUE NOT = SPACES
              ADD 1 TO WK-N-CTR
              EVALUATE WK-C-VTAG57-OPTION
                 WHEN "C"
                 PERFORM B100-READ-RLSGTAG57
                    THRU B199-READ-RLSGTAG57-EX
                 WHEN "D"
                 PERFORM B100-READ-RLSGTAG57
                    THRU B199-READ-RLSGTAG57-EX
                    UNTIL WK-N-CTR > WK-N-OCCURENCE
                    OR WK-C-VAL-SPACES = "Y"
                    OR WK-C-TAG57-NOT-FOUND = "Y"
                 WHEN OTHER
                 CONTINUE
                 END-EVALUATE
              IF WK-C-VTAG57-VALID = "Y"
                 MOVE WK-C-DFLT-57BIC TO WK-C-VTAG57-BIC
              END-IF
       END-IF.
       B999-MAIN-PROCESSING-EX.

       EXIT.
      *----------------------------------------------------------------*
       B100-READ-RLSGTAG57.
      *----------------------------------------------------------------*
           IF  WK-C-VTAG57-INFO(WK-N-CTR) = SPACES
              IF  WK-C-VTAG57-OPTION = "C"
                    OR (WK-C-VTAG57-OPTION = "D"
                    AND WK-N-CTR > 1)
                 MOVE "Y"        TO  WK-C-VAL-SPACES
              END-IF
              ADD 1              TO  WK-N-CTR
              GO TO B199-READ-RLSGTAG57-EX
       END-IF.
           INITIALIZE             RLSGTAG57-REC-1.
           MOVE ZEROES            TO  RFSGTAG57-SEQNO.
           MOVE WK-C-VTAG57-INFO(WK-N-CTR) TO  RFSGTAG57-TAGVAL.
           READ RLSGTAG57 KEY IS EXTERNALLY-DESCRIBED-KEY
           IF  NOT WK-C-RECORD-NOT-FOUND
                 AND NOT WK-C-END-OF-FILE
                 AND NOT WK-C-SUCCESSFUL
              MOVE SPACES        TO  WK-C-VTAG57-VALID
       DISPLAY "TRFVTAG57 READ FILE ERROR - RLSGTAG57"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
           END-IF
           IF  WK-C-RECORD-NOT-FOUND
                 OR  WK-C-END-OF-FILE
              MOVE "N"           TO  WK-C-VTAG57-VALID
              MOVE "Y"           TO  WK-C-TAG57-NOT-FOUND
           ELSE
              IF  WK-C-SUCCESSFUL
                 MOVE "Y"           TO  WK-C-VTAG57-VALID
                 ADD 1              TO  WK-N-CTR
              END-IF
       END-IF.
       B199-READ-RLSGTAG57-EX.
       EXIT.
      *----------------------------------------------------------------*
       Y900-ABNORMAL-TERMINATION.
      *----------------------------------------------------------------*
           PERFORM Z000-END-PROGRAM-ROUTINE
              THRU Z999-END-PROGRAM-ROUTINE-EX.
           EXIT PROGRAM.
      *----------------------------------------------------------------*
       Z000-END-PROGRAM-ROUTINE.
      *----------------------------------------------------------------*
           CLOSE RLSGTAG57.

           IF  NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVTAG57 - CLOSE FILE ERROR - RLSGTAG57"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.

           CLOSE TFSBNKET.
           IF  NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVTAG57 - CLOSE FILE ERROR - TFSBNKET"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.

       Z999-END-PROGRAM-ROUTINE-EX.
       EXIT.
