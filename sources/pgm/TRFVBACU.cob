       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRFVBACU.
       AUTHOR. ACCENTURE.
       DATE-WRITTEN. 03 APR 2019.
      *=================================================================
      *
      *DESCRIPTION : THIS IS A CALLED ROUTINE TO CHECK BANK TABLE      *
      *              EXTENSION FILE                                    *
      *=================================================================
      *
      * HISTORY OF AMENDMENT :                                         *
      *=================================================================
      *
      * G2BL00 - ACNRJR  - 03/04/2019 - CASH MANAGEMENT ROAD MAP - P19 *
      *                     GPI Day4 (Retro from GPI Day2b HO)         *
      *                     - Initial Version.                         *
      *=================================================================
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. LOCAL-DATA IS LOCAL-DATA-AREA
              I-O-FEEDBACK IS I-O-FEEDBACK-AREA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT TFSBANKEXT ASSIGN TO DATABASE-TFSBANKEXT
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  TFSBANKEXT
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFSBANKEXT.
       01  WK-C-TFSBANKEXT.
              COPY DDS-ALL-FORMATS OF TFSBANKEXT.
       01  WK-C-TFSBANKEXT-1.
              COPY TFSBANKEXT.
       WORKING-STORAGE SECTION.
       01  FILLER                      PIC X(24) VALUE
              "** PROGRAM TRFVBACU **".

       01  WK-C-COMMON.
              COPY ASCMWS.
              COPY FIL3090.

       01  WS-C-FLAG.
           05  WS-C-REC-FOUND           PIC X(01).

      ****************
       LINKAGE SECTION.
      ****************
              COPY VBACU.

              EJECT
      ****************************************
       PROCEDURE DIVISION USING WK-C-VBACU-RECORD.
      ****************************************
       MAIN-MODULE.
           PERFORM A000-PROCESS-CALLED-ROUTINE
              THRU A099-PROCESS-CALLED-ROUTINE-EX.
           PERFORM Z000-END-PROGRAM-ROUTINE
              THRU Z999-END-PROGRAM-ROUTINE-EX.
       GOBACK.

      *-----------------------------------------------------------------
      *
       A000-PROCESS-CALLED-ROUTINE.
      *-----------------------------------------------------------------
      *
           OPEN INPUT TFSBANKEXT.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TFRVBACU - OPEN FILE ERROR - TFSBANKEXT"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.

           INITIALIZE               WK-C-VBACU-OUTPUT.
           MOVE ZEROES              TO WK-C-VBACU-FS.
           MOVE WK-C-VBACU-BANKID   TO TFSBANKEXT-BANKID.
           MOVE "N"                 TO WS-C-REC-FOUND.

           PERFORM B100-READ-TFSBANKEXT
              THRU B199-READ-TFSBANKEXT-EX.

           IF WS-C-REC-FOUND = "N"
              MOVE "SUP0016"       TO WK-C-VBACU-ERROR-CD
           ELSE
              MOVE TFSBANKEXT-UOBBRH   TO WK-C-VBACU-UOBBRH
              MOVE TFSBANKEXT-CNTRYINCO TO WK-C-VBACU-CNTRYINCO
              MOVE TFSBANKEXT-CNTRYCD  TO WK-C-VBACU-CNTRYCD
              MOVE TFSBANKEXT-LOCATNCD TO WK-C-VBACU-LOCATNCD
       END-IF.

       A099-PROCESS-CALLED-ROUTINE-EX.
       EXIT.
      *-----------------------------------------------------------------
      *
       B100-READ-TFSBANKEXT.

           READ  TFSBANKEXT KEY IS EXTERNALLY-DESCRIBED-KEY.
           IF  WK-C-SUCCESSFUL
              MOVE "Y"         TO  WS-C-REC-FOUND
           ELSE
              IF  WK-C-END-OF-FILE
                    OR  WK-C-RECORD-NOT-FOUND
                 MOVE "N"     TO  WS-C-REC-FOUND
              ELSE
       DISPLAY "TRFVBACU - READ FILE ERROR - TFSBANKEXT"
                 DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                 GO TO Y900-ABNORMAL-TERMINATION
              END-IF
       END-IF.
       B199-READ-TFSBANKEXT-EX.
       EXIT.
       Y900-ABNORMAL-TERMINATION.
           PERFORM Z000-END-PROGRAM-ROUTINE.
           EXIT PROGRAM.
       Z000-END-PROGRAM-ROUTINE.
           CLOSE TFSBANKEXT.
           IF  NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVBACU - CLOSE FILE ERROR - TFSBANKEXT"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.
       Z999-END-PROGRAM-ROUTINE-EX.
       EXIT.
