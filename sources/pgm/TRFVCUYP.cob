       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRFVCUYP.
       AUTHOR. ACCENTURE.
       DATE-WRITTEN. 23 JUN 2020.
      *----------------------------------------------------------------*
      *DESCRIPTION : THIS PROGRAM WILL SERVE AS A COMMON MODULE TO    *
      *               VALIDATE ELIGIBLE OFX CURRENCY PAIR             *
      *----------------------------------------------------------------*
      * HISTORY OF MODIFICATION:                                      *
      *----------------------------------------------------------------*
      * MOD.#  INIT   DATE        DESCRIPTION                         *
      * ------ ------ ----------  ----------------------------------- *
      * OFX2A2 VENL29 16/10/2020 - PROJ#BW24 -GEBNG ONLINE FX R2      *
      *                          GEBNEXTGEN-85089                    *
      *                          - Add variable initialization and   *
      *                            correct pogram name in the        *
      *                            display message                   *
      * OFX2A1 ACNFAM 24/06/2020 - PROJ#BW24 -GEBNG ONLINE FX R2      *
      *                            INITIAL VERSION                   *
      *----------------------------------------------------------------*
              EJECT
      ********************
       ENVIRONMENT DIVISION.
      ********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. LOCAL-DATA IS LOCAL-DATA-AREA
              I-O-FEEDBACK IS I-O-FEEDBACK-AREA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT TLSMSCUYP1 ASSIGN TO DATABASE-TLSMSCUYP1
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
      ***************
       DATA DIVISION.
      ***************
       FILE SECTION.
      ***************
       FD TLSMSCUYP1
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TLSMSCUYP1-REC.
       01 TLSMSCUYP1-REC.
              COPY DDS-ALL-FORMATS OF TLSMSCUYP1.
OFX2A2        01 TLSMSCUYP1-REC-1.
OFX2A2        COPY TFSMSCUYP.
       WORKING-STORAGE SECTION.

      ************************
       01 FILLER               PIC X(24) VALUE
              "** PROGRAM TRVFCUYP1 **".

      * ---------------- PROGRAM WORKING STORAGE -----------------*
       01 WK-C-COMMON.
              COPY ASCMWS.

       01 WK-C-WORK-AREA.
           05 WK-C-FOUND        PIC X(01) VALUE "Y".
           05 WK-C-NOT-FOUND    PIC X(01) VALUE "N".

       01 WK-C-LITERALS.
           05 C-COM0206        PIC X(07) VALUE "COM0206".
           05 C-FILE           PIC X(07) VALUE "TRVFCUYP".
           05 C-MODE-R         PIC X(07) VALUE "READ".

      *------------------- LOCAL DATA AREA -----------------------*
              COPY TRFLDA.

      ********************
       LINKAGE SECTION.
      ********************
              COPY CUYP.

      ****************************************
       PROCEDURE DIVISION USING WK-C-CUYP-RECORD.
      ****************************************
       MAIN-MODULE.
           PERFORM A000-START-PROGRAM-ROUTINE
              THRU A999-START-PROGRAM-ROUTINE-EX.
           PERFORM B000-MAIN-PROCESSING
              THRU B999-MAIN-PROCESSING-EX.
           PERFORM Z000-END-PROGRAM-ROUTINE
              THRU Z999-END-PROGRAM-ROUTINE-EX.
       GOBACK.

      *----------------------------------------------------------------*
       A000-START-PROGRAM-ROUTINE.
      *----------------------------------------------------------------*
           OPEN INPUT TLSMSCUYP1.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRVFCUYP - OPEN FILE ERROR - TLSMSCUYP1"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              PERFORM Y900-ABNORMAL-TERMINATION
       END-IF.

      *================================================================*
       A999-START-PROGRAM-ROUTINE-EX.
      *================================================================*
       EXIT.

      *----------------------------------------------------------------*
       B000-MAIN-PROCESSING.

      *----------------------------------------------------------------*
           INITIALIZE WK-C-CUYP-OUTPUT
              TLSMSCUYP1-REC
       TLSMSCUYP1-REC-1.
           MOVE WK-C-NOT-FOUND     TO WK-C-CUYP-FOUND
           MOVE WK-C-CUYP-MAJOR    TO CUYMAJOR OF TLSMSCUYP1-REC
           MOVE WK-C-CUYP-MINOR    TO CUYMINOR OF TLSMSCUYP1-REC
           READ TLSMSCUYP1 KEY IS EXTERNALLY-DESCRIBED-KEY
           IF WK-C-SUCCESSFUL
              MOVE WK-C-FOUND     TO WK-C-CUYP-FOUND
              MOVE TFSMSCUYP-RATEDCMAL TO WK-C-CUYP-RATEDCMAL
              MOVE TFSMSCUYP-ASSETX TO WK-C-CUYP-ASSETX
              MOVE TFSMSCUYP-PNTSHIFT TO WK-C-CUYP-PNTSHIFT
              MOVE TFSMSCUYP-SPTFACTOR TO WK-C-CUYP-SPTFACTOR
       END-IF.
           IF WK-C-RECORD-NOT-FOUND
              INITIALIZE TLSMSCUYP1-REC
              MOVE WK-C-CUYP-MINOR TO CUYMAJOR OF TLSMSCUYP1-REC
              MOVE WK-C-CUYP-MAJOR TO CUYMINOR OF TLSMSCUYP1-REC
              READ TLSMSCUYP1 KEY IS EXTERNALLY-DESCRIBED-KEY
              IF WK-C-SUCCESSFUL
                 MOVE WK-C-FOUND TO WK-C-CUYP-FOUND
                 MOVE TFSMSCUYP-RATEDCMAL TO WK-C-CUYP-RATEDCMAL
                 MOVE TFSMSCUYP-ASSETX TO WK-C-CUYP-ASSETX
                 MOVE TFSMSCUYP-PNTSHIFT TO WK-C-CUYP-PNTSHIFT
                 MOVE TFSMSCUYP-SPTFACTOR TO WK-C-CUYP-SPTFACTOR
              END-IF
              IF WK-C-RECORD-NOT-FOUND
                 MOVE WK-C-NOT-FOUND TO WK-C-CUYP-FOUND
              ELSE
                 IF NOT WK-C-SUCCESSFUL
                    DISPLAY "TRFVCUYP - TLSMSCUYP1 READ ERROR"
                    DISPLAY "TRFVCUYP1 - TLSMSCUYP1 READ ERROR"
                    MOVE WK-C-NOT-FOUND TO WK-C-CUYP-FOUND
                    MOVE C-COM0206 TO WK-C-CUYP-ERROR-CD
                    MOVE C-FILE TO WK-C-CUYP-FILE
                    MOVE C-MODE-R TO WK-C-CUYP-MODE
                    STRING WK-C-CUYP-MAJOR WK-C-CUYP-MINOR
                       DELIMITED BY SPACES INTO WK-C-CUYP-KEY
                    MOVE WK-C-FILE-STATUS TO WK-C-CUYP-FS
                    PERFORM Y900-ABNORMAL-TERMINATION
                    END-IF
                 END-IF
              ELSE
              IF NOT WK-C-SUCCESSFUL
                 DISPLAY "TRFVCUYP - TLSMSCUYP1 READ ERROR"
                 MOVE WK-C-NOT-FOUND TO WK-C-CUYP-FOUND
                 MOVE C-COM0206 TO WK-C-CUYP-ERROR-CD
                 MOVE C-FILE TO WK-C-CUYP-FILE
                 MOVE C-MODE-R TO WK-C-CUYP-MODE
                 STRING WK-C-CUYP-MAJOR WK-C-CUYP-MINOR
                    DELIMITED BY SPACES INTO WK-C-CUYP-KEY
                 MOVE WK-C-FILE-STATUS TO WK-C-CUYP-FS
                 PERFORM Y900-ABNORMAL-TERMINATION
                 END-IF

              END-IF
       END-IF.
      *=================================================================
      *
       B999-MAIN-PROCESSING-EX.
      *=================================================================
      *
       EXIT.
      *-----------------------------------------------------------------
      *
       Y900-ABNORMAL-TERMINATION.
      *-----------------------------------------------------------------
      *
           PERFORM Z000-END-PROGRAM-ROUTINE.
           EXIT PROGRAM.
      *-----------------------------------------------------------------
      *
       Z000-END-PROGRAM-ROUTINE.
      *-----------------------------------------------------------------
      *
           CLOSE TLSMSCUVP1.
           IF     NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVCUYP - CLOSE FILE ERROR - TLSMSCUVP1"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.
      *=================================================================
      *
       Z999-END-PROGRAM-ROUTINE-EX.
      *=================================================================
      *
       EXIT.
