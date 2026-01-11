       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRFVGLAC.
       AUTHOR. ACCENTURE.
       DATE-WRITTEN. 24 JAN 2019.
      *=================================================================
      *
      *PROGRAM DESCRIPTION: Validate GL Accno and retrieve payment mode
      *
      * This program will check/retrieve GL Accno or*
      * Payment Mode based on option provided.                         *
      *                                                               *
      * OPTION ACTION................ INPUT..............             *
      * 1 Validate GL Accno only GL Accno                             *
      * 2 Validate GL Accno and GL Accno details,                     *
      * Payment Mode Payment Mode                                    *
      * 3 Retrieve GL Accno GL Accno details,                         *
      * details and Payment Payment Mode                             *
      * mode                                                         *
      *                                                               *
      *=================================================================
      *
      * HISTORY OF MODIFICATION:                                       *
      *=================================================================
      *
      *MOD.# INIT DATE DESCRIPTION                                    *
      *------ ------ ---------- --------------------------------------*
      *SGX201 - ACNESQ - 24/01/2019 - SGX PTS2 PHASE 2                *
      * - Initial Version.                                           *
      *=================================================================
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. LOCAL-DATA IS LOCAL-DATA-AREA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT UFMGLPAY ASSIGN TO DATABASE-UFMGLPAY
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TFSGLPYM ASSIGN TO DATABASE-TFSGLPYM
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  UFMGLPAY
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS UFMGLPAY-REC.

       01  UFMGLPAY-REC.
              COPY DDS-ALL-FORMATS OF UFMGLPAY.
       01  UFMGLPAY-REC-1.
              COPY UFMGLPAY.

       FD  TFSGLPYM
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSGLPYM-REC.
       01  TFSGLPYM-REC.
              COPY DDS-ALL-FORMATS OF TFSGLPYM.
       01  TFSGLPYM-REC-1.
              COPY TFSGLPYM.

       WORKING-STORAGE SECTION.
       01  FILLER                  PIC X(24) VALUE
              "** PROGRAM TRFVGLAC **".

       01  WK-C-VGLAC              PIC X(06) VALUE SPACES.

      * ------------------ PROGRAM WORKING STORAGE ------------------*
       01  WK-C-COMMON.
              COPY ASCMWS.
              COPY VCGL.
              COPY XGSPA.
      ****************
       LINKAGE SECTION.
      ****************
              COPY VGLAC.

              EJECT
      ****************************************
       PROCEDURE DIVISION USING WK-C-VGLAC-RECORD.
      ****************************************
       MAIN-MODULE.

           PERFORM A000-PROCESS-CALLED-ROUTINE
              THRU A099-PROCESS-CALLED-ROUTINE-EX.
           PERFORM B000-MAIN-PROCESSING
              THRU B999-MAIN-PROCESSING-EX.
           PERFORM Z000-END-PROGRAM-ROUTINE
              THRU Z999-END-PROGRAM-ROUTINE-EX.
           GO TO END-PROGRAM.

      *----------------------------------------------------------------*
       A000-PROCESS-CALLED-ROUTINE.
      *----------------------------------------------------------------*
           OPEN INPUT UFMGLPAY.
           IF NOT WK-C-SUCCESSFUL
              DISPLAY "TRFVGLAC - OPEN FILE-ERROR - UFMGLPAY"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO A099-PROCESS-CALLED-ROUTINE-EX
       END-IF.

           OPEN INPUT TFSGLPYM.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVGLAC - OPEN FILE ERROR - TFSGLPYM"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO A099-PROCESS-CALLED-ROUTINE-EX
       END-IF.

           INITIALIZE                WK-C-XGSPA-RECORD
       WK-C-VGLAC.
           MOVE "GLACTPMOD"         TO WK-C-XGSPA-GHAPARCD.
           CALL "TRFXGSPA"          USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD   = SPACES
              MOVE WK-C-XGSPA-GHPARVAL TO WK-C-VGLAC
       END-IF.

      *----------------------------------------------------------------*
       A099-PROCESS-CALLED-ROUTINE-EX.
      *----------------------------------------------------------------*
       EXIT.

      *----------------------------------------------------------------*
       B000-MAIN-PROCESSING.
      *----------------------------------------------------------------*
           INITIALIZE            UFMGLPAY-REC-1
       WK-C-VGLAC-OUTPUT.

           EVALUATE WK-N-VGLAC-OPTION
              WHEN 1
              PERFORM C100-CHECK-GLACNO
                 THRU C199-CHECK-GLACNO-EX
              WHEN 2
              PERFORM C200-CHECK-GLACNO-PAYMODE
                 THRU C299-CHECK-GLACNO-PAYMODE-EX
              WHEN 3
              PERFORM C400-GET-GLACNO-PAYMODE
                 THRU C499-GET-GLACNO-PAYMODE-EX
       END-EVALUATE.

      *--> Retrieve GL Accno Details
           IF WK-C-VGLAC-GLIND = "Y"
                 AND WK-N-VGLAC-GLNO6 IS NUMERIC
              PERFORM C300-GET-GLACNO-DETAILS
                 THRU C399-GET-GLACNO-DETAILS-EX
       END-IF.

       B999-MAIN-PROCESSING-EX.
       EXIT.

      *----------------------------------------------------------------*
       C100-CHECK-GLACNO.
      *----------------------------------------------------------------*
           INITIALIZE            WK-C-VCGL-RECORD
           MOVE WK-N-VGLAC-GLNO6 TO WK-N-VCGL-GLNO6.
           CALL "TRFVGLGL"       USING WK-C-VCGL-RECORD.

           IF  WK-C-VGL-ERROR-CD = SPACES
              MOVE "Y"           TO  WK-C-VGLAC-GLIND
       END-IF.
       C199-CHECK-GLACNO-EX.
       EXIT.

      *----------------------------------------------------------------*
      *C200-CHECK-GLACNO-PAYMODE.                                     *
      *----------------------------------------------------------------*
           PERFORM C100-CHECK-GLACNO
              THRU C199-CHECK-GLACNO-EX.
           IF  WK-C-VGLAC-GLIND = "Y"
              INITIALIZE          UFMGLPAY-REC
                 UFMGLPAY-REC-1
              MOVE WK-C-VGLAC-I-PMODE TO UFMGLPAY-PAYMODE
              READ UFMGLPAY
              IF  WK-C-SUCCESSFUL
                    AND UFMGLPAY-PAYMODE = WK-C-VGLAC
                 MOVE "Y"       TO  WK-C-VGLAC-PYIND
                 MOVE UFMGLPAY-PAYMODE TO WK-C-VGLAC-O-PMODE
              END-IF
       END-IF.
       C299-CHECK-GLACNO-PAYMODE-EX.
       EXIT.

      *----------------------------------------------------------------*
      *C300-GET-GLACNO-DETAILS.                                       *
      *----------------------------------------------------------------*
           INITIALIZE              TFSGLPYM-REC
              TFSGLPYM-REC-1
           MOVE WK-N-VGLAC-GLNO6  TO  TFSGLPYM-GLNO6
           READ TFSGLPYM
           IF  WK-C-SUCCESSFUL
              MOVE TFSGLPYM-FULNAME TO WK-C-VGLAC-FULNAME
              MOVE TFSGLPYM-ADDR1  TO  WK-C-VGLAC-ADDR1
              MOVE TFSGLPYM-ADDR2  TO  WK-C-VGLAC-ADDR2
              MOVE TFSGLPYM-ADDR3  TO  WK-C-VGLAC-ADDR3
              IF  TFSGLPYM-DOMBRCH IS NUMERIC
                 MOVE TFSGLPYM-DOMBRCH TO WK-N-VGLAC-DOMBRCH
              END-IF
              MOVE TFSGLPYM-COSTCTR TO WK-C-VGLAC-COSTCTR
       END-IF.
       C399-GET-GLACNO-DETAILS-EX.
       EXIT.

      *----------------------------------------------------------------*
      *C400-GET-GLACNO-PAYMODE.                                       *
      *----------------------------------------------------------------*
           MOVE WK-C-VGLAC        TO  WK-C-VGLAC-O-PMODE.
           IF  WK-C-VGLAC         NOT = SPACES
              MOVE "Y"           TO  WK-C-VGLAC-PYIND
       END-IF.

           PERFORM C100-CHECK-GLACNO

              THRU C199-CHECK-GLACNO-EX.
       C499-GET-GLACNO-PAYMODE-EX.
       EXIT.
      *----------------------------------------------------------------*
       Z000-END-PROGRAM-ROUTINE.
      *----------------------------------------------------------------*
           CLOSE  UFMGLPAY.
           IF NOT WK-C-SUCCESSFUL
              DISPLAY "TRFVGLAC - CLOSE FILE-ERROR - UFMGLPAY"
              DISPLAY "FILE-STATUS IS " WK-C-FILE-STATUS
       END-IF.
           CLOSE  TFSGLPYM.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVGLAC - CLOSE FILE ERROR - TFSGLPYM"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.
      *----------------------------------------------------------------*
       Z099-END-PROGRAM-ROUTINE-EX.
      *----------------------------------------------------------------*
       EXIT.
       END-PROGRAM.
           EXIT PROGRAM.
