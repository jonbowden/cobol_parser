       IDENTIFICATION DIVISION.
      ***********************
       PROGRAM-ID. TRFVLMIT.
       AUTHOR. ACCENTURE.
       DATE-WRITTEN. FEB 13 2017.
      *DESCRIPTION : THIS PROGRAM WILL CHECK STP LIMIT BY ACCOUNT
      *              CIF AND SEGMENT.
      *
      *    RETURN STATUS:
      *    XX - ACCOUNT NUMBER HAS NO DETAIL FOUND IN REM
      *    SPACES - AMOUNT CONVERSION TO SGD WAS FAILED
      *
      *    A1 - ACCOUNT, CIF AND SEGMENT LIMIT WERE NOT DEFINED
      *    A0 - TRANSACTION AMOUNT IS BELOW OR EQUAL TO STP LIMIT
      *    AA - TRANSACTION AMOUNT IS GREATER THAN ACN STP LIMIT
      *    AC - TRANSACTION AMOUNT IS GREATER THAN CIF STP LIMIT
      *    AS - TRANSACTION AMOUNT IS GREATER THAN SGM STP LIMIT
      *
      *=================================================================
      * HISTORY OF MODIFICATION:
      *=================================================================
      * XXXXXX - XXXXXX  - 99/99/9999 - XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      *
      *-----------------------------------------------------------------
      *
       ENVIRONMENT DIVISION.
      ********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       SPECIAL-NAMES. LOCAL-DATA IS LOCAL-DATA-AREA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * SEGMENT
              SELECT TFS302CH ASSIGN TO DATABASE-TFS302CH
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
      * ACCOUNT
              SELECT TFS303CH ASSIGN TO DATABASE-TFS303CH
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
       FILE STATUS IS WK-C-FILE-STATUS.
      * CIF
              SELECT TFS304CH ASSIGN TO DATABASE-TFS304CH
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY

       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TLSICLCA ASSIGN TO DATABASE-TLSICLCA
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
              WITH DUPLICATES
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TLSICLSA ASSIGN TO DATABASE-TLSICLSA
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
              WITH DUPLICATES
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TFSCLSYS ASSIGN TO DATABASE-TFSCLSYS
              ORGANIZATION IS SEQUENTIAL
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TFSICLCA ASSIGN TO DATABASE-TLSICLCA1
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
              WITH DUPLICATES
       FILE STATUS IS WK-C-FILE-STATUS.
              SELECT TFSICLSA ASSIGN TO DATABASE-TLSICLSA1
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
              WITH DUPLICATES
       FILE STATUS IS WK-C-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
      ***************
      * SEGMENT
       FD TFS302CH
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFS302CH.
       01 WK-C-TFS302CH.
              COPY DDS-ALL-FORMATS OF TFS302CH.
       01 WK-C-TFS302CH-1.
              COPY TFS302CH.
      * ACCOUNT
       FD TFS303CH
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFS303CH.
       01 WK-C-TFS303CH.
              COPY DDS-ALL-FORMATS OF TFS303CH.
       01 WK-C-TFS303CH-1.

              COPY TFS303CH.
      * CIF
       FD TFS304CH
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS WK-C-TFS304CH.
       01 WK-C-TFS304CH.
              COPY DDS-ALL-FORMATS OF TFS304CH.
       01 WK-C-TFS304CH-1.
              COPY TFS304CH.
       FD TLSICLCA
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TLSICLCA-REC.
       01 TLSICLCA-REC.
              COPY DDSR-ALL-FORMATS OF TLSICLCA.
       FD TLSICLSA
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TLSICLSA-REC.
       01 TLSICLSA-REC.
              COPY DDSR-ALL-FORMATS OF TLSICLSA.
       FD TFSCLSYS
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSCLSYS-REC.
       01 TFSCLSYS-REC.
              COPY DDS-ALL-FORMATS OF TFSCLSYS.
       01 TFSCLSYS-REC-1.
              COPY TFSCLSYS.
       FD TFSICLCA
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSICLCA-REC.
       01 TFSICLCA-REC.
              COPY DDSR-ALL-FORMATS OF TFSICLCA1.
       FD TFSICLSA
              LABEL RECORDS ARE OMITTED
       DATA RECORD IS TFSICLSA-REC.
       01 TFSICLSA-REC.
              COPY DDSR-ALL-FORMATS OF TLSICLSA1.
       WORKING-STORAGE SECTION.
      ************************
       01 WK-C-COMMON.
              COPY ASCWMS.
              COPY VCCA.
              COPY VCFA.
              COPY VCSA.
              COPY GERTE.
              COPY XGSPA.
       01 WK-C-WORK-AREA.

           05  WS-TRN-AMT              PIC S9(13)V99.
           05  WS-CASAFA-STATUS        PIC X(1).
           05  WS-CASAFA-CIF           PIC X(19).
           05  WS-CASAFA-SEGMENT       PIC X(1).
           05  WK-N-ACCNO              PIC 9(15) VALUE ZEROES.
           05  WK-CORRECT-CCY          PIC X(3) VALUE SPACES.
           05  WK-CORRECT-CCY-FLG      PIC X(1) VALUE SPACES.
           05  WK-NREQ-INQUIRY         PIC X(1) VALUE SPACES.
           05  WS-C-EOF-C1             PIC X(1) VALUE "N".
           05  WS-N-ACCTLEN            PIC 9(02) VALUE ZEROES.
           05  WS-CNT                  PIC 9(02) VALUE ZEROES.
           05  WS-END                  PIC X(01) VALUE "N".
           05  WS-LEN-INP-ACCNO        PIC 9(02) VALUE ZEROES.

           05  WK-C-ACCNO.
           10  WK-C-ACCNO1         PIC X(03).
           10  WK-C-ACCNO2         PIC X(11).

       LINKAGE SECTION.
      ********************
       01  WK-C-LINK-AREA.

           05  WK-C-LINK-AREA-INPUT.
           10  WS-LINK-BNKENTTY    PIC X(02).
           10  WS-LINK-ACCNO       PIC X(15).
           10  WS-LINK-CCY         PIC X(03).
           10  WS-LINK-AMT         PIC S9(13)V99.
           10  WS-LINK-REMIND      PIC X(01).
           05  WK-C-LINK-AREA-OUTPUT.
           10  WS-LINK-STATUS      PIC X(02).

       PROCEDURE DIVISION USING WK-C-LINK-AREA.
      ***************************************
       MAIN-MODULE.

           INITIALIZE WK-C-LINK-AREA-OUTPUT.

      * SEGMENT
           OPEN INPUT TFS302CH
           IF NOT WK-C-SUCCESSFUL
                 AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVLMIT - OPEN FILE ERROR - TFS302CH"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
           END-IF

      * ACCOUNT
           OPEN INPUT TFS303CH
           IF NOT WK-C-SUCCESSFUL
                 AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVLMIT - OPEN FILE ERROR - TFS303CH"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
           END-IF

      * CIF
           OPEN INPUT TFS304CH

           IF NOT WK-C-SUCCESSFUL
                 AND WK-C-FILE-STATUS NOT = "41"
       DISPLAY "TRFVLMT - OPEN FILE ERROR - TFS304CH"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
           END-IF
           OPEN INPUT TLSICLCA.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - OPEN FILE ERROR - TLSICLCA"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.
           OPEN INPUT TLSICLSA.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - OPEN FILE ERROR - TLSICLSA"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.
           OPEN INPUT TFSCLSYS
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - OPEN FILE ERROR - TFSCLSYS"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.
           OPEN INPUT TFSICLCA.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - OPEN FILE ERROR - TFSICLCA"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.
           OPEN INPUT TFSICLSA.
           IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - OPEN FILE ERROR - TFSICLSA"
              DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.
      * GET ACCOUNT NO. LENGTH
           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "RSYACCLEN" TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.
           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL(1:2)
                 TO WK-N-ACCTLEN
       END-IF.
      * GET COUNTRY ROUTING CODE
           INITIALIZE WK-C-XGSPA-RECORD.
           MOVE "RSYACCR0UT" TO WK-C-XGSPA-GHPARCD.
           CALL "TRFXGSPA" USING WK-C-XGSPA-RECORD.

           IF WK-C-XGSPA-ERROR-CD = SPACES
              MOVE WK-C-XGSPA-GHPARVAL TO WK-C-ACCNO1
           ELSE
              MOVE ZEROS           TO WK-C-ACCNO1
       END-IF.

      * ------------------------------------------------------------
      * INITIALIZE AMOUNT FROM CALLING PROGRAMS & START MAIN PROCESS
      * ------------------------------------------------------------

           MOVE SPACES          TO WS-LINK-STATUS.
           INSPECT WS-LINK-AMT REPLACING ALL " " BY ZEROES.
           MOVE WS-LINK-AMT     TO WS-TRN-AMT.

           READ TFSCLSYS.
           IF NOT WK-C-SUCCESSFUL
              DISPLAY "TRFVLMT  - READ TFSCLSYS ERROR"
              DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
              GO TO Y900-ABNORMAL-TERMINATION
       END-IF.

      * ------------------------------------------------------------
      * VALIDATE IF ACCOUNT HAS DEFINED LIMIT
      * ------------------------------------------------------------

       D100-ACCOUNT-VALIDATION.

           PERFORM CHECK-ACCOUNT-CCY
              THRU CHECK-ACCOUNT-CCY-EX.

           INITIALIZE WK-C-TFS303CH.
           MOVE WS-LINK-ACCNO   TO TFS303CH-ACCNO.
           MOVE TFSCLSYS-LCUYCD TO TFS303CH-CUYCD.
           IF WS-LINK-ACCNO(7:1) = "9"
              MOVE WS-LINK-CCY TO TFS303CH-CUYCD.

              READ TFS303CH KEY IS EXTERNALLY-DESCRIBED-KEY
              IF NOT WK-C-SUCCESSFUL
                    AND WK-CORRECT-CCY-FLG = "Y"
                    AND WK-CORRECT-CCY NOT = WS-LINK-CCY
                 MOVE WK-CORRECT-CCY TO TFS303CH-CUYCD
                 MOVE WS-LINK-ACCNO TO TFS303CH-ACCNO
                 READ TFS303CH KEY IS EXTERNALLY-DESCRIBED-KEY
       END-IF.
              IF WK-C-SUCCESSFUL
                    AND WS-LINK-ACCNO = TFS303CH-ACCNO
                    AND (WS-LINK-REMIND = TFS303CH-REMIND
                    OR TFS303CH-REMIND = "B")

                 PERFORM CONVERT-AMT-SGD
                    THRU CONVERT-AMT-SGD-EX

                 IF WS-LINK-REMIND = "I"
                    IF WS-TRN-AMT > TFS303CH-STPLIMIT

                       MOVE "AA"           TO WS-LINK-STATUS
                    ELSE
                       MOVE "A0"       TO WS-LINK-STATUS
                    END-IF
                 END-IF

                 IF   WS-LINK-REMIND     = "0"
                    IF WS-TRN-AMT  > TFS303CH-STPLIMIT2
                       MOVE "AA"       TO WS-LINK-STATUS
                    ELSE
                       MOVE "A0"       TO WS-LINK-STATUS
                    END-IF
                 END-IF

                 PERFORM Z000-END-PROGRAM-ROUTINE
                    THRU Z999-END-PROGRAM-ROUTINE-EX
       END-IF.

      * ----------------------------------------------------------------
      * RETRIEVES ACCOUNT DETAILS
      * ----------------------------------------------------------------

              MOVE   "N"           TO WS-CASAFA-STATUS.
              IF WS-LINK-ACCNO  IS NOT = SPACES
                    AND WS-LINK-ACCNO(1:WS-LEN-INP-ACCNO) IS NUMERIC
                 COMPUTE WK-N-ACCNO =
           FUNCTION NUMVAL(WS-LINK-ACCNO(1:WS-LEN-INP-ACCNO)) * 1

                 IF WS-LINK-ACCNO(7:1) = "1"
                    INITIALIZE TLSICLSA-REC
                    MOVE TFSCLSYS-LCUYCD  TO ACCCUY OF TLSICLSA-REC
                    MOVE WK-N-ACCNO       TO ACCNO OF TLSICLSA-REC
                    READ TLSICLSA KEY IS EXTERNALLY-DESCRIBED-KEY
                    IF NOT WK-C-RECORD-NOT-FOUND AND
                          NOT WK-C-END-OF-FILE AND
                          NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMIT - OPEN FILE ERROR - TLSICLSA"
                       DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                       GO TO Y900-ABNORMAL-TERMINATION
                    END-IF
                    IF WK-C-SUCCESSFUL
                       MOVE   "Y"        TO WS-CASAFA-STATUS
                       MOVE SEGCODE OF TLSICLSA-REC TO WS-CASAFA-SEGMENT
                       MOVE CIFNO OF TLSICLSA-REC TO WS-CASAFA-CIF
                    END-IF
                 END-IF

                 IF WS-LINK-ACCNO(7:1) = "3"
                    INITIALIZE TLSICLCA-REC
                    MOVE TFSCLSYS-LCUYCD  TO ACCCUY OF TLSICLCA-REC
                    MOVE WK-N-ACCNO       TO ACCNO OF TLSICLCA-REC
                    READ TLSICLCA KEY IS EXTERNALLY-DESCRIBED-KEY
                    IF NOT WK-C-RECORD-NOT-FOUND AND
                          NOT WK-C-END-OF-FILE AND

                          NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - OPEN FILE ERROR - TLSICLCA"
                       DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                       GO TO Y900-ABNORMAL-TERMINATION
                    END-IF
                    IF WK-C-SUCCESSFUL
                       MOVE "Y"            TO WS-CASAFA-STATUS
                       MOVE SEGCODE OF TLSICLCA-REC TO WS-CASAFA-SEGMENT
                       MOVE CIFNO OF TLSICLCA-REC   TO WS-CASAFA-CIF
                    END-IF
                 END-IF
                 IF WS-LINK-ACCNO(7:1) = "9"
                    INITIALIZE TLSICLCA-REC
                    MOVE WS-LINK-CCY        TO ACCCUY OF TLSICLCA-REC
                    MOVE WK-N-ACCNO         TO ACCNO OF TLSICLCA-REC
                    READ TLSICLCA KEY IS EXTERNALLY-DESCRIBED-KEY
                    IF NOT WK-C-RECORD-NOT-FOUND AND
                          NOT WK-C-END-OF-FILE AND
                          NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - OPEN FILE ERROR - TLSICLCA"
                       DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                       GO TO Y900-ABNORMAL-TERMINATION
                    END-IF
                    IF WK-C-SUCCESSFUL
                       MOVE "Y"            TO WS-CASAFA-STATUS
                       MOVE SEGCODE OF TLSICLCA-REC TO WS-CASAFA-SEGMENT
                       MOVE CIFNO OF TLSICLCA-REC   TO WS-CASAFA-CIF
                    END-IF
                    IF NOT WK-C-SUCCESSFUL
                          AND WK-CORRECT-CCY-FLG = "Y"
                          AND WK-CORRECT-CCY NOT = WS-LINK-CCY
                       MOVE WK-CORRECT-CCY TO ACCCUY OF TLSICLCA-REC
                       MOVE WK-N-ACCNO     TO ACCNO OF TLSICLCA-REC
                       READ TLSICLCA KEY IS EXTERNALLY-DESCRIBED-KEY
                       IF WK-C-SUCCESSFUL
                          MOVE "Y"        TO WS-CASAFA-STATUS
           MOVE SEGCODE OF TLSICLCA-REC TO WS-CASAFA-SEGMENT
                          MOVE CIFNO OF TLSICLCA-REC   TO WS-CASAFA-CIF
                       END-IF
                    END-IF
       END-IF.
                 IF WS-CASAFA-STATUS = "Y"
                    PERFORM D100-CIF-VALIDATION
                       THRU D199-CIF-VALIDATION-EX
                    ELSE
                    MOVE "A1"               TO WS-LINK-STATUS
                    PERFORM Z000-END-PROGRAM-ROUTINE
                       THRU Z999-END-PROGRAM-ROUTINE-EX.
       D199-ACCOUNT-VALIDATION-EX.
       EXIT.

      * VALIDATE IF CIF HAS DEFINED LIMIT
      * ----------------------------------------------------------------
      *    
       D100-CIF-VALIDATION.
                    INITIALIZE WK-C-TFS304CH.
                    MOVE WS-CASAFA-CIF       TO TFS304CH-CIFNO.
                    READ TFS304CH KEY IS EXTERNALLY-DESCRIBED-KEY
                    IF WK-C-RECORD-NOT-FOUND OR (WK-C-SUCCESSFUL
                          AND WS-CASAFA-CIF     NOT = TFS304CH-CIFNO)
                          OR (WK-C-SUCCESSFUL
                          AND WS-CASAFA-CIF     = TFS304CH-CIFNO
                          AND WS-LINK-REMIND    NOT = TFS304CH-REMIND
                          AND TFS304CH-REMIND   NOT = "B")
                       PERFORM D100-SEGMENT-VALIDATION
                          THRU D199-SEGMENT-VALIDATION-EX
       END-IF.
                    IF WK-C-SUCCESSFUL
                          AND WS-CASAFA-CIF     = TFS304CH-CIFNO
                       PERFORM CONVERT-AMT-SGD
                          THRU CONVERT-AMT-SGD-EX
                       IF WS-LINK-REMIND     = "I"
                          IF WS-TRN-AMT      > TFS304CH-STPLIMIT
                             MOVE "AC"       TO WS-LINK-STATUS
                          ELSE
                             MOVE "A0"       TO WS-LINK-STATUS
                          END-IF
                       END-IF
                       IF WS-LINK-REMIND     = "O"
                          IF WS-TRN-AMT      > TFS304CH-STPLIMIT2
                             MOVE "AC"       TO WS-LINK-STATUS
                          ELSE
                             MOVE "A0"       TO WS-LINK-STATUS
                          END-IF
                       END-IF
                       PERFORM Z000-END-PROGRAM-ROUTINE
                          THRU Z999-END-PROGRAM-ROUTINE-EX
       END-IF.
       D199-CIF-VALIDATION-EX.
       EXIT.
      * ----------------------------------------------------------------
      *    
      * VALIDATE IF SEGMENT HAS DEFINED LIMIT
      * ----------------------------------------------------------------
      *    
       D100-SEGMENT-VALIDATION.
                    INITIALIZE WK-C-TFS302CH.
                    MOVE WS-CASAFA-SEGMENT   TO TFS302CH-SEGCODE.

                    READ  TFS302CH KEY IS EXTERNALLY-DESCRIBED-KEY
                    IF WK-C-RECORD-NOT-FOUND OR (WK-C-SUCCESSFUL
                          AND WS-CASAFA-SEGMENT NOT = TFS302CH-SEGCODE)
                          OR (WK-C-SUCCESSFUL
                          AND WS-CASAFA-SEGMENT = TFS302CH-SEGCODE
                          AND WS-LINK-REMIND NOT = TFS302CH-REMIND
                          AND TFS302CH-REMIND NOT = "B")
                       MOVE "A1"        TO WS-LINK-STATUS
                       PERFORM Z000-END-PROGRAM-ROUTINE
                          THRU Z999-END-PROGRAM-ROUTINE-EX
       END-IF.

                    IF WK-C-SUCCESSFUL
                          AND WS-CASAFA-SEGMENT = TFS302CH-SEGCODE

                       PERFORM CONVERT-AMT-SGD
                          THRU CONVERT-AMT-SGD-EX

                       IF  WS-LINK-REMIND    = "I"
                          IF WS-TRN-AMT  >  TFS302CH-STPLIMIT
                             MOVE "AS"    TO WS-LINK-STATUS
                          ELSE
                             MOVE "A0"    TO WS-LINK-STATUS
                          END-IF
                       END-IF

                       IF  WS-LINK-REMIND    = "O"
                          IF WS-TRN-AMT  >  TFS302CH-STPLIMIT2
                             MOVE "AS"    TO WS-LINK-STATUS
                          ELSE
                             MOVE "A0"    TO WS-LINK-STATUS
                          END-IF
                       END-IF

                       PERFORM Z000-END-PROGRAM-ROUTINE
                          THRU Z999-END-PROGRAM-ROUTINE-EX
       END-IF.

       D199-SEGMENT-VALIDATION-EX.
       EXIT.

      * ----------------------------------------------------------------
      * PERFORM FX CONVERSION
      * ----------------------------------------------------------------
       CONVERT-AMT-SGD.

                    IF WS-LINK-CCY  NOT = TFSCLSYS-LCUYCD
                       IF WS-LINK-REMIND    = "I"
                          MOVE "BT"    TO WK-C-GERTE-RTE-TYP
                       ELSE
                          MOVE "BR"    TO WK-C-GERTE-RTE-TYP
                       END-IF
                       MOVE WS-LINK-CCY    TO WK-C-GERTE-CUYCD

                       MOVE WS-LINK-BNKENTTY     TO WK-N-GERTE-BNKENTTY
                       CALL "TRFGERTE"           USING WK-C-GERTE-RECORD
                       COMPUTE WS-TRN-AMT ROUNDED = WS-TRN-AMT
      * WK-N-GERTE-EXCH-RTE
      * WK-N-GERTE-FXRATEUT
                    ELSE
                       MOVE WS-LINK-AMT          TO WS-TRN-AMT
       END-IF.

                    IF WS-LINK-AMT > 0 AND WS-TRN-AMT = 0
                       MOVE "XX"                 TO WS-LINK-STATUS
       END-IF.

       CONVERT-AMT-SGD-EX.
       EXIT.
      * ----------------------------------------------------------------
      * CHECK ACCOUNT DETAILS
      * ----------------------------------------------------------------
       CHECK-ACCOUNT-CCY.
                    INITIALIZE WS-LEN-INP-ACCNO.
                    MOVE "N" TO WS-END.

      * Get length of the accno passed to program
                    PERFORM VARYING WS-CNT
                          FROM 1 BY 1 UNTIL WS-CNT > 15
                          OR WS-END = "Y"
                       IF WS-LINK-ACCNO(WS-CNT:1) IS NUMERIC
                          ADD 1 TO WS-LEN-INP-ACCNO
                       ELSE
                          MOVE "Y" TO WS-END
                       END-IF
       END-PERFORM.

                    IF WS-LEN-INP-ACCNO NOT = WK-N-ACCTLEN
                          AND WK-N-ACCTLEN > 0
                       MOVE WS-LINK-ACCNO TO WK-C-ACCNO2
                       MOVE WK-C-ACCNO TO WS-LINK-ACCNO
                       MOVE WK-N-ACCTLEN TO WS-LEN-INP-ACCNO
       END-IF.

                    IF WS-LEN-INP-ACCNO > 0
                       IF WS-LINK-ACCNO(7:1) = "9"
           AND WS-LINK-ACCNO(1:WS-LEN-INP-ACCNO) IS NUMERIC
                          COMPUTE WK-N-ACCNO =
           FUNCTION NUMVAL(WS-LINK-ACCNO(1:WS-LEN-INP-ACCNO)) * 1
                          INITIALIZE TLSICLCA-REC
                          MOVE SPACES TO WK-CORRECT-CCY
                          MOVE "N" TO WK-CORRECT-CCY-FLG
                          MOVE WS-LINK-CCY TO ACCCUY OF TLSICLCA-REC
                          MOVE WK-N-ACCNO TO ACCNO OF TLSICLCA-REC
                          READ TLSICLCA KEY IS EXTERNALLY-DESCRIBED-KEY
                          IF NOT WK-C-RECORD-NOT-FOUND AND
                                NOT WK-C-END-OF-FILE AND
                                NOT WK-C-SUCCESSFUL

       DISPLAY "TRFVLMT - OPEN FILE ERROR - TLSICLCA"
                             DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                             GO TO Y900-ABNORMAL-TERMINATION
                          END-IF
                          IF WK-C-SUCCESSFUL
                             MOVE WS-LINK-CCY        TO WK-CORRECT-CCY
           MOVE "Y"                TO WK-CORRECT-CCY-FLG
                          ELSE
                             MOVE "N"                TO WK-NREQ-INQUIRY
                             INITIALIZE TFSICLCA-REC
           MOVE WK-N-ACCNO         TO ACCNO OF TFSICLCA-REC
           START TFSICLCA KEY >= EXTERNALLY-DESCRIBED-KEY
                             IF WK-C-SUCCESSFUL
                                MOVE "N"            TO WS-C-EOF-C1
                                PERFORM FETCH-CURSOR-1
                                   THRU FETCH-CURSOR-1-EX
                                   UNTIL WS-C-EOF-C1 = "Y"
                                END-IF
                             IF WK-NREQ-INQUIRY NOT = "Y"
                                INITIALIZE WK-C-VCFA-RECORD
                                MOVE WS-LINK-ACCNO(1:WS-LEN-INP-ACCNO)
                                   TO WK-C-VCFA-FCCA
                                MOVE WS-LINK-CCY    TO WK-C-VCFA-CUY
                                CALL "TRVCFA" USING WK-C-VCFA-RECORD
                                IF WK-C-VCFA-ERROR-CD NOT EQUAL SPACES
                                   MOVE "N"        TO WK-CORRECT-CCY-FLG
                                ELSE
                                   MOVE WS-LINK-CCY TO WK-CORRECT-CCY
                                   MOVE "Y"        TO WK-CORRECT-CCY-FLG
                                END-IF
                             END-IF
       END-IF.

       CHECK-ACCOUNT-CCY-EX.
       EXIT.

       FETCH-CURSOR-1.

                          READ TFSICLCA NEXT
                             AT END MOVE "Y"    TO WS-C-EOF-C1
                          GO TO FETCH-CURSOR-1-EX.

                          IF (NOT WK-C-SUCCESSFUL AND
                                NOT WK-C-RECORD-NOT-FOUND)
           OR (WK-N-ACCNO NOT = ACCNO OF TFSICLCA-REC)
                             MOVE "Y"            TO WS-C-EOF-C1
                             GO TO FETCH-CURSOR-1-EX
       END-IF.

                          IF ACCCUY OF TFSICLCA-REC = WS-LINK-CCY AND
                                 ACCCUY OF TFSICLCA-REC IS NOT = SPACES
                             MOVE "Y"            TO WK-NREQ-INQUIRY
       END-IF.

                          IF MSGRETCODE OF TFSICLCA-REC = "G"

           MOVE ACCCUY OF TFSICLCA-REC TO WK-CORRECT-CCY
                             MOVE "Y"         TO WS-C-EOF-C1
                                WK-NREQ-INQUIRY
                                WK-CORRECT-CCY-FLG
                             GO TO FETCH-CURSOR-1-EX
       END-IF.

       FETCH-CURSOR-1-EX.
       EXIT.
      * ----------------------------------------------------------------
      *    -
      * ABNORMAL TERMINATION
      * ----------------------------------------------------------------
      *    -
       Y900-ABNORMAL-TERMINATION.

                          PERFORM Z000-END-PROGRAM-ROUTINE
                             THRU Z999-END-PROGRAM-ROUTINE-EX.

      *-----------------------------------------------------------------
      *    -*
       Z000-END-PROGRAM-ROUTINE.
      *-----------------------------------------------------------------
      *    -*
                          CLOSE TFS302CH
                          IF NOT WK-C-SUCCESSFUL
           DISPLAY "TRFVLMT - CLOSE FILE-ERROR - TFS302CH"
                             DISPLAY "FILE-STATUS IS " WK-C-FILE-STATUS
       END-IF.

                          CLOSE TFS303CH
                          IF NOT WK-C-SUCCESSFUL
           DISPLAY "TRFVLMT - CLOSE FILE-ERROR - TFS303CH"
                             DISPLAY "FILE-STATUS IS " WK-C-FILE-STATUS
       END-IF.

                          CLOSE TFS304CH
                          IF NOT WK-C-SUCCESSFUL
           DISPLAY "TRFVLMT - CLOSE FILE-ERROR - TFS304CH"
                             DISPLAY "FILE-STATUS IS " WK-C-FILE-STATUS
       END-IF.

                          CLOSE TLSICLCA.
                          IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - CLOSE FILE ERROR - TLSICLCA"
                             DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.

                          CLOSE TLSICLSA.
                          IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - CLOSE FILE ERROR - TLSICLSA"
                             DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.

                          CLOSE TFSCLSYS.
                          IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - CLOSE FILE ERROR - TFSCLSYS"
                             DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.

       END-IF.
                       CLOSE TFSICLCA.
                       IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - CLOSE FILE ERROR - TFSICLCA"
                          DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.
                       CLOSE TFSICLSA.
                       IF NOT WK-C-SUCCESSFUL
       DISPLAY "TRFVLMT - CLOSE FILE ERROR - TFSICLSA"
                          DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
       END-IF.
                       GO TO Z000-END-PROGRAM.
       Z999-END-PROGRAM-ROUTINE-EX.
       EXIT.
      * ----------------------------------------------------------------
      *    -
      * END OF THIS PROGRAM
      * ----------------------------------------------------------------
      *    -
       Z000-END-PROGRAM.
                       EXIT PROGRAM.
