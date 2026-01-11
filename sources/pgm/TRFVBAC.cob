      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.     TRFVBAC.
       AUTHOR.         MATILDA WEE TL.
       DATE-WRITTEN.   10 JUL 1989.
      *
      *DESCRIPTION :  THIS IS A CALLED ROUTINE TO CHECK BANK ACCOUNT
      *               TABLE.
      *
       EJECT
      **********************
       ENVIRONMENT DIVISION.
      **********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-AS400.
       OBJECT-COMPUTER.  IBM-AS400.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TFSBNKAC ASSIGN TO DATABASE-TFSBNKAC
                  ORGANIZATION      IS INDEXED
                  ACCESS MODE       IS RANDOM
                  RECORD KEY        IS EXTERNALLY-DESCRIBED-KEY
                  FILE STATUS       IS WK-C-FILE-STATUS.
      
      ***************
       DATA DIVISION.
      ***************
       FILE SECTION.
      **************
       FD  TFSBNKAC
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS WK-C-TFSBNKAC.
       01  WK-C-TFSBNKAC.
           COPY DDS-ALL-FORMATS OF TFSBNKAC.
       01  WK-C-TFSBNKAC-1.
           COPY TFSBNKAC.
      
       WORKING-STORAGE SECTION.
      *************************
       01  FILLER                          PIC X(24)        VALUE
           "** PROGRAM TRFVBAC **".
      
      * ------------------ PROGRAM WORKING STORAGE -------------------*
       01    WK-C-COMMON.
       COPY ASCMWS.
       COPY FIL3090.
      
      *****************
       LINKAGE SECTION.
      *****************
       COPY VBAC.
       EJECT
      ********************************************
       PROCEDURE DIVISION USING WK-C-VBAC-RECORD.
      ********************************************
       MAIN-MODULE.
           PERFORM A000-PROCESS-CALLED-ROUTINE
              THRU A099-PROCESS-CALLED-ROUTINE-EX.
           PERFORM Z000-END-PROGRAM-ROUTINE
              THRU Z999-END-PROGRAM-ROUTINE-EX.
           EXIT PROGRAM.
      
      *---------------------------------------------------------------*
       A000-PROCESS-CALLED-ROUTINE.
      *---------------------------------------------------------------*
           OPEN INPUT TFSBNKAC.
           IF  NOT WK-C-SUCCESSFUL
               DISPLAY "TRFVBAC - OPEN FILE ERROR - TFSBNKAC"
               DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
               GO TO Y900-ABNORMAL-TERMINATION.
      
           MOVE    SPACES                  TO    WK-C-VBAC-OUTPUT.
           MOVE    ZEROES                  TO    WK-C-VBAC-FS.
           MOVE    WK-N-VBAC-BNKENTTY      TO    TFSBNKAC-BNKENTTY.
           MOVE    WK-C-VBAC-BANKID        TO    TFSBNKAC-BANKID.
           MOVE    WK-C-VBAC-CUYCD         TO    TFSBNKAC-CUYCD.
           MOVE    1                       TO    TFSBNKAC-PRIORTY.
      
           READ TFSBNKAC KEY IS EXTERNALLY-DESCRIBED-KEY.
           IF  WK-C-SUCCESSFUL
               GO TO A080-MOVE-DATA.
      
           IF WK-C-RECORD-NOT-FOUND
               MOVE    "SUP0016"           TO    WK-C-VBAC-ERROR-CD
           ELSE
               MOVE    "COM0206"           TO    WK-C-VBAC-ERROR-CD.
      
           MOVE    "TFSBNKAC"              TO    WK-C-VBAC-FILE.
           MOVE    "READ"                  TO    WK-C-VBAC-MODE
           MOVE    WK-C-VBAC-INPUT         TO    WK-C-VBAC-KEY.
           MOVE    WK-C-FILE-STATUS        TO    WK-C-VBAC-FS.
      
           GO TO A099-PROCESS-CALLED-ROUTINE-EX.
      
       A080-MOVE-DATA.
           MOVE    TFSBNKAC-BNKACNO        TO    WK-C-VBAC-BNKACNO.
           MOVE    TFSBNKAC-ACCTYP         TO    WK-C-VBAC-ACCTYP.
           MOVE    TFSBNKAC-ACUDBUI        TO    WK-C-VBAC-ACUDBUI.
      
       A099-PROCESS-CALLED-ROUTINE-EX.
           EXIT.
      *---------------------------------------------------------------*
      *                   PROGRAM SUBROUTINE                         *
      *---------------------------------------------------------------*
       Y900-ABNORMAL-TERMINATION.
           PERFORM Z000-END-PROGRAM-ROUTINE.
           EXIT PROGRAM.
      
       Z000-END-PROGRAM-ROUTINE.
           CLOSE TFSBNKAC.
           IF  NOT WK-C-SUCCESSFUL
               DISPLAY "TRFVBAC - CLOSE FILE ERROR - TFSBNKAC"
               DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS.
      
       Z999-END-PROGRAM-ROUTINE-EX.
           EXIT.
      
      ******************************************************************
      ************** END OF PROGRAM SOURCE -  TRFVBAC ***************
      ******************************************************************
