      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.     TRFXGSPA.
       AUTHOR.         DESMOND LIM.
       DATE-WRITTEN.   23 SEP 2002.
      *
      *DESCRIPTION :  THIS ROUTINE OBTAIN THE PARAMETER VALUE BASED
      *               ON THE SPECIFICATION DEFINED IN THE GLOBAL SYSTEM
      *               PARAMETER FILE.
      *NOTE        :  COPY FROM TRFXPARA PROGRAM.
      *
      *================================================================
      * HISTORY OF MODIFICATION:
      *================================================================
      *  MPIDCK - DCKABINGUE 15/08/2001 - MEPS PHASE 1 MODS
      *                                 - IN ORDER FOR THIS PROGRAM TO
      *                                   BE USED EVEN BY THE CL EXIT
      *                                   PROGRAM COMMAND IS CHANGED
      *                                   TO GOBACK.
      *---------------------------------------------------------------*
       EJECT
      **********************
       ENVIRONMENT DIVISION.
      **********************
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TFSGSYSPA ASSIGN TO DATABASE-TFSGSYSPA
                  ORGANIZATION      IS INDEXED
                  ACCESS MODE       IS RANDOM
                  RECORD KEY        IS EXTERNALLY-DESCRIBED-KEY
                  FILE STATUS       IS WK-C-FILE-STATUS.
       EJECT
      ***************
       DATA DIVISION.
      ***************
       FILE SECTION.
      ***************
       FD  TFSGSYSPA
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS TFSGSYSPA-REC.
       01  TFSGSYSPA-REC.
           COPY DDS-ALL-FORMATS OF TFSGSYSPA.
       01  TFSGSYSPA-REC-1.
           COPY TFSGSYSPA.
      
      *************************
       WORKING-STORAGE SECTION.
      *************************
       01  FILLER              PIC X(24)  VALUE
           "** PROGRAM TRFXGSPA  **".
      
      * ------------------ PROGRAM WORKING STORAGE -------------------*
       COPY FIL3090.
       01 WK-C-COMMON.
       COPY ASCMWS.
      
       EJECT
       LINKAGE SECTION.
      *****************
       COPY XGSPA.
       EJECT
      ********************************************
       PROCEDURE DIVISION USING WK-C-XGSPA-RECORD.
      ********************************************
       MAIN-MODULE.
           PERFORM A000-MAIN-PROCESSING
              THRU A099-MAIN-PROCESSING-EX.
           PERFORM Z000-END-PROGRAM-ROUTINE
              THRU Z099-END-PROGRAM-ROUTINE-EX.
       MPIDCK     GOBACK.
       EJECT
      *---------------------------------------------------------------*
       A000-MAIN-PROCESSING.
      *---------------------------------------------------------------*
           OPEN INPUT TFSGSYSPA.
           IF  NOT WK-C-SUCCESSFUL
               DISPLAY "TRFXGSPA - OPEN FILE ERROR - TFSGSYSPA"
               DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
               GO TO Y900-ABNORMAL-TERMINATION.
      
           MOVE    SPACES                  TO    WK-C-XGSPA-OUTPUT.
           MOVE    ZERO                    TO    WK-C-XGSPA-GHPARVAL.
      
           MOVE WK-C-XGSPA-GHPARCD TO TFSGSYSPA-GHPARCD.
           READ TFSGSYSPA KEY IS EXTERNALLY-DESCRIBED-KEY.
      
           IF  WK-C-SUCCESSFUL
               GO TO A080-MOVE-DATA.
      
           IF WK-C-RECORD-NOT-FOUND
               MOVE "COM0245"          TO    WK-C-XGSPA-ERROR-CD
           ELSE
               MOVE "COM0206"          TO    WK-C-XGSPA-ERROR-CD.
      
           MOVE    "TFSGSYSPA"             TO    WK-C-XGSPA-FILE.
           MOVE    "SELECT"                TO    WK-C-XGSPA-MODE
           MOVE    WK-C-XGSPA-INPUT        TO    WK-C-XGSPA-KEY.
           MOVE    WK-C-FILE-STATUS        TO    WK-C-XGSPA-FS.
      
           GO TO A099-MAIN-PROCESSING-EX.
      
       A080-MOVE-DATA.
      *
           IF TFSGSYSPA-ATTRIBUT = "A"
               MOVE TFSGSYSPA-GHPARVAL TO WK-C-XGSPA-GHPARVAL.
      *
           ELSE
      *
           IF TFSGSYSPA-ATTRIBUT = "N"
               MOVE TFSGSYSPA-PARAVALU TO WK-N-XGSPA-GHPARNUM.
      
      *---------------------------------------------------------------*
       A099-MAIN-PROCESSING-EX.
      *---------------------------------------------------------------*
           EXIT.
      
       Y900-ABNORMAL-TERMINATION.
           PERFORM Z000-END-PROGRAM-ROUTINE
              THRU Z099-END-PROGRAM-ROUTINE-EX.
           EXIT PROGRAM.
      
      *---------------------------------------------------------------*
       Z000-END-PROGRAM-ROUTINE.
      *---------------------------------------------------------------*
           CLOSE TFSGSYSPA.
           IF  NOT WK-C-SUCCESSFUL
               DISPLAY "TRFXGSPA - CLOSE FILE ERROR - TFSGSYSPA"
               DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS.
      
      *---------------------------------------------------------------*
       Z099-END-PROGRAM-ROUTINE-EX.
      *---------------------------------------------------------------*
           EXIT.
      
      ******************************************************************
      *************** END OF PROGRAM SOURCE  TRFXGSPA ***************
      ******************************************************************
