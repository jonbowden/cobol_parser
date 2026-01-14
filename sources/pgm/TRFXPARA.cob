      *************************
       IDENTIFICATION DIVISION.
      *************************
       PROGRAM-ID.     TRFXPARA.
       AUTHOR.         MATILDA WEE TL.
       DATE-WRITTEN.   15 SEP 1989.
      *
      *DESCRIPTION :  THIS ROUTINE OBTAIN THE PARAMETER VALUE BASED
      *               ON THE SPECIFICATION DEFINED IN THE SYSTEM AND
      *               APPLICATION PARAMETER FILE.
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
       SOURCE-COMPUTER.  IBM-AS400.
       OBJECT-COMPUTER.  IBM-AS400.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TFSAPLPA ASSIGN TO DATABASE-TFSAPLPA
                  ORGANIZATION      IS INDEXED
                  ACCESS MODE       IS RANDOM
                  RECORD KEY        IS EXTERNALLY-DESCRIBED-KEY
                  FILE STATUS       IS WK-C-FILE-STATUS.
      
       EJECT
      ***************
       DATA DIVISION.
      ***************
       FILE SECTION.
      **************
       FD  TFSAPLPA
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS TFSAPLPA-REC.
       01  TFSAPLPA-REC.
           COPY DDS-ALL-FORMATS OF TFSAPLPA.
       01  TFSAPLPA-REC-1.
           COPY TFSAPLPA.
      
      *************************
       WORKING-STORAGE SECTION.
      *************************
       01  FILLER              PIC X(24)  VALUE
           "** PROGRAM TRFXPARA  **".
      
      * ------------------ PROGRAM WORKING STORAGE -------------------*
       COPY FIL3090.
       01 WK-C-COMMON.
       COPY ASCMWS.
      
       EJECT
       LINKAGE SECTION.
      *****************
       COPY XPARA.
       EJECT
      ********************************************
       PROCEDURE DIVISION USING WK-C-XPARA-RECORD.
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
           OPEN    INPUT TFSAPLPA.
           IF      NOT WK-C-SUCCESSFUL
                   DISPLAY "TRFXPARA - OPEN FILE ERROR - TFSAPLPA"
                   DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
                   GO TO Y900-ABNORMAL-TERMINATION.
      
           MOVE    SPACES                  TO    WK-C-XPARA-OUTPUT.
           MOVE    ZERO                    TO    WK-C-XPARA-PARAVALU.
      
           MOVE    WK-C-XPARA-PARACD       TO    TFSAPLPA-PARACD.
      
           READ    TFSAPLPA KEY IS EXTERNALLY-DESCRIBED-KEY.
           IF      WK-C-SUCCESSFUL
                   GO TO A080-MOVE-DATA.
      
           IF      WK-C-RECORD-NOT-FOUND
                   MOVE "COM0245"          TO    WK-C-XPARA-ERROR-CD
      
           ELSE
                   MOVE "COM0206"          TO    WK-C-XPARA-ERROR-CD.
      
           MOVE    "TFSAPLPA"              TO    WK-C-XPARA-FILE.
           MOVE    "SELECT"                TO    WK-C-XPARA-MODE
           MOVE    WK-C-XPARA-INPUT        TO    WK-C-XPARA-KEY.
           MOVE    WK-C-FILE-STATUS        TO    WK-C-XPARA-FS.
      
           GO TO A099-MAIN-PROCESSING-EX.
      
       A080-MOVE-DATA.
      *    IF      TFSAPLPA-ATTRIBUT = "A"
                   MOVE TFSAPLPA-PARAVALU TO    WK-C-XPARA-PARAVALU.
      *
           ELSE
      *    IF      TFSAPLPA-ATTRIBUT = "N"
                   MOVE TFSAPLPA-PARAVALU TO    WK-N-XPARA-PARANUM.
      
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
           CLOSE   TFSAPLPA.
           IF      NOT WK-C-SUCCESSFUL
                   DISPLAY "TRFXPARA - CLOSE FILE ERROR - TFSAPLPA"
                   DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS.
      
      *---------------------------------------------------------------*
       Z099-END-PROGRAM-ROUTINE-EX.
      *---------------------------------------------------------------*
           EXIT.
      
      ******************************************************************
      *************** END OF PROGRAM SOURCE - TRFXPARA ***************
      ******************************************************************
