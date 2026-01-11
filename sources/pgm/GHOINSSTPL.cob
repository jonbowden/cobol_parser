       100  IDENTIFICATION DIVISION.
       200  PROGRAM-ID.      GHOINSSTPL IS INITIAL.
       300  AUTHOR.          ACCENTURE.
       400  DATE-WRITTEN.    23 APRIL 2025.
       500
       600  *----------------------------------------------------------------*
      3800  *----------------------------------------------------------------*
      3900  * HISTORY OF MODIFICATION:                                      *
      4000  *----------------------------------------------------------------*
      4100  *|USER    |DATE      | TAG      | DESCRIPTION                  |*
      4200  *----------------------------------------------------------------*
     10700  *----------------------------------------------------------------*
     10800       EJECT
     10900
     11000       ENVIRONMENT DIVISION.
     11100       CONFIGURATION SECTION.
     11200       SOURCE-COMPUTER. IBM-AS400.
     11300       OBJECT-COMPUTER. IBM-AS400.
     11400       SPECIAL-NAMES.    LOCAL-DATA IS LOCAL-DATA-AREA
     11500                         UPSI-0 IS UPSI-SWITCH-0
     11600                           ON  STATUS IS U0-ON
     11700                           OFF STATUS IS U0-OFF
     11800                         UPSI-1 IS UPSI-SWITCH-1
     11900                           ON  STATUS IS U0-ON
     12000                           OFF STATUS IS U0-OFF
     12100                         UPSI-2 IS UPSI-SWITCH-2
     12200                           ON  STATUS IS U0-ON
     12300                           OFF STATUS IS U0-OFF
     12400                         UPSI-3 IS UPSI-SWITCH-3
     12500                           ON  STATUS IS U0-ON
     12600                           OFF STATUS IS U0-OFF.
     12700
     12800       INPUT-OUTPUT SECTION.
     12900       FILE-CONTROL.
     13000
     13100       DATA DIVISION.
     13200       FILE SECTION.
     13300       WORKING-STORAGE SECTION.
     13400       01  F                       PIC  X(024) VALUE
     13500           "** PROGRAM GHOINSSTPL **".
     13600       EJECT.
     13700
     13800       01  WS-DATE                 PIC X(08).
     13900       01  WS-DATE-YYMD.
     14000           05 WS-DATE-CEN          PIC X(02)  VALUE "20".
     14100           05 WS-DATE-YMD          PIC X(06).
     14200
     14300       01  WS-DEL-EXIST-SW         PIC X(01).
     14400           88 WS-DEL-YES                      VALUE "Y".
     14500           88 WS-DEL-NO                       VALUE "N".
     14600
     14700       01  WK-C-WORK-AREA.
     16800
     17500           05  WK-N-SQLCODE        PIC 9(009) VALUE ZERO.
     17700           05  WK-C-ERRMSG         PIC X(050).
     17800           05  WK-COMMAND          PIC X(60).
     17900           05  WK-P-COMMAND-LEN    PIC 9(10)V9(5)  COMP.
     18000           05  WK-COMMAND2         PIC X(90) VALUE SPACES.
     18100           05  WK-CMD2-FILL1       PIC X(29) VALUE
     18200               "OVRPRTF FILE(QPQUPRFIL) OUTQ(".
     18300           05  WK-CMD2-OUTQ        PIC X(10) VALUE SPACES.
     18400           05  WK-CMD2-FILL2       PIC X(31) VALUE
     18500               ") SAVE(*YES) USRDTA(REMINSM101)".
     18600
     18700           EXEC SQL
     18800                INCLUDE SQLCA
     18900           END-EXEC.
     19000
     19100       PROCEDURE DIVISION.
     19600
     19700       MAIN-MODULE.
     19900           PERFORM A001-START-PROGRAM-ROUTINE
     20000              THRU A999-START-PROGRAM-ROUTINE-EX.
     20100
     20200           GOBACK.
     20500       EJECT.
     20600
     20700       A001-START-PROGRAM-ROUTINE.
     20800           SET     UPSI-SWITCH-2           TO      OFF.
     20900           ACCEPT  WS-DATE-YMD              FROM DATE.
     21000
     21200
     23400       A200-DELETE-RECORD.
    151100
    151200           EXEC SQL
    151300                DELETE    FROM TFSSTPL
    151400                WHERE PARALNO IN (SELECT PARALNO
    151800                                  FROM UFIMID
    151900                                  WHERE PRCIND = "Y" AND STRIND = " ")
    152000           END-EXEC
    152200
    152300           PERFORM Y600-CHECK-SQL-ERROR.
    152400
    152500       A200-UPDATE-RECORD.
    152900
    153000           EXEC SQL
    153100                UPDATE  UFIMID
    153200                SET PRCIND = " ", STRIND = " "
    153300                WHERE PRCIND = "Y" AND STRIND = " "
    153500           END-EXEC
    153700
    153800           PERFORM Y600-CHECK-SQL-ERROR.
    153900
    154000       A999-START-PROGRAM-ROUTINE-EX.
    155000           EXIT.
    155100
    155200
    158500       Y600-CHECK-SQL-ERROR.
    160900           IF SQLCODE NOT EQUAL ZERO
    161000              MOVE SQLCODE TO WK-N-SQLCODE
    161100              DISPLAY WK-C-ERRMSG
    161200              DISPLAY "SQLCODE : " WK-N-SQLCODE
    161300              EXEC SQL
    161400                   ROLLBACK
    161500              END-EXEC
    161600           ELSE
    161700              EXEC SQL
    161800                   COMMIT
    161900              END-EXEC
    162000              DISPLAY "RECORD UPDATED  TFSSTPL "
    162100           END-IF.
    162200
    162300       Y900-ABNORMAL-TERMINATION.
    162400           SET UPSI-SWITCH-2 TO ON.
    162500           GOBACK.
    162600
