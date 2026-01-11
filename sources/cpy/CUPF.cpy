      ******************************************************************
      *                                                                *
      *                     COPYBOOK FOR CALLING TRCUFP                *
      *                                                                *
      ******************************************************************
      * MODIFICATION HISTORY                                           *
      ******************************************************************
      * TAG   DATE    DEV    DESCRIPTION                               *
      * ------ ------- ------ ---------------------------------------- *
      * G02JM1 01/02/16 TMPJP6 - REM 2016 Q2 RELEASE                   *
      *                           E-REQUEST# 46332                     *
      *                           MERCURIA PHASE II                    *
      *                           INITIAL VERSION                      *
      ******************************************************************
       01 WK-C-CUPF-RECORD.
           05 WK-C-CUPF-INPUT.
           10 WK-C-CUPF-CUYCD          PIC X(03).
           10 WK-C-CUPF-ACCNO          PIC X(15).
           10 WK-C-CUPF-BANKID         PIC X(11).
           10 WK-C-CUPF-OPTION         PIC X(01).
      *    Valid values for WK-C-CUPF-OPTION:
      *    "1" => Key ACCNO, CUYCD & BANKID
      *    "2" => Key ACCNO & CUYCD
           05 WK-C-CUPF-OUTPUT.
           10 WK-C-CUPF-INVALID-OUTPUT.
           15 WK-C-CUPF-ERROR-CD   PIC X(07).
           15 WK-C-CUPF-COM0216.
           20 WK-C-CUPF-FILE  PIC X(08).
           20 WK-C-CUPF-MODE  PIC X(06).
           20 WK-C-CUPF-KEY   PIC X(20).
           20 WK-C-CUPF-FS    PIC X(02).
           10 WK-C-CUPF-VALID-OUTPUT.
           15 WK-C-CUPF-WEHR-PERIOD PIC X(01).
           15 WK-C-CUPF-BFBYTE    PIC 9(08).
           15 WK-C-CUPF-WEIND     PIC X(01).
           15 WK-C-CUPF-BACKVALIND PIC X(01).
           15 WK-C-CUPF-CHGIND    PIC X(01).
           15 WK-C-CUPF-PRTIND    PIC X(01).
           15 WK-C-CUPF-ACCTYP    PIC X(01).
