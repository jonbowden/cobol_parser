      * NSTP.cpybk
      *---------- COPYBOOK FOR CALLING TRFNSTP - 28/07/2011 ----------*
      *                        AMENDMENT HISTORY
      *----------------------------------------------------------------
      *DATE       BY      AMENDMENT
      *----------------------------------------------------------------
      *23/09/15   GCB     EWF-OTT STP PROJECT
      *STGB1      MODIFIED THE PROGRAM TO CHANGE THE LENGTH OF ACCTBIC
      *           FROM 11 TO 15.
      *----------------------------------------------------------------
       01 WK-NSTP.
           05 WK-NSTP-INPUT.
              STPGB1*       10 WK-NSTP-ACCTBIC         PIC X(11).
STPGB1     10 WK-NSTP-ACCTBIC         PIC X(15).
           05 WK-NSTP-OUTPUT.
           10 WK-NSTP-ERROR-FOUND    PIC X(01).
           10 WK-NSTP-NONSTPCR       PIC X(01).
           10 WK-NSTP-NONSTPDR       PIC X(01).
