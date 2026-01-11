*---------- COPYBOOK FOR CALLING TRFNMVR - 07/06/2004 ----------*
       01  WK-NMVR.
           05 WK-NMVR-INPUT.
               10 WK-NMVR-CUVYCD       PIC X(3).
               10 WK-NMVR-ACCNO        PIC X(11).
               10 WK-NMVR-ACCNM        PIC X(35).
           05 WK-NMVR-OUTPUT.
               10 WK-NMVR-ERROR-FOUND  PIC X(1).
               10 WK-NMVR-INDIC        PIC X(1).