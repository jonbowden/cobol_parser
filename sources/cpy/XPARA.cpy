      * XPARA.CPY
       01  WK-C-XPARA-RECORD.
           05  WK-C-XPARA-INPUT.
               10  WK-C-XPARA-PARACD         PIC X(08).
           05  WK-C-XPARA-OUTPUT.
               10  WK-C-XPARA-INVALID-OUTPUT.
                   15  WK-C-XPARA-ERROR-CD   PIC X(07).
                   15  WK-C-XPARA-COM0206.
                       20  WK-C-XPARA-FILE   PIC X(08).
                       20  WK-C-XPARA-MODE   PIC X(06).
                       20  WK-C-XPARA-KEY    PIC X(20).
                       20  WK-C-XPARA-FS     PIC X(02).
               10  WK-C-XPARA-VALID-OUTPUT.
                   15  WK-C-XPARA-PARAVALU   PIC X(20).
                   15  WK-N-XPARA-PARANUM REDEFINES WK-C-XPARA-PARAVALU
                       PIC S9(18).
