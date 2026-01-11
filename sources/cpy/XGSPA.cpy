      * XGSPA.cpy
       01  WK-C-XGSPA-RECORD.
           05  WK-C-XGSPA-INPUT.
               10  WK-C-XGSPA-GHPARCD         PIC X(10).
           05  WK-C-XGSPA-OUTPUT.
               10  WK-C-XGSPA-INVALID-OUTPUT.
                   15  WK-C-XGSPA-ERROR-CD    PIC X(07).
                   15  WK-C-XGSPA-COM0206.
                       20  WK-C-XGSPA-FILE    PIC X(08).
                       20  WK-C-XGSPA-MODE    PIC X(06).
                       20  WK-C-XGSPA-KEY     PIC X(20).
                       20  WK-C-XGSPA-FS      PIC X(02).
               10  WK-C-XGSPA-VALID-OUTPUT.
                   15  WK-C-XGSPA-GHPARVAL    PIC X(60).
                   15  WK-N-XGSPA-GHPARNUM    REDEFINES
                       WK-C-XGSPA-GHPARVAL    PIC S9(18).
