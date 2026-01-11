      *============================================================================
      * HISTORY OF MODIFICATION:
      *============================================================================
      * RGVKE - KESAVAN - 26/01/2011 - HK RTGS CHANGES
      *----------------------------------------------------------------------------
       01 WK-C-VDRTGS-RECORD.
           05 WK-C-VDRTGS-INPUT.
      *MANDATORY
               10 WK-C-VDRTGS-PU           PIC X(02).
               10 WK-C-VDRTGS-CUY          PIC X(03).
               10 WK-C-VDRTGS-AMT          PIC 9(13)V99.
      *OPTION 1
               10 WK-C-VDRTGS-CNTRYCD      PIC X(02).
               10 WK-C-VDRTGS-LOCATNCD     PIC X(02).
      *OPTION 2
               10 WK-C-VDRTGS-BANKID       PIC X(11).
      *FILLER
               10 WK-C-VDRTGS-FILLER       PIC X(100).
           05 WK-C-VDRTGS-OUTPUT.
               10 WK-C-VDRTGS-INVALID-OUTPUT.
                  15 WK-C-VDRTGS-ERROR-CD  PIC X(07).
                  15 WK-C-VDRTGS-COM0206.
                     20 WK-C-VDRTGS-FILE   PIC X(08).
                     20 WK-C-VDRTGS-MODE   PIC X(06).
                     20 WK-C-VDRTGS-KEY    PIC X(20).
                     20 WK-C-VDRTGS-FS     PIC X(02).
               10 WK-C-VDRTGS-VALID-OUTPUT.
                  15 WK-C-VDRTGS-RTGSIND   PIC X(01).
                  15 WK-C-VDRTGS-RTGSCUYIND PIC X(01).
      * WK-C-VDRTGS-RTGSTYPE IS A PAYMENT MODE ALSO
                  15 WK-C-VDRTGS-RTGSTYPE  PIC X(06).
      *RTGVKE* WK-C-VDRTGS-RTGSTYPE2 IS USED FOR ONLY HK 202 PAYMENT MODE
      *RTGVKE
                  15 WK-C-VDRTGS-RTGSTYPE2 PIC X(06).
                  15 WK-C-VDRTGS-FILLER    PIC X(94).