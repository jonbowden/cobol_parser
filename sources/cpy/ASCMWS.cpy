      * ASCWMS.cpy
      * ***************** START OF COMMON TABLE *****************
      * LAST AMENDED 08/25/1989
      *****************************************************************
      * RELATIVE KEY                                       08-25-1989 *
      *****************************************************************
           05 WK-C-COM0206.
              10 WK-C-COM0206-FILE    PIC X(8).
              10 WK-C-COM0206-MODE    PIC X(6).
              10 WK-C-COM0206-KEY     PIC X(20).
              10 WK-C-COM0206-FS      PIC X(02).
              10 WK-N-COM0206-SQL     PIC 9(09).
      *****************************************************************
      * RELATIVE KEY                                       05-23-1989 *
      *****************************************************************
           05 WK-N-REL-KEY            PIC 9(4) VALUE ZERO.
           05 WK-N-REL-SIZE           PIC 9(4) VALUE ZERO.
      *****************************************************************
      * WHEN-COMPILED DATE                                 03-10-1988 *
      *****************************************************************
           05 WK-C-WHEN-COMPILED      PIC X(8).
      *****************************************************************
      * RETURN VALUE FOR MVS - ONLY FOR VSE                12-09-1988 *
      *****************************************************************
      *    05 RETURN-CODE           PIC 99.
      *****************************************************************
      * MONTH LIST                                         12-09-1988 *
      *****************************************************************
           05 WK-C-CHR-MTH.
              10 FILLER               PIC X(54) VALUE
              "JANUARY FEBRUARY MARCH  APRIL  MAY    JUNE   ".
              10 FILLER               PIC X(54) VALUE
              "JULY    AUGUST  SEPTEMBROCTOBER NOVEMBER DECEMBER ".
           05 WK-C-CHR-12 REDEFINES WK-C-CHR-MTH.
              10 WK-C-MTH OCCURS 12   PIC X(9).
      *****************************************************************
      * FILE STATUS CODES                                  01-01-1988 *
      *****************************************************************
           05 WK-C-FILE-STATUS        PIC XX.
              88 WK-C-SUCCESSFUL      VALUE "00".
              88 WK-C-END-OF-FILE     VALUE "10".
              88 WK-C-RECORD-NOT-FOUND VALUE "23".
              88 WK-C-SEQUENCE-ERROR  VALUE "21".
              88 WK-C-DUPLICATE-KEY   VALUE "22".
              88 WK-C-BOUNDARY-VIOLATION VALUE "24".
              88 WK-C-PERMANENT-ERROR VALUE "30", "34".
              88 WK-C-START-BROWSE-EOF VALUE "94".
      **************** END OF COMMON WORKING STORAGE *****************
