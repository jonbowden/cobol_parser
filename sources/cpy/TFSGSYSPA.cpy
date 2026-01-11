      * I-O FORMAT:TFSGSYSPAR FROM FILE TFSGSYSPA OF LIBRARY COMDB
      * system parameter record
           05 TFSGSYSPAR REDEFINES TFSGSYSPA-RECORD.
           06 TFSGSYSPA-GHPARCD PIC X(10).
      *        para code
           06 TFSGSYSPA-GHPARINTG PIC S9(2).
      *        para length - integer
           06 TFSGSYSPA-GHPARDEC PIC S9(2).
      *        para length - decimal
           06 TFSGSYSPA-GHPARATR PIC X(1).
      *        attribute: n - numeric, a - alphanumeri
           06 TFSGSYSPA-GHPARDESC PIC X(35).
      *        description
           06 TFSGSYSPA-GHPARVAL PIC X(60).
      *        para value
           06 TFSGSYSPA-SETUPDTE PIC S9(8).
      *        set up date.
           06 TFSGSYSPA-LSTUPDTE PIC S9(8).
      *        last update date
