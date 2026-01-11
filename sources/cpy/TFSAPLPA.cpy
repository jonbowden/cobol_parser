      * I-O FORMAT:TFSAPLPAR FROM FILE TFSAPLPA OF LIBRARY COMDB
      * system parameter record
           05 TFSAPLPAR REDEFINES TFSAPLPA-RECORD.
           06 TFSAPLPA-PARACD    PIC X(8).
      *        para code
           06 TFSAPLPA-PARAINTG  PIC S9(2).
      *        para length - integer
           06 TFSAPLPA-PARADEC   PIC S9(2).
      *        para length - decimal
           06 TFSAPLPA-ATTRIBUT  PIC X(1).
      *        attribute: n - numeric, a - alphanumeri
           06 TFSAPLPA-DESCRIPT  PIC X(35).
      *        description
           06 TFSAPLPA-PARAVALU  PIC X(20).
      *        para value
           06 TFSAPLPA-SETUPDTE  PIC S9(8).
      *        set up date
           06 TFSAPLPA-LSTUPDTE  PIC S9(8).
      *        last update date
