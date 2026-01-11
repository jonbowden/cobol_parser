      * LOGG.cpy
      *---------- COPYBOOK FOR CALLING TRFLOGG - 28/07/2011 ----------*
       01 WK-LOGG.
           05 WK-LOGG-INPUT.
              07 WK-LOGG-PARALNO         PIC 9(08).
              07 WK-LOGG-SEQNUM          PIC 9(02).
              07 WK-LOGG-TABTYP          PIC X(03).
              07 WK-LOGG-VALUE-2.
                 10 WK-LOGG-FUNCTID      PIC X(08).
                 10 WK-LOGG-FSTPTYP      PIC X(05).
           05 WK-LOGG-OUTPUT.
              07 WK-LOGG-ERROR-FOUND     PIC X(01).
              07 WK-LOGG-VALUE.
                 10 WK-LOGG-DRMODE       PIC X(08).
                 10 WK-LOGG-CRMODE       PIC X(08).
                 10 WK-LOGG-FUNCTID      PIC X(08).
                 10 WK-LOGG-STPTYP       PIC X(05).
                 10 WK-LOGG-FXRATETY     PIC X(02).
                 10 WK-LOGG-ERRT2        PIC X(01).
                 10 WK-LOGG-ERRCDT       PIC X(01).
                 10 WK-LOGG-ERRCCYT      PIC X(01).
                 10 WK-LOGG-ERRAMT       PIC X(01).
                 10 WK-LOGG-ERRTMD       PIC X(01).
                 10 WK-LOGG-IRSW-ERR     PIC X(01).
              07 WK-LOGG-VALUE-1.
                 10 WK-LOGG-LCAMT        PIC 9(13)V99.
              07 WK-LOGG-DATAAREA.
                 10 WK-LOGG-DATAA1       PIC X(20).
                 10 WK-LOGG-DATAB1       PIC X(20).
                 10 WK-LOGG-DATAB2       PIC X(20).
                 10 WK-LOGG-DATAB3       PIC X(20).
                 10 WK-LOGG-DATAC1       PIC X(20).
                 10 WK-LOGG-DATAD1       PIC X(20).
                 10 WK-LOGG-DATAD2       PIC X(20).
                 10 WK-LOGG-DATAE1       PIC X(20).
                 10 WK-LOGG-DATAE2       PIC X(20).
                 10 WK-LOGG-DATAE3       PIC X(20).
                 10 WK-LOGG-DATAF1A      PIC X(20).
                 10 WK-LOGG-DATAF1B      PIC X(20).
                 10 WK-LOGG-DATAF2       PIC X(20).
              07 WK-LOGG-ACTTAB.
                 10 WK-LOGG-ACTC1        PIC X(10).
                 10 WK-LOGG-ACTB1        PIC X(10).
                 10 WK-LOGG-ACTB2        PIC X(10).
                 10 WK-LOGG-ACTB3        PIC X(10).
                 10 WK-LOGG-ACTC1        PIC X(10).
                 10 WK-LOGG-ACTD1        PIC X(10).
                 10 WK-LOGG-ACTD2        PIC X(10).
                 10 WK-LOGG-ACTE1        PIC X(10).
                 10 WK-LOGG-ACTE2        PIC X(10).
                 10 WK-LOGG-ACTE3        PIC X(10).
                 10 WK-LOGG-ACTF1A       PIC X(10).
                 10 WK-LOGG-ACTF1B       PIC X(10).
                 10 WK-LOGG-ACTF2        PIC X(10).
