       01  WK-VTB3.
           03 WK-VTB3-INPUT.
               05 WK-VTB3-PARALNO         PIC 9(08).
               05 WK-VTB3-SEQNUM          PIC 9(02).
           03 WK-VTB3-OUTPUT.
               05 WK-VTB3-ERROR-FOUND     PIC X(01).
               05 WK-VTB3-DATAB3          PIC X(20).
               05 WK-VTB3-ACT.
                   07 WK-VTB3-ACT1        PIC X(01).
                   07 WK-VTB3-ACT2        PIC X(01).
                   07 WK-VTB3-ACT3        PIC X(01).
                   07 WK-VTB3-ACT4        PIC X(01).
               05 WK-VTB3-STPTYP          PIC X(04).
               05 WK-VTB3-PMODE           PIC X(08).
               05 WK-VTB3-BANKID          PIC X(11).
               05 WK-VTB3-INTEMBNKID      PIC X(11).
               05 WK-VTB3-ACBNKID         PIC X(11).
               05 WK-VTB3-BANKAC          PIC X(11).
               05 WK-VTB3-INTEMBNKACC     PIC X(11).
               05 WK-VTB3-ACBNKACC        PIC X(11).
               05 WK-VTB3-ACBNKNM         PIC X(35).
               05 WK-VTB3-ACBNKADR1       PIC X(35).
               05 WK-VTB3-ACBNKADR2       PIC X(35).
               05 WK-VTB3-ACBNKADR3       PIC X(35).
               05 WK-VTB3-SHIFTNO         PIC X(04).
               05 WK-VTB3-BANKACTYP       PIC X(01).
               05 WK-VTB3-ACUDBUI         PIC X(01).