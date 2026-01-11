       01  WK-VTD1.
           05 WK-VTD1-INPUT.
               10 WK-VTD1-PARALNO         PIC 9(08).
               10 WK-VTD1-SEQNUM          PIC 9(02).
               10 WK-VTD1-RBK-IND         PIC X(01).
           05 WK-VTD1-OUTPUT.
               10 WK-VTD1-ERROR-FOUND     PIC X(01).
               10 WK-VTD1-DATAD1          PIC X(20).
           10 WK-VTD1-ACT.
               15 WK-VTD1-ACT1            PIC X(01).
               15 WK-VTD1-ACT2            PIC X(01).
           10 WK-VTD1-PMODE               PIC 9(08).
           10 WK-VTD1-BANKID              PIC X(11).
           10 WK-VTD1-RCBENKID            PIC X(11).
           10 WK-VTD1-SNDCBNKID           PIC X(11).
           10 WK-VTD1-BANKAC              PIC X(11).
           10 WK-VTD1-BANKACTYP           PIC X(01).
           10 WK-VTD1-ACDUBUI             PIC X(01).
           10 WK-VTD1-PAYRNAME            PIC X(35).
           10 WK-VTD1-PAYRADR1            PIC X(35).
           10 WK-VTD1-PAYRADR2            PIC X(35).
           10 WK-VTD1-PAYRADR3            PIC X(35).
           10 WK-VTD1-PAYRADR4            PIC X(35).
           10 WK-VTD1-PAYRADR5            PIC X(35).
           10 WK-VTD1-PAYRADR6            PIC X(35).
           10 WK-VTD1-AOCD                PIC X(04).
           10 WK-VTD1-RESCD               PIC 9(02).
           10 WK-VTD1-DOMBERCH            PIC 9(03).
      *ID1VKE*
      *ID1VKE
           10 WK-VTD1-DOMBRCH             PIC 9(04).
           10 WK-VTD1-HOLCD1              PIC 9(02).
           10 WK-VTD1-HOLCD2              PIC 9(02).
           10 WK-VTD1-HOLCD3              PIC 9(02).