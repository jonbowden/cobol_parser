      * HISTORY OF MODIFICATION:                                                 *
      *--------------------------------------------------------------------------*
      * G2BL00 - ACNJR   - 25/03/2019 - CASH MANAGEMENT ROAD MAP - P19           *
      *                           GPI Day4 (Retrofit from GPI Day2b HO)          *
      *                           - ADDED NEW FIELD FOR PMODE                    *
      *--------------------------------------------------------------------------*
      *----------  COPYBOOK FOR CALLING TRFVTB1 - 28/07/2011 ----------*        
       01 WK-VTB1.                                                              
           03 WK-VTB1-INPUT.                                                    
               05 WK-VTB1-PARALNO           PIC 9(08).                          
               05 WK-VTB1-SRQNUM            PIC 9(02).                          
      G2BL00  05 WK-VTB1-BP-PMODE           PIC X(08).                          
           03 WK-VTB1-OUTPUT.                                                   
               05 WK-VTB1-ERROR-FOUND       PIC X(01).                          
               05 WK-VTB1-DATAB1            PIC X(20).                          
           05 WK-VTB1-ACT.                                                     
               07 WK-VTB1-ACT1             PIC X(01).                          
               07 WK-VTB1-ACT2             PIC X(01).                          
               07 WK-VTB1-ACT3             PIC X(01).                          
               07 WK-VTB1-ACT4             PIC X(01).                          
               07 WK-VTB1-ACT5             PIC X(01).                          
               07 WK-VTB1-ACT6             PIC X(01).                          
           05 WK-VTB1-SPTYP                PIC X(04).                          
           05 WK-VTB1-PMODE                PIC X(08).                          
           05 WK-VTB1-BANKID               PIC X(11).                          
           05 WK-VTB1-INTEMBNKID           PIC X(11).                          
           05 WK-VTB1-ACBNKID              PIC X(11).                          
           05 WK-VTB1-BANKAC               PIC X(11).                          
           05 WK-VTB1-INTEMBNKACC          PIC X(11).                          
           05 WK-VTB1-ACBNKACC             PIC X(11).                          
           05 WK-VTB1-ACBNKNM              PIC X(35).                          
           05 WK-VTB1-ACBNKADR1            PIC X(35).                          
           05 WK-VTB1-ACBNKADR2            PIC X(35).                          
           05 WK-VTB1-ACBNKADR3            PIC X(35).                          
           05 WK-VTB1-SHIFTNO              PIC X(04).                          
           05 WK-VTB1-BANKACTYP            PIC X(01).                          
           05 WK-VTB1-ACUDBUI              PIC X(01).                          