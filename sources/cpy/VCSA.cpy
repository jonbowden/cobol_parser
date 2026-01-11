      *=================================================================*
      * HISTORY OF MODIFICATION:                                       *
      *=================================================================*
      * 626JTC - DEE JANE - 07/05/2007 - added fields starting from    *
      *           WK-C-VCSA-SEG-CODE upto                              *
      *           WK-C-VCSA-ACCT-ADDR4.                                *
      *-----------------------------------------------------------------*
      * GHRMBA - MBAVILES - 23/06/2003 - INCLUDE CURRENCY CODE         *
      *-----------------------------------------------------------------*
      * GH1NVB - NVBUOT - 02/10/2002 - GLOBAL HUBBING.                 *
      *           1. CHANGE WK-N-VCSA-DOMBRCH                          *
      *              S9(3) TO X(4).                                    *
      *           2. CHANGE WK-C-VCSA-SA-NO                            *
      *              X(11) TO X(15).                                   *
      *-----------------------------------------------------------------*
      *----------  COPYBOOK FOR CALLING TRFVCSA - 12/10/89  ----------*
       01  WK-C-VCSA-RECORD.                                          
           05  WK-C-VCSA-INPUT.                                       
GH1NVB*      10  WK-C-VCSA-SA-NO           PIC X(11).                 
GH1NVB       10  WK-C-VCSA-SA-NO           PIC X(15).                 
GHRMBA       10  WK-C-VCSA-SA-CUY          PIC X(03).                 
           05  WK-C-VCSA-OUTPUT.                                      
               10  WK-C-VCSA-INVALID-OUTPUT.                          
                   15  WK-C-VCSA-ERROR-CD    PIC X(07).               
                   15  WK-C-VCSA-COM8026.                             
                       20  WK-C-VCSA-FILE    PIC X(08).               
                       20  WK-C-VCSA-MODE    PIC X(06).               
                       20  WK-C-VCSA-KEY     PIC X(20).               
                       20  WK-C-VCSA-FS      PIC X(02).               
               10  WK-C-VCSA-VALID-OUTPUT.                            
                   15  WK-C-VCSA-CUSTFNAM    PIC X(35).               
                   15  WK-C-VCSA-ADDR1       PIC X(35).               
                   15  WK-C-VCSA-ADDR2       PIC X(35).               
                   15  WK-C-VCSA-ADDR3       PIC X(35).               
                   15  WK-C-VCSA-ADDR4       PIC X(35).               
                   15  WK-C-VCSA-ADDR5       PIC X(35).               
                   15  WK-C-VCSA-ADDR6       PIC X(35).               
                   15  WK-N-VCSA-RESCD       PIC S9(02).              
                   15  WK-N-VCSA-HOLDCD1     PIC S9(02).              
                   15  WK-N-VCSA-HOLDCD2     PIC S9(02).              
                   15  WK-N-VCSA-HOLDCD3     PIC S9(02).              
                   15  WK-C-VCSA-AOCD        PIC X(04).               
                   15  WK-N-VCSA-STAFFIND    PIC S9(02).              
GH1NVB*          15  WK-N-VCSA-DOMBRCH     PIC S9(03).               
GH1NVB           15  WK-N-VCSA-DOMBRCH     PIC X(04).                
                   15  WK-N-VCSA-AVAIL-BAL   PIC S9(13)V99.           
                   15  WK-N-VCSA-CURR-BAL    PIC S9(13)V99.           
      * start of 626JTC                                               
                   15  WK-C-VCSA-SEG-CODE    PIC X(01).               
                   15  WK-C-VCSA-CTO-CODE    PIC X(10).               
                   15  WK-C-VCSA-REG-ADDR1   PIC X(35).               
                   15  WK-C-VCSA-REG-ADDR2   PIC X(35).               