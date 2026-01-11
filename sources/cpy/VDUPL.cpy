***********************************************************************
* HISTORY OF MODIFICATION:                                            *
***********************************************************************
* 7Q1EM1 - TMPEYM - 16/11/2016 - REM Q1 2017 RELEASE                  *
*              - e-Req 47511 Refinement of                           *
*                Duplicate checking for Inw                          *
*              - Added new fields                                    *
*---------------------------------------------------------------------*
* GH1NVB - NVBUOT - 22/10/2002 - GLOBAL HUBBING.                     *
*              1. CHANGE WK-C-VDUPL-ACCNO,                           *
*                 X(11) TO X(15).                                    *
*---------------------------------------------------------------------*
*---------- COPYBOOK FOR CALLING TREVDUPL - 20/10/89 ----------      *
       01  WK-C-VDUPL-RECORD.                                        
           05  WK-C-VDUPL-INPUT.                                     
GH1NVB*      10  WK-C-VDUPL-ACCNO     PIC X(11).                     
GH1NVB       10  WK-C-VDUPL-ACCNO     PIC X(15).                     
               10  WK-N-VDUPL-REMCUV  PIC X(03).                     
               10  WK-N-VDUPL-REMAMT  PIC S9(13)V9(02).              
7Q1EM1        10  WK-C-VDUPL-TRNREF   PIC X(16).                     
           05  WK-C-VDUPL-OUTPUT.                                    
               10  WK-C-VDUPL-ERROR-CD PIC X(07).                    
               10  WK-C-VDUPL-VALID-OUTPUT.                          
               15  WK-C-VDUPL-DPTRNNO PIC X(12).                     
               10  WK-C-VDUPL-COM0206.                               
               15  WK-C-VDUPL-FILE    PIC X(08).                     
               15  WK-C-VDUPL-MODE    PIC X(06).                     
               15  WK-C-VDUPL-KEY     PIC X(20).                     
               15  WK-C-VDUPL-FS      PIC X(02).                     