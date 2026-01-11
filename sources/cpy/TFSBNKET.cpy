      * TFSBNKET.cpybk
      ******************************************************************
      * HISTORY OF MODIFICATION:
      ******************************************************************
      * GH1MBA  27/11/2002 MBAVILES -EXPAND SHIFTNO FIELD
      *-----------------------------------------------------------------
      * GH1NVB - NVBUOT  - 03/10/2002 - GLOBAL HUBBING.
      *                     1. CHANGE TFSBNKET-BNKENTTY
      *                        S9(1) TO X(2)
      *-----------------------------------------------------------------
              GH1NVB*  05 TFSBNKET-RECORD PIC X(316).
GH1MBA     05 TFSBNKET-RECORD PIC X(323).
      *  I-O FORMAT:TFSBNKETR FROM FILE TFSBNKET  OF LIBRAR Y COMLIB
           05 TFSBNKETR REDEFINES TFSBNKET-RECORD.
              GH1NVB*      06 TFSBNKET-BNKENTTY PIC S9(1).
GH1NVB     06 TFSBNKET-BNKENTTY PIC X(2).
      *         BANK ENTITY
           06 TFSBNKET-BNKENTSN PIC X(3).
      *         BANK ENTITY SHORT NAME EG UOB,CKB
           06 TFSBNKET-FULNAME PIC X(35).
      *         FULL NAME
      *      06 TFSBNKET-ADDR1 PIC X(35).
      *         BANK ADDR1
      *      06 TFSBNKET-ADDR2 PIC X(35).
      *         BANK ADDR2
      *      06 TFSBNKET-ADDR3 PIC X(35).
      *         BANK ADDR3
      *      06 TFSBNKET-POSTAL PIC X(10).
      *         BANK POSTAL CODE
           06 TFSBNKET-SWFTBNK PIC X(11).
      *         SWIFT BANK CODE
              GH1MBA*      06 TFSBNKET-SHFTBNK PIC X(4).
GH1MBA     06 TFSBNKET-SHFTBNK PIC X(10).
      *         SHIFT BANK CODE
           06 TFSBNKET-TELNO PIC X(15).
      *         TELEPHONE NO
           06 TFSBNKET-TELEXNO PIC X(15).
      *         TELEX NO
      *      06 TFSBNKET-FAXNO PIC X(15).
      *         FAX NO
      *      06 TFSBNKET-CABLADDR PIC X(35).
      *         CABLE ADDRESS
      *      06 TFSBNKET-ANSBKCD PIC X(10).
      *         ANSWER BACK CODE
      *      06 TFSBNKET-TID PIC X(1).
      *         SWIFT TERMINAL CODE
           06 TFSBNKET-MASLMTP PIC S9(13)V9(2).
      *         MAS PRI DISCOUNTING LIMIT
           06 TFSBNKET-MASLMTS PIC S9(13)V9(2).
      *         MAS SEC DISCOUNTING LIMIT
      *      06 TFSBNKET-BRIND PIC X(1).
      *         BOARD RATE TABLE OK IND -Y/N
           06 TFSBNKET-MASNOSTR PIC S9(9).

      *      MAS NOSTRO A/C NO
      *  06 TFSBNKET-SETUPDTE     PIC S9(8).
      *                           SET UP DATE
      *  06 TFSBNKET-LSTUPDTE     PIC S9(8).
      *                           LAST UPDATE DATE
