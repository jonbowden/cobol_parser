      * AMENDMENT HISTORY
      -    *
      ***********************************************************
      -    ***********
      * NUMBER  DATE      BY      DESCRIPTION
      -    *
      *-----------------------------------------------------------------
      -    ---*
      * GHIMBA  28/11/2002 MBAVILES -EXPAND SHIFTNO FIELD
      -    *
      *-----------------------------------------------------------------
      -    ---*
      * GH1SDF  04/10/2002 TMPSDF  GLOBAL HUBBING:
      -    *
      *                          AMEND FIELD LENGHT SIZE/TYPE OF -
      -    *
      *                          - TFSBANK-TRADEAC FROM X(11) TO
      -    *
      *                            X(15).
      -    *
      *                          - TFSBANK-RECORD FROM X(264) TO
      -    *
      *                            X(268).
      -    *
      ***********************************************************
      -    ***********
              GH1SDF*  05 TFSBANK-RECORD PIC X(264).
GH1MBA     05 TFSBANK-RECORD PIC X(274).
       1-0 FORMAT-TFSBANKR FROM FILE TFSBANK  OF LIBRARY COML  IB
      *
           05 TFSBANKR REDEFINES TFSBANK-RECORD.
           06 TFSBANK-BANKID    PIC X(11).
BANK          ID
           06 TFSBANK-BKNSNAME  PIC X(11).
BANK          SHORT NAME
           06 TFSBANK-BNKFNAME  PIC X(35).
BANK          FULL NAME
           06 TFSBANK-ADDR1     PIC X(35).
BANK          ADDR1
           06 TFSBANK-ADDR2     PIC X(35).
BANK          ADDR2
           06 TFSBANK-ADDR3     PIC X(35).
BANK          ADDR3
           06 TFSBANK-POSTAL    PIC X(10).
BANK          POSTAL CODE
           06 TFSBANK-TELEXNO   PIC X(15).
TELEX         NO
           06 TFSBANK-FAXNO     PIC X(15).
              FAX NO
              GH1MBA*  06 TFSBANK-SHIFTNO  PIC X(4).
GH1MBA     06 TFSBANK-SHIFTNO  PIC X(10).
SHIFT         NO
           06 TFSBANK-SWFTADRI  PIC X(1).
SWIFT         ADDR IND
           06 TFSBANK-CHIPABA   PIC S9(3).
CHIPS         ABA NO
           06 TFSBANK-CHATNO    PIC X(6).
LONDON        CHAT NO
           06 TFSBANK-CHIPNO    PIC X(6).
CHIP          NO
           06 TFSBANK-FEDWIRE   PIC X(9).
              FED WIRE
           06 TFSBANK-CNTRYINCO PIC X(2).
              COUNTRY OF INCORP
           06 TFSBANK-CNTRYCD   PIC X(2).
              COUNTRY CODE
           06 TFSBANK-LOCATNCD  PIC X(2).
              LOCATION CODE
              GH1SDF*  06 TFSBANK-TRADEAC  PIC X(11).
           06 TFSBANK-TRADEAC   PIC X(15).
TRADE         A/C NO
           06 TFSBANK-SETUPDTE  PIC S9(8).
           SET UP DATE
           06 TFSBANK-LSTUPDTE  PIC S9(8).
              LAST UPDATE DATE
