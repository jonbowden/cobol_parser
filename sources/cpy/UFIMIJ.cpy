      *****************************************************************
      * AMENDMENT HISTORY:
      *****************************************************************
      * P14B00 06/11/2020 ACNRJR CASH MANAGEMENT ROAD MAP
      * P14 GLMS (Stream 2)
      * PCRMAPDLMC-506
      * Modified to correct record length in copybook
      * From 21669 to 21667
      * - REM UOBI PROJECT: STP CHANGES
      * - EXPAND IMSGBODY / OMSGBODY TO 10000 EACH
      * - INCREASE RECORD LEN (ADD 12K)
      * - EXPAND FROM 5 TO 8 BYTES
      *****************************************************************
      * IDIVKE 02/09/2011
      * MQIMAF 07/10/2008
      * MPIMBA 07/08/2001
      *****************************************************************

       05  UFIMIJ-RECORD                PIC X(8849).
       05  UFIMIJ-RECORD                PIC X(8855).
       05  UFIMIJ-RECORD                PIC X(8856).
       05  UFIMIJ-RECORD                PIC X(20856).
       05  UFIMIJ-RECORD                PIC X(21669).
       05  UFIMIJ-RECORD                PIC X(21667).

      *****************************************************************
      * I-O FORMAT: UFIMIJR
      * FROM FILE UFIMIJ
      * INCOMING MSG INTERFACE JNL
      *****************************************************************

       05  UFIMIJR REDEFINES UFIMIJ-RECORD.

      MPIMBA*
           06  UFIMIJ-PARALNO            PIC S9(8).
      *        INCOMING MSG NO

           06  UFIMIJ-SEQNUM             PIC S9(2).
      *        SEQUENCE NO

           06  UFIMIJ-PREFIX             PIC X(1).
      *        PREFIX
      *        R : REMITTANCE
      *        T : TRADE

      MPIMBA*
           06  UFIMIJ-NPARALNO            PIC S9(8).
      *        NEW PARAL NO

           06  UFIMIJ-INTTXNCD            PIC X(4).
      *        INTERFACE TXN CODE

           06  UFIMIJ-INTTYP              PIC X(1).
      *        INTERFACE TYPE

           06  UFIMIJ-PRCIND              PIC X(1).
      *        PROCESS IND
      *        Y : STRAIGHT THRU
      *        R : REPAIR
      *        F : FORWARD DATE TRN
      *        SPACE : HAVEN'T PROCESSED

           06  UFIMIJ-STRIND              PIC X(1).
      *        STRAIGHT THRU IND
      *        Y - STRAIGHT THRU

           06  UFIMIJ-INTDTE              PIC S9(8).
      *        INTERFACE DATE

           06  UFIMIJ-INTTIM              PIC S9(8).
      *        INTERFACE TIME

           06  UFIMIJ-INTSYS              PIC X(1).
      *        INTERFACE SYSTEM
      *        M : MERVA

           06  UFIMIJ-USERID              PIC X(8).
      *        USER ID

      GHIMBA*
           06  UFIMIJ-BNKENTTY            PIC X(2).
      *        BANK ENTITY

           06  UFIMIJ-PROCUNIT            PIC X(3).
      *        BASE PROCESSING UNIT

           06  UFIMIJ-SENBNKID            PIC X(11).
      *        SENDING BANK ID

           06  UFIMIJ-RCBNKID             PIC X(11).
      *        RECEIVING BANK ID

           06  UFIMIJ-TELEXNO             PIC X(15).
      *        TELEX NO

           06  UFIMIJ-MSGRETCD            PIC X(1).
      *        INTERFACE RETURN CODE

           06  UFIMIJ-MERVAERCD           PIC X(2).
      *        MERVA ERROR CODE

           06  UFIMIJ-MERVAERTX           PIC X(79).
      *        MERVA ERROR TEXT

           06  UFIMIJ-CUYCD               PIC X(3).
      *        CURRENCY CODE

           06  UFIMIJ-AMT                 PIC S9(13)V9(2).
      *        INTERFACE AMOUNT

           06  UFIMIJ-TRNSMTMD             PIC X(1).
      *        TRANSMIT MODE
      *        W : SWIFT
      *        T : TLX
      *        H : SHIFT

           06  UFIMIJ-TLXTESTK             PIC X(1).
      *        TELEX WITH TEST KEY Y/N

           06  UFIMIJ-SWFTMGTY             PIC S9(3).
      *        SWIFT MSG TYPE

           06  UFIMIJ-PRIORTY              PIC S9(2).
      *        PRIORITY

      IDIVKE*
           06  UFIMIJ-SSNO                PIC X(4).
      *        SESSION NO

           06  UFIMIJ-OSNNO               PIC X(6).
      *        OSN NUMBER

           06  UFIMIJ-ISNNO               PIC S9(6).
      *        ISN NO

           06  UFIMIJ-SWIFTOHDR            PIC X(53).
      *        SWIFT OUTPUT HEADER

           06  UFIMIJ-SWIFTTRA             PIC X(585).
      *        SWIFT TRAILER

           06  UFIMIJ-MSGLEN               PIC S9(4).
      *        MESSAGE LENGTH

      MQIMAF*
           06  UFIMIJ-IMSGBDY              PIC X(4000).
           06  UFIMIJ-IMSGBDY-EXT          PIC X(10000).

           06  UFIMIJ-NOCOPY               PIC S9(2).

      MQIMAF*
           06  UFIMIJ-OMSGBDY              PIC X(4000).
           06  UFIMIJ-OMSGBDY-EXT          PIC X(10000).

      IDIVKE*
           06  UFIMIJ-CLSPIND              PIC X(1).
      *        CLS PAYMENT INDICATOR

           06  UFIMIJ-CLSTIME              PIC S9(4).
      *        CLS TIME

           06  UFIMIJ-HDRT19               PIC X(17).
      *        203 HDR TAG 19 SUM OF AMOUNTS

           06  UFIMIJ-HDRT30               PIC X(6).
      *        203 HDR TAG 30 VALUE DATE

           06  UFIMIJ-HDRT52BK1            PIC X(37).
           06  UFIMIJ-HDRT52BK2            PIC X(140).
      *        203 HDR TAG 52 ORDERING BANK

           06  UFIMIJ-HDRT53SN1            PIC X(37).
           06  UFIMIJ-HDRT53SN2            PIC X(140).
      *        203 HDR TAG 53 SENDING BANK

           06  UFIMIJ-HDRT54RC1            PIC X(37).
           06  UFIMIJ-HDRT54RC2            PIC X(140).
      *        203 HDR TAG 54 RECEIVING BANK

           06  UFIMIJ-HDRT72               PIC X(210).
      *        203 HDR TAG 72 SENDER TO RECEIVER INFO

           06  UFIMIJ-T20REF               PIC X(16).
      *        TAG 20 REFERENCE NUMBER

           06  UFIMIJ-TXNCCY               PIC X(3).
      *        TRANSACTION CURRENCY

           06  UFIMIJ-TXNAMT               PIC S9(13)V9(2).
      *        TRANSACTION AMOUNT

           06  UFIMIJ-INTYPE               PIC X(1).
      *        S-SWIFT  C-CLS  M-MIX/COMBINATION

           06  UFIMIJ-HDRAMTE              PIC X(1).
      *        HEADER AMOUNT IN ERROR

           06  UFIMIJ-IMSGTYPE             PIC X(1).
      *        INCOMING MESSAGE TYPE INDICATOR

           06  UFIMIJ-T59OPT               PIC X(1).
      *        TAG 59 OPTION
