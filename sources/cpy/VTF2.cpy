      * HISTORY OF MODIFICATION:
      * ==========================================================================
      * TAG NAME DATE DESCRIPTION
      * --------------------------------------------------------------------------
      * 5Q2JM1 TMPJZM 26/03/2015 - 14HOREM033 E-REQUEST 41722
      * - RATE CONVERSION LIMIT PER BUSINESS
      * SEGMENT
      * - Extend WK-VTF2-ACCNO to 15 bytes
      * --------------------------------------------------------------------------
       01 WK-VTF2.
           05 WK-VTF2-INPUT.
              10 WK-VTF2-PARALNO    PIC 9(08).
              10 WK-VTF2-SEQNUM     PIC 9(02).
              10 WK-VTF2-BNKENTITY  PIC X(02).
              10 WK-VTF2-CUYCID     PIC X(03).
              10 WK-VTF2-AMOUNT     PIC S9(15)V9(2) COMP-3.
              10 WK-VTF2-ACCNO      PIC X(11).
      5Q2JM1* 10 WK-VTF2-ACCNO      PIC X(15).
      5Q2JM1  05 WK-VTF2-OUTPUT.
              10 WK-VTF2-NO-ERROR   PIC X(01).
              10 WK-VTF2-DATAF2     PIC X(20).
              10 WK-VTF2-FXRATETY   PIC X(02).
              10 WK-VTF2-FXRATE     PIC S9(09)V9(07).
              10 WK-VTF2-FXRATEUT   PIC S9(05).