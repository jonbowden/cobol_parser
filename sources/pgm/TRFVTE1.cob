       IDENTIFICATION DIVISION.
       ***********************
       PROGRAM-ID. TRFVTE1.
       AUTHOR. TYK.
       DATE-WRITTEN. JUN 04.
       *DESCRIPTION : TABLE E1 VALIDATION.
       *              SUBROUTINE - CREDIT PARTY CHECKING FIELD 56/57 FOR
       *              INCOMING SWIFT MT103 OR RTGS MT103 FCY
       *
       *========================================================================
       * GP4D02 - VENTEH   - 16/10/2020 - CASH MANAGEMENT ROAD MAP - P19
       *              GPI Day4 (POST IMPEM IMPROVEMENT)
       *              (For HK only)
       *              - JIRA PCRMAPKGPI-2109
       *              - STP #5 Inward (Inward receipt,
       *                IAFT & In-as-out) TT/RTGS- STP
       *                by CCY.
       *              - Bypass the STP currency setup
       *                checking to avoid double
       *                RSNCODE
       *------------------------------------------------------------------------
       * GP4D01 - VENTEH   - 22/07/2020 - CASH MANAGEMENT ROAD MAP - P19
       *              GPI Day4 (In-Country Req)
       *              - STP #5 Inward (Inward receipt,
       *                IAFT & In-as-out) TT/RTGS- STP
       *                by CCY.
       *              - JIRA PCRMAPKGPI-1881
       *              - To cater negative scenario when
       *                ACC/CIF/SEG STP limit switch is OFF
       *                proceed to check STP by CCY
       *------------------------------------------------------------------------
       * GP3M01 - VENTEH   - 26/05/2020 - CASH MANAGEMENT ROAD MAP
       *              - gpi DAY 4 (Retro from GPI Day3 HO)
       *              - PCRMAPKGPI-1331
       *              - To add "RTGS" Mode pay on
       *                the evaluation of DR NOSTRO.
       *              - "RTGS" PMODE will be passed from
       *                TRFVTD1 only when:
       *                1. Incoming RTGS
       *                2. Doesnt have Tag53/54
       *                3. Sending BIC is MEPS
       *------------------------------------------------------------------------
       * GP3A02 - VENTEH   - 11/03/2020 - CASH MANAGEMENT ROAD MAP
       *              - GPI Day4 (BAU BUG FIX)
       *              - Expand ACCNO field length to
       *                15 bytes
       *------------------------------------------------------------------------
       * GP4D00 - VENADG   - 14/02/2020 - CASH MANAGEMENT ROAD MAP - P19
       *              GPI Day4 (In-Country Req)
       *              - STP #5 Inward (Inward receipt,
       *                IAFT & In-as-out) TT/RTGS- STP
       *                by CCY.
       *              - To enable ITT STP CCY check
       *------------------------------------------------------------------------

      * GP3A01 - VENADG   - 13/01/2020 - GPI Day4 (Retro from GPI Day3 HO)
      *                      - PCRMAPKGPI-1050 (Retro from GPI Day3 HO
      *                      - Ensure only line 1 and 2 of
      *                        Tag 57 C/D are being validated
      *                        against Tag57 Validation Table
      *--------------------------------------------------------------------
      * G2BL00 - ACNRJR   - 09/10/2019 - CASH MANAGEMENT ROAD MAP - P19
      *                      GPI Day4 (Retro from GPI Day2b HO)
      *                      - PCRMAPKGPI-517
      *                      - Add call of TRFVBACU to
      *                        validate if crediting Nostro
      *                        BIC is a UOB branch. If UOB
      *                        branch, then bypass STP limit
      *                      - Bypass if debitting leg is
      *                        VOSTRO and if cover is already
      *                        received for 103
      * G2BL01 - VENADG   - 02/03/2020 - Removed dependency on Day2b Main
      *                      technical switch upon applying
      *                      STP Limit Bypass Enhancement.
      *--------------------------------------------------------------------
      * GP3A00 - ACNRJR   - 09/10/2019 - GPI Day4 (Retro from GPI Day3 HO)
      *                      - Tag57 Enhancement (Item5b)
      *                      - Previously Tag 57C/D validation
      *                        will proceed even if Tag56 has
      *                        a value.
      *                      - Rectified that Tag 57C/D vali-
      *                        dation will not proceed as long
      *                        as Tag56 has value.
      *                      - To check Tag57 C/D Lines 1-5
      *                        againts Tag Validation Table.
      *                        This is to enable such tags to
      *                        further proceed with STP processing
      *                        if exact matches.
      * GP3M00 - VENTEH   - 05/12/2019 - Inward serial payment Bypass
      *                      STP Limit for Nostro (Item5a)
      *                      - Bypass STP Limit if Dr Leg
      *                        = NOSTRO and CR Leg = VOSTRO/
      *                        NOSTRO
      *--------------------------------------------------------------------
      * REM269 - TMPSRK   - 07/04/2017 - JIRA LOG REM-269
      *                      - STANDARDIZATION OF PROGRAM TO
      *                        RETRIEVE CURRENCY AND COUNTRY
      *                        CODE FROM SYSTEM PARAMETER FILE.
      *--------------------------------------------------------------------
      * CMP3A1 - ACNESEQ  - 14/02/2017 - CASH MANAGEMENT PROJECT 3
      *                      ALLOW MT101 STP PROCESSING
      *--------------------------------------------------------------------
      * CMP3F1 - ACNFLA   - 14/02/2017 - CASH MANAGEMENT PROJECT RELEASE 3
      *                      STP Limit by Account, CIF
      *                      and Segment
      *--------------------------------------------------------------------
      * 7Q1EM1 - TMPFYM   - 25/11/2016 - REM Q1 2017 RELEASE
      *                      - e-Req 47511 Refinement of
      *                        Duplicate checking for Inw

      *       - Recompiled due to changes made in
      *         VSTPL copy book.
      *---------------------------------------------------------------------*
      * 5Q1JM1 - TMPJZM - 23/12/2014 - 14HOREM024/14HOREM029/14HOREM028
      *           Retrofit NON PSTP Reason
      *           Enhancement Project
      *---------------------------------------------------------------------

       ENVIRONMENT DIVISION.
       *********************
       CONFIGURATION SECTION.
           SOURCE-COMPUTER.  IBM-AS400.
           OBJECT-COMPUTER.  IBM-AS400.
           SPECIAL-NAMES.    LOCAL-DATA IS LOCAL-DATA-AREA
                            I-O-FEEDBACK IS I-O-FEEDBACK-AREA
                            UPSI-0 IS UPSI-SWITCH-0
                              ON STATUS IS U0-ON
                              OFF STATUS IS U0-OFF
                            UPSI-1 IS UPSI-SWITCH-1
                              ON STATUS IS U0-ON
                              OFF STATUS IS U0-OFF
                            UPSI-2 IS UPSI-SWITCH-2
                              ON STATUS IS U0-ON
                              OFF STATUS IS U0-OFF
                            UPSI-3 IS UPSI-SWITCH-3
                              ON STATUS IS U0-ON
                              OFF STATUS IS U0-OFF.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TFSSTPL ASSIGN TO DATABASE-TFSSTPL
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
               FILE STATUS IS WK-C-FILE-STATUS.

           SELECT TFSCLSYS ASSIGN TO DATABASE-TFSCLSYS
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WK-C-FILE-STATUS. .

      ID1VKE SELECT TFSBNKET ASSIGN TO DATABASE-TFSBNKET
      ID1VKE     ORGANIZATION IS INDEXED
      ID1VKE     ACCESS MODE IS RANDOM
      ID1VKE     RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
      ID1VKE     FILE STATUS IS WK-C-FILE-STATUS.

      G2BL00 SELECT UFIMJICON ASSIGN TO DATABASE-UFIMJICON
      G2BL00     ORGANIZATION IS INDEXED
      G2BL00     ACCESS MODE IS RANDOM
      G2BL00     RECORD KEY IS EXTERNALLY-DESCRIBED-KEY
      G2BL00         WITH DUPLICATES
      G2BL00     FILE STATUS IS WK-C-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.

       **************
       FD  TFSSTPL
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS WK-C-TFSSTPL.
       01  WK-C-TFSSTPL.
           COPY DDS-ALL-FORMATS OF TFSSTPL.
       01  WK-C-TFSSTPL-1.
           COPY TFSSTPL.

       FD  TFSCLSYS
           LABEL RECORDS ARE OMITTED
           DATA RECORD IS TFSCLSYS-REC.
       01  TFSCLSYS-REC.
           COPY DDS-ALL-FORMATS OF TFSCLSYS.
       01  TFSCLSYS-REC-1.
           COPY TFSCLSYS.

ID1VKE FD  TFSBNKET
ID1VKE     LABEL RECORDS ARE OMITTED
ID1VKE     DATA RECORD IS TFSBNKET-REC.
ID1VKE 01  TFSBNKET-REC.
ID1VKE     COPY DDS-ALL-FORMATS OF TFSBNKET.
ID1VKE 01  TFSBNKET-REC-1.
ID1VKE     COPY TFSBNKET.

G2BL00 FD  UFIMJICON
G2BL00     LABEL RECORDS ARE OMITTED
G2BL00     DATA RECORD IS WK-C-UFIMJICON.
G2BL00 01  WK-C-UFIMJICON.
G2BL00     COPY DDS-ALL-FORMATS OF UFIMJICON.
G2BL00 01  UFIMJICON-REC.
G2BL00     COPY UFIMJICON.

           WORKING-STORAGE SECTION.
       ****************************
       01  WK-C-COMMON.
           COPY ASCMWS.

CMP3F1 01  WK-C-LINK-LIMIT.
CMP3F1 05  WK-C-LINK-AREA-INPUT.
CMP3F1 10  WS-LINK-BNKENTTY     PIC X(02).
CMP3F1 10  WS-LINK-ACCNO        PIC X(15) VALUE 0.
CMP3F1 10  WS-LINK-CCY          PIC X(03) VALUE SPACES.
CMP3F1 10  WS-LINK-AMT          PIC S9(13)V99 VALUE 0.
CMP3F1 10  WS-LINK-REMIND       PIC X(01).
CMP3F1 05  WK-C-LINK-AREA-OUTPUT.
CMP3F1 10  WS-LINK-STATUS       PIC X(02) VALUE SPACES.

       01  TAG56-FORMAT.
           05  TAG56-LINE-1.
               07  TAG56-FIL1  PIC X(2).
               07  TAG56-OPT   PIC X(1).
               07  TAG56-FIL2  PIC X(1).
               07  TAG56-PTID.

       09  TAG56-PTID-1           PIC X(02).
       09  TAG56-PTID-2           PIC X(35).
   05  TAG56-LINE-2               PIC X(35).
   05  TAG56-BIC  REDEFINES TAG56-LINE-2.
       07  TAG56A-SUB1            PIC X(4).
       07  TAG56A-SUB2            PIC X(2).
       07  TAG56A-SUB3            PIC X(2).
       07  TAG56A-SUB4            PIC X(3).
       07  TAG56A-FILLER          PIC X(24).
   05  TAG56-LOC REDEFINES TAG56-LINE-2
                                  PIC X(35).
   05  TAG56-NAME REDEFINES TAG56-LINE-2
                                  PIC X(35).
   05  TAG56-LINE-3               PIC X(35).
   05  TAG56-LINE-4               PIC X(35).
   05  TAG56-LINE-5               PIC X(35).

  01  TAG57-FORMAT.
   05  TAG57-LINE-1.
       07  TAG57-FIL1             PIC X(2).
       07  TAG57-OPT              PIC X(1).
       07  TAG57-FIL2             PIC X(1).
       07  TAG57-PTID.
           09  TAG57-PTID-1       PIC X(02).
           09  TAG57-PTID-2       PIC X(35).
   05  TAG57-LINE-2               PIC X(35).
   05  TAG57-BIC  REDEFINES TAG57-LINE-2.
       07  TAG57A-SUB1            PIC X(4).
       07  TAG57A-SUB2            PIC X(2).
       07  TAG57A-SUB3            PIC X(2).
       07  TAG57A-SUB4            PIC X(3).
       07  TAG57A-FILLER          PIC X(24).
   05  TAG57-LOC REDEFINES TAG57-LINE-2
                                  PIC X(35).
   05  TAG57-NAME REDEFINES TAG57-LINE-2
                                  PIC X(35).
   05  TAG57-LINE-3               PIC X(35).
   05  TAG57-LINE-4               PIC X(35).
   05  TAG57-LINE-5               PIC X(35).

  01  TABLE-ARRAY.
   05  TAB-VAL OCCURS 20 TIMES    PIC X VALUE "X".

  01  TABLE-ARR2.
   05  TAB-VL2 OCCURS 20 TIMES    PIC X VALUE "X".

  01  PATH-P1                     PIC X(20)
                                  VALUE "YXXXXXXXXXXXXXXXXXXXX".
  01  PATH-P2                     PIC X(20)
                                  VALUE "XXYXXXXXXXXXXXXXXXXXXX".
  01  PATH-P3                     PIC X(20)
                                  VALUE "XXXYXXXXXXXXXXXXXXXXXX".
  01  PATH-P4                     PIC X(20)
                                  VALUE "YXXYXXXXXXXXXXXXXXXXXX".

       01 PATH-P5                  PIC X(20)
                                   VALUE "XXXXYYYYXXXXXXXXXXXXXXXX".
       01 PATH-P6                  PIC X(20)
                                   VALUE "XXXXNNYYYYXXXXXXXXXXXXXXXX".
       01 PATH-P7                  PIC X(20)
                                   VALUE "XXXXXXXXXXYYYYXXXXXXXXXXXX".
       01 PATH-P8                  PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P9                  PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P10                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P11                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P12                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P13                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P14                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P15                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P16                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P17                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P18                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".
       01 PATH-P19                 PIC X(20)
                                   VALUE "XXXXXXXXXX(20)XXXXXXXXXXXX".

       01 WK-C-PARADATA.
          05 WK-C-PARAVALU         PIC X(20).
          05 WK-N-PARAVALU         REDEFINES WK-C-PARAVALU
                                   PIC 9(13)V99.
          05 WK-N-IRMPSTP          PIC 9(13)V99.
          05 WK-N-IRM1STP          PIC 9(13)V99.

       01 WK-C-WORK-AREA.
          05 FIRST-TIME            PIC X(01) VALUE "Y".
          05 WS-FLAG1              PIC X(01) VALUE SPACE.
          05 WS-FLAG2              PIC X(01) VALUE SPACE.
          05 WS-ACT1               PIC X(01) VALUE SPACE.
          05 WS-ACT2               PIC X(01) VALUE SPACE.
          05 WS-ACT3               PIC X(01) VALUE SPACE.
          05 WS-SPTYP              PIC X(04) VALUE SPACE.
          05 BKAC56-IND            PIC X(01) VALUE SPACE.
          05 BKAC57-IND            PIC X(01) VALUE SPACE.
          05 STTL56-IND            PIC X(01) VALUE SPACE.
          05 STTL57-IND            PIC X(01) VALUE SPACE.
          05 WS-OKAY               PIC X(01) VALUE SPACE.
GP3A02*   05 WS-ACCNO              PIC X(11) VALUE SPACE.
GP3A02*   05 WS-INTEMBNKACC        PIC X(11) VALUE SPACE.
GP3A02*   05 WS-ACBNKACC           PIC X(11) VALUE SPACE.

       GP3A02  05  WS-ACCNO                PIC X(15) VALUE SPACE.
       GP3A02  05  WS-INTEMBNKACC          PIC X(15) VALUE SPACE.
       GP3A02  05  WS-ACBNKACC             PIC X(15) VALUE SPACE.
       GP3A02  05  WS-BANKID               PIC X(11) VALUE SPACE.
       GP3A02  05  WS-INTEMBNKID           PIC X(11) VALUE SPACE.
       GP3A02  05  WS-ACBNKID              PIC X(11) VALUE SPACE.
       GP3A02  05  WS-LCAMT                PIC 9(13)V99 VALUE ZEROS.
       G2BL00  05  WK-C-BYPASS-LMT-IND     PIC X(01) VALUE SPACE.
       G2BL00  05  WK-C-GPI-SW             PIC X(01) VALUE SPACE.
       G2BL00  05  WK-C-DR-PMODE           PIC X(08) VALUE SPACE.
       G2BL00  05  WK-C-COV-SW             PIC X(01) VALUE SPACE.
       GP3A00  05  WK-C-GPI3-SW            PIC X(01) VALUE SPACE.
       GP3A00  05  WK-C-TAG57-CD-SW        PIC X(01) VALUE SPACE.
       GP3A00  05  WK-C-TAG56-SW           PIC X(01) VALUE SPACE.
       GP3M00  05  WK-C-NSLMT-SW           PIC X(01) VALUE SPACE.
       GP4D00  05  WK-C-STP-CCY-SW         PIC X(01) VALUE SPACE.
       GP4D02  05  WK-C-STP-CCY-IMP-SW     PIC X(01) VALUE SPACE.

       G2BL00  01  WK-C-LIT-GPI.
       G2BL00  05  WK-C-Y                 PIC X(01) VALUE "Y".
       G2BL00  05  WK-C-A                 PIC X(01) VALUE "A".
       GP3A00  05  WK-C-GPI3-SW-PARCD     PIC X(10)
       GP3A00                            VALUE "GPISWITCH3".
       GP3A00  05  WK-C-TAG57-SW-PARCD    PIC X(10)
       GP3A00                            VALUE "GPI3T57SW".
       GP3A00  05  WK-C-TAG57-MT-PARCD    PIC X(10)
       GP3A00                            VALUE "GPI3T57MT".
       GP3M00  05  WK-C-NSLMT-PARCD       PIC X(10)
       GP3M00                            VALUE "GPI3NSLMT".
       GP4D00  05  WK-C-STPCCY-PARCD      PIC X(10)
       GP4D00                            VALUE "GPI4ISTPCY".
       GP4D02  05  WK-C-STPCCY2-PARCD     PIC X(10)
       GP4D02                            VALUE "GPI4STPCY2".

       GP3A00  01  WK-C-MT-TAG57-TBL      PIC X(18) VALUE SPACES.
       GP3A00  05  WK-C-MT-TAG57          PIC X(03) OCCURS 6 TIMES.

       1DIVKIE 01  WK-C-SWIFTBICDCE       PIC X(11) VALUE SPACE.
       1DIVKIE 01  WK-C-RTGSBICDCE        PIC X(11) VALUE SPACE.
       5Q1JM1  01  WK-C-RPPRSN-AREA.
       5Q1JM1  05  WK-C-SEGCODE           PIC X(01) VALUE SPACE.
       5Q1JM1  05  WK-N-STAFFIND          PIC S9(02) VALUE ZEROS.
       5Q1JM1  05  WK-C-ACCNO             PIC X(15) VALUE SPACE.
       5Q1JM1  05  WK-C-QRATE             PIC X(02) VALUE SPACE.
       5Q1JM1  05  WK-C-RPRCODE           PIC X(07) VALUE SPACE.
       5Q1JM1  05  WK-C-TRNNO             PIC X(12) VALUE SPACE.
       5Q1JM1  05  WK-C-FUNCTID           PIC X(08) VALUE SPACE.
       5Q1JM1  01  WK-N-SYSDTE            PIC S9(08) VALUE ZEROS.
       5Q1JM1  01  WK-C-RPRPGM            PIC X(10) VALUE "TRFVTE1".
       CMP3A1  01  WK-101-TAG50H-ACCNO    PIC X(11) VALUE SPACE.
       CMP3F1  01  WS-C-STPLMT-FLAG       PIC X(01) VALUE "N".
       CMP3F1  01  WS-C-M101STPIND        PIC X(01) VALUE "N".
       REM269  01  WK-C-LUCYCD            PIC X(03).

      COPY VSTPL.
      COPY VBAC.
      COPY VBANO.
      COPY VBSET.
      COPY XPARA.
      COPY GERTE.
      COPY LOGG.
ID1VKE COPY XGSPA.
5Q1JM1 COPY RRSN.
G2BL00 COPY GPISTPSW.
G2BL00 COPY VBACU.
GP3A00 COPY VTAG57.
GP4D00 COPY VSTPC.

       LINKAGE SECTION.
       ****************
       COPY VTE1.

       PROCEDURE DIVISION USING WK-VTE1.
       ********************************
           MAIN-MODULE.

CMP3F1    INITIALIZE               WK-C-XGSPA-RECORD.
CMP3F1    MOVE "RSYSSTPLMT"        TO WK-C-XGSPA-GHPARCD.
CMP3F1    CALL "TRFXGSPA"          USING WK-C-XGSPA-RECORD.
CMP3F1    IF WK-C-XGSPA-ERROR-CD   = SPACES
CMP3F1        MOVE WK-C-XGSPA-GHPARVAL TO WS-C-STPLMT-FLAG
CMP3F1    ELSE
CMP3F1        MOVE SPACES          TO WS-C-STPLMT-FLAG
CMP3F1    END-IF.
CMP3F1
CMP3A1    INITIALIZE               WK-C-XGSPA-RECORD.
CMP3A1    MOVE "M101STPIND"        TO WK-C-XGSPA-GHPARCD.
CMP3A1    CALL "TRFXGSPA"          USING WK-C-XGSPA-RECORD.
CMP3A1    IF WK-C-XGSPA-ERROR-CD   = SPACES
CMP3A1        MOVE WK-C-XGSPA-GHPARVAL TO WS-C-M101STPIND
CMP3A1    ELSE
CMP3A1        MOVE SPACES          TO WS-C-M101STPIND
CMP3A1    END-IF.
CMP3A1
CMP3A1        MOVE SPACES          TO WK-101-TAG50H-ACCNO.
CMP3A1    IF WS-C-M101STPIND = "Y"
CMP3A1        IF WK-VTE1-BANKAC NOT = SPACES
CMP3A1            MOVE WK-VTE1-BANKAC TO WK-101-TAG50H-ACCNO
CMP3A1            MOVE SPACES      TO WK-VTE1-BANKAC
CMP3A1        END-IF
CMP3A1    END-IF.

REM269*----------------------------------------------------------------*
       *    GET SYSTEM PARAMETERS FOR LOCAL CURRENCY CODE.            *
       *--------------------------------------------------------------*
       INITIALIZE WK-C-XGSPA-RECORD.

        MOVE "RSYTCLLCUY"        TO WK-C-XGSPA-GHPARCD.
        CALL "TRFXGSPA"          USING WK-C-XGSPA-RECORD.
        IF WK-C-XGSPA-ERROR-CD   = SPACES
            MOVE WK-C-XGSPA-GHPARVAL TO WK-C-LCUYCD
        ELSE
            MOVE SPACES           TO WK-C-LCUYCD
REM269  END-IF

        INITIALIZE WK-VTE1-OUTPUT
                   WK-LOGG
                   WK-C-WORK-AREA.
        MOVE ALL "X" TO TABLE-ARRAY.
        MOVE ALL "X" TO TABLE-ARR2.
        MOVE "Y"    TO FIRST-TIME.

5Q1JM1  MOVE ZEROS TO WK-C-RRSN-QUENUM
5Q1JM1               WK-C-RRSN-QUESUF
5Q1JM1               WK-C-RRSN-STAFFIND
5Q1JM1               WK-C-RRSN-SEQNUM
5Q1JM1               WK-C-RRSN-RPRDTE.

G2BL00  MOVE WK-VTE1-DR-PMODE TO WK-C-DR-PMODE.

        IF FIRST-TIME = "Y"
            OPEN INPUT TFSSTPL
            IF NOT WK-C-SUCCESSFUL
               AND WK-C-FILE-STATUS NOT = "41"
                DISPLAY "TRFSTPL - OPEN FILE ERROR - TFSSTPL"
                DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
            END-IF
            OPEN INPUT TFSCLSYS
            IF NOT WK-C-SUCCESSFUL
               AND WK-C-FILE-STATUS NOT = "41"
                DISPLAY "TFSCLSYS - OPEN FILE ERROR - TFSCLSYS"
                DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
            END-IF

ID1VKE  OPEN INPUT TFSBNKET
        IF NOT WK-C-SUCCESSFUL
           AND WK-C-FILE-STATUS NOT = "41"
            DISPLAY "TRFVTE1 - OPEN FILE ERROR - TFSBNKET"
            DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
        END-IF

        INITIALIZE WK-C-XGSPA-RECORD
        MOVE "STPTRGSBIC" TO WK-C-XGSPA-GHPARCD
        CALL "TRFXGSPA"   USING WK-C-XGSPA-RECORD
        IF WK-C-XGSPA-ERROR-CD = SPACES THEN
            MOVE WK-C-XGSPA-GHPARVAL TO WK-C-RTGSBICCDE
        ELSE
            MOVE SPACES TO WK-C-RTGSBICCDE
        END-IF

ID1VKE  OPEN INPUT UFIMIJCON
G2BL00  IF NOT WK-C-SUCCESSFUL

      G2BL00           AND WK-C-FILE-STATUS NOT = "41"
      G2BL00           DISPLAY "UFIMIJCON - OPEN FILE ERROR - UFIMIJCON"
      G2BL00           DISPLAY "FILE STATUS IS " WK-C-FILE-STATUS
      G2BL00           END-IF
                 END-IF.

                 MOVE WK-VTE1-PARALNO    TO TFSSTPL-PARALNO.
                 MOVE WK-VTE1-SEQNUM     TO TFSSTPL-SEQNUM.

                 READ TFSSTPL
                     KEY IS EXTERNALLY-DESCRIBED-KEY.

                 IF WK-C-SUCCESSFUL
                     MOVE "N"            TO WS-OKAY
                     MOVE TFSSTPL-TAG56  TO TAG56-FORMAT
                     MOVE TFSSTPL-TAG57  TO TAG57-FORMAT
      ID1VKE        MOVE TFSBNKET-BNKENTTY TO TFSBNKET-BNKENTTY
      ID1VKE        READ TFSBNKET KEY IS EXTERNALLY-DESCRIBED-KEY
      ID1VKE        INVALID KEY
      ID1VKE           MOVE SPACES       TO WK-C-SWIFTBICCODE
      ID1VKE        NOT INVALID KEY
      ID1VKE           MOVE TFSBNKET-SWFTBNK TO WK-C-SWIFTBICCODE
      ID1VKE        END-READ
                     PERFORM A100-INITIAL-SUBROUTINE
                        THRU A199-INITIAL-SUBROUTINE-EX
                     PERFORM A200-MOVE-TAG-VALUES
                        THRU A299-MOVE-TAG-VALUES-EX
                     PERFORM B100-PATH-CHOICE THRU B199-PATH-CHOICE-EX
                 END-IF.
                 GO TO Z000-END-PROGRAM.

      A100-INITIAL-SUBROUTINE.
      *----------------------------------------------------------------*
      *    GET DATA FROM "TFSCLSYS" TABLE                              *
      *----------------------------------------------------------------*
                 READ TFSCLSYS.
                 IF NOT WK-C-SUCCESSFUL
                     DISPLAY "TRFVE1    - READ TFSCLSYS ERROR"
                     DISPLAY "FILE STATUS - " WK-C-FILE-STATUS
                     GO TO Z000-END-PROGRAM.

***********MOVE TFSCLSYS-SYSDTE TO L-N-G-SYSDTE.
      5Q1JM1     MOVE TFSCLSYS-SYSDTE TO WK-N-SYSDTE.
***********MOVE TFSCLSYS-LCNTRYCD TO L-C-G-L-CNTRYCD.
***********MOVE TFSCLSYS-LCUYCD TO L-C-G-L-CUYCD.

      *----------------------------------------------------------------*
      *    GET SYSTEM PARAMETERS FOR PSTP & 1STP                       *
      *----------------------------------------------------------------*
                 IF TFSSTPL-IMSGTYPE = "M"
                     MOVE "IRMMSG"      TO WK-C-XPARA-PARACD
                 ELSE
                     MOVE "IRSMSGP"     TO WK-C-XPARA-PARACD
                 END-IF.

      CALL "TRFXPARA"           USING WK-C-XPARA-RECORD.
      IF   WK-C-XPARA-ERROR-CD NOT = SPACES
           DISPLAY "TREEEDT   - TRFXPARA ROUTINE ERROR"
           DISPLAY "FILE STATUS - " WK-C-XPARA-FS
           DISPLAY "ERROR ID    - " WK-C-XPARA-ERROR-CD
           DISPLAY "KEY         - " WK-C-XPARA-INPUT
           GO TO Z000-END-PROGRAM
      ELSE
           MOVE WK-C-XPARA-PARAVALU
                TO   WK-C-PARAVALU
           MOVE WK-N-PARAVALU
                TO   WK-N-IRM1STP
      END-IF.
      IF   TFSSTPL-IMSGTYPE = "M"
           MOVE "IRMMSG1"       TO WK-C-XPARA-PARACD
      ELSE
           MOVE "IRSMSG1"       TO WK-C-XPARA-PARACD
      END-IF.
      CALL "TRFXPARA"           USING WK-C-XPARA-RECORD.
      IF   WK-C-XPARA-ERROR-CD NOT = SPACES
           DISPLAY "TREEEDT   - TRFXPARA ROUTINE ERROR"
           DISPLAY "FILE STATUS - " WK-C-XPARA-FS
           DISPLAY "ERROR ID    - " WK-C-XPARA-ERROR-CD
           DISPLAY "KEY         - " WK-C-XPARA-INPUT
           GO TO Z000-END-PROGRAM
      ELSE
           MOVE WK-C-XPARA-PARAVALU
                TO   WK-C-PARAVALU
           MOVE WK-N-PARAVALU
                TO   WK-N-IRM1STP
      END-IF.
G2BL00*----------------------------------------------------------------*
G2BL00* RETRIEVE GPI TECHNICAL SWITCH FROM SYSTEM PARAMETER FILE      *
G2BL00*----------------------------------------------------------------*
G2BL00
G2BL00     INITIALIZE            WK-C-XGSPA-RECORD
G2BL00                            WK-C-GPI-SW
G2BL00       REPLACING ALPHANUMERIC BY SPACES
G2BL00                 NUMERIC    BY ZEROS.
G2BL00
G2BL00     MOVE "GPISWITCH2"     TO WK-C-XGSPA-GHPARCD.
G2BL00     CALL "TRFXGSPA"       USING WK-C-XGSPA-RECORD.
G2BL00
G2BL00     IF WK-C-XGSPA-ERROR-CD = SPACES
G2BL00        MOVE WK-C-XGSPA-GHPARVAL TO WK-C-GPI-SW
G2BL00     END-IF.
G2BL00*----------------------------------------------------------------*
G2BL00* RETRIEVE GPI STP SWITCH FROM SYSTEM PARAMETER FILE            *
G2BL00*----------------------------------------------------------------*
G2BL00
G2BL00     INITIALIZE            WK-C-XGSPA-RECORD
G2BL00                            SW-STP-LMT-SKP

G2BL00        REPLACING ALPHANUMERIC BY SPACES
G2BL00        NUMERIC BY ZEROS.
G2BL00
G2BL00        MOVE "GP1STPSW"        TO  WK-C-XGSPA-GHPARCD.
G2BL00        CALL "TRFXGSPA"        USING WK-C-XGSPA-RECORD.
G2BL00
G2BL00        IF  WK-C-XGSPA-ERROR-CD = SPACES
G2BL00            MOVE WK-C-XGSPA-GHPARVAL(2:1)
G2BL00                TO  SW-STP-LMT-SKP
G2BL00        END-IF.
GP3A00*----------------------------------------------------------------
GP3A00*-->Retrieve GPI Day 3 Technical Switch
GP3A00        INITIALIZE              WK-C-XGSPA-RECORD
GP3A00                                WK-C-GPI3-SW.
GP3A00
GP3A00        MOVE WK-C-GPI3-SW-PARCD TO  WK-C-XGSPA-GHPARCD.
GP3A00        CALL "TRFXGSPA"         USING WK-C-XGSPA-RECORD.
GP3A00
GP3A00        IF  WK-C-XGSPA-ERROR-CD = SPACES
GP3A00            MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP3A00                TO  WK-C-GPI3-SW
GP3A00        END-IF.
GP3A00
GP3A00*----------------------------------------------------------------
GP3A00*-->Retrieve GPI Day3 Tag57 C/D Enhancement Switch
GP3A00        INITIALIZE              WK-C-XGSPA-RECORD
GP3A00                                WK-C-TAG57-CD-SW.
GP3A00
GP3A00        MOVE WK-C-TAG57-SW-PARCD TO  WK-C-XGSPA-GHPARCD.
GP3A00        CALL "TRFXGSPA"         USING WK-C-XGSPA-RECORD.
GP3A00
GP3A00        IF  WK-C-XGSPA-ERROR-CD = SPACES
GP3A00            MOVE WK-C-XGSPA-GHPARVAL(1:1)
GP3A00                TO  WK-C-TAG57-CD-SW
GP3A00        END-IF.
GP3A00
GP3A00*----------------------------------------------------------------
GP3A00*-->Retrieve GPI Day3 Tag57 C/D Enhancement Eligable MT Types
GP3A00        INITIALIZE              WK-C-XGSPA-RECORD
GP3A00                                WK-C-MT-TAG57-TBL.
GP3A00
GP3A00        MOVE WK-C-TAG57-MT-PARCD TO  WK-C-XGSPA-GHPARCD.
GP3A00        CALL "TRFXGSPA"         USING WK-C-XGSPA-RECORD.
GP3A00
GP3A00        IF  WK-C-XGSPA-ERROR-CD = SPACES
GP3A00            MOVE WK-C-XGSPA-GHPARVAL
GP3A00                TO  WK-C-MT-TAG57-TBL
GP3A00        END-IF.
GP3M00*----------------------------------------------------------------
GP3M00*-->Retrieve GPI Day3 Nostro Bypass STP Limit Enhancement
GP3M00        INITIALIZE              WK-C-XGSPA-RECORD
GP3M00                                WK-C-NSLMT-SW.
GP3M00
GP3M00        MOVE WK-C-NSLMT-PARCD   TO  WK-C-XGSPA-GHPARCD.
GP3M00        CALL "TRFXGSPA"         USING WK-C-XGSPA-RECORD.
GP3M00

      GP3M00    IF  WK-C-XGSPA-ERROR-CD = SPACES
      GP3M00        MOVE WK-C-XGSPA-GHPARVAL(1:1)
      GP3M00            TO  WK-C-NSLMT-SW
      GP3M00    END-IF.
      GP4D00*-->Retrieve GPI Day4 In-Country ITT STP by Currency Switch
      GP4D00        INITIALIZE            WK-C-XGSPA-RECORD
      GP4D00                               WK-C-STP-CCY-SW
      GP4D00
      GP4D00        MOVE WK-C-STPCCY-PARCD  TO  WK-C-XGSPA-GHPARCD.
      GP4D00        CALL "TRFXGSPA"        USING WK-C-XGSPA-RECORD.
      GP4D00
      GP4D00        IF  WK-C-XGSPA-ERROR-CD = SPACES
      GP4D00            MOVE WK-C-XGSPA-GHPARVAL(1:1)
      GP4D00                TO  WK-C-STP-CCY-SW
      GP4D00        END-IF.
      GP4D02*-->Retrieve GPI Day4 ITT STP by Currency Improvement Switch
      GP4D02        INITIALIZE            WK-C-XGSPA-RECORD
      GP4D02                               WK-C-STP-CCY-IMP-SW.
      GP4D02        MOVE WK-C-STPCCY2-PARCD TO  WK-C-XGSPA-GHPARCD.
      GP4D02        CALL "TRFXGSPA"        USING WK-C-XGSPA-RECORD.
      GP4D02
      GP4D02        IF  WK-C-XGSPA-ERROR-CD = SPACES
      GP4D02            MOVE WK-C-XGSPA-GHPARVAL(1:1)
      GP4D02                TO  WK-C-STP-CCY-IMP-SW
      GP4D02        ELSE
      GP4D02            MOVE "N"           TO  WK-C-STP-CCY-IMP-SW
      GP4D02        END-IF.
                A199-INITIAL-SUBROUTINE-EX.
                    EXIT.
                A200-MOVE-TAG-VALUES.
      GP3A00        MOVE "N"           TO  WK-C-TAG56-SW
                    IF  TAG56-BIC NOT = SPACES
                    AND TAG56-OPT = "A"
                        MOVE TAG56-BIC   TO  WS-INTEMBNKID
                                           WS-BANKID
                        MOVE TAG56-PTID  TO  WS-INTEMBNKACC
                                           WS-ACCNO
      GP3A00        MOVE "Y"           TO  WK-C-TAG56-SW
                    END-IF.
                    IF  TAG57-BIC NOT = SPACES
                    AND TAG57-OPT = "A"
                        MOVE TAG57-BIC   TO  WS-ACBNKID
                                           WS-BANKID
                        MOVE TAG57-PTID  TO  WS-ACBNKACC
                                           WS-ACCNO
                    END-IF.
                    IF  TAG57-NAME NOT = SPACES
                    AND TAG57-OPT = "D"
                        MOVE TAG57-PTID  TO  WS-ACBNKACC

      *-----------------------------------------------------------
      *  WS-ACCNO
           MOVE TAG57-NAME    TO WK-VTE1-ACBNKNM
           MOVE TAG57-LINE-3  TO WK-VTE1-ACBNKADR1
           MOVE TAG57-LINE-4  TO WK-VTE1-ACBNKADR2
           MOVE TAG57-LINE-5  TO WK-VTE1-ACBNKADR3
           END-IF.
      
      GP3A00*--->GPI Day3 Tag57 C/D Enhancement
      GP3A00      IF WK-C-GPI3-SW = "Y"
      GP3A00      AND WK-C-TAG57-CD-SW = "Y"
      GP3A00*--   Skip Tag 57C/D validation IF Tag56A switch is ON
      GP3A00      IF WK-C-TAG56-SW = "Y"
      GP3A00          CONTINUE
      GP3A00      ELSE
      GP3A00          IF TAG57-OPT = "C" OR "D"
      GP3A00          AND (TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(1)
      GP3A00          OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(2)
      GP3A00          OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(3)
      GP3A00          OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(4)
      GP3A00          OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(5)
      GP3A00          OR TFSSTPL-SWFTMGTY = WK-C-MT-TAG57(6))
      GP3A00              PERFORM D600-EVAL-TAG57-CD
      GP3A00              THRU D699-EVAL-TAG57-CD-EX
      GP3A00          END-IF
      GP3A00      END-IF
      GP3A00      END-IF.
      
           MOVE TFSSTPL-BNKENTTY TO WK-N-VBAC-BNKENTTY.
           MOVE WS-BANKID       TO WK-C-VBAC-BANKID.
           MOVE TFSSTPL-CUYCD   TO WK-C-VBAC-CUYCD.
           CALL "TRFVBAC" USING WK-C-VBAC-RECORD.
           IF WK-C-VBAC-ACUDBUI NOT = SPACES
               MOVE WK-C-VBAC-ACUDBUI TO WK-VTE1-ACUDBUI
               IF WS-ACCNO = SPACES
                   MOVE WK-C-VBAC-BNKACNO TO WS-ACCNO
               END-IF
           ELSE
               MOVE "D" TO WK-VTE1-ACUDBUI
           END-IF.
      
      A299-MOVE-TAG-VALUES-EX.
           EXIT.
           EJECT
      
      B100-PATH-CHOICE.
           PERFORM D400-BKAC-VALIDATION.
           IF TAG56-OPT = SPACES
           AND TAG56-PTID = SPACES
           AND TAG56-BIC  = SPACES
           OR TAG56-OPT = "A"
ID1VKE*    AND TAG56-BIC = "UOVBSGSGXXX"      
ID1VKE     AND TAG56-BIC = WK-C-SWIFTBICCODE      
CMP3A1     OR (TAG56-OPT = "A"      
CMP3A1     AND TAG56-BIC = WK-C-SWIFTBICCODE
CMP3A1     AND TFSSTPL-SWFTMGTY      = 101
CMP3A1     AND WS-C-M101STPIND       = "Y")
                 PERFORM C100-VALIDATION-PART
                    THRU C199-VALIDATION-PART-EX
           END-IF.
           IF TAG56-OPT           = "A"
1D1VKE*    AND TAG56-BIC     NOT = SPACES       
1D1VKE*    AND TAG56-BIC     NOT = "UOVBSGSGXXX"       
1D1VKE     AND TAG56-BIC     NOT = WK-C-SWIFTBICCDE       
1D1VKE*    AND TAG56-BIC     NOT = "MASGSGSGXXX"       
1D1VKE     AND TAG56-BIC     NOT = WK-C-RTGSBICCDE       
           AND TFSSTPL-IMSGTYPE     = "M"       
REM269**** AND TFSSTPL-CUYCD        = TFSCLSYS-LCUYCD       
REM269     AND TFSSTPL-CUYCD        = WK-C-LCUYCD       
           AND BKAC56-IND            = "Y"       
           OR TAG56-OPT              = "A"       
           AND TAG56-BIC             NOT = SPACES       
1D1VKE*    AND TAG56-BIC            NOT = "UOVBSGSGXXX"       
1D1VKE     AND TAG56-BIC            NOT = WK-C-SWIFTBICCDE       
1D1VKE*    AND TAG56-BIC            NOT = "MASGSGSGXXX"       
1D1VKE     AND TAG56-BIC            NOT = WK-C-RTGSBICCDE       
           AND TFSSTPL-IMSGTYPE      = "S"       
REM269**** AND TFSSTPL-CUYCD        = TFSCLSYS-LCUYCD       
REM269     AND TFSSTPL-CUYCD        NOT = WK-C-LCUYCD       
           AND (STLL56-IND           = "Y"       
           OR BKAC56-IND             = "Y")       
CMP3A1     OR ((TAG56-OPT           = "A"       
CMP3A1     AND TAG56-BIC            NOT = SPACES       
CMP3A1     AND TAG56-BIC            NOT = WK-C-SWIFTBICCDE       
CMP3A1     AND TAG56-BIC            NOT = "MASGSGSGXXX"       
CMP3A1     AND WS-C-M101STPIND      = "Y"       
CMP3A1     AND TFSSTPL-SWFTMGTY     = 101       
CMP3A1*REM269 AND TFSSTPL-IMSGTYPE  = "M"       
CMP3A1*REM269 AND TFSSTPL-CUYCD     = TFSCLSYS-LCUYCD       
REM269     AND TFSSTPL-CUYCD        = WK-C-LCUYCD       
CMP3A1     AND BKAC56-IND           = "Y")       
CMP3A1     OR (TAG56-OPT            = "A"       
CMP3A1     AND TAG56-BIC            NOT = SPACES       
CMP3A1     AND TAG56-BIC            NOT = WK-C-SWIFTBICCDE       
CMP3A1     AND TAG56-BIC            NOT = "MASGSGSGXXX"       
CMP3A1     AND WS-C-M101STPIND      = "Y"       
CMP3A1     AND TFSSTPL-SWFTMGTY     = 101       
CMP3A1     AND TFSSTPL-IMSGTYPE     = "S"       
CMP3A1*REM269AND TFSSTPL-CUYCD      NOT = TFSCLSYS-LCUYCD       
REM269     AND TFSSTPL-CUYCD        NOT = WK-C-LCUYCD       
CMP3A1     AND (STLL56-IND          = "Y"       
CMP3A1     OR BKAC56-IND            = "Y")))       
                 PERFORM C200-VALIDATION-PART
                    THRU C299-VALIDATION-PART-EX
       END-IF.
                 PERFORM D100-VALIDATION THRU D199-VALIDATION-EX.
                 PERFORM D200-VALIDATION THRU D299-VALIDATION-EX.

       B199-PATH-CHOICE-EX.

      EXIT.
      
      C100-VALIDATION-PART.
           IF TAG57-OPT     = SPACES
           AND TAG57-PTID   = SPACES
           AND TAG57-BIC    = SPACES
               MOVE PATH-P1 TO TABLE-ARRAY
               MOVE "Y"    TO WS-OKAY
           END-IF.
CMP3A1*    IF TAG57-OPT     = "A"      
ID1VKE*    AND TAG57-BIC    = "UOVBSGSGXXX"      
ID1VKE*CMP3A1 AND TAG57-BIC = WK-C-SWIFTBICCDE      
CMP3A1     IF (TAG57-OPT    = "A"      
CMP3A1     AND TAG57-BIC    = WK-C-SWIFTBICCDE)      
CMP3A1     OR (TAG57-OPT    = "A"           AND      
CMP3A1     TAG57-BIC        = WK-C-SWIFTBICCDE AN      D
CMP3A1     WS-C-M101STPIND  = "Y"           AND      
CMP3A1     TFSSTPL-SWFTMGTY = 101)      
               MOVE PATH-P2 TO TABLE-ARRAY
               MOVE "Y"    TO WS-OKAY
           END-IF.
           IF TAG57-OPT     = "A"
           AND TAG57-BIC    NOT = SPACES
ID1VKE*    AND TAG57-BIC    NOT = "UOVBSGSGXXX"      
ID1VKE     AND TAG57-BIC    NOT = WK-C-SWIFTBICCDE      
ID1VKE     AND TAG57-BIC    NOT = "MASGSGSGXXX"      
ID1VKE     AND TAG57-BIC    NOT = WK-C-RTGSBICCDE      
REM269**** AND TFSSTPL-IMSGTYPE = "M"      
REM269     AND TFSSTPL-CUYCD = TFSCLSYS-LCUYCD      
REM269     AND TFSSTPL-CUYCD = WK-C-LCUYCD      
REM269     AND BKAC57-IND   = "Y"      
           OR TAG57-OPT     = "A"
           AND TAG57-BIC    NOT = SPACES
ID1VKE*    AND TAG57-BIC    NOT = "UOVBSGSGXXX"      
ID1VKE     AND TAG57-BIC    NOT = WK-C-SWIFTBICCDE      
ID1VKE     AND TAG57-BIC    NOT = "MASGSGSGXXX"      
ID1VKE     AND TAG57-BIC    NOT = WK-C-RTGSBICCDE      
ID1VKE*    AND TFSSTPL-IMSGTYPE = "S"      
REM269**** AND TFSSTPL-CUYCD = TFSCLSYS-LCUYCD      
REM269     AND TFSSTPL-CUYCD NOT = WK-C-LCUYCD      
     	   AND (STTL57-IND   = "Y"      
           OR BKAC57-IND     = "Y")      
CMP3A1     OR ((TAG57-OPT   = "A"      
CMP3A1     AND TAG57-BIC    NOT = SPACES      
CMP3A1     AND TAG57-BIC    NOT = WK-C-SWIFTBICCDE      
CMP3A1     AND TAG57-BIC    NOT = "MASGSGSGXXX"      
CMP3A1     AND WS-C-M101STPIND = "Y"      
CMP3A1     AND TFSSTPL-SWFTMGTY = 101      
CMP3A1     AND TFSSTPL-IMSGTYPE = "M"      
CMP3A1*REM269 AND TFSSTPL-CUYCD = TFSCLSYS-LCUYCD      
REM269     AND TFSSTPL-CUYCD = WK-C-LCUYCD      
CMP3A1     AND BKAC57-IND   = "Y")      
CMP3A1     OR (TAG57-OPT    = "A"      
CMP3A1     AND TAG57-BIC    NOT = SPACES      

CMP3A1    AND TAG57-BIC          NOT = WK-C-SWIFTBICCDE      
CMP3A1    AND TAG57-BIC          NOT = "MASGSGSGXXX"      
CMP3A1    AND WS-C-M101STPIND    = "Y"      
CMP3A1    AND TFSSPTL-SWFMTGTY   = 101      
CMP3A1    AND TFSSPTL-IMSGTYPE   = "S"      
CMP3A1*REM269   AND TFSSPTL-CUYCD      NOT = TFSCLSYS-LCUYCD
REM269          AND TFSSPTL-CUYCD      NOT = WK-C-LCUYCD
CMP3A1    AND (STTL57-IND        = "Y"
CMP3A1    OR  BKAC57-IND         = "Y"))
                 MOVE PATH-P3          TO TABLE-ARRAY
                 MOVE "Y"              TO WS-ACT1
                                       WS-OKAY

                 END-IF.
      C199-VALIDATION-PART-EX.
                 EXIT.
                 EJECT

      C200-VALIDATION-PART.
                 IF TAG57-OPT          NOT = "A"
                 AND NOT(TAG57-OPT     = SPACES
                 AND TAG57-PTID        = SPACES
                 AND TAG57-BIC         = SPACES)
                 MOVE PATH-P4          TO TABLE-ARRAY
                 MOVE "Y"              TO WS-ACT2
                                       WS-OKAY

                 ELSE
                 IF TAG57-OPT          = "A"
                 AND TAG57-BIC         NOT = SPACES
1D1VKE*          AND TAG57-BIC         NOT = "UOVBGSBGXXX"
1D1VKE*          AND TAG57-BIC         NOT = WK-C-SWIFTBICCDE
1D1VKE*          AND TAG57-BIC         NOT = "MASGSGSGXXX"
1D1VKE*          AND TAG57-BIC         NOT = WK-C-RTGSBICCDE
REM269****       AND TFSSPTL-CUYCD     = TFSCLSYS-LCUYCD
REM269           AND TFSSPTL-CUYCD     = WK-C-LCUYCD
                 OR TAG57-OPT          = "A"
                 AND TAG57-BIC         NOT = SPACES
1D1VKE*          AND TAG57-BIC         NOT = "UOVBGSBGXXX"
1D1VKE*          AND TAG57-BIC         NOT = WK-C-SWIFTBICCDE
1D1VKE*          AND TAG57-BIC         NOT = "MASGSGSGXXX"
1D1VKE*          AND TAG57-BIC         NOT = WK-C-RTGSBICCDE
REM269****       AND TFSSPTL-CUYCD     NOT = TFSCLSYS-LCUYCD
REM269           AND TFSSPTL-CUYCD     NOT = WK-C-LCUYCD
CMP3A1    OR (TAG57-OPT          = "A"     
CMP3A1    AND TAG57-BIC          NOT = SPACES     
CMP3A1    AND TAG57-BIC          NOT = WK-C-SWIFTBICCDE     
CMP3A1    AND TAG57-BIC          NOT = "MASGSGSGXXX"     
CMP3A1    AND WS-C-M101STPIND    = "Y"     
CMP3A1    AND TFSSPTL-SWFMTGTY   = 101     
CMP3A1*REM269   AND TFSSPTL-CUYCD      = TFSCLSYS-LCUYCD)
REM269          AND TFSSPTL-CUYCD      = WK-C-LCUYCD)
CMP3A1    OR TAG57-OPT           = "A"
CMP3A1    AND TAG57-BIC          NOT = SPACES      
CMP3A1    AND TAG57-BIC          NOT = WK-C-SWIFTBICCDE      
CMP3A1    AND TAG57-BIC          NOT = "MASGSGSGXXX"      

CMP3A1*REM269  AND TFSSTPL-CUYCD  NOT = TFSCLSYS-LCUYCD      
REM269   AND TFSSTPL-CUYCD  NOT = WK-C-LCUYCD      
CMP3A1   AND WS-C-M101STPIND = "Y"      
CMP3A1   AND TFSSTPL-SWFTMGTY = 101)      
               IF TFSSTPL-IMGSTYPE = "M"
               AND BKAC57-IND = "Y"
               OR TFSSTPL-IMGSTYPE = "S"
               AND (STTL57-IND = "Y"
               OR BKAC57-IND = "Y")
                  MOVE PATH-P5 TO TABLE-ARRAY
                  MOVE "Y" TO WS-ACT3
                              WS-OKAY
               ELSE
                  MOVE PATH-P6 TO TABLE-ARRAY
                  MOVE "Y" TO WS-ACT3
                              WS-OKAY
               END-IF
            END-IF
         END-IF.
         C299-VALIDATION-PART-EX.
         EXIT.
         EJECT

      D100-VALIDATION.
               MOVE "Y" TO WS-FLAG1.
               IF TABLE-ARRAY = ALL "X"
                  MOVE "N" TO WS-OKAY
               5Q1JM1   INITIALIZE WK-C-RPRRSN-AREA
               5Q1JM1   MOVE "RSN0116" TO WK-C-RPRCODE
               5Q1JM1   PERFORM D500-PROCESS-RPRRSN
               5Q1JM1   THRU D599-PROCESS-RPRRSN-EX
               END-IF.

G2BL00**If GPI 2B technical switch and GPI STP limit switch are ON,      
G2BL00**check if crediting NOSTRO BIC is UOB branch. If it is, bypass      
G2BL00**limit check. If not, continue with the BAU behavior.      
G2BL01*G2BL00IF WK-C-GPI-SW = WK-C-Y      
G2BL00   IF SW-STP-LMT-SKP-Y      
G2BL00   AND TFSSTPL-SWFTMGTY = "103"      
G2BL00      PERFORM D110-VALIDATE-STP-BYPASS      
G2BL00      THRU D119-VALIDATE-STP-BYPASS-EX      
G2BL00   ELSE      
G2BL00      MOVE SPACES TO WK-C-BYPASS-LMT-IND      
G2BL00   END-IF      
G2BL00   IF WK-C-BYPASS-LMT-IND = WK-C-Y      
G2BL00      GO TO D101-BYPASS-STP-LMT      
G2BL00   END-IF.      
G2BL01*G2BL00END-IF.      

CMP3F1   IF WS-C-STPLMT-FLAG = "Y"      
CMP3F1      MOVE "A1" TO WS-LINK-STATUS      
CMP3F1   IF WS-ACCNO NOT = SPACES      
CMP3A1   OR (WK-101-TAGS0H-ACCNO NOT = SPACES AND      
         WS-C-M101STPIND = "Y" AND      

CMP3A1      TFSSTPL-SWFTMGTY = "101")
CMP3F1      INITIALIZE WK-C-RPRRSN-AREA      
CMP3F1      INITIALIZE WK-C-LINK-LIMIT      
CMP3F1      MOVE TFSSTPL-BNKENTITY       TO WS-LINK-BNKENTITY      
CMP3A1      IF TFSSTPL-SWFTMGTY = "101"      
CMP3A1          MOVE WK-101-TAG50H-ACCNO      
CMP3A1               TO WS-LINK-ACCNO      
CMP3A1      ELSE      
CMP3A1          MOVE WS-ACCNO            TO WS-LINK-ACCNO      
CMP3A1      END-IF      
CMP3F1      MOVE TFSSTPL-CUYCD          TO WS-LINK-CCY      
CMP3F1      MOVE TFSSTPL-AMT            TO WS-LINK-AMT      
CMP3F1      MOVE "I"                    TO WS-LINK-REMIND      
CMP3F1      CALL "TRFVLM" USING WK-C-LINK-LIMIT      
CMP3F1      EVALUATE WS-LINK-STATUS      
CMP3F1          WHEN "XX"      
CMP3F1              MOVE "N"            TO WS-OKAY      
CMP3F1              MOVE "RSN0311"      TO WK-C-RPRCODE      
CMP3F1              PERFORM D500-PROCESS-RPRRSN      
CMP3F1                  THRU D599-PROCESS-RPRRSN-EX      
CMP3F1      END-EVALUATE      
CMP3F1      END-IF      
CMP3F1  END-IF.      

G2BL00  0101-BYPASS-STP-LMT.      
              IF TAB-VAL(01) NOT = "X"
                  MOVE TAB-VAL(01) TO TAB-VL2(01)
                  PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(02) NOT = "X"
                  MOVE TAB-VAL(02) TO TAB-VL2(02)
                  PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(03) NOT = "X"
                  MOVE TAB-VAL(03) TO TAB-VL2(03)
                  PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(04) NOT = "X"
                  MOVE TAB-VAL(04) TO TAB-VL2(04)
                  PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
              IF TAB-VAL(05) NOT = "X"
                  MOVE TAB-VAL(05) TO TAB-VL2(05)
                  PERFORM D300-LOGGING THRU D399-LOGGING-EX
              END-IF.
REM269**** IF TFSSTPL-CUYCD NOT = TFSCLSYS-LCUYCD
REM269      IF TFSSTPL-CUYCD NOT = WK-C-LCUYCD
                  MOVE "BT" TO WK-C-GERTE-RTE-TYP
                  MOVE TFSSTPL-CUYCD TO WK-C-GERTE-CUYCD
                  MOVE TFSSTPL-BNKENTITY TO WK-N-GERTE-BNKENTITY
                  CALL "TRFGERTE" USING WK-C-GERTE-RECORD
                  COMPUTE WS-LCAMT ROUNDED = TFSSTPL-AMT
                      * WK-N-GERTE-EXCH-RTE
                      / WK-N-GERTE-FXRATETU

           ELSE
               MOVE TFSSTPL-AMT         TO WS-LCAMT
           END-IF.
           IF TAB-VAL(06) NOT = "X" AND WS-OKAY = "Y"
               MOVE "Y"                TO TAB-VL2(06)
CMP3F1         IF WS-LCAMT <= WK-N-IRM1STP
CMP3F1             IF (WS-C-STPLMT-FLAG = "N"
CMP3F1                 AND WS-LCAMT <= WK-N-IRMSTP)
CMP3F1                 OR (WS-C-STPLMT-FLAG = "Y"
CMP3F1                 AND ((WS-LCAMT <= WK-N-IRMSTP
CMP3F1                 AND WS-LINK-STATUS = "A1")
CMP3F1                 OR (WS-LINK-STATUS = "A0")))
G2BL00             OR (SW-STP-LMT-SKP-Y
G2BL01*G2BL00         AND WK-C-GPI-SW = WK-C-Y
G2BL00             AND WK-C-BYPASS-LMT-IND = WK-C-Y)
                       MOVE "PSTP"     TO WS-STPTYP
                       MOVE "N"        TO TAB-VL2(06)
                       MOVE "X"        TO TAB-VL2(07)
               END-IF
*GP4D00*-->If within Parm STP Limit and w/out Acc/CIF/Seg STP Limit setup
*GP4D00*-->further check ITT STP CCY table.
GP4D00         IF WK-C-STP-CCY-SW = "Y"
GP4D00             AND WS-LCAMT <= WK-N-IRMSTP
GP4D00*GP4D01     AND WS-C-STPLMT-FLAG = "Y"
GP4D00*GP4D01     AND WS-LINK-STATUS = "A1"
GP4D01             AND ((WS-C-STPLMT-FLAG = "Y"
GP4D01             AND WS-LINK-STATUS = "A1")
GP4D01             OR WS-C-STPLMT-FLAG NOT = "Y")
                   PERFORM D120-EVAL-STP-CCY
GP4D00             THRU D120-EVAL-STP-CCY-EX
GP4D00         END-IF
               PERFORM D300-LOGGING     THRU D399-LOGGING-EX
CMP3F1         IF WS-LINK-STATUS = "A0"
CMP3F1             AND WS-C-STPLMT-FLAG = "Y"
CMP3F1             GO TO D199-VALIDATION-EX
CMP3F1         END-IF
           END-IF.
           IF TAB-VAL(07) NOT = "X" AND WS-OKAY = "Y"
G2BL00         IF (SW-STP-LMT-SKP-Y
G2BL01*G2BL00     AND WK-C-GPI-SW = WK-C-Y
G2BL00         AND WK-C-BYPASS-LMT-IND = WK-C-Y)
G2BL00             CONTINUE
               ELSE
                   IF WS-LCAMT > WK-N-IRM1STP
                       MOVE "2STP"     TO WS-STPTYP
                       MOVE "Y"        TO TAB-VL2(07)
5Q1JM1             INITIALIZE WK-C-RPRRSN-AREA
5Q1JM1             MOVE "RSN0023" TO WK-C-RPRCODE
5Q1JM1             PERFORM D500-PROCESS-RPRRSN
5Q1JM1             THRU D599-PROCESS-RPRRSN-EX
               ELSE
                   IF WS-LCAMT > WK-N-IRMSTP
CMP3F1                 OR (WS-C-STPLMT-FLAG = "Y"
CMP3F1                 AND (WS-LINK-STATUS = "A0"
CMP3F1     OR WS-LINK-STATUS        = "AC"
CMP3F1     OR WS-LINK-STATUS        = "AS"))       
          MOVE "1STP" TO WS-STPTYP       
          MOVE "N"    TO TAB-VL2(07)       
5Q1JM1     INITIALIZE WK-C-RPRRSN-AREA       
5Q1JM1     MOVE "RSN0028" TO WK-C-RPRCODE       
5Q1JM1     PERFORM D500-PROCESS-RPRRSN       
             THRU D599-PROCESS-RPRRSN-EX       
5Q1JM1 END-IF       
       END-IF
       G2BL00 END-IF
                 PERFORM D300-LOGGING
                    THRU D399-LOGGING-EX
       END-IF.
       D199-VALIDATION-EX.
                 EXIT.
                 EJECT

G2BL00*----------------------------------------------------------------*       
G2BL00* VALIDATE IF CREDITING NOSTRO BIC IS UOB BRANCH BY CALLING      *       
G2BL00* TRFVBACU TO READ TFSBNKACE FILE.                               *       
G2BL00*----------------------------------------------------------------*       
G2BL00 D110-VALIDATE-STP-BYPASS.       
G2BL00       
G2BL00     MOVE SPACES              TO WK-C-BYPASS-LMT-IND.       
G2BL00       
G2BL00*--Bypass STP Limit if Debit Leg is a VOSTRO account       
G2BL00     IF WK-C-DR-PMODE = "CA"       
G2BL00        OR WK-C-DR-PMODE = "FCCA"       
G2BL00        MOVE WK-C-Y          TO WK-C-BYPASS-LMT-IND       
G2BL00        GO TO D119-VALIDATE-STP-BYPASS-EX       
G2BL00     END-IF.       
G2BL00       
GP3M00*--Bypass STP Limit if Dr Leg = NOSTRO and CR Leg = NOSTRO/VOSTRO       
GP3M00     IF WK-C-GP13-SW = WK-C-Y       
GP3M00        AND WK-C-NSLMT-SW = WK-C-Y       
GP3M00*GP3M01 IF WK-C-DR-PMODE = "NOSTRO"       
GP3M01        IF (WK-C-DR-PMODE = "NOSTRO"       
GP3M01        OR WK-C-DR-PMODE(1:4) = "RTGS")       
GP3M00           AND (WK-C-VBAC-ACCTYP = "C"       
GP3M00           OR WK-C-VBAC-ACCTYP = "F"       
GP3M00           OR WK-C-VBAC-ACCTYP = "N"       
GP3M00           OR WK-C-VBAC-ACCTYP = "V")       
GP3M00           MOVE WK-C-Y        TO WK-C-BYPASS-LMT-IND       
GP3M00           GO TO D119-VALIDATE-STP-BYPASS-EX       
GP3M00        END-IF.       
GP3M00     END-IF.       
G2BL00       
G2BL00*--Check the cover received indicator       
G2BL00     IF TFSSTPL-SWFTMGTY = "103"       
G2BL00        MOVE SPACE            TO WK-C-COV-SW       
G2BL00        PERFORM R100-READ-UFIMJICON       
                THRU R199-READ-UFIMJICON-EX       
G2BL00        IF WK-C-COV-SW = WK-C-Y       
G2BL00           MOVE WK-C-Y        TO WK-C-BYPASS-LMT-IND       
G2BL00        END-IF       
G2BL00     END-IF.          
G2BL00           GO TO D119-VALIDATE-STP-BYPASS-EX      
G2BL00           END-IF.      
G2BL00       END-IF.      
G2BL00*----------------------------------------------------------------*      
G2BL00*--Check if Sending BankID is a Nostro - UOB Branch             *      
G2BL00           INITIALIZE          WK-C-VBACU-RECORD      
G2BL00               REPLACING NUMERIC BY ZEROS      
G2BL00               ALPHANUMERIC BY SPACES.      
G2BL00           MOVE TFSSTPL-SENBNKID  TO WK-C-VBACU-BANKID.      
G2BL00           CALL "RTFVBACU"       USING WK-C-VBACU-RECORD.      
G2BL00      
G2BL00           IF WK-C-VBACU-ERROR-CD = SPACES      
G2BL00               IF  WK-C-VBACU-UOBBRH = WK-C-Y      
G2BL00                   MOVE WK-C-Y   TO WK-C-BYPASS-LMT-IND      
G2BL00               ELSE      
G2BL00                   MOVE SPACES   TO WK-C-BYPASS-LMT-IND      
G2BL00               END-IF      
G2BL00           END-IF.      
G2BL00      
G2BL00       D119-VALIDATE-STP-BYPASS-EX.      
G2BL00           EXIT.      
G2BL00           EJECT.      
GP4D00       D120-EVAL-STP-CCY.      
GP4D00*----------------------------------------------------------------*      
GP4D00* This routine will call TRFVSTPC to check if CCY is eligable   *      
GP4D00* for STP and if AMT is within CCY STP Limit.                   *      
GP4D00*    A0 = Currency is setup and within limit                    *      
GP4D00*    A1 = Currency is setup however exceeds STP Limit           *      
GP4D00*    A2 = Currency is NOT setup                                 *      
GP4D00*----------------------------------------------------------------*      
GP4D00           INITIALIZE          WK-C-VSTPC-RECORD      
GP4D00                                   WK-C-RPRRSN-AREA.      
GP4D00      
GP4D00           MOVE TFSSTPL-IMSGTYPE TO WK-C-VSTPC-I-IMSGTYPE.      
GP4D00           MOVE TFSSTPL-CUYCD    TO WK-C-VSTPC-I-CUYCD.      
GP4D00           MOVE TFSSTPL-AMT      TO WK-N-VSTPC-I-AMT.      
GP4D00           CALL "TRFVSTPC"       USING WK-C-VSTPC-RECORD.      
GP4D00      
GP4D00           IF WK-C-VSTPC-ERROR-CD = SPACES      
GP4D00               CONTINUE      
GP4D00           ELSE      
GP4D00               GO TO D120-EVAL-STP-CCY-EX      
GP4D00           END-IF.      
GP4D00      
GP4D00           EVALUATE WK-C-VSTPC-STATUS      
GP4D00               WHEN "A0"      
GP4D00                   GO TO D120-EVAL-STP-CCY-EX      
GP4D00               WHEN "A1"      
GP4D00*--------->RSN0370 - Inward STP CCY: Beyond CCY STP Limit      
GP4D00                   MOVE "RSN0370" TO WK-C-RPRCODE      
GP4D02               WHEN "A2"      
GP4D02*--------->Improvement: Checking of STP currency has move to ITT      
GP4D02*           Handler      

GP4D02    IF WK-C-STP-CCY-IMP-SW = "Y"      
GP4D02        GO TO D120-EVAL-STP-CCY-EX      
GP4D02    ELSE      
GP4D00*-------->RSN0369 - Inward STP CCY: Non STP Currency.      
GP4D00        MOVE "RSN0369"       TO WK-C-RPRCODE      
GP4D02    END-IF      
GP4D00    WHEN OTHER      
GP4D02        GO TO D120-EVAL-STP-CCY-EX      
GP4D00    END-EVALUATE.      
GP4D00      
GP4D00    MOVE SPACES           TO WS-STPTYP.      
GP4D00    MOVE "N"              TO TAB-VL2(06)      
GP4D00                           TAB-VL2(07)      
GP4D00                           WS-OKAY.      
GP4D00      
GP4D00    PERFORM D500-PROCESS-RPRRSN      
GP4D00        THRU D599-PROCESS-RPRRSN-EX.      
GP4D00      
GP4D00    D120-EVAL-STP-CCY-EX.      
GP4D00        EXIT.      
GP4D00    /      
GP4D00    D200-VALIDATION.      
GP4D00      
GP4D00        MOVE WS-BANKID        TO WK-VTE1-BANKID.      
GP4D00        MOVE WS-INTEMBNKID    TO WK-VTE1-INTEMBNKID.      
GP4D00        MOVE WS-ACBNKID       TO WK-VTE1-ACBNKID.      
GP4D00        MOVE WS-ACCNO         TO WK-VTE1-BANKAC.      
GP4D00        MOVE WK-C-VBAC-ACCTYP TO WK-VTE1-BANKACCTYP.      
GP4D00        MOVE WS-INTEMBNKACC   TO WK-VTE1-INTEMBNKACC.      
GP4D00        MOVE WS-ACBNKACC      TO WK-VTE1-ACBNKACC.      
GP4D00        MOVE TABLE-ARR2       TO WK-VTE1-DATAE1.      
GP4D00        MOVE "TT"             TO WK-VTE1-PMODE.      
GP4D00      
GP4D00        IF WS-OKAY = "Y"      
GP4D00            MOVE WS-ACT1      TO WK-VTE1-ACT1      
GP4D00            MOVE WS-ACT2      TO WK-VTE1-ACT2      
GP4D00            MOVE WS-ACT3      TO WK-VTE1-ACT3      
GP4D00            MOVE WS-STPTYP    TO WK-VTE1-STPTYP      
GP4D00            MOVE "N"          TO WK-VTE1-ERROR-FOUND      
GP4D00        ELSE      
GP4D00            MOVE SPACES       TO WK-VTE1-ACT1      
GP4D00                               WK-VTE1-ACT2      
GP4D00                               WK-VTE1-ACT3      
GP4D00                               WK-VTE1-STPTYP      
GP4D00            MOVE "Y"          TO WK-VTE1-ERROR-FOUND      
GP4D00        END-IF.      
GP4D00        MOVE "N"              TO WS-FLAG1.      
GP4D00        PERFORM D300-LOGGING  THRU D399-LOGGING-EX.      
GP4D00    D299-VALIDATION-EX.      
GP4D00        EXIT.      
GP4D00    EJECT      
GP4D00      
GP4D00    D300-LOGGING.      
GP4D00        MOVE WK-VTE1-PARALNO  TO WK-LOGG-PARALNO.      

      MOVE WK-VTE1-SEQNUM     TO WK-LOGG-SEQNUM.
      MOVE TABLE-ARR2         TO WK-LOGG-DATAE1.
      MOVE "E1"               TO WK-LOGG-TABTYP.
      MOVE WK-VTE1-ACT        TO WK-LOGG-ACTE1.
      CALL "TRFLOGGCL" USING WK-LOGG
                               WS-FLAG1
                               WS-FLAG2.
      IF WK-LOGG-ERROR-FOUND = "Y"
         GO TO D399-LOGGING-EX
      END-IF.
  D399-LOGGING-EX.
      EXIT.
      EJECT
  D400-BKAC-VALIDATION.
      IF NOT(TAG56-OPT  = SPACES
         AND TAG56-PTID = SPACES
         AND TAG56-BIC  = SPACES)
         MOVE TFSSTPL-BNKENTTY TO WK-N-VBAC-BNKENTTY
         MOVE TFSSTPL-CUYCD    TO WK-C-VBAC-CUYCD
         MOVE TAG56-BIC        TO WK-C-VBAC-BANKID
         CALL "TRFVBAC" USING WK-C-VBAC-RECORD
         IF WK-C-VBAC-ERROR-CD = SPACES
            AND TAG56-PTID     = SPACES
            OR WK-C-VBAC-ERROR-CD = SPACES
            AND TAG56-PTID     NOT = SPACES
            MOVE "Y" TO BKAC56-IND
         ELSE
            MOVE TFSSTPL-BNKENTTY TO WK-N-VBSET-BNKENTTY
            MOVE TFSSTPL-CUYCD    TO WK-C-VBSET-CUYCD
            MOVE TAG56-BIC        TO WK-C-VBSET-BANKID
            CALL "TRFVBSET" USING WK-C-VBSET-RECORD
            IF WK-C-VBSET-STLMTBNK NOT = SPACES
               MOVE "Y" TO STTL56-IND
            ELSE
               MOVE "N" TO STTL56-IND
            BKAC56-IND
         END-IF
      END-IF.
      END-IF.
      IF NOT(TAG57-OPT  = SPACES
         AND TAG57-PTID = SPACES
         AND TAG57-BIC  = SPACES)
         MOVE TFSSTPL-BNKENTTY TO WK-N-VBAC-BNKENTTY
         MOVE TFSSTPL-CUYCD    TO WK-C-VBAC-CUYCD
         MOVE TAG57-BIC        TO WK-C-VBAC-BANKID
         CALL "TRFVBAC" USING WK-C-VBAC-RECORD
         IF WK-C-VBAC-ERROR-CD = SPACES
            AND TAG57-PTID     = SPACES
            OR WK-C-VBAC-ERROR-CD = SPACES
            AND TAG57-PTID     NOT = SPACES
            AND WK-C-VBAC-BNKACNO = TAG57-PTID

       MOVE "Y" TO BKAC57-IND
       ELSE
           MOVE TFSSTPL-BNKENTTY  TO WK-N-VBSET-BNKENTTY
           MOVE TFSSTPL-CUYCD     TO WK-C-VBSET-CUYCD
           MOVE TAG57-BIC         TO WK-C-VBSET-BANKID
           CALL "TRFVBSET" USING WK-C-VBSET-RECORD
           IF WK-C-VBSET-STLMTBNK NOT = SPACES
               MOVE "Y" TO STTL57-IND
           ELSE
               MOVE "N" TO STTL57-IND
           END-IF
       END-IF
       END-IF.
   D499-BKAC-VALIDATION-EX.
       EXIT.
       EJECT

G2BL00  R100-READ-UFIMIJCON.
G2BL00
G2BL00      INITIALIZE UFIMIJCON-REC WK-C-UFIMIJCON.
G2BL00
G2BL00      MOVE WK-VTE1-PARALNO  TO UFIMIJCON-QUENUM
G2BL00      MOVE WK-VTE1-SEQNUM   TO UFIMIJCON-QUESUF
G2BL00
G2BL00      READ UFIMIJCON KEY IS EXTERNALLY-DESCRIBED-KEY
G2BL00
G2BL00      IF NOT WK-C-SUCCESSFUL
G2BL00          GO TO R199-READ-UFIMIJCON-EX
G2BL00      END-IF.
G2BL00
G2BL00*--Turn on the switch if COVER is already received.
G2BL00      IF UFIMIJCON-STATUS EQUAL WK-C-A
G2BL00          MOVE WK-C-Y TO WK-C-COV-SW
G2BL00      END-IF.
G2BL00
G2BL00  R199-READ-UFIMIJCON-EX.
G2BL00      EXIT.
G2BL00

       *=================================================================*
GP3A00  D060-EVAL-TAG57-CD.
       *=================================================================*
GP3A00*--This routine will check Tag57 C/D Lines 1-5 if it exact matches
GP3A00*--Tag Validation table. If Match, treat it as Tag57A w/ our Own BIC
GP3A00*--(UOVBSGSGXXX - parameterized) to further proceed with STP processing.
GP3A00*--E.g Raw Tag57D Line1:/123456789
GP3A00*--          Line2:UNITED OVERSEAS BANK
GP3A00*--          Line3:SINGAPORE
GP3A00*--          Line4:BUKIT BATOK
GP3A00*--          Line5:SG
GP3A00*--If Line 1 "UNITED OVERSEAS BANK" exact matches Tag validation table
GP3A00*--system will treat this as Tag57A Line1: *blank
GP3A00*--                      Line2: UOVBSGSGXXX

GP3A00*------------------------------------------------------------------      
GP3A00--                                                              Line3: *blank      
GP3A00--                                                              Line4: *blank      
GP3A00--                                                              Line5: *blank      
GP3A00--and proceed with BAU STP processing.      
GP3A00      
GP3A00      INITIALIZE              WK-C-VTAG57-RECORD.      
GP3A00      MOVE TAG57-OPT          TO  WK-C-VTAG57-OPTION.      
GP3A00      
GP3A00*------------------------------------------------------------------      
GP3A00--Tag57C:      
GP3A00      IF  TAG57-OPT = "C"      
GP3A00          IF  TAG57-PTID = SPACES      
GP3A00              GO TO D699-EVAL-TAG57-CD-EX      
GP3A00          ELSE      
GP3A00              MOVE TAG57-PTID   TO  WK-C-VTAG57-INFO(1)      
GP3A00          END-IF.      
GP3A00      
GP3A00*------------------------------------------------------------------      
GP3A00--Tag57D:      
GP3A00      IF  TAG57-OPT = "D"      
GP3A00          IF  TAG57-PTID = SPACES      
GP3A00              AND TAG57-NAME = SPACES      
GP3A00              GO TO D699-EVAL-TAG57-CD-EX      
GP3A00          ELSE      
GP3A01*GP3A00      MOVE TAG57-PTID   TO  WK-C-VTAG57-INFO(1)      
GP3A00              MOVE TAG57-NAME   TO  WK-C-VTAG57-INFO(2)      
GP3A01*GP3A00      MOVE TAG57-LINE-3 TO  WK-C-VTAG57-INFO(3)      
GP3A01*GP3A00      MOVE TAG57-LINE-4 TO  WK-C-VTAG57-INFO(4)      
GP3A01*GP3A00      MOVE TAG57-LINE-5 TO  WK-C-VTAG57-INFO(5)      
GP3A00          END-IF      
GP3A00      END-IF.      
GP3A00      
GP3A00      MOVE TFSSTPL-BNKENTTY    TO  WK-C-VTAG57-I-BNKENTTY.      
GP3A00      
GP3A00*------------------------------------------------------------------      
GP3A00--Check Tag57 if either Lines 1-5 matches Tag validation table.      
GP3A00      CALL "TRFVTAG57"        USING  WK-C-VTAG57-RECORD.      
GP3A00      CANCEL "TRFVTAG57".      
GP3A00      
GP3A00      IF  WK-C-VTAG57-ERROR-CD = SPACES      
GP3A00          CONTINUE      
GP3A00      ELSE      
GP3A00          GO TO D699-EVAL-TAG57-CD-EX      
GP3A00      END-IF.      
GP3A00      
GP3A00*------------------------------------------------------------------      
GP3A00--If it match, overide w/ Tag57A:<Own BIC> (parameterized)      
GP3A00      IF  WK-C-VTAG57-VALID = "Y"      
GP3A00          MOVE SPACES         TO  TAG57-PTID      
GP3A00                                  WS-ACBNKACC      
GP3A00                                  WS-ACCNO      
GP3A00                                  WK-VTE1-ACBNKNM      
GP3A00                                  WK-VTE1-ACBNKADR1      
GP3A00                                  WK-VTE1-ACBNKADR2      
GP3A00                                  WK-VTE1-ACBNKADR3      
GP3A00          MOVE "A"            TO  TAG57-OPT      
GP3A00          MOVE WK-C-VTAG57-BIC TO  TAG57-BIC      

GP3A00           WS-ACBNKID      
GP3A00           WS-BANKID      
GP3A00           END-IF.      
      *=================================================================*
      *=================================================================*
      GP3A00 D699-EVAL-TAG57-CD-EX.
      *=================================================================*
GP3A00           EXIT.      
 
5Q1JM1 D500-PROCESS-RPRRSN SECTION.      
5Q1JM1 D500-ENTRY.      
5Q1JM1      
5Q1JM1           MOVE WK-VTE1-PARALNO    TO WK-C-RRSN-QUENUM.      
5Q1JM1           MOVE WK-VTE1-SEQNUM     TO WK-C-RRSN-QUESUF.      
5Q1JM1           MOVE WK-C-TRNNO         TO WK-C-RRSN-TRNNO.      
5Q1JM1           MOVE WK-C-FUNCTID       TO WK-C-RRSN-FUNCTID.      
5Q1JM1           MOVE WK-C-SEGCODE       TO WK-C-RRSN-SEGCODE.      
5Q1JM1           MOVE SPACES             TO WK-C-RRSN-SEGDESC.      
5Q1JM1           MOVE WK-N-STAFFIND      TO WK-C-RRSN-STAFFIND.      
5Q1JM1           MOVE WS-ACCNO           TO WK-C-RRSN-ACCNO.      
CMP3A1           IF WK-101-TAG50H-ACCNO NOT = SPACES AND      
CMP3A1               WS-C-M101STPIND = "Y"       AND      
CMP3A1               TFSSTPL-SWFTMGTY = "101"      
CMP3A1               MOVE WK-101-TAG50H-ACCNO  TO WK-C-RRSN-ACCNO      
CMP3A1           END-IF.      
5Q1JM1           MOVE WK-C-QRATE         TO WK-C-RRSN-QRATE.      
5Q1JM1           MOVE WK-N-SYSDTE        TO WK-C-RRSN-RPRRDTE.      
5Q1JM1*          MOVE WK-C-RPRPCODE      TO WK-C-RRSN-RSNCDE.      
5Q1JM1           IF WK-C-RPRPCODE = SPACE      
5Q1JM1               MOVE "RSN9999"      TO WK-C-RRSN-RSNCDE      
5Q1JM1           ELSE      
5Q1JM1               MOVE WK-C-RPRPCODE  TO WK-C-RRSN-RSNCDE      
5Q1JM1           END-IF.      
5Q1JM1           MOVE SPACES             TO WK-C-RRSN-RSNDESC.      
5Q1JM1           MOVE WK-C-RPRPGM        TO WK-C-RRSN-RPRPGM.      
5Q1JM1           CALL "TRFGRRSN" USING WK-C-RRSN-RECORD.      
5Q1JM1      
5Q1JM1 D599-PROCESS-RPRRSN-EX.      
5Q1JM1           EXIT.      
5Q1JM1 EJECT      
 
           Z000-END-PROGRAM.
                CLOSE TFSSTPL
 ID1VKE       TFSBNKET     
 G2BL00       UFMIJCON     
                   TFSCLSYS.
                EXIT PROGRAM.