      *=====================================================================*
      *Copybook Name       : VTAG57                                        *
      *Copybook Description: REM Copybook for TRFVTAG57 Program            *
      *Date Created        : 26 October 2019                               *
      *Created by          : Accenture                                     *
      *=====================================================================*
      * HISTORY OF MODIFICATION:                                           *
      *=====================================================================*
      * MOD.#   INIT    DATE        DESCRIPTION                            *
      * ------  ------  ----------  ---------------------------------------*
      * GP3A00 - ACNDU5 - 26/10/19 - CASH MANAGEMENT ROAD MAP - P19        *
      *                        - GPI Day4 (Retro from GPI Day3 HO)         *
      *                        - Initial Version.                          *
      *=====================================================================*
       01 WK-C-VTAG57-RECORD.
           05 WK-C-VTAG57-INPUT.
               10 WK-C-VTAG57-OPTION PIC X(01).
               10 WK-C-VTAG57-VALUE.
                   15 WK-C-VTAG57-I-BNKENTRY PIC X(02).
                   10 WK-C-VTAG57-I-FILLA1 PIC X(35).
                   10 WK-C-VTAG57-I-FILLA2 PIC X(35).
                   10 WK-C-VTAG57-I-FILLA3 PIC X(35).
                   10 WK-C-VTAG57-I-FILLA4 PIC X(35).
                   10 WK-C-VTAG57-I-FILLA5 PIC X(35).
                   10 WK-C-VTAG57-I-FILLN1 PIC S9(13)V9(2).
                   10 WK-C-VTAG57-I-FILLN2 PIC S9(13)V9(2).
                   10 WK-C-VTAG57-I-FILLN3 PIC S9(13)V9(2).
                   10 WK-C-VTAG57-I-FILLN4 PIC S9(13)V9(2).
                   10 WK-C-VTAG57-I-FILLN5 PIC S9(13)V9(2).
           05 WK-C-VTAG57-OUTPUT.
               10 WK-C-VTAG57-INVALID-OUTPUT.
                   15 WK-C-VTAG57-ERROR-CD PIC X(07).
                   15 WK-C-VTAG57-COM0206.
                       20 WK-C-VTAG57-FILE PIC X(08).
                       20 WK-C-VTAG57-MODE PIC X(06).
                       20 WK-C-VTAG57-KEY PIC X(20).
                       20 WK-C-VTAG57-FS PIC X(02).
               10 WK-C-VTAG57-VALID-OUTPUT.
                   15 WK-C-VTAG57-VALID PIC X(01).
                   15 WK-C-VTAG57-BIC PIC X(11).
                   10 WK-C-VTAG57-O-FILLA1 PIC X(35).
                   10 WK-C-VTAG57-O-FILLA2 PIC X(35).
                   10 WK-C-VTAG57-O-FILLA3 PIC X(35).
                   10 WK-C-VTAG57-O-FILLA4 PIC X(35).
                   10 WK-C-VTAG57-O-FILLA5 PIC X(35).
                   10 WK-C-VTAG57-O-FILLN1 PIC S9(13)V9(2).
                   10 WK-C-VTAG57-O-FILLN2 PIC S9(13)V9(2).
                   10 WK-C-VTAG57-O-FILLN3 PIC S9(13)V9(2).
                   10 WK-C-VTAG57-O-FILLN4 PIC S9(13)V9(2).
                   10 WK-C-VTAG57-O-FILLN5 PIC S9(13)V9(2).
* XPARA.CPY
*-----------  COPYBOOK FOR CALLING TRFXPARA - 27/09/89  -----------*
       
       01  WK-C-XPARA-RECORD.
           05  WK-C-XPARA-INPUT.
               10  WK-C-XPARA-PARACD       PIC X(08).
           05  WK-C-XPARA-OUTPUT.
               10  WK-C-XPARA-INVALID-OUTPUT.
                   15  WK-C-XPARA-ERROR-CD PIC X(07).
                   15  WK-C-XPARA-COM0206.
                       20  WK-C-XPARA-FILE PIC X(08).
                       20  WK-C-XPARA-MODE PIC X(06).
                       20  WK-C-XPARA-KEY  PIC X(20).
                       20  WK-C-XPARA-FS   PIC X(02).
               10  WK-C-XPARA-VALID-OUTPUT.
                   15  WK-C-XPARA-PARAVALU PIC X(20).
                   15  WK-N-XPARA-PARANUM REDEFINES WK-C-XPARA-PARAVALU
                       PIC S9(18).
* VSTPL.cpy
***********************************************************************
* HISTORY OF MODIFICATION:                                            *
*---------------------------------------------------------------------*
* 7Q1EM1 - TMPBYM  - 25/11/2016 - REM Q1 2017 RELEASE                 *
*           - e-Req 47511 REFINEMENT OF                              *
*             DUPLICATE CHECKING FOR INW                             *
*           - ADDED NEW FIELD                                        *
*---------------------------------------------------------------------*
*----------- COPYBOOK FOR CALLING TRFVSTPL - 07/06/2004 -------------*
       01 WK-VSTPL.
           03 WK-VSTPL-INPUT.
               05 WK-VSTPL-PARALINO        PIC 9(08).
               05 WK-VSTPL-SEQNUM          PIC 9(02).
               05 WK-VSTPL-CUYCID          PIC X(03).
               05 WK-VSTPL-AMT             PIC S9(14)V9(2).
               05 WK-VSTPL-TAG72           PIC X(11).
               05 WK-VSTPL-VCDT-CD         PIC X(01).
               05 WK-VSTPL-VDCUY-CD        PIC X(01).
               05 WK-VSTPL-AMT-ERR         PIC X(01).
               05 WK-VSTPL-TRNSMTMID       PIC X(01).
               05 WK-VSTPL-ORDACC1         PIC X(35).
       7Q1EM1 05 WK-VSTPL-TRNREF          PIC X(16).
           03 WK-VSTPL-OUTPUT.
               05 WK-VSTPL-ERROR-FOUND     PIC X(01).
               05 WK-VSTPL-FUNCTID         PIC X(08).
               05 WK-VSTPL-STYPTP          PIC X(05).
               05 WK-VSTPL-FXRATETYP       PIC X(02).
               05 WK-VSTPL-FXRATE          PIC S9(09)V9(07).
               05 WK-VSTPL-FXRATEOUT       PIC S9(05).
               05 WK-VSTPL-DRMODE          PIC X(08).
               05 WK-VSTPL-CRMMODE         PIC X(08).
               05 WK-VSTPL-PRODCD          PIC X(06).
               05 WK-VSTPL-ACCCUY          PIC X(03).
               05 WK-VSTPL-BANKID          PIC X(11).
               05 WK-VSTPL-RECBNKID        PIC X(11).
               05 WK-VSTPL-SNDCBNKID       PIC X(11).
               05 WK-VSTPL-INTMBNKID       PIC X(11).
               05 WK-VSTPL-ACBNKID         PIC X(11).
               05 WK-VSTPL-BENBNKID        PIC X(11).
               05 WK-VSTPL-BANKAC          PIC X(11).
               05 WK-VSTPL-BANKAC-TYPE     PIC X(01).
               05 WK-VSTPL-INTMBNKACC      PIC X(11).
               05 WK-VSTPL-ACBNKACC        PIC X(11).
       SM1     05 WK-VSTPL-BENBNKACC       PIC X(35).
               05 WK-VSTPL-BENEBACC        PIC X(11).
               05 WK-VSTPL-RECBNKNM        PIC X(35).
               05 WK-VSTPL-SNDBNKNM        PIC X(35).
               05 WK-VSTPL-ACBNKNM         PIC X(35).
               05 WK-VSTPL-ACBNKADR1       PIC X(35).
               05 WK-VSTPL-ACBNKADR2       PIC X(35).
               05 WK-VSTPL-ACBNKADR3       PIC X(35).
               05 WK-VSTPL-ACBNKADR4       PIC X(35).
               05 WK-VSTPL-ACBNKADR5       PIC X(35).
               05 WK-VSTPL-ACBNKADR6       PIC X(35).
ID1VKER
           05  WK-VSTPL-BENBKNM           PIC X(35).
           05  WK-VSTPL-BENBKADR1         PIC X(35).
           05  WK-VSTPL-BENBKADR2         PIC X(35).
           05  WK-VSTPL-BENBKADR3         PIC X(35).
           05  WK-VSTPL-BENBKADR4         PIC X(35).
           05  WK-VSTPL-BENBKADR5         PIC X(35).
           05  WK-VSTPL-BENBKADR6         PIC X(35).
           05  WK-VSTPL-BENENAME          PIC X(35).
           05  WK-VSTPL-BENEADR1          PIC X(35).
           05  WK-VSTPL-BENEADR2          PIC X(35).
           05  WK-VSTPL-BENEADR3          PIC X(35).
           05  WK-VSTPL-BENEADR4          PIC X(35).
           05  WK-VSTPL-BENEADR5          PIC X(35).
           05  WK-VSTPL-BENEADR6          PIC X(35).
           05  WK-VSTPL-SHIFTNO           PIC X(04).
           05  WK-VSTPL-CHACUDBUI         PIC X(01).
           05  WK-VSTPL-BENEFLG           PIC X(01).
           05  WK-VSTPL-N0910             PIC X(01).
           05  WK-VSTPL-DR-AOCD           PIC X(04).
           05  WK-VSTPL-CR-AOCD           PIC X(04).
           05  WK-VSTPL-TABLE-A.
               07  WK-VSTPL-ACTA1         PIC X(01).
               07  WK-VSTPL-ACTA2         PIC X(01).
               07  WK-VSTPL-PYBNKID       PIC X(11).
               07  WK-VSTPL-PYACCNO       PIC X(11).
               07  WK-VSTPL-PYACCNO-TYPE  PIC X(01).
               07  WK-VSTPL-PYACUDBUI     PIC X(01).
               07  WK-VSTPL-PAYRNAM       PIC X(35).
               07  WK-VSTPL-PAYRAD1       PIC X(35).
               07  WK-VSTPL-PAYRAD2       PIC X(35).
               07  WK-VSTPL-PAYRAD3       PIC X(35).
               07  WK-VSTPL-PAYRAD4       PIC X(35).
               07  WK-VSTPL-PAYRAD5       PIC X(35).
               07  WK-VSTPL-PAYRAD6       PIC X(35).
           05  WK-VSTPL-TABLE-B1.
               07  WK-VSTPL-ACTB11        PIC X(01).
               07  WK-VSTPL-ACTB12        PIC X(01).
               07  WK-VSTPL-ACTB13        PIC X(01).
               07  WK-VSTPL-ACTB14        PIC X(01).
               07  WK-VSTPL-ACTB15        PIC X(01).
               07  WK-VSTPL-ACTB16        PIC X(01).
           05  WK-VSTPL-TABLE-B2.
               07  WK-VSTPL-ACTB21        PIC X(01).
               07  WK-VSTPL-ACTB22        PIC X(01).
               07  WK-VSTPL-ACTB23        PIC X(01).
           05  WK-VSTPL-TABLE-B3.
               07  WK-VSTPL-ACTB31        PIC X(01).
               07  WK-VSTPL-ACTB32        PIC X(01).
               07  WK-VSTPL-ACTB33        PIC X(01).
               07  WK-VSTPL-ACTB34        PIC X(01).
           05  WK-VSTPL-TABLE-C1.
               07  WK-VSTPL-RESCD         PIC 9(02).
               07  WK-VSTPL-DOMBRCH       PIC 9(03).
               07  WK-VSTPL-DOMBRCH       PIC 9(04).
               07  WK-VSTPL-HOLCDCD1      PIC 9(02).
* XGSPA.cpy
*---------  COPYBOOK FOR CALLING TRFXGSPA - 28/11/02  ---------*
       01  WK-C-XGSPA-RECORD.
           05  WK-C-XGSPA-INPUT.
               10  WK-C-XGSPA-GHPARCD      PIC X(10).
           05  WK-C-XGSPA-OUTPUT.
               10  WK-C-XGSPA-INVALID-OUTPUT.
                   15  WK-C-XGSPA-ERROR-CD PIC X(07).
                   15  WK-C-XGSPA-COM0206.
                       20  WK-C-XGSPA-FILE PIC X(08).
                       20  WK-C-XGSPA-MODE PIC X(06).
                       20  WK-C-XGSPA-KEY  PIC X(20).
                       20  WK-C-XGSPA-FS   PIC X(02).
               10  WK-C-XGSPA-VALID-OUTPUT.
                   15  WK-C-XGSPA-GHPARVAL PIC X(60).
                   15  WK-N-XGSPA-GHPARNUM REDEFINES
                       WK-C-XGSPA-GHPARVAL PIC S9(18).
      * VBAC.cpy
      *=============================================================================*
      * HISTORY OF MODIFICATION:                                                   *
      *=============================================================================*
      * GH1NVB - NVBUOT   - 03/10/2002 - GLOBAL HUBBING.                           *
      *                      1. CHANGE WK-N-VBAC-BNKENTTY                          *
      *                         S9(1) TO X(2).                                     *
      *                      2. CHANGE WK-C-VBAC-BNKACNO                           *
      *                         X(11) TO X(15).                                    *
      *-----------------------------------------------------------------------------*
      *----------  COPYBOOK FOR CALLING TRFVBAC - 23/09/89  -----------*           
       01 WK-C-VBAC-RECORD.                                                        
           05 WK-C-VBAC-INPUT.                                                    
               10 WK-N-VBAC-BNKENTTY  PIC S9(01).                                 
               10 WK-N-VBAC-BNKENTTY  PIC X(02).                                  
               10 WK-C-VBAC-BANKID    PIC X(11).                                  
               10 WK-C-VBAC-CUYCD     PIC X(03).                                  
           05 WK-C-VBAC-OUTPUT.                                                   
               10 WK-C-VBAC-INVALID-OUTPUT.                                       
                   15 WK-C-VBAC-ERROR-CD PIC X(07).                               
                   15 WK-C-VBAC-COMO2026.                                         
                       20 WK-C-VBAC-FILE PIC X(08).                               
                       20 WK-C-VBAC-MODE PIC X(06).                               
                       20 WK-C-VBAC-KEY  PIC X(20).                               
                       20 WK-C-VBAC-FS   PIC X(02).                               
               10 WK-C-VBAC-VALID-OUTPUT.                                         
                   15 WK-C-VBAC-BNKACNO  PIC X(11).                               
                   15 WK-C-VBAC-BNKACNO  PIC X(15).                               
                   15 WK-C-VBAC-ACCTYP   PIC X(01).                               
                   15 WK-C-VBAC-ACUDBUI  PIC X(01).                               
* LOGG.cpy
      *----------  COPYBOOK FOR CALLING TRFLOGG - 28/07/2011  ----------*
       01 WK-LOGG.
           05 WK-LOGG-INPUT.
               07 WK-LOGG-PARALNO      PIC 9(08).
               07 WK-LOGG-SEQNUM       PIC 9(02).
               07 WK-LOGG-TABTYP       PIC X(03).
               07 WK-LOGG-VALUE-2.
                  10 WK-LOGG-FUNCTID   PIC X(08).
                  10 WK-LOGG-FSTPTYP   PIC X(05).
           05 WK-LOGG-OUTPUT.
               07 WK-LOGG-ERROR-FOUND  PIC X(01).
               07 WK-LOGG-VALUE.
                  10 WK-LOGG-DRMODE    PIC X(08).
                  10 WK-LOGG-CRMODE    PIC X(08).
                  10 WK-LOGG-FUNCTID   PIC X(08).
                  10 WK-LOGG-STPTYP    PIC X(05).
                  10 WK-LOGG-FXRTTYP   PIC X(02).
                  10 WK-LOGG-ERRT2     PIC X(01).
                  10 WK-LOGG-ERRCDT    PIC X(01).
                  10 WK-LOGG-ERRCCY    PIC X(01).
                  10 WK-LOGG-ERRAMT    PIC X(01).
                  10 WK-LOGG-ERRTMD    PIC X(01).
                  10 WK-LOGG-IRSW-ERR  PIC X(01).
               07 WK-LOGG-VALUE-1.
                  10 WK-LOGG-LCAMT     PIC 9(13)V99.
               07 WK-LOGG-DATAAREA.
                  10 WK-LOGG-DATAA1    PIC X(20).
                  10 WK-LOGG-DATAB1    PIC X(20).
                  10 WK-LOGG-DATAB2    PIC X(20).
                  10 WK-LOGG-DATAB3    PIC X(20).
                  10 WK-LOGG-DATAC1    PIC X(20).
                  10 WK-LOGG-DATAD1    PIC X(20).
                  10 WK-LOGG-DATAD2    PIC X(20).
                  10 WK-LOGG-DATAE1    PIC X(20).
                  10 WK-LOGG-DATAE2    PIC X(20).
                  10 WK-LOGG-DATAE3    PIC X(20).
                  10 WK-LOGG-DATAF1A   PIC X(20).
                  10 WK-LOGG-DATAF1B   PIC X(20).
                  10 WK-LOGG-DATAF2    PIC X(20).
               07 WK-LOGG-ACTTAB.
                  10 WK-LOGG-ACTC1     PIC X(10).
                  10 WK-LOGG-ACTB1     PIC X(10).
                  10 WK-LOGG-ACTB2     PIC X(10).
                  10 WK-LOGG-ACTB3     PIC X(10).
                  10 WK-LOGG-ACTC1     PIC X(10).
                  10 WK-LOGG-ACTD1     PIC X(10).
                  10 WK-LOGG-ACTD2     PIC X(10).
                  10 WK-LOGG-ACTE1     PIC X(10).
                  10 WK-LOGG-ACTE2     PIC X(10).
                  10 WK-LOGG-ACTE3     PIC X(10).
                  10 WK-LOGG-ACTF1A    PIC X(10).
                  10 WK-LOGG-ACTF1B    PIC X(10).
                  10 WK-LOGG-ACTF2     PIC X(10).
* ASCWMS.cpy
       *****************************************************************
       * ****************  START OF COMMON TABLE  ******************** *
       * ****************  LAST AMENDED 08/25/1989  ****************** *
       *****************************************************************
       * RELATIVE KEY                                      08-25-1989 *
       *****************************************************************
           05 WK-C-COM0206.
               10 WK-C-COM0206-FILE    PIC X(8).
               10 WK-C-COM0206-MODE    PIC X(6).
               10 WK-C-COM0206-KEY     PIC X(20).
               10 WK-C-COM0206-FS      PIC X(02).
               10 WK-N-COM0206-SQL     PIC 9(09).

       *****************************************************************
       * RELATIVE KEY                                      05-23-1989 *
       *****************************************************************
           05 WK-N-REL-KEY             PIC 9(4) VALUE ZERO.
           05 WK-N-REL-SIZE            PIC 9(4) VALUE ZERO.

       *****************************************************************
       * WHEN-COMPILED DATE                                 03-10-1988 *
       *****************************************************************
           05 WK-C-WHEN-COMPILED       PIC X(8).

       *****************************************************************
       * RETURN VALUE FOR MVS - ONLY FOR VSE                 12-09-1988 *
       *****************************************************************
       *   05 RETURN-CODE             PIC 99.

       *****************************************************************
       * MONTH LIST                                          12-09-1988 *
       *****************************************************************
           05 WK-C-CHR-MTH.
               10 FILLER               PIC X(54) VALUE
                   "JANUARY  FEBRUARY  MARCH     APRIL     MAY       JUNE   ".
               10 FILLER               PIC X(54) VALUE
                   "JULY     AUGUST    SEPTEMBEROCTOBER   NOVEMBER  DECEMBER ".
           05 WK-C-CHR-12 REDEFINES WK-C-CHR-MTH.
               10 WK-C-MTH OCCURS 12   PIC X(9).

       *****************************************************************
       * FILE STATUS CODES                                  01-01-1988 *
       *****************************************************************
           05 WK-C-FILE-STATUS         PIC XX.
               88 WK-C-SUCCESSFUL      VALUE "00".
               88 WK-C-END-OF-FILE     VALUE "10".
               88 WK-C-RECORD-NOT-FOUND VALUE "23".
               88 WK-C-SEQUENCE-ERROR  VALUE "21".
               88 WK-C-DUPLICATE-KEY   VALUE "22".
               88 WK-C-BOUNDARY-VIOLATION VALUE "24".
               88 WK-C-PERMANENT-ERROR VALUE "30", "34".
               88 WK-C-START-BROWSE-EOF VALUE "94".
       *****************************************************************
       * **********  END OF COMMON WORKING STORAGE  ****************** *