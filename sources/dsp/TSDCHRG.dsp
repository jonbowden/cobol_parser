A*%%TS     SD       20130802       193839           TMP3CA                  REL-V6R1M0     5761-WDS
     *************************************************************y
A*                             AMENDMENT            HISTORY
A* ************************************************************* /
A*DATE              BY..     AMENDMENT                                                             TAG
A*

A*22/12/14           RDC     14HOREM024         14HOREM028             14HOREM029                5Q1RC
A*                           NON   PSTP     REASON         ENHANCEMENT
A*                           ADDED    NEW      FUNCTION          F7=REASON
A*

A*02/08/2013         AOC     Screen      standardization                                         MYIAC
A*                          -Change to small caps
A*                          -remove      Enter
A*
A*23/01/06          MAF      ADD   FI9    FOR       OUST.       INST.                            R27AF
A*17/06/09          CE4      LAUNCH      PIB    TT    IN    CHINA                                CPBCE
A*                           Retrofit       PIB mods            from    HO    src
A*                            (ADD F02 TO VIEW PIB/BIB                        INCOMING MSG)
A*

     *************************************************************y
A*     90/07/09  17:22:20    ACSWTL      REL-R02M00  5728-PW1
A*%%EC
A                                                                     DSPSIZ(24     80   *DS3)
A                                                                     PRINT
A*     90/05/08       14:56:22            ACSHWY                  REL-RO2MO0         5728-PW1
A               R    TSDCHRGIS                                        SFL
A*%%TS     SD       20030228       200454           UNCMBA                  REL-V4R4M0     5769-PW1
A                                                                     TEXT('SUBFILE       FOR CHARGES    ITEMS')
A    25                                                               SFLNXTCHG
A                    TRNNO                  12A       H
A                    FUNCTID                   8A     H
A                    CRTO                      3A     H
A                    LCEAMT                 15Y      2H
A                    REMAMT                 15Y      2H
A                   NRMLCHG                    8A     H
A                   MAXMIN                     1A     H
A                    INCOMEIND                 1A     H
A                    STDCHAMT               15Y      2H
A                    POS                       1A     H
A                   REPECNT                    2Y    OH
A                    SEQNUM                    2Y    OH
A                   ACTCHGN2                   2Y    OH
A                   ACTCHGD2                   3Y    OH
A                   ACTCHGN3                   2Y    OH
A                   ACTCHGD3                   3Y    OH
A                   ACTCHGN4                   2Y    OH
A                   ACTCHGD4                   3Y    OH
A                   ACTCHGN5                   2Y    OH
A                   ACTCHGD5                   3Y    OH
A                    PACTION                   1A     H
A                   ACTION                     1A     B    15     2
A    55                                                               DSPATR(RI)
A    55                                                               DSPATR(PC)
A    81                                                               DSPATR(PR)
A                    CHARGECD                  8A     0    15     4
A                    PAYIND                    1A     B    15    13
A    67                                                               DSPATR(RI)
A    67                                                               DSPATR(PC)
A    81                                                               DSPATR(PR)
A                    ACTCHGN1                  2Y    OB    15    15
A    83                                                               DSPATR(RI)
A     84                                                              DSPATR(PC)
A     81                                                              DSPATR(PR)
A                    ACTCHGD1                  3Y    OB    15    20
A     51                                                              DSPATR(RI)
A     51                                                              DSPATR(PC)
    A    81                                                        DSPATR(PR)
    A                   DRFROM                3A     B   15   24
    A    53                                                        DSPATR(RI)
    A    53                                                        DSPATR(PC)
    A    81                                                        DSPATR(PR)
    A                   PRINCUY               3A     0   15   28
    A                   PRINAMT           12Y      2B    15   32EDTCDE(3)
    A    54                                                     DSPATR(RI)
    A    54                                                     DSPATR(PC)
    A    82                                                     DSPATR(PR)
    A                   EXCHRATE          16Y       70   15   46EDTCDE(3)
    A                   PAYMCUY               3A     O   15   64
    A                   PAYMAMT           12Y       20   15 68EDTCDE(3)
    A                                                    15 18'/     r



                                         ACSWTL                REL-R02M00                   5728-PW1
    A*    90/07/09        17:22:20
    A               R    TSDCHRGIC                                 SFLCTL(TSDCHRGIS)
    A*%%TS     SD       20130802     102326        TMP3CA                REL-V6R1M0                  5761-WDS

    A                                                              SFLSIZ(0050)
    A                                                              SFLPAG(0006)
    A                                                              TEXT('TSDCHRGIS CONTROL RECORD FOR -
    A                                                              TSDCHRG1')
    A    87
    AO   8 8                                                       CF02(02
                                                                                   I
                                                                                     INCOMING TT')
    A                                                              CF03 (03          EXIT PROGRAM')
    A                                                              CF04 (04
                                                                                   I
                                                                                     PROMPT')
5Q1RCA                                                             CF07 (07
                                                                                   I
                                                                                     REPAIR REASON')
     A                                                             CF19 (19
                                                                                   1
                                                                                     CUSTINSTR')
     A                                                             OVERLAY

     A   30                                                        SFLDSP
     A   31                                                        SFLDSPCTL
     A   35                                                        SFLEND
     A                   DSPREC1              4S    OH             SFLRCDNBR(CURSOR)
     A                   GHENV                5A     0    2    3
     A                                                    2    9'TSDCHRG1 I
     A                   TRNMD                7A     0    2   18
                                                          2   28'CHARGES
                                                                                        1
     A
     A                                                          COLOR(WHT)
     A                   FUNDESCR         20         0    2   38COLOR(WHT)
     A                   GHDATE               8A     O    2   62

     A                   GHTIME               8A     O    2   71
                                                                                             .   I
     A                                                    4    3'Trn         No
     A                                                             COLOR(WHT)
     A                   TRNNO            12A        0    4   18
                                                                                             .   I
     A                                                    5    3'Customer
     A                                                             COLOR(WHT)
     A                   CUSTNO           15A        0    5   18
     A                                                    6    3'Name                        ●   1



     A                                                             COLOR(WHT)
     A                   CUSTLNM          35A        0    6   18
     A                                                    8    3'DR#                   Payment            Cuy        Ref e-
     A                                                             rence      No                       Remarks/Account No t



     A                                                             COLOR(WHT)
     A                                                    9    3 'XI     I



     A                   PAYMODE1             6A     B    9   11
     A   70                                                        DSPATR(RI)
     A   80                                                        DSPATR(PR)
     A   71                                                        DSPATR(PC)
     A                   PAYMCUY1             3A     B    9   22
     A   72                                                        DSPATR(RI)
     A   73                                                        DSPATR(PC)
     A   80                                                        DSPATR(PR)
     A                   REFERNO1         15A        B    9   31
     A   43                                                        DSPATR(RI)
     A   43                                                        DSPATR(PC)
     A   80                                                        DSPATR(PR)
     A                   REMARK1          30A        B    9   50
A    44                                                       DSPATR(RI)
A    44                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                                                   10    3'X2    I



A                   PAYMODE2             6A     B   10   11
A    74                                                       DSPATR(RI)
A    75                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                   PAYMCUY2             3A     B   10   22
A    76                                                       DSPATR(RI)
A    77                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                   REFERN02         15A        B   10   31
A    48                                                       DSPATR(RI)
A    48                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                   REMARK2          30A        B   10   50
A    49                                                       DSPATR(RI)
A    49                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                                                   11    3'X3    I



A                   PAYMODE3             6A     B   11   11
A    78                                                       DSPATR(RI)
A    79                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                   PAYMCUY3             3A     B   11   22
A    65                                                       DSPATR(RI)
A    66                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                  REFERN03          15A        B   11   31
A    63                                                       DSPATR(RI)
A    63                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                  REMARK3           30A        B   11   50
A    64                                                       DSPATR(RI)
A    64                                                       DSPATR(PC)
A    80                                                       DSPATR(PR)
A                                                   13    3      Charges Items
                                                              t
                                                                                                  Act
A                                                           X   or   C  or Space
                                                                                           1



A                                                          COLOR(WHT)
A                                                   14    2'Act    Code  Pay Rate      FR         Cuy     Ch-
A                                                          arge Amt         Ex.   Rate              Amt    D-
A                                                             ebited r
A                                                             COLOR(WHT)
A              R    TSDCHRGID
A*%%TS    SD       20130802     193839        TMP3CA              REL-V6R1M0       5761-WDS
A                                                             TEXT('TSDCHRG1 GL IND SCREEN REC')
A                                                             OVERLAY
A                                                             CF03(03
                                                                         1
                                                                             EXIT PROGRAM')
A                                                             CF04(04
                                                                         1
                                                                             PROMPT')
A                                                   22   55'GL Screen          (Y/N)   t



A                                                             COLOR(WHT)
A                  GLIND                 1A     B   22   73
A    90                                                       DSPATR(RI)
A    90                                                       DSPATR(PC)
A    91                                                       DSPATR(PR)
A*    90/03/15       19:39:53       KANGM                 REL-R01M02           5728-PW1
A              R    TSDCHRGIE
A*%%TS    SD       20130802     102326        TMP3CA              REL-V6R1M0       5761-WDS
A                                                             TEXT('TSDCHRG1 ERROR SCREEN RECORD')
A                                                             OVERLAY

A                   SCRMSGID             7      0   23    2DSPATR(BL)
A                                                          DSPATR(HI)
A    89                                                       ERRMSGID(CPF5204         *LIBL/QCPFMSG)
A                                                          COLOR(PNK)
A                   SCRDASH              1      O 23     IIDSPATR(HI)
      A                                           COLOR(PNK)
      A                SCRMSGTEXT   50   0 23   14DSPATR(HI)
      A                                           COLOR(PNK)
                                                           F3=Exit F4=Prompt
                                                                             I
R27AFA*   85                               24    2'ENTER

MYIACA*   85                               24    2'ENTER   F3=Exit F4=Prompt F19=Cust                               -
                                                 2'F3=Exit F4=Prompt F19=Cust Instr
                                                                                     I
MYIACA*   85   5Q1RC                       24
                                                  Instr.
                                                              1
MYIACA*
5Q1RCA    85                               24    2'F3=Exit         F4=Prompt           F7=Reason -
                                                  F19=Cus         Ins
                                                                        I

5Q1RCA
      A                                           COLOR(BLU)
                                                                  F3=Exit I
  I   A* 86                                24    2'ENTER
                                                                  F3=Exit           F19=Cus       Instr.
                                                                                                           1

MYIACA*   86                               24    2'ENTER
                                                 2'F3=Exit         F19=Cus           Instr.
                                                                                              t

MYIACA*   86   5Q1RC                       24
                                                 2'F3=Exit         F7=Reason           F19=Cus         Instr.
                                                                                                                I

5Q1RCA    86                               24

      A                                           COLOR(BLU)
MYIACA*   87                               24    2'ENTER  F2=Incoming TT F3=Exit F4=P-
                                                  rompt F19=Cust Instr.
                                                                                         r
MYIACA*
MYIACA*   87   5Q1RC                       24    2'F2=Incoming TT F3=Exit F4=Prompt F-
                                                                                t
MYIACA*        5Q1RC                               19=Cust         Instr.

5Q1RCA    87                               24    2'F2=Incoming TT F3=Exit F4=Prompt -
                                                                                                   t
5Q1RCA                                            F7=Reason         F19=Cust           Instr.
      A                                           COLOR(BLU)
MYIACA*   88                               24    2’ENTER          F2=Incoming TT F3=Exit F19=-
MYIACA*                                           Cust       Instr.         r




MYIACA*   88   5Q1RC                       24    2’F2=Incoming TT F3=Exit F19=Cust In-
MYIACA*        5Q1RC                               tr.   1




5Q1RCA    88                               24    2'F2=Incoming TT F3=Exit F7=Reason -
                                                  F19=Cust         Instr.
                                                                                I
5Q1RCA
