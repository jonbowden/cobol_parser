     A*%%TS   SD       20130807        153755           TMP3CA                     REL-V6R1M0           5761-WDS
     A**************************************************************/
     A*                              AMENDMENT          HISTORY
     A**************************************************************y /
     A*DATE            BY..     AMENDMENT                                                                     TAG
     A*

     A*19/09/16        JZM      REM 2016 Q4 Release                                                           6Q4J2
     A*                         E-REQUEST# 46959
     A*                         JIRA LOG PQR-14118
     A*                         Exchange Rate Type Validation enhancement
     A*                         Set    PC    of    FXCNFRM                field    to    IN16
     A*                                                                                                                 */
     A*14/06/16        JZM      REM 2016 Q4 Release                                                           6Q4J1
     A*                         E-REQUEST# 46959
     A*                         Exchange Rate Type Validation enhancement
     A*                         Add FXCNFRM              field
     A*04/01/15        ARV      14HOREM024          14HOREM028                   14HOREM029                   5Q1AR
     A*                         NON    PSTP       REASON             ENHANCEMENT
     A*                         ADDED       NEW    FUNCTION                F7=REASON
     A*04/09/13 TMPAJ6 UOBM PROJECT                                                                           MYIAJ
     A*                       -CHANGE        FICODE          PR       INDICATOR          FROM    IN08
     A*                         TO    INI9
     A*
     A*04/08/13        AOC      Screen       standardization                                                  MYIAC
     A*                       -Change label to small                             caps
     A*                       -Remove        Enter
     A*
     A*23/05/13        VKE      HK/CR/2011/REM/03                                                             H03VK
     A*                         TO    INCLUDE       MBPS             COVER       FIELD
     A*                         IF    USER    INPUT          "Y"          THEN    SYSTEM    WILL      SEND
     A*                         OUTGOING          MESSAGE             AS    RTGS    COVER
     A*                                                                                                             /
     A*25/06/12        RAL      UOBM    REM       PROJECT                                                    MYIRAL
     A*                         Added the          following fields:
     A*                         1.    FI CODE (FICODE)
     A*                         2.    FI NAME (FINAME)
     A*                         3.    ROUTING NO (RTGNUM)
     A*                                                                                                             /
     A*05/04/12        LCN      UOBI    PROJECT:             (UOBI-1202)                                      ID1L2
     A*                         Moved the position fo EMP FLAG to be
     A*                         consistent          with             other       function.
     A*                                                                                                             /
    A*08/02/12         LCN      Added EMPFLAG field.                                                          IDILI
    A*                                                                                                           /
     A*19/03/10        GIH      Format REMAMT to display with decimal                                     (.)103GH
     A*                         when    amount          is       <    0
    A*                                                                                                              /
    A*01/12/09         CMT      Add Option 55/56/57 indicator for auto                                        103CMT
    A*                          mapping of outgoing MT103 from Incoming
    A*                          functions
       *************************************************************/
    A*   91/04/08  16:39: 52   ACSYSC      REL-R02M00  5728-PW1
    A*%%EC
    A                                                                           DSPSIZ(24       80   *DS3)
    A                                                                           PRINT
    A*    91/04/08       16:39:52             ACSYSC                        REL-R02M00           5728-PW1
    A              R    RSDPAYCR
    A*%%TS    SD       20130807        153755           TMP3CA                     REL-V6R1M0           5761-WDS
    A                                                                           TEXT('RSDPAYC KEY SCREEN RECORD')
    A                                                                           CF02(02 'INCOMING MSG')
    A                                                                           CF03(03 'END OF PROGRAM')
    A                                                                           CF06(06 'CANCEL')
5Q1ARA                                                                          CF07(07 'REPAIR REASON')
    A                                                                           OVERLAY
    A                   GHENV                      5A        O        1     3
     A                                                                1    10'RSDPAYC 1
                                        1   30'PAYMENT                KEY    VERIFY
                                                                                             1
     A
     A                                           COLOR(WHT)
     A         GHDATE        8A     0   1   62
     A         GHTIME        8A     0   1   72
                                        3    3'Trn       No
                                                              I
     A
     A                                           COLOR(WHT)
     A                                  3   20':'
     A                                           COLOR(WHT)
     A         TRNNO        12A     O   3   22
     A                                  3   37'Remittance                   Amt:     1




     A                                           COLOR(WHT)
     A         REMCUY        3A     O   3   53
                                        3   59EDTWRD( I
                                                                                                           I
103GHA*        REMAMT       15Y    20
                                        3   60EDTWRD(                                                0 .   I
                            15Y    20
                                                              I
MYIACA*        REMAMT
MYIACA         REMAMT       15Y    20   3   60EDTCDE(1)
                                             3'Debit/Credit No
                                                                                     r
     A                                  4
     A                                           COLOR(WHT)
     A                                  4   20 ' : '
     A                                           COLOR(WHT)
     A         DRCRNO1       1A     B   4   22
     A    30                                     DSPATR(RI)
     A    30                                     DSPATR(PC)
     A    31                                     DSPATR(PR)
     A         DRCRN023      2A     B   4   24
     A    30                                     DSPATR(RI)
     A    30                                     DSPATR(PC)
     A    31                                     DSPATR(PR)
     A         DRCRDESC      6A     0   4   28
                                        4   37'Payment Mode
                                                                                 ,   I
     A
     A                                           COLOR(WHT)
     A         MODEPAY       6A     B   4   53
     A    32                                     DSPATR(RI)
     A    32                                     DSPATR(PC)
     A    33                                     DSPATR(PR)
     A         MDDESCR      20A     O   4   60
     A         DESCR        17      O   5    3COLOR(WHT)
     A                                  6   20 ’ :   r




     A                                           COLOR(WHT)
     A         REFERNO      15A     B   5   22
     A    34                                     DSPATR(RI)
     A    34                                     DSPATR(PC)
     A    35                                     DSPATR(PR)
     A         ACTIME        4A     B   5   38
     A    87                                     DSPATR(RI)
     A    87                                     DSPATR(PC)
     A    88                                     DSPATR(PR)
     A         REFDESCR     3 5A    O   5   45
     A                                  6     3'Rec      SWIFT          (MT910):                 I



     A                                           COLOR(WHT)
     A         MT910BNK     11A     B   6   22
     A    85                                     DSPATR(RI)
     A    85                                     DSPATR(PC)
     A    86                                     DSPATR(PR)
     A         MT910BNKNM   35A     0   6   45

                                        7     3'Payable Amt:
                                                                             1
MYIACA
     A                                           COLOR(WHT)
MYIACA*                                 7   20':     I



MYIACA         PRINCUY       3A     B   7   17
     A    36                                     DSPATR(RI)
     A    36                                     DSPATR(PC)
     A    37                                     DSPATR(PR)
MYIACA         PRINAMT      15Y    2B   7   22
     A    38                                     DSPATR(RI)
     A    38                                     DSPATR(PC)
     A    39                                     DSPATR(PR)
                                                 EDTWRD(
                                                                  1
                                                                                                     0 .
                                                                                                           r
MYIACA*                                                                                  /
MYIACA                                      EDTCDE(l)
MYIACA                                7   44'Payment Amt:
                                                                      r



     A                                      COLOR(WHT)
MYIACA         PAYMCUY     3A    B    7   57
     A    40                                   DSPATR(RI)
     A    40                                   DSPATR(PC)
     A    41                                   DSPATR(PR)
MYIACA         PAYMAMT    15Y   2B    7   61
     A    42                                   DSPATR(RI)
     A    42                                   DSPATR(PC)
     A    43                                   DSPATR(PR)
MYIACA*                                        EDTWRD('                       0.   ' )
MYIACA                                         EDTCDE(l)
     A                                8    3'Ex Rate Type
                                                                      r



     A                                      COLOR(WHT)
     A                                8   20':   1



     A                                         COLOR(WHT)
     A         FXRATETY    2A    B    8   22
     A    44                                   DSPATR(RI)
     A    44                                   DSPATR(PC)
     A    45                                   DSPATR(PR)
MYIACA    82                          8   44'Account       No         ,   1



     A                                         COLOR(WHT)
                                      9   44'Cheque No
                                                                      ,   1
MYIACA    83
     A                                         COLOR(WHT)
     A         SERLN015   15A    B    8   59
     A    28                                   DSPATR(RI)
     A    28                                   DSPATR(PC)
     A    29                                   DSPATR(PR)
     A    84                                   DSPATR(UL)
     A                                9    3'Deal/FW#       1



     A                                         COLOR(WHT)
     A                                9   20':
                                                 I



     A                                         COLOR(WHT)
     A         FXCONTR    12A    B    9   22
     A    46                                   DSPATR(RI)
     A    46                                   DSPATR(PC)
     A    47                                   DSPATR(PR)
     A         FXNAME     35A    0    9   45
     A                               10    3'Deal    Class t
     A                                         COLOR(WHT)
     A                               10   20':
     A                                         COLOR(WHT)
     A         DEALCLAS    1A    B   10   22
     A    72                                   DSPATR(RI)
     A    72                                   DSPATR(PC)
     A    73                                   DSPATR(PR)
MYIACA                               10   44'Deal Type          I     :
                                                                          r



     A                                         COLOR(WHT)
     A         DEALTYP     1A    B   10   59
     A    74                                DSPATR(RI)
     A    74                                DSPATR(PC)
     A    75                                DSPATR(PR)
     A                               11    3'A/C Class          3 1
     A                                      COLOR(WHT)
     A                               11   20':   I



     A                                         COLOR(WHT)
     A         ACCCLAS3    3A    B   11   22
     A    76                                   DSPATR(RI)
     A    76                                   DSPATR(PC)
     A    77                                   DSPATR(PR)
                                          44'Fund Method              :
                                                                          r
MYIACA                               11
     A                                         COLOR(WHT)
     A         FUNDMTHD    1A    B   11   59
     A    78                                   DSPATR(RI)
     A    78                                   DSPATR(PC)
     A    79                                   DSPATR(PR)
                                     12    3'Exchange Rate
                                                                            1
     A
     A                                         COLOR(WHT)
     A                               12   20':   1




     A                                      COLOR(WHT)
     A         EXCHRATE   16Y   7B   12   22EDTCDE(3)
     A    50                                DSPATR(RI)
     A    50                                DSPATR(PC)
     A    51                                DSPATR(PR)
                                     12   44'Cross               Rate
                                                                        ●   r
MYIACA
     A                                      COLOR(WHT)
     A         CONTRRTE   16Y   7B   12   59EDTCDE(3)
     A    48                                DSPATR(RI)
     A    48                                DSPATR(PC)
     A    49                                DSPATR(PR)
     A         GHLCY       3     O   13    3COLOR(WHT)
                                                     I
     A                               13    7'Amt
     A                                         COLOR(WHT)
                                     13   20':
                                                 t
     A
     A                                         COLOR(WHT)
     A         LCEAMT     15Y   2B   13   22
     A    52                                   DSPATR(RI)
     A    52                                   DSPATR(PC)
     A    53                                   DSPATR(PR)
     A                                         EDTCDE(1)
ID1L2A*                              13   45'MT950 t
                                     13   51'OWN
                                                     r
ID1L2A*
                                     13   55'RWF:
                                                         1
ID1L2A*
H03VKA*                              17   45'MT950 I
H03VKA*                              17   51'OWN r
                                     17   55'REF:
                                                         1
H03VKA*
                                     17   48'MT950
                                                             1
     A
     A                                         COLOR(WHT)
     A                               17   54'Own I
     A                                         COLOR(WHT)
     A                               17   58'Ref:        1



     A                                         COLOR(WHT)
ID1L2A*        OWNREF     16A    B   13   60
H03VKA*        OWNREF     16A    B   17   60
     A         OWNREF     16A    B   17   63
     A    89                                DSPATR(RI)
     A    89                                DSPATR(PC)
     A    90                                DSPATR(PR)
     A                               14    3'MT103 Inst Amt(33B)
     A                                      COLOR(WHT)
     A         INSTRCUY    3A    B   14   23
     A    91                                   DSPATR(RI)
     A    91                                   DSPATR(PC)
     A    92                                   DSPATR(PR)
     A         INSTRAMT   15Y   2B   14   27
     A    93                                DSPATR(RI)
     A    93                                DSPATR(PC)
     A    94                                DSPATR(PR)
     A                                      EDTCDE(1)
     A                               14   48'Ex Rate (36):                  1



     A                                      COLOR(WHT)
     A         EXCHRTE    12A    B   14   62
     A    95                                   DSPATR(RI)
     A    95                                   DSPATR(PC)
     A    96                                   DSPATR(PR)
     A    25                         18    4'FI      CODE 1
     A                                         COLOR(WHT)
     A    25   FICODE      3A    B   18   12
5Q1ARA*   07                                   DSPATR(RI)
5Q1ARA*   07                                   DSPATR(PC)
5Q1ARA    62                                   DSPATR(RI)
5Q1ARA    62                                    DSPATR(PC)
MYIAJA*   08                                    DSPATR(PR)
MYIAJA    19                                    DSPATR(PR)
     A    25                          18 18'Routing No
                                                                        1



     A                                          COLOR(WHT)
     A    25   RTGNUM       9Y   00   18   30
     A    25                          19    4'FI       Name r
     A                                          COLOR(WHT)
     A    25   FINAME      40A    0   19   12
     A    97                          15    3'FLD 55/56/77(Y/N):
                                                                                             r



     A                                       COLOR(WHT)
     A    97   OPT56IND     1A    B   15   22
     A    21                                 DSPATR(RI)
     A    21                                 DSPATR(PC)
     A    22                                 DSPATR(PR)
     A    97                          15   28'MT103 Time                    Indication           (13C):
                                                                                                          I



     A                                       COLOR(WHT)
     A    97   TI13CCODE    8A    B   15   57
     A    23                                    DSPATR(RI)
     A    23                                    DSPATR(PC)
     A    24                                    DSPATR(PR)
     A    97                          15 66’/      I



     A    97   TI13CTI      4Y   OB   15   68
     A    23                                    DSPATR(RI)
     A    23                                    DSPATR(PC)
     A    24                                    DSPATR(PR)
     A    97   TI13CSIGN    1A    B   15   73
     A    23                                    DSPATR(RI)
     A    23                                    DSPATR(PC)
     A    24                                    DSPATR(PR)
     A    97   TI13CTO      4Y   OB   15   75
     A    23                                    DSPATR(RI)
     A    23                                    DSPATR(PC)
     A    24                                    DSPATR(PR)
     A    80                          16    3'Remit to Country
                                                                                     r



     A                                          COLOR(WHT)
     A    80                          16   20';'
     A                                          COLOR(WHT)
     A    80   CNTRYCD      2A    B   16   22
     A    56                                    DSPATR(PC)
     A    56                                    DSPATR(RI)
     A    57                                    DSPATR(PR)
     A    80   CNTDESCR    11A   0    16   26
     A                                16   39'Remarks           :
                                                                    1



     A                                          COLOR(WHT)
     A         REMARK      30A    B   16   49
     A    54                                    DSPATR(RI)
     A    54                                    DSPATR(PC)
     A    55                                    DSPATR(PR)
ID1L2A*                               17   39'EMPLOYEE              :
ID1L2A*        EMPFLG       1A   0    17   50DSPATR(UL)
                                      13   44'Employee
                                                                             «   1
MYIACA
     A                                       COLOR(WHT)
     A         EMPFLG       1A   0    13   59DSPATR(UL)
     A*   80                          17    3'LOCATION 1
H03VKA*   80                          17   20':'
     A    80                          17    3'Location:             I



     A                                          COLOR(WHT)
H03VKA*   80   LOCATNCD     2A   B    17   22
     A    80   LOCATNCD     2A   B    17   13
     A    58                                    DSPATR(RI)
     A    58                                    DSPATR(PC)
     A    59                                    DSPATR(PR)
H03VKA*   80   LOCDESCR    11A   0    17   26
     A    80   LOCDESCR    11A   0    17   16
                                      17 28'RTGS Cover(Y/N)                          :
                                                                                         1
     A    09
    A                                            COLOR(WHT)
    A    09      RTGSCVR     1A   B    17   46
    A    08                                      DSPATR(RI)
    A    08                                      DSPATR(PC)
    A    05                                      DSPATR(PR)
                                             3'Paying Bank ID
                                                                             1
    A    80N20                         18
    A                                         COLOR(WHT)
    A    80N20                         18   20':   I



    A                                            COLOR(WHT)
    A    80N20   PYBNKID    11A   B    18   22
    A    60                                      DSPATR(RI)
    A    60                                      DSPATR(PC)
    A    61                                      DSPATR(PR)
    A    80N20                         18   35’A/C     1



    A                                            COLOR(WHT)
    A    80N20   PYBNKAC    15A    B   18   39
    A    26                                      DSPATR(RI)
    A    26                                      DSPATR(PC)
    A    27                                      DSPATR(PR)
                                       18   55'Chip
                                                           t
    A    80N20
    A                                            COLOR(WHT)
    A    80N20   CHIPNO      6A    B   18   60
    A    98                                      DSPATR(RI)
    A    98                                      DSPATR(PC)
    A    99                                      DSPATR(PR)
    A    80      PYSNAME    11A    0   18   67
    A    80                            19    3'Settlmt           Bank       ID I
    A                                            COLOR(WHT)
    A    80                            19   20':   I



    A                                            COLOR(WHT)
    A    80      SETBNKID   11A    B   19   22
    A    64                                      DSPATR(RI)
    A    64                                      DSPATR(PC)
    A    65                                      DSPATR(PR)
    A    80                            19 35'A/C       r




    A                                            COLOR(WHT)
    A    80      SETBNKAC   15A    B   19   39
    A    66                                      DSPATR(RI)
    A    66                                      DSPATR(PC)
    A    67                                      DSPATR(PR)
    A    80      SETSNAME   11A    O   19   67
    A    80N20                         20    3'Reimb           Bank   ID t
    A                                            COLOR(WHT)
                                       20   20':
                                                   r
    A    80N20
    A                                            COLOR(WHT)
    A    80N20   REIMBNK    11A    B   20   22
    A    68                                      DSPATR(RI)
    A    68                                      DSPATR(PC)
    A    69                                      DSPATR(PR)
    A    80N20                         20 35’A/C       1



    A                                            COLOR(WHT)
    A    80N20   REIMBAC    15A    B   20   39
    A    70                                      DSPATR(RI)
    A    70                                      DSPATR(PC)
    A    71                                      DSPATR(PR)
    A    80      REMSNAME   11A    O   20   67
    A    81                            21    3'Hided I
    A                                            COLOR(WHT)
    A    81      HOLDCD1     2S   OO   21   10
    A    81      HCDESCR1   30A    O   21   13
    A    81      HOLDCD2     2S   OO   21   45
    A    81      HCDESCR2   30A    O   21   48
    A    81      HOLDCD3     2S   OO   22   10
    A    81      HCDESCR3   30A    0   22   13
6Q4J1A   17                            22 54'FX Deal Y/N?
                                                                        r




6Q4J1A                                           COLOR(WHT)
6Q4J1A    17          FXCNFRM        1A   B   22   67
6Q4J1A    18                                            DSPATR(RI)
6Q4J2A    16                                            DSPATR(PC)
     A            R   RSDPAYCE
     A*    89/10/23    19:03:40    MATILDA          REL-R01M02       5728-PW1
     A                                                  TEXT('RSDPAYC    ERROR   SCREEN RECORD')
     A                                                  OVERLAY
     A                SCRMSGID       7A   0   23    2
     A                                                  DSPATR(BL)
     A                                                  DSPATR(HI)
     A                                                  COLOR(PNK)
     A                SCRDASH        1A   0   23   11
     A                                                  DSPATR(HI)
     A                                                  COLOR(PNK)
     A                SCRMSGTEXT    50A   0   23   14
     A                                                  DSPATR(HI)
     A                                                  COLOR(PNK)
MPIMBA*                                       24    2'ENTER       F3=Exit   F6=Cancel   I



MPIMBA                                        24    2'Enter       F2=Incoming Message       F3=Exi-
5Q1ARA*                                                 t   F6=Cancel
5Q1ARA                                                  t   F6=Cancel   F7=Reason I
     A                                                  COLOR(BLU)
     A*%%TS     SD       20151203        105607        TMPJZM                     REL-V6R1M0           5761-WDS
