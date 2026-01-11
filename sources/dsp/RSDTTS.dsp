      ****** -k it ***************************************************                                                                                                             ** /
     A*                             AMENDMENT          HISTORY
     pA* ************************************************************ * ^
     A* DATE             BY..     AMENDMENT                                                                      TAG
     A*                                                                                                                 /
     A*26/11/15          JZM      REM    2016    Q1    RELEASE          E-REQUEST#             44832        6Q1J2
     A*                           MAS626 GTD and Loans Registered Address
     A*                           Add    (ND)    indicator             to    ensure          that   the    new
     A*                           Gust ID indicator field will only be displayed
     A*                           when GTD/LNS flag is ON.
     A*                                                                                                                 /
     A*16/11/15          JZM      REM    2016    Q1    RELEASE E-REQUEST#                      44832        6Q1J1
     A*                           MAS626 GTD and Loans Registered Address
     A*                           Add Gust       ID    indicator             to    determine         if    the
     A*                           Gust ID field is for Loans or Fund Deposit
     A*                                                                                                                 /
     A*29/10/14          GGB        14HOREM017          REM       VPM       ENHANGEMENT                    GHGB1       */
     A*                             ADD REGEIVED             DATE,          REGEIVE                                    */
     A*                             TIME                                                                               */
     A*                                                                                                                 /
     A*

     A*09/12/ll          LCN      UOBI PROJEGT-ADD DISPLAY FOR EMP FLAG                                    IDILNI
     A*26/09/10          VKE      RTGS GHANGES                                                             RTGVKE

     A*23/01/09          RGB      WSS BRANCH                                                               090001
     A*                           ALLOW    THE    USER       TO       KEY    IN    TRANSAGTION            BRANCH
     A*                           (TXN BR) IN ADDITION TO THE ACCOUNT                                  BRANCH
     A*                           (ACC BR).
     A*                                                                                                                 /
     A*29/01/08          RGB      CR#:    CNREM/CR/2007/12/002                                             RB7122
     A*                           PRJ:    DRGH071202
     A*                           INCLUDE       TRANSACTION             BRANCH       INPUT
     A*                           FOR    WALK-IN       CUSTOMER.
     A*                                                                                                                 /
     A*16/05/07          MAF      Add Function Key Fll                                                           MAF

     A*07/05/07          JTC      Added    the    customer             id    field                               JTC
     A*                                                                                                                 /
     A*26/07/13          AOC      Change label to small                      caps                                IDlAl


     A*     91/07/27       11:09:02             ACSWTL                  REL-R03M00              5728-PW1
     A*%%EC
     A                                                                      DSPSIZ(24          80   *DS3)
     A                                                                      PRINT

     A*     91/04/08       17:15:57             ACSYSC                  REL-R02M00              5728-PW1
     A               R    RSDTTSR1
     A*%%TS     SD       20151203        105607        TMPJZM                     REL-V6R1M0           5761-WDS
     A                                                                      TEXT(RSDTTS KEY SCREEN RECORD')
     A                                                                      CF03(03 'END OF PROGRAM')
     A                                                                      CF04(04 'PROMPT')
     A                                                                      CF06(06 'CANCEL')
     A                                                                      CF19U9 'CUSTINSTR' )
     A     90                                                               OVERLAY
     A                    GHENV                   5A     0        1     3
     A                                                            1    12'RSDTTS I
     A                    TRNMD                   6A     O        1    22
     A                    FUNDESCP               20A     O        1    34COLOR(WHT)
     A                    GHDATE                  8A     O        1    63
     A                    GHTIME                  8A     0        1    72
IDIAIA*                                                           3     2'TRN       NO 1
                                                                  3     2'Trn       No
                                                                                         1
     A
     A                                                                      COLOR(WHT)
     A                    TRNNO                  12A     0        3     9
     A     30                                                               DSPATR(RI)
IDIAIA*                                                           3    45'REG.NO'
     A                                                            3    44'Reg.No'
     A                                                                   COLOR(WHT)
IDIAIA*        REGNO       4Y    OB   3   52
     A         REGNO       4Y    OB   3   51
     A    29                                   DSPATR(RI)
     A    29                                   DSPATR(PC)
     A    28                                   DSPATR(PR)
     A                                         EDTCDE(4)
                                      3   57'NORMAL/VALUED(N/V)
                                                                                 I
IDIAIA*
     A                                3   59'Normal/Valued(N/V)
     A                                         COLOR(WHT)
IDIAIA*        CUSTTYP     1A     B   3   76
     A         CUSTTYP     1A     B   3   78
     A    35                                   DSPATR(RI)
     A    35                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
     A*JTC                            4    2'REMITTER A/C NO                 (A/C HOLDER)
     A*JTC     ACCCUY      3A     B   4   42
                                                            r
IDIAIA*                               4    2'REMITTER

     A                                4    2'Remitter I
     A                                         COLOR(WHT)
     A         ACCCUY      3A     B   4   11
     A    32                                   DSPATR(RI)
     A    33                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
     A*JTC     ACCNO      15A     B   4   47
     A         ACCNO      15A     B   4   16
     A    34                                   DSPATR(RI)
     A    34                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
IDIAIA*                               5    2'REMITTER           NAME       AND   ADDRESS 1
     A                                5    2'Remitter           Name       and Address 1
     A                                         COLOR(WHT)
     A         REMRNAME   35A     B   5   42
     A    36                                   DSPATR(RI)
     A    36                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
     A         REMRADR1   35A     B   6    2
     A    37                                   DSPATR(RI)
     A    37                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
     A         REMRADR2   35A     B   6   42
     A    37                                   DSPATR(RI)
     A    37                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
     A         REMRADR3   35A     B   7    2
     A    37                                   DSPATR(RI)
     A    37                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
     A         REMRADR4   3 5A    B   7   42
     A    37                                   DSPATR(RI)
     A    37                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
IDIAIA*                               8    2'REMIT    AMOUNT t
     A                                8    2'Remit    Amt I
     A                                         COLOR(WHT)
     A         REMCUY      3A     B   8   12
     A    38                                   DSPATR(RI)
     A    39                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
     A         REMAMT     15Y    2B   8   17
     A    40                                   DSPATR(RI)
     A    40                                   DSPATR(PC)
     A    83                                   DSPATR(PR)
     A                                         EDTCDE(1)
IDIAIA*                               8   42'ORDERING           INST t
     A                                8   42'Ordering           Inst
                                                                       1


     A                                         COLOR(WHT)
     A          ORDINSTID   11A   B    8   57
     A     21                                   DSPATR(RI)
     A     21                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
IDIAIA*                                9    2’REMIT    TO    :
                                                                 I



     A                                 9    2'Remit    to Country
                                                                                  1



     A                                          COLOR(WHT)
     A          CNTRYCD      2A   B    9   20
     A     41                                   DSPATR(RI)
     A     42                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
     A          CNTDESCR    11A   0    9   23
IDIAIA*                                9   42'LOCATION t
     A                                 9   42'Location I
     A                                          COLOR(WHT)
     A          LOCATNCD     2A   B    9   57
     A     43                                   DSPATR(RI)
     A     44                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
     A          LOCDESCR    11A   0    9   60
IDIAIA*                               10    2'BENEFICIARY            OPTION t
     A                                10    2'Beneficiary Opt
                                                                              1



     A                                          COLOR(WHT)
     A          BENEOPT      1A   B   10   20
     A     22                                   DSPATR(RI)
     A     22                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
IDIAIA*                               10 26'(A/NIL)     t



     A                                10 23'(A/Nil)
                                                        1



     A                                          COLOR(WHT)
IDIAIA*                               10   42'BENEFICIARY            ID
                                                                          1



     A                                10 42'Beneficiary Id
     A                                          COLOR(WHT)
     A          SWIFTID     11A   B   10   57
     A     23                                   DSPATR(RI)
     A     23                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
     A          BENEACCNO   34A   B   11   42
     A     24                                   DSPATR(RI)
     A     24                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
     A          BENENAME    35A   B   13    2
     A     45                                   DSPATR(RI)
     A     45                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
     A          BENEADR1    35A   B   13   42
     A     46                                   DSPATR(RI)
     A     46                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
     A          BENEADR2    35A   B   14    2
     A     46                                   DSPATR(RI)
     A     46                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
     A          BENEADR3    35A   B   14   42
     A     46                                DSPATR(RI)
     A     46                                DSPATR(PC)
     A     83                                DSPATR(PR)
IDILNA**                              15    2'HOLD ADVICE            (Y/N)
IDILNA**        HOLDIND      1A   B   15   20
                                      15    2'HOLD ADVICE(Y/N)
                                                                                  1
IDILNA*
                                      15    2'Hold Advice(Y/N)
                                                                                  I
     A
     A                                       COLOR(WHT)
     A          HOLDIND      1A   B   15   19
     A     48                                   DSPATR(RI)
     A     48                                   DSPATR(PC)
     A     83                                   DSPATR(PR)
IDILNA**                             15 25'RTGS      (Y/N)       I



IDILNA**        TT         1A   B    15   36
     A                               15 22'RTGS(Y/N)         I



     A                                         COLOR(WHT)
     A          TT         1A    B   15   32
     A     49                                  DSPATR(RI)
     A     49                                  DSPATR(PC)
     A     83                                  DSPATR(PR)
                                     15 42'BR-TT       (Y/N)
                                                                     1
IDILNA**
IDILNA**        BRCHIND    1A    B   15   55
     A                               15 35'BR-TT(Y/N)            t



     A                                         COLOR(WHT)
     A          BRCHIND    1A    B   15   46
     A     78                                  DSPATR(RI)
     A     78                                  DSPATR(PC)
     A     83                                  DSPATR(PR)
IDILNA**                             15 63'PC-TT       (Y/N)         t



IDILNA**        PCTTIND    1A    B   15   76
     A                               15 49'PC-TT(Y/N)'
     A                                         COLOR(WHT)
     A          PCTTIND    1A    B   15   60
     A     79                                  DSPATR(RI)
     A     79                                  DSPATR(PC)
     A     83                                  DSPATR(PR)
IDILNA*IDlAl                         15 63'EMPLOYEE(Y/N)                  I



                                     15 63'Employee(Y/N)
                                                                          1
     A
     A                                      COLOR(WHT)
IDIAIA*         EMPFLAG    1A    B   15   76DSPATR(PR)
     A          EMPFLAG    1A    B   15   77DSPATR(PR)
IDIAIA*                              11    2'BENEFICIARY                 ACCOUNT      NO I
                                           2'Beneficiary Account No
                                                                                         I
     A                               11

     A                                         COLOR(WHT)
                                           2'BENEFICIARY                 NAME   AND    ADDRESS
                                                                                                 I
IDIAIA*                              12
                                           2'Beneficiary Name and Address
                                                                                                 1
     A                               12
     A                                         COLOR(WHT)
     A                                4   34'OUST-ID r
     A                                         COLOR(WHT)
6Q1J1A     97   CUSTIND    1A    B    4   42

6Q1J1A     96                                  DSPATR(RI)
6Q1J1A     96                                  DSPATR(PC)
6Q1J1A     83                                  DSPATR(PR)
6Q1J2A*         CUSTID    35A    B    4   42

6Q1J2A          CUSTID    35A    B    4   44
     A     10                               DSPATR(RI)
     A     10                               DSPATR(PC)
     A     83                               DSPATR(PR)
                                      3   24'INC QUENUM
                                                                 I
IDIAIA*
                                      3   23'Inc    Quenum
                                                                 1
     A
     A                                      COLOR(WHT)
     A     12                               DSPATR(ND)
IDIAIA*         QUENUM     8Y   OB    3   35EDTCDE(4)
     A          QUENUM     8Y   OB    3   34EDTCDE(4)
     A     13                               DSPATR(RI)
     A     13                               DSPATR(PC)
     A     14                               DSPATR(PR)
     A     12                               DSPATR(ND)
     A                               12   42'Rev    Date r
     A                                         COLOR(WHT)
     A          RECDATE    8Y   OB   12   51
     A     98                                  DSPATR(RI)
     A     98                                  DSPATR(PC)
     A     83                                  DSPATR(PR)
     A                               12   63'Rev    Time I
     A                                         COLOR(WHT)
     A          RECTIME    4Y   OB   12   72
     A     99                                  DSPATR(RI)
     A    99                                                        DSPATR(PC)
     A    83                                                        DSPATR(PR)
     A*    91/07/27        11:09:02       ACSWTL                REL-R03M00                5728-PW1
     A               R    RSDTTSY
     A*%%TS     SD       20130801     153704        TMP3CA               REL-V6R1M0           5761-WDS
     A                                                              TEXT('RSDTTS CONFIRM SCREEN RECORD')
     A                                                              CF03(03 'END OF PROGRAM')
     A                                                              OVERLAY
IDIAIA*   82                                              22   64'CONFIRM
                                                                                 I



     A    82                                              22   64'Confirm I
     A                                                              COLOR(WHT)
     A    82              CONFIRM              1A     B   22   72
     A    80                                                        DSPATR(RI)
     A    81                                                        DSPATR(PC)
     A    82                                              22 74'(Y/N)       I


     A                                                              COLOR(WHT)
     A*    91/07/27        11:09:02       ACSWTL                REL-R03M00                5728-PW1
     A               R    RSDTTSE
     A*%%TS     SD       20130801     153704        TMP3CA               REL-V6R1M0           5761-WDS
     A                                                              TEXT('RSDTTS            ERROR       SCREEN RECORD')
     A    91                                                        OVERLAY
     A                    SCRMSGID             7      0   23    2DSPATR(BL)
     A                                                           DSPATR(HI)
     A    89                                                        ERRMSGID(CPF5204                  *LIBL/QCPFMSG)
     A                                                           COLOR(PNK)
     A                    SCRDASH              1      0 23     IIDSPATR(HI)
     A                                                           COLOR(PNK)
     A                    SCRMSGTEXT          50      O   23   14DSPATR(HI)
     A                                                           COLOR(PNK)
     A    86                                              24    2'ENTER
                                                                            1



     A                                                              COLOR(BLU)
MYIACA*   87                                              24    2'ENTER          F3=Exit F4=Prompt F6=Cancel-
MYIACA*                                                              F19=Cust            Instr.
                                                                                                  1



MYIACA    87                                              24    2'F3=Exit F4=Prompt F6=Cancel F19=Cu-
MYIACA                                                              st   Instr.      t



     A                                                              COLOR(BLU)
626AFA**88                                                24    2'ENTER          F3=Exit 1
MYIACA*   88                                              24    2'ENTER   F3=Exit  Fll=A/CAddr
                                                                                                                  I



MYIACA    88                                              24    2'F3=Exit   F11=A/C Addr
                                                                                                            I



     A                                                              COLOR(BLU)
     A*    91/07/27        11:09:02       ACSWTL                REL-R03M00                5728-PW1
     A               R    RSDTTSR2
     A*%%TS     SD       20130805     190019        TMP3CA               REL-V6R1M0           5761-WDS
     A                                                              TEXT('RSDTTSR BODY 2 OF SCREEN')
     A                                                              CF03(03 'END OF PROGRAM')
     A                                                              CF04(04 'PROMPT')
     A                                                              CF06(06 'END OF BODY')
     A                                                              CFlKll 'REMADDR')
     A                                                              CF19(19 'CUSTINSTR')
     A                                                              OVERLAY
IDIAIA*   90                                              16    2'VALUE         DATE I
     A    90                                              16    2'Value         Date I
     A                                                              COLOR(WHT)
     A    90             VALUEDTE              6Y    OB   16   13
     A    31                                                        DSPATR(RI)
     A    31                                                        DSPATR(PC)
     A    84                                                        DSPATR(PR)
     A                                                              EDTCDE(4)
     A*RTGVKE    RTGS      CHANGES    START
                                                          16 20'FLD 55/56/77                 (Y/N)
                                                                                                        t
     A*   94
     A*   94             TAG56IND              1A     B   16   39
                                                          16 20'FLD 55/56/77                 (Y/N)
                                                                                                        r
     A    07
     A                                                              COLOR(WHT)
     A    07             TAG56IND              1A     B   16   39
     A    47                                                        DSPATR(RI)
     A    47                                                        DSPATR(PC)
     A    84                                                    DSPATR(PR)
IDIAIA*                                               16   42'BR-REF'
     A                                                16   42'BR-Ref
     A                                                          COLOR(WHT)
     A    90           BRTTREF             16A    B   16   49
     A    76                                                    DSPATR(RI)
     A    76                                                    DSPATR(PC)
     A    77                                                    DSPATR(PR)
     A    90           BRTTTME              4Y   OB   16   66
     A    92                                                    DSPATR(RI)
     A    92                                                    DSPATR(PC)
     A    77                                                    DSPATR(PR)
     A    90           BRTTDTE              6Y   OB   16   71
     A    95                                                    DSPATR(RI)
     A    95                                                    DSPATR(PC)
     A    77                                                    DSPATR(PR)
                                                      17   28'PC-REF
                                                                       r
IDIAIA*   90
     A    90                                          17   28'PC-Ref r
     A                                                          COLOR(WHT)
     A    90           PCTTREF             30A    B   17   35
     A    98                                                    DSPATR(PR)
     A    99                                                    DSPATR(RI)
     A    99                                                    DSPATR(PC)
     A    90           PCTTTME              4Y   OB   17   66
     A    97                                                    DSPATR(RI)
     A    97                                                    DSPATR(PC)
     A    98                                                    DSPATR(PR)
     A*RTGVKE   RTGS    CHANGES    START
     A*   94                                          18    2'MT103    TIME       INDICATION           (13C)
     A*   94           TI13CCODE            8A    B   18   30
                                                            2'MT103 TIME          INDICATION           (13C)
                                                                                                               1
IDIAIA*   07                                          18
                                                            2'MT103 Time          Indication           (13C)
                                                                                                               I
     A    07                                          18
     A                                                       COLOR(WHT)
     A    07           TI13CCODE            8A    B   18   35
     A    25                                                    DSPATR(RI)
     A    25                                                    DSPATR(PC)
     A    84                                                    DSPATR(PR)
     A*RTGVKE   RTGS    CHANGES    START
     A*   94                                          18   39'/   1



     A*   94           TI13CTI              4Y   OB   18   41
     A    07                                          18 44'/     1



     A    07           TI13CTI              4Y   OB   18   46
     A    25                                                    DSPATR(RI)
     A    25                                                    DSPATR(PC)
     A    84                                                    DSPATR(PR)
     A                                                          RANGE(0 2400)
     A*RTGVKE   RTGS    CHANGES    START
     A*   94           TI13CSIGN            1A    B   18   46
     A    07           TI13CSIGN            1A    B   18   51
     A    25                                                    DSPATR(RI)
     A    25                                                    DSPATR(PC)
     A    84                                                    DSPATR(PR)
     A                                                          VALUES(
                                                                       1     t   1-1       1
                                                                                               + ' )
     A*RTGVKE   RTGS    CHANGES    START
     A*   94           Til3CTO              4Y   OB   18   48
     A    07           Til3CTO              4Y   OB   18   53
     A    25                                                    DSPATR(RI)
     A    25                                                    DSPATR(PC)
     A    84                                                    DSPATR(PR)
     A                                                          RANGE(0 2400)
IDIAIA*   93                                          19    2'BENE    BANK       ID 1
     A    93                                          19    2'Bene    Bank       Id 1
     A                                                          COLOR(WHT)
IDIAIA*   94                                          19    2'PAYING       BANK     ID I
     A    94                                          19    2'Paying Bank Id
                                                                                        1



     A                                                          COLOR(WHT)
     A    93    BENEBNKID   11A   B   19   17
     A    72                                    DSPATR(RI)
     A    73                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
     A    94    PYBNKID     11A   B   19   17
     A    50                                    DSPATR(RI)
     A    51                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
     A    94                          19 30'A/C
     A    94    PYBNKAC     15A   B   19   35
     A    52                                    DSPATR(RI)
     A    53                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
IDIAIA*   94                          19   52'CHIP t
     A    94                          19 52'Chip
                                                        t



     A                                          COLOR(WHT)
     A    94    CHIPNO       6A   B   19   59
     A    74                                    DSPATR(RI)
     A    75                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
     A    94    PYSNAME     11A   0   19   66
IDIAIA*   94                          20    2'SETL          BANK    ID t
     A    94                          20    2'Setl          Bank    Id I
     A                                          COLOR(WHT)
     A    94    SETBNKID    11A   B   20   17
     A    54                                    DSPATR(RI)
     A    55                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
     A    94                          20 30'A/C     I


     A    94    SETBNKAC    15A   B   20   35
     A    56                                    DSPATR(RI)
     A    57                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
IDIAIA*   94                          20   52'CRTIME I
     A    94                          20   52'CrTime         I



     A                                          COLOR(WHT)
     A    94    CRTIME       4A   B   20   59
     A    15                                    DSPATR(RI)
     A    15                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
     A    94    SETSNAME    11A   O   20   66
IDIAIA*   94                          21    2'REIMB          BANK    ID
                                                                           I



     A    94                          21    2'Reimb          Bank    Id I
     A                                          COLOR(WHT)
     A    94    REIMBNK     11A   B   21   17
     A    58                                    DSPATR(RI)
     A    59                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
     A    94                          21 30'A/C
     A    94    REIMBAC     15A   B   21   35
     A    60                                    DSPATR(RI)
     A    61                                    DSPATR(PC)
     A    84                                    DSPATR(PR)
     A    94    REMSNAME    11A   0   21   66
     A*090001
IDIAIA*                               17    2'ACC       BR
                                      17    2'Acc       Br
                                                             I
     A
     A                                          COLOR(WHT)
     A    20                                    DSPATR(ND)
     A*090001
     A          TRNBRCHID    4A   B   17    9
     A    26                                    DSPATR(RI)
     A    26                                    DSPATR(PC)
     A    27                                    DSPATR(PR)
     A    20                                    DSPATR(ND)
IDIAIA*                               17   15'TXN       BR
                                                                                   I
     A                                                         17   15'Txn Br
     A                                                                   COLOR(WHT)
     A    18                                                             DSPATR(ND)
     A*090001
     A                       TRNBRCHID2             4A    B    17   22

     A    16                                                             DSPATR(RI)
     A    16                                                             DSPATR(PC)
     A    17                                                             DSPATR(PR)
     A    18                                                             DSPATR(ND)
     A*RTGVKE RTGS             CHANGES     START
                                                               18   55'RTGS       COVER         (Y/N)
                                                                                                        I

IDIAIA*   09
                                                                                                (Y/N)
                                                                                                        I
     A    09                                                   18   58'RTGS       Cover
     A                                                                   COLOR(WHT)
     A    09                  RTGSCVR               1A     B   18   76

     A    08                                                             DSPATR(RI)
     A    08                                                             DSPATR(PC)
     A    05                                                             DSPATR(PR)
     A*RTGVKE      RTGS        CHANGES     END
     A*090001
     A*   RB7122     -       END
                                                                     REL-R02M00                5728-PW1
     A*    91/04/08            17:15:57          ACSYSC
     A                   R    RSDTTSR3
     A*%%TS     SD           20130802      151906        TMP3CA             REL-V6R1M0                5761-WDS

     A                                                                   TEXT('RSDTTS KEY SCREEN RECORD')
     A    90                                                             OVERLAY
     A                        GHENV                 5A     0    1    3
                                                                1   11'RSDTTSR
                                                                                           t
     A
     A                        TRNMD                 6A     0    1   19
     A                        FUNDESCP             20A     O    1   31COLOR(WHT)
     A                        GHDATE                8A     O    1   62
     A                        GHTIME                8A     0    1   72

IDIAIA*                                                         5    4'TRN    NO 1
                                                                5    4'Trn    No
                                                                                       1
     A
     A                                                                   COLOR(WHT)
     A                        TRNNO                12A     0    5   28
     A    30                                                             DSPATR(RI)
IDIAIA*                                                         5   45'REMITTANCE               AMT
     A                                                          5   43'Remittance               Amt 1
     A                                                                COLOR(WHT)
                                                                5   62EDTWRD( I
                                                                                                                     t
IDIAIA*                       REPAMT               15Y    20
     A                        REPAMT               15Y    20    5   60EDTCDE(1)
IDIAIA*                                                         7    4'REPETITIVE               INSTRUCTION      «   I



                                                                7    4'Repetitive Instruction
                                                                                                                 .   1
     A
     A                                                                   COLOR(WHT)
IDIAIA*                                                         9    4'NO    OF    ITEMS
     A                                                          9    4'No    of    Items I
     A                                                                   COLOR(WHT)
     A                        REPECNT1              2Y    OB    9   21
     A    62                                                             DSPATR(RI)
     A    62                                                             DSPATR(PC)
     A    85                                                             DSPATR(PR)
     A                                                                   EDTCDE(4)
IDIAIA*                                                         928'AMT
     A                                                          928'Amt
     A                                                                   COLOR(WHT)
     A                        REPCUY                3A     0    9   33
     A                        REPEAT-lTl           15Y    2B    9   40
     A    62                                                             DSPATR(RI)
     A    62                                                             DSPATR(PC)
     A    85                                                             DSPATR(PR)
     A                                                                   EDTCDE(1)
     A                        REPECNT2              2Y    OB   10   21
     A    63                                                             DSPATR(RI)
     A    63                                                             DSPATR(PC)
     A    85                                                             DSPATR(PR)
     A                                                                   EDTCDE(4)
     A         REPEAMT2     15Y   2B   10   40
     A    63                                     DSPATR(RI)
     A    63                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(1)
     A         REPECNT3      2Y   OB   11   21
     A    64                                     DSPATR(RI)
     A    64                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(4)
     A         REPEAiyiT3   15Y   2B   11   40
     A    64                                     DSPATR(RI)
     A    64                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
MYIACA*                                          EDTWRD( I    1
                                                                  )
MYIACA                                           EDTCDE(1)
     A         REPECNT4      2Y   OB   12   21
     A    65                                     DSPATR(RI)
     A    65                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(4)
     A         REPEAMT4     15Y   2B   12   40
     A    65                                     DSPATR(RI)
     A    65                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(1)
     A         REPECNT5      2Y   OB   13   21
     A    66                                     DSPATR(RI)
     A    66                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(4)
     A         REPEAMT5     15Y   2B   13   40
     A    65                                     DSPATR(RI)
     A    65                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(1)
     A         REPECNT6      2Y   OB   14   21
     A    67                                     DSPATR(RI)
     A    67                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(4)
     A         REPEAMT6     15Y   2B   14   40
     A    67                                     DSPATR(RI)
     A    67                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(1)
     A         REPECNT7      2Y   OB   15   21
     A    68                                     DSPATR(RI)
     A    68                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(4)
     A         REPEAMT7     15Y   2B   15   40
     A    68                                     DSPATR(RI)
     A    68                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(1)
     A         REPECNT8      2Y   OB   16   21
     A    69                                     DSPATR(RI)
     A    69                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
     A                                           EDTCDE(4)
     A         REPEAMT8     15Y   2B   16   40
     A    69                                     DSPATR(RI)
     A    69                                     DSPATR(PC)
     A    85                                     DSPATR(PR)
MYIACA*                                          EDTWRD(
                                                        r
                                                                  )
MYIACA                                         EDTCDE(l)
     A        REPECNT9     2Y   OB   17   21
     A   70                                    DSPATR(RI)
     A   70                                    DSPATR(PC)
     A   85                                    DSPATR(PR)
     A                                         EDTCDE(4)
     A        REPEA.MT9   15Y   2B   17   40
     A   70                                    DSPATR(RI)
     A   70                                    DSPATR(PC)
     A   85                                    DSPATR(PR)
     A                                         EDTCDE(1)
     A        REPECNTIO    2Y   OB   18   21
     A   71                                    DSPATR(RI)
     A   71                                    DSPATR(PC)
     A   85                                    DSPATR(PR)
     A                                         EDTCDE(4)
     A        REPEAMTIO   15Y   2B   18   40
     A   71                                    DSPATR(RI)
     A   71                                    DSPATR(PC)
     A   85                                    DSPATR(PR)
     A                                         EDTCDE(1)
