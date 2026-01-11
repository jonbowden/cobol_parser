     A*%%TS       SD       20130819        103244             TMP3CA                     REL-V6R1M0          5761-WDS
     A*                      --    MODIFICATION               HISTORY
     A*   Date               Modification                                                            By
     A*

     A*   09/07/2014          REM    CNAPS         G2    PROJECT                                     TMPJC5
     A*                       -    CNAPS    G2      Enhancement
     A*   G2J01
     A*                                                                                                                 /
     A*02/08/2013             Screen       standardization                                           TMP3CA
     A*                      -Remove       Enter
     A*   MYIAC
     A*                                                                                                                 /
     A*   16/04/2012          UOBTH CORE RETAIL STANDARDIZATION                                      TMPJC3
     A*                       PROJECT
     A*                       Retrofit SA handling.
     A*                       Modify 'CA/FCCA OVERRIDE REQUIRED'                                      into
     A*                        SA/CA/FCCA OVERRIDE REQUIRED
                              I                                                            I



     A*   JTHO1
     A*                                                                                                                 /
     A*   21/11/2011          Add display record for validation,                                      if without
     A*                       GL    entries.
     A*   JCMO1                                                                           llPLUOBTREMOO1
     A*                                                                                                                 /
     A*   27/10/2010          Add Accounting Entries Incomplete                                      TMPJFC
     A*   CJFO1               message                                                     llPLUOBTREMOO1
     A*                                                                                                                 /
     A*   19/02/2010          Format       * * *
                                                   AMT to display                                    TMPGIH
     A*   103GIH              with decimal               (')    when amt             is    <   0
     A*                                                                                                                 /
     A* 11/08/2008 UOB HCMC Project - expand TOTALDR                                                 TMPARI
     A*   (VNIAR)            and TOTALCR            from       15,2       to       17,2
     A*

     A*   01/04/2009 CNAPS            PROJECT            - Add TPB Ref No                            Ciselle
     A*   CNAP1
     A*
     A*

     A*    91/04/12          15:19:53              ACSYSC                      REL-R02M00            5728-PW1
     A*%%EC
     A                                                                             DSPSIZ(24        80    *DS3)
     A                                                                             PRINT

     A*    91/04/12          15:19:53              ACSYSC                      REL-R02M00            5728-PW1
     A                 R    TSIACCGSR                                              SFL
     A*%%TS       SD       20030301        122641             UNCDCK                     REL-V4R4M0          5769-PW1
     A                                                                             TEXT('subfile           record')
     A                     DRCRC                         1A     O   10         2
     A                     ACCTCD                        7A     O   10         4
     A                     REFERNO                      13A     O   10     12
     A                      PRINCUY                      3A     O   10     26
     A                      PRINAMT                     15Y    20   10     30EDTCDE(3)
                                                        16Y    70   10     47EDTWRD(                          0 .               )
                                                                                               1                            I
     A                      EXCHRATE
     A                      LCEAMT                      15Y    20   10     65EDTCDE(3)
     A                      TRNNO                       12A     H
     A                      FUNCTID                      8A     H
     A                      ENTRYTYP                     2A     H

     A*    91/04/12          15:19:53               ACSYSC                     REL-R02M00            5728-PW1
     A                 R    TSIACCGSC                                              SFLCTL(TSIACCGSR)
     A*%%TS       SD       20130802        201204             TMP3CA                     REL-V6R1M0          5761-WDS
     A                                                                             SFLSIZ(0020)
     A                                                                             SFLPAG(OOIO)
     A    12                                                                       CF03(03         'EXIT')
     A    13                                                                       CF06(06         'CANCEL')
     A                                                                             OVERLAY
     A    30                                                                       SFLDSP
     A    31                                                                       SFLDSPCTL
     A    35                                                                       SFLEND(*MORE)
CNAPIA*                     GHENV                        5A     O     2        3
                                           2    9'TSIACCGS                  VIEW
                                                                                       r
CNAPIA*
                                           2   24'A/C ENTRIES                      -
                                                                                       I
CNAPIA*
CNAPIA*             FNDESP      20A    0   2   40

CNAPIA*             GHDATE       8A    0   2   62

CNAPIA*             GHTIME       8A    0   2   71
                                                                               .   1
CNAPIA*                                    4    2'TRN         NO

CNAPIA*             TRNNO       12A    0   4   16
                                                                                       ●   1
CNAPIA*                                    4   39'REM         AMT

CNAPIA*             REMCUY       3A    O   4   55
                                                                                                              t
                                           4   60EDTWRD(
                                                                   I
CNAPIA*             REMAMT      15Y   20
     A              GHENV        5A    0   1    3
                                           1    9’TSIACCGS                  VIEW
                                                                                       t
     A
                                           1   24'A/C ENTRIES
                                                                                       I
     A
     A                                           COLOR(WHT)
     A              FNDESP      20     0   1   40COLOR(WHT)
     A              GHDATE       8A    0   1   62
     A              GHTIME       8A    0   1   71
                                           3    2'Trn No
                                                                               «   1
     A
     A                                              COLOR(WHT)
     A              TRNNO       12A    O   3   16
                                                                                       .   1
     A                                     3   39'Rem         Amt
     A                                              COLOR(WHT)
     A              REMCUY       3A    0   3   55
                                           3   60EDTWRD('                                                         )
                                                                                                              t
CNAP1A**103GH       REMAMT      15Y   20                                                           /


                                           3   60EDTWRDC                                                0 .   t
MYIACA*             REMAMT      15Y   20
MYIACA              REMAMT      15Y   20   3   60EDTCDE(1)
     A    52                               4    2'TPB          Ref     No      ●   r



     A                                              COLOR(WHT)
G2J01A*   52        TPBREFN     33A    0   4   16
G2J01A    52        TPBREFN     35A    0   4   16
     A                                     5    2'Customer                     ●   r



     A                                              COLOR(WHT)
     A              CUSTNO      15A    0   5   16
                                           5 34'CA/FCCA OVERRIDE REQUIRED
                                                                                                                      I
JTHOIA*   51
                                           5 34'SA/CA/FCCA OVERRIDE REQUIRED
                                                                                                                          1
     A    51
     A                                              DSPATR(BL)
     A                                              DSPATR(RI)
     A                                              COLOR(PNK)
     A                                     6    2'Name                         :'
     A                                              COLOR(WHT)
     A              CUSTNAME    35A    O   6   16
     A                                     7    2 'D/     1



     A                                              COLOR(WHT)
     A                                     8    2 'C r
     A                                              COLOR(WHT)
     A                                     8    6'Code t
     A                                              COLOR(WHT)
     A                                     8   12'Reference                 No 1
     A                                              COLOR(WHT)
     A                                     8   26 I           Remittance               Amt
     A                                              COLOR(WHT)
     A                                     8   51'Exch          Rate 1
     A                                              COLOR(WHT)
     A                                     8   69'LCY         Amt 1
     A                                              COLOR(WHT)
     A                                     9    2 ' - I
     A                                     9    4 1
     A                                     9   12   1


     A                                     9   26 1                                                     I



     A                                     9   47                                                  1



     A                                     9   65                                              I



     A    53                               7   16'ACCOUNTING                 ENTRIES                   INCOMPLETE 1
     A                                              DSPATR(BL)
     A                                              DSPATR(RI)
     A                                              COLOR(PNK)
     A          R   TSIACCGSM
     A*%%TS    SD       20130819     103244        TMP3CA             REL-V6R1M0                     5761-WDS
     A    12                                                       CF03(03        'EXIT')
     A    13                                                       CF06(06        'CANCEL')
     A                                                             OVERLAY
     A                   GHENV                5A     0    1    3
     A                                                    1    9'TSIACCGM I
     A                                                    1   19'VIEW A/C ENTRIES
     A                                                          COLOR(WHT)
     A                   FNDESP           20A        0    1   38COLOR(WHT)
     A                   GHDATE               8A     0    1   62
     A                   GHTIME               8A     O    1   71
     A                                                    4   21'Please       be        informed            that     the   transa
     A                                                             ction t
     A                                                          COLOR(PNK)
     A                                                    6   21'has no GL              entries.            Please       walk   thr
     A
                                                                   ough
                                                                          I


     A                                                             COLOR(PNK)
     A
                                                          8 21'the transaction to generate the GL-
     A                                                              entries.       I



     A                                                             COLOR(PNK)
     A                                                   11   21'Particulars                     I



     A                                                             DSPATR(UL)
     A                                                             COLOR(WHT)
     A                                                   13   26'Trnno        «    r


     A                                                             COLOR(WHT)
     A                   TRNNO            12A        0   13   35
     A                                                   15   21'Function              ID    :   1



     A                                                             COLOR(WHT)
     A                   FUNCTID              8A     0   15   35
     A                                                   18   43'Please       read           the       information and          1



     A                                                             COLOR(PNK)
     A                                                   20   43'Input "Y
                                                                                  II
                                                                                        to       continue       ;'
     A                                                          COLOR(PNK)
     A                  RESP                  1A     B   20   67
     A    14                                                       DSPATR(RI)
     A    15                                                       DSPATR(PC)
     A*    89/10/11       18:32:06       TANK?                 REL-R01M02               5728-PW1
     A              R    TSIACCGSE
     A*%%TS    SD       20130802     201204        TMP3CA             REL-V6R1M0                     5761-WDS
     A    12                                                       CF03(03    'EXIT')
     A    13                                                       CF06(06    'CANCEL')
     A                                                             OVERLAY
JCMOIA*                 MSGTEXT           50A        0   23    2
     A                  MSGID                 7      0   23    3DSPATR(BL)
     A                                                          DSPATR(HI)
     A                                                          COLOR(PNK)
     A                  MSGDASH               1      O 23     12DSPATR(HI)
     A                                                          COLOR(PNK)
     A                  MSGTEXT           50         O 23     15DSPATR(HI)
     A                                                          COLOR(PNK)
MYIACA*                                                  24    3'ENTER        F3=Exit                 F6=Cancel I
                                                               3'F3=Exit          F6=Cancel
                                                                                                        r
MYIACA                                                   24
     A                                                             COLOR(BLU)
     A*             R    TSIACCGSK
     A*    89/10/11       18:32:06       TANKP                 REL-R01M02               5728-PW1
     A*                                                            OVERLAY
     A*   10                                             24    2'ENTER 1
                                                               2'ENTER        F3=Exit                 F6=Cancel
                                                                                                                     t
     A*   11                                             24

     A*    91/04/12       10:53:45       ACSYSC                REL-R02M00               5728-PW1
     A              R    TSIACCGST
     A*%%TS    SD       20130802     201204        TMP3CA             REL-V6R1M0                     5761-WDS
     A    12                                                       CF03(03    'EXIT')
     A    13                                                       CF06(06    'CANCEL')
     A                                                             OVERLAY
     A                                                   21   50'Total DR I
     A                                                          COLOR(WHT)
                          17Y   20 21     59EDTWRD(
                                                         1

VN1ARA**103GH   TOTALDR
MYIACA*         TOTALDR   17Y   20 21     59EDTWRD( t                0.        ')
MYIACA          TOTALDR   17Y   20 21     59EDTCDEU)
                                     22   50'Total OR
                                                             1

     A
     A                                      COLOR(WHT)
                                                                                   )
                                                                               1
                          17Y   20   22   59EDTWRD(
                                                         1
VN1ARA**103GH   TOTALCR
                                                                     0 .
                                                                               1
                          17Y   20   22   59EDTWRD(
                                                         1

MYIACA*         TOTALCR
MYIACA          TOTALCR   17Y   20   22   59EDTCDE(1)
                                           2'ENTER
                                                     I
MYIACA*   10                         24
                                                     F3=Exit F6=Cancel
                                                                           1

MYIACA*   11                         24    2'ENTER

MYIACA    11                         24    2'F3=Exit   F6=Cancel I




     A                                      COLOR(BLU)
