     A*%%TS    SD       20090323     171932           TMPRGB                        REL-V5R3M0        5722-WDS
     A************************************************************** /
     A*                            AMENDMENT          HISTORY
     A**************************************************************/
     A*    DATE   PGMR     DESCRIPTION                       NUMBER */
     A*                                                             */
     A*09/03/20  ACNTU1 - 2020 Q2 RELEASE                      20Q21*/
     A*                   E-REQUEST # 52995                         */
     A*                   PQR-36601                                 */
     A*                   JIRA PQR-36601                            */
     A*                 - Fix Debit Amount is Displayed without     */
     A*                   Decimal Point                             */
     A*                                                             */
     A*31/01/18  ACNSGD - 2019 Q2 OFF RELEASE                  9Q2S1*/
     A*                   E-REQUEST # 51068                         */
     A*                   IMPROVE IAFT PROCESSING                   */
     A*                   ALLOW DEBIT AMOUNT CURRENCY TO            */
     A*                   BE AN INPUT.                              */
     A*                                                             */
     A*01/02/16  TMPJP6 REM 2016 Q2 RELEASE                   6Q2J1 */
     A*                 - E-REQUEST# 46332 MERCURIA PHASE II        */
     A*                 - ADDED VALUE DATE FIELD TO CATER           */
     A*                   FOR BACK VALUE PROCESSING.                */
     A*                                                             */
     A* 15/12/14 TMPJZM 14HOREM017 REM VPM Enhancement        GHJM1 */
     A*                   ADD RECEIVED DATE AND RECEIVED TIME       */
     A*                                                             */
     A*11/02/14  TMPRS8 SCREEN STANDARDIZATION                GHIRS
     A*                             REMOVE       ENTER
     A*                             CHANGE       TEXT       CONSTANTS                TO    SMALL    LETTERS
     A*                             AND    FONT       COLOR          TO       WHITE
     A*                             CHANGE       FUNCTION             KEYS          FONT    COLOR    TO    BLUE
     A*                                                                                                             /
     A*23/01/09         RGB      WSS BRANCH                                                                090001
     A*                          ALLOW    THE    USER       TO       KEY       IN    TRANSACTION          BRANCH
     A*                          (TXN BR) IN ADDITION TO THE ACCOUNT BRANCH
     A*                          (ACC BR).
     A*                                                                                                             /
     A*    91/04/08       16:50:21          ACSYSC                        REL-R02M00           5728-PW1
     A*%%EC
     A                                                                        DSPSIZ(24       80    *DS3)
     A                                                                        PRINT

     A*    91/04/08       16:50:21          ACSYSC                        REL-R02M00           5728-PW1
     A              R    RSDIATSR
     A*%%TS    SD       20090323     171932           TMPRGB                        REL-V5R3M0        5722-WDS
     A                                                                        TEXT('RSDCOS KEY SCREEN RECORD')
     A                                                                        CF03(03 'END OF PROGRAM')
     A                                                                        CF04(04       'PROMPT')
     A                                                                        CF06(06       'CANCEL')
     A                                                                        CF19(19       'CUSTINSTR')
     A                                                                        OVERLAY
     A                   GHENV                   5A     0        1        3
     A                                                           1    10'RSDIATS I
     A                   TRNMD                   6A     O        1    19
GHIRSA*                  FUNDESCP               20A     O        1    34
GHIRSA                   FUNDESCP               20A     O        1 34COLOR(WHT)
     A                   GHDATE                  8A     O        1    62
     A                   GHTIME                  8A     O        1    72
GHIRSA*                                                          4        6'TRN NO 1
GHIRSA                                                           4        6'Trn No f
GHIRSA                                                                        COLOR(WHT)
     A                   TRNNO                  12A     O        4    21
     A    70                                                                  DSPATR(RI)
                                                                 4    43'REGISTRATION NO
                                                                                                           I
GHIRSA*
                                                                 4 43'Registration No
                                                                                                           I
GHIRSA
GHIRSA                                                                        COLOR(WHT)
     A         REGNO      4Y    OB    4   60
     A    49                                   DSPATR(RI)
     A    49                                   DSPATR(PC)
     A    83
     AO   21                                   DSPATR(PR)
     A                                         EDTCDE(4)
GHIRSA*                               5    6'DEBIT A/C NO        I



GHIRSA                                5    6'Debit A/C No        I



GHIRSA                                         COLOR(WHT)
     A         ACCCUY      3A    B    5   21
     A    50                                   DSPATR(RI)
     A    50                                   DSPATR(PC)
     A    84
     AO   21                                   DSPATR(PR)
     A         ACCNO      15A    B    5   26
     A    51                                   DSPATR(RI)
     A    51                                   DSPATR(PC)
     A    84
     AO   21                                   DSPATR(PR)
     A         BENENAME   35A    O    5   43
GHIRSA*                               6    6'DEBIT AMT t
GHIRSA                                6    6'Debit    Amt I
GHIRSA                                         COLOR(WHT)
9Q2S1A*        REMCUY      3A    O    6   21

9Q2S1A         REMCUY      3A    B    6   21
9Q2S1A    55                                   DSPATR(RI)
9Q2S1A    55                                   DSPATR(PC)
9Q2S1A    22                                   DSPATR(PR)
     A         REMAMT     15Y   2B    6   26
     A    52                                   DSPATR(RI)
     A    52                                   DSPATR(PC)
     A    84                                   DSPATR(PR)
20Q21A*                                        EDTWRD( I                    I
                                                                                )
20Q21A                                         EDTCDE(l)
GHIRSA*                               7    6'CCEX    NO'
GHIRSA                                7    6'CCEX    No'
GHIRSA                                         COLOR(WHT)
     A         CCEXNO      6A    B    7   26
     A    53                                   DSPATR(RI)
     A    53                                   DSPATR(PC)
     A    84
     AO   21                                   DSPATR(PR)
GHIRSA*                               8    6'DEBIT    TIME I
GHIRSA                                8    6'Debit    Time I
GHIRSA                                         COLOR(WHT)
     A         DRTIME      4A    B    8   21
     A    54                                   DSPATR(RI)
     A    56                                   DSPATR(PC)
     A    84
     AO   21                                   DSPATR(PR)
GHIRSA*                               9    6'DETAILS       OF   PAYMENT I
GHIRSA                                9    6'Details Of Payment'
GHIRSA                                         COLOR(WHT)
     A         DETAIL1    35A    B    9   26
     A    84                                   DSPATR(PR)
     A         DETAIL2    35A    B   10   26
     A    84                                   DSPATR(PR)
     A         DETAIL3    35A    B   11   26
     A    84                                   DSPATR(PR)
     A         DETAIL4    35A    B   12   26
     A    84                                   DSPATR(PR)
GHIRSA*   30                         14    6'MT950    FIELD I
GHIRSA    30                         14    6'MT950    Field'
GHIRSA                                         COLOR(WHT)
     A    30   MT950      16A    B   14   26
     A    31                                   DSPATR(RI)
     A    31
     AO   3 0                                             DSPATR(PC)
     A    84                                              DSPATR(PR)
     A                                                6'PC-FT       (Y/N)
                                                                             1
                                                16
GHIRSA                                                    COLOR(WHT)
     A                PCFTIND        1A     B   16   26
     A    90                                              DSPATR(RI)
     A    90                                              DSPATR(PC)
     A    84                                              DSPATR(PR)
GHIRSA*                                         17    6'PC-FT REF / TIME
                                                                                 I



GHIRSA                                          17    6'PC-FT Ref / Time
                                                                                 1



GHIRSA                                                 COLOR(WHT)
     A                PCFTREF       30A     B   17   26
     A    91                                           DSPATR(RI)
     A    91                                           DSPATR(PC)
     A    84                                           DSPATR(PR)
     A                PCFTTME        4Y    OB   17   58EDTCDE(3)
     A    92                                           DSPATR(RI)
     A    92                                           DSPATR(PC)
     A    84                                           DSPATR(PR)
GHJMIA                                          18    6'RCV    DATE r
GHJMIA                RECDATE        8Y    OB   18   15
GHJMIA    93                                              DSPATR(RI)
GHJMIA    93                                              DSPATR(PC)
GHJMIA    84                                              DSPATR(PR)
GHJMIA                                          18   26'RCV    TIME t
GHJMIA                RECTIME        4Y    OB   18   35
GHJMIA    94                                              DSPATR(RI)
GHJMIA    94                                              DSPATR(PC)
GHJMIA    84                                              DSPATR(PR)
GHIRSA*                                              49'ACC    BR
                                                                    1
                                                 7
GHIRSA                                           7   49'Acc    Br
                                                                    1



GHIRSA                                                    COLOR(WHT)
     A    71                                              DSPATR(ND)
     A                TRNBRCHID      4A     B    7   57
     A    68                                              DSPATR(RI)
     A    71                                              DSPATR(ND)
     A    68                                              DSPATR(PC)
     A    69                                              DSPATR(PR)
     A*090001
GHIRSA*                                          8   4 9'TXN   BR
                                                                    r



GHIRSA                                           8   49'Txn    Br I
GHIRSA                                                    COLOR(WHT)
     A    74                                              DSPATR(ND)
     A                TRNBRCHID2     4A     B    8   57
     A    72                                              DSPATR(RI)
     A    72                                              DSPATR(PC)
     A    73                                              DSPATR(PR)
     A    74                                              DSPATR(ND)
6Q2J1A                                          18   4 2'VALUE      DATE t
6Q2J1A                VALDATE        8Y    OB   18   53
6Q2J1A    95                                              DSPATR(RI)
6Q2J1A    95                                              DSPATR(PC)
6Q2J1A    84                                              DSPATR(PR)
     A*090001
     A            R   RSDIATSY
     A*    89/10/23    09:16:11    SAWGT              REL-R01M02         5728-PW1
     A                                                    TEXT('RSDCOS CONFIRM SCREEN RECORD')
     A                                                    CF03(03 'END OF PROGRAM')
     A                                                    OVERLAY
GHIRSA*   82                                    22   64'CONFIRM
GHIRSA    82                                    22   64'Confirm I
GHIRSA                                                    COLOR(WHT)
     A    82          CONFIRM        1A     B   22   72
     A    80                                              DSPATR(RI)
     A    81                                              DSPATR(PC)
     A    82                                  22   74'(Y/N)    t



GHIRSA                                                  COLOR(WHT)
     A            R   RSDIATSE
     A*    89/10/11    12:24 :24   MATILDA          REL-R01M02        5728-PW1
     A                                                  TEXT(RSDCOS      ERROR SCREEN RECORD')
     A                                                  OVERLAY
     A                SCRMSGID       7A   O   23    2
GHIRSA                                                  COLOR(PNK)
     A                                                  DSPATR(BL)
     A                                                  DSPATR(HI)
     A    89                                            ERRMSGID(CPF5204           *LIBL/QCPFMSG)
     A                SCRDASH        1A   O   23   11
GHIRSA                                                  COLOR(PNK)
     A                                                  DSPATR(HI)
     A                SCRMSGTEXT    BOA   0   23   14
GHIRSA                                                  COLOR(PNK)
     A                                                  DSPATR(HI)
GHIRSA*   87                                  24    2'ENTER        F3=Exit F4=Prompt F6=Cancel-
GHIRSA    87                                  24    2'F3=Exit       F4=Prompt F6=Cancel-
     A                                                    F19=Cust   Instr.    1



GHIRSA                                                  COLOR(BLU)
GHIRSA*   88                                  24    2'ENTER        F3=Exit )
GHIRSA    88                                  24    2'F3=Exit I
GHIRSA                                                  COLOR(BLU)
GHIRSA*   86                                  24    2'ENTER t
GHIRSA    86                                  24    2 '    I
