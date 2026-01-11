     A*%%TS    SD       20130806          111746        TMP3CA               REL-V6R1M0     5761-WDS
     A**************************************************************y
     A*                               AMENDMENT         HISTORY
      *************************************************************^
     A*DATE             BY. .       AMENDMENT                                                       TAG
     A*21/04/16         LCN       e-Req 45846 REM Q3 2016 Rls                                      6Q3L1
     A*                         - Added F2 Key for incoming message in
     A*                             CSI   screen
     A*                                                                                                    /
     A*04/08/13         AOC         Screen   standardization                                       MY2AC
     A*                         -   Change to small caps
     A*                         -   Remove   Enter
     A*                                                                                                    /
     A*%%EC
     A                                                                 DSPSIZ(24    80    *DS3)
     A                                                                 PRINT
     A              R    CSISCRDTL                                     SFL
     A*%%TS    SD       20130806          111746        TMP3CA               REL-V6R1M0     5761-WDS
     A                                                                 TEXT('Subfile of Country Specific I-
     A                                                                 nf o' )
     A    25                                                           SFLNXTCHG
     A                   CSICOD                 10        O   6    3COLOR(WHT)
     A                   CSIDES                 30        O   6   15COLOR(WHT)
     A                   CSILEN                    2      O   6   47COLOR(WHT)
     A                   CSIMAN                    1      0   6   53COLOR(WHT)
     A                   CSIVAL                    1      O   6   58
     A    41                                                           DSPATR(RI)
     A                                                                 COLOR(WHT)
     A                   CSITBL                 10        0   6   62
     A    41                                                        DSPATR(RI)
     A                                                              COLOR(WHT)
     A                   CSIFXN                    1      0   6   78COLOR(WHT)
     A                   CSIINP                 60A       B   7   15
     A    42                                                        DSPATR(RI)
     A    44                                                        DSPATR(PC)
     A    45                                                        DSPATR(PR)
     A                   CSIDSC                 60A       0   8   15DSPATR(HI)
     A                                                              COLOR(GRN)
     A                   CSIDS2                 60A       O   9   15DSPATR(HI)
     A                                                              COLOR(GRN)
     A                   CSISCD                    1A     O   7   12DSPATR(ND)
     A                   CSISEQ                    3A     O   7    3DSPATR(ND)
     A                   CSICOM                    3      0   6   73
     A    47                                                           DSPATR(RI)
     A                                                                 COLOR(WHT)
     A              R    CSISCRCTL                                     SFLCTL(CSISCRDTL)
     A*%%TS    SD       20130805          162024        TMP3CA               REL-V6R1M0     5761-WDS
     A                                                                 SFLSIZ(1000)
     A                                                                 SFLPAG(0004)
     A                                                                 TEXT('CSISCRCTL Control             Screen of      C-
     A                                                                 SISCRDTL’)
     A    14                                                           CF04 (04   ’DSP-Loo)cup' )
     A    16                                                           CF06(06    'SET-Default')
     A    22                                                           CF12(12    'DSP-Cancel')
6Q3L1A    49                                                           CF02(02    'Incoming Message')
     A                                                                 RTNCSRLOC(&CURREC          iCURFLD      &CURPOS)
     A                                                                 CSRLOC(CROW                CCOL)
     A                                                                 OVERLAY
     A                                                                 SFLCSRRRN(&CURRRN)
     A    30                                                           SFLDSP
     A    31                                                           SFLDSPCTL
     A    34                                                           SFLCLR
     A    35                                                           SFLEND
     A                   CURRRN                    5S    OH
     A                   CURREC                 lOA       H
     A                   CURFLD                 lOA       H
     A                    CURPOS                   4S   OH
     A                    CROW                     3Y   OH
     A                    CCOL                     3Y   OH

     A                    CSIRECCN                 4S   OH              SFLRCDNBR(CURSOR)
     A                    CSILOCNM                 5A    0    1     3

     A                    CSIPGMID                 8A    O    1    10
                                                              1    31'COUNTRY SPECIFIC                       INFO
                                                                                                                    I

     A
     A                                                               COLOR(WHT)
     A                    CSISDATE                 8A    O    1    60

     A                    CSISTIME                 8A    0     1   71
                                                                    3'CODE
                                                                                     r
ID1J2A*                                                       3

     A                                                        3     3'Code r
     A                                                               DSPATR(HI)
ID1J2A*                                                        3   15'CODE DESCRIPTION
                                                               3   15'Code Description
                                                                                                         1

     A
     A                                                                  DSPATR(HI)
ID1J2A*                                                       4 15'INPUT /                I



     A                                                         4   15’Input /
     A                                                                  DSPATR(HI)
                                                               5   15'INPUT              DESCRIPTION
                                                                                                             I
ID1J2A*
                                                               5 15'Input Description
                                                                                                             I
     A
     A                                                                  DSPATR(HI)
                                                               3   47'LEN
                                                                             I
     A
     A                                                                  DSPATR(HI)
                                                               3   52'M/O
                                                                             I
     A
     A                                                                  DSPATR(HI)
     A                                                         3   57'V/N    (




     A                                                                  DSPATR(HI)
                                                                                               I
ID1J2A*                                                        3   62'VALID              TBL
     A                                                         3   62'Valid              TBL   1




     A                                                                  DSPATR(HI)
     A                                                         3   78'FN I

     A                                                                  DSPATR(HI)
     A                    CSITRNTP                 8A    0     1   20
                                                               3   73'CMN
                                                                                 I
     A
     A                                                                  DSPATR(HI)
                                                               4   73'VLD
                                                                                 I
     A
     A                                                                  DSPATR(HI)
     A*Function Keys          (ENTER to proceed)
     A*                       (F3    to   exit)
     A*                       (F12 to go to previous screen)
     A*Function Keys          (ENTER to proceed)
     A*                       (F3    to   exit)
     A*                       (F12 to go to previous screen)
     A*Error Message          ID
     A                R    CSISCRFTR
     A*%%TS      SD       20130805        162024        TMP3CA             REL-V6R1M0                  5761-WDS

     A                                                                  INZRCD

     A*Error Message          ID
     A                     SCRMSGID                7      O   23    3
     A    N4 0                                                          DSPATR(ND)
     A                                                                  DSPATR(BL)
MYIACA                                                                  COLOR(PNK)
MYIACA*                                                                 COLOR(RED)
     A*Character Dash
     A                     SCRDASH                 1      O   23   12
     A N4 0                                                             DSPATR(ND)
     A                                                                  COLOR(PNK)
MYIACA*                                                                 COLOR(WHT)
     A*Error Message          Text
     A                     SCRMSGTEXT          50         0   23   15
     A N4 0                                                             DSPATR(ND)
     A                                                                  COLOR(PNK)
MYIACA*                                                       22    3'ENTER I

6Q3L1A                                                        22    3'F2=In Msg
                                                                                                   1



6Q3L1A N48                                                           DSPATR(ND)
6Q3L1A               COLOR(BLU)
MYIACA*      22   10'F4=Prompt    F6=Default
MYIATA       22   14'F4=Prompt    F6=Default
     A N46          DSPATR(ND)
     A              COLOR(BLU)
