     A*%%TS     SD       20130819        104054        TMP3CA                    REL-V6R1M0            5761-WDS
          *************************************************************^
     A*                             AMENDMENT          HISTORY
          *************************************************************y
     A*DATE              BY..     AMENDMENT                                                                  TAG
     A*                                                                                                            /
     A*02/07/15          AGG      UOBM    RENTAS       SWIFT       MIGRATION         PROJECT           PHI   RNlAl
     A*                          -Changed MSGTYPE field length.
     A*                                                                                                            /
     A*04/08/13          AOC      Screen       standardization                                               MYIAC
     A*
                                 -Change label to small caps
     A*                          -Remove       Enter
     A*                                                                                                            /
     A*    90/09/21        13:43:39            ACSWTL                REL-R02M00               5728-PW1
     A*    89/12/29        15:23:47            MASHITA
     A*%%EC
     A                                                                   DSPSIZ(24           80    *DS3)
     A                                                                   PRINT
     A               R    TSSSWIFTR                                      SFL
     A*%%TS     SD       20011116        104442        UNCLC2                    REL-V4R4M0            5769-PW1
     A                                                                   TEXT('tssswiftr                subfile    rec')
     A                    TAGID                   3A     0   14     10
     A                    DESC                   50A     0   14    26
     A*

     A*    89/12/29        15:23:47            MASHITA
     A               R    TSSSWIFTC                                      SFLCTL(TSSSWIFTR)
     A*%%TS     SD       20130807        100834        TMP3CA                    REL-V6R1M0            5761-WDS
     A                                                                   SFLSIZ(0020)
     A                                                                   SFLPAG(0008)
     A                                                                   TEXT('tssswift                control-rec')
     A    51                                                             CF03(03        'end of program')
     A    52                                                             CF06(06        'END OF BODY')
     A                                                                   OVERLAY
     A    30                                                             SFLDSP
     A    31                                                             SFLDSPCTL
     A    32                                                             SFLINZ
     A    35                                                             SFLEND
     A                   GHENV                    5A     0     1     3
     A                                                         1   10'TSSSMAT
                                                                                        t



     A    36                                                   1   30'SWIFT/TELEX MESSAGE
                                                                                                             I



     A                                                                   COLOR(WHT)
     A N3 6                                                    1   30            RTGS       MESSAGE
                                                                         r                              I



     A                   GHDATS                   8A     0     1   61
     A                   GHTIMS                   8A     0     1   70
                                                                                                             1
     A    36                                                   2   30
                                                                         r



     A                                                                   COLOR(WHT)
                                                                                                        r
     A N36                                                     2   30 I
     A                                                                   COLOR(WHT)
                                                               4     7'Sending
                                                                                        1
     A
     A                                                                   COLOR(WHT)
     A                                                         4   15'bank I
     A                                                                   COLOR(WHT)
     A                                                         4   24 ' : '
     A                                                                   COLOR(WHT)
     A                    SENDBNK                11A     0     4   26
                                                                                                   I
     A                                                         5     7'Message              type
     A                                                                   COLOR(WHT)
                                                               5   24 ' :
                                                                             1
     A
     A                                                                   COLOR(WHT)
     A                   MFORMAT                  3A     0     5   26
     A                    SWFTMGTY                3S    OO     5   30
     A*RTGVKE    RTGS      CHANGES       START
RNIAIA*                  MSGTYPE                  5      O     5   39DSPATR(HI)
RNIAIA                   MSGTYPE                  6      0     5   39DSPATR(HI)
     A                                                               COLOR(GRN)
     A*RTGVKE    RTGS      CHANGES       END
A                  COVER                 3A    O     5   34
                                                     6    7'Priority
                                                                                  t
A
A                                                              COLOR(WHT)
A                                                    6   24 ' :
A                                                              COLOR(WHT)
A                  PRIORITY              IS   00     6   26
                                                     7    7'Receiving
                                                                                      1
A
A                                                              COLOR(WHT)
A                                                    7   17'bank I
A                                                              COLOR(WHT)
                                                     7   24 ' :
                                                                    t
A
A                                                              COLOR(WHT)
A                  RCBNKID           11A        0    7   26
A                  RCBNKNM           3 5A       0    8   26
A                  RCBNKA1           3 5A       0    9   26
A                  RCBNKA2           3 5A       0   10   26
A                  RCBNKA3           3 5A       0   11   26
                                                          7'Tag/Option
                                                                                          1
A                                                   12

A                                                              COLOR(WHT)
                                                    12 26'Description/Text
                                                                                                        I
A
A                                                              COLOR(WHT)
                                                                                          I
A                                                   13    7    I




A                                                              COLOR(WHT)
                                                                                                        t
A                                                   13   26 1

A                                                              COLOR(WHT)
A*
A*    89/12/29       15:23:47       MASHITA               REL-R01M02                      5728-PW1

A*    90/09/21       13:43:39       ACSWTL                REL-R02M00                      5728-PW1
A              R    TSSSHIFTA
A*%%TS    SD       20130819     104054        TMP3CA                    REL-V6R1M0                    5761-WDS
A                                                              TEXT('TSSSHIFT                         record')
A    51                                                        CF03(03            'end of program')
A    52                                                        CF06(06            'END OF BODY')
A                                                              OVERLAY
A                  GHENV                 5A     O    1     3
A                                                    1   11'TSSSMAT r
A                                                    1   28'RTGS             MESSAGE
A                                                              COLOR(WHT)
A                  GHDATE                8A     0    1   62
A                  GHTIME                8A     0    1   71
A                                                    2   28     r                                 I



A                                                              COLOR(WHT)
A                                                    3     3'Sending Bank                                   «   r



A                                                              COLOR(WHT)
A                   SENDBNK              4A     O    3   23
A                                                    4     3'Receiving Bank
                                                                                                            ●   r



A                                                              COLOR(WHT)
A                  RCBNKID           11A        0    4   23
A                                                    5     3'Trn                                            .   t


A                                                              COLOR(WHT)
A                  TRNNO             16A        O    5   23
A                                                    548'REL                Trn               .   I


A                                                              COLOR(WHT)
A                  RELTRNNO          16A        0    5   61
A                                                    6    3'Amount 1
A                                                              COLOR(WHT)
A                   PRINAMT          15Y       20    6   23EDTCDE(1               $)
A                                                    6   48'Value             Date            :
A                                                          COLOR(WHT)
A                  VALUEDTE              6Y    OO    6   61EDTCDE(Y)
A                  ACC                   9      O    7    3COLOR(WHT)
A                                                    7   20 I       .   r


A                                                              COLOR(WHT)
A                  CUSTACC1              3A     0    7   23
A                  ORDACC2           15A        0    7   27
A                  AID                   9      O    7   48COLOR(WHT)
     A                                      7   59':   I


     A                                               COLOR(WHT)
     A             CUSTAID       6A    0    7   61
     A             NA           13     O    8    3COLOR(WHT)
     A                                      8   20 1   .   1


     A                                               COLOR(WHT)
     A             CUSTFNAM     3 5A   0    9    5
     A             ADDR1        3 5A   O    9   44
     A             ADDR2        3 5A   0   10    5
     A             ADDR3        3 5A   0   10   44
     A                                     11    3'By Order of No
                                                                                          ,   t


     A                                               COLOR(WHT)
     A             ORDACC1       3A    0   11   23
     A             CUSTACC2     15A    0   11   27
     A                                     11   48'Send         Aid    .   I


     A                                               COLOR(WHT)
     A             ORDAID        6A    O   11   61
     A                                     12    3'By Order of N/A
                                                                                          ,   I


     A                                               COLOR(WHT)
     A             ORDBNK       3 5A   O   13    5
     A             ORDADDR1     3 5A   O   13   44
     A             ORDADDR2     3 5A   0   14    5
     A             ORDADDR3     3 5A   0   14   44
     A                                     15    3'Payment Details                        .   t


     A                                               COLOR(WHT)
     A             DTLPAY1      3 5A   0   16    5
     A             DTLPAY2      3 5A   0   16   44
     A             DTLPAY3      3 5A   0   17    5
     A             DTLPAY4      3 5A   O   17   44
     A                                     18    3'Bank        to   Bank       Info       :   r



     A                                               COLOR(WHT)
     A             BNKINFOR1    3 5A   0   19    5
     A             BNKINFOR2    3 5A   0   19   44
     A             BNKINFOR3    3 5A   0   20    5
     A             BNKINFOR4    3 5A   0   20   44
     A             GHLCY         3     O    6   IICOLOR(WHT)
                                            6   21':
                                                       1
     A
     A                                               COLOR(WHT)
     A         R   TSSSHIFTE
     A                                               TEXT('TSSSHIFT ERROR RECORD')
     A                                               OVERLAY
     A             SCRMSGID      7A    0   23    3
     A                                               DSPATR(BL)
     A                                               DSPATR(HI)
     A                                               COLOR(PNK)
     A             SCRDASH       1A    O   23   12
     A                                               DSPATR(HI)
     A                                               COLOR(PNK)
     A             SCRMSGTEXT   BOA    0   23   15
     A                                               DSPATR(HI)
     A                                               COLOR(PNK)
MYIACA*   81                               24    3'ENTER'
                                           24    3'ENTER   F3=Exit F6=Cancel
                                                                                                  I

MYIACA*   82
                                           24    3'F3=Exit F6=Cancel
                                                                                      t

MYIACA    82
     A                                            COLOR(BLU)
