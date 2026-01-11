*=====================================================================
* HISTORY OF AMENDMENT :
*=====================================================================
* IDL2A1 - ACNNPL - 16/09/2016 - INTRA-DAY LIMIT PHASE 2
*                                INCLUDE FIELDS FOR REVERSAL
*                                FUNCTIONS
*
*=====================================================================
* CMP3E5 - VENAF2 - 14/03/2016 - CASH MANAGEMENT PROJECT 3
*                                AUTO COVER MATCHING
*                                INITIAL VERSION
*=====================================================================
                                REF(DDSDB/REMREF)
*
*
R UFIMIJCONR
     QUENUM       R
     QUESUF       R
     TRNNO        R
     STATUS                    1A         TEXT('STATUS')
     SWFTMGTY     R
     SENBNKID                 11A         TEXT('SENDING BANK')
     RELTRNREF                16A         TEXT('RELATED TRANS REF')
     VALUEDTE                  6S         TEXT('VALUE DATE')
     CUYCD        R
     AMOUNT                   15S 2       TEXT('AMOUNT')
     BENEBNK                  11A         TEXT('BENE BANK')
     INTDTE       R
     TOLERA                    1A         TEXT('TOLERANCE')
     UTLIDL                    1A         TEXT('IDL UTILIZATION')
     ACCNO        R
     ACCCUY       R
     BNKENTTY     R
     MSGBDY                10000A
IDL2A1     REVTRNNO                 12A         TEXT('ITT REVERSAL TXN NO')
IDL2A1     RESETCOV                  1A         TEXT('RESET COVER INDICATOR')
IDL2A1     PASTDUE                  3S 0        TEXT('DAYS PAST DUE')
IDL2A1     LSTUPDTE                  6S         TEXT('LAST UPDATE PAST DUE')
IDL2A1     SEGCDE                    1A         TEXT('SEGMENT CODE')
IDL2A1     LCCUY                     3A         TEXT('LOCAL CURRENCY CODE')
IDL2A1     LCAMT                   15S 2        TEXT('LOCAL CURRENCY AMOUNT')
IDL2A1     FUNCTID                  12A         TEXT('FUNCTION ID')
IDL2A1     OPRID                    10A         TEXT('OPERATOR ID')
IDL2A1     APROVID                   8A         TEXT('APPROVAL ID')
IDL2A1     CREATEDTE                6S 0        TEXT('CREATE DATE')
IDL2A1     APROVDTE                 6S 0        TEXT('APPROVED DATE')
IDL2A1     BENEACCNO                11A         TEXT('BENE ACC NO')
IDL2A1     BENEACCNAM               35A         TEXT('BENE ACC NAME')
IDL2A1     FILLER1                 250A         TEXT('FILLER 1')
IDL2A1     FILLER2                 250A         TEXT('FILLER 2')
IDL2A1     FILLER3                 250A         TEXT('FILLER 3')
IDL2A1     FILLER4                 250A         TEXT('FILLER 4')
IDL2A1     FILLER5                  18S         TEXT('FILLER 5')
IDL2A1     FILLER6                  18S         TEXT('FILLER 6')
IDL2A1     FILLER7                  18S         TEXT('FILLER 7')
IDL2A1     FILLER8                  18S         TEXT('FILLER 8')
     K QUENUM
