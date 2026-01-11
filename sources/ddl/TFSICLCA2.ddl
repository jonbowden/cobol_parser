*=====================================================================
* AMENDMENT LOG:
*=====================================================================
* SEARCH KEY      DATE            DESCRIPTION
*---------------------------------------------------------------------
* VASA03 - VENUFQ - 03/07/2025    PRO3#3W02 UOVB SG VASA
*                                  - Update reg addr field length
*                                    Update position of filler field for
*                                    master account
*
* VASA02 - VENUFQ - 12/06/2025    PRO3#3W02 - UOVB VASA
*                                  - Additional fields and re-mapping of
*                                    new VASA fields
*
* VASA01 - VENUFQ - 23/05/2025    - Initial Version.
*                                    This file is a duplicate of TFSICLCA
*                                    with additional fields for VASA
*
*=====================================================================
R TFSICLCA2R
*********************************************************************
***     05 WK-ISLCAQ-OUTPUT.
***         10 WK-O-ISLCAQ-MQ-HEADER.
     APPLISYS                  5A
     MQSERNO                  10S 0
     RTNDATAQNM              10A
     MQRTNCODE                4A
     FILLER001                3A
***         10 WK-O-ISLCAQ-MSG-HEADER.
     TRANCODE                 4S 0
     TELLERID                 4A
     TLRTRNSSQN              4S 0
     REVIND                   1A
     PTIME                   4S 0
     ACCCUY                   3A
     ACCTYPE                 2S 0
     ACCNO                  18S 0
     TRNNO                   12A
     TERMID                   4A
     SUPOVRID                 8A
     FILLER002               36A
     REQFNCODE               2S 0
****         10 WK-O-ISLCAQ-OUTPUT-DETAILS.
     MSGRETCODE               1A
     ERRORCODE               4S 0
     ERRORTEXT               30A
     STATUS                  2S 0
     HOLDA                   2S 0
     HOLDB                   2S 0
     HOLDC                   2S 0
     SHORTNAME               20A
     NAMEADDRl               40A
     NAMEADDR2               40A
     NAMEADDR3               40A
     NAMEADDR4               40A
     NAMEADDR5               40A
     NAMEADDR6               40A
     NAMEADDR7               40A
     POSTALID                10A
     OLNOIDS                 1S 0
     IDACDE                   1A
     IDANUM                  30A
     IDBCDE                   1A
     IDBNUM                  30A
     STAFFCODE               2S 0
     RESCODE                 2S 0
     INFORACE                2S 0
     BIRTHDATE               8S 0
     INFOSEX                  1A
     INFOCTRY                4S 0
     CUSTTYPE                3S 0
     ILLIMITl               15S 2
     ILLIMIT2               15S 2
     ILLIMITRES             15S 2
     OLAVLBALSG               1A
     OLAVLBAL               15S 2
     OLCURBALSG               1A
     OLCURBAL               15S 2
     CCYCODE                  3A
     PRODTYPE                 2A
     CIFNO                   19A
     OFFICERCD               10A
     BANKCODE                2S 0
     BRANCHCODE              3S 0
     ISSCTYCDl                3A
     ISSCTYCD2                3A
     SEGCODE                  1A
     CTOCODE                 10A
VASA02*     REGADDRl                35A
VASA02*     REGADDR2                35A
VASA02*     REGADDR3                35A
VASA02*     REGADDR4                35A
VASA03*VASA02     REGADDRl                40A
VASA03*VASA02     REGADDR2                40A
VASA03*VASA02     REGADDR3                40A
VASA03*VASA02     REGADDR4                40A
VASA03     REGADDRl                35A
VASA03     REGADDR2                35A
VASA03     REGADDR3                35A
VASA03     REGADDR4                35A
     REGBLK                   7A
     REGSTOREY                4A
     REGUNIT                  7A
     REGPOBOX                 6A
     REGBUILD                45A
     REGSTREET               32A
     REGSTATE                20A
     REGPOSTAL                9S
     REGCTYCODE               3A
     REGADDRTYP               1A
     REGADDRFMT               1A
     REGFOREIGN               1A
     REGVERIFY                1A
     HOLDMAIL                 1A
     ACCTADRFMT               1A
     PRIIDNUM                30A
     PRIIDTYPE                2A
     PRIOWNCTY                3A
     PRIDOB                   8S
     PRICTZENCT               3A
     ENGACNAMEl              40A
     ENGACNAME2              40A
     ACNAMECNT                1A
     ACCTNRA                  3A
     HOLDTXTA                30A
     HOLDTXTB                30A
     HOLDTXTC                30A
     VERIADDI                 1A
*** VASA fields
VASA03     FILLER003                1A
     SMACT                  18S 0
VASA03*VASA02     FILLER003                1A
     SMPURP                   3A
VASA02     SMHCDl                  2S 0
VASA02     SMHCD2                  2S 0
VASA02     SMHCD3                  2S 0
VASA02     SMHCD4                  2S 0
VASA02     SMHCD5                  2S 0
VASA02     SMHCD6                  2S 0
VASA02     SMHCD7                  2S 0
VASA02     SMHCD8                  2S 0
VASA02     SMHCD9                  2S 0
VASA02     SMHCNl                  30A
VASA02     SMHCN2                  30A
VASA02     SMHCN3                  30A
VASA02     SMHCN4                  30A
VASA02     SMHCN5                  30A
VASA02     SMHCN6                  30A
VASA02     SMHCN7                  30A
VASA02     SMHCN8                  30A
VASA02     SMHCN9                  30A
VASA02     ACCTMl                  35A
VASA02     ACCTM2                  35A
VASA02     ACCTLN                 140A
VASA02     CUSTNl                  35A
VASA02     CUSTN2                  35A
VASA02     CUSTLN                 140A
VASA02     MACTNl                  35A
VASA02     MACTN2                  35A
VASA02     MACTLN                 140A
VASA02     ADDIMA                  40A
VASA02     ADD2MA                  40A
VASA02     ADD3MA                  40A
VASA02     ADD4MA                  40A
VASA02     ABNMMA                   7A
VASA02     ASTYMA                   4A
VASA02     AUNMMA                   7A
VASA02     ABOXMA                   6A
VASA02     ABDNMA                  45A
VASA02     ASTNMA                  32A
VASA02     CITYMA                  20A
VASA02     ZIPMA                    9S
VASA02     COUNMA                   3A
VASA02     ADTPMA                   1A
VASA02     ADFMMA                   1A
VASA02     FORNMA                   1A
VASA02     BADAMA                   1A
VASA02     HLDMMA                   1A
VASA02     ADFHMA                   1A
VASA02     SSPI                    10A
VASA02     LCKI                    10A
VASA03     STFFLG                   2S
VASA02*     SMRADl                  35A
VASA02*     SMRAD2                  35A
VASA02*     SMRAD3                  35A
VASA02*     SMRAD4                  35A
VASA02*     SMRBLK                   7A
VASA02*     SMRSTO                   4A
VASA02*     SMRUNT                   7A
VASA02*     SMRPOB                   6A
VASA02*     SMRBLD                  45A
VASA02*     SMRSTR                  32A
VASA02*     SMRSTA                  20A
VASA02*     SMRZIP                  9S 0
VASA02*     SMRCOU                   3A
VASA02*     SMRATY                   1A
VASA02*     SMRADF                   1A
VASA02*     SMRFOR                   1A
VASA02*     BADACA                   1A
VASA02*     HLDMCA                   1A
VASA02*     ADFHCA                   1A
VASA02*     SMADRl                  40A
VASA02*     SMADR2                  40A
VASA02*     SMADR3                  40A
VASA02*     SMADR4                  40A
VASA02*     SMADR5                  40A
VASA02*     SMADR6                  40A
VASA02*     SMADR7                  40A
VASA02*     SMAZIP                  10A
VASA02*     SMHCDl                  2S 0
VASA02*     SMHCD2                  2S 0
VASA02*     SMHCD3                  2S 0
VASA02*     SMHCD4                  2S 0
VASA02*     SMHCD5                  2S 0
VASA02*     SMHCD6                  2S 0
VASA02*     SMHCD7                  2S 0
VASA02*     SMHCD8                  2S 0
VASA02*     SMHCD9                  2S 0
VASA02*     SMHCNl                  30A
VASA02*     SMHCN2                  30A
VASA02*     SMHCN3                  30A
VASA02*     SMHCN4                  30A
VASA02*     SMHCN5                  30A
VASA02*     SMHCN6                  30A
VASA02*     SMHCN7                  30A
VASA02*     SMHCN8                  30A
VASA02*     SMHCN9                  30A
VASA02*     FILSUB                2013A
VASA02     FILSUB                 628A
     K ACCCUY
     K ACCTYPE
     K ACCNO
