*MPIMBA*      05  TFSCLSYS-RECORD                PIC X(91).
MPIMBA       05  TFSCLSYS-RECORD                PIC X(97).
             05  TFSCLSYS-RECORD                PIC X(100).
*            I-O FORMAT:TFSCLSYSR
*                 FROM FILE TFSCLSYS OF LIBRARY COMDB
*                 system control table record
             05  TFSCLSYSR    REDEFINES TFSCLSYS-RECORD.
                 06  TFSCLSYS-TSYSIND          PIC X(2).
*                    system indicator - for trade
                 06  TFSCLSYS-RSYSIND          PIC X(2).
*                    system indicator - for remittance
                 06  TFSCLSYS-SYSDTE           PIC S9(8).
*                    system date
                 06  TFSCLSYS-NXTPROCDT        PIC S9(8).
*                    next processing date
                 06  TFSCLSYS-LSTPROCDT        PIC S9(8).
*                    last processing date
                 06  TFSCLSYS-LCNTRYCD         PIC X(2).
*                    local country code
                 06  TFSCLSYS-LCUYCD           PIC X(3).
*                    local currency code
                 06  TFSCLSYS-USCUYCD          PIC X(3).
*                    USD currency code
                 06  TFSCLSYS-CAIND            PIC X(1).
*                    C/A interface available ind
                 06  TFSCLSYS-FCCAIND          PIC X(1).
*                    foreign C/A interface indicator
                 06  TFSCLSYS-SAIND            PIC X(1).
*                    S/A interface available indicate
                 06  TFSCLSYS-FXIND            PIC X(1).
*                    FOREX interface available in
                 06  TFSCLSYS-CLSIND           PIC X(1).
*                    Out Brch TT interface avail ind
                 06  TFSCLSYS-MERVIND          PIC X(1).
*                    MERVA interface avail ind -OUT
                 06  TFSCLSYS-MERVTIND         PIC X(1).
*                    MERVA interface avail ind -IN(tr)
                 06  TFSCLSYS-MERVRIND         PIC X(1).
*                    MERVA interface avail ind -in(RM)
                 06  TFSCLSYS-MSGNOTRD         PIC S9(05).
*                    Outgoing message no for trade
                 06  TFSCLSYS-MSGNOREM         PIC S9(05).
*                    Outgoing message no for remit
                 06  TFSCLSYS-CAEMNO           PIC S9(06).
*                    c/a earmark no
                 06  TFSCLSYS-FCCAEMNO         PIC S9(06).
*                    fcca earmark no
                 06  TFSCLSYS-FORMLNO          PIC S9(4).
*                    form file last running no
*MPIMBA - PARALNO EXPANDED FROM 5 TO 8 BYTES
                 06  TFSCLSYS-PARALNO          PIC S9(8).
*                    parameter file last running no
                 06  TFSCLSYS-FXDEAL           PIC S9(6).
*                    tremis batch no
                 06  TFSCLSYS-SETUPDTE         PIC S9(8).
*                    set up date
                 06  TFSCLSYS-LSTUPDTE         PIC S9(8).
*                    last update date
