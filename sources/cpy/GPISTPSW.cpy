      * GPISTPSW.cpybk
      *=================================================================
      *
      *Copybook Name    : GPISTPSW                                     *
      *Copybook Description : REM GPI - STP Enhancement Switch CPYBK   *
      *   This copy book represents Switch Variables for               *
      *   GPISTPSW System Parm List of GPI STP Proc Enh                *
      *Date Created     : 09 October 2019                              *
      *Created by       : Accenture                                    *
      *=================================================================
      *
      * HISTORY OF MODIFICATION:                                       *
      *=================================================================
      *
      * MOD.#   INIT   DATE       DESCRIPTION                          *
      * ------  ------ ---------- -------------------------------------*
      * G2BE00 - VENDUS - 09/10/19 - CASH MANAGEMENT ROAD MAP - P19    *
      *                               GPI Day4 (Retro from GPI Day2b HO)
      *
      *                               - Initial Version.               *
      *=================================================================
      *
       01  WK-GPI-STP-SW.
      ***STP Enhancement Item 4: Routing Agent
      ***Switch to route serial/cover payment to Nostro agent to allow
      -    STP of
      ***In as out transaction
           05  SW-ROUTE-AGENT              PIC X.
              88  SW-ROUTE-AGENT-Y        VALUE "Y".
              88  SW-ROUTE-AGENT-N        VALUE "N".
      ***STP Enhancement Item 5.1: Inward Serial Payment
      ***This is to skip STP limit validations for Incoming MT103/MT101
      -    serial
      ***payments for Commercial and UOBPay ITT txn
           05  SW-STP-LMT-SKP              PIC X.
              88  SW-STP-LMT-SKP-Y        VALUE "Y".
              88  SW-STP-LMT-SKP-N        VALUE "N".
      ***STP Enhancement Item 5.2: Tag 57 Enhancement
      ***Check Tag57(A/B/C/D/E) Line1 and Line2 againts GPI Tag Val
      -    Table
           05  SW-TAG57-VAL                PIC X.
              88  SW-TAG57-VAL-Y          VALUE "Y".
              88  SW-TAG57-VAL-N          VALUE "N".
      ***STP Enhancement Item 5.3: Special Cust contact QR limit
      ***To allow QR limit checking for Special Cust contact QR
           05  SW-SPEC-CUST-LMT            PIC X.
              88  SW-SPEC-CUST-LMT-Y      VALUE "Y".
              88  SW-SPEC-CUST-LMT-N      VALUE "N".
      ***STP Enhancement Item 10: IN as OUT Nested Txn
      ***Route the payment to NOSTRO account for Nested trans to allow
      -    STP of
      ***In as Out
           05  SW-IN-OUT-NESTED            PIC X.
              88  SW-IN-OUT-NESTED-Y      VALUE "Y".
              88  SW-IN-OUT-NESTED-N      VALUE "N".
      ***STP Enhancement Item 12: No Paying Bank Found
      ***Check against Nostro agent to STP the transaction
           05  SW-NO-PAYBNK-SW             PIC X.
              88  SW-NO-PAYBNK-Y          VALUE "Y".
              88  SW-NO-PAYBNK-N          VALUE "N".
      ***STP Enhancement Item 13: STP handling for IAFT
      ***To bypass/remove redundant STP validations.

           05  SW-IAFT-BYPASS-VAL              PIC X.
              88  SW-IAFT-BYPASS-VAL-Y            VALUE "Y".
              88  SW-IAFT-BYPASS-VAL-N            VALUE "N".
      * **To add Upfront Auto reject scenarios for IAFT.
           05  SW-IAFT-AUTO-REJ                PIC X.
              88  SW-IAFT-AUTO-REJ-Y              VALUE "Y".
              88  SW-IAFT-AUTO-REJ-N              VALUE "N".
      * **STP Enhancement Item 17: SWIFT BIC Codes
      * **To convert Lower Cased SWIFT BIC to Upper Case.
           05  SW-UPPER-CASE-BIC               PIC X.
              88  SW-UPPER-CASE-BIC-Y             VALUE "Y".
              88  SW-UPPER-CASE-BIC-N             VALUE "N".
      * **STP Enhancement Item 18: Standing instruction enhancement
      * **To add new field to determine if Standing Instruction will be
      -    applied
      * **for Inward, Outward, or Both flow.
           05  SW-IO-STAND-INSTR               PIC X.
              88  SW-IO-STAND-INSTR-Y             VALUE "Y".
              88  SW-IO-STAND-INSTR-N             VALUE "N".
      * **STP Enhancement Item 20: Auto-retry for NAB items
      * **To submit new Background job that will Auto Retry NAB txn w/
      -    Parm intervl
           05  SW-AUTO-RETRY-NAB               PIC X.
              88  SW-AUTO-RETRY-NAB-Y             VALUE "Y".
              88  SW-AUTO-RETRY-NAB-N             VALUE "N".
      * **STP Enhancement Item 4: In as Out FI Prefered NOSTRO
      * **To submit new Background job that will Auto Retry NAB txn w/
      -    Parm intervl
           05  SW-IN-OUT-FI-PREF-NOS           PIC X.
              88  SW-IN-OUT-FI-PREF-NOS-Y         VALUE "Y".
              88  SW-IN-OUT-FI-PREF-NOS-N         VALUE "N".
      * **Filler1 for future use enhancements
           05  SW-IN-OUT-FILLER1               PIC X.
              88  SW-IN-OUT-FILLER1-Y             VALUE "Y".
              88  SW-IN-OUT-FILLER1-N             VALUE "N".
      * **Filler2 for future use enhancements
           05  SW-IN-OUT-FILLER2               PIC X.
              88  SW-IN-OUT-FILLER2-Y             VALUE "Y".
              88  SW-IN-OUT-FILLER2-N             VALUE "N".
      * **Filler3 for future use enhancements
           05  SW-IN-OUT-FILLER3               PIC X.
              88  SW-IN-OUT-FILLER3-Y             VALUE "Y".
              88  SW-IN-OUT-FILLER3-N             VALUE "N".
      * **Filler4 for future use enhancements
           05  SW-IN-OUT-FILLER4               PIC X.
              88  SW-IN-OUT-FILLER4-Y             VALUE "Y".
              88  SW-IN-OUT-FILLER4-N             VALUE "N".
      * **Filler5 for future use enhancements
           05  SW-IN-OUT-FILLERS               PIC X.
              88  SW-IN-OUT-FILLERS-Y             VALUE "Y".
              88  SW-IN-OUT-FILLERS-N             VALUE "N".
      * **Filler6 for future use enhancements
           05  SW-IN-OUT-FILLER6               PIC X.
              88  SW-IN-OUT-FILLER6-Y             VALUE "Y".
              88  SW-IN-OUT-FILLER6-N             VALUE "N".
      * **Filler7 for future use enhancements
           05  SW-IN-OUT-FILLER7               PIC X.

              88  SW-IN-OUT-FILLER7-Y       VALUE "Y".
              88  SW-IN-OUT-FILLER7-N       VALUE "N".
      *Filler8 for future use enhancements
           05  SW-IN-OUT-FILLER8            PIC X.
              88  SW-IN-OUT-FILLER8-Y       VALUE "Y".
              88  SW-IN-OUT-FILLER8-N       VALUE "N".
