      ***********************************************************
      * Program name:    SUPPCALL
      * Original author: David Stagowski
      *
      *    Description: Validate the Suppliers from the
      *       Part-Suppliers dataset.
      *    If the record is valid, return to caller with 
      *    RETURN-CODE = 0.
      *
      *    If the record has errors:
      *    1) Return to caller with RETURN-CODE = 8,
      *    2) Update the error-counter with number of errors found.
      *    3) Update Errors-Status flag = Found. 
      *
      * Maintenance Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-26 dastagg       Created for ECBAP Final Project
      * 20XX-XX-XX               If you change me, change this.
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPCALL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-Status-Flags.
           12 WS-Supplier-Valid-Flag         PIC X.
              88 WS-Supplier-Valid           VALUE 'Y'.
              88 WS-Supplier-Invalid         VALUE 'N'.

       01  WS-Hold-Storage.
           12 WS-Hold-Error-Message      PIC X(30) VALUE SPACES.

       01  WS-Program-Hold-Fields.
           12 WS-SUPPLIER-ACT-DATE         PIC 9(08).
           12 WS-Date-Int-Returned         PIC 9(10).
      * Hold fields for CEEDAYS called module.
           12 W-INPUT-DATE-INT        PIC 9(9) COMP.
           12 W-PICSTR-IN.
              15  W-PICSTR-LTH-IN     PIC S9(4) COMP VALUE 8.
              15  W-PICSTR-STR-IN     PIC X(8)  value 'YYYYMMDD'.
           12 W-DATE-IN-CEE.
              15  W-DATE-IN-LTH-CEE   PIC S9(4) COMP VALUE 8.
              15  W-DATE-IN-STR-CEE   PIC X(8).
           12 FC.
              15  FC-SEV              PIC S9(4) COMP.
              15  FC-MSG              PIC S9(4) COMP.
              15  FC-CTW              PIC X.
              15  FC-FAC              PIC X(3).
              15  FC-ISI              PIC S9(8) COMP.

       LINKAGE SECTION.
       COPY PARTSUPP.
       01  WS-Call-Tracking.
           12 WS-Error-Message-Setup.
              15 WS-Error-Message-Max-Cnt     PIC S9(4) COMP VALUE +3.
              15 WS-Error-Message-Occurs-Cnt  PIC S9(4) COMP VALUE ZERO.
              15 WS-EM-Table.
                 18 WS-Error-Messages OCCURS 0 TO 3 TIMES
                 DEPENDING ON WS-Error-Message-Occurs-Cnt
                 INDEXED BY WS-EM-IDX.
                    21 WS-EM-Message            PIC X(30) VALUE SPACES.

       PROCEDURE DIVISION USING PART-SUPP-ADDR-PO, WS-Call-Tracking.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Validate-Supplier.
           PERFORM 3000-End-Job.

           IF WS-Supplier-Valid
              MOVE 0 to RETURN-CODE
           ELSE
              MOVE 8 to RETURN-CODE
           END-IF.

           GOBACK.

       1000-Begin-Job.
      D    DISPLAY "SUPPCALL: 1000-Begin-Job"     
           SET WS-Supplier-Valid TO TRUE.

       2000-Validate-Supplier.
      D    DISPLAY "SUPPCALL: 2000-Validate-Supplier"     
           PERFORM 2100-Validate-Suppliers.

       2100-Validate-Suppliers.
           PERFORM 2110-Validate-Required-Fields.
           PERFORM 2120-Validate-Correct-Types.
           PERFORM 2130-Validate-Supplier-Date.

       2110-Validate-Required-Fields.
      *    Required fields: SUPPLIER-CODE, SUPPLIER-TYPE, SUPPLIER-NAME,
      *       SUPPLIER-PERF
           IF SUPPLIER-CODE(1:5) NOT > SPACE
              SET WS-Supplier-Invalid TO TRUE
              MOVE 'Supplier Code Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
           IF SUPPLIER-NAME NOT > SPACE
              SET WS-Supplier-Invalid TO TRUE
              MOVE 'Supplier Name Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
           IF SUPPLIER-PERF IS NOT NUMERIC OR SUPPLIER-PERF = ZEROES
              SET WS-Supplier-Invalid TO TRUE
              MOVE 'Supplier PERF Incorrect' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.

       2120-Validate-Correct-Types.
      *    SUPPLIER-TYPE, SUPPLIER-RATING, SUPPLIER-STATUS must be one 
      *       of the listed 88-level fields
           EVALUATE TRUE
              WHEN SUBCONTRACTOR 
              WHEN DISTRIBUTOR
              WHEN MANUFACTURER
              WHEN IMPORTER
                 CONTINUE
              WHEN OTHER
                SET WS-Supplier-Invalid TO TRUE
              MOVE 'Supplier Type Incorrect' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-EVALUATE.

           EVALUATE TRUE
              WHEN HIGHEST-QUALITY 
              WHEN AVERAGE-QUALITY
              WHEN LOWEST-QUALITY
                 CONTINUE
              WHEN OTHER
                SET WS-Supplier-Invalid TO TRUE
              MOVE 'Supplier Rating Incorrect' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-EVALUATE.

           EVALUATE TRUE
              WHEN GOVT-COMM 
              WHEN GOVT-ONLY
              WHEN COMMERCIAL-ONLY
                 CONTINUE
              WHEN OTHER
                SET WS-Supplier-Invalid TO TRUE
              MOVE 'Supplier Status Incorrect' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-EVALUATE.

       2130-Validate-Supplier-Date.
      *     SUPPLIER-ACT-DATE must be a valid date
      *     MOVE SUPPLIER-ACT-DATE TO WS-SUPPLIER-ACT-DATE
      *     COMPUTE WS-Date-Int-Returned =
      *        FUNCTION INTEGER-OF-DATE(WS-SUPPLIER-ACT-DATE).
      *     IF WS-Date-Int-Returned = 0
      *        SET WS-Supplier-Invalid TO TRUE
      *        MOVE 'Supplier ACT Date Incorrect' TO 
      *           WS-Hold-Error-Message
      *        PERFORM 2199-Add-Error-Message
      *     END-IF.

      * On the mainframe, use this instead
           MOVE SUPPLIER-ACT-DATE TO W-DATE-IN-STR-CEE
           CALL 'CEEDAYS' USING W-DATE-IN-CEE
               W-PICSTR-IN, W-INPUT-DATE-INT, FC
           IF FC-SEV = ZERO
              NEXT SENTENCE
           ELSE
              SET WS-Supplier-Invalid TO TRUE
              MOVE 'Supplier ACT Date Incorrect' TO 
                 WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.

       2199-Add-Error-Message.
           EVALUATE WS-Error-Message-Occurs-Cnt
              WHEN 0
                 MOVE +1 TO WS-Error-Message-Occurs-Cnt
                 SET WS-EM-IDX TO 1
                 MOVE WS-Hold-Error-Message TO 
                    WS-EM-Message(WS-EM-IDX)
              WHEN 1
              WHEN 2
                 ADD +1 TO WS-Error-Message-Occurs-Cnt
                 SET WS-EM-IDX UP BY 1
                 MOVE WS-Hold-Error-Message TO 
                    WS-EM-Message(WS-EM-IDX)
              WHEN 3
                 ADD +1 TO WS-Error-Message-Occurs-Cnt 
           END-EVALUATE.

       3000-End-Job.
      D    DISPLAY "PARTCALL: 3000-End-Job".     
