      ***********************************************************
      * Program name:    SUPPCALL
      * Original author: David Stagowski
      *
      *    Description: Validate the Supplier from the
      *       Part-Suppliers dataset.
      *    If the record is valid, write the record to new file.
      *
      *
      * Maintenance Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      * 20XX-XX-XX               If you change me, change this.
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPCALL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFile
      *     ASSIGN TO SUPPOUT
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/suppout.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-OutFile-Status.

       DATA DIVISION.
       FILE SECTION.

       FD  OUTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY SUPPOUT REPLACING ==:tag:== BY ==OutFile==.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==OutFile==.

       01  WS-File-Counters.
           12 FD-SuppFile-Record-Cnt         PIC S9(4) COMP VALUE ZERO.
           12 FD-OutFile-Record-Cnt          PIC S9(4) COMP VALUE ZERO.

       01  WS-Status-Flags.
           12 WS-Supplier-Valid-Flag         PIC X.
              88 WS-Supplier-Valid           VALUE 'Y'.
              88 WS-Supplier-Invalid         VALUE 'N'.

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

       01  WS-Validation-Counters.
           12 WS-InValid-Req-Fields-Cnt    PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Type-Count        PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Supp-Date-Cnt     PIC S9(4) COMP VALUE ZERO.

       LINKAGE SECTION.
       COPY SUPPIN.
       01  WS-Call-Tracking.
           12 WS-Call-Tracking-Flags.
              15 WS-CT-Parts-Valid-Flag       PIC X.
                 88 WS-CT-Parts-V                VALUE 'Y'.
                 88 WS-CT-Parts-I                VALUE 'I'.
              15 WS-CT-Supp-Valid-Flag        PIC X.
                 88 WS-CT-Supp-V                 VALUE 'Y'.
                 88 WS-CT-Supp-I                 VALUE 'I'.
              15 WS-CT-Addr-Valid-Flag        PIC X.
                 88 WS-CT-Addr-V                 VALUE 'Y'.
                 88 WS-CT-Addr-I                 VALUE 'I'.
              15 WS-CT-PO-Valid-Flag          PIC X.
                 88 WS-CT-PO-V                   VALUE 'Y'.
                 88 WS-CT-PO-I                   VALUE 'I'.
           12 WS-Call-Tracking-Area.
              15 WS-CT-Valid-or-Write-Flag     PIC X.
                 88 WS-CT-VW-First-Time        VALUE 'F'.
                 88 WS-CT-VW-Validate          VALUE 'V'.
                 88 WS-CT-VW-Write             VALUE 'W'.
                 88 WS-CT-VW-Done              VALUE 'D'.
      *    Parts: 01-06, Supp:07-11, Addr: xx-xx PO: xx-xx.
              15 WS-Error-Message-Area OCCURS 25 TIMES.
                 18 WS-EM-Message            PIC X(30).
                 18 WS-EM-Counter            PIC S9(4).           


       PROCEDURE DIVISION USING SUPPLIERS, WS-Call-Tracking.
       0000-Mainline.
           EVALUATE TRUE 
              WHEN WS-CT-VW-First-Time
                 PERFORM 1000-Begin-Job
                 PERFORM 2000-Validate-Supplier
              WHEN WS-CT-VW-Validate 
                 PERFORM 2000-Validate-Supplier
              WHEN WS-CT-VW-Write
                 PERFORM 2200-Build-Outfile
                 PERFORM 6000-Write-Supp-Record              
              WHEN WS-CT-VW-Done
                 PERFORM 3000-End-Job
           END-EVALUATE.
           GOBACK.

       1000-Begin-Job.
      D    DISPLAY "SUPPCALL: 1000-Begin-Job"     
           OPEN OUTPUT OUTFILE.

       2000-Validate-Supplier.
      D    DISPLAY "SUPPCALL: 2000-Validate-Supplier"     
           SET WS-Supplier-Valid TO TRUE.
           ADD +1 TO FD-SuppFile-Record-Cnt.
           PERFORM 2100-Validate-Suppliers.

       2100-Validate-Suppliers.
           PERFORM 2110-Validate-Required-Fields.
           PERFORM 2120-Validate-Correct-Types.
           PERFORM 2130-Validate-Supplier-Date.

           IF WS-Supplier-Valid
              SET WS-CT-Supp-V TO TRUE
           ELSE
              SET WS-CT-Supp-I TO TRUE
           END-IF.

       2110-Validate-Required-Fields.
      *    Required fields: SUPPLIER-CODE, SUPPLIER-TYPE, SUPPLIER-NAME,
      *       SUPPLIER-PERF
           IF SUPPLIER-CODE(1:5) NOT > SPACE
              SET WS-Supplier-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF SUPPLIER-NAME NOT > SPACE
              SET WS-Supplier-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF SUPPLIER-PERF IS NOT NUMERIC OR SUPPLIER-PERF = ZEROES
              SET WS-Supplier-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
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
                ADD +1 TO WS-InValid-Type-Count
           END-EVALUATE.

           EVALUATE TRUE
              WHEN HIGHEST-QUALITY 
              WHEN AVERAGE-QUALITY
              WHEN LOWEST-QUALITY
                 CONTINUE
              WHEN OTHER
                SET WS-Supplier-Invalid TO TRUE
                ADD +1 TO WS-InValid-Type-Count
           END-EVALUATE.

           EVALUATE TRUE
              WHEN GOVT-COMM 
              WHEN GOVT-ONLY
              WHEN COMMERCIAL-ONLY
                 CONTINUE
              WHEN OTHER
                SET WS-Supplier-Invalid TO TRUE
                ADD +1 TO WS-InValid-Type-Count
           END-EVALUATE.

       2130-Validate-Supplier-Date.
      *     SUPPLIER-ACT-DATE must be a valid date
           MOVE SUPPLIER-ACT-DATE TO WS-SUPPLIER-ACT-DATE
           COMPUTE WS-Date-Int-Returned =
              FUNCTION INTEGER-OF-DATE(WS-SUPPLIER-ACT-DATE).
           IF WS-Date-Int-Returned = 0
              SET WS-Supplier-Invalid TO TRUE
              ADD +1 TO WS-InValid-Supp-Date-Cnt
           END-IF.

      * On the mainframe, use this instead
      *     MOVE SUPPLIER-ACT-DATE TO W-DATE-IN-STR-CEE
      *     CALL 'CEEDAYS' USING W-DATE-IN-CEE
      *         W-PICSTR-IN, W-INPUT-DATE-INT, FC
      *     IF FC-SEV = ZERO
      *        NEXT SENTENCE
      *     ELSE
      *        SET WS-Supplier-Invalid TO TRUE
      *        ADD +1 TO WS-InValid-Order-Date-Cnt
      *     END-IF

       2200-Build-Outfile.
           MOVE SUPPLIER-CODE(1:5) TO
                OutFile-SUPPLIER-CODE.
           MOVE SUPPLIER-TYPE TO
                OutFile-SUPPLIER-TYPE.
           MOVE SUPPLIER-NAME TO
                OutFile-SUPPLIER-NAME.
           MOVE SUPPLIER-PERF TO
                OutFile-SUPPLIER-PERF.
           MOVE SUPPLIER-RATING TO
                OutFile-SUPPLIER-RATING.
           MOVE SUPPLIER-STATUS TO
                OutFile-SUPPLIER-STATUS.
           MOVE SUPPLIER-ACT-DATE TO
                OutFile-SUPPLIER-ACT-DATE.


       3000-End-Job.
      D    DISPLAY "PARTCALL: 3000-End-Job"     
           IF WS-CT-VW-Done
           MOVE "  Supp:  Records Processed: " TO 
              WS-EM-Message(7).
           MOVE FD-SuppFile-Record-Cnt TO 
              WS-EM-Counter(7).

           MOVE "  Supp:    Records Written: " TO  
              WS-EM-Message(8).
           MOVE FD-OutFile-Record-Cnt TO
              WS-EM-Counter(8).

           MOVE "  Supp: InValid-Req-Fields: "  TO 
              WS-EM-Message(9).
           MOVE WS-InValid-Req-Fields-Cnt TO
              WS-EM-Counter(9).

           MOVE "  Supp:       InValid-Type: "  TO 
              WS-EM-Message(10).
           MOVE WS-InValid-Type-Count TO
              WS-EM-Counter(10).

           MOVE "  Supp:  InValid-Supp-Date: "  TO 
              WS-EM-Message(11).
           MOVE WS-InValid-Supp-Date-Cnt TO
              WS-EM-Counter(11).

           CLOSE OUTFILE.
                 
       6000-Write-Supp-Record.
           WRITE OutFile-SUPPLIERS.

           IF WS-OutFile-Good
              ADD +1 TO FD-OutFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE OutFile Failed."
              DISPLAY "File Status: " WS-OutFile-Status
              GOBACK
           END-IF.
