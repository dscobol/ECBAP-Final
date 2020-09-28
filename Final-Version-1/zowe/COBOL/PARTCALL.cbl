      ***********************************************************
      * Program name:    PARTCALL
      * Original author: David Stagowski
      *
      *    Description: Validate the Parts from the
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
       PROGRAM-ID. PARTCALL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFile
           ASSIGN TO PARTOUT
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-OutFile-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PARTOUT REPLACING ==:tag:== BY ==OutFile==.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==OutFile==.

       01  WS-File-Counters.
           12 FD-PartFile-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-OutFile-Record-Cnt        PIC S9(4) COMP VALUE ZERO.

       01  WS-Status-Flags.
           12 WS-Part-Valid-Flag         PIC X.
              88 WS-Part-Valid           VALUE 'Y'.
              88 WS-Part-Invalid         VALUE 'N'.

       01  WS-Validation-Counters.
           12 WS-InValid-Req-Fields-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Make-Cnt           PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Year-Cnt          PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-WLead-Cnt          PIC S9(4) COMP VALUE ZERO.

       LINKAGE SECTION.
       COPY PARTIN.
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
      *    Parts: 01-06, Supp:07-11, Addr: 12-17 PO: xx-xx.
              15 WS-Error-Message-Area OCCURS 25 TIMES.
                 18 WS-EM-Message            PIC X(30).
                 18 WS-EM-Counter            PIC S9(4).           


       PROCEDURE DIVISION USING PARTS, WS-Call-Tracking.
       0000-Mainline.
           EVALUATE TRUE 
              WHEN WS-CT-VW-First-Time
                 PERFORM 1000-Begin-Job
                 PERFORM 2000-Validate-Parts
              WHEN WS-CT-VW-Validate 
                 PERFORM 2000-Validate-Parts
              WHEN WS-CT-VW-Write
                 PERFORM 2200-Build-Outfile
                 PERFORM 6000-Write-Part-Record              
              WHEN WS-CT-VW-Done
                 PERFORM 3000-End-Job
           END-EVALUATE.
           GOBACK.

       1000-Begin-Job.
      D    DISPLAY "PARTCALL: 1000-Begin-Job"     
           OPEN OUTPUT OUTFILE.

       2000-Validate-Parts.
      D    DISPLAY "PARTCALL: 2000-Validate-Parts"     
           SET WS-Part-Valid TO TRUE.
           ADD +1 TO FD-PartFile-Record-Cnt.
           PERFORM 2100-Validate-Parts.

       2100-Validate-Parts.
           PERFORM 2110-Validate-Required-Fields.
           PERFORM 2120-Validate-Make.
           PERFORM 2130-Validate-Year.
           PERFORM 2130-Validate-WLead.

           IF WS-Part-Valid
              SET WS-CT-Parts-V TO TRUE
           ELSE
              SET WS-CT-Parts-I TO TRUE
           END-IF.

       2110-Validate-Required-Fields.
      *    Required fields: PART-NUMBER/PART-NAME/VEHICLE-MAKE,
      *       VEHICLE-MODEL 
           IF PART-NUMBER NOT > SPACE
              SET WS-Part-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF PART-NAME NOT > SPACE
              SET WS-Part-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF VEHICLE-MAKE NOT > SPACE
              SET WS-Part-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF VEHICLE-MODEL(1:5) NOT > SPACE
              SET WS-Part-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.

       2120-Validate-Make.
      *    VEHICLE-MAKE must be one of the listed 88-level fields
           EVALUATE TRUE
              WHEN CHRYSLER 
              WHEN FORD
              WHEN GM
              WHEN VOLKSWAGON
              WHEN TOYOTA
              WHEN JAGUAR
              WHEN PEUGEOT
              WHEN BMW
                 CONTINUE
              WHEN OTHER
                SET WS-Part-Invalid TO TRUE
                ADD +1 TO WS-InValid-Make-Cnt
           END-EVALUATE.


       2130-Validate-Year.
      *    VEHICLE-YEAR must be between 1990 and 2019
           IF VEHICLE-YEAR NOT > SPACE
              SET WS-Part-Invalid TO TRUE
              ADD +1 TO WS-InValid-Year-Cnt
           ELSE
              IF VEHICLE-YEAR >= "1990" AND
                 VEHICLE-YEAR <= "2019"
                 CONTINUE
              ELSE
                 SET WS-Part-Invalid TO TRUE
                 ADD +1 TO WS-InValid-Year-Cnt
              END-IF 
           END-IF.
              
       2130-Validate-WLead.
      *    WEEKS-LEAD-TIME must be numeric and between 1 and 4
           IF WEEKS-LEAD-TIME IS NUMERIC AND
              WEEKS-LEAD-TIME >= 1 AND
              WEEKS-LEAD-TIME <= 4
              CONTINUE
           ELSE
              SET WS-Part-Invalid TO TRUE
              ADD +1 TO WS-InValid-WLead-Cnt
           END-IF.

       2200-Build-Outfile.
           MOVE PART-NUMBER TO
                OutFile-PART-NUMBER.
           MOVE PART-NAME TO
                OutFile-PART-NAME.
           MOVE SPEC-NUMBER TO
                OutFile-SPEC-NUMBER.
           MOVE GOVT-COMML-CODE TO
                OutFile-GOVT-COMML-CODE.
           MOVE UNIT-OF-MEASURE TO
                OutFile-UNIT-OF-MEASURE.
           MOVE WEEKS-LEAD-TIME TO
                OutFile-WEEKS-LEAD-TIME.
           MOVE VEHICLE-MAKE TO
                OutFile-VEHICLE-MAKE.
           MOVE VEHICLE-YEAR TO
                OutFile-VEHICLE-YEAR.

           MOVE BLUEPRINT-NUMBER(1:5) TO
                OutFile-BLUEPRINT-NUMBER.

           MOVE VEHICLE-MODEL(1:5) TO
                OutFile-VEHICLE-MODEL.


       3000-End-Job.
      D    DISPLAY "PARTCALL: 3000-End-Job"     
           IF WS-CT-VW-Done
           MOVE "  Part:  Records Processed: " TO 
              WS-EM-Message(1).
           MOVE FD-PartFile-Record-Cnt TO 
              WS-EM-Counter(1).

           MOVE "  Part:    Records Written: " TO  
              WS-EM-Message(2).
           MOVE FD-OutFile-Record-Cnt TO
              WS-EM-Counter(2).

           MOVE "  Part: InValid-Req-Fields: "  TO 
              WS-EM-Message(3).
           MOVE WS-InValid-Req-Fields-Cnt TO
              WS-EM-Counter(3).

           MOVE "  Part:       InValid-Make: "  TO 
              WS-EM-Message(4).
           MOVE WS-InValid-Make-Cnt TO
              WS-EM-Counter(4).

           MOVE "  Part:       InValid-Year: "  TO 
              WS-EM-Message(5).
           MOVE WS-InValid-Year-Cnt TO
              WS-EM-Counter(5).

           MOVE "  Part:      InValid-WLead: "  TO 
              WS-EM-Message(6).
           MOVE WS-InValid-WLead-Cnt TO
              WS-EM-Counter(6).
           CLOSE OUTFILE.
                 

       6000-Write-Part-Record.
      D    DISPLAY "PARTCALL: 6000-Write-Part-Record"     
           WRITE Outfile-PARTS.

           IF WS-OutFile-Good
              ADD +1 TO FD-OutFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE OutFile Failed."
              DISPLAY "File Status: " WS-OutFile-Status
              GOBACK
           END-IF.
