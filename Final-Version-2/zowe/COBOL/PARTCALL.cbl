      ***********************************************************
      * Program name:    PARTCALL
      * Original author: David Stagowski
      *
      *    Description: Validate the Parts from the
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
      * 2020-08-25 dastagg       Created for ECBAP Final Project
      * 20XX-XX-XX               If you change me, change this.
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTCALL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-Status-Flags.
           12 WS-Part-Valid-Flag         PIC X.
              88 WS-Part-Valid           VALUE 'Y'.
              88 WS-Part-Invalid         VALUE 'N'.

       01  WS-Hold-Storage.
           12 WS-Hold-Error-Message      PIC X(30) VALUE SPACES.

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
           PERFORM 2000-Validate-Parts
           PERFORM 3000-End-Job.
 
           IF WS-Part-Valid
              MOVE 0 to RETURN-CODE
           ELSE 
              MOVE 8 TO RETURN-CODE 
           END-IF.

           GOBACK.

       1000-Begin-Job.
      D    DISPLAY "PARTCALL: 1000-Begin-Job".
           SET WS-Part-Valid TO TRUE.

       2000-Validate-Parts.
      D    DISPLAY "PARTCALL: 2000-Validate-Parts"     
           PERFORM 2100-Validate-Parts.

       2100-Validate-Parts.
           PERFORM 2110-Validate-Required-Fields.
           PERFORM 2120-Validate-Make.
           PERFORM 2130-Validate-Year.
           PERFORM 2130-Validate-WLead.

       2110-Validate-Required-Fields.
      *    Required fields: PART-NUMBER/PART-NAME,
      *       VEHICLE-MODEL  
           IF PART-NUMBER NOT > SPACE
              SET WS-Part-Invalid TO TRUE
              MOVE 'Part Number Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
           IF PART-NAME NOT > SPACE
              SET WS-Part-Invalid TO TRUE
              MOVE 'Part Name Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
           IF VEHICLE-MODEL(1:5) NOT > SPACE
              MOVE 'Vehicle Model Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
              SET WS-Part-Invalid TO TRUE
           END-IF.

       2120-Validate-Make.
      *    /VEHICLE-MAKE is Required and 
      *    VEHICLE-MAKE must be one of the listed 88-level fields
           IF VEHICLE-MAKE NOT > SPACE
              SET WS-Part-Invalid TO TRUE
              MOVE 'Vehicle Make Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           ELSE
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
                   MOVE 'Vehicle Make Invalid' TO WS-Hold-Error-Message
                   PERFORM 2199-Add-Error-Message
              END-EVALUATE
           END-IF.

       2130-Validate-Year.
      *    VEHICLE-YEAR must be present and between 1990 and 2019
           IF VEHICLE-YEAR NOT > SPACE
              MOVE 'Vehicle Year Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
              SET WS-Part-Invalid TO TRUE
           ELSE
              IF VEHICLE-YEAR >= "1990" AND
                 VEHICLE-YEAR <= "2019"
                 CONTINUE
              ELSE
                 SET WS-Part-Invalid TO TRUE
                 MOVE 'Vehicle Year not in Range' TO 
                    WS-Hold-Error-Message
                 PERFORM 2199-Add-Error-Message
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
              MOVE 'Weeks Lead Time not in range' TO 
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

