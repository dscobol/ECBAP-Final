      ***********************************************************
      * Program name:    ADDRCALL
      * Original author: David Stagowski
      *
      *    Description: Validate the Addresses from the
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
       PROGRAM-ID. ADDRCALL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ZipFile
           ASSIGN TO ZIPFILE
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-ZipFile-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  ZipFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-ZipFile-Record  PIC X(33).


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==ZipFile==.

       01  WS-File-Counters.
           12 FD-ZipFile-Record-Cnt        PIC S9(4) COMP VALUE ZERO.

       01  WS-Status-Flags.
           12 WS-All-Addresses-Valid-Flag       PIC X.
              88 WS-All-Addresses-Valid            VALUE 'Y'.
              88 WS-All-Addresses-Invalid          VALUE 'N'.
           12 WS-Address-Valid-Flag         PIC X.
              88 WS-Address-Valid              VALUE 'Y'.
              88 WS-Address-Invalid            VALUE 'N'.
           12 WS-State-Zip-Found-Flag       PIC X.
              88 WS-State-Zip-Found            VALUE 'Y'.
              88 WS-State-Zip-Not-Found        VALUE 'N'.

       01  WS-Hold-Storage.
           12 WS-Hold-Error-Message      PIC X(30) VALUE SPACES.
           12 WS-Hold-local-Date         PIC 9(8) VALUE ZERO.

       01  WS-Zip-Table-Storage.
           12 WS-Zip-Max-Element-Counter   PIC S9(4) COMP VALUE +100.
           12 WS-Zip-Occurs-Dep-Counter    PIC S9(4) COMP VALUE ZERO.
           12 WS-Zip-Table OCCURS 0 TO 100 TIMES
              DEPENDING ON WS-Zip-Occurs-Dep-Counter
              INDEXED BY WS-Zip-IDX.
              15 WS-Zip-State       PIC X(15).
              15 WS-Zip-Abbr        PIC X(2).
              15 WS-Zip-Low         PIC X(5).
              15 WS-ZIP-High        PIC X(5).
       
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
           PERFORM 2000-Validate-Addresses.
           PERFORM 3000-End-Job.

           IF WS-All-Addresses-Valid
              MOVE 0 to RETURN-CODE
           ELSE
              MOVE 8 to RETURN-CODE
           END-IF.

           GOBACK.

       1000-Begin-Job.
      D    DISPLAY "ADDRCALL: 1000-Begin-Job"     
           SET WS-All-Addresses-Valid TO TRUE.

           PERFORM 8010-Load-Zip-Table.


       2000-Validate-Addresses.
              PERFORM 2100-Validate-Address VARYING ADDR-IDX 
                 FROM 1 BY 1 UNTIL ADDR-IDX > 3.

       2100-Validate-Address.
      *    Required fields: ADDRESS-1, CITY, ADDR-STATE and ZIP-CODE
      *    ADDRESS-TYPE must be one of the 88-level fields
      *    ZIP-CODE and ADDR-STATE must match in the STATEZIP file
           SET WS-Address-Valid TO TRUE.
           PERFORM 2110-Validate-Required-Fields.
           PERFORM 2120-Validate-Type.
           PERFORM 2130-Validate-State-Zip.

           IF WS-Address-Invalid
              SET WS-All-Addresses-Invalid TO TRUE
           END-IF.
          
       2110-Validate-Required-Fields.
      *    Required fields: ADDRESS-1, CITY, ADDR-STATE and ZIP-CODE
           IF ADDRESS-1(ADDR-IDX) = SPACE OR 
              ADDRESS-1(ADDR-IDX) = LOW-VALUE
              SET WS-Address-Invalid TO TRUE
              MOVE 'Address Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
           IF CITY(ADDR-IDX) = SPACE OR 
              CITY(ADDR-IDX) = LOW-VALUE
              SET WS-Address-Invalid TO TRUE
              MOVE 'City Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
           IF ADDR-STATE(ADDR-IDX) = SPACE OR 
              ADDR-STATE(ADDR-IDX) = LOW-VALUE
              SET WS-Address-Invalid TO TRUE
              MOVE 'State Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
      *     IF ZIP-CODE(ADDR-IDX) = SPACE OR 
      *        ZIP-CODE(ADDR-IDX) = LOW-VALUE
      *        SET WS-Address-Invalid TO TRUE
      *        ADD +1 TO WS-InValid-Req-Fields-Cnt
      *     END-IF.

       2120-Validate-Type.
      *    ADDRESS-TYPE must be one of the 88-level fields
           EVALUATE TRUE
              WHEN ORDER-ADDRESS(ADDR-IDX) 
              WHEN SCHED-ADDRESS(ADDR-IDX)
              WHEN REMIT-ADDRESS(ADDR-IDX)
                 CONTINUE
              WHEN OTHER
                SET WS-Address-Invalid TO TRUE
              MOVE 'Address Type Incorrect' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-EVALUATE.

       2130-Validate-State-Zip.
      *    ZIP-CODE and ADDR-STATE must match in the STATEZIP file
           SET WS-State-Zip-Not-Found TO TRUE.
           PERFORM VARYING WS-Zip-IDX FROM 1 BY 1
              UNTIL WS-Zip-IDX > WS-Zip-Occurs-Dep-Counter
              IF ADDR-STATE(ADDR-IDX) = WS-Zip-Abbr(WS-Zip-IDX) AND
                 ZIP-CODE(ADDR-IDX)(1:5) >= WS-ZIP-Low(WS-Zip-IDX) AND 
                 ZIP-CODE(ADDR-IDX)(1:5) <= WS-ZIP-High(WS-Zip-IDX)
                 SET WS-State-Zip-Found TO TRUE
                 SET WS-Zip-IDX TO WS-Zip-Occurs-Dep-Counter
              END-IF
           END-PERFORM.

           IF WS-State-Zip-Not-Found
      D       DISPLAY "Zip check Zip Not Found: " 
      D           ADDR-STATE(ADDR-IDX), ZIP-CODE(ADDR-IDX)
              SET WS-Address-Invalid TO TRUE
              MOVE 'Zip Code Incorrect' TO WS-Hold-Error-Message
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
      D    DISPLAY "ADDRCALL: 3000-End-Job".     

       8010-Load-Zip-Table.
           IF WS-Zip-Occurs-Dep-Counter = 0              
              OPEN INPUT ZIPFile
              SET WS-Zip-IDX TO +1
              PERFORM 8015-Load-Zip Until WS-ZipFile-EOF
              CLOSE ZipFile
              PERFORM 8019-Verify-Zip-Table
           END-IF.

       8015-Load-Zip.
           READ ZipFile
              AT END SET WS-ZipFile-EOF TO TRUE
           END-READ.
           IF WS-ZipFile-Good
              ADD +1 TO
                 FD-ZipFile-Record-Cnt
                 WS-Zip-Occurs-Dep-Counter
              MOVE FD-ZipFile-Record(1:16) TO 
                 WS-Zip-State(WS-Zip-IDX)
              MOVE FD-ZipFile-Record(17:2) TO 
                 WS-Zip-Abbr(WS-Zip-IDX)
              MOVE FD-ZipFile-Record(21:5) TO 
                 WS-Zip-Low(WS-Zip-IDX)
              MOVE FD-ZipFile-Record(29:5) TO 
                 WS-ZIP-High(WS-Zip-IDX)
              SET WS-Zip-IDX UP BY +1
           ELSE
              IF WS-ZipFile-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 1015-Load-Zip"
                 DISPLAY "Read ZipFile Failed."
                 DISPLAY "File Status: " WS-ZipFile-Status
                 MOVE 9 TO RETURN-CODE
                 GOBACK
              END-IF
           END-IF.

       8019-Verify-Zip-Table.
      *D   DISPLAY "WS-Zip-Table: "
      *D    PERFORM VARYING WS-Zip-IDX FROM 1 BY 1
      *D       UNTIL WS-Zip-IDX > WS-Zip-Occurs-Dep-Counter
      *D       DISPLAY WS-Zip-Table(WS-Zip-IDX)
      *D    END-PERFORM.
           IF WS-Zip-Occurs-Dep-Counter >
              WS-Zip-Max-Element-Counter
                 DISPLAY "** ERROR **: 1019-Verify-Zip-Table"
                 DISPLAY "WS table size is too small for file."
                 DISPLAY "Records read in: " WS-Zip-Occurs-Dep-Counter
                 DISPLAY "WS table size: " WS-Zip-Max-Element-Counter
                 DISPLAY "Increase WS-Zip-Table-Storage variables."
                 MOVE 9 TO RETURN-CODE
                 GOBACK
           END-IF.
