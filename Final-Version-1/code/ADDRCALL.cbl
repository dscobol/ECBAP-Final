      ***********************************************************
      * Program name:    ADDRCALL
      * Original author: David Stagowski
      *
      *    Description: Validate the Addresses from the
      *       Part-Suppliers dataset.
      *    If the record is valid, write the record to new file.
      *
      *
      * Maintenance Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-24 dastagg       Created for ECBAP Final Project
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
      *     ASSIGN TO ZIPFILE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/state-address-zip.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-ZipFile-Status.

           SELECT OUTFile
      *     ASSIGN TO ADDROUT
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/addrout.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-OutFile-Status.


       DATA DIVISION.
       FILE SECTION.
       FD  ZipFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-ZipFile-Record  PIC X(33).

       FD  OUTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY ADDROUT REPLACING ==:tag:== BY ==OutFile==.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==ZipFile==.
           COPY WSFST REPLACING ==:tag:== BY ==OutFile==.

       01  WS-File-Counters.
           12 FD-ZipFile-Record-Cnt        PIC S9(4) COMP VALUE ZERO.
           12 FD-AddrFile-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-OutFile-Record-Cnt        PIC S9(4) COMP VALUE ZERO.

       01  WS-Status-Flags.
           12 WS-Addresses-Valid-Flag       PIC X.
              88 WS-Addresses-Valid            VALUE 'Y'.
              88 WS-Addresses-Invalid          VALUE 'N'.
           12 WS-Address-Valid-Flag         PIC X.
              88 WS-Address-Valid              VALUE 'Y'.
              88 WS-Address-Invalid            VALUE 'N'.
           12 WS-State-Zip-Found-Flag       PIC X.
              88 WS-State-Zip-Found            VALUE 'Y'.
              88 WS-State-Zip-Not-Found        VALUE 'N'.

       01  WS-Validation-Counters.
           12 WS-InValid-Req-Fields-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Type-Cnt           PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-St-Zip-Cnt         PIC S9(4) COMP VALUE ZERO.

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
       COPY ADDRIN.
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
      *    Parts: 01-06, Supp:07-11, Addr: 12-17 PO: 18-23.
              15 WS-Error-Message-Area OCCURS 25 TIMES.
                 18 WS-EM-Message            PIC X(30).
                 18 WS-EM-Counter            PIC S9(4).           

       PROCEDURE DIVISION USING SUPP-ADDRESSES, WS-Call-Tracking.
       0000-Mainline.
           EVALUATE TRUE 
              WHEN WS-CT-VW-First-Time
                 PERFORM 1000-Begin-Job
                 PERFORM 2000-Validate-Addresses
              WHEN WS-CT-VW-Validate 
                 PERFORM 2000-Validate-Addresses
              WHEN WS-CT-VW-Write
                 PERFORM 6000-Write-Address-Records              
              WHEN WS-CT-VW-Done
                 PERFORM 3000-End-Job
           END-EVALUATE.
           GOBACK.

       1000-Begin-Job.
           PERFORM 1010-Load-Zip-Table.
           OPEN OUTPUT OUTFILE.

       1010-Load-Zip-Table.
           OPEN INPUT ZIPFile.
           SET WS-Zip-IDX TO +1.
           PERFORM 1015-Load-Zip Until WS-ZipFile-EOF.
           CLOSE ZipFile.
           PERFORM 1019-Verify-Zip-Table.

       1015-Load-Zip.
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
                 MOVE +8 TO RETURN-CODE
                 GOBACK
              END-IF
           END-IF.

       1019-Verify-Zip-Table.
      *D   DISPLAY "WS-Zip-Table: "
      *D    PERFORM VARYING WS-Zip-IDX FROM 1 BY 1
      *D       UNTIL WS-Zip-IDX > WS-Zip-Occurs-Dep-Counter
      *D       DISPLAY WS-Zip-Table(WS-Zip-IDX)
      *D    END-PERFORM.
           IF WS-Zip-Occurs-Dep-Counter >
              WS-Zip-Max-Element-Counter
                 DISPLAY "** ERROR **: 1019-Verify-Zip-Table"
                 DISPLAY "WS table size is too small for file."
                 DISPLAY "Increase WS-Zip-Table-Storage variables."
                 MOVE +9 TO RETURN-CODE
                 GOBACK
           END-IF.

       2000-Validate-Addresses.
              SET WS-Addresses-Valid TO TRUE
              ADD +1 TO FD-AddrFile-Record-Cnt.
              PERFORM 2100-Validate-Address VARYING ADDR-IDX 
                 FROM 1 BY 1 UNTIL ADDR-IDX > 3.

           IF WS-Addresses-Valid
              SET WS-CT-Addr-V TO TRUE
           ELSE
              SET WS-CT-Addr-I TO TRUE
           END-IF.

       2100-Validate-Address.
      *    Required fields: ADDRESS-1, CITY, ADDR-STATE and ZIP-CODE
      *    ADDRESS-TYPE must be one of the 88-level fields
      *    ZIP-CODE and ADDR-STATE must match in the STATEZIP file
           SET WS-Address-Valid TO TRUE.
           PERFORM 2110-Validate-Required-Fields.
           PERFORM 2120-Validate-Type.
           PERFORM 2130-Validate-State-Zip.

           IF WS-Address-Invalid
              SET WS-Addresses-Invalid TO TRUE
           END-IF.
          
       2110-Validate-Required-Fields.
      *    Required fields: ADDRESS-1, CITY, ADDR-STATE and ZIP-CODE
           IF ADDRESS-1(ADDR-IDX) = SPACE OR 
              ADDRESS-1(ADDR-IDX) = LOW-VALUE
              SET WS-Address-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF CITY(ADDR-IDX) = SPACE OR 
              CITY(ADDR-IDX) = LOW-VALUE
              SET WS-Address-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF ADDR-STATE(ADDR-IDX) = SPACE OR 
              ADDR-STATE(ADDR-IDX) = LOW-VALUE
              SET WS-Address-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
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
                ADD +1 TO WS-InValid-Type-Cnt
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
              ADD +1 TO WS-InValid-St-Zip-Cnt
           END-IF.
             
       3000-End-Job.
      D    DISPLAY "ADDRCALL: 3000-End-Job"     
           IF WS-CT-VW-Done
           MOVE "  Addr:   Zip-Records Read: " TO 
              WS-EM-Message(12).
           MOVE FD-ZipFile-Record-Cnt TO 
              WS-EM-Counter(12).

           MOVE "  Addr:  Records Processed: " TO 
              WS-EM-Message(13).
           MOVE FD-AddrFile-Record-Cnt TO 
              WS-EM-Counter(13).

           MOVE "  Addr:    Records Written: " TO  
              WS-EM-Message(14).
           MOVE FD-OutFile-Record-Cnt TO
              WS-EM-Counter(14).

           MOVE "  Addr: InValid-Req-Fields: "  TO 
              WS-EM-Message(15).
           MOVE WS-InValid-Req-Fields-Cnt TO
              WS-EM-Counter(15).

           MOVE "  Addr:       InValid-Type: "  TO 
              WS-EM-Message(16).
           MOVE WS-InValid-Type-Cnt TO
              WS-EM-Counter(16).

           MOVE "  Addr:     InValid-St-Zip: "  TO 
              WS-EM-Message(17).
           MOVE WS-InValid-St-Zip-Cnt TO
              WS-EM-Counter(17).
           CLOSE OUTFILE.

       6000-Write-Address-Records.
           PERFORM 6100-Write-Address VARYING ADDR-IDX 
                 FROM 1 BY 1 UNTIL ADDR-IDX > 3.
           
       6100-Write-Address.
           MOVE ADDRESS-TYPE(ADDR-IDX) TO
                OutFile-ADDRESS-TYPE.
           MOVE ADDRESS-1(ADDR-IDX) TO
                OutFile-ADDRESS-1.
           MOVE ADDRESS-2(ADDR-IDX) TO
                OutFile-ADDRESS-2.
           MOVE ADDRESS-3(ADDR-IDX) TO
                OutFile-ADDRESS-3.
           MOVE CITY(ADDR-IDX) TO
                OutFile-CITY.
           MOVE ADDR-STATE(ADDR-IDX) TO
                OutFile-ADDR-STATE.
           MOVE ZIP-CODE(ADDR-IDX)(1:5) TO
                OutFile-ZIP-CODE.

           WRITE OutFile-SUPP-ADDRESS.

           IF WS-OutFile-Good
              ADD +1 TO FD-OutFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE OutFile Failed."
              DISPLAY "File Status: " WS-OutFile-Status
              GOBACK
           END-IF.
