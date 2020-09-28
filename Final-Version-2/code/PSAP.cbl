      ***********************************************************
      * Program name:    PSAP
      * Original author: David Stagowski
      *
      *    Description: This is the main driver program.
      *    This will read the PART-SUPPLIER-ADDRESS-PO file and
      *    divide it up into 4 areas(segments).
      *
      *    It will call 4 modules to validate the data of the
      *    individual segments.
      *
      *    If ALL the data in the 4 segments pass validation, records 
      *    from each segment will be written to it's own dataset and 
      *    a copy of the now valid record will be written to a new
      *    dataset. If there are errors, then the record will be 
      *    written to an errorfile with a message describing the error.
      *
      *    Parts, Suppliers, Addresses, and PO's:  
      *         Input: PARTSUPP - One record
      *         (IF Valid:)
      *        Output: PSAPGood  - One record
      *
      *    Error File:  
      *         Input: PARTSUPP - One record
      *         (IF Invalid:)
      *        Output: ERRFILE  - One record
      *
      *    Parts:  
      *       Validation Module: PARTCALL
      *        Output: PARTOUT - One record per segment
      *    Supplier:
      *       Validation Module: SUPPCALL
      *        Output: SUPPOUT - One record per segment
      *    Address:
      *       Validation Module: ADDRCALL
      *        Output: ADDROUT - Three records per segment
      *    Purchase Orders:
      *       Validation Module: POCALL
      *        Output: POOUT   - Three records per segment
      *
      *
      * Maintenance Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-25 dastagg       Created for ECBAP Final Project
      * 20XX-XX-XX               If you change me, change this.
      *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PSAP.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PSAPFile
      *     ASSIGN TO PSAPFILE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/psap.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-PSAPFile-Status.

           SELECT PSAPError
      *     ASSIGN TO ERRFILE
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/psaperror.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-PSAPError-Status.

           SELECT PSAPGood
      *     ASSIGN TO PSAPGood
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/psapout.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-PSAPGood-Status.

           SELECT PartOut
      *     ASSIGN TO PARTOUT
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/partout.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-PartFile-Status.

           SELECT SuppOut
      *     ASSIGN TO SUPPOUT
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/suppout.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-SuppFile-Status.

           SELECT AddrOut
      *     ASSIGN TO ADDROUT
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/addrout.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-AddrFile-Status.

           SELECT PoOut
      *     ASSIGN TO POOUT
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/poout.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-PoFile-Status.


       DATA DIVISION.
       FILE SECTION.
       FD  PSAPFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PARTSUPP.

       FD  PSAPError
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS V
           RECORD IS VARYING IN SIZE 
           FROM 473 TO 570 CHARACTERS
           DEPENDING ON WS-Error-Length
           BLOCK CONTAINS 0 RECORDS.
       01  FD-Error-Record  PIC X(570).

       FD  PSAPGood
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-PSAPGood-Record  PIC X(473).

       FD  PartOut
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PARTS.

       FD  SuppOut
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY SUPLIERS.

       FD  AddrOut
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY ADRESSES.

       FD  PoOut
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PURCHRDS.


       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==PSAPFile==.
           COPY WSFST REPLACING ==:tag:== BY ==PSAPError==.
           COPY WSFST REPLACING ==:tag:== BY ==PSAPGood==.
           COPY WSFST REPLACING ==:tag:== BY ==PartFile==.
           COPY WSFST REPLACING ==:tag:== BY ==SuppFile==.
           COPY WSFST REPLACING ==:tag:== BY ==AddrFile==.
           COPY WSFST REPLACING ==:tag:== BY ==PoFile==.

       01  WS-File-Counters.
           12 WS-Error-Length              PIC 9(4) COMP VALUE ZERO.
           12 FD-PSAPFile-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-PSAPError-Record-Cnt      PIC S9(4) COMP VALUE ZERO.
           12 FD-PSAPGood-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-PartFile-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-SuppFile-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-AddrFile-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-PoFile-Record-Cnt         PIC S9(4) COMP VALUE ZERO.

       01  WS-Hold-Error-Messages.
           12 WS-EM-Tally-Setup.
              15 WS-EMT-Hold-String           PIC X(97) VALUE SPACES.
              15 WS-EMT-Hold-String-Length    PIC 9(4)  VALUE ZERO.
              15 WS-EMT-Hold-String-Pointer   PIC S9(4) COMP VALUE ZERO.
              15 WS-EM-Tally-Max-Cnt          PIC S9(4) COMP VALUE +3.
              15 WS-EM-Tally-Occurs-Cnt       PIC S9(4) COMP VALUE ZERO.
              15 WS-EM-Tally-Perform-Cnt      PIC S9(4) COMP VALUE ZERO.
           12 WS-EM-Tally-Table-Setup.
              15 WS-EM-Tally-Table OCCURS 0 TO 3 TIMES
                 DEPENDING ON WS-EM-Tally-Occurs-Cnt
                 INDEXED BY WS-EMT-IDX.
                 18 WS-EMT-Tally           PIC S9(4) VALUE ZERO.
                 18 WS-EMT-Msg             PIC X(30) VALUE SPACES.
       
       01  WS-Status-Flags.
           12 WS-PSAP-Valid-Flag           PIC X.
              88 WS-PSAP-Valid                VALUE 'Y'.
              88 WS-PSAP-Invalid              VALUE 'N'.

       01  WS-Call-Tracking.
           12 WS-Error-Message-Setup.
              15 WS-Error-Message-Max-Cnt     PIC S9(4) COMP VALUE +3.
              15 WS-Error-Message-Occurs-Cnt  PIC S9(4) COMP VALUE ZERO.
              15 WS-EM-Table.
                 18 WS-Error-Messages OCCURS 0 TO 3 TIMES
                 DEPENDING ON WS-Error-Message-Occurs-Cnt
                 INDEXED BY WS-EM-IDX.
                    21 WS-EM-Message            PIC X(30) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           OPEN INPUT  PSAPFile.
           OPEN OUTPUT PSAPGood PSAPError
                       PartOut SuppOut AddrOut PoOut.
                       
           PERFORM 5000-Read-PSAPFile.

       2000-Process.
           PERFORM 2100-Process-PSAPFile-Records UNTIL WS-PSAPFile-EOF.

       2100-Process-PSAPFile-Records.
           SET WS-PSAP-Valid TO TRUE.
           MOVE 0 TO WS-Error-Message-Occurs-Cnt.

           PERFORM 2200-Validate-PSAPFile-Record.
           IF WS-PSAP-Valid
             MOVE PART-SUPP-ADDR-PO TO FD-PSAPGood-Record
             PERFORM 6000-Write-PSAPGood
             PERFORM 2300-Write-Segments
           ELSE
              PERFORM 6200-Write-ErrFile
           END-IF.
           
           PERFORM 5000-Read-PSAPFile.

       2200-Validate-PSAPFile-Record.
           PERFORM 2210-Validate-Part.
           IF WS-Error-Message-Occurs-Cnt < 4
              PERFORM 2220-Validate-Supplier
           END-IF.
           IF WS-Error-Message-Occurs-Cnt < 4
              PERFORM 2230-Validate-Addresses
           END-IF.
           IF WS-Error-Message-Occurs-Cnt < 4
              PERFORM 2240-Validate-Purch-Orders
           END-IF.

       2210-Validate-Part.
           CALL 'PARTCALL' USING PART-SUPP-ADDR-PO, WS-Call-Tracking.
           IF RETURN-CODE = 8  
              SET WS-PSAP-Invalid TO TRUE
              MOVE 0 to RETURN-CODE
           END-IF.

       2220-Validate-Supplier.
           CALL 'SUPPCALL' USING PART-SUPP-ADDR-PO, WS-Call-Tracking.
           IF RETURN-CODE = 8             
              SET WS-PSAP-Invalid TO TRUE
              MOVE 0 to RETURN-CODE
           END-IF.

       2230-Validate-Addresses.
           CALL 'ADDRCALL' USING PART-SUPP-ADDR-PO, WS-Call-Tracking.
           EVALUATE RETURN-CODE
              WHEN 8             
                 SET WS-PSAP-Invalid TO TRUE
                 MOVE 0 to RETURN-CODE
              WHEN 9
                 PERFORM 3000-End-Job
                 GOBACK
           END-EVALUATE.

       2240-Validate-Purch-Orders.
           CALL 'POCALL' USING PART-SUPP-ADDR-PO, WS-Call-Tracking.
           IF RETURN-CODE = 8             
              SET WS-PSAP-Invalid TO TRUE
              MOVE 0 to RETURN-CODE
           END-IF.

       2300-Write-Segments.
           PERFORM 2310-Write-Part.
           PERFORM 2320-Write-Supplier.
           PERFORM 2330-Write-Addresses.
           PERFORM 2340-Write-Purch-Orders.

       2310-Write-Part.
           MOVE PART-NUMBER IN PART-SUPP-ADDR-PO TO
                PART-NUMBER IN PartOut.
           MOVE PART-NAME IN PART-SUPP-ADDR-PO TO
                PART-NAME IN PartOut.
           MOVE SPEC-NUMBER IN PART-SUPP-ADDR-PO TO
                SPEC-NUMBER IN PartOut.
           MOVE GOVT-COMML-CODE IN PART-SUPP-ADDR-PO TO
                GOVT-COMML-CODE IN PartOut.
           MOVE UNIT-OF-MEASURE IN PART-SUPP-ADDR-PO TO
                UNIT-OF-MEASURE IN PartOut.
           MOVE WEEKS-LEAD-TIME IN PART-SUPP-ADDR-PO TO
                WEEKS-LEAD-TIME IN PartOut.
           MOVE VEHICLE-MAKE IN PART-SUPP-ADDR-PO TO
                VEHICLE-MAKE IN PartOut.
           MOVE VEHICLE-YEAR IN PART-SUPP-ADDR-PO TO
                VEHICLE-YEAR IN PartOut.

           MOVE BLUEPRINT-NUMBER IN PART-SUPP-ADDR-PO(1:5) TO
                BLUEPRINT-NUMBER IN PartOut.

           MOVE VEHICLE-MODEL IN PART-SUPP-ADDR-PO(1:5) TO
                VEHICLE-MODEL IN PartOut.               

           WRITE PARTS IN PartOut.

           IF WS-PartFile-Good
              ADD +1 TO FD-PartFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **: 2310-Write-Part"
              DISPLAY "WRITE PartFile Failed."
              DISPLAY "File Status: " WS-PartFile-Status
              GOBACK
           END-IF.

       2320-Write-Supplier.
           MOVE SUPPLIER-CODE IN PART-SUPP-ADDR-PO(1:5) TO
                SUPPLIER-CODE IN SuppOut.

           MOVE SUPPLIER-TYPE IN PART-SUPP-ADDR-PO TO
                SUPPLIER-TYPE IN SuppOut.
           MOVE SUPPLIER-NAME IN PART-SUPP-ADDR-PO TO
                SUPPLIER-NAME IN SuppOut.
           MOVE SUPPLIER-PERF IN PART-SUPP-ADDR-PO TO
                SUPPLIER-PERF IN SuppOut.
           MOVE SUPPLIER-RATING IN PART-SUPP-ADDR-PO TO
                SUPPLIER-RATING IN SuppOut.
           MOVE SUPPLIER-STATUS IN PART-SUPP-ADDR-PO TO
                SUPPLIER-STATUS IN SuppOut.
           MOVE SUPPLIER-ACT-DATE IN PART-SUPP-ADDR-PO TO
                SUPPLIER-ACT-DATE IN SuppOut.

           WRITE SUPPLIERS IN SuppOut.

           IF WS-SuppFile-Good
              ADD +1 TO FD-SuppFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **: 2320-Write-Supplier"
              DISPLAY "WRITE SuppFile Failed."
              DISPLAY "File Status: " WS-SuppFile-Status
              GOBACK
           END-IF.

       2330-Write-Addresses.
           SET ADDR-IDX TO +1.
           PERFORM 2331-Write-Address 3 TIMES.

       2331-Write-Address.

           MOVE ADDRESS-TYPE IN PART-SUPP-ADDR-PO(ADDR-IDX) TO
                ADDRESS-TYPE IN AddrOut.
           MOVE ADDRESS-1 IN PART-SUPP-ADDR-PO(ADDR-IDX) TO
                ADDRESS-1 IN AddrOut.
           MOVE ADDRESS-2 IN  PART-SUPP-ADDR-PO(ADDR-IDX) TO
                ADDRESS-2 IN AddrOut.
           MOVE ADDRESS-3 IN  PART-SUPP-ADDR-PO(ADDR-IDX) TO
                ADDRESS-3 IN AddrOut.
           MOVE CITY IN  PART-SUPP-ADDR-PO(ADDR-IDX) TO
                CITY IN AddrOut.
           MOVE ADDR-STATE IN  PART-SUPP-ADDR-PO(ADDR-IDX) TO
                ADDR-STATE IN AddrOut.
           MOVE ZIP-CODE IN PART-SUPP-ADDR-PO(ADDR-IDX)(1:5) TO
                ZIP-CODE IN AddrOut.

           WRITE SUPP-ADDRESS IN AddrOut.

           IF WS-AddrFile-Good
              ADD +1 TO FD-AddrFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **: 2331-Write-Address"
              DISPLAY "WRITE AddrFile Failed."
              DISPLAY "File Status: " WS-AddrFile-Status
              GOBACK
           END-IF.

           SET ADDR-IDX UP BY +1.

       2340-Write-Purch-Orders.
           SET PO-IDX TO +1.
           PERFORM 2341-Write-Purch-Order 3 TIMES.

       2341-Write-Purch-Order.
           MOVE PO-NUMBER IN PART-SUPP-ADDR-PO(PO-IDX) TO
                PO-NUMBER IN PoOut.
           MOVE BUYER-CODE IN PART-SUPP-ADDR-PO(PO-IDX) TO
                BUYER-CODE IN PoOut.
           MOVE QUANTITY IN PART-SUPP-ADDR-PO(PO-IDX) TO
                QUANTITY IN PoOut.
           MOVE UNIT-PRICE IN PART-SUPP-ADDR-PO(PO-IDX) TO
                UNIT-PRICE IN PoOut.
           MOVE ORDER-DATE IN PART-SUPP-ADDR-PO(PO-IDX) TO
                ORDER-DATE IN PoOut.
           MOVE DELIVERY-DATE IN PART-SUPP-ADDR-PO(PO-IDX) TO
                DELIVERY-DATE IN PoOut.

           WRITE PURCHASE-ORDERS IN PoOut.

           IF WS-PartFile-Good
              ADD +1 TO FD-PartFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **: 2310-Write-Part"
              DISPLAY "WRITE PartFile Failed."
              DISPLAY "File Status: " WS-PartFile-Status
              GOBACK
           END-IF.

           SET PO-IDX UP BY +1.

       3000-End-Job.
           PERFORM 3100-Print-End-Job-Messages.

           CLOSE PSAPFile PSAPError PSAPGood
                 PartOut SuppOut AddrOut PoOut.      

       3100-Print-End-Job-Messages.
           DISPLAY "         Records Read: " FD-PSAPFile-Record-Cnt.
           DISPLAY " Good Records Written: " FD-PSAPGood-Record-Cnt.
           DISPLAY "Error Records Written: " FD-PSAPError-Record-Cnt.

       5000-Read-PSAPFile.
           READ PSAPFile
              AT END SET WS-PSAPFile-EOF TO TRUE
           END-READ.
           EVALUATE TRUE
              WHEN WS-PSAPFile-Good
                 ADD +1 TO FD-PSAPFile-Record-Cnt
              WHEN WS-PSAPFile-EOF
                 CONTINUE
              WHEN OTHER
                 DISPLAY "** ERROR **: 5000-Read-PSAPFile"
                 DISPLAY "Read PSAPFile Failed."
                 DISPLAY "File Status: " WS-PSAPFile-Status
                 PERFORM 3000-End-Job
                 MOVE 8 TO RETURN-CODE
                 GOBACK
           END-EVALUATE.

       6000-Write-PSAPGood.
           WRITE FD-PSAPGood-Record.

           IF WS-PSAPGood-Good
              ADD +1 TO FD-PSAPGood-Record-Cnt
           ELSE
              DISPLAY "** ERROR **: 6000-Write-PSAPGood"
              DISPLAY "WRITE PSAPGood Failed."
              DISPLAY "File Status: " WS-PSAPGood-Status
              PERFORM 3000-End-Job
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       6200-Write-ErrFile.
           IF WS-Error-Message-Occurs-Cnt > 3
              PERFORM 6210-Write-Bad-Record
           ELSE
              PERFORM 6220-Write-Error-Record
           END-IF.

       6210-Write-Bad-Record.
           STRING
              "*** Many Errors  Bad Rec ***: " DELIMITED BY SIZE,
              PART-SUPP-ADDR-PO DELIMITED BY SIZE
              INTO FD-Error-Record.

           COMPUTE WS-Error-Length = 
              FUNCTION SUM(
                 FUNCTION LENGTH(PART-SUPP-ADDR-PO), 30).
                  
           WRITE FD-Error-Record.   
           IF WS-PSAPError-Good
              ADD +1 TO FD-PSAPError-Record-Cnt
           ELSE
              DISPLAY "** ERROR **: 6210-Write-Bad-Record"
              DISPLAY "WRITE PSAPError Failed."
              DISPLAY "File Status: " WS-PSAPError-Status
              PERFORM 3000-End-Job
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       6220-Write-Error-Record.
           PERFORM 6221-Build-Error-Msg.            
                  
           WRITE FD-Error-Record.   
           IF WS-PSAPError-Good
              ADD +1 TO FD-PSAPError-Record-Cnt
           ELSE
              DISPLAY "** ERROR **: 6210-Write-Bad-Record"
              DISPLAY "WRITE PSAPError Failed."
              DISPLAY "File Status: " WS-PSAPError-Status
              PERFORM 3000-End-Job
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.

       6221-Build-Error-Msg.
           PERFORM 6222-Setup-EMT-Area.
           PERFORM 6223-Tally-Size.
           PERFORM 6224-Build-Error-String.
           PERFORM 6225-Concatentate.

       6222-Setup-EMT-Area.
           INITIALIZE WS-EM-Tally-Setup.
           SET WS-EM-IDX TO 1.
           SET WS-EMT-IDX TO 1.
      *    Clean up the table.
           PERFORM 3 TIMES
              MOVE ZERO TO WS-EMT-Tally(WS-EMT-IDX)
              MOVE SPACES TO WS-EMT-Msg(WS-EMT-IDX)
              SET WS-EM-IDX UP BY 1
              SET WS-EMT-IDX UP BY 1
           END-PERFORM.
      *    Load the new WS Error msg table.
           SET WS-EM-IDX TO 1.
           SET WS-EMT-IDX TO 1.
           MOVE 0 TO WS-EM-Tally-Occurs-Cnt
           PERFORM WS-Error-Message-Occurs-Cnt TIMES
              MOVE WS-EM-Message(WS-EM-IDX) TO 
                 WS-EMT-Msg(WS-EMT-IDX)
              ADD +1 TO WS-EM-Tally-Occurs-Cnt
              SET WS-EM-IDX UP BY 1
              SET WS-EMT-IDX UP BY 1
           END-PERFORM.
         
       6223-Tally-Size.
           SET WS-EMT-IDX TO 1.
           PERFORM WS-EM-Tally-Occurs-Cnt TIMES
              INSPECT FUNCTION REVERSE(WS-EMT-Msg(WS-EMT-IDX)) 
                TALLYING WS-EMT-Tally(WS-EMT-IDX) FOR LEADING SPACES
              COMPUTE WS-EMT-Tally(WS-EMT-IDX) = 
                 30 - WS-EMT-Tally(WS-EMT-IDX) 
              END-COMPUTE
              SET WS-EMT-IDX UP BY 1
           END-PERFORM.

       6224-Build-Error-String.
           SET WS-EMT-IDX TO 1.
           MOVE SPACES TO WS-EMT-Hold-String.
           MOVE +1 TO WS-EMT-Hold-String-Pointer.
           MOVE +1 TO WS-EM-Tally-Perform-Cnt.
      *    If the last err msg, end with ":" else ","
           PERFORM WS-EM-Tally-Occurs-Cnt TIMES
              EVALUATE (WS-EM-Tally-Occurs-Cnt - 
                 WS-EM-Tally-Perform-Cnt)
                 WHEN 0
                 STRING
                    WS-EMT-Msg(WS-EMT-IDX)(1:WS-EMT-Tally(WS-EMT-IDX))
                       DELIMITED BY WS-EMT-Tally(WS-EMT-IDX)
                    ":"
                       DELIMITED BY SIZE
                  INTO WS-EMT-Hold-String
                  WITH POINTER WS-EMT-Hold-String-Pointer
                 END-STRING
                 WHEN OTHER
                 STRING
                    WS-EMT-Msg(WS-EMT-IDX)(1:WS-EMT-Tally(WS-EMT-IDX))
                       DELIMITED BY WS-EMT-Tally(WS-EMT-IDX)
                    ","
                       DELIMITED BY SIZE
                  INTO WS-EMT-Hold-String
                  WITH POINTER WS-EMT-Hold-String-Pointer
                 END-STRING
              END-EVALUATE
              SET WS-EMT-IDX UP BY 1
              ADD +1 TO WS-EM-Tally-Perform-Cnt
              ADD +1 TO WS-EMT-Hold-String-Pointer
           END-PERFORM.

       6225-Concatentate.
      *    Find out where the data ends.
           INSPECT FUNCTION REVERSE(WS-EMT-Hold-String) 
             TALLYING WS-EMT-Hold-String-Length FOR LEADING SPACES.
      *    Trim the spaces.
           COMPUTE WS-EMT-Hold-String-Length = 
              97 - WS-EMT-Hold-String-Length 
           END-COMPUTE.
      *    Put it all together.
           STRING
              WS-EMT-Hold-String(1:WS-EMT-Hold-String-Length) 
                 DELIMITED BY WS-EMT-Hold-String-Length,
              SPACE
                 DELIMITED BY SIZE,
              PART-SUPP-ADDR-PO 
                 DELIMITED BY SIZE
              INTO FD-Error-Record.
      *    Calc the total length of the record.
           COMPUTE WS-Error-Length = 
              FUNCTION SUM(
                 FUNCTION LENGTH(PART-SUPP-ADDR-PO), 
                 WS-EMT-Hold-String-Length)
           END-COMPUTE.
