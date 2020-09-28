      ***********************************************************
      * Program name:    PSAP
      * Original author: David Stagowski
      *
      *    Description: This is the main driver program.
      *    This will read the PART-SUPPLIER-ADDRESS-PO file and
      *    divide it up into 4 parts and call the 4 modules to
      *    process the data and write valid records to a new dataset.
      *
      *
      *    Parts:  PARTEDIT
      *        Input: From driver pgm
      *        Output: PARTFile
      *    Supplier: SUPPEDIT
      *        Input: From driver pgm
      *        Output: SUPPFILE
      *    Address: ADDREDIT
      *        Input: From driver pgm
      *        Output: ADDRFILE
      *    Purchase Orders: POEDIT
      *        Input: From driver pgm
      *        Output: POFILE
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
       PROGRAM-ID. PSAP.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PSAPFile
           ASSIGN TO PSAPFILE
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-PSAPFile-Status.

           SELECT OUTFile
           ASSIGN TO PSAPGOOD
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-OutFile-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  PSAPFile
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY PSAP.

       FD  OUTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
       01  FD-OutFile-Record  PIC X(473).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==PSAPFile==.
           COPY WSFST REPLACING ==:tag:== BY ==OutFile==.

       01  WS-File-Counters.
           12 FD-PSAPFile-Record-Cnt       PIC S9(4) COMP VALUE ZERO.
           12 FD-OutFile-Record-Cnt        PIC S9(4) COMP VALUE ZERO.

       01  WS-Status-Flags.
           12 WS-PSAP-Valid-Flag           PIC X.
              88 WS-PSAP-Valid                VALUE 'Y'.
              88 WS-PSAP-Invalid              VALUE 'N'.
           12 WS-Program-Run               PIC X.
              88 WS-First-Time                VALUE 'F'.
              88 WS-Continue                  VALUE 'C'.
              88 WS-End-of-Run                VALUE 'E'.
       01  WS-Program-Hold-Storage.
           12 WS-Subscript                 PIC S9(4) COMP VALUE ZEROES.
           12 WS-Display-Counter           PIC ZZZ9 VALUE ZEROES.

      *    This is a GREAT candidate for a copybook.
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
              15 WS-CT-Valid-or-Write-Flag    PIC X.
                 88 WS-CT-VW-First-Time          VALUE 'F'.
                 88 WS-CT-VW-Validate            VALUE 'V'.
                 88 WS-CT-VW-Write               VALUE 'W'.
                 88 WS-CT-VW-Done                VALUE 'D'.
      *    Parts: 01-06, Supp:07-11, Addr: 12-17 PO: 18-23.
              15 WS-Error-Message-Area OCCURS 25 TIMES.
                 18 WS-EM-Message             PIC X(30).
                 18 WS-EM-Counter             PIC S9(4).           

       01  WS-Validation-Counters.
           12 WS-InValid-Req-Fields-Cnt     PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Make-Cnt           PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Model-Cnt          PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-WLead-Cnt          PIC S9(4) COMP VALUE ZERO.

       PROCEDURE DIVISION.
       0000-Mainline.
           PERFORM 1000-Begin-Job.
           PERFORM 2000-Process UNTIL WS-PSAPFile-EOF.
           PERFORM 3000-End-Job.
           GOBACK.

       1000-Begin-Job.
           SET WS-First-Time TO TRUE.
           OPEN  INPUT PSAPFile.
           OPEN OUTPUT OUTFILE.
      D    DISPLAY "PSAPFile Status: " WS-PSAPFile-Status.
           PERFORM 5000-Read-PSAPFile.
      *
      *    The output has to be kept in synch.
      *    Each PSAP record will be split up into
      *    1 Part record
      *    1 Supplier record
      *    3 Supplier address records
      *    3 Purchase Order records
     
      *    Each program will be called two times during processing.
      *    1) Validate the data.
      *    2) If ALL the data is valid, call each program again 
      *    to actually write the data to it's particular dataset.
      *
      *    Then at the end of the job, each will be called to close
      *    the outfiles.
      *
       2000-Process.
           IF WS-PSAPFile-Good
              SET WS-PSAP-Valid TO TRUE
              IF WS-First-Time
                 SET WS-CT-VW-First-Time TO TRUE
                 SET WS-Continue TO TRUE
              ELSE
                 SET WS-CT-VW-Validate TO TRUE
              END-IF
              PERFORM 2110-Validate-Parts
              PERFORM 2120-Validate-Suppliers
              PERFORM 2130-Validate-Addresses
              PERFORM 2140-Validate-Purchase-Orders

              IF WS-PSAP-Valid
                 SET WS-CT-VW-Write TO TRUE
                 PERFORM 2210-Write-Parts
                 PERFORM 2220-Write-Suppliers
                 PERFORM 2230-Write-Addresses
                 PERFORM 2230-Write-Purchase-Orders
                 PERFORM 6000-Write-Outfile
              END-IF

              PERFORM 5000-Read-PSAPFile
           END-IF.


       2110-Validate-Parts.
           CALL 'PARTCALL' USING  
              PARTS, WS-Call-Tracking.

           IF WS-CT-Parts-I
              SET WS-PSAP-Invalid TO TRUE
           END-IF.

       2120-Validate-Suppliers.
           CALL 'SUPPCALL' USING  
              SUPPLIERS, WS-Call-Tracking.

           IF WS-CT-Supp-I
              SET WS-PSAP-Invalid TO TRUE
           END-IF.

       2130-Validate-Addresses.
           CALL 'ADDRCALL' USING  
              SUPP-ADDRESSES, WS-Call-Tracking.

           IF WS-CT-Addr-I
              SET WS-PSAP-Invalid TO TRUE
           END-IF.

       2140-Validate-Purchase-Orders.
           CALL 'POCALL' USING  
              PURCHASE-ORDERS, WS-Call-Tracking.

           IF WS-CT-PO-I
              SET WS-PSAP-Invalid TO TRUE
           END-IF.

       2210-Write-Parts.
           CALL 'PARTCALL' USING  
              PARTS, WS-Call-Tracking.

       2220-Write-Suppliers.
           CALL 'SUPPCALL' USING  
              SUPPLIERS, WS-Call-Tracking.

       2230-Write-Addresses.
           CALL 'ADDRCALL' USING  
              SUPP-ADDRESSES, WS-Call-Tracking.

       2230-Write-Purchase-Orders.
           CALL 'POCALL' USING  
              PURCHASE-ORDERS, WS-Call-Tracking.

       3000-End-Job.
           SET WS-End-of-Run TO TRUE.
           SET WS-CT-VW-Done TO TRUE.

           CALL 'PARTCALL' USING  
              PARTS, WS-Call-Tracking. 
           CALL 'SUPPCALL' USING  
              SUPPLIERS, WS-Call-Tracking.
           CALL 'ADDRCALL' USING  
              SUPP-ADDRESSES, WS-Call-Tracking.
           CALL 'POCALL' USING  
              PURCHASE-ORDERS, WS-Call-Tracking.


           PERFORM 3100-Print-End-Job-Messages.

           CLOSE PSAPFile
                 OUTFILE.

       3100-Print-End-Job-Messages.
           DISPLAY "         Records Read: " FD-PSAPFile-Record-Cnt.
           DISPLAY "      Records Written: " FD-OutFile-Record-Cnt.
           COMPUTE FD-PSAPFile-Record-Cnt = 
             FD-PSAPFile-Record-Cnt - FD-OutFile-Record-Cnt.
           DISPLAY "      Bad Rec Written: " FD-PSAPFile-Record-Cnt.
           PERFORM 3110-Print-Part-Messages.
           PERFORM 3120-Print-Supp-Messages.
           PERFORM 3130-Print-Addr-Messages.
           PERFORM 3140-Print-Pur-Ord-Messages.

       3110-Print-Part-Messages.
           PERFORM VARYING WS-Subscript FROM 1 BY 1 UNTIL
              WS-Subscript > 6
              IF WS-EM-Message(WS-Subscript) > SPACES
                 MOVE WS-EM-Counter(WS-Subscript)
                    TO WS-Display-Counter
                 DISPLAY WS-EM-Message(WS-Subscript),
                         WS-Display-Counter
              END-IF
           END-PERFORM.

       3120-Print-Supp-Messages.
           PERFORM VARYING WS-Subscript FROM 7 BY 1 UNTIL
              WS-Subscript > 11
              IF WS-EM-Message(WS-Subscript) > SPACES
                 MOVE WS-EM-Counter(WS-Subscript)
                    TO WS-Display-Counter
                 DISPLAY WS-EM-Message(WS-Subscript),
                         WS-Display-Counter
              END-IF
           END-PERFORM.

       3130-Print-Addr-Messages.
           PERFORM VARYING WS-Subscript FROM 12 BY 1 UNTIL
              WS-Subscript > 17
              IF WS-EM-Message(WS-Subscript) > SPACES
                 MOVE WS-EM-Counter(WS-Subscript)
                    TO WS-Display-Counter
                 DISPLAY WS-EM-Message(WS-Subscript),
                         WS-Display-Counter
              END-IF
           END-PERFORM.

       3140-Print-Pur-Ord-Messages.
           PERFORM VARYING WS-Subscript FROM 18 BY 1 UNTIL
              WS-Subscript > 23
              IF WS-EM-Message(WS-Subscript) > SPACES
                 MOVE WS-EM-Counter(WS-Subscript)
                    TO WS-Display-Counter
                 DISPLAY WS-EM-Message(WS-Subscript),
                         WS-Display-Counter
              END-IF
           END-PERFORM.

       5000-Read-PSAPFile.
           READ PSAPFile
              AT END SET WS-PSAPFile-EOF TO TRUE
           END-READ.
           IF WS-PSAPFile-Good
              ADD +1 TO FD-PSAPFile-Record-Cnt
      D       DISPLAY "PSAPFile Record: " PARTS
           ELSE
              IF WS-PSAPFile-EOF
                 NEXT SENTENCE
              ELSE
                 DISPLAY "** ERROR **: 5000-Read-PSAPFile"
                 DISPLAY "Read PSAPFile Failed."
                 DISPLAY "File Status: " WS-PSAPFile-Status
                 GOBACK
              END-IF
           END-IF.

       6000-Write-Outfile.
           MOVE PART-SUPP-ADDR-PO TO FD-OutFile-Record.
           WRITE FD-OutFile-Record.

           IF WS-OutFile-Good
              ADD +1 TO FD-OutFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **: 6000-Write-Outfile"
              DISPLAY "WRITE OutFile Failed."
              DISPLAY "File Status: " WS-OutFile-Status
              PERFORM 3000-End-Job
              MOVE 8 TO RETURN-CODE
              GOBACK
           END-IF.
