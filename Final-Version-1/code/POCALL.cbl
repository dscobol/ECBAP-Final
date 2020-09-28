      ***********************************************************
      * Program name:    POCALL
      * Original author: David Stagowski
      *
      *    Description: Validate the Purchase Orders from the
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
       PROGRAM-ID. POCALL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFile
      *     ASSIGN TO POOUT
      *     ORGANIZATION IS SEQUENTIAL
           ASSIGN TO "../data/poout.dat.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS WS-OutFile-Status.

       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS.
           COPY POOUT REPLACING ==:tag:== BY ==OutFile==.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           COPY WSFST REPLACING ==:tag:== BY ==OutFile==.

       01  WS-File-Counters.
           12 FD-POFile-Record-Cnt         PIC S9(4) COMP VALUE ZERO.
           12 FD-OutFile-Record-Cnt        PIC S9(4) COMP VALUE ZERO.

       01  WS-Status-Flags.
           12 WS-Purch-Orders-Valid-Flag   PIC X.
              88 WS-Purch-Orders-Valid        VALUE 'Y'.
              88 WS-Purch-Orders-Invalid      VALUE 'N'.
           12 WS-Pur-Ord-Valid-Flag        PIC X.
              88 WS-Pur-Ord-Valid             VALUE 'Y'.
              88 WS-Pur-Ord-Invalid           VALUE 'N'.
           12 WS-Qty-Valid-Flag            PIC X.
              88 WS-Qty-Valid                 VALUE 'Y'.
              88 WS-Qty-Invalid               VALUE 'N'.
           12 WS-Unit-Valid-Flag           PIC X.
              88 WS-Unit-Valid                VALUE 'Y'.
              88 WS-Unit-Invalid              VALUE 'N'.

       01  WS-Program-Hold-Fields.
           12 WS-Hold-PO-Date              PIC 9(8).
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
           12 WS-InValid-Quantity-Cnt      PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Unit-Price-Cnt    PIC S9(4) COMP VALUE ZERO.
           12 WS-InValid-Order-Date-Cnt    PIC S9(4) COMP VALUE ZERO.

       LINKAGE SECTION.
       COPY POIN.
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

       PROCEDURE DIVISION USING PURCHASE-ORDERS, WS-Call-Tracking.
       0000-Mainline.
           EVALUATE TRUE 
              WHEN WS-CT-VW-First-Time
                 PERFORM 1000-Begin-Job
                 PERFORM 2000-Validate-Purch-Orders
              WHEN WS-CT-VW-Validate 
                 PERFORM 2000-Validate-Purch-Orders
              WHEN WS-CT-VW-Write
                 PERFORM 6000-Write-Purch-Orders              
              WHEN WS-CT-VW-Done
                 PERFORM 3000-End-Job
           END-EVALUATE.
           GOBACK.

       1000-Begin-Job.
           OPEN OUTPUT OUTFILE.

       2000-Validate-Purch-Orders.
              SET WS-Purch-Orders-Valid TO TRUE
              ADD +1 TO FD-POFile-Record-Cnt.
              PERFORM 2100-Validate-Purchase-Order VARYING PO-IDX 
                 FROM 1 BY 1 UNTIL PO-IDX > 3.

           IF WS-Purch-Orders-Valid
              SET WS-CT-PO-V TO TRUE
           ELSE
              SET WS-CT-PO-I TO TRUE
           END-IF.

       2100-Validate-Purchase-Order.
      *     The following fields are required: PO-NUMBER, BUYER-CODE,
      *          ORDER-,DATE, QUANTITY
      *     QUANTITY must be between 0 and 999,999
      *     If QUANTITY is > 0, UNIT-PRICE must be > 0.
      *     UNIT-PRICE must be between $1 and $1,000,000.00
      *     ORDER-DATE must be a valid date
      *     You will use the code from CDAT2.CBL -calling 'CEEDAYS'
      *          to determine if the date is valid
           SET WS-Pur-Ord-Valid TO TRUE.
           PERFORM 2110-Validate-Required-Fields.
           PERFORM 2120-Validate-Qty-Unit-Price.
           PERFORM 2130-Validate-Order-Date.
           
           IF WS-Pur-Ord-InValid
              SET WS-Purch-Orders-Invalid TO TRUE
           END-IF.


       2110-Validate-Required-Fields.
      *     The following fields are required: PO-NUMBER, BUYER-CODE,
      *          ORDER-,DATE, QUANTITY
           IF PO-NUMBER(PO-IDX) NOT > SPACE
              SET WS-Pur-Ord-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF BUYER-CODE(PO-IDX) NOT > SPACE
              SET WS-Pur-Ord-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.
           IF ORDER-DATE(PO-IDX) NOT > SPACE
              SET WS-Pur-Ord-Invalid TO TRUE
              ADD +1 TO WS-InValid-Req-Fields-Cnt
           END-IF.

       2120-Validate-Qty-Unit-Price.
      *     QUANTITY must be between 0 and 999,999
      *     If QUANTITY is > 0, UNIT-PRICE must be > 0.
           SET WS-Qty-Valid TO TRUE.
           SET WS-Unit-Valid TO TRUE.
           IF QUANTITY(PO-IDX) IS NOT NUMERIC
              SET WS-Qty-Invalid TO TRUE
           END-IF.

           IF UNIT-PRICE(PO-IDX) IS NOT NUMERIC
              SET WS-Unit-Invalid TO TRUE
           END-IF.

           IF WS-Qty-Valid AND QUANTITY(PO-IDX) NOT ZERO
              IF QUANTITY(PO-IDX) >= 1.00 AND
                 QUANTITY(PO-IDX) <= 999999
                 NEXT SENTENCE
              ELSE
                 SET WS-Unit-Invalid TO TRUE
              END-IF.

           IF WS-Qty-Valid AND WS-Unit-Valid
              IF QUANTITY(PO-IDX) = ZERO AND
                 UNIT-PRICE(PO-IDX) > ZERO
                 SET WS-Qty-Invalid TO TRUE
              END-IF
              IF QUANTITY(PO-IDX) > ZERO AND
                 UNIT-PRICE(PO-IDX) = ZERO
                 SET WS-Unit-Invalid TO TRUE
              END-IF
           END-IF.

      *     UNIT-PRICE must be between $1 and $1,000,000.00
           IF WS-Unit-Valid AND UNIT-PRICE(PO-IDX) NOT ZERO
              IF UNIT-PRICE(PO-IDX) >= 1.00 AND
                 UNIT-PRICE(PO-IDX) <= 1000000.00
                 NEXT SENTENCE
              ELSE
                 SET WS-Unit-Invalid TO TRUE
              END-IF
           END-IF.

           IF WS-Qty-Invalid
              SET WS-Pur-Ord-Invalid TO TRUE
              ADD +1 TO WS-InValid-Quantity-Cnt
           END-IF.

           IF WS-Unit-Invalid
              SET WS-Pur-Ord-Invalid TO TRUE
              ADD +1 TO WS-InValid-Unit-Price-Cnt
           END-IF.


       2130-Validate-Order-Date.
      *     ORDER-DATE must be a valid date
           MOVE ORDER-DATE(PO-IDX) TO WS-Hold-PO-Date.
           COMPUTE WS-Date-Int-Returned =
              FUNCTION INTEGER-OF-DATE(WS-Hold-PO-Date).
           IF WS-Date-Int-Returned = 0
              SET WS-Pur-Ord-Invalid TO TRUE
              ADD +1 TO WS-InValid-Order-Date-Cnt
           END-IF.
     
      * On the mainframe, use this instead
      *     MOVE ORDER-DATE(PO-IDX) TO W-DATE-IN-STR-CEE
      *     CALL 'CEEDAYS' USING W-DATE-IN-CEE
      *         W-PICSTR-IN, W-INPUT-DATE-INT, FC
      *     IF FC-SEV = ZERO
      *        NEXT SENTENCE
      *     ELSE
      *        SET WS-Pur-Ord-Invalid TO TRUE
      *        ADD +1 TO WS-InValid-Order-Date-Cnt
      *     END-IF.

       3000-End-Job.
      D    DISPLAY "POCALL: 3000-End-Job"     
           IF WS-CT-VW-Done
           MOVE "    Po:  Records Processed: " TO 
              WS-EM-Message(18).
           MOVE FD-POFile-Record-Cnt TO 
              WS-EM-Counter(18).

           MOVE "    Po:    Records Written: " TO  
              WS-EM-Message(19).
           MOVE FD-OutFile-Record-Cnt TO
              WS-EM-Counter(19).

           MOVE "    Po: InValid-Req-Fields: "  TO 
              WS-EM-Message(20).
           MOVE WS-InValid-Req-Fields-Cnt TO
              WS-EM-Counter(20).

           MOVE "    Po:   InValid-Quantity: "  TO 
              WS-EM-Message(21).
           MOVE WS-InValid-Quantity-Cnt TO
              WS-EM-Counter(21).

           MOVE "    Po: InValid-Unit-Price: "  TO 
              WS-EM-Message(22).
           MOVE WS-InValid-Unit-Price-Cnt TO
              WS-EM-Counter(22).

           MOVE "    Po: InValid-Order-Date: "  TO 
              WS-EM-Message(23).
           MOVE WS-InValid-Order-Date-Cnt TO
              WS-EM-Counter(23).

           CLOSE OUTFILE.
                 
       6000-Write-Purch-Orders.
           PERFORM 6100-Write-Purch-Order VARYING PO-IDX 
                 FROM 1 BY 1 UNTIL PO-IDX > 3.
           
       6100-Write-Purch-Order.
           MOVE PO-NUMBER(PO-IDX) TO
                OutFile-PO-NUMBER.
           MOVE BUYER-CODE(PO-IDX) TO
                OutFile-BUYER-CODE.
           MOVE QUANTITY(PO-IDX) TO
                OutFile-QUANTITY.
           MOVE UNIT-PRICE(PO-IDX) TO
                OutFile-UNIT-PRICE.
           MOVE ORDER-DATE(PO-IDX) TO
                OutFile-ORDER-DATE.
           MOVE DELIVERY-DATE(PO-IDX) TO
                OutFile-DELIVERY-DATE.

           WRITE OutFile-PURCHASE-ORDERS.

           IF WS-OutFile-Good
              ADD +1 TO FD-OutFile-Record-Cnt
           ELSE
              DISPLAY "** ERROR **"
              DISPLAY "WRITE OutFile Failed."
              DISPLAY "File Status: " WS-OutFile-Status
              GOBACK
           END-IF.
