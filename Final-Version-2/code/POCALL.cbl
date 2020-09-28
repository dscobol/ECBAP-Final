      ***********************************************************
      * Program name:    POCALL
      * Original author: David Stagowski
      *
      *    Description: Validate the Purchase Orders from the
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
       PROGRAM-ID. POCALL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      * SOURCE-COMPUTER.   IBM WITH DEBUGGING MODE.

       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-Status-Flags.
           12 WS-All-POs-Valid-Flag   PIC X.
              88 WS-All-POs-Valid        VALUE 'Y'.
              88 WS-All-POs-Invalid      VALUE 'N'.
           12 WS-Pur-Ord-Valid-Flag        PIC X.
              88 WS-Pur-Ord-Valid             VALUE 'Y'.
              88 WS-Pur-Ord-Invalid           VALUE 'N'.
           12 WS-Qty-Valid-Flag            PIC X.
              88 WS-Qty-Valid                 VALUE 'Y'.
              88 WS-Qty-Invalid               VALUE 'N'.
           12 WS-Unit-Valid-Flag           PIC X.
              88 WS-Unit-Valid                VALUE 'Y'.
              88 WS-Unit-Invalid              VALUE 'N'.
           12 WS-ODate-Valid-Flag            PIC X.
              88 WS-ODate-Valid                 VALUE 'Y'.
              88 WS-ODate-Invalid               VALUE 'N'.
           12 WS-DDate-Valid-Flag           PIC X.
              88 WS-DDate-P-Valid              VALUE 'P'.
              88 WS-DDate-P-Invalid            VALUE 'I'.
              88 WS-DDate-Not-Present          VALUE 'N'.

       01  WS-Hold-Storage.
           12 WS-Hold-Error-Message      PIC X(30) VALUE SPACES.
           12 WS-Hold-Compute-Date1      PIC 9(8) VALUE ZERO.
           12 WS-Hold-Compute-Date2      PIC 9(8) VALUE ZERO.

       01  WS-Program-Hold-Fields.
           12 WS-Date-Int-Returned         PIC 9(10) VALUE ZERO.
           12 WS-Date-Diff                 PIC S9(10) VALUE ZERO.
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
           PERFORM 2000-Validate-Purch-Orders.
           PERFORM 3000-End-Job.

           IF WS-All-POs-Valid
              MOVE 0 to RETURN-CODE
           ELSE
              MOVE 8 to RETURN-CODE
           END-IF.

           GOBACK.

       1000-Begin-Job.
      D    DISPLAY "POCALL: 1000-Begin-Job"     
           SET WS-All-POs-Valid TO TRUE.

       2000-Validate-Purch-Orders.
              PERFORM 2100-Validate-Purchase-Order VARYING PO-IDX 
                 FROM 1 BY 1 UNTIL PO-IDX > 3.

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
              SET WS-All-POs-Invalid TO TRUE
           END-IF.

       2110-Validate-Required-Fields.
      *     The following fields are required: PO-NUMBER, BUYER-CODE,
      *          ORDER-,DATE, QUANTITY
           IF PO-NUMBER(PO-IDX) NOT > SPACE 
              SET WS-Pur-Ord-Invalid TO TRUE
              MOVE 'PO Number Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
           IF BUYER-CODE(PO-IDX) NOT > SPACE
              SET WS-Pur-Ord-Invalid TO TRUE
              MOVE 'Buyer Code Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.
           IF ORDER-DATE(PO-IDX) NOT > SPACE
              SET WS-Pur-Ord-Invalid TO TRUE
              MOVE 'Order Date Missing' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
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
              IF QUANTITY(PO-IDX) >= 1 AND
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
              MOVE 'Quantity Incorrect' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.

           IF WS-Unit-Invalid
              SET WS-Pur-Ord-Invalid TO TRUE
              MOVE 'Unit Price Incorrect' TO WS-Hold-Error-Message
              PERFORM 2199-Add-Error-Message
           END-IF.


       2130-Validate-Order-Date.
      *     ORDER-DATE must be a valid date
           IF ORDER-DATE(PO-IDX) > SPACE
              MOVE ORDER-DATE(PO-IDX) TO WS-Hold-Compute-Date1
              COMPUTE WS-Date-Int-Returned =
                 FUNCTION INTEGER-OF-DATE(WS-Hold-Compute-Date1)
              IF WS-Date-Int-Returned = 0
                 SET WS-Pur-Ord-Invalid TO TRUE
                 SET WS-ODate-Invalid TO TRUE
                 MOVE 'Order Date Invalid' TO 
                    WS-Hold-Error-Message
                 PERFORM 2199-Add-Error-Message
              ELSE 
                 SET WS-ODate-Valid TO TRUE
              END-IF
           END-IF.

      * On the mainframe, use this instead
      *     MOVE ORDER-DATE(PO-IDX) TO W-DATE-IN-STR-CEE
      *     CALL 'CEEDAYS' USING W-DATE-IN-CEE
      *         W-PICSTR-IN, W-INPUT-DATE-INT, FC
      *     IF FC-SEV = ZERO
      *        SET WS-ODate-Valid TO TRUE
      *     ELSE
      *        SET WS-Pur-Ord-Invalid TO TRUE
      *        SET WS-ODate-Invalid TO TRUE
      *        MOVE 'Order Date Incorrect' TO 
      *           WS-Hold-Error-Message
      *        PERFORM 2199-Add-Error-Message
      *     END-IF.


      *    IF present, DELIVERY-DATE must be a valid date
           IF DELIVERY-DATE(PO-IDX) > SPACES
              MOVE DELIVERY-DATE(PO-IDX) TO WS-Hold-Compute-Date2
              COMPUTE WS-Date-Int-Returned =
                 FUNCTION INTEGER-OF-DATE(WS-Hold-Compute-Date2)
              IF WS-Date-Int-Returned = 0
                 SET WS-Pur-Ord-Invalid TO TRUE
                 SET WS-DDate-P-Invalid TO TRUE
                 MOVE 'Delivery Date Incorrect' TO 
                    WS-Hold-Error-Message
                 PERFORM 2199-Add-Error-Message
              ELSE 
                 SET WS-DDate-P-Valid TO TRUE
              END-IF
           ELSE 
              SET WS-DDate-Not-Present TO TRUE
           END-IF.

      * On the mainframe, use this instead
      *     IF DELIVERY-DATE(PO-IDX) > SPACES
      *        MOVE DELIVERY-DATE(PO-IDX) TO W-DATE-IN-STR-CEE
      *        CALL 'CEEDAYS' USING W-DATE-IN-CEE
      *            W-PICSTR-IN, W-INPUT-DATE-INT, FC
      *        IF FC-SEV = ZERO
      *           SET WS-DDate-P-Valid TO TRUE
      *        ELSE
      *           SET WS-Pur-Ord-Invalid TO TRUE
      *           SET WS-DDate-P-Invalid TO TRUE
      *           MOVE 'Delivery Date Incorrect' TO 
      *              WS-Hold-Error-Message
      *           PERFORM 2199-Add-Error-Message
      *        END-IF
      *     ELSE 
      *        SET WS-DDate-Not-Present TO TRUE
      *     END-IF.

      *    I can use the same function for Date Diff
      *    but if on mainframe, need to move the values first.                      
      *     MOVE ORDER-DATE(PO-IDX) TO WS-Hold-Compute-Date1
      *     MOVE DELIVERY-DATE(PO-IDX) TO WS-Hold-Compute-Date2
           IF WS-ODate-Valid AND WS-DDate-P-Valid
              COMPUTE WS-Date-Diff =
                 FUNCTION INTEGER-OF-DATE(WS-Hold-Compute-Date2) -
                 FUNCTION INTEGER-OF-DATE(WS-Hold-Compute-Date1)
              IF WS-Date-Diff >= ZERO
                 CONTINUE
              ELSE 
                 SET WS-Pur-Ord-Invalid TO TRUE
                 MOVE 'Delivery Date < Ord Date' TO 
                    WS-Hold-Error-Message
                 PERFORM 2199-Add-Error-Message
              END-IF
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
      D    DISPLAY "POCALL: 3000-End-Job".     
