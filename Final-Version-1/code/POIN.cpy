      ***********************************************************
      * Copybook name: POIN
      * Original author: David Stagowski
      *
      * Description: A copybook used in the POCALL program.
      *
      *    This is a shortened version of the PSAP copybook.
      *    This is only the Purchase Orders part.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01 PURCHASE-ORDERS.
          05 PURCHASE-ORDER OCCURS 3 TIMES INDEXED BY PO-IDX.
              10 PO-NUMBER         PIC X(06) VALUE SPACES.
              10 BUYER-CODE        PIC X(03) VALUE SPACES.
              10 QUANTITY          PIC S9(7) VALUE ZERO.
              10 UNIT-PRICE        PIC S9(7)V99 VALUE ZERO.
              10 ORDER-DATE        PIC 9(08) VALUE ZERO.
              10 DELIVERY-DATE     PIC 9(08) VALUE ZERO.
