      ***********************************************************
      * Copybook name: PURCHRDS
      * Original author: David Stagowski
      *
      * Description: A copybook for PO Output file
      *
      *    This is used in the PSAP program.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01  PURCHASE-ORDERS.
           05  PO-NUMBER           PIC X(06) VALUE SPACES.
           05  BUYER-CODE          PIC X(03) VALUE SPACES.
           05  QUANTITY            PIC S9(8) COMP VALUE ZERO.
           05  UNIT-PRICE          PIC S9(7)V99 COMP-3 VALUE ZERO.
           05  ORDER-DATE          PIC X(08) VALUE SPACES.
           05  DELIVERY-DATE       PIC X(08) VALUE SPACES.
