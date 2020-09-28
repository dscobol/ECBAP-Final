      ***********************************************************
      * Copybook name: PSAP
      * Original author: David Stagowski
      *
      * Description: A copybook used in the PSAP program.
      *
      *    This is the complete definition of the
      *    PART-SUPP-ADDR-PO record definition.
      *
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01  PART-SUPP-ADDR-PO.
           05 PARTS.
               10  PART-NUMBER       PIC X(23) VALUE SPACES.
               10  PART-NAME         PIC X(14) VALUE SPACES.
               10  SPEC-NUMBER       PIC X(07) VALUE SPACES.
               10  GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
               10  BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
               10  UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
               10  WEEKS-LEAD-TIME   PIC 9(03) VALUE ZERO.
               10  VEHICLE-MAKE      PIC X(03) VALUE SPACES.
                    88 CHRYSLER       VALUE 'CHR'.
                    88 FORD           VALUE 'FOR'.
                    88 GM             VALUE 'GM '.
                    88 VOLKSWAGON     VALUE 'VW '.
                    88 TOYOTA         VALUE 'TOY'.
                    88 JAGUAR         VALUE 'JAG'.
                    88 PEUGEOT        VALUE 'PEU'.
                    88 BMW            VALUE 'BMW'.
               10  VEHICLE-MODEL     PIC X(10) VALUE SPACES.
               10  VEHICLE-YEAR      PIC X(04) VALUE '0000'.
               10  FILLER            PIC X(14) VALUE SPACES.
           05 SUPPLIERS.
               10  SUPPLIER-CODE     PIC X(10) VALUE SPACES.
               10  SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                    88 SUBCONTRACTOR  VALUE 'S'.
                    88 DISTRIBUTOR    VALUE 'D'.
                    88 MANUFACTURER   VALUE 'M'.
                    88 IMPORTER       VALUE 'I'.
               10  SUPPLIER-NAME     PIC X(15) VALUE SPACES.
               10  SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
               10  SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                    88 HIGHEST-QUALITY VALUE '3'.
                    88 AVERAGE-QUALITY VALUE '2'.
                    88 LOWEST-QUALITY  VALUE '1'.
               10  SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                    88 GOVT-COMM       VALUE '1'.
                    88 GOVT-ONLY       VALUE '2'.
                    88 COMMERCIAL-ONLY VALUE '3'.
               10  SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO. 
           05 SUPP-ADDRESSES.
              10 SUPP-ADDRESS OCCURS 3 TIMES INDEXED BY ADDR-IDX.
                  15 ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                     88 ORDER-ADDRESS           VALUE '1'.
                     88 SCHED-ADDRESS           VALUE '2'.
                     88 REMIT-ADDRESS           VALUE '3'.
                  15 ADDRESS-1         PIC X(15) VALUE SPACES.
                  15 ADDRESS-2         PIC X(15) VALUE SPACES.
                  15 ADDRESS-3         PIC X(15) VALUE SPACES.
                  15 CITY              PIC X(15) VALUE SPACES.
                  15 ADDR-STATE        PIC X(02) VALUE SPACES.
                  15 ZIP-CODE          PIC 9(10) VALUE ZERO.
           05 PURCHASE-ORDERS.
              10 PURCHASE-ORDER OCCURS 3 TIMES INDEXED BY PO-IDX.
                  15  PO-NUMBER         PIC X(06) VALUE SPACES.
                  15  BUYER-CODE        PIC X(03) VALUE SPACES.
                  15  QUANTITY          PIC S9(7) VALUE ZERO.
                  15  UNIT-PRICE        PIC S9(7)V99 VALUE ZERO.
                  15  ORDER-DATE        PIC 9(08) VALUE ZERO.
                  15  DELIVERY-DATE     PIC 9(08) VALUE ZERO.
