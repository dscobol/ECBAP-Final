      ***********************************************************
      * Copybook name: SUPPIN
      * Original author: David Stagowski
      *
      * Description: A copybook used in the SUPPCALL program.
      *
      *    This is a shortened version of the PSAP copybook.
      *    This is only the Suppliers part.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01 SUPPLIERS.
           05  SUPPLIER-CODE     PIC X(10) VALUE SPACES.
           05  SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                88 SUBCONTRACTOR  VALUE 'S'.
                88 DISTRIBUTOR    VALUE 'D'.
                88 MANUFACTURER   VALUE 'M'.
                88 IMPORTER       VALUE 'I'.
           05  SUPPLIER-NAME     PIC X(15) VALUE SPACES.
           05  SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
           05  SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                88 HIGHEST-QUALITY VALUE '3'.
                88 AVERAGE-QUALITY VALUE '2'.
                88 LOWEST-QUALITY  VALUE '1'.
           05  SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                88 GOVT-COMM       VALUE '1'.
                88 GOVT-ONLY       VALUE '2'.
                88 COMMERCIAL-ONLY VALUE '3'.
           05  SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO. 
