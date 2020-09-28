      ***********************************************************
      * Copybook name: SUPPOUT
      * Original author: David Stagowski
      *
      * Description: A copybook for Suppliers Output file
      *
      *    This is used in the SUPPCALL program.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01 :tag:-SUPPLIERS.
           05  :tag:-SUPPLIER-CODE     PIC X(05) VALUE SPACES.
           05  :tag:-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                88 :tag:-SUBCONTRACTOR  VALUE 'S'.
                88 :tag:-DISTRIBUTOR    VALUE 'D'.
                88 :tag:-MANUFACTURER   VALUE 'M'.
                88 :tag:-IMPORTER       VALUE 'I'.
           05  :tag:-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
           05  :tag:-SUPPLIER-PERF     PIC 9(03) COMP VALUE ZERO.
           05  :tag:-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                88 :tag:-HIGHEST-QUALITY VALUE '3'.
                88 :tag:-AVERAGE-QUALITY VALUE '2'.
                88 :tag:-LOWEST-QUALITY  VALUE '1'.
           05  :tag:-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                88 :tag:-GOVT-COMM       VALUE '1'.
                88 :tag:-GOVT-ONLY       VALUE '2'.
                88 :tag:-COMMERCIAL-ONLY VALUE '3'.
           05  :tag:-SUPPLIER-ACT-DATE PIC X(08) VALUE SPACES. 
