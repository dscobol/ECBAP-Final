      ***********************************************************
      * Copybook name: PARTOUT
      * Original author: David Stagowski
      *
      * Description: A copybook for PART Output file
      *
      *    This is used in the PARTCALL program.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01  :tag:-PARTS.
           05  :tag:-PART-NUMBER       PIC X(23) VALUE SPACES.
           05  :tag:-PART-NAME         PIC X(14) VALUE SPACES.
           05  :tag:-SPEC-NUMBER       PIC X(07) VALUE SPACES.
           05  :tag:-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
           05  :tag:-BLUEPRINT-NUMBER  PIC X(05) VALUE SPACES.
           05  :tag:-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
           05  :tag:-WEEKS-LEAD-TIME   PIC S9(04) COMP VALUE ZEROS.
           05  :tag:-VEHICLE-MAKE      PIC X(03) VALUE SPACES.
                88 :tag:-CHRYSLER       VALUE 'CHR'.
                88 :tag:-FORD           VALUE 'FOR'.
                88 :tag:-GM             VALUE 'GM '.
                88 :tag:-VOLKSWAGON     VALUE 'VW '.
                88 :tag:-TOYOTA         VALUE 'TOY'.
                88 :tag:-JAGUAR         VALUE 'JAG'.
                88 :tag:-PEUGEOT        VALUE 'PEU'.
                88 :tag:-BMW            VALUE 'BMW'.
           05  :tag:-VEHICLE-MODEL     PIC X(05) VALUE SPACES.
           05  :tag:-VEHICLE-YEAR      PIC X(04) VALUE '0000'.
