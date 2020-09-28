      ***********************************************************
      * Copybook name: PARTS
      * Original author: David Stagowski
      *
      * Description: A copybook for PART Output file
      *
      *    This is used in the PSAP program.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01  PARTS.
           05  PART-NUMBER       PIC X(23) VALUE SPACES.
           05  PART-NAME         PIC X(14) VALUE SPACES.
           05  SPEC-NUMBER       PIC X(07) VALUE SPACES.
           05  GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
           05  BLUEPRINT-NUMBER  PIC X(05) VALUE SPACES.
           05  UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
           05  WEEKS-LEAD-TIME   PIC S9(04) COMP VALUE ZEROS.
           05  VEHICLE-MAKE      PIC X(03) VALUE SPACES.
                88 CHRYSLER       VALUE 'CHR'.
                88 FORD           VALUE 'FOR'.
                88 GM             VALUE 'GM '.
                88 VOLKSWAGON     VALUE 'VW '.
                88 TOYOTA         VALUE 'TOY'.
                88 JAGUAR         VALUE 'JAG'.
                88 PEUGEOT        VALUE 'PEU'.
                88 BMW            VALUE 'BMW'.
           05  VEHICLE-MODEL     PIC X(05) VALUE SPACES.
           05  VEHICLE-YEAR      PIC X(04) VALUE '0000'.
