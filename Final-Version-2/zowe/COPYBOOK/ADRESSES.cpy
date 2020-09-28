      ***********************************************************
      * Copybook name: ADRESSES
      * Original author: David Stagowski
      *
      * Description: A copybook for Address Output file
      *
      *    This is used in the PSAP program.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01  SUPP-ADDRESS.
           05 ADDRESS-TYPE      PIC X(01) VALUE SPACES.
              88 ORDER-ADDRESS           VALUE '1'.
              88 SCHED-ADDRESS           VALUE '2'.
              88 REMIT-ADDRESS           VALUE '3'.
           05 ADDRESS-1         PIC X(15) VALUE SPACES.
           05 ADDRESS-2         PIC X(15) VALUE SPACES.
           05 ADDRESS-3         PIC X(15) VALUE SPACES.
           05 CITY              PIC X(15) VALUE SPACES.
           05 ADDR-STATE        PIC X(02) VALUE SPACES.
           05 ZIP-CODE          PIC X(05) VALUE SPACES.
