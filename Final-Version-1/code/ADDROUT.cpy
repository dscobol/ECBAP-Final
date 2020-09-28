      ***********************************************************
      * Copybook name: ADDROUT
      * Original author: David Stagowski
      *
      * Description: A copybook for Address Output file
      *
      *    This is used in the ADDRCALL program.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01  :tag:-SUPP-ADDRESS.
           05 :tag:-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
              88 :tag:-ORDER-ADDRESS           VALUE '1'.
              88 :tag:-SCHED-ADDRESS           VALUE '2'.
              88 :tag:-REMIT-ADDRESS           VALUE '3'.
           05 :tag:-ADDRESS-1         PIC X(15) VALUE SPACES.
           05 :tag:-ADDRESS-2         PIC X(15) VALUE SPACES.
           05 :tag:-ADDRESS-3         PIC X(15) VALUE SPACES.
           05 :tag:-CITY              PIC X(15) VALUE SPACES.
           05 :tag:-ADDR-STATE        PIC X(02) VALUE SPACES.
           05 :tag:-ZIP-CODE          PIC X(05) VALUE SPACES.
