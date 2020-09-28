      ***********************************************************
      * Copybook name: ADDRIN
      * Original author: David Stagowski
      *
      * Description: A copybook used in the ADDRCALL program.
      *
      *    This is a shortened version of the PSAP copybook.
      *    This is only the SUPP-ADDRESSES part.
      *
      * Maintenence Log
      * Date       Author        Maintenance Requirement
      * ---------- ------------  --------------------------------
      * 2020-08-22 dastagg       Created for ECBAP Final Project
      *
      **********************************************************
       01 SUPP-ADDRESSES.
          05 SUPP-ADDRESS OCCURS 3 TIMES INDEXED BY ADDR-IDX.
              10 ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                 88 ORDER-ADDRESS           VALUE '1'.
                 88 SCHED-ADDRESS           VALUE '2'.
                 88 REMIT-ADDRESS           VALUE '3'.
              10 ADDRESS-1         PIC X(15) VALUE SPACES.
              10 ADDRESS-2         PIC X(15) VALUE SPACES.
              10 ADDRESS-3         PIC X(15) VALUE SPACES.
              10 CITY              PIC X(15) VALUE SPACES.
              10 ADDR-STATE        PIC X(02) VALUE SPACES.
              10 ZIP-CODE          PIC 9(10) VALUE ZERO.
