//Z81187A JOB ,NOTIFY=&SYSUID,
// MSGCLASS=H,MSGLEVEL=(1,1),REGION=144M
//*
//* THE FOLLOWING HLQ SYMBOLIC MUST CONTAIN THE HIGH LEVEL
//* QUALIFIER UNDER WHICH THE &PROJECT. DATASETS MAY RESIDE.
//*
//    SET HLQ='Z81187'                     *TSO USER ID
//    SET PROJECT='FP1'                    *PROJECT ID
//*************************
//* CLEAN UP DATASETS
//*************************
//DELETE   EXEC PGM=IEFBR14
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//DD2      DD DSN=&HLQ..&PROJECT..LOAD,
//            DISP=(MOD,DELETE,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(0))
//DD3      DD DSN=&HLQ..&PROJECT..CBL,
//            DISP=(MOD,DELETE,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(0))
//DD4      DD DSN=&HLQ..&PROJECT..CPY,
//            DISP=(MOD,DELETE,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(0))
//DD5      DD DSN=&HLQ..&PROJECT..PSAP,
//            DISP=(MOD,DELETE,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(0))
//DD6      DD DSN=&HLQ..&PROJECT..ZIPFILE,
//            DISP=(MOD,DELETE,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(0))
//*
//*************************
//* ALLOCATE DATASETS
//*************************
//ALLOCAT EXEC PGM=IEFBR14,COND=(8,LT)
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
/*
//DD2      DD DSN=&HLQ..&PROJECT..LOAD,
//            DISP=(NEW,CATLG),
//            DCB=(BLKSIZE=0,LRECL=80,RECFM=U,DSORG=PO),
//            SPACE=(TRK,(5,2),RLSE),
//            DSNTYPE=LIBRARY
/*
//DD3      DD DSN=&HLQ..&PROJECT..CBL,
//            DISP=(NEW,CATLG),
//            DCB=(BLKSIZE=0,LRECL=80,RECFM=FB,DSORG=PO),
//            SPACE=(TRK,(5,2),RLSE),
//            DSNTYPE=LIBRARY
/*
//DD4      DD DSN=&HLQ..&PROJECT..CPY,
//            DISP=(NEW,CATLG),
//            DCB=(BLKSIZE=0,LRECL=80,RECFM=FB,DSORG=PO),
//            SPACE=(TRK,(5,2),RLSE),
//            DSNTYPE=LIBRARY
/*
//DD5      DD DSN=&HLQ..&PROJECT..PSAP,
//            DCB=(BLKSIZE=0,LRECL=473,RECFM=FB,DSORG=PS),
//            DISP=(NEW,CATLG),
//            SPACE=(TRK,(5,2),RLSE)
/*
//DD6      DD DSN=&HLQ..&PROJECT..ZIPFILE,
//            DCB=(BLKSIZE=0,LRECL=33,RECFM=FB,DSORG=PS),
//            DISP=(NEW,CATLG),
//            SPACE=(TRK,(5,2),RLSE)
/*
//*
