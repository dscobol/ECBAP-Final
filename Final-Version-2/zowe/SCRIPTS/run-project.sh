#!/bin/sh

HLQ="Z81187"
PROJECT='FP2'
FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs

echo "Submitted job to allocate data sets.."
zowe ${JOBS_CMD} submit local-file ../JCL/ALLOCATE.jcl
sleep 5s

echo "Copy my app to the created PDS.."
zowe ${FILES_CMD} upload dir-to-pds ../COBOL ${HLQ}.${PROJECT}.CBL
zowe ${FILES_CMD} upload dir-to-pds ../COPYBOOK ${HLQ}.${PROJECT}.CPY
zowe ${FILES_CMD} upload file-to-data-set ../RESOURCES/psap.dat.txt ${HLQ}.${PROJECT}.PSAP
zowe ${FILES_CMD} upload file-to-data-set ../RESOURCES/state-address-zip.dat.txt ${HLQ}.${PROJECT}.ZIPFILE
sleep 3s

echo "Compile and Run the FP2 app..."
zowe ${JOBS_CMD} submit local-file ../JCL/RUNALL.jcl
