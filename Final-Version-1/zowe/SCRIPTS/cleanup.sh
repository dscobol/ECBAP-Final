#!/bin/sh

HLC="Z81187"
PROJECT='FP1'
FILES_CMD="zos-files" # files
JOBS_CMD="zos-jobs" # zos-jobs

echo "Deleting data sets for FP1 app.."
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.ZIPFILE -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.PSAP -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.CBL -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.CPY -f
zowe ${FILES_CMD} delete data-set ${HLC}.${PROJECT}.LOAD -f
