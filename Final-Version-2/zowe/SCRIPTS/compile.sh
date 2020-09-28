#!/bin/sh
################################################################
# Just compile the programs.
JOBS_CMD="zos-jobs" # zos-jobs

echo "Compile all the FP2 programs..."
zowe ${JOBS_CMD} submit local-file ../JCL/COMPALL.jcl
