#!/bin/sh
################################################################
# Just run the job.
JOBS_CMD="zos-jobs" # zos-jobs

echo "Run the FP2 app"
zowe ${JOBS_CMD} submit local-file ../JCL/RUNJOB.jcl
