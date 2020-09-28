# The structure of this directory
```
├───code
├───data
└───zowe
    ├───COBOL
    ├───COPYBOOK
    ├───JCL
    ├───RESOURCES
    └───SCRIPTS
```

In other projects, I "declared" GnuCOBOL and ZOS sections. This project separates them differently.

For this, the code in the "code" directory was written for GnuCOBOL and will run in that environment.

The code was then copied to the zowe/COBOL directory and modified to run on ZOS.