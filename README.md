# This is my code for the ECBAP Final Module.

There are two versions of it because I downloaded an old version of the specifications and wrote Version 1 based on that.

Then, I noticed there was an updated document and saw the specifications had changed and wrote Version 2.

While the end result is almost the same, the method used is different between the two.

## Version 1

The driver program is responsible for the input file and a "good/valid" output file.

For each record it reads it will call the 4 sub-modules to:
- Validate its area of responsibility of the input file.
- If all 4 modules report back that the record is valid, call each module again to write the output file for its area of responsibility. It will then write the "good/valid" output file.

At the end of the run, call each module to close its output file.

Each of the 4 Sub-Modules is responsible for:
- Opening the output file for its area of responsibility.
- Validating its section of the input file.
- Report back to driver program if data is Valid/Invalid.
- Write the output file.
- Close the output file.


## Version 2

The driver program is responsible for everything except validation of data.

It will:
- Open all input/output files
- Read input
- Call each Sub-Module to validate the data
- If all data is valid:
  - Write the individual output files
  - Write a copy of the record to the "good" output file
- Else
  - Create and write an "error" record and write that.


### Differences in output

There is a small difference between the output of the two versions but that is because the second version added more validation rules.

So even though the input files are the same,
the second version produces less "Good/Valid" records.
