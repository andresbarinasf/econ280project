
* this file just cleans the employment data (collected in a supplementary followup survey) from csv into Stata format for merging onto the main survey dataset

clear

*global path "/Users/`c(username)'/Dropbox/ChatGPT/REPLICATION PACKAGE FINAL"

global raw "$path/raw"
global intermediate "$path/intermediate"
global outfolder "$path/output"

set scheme s1color
cap noisily set linesize 255
cap graph set window fontface default


insheet using "$raw/employmentdata.csv", names clear


destring *, replace

label define empstat 1 "Employed fulltime" 2 "Employed part-time" 3 "Unemployed" 4 "NILF"
label val empstat empstat 

save "$intermediate/empstat.dta", replace
