

clear

*global path "/Users/`c(username)'/Dropbox/ChatGPT/REPLICATION PACKAGE FINAL"

global raw "$path/raw"
global intermediate "$path/intermediate"
global outfolder "$path/output"

set scheme s1color
cap noisily set linesize 255
cap graph set window fontface default


insheet using "$raw/twomonth_followup.csv", names clear 

merge m:1 prolific_pid using "$intermediate/analysis_clean.dta", keep(master match) keepusing(treatment chatgpt_use) // some people not merging, check this out


foreach var in automation_specific automation_good automation_future {
	reg `var' treatment, robust
}

tab chatgpt_use
tab usedgpt_job
recode usedgpt_job (2 = 0)
recode usedgpt_ever (2 = 0)

foreach var in usedgpt_ever usedgpt_job {
	reg `var' treatment, robust
}

ttest usedgpt_job, by(treatment)
