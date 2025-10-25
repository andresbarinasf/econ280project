

clear

*global path "/Users/`c(username)'/Dropbox/ChatGPT/REPLICATION PACKAGE FINAL"

global raw "$path/raw"
global intermediate "$path/intermediate"
global outfolder "$path/output"

set scheme s1color
cap noisily set linesize 255
cap graph set window fontface default


insheet using "$raw/followupsurvey.csv", names clear

merge m:1 prolific_pid using "$intermediate/analysis_clean.dta", keep(master match) keepusing(treatment chatgpt_use) // merging on treatment status and ChatGPT usage from the main survey
* chatgpt_use is an indicator for whether the respondent had used ChatGPT prior to participating in our experiment

tab usedgpt_job if treatment==1
tab usedgpt_job if treatment==0



tab usedgpt_job if treatment==1 & chatgpt_use==0
tab usedgpt_job if treatment==0 & chatgpt_use==0

su usefulness_job

label define whynot 1 "Not useful" 2 "Not allowed" 3 "Don't know of it" 4 "Usually down" 5 "Other"
label val usedgpt_whynot_job whynot

tab usedgpt_whynot_job

tab why_notuseful

recode usedgpt_job (2 = 0)

ttest usedgpt_job, by(treatment)



ttest usedgpt_job if chatgpt_use==0, by(treatment)

* looking if there is any difference in job satisfaction
reg jobsatisfacton_job treatment, robust
ivreg jobsatisfacton_job (usedgpt_job = treatment)
