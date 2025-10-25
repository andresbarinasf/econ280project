/* This dataset cleans the raw survey dataset and prepares it for analysis. */

clear


*global path "/Users/`c(username)'/Dropbox/ChatGPT/REPLICATION PACKAGE FINAL"

global raw "$path/raw"
global intermediate "$path/intermediate"
global outfolder "$path/output"

set scheme s1color
cap noisily set linesize 255
cap graph set window fontface default

insheet using "$raw/fullsurvey.csv", names clear

destring *, replace


* attrition statistics (this is attrition conditional on completing the first task, which is when the control and treatment conditions diverge)
tab finished if treatment==0 & consent==1 & task_realism!=.
tab finished if treatment==1 & consent==1 & task_realism!=.


save "$intermediate/fullsample_leebounds.dta", replace // saving full sample of survey-starters for selective attrition analysis

drop if finished==0
drop if consent==2 | consent_exact==2 // keeping only people who consented and finished the survey

preserve

	* this is used to identify people who made it into our grading surveys after completing the tasks
	* it is used later to drop those graders

	gen datetasked = date(startdate,"YMD###")
	
	keep prolific_pid datetasked startdate
	
	rename prolific_pid grader_id
	rename startdate task_date
	
	save "$intermediate/datetasked.dta", replace 

restore



************************** cleaning various variables **************************

* calculating final word counts for task entries, from the snapshots of word counts each minute. This works a bit better than using Stata's wordcount function on the transcripts themselves
foreach suff in a b {
	gen finalword`suff' = word_count_`suff'_0
	forvalues n = 1/90 {
		replace finalword`suff' = word_count_`suff'_`n' if word_count_`suff'_`n'>finalword`suff' & word_count_`suff'_`n'!=. // variables used for the plots of cumulative word counts at a point in time
	}
}

* the mapping between 'your final word count on the first task' and 'your final word count on occupational task A' depends whether you saw occupational task A first or second (this is randomized)
* we'll see this kind of replacement many times
gen finalword1 = finalworda
replace finalword1 = finalwordb if a_first==0 

gen finalword2 = finalwordb
replace finalword2 = finalworda if a_first==0

gen chatgpt_aware = strpos(awareness,"3")>0 if awareness!="" & awareness!="." // dummy for saying 'is aware of ChatGPT' pre-treatment
gen chatgpt_use = strpos(usage,"3")>0 if usage!="" & usage!="." // dummy for saying 'have used ChatGPT before' pre-treatment

********************************* dropping responses ***************************

/* in this section we drop

- People who submitted identifiably useless responses on the first task - responses plagiarized from the web,
empty responses, or responses that didn't take the tasks seriously [e.g. wrote a satirical scifi story]. These 
were identified manually by us, in a Google Sheet that we piped task responses into, to pass on to graders. We could not
see treatment status in this sheet and any differential effect by treatment group should show up in our selective attrition analyses.

- People who completed the grading survey first or completed the survey for a second or third time somehow

*/

* these are all identified manually by looking at the first-essay responses - these are responses so identifiably useless we don't even send them to graders
gen plagiarism = prolific_pid=="484d7e16cdcac3774e045744722ecd4610f01d40069b05c9affb00499e8f3731"
gen empty = prolific_pid=="727c882101c4438387011b92398c999d46e76b0f3c59bb45b17e6ea1c64e4561" | prolific_pid=="8b0833b266be60de6278a28d4ae8052236ecac85b248710ac6efaf57420ee11a"
gen gradedfirst = prolific_pid=="174207d4312d46b6b908d28dc34c9f6dbe62d22844b6b9b8b875ac4de272b275" | prolific_pid=="f9a1e36ff890a83c3dc1634403f02e0d250e66cf08218f197b7127f2993892ec"

gen multiplesurveys = .
replace multiplesurveys = 1 if prolific_pid=="bee7a8f1c3bdc0c2a6e6303ab33a8253a69894569927a633222bd5e3a6d244ff" // this is someone who took the survey multiple times

* dropping bad responses and duplicate IDs
sort prolific_pid startdate
quietly by prolific_pid: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup
drop if plagiarism==1 | empty==1 | gradedfirst==1 | multiplesurveys==1

* people getting quality-checked out, again this is based on manual examination of the first-essay responses in the Google Sheet we pipe responses into
preserve 

	local n = 0 
	
	foreach sheet in grading grading_retry {
		
		local n = `n'+1
		
		import excel using "$raw/`sheet'.xlsx", clear firstrow
		 
		recode qcheck (3 = 1) // this was a special code used in the arm where we allowed people to resubmit a ChatGPT-edited version of their first response
		
		egen minqcheck = min(qcheck), by(prolific_pid)
		egen maxqcheck = max(qcheck), by(prolific_pid)

		sort prolific_pid
		quietly by prolific_pid: gen dup = cond(_N==1,0,_n)
		drop if dup>1

		keep prolific_pid maxqcheck minqcheck
		
		tab minqcheck
		tab maxqcheck
		
		drop if prolific_pid=="" | prolific_pid=="1" | prolific_pid=="2" | prolific_pid=="3" | prolific_pid=="4" // these were test responses to the survey that we submitted
		 
		 save "$intermediate/qcheck_idlist_`n'.dta", replace
		
	 
	}
	 
	 clear
	 
	 use "$intermediate/qcheck_idlist_1.dta", clear
	 append using "$intermediate/qcheck_idlist_2.dta"
	 
	 sort prolific_pid
	 quietly by prolific_pid: gen dup = cond(_N==1,0,_n)
	 drop if dup>1 // repeatedly clearing duplicates to enable merging
	 
	 save "$intermediate/qcheck_idlist.dta", replace

restore

merge m:1 prolific_pid using "$intermediate/qcheck_idlist.dta", keep(master match) nogen
tab treatment if (maxqcheck>1 & maxqcheck!=.) | minqcheck<1 // no clear selective pattern
drop if (maxqcheck>1 & maxqcheck!=.) | minqcheck<1

* dropping people who did the grading survey before this
gen datetasked = date(startdate,"YMD###")

preserve

	insheet using "$raw/grading_long_complete.csv", clear names
	
	drop prolific_pid
	rename grader_id prolific_pid
	
	gen dategraded = date(startdate,"YMD###")
	
	keep prolific_pid dategraded
	sort prolific_pid dategraded
	
	quietly by prolific_pid: gen dup = cond(_N==1,0,_n)
	drop if dup>1
	drop dup
	
	
	save "$intermediate/gradedates.dta", replace
	
	
restore

merge m:1 prolific_pid using "$intermediate/gradedates.dta", keep(master match) 

drop if datetasked>dategraded & (datetasked!=. & dategraded!=.)

********************************************************************************

* balance checks, tabulating variables, generating more variables
tab outcome
tab treatment
tab incentive_arm
tab techdiff

tab chatgpt_use if treatment==1
tab usedgpt if treatment==1

forvalues n = 1/2 {
	rename time`n'_4 brainstorming`n'
	rename time`n'_5 roughdraft`n'
	rename time`n'_6 editing`n'
	rename time`n'_7 other`n'
	
}

forvalues n = 1/2 { // self-reported time variable
	gen time`n' = brainstorming`n'+roughdraft`n'+editing`n'+other`n'
}
	
su time1
su time2 if treatment==1
su time2 if treatment==1 & usedgpt==1
su time2 if treatment==0


* manual coding for people who ran into technical difficulties, based on what they put in the open textbox for 'technical difficulties'
gen techdiff_yes = 0
replace techdiff_yes = 1 if prolific_pid=="011a99143c5e6b94624cd1848bcbff4cca4a7e422480028940af3a4c4523249d"
replace techdiff_yes = 1 if prolific_pid=="7dd84051685ca8f70b301bd03c8dc2d40c6acfcb326fe3440597876df5dce587"
replace techdiff_yes = 1 if prolific_pid=="e5d99c65519cb12311717c8b2ce24f0e1357700deccf67a64bf6b20af8e01f4e"
replace techdiff_yes = 1 if prolific_pid=="189869f03a2064be9d3304dd85f89700b99d41905b7f51a889fbf8967c68ad16"
replace techdiff_yes = 1 if prolific_pid=="564470411114823121ef0812dfca2537bb490d290b6dc436fa73b0d19945470a"
replace techdiff_yes = 1 if prolific_pid=="46c82db47b00d5927e08bb5659da3a34a157ce90f8dc35e45a66d9593d2e08e8"
replace techdiff_yes = 1 if prolific_pid=="49de82ca4184d86d999698d04ae0013ad3be11aa7d93f69e9082d31f3e34a6de"
replace techdiff_yes = 1 if prolific_pid=="e60e565dd80e71e9d6545a5c161e60cde56761db3e6f0dc62d2b02928564b0fc"
replace techdiff_yes = 1 if prolific_pid=="5877aaccba529f43e4df3adf9ed1cd6977e61a920d951c8c8688b9dfe714817d"
replace techdiff_yes = 1 if prolific_pid=="a1298cacd91ac78832a0d2ecfb339bbd4cdec4ab1fc2af8e2f43cb63841cb1ce"
replace techdiff_yes = 1 if prolific_pid=="647545e53b7f86f5aea379a16ac110c99bde492108ca973bbf3d0d8559c014f9"
replace techdiff_yes = 1 if prolific_pid=="4c4a070d3add9c77594823c9fc983cb58fc6478b75bb8edc08f40c14561fbb06"
replace techdiff_yes = 1 if prolific_pid=="65ec527ed62610cc0a38d7eed91e689ee6217133d7a4f5e6742053ea89f40c76"
replace techdiff_yes = 1 if prolific_pid=="d56a0645ee2103e01f794ff09d7d2d7bbd53d21d65866a0d80d05f66352f424e"
replace techdiff_yes = 1 if prolific_pid=="9bc0dfe8934cd7d0b50ca8b0cc3da797b2766cc9cccb5484f6eea5c47099491a"
replace techdiff_yes = 1 if prolific_pid=="f42e0c7cfab2f5c56c238b39e4cfcb86513a9f2b0bf66c33d827a154f52ef610"
replace techdiff_yes = 1 if prolific_pid=="80bdc6e33675137f4bd073efb73c916f23e2c1fa4ba0fc1b80d97f0ed847e0fb"
replace techdiff_yes = 1 if prolific_pid=="88f62446dd1c29b4b76fd4c6fb8e5d09882eca2f279bb273b19d6981a7e36cb3"
replace techdiff_yes = 1 if prolific_pid=="6571c0e4d3ceed33fbdfe88b6a3587b209abe7f04a6a0b4cff32c10eb825455b"
replace techdiff_yes = 1 if prolific_pid=="7783416748cb697a3820dab267a7f8a5505655b811436e7dd4c24ddc7af0aa29"
replace techdiff_yes = 1 if prolific_pid=="f42e0c7cfab2f5c56c238b39e4cfcb86513a9f2b0bf66c33d827a154f52ef610"
replace techdiff_yes = 1 if prolific_pid=="5877aaccba529f43e4df3adf9ed1cd6977e61a920d951c8c8688b9dfe714817d"

sort startdate


foreach prefix in word_count modified_word_count { // doing the a/b pre/post mapping
	forvalues n = 1/45 {
		
		gen `prefix'_pre_`n' = .
		gen `prefix'_post_`n' = .
		
		replace `prefix'_pre_`n' = `prefix'_a_`n' if a_first==1
		replace `prefix'_pre_`n' = `prefix'_b_`n' if a_first==0
		
		replace `prefix'_post_`n' = `prefix'_a_`n' if a_first==0
		replace `prefix'_post_`n' = `prefix'_b_`n' if a_first==1
	}
}
gen ivar = _n

* indicator for being active in a given minute: definition is 'modified at least 3 words in that minute'
foreach suff in a b {
	
	gen active_`suff' = 0 if finalword`suff'!=.
	
	forvalues n = 1/90 {
		
		replace active_`suff' = active_`suff'+1 if modified_word_count_`suff'_`n'>3 & modified_word_count_`suff'_`n'!=.
		
	}
	
}

* a/b to 1/2 recoding
gen active1 = active_a
replace active1 = active_b if a_first==0

gen active2 = active_b
replace active2 = active_a if a_first==0

gen timepage1 = task_a_timespent_pagesubmit
replace timepage1 = task_b_timespent_pagesubmit if a_first==0

gen timepage2 = task_b_timespent_pagesubmit
replace timepage2 = task_a_timespent_pagesubmit if a_first==1

* converting Qualtrics's automatic time spent recording from seconds to minutes
replace timepage1 = timepage1/60
replace timepage2 = timepage2/60

rename empstat empstat_temp

* merging on employment survey data
merge m:1 prolific_pid using "$intermediate/empstat.dta", keep(master match) nogen force

replace empstat = empstat_temp if empstat==.
drop empstat_temp

cap drop _merge

save "$intermediate/analysis_clean.dta", replace




