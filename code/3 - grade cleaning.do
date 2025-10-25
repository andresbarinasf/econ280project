* this file cleans the grading data

clear

*global path "/Users/`c(username)'/Dropbox/ChatGPT/REPLICATION PACKAGE FINAL"

global raw "$path/raw"
global intermediate "$path/intermediate"
global outfolder "$path/output"
adopath ++ "$path/ado" // binscatter2 allows color customization, thanks Hemanshu Kumar from Statalist

set seed 6000
 
cap ssc install outreg

set scheme s1color
cap noisily set linesize 255
cap graph set window fontface "P052-Roman"

local green "12 164 124"
local blue "4 150 255"
local pink "255 100 210"
local red "255 80 115"
local black "76 87 96"


* loading grading sheet
*insheet using "/Users/shakkednoy/Dropbox/ChatGPT/Analysis Final/raw/grading_long_complete.csv"


*** test
insheet using "$raw/grading_long_complete.csv", clear names


rename prolific_pid pid_hash

merge m:1 pid_hash using "/Users/`c(username)'/Dropbox/ChatGPT/Analysis/intermediate/anonymization.dta", keep(master match)

	drop if _merge!=3 & !(pid_hash=="chatgpt" | pid_hash=="ChatGPT")
	drop _merge
	drop prolific_pid

rename pid_hash prolific_pid

*** end test


drop if grader_id=="043a718774c572bd8a25adbeb1bfcd5c0256ae11cecf9f9c3f925d0e52beaf89" // this is one of us test-running the grading survey


drop if retry==1 // grades for respondents who could replace their first response with a ChatGPT-edited version, not used in main analyses

gen dategraded = date(startdate,"YMD###")

* excluding graders who completed the grading survey after doing the task survey
preserve

	use "$intermediate/datetasked.dta", clear
	sort grader_id datetasked
	quietly by grader_id: gen dup = cond(_N==1,0,_n) // restricting to earliest task completion
	drop if dup>1
	drop dup
	
	save "$intermediate/datetasked_unique.dta", replace
restore

merge m:1 grader_id using "$intermediate/datetasked_unique.dta", keep(master match) nogen // merging on task completion dates

drop if dategraded>datetasked & (dategraded!=. & datetasked!=.) // dropping graders who graded after they tasked

drop if overall==.

* for cross-grader validation (i.e. checking how correlated graders are), keeping a list of human grades
preserve

	keep if prolific_pid!="chatGPT" & prolific_pid!="ChatGPT" // dropping ChatGPT-generated-essays
	sort prolific_pid essaynum
	
	egen essay_id = group(prolific_pid essaynum) // note prolific_pid is the ID of the person who wrote the essay
	
	keep essay_id overall grader_id occupation
	
	egen gradermean = mean(overall), by(grader_id)
	
	save "$intermediate/gradervalidation.dta", replace
restore

gen grader_id_short = substr(grader_id,1,15) // easier for merging

save "$intermediate/gradedata_predropping.dta", replace

* cross-grader correlation: create a dataset where an observation is essay-grader1-grader2 for all pairwise combinations per essay

use "$intermediate/gradervalidation.dta", clear

gen grader_id_short = substr(grader_id,1,15)

* creating list of grader ids, which enables a loop later
preserve
	
	sort grader_id
	
	quietly by grader_id: gen dup = cond(_N==1,0,_n)
	drop if dup>1
	
	keep grader_id_short
	
	local maxid = _N
	
	local graderlist ""
	forvalues n = 1/`maxid' {
		local temp = grader_id_short[`n']
		local graderlist "`graderlist' `temp'" // local containing a list of grader IDs, for the aforementioned loop
	}
	
restore
	
sort essay_id grader_id

quietly by essay_id: gen dup = cond(_N==1,0,_n)
egen maxdup = max(dup), by(essay_id) // how many grades for this essay are there?
		
quietly su maxdup
local max = r(max)
	
forvalues n = 1/`max' {
	preserve
		keep if dup==`n'
		rename grader_id_short grader_id_short`n' // saving the nth grade for each essay
		save "$intermediate/gradevalidation_merge`n'.dta", replace
	restore
}


local x = 0
forvalues n = 1/`max' { // looping over each dataset of nth grades
	
	forvalues m = `n'/`max' { // matching 1st grade to 2nd, 3rd, 4th, 5th, matching 2nd grade to 3rd, 4th, 5th, matching 3rd grade to 4th, 5th, etc...
		
		if `m'!=`n' & `n'!=`max' {
			
			local x = `x'+1
			
			use "$intermediate/gradevalidation_merge`n'.dta", clear
			
			rename overall overall1
			rename grader_id_short`n' grader_id1
			
			merge m:1 essay_id using "$intermediate/gradevalidation_merge`m'.dta", keep(master match)
			keep if _merge==3
			drop _merge
			
			rename overall overall2
			rename grader_id_short`m' grader_id2
			
			keep overall1 overall2 essay_id grader_id1 grader_id2
			
			save "$intermediate/gradeval_temp`x'.dta", replace
		
		}
		
	}
	
}

clear
di `x'
forvalues n = 1/`x' {
	di `n'
	append using "$intermediate/gradeval_temp`n'.dta"
}

gen gradercorr = .
local redlist ""
* assessing each grader: calculating this grader's average correlation with other graders
foreach id in `graderlist' {
	di "`id'"
	
	quietly su overall2 if grader_id1=="`id'" | grader_id2=="`id'"
	if r(N)>1 reg overall2 overall1 if grader_id1=="`id'" | grader_id2=="`id'"
	
	local corr_`id' = _b[overall1]
		di `corr_`id''
	
	
	if r(N)>1 { // calculating the relevant correlation
		gen tempoverall2 = overall2 if grader_id2=="`id'"
		replace tempoverall2 = overall1 if grader_id1=="`id'"
		
		gen tempoverall1 = overall2 if grader_id1=="`id'"
		replace tempoverall1 = overall1 if grader_id2=="`id'"
		
		reg tempoverall2 tempoverall1
		
		local corr_`id' = _b[tempoverall1]
		di `corr_`id''
		
		drop tempoverall2 tempoverall1
	}
	if `corr_`id''<0.1 local redlist "`redlist' `id'" // if this grader is basically uncorrelated with other graders, dropping them
}


* intergrader reliability graph, excluding bad graders
preserve

	foreach bad_id in `redlist' {
		drop if grader_id1=="`bad_id'" | grader_id2=="`bad_id'" // dropping pairs associated with bad graders
	}

	reg overall2 overall1
	local beta = round(_b[overall1],0.001)
	local se = round(_se[overall1],0.001)

	binscatter2 overall2 overall1, xtitle("First Grader", size(medlarge)) ytitle("Second Grader", size(medlarge)) ///
	 text(5 2 "Slope: `beta' (SE `se')", placement(e) size(large)) lcolors(`""`green'""') mcolors(`""`green'""') yscale(range(1 7)) ylabel(1(1)7, angle(0)) ///
	 xscale(range(1 7)) xlabel(1(1)7)
	graph export "$outfolder/app_sc_grades_crossgrader.pdf", replace

restore

* intergrader reliability graph, including bad graders
preserve

	reg overall2 overall1
	local beta = round(_b[overall1],0.001)
	local se = round(_se[overall1],0.001)

	binscatter2 overall2 overall1, xtitle("First Grader", size(medlarge)) ytitle("Second Grader", size(medlarge)) ///
	 text(5 2 "Slope: `beta' (SE `se')", placement(e) size(large)) lcolors(`""`green'""') mcolors(`""`green'""') yscale(range(1 7)) ylabel(1(1)7, angle(0)) ///
	 xscale(range(1 7)) xlabel(1(1)7)
	graph export "$outfolder/app_sc_grades_crossgrader_yesbad.pdf", replace

restore

* saving a list of the bad graders and their occupations
clear
set obs 1

gen id = ""

local n = 0
foreach bad_id in `redlist' {
	local n = `n'+1
	local m = _N+1
	set obs `m'
	replace id = "`bad_id'" if _n==`n'
}

rename id grader_id_short
merge 1:m grader_id_short using "$intermediate/gradedata_predropping.dta", keep(master match) keepusing(occupation) nogen // just merging on their occupations

sort grader_id
quietly by grader_id: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop if grader_id==""

keep grader_id grader_id_short occupation

export excel "$outfolder/badgraders.xlsx", replace
save "$intermediate/badgraders.dta", replace // for merging onto the grades dataset in the analysis file later


*** creating datasets for merging onto main analysis file

use "$intermediate/gradedata_predropping.dta", clear

* mean graders including bad graders, for merging
preserve

	gen ngraders = 1

	collapse (mean) overall writing content originality (sum) ngraders, by(prolific_pid essaynum)
	
	egen min_ngraders = min(ngraders), by(prolific_pid)
	replace ngraders = min_ngraders
	drop min_ngraders
	
	reshape wide overall writing content originality, i(prolific_pid) j(essaynum) string
	
	save "$intermediate/meangrades_yesbad.dta", replace
	
restore


foreach bad_id in `redlist' {
	drop if grader_id_short=="`bad_id'" 
}


* full list of human grades
preserve

	drop if prolific_pid=="chatGPT" | prolific_pid=="ChatGPT"
	save "$intermediate/humangrades.dta", replace

restore

* mean grades for merging
preserve

	gen ngraders = 1

	collapse (mean) overall writing content originality (sum) ngraders, by(prolific_pid essaynum)
	
	egen min_ngraders = min(ngraders), by(prolific_pid)
	replace ngraders = min_ngraders
	drop min_ngraders
	
	reshape wide overall writing content originality, i(prolific_pid) j(essaynum) string
	
	save "$intermediate/meangrades.dta", replace
	
restore

* 469, 3.957

