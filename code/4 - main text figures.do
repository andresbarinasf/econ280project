
* this file generates all of the main analyses described in the text

clear


*global path "/Users/`c(username)'/Dropbox/ChatGPT/REPLICATION PACKAGE FINAL"

global raw "$path/raw"
global intermediate "$path/intermediate"
global outfolder "$path/output"
adopath ++ "$path/ado" // binscatter2 allows color customization, thanks Hemanshu Kumar from Statalist

set seed 6000
 
set scheme s1color
cap noisily set linesize 255
graph set window fontface "Times New Roman"


* cap ssc install leebounds

local green "12 164 124"
local blue "4 150 255"
local pink "255 100 210"
local red "255 80 115"
local black "76 87 96"

local darkgreen "7 75 57"
local lightgreen "46 239 188"


****** preparing dataset at the participant-grader level, for the regressions with grades as the outcome variable 

insheet using "$raw/grading_wide_complete.csv", clear names // loading the grades dataset in wide format

drop if grader_id=="043a718774c572bd8a25adbeb1bfcd5c0256ae11cecf9f9c3f925d0e52beaf89" // this is one of us test-running the grading survey

drop if retry_a==1 | retry_b==1 // dropping grades for tasks where the respondents can edit and resubmit their first response using GPT

cap drop _merge

merge m:1 prolific_pid using "$intermediate/analysis_clean.dta", keep(master match) // merging on participant information
export delimited "$intermediate/for_gptgrades_analysis.csv", replace
keep if _merge==3
drop _merge

* mapping from a/b to first/second using the order in which the respondent saw the a/b tasks
foreach var in overall writing content originality {
	gen `var'_1 = `var'_b
	replace `var'_1 = `var'_a if a_first==1
	
	gen `var'_2 = `var'_a
	replace `var'_2 = `var'_b if a_first==1
}

foreach var in writing content originality {
	gen `var'_diff = `var'_2-`var'_1
}

forvalues n = 1/2 { // how much respondents earn
		gen earnings`n' = overall_`n'
		replace earnings`n' = earnings`n'+3 if incentive_arm=="convex" & overall_`n'==6 | overall_`n'==7
	}
	
	
rename overall_2 overall2
rename overall_1 overall1


foreach var in time finalword active overall jobsatisfaction selfefficacy {
	gen `var'_diff = `var'2-`var'1 // change in the outcome from pre-treatment to post-treatment 
}


gen grader_id_short = substr(grader_id,1,15)
merge m:1 grader_id_short using "$intermediate/badgraders.dta", keep(master match) // identifying bad graders

* dropping bad graders, for the main analysis
preserve

	drop if _merge==3
	drop _merge


	encode grader_id, gen(n_grader_id) // grader fixed effects
	egen occnum = group(occupation) // occupation fixed effects
	egen incentivenum = group(incentive_arm) // incentive-arm fixed effects

	save "$intermediate/gradingresults_clean.dta", replace

restore

drop _merge


encode grader_id, gen(n_grader_id) // grader fixed effects
egen occnum = group(occupation) // occupation fixed effects
egen incentivenum = group(incentive_arm) // incentive-arm fixed effects

save "$intermediate/gradingresults_clean_yesbad.dta", replace

/*******************************************************************************
This section runs the regressions whose results are printed in our diff-in-diff 
plots. See the Supplementary Materials for the full regression specifications.
When grades are the outcome variable, the regressions are at the participant-grader
level. When the outcome variable is something else, they're at the participant level.

There's 3 samples here
-- All participants
-- Only the linear and convex incentive groups
-- Only the exact-time incentive groups

All of the main figures in the paper use the linear and convex incentive groups.
Supplementary results use the exact-time group as well, and the results on subjective
outcomes use the full sample.
*******************************************************************************/

* 3 different sample types: all taskers, only taskers in the 'linear' or 'convex' incentive groups, or only taskers in the exact-time groups
foreach sample in all linconv exact {
	
	if "`sample'"=="all" local restriction ""
	if "`sample'"=="linconv" local restriction `"if incentive_arm!="exact""'
	if "`sample'"=="exact" local restriction `"if incentive_arm=="exact""'
	
	foreach standardized in 1 { // we standardize the regression coefficient to be in terms of pre-treatment SDs of the outcome variable, before printing it on the plot
		
		if `standardized'==0 local suffix ""
		if `standardized'==1 local suffix "_s"
		
		foreach outcome in time overall jobsatisfaction selfefficacy { // looping over outcomes: time taken, overall grades, job satisfaction, self-efficacy
			
			* grade-outcome regressions - at the participant-grader level
			if "`outcome'"=="overall" {
				use "$intermediate/gradingresults_clean.dta", clear
				reg `outcome'_diff treatment i.n_grader_id i.occnum##i.a_first i.incentivenum `restriction', cluster(prolific_pid)
				* note the grader fixed effects and the clustering at the participant level
			}
			
			* non-grade-outcome regressions - at the tasker level
			if "`outcome'"!="overall" {
				
				use "$intermediate/analysis_clean.dta", clear
								
				egen occnum = group(occupation) // occupation fixed effects
				egen incentivenum = group(incentive_arm) // incentive-arm fixed effects

				gen `outcome'_diff = `outcome'2-`outcome'1
				
				reg `outcome'_diff treatment i.occnum##i.a_first i.incentivenum `restriction', robust
								
			}
			
									
			local `outcome'_coef_`sample'`suffix' = string(round(_b[treatment],0.001)) // recording the treatment coefficient and SE
			local `outcome'_se_`sample'`suffix' = string(round(_se[treatment],0.001))
									
			local t = _b[treatment]/_se[treatment] // t-stat, for calculating the p-value of the treatment coefficient
			
			if "`outcome'"=="time" & "`sample'"=="linconv" & `standardized'==0 local ptime = 2*ttail(e(df_r),abs(`t')) // recording relevant p-values to report in the paper
			if "`outcome'"=="overall" & "`sample'"=="linconv" & `standardized'==0 local pgrade1 = 2*ttail(e(df_r),abs(`t'))
			if "`outcome'"=="overall" & "`sample'"=="exact" & `standardized'==0 local pgrade2 = 2*ttail(e(df_r),abs(`t'))
			if "`outcome'"=="jobsatisfaction" & "`sample'"=="all" & `standardized'==0 local pjobsat = 2*ttail(e(df_r),abs(`t'))
			if "`outcome'"=="selfefficacy" & "`sample'"=="all" & `standardized'==0 local pselfefficacy = 2*ttail(e(df_r),abs(`t'))
			
			quietly su `outcome'1 `restriction'
			local `outcome'_mean_`sample' = string(round(r(mean),0.001)) // pre-treatment outcome mean, for reporting in the paper
			
			* standardizing the regression coefficient: dividing both coefficient and SE by standard deviations of the pre-treatment variable
			if `standardized'==1 {
				local `outcome'_coef_`sample'`suffix' = string(round(``outcome'_coef_`sample'`suffix''/r(sd),0.01))
				local `outcome'_se_`sample'`suffix' = string(round(``outcome'_se_`sample'`suffix''/r(sd),0.01))
			}
			
	
			di ``outcome'_coef_`sample'`suffix''
			di ``outcome'_se_`sample'`suffix''
			
		}
	
	}
}

*********** main diff-in-diff figures

foreach loop in noexact yesexact all { // same sample restriction loops
	
	local outcomelist "time overall jobsatisfaction selfefficacy" // same outcome variable list
	
	local meanlist ""
	local sdlist ""
	local reshapelist ""
	foreach var in `outcomelist' {
		local meanlist "`meanlist' `var'1 `var'2"
		local sdlist "`sdlist' sd_`var'1=`var'1 sd_`var'2=`var'2"
		local reshapelist "`reshapelist' `var' se_`var'"
	}
	use "$intermediate/analysis_clean.dta", clear
	
	merge m:1 prolific_pid using "$intermediate/meangrades.dta", keep(master match) nogen
	
	if "`loop'"=="all" { // this is the loop we use for the job satisfaction/self-efficacy results
		forvalues n = 1/2 {
			quietly su jobsatisfaction`n'
			replace jobsatisfaction`n' = (jobsatisfaction`n'-r(mean))/r(sd) // standardizing these variables
			
			quietly su selfefficacy`n'
			replace selfefficacy`n' = (selfefficacy`n'-r(mean))/r(sd)
		}
	}
	
	egen occnum = group(occupation) 
	
	* cleaning mean-grade variables
	gen overall1 = overallA
	replace overall1 = overallB if a_first==0
	gen overall2 = overallB
	replace overall2 = overallA if a_first==0
		
	gen N = 1
	
	local suffix ""
	
	if "`loop'"=="noexact" {
		keep if incentive_arm!="exact"
		local sample "linconv"
	}
	if "`loop'"=="yesexact" {
		keep if incentive_arm=="exact"
		local sample "exact"
	}
	if "`loop'"=="all" {
		local sample "all"
		local coefsuffix "_s"
	}
	
	preserve
		
		* means and standard deviations of pre- and post-treatment variables, by treatment group
		collapse (mean) `meanlist' (sd) `sdlist' (sum) N, by(treatment)
			
		foreach suffix in `meanlist' {
			gen se_`suffix' = sd_`suffix'/sqrt(N)
		}
		
		* reshaping to enable plot
		reshape long `reshapelist', i(treatment) j(jvar)
		
		* top and bottom of confidence intervals on means
		foreach suffix in `outcomelist' {
			gen high_`suffix' = `suffix'+(1.96*se_`suffix')
			gen low_`suffix' = `suffix'-(1.96*se_`suffix')
		}
		
		replace jvar = jvar-0.01 if treatment==1
		replace jvar = jvar+0.01 if treatment==0
		
		foreach var in `outcomelist' {
			
			* setting aesthetic elements of plots
			local clockpos = 7
			local ylabel ""
			
			if "`var'"=="time" {
				local ytitle "Self-Reported Time Spent (Minutes)"
				local coefpos0 "34 1.2"
				local coefpos1 "32.5 1.2"
			}
			if "`var'"=="overall" {
				local ytitle "Mean Grade"
				local coefpos0 "4.9 1.3"
				local coefpos1 "4.8 1.3"
				
				local clockpos = 11
				
				if "`loop'"=="noexact" local ylabel "3.6(0.4)5.2"
			}
			if "`var'"=="jobsatisfaction" {
				local ytitle "Job Satisfaction (1-10)"
				if "`loop'"=="all" local ytitle "Job Satisfaction (SDs)"
				local coefpos0 "7.4 0.75"
				local coefpos1 "7.2 0.75"
				
				if "`loop'"=="all" {
					local coefpos0 "0.32 0.75"
					local coefpos1 "0.27 0.75"
					
					local ylabel `" -0.4 "-.4" -0.2 "-.2" 0 "0" 0.2 ".2" 0.4 ".4" "'
				}
			}
			if "`var'"=="selfefficacy" {
				local ytitle "Self-Efficacy (1-10)"
				if "`loop'"=="all" local ytitle "Self-Efficacy (SDs)"
				local coefpos0 "0.24 0.8"
				local coefpos1 "0.21 0.8"
			}

			* CI bounds to report in print
			local high = string(round(``var'_coef_`sample'_s'+(1.96*``var'_se_`sample'_s'),0.01))
			local low = string(round(``var'_coef_`sample'_s'-(1.96*``var'_se_`sample'_s'),0.01))
			
			foreach digit in low high { // standardizing leading and lagging zeroes in printed coefficients
				if ``digit''<0 {
					if substr("``digit''",2,1)=="." {
						local `digit' = substr("``digit''",2,.)
						local `digit' = "-0"+"``digit''"
					}
				}
				if ``digit''>=0 {
					if substr("``digit''",1,1)=="." {
						local `digit' = "0"+"``digit''"
					}
				}
			}
						
			* formatting printed text
			if ``var'_coef_`sample'_s'<0 {
				local citext "[`high', `low']"
				local newlocal "``var'_coef_`sample'_s'"
				
				
				if substr("``var'_coef_`sample'_s'",2,1)=="." {
					local newlocal = substr("``var'_coef_`sample'_s'",2,.)
					local newlocal = "-0"+"`newlocal'"
				}
			}
	
			if ``var'_coef_`sample'_s'>=0 {
				local citext "[`low', `high']"
				local newlocal "``var'_coef_`sample'_s'"
				
				if substr("``var'_coef_`sample'_s'",1,1)=="." {
					local newlocal = "0"+"``var'_coef_`sample'_s'"
				}
			}
			
			if "`loop'"=="noexact" & ("`var'"=="time" | "`var'"=="overall") export excel "$outfolder/`var'_reproduce.xlsx", replace firstrow(var) // some people asked for the raw data points here
			
			twoway (connected `var' jvar if treatment==1, color("`green'") msymbol(triangle) msize(large)) (rcap high_`var' low_`var' jvar if treatment==1, color("`green'")) ///
				(connected `var' jvar if treatment==0, color(black) msymbol(square_hollow) msize(large) lpattern(dash)) (rcap high_`var' low_`var' jvar if treatment==0, color(black)) ///
				, xscale(range(0.5 2.5)) xlabel(1 "Pre-Treatment" 2 "Post-Treatment", labsize(large)) legend(order(1 "Treated" 3 "Control") region(lstyle(none)) cols(1) ring(0) position(`clockpos') size(large)) ///
				xtitle("") ytitle(`ytitle', size(large)) ylabel(`ylabel',labsize(medlarge) angle(0)) text(`coefpos0' "Treatment Effect:     `newlocal' SDs", size(large) placement(e)) ///
				text(`coefpos1' "                95% CI:  `citext'", size(large) placement(e)) 
			graph export "$outfolder/sc_did_`var'_`loop'.pdf", replace
			
		}
		
	restore
	
}

*** inequality analysis: since at least one of the axes is always grades, we do this at the participant-grader level
foreach yvar in overall2 time2 { // looping over dependent variables
	
	* aesthetic stuff
	if "`yvar'"=="overall2" {
		local ytitle "Grade: Task 2"
		local titlesuff1 "grades"
		local yscaleextra ""
		local ylabelextra ""
	}
	if "`yvar'"=="time2" {
		local ytitle "Time Taken: Task 2"
		local titlesuff1 "_timeY"
		local yscaleextra "yscale(range(10 40))"
		local ylabelextra "10(5)40"
	}
	
	foreach xvar in overall1 { // looping over independent variables
		
		if "`xvar'"=="overall1" {
			local xtitle "Grade: Task 1"
			local titlesuff2 ""
		}
					
		* load dataset, keeping main incentive arms
		use "$intermediate/gradingresults_clean.dta", clear
		keep if incentive_arm!="exact"
					
		gen bin = . // assigning observations to bins to create the scatterplot
		if "`xvar'"=="overall1" {
			
			forvalues m = 1/7 {
				replace bin = `m' if overall1==`m' // of course here the bins are trivial, but in other versions of the scatterplot in the Supplementary Materials they're nontrivial
			}
								
		}
		
		* saving the treatment and control scatterplot datasets
		forvalues n = 0/1 {
			
			cap erase "$intermediate/binscdata`n'.do"
			cap erase "$intermediate/binscdata`n'.csv"
			
			* calculating plotted slope from raw data
			* note this is the 'raw' slope - the proper regression, which underlies the numbers printed in the plot, uses grader fixed effects and clustering (see below)
			reg `yvar' `xvar' if treatment==`n', robust
			local beta`n' = _b[`xvar']
			local constant`n' = _b[_cons]
					
			cap drop N
			gen N = 1
					
			preserve
					
				keep if treatment==`n'
				collapse (mean) `yvar' `xvar' (sum) N, by(bin)
						
				save "$intermediate/binscdata`n'.dta", replace
			restore	
		}
		
	
		preserve

			clear

			use "$intermediate/binscdata1.dta", clear
			
				
			gen mergevar = bin
				
			rename N N_treated
			rename `yvar' `yvar'_treated
			rename `xvar' `xvar'_treated
				
			save "$intermediate/binscdata1_temp.dta", replace
				
				
		restore
			
		* calculating the 'difference in slopes' point estimate and standard error printed in the chart - this is with the 'proper' regression
		gen ineqinter = `xvar'*treatment

		reg `yvar' `xvar' treatment ineqinter i.n_grader_id, cluster(prolific_pid)
		local high = round(_b[ineqinter]+(1.96*_se[ineqinter]),0.001) // top and bottom of CI
		local low = round(_b[ineqinter]-(1.96*_se[ineqinter]),0.001)
		local formalbeta = string(round(_b[ineqinter],0.001)) // change in slope
		local originalbeta = string(round(_b[overall1],0.001)) // control slope
			
		if "`yvar'"=="overall2" & "`xvar'"=="overall1" {
			local t = _b[ineqinter]/_se[ineqinter]
			local pineq = 2*ttail(e(df_r),abs(`t')) // the p-value, for reporting in the paper
		}

		* building the plot
		preserve

			clear
				
			use "$intermediate/binscdata0", clear
				
			gen mergevar = bin
				
			merge m:1 mergevar using "$intermediate/binscdata1_temp.dta", keep(master match) nogen
								
			if "`xvar'"=="overall1" {
					local y = 3
					local xtitle "Grade: Task 1"
			}

			if "`xvar'"=="overall1" local linerange "1 7"
				
			if "`yvar'"=="overall2" & "`xvar'"=="overall1" {
				local textloc0 "3.2 3.41"
				local textloc "2.8 3.15"
				local textloc2 "2.4 2.87"
			}
			
			* correct number of leading and trailing digits
			foreach digit in originalbeta formalbeta high low {
				if ``digit''<0 {
					if substr("``digit''",2,1)=="." {
						local `digit' = substr("``digit''",2,.)
						local `digit' = "-0"+"``digit''"
					}
				}
				if ``digit''>=0 {
					if substr("``digit''",1,1)=="." {
						local `digit' = "0"+"``digit''"
					}
				}
			}
			
			if "`yvar'"=="overall2" { // version with slopes etc printed on plot
			twoway (connect `yvar' `xvar' if `yvar'==69, lc(black) mc(black) msymbol(square_hollow) lpattern(dash)) ///
				(connect `yvar'_treated `xvar'_treated if `yvar'==69, lc("`green'") mc("`green'") msymbol(triangle)) ///
				(function y = (`beta0'*x)+`constant0', color(black) range(`linerange') lpattern(dash)) ///
				(function y = (`beta1'*x)+`constant1', color("`green'") range(`linerange')) ///
				(scatter `yvar' `xvar' [w=N], color(black) msymbol(square_hollow) msize(small)) ///
				(scatter `yvar'_treated `xvar'_treated [w=N_treated], color("`green'") msymbol(triangle) msize(small)) ///
				, ytitle(`ytitle') xtitle(`xtitle') `yscaleextra' ///
				legend(order(1 "Control" 2 "Treatment") cols(2) region(lstyle(none)) size(large)) ///
				text(`textloc0' "Control Slope:   `originalbeta'", placement(e) size(large)) ///
				text(`textloc' "Change in Slope:   `formalbeta'", placement(e) size(large)) ylabel(`ylabelextra',angle(0)) xlabel(1(1)7) ///
				text(`textloc2' "95% CI on Change: [`high', `low']", placement(e) size(large))
			}
			if "`yvar'"=="time2" { // version without stuff printed on plot
			twoway (connect `yvar' `xvar' if `yvar'==69, lc(black) mc(black) msymbol(square_hollow) lpattern(dash)) ///
				(connect `yvar'_treated `xvar'_treated if `yvar'==69, lc("`green'") mc("`green'") msymbol(triangle)) ///
				(function y = (`beta0'*x)+`constant0', color(black) range(`linerange') lpattern(dash)) ///
				(function y = (`beta1'*x)+`constant1', color("`green'") range(`linerange')) ///
				(scatter `yvar' `xvar' [w=N], color(black) msymbol(square_hollow) msize(small)) ///
				(scatter `yvar'_treated `xvar'_treated [w=N_treated], color("`green'") msymbol(triangle) msize(small)) ///
				, ytitle(`ytitle') xtitle(`xtitle') `yscaleextra' ///
				legend(order(1 "Control" 2 "Treatment") cols(2) region(lstyle(none)) size(large)) ///
				 ylabel(`ylabelextra',angle(0)) xlabel(1(1)7)

			}
			graph export "$outfolder/sc_inequality_`titlesuff1'`titlesuff2'.pdf", replace
				
			restore
			
			
			
	}
}


* time histogram
use "$intermediate/analysis_clean.dta", clear
local green "12 164 124"

keep if incentive_arm!="exact" // keeping linear and convex arms

forvalues n = 1/9 {
	gen time2_`n' = time2>=(5*`n'-5) & time2<5*`n' // defining bin membership dummies
	replace time2_`n' = time2_`n'*100 // so that the x-axis is in percentage terms
}
gen time2_10 = time2>45
replace time2_10 = time2_10*100
drop time2_7_tex

su time2 if treatment==0
local mean0 = string(round(r(mean),1)) // calculating means to print in the plot

su time2 if treatment==1
local mean1 = string(round(r(mean),1))


collapse (mean) time2_*, by(treatment)

reshape long time2_, i(treatment) j(jvar)

replace jvar = jvar+0.2 if treatment==1
replace jvar = jvar-0.2 if treatment==0

twoway (bar time2_ jvar if treatment==0, barwidth(0.4) fcolor("black%30") bcolor(black)) ///
		(bar time2_ jvar if treatment==1, barwidth(0.4) color("`green'")) ///
		, legend(order(1 "Control, Mean: `mean0'" 2 "Treatment, Mean: `mean1'") cols(1) region(lstyle(none)) size(medlarge) ring(0) pos(1)) ///
		xtitle("Grade") ytitle("Percent of Respondents", size(large)) xlabel(1 "0-5" 2 "6-10" 3 "11-15" 4 "16-20" 5 "21-25" 6 "26-30" 7 "31-35" 8 "36-40" 9 "41-45" 10 ">45") ///
		xtitle("Minutes Spent on Second Task", size(large)) ylabel(,angle(0))

graph export "$outfolder/sc_timespent_hist.pdf", replace

* grade histogram - this is at the grader-participant level
foreach grade in overall {

	local green "12 164 124"
	use "$intermediate/analysis_clean.dta", clear
	
	keep if incentive_arm!="exact"
	keep prolific_pid a_first treatment usedgpt used occupation
	
	save "$intermediate/a_firstlist.dta", replace // saving identifiers of which participants saw which task pre- vs post-treatment
	
	use "$intermediate/humangrades.dta", clear // loading grading dataset in long format
	
	merge m:1 prolific_pid using "$intermediate/a_firstlist.dta", keep(master match) // merging those identifiers onto the grading data
	keep if _merge==3 // keeping only linear/convex people
	drop _merge
	
	keep if (a_first==1 & essaynum=="B") | (a_first==0 & essaynum=="A") // keeping only post-treatment grades
		
	keep if `grade'!=.
	
	local meanlist ""
	local sdlist ""
	forvalues n = 1/7 {
		gen `grade'_`n' = `grade'==`n'
		local meanlist "`meanlist' `grade'_`n'"
		local sdlist "`sdlist' sd_`grade'_`n'=`grade'_`n'"
	}
	
	su `grade' if treatment==0
	local mean0 = string(round(r(mean),0.001))
	su `grade' if treatment==1
	local mean1 = string(round(r(mean),0.001))
	
	preserve
	
		keep `grade' treatment
		
		reg `grade' treatment, robust
		
		local `grade'coef = _b[treatment]
		local `grade'se = _se[treatment]
	
	restore
	
	gen N = 1
	collapse (mean) `meanlist' (sd) `sdlist' (sum) N, by(treatment)
	
	forvalues n = 1/7 {
		gen se`n' = sd_`grade'_`n'/sqrt(N)
		gen high`n' = `grade'_`n'+(1.96*se`n')
		gen low`n' = `grade'_`n'-(1.96*se`n')
	}
	
	keep `grade'_* high* low* treatment
		
	reshape long `grade'_ high low, i(treatment) j(jvar)
	
	replace jvar = jvar-0.1 if treatment==0
	replace jvar = jvar+0.1 if treatment==1
	
	foreach var in `grade'_ high low {
		replace `var' = `var'*100
	}
	
	twoway (bar `grade'_ jvar if treatment==0, barwidth(0.2) fcolor("black%30") bcolor(black)) ///
		(bar `grade'_ jvar if treatment==1, barwidth(0.2) color("`green'")) ///
		(rcap high low jvar, color(black)) ///
		, legend(order(1 "Control: Mean `mean0'" 2 "Treatment: Mean `mean1'") cols(1) ring(0) position(1) region(lstyle(none)) size(medlarge)) ///
		xtitle("Second-Task Grade", size(large)) xlabel(1(1)7) xscale(range(0.5 7.5)) ytitle("Percent of Essay-Grades", size(large)) yscale(range(0 50)) ylabel(0(10)50, angle(0))
	graph export "$outfolder/sc_gradehist_`grade'.pdf", replace
	
	
}

* beliefs about automation plot
use "$intermediate/analysis_clean.dta", clear

rename automation_specific automation1
rename automation_good automation2
rename automation_future automation3

forvalues n = 1/3 {
	
	quietly su automation`n' // means and standard deviations
	gen s_automation`n' = (automation`n'-r(mean))/r(sd)
	
	reg s_automation`n' treatment, robust
	local beta`n' = string(round(_b[treatment],0.001))
	
	local t = _b[treatment]/_se[treatment]
	local p`n' = string(round(2*ttail(e(df_r),abs(`t')),0.001))
	

	* for percentage reporting
	di "percentage"
	reg automation`n' treatment, robust
	su automation`n' if treatment==0
	
}


foreach digit in beta1 beta2 beta3 p1 p2 p3 {
				if ``digit''<0 {
					if substr("``digit''",2,1)=="." {
						local `digit' = substr("``digit''",2,.)
						local `digit' = "-0"+"``digit''"
					}
				}
				if ``digit''>=0 {
					if substr("``digit''",1,1)=="." {
						local `digit' = "0"+"``digit''"
					}
				}
				
				if ``digit''==0 {
					local `digit' = "0."+"``digit''"+"00"
				}
			}
			


local meanlist ""
local sdlist ""
forvalues n = 1/3 {
	local meanlist "`meanlist' automation`n'"
	local sdlist "`sdlist' sd_automation`n'=automation`n'"
}

gen N = 1
collapse (mean) `meanlist' (sd) `sdlist' (sum) N, by(treatment)

forvalues n = 1/3 {
	gen high`n' = automation`n'+(1.96*(sd_automation`n'/sqrt(N)))
	gen low`n' = automation`n'-(1.96*(sd_automation`n'/sqrt(N)))
}

reshape long automation high low, i(treatment) j(jvar)

gen jvar2 = .
replace jvar2 = jvar-0.2 if treatment==0
replace jvar2 = jvar+0.2 if treatment==1

twoway (bar automation jvar2 if treatment==0 & jvar==1, fcolor(black%30) bcolor(black) barwidth(0.35)) ///
	(bar automation jvar2 if treatment==1 & jvar==1, color("`green'") barwidth(0.35)) ///
	 (bar automation jvar2 if treatment==0 & jvar==2, fcolor(black%30) bcolor(black)  barwidth(0.35)) ///
	(bar automation jvar2 if treatment==1 & jvar==2, color("`green'") barwidth(0.35)) ///
	 (bar automation jvar2 if treatment==0 & jvar==3, fcolor(black%30) bcolor(black)  barwidth(0.35)) ///
	(bar automation jvar2 if treatment==1 & jvar==3, color("`green'") barwidth(0.35)) ///
	(rcap high low jvar2, color(black)) ///
	, xlabel(1 `" "Worried About" "Replacement "' 2 `" "Excited About" "Enhancement" "' 3 `" "Overall" "Optimism" "', labsize(large)) ///
	ytitle("Average Response (1-10 Scale)") ///
	text(6.1 0.75 "diff: `beta1' SDs", placement(e)) text(5.8 0.75 "p-value: `p1'", placement(e)) ///
	text(7.9 1.75 "diff: `beta2' SDs", placement(e)) text(7.6 1.75 "p-value: `p2'", placement(e)) ///
	text(7.9 2.75 "diff: `beta3' SDs", placement(e)) text(7.6 2.75 "p-value: `p3'", placement(e)) ///
	xtitle("") legend(order(1 "Control" 2 "Treatment") cols(2) region(lstyle(none)))
graph export "$outfolder/sc_automation_concerns.pdf", replace


* descriptive statistics
use "$intermediate/analysis_clean.dta", clear

merge m:1 prolific_pid using "$raw/screener_edudata.dta", keep(master match) nogen

merge m:1 prolific_pid using "$intermediate/meangrades.dta", keep(master match) nogen

foreach prefix in overall writing {
	gen `prefix'1 = `prefix'A
	replace `prefix'1 = `prefix'B if a_first==0
	
	gen `prefix'2 = `prefix'B
	replace `prefix'2 = `prefix'A if a_first==0
}

gen college = 1 // community college or BA
replace college = 0 if education<4 // check screener_edudata, about 87% are BA/postgrad

gen hr = occupation=="HR professional"
gen consultant = occupation=="consultant"
gen data_analyst = occupation=="data analyst"
gen grantwriter = occupation=="grant writer"
gen manager = occupation=="manager"
gen marketer = occupation=="marketer"

gen fulltime = empstat==1
gen parttime = empstat==2
gen unemployed = empstat==3
gen nilf = empstat==4
gen missing_emp = empstat==5

replace salary = . if salary>500000

gen employed = fulltime==1 | parttime==1 // CHANGE EMPLOYMENT DEFINITION TO ACCOUNT FOR MISSING
replace employed = . if empstat==.


local descriptivelist "salary tenure employed college hr consultant data_analyst grantwriter manager marketer"

label var salary "Annual Salary"
label var tenure "Years of Tenure"
label var employed "Employed"
label var college "College Degree"
label var hr "Occ: HR"
label var consultant "Occ: Cons"
label var data_analyst "Occ: Data"
label var grantwriter "Occ: Grantwriter"
label var manager "Occ: Manager"
label var marketer "Occ: Marketer" 
label var time1 "Time Spent Task 1"
label var overall1 "Grade Task 1"

global DESCVARS salary tenure employed college hr consultant data_analyst grantwriter manager marketer time1 overall1 jobsatisfaction1 selfefficacy1
mata: mata clear

* balance table (code sourced from Paul Hofman https://hofmanpaul.com/automation/descriptive-and-balance-tables-in-stata/)
rename treatment treated
local i = 1

foreach var in $DESCVARS {
    reg `var' treated, robust
    outreg, keep(treated)  rtitle("`: var label `var''") stats(b) ///
        noautosumm store(row`i')  starlevels(10 5 1) starloc(1)
    outreg, replay(diff) append(row`i') ctitles("",Difference ) ///
        store(diff) note("")
    local ++i
}
outreg, replay(diff)

local count: word count $DESCVARS
mat sumstat = J(`count',6,.)

local i = 1
foreach var in $DESCVARS {
    quietly: summarize `var' if treated==0
    mat sumstat[`i',1] = r(N)
    mat sumstat[`i',2] = r(mean)
    mat sumstat[`i',3] = r(sd)
    quietly: summarize `var' if treated==1
    mat sumstat[`i',4] = r(N)
    mat sumstat[`i',5] = r(mean)
    mat sumstat[`i',6] = r(sd)
    local i = `i' + 1
}
frmttable, statmat(sumstat) store(sumstat) sfmt(g,f,f,g,f,f)

outreg using "$outfolder/sc_balance", ///
    replay(sumstat) merge(diff) tex nocenter note("") fragment plain replace ///
    ctitles("", Control, "", "", Treatment, "", "", "" \ "", n, mean, sd, n, mean, sd, Diff) ///
    multicol(1,2,3;1,5,3) 
	
	
outreg using "$outfolder/sc_balance.rtf", ///
    replay(sumstat) merge(diff) nocenter note("") plain replace ///
    ctitles("", Control, "", "", Treatment, "", "", "" \ "", n, mean, sd, n, mean, sd, Diff) ///
    multicol(1,2,3;1,5,3) 

* printing out p-values
foreach p in ptime pgrade1 pgrade2 controlgini treatgini pineq pjobsat pselfefficacy {
	di "`p'"
	di ``p''
}


use "$intermediate/analysis_clean.dta", clear



* how many respondents retried?
use "$intermediate/analysis_clean.dta", clear

keep if lucasarm=="on"

gen lucas_replaced = retry_yn==2 & (replace!=2)
gen lucas_edited = retry_yn==3 & (edit!=2)

tab lucas_replaced
tab lucas_edited

* grade effects of the arm where participants are allowed to edit their original output with ChatGPT

insheet using "$raw/grading_wide_complete.csv", clear names

keep if retry_a==1 | retry_b==1

rename overall_a retry_overall_A
rename overall_b retry_overall_B

collapse (mean) retry_overall_A retry_overall_B, by(prolific_pid)

save "$intermediate/retry_grades_merge.dta", replace


use "$intermediate/analysis_clean.dta", clear
merge m:1 prolific_pid using "$intermediate/meangrades.dta", keep(master match) nogen
merge m:1 prolific_pid using "$intermediate/retry_grades_merge.dta", keep(master match) nogen

keep if retry_yn!=.

foreach prefix in overall retry_overall_ {
	gen `prefix'1 = `prefix'A
	replace `prefix'1 = `prefix'B if a_first==0
	
	gen `prefix'2 = `prefix'B
	replace `prefix'2 = `prefix'A if a_first==0
}

gen retried = retry_yn==2 | retry_yn==3

ttest overall1, by(retried)

ttest overall1==retry_overall_1 if retried==1

use "$intermediate/analysis_clean.dta", clear

gen noedit = strpos(used,"3")>0 & strpos(used,"4")==0 & strpos(used,"5")==0
tab noedit if treatment==1 & usedgpt==1

* how active are treatment participants after pasting in output from ChatGPT?
use "$intermediate/analysis_clean.dta", clear

merge m:1 prolific_pid using "$intermediate/meangrades.dta", keep(master match) nogen

foreach prefix in overall writing {
	gen `prefix'1 = `prefix'A
	replace `prefix'1 = `prefix'B if a_first==0
	
	gen `prefix'2 = `prefix'B
	replace `prefix'2 = `prefix'A if a_first==0
}

keep if usedgpt==1

gen bigpaste_post = 0
gen bigpaste_pre = 0
forvalues n = 1/45 {
	replace bigpaste_post = bigpaste_post+1 if modified_word_count_post_`n'>100 & modified_word_count_post_`n'!=.
	replace bigpaste_pre = bigpaste_pre+1 if modified_word_count_pre_`n'>100 & modified_word_count_pre_`n'!=.
}

tab bigpaste_pre
tab bigpaste_pre if usedgpt_first==1
tab bigpaste_pre if usedgpt_first==2

tab usedgpt if treatment==0
tab usedgpt_first if treatment==0
tab usedgpt_first if treatment==1


gen usedgpt_first_withzeroes = usedgpt_first
replace usedgpt_first_withzeroes = 0 if usedgpt_first_withzeroes==.
tab usedgpt_first_withzeroes if treatment==1

gen firstpaste = .
forvalues n = 1/15 {
	replace firstpaste = `n' if modified_word_count_post_`n'>100 & modified_word_count_post_`n'!=. & firstpaste==.
}

* control group usage of ChatGPT
gen used_objective = firstpaste!=.
tab used_objective if treatment==0
tab used_objective if usedgpt==1

* activity post-paste among treated GPT users
keep if usedgpt==1 & treatment==1

tab firstpaste, m
tab bigpaste_post, m

* does the first-paste measure select for people who submitted without editing?
gen noedit = strpos(used,"2")>0 if used!=""
gen yesedit = strpos(used,"3")>0 & noedit==0 if used!=""
gen otheredit = noedit==0 & yesedit==0

gen firstpaste_nm = firstpaste!=.

gen keep = firstpaste!=. & usedgpt==1 & treatment==1 & bigpaste_post==1

tab keep if treatment==1 & usedgpt==1

tab noedit
tab keep if noedit==1

tab yesedit
tab keep if yesedit==1

tab otheredit
tab keep if otheredit==1


keep if firstpaste!=. & usedgpt==1 & treatment==1 & bigpaste_post==1

gen active_postpaste = 0
forvalues n = 1/45 {
	replace active_postpaste = active_postpaste+1 if `n'>firstpaste & modified_word_count_post_`n'>3 & modified_word_count_post_`n'!=.
}
local green "12 164 124"

su active_postpaste

keep if yesedit==1 // restricting to people who say they edited

su active_postpaste


gen activemore = active_postpaste>1 if active_postpaste!=.
ttest overall2, by(activemore)

reg overall2 active_postpaste if yesedit==1
reg overall2 active_postpaste if yesedit==1 & active_postpaste<10
reg overall2 active_postpaste if yesedit==1 & incentive_arm=="convex"

hist active_postpaste, discrete percent fcolor("`green'"%70) bcolor(black) xtitle("Minutes Active Post-Paste", size(large)) xlabel(,labsize(medlarge)) ytitle("Percent of Users")
graph export "$outfolder/app_sc_postpaste_activity.pdf", replace

preserve
 * for Levenshtein distance analysis
 
 keep prolific_pid responseid firstpaste
 
 export delimited "$intermediate/levenshtein.csv", replace


restore


su active_postpaste
su time2

gen total_modified_postpaste = 0 // how many modifications do you make?
forvalues n = 1/45 {
	replace total_modified_postpaste = total_modified_postpaste+modified_word_count_post_`n' if `n'>firstpaste & modified_word_count_post_`n'!=. 
}
gen modified_share = total_modified_postpaste/finalword2
su modified_share


* use of overleaf in control group
use "$intermediate/analysis_clean.dta", clear

merge m:1 prolific_pid using "$intermediate/meangrades.dta", keep(master match) nogen

gen overleaf = strpos(software,"2")>0 if software!=""
tab overleaf if treatment==0

gen overall1 = overallA
				replace overall1 = overallB if a_first==0
				
				gen overall2 = overallB
				replace overall2 = overallA if a_first==0
				
reg overall2 overleaf if treatment==0


* other misc calculations: for numbers corresponding to Figure 2b
su overall1 if overall1<4 & treatment==1
su overall2 if overall1<4 & treatment==1
su time1 if overall1<4 & treatment==1
su time2 if overall1<4 & treatment==1

* earnings
use "$intermediate/gradingresults_clean_yesbad.dta", clear

drop if overall1==. | overall2==.

cap drop dup
gen rand = runiform(0,1)
sort prolific_pid rand
quietly by prolific_pid: gen dup = cond(_N==1,0,_n)
drop if dup>1

gen dummy1 = overall1>=6 if incentive_arm=="convex"
gen dummy2 = overall2>=6 if incentive_arm=="convex"

tab dummy1 
tab dummy2

gen earnings = earnings1+earnings2

su earnings

gen base = 10
replace base = 8 if incentive_arm=="exact"

gen total_earnings = earnings+base
su total_earnings

use "$intermediate/analysis_clean.dta", clear

gen time = durationinseconds/60

su time


* other misc tabulations
use "$intermediate/analysis_clean.dta", clear

tab techdiff_yes if treatment==1
tab usedgpt if treatment==1

su usefulness if treatment==1

tab chatgpt_aware
tab chatgpt_use




