
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


********************************************************************************
******** versions of the main grading results that include bad graders *********
********************************************************************************

* 2sls versions of regressions				
		
		eststo clear
		local m = 0
foreach outcome in time overall jobsatisfaction selfefficacy { // looping over outcomes: time taken, overall grades, job satisfaction, self-efficacy
			
			if "`outcome'"=="time" | "`outcome'"=="overall" local sample "linconv"
			if "`outcome'"=="jobsatisfaction" | "`outcome'"=="selfefficacy" local sample "all"
			
				if "`sample'"=="all" local restriction ""
			if "`sample'"=="linconv" local restriction `"if incentive_arm!="exact""'
			if "`sample'"=="exact" local restriction `"if incentive_arm=="exact""'

			local m = `m'+1
			
			* grade-outcome regressions - at the participant-grader level
			if "`outcome'"=="overall" {
				use "$intermediate/gradingresults_clean_yesbad.dta", clear
				
				recode usedgpt (2 4 = 0)
				
				reg `outcome'_diff treatment i.n_grader_id i.occnum##i.a_first i.incentivenum `restriction', cluster(prolific_pid)
				eststo est`m'
				
				local m = `m'+1
				
				ivregress 2sls `outcome'_diff (treatment = usedgpt) i.n_grader_id i.occnum##i.a_first i.incentivenum `restriction', cluster(prolific_pid)
				eststo est`m'
				
				
				* note the grader fixed effects and the clustering at the participant level
			}
			
			
			
			
			* non-grade-outcome regressions - at the tasker level
			if "`outcome'"!="overall" {
				
				use "$intermediate/analysis_clean.dta", clear
								
				recode usedgpt (2 4 = 0)
				
				egen occnum = group(occupation) // occupation fixed effects
				egen incentivenum = group(incentive_arm) // incentive-arm fixed effects

				gen `outcome'_diff = `outcome'2-`outcome'1
				
				reg `outcome'_diff treatment i.occnum##i.a_first i.incentivenum `restriction', robust
				eststo est`m'
				
				
				local m = `m'+1
		
				
				ivregress 2sls `outcome'_diff (treatment = usedgpt) i.occnum##i.a_first i.incentivenum `restriction', robust
				eststo est`m'
							
			}
									
			
}
	
	
esttab est1 est2 est3 est4 using "$outfolder/app_sc_2sls1.tex", replace ///
				b(3) se(3) nodep legend label ///
					varlabels(treatment "& &\\  \textbf{Treatment}" ///
					) ///
					keep(treatment) ///
					order(treatment) ///
					nomtitles  mlabels("Time (OLS)" "Time (IV)" "Grades (OLS)" "Grades (IV)") ///
					collabels(none) stats(N, fm(%8.0fc) ///
					labels("\textbf{Nb. obs}")) starlevels(* 0.1 ** 0.05 *** 0.01)


		esttab est5 est6 est7 est8 using "$outfolder/app_sc_2sls2.tex", replace ///
				b(3) se(3) nodep legend label ///
					varlabels(treatment "& &\\  \textbf{Treatment}" ///
					) ///
					keep(treatment) ///
					order(treatment) ///
					nomtitles  mlabels( "Job Satisfaction (OLS)" "Job Satisfaction (IV)" "Self-Efficacy (OLS)" "Self-Efficacy (IV)") ///
					collabels(none) stats(N, fm(%8.0fc) ///
					labels("\textbf{Nb. obs}")) starlevels(* 0.1 ** 0.05 *** 0.01)			
					
					
********************************************************************************
******** versions of the main grading results that include bad graders *********
********************************************************************************

* regressions
foreach sample in linconv {
	
	if "`sample'"=="all" local restriction ""
	if "`sample'"=="linconv" local restriction `"if incentive_arm!="exact""'
	if "`sample'"=="exact" local restriction `"if incentive_arm=="exact""'
	
	foreach standardized in 1 { // we standardize the regression coefficient to be in terms of pre-treatment SDs of the outcome variable, before printing it on the plot
		
		if `standardized'==0 local suffix ""
		if `standardized'==1 local suffix "_s"
		
		foreach outcome in overall { // looping over outcomes: time taken, overall grades, job satisfaction, self-efficacy
			
			* grade-outcome regressions - at the participant-grader level
			if "`outcome'"=="overall" {
				use "$intermediate/gradingresults_clean_yesbad.dta", clear
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

* diff-in-diff plot
foreach loop in noexact { // same sample restriction loops
	
	local outcomelist "overall" // same outcome variable list
	
	local meanlist ""
	local sdlist ""
	local reshapelist ""
	foreach var in `outcomelist' {
		local meanlist "`meanlist' `var'1 `var'2"
		local sdlist "`sdlist' sd_`var'1=`var'1 sd_`var'2=`var'2"
		local reshapelist "`reshapelist' `var' se_`var'"
	}
	use "$intermediate/analysis_clean.dta", clear
	
	merge m:1 prolific_pid using "$intermediate/meangrades_yesbad.dta", keep(master match) nogen
	
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
			graph export "$outfolder/app_sc_did_`var'_`loop'_yesbad.pdf", replace
			
		}
		
	restore
	
}


* inequality figure
foreach yvar in overall2 { // looping over dependent variables
	
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
		use "$intermediate/gradingresults_clean_yesbad.dta", clear
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
		
			graph export "$outfolder/app_sc_inequality_`titlesuff1'`titlesuff2'_yesbad.pdf", replace
				
			restore
			
	}
}



********************************************************************************
******** Lee bounds for effects of selective attrition on main results *********
********************************************************************************

use "$intermediate/fullsample_leebounds.dta", clear // loading full, pre-attrition dataset

sort prolific_pid startdate
quietly by prolific_pid: gen dup = cond(_N==1,0,_n)
drop if dup>1
drop dup

gen selected = 1
merge m:1 prolific_pid using "$intermediate/analysis_clean.dta", keep(master match) keepusing() // detecting who ends up in the final sample
replace selected = 0 if _merge!=3
drop _merge

merge m:1 prolific_pid using "$intermediate/meangrades.dta", keep(master match) nogen

gen overall1 = overallA
	replace overall1 = overallB if a_first==0
	
	gen overall2 = overallB
	replace overall2 = overallA if a_first==0

gen employed = empstat==1 | empstat==2 | empstat==.

foreach outcome in time overall jobsatisfaction selfefficacy {
	gen `outcome'_diff = `outcome'2-`outcome'1
}


egen incentivenum = group(incentive_arm)

egen occnum = group(occupation)

foreach sample in all linconv {
	
	if "`sample'"=="all" {
		local restriction ""
		local restriction2 ""
	}
	if "`sample'"=="linconv" {
		local restriction "& incentivenum!=2"
		local restriction2 "if incentivenum!=2"
	}
	if "`sample'"=="exact" {
		local restriction "& incentivenum==2"
		local restriction2 "if incentivenum!=2"
	}
	
	foreach standardized in 0 1 {
		
		if `standardized'==0 local suffix ""
		if `standardized'==1 local suffix "_s"
		
		foreach outcome in time overall jobsatisfaction selfefficacy {
			
			reg `outcome'_diff treatment if selected==1 `restriction', cluster(prolific_pid)
			local `outcome'_lb1_`sample'`suffix' = _b[treatment]
			local `outcome'_lbse1_`sample'`suffix' = _se[treatment]
			
			
			reg `outcome'_diff treatment i.occnum employed if selected==1  `restriction', cluster(prolific_pid)
			local `outcome'_lb2_`sample'`suffix' = _b[treatment]
			local `outcome'_lbse2_`sample'`suffix' = _se[treatment]
			
			
			leebounds `outcome'_diff treatment `restriction2', cie select(selected)
			
			local `outcome'_lbh_`sample'`suffix' = e(ciupper)
			local `outcome'_lbl_`sample'`suffix' = e(cilower)
			
			
			quietly su `outcome'1 if selected==1 `restriction'
			local `outcome'_mean_`sample' = string(round(r(mean),0.001))
			
			if `standardized'==1 {
				local `outcome'_lb1_`sample'`suffix' = ``outcome'_lb1_`sample'`suffix''/r(sd)
				local `outcome'_lbse1_`sample'`suffix' = ``outcome'_lbse1_`sample'`suffix''/r(sd)
				
				local `outcome'_lb2_`sample'`suffix' = ``outcome'_lb2_`sample'`suffix''/r(sd)
				local `outcome'_lbse2_`sample'`suffix' = ``outcome'_lbse2_`sample'`suffix''/r(sd)
				
				local `outcome'_lbh_`sample'`suffix' = ``outcome'_lbh_`sample'`suffix''/r(sd)
				local `outcome'_lbl_`sample'`suffix' = ``outcome'_lbl_`sample'`suffix''/r(sd)
			}

		}
	
	}
}


* plotting the lee bounds results
clear

set obs 4

forvalues n = 1/2 {
	gen lb`n' = .
	gen lb`n'_high = .
	gen lb`n'_low = .
	
	local m = 0
	foreach outcome in time overall jobsatisfaction selfefficacy {
		
		di "`outcome'"
		
		if "`outcome'"=="time" | "`outcome'"=="overall" local group "linconv"
		if "`outcome'"=="jobsatisfaction" | "`outcome'"=="selfefficacy" local group "all"
		
		local m = `m'+1
		
		replace lb`n' =  ``outcome'_lb`n'_`group'_s' if _n==`m'
		
		replace lb`n'_high = lb`n'+(1.96*``outcome'_lbse`n'_`group'_s') if _n==`m'
		replace lb`n'_low = lb`n'-(1.96*``outcome'_lbse`n'_`group'_s') if _n==`m'
		
		
	}
	
}

gen leehigh = .
gen leelow = .

local m = 0
foreach outcome in time overall jobsatisfaction selfefficacy {
	
	if "`outcome'"=="time" | "`outcome'"=="overall" local group "linconv"
		if "`outcome'"=="jobsatisfaction" | "`outcome'"=="selfefficacy" local group "all"
		
	local m = `m'+1
	di ``outcome'_lbh_`group'_s'
	replace leehigh = ``outcome'_lbh_`group'_s' if _n==`m'
	replace leelow = ``outcome'_lbl_`group'_s' if _n==`m'
	
}

gen jvar2 = _n
gen jvar1 = jvar2-0.1
gen jvar3 = jvar2+0.1

twoway (connected lb1 jvar1 if jvar1==69, color(black)) (connected lb2 jvar2 if jvar2==69, color(blue) msymbol(triangle_hollow)) (line leehigh jvar2 if jvar2==69, color(red)) ///
	(scatter lb1 jvar1, color(black) msymbol(circle)) (scatter lb2 jvar2, color(blue) msymbol(triangle_hollow)) ///
	(rcap lb1_high lb1_low jvar1, color(black)) (rcap lb2_high lb2_low jvar2, color(blue)) ///
	(rcap leehigh leelow jvar3, color(red)) ///
	, yline(0, lcolor(black) lpattern(dash)) ytitle("Treatment Effect (SDs)") ylabel(,angle(0)) ///
	legend(order(1 "No Controls" 2 "Controlling for Employment Status, Occupation" 3 "Lee Bound Confidence Interval") cols(1) region(lstyle(none))) ///
	xlabel(1 `" "Time Taken" "(Linear+Convex)" "' 2 `" "Overall Grade" "(Linear+Convex)" "' 3 `" "Job Satisfaction" "(All)" "' 4 `" "Self-Efficacy" "(All)" "')

graph export "$outfolder/app_sc_leebounds.pdf", replace


********************************************************************************
*************** Heterogeneity in ChatGPT effects by writing skill **************
********************************************************************************

use "$intermediate/analysis_clean.dta", clear

merge m:1 prolific_pid using "$intermediate/meangrades.dta", keep(master match) nogen

foreach prefix in overall writing content originality {
	gen `prefix'1 = `prefix'A
	replace `prefix'1 = `prefix'B if a_first==0
	
	gen `prefix'2 = `prefix'B
	replace `prefix'2 = `prefix'A if a_first==0
}

* first measure of relative writing skill: difference between writing grade and overall grade, on first task
gen diff1 = writing1-overall1
su diff1, d


* first measure of ChatGPT effects: change in grades between first and second task
gen diffgrade = overall2-overall1

keep if chatgpt_wtp!=. & diffgrade!=.

drop if salary<10000 | salary>500000 // excluding implausible earnings outliers (in a pretty ad-hoc way)

* second measure of ChatGPT effects: willingness to pay for a ChatGPT subscription
gen wtp_pct = (chatgpt_wtp/(salary/12))*100

winsor wtp_pct, p(0.02) gen(_wtp_pct) // hammering outliers again
drop wtp_pct
rename _wtp_pct wtp_pct

* second measure of relative writing skills: self-assessed skill ranking
gen good_communicator = skillranking_1==1
gen med_communicator = skillranking_1==2
gen bad_communicator = skillranking_1==3

reg writing1 good_communicator

reg diff1 good_communicator


foreach outcome in wtp_pct diffgrade {
	su `outcome' if good_communicator==1

	local `outcome'1 = r(mean)
	local `outcome'_se1 = r(sd)/sqrt(r(N))

	su `outcome' if med_communicator==1

	local `outcome'2 = r(mean)
	local `outcome'_se2 = r(sd)/sqrt(r(N))

	su `outcome' if bad_communicator==1

	local `outcome'3 = r(mean)
	local `outcome'_se3 = r(sd)/sqrt(r(N))
		
	su `outcome' if diff1>0
	local `outcome'4 = r(mean)
	local `outcome'_se4 = r(sd)/sqrt(r(N))	
	
	su `outcome' if diff1==0
	local `outcome'5 = r(mean)
	local `outcome'_se5 = r(sd)/sqrt(r(N))	
	
	su `outcome' if diff1<0
	local `outcome'6 = r(mean)
	local `outcome'_se6 = r(sd)/sqrt(r(N))	
	
	
}

clear

set obs 6
gen wtp_pct = .
gen wtp_pct_se = .

gen diffgrade = .
gen diffgrade_se = .

di ``outcome'`n''

foreach outcome in wtp_pct diffgrade {
	forvalues n = 1/6 {
		replace `outcome' = ``outcome'`n'' if _n==`n'
		replace `outcome'_se = ``outcome'_se`n'' if _n==`n'
	}
	gen high_`outcome' = `outcome'+(1.96*`outcome'_se)
gen low_`outcome' = `outcome'-(1.96*`outcome'_se)
}
gen jvar = _n

replace jvar = jvar+1 if jvar>3
replace jvar = jvar+1 if jvar>7

twoway (bar wtp_pct jvar if jvar==3 | jvar==7 , barwidth(0.8) color("`lightgreen'")  ) ///
	(bar wtp_pct jvar if jvar==2 | jvar==6 , barwidth(0.8) color("`green'")) ///
	(bar wtp_pct jvar if jvar==1 | jvar==5 , barwidth(0.8) color("`darkgreen'") ) ///
	(rcap high_wtp_pct low_wtp_pct jvar, color(black)) ///
	, xlabel(none) ylabel(,angle(0)) ///
	title("Outcome: Willingness to Pay for ChatGPT (% Salary)", size(large)) yscale(range(0 1)) ///
	legend(off) xtitle("") 
graph save graph1.gph, replace

twoway (bar diffgrade jvar if jvar==3 | jvar==7 , barwidth(0.8) color("`lightgreen'") ) ///
	(bar diffgrade jvar if jvar==2 | jvar==6 , barwidth(0.8) color("`green'")) ///
	(bar diffgrade jvar if jvar==1 | jvar==5 , barwidth(0.8) color("`darkgreen'")  ) ///
	(rcap high_diffgrade low_diffgrade jvar, color(black)) ///
	, xlabel(2 `" "Self-Rated" "Communication Skills" "' 6 `" "Overall versus Writing" "Grade on First Task" "', labsize(vlarge)) ///
	title("Outcome: Grade Gain from ChatGPT", size(large)) yscale(range(0 1)) ylabel(,angle(0)) ///
	legend(order(3 "Bad Writer" 2 "Medium" 1 "Good Writer") cols(3) region(lstyle(none)) size(large)) xtitle("") 
graph save graph2.gph, replace
graph export "$outfolder/sc_subj_heterogeneity1.pdf", replace

grc1leg graph1.gph graph2.gph, colfirst cols(1) legendfrom(graph2.gph) 
graph export "$outfolder/sc_subj_heterogeneity.pdf", replace
	

********************************************************************************
********************* Effects of ChatGPT on 'task structure' *******************
********************************************************************************

* time shares on the three components of time spent - brainstorming, writing a rough draft, and editing the draft
use "$intermediate/analysis_clean.dta", clear

foreach var in brainstorming roughdraft editing {
	forvalues n = 1/2 {
		gen `var'_pct`n' = `var'`n'/time`n'
	}
}

rename brainstorming_pct2 share1
rename roughdraft_pct2 share2
rename editing_pct2 share3


gen N = 1
collapse (mean) share1 share2 share3 (sd) sd_share1=share1 sd_share2=share2 sd_share3=share3 (sum) N, by(treatment)

forvalues n = 1/3 {
	gen high`n' = share`n'+(1.96*(sd_share`n'/sqrt(N)))
	gen low`n' = share`n'-(1.96*(sd_share`n'/sqrt(N)))
}


reshape long share high low, i(treatment) j(jvar)

foreach var in share high low {
	replace `var' = `var'*100
}

replace jvar = jvar-0.2 if treatment==0
replace jvar = jvar+0.2 if treatment==1


twoway (bar share jvar if treatment==0, bcolor(black) fcolor(black%30) barwidth(0.4))  ///
 (bar share jvar if treatment==1, color("`green'") barwidth(0.4)) (rcap high low jvar, color(black)) ///
	, ytitle("Percent of Time Spent", size(large)) xlabel(1 "Brainstorming" 2 "Rough-Drafting" 3 "Editing", labsize(large)) ///
	xtitle("") legend(order(1 "Control" 2 "Treatment") cols(1) region(lstyle(none)) ring(0) pos(1) size(large)) yscale(range(0 60)) ylabel(10(10)60, angle(0))
graph export "$outfolder/sc_taskstructure_hist_diff.pdf", replace


********************************************************************************
************************ Self-Reported Task Realism ****************************
********************************************************************************

use "$intermediate/analysis_clean.dta", clear


tab task_experience
recode task_experience (2 = 0) (1 = 100)
su task_experience
local mean = round(r(mean),0)
local mean = substr("`mean'",1,2)


forvalues n = 1/5 {
		gen realism`n' = task_realism==`n'
	}
	
	drop realism_text
	
	collapse (mean) realism*
	
	gen ivar = 1
	
	reshape long realism, i(ivar) j(jvar)
	
	replace realism = realism*100
	
	twoway (bar realism jvar, horizontal color("`green'")) ///
		, ytitle("") ylabel(1 "Very Unrealistic" 2 "Unrealistic" 3 "Neutral" 4 "Realistic" 5 "Very Realistic", angle(360) labsize(medlarge)) ///
		xtitle("Percent of Respondents", size(medlarge)) xlabel(,labsize(medlarge)) text(2 20 "`mean'% have completed similar task in real job", placement(e))
	graph export "$outfolder/app_sc_realism_hist.pdf", replace
	

********************************************************************************
* Did Everyone Take The Tasks Seriously? Distributions of Time Spent and Word Count *
********************************************************************************	
	
* compliance for linear/convex groups
use "$intermediate/analysis_clean.dta", clear

preserve

	keep if incentive_arm!="exact"
	
	local n = 0
	foreach var in time1 timepage1 active1 {
		local n = `n'+1
		quietly su `var'
		local mean`n' = round(r(mean))
	}
	
	graph hbox time1 timepage1 active1, noout ///
		legend(order(1 "Self-Reported Time (Mean: `mean1' minutes)" 2 "Time Spent on Page (Mean: `mean2' minutes)" 3 "Minutes 'Active' (Mean: `mean3' minutes)") cols(1) region(lstyle(none)) size(medlarge)) ///
		ylabel(0(10)70,labsize(medlarge)) box(1, color(`green')) box(2, color(`pink')) box(3,color(`blue'))
		

	graph export "$outfolder/app_sc_timespent_noexact.pdf", replace
	
	drop if time1>90
	
	su finalword1
	local mean = round(r(mean))
	
	hist finalword1 if finalword1<700, fcolor("`green'") bcolor(black) xtitle("Final Word Count", size(medlarge)) xlabel(,labsize(medlarge)) ///
		text(20 0 "Mean Word Count: `mean'", placement(e) size(medlarge)) percent ylabel(,angle(0))
	graph export "$outfolder/app_sc_wordcount_noexact.pdf", replace
	
	
restore



* compliance for exact-time group
preserve

	keep if incentive_arm=="exact"
	
	quietly su active1
	local mean3 = r(mean)
	
	graph hbox active1, noout yscale(range(0 15)) ylabel(0(3)15, labsize(large)) ytitle("Minutes 'Active'", size(large)) box(1,color("`blue'")) 
	graph export "$outfolder/app_sc_timespent_yesexact.pdf", replace
	
	su finalword1
	local mean = round(r(mean))
	
	hist finalword1, fcolor("`green'") bcolor(black) xtitle("Final Word Count", size(medlarge)) xlabel(,labsize(medlarge)) ///
		text(19 300 "Mean Word Count: `mean'", placement(e) size(medlarge)) percent
	graph export "$outfolder/app_sc_wordcount_yesexact.pdf", replace

restore


********************************************************************************
************************ Cumulative Word Count Plots ***************************
********************************************************************************

use "$intermediate/analysis_clean.dta", clear
	
	keep if incentive_arm!="exact"
		forvalues n = 1/30 {
			foreach suff in pre post {
				if "`suff'"=="pre" local m = 1
				if "`suff'"=="post" local m = 2
				replace word_count_`suff'_`n' = finalword`m' if word_count_`suff'_`n'==. & word_count_`suff'_1!=.
			}
		}
		
		collapse (mean) word_count_pre_* word_count_post_*, by(treatment)
		
		reshape long word_count_pre_ word_count_post_, i(treatment) j(jvar)
		
		keep if jvar<=30
		
		local extra ""
		if "`loop'"=="yesexact" local extra "& jvar<=15"
		
		foreach suffix in pre post {
			twoway (connected word_count_`suffix'_ jvar if treatment==1 `extra', color("`green'")) ///
				(connected word_count_`suffix'_ jvar if treatment==0 `extra', color(black) msymbol(square_hollow)) ///
				, xtitle("Minute", size(medlarge)) ytitle("Mean Word Count", size(medlarge)) legend(order(1 "Treatment" 2 "Control") cols(1) ring(0) position(5) region(lstyle(none)) size(large)) ///
				xlabel(,labsize(medlarge)) ylabel(,labsize(medlarge) angle(0))
			graph export "$outfolder/app_sc_wordcount_`suffix'_noexact.pdf", replace
		}


********************************************************************************
***** Alternative Versions of the Inequality Plots, with time on the x-axis ****
********************************************************************************

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
	
	foreach xvar in time1 { // looping over independent variables
		
						if "`xvar'"=="time1" local linerange "0 80"

						
		if "`xvar'"=="time1" {
			local xtitle "Time Taken: Task 1"
			local titlesuff2 "_timeX"
			local xlabel "0(10)80"
		}
					
		* load dataset, keeping main incentive arms
		use "$intermediate/gradingresults_clean.dta", clear
		keep if incentive_arm!="exact"
		
		
		* if it's time-on-time, there's no need to keep grade duplicates
		if "`yvar'"=="time2" {
			cap drop dup
			sort prolific_pid
			quietly by prolific_pid: gen dup = cond(_N==1,0,_n)
			drop if dup>1
			drop dup
		}
		
		* assigning observations to bins
		xtile bintemp = time1, nq(12)
			forvalues n = 1/12 {
				su time1 if bintemp==`n'
				local bin`n' = r(mean)
				di `bin`n''
			}
					
		gen bin = .
			if "`xvar'"=="time1" {
				
				replace bin = 1 if time1<=`bin1'
				
				forvalues n = 2/11 {
					local m = `n'-1
					replace bin = `n' if time1>`bin`m'' & time1<=`bin`n''
				}
				
				replace bin = 12 if time1>`bin11'
				
				
			}
		
		* saving the treatment and control scatterplot datasets
		forvalues n = 0/1 {
			
			cap erase "$intermediate/binscdata`n'.do"
			cap erase "$intermediate/binscdata`n'.csv"
			
			* calculating plotted slope from raw data
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

		if "`yvar'"=="overall2" reg `yvar' `xvar' treatment ineqinter i.n_grader_id, cluster(prolific_pid)
		if "`yvar'"=="time2" reg `yvar' `xvar' treatment ineqinter, robust
		
		local high = round(_b[ineqinter]+(1.96*_se[ineqinter]),0.001) // top and bottom of CI
		local low = round(_b[ineqinter]-(1.96*_se[ineqinter]),0.001)
		local formalbeta = string(round(_b[ineqinter],0.001)) // change in slope
		local originalbeta = string(round(_b[`xvar'],0.001)) // control slope
			
		* building the plot
		preserve

			clear
				
			use "$intermediate/binscdata0", clear
				
			gen mergevar = bin
				
			merge m:1 mergevar using "$intermediate/binscdata1_temp.dta", keep(master match) nogen
								
			if "`yvar'"=="overall2" & "`xvar'"=="time1" {
					local textloc "100 40"
					local textloc2 "2.4 3"
				}
				if "`yvar'"=="time2" & "`xvar'"=="time1" {
					local textloc "100 0"
					local textloc2 "100 3"
					local ylabelextra "10(10)70"
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
			
			twoway (connect `yvar' `xvar' if `yvar'==69, lc(black) mc(black) msymbol(square_hollow) lpattern(dash)) ///
				(connect `yvar'_treated `xvar'_treated if `yvar'==69, lc("`green'") mc("`green'") msymbol(triangle)) ///
				(function y = (`beta0'*x)+`constant0', color(black) range(`linerange') lpattern(dash)) ///
				(function y = (`beta1'*x)+`constant1', color("`green'") range(`linerange')) ///
				(scatter `yvar' `xvar' [w=N], color(black) msymbol(square_hollow) msize(small)) ///
				(scatter `yvar'_treated `xvar'_treated [w=N_treated], color("`green'") msymbol(triangle) msize(small)) ///
				, ytitle(`ytitle') xtitle(`xtitle') `yscaleextra' ///
				legend(order(1 "Control" 2 "Treatment") cols(2) region(lstyle(none)) size(large)) ///
				 ylabel(`ylabelextra',angle(0)) xlabel(`xlabel')

			graph export "$outfolder/app_sc_inequality_`titlesuff1'`titlesuff2'.pdf", replace
				
			restore
			
			
			
	}
}


********************************************************************************
***************** Alternative Versions of Diff-in-Diff figures *****************
********************************************************************************
	
foreach loop in noexact yesexact all nomanagers nodanalysts onlyexperienced {
	
	local outcomelist "time finalword active overall writing content originality jobsatisfaction selfefficacy" // expanded list of outcome variables
	
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
	
	if "`loop'"=="all" {
		forvalues n = 1/2 {
			quietly su jobsatisfaction`n'
			replace jobsatisfaction`n' = (jobsatisfaction`n'-r(mean))/r(sd)
			
			quietly su selfefficacy`n'
			replace selfefficacy`n' = (selfefficacy`n'-r(mean))/r(sd)
		}
	}
	
	egen occnum = group(occupation)
	
	foreach prefix in overall writing content originality {
	gen `prefix'1 = `prefix'A
	replace `prefix'1 = `prefix'B if a_first==0
	
	gen `prefix'2 = `prefix'B
	replace `prefix'2 = `prefix'A if a_first==0
}
	
	forvalues n = 1/2 {
		gen earnings`n' = overall`n'
		replace earnings`n' = earnings`n'+3 if incentive_arm=="convex" & overall`n'==6 | overall`n'==7
	}
	
	gen productivity1 = earnings1/time1
	gen productivity2 = earnings2/time2
	
	
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
	if "`loop'"=="nomanagers" {
		local sample "nomanagers"
		keep if incentive_arm!="exact" & occupation!="manager"
	}
	if "`loop'"=="nodanalysts" {
		local sample "nodanalysts"
		keep if incentive_arm!="exact" & occupation!="data analyst"
	}
	if "`loop'"=="onlyexperienced" {
		local sample "onlyexp"
		keep if task_experience==1
	}
	
	preserve
		
		collapse (mean) `meanlist' (sd) `sdlist' (sum) N, by(treatment)
			
		foreach suffix in `meanlist' {
			gen se_`suffix' = sd_`suffix'/sqrt(N)
		}
		
		reshape long `reshapelist', i(treatment) j(jvar)
		
		foreach suffix in `outcomelist' {
			gen high_`suffix' = `suffix'+(1.96*se_`suffix')
			gen low_`suffix' = `suffix'-(1.96*se_`suffix')
		}
		
		replace jvar = jvar-0.01 if treatment==1
		replace jvar = jvar+0.01 if treatment==0
		
		foreach var in `outcomelist' {
			
			local clockpos = 7
			local ylabel ""
			
			if "`var'"=="time" {
				local ytitle "Self-Reported Time Spent (Minutes)"
				local coefpos0 "34 1.2"
				local coefpos1 "32.5 1.2"
			}
			if "`var'"=="finalword" {
				local ytitle "Final Word Count"
				local coefpos0 "450 1.5"
				local coefpos1 "425 1.5"
			}
			if "`var'"=="active" {
				local ytitle "Minutes Active in Text Box"
				local coefpos0 "18 1.5"
				local coefpos1 "16.5 1.5"
			}
			if "`var'"=="overall" | "`var'"=="content" | "`var'"=="writing" | "`var'"=="originality" {
				local ytitle "Mean Grade"
				local coefpos0 "4.9 1.3"
				local coefpos1 "4.8 1.3"
				
				if "`loop'"=="yesexact" {
					local coefpos0 "5.25 1"
				local coefpos1 "5 1"
				}
				local clockpos = 11
			}
			if "`var'"=="productivity" {
				local ytitle "Earnings Per Minute"
				local coefpos0 "0.65 1"
				local coefpos1 "0.6 1"
				local clockpos = 11
			}
			if "`var'"=="jobsatisfaction" {
				local ytitle "Job Satisfaction (1-10)"
				if "`loop'"=="all" local ytitle "Job Satisfaction (SDs)"
				local coefpos0 "7.4 0.75"
				local coefpos1 "7.2 0.75"
				
				if "`loop'"=="all" {
					local coefpos0 "0.32 0.75"
					local coefpos1 "0.27 0.75"
					* 0.27
					
					local ylabel `" -0.4 "-.4" -0.2 "-.2" 0 "0" 0.2 ".2" 0.4 ".4" "'
				}
			}
			if "`var'"=="selfefficacy" {
				local ytitle "Self-Efficacy (1-10)"
				if "`loop'"=="all" local ytitle "Self-Efficacy (SDs)"
				local coefpos0 "0.24 0.8"
				local coefpos1 "0.21 0.8"
			}
			
		
			
			
			
			twoway (connected `var' jvar if treatment==1, color("`green'") msymbol(triangle) msize(large)) (rcap high_`var' low_`var' jvar if treatment==1, color("`green'")) ///
				(connected `var' jvar if treatment==0, color(black) msymbol(square_hollow) msize(large) lpattern(dash)) (rcap high_`var' low_`var' jvar if treatment==0, color(black)) ///
				, xscale(range(0.5 2.5)) xlabel(1 "Pre-Treatment" 2 "Post-Treatment", labsize(large)) legend(order(1 "Treated" 3 "Control") region(lstyle(none)) cols(1) ring(0) position(`clockpos') size(large)) ///
				xtitle("") ytitle(`ytitle', size(large)) ylabel(`ylabel',labsize(medlarge) angle(0)) 
			graph export "$outfolder/app_sc_did_`var'_`loop'.pdf", replace
			
			
		
		}
		
	restore
	

}


********************************************************************************
******************** Alternative Versions of Inequality figure *****************
********************************************************************************


foreach loop in nomanagers nodanalysts onlyexperienced {
	
	*** inequality analysis: since at least one of the axes is always grades, we do this at the participant-grader level
	foreach yvar in overall2 { // looping over dependent variables
		
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
			
			
			if "`loop'"=="nomanagers" {
				local sample "nomanagers"
				keep if incentive_arm!="exact" & occupation!="manager"
			}
			if "`loop'"=="nodanalysts" {
				local sample "nodanalysts"
				keep if incentive_arm!="exact" & occupation!="data analyst"
			}
			if "`loop'"=="onlyexperienced" {
				local sample "onlyexp"
				keep if task_experience==1
			}
			
						
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
				graph export "$outfolder/app_sc_inequality_`titlesuff1'`titlesuff2'_`sample'.pdf", replace
					
				restore
				
				
				
		}
	}

}



********************************************************************************
************************** How did you use ChatGPT? ****************************
********************************************************************************


use "$intermediate/analysis_clean.dta", clear

tab chatgpt_aware
tab chatgpt_use

replace usedgpt_first = 2 if usedgpt_first==. // this elicitation assumes people who used it on the first task also did so on the second
tab usedgpt_first

gen usedfirst_flag = 0
gen usedfirst_flag_t = .
forvalues n = 2/20 {
	local m = `n'-1
	replace usedfirst_flag = 1 if word_count_pre_`n'>word_count_pre_`m'+200 & word_count_pre_`n'!=.
	replace usedfirst_flag_t = `n' if word_count_pre_`n'>word_count_pre_`m'+200 & word_count_pre_`n'!=.
}

tab usedgpt_first

tab usedgpt if treatment==1

preserve

	keep if usedgpt==1
	
	forvalues n = 1/6 {
		gen used`n' = strpos(used,"`n'")>0 if used!=""
	}
	
	collapse (mean) used1 used2 used3 used4 used5 used6
	
	gen ivar = 1
	reshape long used, i(ivar) j(jvar)
	
	replace used = used*100
	
	twoway (bar used jvar, horizontal fcolor("`green'") bcolor(black)), xscale(range(0 100)) xlabel(0(20)100) xtitle("Percent of Users") ///
		ylabel(1 "Brainstorming" 2 "Write Draft, Submit" 3 "Write Draft, Edit" 4 "Summarize Text" 5 "Write Draft, Ask GPT to Edit" 6 "Edit/Simplify My Work", angle(360)) ///
		ytitle("")
	graph export "$outfolder/usage_bar.pdf", replace
	
	
restore
