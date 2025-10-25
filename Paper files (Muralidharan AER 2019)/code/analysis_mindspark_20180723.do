// ****************** ANALYZE MINDSPARK DATA ****************** //

/// authors: abhijeet singh & alejandro ganimian 

/// date: june 18, 2018

/// objectives:

	/// tables
	
		* 1. sample descriptives and balance on observables
		* 2. intent-to-treat effects in a regression framework
		* 3. treatment effect by specific competence assessed
		* 4. heterogeneity in treatment effect by sex, ses and baseline score
		* 5. heterogeneity in treatment effect by within-grade terciles
		* 6. treatment effect on items linked to grade levels
		* 7. treatment effect on school exams
		* 8. dose-response of mindspark attendance
				
		* a1. comparing pre-program exam results of study participants and non-participants
		* a2. itt estimates with within-grade normalized test scores
		* a3. heterogeneous effects on school tests, by terciles of baseline achievement
		* a4. heterogeneous effects on independent tests, by terciles of baseline achievement
		* a5. correlates of attendance
		* a6. quadratic dose-response relationship
		* a7. dose-response of subject-specific mindspark attendance
		* a8. itt estimates with inverse probability weighting
		* a9. lee bounds estimates of ITT effects	
		* a10. itt estimates, by source of test item
		* a11. treatment effect on take-up of other private tutoring	
			
	/// figures
	
		* 1. assessed levels of student achievement vs. current grade enrolled in school
		* 2. mean difference in test scores between lottery winners and losers
		* 3. non-parametric investigation of treatment effects by baseline percentiles
		* 4. dose response relationship
		* 5. precise customization of instruction by the mindspark cal program
		* 6. dynamic updating and individualization of content in mindspark
		
		* a1. comparing pre-program achievement of study participants and non-participants
		* a2. distribution of take-up among lottery-winners
		* a3. growth in achievement in treatment and control groups
		* a4. comparison of mindspark initial assessment of grade-level of student achievement with (independent) baseline test scores
		* a5. distribution of questions administered by mindspark cal system
		* a6. composition of group instruction batches in mindspark centers
		* a7. learning trajectories of individual students in the treatment group
	
		* d1. distribution of raw percentage correct scores
		* d2. distribution of irt scores, by round and treatment status
		* d3. item characteristic curves: hindi
		* d4. item characteristic curves: math
			
// ************************************************* //

clear all
set more off
set seed 12345
version 13.1

global	establishglobals 	"1"
global	gentables			"0"
global	gengraphs			"1"
global	plotmathiccs		"0" 
*ajg: takes a long time to run
global	plothindiiccs		"0"
*ajg: takes a long time to run

// ****************** ESTABLISH GLOBALS ****************** //

if $establishglobals==1 {

/// directory globals

	if c(username) == "ab.4351" {  
		global	research	/Users/ab.4351/Dropbox/
		global	projects	${research}Projects_in_pipeline/
		global	mindspark			${projects}Mindspark/
	}

	if c(username) == "alejandroganimian" {
		global	research	/Users/alejandroganimian/Dropbox/1_Research/
		global	projects	${research}Projects/Ongoing/
		global	ms			${projects}Mindspark_Study/
	}
	z	
///	folder globals

	global	bl			${mindspark}3_Data_processing/Baseline/
	global	el			${mindspark}3_Data_processing/Endline/
	global	ms			${mindspark}3_Data_processing/Mindspark/
	global	sc			${mindspark}3_Data_processing/Schools/
	global	hh			${mindspark}3_Data_processing/Household/
	
	global	graphs		${mindspark}4_Data_analysis/Graphs/
	global	tables		${mindspark}4_Data_analysis/Tables/

///	subfolder globals

	global	bl_temp		${bl}Temp/
	global	bl_clean	${bl}Clean/
	
	global	el_temp		${el}Temp/
	global	el_clean	${el}Clean/
	
	global	ms_temp		${ms}Temp/
	global	ms_clean	${ms}Clean/
	
	global	sc_temp		${sc}Temp/
	global	sc_clean	${sc}Clean/
	
	global	hh_temp		${hh}Temp/
	global	hh_clean	${hh}Clean/
		
}
// ****************** GENERATE TABLES ****************** //

if $gentables==1 {

/// table 1: sample descriptives and balance on observables
	
	/// load j-pal data long

		use ${el_clean}ms_blel_jpal_long, clear
	
	///	create table
	
		estpost ttest st_female st_age ses d_sch_grade* m_theta_mle ///
		h_theta_mle in_r2 if round==1, by(control)
		esttab . using "$tables/table1a.csv", replace wide nostar ///
		cells(" mu_1 (fmt(2)) mu_2 (fmt(2)) b(fmt(2)) se(fmt(2)) N_1(fmt(0)) N_2(fmt(0))")
		
	///	create table w/only those present at endline
	
		estpost ttest st_female st_age ses d_sch_grade* m_theta_mle ///
		h_theta_mle if round==1 & in_r2==1, by(control)
		esttab . using "$tables/table1b.csv", replace nostar ///
		cells(" mu_1 (fmt(2)) mu_2 (fmt(2)) b(fmt(2)) se(fmt(2)) N_1(fmt(0)) N_2(fmt(0))") 
	
/// table 2: intent-to-treat effects in a regression framework
		
	/// load j-pal data wide

		use ${el_clean}ms_blel_jpal_wide, clear
		
	///	relabel vars
	
		lab var m_theta_mle1	"Baseline score"
		lab var h_theta_mle1	"Baseline score"
		
	/// run regressions
		
		reg m_theta_mle2 treat m_theta_mle1, robust
		outreg2 using ${tables}table2.xls, label less(1) replace noaster

		reg h_theta_mle2 treat h_theta_mle1, robust
		outreg2 using ${tables}table2.xls, label less(1) append noaster
		
		xtreg m_theta_mle2 treat m_theta_mle1, robust  i(strata) fe
		outreg2 using ${tables}table2.xls, label less(1) append noaster
		
		xtreg h_theta_mle2 treat h_theta_mle1, robust i(strata) fe
		outreg2 using ${tables}table2.xls, label less(1) append noaster
	
///	table 3: treatment effect by specific competence assessed

	/// table 3a: math
	
		/// load j-pal data long

			use ${el_clean}ms_blel_jpal_long, clear
				
		///	merge with % correct data
			
			merge 1:1 st_id round using ${el_clean}ms_blel_forirt, ///
			keep(master match) keepus(cm*) nogen	
						
		///	keep key vars
			
			keep st_id round strata cm* m_theta_mle h_theta_mle treat
				
		///	reshape wide
			
			reshape wide h_theta* m_theta* cm*, i(st_id strata) j(round)
			
		///	run regressions
		
			xtreg cm_arithmetic2 treat m_theta_mle1, robust i(strata) fe
			outreg2 using ${tables}table3a.xls, label less(1) replace noaster
			
			foreach v in cm_word_compute2 cm_data2 cm_fraction2 cm_geometry2 ///
			cm_number2 cm_patterns2 {			
				xtreg `v' treat m_theta_mle1, robust i(strata) fe
				outreg2 using ${tables}table3a.xls, label less(1) append noaster
			}

	/// table 3b: hindi
	
		/// load j-pal data long

			use ${el_clean}ms_blel_jpal_long, clear
				
		///	merge with % correct data
			
			merge 1:1 st_id round using ${el_clean}ms_blel_forirt, ///
			keep(master match) keepus(ch*) nogen	
						
		///	keep key vars
			
			keep st_id round strata ch* m_theta_mle h_theta_mle treat
				
		///	reshape wide
			
			reshape wide h_theta* m_theta* ch*, i(st_id strata) j(round)
		
			xtreg ch_sentence2 treat h_theta_mle1, robust i(strata) fe
			outreg2 using ${tables}table3b.xls, label less(1) replace noaster
			
			foreach v in ch_retrieve2 ch_inference2 ch_interpret2 {			
				xtreg `v' treat h_theta_mle1, robust i(strata) fe
				outreg2 using ${tables}table3b.xls, label less(1) append noaster
			}
				
///	table 4: heterogeneity in treatment effect by sex, ses and baseline score
	
	/// load j-pal data wide

		use ${el_clean}ms_blel_jpal_wide, clear
	
	/// gen interactions
		
		gen treat_fem=treat*st_fe
		gen treat_ses=treat*ses
		gen treat_lmath=treat*m_theta_mle1
		*ajg: 2 students w/o endline math scores
		gen treat_lhindi=treat*h_theta_mle1
		*ajg: 2 students w/o endline hindi scores

	///	label interactions	
			
		lab var treat_fem "Treatment * Female"
		lab var treat_ses "Treatment * SES index"
		lab var treat_lmath "Treatment * Baseline math score"
		lab var treat_lhindi "Treatment * Baseline Hindi score"

	///	run regressions
		
		xtreg m_theta_mle2 treat st_fe treat_fem m_theta_mle1, robust i(strata) fe
		outreg2 using ${tables}table4.xls, label less(1) replace noaster
			
		xtreg h_theta_mle2 treat st_fe treat_fem h_theta_mle1, robust i(strata) fe
		outreg2 using ${tables}table4.xls, label less(1) append noaster

		xtreg m_theta_mle2 treat ses treat_ses m_theta_mle1, robust i(strata) fe
		outreg2 using ${tables}table4.xls, label less(1) append noaster
			
		xtreg h_theta_mle2 treat ses treat_ses h_theta_mle1, robust i(strata) fe
		outreg2 using ${tables}table4.xls, label less(1) append noaster
		
		xtreg m_theta_mle2 treat treat_lmath m_theta_mle1, robust i(strata) fe
		outreg2 using ${tables}table4.xls, label less(1) append noaster
			
		xtreg h_theta_mle2 treat treat_lhind h_theta_mle1, robust i(strata) fe
		outreg2 using ${tables}table4.xls, label less(1) append noaster

///	table 5: heterogeneity in treatment effect by within-grade terciles
	
	/// load j-pal data wide

		use ${el_clean}ms_blel_jpal_wide, clear
	
	/// gen tercile dummies
		
		tab terc_math, gen(d_m_terc)
		tab terc_hind,gen(d_h_terc)	

	///	gen and label interactions with treat dummies
		
		forval i = 1/3{
			foreach j in m h {
				gen terc_t`i'`j' = d_`j'_terc`i'*treat
			}
			lab var terc_t`i'm "Treatment*Tercile `i' (Math)"
			lab var terc_t`i'h "Treatment*Tercile `i' (Hindi)"
		}

	///	save tempfile
	
		tempfile tfile1
		save `tfile1'
		
	///	run regressions
		
		shell rm ${tables}table5.xls
			
		reg m_theta_mle2 treat terc_t2m terc_t3m d_m_terc1 d_m_terc2 ///
		d_m_terc3 m_theta_mle1, nocons robust 
		outreg2 using ${tables}table5.xls, label less(1) replace noaster
			
		reg h_theta_mle2 treat terc_t2h terc_t3h d_h_terc1 d_h_terc2 ///
		d_h_terc3 h_theta_mle1, nocons robust
		outreg2 using ${tables}table5.xls, label less(1) append noaster
	
///	table 6: treatment effect on items linked to grade levels
	
	/// load j-pal data long

		use ${el_clean}ms_blel_jpal_long, clear
			
	///	merge with % correct data	
		
		merge 1:1 st_id round using ${el_clean}ms_blel_forirt, ///
		keep(master match) keepus(m_at m_below h_at h_below) nogen
	
	///	copy baseline scores to round 2
	
		foreach s in m h {
			g `s'_theta1=`s'_theta_mle if round==1
			bys st_id: egen `s'_theta_mle1=min(`s'_theta1)
		}

	///	copy terciles to round 2
	
		foreach s in math hindi {
			bys st_id:	egen terc_`s'_1=min(terc_`s'1)
			drop terc_`s'1
		}
		*ajg: this won't be used until table a.3
		
	///	drop baseline obs
		
		drop if round==1
	
	///	save tempfile
	
		tempfile tfile2
		save `tfile2'
	
	/// run regressions			
		
		reg m_at treat m_theta_mle1 if st_grade~=., robust
		outreg2 using "$tables/table6.xls", noas replace label less(1)
		
		reg m_below_grade treat m_theta_mle1 if st_grade~=., robust
		outreg2 using "$tables/table6.xls", noas append label less(1)
		
		reg h_at treat h_theta_mle1 if st_grade~=., robust
		outreg2 using "$tables/table6.xls", noas append label less(1)
		
		reg h_below_g treat h_theta_mle1 if st_grade~=., robust
		outreg2 using "$tables/table6.xls", noas append label less(1)

///	table 7: treatment effect on school exams

	/// load school results
	
		use ${sc_clean}sc_results, clear
		
	///	keep 2015-2016 school year
	
		keep if year=="2015-16"
		
	///	group by school-by-class combination
	
		egen group=group(schoolid class)
	
	///	standardize scores within each class
	
		foreach s in math science social {
			bys group: egen mean_`s'_c=mean(`s'_term2_sa2) if treat==0
			bys group: egen sd_`s'_c = sd(`s'_term2_sa2) if treat==0
			bys group: egen mean_`s' = min(mean_`s'_c)
			bys group: egen sd_`s' = min(sd_`s'_c)
			gen z_`s'=(`s'_term2_sa2 - mean_`s')/sd_`s'
		}
		
		bys group: egen mean_hindi_c=mean(lang2_term2_sa2) if treat==0
		bys group: egen sd_hindi_c = sd(lang2_term2_sa2) if treat==0
		bys group: egen mean_hindi = min(mean_hindi_c)
		bys group: egen sd_hindi = min(sd_hindi_c)
		gen z_hindi=(lang2_term2_sa2 - mean_hindi)/sd_hindi

		bys group: egen mean_english_c=mean(lang1_term2_sa2) if treat==0
		bys group: egen sd_english_c = sd(lang1_term2_sa2) if treat==0
		bys group: egen mean_english = min(mean_english_c)
		bys group: egen sd_english = min(sd_english_c)
		gen z_english=(lang1_term2_sa2 - mean_english)/sd_english

		egen z_aggregate=rowmean(z_math z_hindi z_science z_social z_english)	
	
	///	drop students without treatment group
	
		drop if treat==.
		
	///	rename child id
	
		ren childid st_id
		
	///	keep key vars
	
		keep st_id-schoolid z_* finalresult group 
			
	/// merge with j-pal data wide

		mer 1:m st_id using ${el_clean}ms_blel_jpal_wide
	
	///	save tempfile
	
		tempfile tfile3
		save `tfile3'
	
	///	run regressions
	
		xtreg z_hindi treat h_theta_mle1 i.schoolid i.class_m, i(strata) fe robust
		outreg2 using "$tables/table7.xls", label dec(3) ///
		replace keep(treat h_theta_mle1) noaster
		
		xtreg z_math treat m_theta_mle1 i.schoolid i.class_m, i(strata) fe robust
		outreg2 using "$tables/table7.xls", label dec(3) ///
		append keep(treat m_theta_mle1) noaster

		xtreg z_science treat m_theta_mle1 h_theta_mle1 i.schoolid i.class_m, i(strata) fe robust
		outreg2 using "$tables/table7.xls", label dec(3) ///
		append keep(treat h_theta_mle1 m_theta_mle1) noaster

		xtreg z_social treat m_theta_mle1 h_theta_mle1 i.schoolid i.class_m, i(strata) fe robust
		outreg2 using "$tables/table7.xls", label dec(3) ///
		append keep(treat h_theta_mle1 m_theta_mle1) noaster
		
		xtreg z_english treat m_theta_mle1 h_theta_mle1 i.schoolid i.class_m, i(strata) fe robust
		outreg2 using "$tables/table7.xls", label dec(3) ///
		append keep(treat h_theta_mle1 m_theta_mle1) noaster

		xtreg z_aggregate treat m_theta_mle1 h_theta_mle1 i.schoolid i.class_m, i(strata) fe robust
		outreg2 using "$tables/table7.xls", label dec(3) ///
		append keep(treat h_theta_mle1 m_theta_mle1) noaster

///	table 8: heterogeneous effects on school tests, by terciles of baseline achievement

	/// load previously-saved temp dataset
	
		use `tfile3', clear

	/// gen tercile dummies
		
		tab terc_math, gen(d_m_terc)
		tab terc_hind, gen(d_h_terc)	

	///	gen and label interactions with treat dummies
		
		forval i = 1/3{
			foreach j in m h {
				gen terc_t`i'`j' = d_`j'_terc`i'*treat
			}
			lab var terc_t`i'm "Treatment*Tercile `i' (Math)"
			lab var terc_t`i'h "Treatment*Tercile `i' (Hindi)"
		}
		
	///	run regressions
	
		xtreg z_hindi treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 i.schoolid i.class_, i(strata) fe robust
		outreg2 using "$tables/table8.xls", label less(1) replace keep(treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1) noaster
		
		test treat=0
		test treat + terc_t2h=0
		test treat + terc_t3h=0

		xtreg z_math treat terc_t2m terc_t3m d_m_terc2 ///
		d_m_terc3 m_theta_mle1  i.schoolid i.class_, i(strata) fe robust
		outreg2 using "$tables/table8.xls", label less(1) append keep(treat terc_t2m terc_t3m d_m_terc2 ///
		d_m_terc3 m_theta_mle1) noaster
		
		test treat = 0
		test treat + terc_t2m=0
		test treat + terc_t3m=0

		xtreg z_science treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1  m_theta_mle1 i.schoolid i.class_, i(strata) fe robust
		outreg2 using "$tables/table8.xls", label less(1) append keep(treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 m_theta_mle1) noaster
		test treat=0
		test treat + terc_t2h=0
		test treat + terc_t3h=0

		xtreg z_social treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 m_theta_mle1 i.schoolid i.class_, i(strata) fe robust
		outreg2 using "$tables/table8.xls", label less(1) append keep(treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 m_theta_mle1) noaster
		test treat=0
		test treat + terc_t2h=0
		test treat + terc_t3h=0
		
		xtreg z_eng treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 m_theta_mle1 i.schoolid i.class_, i(strata) fe robust
		outreg2 using "$tables/table8.xls", label less(1) append keep(treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 m_theta_mle1) noaster
		test treat=0
		test treat + terc_t2h=0
		test treat + terc_t3h=0


		xtreg z_aggreg treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 m_theta_mle1 i.schoolid i.class_, i(strata) fe robust
		outreg2 using "$tables/table8.xls", label less(1) append keep(treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 m_theta_mle1) noaster
		test treat + terc_t2h=0
		test treat + terc_t3h=0
		
/// table 9: dose-response of mindspark attendance
	
	/// load sgi data
	
		use ${ms_clean}ms_ei_sgi, clear
							
	///	drop duplicates
		
		duplicates drop st_id, force
		/* ajg: multiple obs per st_id due to individual attendance data,
		dropping here because i only need aggregate attendance data */	
			
	///	keep key vars
		
		keep st_id att_tot
			
	/// merge with j-pal data wide

		mer 1:1 st_id using ${el_clean}ms_blel_jpal_wide
			
	///	impute 0 attendance for control children
		
		replace att_tot=0 if treat==0
	
	///	save tempfile
	
		tempfile tfile4
		save `tfile4'
	
	///	run regressions
		
		xtivreg2 m_theta_mle2 (att_tot=treat) m_theta_mle1, robust ///
		i(strata) fe endog(att_tot)
		outreg2 using ${tables}table9.xls, label less(1) ///
		replace adds(Angrist-Pischke F-statistic for weak instrument, ///
		e(widstat), Difference-in-Sargan statistic for exogeneity ///
		(p-value), e(estat p)) noaster
			
		xtivreg2 h_theta_mle2 (att_tot=treat) h_theta_mle1, robust ///
		i(strata) fe endog(att_tot)
		outreg2 using ${tables}table9.xls, label less(1) append ///
		adds(Angrist-Pischke F-statistic for weak instrument, ///
		e(widstat), Difference-in-Sargan statistic for exogeneity ///
		(p-value), e(estat p)) noaster
	
		reg m_theta_mle2 att_tot m_theta_mle1, robust 
		outreg2 using ${tables}table9.xls, label less(1) append noaster
			
		reg h_theta_mle2 att_tot h_theta_mle1, robust
		outreg2 using ${tables}table9.xls, label less(1) append noaster
	
		reg m_theta_mle2 att_tot m_theta_mle1 if treat==1, robust 
		outreg2 using ${tables}table9.xls, label less(1) append noaster
	
		reg h_theta_mle2 att_tot h_theta_mle1 if treat==1, robust 
		outreg2 using ${tables}table9.xls, label less(1) append noaster
		
/// table a.1: comparing pre-program achievement of study participants and non-participants

	/// load school results
	
		use ${sc_clean}sc_results, clear

	///	gen dummy for non-rct students
		
		g nonrct=(treat==.)

	///	keep 2014-2015 school year

		keep if year=="2014-15"
	
	///	group by school-by-class combination

		egen group=group(schoolid class)
	
	///	standardize scores within each class
		 
		foreach s in math science social {
			bys group: egen mean_`s'_c=mean(`s'_ovrall_tot) 
			bys group: egen sd_`s'_c = sd(`s'_ovrall_tot) 
			bys group: egen mean_`s' = min(mean_`s'_c)
			bys group: egen sd_`s' = min(sd_`s'_c)
			gen z_`s'=(`s'_ovrall_tot - mean_`s')/sd_`s'
		}
		
		bys group: egen mean_hindi_c=mean(lang2_ovrall_tot) 
		bys group: egen sd_hindi_c = sd(lang2_ovrall_tot) 
		bys group: egen mean_hindi = min(mean_hindi_c)
		bys group: egen sd_hindi = min(sd_hindi_c)
		gen z_hindi=(lang2_ovrall_tot - mean_hindi)/sd_hindi

		bys group: egen mean_english_c=mean(lang1_ovrall_tot) 
		bys group: egen sd_english_c = sd(lang1_ovrall_tot) 
		bys group: egen mean_english = min(mean_english_c)
		bys group: egen sd_english = min(sd_english_c)
		gen z_english=(lang1_ovrall_tot - mean_english)/sd_english

		egen z_aggregate=rowmean(z_math z_hindi z_science z_social z_english)

		estpost ttest z_math z_hindi z_science z_social z_english, by(nonrct)
		esttab . using "$tables/tablea1.csv", nostar replace wide ///
		cells(" mu_1 (fmt(2)) mu_2 (fmt(2)) b(fmt(2)) se(fmt(2)) N_1(fmt(0)) N_2(fmt(0))")
	
/// table a.2: itt estimates with within-grade normalized test scores

	/// load j-pal data long

		use ${el_clean}ms_blel_jpal_long, clear

	///	standardize scores within each grade
			
		foreach v of varlist m_theta_mle h_theta_mle {
			gen sd_`v'=.
			gen mean_`v'=.
			forval i = 4/9{	
				sum `v' if st_grade1==`i' & round==1
				gen sd_`v'_`i'=r(sd)
				gen mean_`v'_`i'=r(mean)
				replace sd_`v' = sd_`v'_`i' if st_grade1==`i'
				replace mean_`v' = mean_`v'_`i' if st_grade1==`i'
			}
			replace `v'=((`v'-mean_`v')/sd_`v')
			drop mean_`v' sd_`v'
		}
	
	/// keep key vars
		
		keep st_female1 ses_index h_theta* m_theta*  quint* terc* per* ///
		st_tui_math st_tui_hindi st_tui_other st_tui_math_hrs st_id ///
		round treat strata
	
	/// reshape wide
		
		reshape wide h_theta* m_theta*  quint* terc* per* ///
		st_tui_math st_tui_hindi st_tui_other st_tui_math_hrs, ///
		i(st_id) j(round)
	
	/// drop unnecessary vars
			
		drop terc_math12 terc_hindi12 quint_math12 quint_hindi12
	
	/// relabel vars
		
		lab var m_theta_mle1 "Baseline math score"
		lab var h_theta_mle1 "Baseline Hindi score"
		lab var m_theta_mle2 "Endline math score"
		lab var h_theta_mle2 "Endline Hindi score"
		
	///	run regressions
	
		reg m_theta_mle2 treat m_theta_mle1, robust
		outreg2 using ${tables}tablea2.xls, label less(1) replace noaster

		reg h_theta_mle2 treat h_theta_mle1, robust
		outreg2 using ${tables}tablea2.xls, label less(1) append noaster
		
		xtreg m_theta_mle2 treat m_theta_mle1, robust  i(strata) fe
		outreg2 using ${tables}tablea2.xls, label less(1) append noaster
		
		xtreg h_theta_mle2 treat h_theta_mle1, robust i(strata) fe
		outreg2 using ${tables}tablea2.xls, label less(1) append noaster


		
///	table a.3:	heterogeneous effects on independent tests, by terciles of baseline achievement

	/// load previously-saved temp dataset

		use `tfile2', clear
			
	/// gen tercile dummies
		
		tab terc_math, gen(d_m_terc)
		tab terc_hind,gen(d_h_terc)	

	///	gen and label interactions with treat dummies
		
		forval i = 1/3{
			foreach j in m h {
				gen terc_t`i'`j' = d_`j'_terc`i'*treat
			}
			lab var terc_t`i'm "Treatment*Tercile `i' (Math)"
			lab var terc_t`i'h "Treatment*Tercile `i' (Hindi)"
		}

	///	run regressions
	
		reg m_at treat terc_t2m terc_t3m d_m_terc2 ///
		d_m_terc3 m_theta_mle1 if st_grade~=., robust
		outreg2 using "$tables/tablea3.xls", label less(1) replace noaster
		test treat=0
		test treat + terc_t2m=0
		test treat + terc_t3m=0	
		
		reg m_below_grade treat terc_t2m terc_t3m d_m_terc2 ///
		d_m_terc3 m_theta_mle1 if st_grade~=.,  robust
		outreg2 using "$tables/tablea3.xls", label less(1) append noaster
		test treat=0
		test treat + terc_t2m=0
		test treat + terc_t3m=0	
		
		reg h_at treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 if st_grade~=.,  robust
		outreg2 using "$tables/tablea3.xls", label less(1) append noaster
		test treat=0
		test treat + terc_t2h=0
		test treat + terc_t3h=0
		
		reg h_below treat terc_t2h terc_t3h d_h_terc2 ///
		d_h_terc3 h_theta_mle1 if st_grade~=., robust
		outreg2 using "$tables/tablea3.xls", label less(1) append noaster
		test treat=0
		test treat + terc_t2h=0
		test treat + terc_t3h=0

///	table a.4: correlates of attendance

	/// load previously-saved temp dataset

		use `tfile4', clear
		
	///	run regressions
			
		reg att_tot  st_female1 ses_index if treat==1, robust
		outreg2 using ${tables}tablea4.xls, label less(1) replace noaster
		
		reg att_tot  st_female1 ses_index m_theta_mle1 h_theta_mle1 ///
		if treat==1, robust
		outreg2 using ${tables}tablea4.xls, label less(1) append noaster
		
		reg att_tot  st_female1 ses_index st_tui_math1 st_tui_hindi1 ///
		m_theta_mle1 h_theta_mle1 if treat==1, robust
		outreg2 using ${tables}tablea4.xls, label less(1) append noaster
		
		areg att_tot  st_female1 ses_index st_tui_math1 st_tui_hindi1 ///
		m_theta_mle1 h_theta_mle1 if treat==1, robust absorb(st_grade1)
		outreg2 using ${tables}tablea4.xls, label less(1) append noaster

///	table a.5: quadratic dose-response relationship

	/// gen and label attendance squared
	
		replace att_tot=0 if treat==0
		gen att_sq = att_tot^2
		lab var att_sq "Attendance squared"
	
	///	run regressions
	
		reg m_theta_mle2 att_tot att_sq m_theta_mle1, robust 
		outreg2 using ${tables}tablea5.xls, label less(1) replace noaster
		
		reg h_theta_mle2 att_tot att_sq h_theta_mle1, robust
		outreg2 using ${tables}tablea5.xls, label less(1) append noaster
				
		reg m_theta_mle2 att_tot att_sq m_theta_mle1 if treat==1, robust 
		outreg2 using ${tables}tablea5.xls, label less(1) append noaster
		
		reg h_theta_mle2 att_tot att_sq h_theta_mle1 if treat==1, robust 
		outreg2 using ${tables}tablea5.xls, label less(1) append noaster		
		
///	table a.6: dose-response of subject-specific mindspark attendance

	/// load sgi data
	
		use ${ms_clean}ms_ei_sgi, clear
		
	/// gen mean attendance by date
	
		bys date: egen mean_attend=mean(present)
	
	/// drop dates in which everyone was absent
	
		drop if mean==.
		
	///	drop duplicates
		
		duplicates drop ms_id, force
		/* ajg: multiple obs per st_id due to individual attendance data,
		dropping here because i only need aggregate attendance data */	
	
	///	keep key vars
	
		keep ms_id ms_center att_tot* st_id att_per ///
		sgi_att_mat_tot sgi_att_hin_tot
	
	///	save tempfile
	
		tempfile tfile5
		save `tfile5'

	///	merge w/cal data
	
		mer 1:1 ms_id using ${ms_clean}ms_ei_cal
		*ajg: 46 not matched (0 from using)
		tab att_tot if _m!=3
		*ajg: these are students with little or no attendance
		
	///	replace cal attendance data for these students
	
		foreach v in cal_att_mat_tot cal_att_hin_tot {
			replace `v'=0 if _m!=3
		}
		drop _m
	
	///	save tempfile
	
		tempfile tfile6
		save `tfile6'
	
	///	merge w/jpal data
	
		mer 1:1 st_id using ${el_clean}ms_blel_jpal_wide, nogen
		*ajg 306 not matched (all from using) -- control students
	
	///	replace attendance for controls
	
		replace att_tot=0 if treat==0
		
	///	gen days of instruction by subject
	
		g att_mat_tot = (cal_att_mat_tot+sgi_att_mat_tot)/2
		g att_hin_tot = (cal_att_hin_tot+sgi_att_hin_tot)/2
		
		replace att_mat_tot=0 if treat==0
		replace att_hin_tot=0 if treat==0
		
		/* ajg: a math/hindi day is a full instruction session across cal+sgi.
		the sum of these is ~25% lower than days attended due to scheduled
		instruction in english, value education, parent-teacher mtgs., etc. */
		
		lab var att_mat_tot "Days of math instruction"
		lab var	att_hin_tot "Days of Hindi instruction"
	
	///	run regressions
		xtivreg2 m_theta_mle2 (att_mat_tot=treat) m_theta_mle1, ///
		robust i(strata) fe endog(att_mat_tot)
		
		outreg2 using ${tables}tablea6.xls, label less(1) replace noaster ///
		adds(Angrist-Pischke F-statistic for weak instrument, ///
		e(widstat), Difference-in-Sargan statistic for exogeneity (p-value), ///
		e(estat p))	
		
		xtivreg2 h_theta_mle2 (att_hin_tot=treat) h_theta_mle1, ///
		robust i(strata) fe endog(att_hin_tot)
		
		outreg2 using ${tables}tablea6.xls, label less(1) append noaster ///
		adds(Angrist-Pischke F-statistic for weak instrument, ///
		e(widstat), Difference-in-Sargan statistic for exogeneity (p-value), ///
		e(estat p))	

		reg m_theta_mle2 att_mat_tot m_theta_mle1, robust
		outreg2 using ${tables}tablea6.xls, label less(1) append noaster
			
		reg h_theta_mle2 att_hin_tot h_theta_mle1, robust
		outreg2 using ${tables}tablea6.xls, label less(1) append noaster
			
		reg m_theta_mle2 att_mat_tot m_theta_mle1 if treat==1, robust
		outreg2 using ${tables}tablea6.xls, label less(1) append noaster	

		reg h_theta_mle2 att_hin_tot h_theta_mle1 if treat==1, robust
		outreg2 using ${tables}tablea6.xls, label less(1) append noaster	

/// table a.7: itt estimates with inverse probability weighting

	/// load j-pal data long

		use ${el_clean}ms_blel_jpal_wide, clear

	///	gen dummy for having scores at bl and el
	
		gen in_el=(m_theta_mle2!=. & h_theta_mle2!=.)
		lab var in_el "In endline"

	///	gen center fe
	
		egen ms_center_d=group(ms_center1)
		
	///	run regressions
	
		reg in_el st_fe ses_index m_theta_mle1 h_theta_mle1 i.ms_center_d
	
		probit in_el st_fe ses_index m_theta_mle1 h_theta_mle1 ///
		i.ms_center_d, robust
		predict prob
		gen inv_prob=1/prob
	
		reg m_theta_mle2 treat m_theta_mle1 [pw=inv_prob], robust
		outreg2 using ${tables}tablea7.xls, label less(1) replace noaster

		reg h_theta_mle2 treat h_theta_mle1 [pw=inv_prob], robust
		outreg2 using ${tables}tablea7.xls, label less(1) append noaster
			
		areg m_theta_mle2 treat m_theta_mle1 [pw=inv_prob], robust ///
		absorb(strata)
		outreg2 using ${tables}tablea7.xls, label less(1) append noaster
			
		areg h_theta_mle2 treat h_theta_mle1 [pw=inv_prob], robust ///
		absorb(strata)
		outreg2 using ${tables}tablea7.xls, label less(1) append noaster

/// table a.8: lee bounds estimates of ITT effects

	/// run regressions
	
		reg m_theta_mle2 m_theta_mle1, robust
		predict m_res, res
		leebounds m_res treat,vce(analytic) tight(ms_center1) cie

		reg h_theta_mle2 h_theta_mle1, robust
		predict h_res, res
		leebounds h_res treat,vce(analytic) tight(ms_center1) cie
	
///	table a.9: itt estimates, by source of test item		
	
	/// load j-pal data long

		use ${el_clean}ms_blel_jpal_long, clear

	///	merge with % correct data
			
		merge 1:1 st_id round using ${el_clean}ms_blel_forirt, keep(master ///
		match) keepus(math_ei math_nonei hindi_ei hindi_nonei) nogen	
	
	///	copy baseline scores to round 2
	
		foreach s in m h {
			g `s'_theta1=`s'_theta_mle if round==1
			bys st_id: egen `s'_theta_mle1=min(`s'_theta1)
		}

	///	drop baseline obs
		
		drop if round==1
		
	///	run regressions
	
		xtreg math_ei treat m_theta_mle1, robust  i(strata) fe
		outreg2 using ${tables}tablea9.xls, label less(1) replace noaster
		
		xtreg math_nonei treat m_theta_mle1, robust  i(strata) fe
		outreg2 using ${tables}tablea9.xls, label less(1) append noaster
		
		xtreg hindi_ei treat h_theta_mle1, robust  i(strata) fe
		outreg2 using ${tables}tablea9.xls, label less(1) append noaster
		
		xtreg hindi_nonei treat h_theta_mle1, robust  i(strata) fe
		outreg2 using ${tables}tablea9.xls, label less(1) append noaster
		
/// table a.10: average itt effects on take-up of private tutoring

	/// load hh survey data
	
		use ${hh_clean}hh_survey, clear
	
	///	reshape long
	
		reshape long math_ hindi_ english_ science_ soc_sci_, i(st_id) j(month)
	
	///	drop empty obs
	
		drop if math_==.

	///	gen post-treat interaction
	
		gen post=month>3 & month~=.
		gen post_treat=post*treat
			
	///	group student ids
	
		egen id=group(st_id)
		
	///	run regressions
	
		xtreg math_ post post_treat, i(id) fe robust
		outreg2 using "$tables/tablea10.xls", label less(1) replace noaster
		
		xtreg hindi_ post post_treat, i(id) fe robust
		outreg2 using "$tables/tablea10.xls", label less(1) append noaster
		
		xtreg english_ post post_treat, i(id) fe robust
		outreg2 using "$tables/tablea10.xls", label less(1) append noaster
		
		xtreg science_ post post_treat, i(id) fe robust
		outreg2 using "$tables/tablea10.xls", label less(1) append noaster
		
		xtreg soc_sci_ post post_treat, i(id) fe robust
		outreg2 using "$tables/tablea10.xls", label less(1) append noaster
	
}
// ****************** GENERATE GRAPHS ****************** //

if $gengraphs==1 {

///	fig 1: assessed levels of student achievement vs. current grade enrolled in school

	/// load ms levels data

		use ${ms_clean}ms_levels, clear
		
	/// plot math graph
	
		graph twoway (scatter mathlevel class, mcolor(gray*0.8) ///
		msymbol(circle_hollow) msize(small) jitter(3)) ///
		(lfitci mathlevel class, clcolor(red) lcolor(red) ///
		clwidth(medthick)ciplot(rline) clpattern(solid) lpattern(dash) ///
		lwidth(thin)) (function y=x, range(5.5 9.5) lcolor(navy)) ///
		if class>5, xtitle (Grade enrolled in) ///
		ytitle(Assessed level of student achievement) ///
		title(Math) graphregion(fcolor(white) lcolor(white)) ///
		legend(order(3 "Linear fit" 4 "Line of equality")) ///
		name(base_actual_m, replace)
		
	///	plot hindi graph
	
		graph twoway (scatter  hindilevel class, mcolor(gray*0.8) ///
		msymbol(circle_hollow) msize(small) jitter(3)) ///
		(lfitci hindilevel class, clcolor(red) lcolor(red) ///
		clwidth(medthick)ciplot(rline) clpattern(solid) lpattern(dash) ///
		lwidth(thin)) (function y=x, range(5.5 9.5) lcolor(navy)) ///
		if class>5, xtitle(Grade enrolled in) ///
		ytitle(Assessed level of student achievement) ///
		title(Hindi) graphregion(fcolor(white) lcolor(white)) ///
		legend(order(3 "Linear fit" 4 "Line of equality")) ///
		name(base_actual_h, replace)

	///	combine graphs
	
		grc1leg base_actual_m base_actual_h, leg(base_actual_m) ///
		xcommon ycommon graphregion(fcolor(white) lcolor(white))
	
	///	export
	
		gr export ${graphs}fig1.png, replace as(png)
	
/// fig 2: mean difference in test scores between lottery winners and losers

	/// load j-pal data long

		use ${el_clean}ms_blel_jpal_long, clear

	/// math
	
		cibar m_theta_mle, over1(treat) over2(round) barlabel(on) ///
		blsize(small) gap(10) barcol(gs12 dknavy) /// 
		graphop(title("Mathematics") ytitle(Score) note("") ///
		yscale(range(-0.1 0.8)) graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "Control" 2 "Treatment")) name(fig2a,replace))
	
	/// hindi
	
		cibar h_theta_mle, over1(treat) over2(round) barlabel(on) ///
		blsize(small) gap(10) barcol(gs12 dknavy) /// 
		graphop(title("Hindi") ytitle(Score) note("") ///
		yscale(range(-0.1 0.6)) graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "Control" 2 "Treatment")) name(fig2b,replace))

	///	combine	graphs
			
		grc1leg fig2a fig2b, ///
		leg(fig2a) xcommon ycommon ///
		graphregion(fcolor(white) lcolor(white)) name(fig2, replace)		
		gr export ${graphs}fig2.png, replace as(png)
	
///	fig 3: non-parametric investigation of treatment effects by baseline percentiles

	/// load j-pal data wide

		use ${el_clean}ms_blel_jpal_wide, clear
	
	///	gen baseline percentiles
	
		xtile perc_m=m_theta_mle1, nq(100)
		xtile perc_h=h_theta_mle1, nq(100)
		
	///	math
		
		graph twoway (lpolyci m_theta_mle2 perc_m if treat==0, ///
		bwidth(10) lcolor(red) clcolor(red) clwidth(medthick)ciplot(rline) ///
		clpattern(longdash) lpattern(dash) lwidth(thin)) (lpolyci ///
		m_theta_mle2 perc_m if treat==1, bwidth(10) lcolor(blue) clcolor(blue) ///
		clwidth(medthick)ciplot(rline) clpattern(solid) lpattern(dash) ///
		lwidth(thin)), ytitle("Endline test score (SD)") ///
		xtitle("Percentile on baseline test") legend (order(2 "Control" ///
		4 "Treatment")) title("Mathematics") graphregion(fcolor(white) ///
		lcolor(white)) name(fig3a,replace)		
			
	/// hindi
		
		graph twoway (lpolyci h_theta_mle2 perc_h if treat==0, ///
		bwidth(10) lcolor(red) clcolor(red) clwidth(medthick)ciplot(rline) ///
		clpattern(longdash) lpattern(dash) lwidth(thin)) (lpolyci ///
		h_theta_mle2 perc_h if treat==1, bwidth(10) lcolor(blue) clcolor(blue) ///
		clwidth(medthick)ciplot(rline) clpattern(solid) lpattern(dash) ///
		lwidth(thin)), ytitle("Endline test score (SD)") ///
		xtitle("Percentile on baseline test") legend (order(2 "Control" 4 ///
		"Treatment")) title("Hindi") graphregion(fcolor(white) ///
		lcolor(white)) name(fig3b,replace)
	
	///	combine graphs
	
		grc1leg fig3a fig3b, leg(fig3a) xcommon ycommon ///
		graphregion(fcolor(white) lcolor(white))
		gr export ${graphs}fig3.png, replace as(png)
		
///	fig 4: growth in achievement in treatment and control groups
	
	/// load j-pal data wide

		use ${el_clean}ms_blel_jpal_wide, clear
	
	/// gen tercile dummies
		
		tab terc_math, gen(d_m_terc)
		tab terc_hind,gen(d_h_terc)	

	///	run regressions	
	
		reg m_theta_mle2 d_m_terc* m_theta_mle1 if treat==0 & st_grade1>5, ///
		nocons robust
		est store control_ter_m

		reg m_theta_mle2 d_m_terc* m_theta_mle1 if treat==1 & st_grade1>5, ///
		nocons robust
		est store treat_ter_m
		
		reg h_theta_mle2 d_h_terc* h_theta_mle1 if treat==0 & st_grade1>5, ///
		nocons robust
		est store control_ter_h

		reg h_theta_mle2 d_h_terc* h_theta_mle1 if treat==1 & st_grade1>5, ///
		nocons robust
		est store treat_ter_h
	
	///	plot fig 4a
	
		coefplot (control_ter_m, label(Control) mcolor(red) ///
		ciopts( lcolor(red) recast(rcap))) ///
		(treat_ter_m, label(Treatment) msymbol(D) mcolor(blue) ///
		ciopts(lcolor(blue) recast(rcap))), vertical drop(m_theta_mle1) ///
		yline(0) xtitle(Terciles) ///
		ytitle(Residualized scores) xlabel(1 "Bottom" 2 "Middle" 3 "Top") ///
		title("Math")  graphregion(fcolor(white) lcolor(white)) ///
		name(ter_m2,replace)
	
	///	plot fig 4b

		coefplot (control_ter_h, label(Control) mcolor(red) ///
		ciopts( lcolor(red) recast(rcap))) (treat_ter_h, label(Treatment) ///
		mcolor(blue) msymbol(D) ciopts( lcolor(blue) recast(rcap))), vertical ///
		drop(h_theta_mle1) yline(0) xtitle(Terciles) ciopts( recast(rcap)) ///
		ytitle(Residualized scores) xlabel(1 "Bottom" 2 "Middle" 3 "Top") ///
		title("Hindi")  graphregion(fcolor(white) lcolor(white)) ///
		name(ter_h2,replace)
	
	///	combine graphs

		grc1leg ter_m2 ter_h2 , ycommon graphregion(fcolor(white) lcolor(white)) 
		gr save ${graphs}fig4.gph, replace
		gr export ${graphs}fig4.png, replace as(png)		
	
	
///	fig 5: dose-response relationship

	/// load sgi data
	
		use ${ms_clean}ms_ei_sgi, clear
		
	///	drop duplicates
		
		duplicates drop st_id, force

	/// merge w/j-pal data wide

		mer 1:1 st_id using ${el_clean}ms_blel_jpal_wide, nogen

	///	replace attendance for controls
	
		replace att_tot=0 if treat==0
		
	///	plot math graph
	
		cmogram m_theta_mle2 att_tot if treat==1, lfitci  scatter ///
		histopts(width(10)) lowess controls(m_theta_mle1) legend ///
		graphopts(legend(label(1 "Binned mean") label(3 "Linear fit") ///
		label(2 "95% CI") label(4 "Lowess")) ///
		ytitle("Residualized test scores") title(Math) xtitle("Attendance (days)")) lowopt(bwidth(10) ///
		lcolor(navy) lpattern(dash) lwidth(medthick)) 
		graph save ${graphs}fig5a.gph, replace
		
	///	plot hindi graph
	
		cmogram h_theta_mle2 att_tot if treat==1, lfitci scatter ///
		histopts(width(10)) lowess controls(h_theta_mle1) legend ///
		graphopts(legend(label(1 "Binned mean") label(3 "Linear fit") ///
		label(2 "95% CI") label(4 "Lowess")) ///
		ytitle("Residualized test scores") title(Hindi) xtitle("Attendance (days)")) ///
		rcapopt(clpattern(dash) clwidth(thin)) lowopt(bwidth(10) ///
		lcolor(navy) lpattern(dash) lwidth(medthick)) 
		gr save ${graphs}fig5b.gph, replace
	
	///	combine graphs
	
		grc1leg ${graphs}fig5a.gph ${graphs}fig5b.gph, xcommon ///
		graphregion(fcolor(white) lcolor(white)) leg(${graphs}fig5a.gph)
		gr export ${graphs}fig5.png, replace as(png)
		
///	fig 6: precise customization of instruction by the mindspark cal program
///	fig 7: dynamic updating and individualization of content in Mindspark

	*ajg: doing these two figures together bc they draw on the same data

	/// load ms levels data

		use ${ms_clean}ms_levels, clear

	///	drop duplicates

		duplicates drop ms_id, force
	
	///	merge with ms roster
	
		mer 1:1 ms_id using ${ms_clean}ms_roster, nogen keep(match)
	
	///	merge with ms math questions
	
		mer 1:m ms_id using ${ms_clean}ms_mathqs, nogen keep(match)
	
	///	gen ms levels
	
		gen ms_q_level=1 if q_grade=="1"
	
		forval i = 2/8{
			replace ms_q_level = `i' if q_grade=="`i'"
		}

		forval i=1/8{
			local j = `i'+ 1 
			replace ms_q_level = (`i' + `j')/2 if q_grade=="`i',`j'"
		}
		
		replace ms_q_ =2 if q_grade=="1,2,3"
		replace ms_q_ =7 if q_grade=="6,7,8"

	///	re-label mathlevel
		
		lab var mathlevel "Math level assessed by CAL system at baseline"
	
	/// plot fig 6a
	
		twoway (scatter ms_q_le mathlevel, jitter(2) msize(tiny)) if ///
		(q_date==20395) & class>5, ///
		by(class, note(" ") title(Math) ///
		graphregion(fcolor(white) lcolor(white))) ///
		ytitle(Grade level of question administered by CAL) ///
		xlabel(1 2 3 4 5 6 7 8) name(tiger_paws_m, replace) 
	
	/// plot fig 7a
	
		graph twoway (lpolyci ms_q_ q_date if class==6, lcolor(red) ///
		clcolor(red) clwidth(medthick)ciplot(rline) clpattern(solid) ///
		lpattern(dash) lwidth(thin)) (lpolyci ms_q_ q_date if class==7, ///
		lcolor(blue) clcolor(blue) clwidth(medthick)ciplot(rline) ///
		clpattern(solid) lpattern(longdash) lwidth(thin)) (lpolyci ms_q_ ///
		q_date if class==8, lcolor(green) clcolor(green) ///
		clwidth(medthick)ciplot(rline) clpattern(solid) lpattern(shortdash) ///
		lwidth(thin)) (lpolyci ms_q_ q_date if class==9, lcolor(black) ///
		clcolor(black) clwidth(medthick)ciplot(rline) clpattern(solid) ///
		lpattern(solid) lwidth(thin)) if q_date>20392 & q_date<20511, ///
		legend(order(2 "Grade 6" 4 "Grade 7" 6 "Grade 8" 8 "Grade 9")) ///
		xtitle(Date) xlabe(20393 "Nov 1" 20422 "Dec 1" 20454 "Jan1" ///
		20485 "Feb 1") graphregion(fcolor(white) lcolor(white)) ///
		ytitle(Grade level of question administered by CAL) ///
		title("Enrolled Grade") ///
		name(ei_grade, replace)

	/// plot fig 7b
	
		graph twoway (lpolyci ms_q_ q_date if mathlevel==2, lcolor(red) ///
		clcolor(red) clwidth(medthick)ciplot(rline) clpattern(dash) ///
		lpattern(dash) lwidth(thin)) (lpolyci ms_q_ q_date if mathlevel==3, ///
		lcolor(blue) clcolor(blue) clwidth(medthick)ciplot(rline) ///
		clpattern(longdash) lpattern(longdash) lwidth(thin)) (lpolyci ms_q_ ///
		q_date if mathlevel==4, lcolor(green) clcolor(green) ///
		clwidth(medthick)ciplot(rline) clpattern(shortdash) ///
		lpattern(shortdash) lwidth(thin)) (lpolyci ms_q_ q_date if ///
		mathlevel==5, lcolor(black) clcolor(black) ///
		clwidth(medthick)ciplot(rline) clpattern(solid) lpattern(solid) ///
		lwidth(thin)) (lpolyci ms_q_ q_date if mathlevel==6, lcolor(brown) ///
		clcolor(brown) clwidth(medthick)ciplot(rline) clpattern(dash_dot) ///
		lpattern(dash_dot) lwidth(thin)) if q_date>20392 & ///
		q_date<20511, legend(order(2 "Grade 2" 4 "Grade 3" 6 "Grade 4" ///
		8 "Grade 5" 10 "Grade 6")) xtitle(Date) xlabe(20393 "Nov 1" 20422 ///
		"Dec 1" 20454 "Jan1" 20485 "Feb 1") graphregion(fcolor(white) ///
		lcolor(white)) ytitle(Grade level of question administered by CAL) ///
		title("Baseline achievement level") ///
		name(ei_actual, replace)
		
	///	combine fig7a & 7b
	
		graph combine ei_grade ei_actual, xcommon ycommon ///
		graphregion(fcolor(white) lcolor(white)) ///
		name(het_qdiff, replace) title(Math)
		gr export ${graphs}fig7.png, replace as(png)
		
	/// load ms levels data

		use ${ms_clean}ms_levels, clear

	///	drop duplicates

		duplicates drop ms_id, force
	
	///	merge with ms roster
	
		mer 1:1 ms_id using ${ms_clean}ms_roster, nogen keep(match)
	
	///	merge with ms hindi questions
	
		mer 1:m ms_id using ${ms_clean}ms_hindiqs, nogen keep(match)
		
	///	gen ms levels

		destring q_grade, g(hindi_q_level)

	///	re-label hindilevel
		
		lab var hindilevel "Hindi level assessed by CAL system at baseline"

	///	format date of question being attempted
	
		format q_date %td

	/// plot fig 6b
	
		twoway (scatter hindi_q_level hindilevel, ///
		yscale(range(1 9)) jitter(3) msize(tiny)) if ///
		(q_date==20391) & class>5, ///
		by(class, note(" ") title(Math) ///
		graphregion(fcolor(white) lcolor(white))) ///
		ytitle(Grade level of question administered by CAL) ///
		xlabel(1 2 3 4 5 6 7 8 9) name(tiger_paws_h, replace) 

	///	combine fig6a & 6b
		
		grc1leg tiger_paws_m tiger_paws_h, leg(tiger_paws_m) ///
		xcommon ycommon graphregion(fcolor(white) lcolor(white)) 
		gr export ${graphs}fig6.png, replace as(png)
		
///	fig a.1: comparing pre-program achievement of study participants and non-participants		

	/// load school results
	
		use ${sc_clean}sc_results, clear

	///	gen dummy for rct students
		
		g rct=(treat!=.)

	///	keep 2014-2015 school year

		keep if year=="2014-15"
	
	///	group by school-by-class combination

		egen group=group(schoolid class)
	
	///	standardize scores within each class
		 
		foreach s in math science social {
			bys group: egen mean_`s'_c=mean(`s'_ovrall_tot) 
			bys group: egen sd_`s'_c = sd(`s'_ovrall_tot) 
			bys group: egen mean_`s' = min(mean_`s'_c)
			bys group: egen sd_`s' = min(sd_`s'_c)
			gen z_`s'=(`s'_ovrall_tot - mean_`s')/sd_`s'
		}
		
		bys group: egen mean_hindi_c=mean(lang2_ovrall_tot) 
		bys group: egen sd_hindi_c = sd(lang2_ovrall_tot) 
		bys group: egen mean_hindi = min(mean_hindi_c)
		bys group: egen sd_hindi = min(sd_hindi_c)
		gen z_hindi=(lang2_ovrall_tot - mean_hindi)/sd_hindi

		bys group: egen mean_english_c=mean(lang1_ovrall_tot) 
		bys group: egen sd_english_c = sd(lang1_ovrall_tot) 
		bys group: egen mean_english = min(mean_english_c)
		bys group: egen sd_english = min(sd_english_c)
		gen z_english=(lang1_ovrall_tot - mean_english)/sd_english

		egen z_aggregate=rowmean(z_math z_hindi z_science z_social z_english)

	///	plot fig a1a
	
		graph twoway (kdensity  z_math, lcolor(red)) ///
		(kdensity  z_math if rct==1, lcolor(blue)) ///
		if year=="2014-15" & z_math>-6 & z_math<6 & class>5, ///
		graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "All students" 2 "RCT")) xtitle(Standardized score) ///
		ytitle(Density) title(Math) name(nonRCT_m, replace)
	
	///	plot fig a1b

		graph twoway (kdensity  z_hindi, lcolor(red)) ///
		(kdensity  z_hindi if rct==1, lcolor(blue)) ///
		if year=="2014-15" & z_hindi>-6 & z_hindi<6 & class>5, ///
		graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "All students" 2 "RCT")) xtitle(Standardized score) ///
		ytitle(Density) title(Hindi) name(nonRCT_h, replace)

	///	combine graphs	
		
		grc1leg nonRCT_m nonRCT_h, leg(nonRCT_m) graphregion(fcolor(white) ///
		lcolor(white)) note(403 study children matched to school records of 2014-15)
		gr save ${graphs}figa1.gph, replace
		gr export ${graphs}figa1.png, replace as(png)	
	
///	fig a.2: distribution of take-up among lottery-winners

	/// load sgi data
	
		use ${ms_clean}ms_ei_sgi, clear
	
	///	drop duplicates
		
		duplicates drop st_id, force
	
	///	plot graph
	
		hist att_tot, width(1) freq xtitle(Days attended) ///
		name(attend, replace) graphregion(fcolor(white) lcolor(white))
		
	///	save
	
		gr save attend ${graphs}figa2.gph, replace
		gr export ${graphs}figa2.png, as(png) replace

///	fig a.3: comparison of Mindspark initial assessment of grade-level of student achievement with (independent) baseline test scores
	
	/// load ms levels data

		use ${ms_clean}ms_levels, clear

	///	drop duplicates

		duplicates drop ms_id, force

	/// merge w/j-pal data wide

		mer 1:1 st_id using ${el_clean}ms_blel_jpal_wide, nogen ///
		keepus(m_theta_mle1 h_theta_mle1) keep(match)
	
	///	calculate mean performance by level
	
		tabstat m_theta_mle1,by(mathlevel) stat(mean n)
		tabstat h_theta_mle1,by(hindilevel) stat(mean n)

	///	plot fig a3a	

		cibar m_theta_mle1, over1(mathlevel) barlabel(on) ///
		graphopts(graphregion(fcolor(white) lcolor(white)) name(math_bl,replace) legend(off) ///
		xtitle("Assessed grade level of achievement") xlabel(1(1)8) ///
		ytitle("Baseline test score (mean)") title("Math")) ///
		barcolor(gs12 gs12 gs12 gs12 gs12 gs12 gs12 gs12) ///
		ciopts(lcolor(black)) 
	
	///	plot fig a3b
	
		cibar h_theta_mle1 , over1(hindilevel) barlabel(on) ///
		graphopts(graphregion(fcolor(white) lcolor(white)) name(hindi_bl,replace) legend(off) ///
		xtitle("Assessed grade level of achievement") xlabel(1(1)8) ///
		ytitle("Baseline test score (mean)") title("Hindi")) ///
		barcolor(gs12 gs12 gs12 gs12 gs12 gs12 gs12 gs12) ///
		ciopts(lcolor(black)) 
		*ajg: this generates incorrect xlabels -- correct manually
	
	///	combine graphs
	
		gr combine math_bl hindi_bl, xcommon ycommon ///
		graphregion(fcolor(white) lcolor(white))
		gr save ${graphs}figa3.gph, replace		
		gr export ${graphs}figa3.png, replace		
		
///	fig a.4: distribution of questions of ms software	

	/// load ms levels data

		use ${ms_clean}ms_levels, clear

		///	drop duplicates

			duplicates drop ms_id, force

		///	merge with ms math questions
		
			mer 1:m ms_id using ${ms_clean}ms_mathqs, nogen keep(match)

		///	restrict to observations before feb 5, 2016 
			*ajg: endline was administered on feb 7, 2016
		
			keep if q_date<20490
			*ajg: 88,480 obs dropped
			
		///	gen ms levels
		
			gen ms_q_level=1 if q_grade=="1"
		
			forval i = 2/8{
				replace ms_q_level = `i' if q_grade=="`i'"
			}

			forval i=1/8{
				local j = `i'+ 1 
				replace ms_q_level = (`i' + `j')/2 if q_grade=="`i',`j'"
			}
			
			replace ms_q_ =2 if q_grade=="1,2,3"
			replace ms_q_ =7 if q_grade=="6,7,8"

		///	re-label mathlevel
			
			lab var mathlevel "Math level assessed by CAL system at baseline"

		///	gen question distribution
		
			forval i = 1(0.5)8 {
				local k = `k'+1
				gen q_`k'=ms_q_l==`i'
				lab var q_`k' "Diff level `i'"
			}	

		/// plot fig a5a
			
			hist ms_q if (class>5 & class<10), by(class, title(Math) ///
			note(" ")graphregion(fcolor(white) lcolor(white)) ) ///
			xlab(1(1)8) xtitle(Grade level of question administered) ///
			percent name(figa5a, replace)

		/// save
			
			gr save ${graphs}figa4a.gph, replace		
			gr export ${graphs}figa4a.png, replace as(png)

	/// reload ms levels data

		use ${ms_clean}ms_levels, clear
		
		///	drop duplicates

			duplicates drop ms_id, force

		///	merge with ms hindi questions
		
			mer 1:m ms_id using ${ms_clean}ms_hindiqs, nogen keep(match)

		///	restrict to observations before feb 5, 2016 
			*ajg: endline was administered on feb 7, 2016
		
			keep if q_date<20490
			*ajg: 88,480 obs dropped
			
		///	gen ms levels

			destring q_grade, g(hindi_q_level)

		///	re-label hindilevel
			
			lab var hindilevel "Hindi level assessed by CAL system at baseline"

		///	format date of question being attempted
		
			format q_date %td
	
		///	plot graph
		
			hist hindi_q_level if (class>5 & class<10), by(class, title(Hindi) ///
			note(" ")graphregion(fcolor(white) lcolor(white)) ) ///
			xlab(1(1)8) xtitle(Grade level of question administered) ///
			percent name(figa5b, replace)
	
		/// save
		
			gr save ${graphs}figa4b.gph, replace		
			gr export ${graphs}figa4b.png, replace as(png)

///	fig a.5: composition of group instruction batches in mindspark centers			
	
	/// load ms batch composition
	
		use ${ms_clean}ms_batches, clear
		
	///	plot fig a5a	
	
		gr twoway (scatter grade group if rct=="RCT", ///
		mcolor(red) msymbol(oh) jitter(2))(scatter grade group if rct!="RCT", ///
		mcolor(blue) msymbol(dh) jitter(2)), legend(off) ytitle("Enrolled grade") ///
		graphregion(fcolor(white) lcolor(white))  xtitle (Batch) ///
		title("Dispersion in grade enrolled") ///
		legend(order(1 "RCT" 2 "non-RCT")) name(figa5a,replace)
	
	///	plot fig a5b
	
		gr twoway (scatter math group if rct=="RCT", mcolor(red) msymbol(oh) ///
		jitter(2))(scatter math group if rct!="RCT", mcolor(blue) msymbol(dh) ///
		jitter(2)) if ms_id>8680, graphregion(fcolor(white) lcolor(white)) ///
		legend(off) title("Dispersion in level of achievement") xtitle (Batch) ///
		ytitle("Initial achievement level assessed by CAL system") ///
		name(figa5b,replace) 
		*ajg: only plotting non-rct children enrolled in 2015
		grc1leg figa5a figa5b, xcommon ycommon leg(figa5a) ///
		graphregion(fcolor(white) lcolor(white))
		
		gr save ${graphs}figa5.gph, replace		
		gr export ${graphs}figa5.png, replace as(png)


///	fig e.1 distribution of raw percentage correct scores
		use ${el_clean}ms_blel_jpal_long, clear
		merge 1:1 st_id round using ${el_clean}ms_blel_forirt, keep(master match)

		sort st_id round
		egen id=group(st_id round)
		drop if round==.

		egen round_treat=group(round treat)
			lab define round_treat 1 "Control BL" 2 "Treat BL" 3 "Control EL" 4 "Treat EL" 
			lab val round_treat round_treat
		tempfile main
		save `main'	

		*	Proportion correct
		egen rawsc_math=rowtotal(qm*)
		egen rawsc_hindi=rowtotal(qh*)

		gen prop_m=rawsc_m/35 if round==2 & st_grade>7 & st_grade~=.
			replace prop_m=rawsc_m/34 if round==2 & st_grade<8
			replace prop_m=rawsc_m/35 if round==1 
			
		gen prop_h=rawsc_h/30

		graph twoway (kdensity prop_m if round==1, lcolor(red)) ///
		(kdensity prop_m if round==2, lcolor(blue)), title(Math) ytitle(Density) ///
		xtitle("Proportion Correct") graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "Baseline" 2 "Endline")) name(math_raw,replace)

		graph twoway (kdensity prop_h if round==1, lcolor(red)) ///
		(kdensity prop_h if round==2, lcolor(blue)), title(Hindi) ytitle(Density) ///
		xtitle("Proportion Correct") graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "Baseline" 2 "Endline")) name(hindi_raw,replace)

		grc1leg math_raw hindi_raw, xcommon ycommon leg(math_raw) graphregion(fcolor(white) lcolor(white))
			gr save ${graphs}fige1.gph, replace		
			gr export ${graphs}fige1.png, replace as(png)

///	fig e2: distribution of IRT scores

		graph twoway (kdensity m_theta_mle if treat==0,lcolor(red)) ///
		(kdensity m_theta_mle if treat==1,lcolor(blue)) if round==1, ///
		title(Math) subtitle(Baseline) ytitle(Density) xtitle(IRT score) ///
		graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "Control" 2 "Treatment")) name(m_bl,replace)

		graph twoway (kdensity m_theta_mle if treat==0,lcolor(red)) ///
		(kdensity m_theta_mle if treat==1,lcolor(blue)) if round==2, ///
		title(Math) subtitle(Endline) ytitle(Density) xtitle(IRT score) ///
		graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "Control" 2 "Treatment")) name(m_el,replace)

		graph twoway (kdensity h_theta_mle if treat==0,lcolor(red)) ///
		(kdensity h_theta_mle if treat==1,lcolor(blue)) if round==1, ///
		title(Hindi) subtitle(Baseline) ytitle(Density) xtitle(IRT score) ///
		graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "Control" 2 "Treatment")) name(h_bl,replace)

		graph twoway (kdensity h_theta_mle if treat==0,lcolor(red)) ///
		(kdensity h_theta_mle if treat==1,lcolor(blue)) if round==2, ///
		title(Hindi) subtitle(Endline) ytitle(Density) xtitle(IRT score) ///
		graphregion(fcolor(white) lcolor(white)) ///
		legend(order(1 "Control" 2 "Treatment")) name(h_el,replace)

		grc1leg m_bl h_bl m_el h_el, xcommon ycommon leg(m_bl) ///
		graphregion(fcolor(white) lcolor(white))
		
			gr save ${graphs}fige1.gph, replace		
			gr export ${graphs}fige1.png, replace as(png)

	
}
// ****************** PLOT MATH ICCS ****************** //

if $plotmathiccs==1 {

		use ${el_clean}ms_blel_jpal_long, clear
		merge 1:1 st_id round using ${el_clean}ms_blel_forirt, keep(master match)

		sort st_id round
		egen id=group(st_id round)
		drop if round==.

		egen round_treat=group(round treat)
			lab define round_treat 1 "Control BL" 2 "Treat BL" 3 "Control EL" 4 "Treat EL" 
			lab val round_treat round_treat
		
		
		
		xtile perc_thet=m_theta_mle,nq(5)
		sort perc
		
		by perc: egen mean_theta=mean(m_theta_mle)

		forval i=1/94{
			by perc: egen mean_maths`i'=mean(qm`i')
		}
			
			
		forval i=1/94{
		forval j=1/4{
			by perc: egen mean_maths`i'_`j'=mean(qm`i') if round_treat==`j'
		}
		}

		keep perc mean* round_treat

		forval i=1/94{
		ren mean_maths`i' id`i'
		}

		duplicates drop perc round_treat, force
		egen group_perc=group(round_treat perc)
		drop if group_perc==.
		reshape long id,i(group_perc) j(item)
		ren id prop
		ren ite id


		merge m:1 id using ${el_temp}math_items_all_2.dta
		drop _m

		cd "$graphs"

		replace c_pv1=0 if c_pv1==.

		forval i=1/38 {
		local j=((`i' - 1)*20 + 1)
		twoway (scatter mean_maths`i'_3 mean_theta if id==`i' & round_treat==3, ///
		sort msymbol(circle) mcolor(red)msize(small)) ///
		(scatter mean_maths`i'_4 mean_theta if id==`i' & round_treat==4, sort ///
		msymbol(lgx) mcolor(blue)msize(small)) ///
		(function c_pv1[`j'] + (1-c_pv1[`j'])/(1+exp(-1.7*a_pv1[`j']*(x-b_pv1[`j']))) ///
		if id==`i', range(-5 5)), name("Item_`i'", replace) xtitle("Theta") ///
		ytitle("P(X=1|Theta)") title("Item `i'") nodraw ///
		legend(order(1 "Control EL" 2 "Treat EL")) graphregion(fcolor(white) lcolor(white)) ///

		}
		grc1leg Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9 Item_10 Item_11 Item_12,  ycommon xcommon ///
		 title("Item Characteristic Curves") subtitle("Mathematics") note("Combining all grades") leg(Item_1) graphregion(fcolor(white) lcolor(white))
		graph save Graph "$graphs/fige4a.gph", replace
			gr export ${graphs}fige4a.png, replace as(png)

		grc1leg Item_13 Item_14 Item_15 Item_16 Item_17 Item_18 Item_19 Item_20 Item_21 Item_22 Item_23 Item_24,  ycommon xcommon ///
		 title("Item Characteristic Curves") subtitle("Mathematics") note("Combining all grades") leg(Item_13) graphregion(fcolor(white) lcolor(white))
		graph save Graph "$graphs/fige4b.gph", replace
			gr export ${graphs}fige4b.png, replace as(png)

		grc1leg Item_25 Item_26 Item_27 Item_28 Item_29 Item_30 Item_31 Item_32 Item_33 Item_34 Item_35 Item_36 Item_37 Item_38 ,  ycommon xcommon ///
		 title("Item Characteristic Curves") subtitle("Mathematics") note("Combining all grades") leg(Item_25) graphregion(fcolor(white) lcolor(white))
		graph save Graph "$graphs/fige4c.gph", replace
			gr export ${graphs}fige4c.png, replace as(png)
		
}
// ****************** PLOT HINDI ICCS ****************** //

if $plothindiiccs==1 {

		use ${el_clean}ms_blel_jpal_long, clear
		merge 1:1 st_id round using ${el_clean}ms_blel_forirt, keep(master match)

		sort st_id round
		egen id=group(st_id round)
		drop if round==.

		egen round_treat=group(round treat)
			lab define round_treat 1 "Control BL" 2 "Treat BL" 3 "Control EL" 4 "Treat EL" 
			lab val round_treat round_treat
			
		xtile perc_thet=h_theta_mle,nq(5)
			sort perc
			
		by perc: egen mean_theta=mean(h_theta_mle)

		forval i=1/71{
			by perc: egen mean_hindi`i'=mean(qh`i')
		}
			
			
		forval i=1/71{
		forval j=1/4{
			by perc: egen mean_hindi`i'_`j'=mean(qh`i') if round_treat==`j'
		}
		}

		keep perc mean* round_treat

		forval i=1/71{
		ren mean_hindi`i' id`i'
		}

		duplicates drop perc round_treat, force
		egen group_perc=group(round_treat perc)
		drop if group_perc==.
		reshape long id,i(group_perc) j(item)
		ren id prop
		ren ite id

		merge m:1 id using ${el_temp}hindi_items_all_2.dta
		drop _m

		cd "$graphs"

		replace c_pv1=0 if c_pv1==.

		forval i=1/36 {
		local j=((`i' - 1)*20 + 1)
		twoway (scatter mean_hindi`i'_3 mean_theta if id==`i' & round_treat==3, ///
		sort msymbol(circle) mcolor(red)msize(small)) ///
		(scatter mean_hindi`i'_4 mean_theta if id==`i' & round_treat==4, sort ///
		msymbol(lgx) mcolor(blue)msize(small)) ///
		(function c_pv1[`j'] + (1-c_pv1[`j'])/(1+exp(-1.7*a_pv1[`j']*(x-b_pv1[`j']))) ///
		if id==`i', range(-5 5)), name("Item_`i'", replace) xtitle("Theta") ///
		ytitle("P(X=1|Theta)") title("Item `i'") nodraw ///
		legend(order(1 "Control EL" 2 "Treat EL")) graphregion(fcolor(white) lcolor(white)) ///

		}
		grc1leg Item_1 Item_2 Item_3 Item_4 Item_5 Item_6 Item_7 Item_8 Item_9 Item_10 Item_11 Item_12,  ycommon xcommon ///
		 title("Item Characteristic Curves") subtitle("Hindi") note("Combining all grades") leg(Item_1) graphregion(fcolor(white) lcolor(white))
		graph save Graph "$graphs/fige3a.gph", replace
			gr export ${graphs}fige3a.png, replace as(png)

		grc1leg Item_13 Item_14 Item_15 Item_16 Item_17 Item_18 Item_19 Item_20 Item_21 Item_22 Item_23 Item_24,  ycommon xcommon ///
		 title("Item Characteristic Curves") subtitle("Hindi") note("Combining all grades") leg(Item_13) graphregion(fcolor(white) lcolor(white))
		graph save Graph "$graphs/fige3b.gph", replace
			gr export ${graphs}fige3b.png, replace as(png)

		grc1leg Item_25 Item_26 Item_27 Item_28 Item_29 Item_30 Item_31 Item_32 Item_33 Item_34 Item_35 Item_36 ,  ycommon xcommon ///
		 title("Item Characteristic Curves") subtitle("Hindi") note("Combining all grades") leg(Item_25) graphregion(fcolor(white) lcolor(white))
		graph save Graph "$graphs/fige3c.gph", replace
			gr export ${graphs}fige3c.png, replace as(png)

		
}
