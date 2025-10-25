	
	*================================================
	* Replication Package - ECON 280 UC San Diego 
	*================================================
	

clear all
set more off
set seed 12345
version 13.1

* Globals
* Set directory path 

global path "/Users/andresbarinasf/Documents/GitHub/econ280project/"
global code "${path}/code"
global data "${path}/data"
global results "${path}/results"

*Replication of First Figure 

use "${data}/ms_levels.dta", clear 

* plot math graph
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
	
		gr export "${results}/fig1.pdf", replace
		
*========================================================
* Choose a variable of interest and plot a histogram. 
*========================================================

	use "${data}/ms_blel_jpal_long.dta", clear
	
	/* Histogram of Hindi test - percent correct score across treatments
	in the endline of the intervention */
	
	qui sum per_hindi if round == 2 & treat == 1
	local mean_treated = `r(mean)'
	qui sum per_hindi if round == 2 & treat == 0
	local mean_control = `r(mean)'
	
	twoway (hist per_hindi if round == 2 & treat == 1, ///
		   discrete color(black%50) xline(`mean_treated', lwidth(medium) lcolor(black))) ///
		   (hist per_hindi if round == 2 & treat == 0, ///
		   discrete color(red%50) xline(`mean_control', lwidth(medium) lcolor(red))) ///
		   , legend(order(1 "Treatment" 2 "Control") pos(6) row(1)) ///
		   xtitle("{bf: Hindi-scores} across treatments {it:(Endline)} ") ///
		   text(2.9 0.7 "Treatment mean: `: display %4.2f `mean_treated''{&sigma}", size(small)) ///
		   text(2.9 0.41 "Control mean: `: display %4.2f `mean_control''{&sigma}", size(small) color(red))
		   
	graph export "${results}/histogram_hindi_scores.pdf", replace

		   
		   
	
	
	
		
		
		
		
		