********************************************************************************
* The following Stata do.file is one of the coding tests that I completed. This test involves using Stata to clean, wrangle, and visualize data, and build econometric models. The last part also involves using econometric knowledge to answer several short questions. The codes are my own work. Please do not distribute it without my permission. The folder can also be accessed on my GitHub at https://github.com/GGGGUKIM/Stata_codingsample2.
********************************************************************************

* Basic setup
clear all
global username "/Users/raymond/Desktop/georgetown/career development/RA_fulltime/chicago/EPIC/test"
cd "${username}"

*Section 2
// append production data
clear
cd "${username}/production"
tempfile table1 // create a temporary dataset
save "table1.dta", replace emptyok
foreach i of numlist 1990/2018 {
	import delimited "Barley_production_`i'.csv", clear
	append using "table1.dta", force
	save "table1.dta", replace
	} // append production datasets
gen production = subinstr(value,",","",.) // get rid of commas
destring production, replace // transform string values to numeric ones
drop value
save "table1.dta", replace // export

// append price data
clear
cd "${username}/price"
tempfile table2 // create a temporary dataset
save "table2.dta", replace emptyok
foreach i of numlist 1990/2018 {
	import delimited "Barley_price_`i'.csv", clear
	append using "table2.dta", force
	save "table2.dta", replace
	} // append price datasets
rename value price // change variable names
save "table2.dta", replace

// merge
cd "${username}"
use "${username}/production/table1.dta", clear
merge m:1 year state stateansi using "${username}/price/table2.dta", force // merge based on states
keep year state agdistrict production price // keep variables needed
sort state year agdistrict
save "final.dta", replace // export
erase "${username}/production/table1.dta"
erase "${username}/price/table2.dta"

*Section 3
**3.1
use final, clear
bys state year: egen product_state = sum(production) // create yearly production for each state 
duplicates drop state year price product_state, force // only keep one observation for state-year level
collapse (mean) price [pw=product_state], by(year) // generate weighted means of prices based on each state's production
scatter price year , c(l) ms(none) lcolor(black) ytitle("Average Price Per Bushel of Barley") xtitle("Year") graphregion(fcolor(white)) // plot
graph export "Time Series Plot: Price.pdf", replace // export


**3.2
use final, clear
preserve
keep if year == 2018
collapse (sum) production, by(state) // get production for each state in 2018
gsort -production 
list
restore // Find states with top 3 production in 2018
keep if state == "IDAHO" | state == "MONTANA" | state == "NORTH DAKOTA"
collapse (sum) production, by(state year)
gen product_million = production/1000000 // scale
scatter product_million year if state == "IDAHO", c(l) ms(none) lcolor(black) || scatter product_million year if state == "MONTANA", c(l) ms(none) lcolor(red) || scatter product_million year if state == "NORTH DAKOTA", c(l) ms(none) lcolor(blue) ytitle("Yearly Production (Millions of Bushels)") xtitle("Year") legend(label(1 "Idaho") label(2 "Montana") label(3 "North Dakota") order(1 2 3) row(1)) graphregion(fcolor(white)) // plot
graph export "Time Series Plot: Production.pdf", replace // export

**3.3
use final, clear
keep if state == "IDAHO" | state == "MINNESOTA" | state == "MONTANA" | state == "NORTH DAKOTA" | state == "WYOMING" 
bys state year: egen product_state = sum(production) // get yearly production for these states 
duplicates drop state year product_state price, force // only keep one observation for state-year level
drop agdistrict production price // keep variables needed
tempfile table3 // create a temporary dataset
save "table3.dta", replace emptyok

clear
tempfile table4 
save "table4.dta", replace emptyok

foreach n of numlist 1990 2000 {
	use table3, clear
	keep if year <= `n'+9 & year >= `n'
	collapse (mean) product_state, by(state)
	gen year = `n'
	append using "table4.dta", force
	save "table4.dta", replace
	} // loop for appending average annual state-level production data for the first two time period

use table3, clear
keep if year <= 2018 & year >= 2010
collapse (mean) product_state, by(state)
gen year = 2010
append using "table4.dta", force // add average annual state-level production data for the last time period to the existing dataset
replace product_state = round(product_state/1000000, .01) // round
save "table4.dta", replace

reshape wide product_state, i(state) j(year) // change the long dataset to a wide one
rename state State
rename product~1990 Production_1990_1999 // change names
rename product~2000 Production_2000_2009
rename product~2010 Production_2010_2018

putdocx begin, font("Time New Roman", 12)
putdocx table tbl = data("State - Production_2010_2018"), varnames // export
putdocx save "Summary Table.docx", replace

erase table3.dta
erase table4.dta

*Section 4
use final, clear

**1
count if production == 0
count if price == 0 // no values equal to 0 so a direct log transformation can be carried out 

gen lg_product = log(production)
gen lg_price = log(price) // log transformation
reg lg_product lg_price // simple linear regression
asdoc reg lg_product lg_price, nest replace // export results
erase "Myfile.doc"

**4
encode state, gen(state_id) // create state dummies
reg lg_product lg_price i.year i.state_id, vce(cluster state_id) // regression with state and year fixed effects
asdoc reg lg_product lg_price i.year i.state_id, vce(cluster state_id), nest replace // export results
erase "Myfile.doc"

**5
gen miss = 0
replace miss = 1 if production == . // create a missing dummy indicating whether an observation will be omitted in the regression
reg price miss // see the difference between observations with missing values and ones without missing values
asdoc reg price miss, nest replace // export
erase "Myfile.doc"

tab miss state_id // check the missing observations in each state







