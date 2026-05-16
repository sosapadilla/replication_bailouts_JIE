
clear all
cd "."

insheet using "../data/GDP_data_wb.csv"

drop countrycode seriescode SeriesName yr2020
drop if countryname==""


reshape long  yr, i(countryname) j(year)
rename yr GDP
rename countryname country
replace GDP = GDP/1000000


sort country year
save "../data/gdp_data", replace

clear all
use "../data/banking_crisis_dates.dta"
replace country="Czechia" if country=="The Czech Republic"
replace country="Netherlands" if country=="The Netherlands"
replace country="UK" if country=="U.K."
sort country year
save "../data/banking_crisis_dates_y", replace

clear all

insheet using "../data/Eurostat_total_list.csv"

sort country year
merge country year using "../data/gdp_data"
tab _m
drop _m


* Merge crisis date data
sort country year
merge country year using "../data/banking_crisis_dates_y.dta"
tab _m
drop _m

replace banking_crisis=0 if banking_crisis==.

rename contingentliabilities cl
rename liabilitiesandassetsoutsidegener gov_guarantee
rename capitalinjectionsrecordedasdefic ct
rename othercapitaltransferegassetpurch ap

keep country year cl gov_guarantee ct ap banking_crisis GDP
egen country_group = group(country)
xtset country_group year


* Take the first difference of contingent liabilities 
sort country_group year
by country_group: gen diffcl = cl - L.cl
by country_group: gen diffgov = gov_guarantee - L.gov_guarantee

gen diffcl_gdp_ratio = (diffcl/GDP)*100
gen diffgov_gdp_ratio = (diffgov/GDP)*100
 
* Generate capital transfer measure

gen ct_gdp_ratio = (ct/GDP)*100
gen totalct_gdp_ratio = ((ct+ap)/GDP)*100



preserve



tempfile full crisis

* Unconditional means
collapse (mean) diffgov_gdp_ratio ct_gdp_ratio
gen group = 0
save `full'

restore
preserve

* Banking crisis means
keep if banking_crisis == 1
collapse (mean) diffgov_gdp_ratio ct_gdp_ratio
gen group = 1
save `crisis'

use `full', clear
append using `crisis'

label define crisis_group 0 "Unconditional" 1 "Banking Crisis"
label values group crisis_group

graph bar diffgov_gdp_ratio ct_gdp_ratio, ///
    over(group, gap(60)) ///
    bargap(10) ///
    graphregion(color(white)) ///
    plotregion(color(white)) ///
    bar(1, color(black) lcolor(black)) ///
    bar(2, color(gs10) lcolor(gs10)) ///
    legend(label(1 "Government Guarantees") ///
           label(2 "Capital Transfer") ///
           rows(2) ring(0) pos(11) region(lcolor(white))) ///
    ytitle("Percent of GDP") ///
    ylabel(0(.5)2, nogrid angle(0)) ///
    yscale(range(0 2.1))

graph export "../figures/guarantees_plot_new.png", replace

restore

* Plot: Unconditional vs. Banking Crisis for contingent liabilities
preserve

tempfile full crisis

collapse (mean) diffcl_gdp_ratio ct_gdp_ratio
gen group = 0
save `full'

restore
preserve

keep if banking_crisis == 1
collapse (mean) diffcl_gdp_ratio ct_gdp_ratio
gen group = 1
save `crisis'

use `full', clear
append using `crisis'

label define crisis_group 0 "Unconditional" 1 "Banking Crisis"
label values group crisis_group

graph bar diffcl_gdp_ratio ct_gdp_ratio, ///
    bargap(5) ///
    graphregion(color(white)) ///
    over(group, gap(50)) ///
    bar(1, color(black*0.9)) ///
    bar(2, color(black*0.3)) ///
    legend(label(1 "Contingent Liabilities") ///
           label(2 "Capital Transfer") ///
           rows(2) ring(0) pos(11) region(lcolor(white))) ///
    ytitle("Percent of GDP") ///
    ylabel(0(0.5)2.0, nogrid)

graph export "../figures/contingent_liabilities_new.png", replace

restore
