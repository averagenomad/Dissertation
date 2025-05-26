clear
// cd "/Users/jamilion/Desktop/Dissertation Data/Analysis"

// import delimited "analysis.csv"
//  foreach var of varlist * {
//    cap replace `var' = "" if `var'=="NA"
//   }
// drop v1
// destring pts, replace
// destring democ, replace
// destring autoc, replace
// destring polity, replace
// destring iso3, replace
// destring rpe_agri, replace
// destring rpe_gdp, replace
// destring ape1, replace
// destring ape1n, replace
// destring rpr_work, replace
// destring rpr_eap, replace
// destring rpe_gdp_nonres, replace
// destring exports_gdp, replace
// destring gdppc, replace
// destring gdppc_growth, replace
// destring internet, replace
// destring labor_female, replace
// destring labor_male, replace
// destring labor_force, replace
// destring population, replace
// destring top10_income, replace
// destring bottom50_income, replace
// destring deaths, replace
// destring conflict, replace
// destring onset, replace
// destring transition, replace
// destring adjacent_conflict, replace
// destring laborforce, replace
// destring protest, replace
// destring protesterviolence, replace
// destring violence, replace
// destring v2x_polyarchy, replace
// destring v2x_libdem, replace
// destring v2x_partipdem, replace
// destring participants, replace
// destring violence2, replace

// mdesc
// duplicates drop
// qui gen pop_th = population/1000
// qui gen logpop_th = log(pop_th)
// qui gen lagonset = onset[_n-1]
// qui gen logdeaths = log(deaths)
// replace logdeaths = logdeaths[_n-1] if missing(logdeaths) 
// qui gen lrpe_agri = rpe_agri[_n-1]
// qui gen lpts = pts[_n-1]
// qui gen ldemoc = democ[_n-1]
// qui gen lbottom50_income = bottom50_income[_n-1]
// qui gen llaborforce = laborforce[_n-1]
// qui gen ladjacent_conflict = adjacent_conflict[_n-1]
// qui gen lgdppc = gdppc[_n-1]
// qui gen lexports_gdp = exports_gdp[_n-1]
// qui gen lgdppc_growth = gdppc_growth[_n-1]
// qui gen linternet = internet[_n-1]
// qui gen llogpop_th = logpop_th[_n-1]
// qui gen lv2x_polyarchy = v2x_polyarchy[_n-1]
// qui gen llagonset = lagonset[_n-1]
// qui gen llogdeaths = logdeaths[_n-1]
// qui gen lconflict = log(conflict)
// replace lconflict = lconflict[_n-1] if missing(lconflict) 
// replace lconflict = lconflict[_n-1] if missing(lconflict) 
// gen popdeaths = (deaths/population)*1000000
// qui gen lconflict = log(conflict)
// replace lconflict = lconflict[_n-1] if missing(lconflict) 
// qui gen llconflict = lconflict[_n-1]
// qui gen loggdppc = log(gdppc)
// replace loggdppc = loggdppc[_n-1] if missing(loggdppc) 
// qui gen loggdppc_growth = log(gdppc_growth)
// replace loggdppc_growth = loggdppc_growth[_n-1] if missing(loggdppc_growth) 
// qui gen lloggdppc= loggdppc[_n-1]
// qui gen lloggdppc_growth = loggdppc_growth[_n-1]

//Oct02 - downloading modely1 where I added a new "region_id" var to conduct the regional study
// use "/Users/zhamiliaklycheva/Documents/Dissertation/Final Data/modely1.dta"
clear
use "/Users/zhamiliaklycheva/Documents/Dissertation/Final Data/modely2.dta"
* save modely.dta, replace
xtset ccode year

 ***** EDA
 outreg2 using test.tex, replace sum(log) keep (onset_new deaths duration protest)


************************************************************ 
************************** Probit **************************
************************************************************
//serial correlation test
xtserial onset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th lagonset_new

***** Standardize Variables *****
** Rename var for new
egen onset = std(onset_new)
egen lagonset = std(lagonset_new)
egen logfatalities = std(lndeaths) 
egen logfatalities_1 = std(laglndeaths) 
egen lnduration = std(lnduration)
egen llogduration_1 = std(laglnduration)
egen rotest = std(lnprotest)
egen Ln Protest = std(laglnprotest)
egen APE = std(lagape)
egen repression = std(lagpts)
egen Democracy = std(lagdemoc)
egen income_equality = std(lagbottom50) 
egen gender_equality = std(laglaborforce)
egen adjacent_conflict = std(lagadjacent_conflict)
egen logGDPpc = std(lagloggdppc) 
egen logGDPpc_growth = std(lagloggdppc_growth)
egen exports_gdp = std(lagexports_gdp) ≠'
egen internet = std(laginternet) 
egen logpopulation = std(laglogpop_th)

** Rename labels
label variable z2lagadjacent_conflict "Adjacent Conflict*"
label variable z2lagape "lAPE"
label variable z2lagbottom50 "lIncome Equality"
label variable z2lagdemoc "lDemocracy"
label variable z2lagexports_gdp "lExports share_GDP"
label variable z2laginternet "lInternet"
label variable z2laglaborforce "lGender Equality"
label variable z2laglndeaths "Lag Fatalities"
label variable z2laglnduration "Lag Duration"
label variable z2laglnprotest "Lag Protest"
label variable z2lagloggdppc "lLn GDPpc"
label variable z2lagloggdppc_growth "Ln GDPpc Growth"
label variable z2laglogpop_th "LnPopulation"
label variable z2lagonset_new "Lag Onset"
label variable z2lagpts "lPolitical Repression"
label variable z2lndeaths "Ln Fatalities"
label variable z2lnduration "lLn Duration"
label variable z2lnprotest "Ln Protest"
label variable z2onset_new "Onset"

*RE vs FE test for probit
xtreg onset_new lagonset_new lagape lagpts lagdemoc lagbottom50 laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth lagexports_gdp laginternet laglogpop_th,fe 
estimates store onsetf
xtreg onset_new lagonset_new lagape lagpts lagdemoc lagbottom50 laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth lagexports_gdp laginternet laglogpop_th,re
//indicates we need to use FE
hausman onsetf ., sigmamore 

//////////////////////////////////////
*** Pooled Probit
qui probit onset_new lagonset_new, vce(robust)
est store m1, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new) replace eq(auto) 
*// (1801.2558- 1316.7667 )/1801.2558 = 0.269

qui probit onset_new lagape  lagpts  lagdemoc, vce(robust)
est store m2, title(Probit 2)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probit.tex, dec(3)  addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagape  lagpts  lagdemoc) append eq(auto) 
*// (1674.3934- 1210.2214 )/1674.3934 = 0.278

qui probit onset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce, vce(robust)
est store m3, title(Probit 3)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probit.tex, dec(3)  addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagape  lagpts  lagdemoc lagbottom50  laglaborforce) append eq(auto) 
*// (1674.3934 -1207.2643 )/1674.3934 = 0.279

qui probit onset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict, vce(robust)
est store m3, title(Probit 3)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probit.tex, dec(3)  addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict) append eq(auto) 
*// (1674.3934 -1199.6321 )/1674.3934 = 0.284
		 
qui probit onset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust)
est store m4, title(Probit 4)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probit.tex, dec(3)  addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th) append eq(auto) 
*// (1492.5792 -1063.0577  )/ 1492.5792 = 0.288

qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
est store m5, title(Probit 5)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probit.tex, dec(3) addstat(AIC, `AIC', Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 
*// (1492.5792 -1051.4487 )/1492.5792 = 0.296

outreg2 using probit.tex, dec(3) addstat(AIC, `AIC', Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

qui probit onset_new laglnprotest lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitprot.tex, dec(3) addstat(AIC, `AIC', Log Lik, e(ll)) groupvar(laglnprotest lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  replace eq(auto) 

qui probit onset_new lagonset_new laglnprotest lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitprot.tex, dec(3) addstat(AIC, `AIC', Log Lik, e(ll)) groupvar(lagonset_new  laglnprotest lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 


 margins , dydx(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th) atmeans

 est store probit
 coefplot probit, /// 
	plotregion(fcolor(white) lcolor(gs10) lwidth(thin)) ///
	graphregion(fcolor(white)) ///
	keep(lagonset_new lagape lagpts lagdemoc lagbottom50 laglaborforce ///
	lagadjacent_conflict lagloggdppc lagloggdppc_growth lagexports_gdp laginternet laglogpop_th) ///
	level(95) xline(0, lcolor(gray)lwidth(thin) ) ///
	mcolor(white) mlcolor(black) ///
	xlabel(, labcolor(gs4) tlcolor() tlwidth(thin) labsize(small) nogrid) ///
	ylabel(, labcolor(gs4) tlcolor() tlwidth(thin) labsize(small) nogrid) 	///
	ciopts(lcolor(black)) 
 
*************** Regional Dummies experiments ***************
//1
qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th east_asia, vce(robust) 
est store r1, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using regprobit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th east_asia) replace eq(auto) 

//2
qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th europe, vce(robust) 
est store r2, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using regprobit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th europe) append eq(auto) 

//3
qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th ussr, vce(robust) 
est store r3, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using regprobit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th ussr) append eq(auto) 

//4
qui qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th latin, vce(robust) 
est store r4, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using regprobit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th latin) append eq(auto) 

//5
qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th mid_east, vce(robust) 
est store r5, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using regprobit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th mid_east) append eq(auto) 

//6
qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th south_asia, vce(robust) 
est store r6, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using regprobit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th south_asia) append eq(auto) 

//7
qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th sub_africa, vce(robust) 
est store r7, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using regprobit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th sub_africa) append eq(auto) 
 
//8 
qui probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th east_asia europe ussr latin mid_east sub_africa, vce(robust) 
est store r7, title(Probit 1)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using regprobit.tex, dec(3)  eqdrop(lnsig2u) addstat(AIC, `AIC',Log Lik, e(ll)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th east_asia europe ussr latin mid_east sub_africa) append eq(auto) 
  
 
************************************************************ 
****************** Severity - Fatalities *******************
************************************************************

*** Breusch-Pagan Test for Heterscedasticity
estat hettest

** RE vs. FE Hausman Test -->> use FE
xtreg z2lndeaths z2laglndeaths, fe
estimates store fixed1
xtreg z2lndeaths z2laglndeaths, re
hausman fixed1 ., sigmamore 

qui xtreg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc, fe
estimates store fixed2
qui xtreg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc, re
hausman fixed2 ., sigmamore 

qui xtreg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce, fe
estimates store fixed3
qui xtreg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce, re
hausman fixed3 ., sigmamore 

qui xtreg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict, fe
estimates store fixed4
qui xtreg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict, re
hausman fixed4 ., sigmamore 

qui xtreg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th, fe
estimates store fixed5
qui xtreg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th, re
hausman fixed5 ., sigmamore 

xtreg lndeaths laglndeaths lagape lagpts lagdemoc lagbottom50 laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth lagexports_gdp laginternet laglogpop_th, fe  
estimates store fixed6
xtreg lndeaths laglndeaths lagape lagpts lagdemoc lagbottom50 laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth lagexports_gdp laginternet laglogpop_th, re
hausman fixedd ., sigmamore 


// Panel Corrected SE - Country FE
*corrects for autocorrelation and heteroscedasticity
xtpcse z2lndeaths z2laglndeaths i.ccode,correlation (psar1) pairwise
estimates store fixedf1
outreg2 using fixedfse.tex,addstat(RMSE, `e(rmse)') replace

xtpcse z2lndeaths z2lagape z2lagpts z2lagdemoc i.ccode,correlation (psar1) pairwise
estimates store fixedf2
outreg2 using fixedfse.tex, addstat(RMSE, `e(rmse)') append

xtpcse z2lndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce i.ccode,correlation (psar1) pairwise
estimates store fixedf3
outreg2 using fixedfse.tex,addstat(RMSE, `e(rmse)') append

xtpcse z2lndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict i.ccode,correlation (psar1) pairwise
estimates store fixedf4
outreg2 using fixedfse.tex,addstat(RMSE, `e(rmse)') append

xtpcse z2lndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
estimates store fixedf5
outreg2 using fixedfse.tex,addstat(RMSE, `e(rmse)') append

xtpcse z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
estimates store fixedf6
outreg2 using fixedfse.tex,addstat(RMSE, `e(rmse)') append

coefplot fixedf6, /// 
	plotregion(fcolor(white) lcolor(gs10) lwidth(thin)) ///
	graphregion(fcolor(white)) ///
	keep(z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce ///
	z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th) ///
	level(95) xline(0, lcolor(gray)lwidth(thin) ) ///
	mcolor(white) mlcolor(black) ///
	xlabel(, labcolor(gs4) tlcolor() tlwidth(thin) labsize(small) nogrid) ///
	ylabel(, labcolor(gs4) tlcolor() tlwidth(thin) labsize(small) nogrid) 	///
	ciopts(lcolor(black)) mlabel(cond(@pval<.001, "***", cond(@pval<.01, "**", cond(@pval<.05, "*", ""))))

	
	east_asia europe ussr latin mid_east south_asia sub_africa
***** Regional Dummies
qui reg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th east_asia
outreg2 using regionf.tex,addstat(RMSE, `e(rmse)') replace

qui reg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th europe
outreg2 using regionf.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th ussr
outreg2 using regionf.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th latin
outreg2 using regionf.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th mid_east
outreg2 using regionf.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th south_asia
outreg2 using regionf.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th sub_africa
outreg2 using regionf.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th east_asia europe ussr latin mid_east sub_africa
outreg2 using regionf.tex,addstat(RMSE, `e(rmse)')  append 



	
	
************************************************************ 
********************* DURATION *****************************
************************************************************

*** Autocorrelation test -> Presence of Autocorr
xtserial z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th
*** Across Panel AR Test
xtcdf z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th


// Panel Corrected SE - Country FE - 
////// psar1
xtpcse z2lnduration z2laglnduration i.ccode,correlation(psar1)  pairwise
outreg2 using fixeddse.tex, addstat(RMSE, `e(rmse)') replace

xtpcse z2lnduration z2lagape z2lagpts z2lagdemoc i.ccode,correlation(psar1)  pairwise
outreg2 using fixeddse.tex, addstat(RMSE, `e(rmse)')append

xtpcse z2lnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce i.ccode,correlation(psar1)  pairwise
outreg2 using fixeddse.tex, addstat(RMSE, `e(rmse)')append

xtpcse z2lnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict i.ccode,correlation(psar1)  pairwise
outreg2 using fixeddse.tex, addstat(RMSE, `e(rmse)')append

xtpcse z2lnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode, correlation(psar1) pairwise
outreg2 using fixeddse.tex, addstat(RMSE, `e(rmse)')append

xtpcse z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation(psar1)  pairwise
estimates store duration
outreg2 using fixeddse.tex, addstat(RMSE, `e(rmse)')append

** add severity into duration
xtpcse z2lnduration z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation(psar1)  pairwise
outreg2 using fixedDSse.tex, addstat(RMSE, `e(rmse)')replace

xtpcse z2lnduration z2laglnduration z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation(psar1)  pairwise
outreg2 using fixedDSse.tex, addstat(RMSE, `e(rmse)')append

coefplot duration, /// 
	plotregion(fcolor(white) lcolor(gs10) lwidth(thin)) ///
	graphregion(fcolor(white)) ///
	keep(z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce ///
	z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th) ///
	level(95) xline(0, lcolor(gray)lwidth(thin) ) ///
	mcolor(white) mlcolor(black) ///
	xlabel(, labcolor(gs4) tlcolor() tlwidth(thin) labsize(small) nogrid) ///
	ylabel(, labcolor(gs4) tlcolor() tlwidth(thin) labsize(small) nogrid) 	///
	ciopts(lcolor(black)) 

***** Regional Dummies
qui reg z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th east_asia
outreg2 using regiond.tex,addstat(RMSE, `e(rmse)') replace

qui reg z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th europe
outreg2 using regiond.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th ussr
outreg2 using regiond.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th latin
outreg2 using regiond.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th mid_east
outreg2 using regiond.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th south_asia
outreg2 using regiond.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th sub_africa
outreg2 using regiond.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th east_asia europe ussr latin mid_east sub_africa
outreg2 using regiond.tex,addstat(RMSE, `e(rmse)')  append 
	
	
************************************************************ 
************************* PROTEST **************************
************************************************************ 

*** Breusch-Pagan Test for Heterscedasticity -> presence of heteroscedasticity
estat hettest

** RE vs. FE Hausman Test -->> use FE
qui xtreg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th, fe
estimates store fixedp1
qui xtreg z2lnprotest z2lnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th, re
hausman fixedp1 ., sigmamore 

*** Autocorrelation test -> presence of autocorr
xtserial z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th
*** Across Panel AR Test
xtcdf z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th

// Panel Corrected SE - Country FE
*corrects for autocorrelation and heteroscedasticity
//psar1
xtpcse z2lnprotest z2laglnprotest i.ccode ,correlation (psar1) pairwise
outreg2 using fixedpse.tex,addstat(RMSE, `e(rmse)') replace

xtpcse z2lnprotest z2lagape z2lagpts z2lagdemoc i.ccode,correlation (psar1) pairwise
outreg2 using fixedpse.tex,addstat(RMSE, `e(rmse)') append

xtpcse z2lnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce i.ccode ,correlation (psar1) pairwise
outreg2 using fixedpse.tex,addstat(RMSE, `e(rmse)') append

xtpcse z2lnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict i.ccode ,correlation (psar1) pairwise
outreg2 using fixedpse.tex,addstat(RMSE, `e(rmse)') append

xtpcse z2lnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using fixedpse.tex,addstat(RMSE, `e(rmse)') append

xtpcse z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
estimates store protest
outreg2 using fixedpse.tex,addstat(RMSE, `e(rmse)') append

coefplot protest, /// 
	plotregion(fcolor(white) lcolor(gs10) lwidth(thin)) ///
	graphregion(fcolor(white)) ///
	keep(z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce ///
	z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th) ///
	level(95) xline(0, lcolor(gray)lwidth(thin) ) ///
	mcolor(white) mlcolor(black) ///
	xlabel(, labcolor(gs4) tlcolor() tlwidth(thin) labsize(small) nogrid) ///
	ylabel(, labcolor(gs4) tlcolor() tlwidth(thin) labsize(small) nogrid) 	///
	ciopts(lcolor(black)) 

***** Regional Dummies
qui reg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th east_asia
outreg2 using regionp.tex,addstat(RMSE, `e(rmse)') replace

qui reg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th europe
outreg2 using regionp.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th ussr
outreg2 using regionp.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th latin
outreg2 using regionp.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th mid_east
outreg2 using regionp.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th south_asia
outreg2 using regionp.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th sub_africa
outreg2 using regionp.tex,addstat(RMSE, `e(rmse)')  append 

qui reg z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th east_asia europe ussr latin mid_east sub_africa
outreg2 using regionp.tex,addstat(RMSE, `e(rmse)')  append 

	
	
**************************************
************** Regional **************
**************************************
*************** PROBIT ***************
** Pooled
*** Europe and North America
preserve
keep if region_id == "North America" | region_id == "Europe & Central Asia" 

drop if  (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan"| country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")

probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
est store m1, title(N America)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using poolprobreg.tex,  ctitle (Europe \& N America) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  replace eq(auto) 

restore


*** Latin America & Caribbean 
preserve

keep if region_id == "Latin America & Caribbean"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
est store m2, title(Lat Am & Carib)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using poolprobreg.tex, ctitle (LA \& Caribb) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 


restore 
 
*** Former USSR
preserve

keep if (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan" | country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")

probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
est store m3, title(Former FSU)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using poolprobreg.tex,  ctitle (Former USSR) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 
restore 
 
*** Middle East & North Africa 
preserve

keep if region_id == "Middle East & North Africa"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
est store m4, title(ME & N Africa)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using poolprobreg.tex,  ctitle (ME \& N Africa) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

restore 
 
 
*** Sub-Saharan Africa
preserve

keep if region_id == "Sub-Saharan Africa"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
est store m5, title(Sub-S Africa)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using poolprobreg.tex,  ctitle (Sub-S Africa) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

restore
 
 
*** South Asia
preserve

keep if region_id == "South Asia"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
est store m6, title(South Asia)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using poolprobreg.tex,  ctitle (South Asia) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

restore 
 
 
*** East Asia & Pacific
preserve

keep if region_id == "East Asia & Pacific"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th, vce(robust) 
est store m7, title(East Asia & Pacific)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using poolprobreg.tex,  ctitle (East Asia \& Pacific) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

restore 
 
*** Country FE probit region
*** Europe and North America
preserve
keep if region_id == "North America" | region_id == "Europe & Central Asia" 

drop if  (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan"| country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")

probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th  i.ccode, vce(robust) 
est store m1, title(N America)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitregion.tex,  ctitle (Europe \& N America) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  replace eq(auto) 

restore


*** Latin America & Caribbean 
preserve

keep if region_id == "Latin America & Caribbean"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th  i.ccode, vce(robust) 
est store m2, title(Lat Am & Carib)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitregion.tex, ctitle (LA \& Caribb) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 


restore 
 
*** Former USSR
preserve

keep if (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan" | country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")

probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th  i.ccode, vce(robust) 
est store m3, title(Former FSU)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitregion.tex,  ctitle (Former USSR) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 
restore 
 
*** Middle East & North Africa 
preserve

keep if region_id == "Middle East & North Africa"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th  i.ccode, vce(robust) 
est store m4, title(ME & N Africa)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitregion.tex,  ctitle (ME \& N Africa) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

restore 
 
 
*** Sub-Saharan Africa
preserve

keep if region_id == "Sub-Saharan Africa"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th  i.ccode, vce(robust) 
est store m5, title(Sub-S Africa)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitregion.tex,  ctitle (Sub-S Africa) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

restore
 
 
*** South Asia
preserve

keep if region_id == "South Asia"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th  i.ccode, vce(robust) 
est store m6, title(South Asia)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitregion.tex,  ctitle (South Asia) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

restore 
 
 
*** East Asia & Pacific
preserve

keep if region_id == "East Asia & Pacific"
probit onset_new lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th  i.ccode, vce(robust) 
est store m7, title(East Asia & Pacific)
estat ic
mat es_ic = r(S)
local AIC: display %4.1f es_ic[1,5]
outreg2 using probitregion.tex,  ctitle (East Asia \& Pacific) dec(3) addstat(AIC, `AIC', Log Lik, e(ll),Pseudo R2,e(r2_p)) groupvar(lagonset_new lagape  lagpts  lagdemoc lagbottom50  laglaborforce lagadjacent_conflict lagloggdppc lagloggdppc_growth  lagexports_gdp  laginternet  laglogpop_th)  append eq(auto) 

restore 
 


*************** FATALITIES *************** 
*** Europe and North America
preserve
keep if region_id == "North America" | region_id == "Europe & Central Asia" 

drop if  (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan" | country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")

xtpcse z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedf.tex, addstat(RMSE, `e(rmse)') ctitle (Europe \& N America) replace

restore


*** Latin America & Caribbean 
preserve

keep if region_id == "Latin America & Caribbean"
xtpcse z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedf.tex,addstat(RMSE, `e(rmse)') ctitle (LA \& Caribb) append

restore 
 
*** Former USSR 
preserve

keep if (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  |country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" |country == "Russian Federation" | country == "Tajikistan" | country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")

xtpcse z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedf.tex,addstat(RMSE, `e(rmse)') ctitle (Former USSR) append 

restore 
 
 
*** Middle East & North Africa 
preserve

keep if region_id == "Middle East & North Africa"
xtpcse z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedf.tex,addstat(RMSE, `e(rmse)') ctitle (ME \& N Africa) append

restore 
 
 
*** Sub-Saharan Africa
preserve

keep if region_id == "Sub-Saharan Africa"
xtpcse z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedf.tex,addstat(RMSE, `e(rmse)') ctitle (Sub-S Africa) append 

restore
 
 
*** South Asia
preserve

keep if region_id == "South Asia"
xtpcse z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedf.tex,addstat(RMSE, `e(rmse)') ctitle (South Asia) append 

restore 
 
 
*** East Asia & Pacific
preserve

keep if region_id == "East Asia & Pacific"
xtpcse z2lndeaths z2laglndeaths z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedf.tex,addstat(RMSE, `e(rmse)') ctitle (East Asia \& Pacific) append 

restore 
 

*************** DURATION *************** 
*** Europe and North America
preserve
keep if region_id == "North America" | region_id == "Europe & Central Asia" 

drop if  (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan" | country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")

xtpcse z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedd.tex,addstat(RMSE, `e(rmse)') ctitle (Europe \& N America) replace

restore


*** Latin America & Caribbean 
preserve

keep if region_id == "Latin America & Caribbean"
xtpcse z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedd.tex,addstat(RMSE, `e(rmse)')addstat(RMSE, `e(rmse)') ctitle (LA \& Caribb) append

restore 
 
*** Former USSR
preserve
keep if (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan" | country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")

xtpcse z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedd.tex,addstat(RMSE, `e(rmse)') ctitle (Former USSR) append 

restore 
 
 
*** Middle East & North Africa 
preserve

keep if region_id == "Middle East & North Africa"
xtpcse z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedd.tex,addstat(RMSE, `e(rmse)') ctitle (ME \& N Africa) append

restore 
 
 
*** Sub-Saharan Africa
preserve

keep if region_id == "Sub-Saharan Africa"
xtpcse z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedd.tex,addstat(RMSE, `e(rmse)')addstat(RMSE, `e(rmse)') ctitle (Sub-S Africa) append 

restore
 
 
*** South Asia
preserve

keep if region_id == "South Asia"
xtpcse z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedd.tex,addstat(RMSE, `e(rmse)') ctitle (South Asia) append 

restore 
 
 
*** East Asia & Pacific
preserve

keep if region_id == "East Asia & Pacific"
xtpcse z2lnduration z2laglnduration z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedd.tex,addstat(RMSE, `e(rmse)') ctitle (East Asia \& Pacific) append 

restore  

********* Protest  *********
*** Europe and North America
preserve
keep if region_id == "North America" | region_id == "Europe & Central Asia" 

drop if  (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan" | country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")
xtpcse z2lnprotest z2laglnprotest z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedp.tex,addstat(RMSE, `e(rmse)') ctitle (Europe \& N America) replace

restore


*** Latin America & Caribbean 
preserve

keep if region_id == "Latin America & Caribbean"
xtpcse z2lnprotest z2laglnprotest  z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedp.tex,addstat(RMSE, `e(rmse)') ctitle (LA \& Caribb) append

restore 
 
 
*** Former USSR
preserve
keep if (country == "Armenia" | country == "Azerbaijan" | country == "Belarus" | country == "Georgia" | country == "Estonia"  | country == "Kazakhstan" | country == "Kyrgyz Republic" | country == "Latvia" | country == "Lithuania" | country == "Moldova" | country == "Russian Federation" | country == "Tajikistan" | country == "Turkmenistan" | country == "Ukraine" | country == "Uzbekistan")
xtpcse z2lnprotest z2laglnprotest  z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedp.tex,addstat(RMSE, `e(rmse)') ctitle (Former USSR) append 

restore 
 
 
*** Middle East & North Africa 
preserve

keep if region_id == "Middle East & North Africa"
xtpcse z2lnprotest z2laglnprotest  z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedp.tex,addstat(RMSE, `e(rmse)') ctitle (ME \& N Africa) append

restore 
 
 
*** Sub-Saharan Africa
preserve

keep if region_id == "Sub-Saharan Africa"
xtpcse z2lnprotest z2laglnprotest  z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedp.tex,addstat(RMSE, `e(rmse)') ctitle (Sub-S Africa) append 

restore
 
 
*** South Asia
preserve

keep if region_id == "South Asia"
xtpcse z2lnprotest z2laglnprotest  z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedp.tex,addstat(RMSE, `e(rmse)') ctitle (South Asia) append 

restore 
 
 
*** East Asia & Pacific
preserve

keep if region_id == "East Asia & Pacific"
xtpcse z2lnprotest z2laglnprotest  z2lagape z2lagpts z2lagdemoc z2lagbottom50 z2laglaborforce z2lagadjacent_conflict z2lagloggdppc z2lagloggdppc_growth z2lagexports_gdp z2laginternet z2laglogpop_th i.ccode,correlation (psar1) pairwise
outreg2 using regionfixedp.tex,addstat(RMSE, `e(rmse)') ctitle (East Asia \& Pacific) append 

restore  
 


 
 
 
 
 
 
 
 