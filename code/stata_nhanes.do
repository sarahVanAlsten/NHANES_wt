//read in data
//note: you need spost13 for some of these commands

//read in the data
use "C:/Users/sarah.vanalsten/Downloads/nhanes.dta", clear

//set the survey sampling wt
svyset SDMVPSU [pweight = WTMEC6YR], strata(SDMVSTRA)

//run the regressions to see if interactions by race and sex
//are significant

////////////////////////////////////////////////////////////
//What they're doing about weight
///////////////////////////////////////////////////////////
//base model
svy: mlogit doingWt i.fsWithHunger, rrr
mlogtest, wald
listcoef, help

//adjust for confounders except BMIcat
svy: mlogit doingWt i.fsWithHunger i.age4 i.edu i.Race, rrr
mlogtest, wald
listcoef, help

//adjust for confounders, male not race, except BMIcat
svy: mlogit doingWt i.fsWithHunger i.age4 i.edu i.Male, rrr
mlogtest, wald
listcoef, help

//add BMIcat
svy: mlogit doingWt i.fsWithHunger i.age4 i.edu i.Race i.BMIcat, rrr
mlogtest, wald

svy: mlogit doingWt i.fsWithHunger i.age4 i.edu i.Male i.BMIcat, rrr
mlogtest, wald
listcoef, help

//model including interaction with sex
svy: mlogit doingWt i.fsWithHunger##i.Male i.age4 i.edu i.Race i.BMIcat, rrr
mlogtest, wald

//test if interaction terms simultaneously = 0
test 2.fsWithHunger#1.Male 1.fsWithHunger#1.Male , nosvyadjust

//model including interaction with Race
svy: mlogit doingWt i.fsWithHunger##i.Race i.age4 i.edu i.Male i.BMIcat, rrr
mlogtest, wald
listcoef, help

//test if interaction terms for race by simultaneously = 0
test 1.fsWithHunger#1.Race 2.fsWithHunger#1.Race , nosvyadjust
test 1.fsWithHunger#2.Race 2.fsWithHunger#2.Race , nosvyadjust
test 1.fsWithHunger#3.Race 2.fsWithHunger#3.Race , nosvyadjust

//3 way interaction?... Nope
svy: mlogit doingWt i.fsWithHunger##i.Race##i.Male i.age4 i.edu i.BMIcat, rrr
/////////////////////////////////////////////////////////////////////
// What they would like to weigh
////////////////////////////////////////////////////////////////////
//base model
svy: mlogit likeToWeigh i.fsWithHunger, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//adjust for confounders except BMIcat
svy: mlogit likeToWeigh i.fsWithHunger i.age4 i.edu i.Race, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//adjust for confounders, male not race, except BMIcat
svy: mlogit likeToWeigh i.fsWithHunger i.age4 i.edu i.Male, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//add BMIcat
svy: mlogit likeToWeigh i.fsWithHunger i.age4 i.edu i.Race i.BMIcat, rrr baseoutcome(1)
mlogtest, wald

svy: mlogit likeToWeigh i.fsWithHunger i.age4 i.edu i.Male i.BMIcat, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//model including interaction with sex
svy: mlogit likeToWeigh i.fsWithHunger##i.Male i.age4 i.edu i.Race i.BMIcat, rrr baseoutcome(1)
mlogtest, wald

//test if interaction terms simultaneously = 0
test 2.fsWithHunger#1.Male 1.fsWithHunger#1.Male , nosvyadjust

//model including interaction with Race
svy: mlogit likeToWeigh i.fsWithHunger##i.Race i.age4 i.edu i.Male i.BMIcat, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//test if interaction terms for race by simultaneously = 0
test 1.fsWithHunger#1.Race 2.fsWithHunger#1.Race , nosvyadjust
test 1.fsWithHunger#2.Race 2.fsWithHunger#2.Race , nosvyadjust
test 1.fsWithHunger#3.Race 2.fsWithHunger#3.Race , nosvyadjust

//3 way interaction?... Nope again
svy: mlogit likeToWeigh i.fsWithHunger##i.Race##i.Male i.age4 i.edu i.BMIcat, rrr baseoutcome(1)
///////////////////////////////////////////////////////
// How they Consider their weight
/////////////////////////////////////////////////////////
//base model
svy: mlogit consid i.fsWithHunger, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//adjust for confounders except BMIcat
svy: mlogit consid i.fsWithHunger i.age4 i.edu i.Race, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//adjust for confounders, male not race, except BMIcat
svy: mlogit consid i.fsWithHunger i.age4 i.edu i.Male, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//add BMIcat
svy: mlogit consid i.fsWithHunger i.age4 i.edu i.Race i.BMIcat, rrr baseoutcome(1)
mlogtest, wald

svy: mlogit consid i.fsWithHunger i.age4 i.edu i.Male i.BMIcat, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//model including interaction with sex
svy: mlogit consid i.fsWithHunger##i.Male i.age4 i.edu i.Race i.BMIcat, rrr baseoutcome(1)
mlogtest, wald

//test if interaction terms simultaneously = 0
test 2.fsWithHunger#1.Male 1.fsWithHunger#1.Male , nosvyadjust

//model including interaction with Race
svy: mlogit consid i.fsWithHunger##i.Race i.age4 i.edu i.Male i.BMIcat, rrr baseoutcome(1)
mlogtest, wald
listcoef, help

//test if interaction terms for race by simultaneously = 0
test 1.fsWithHunger#1.Race 2.fsWithHunger#1.Race , nosvyadjust
test 1.fsWithHunger#2.Race 2.fsWithHunger#2.Race , nosvyadjust
test 1.fsWithHunger#3.Race 2.fsWithHunger#3.Race , nosvyadjust

//3 way interaction?... here YES
svy: mlogit consid i.fsWithHunger##i.Race##i.Male i.age4 i.edu i.BMIcat, rrr baseoutcome(1)
margins
mchange
margins, at (Race = 0 fsWithHunger = 0 Male = 0)
margins, at (Race = 0 fsWithHunger = 0 Male = 1)
margins, at (Race = 0 fsWithHunger = 1 Male = 0)
margins, at (Race = 0 fsWithHunger = 1 Male = 1)
margins, at (Race = 0 fsWithHunger = 2 Male = 0)
margins, at (Race = 0 fsWithHunger = 2 Male = 1)
margins, at (Race = 1 fsWithHunger = 0 Male = 0)
margins, at (Race = 1 fsWithHunger = 0 Male = 1)
margins, at (Race = 1 fsWithHunger = 1 Male = 0)
margins, at (Race = 1 fsWithHunger = 1 Male = 1)
margins, at (Race = 1 fsWithHunger = 2 Male = 0)
margins, at (Race = 1 fsWithHunger = 2 Male = 1)
margins, at (Race = 2 fsWithHunger = 0 Male = 0)
margins, at (Race = 2 fsWithHunger = 0 Male = 1)
margins, at (Race = 2 fsWithHunger = 1 Male = 0)
margins, at (Race = 2 fsWithHunger = 1 Male = 1)
margins, at (Race = 2 fsWithHunger = 2 Male = 0)
margins, at (Race = 2 fsWithHunger = 2 Male = 1)
margins, at (Race = 3 fsWithHunger = 0 Male = 0)
margins, at (Race = 3 fsWithHunger = 0 Male = 1)
margins, at (Race = 3 fsWithHunger = 1 Male = 0)
margins, at (Race = 3 fsWithHunger = 1 Male = 1)
margins, at (Race = 3 fsWithHunger = 2 Male = 0)
margins, at (Race = 3 fsWithHunger = 2 Male = 1)
