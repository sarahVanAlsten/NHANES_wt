//read in data
//note: you need spost13 for some of these commands

//read in the data
//use "C:/Users/sarah.vanalsten/Downloads/nhanes2.dta", clear
use "C:\Users\Owner\OneDrive\Documents\Duncan_Lab_2018\NHANES_WeightPerception\NHANES_wt\data\nhanes30", clear

//recode a new age variable
recode RIDAGEYR (18/30 = 1) (30/40 = 2) (40/50 = 3) (50/max = 4), gen(ageNew)
recode RIDAGEYR (18/30 = 1) (30/40 = 2) (40/50 = 3) (50/59 = 4) (60/max = 5), gen(ageNew2)

gen raceNew = .
replace raceNew = 0 if Race ==0 
replace raceNew = 1 if Race > 0

tab Race raceNew

tab ageNew
tab cycle

gen cyclefact = .
replace cyclefact = 1 if cycle == "2005-2006"
replace cyclefact = 2 if cycle == "2007-2008"
replace cyclefact = 3 if cycle == "2009-2010"
replace cyclefact = 4 if cycle == "2011-2012"
replace cyclefact = 5 if cycle == "2013-2014"

gen fsAny = 0
replace fsAny = 1 if fsWithHunger > 0

gen obese = .
replace obese = 1 if BMIcat > 3
replace obese = 0 if BMIcat <= 3

gen overweight = .
replace overweight = 1 if BMIcat ==3
replace overweight = 0 if BMIcat != 3

//set the survey sampling wt
svyset SDMVPSU [pweight = WTMEC10YR], strata(SDMVSTRA)


////////////////////////////////////////////////////////////
//What they're doing about weight
///////////////////////////////////////////////////////////
//base model
svy: mlogit doingAbtWt i.fsAny, rrr baseoutcome(5)
mlogtest, wald
listcoef, help

//adjust for confounders except BMIcat
svy: mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Male i.Race, rrr baseoutcome(5)
mlogtest, wald
listcoef, help

svy: mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Male i.Race i.BMIcat, rrr baseoutcome(5)
mlogtest, wald
listcoef, help

svy: mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Male i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(5)
mlogtest, wald
listcoef, help

//model including interaction with sex
svy: mlogit doingAbtWt i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat, rrr baseoutcome(5)
mlogtest, wald

//test if interaction terms simultaneously = 0
test 0.fsAny#1.Male 1.fsAny#1.Male , nosvyadjust

//model including interaction with Race
svy: mlogit doingAbtWt i.fsAny##i.Race i.ageNew i.edu i.Male i.BMIcat, rrr baseoutcome(5)
mlogtest, wald
listcoef, help

//test if interaction terms for race by simultaneously = 0
test 1.fsAny#1.Race 0.fsAny#1.Race , nosvyadjust
test 1.fsAny#2.Race 0.fsAny#2.Race , nosvyadjust
test 1.fsAny#3.Race 0.fsAny#3.Race , nosvyadjust

//3 way interaction?... Nope
svy: mlogit doingAbtWt i.fsAny##i.Race##i.Male i.ageNew i.edu i.BMIcat, rrr


svy, subpop(if Race == 0):mlogit doingAbtWt i.fsAny i.ageNew i.edu i.BMIcat i.Male i.depressionBinary, rrr baseoutcome(5)
svy, subpop(if Race == 1):mlogit doingAbtWt i.fsAny i.ageNew i.edu i.BMIcat i.Male i.depressionBinary, rrr baseoutcome(5)
svy, subpop(if Race == 2):mlogit doingAbtWt i.fsAny i.ageNew i.edu i.BMIcat i.Male i.depressionBinary, rrr baseoutcome(5)


svy, subpop(if Race == 0): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.BMIcat i.Male, rrr baseoutcome(5)
svy, subpop(if Race== 1): mlogit doingAbtWt i.fsAny i.ageNew i.BMIcat i.Male i.edu, rrr baseoutcome(5)

/////////////////////////////////////////////////////////////////////
// What they would like to weigh
////////////////////////////////////////////////////////////////////
//base model
svy: mlogit LikeToWeigh i.fsAny, rrr baseoutcome(0)
mlogtest, wald
listcoef, help

svy: mlogit ConsiderWt i.fsAny, rrr baseoutcome(0)
mlogtest, wald
listcoef, help

svy: mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(0)
svy: mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(0)

svy: mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(0)
svy: mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(0)

svy: mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy: mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)

svy: mlogit LikeToWeigh i.fsAny##i.Race i.ageNew i.edu i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)

svy: mlogit LikeToWeigh i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy: mlogit LikeToWeigh i.fsAny##i.Male##i.Race i.ageNew i.edu  i.BMIcat i.depressionBinary, rrr baseoutcome(0)

svy, subpop(if Male ==0): mlogit LikeToWeigh i.fsAny i.Race i.ageNew i.edu i.BMIcat, rrr baseoutcome(0)
svy, subpop(if Male ==1): mlogit LikeToWeigh i.fsAny i.Race i.ageNew i.edu i.BMIcat, rrr baseoutcome(0)

svy, subpop(if Male ==0): mlogit LikeToWeigh i.fsAny i.Race i.ageNew i.depressionBinary i.edu i.BMIcat, rrr baseoutcome(0)
svy, subpop(if Male ==1): mlogit LikeToWeigh i.fsAny i.Race i.ageNew i.depressionBinary i.edu i.BMIcat, rrr baseoutcome(0)


svy: mlogit ConsiderWt i.fsAny##i.Race i.Male i.ageNew i.edu  i.BMIcat i.depressionBinary, rrr baseoutcome(0)
mlogtest, wald
svy: mlogit ConsiderWt i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy: mlogit ConsiderWt i.fsAny##i.Male##i.Race i.ageNew i.edu  i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//because of effect mod, get subpops by sex AND Race
svy, subpop(if Male == 1): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.BMIcat i.Race, rrr baseoutcome(0)
svy, subpop(if Male == 0): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.BMIcat i.Race, rrr baseoutcome(0)
svy, subpop(if Male == 1): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.BMIcat i.Race i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if Male == 0): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.BMIcat i.Race i.depressionBinary, rrr baseoutcome(0)


margins
mchange
margins, at (Race = 0 fsAny = 0 Male = 0)
margins, at (Race = 0 fsAny = 0 Male = 1)
margins, at (Race = 0 fsAny = 1 Male = 0)
margins, at (Race = 0 fsAny = 1 Male = 1)

margins, at (Race = 1 fsAny = 0 Male = 0)
margins, at (Race = 1 fsAny = 0 Male = 1)
margins, at (Race = 1 fsAny = 1 Male = 0)
margins, at (Race = 1 fsAny = 1 Male = 1)

margins, at (Race = 2 fsAny = 0 Male = 0)
margins, at (Race = 2 fsAny = 0 Male = 1)
margins, at (Race = 2 fsAny = 1 Male = 0)
margins, at (Race = 2 fsAny = 1 Male = 1)

margins, at (Race = 3 fsAny = 0 Male = 0)
margins, at (Race = 3 fsAny = 0 Male = 1)
margins, at (Race = 3 fsAny = 1 Male = 0)
margins, at (Race = 3 fsAny = 1 Male = 1)

////////////////////////////////////////////////////////////////////////////////
//subset to individuals with obesity only and see if there's anything there
////////////////////////////////////////////////////////////////////////

//unadj
svy, subpop(if obese == 1): mlogit doingAbtWt i.fsAny, rrr baseoutcome(5)

//model 2
svy, subpop(if obese == 1): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(5)

//also adjust for bmicat (degree of obesity here)
svy, subpop(if obese == 1): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(5)

//and depression
svy, subpop(if obese == 1): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(5)

//look for interactions
svy, subpop(if obese == 1): mlogit doingAbtWt i.fsAny##i.Race i.ageNew i.edu i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(5)
mlogtest, wald
svy, subpop(if obese == 1): mlogit doingAbtWt i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(5)
svy, subpop(if obese == 1): mlogit doingAbtWt i.fsAny##i.Race##i.Male i.ageNew i.edu i.BMIcat i.depressionBinary, rrr baseoutcome(5)


//////////////what they would liketo weigh
svy, subpop(if obese == 1): mlogit LikeToWeigh i.fsAny, rrr baseoutcome(0)

//model 2
svy, subpop(if obese == 1): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(0)

//also adjust for bmicat (degree of obesity here)
svy, subpop(if obese == 1): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(0)

//and depression
svy, subpop(if obese == 1): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//look for interactions
svy, subpop(if obese == 1): mlogit LikeToWeigh i.fsAny##i.Race i.ageNew i.eduNew i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if obese == 1): mlogit LikeToWeigh i.fsAny##i.Male i.ageNew i.eduNew i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if obese == 1): mlogit LikeToWeigh i.fsAny##i.Race##i.Male i.ageNew i.eduNew i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//how they consider their weight
//////////////what they would liketo weigh
svy, subpop(if obese == 1): mlogit ConsiderWt i.fsAny, rrr baseoutcome(0)

//model 2
svy, subpop(if obese == 1): mlogit ConsiderWt i.fsAny i.ageNew i.ed i.Race i.Male, rrr baseoutcome(0)

//also adjust for bmicat (degree of obesity here)
svy, subpop(if obese == 1): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(0)

//and depression
svy, subpop(if obese == 1): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//look for interactions
svy, subpop(if obese == 1): mlogit ConsiderWt i.fsAny##i.Race i.ageNew i.eduNew i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if obese == 1): mlogit ConsiderWt i.fsAny##i.Male i.ageNew i.eduNew i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if obese == 1): mlogit ConsiderWt i.fsAny##i.Race##i.Male i.ageNew i.eduNew i.BMIcat i.depressionBinary, rrr baseoutcome(0)
////////////////////////////////////////////////////////////////////////////
//and what if we do obese OR overweight
//unadj
svy, subpop(if BMIcat >2): mlogit doingAbtWt i.fsAny, rrr baseoutcome(5)

//model 2
svy, subpop(if BMIcat >2): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(5)

//also adjust for bmicat (degree of obesity here)
svy, subpop(if BMIcat>2): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(5)

//and depression
svy, subpop(if BMIcat>2): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(5)

//look for interactions
svy, subpop(if BMIcat >2): mlogit doingAbtWt i.fsAny##i.Race i.ageNew i.eduNew i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(5)
mlogtest, wald
svy, subpop(if BMIcat >2): mlogit doingAbtWt i.fsAny##i.Male i.ageNew i.eduNew i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(5)
svy, subpop(if BMIcat >2): mlogit doingAbtWt i.fsAny##i.Race##i.Male i.ageNew i.eduNew i.BMIcat i.depressionBinary, rrr baseoutcome(5)


//////////////what they would liketo weigh
svy, subpop(if BMIcat >2): mlogit LikeToWeigh i.fsAny, rrr baseoutcome(0)

//model 2
svy, subpop(if BMIcat >2): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(0)

//also adjust for bmicat (degree of obesity here)
svy, subpop(if BMIcat >2): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(0)

//and depression
svy, subpop(if BMIcat >2): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//look for interactions
svy, subpop(if BMIcat >2): mlogit LikeToWeigh i.fsAny##i.Race i.ageNew i.edu i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if BMIcat >2): mlogit LikeToWeigh i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if BMIcat >2): mlogit LikeToWeigh i.fsAny##i.Race##i.Male i.ageNew i.edu i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//how they consider their weight
//////////////what they would liketo weigh
svy, subpop(if BMIcat >2): mlogit ConsiderWt i.fsAny, rrr baseoutcome(0)

//model 2
svy, subpop(if BMIcat >2): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(0)

//also adjust for bmicat (degree of obesity here)
svy, subpop(if BMIcat >2): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(0)

//and depression
svy, subpop(if BMIcat >2): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//look for interactions
svy, subpop(if BMIcat >2): mlogit ConsiderWt i.fsAny##i.Race i.ageNew i.edu i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if BMIcat >2): mlogit ConsiderWt i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if BMIcat >2): mlogit ConsiderWt i.fsAny##i.Race##i.Male i.ageNew i.edu i.BMIcat i.depressionBinary, rrr baseoutcome(0)
/////////////////////////////////////////////////////////////////////
//amon 'normal' weight
//unadj
svy, subpop(if BMIcat ==2): mlogit doingAbtWt i.fsAny, rrr baseoutcome(5)

//model 2
svy, subpop(if BMIcat ==2): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(5)

//also adjust for bmicat (degree of obesity here)
svy, subpop(if BMIcat==2): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(5)

//and depression
svy, subpop(if BMIcat==2): mlogit doingAbtWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(5)

//look for interactions
svy, subpop(if BMIcat ==2): mlogit doingAbtWt i.fsAny##i.Race i.ageNew i.edu i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(5)
mlogtest, wald
svy, subpop(if BMIcat ==2): mlogit doingAbtWt i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(5)
svy, subpop(if BMIcat ==2): mlogit doingAbtWt i.fsAny##i.Race##i.Male i.ageNew i.edu i.BMIcat i.depressionBinary, rrr baseoutcome(5)


//////////////what they would liketo weigh
svy, subpop(if BMIcat ==2): mlogit LikeToWeigh i.fsAny, rrr baseoutcome(0)

//model 2
svy, subpop(if BMIcat ==2): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(0)

//also adjust for bmicat (degree of obesity here)
svy, subpop(if BMIcat ==2): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat, rrr baseoutcome(0)

//and depression
svy, subpop(if BMIcat ==2): mlogit LikeToWeigh i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//look for interactions
svy, subpop(if BMIcat ==2): mlogit LikeToWeigh i.fsAny##i.Race i.ageNew i.edu i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if BMIcat ==2): mlogit LikeToWeigh i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if BMIcat ==2): mlogit LikeToWeigh i.fsAny##i.Race##i.Male i.ageNew i.edu i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//how they consider their weight
//////////////what they would liketo weigh
svy, subpop(if BMIcat ==2): mlogit ConsiderWt i.fsAny, rrr baseoutcome(0)

//model 2
svy, subpop(if BMIcat ==2): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male, rrr baseoutcome(0)

//and depression
svy, subpop(if BMIcat ==2): mlogit ConsiderWt i.fsAny i.ageNew i.edu i.Race i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)

//look for interactions
svy, subpop(if BMIcat ==2): mlogit ConsiderWt i.fsAny##i.Race i.ageNew i.edu i.Male i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if BMIcat ==2): mlogit ConsiderWt i.fsAny##i.Male i.ageNew i.edu i.Race i.BMIcat i.depressionBinary, rrr baseoutcome(0)
svy, subpop(if BMIcat ==2): mlogit ConsiderWt i.fsAny##i.Race##i.Male i.ageNew i.edu i.BMIcat i.depressionBinary, rrr baseoutcome(0)
