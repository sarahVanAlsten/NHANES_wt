libname julia 'C:\Users\aduncan\Box\Manuscripts\In progress\Julia_SMW&Race&health';



proc contents data = nhanes_.sxq_g;
run;

proc sort data= nhanes_.demo_h; by seqn;
run;
proc sort data= nhanes_.hsq_h; by seqn;
proc sort data= nhanes_.sxq_h; by seqn;
proc sort data= nhanes_.dpq_h; by seqn;
proc sort data= nhanes_.alq_h; by seqn;
proc sort data= nhanes_.duq_h; by seqn;
proc sort data= nhanes_.smq_h; by seqn;

proc sort data= nhanes_.demo_g; by seqn;
proc sort data= nhanes_.hsq_g; by seqn;
proc sort data= nhanes_.sxq_g; by seqn;
proc sort data= nhanes_.dpq_g; by seqn;
proc sort data= nhanes_.alq_g; by seqn;
proc sort data= nhanes_.duq_g; by seqn;
proc sort data= nhanes_.smq_g; by seqn;

proc sort data= nhanes_.demo_i; by seqn;
proc sort data= nhanes_.hsq_i; by seqn;
proc sort data= nhanes_.sxq_i; by seqn;
proc sort data= nhanes_.dpq_i; by seqn;
proc sort data= nhanes_.alq_i; by seqn;
proc sort data= nhanes_.duq_i; by seqn;
proc sort data= nhanes_.smq_i; by seqn;
run;

data nhanes.combined;
merge 
nhanes_.demo_i 
nhanes_.hsq_i
nhanes_.sxq_i
nhanes_.dpq_i
nhanes_.alq_i
nhanes_.duq_i
nhanes_.smq_i
nhanes_.demo_h 
nhanes_.hsq_h
nhanes_.sxq_h
nhanes_.dpq_h
nhanes_.alq_h
nhanes_.duq_h
nhanes_.smq_h
nhanes_.demo_g 
nhanes_.hsq_g
nhanes_.sxq_g
nhanes_.dpq_g
nhanes_.alq_g
nhanes_.duq_g
nhanes_.smq_g;
by SEQN;
run;


/*
  proc contents data=julia.combined ; run;
proc means data=julia.combined n nmiss min max maxdec=2;
run;

*/
  
  data combined_data;
set nhanes.combined;

if sddsrvyr in (7,8,9) then WTMEC6YR= 1/3*WTMEC2YR;

/*if ridreth1 = 3 then minority = 0;
if ridreth1 in (1,2,4,5) then minority = 1;*/ 
  
  if ridreth1 =3 then race = 0;
if ridreth1 =4 then race = 1;  
if ridreth1 in (1,2) then race = 2; 
if ridreth1 =5 then race = 3; 

if race= 0 then minority = 0;
if race in (1, 2, 3) then minority = 1;

if riagendr = 1 then gender = 0;
if riagendr = 2 then gender = 1;

***Changed;
if ridageyr ge 20 and ridageyr le 29 then age4 = 1;
if ridageyr gt 29 and ridageyr le 39 then age4 = 2;
if ridageyr gt 39 and ridageyr le 49 then age4 = 3;
else if ridageyr gt 49 and ridageyr le 59 then age4=4;


***NOTE: I changed this so that all people who were out of age range for the sexual orientation questions
were coded 2;

if ridageyr ge 20 and ridageyr le 39 then age = 0;
else if ridageyr ge 40 and ridageyr le 59 then age = 1;
else age = 2;


if DMDMARTL =1 then maritalstatus = 0;
if DMDMARTL =5 then maritalstatus = 1;
if DMDMARTL =6 then maritalstatus = 2;
if DMDMARTL in (2, 3, 4) then maritalstatus = 3;

if maritalstatus in (0,1) then maritalstatus2 = 0;
if maritalstatus = 2 then maritalstatus2 = 1;
if maritalstatus = 3 then maritalstatus2 = 2;


if DMDEDUC2 in (1, 2) then edu = 0;
if DMDEDUC2 = 3 then edu= 1;
if DMDEDUC2 in (4,5) then edu = 2;



***added a category for missing so that we don't end up dropping 209 people when we add this variable to the model;
if indfmin2 in(1, 2, 3, 4, 13) then income = 0;
  else if indfmin2 in (5, 6, 7, 8, 9, 10, 12, 14, 15) then income= 1;
  else income = 2;
Label income = annual family income;

if gender=1 then do;
if sxq700 ne 1 and sxq703 ne 1 and sxq706 ne 1 and sxq709 ne 1 then neversex=1;
  else neversex=0;
end;

if sddsrvyr in (7,8) then do;
if sxq294 = 1 then whet = 1;
  else if sxq294 in (2,3,4,5,9) then whet=0;

if sxq294 = 1 then orient=1;
  else if sxq294 in (2,3) then orient=2;
  else if sxq294 in (4) then orient=3;
  else if sxq294 in (5,9) then orient=4;

if sxq709 = 1 then wsexw = 1;
  else if sxq709 = 2 then wsexw=0;

if sxq294 = 1 or sxq709 = 2 then newwsw = 0;
if sxq294 in (2, 3, 4) or sxq709 = 1 then newwsw = 1;
end;


if sddsrvyr in (9) then do;
if sxq295 = 2 then whet = 1;
  else if sxq295 in (1,3,4,6,9) then whet=0;

  if sxq295 = 2 then orient=1;
  else if sxq295 in (1,3) then orient=2;
  else if sxq295 in (4) then orient=3;
  else if sxq295 in (6,9) then orient=4;

if sxq709 = 1 then wsexw = 1;
  else if sxq709 = 2 then wsexw=0;

if sxq295 = 2 or sxq709 = 2 then newwsw = 0;
if sxq295 in (1,3,4,6) or sxq709 = 1 then newwsw = 1;
end;

label whet = heterosexual female;
label orient = female hetero, lesbian/bi, something else, do not know;
label wsexw = woman who has had sex with another woman;
label newwsw = identifes as sexual minority or has had sex with another woman;




array depress (10) dpq010 dpq020 dpq030 dpq040 dpq050 dpq060 dpq070 dpq080 dpq090 dpq100;
array depress_r (10) dep1-dep10;
do i = 1 to 10;
  if depress(i) in (0,1,2,3) then depress_r(i) = depress(i);
  end;


phq = sum(of dep1-dep10);


if phq ge 10 and phq le 27 then dep = 1;
if phq lt 10 and phq ge 0 then dep = 0;

***changed so that good/fair/bad health was coded 1 and very good/excellent was coded 0);
if hsd010 in (4, 5) then badhealth = 1;
if hsd010 in (1, 2, 3) then badhealth = 0;

if hsd010 in (4, 5) then goodhealth = 0;
if hsd010 in (1, 2, 3) then goodhealth = 1;


if SMQ040 = 3 then smoker = 0;
if SMQ040 in (1, 2) then smoker = 1;

if SMQ020 = 1 then lifetimesmoker = 1;
if SMQ020 = 2 then lifetimesmoker = 0;

***added marijuana variables;
if DUQ200 = 1 then mjever = 1;
  else if duq200 = 2 then mjever = 0;

if DUQ211 = 1 then momjuse = 1;
  else if DUQ211 = 2 or mjever = 0 then momjuse = 0;

if DUQ217 = 5 then mjuse4 = 3;
  else if DUQ217 in (1,2,3,4) then mjuse4 = 2;
  else if mjever = 1 and momjuse ne . and duq217 not in (7,9) then mjuse4 = 1;
  else if mjever = 0 then mjuse4 = 0;

if mjuse4 = 3 then dailymjuse = 1;
  else if mjuse4 in (0,1,2) then dailymjuse = 0;

LABEL momjuse = monthly marijuana use;
LABEL mjuse4 = daily, monthly, less than monthly, never;


if DUQ240 = 2 then illdruguse = 0;
if DUQ240 = 1 then illdruguse = 1;

if alq101 = 1 or alq110 = 1 then everdrink12 = 1;
  else if alq101=2 and alq110=2 then everdrink12=0;
label everdrink12 = ever had 12 drinks in lifetime;

if alq120q = 0 or everdrink12 = 0 then drink12mo=0;
  else if alq120u in (1,2,3) then drink12mo=1;
label drink12mo = drank in past 12 months;


if alq141u = 1 then drinking = alq141q *4.3;
if alq141u = 2 then drinking = alq141q;
if alq141u = 3 then drinking = alq141q/12;

if  1<= drinking <5 then alcohol = 2;
else if drinking >=5 then alcohol = 3;
else if drink12mo = 1 then alcohol = 1;
else if .< drinking <1 or everdrink12 ne . then alcohol = 0;


if (newwsw = 0 and minority =0) then mwsw= 0;
if (newwsw = 1 and minority = 1) then mwsw= 1;
if (newwsw = 1 and minority = 0) then mwsw= 2;
if (newwsw = 0 and minority = 1) then mwsw=3;

if (whet = 1 and minority =0) then orient_mwsw= 0;
  else if (whet = 0 and minority = 1) then orient_mwsw= 3;
  else if (whet = 0 and minority = 0) then orient_mwsw= 2;
  else if (whet = 1 and minority = 1) then orient_mwsw=1;
label orient_mwsw = sexual orientation and race - no WSW;


if (wsexw = 0 and minority =0) then wsexw_mwsw= 0;
  else if (wsexw = 1 and minority = 1) then wsexw_mwsw= 3;
  else if (wsexw = 1 and minority = 0) then wsexw_mwsw= 2;
  else if (wsexw = 0 and minority = 1) then wsexw_mwsw=1;
label wsexw_mwsw = sexual behavior and race - no orientation;

if (newwsw = 1 and minority = 0) then wswrace=0;
if (newwsw = 1 and minority = 1) then wswrace=1;

if mwsw ne . and age in (0,1) then subpop=1;


proc freq; tables orient_mwsw*minority*race /list missprint;
run;


keep sdmvstra sdmvpsu wtmec6yr gender wswrace mwsw newwsw race age age4 edu income maritalstatus2 dep alcohol illdruguse lifetimesmoker minority
  whet wsexw mjever wswrace mjuse4 momjuse dailymjuse badhealth goodhealth subpop wsexw_mwsw
  orient_mwsw;


run;



proc freq; tables mwsw; where subpop=1;
run;

data nhanes.stdata;
set combined_data;

run;

proc surveyfreq data=combined_data; 
strata sdmvstra;
cluster sdmvpsu;
weight wtmec6yr; 
table subpop*mwsw*race subpop*mwsw*(age edu income maritalstatus2 dep lifetimesmoker alcohol illdruguse mjuse4 health momjuse dailymjuse) /chisq; run;


PROC SURVEYLOGISTIC DATA=combined;
 CLUSTER SDMVPSU; STRATA SDMVSTRA; WEIGHT WTMEC4YR;
 CLASS mwsw (ref='0') age (ref='0') edu (ref='0') income (ref='0') maritalstatus2 (ref='0') /param=ref;
 MODEL dep(EVENT='1') = mwsw age edu income maritalstatus2 /rsq ;
RUN; 

PROC SURVEYLOGISTIC DATA=combined;
 CLUSTER SDMVPSU; STRATA SDMVSTRA; WEIGHT WTMEC4YR;
 CLASS mwsw (ref='0') age (ref='0') edu (ref='0') income (ref='0') maritalstatus2 (ref='0') /param=ref;
 MODEL alcohol (ref='0')= mwsw age edu income maritalstatus2 /link=glogit;
RUN; 


PROC SURVEYLOGISTIC DATA=combined;
 CLUSTER SDMVPSU; STRATA SDMVSTRA; WEIGHT WTMEC4YR;
 CLASS mwsw (ref='0') age (ref='0') edu (ref='0') income (ref='0') maritalstatus2 (ref='0') /param=ref;
 MODEL druguse(EVENT='1')= mwsw age edu income maritalstatus2;
RUN; 


PROC SURVEYLOGISTIC DATA=combined;
 CLUSTER SDMVPSU; STRATA SDMVSTRA; WEIGHT WTMEC4YR;
 CLASS mwsw (ref='0') age (ref='0') edu (ref='0') income (ref='0') maritalstatus2 (ref='0') /param=ref;
 MODEL lifetimesmoker(EVENT='1')= mwsw age edu income maritalstatus2;
RUN; 


PROC SURVEYLOGISTIC DATA=combined;
 CLUSTER SDMVPSU; STRATA SDMVSTRA;  WEIGHT WTMEC4YR;
 CLASS mwsw (ref='0') age (ref='0') edu (ref='0') income (ref='0') maritalstatus2 (ref='0') /param=ref;
 MODEL health(EVENT='0')= mwsw age edu income maritalstatus2;
RUN; 
