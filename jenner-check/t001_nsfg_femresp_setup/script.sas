/*--------------------------------------------------------------------------
 |  NSFG 2006-2010 Female Respondent setup — compatibility bundle
 |
 |  Adapted from inputs/data/2006_2010_FemRespSetup.sas in this repository.
 |  The variable names and the LABEL text below are taken verbatim from that
 |  setup file. The full setup reads a 6251-byte fixed-record IPUMS/NSFG
 |  extract that is not distributed with the repo; here the manuscript-relevant
 |  subset (age, education, race/ethnicity, parity, survey weights) is read
 |  from a small inline sample of 25 synthetic records so the step runs
 |  standalone. A short childlessness tabulation (PARITY = 0 by education and
 |  race) is added at the end, mirroring the quantities the manuscript estimates.
 *-------------------------------------------------------------------------*/

DATA femresp;
   INPUT CASEID AGE_R EDUCAT HIEDUC HISPANIC RACE PARITY BIRTHS5
         FINALWGT30 WGTQ5Q16;

   * LABEL text taken verbatim from 2006_2010_FemRespSetup.sas;
   LABEL
      CASEID = "Respondent ID number"
      AGE_R = "R's age at interview"
      EDUCAT = "Education (number of years of schooling) (RECODE)"
      HIEDUC = "Highest completed year of school or highest degree received (RECODE)"
      HISPANIC = "Hispanic origin of respondent (RECODE)"
      RACE = "Race of respondent (RECODE)"
      PARITY = "CAPI-based total number of live births (accounting for mult birth) (RECODE)"
      BIRTHS5 = "Number of live births in last 5 years (RECODE)"
      FINALWGT30 = "Final weight for the 1st 2 1/2 years of data collection (quarters 1 to 10)"
      WGTQ5Q16 = "Final weight for years 2, 3, and 4 of data collection (quarters 5 to 16)"
   ;
DATALINES;
1  24 16 11 2 2 0 0 27891.23 31002.10
2  31 12  9 2 1 2 1 18234.50 20011.75
3  29 18 13 2 2 0 0 35120.00 37890.25
4  22 12  9 1 3 1 1 15003.13 16544.00
5  38 16 11 2 2 3 0 29876.75 30015.50
6  27 14 10 2 1 0 0 21450.00 22876.10
7  33 19 15 2 2 1 0 41200.00 44001.90
8  19 11  8 1 3 0 0 12005.50 13100.25
9  41 12  9 2 1 4 0 19870.00 20500.00
10 26 16 11 2 2 0 0 28100.75 29500.50
11 30 18 13 1 3 2 0 33450.00 35120.10
12 35 12  9 2 1 3 0 17650.25 18900.00
13 23 14 10 2 2 0 0 22340.00 23980.50
14 44 16 11 2 2 2 0 30980.50 32100.00
15 28 12  9 1 3 1 0 14500.00 15750.25
16 21 13 10 2 1 0 0 13780.75 14900.00
17 37 19 15 2 2 1 0 42010.00 45000.50
18 25 12  9 2 1 0 0 18990.25 20100.00
19 32 16 11 1 3 2 0 31200.00 33450.75
20 40 12  9 2 2 5 0 20100.50 21300.00
21 20 11  8 2 1 0 0 11500.00 12600.25
22 34 18 13 2 2 1 0 36700.75 38900.00
23 29 14 10 1 3 0 0 23100.00 24500.50
24 42 16 11 2 2 3 0 29500.25 31000.00
25 27 12  9 2 1 1 0 19200.00 20800.75
;
RUN;

* Childless indicator: PARITY = 0 means no live births (RECODE definition above);
DATA femresp;
   SET femresp;
   CHILDLESS = (PARITY = 0);
   LABEL CHILDLESS = "R has had no live births (PARITY = 0)";
RUN;

* Confirm the DATA step read the sample as expected;
PROC PRINT DATA=femresp (OBS=8);
   VAR CASEID AGE_R EDUCAT HIEDUC HISPANIC RACE PARITY CHILDLESS;
   TITLE "NSFG 2006-2010 female respondents -- first 8 records";
RUN;

* Childlessness by highest education (unweighted counts + row percents);
PROC FREQ DATA=femresp;
   TABLES HIEDUC*CHILDLESS / NOCOL NOPERCENT;
   TITLE "Childlessness by highest completed education (HIEDUC)";
RUN;

* Childlessness by race, mirroring the manuscript's race/ethnicity breakdown;
PROC FREQ DATA=femresp;
   TABLES RACE*CHILDLESS / NOCOL NOPERCENT;
   TITLE "Childlessness by race (RACE recode)";
RUN;

* Age-at-interview distribution among childless vs. mothers;
PROC MEANS DATA=femresp N MEAN MIN MAX MAXDEC=1;
   CLASS CHILDLESS;
   VAR AGE_R PARITY;
   TITLE "Age at interview and parity by childless status";
RUN;
