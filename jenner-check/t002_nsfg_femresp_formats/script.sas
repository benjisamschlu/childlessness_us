/*--------------------------------------------------------------------------
 |  NSFG 2006-2010 Female Respondent — user-defined formats bundle
 |
 |  Adapted from inputs/data/2006_2010_FemRespSetup.sas in this repository.
 |  The PROC FORMAT value definitions (PARITY, HIEDUC1F, HISPANIC, RACE) and
 |  the LABEL text below are taken verbatim from that setup file. The setup
 |  ships those value formats commented out; this bundle un-comments the ones
 |  the manuscript's breakdowns depend on and applies them, so the recode
 |  codes read as their descriptive categories. Data is the same small inline
 |  sample of manuscript-relevant variables used in the other bundle.
 *-------------------------------------------------------------------------*/

* SAS PROC FORMAT -- value definitions taken verbatim from the setup file;
PROC FORMAT;
   value PARITY
      0 = '0 BABIES'
      1 = '1 BABY'
      2 = '2 BABIES'
      3 = '3 BABIES'
      4 = '4 BABIES'
      5-95 = '5 OR MORE BABIES' ;
   value HIEDUC1F
      5 = '9TH GRADE OR LESS'
      6 = '10TH GRADE'
      7 = '11TH GRADE'
      8 = '12TH GRADE, NO DIPLOMA (NOR GED)'
      9 = 'HIGH SCHOOL GRADUATE (DIPLOMA OR GED)'
      10 = 'SOME COLLEGE BUT NO DEGREE'
      11 = 'ASSOCIATE DEGREE IN COLLEGE/UNIVERSITY'
      12 = "BACHELOR'S DEGREE"
      13 = "MASTER'S DEGREE"
      14 = 'DOCTORATE DEGREE'
      15 = 'PROFESSIONAL DEGREE' ;
   value HISPANIC
      1 = 'HISPANIC'
      2 = 'NON-HISPANIC' ;
   value RACE
      1 = 'BLACK'
      2 = 'WHITE'
      3 = 'OTHER' ;
RUN;

DATA femresp;
   INPUT CASEID AGE_R EDUCAT HIEDUC HISPANIC RACE PARITY BIRTHS5
         FINALWGT30 WGTQ5Q16;

   * LABEL text taken verbatim from 2006_2010_FemRespSetup.sas;
   LABEL
      AGE_R = "R's age at interview"
      HIEDUC = "Highest completed year of school or highest degree received (RECODE)"
      HISPANIC = "Hispanic origin of respondent (RECODE)"
      RACE = "Race of respondent (RECODE)"
      PARITY = "CAPI-based total number of live births (accounting for mult birth) (RECODE)"
   ;

   * FORMAT associations, taken verbatim from the setup file's FORMAT block;
   FORMAT HIEDUC HIEDUC1F.
          HISPANIC HISPANIC.
          RACE RACE.
          PARITY PARITY. ;
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

* Parity distribution using the verbatim PARITY format ('0 BABIES' = childless);
PROC FREQ DATA=femresp;
   TABLES PARITY / NOCUM;
   TITLE "Number of live births (PARITY), formatted";
RUN;

* Childlessness ('0 BABIES') by highest education, both formatted;
PROC FREQ DATA=femresp;
   TABLES HIEDUC*PARITY / NOCOL NOPERCENT;
   TITLE "Parity by highest completed education, formatted categories";
RUN;

* Parity by race, formatted -- mirrors the manuscript's race/ethnicity breakdown;
PROC FREQ DATA=femresp;
   TABLES RACE*HISPANIC / NOCOL NOPERCENT;
   TITLE "Race by Hispanic origin, formatted categories";
RUN;
