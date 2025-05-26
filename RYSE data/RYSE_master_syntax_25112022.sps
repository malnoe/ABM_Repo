* Encoding: UTF-8.

Syntax for RYSE master dataset

*************************************************************************************************************************************************************************
************************************************************ T1 ****************************************************************************************** T1 **********
*************************************************************************************************************************************************************************

** socio-demographics
T1_school_2: SA ID (eMba/Secunda): 396, value = 1054; SA ID (eMba/Secunda): 212, value = 84 -> recoded into system missing
T1_school_SA_A2: SA ID (eMba/Secunda): 282, value = 75 -> outlier or incorrect?
T1_school_SA_A7: SA ID (eMba/Secunda): 502, value = 17; SA ID (eMba/Secunda): 68, value = 19; SA ID (eMba/Secunda): 94, value = 22; SA ID (eMba/Secunda): 610, value = 2017 -> recoded into system missing

**household residents
Explanation: Some make no sense when looking at the values of different items, for example
sometimes the total number of household residents is smaller than the number of household residents that are younger than 17. However, so far I left most of them as they are and did not delete them
T1_household_residents_1: CA ID: 328, value = 0 (-> recoded into system missing); CA ID: 778, value = 0 -> recoded into system missing); SA ID (eMba/Secunda): 9, value = 67 (-> recoded into system missing) 
T1_household_residents_2: SA ID (eMba/Secunda): 304, value = 15 (number of household residents is lower!); SA ID (eMba/Secunda): 24, value = 17 (number of household residents is lower!);
                                         SA ID (eMba/Secunda): 79, value = 17 (number of household residents is lower!); SA ID (eMba/Secunda): 225, value = 17 (number of household residents is lower!);
                                         SA ID (eMba/Secunda): 367, value = 17 (number of household residents is lower!); SA ID (eMba/Secunda): 402, value = 17 (number of household residents is lower!);
                                         SA ID (eMba/Secunda): 419, value = 17 (number of household residents is lower!); SA ID (eMba/Secunda): 1348, value = 17 (number of household residents is lower!);
                                         SA ID (Zamdela): VO-086, value = 173 (-> recoded into system missing);
                                         SA ID (eMba/Secunda): 278, value = 7 (number of household residents is lower!);
                                         SA ID (eMba/Secunda): 418, value = 8(number of household residents is lower!)
                                         SA ID (eMba/Secunda): 421, value = 5(number of household residents is lower!)
T1_household_residents_3: CA ID: 63, value = 6 (number of household residents is way lower! -> recoded into system missing);


*************************************************************************************
*********************************RECODING**************************************

**CPTS - Child Posttraumtic Stress**
* T1_CPTS_9.
RECODE T1_CPTS_9 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_CPTS_9R.
EXECUTE.

* T1_CPTS_11.
RECODE T1_CPTS_11 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_CPTS_11R.
EXECUTE.

**SF15 - Short Form Health Survey 15**
* T1_SF15_1.
RECODE T1_SF15_1 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_SF15_1R.
EXECUTE.

* T1_SF15_8.
RECODE T1_SF15_8 (1=6) (2=5) (3=4) (4=3) (5=2) (6=1) (ELSE=SYSMIS) INTO T1_SF15_8R.
EXECUTE.

* T1_SF15_13.
RECODE T1_SF15_13 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_SF15_13R.
EXECUTE.

* T1_SF15_14.
RECODE T1_SF15_14 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_SF15_14R.
EXECUTE.

**FAS - Family Adversity Scale**.
RECODE T1_FAS_1 T1_FAS_2 T1_FAS_3 T1_FAS_4 T1_FAS_5 T1_FAS_6 T1_FAS_7 T1_FAS_8 T1_FAS_9 
    T1_FAS_SA_A1 (2=0) (ELSE=Copy) INTO T1_FAS_1R T1_FAS_2R T1_FAS_3R T1_FAS_4R T1_FAS_5R T1_FAS_6R 
    T1_FAS_7R T1_FAS_8R T1_FAS_9R T1_FAS_SA_A1R.
EXECUTE.


**PoNS - Perception of Neighborhood**
*T1_PoNS_3.
RECODE T1_PoNS_3 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1_PoNS_3R.
EXECUTE.

*T1_PoNS_6.
RECODE T1_PoNS_6 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1_PoNS_6R.
EXECUTE.

*T1_PoNS_8.
RECODE T1_PoNS_8 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1_PoNS_8R.
EXECUTE.

*T1_PoNS_CA_A1.
RECODE T1_PoNS_CA_A1 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1_PoNS_CA_A1R.
EXECUTE.

*T1_PoNS_CA_A2.
RECODE T1_PoNS_CA_A2 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1_PoNS_CA_A2R.
EXECUTE.


**BCE - Benevolent Childhood Experiences**.
RECODE T1_BCE_1 T1_BCE_2 T1_BCE_3 T1_BCE_4 T1_BCE_5 T1_BCE_6 T1_BCE_7 T1_BCE_8 T1_BCE_9 T1_BCE_10 
    (2=0) (ELSE=Copy) INTO T1_BCE_1R T1_BCE_2R T1_BCE_3R T1_BCE_4R T1_BCE_5R T1_BCE_6R T1_BCE_7R 
    T1_BCE_8R T1_BCE_9R T1_BCE_10R.
EXECUTE.


**JS - Job Satisfaction**

*T1_SA_JS_2.
RECODE T1_SA_JS_1 T1_SA_JS_3  (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) (ELSE=SYSMIS) INTO T1_SA_JS_1R T1_SA_JS_3R.
EXECUTE.


**SES - School Engagement**
*T1_SES_5.
RECODE T1_SES_5 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_SES_5R.
EXECUTE.

*T1_SES_14.
RECODE T1_SES_14 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_SES_14R.
EXECUTE.

*T1_SES_15.
RECODE T1_SES_15 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_SES_15R.
EXECUTE.

*T1_SES_16.
RECODE T1_SES_16 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1_SES_16R.
EXECUTE.

**PSS - Perceived Stress Scale**



***************************** data cleaning *****************************

****** identifying duplicates ******
Go 'Data' > 'Identify Duplicate Cases' > put the variables that you want to use to identify duplicates into 'Define matching cases by'. Do not only check what you're interested in but also screen other items and look of they are duplicates, too
Note: This will change the order of participants in your dataset since SPSS puts the participants that could be potential duplicates based on the chosen variables at the top of the data set.


****** outliers / extreme values ******
All data has been checked for impossible and extreme values by comparing the values in the dataset with what was written in the participant's survey when the dataset was built (4 times by independent researchers),
If an extreme value is still present, then it was chosen by the participant and was therefore kept. It is up to the analyst to decide what will happen with that value.


****** Response bias ******
A variance of 0 indicates that this person has used the same value for each item of a scale. However, have a look at each respective scale if this could be realistic. E.g., scales with 20+ items should have some variance, no matter what's the topic,
However, exceptions could still be possible...  If many participants have a variance of 0 for the same scale (e.g., BCE) that would indicate that it is realistic. If a person has a variance of 0 for two or more scales (like CPTS and BDI), that should 
make suspicious. For this, calculate a new variable that just sums up the variance of the selected scales of interest. Use a frequency table to check how many have variance of 0.

**** Child Post-Traumatic Stress-Reaction Index (CPTS).
COMPUTE T1_CPTS_V = VARIANCE(T1_CPTS_1,T1_CPTS_2,T1_CPTS_3,T1_CPTS_4,T1_CPTS_5,T1_CPTS_6,T1_CPTS_7,T1_CPTS_8,T1_CPTS_9R,T1_CPTS_10,
                                          T1_CPTS_11R,T1_CPTS_12,T1_CPTS_13,T1_CPTS_14,T1_CPTS_15,T1_CPTS_16,T1_CPTS_17,T1_CPTS_18,T1_CPTS_19,T1_CPTS_20).
EXECUTE.


**** Beck Depression Inventory (BDI_II).
COMPUTE T1_BDI_II_V=VARIANCE(T1_BDI_1,T1_BDI_2,T1_BDI_3,T1_BDI_4,T1_BDI_5,T1_BDI_6,T1_BDI_7,T1_BDI_8,T1_BDI_9,
                                        T1_BDI_10,T1_BDI_11,T1_BDI_12,T1_BDI_13,T1_BDI_14,T1_BDI_15,T1_BDI_16,T1_BDI_17,T1_BDI_18,T1_BDI_19,
                                        T1_BDI_20,T1_BDI_21).
EXECUTE.


**** Short Form Health Survey (SF-15).
COMPUTE T1_SF_15_physical_V = VARIANCE(T1_SF15_2,T1_SF15_3,T1_SF15_4,T1_SF15_5,T1_SF15_6,T1_SF15_7).
EXECUTE.

COMPUTE T1_SF_15_role_V = VARIANCE(T1_SF15_9,T1_SF15_10).
EXECUTE.

COMPUTE T1_SF_15_perceptions_V = VARIANCE(T1_SF15_1R,T1_SF15_12,T1_SF15_13R,T1_SF15_14R,T1_SF15_15).
EXECUTE.


**** School Engagement Scale: SA and CA have the same 32 items and SA has 1 extra item (SES).
COMPUTE T1_SES_total_V = VARIANCE(T1_SES_1,T1_SES_2,T1_SES_3,T1_SES_4,T1_SES_5R,T1_SES_6,T1_SES_7,
    T1_SES_8,T1_SES_9,T1_SES_10,T1_SES_11,T1_SES_12,T1_SES_13,T1_SES_14R,T1_SES_15R,T1_SES_16R,
    T1_SES_17,T1_SES_18,T1_SES_19,T1_SES_20,T1_SES_21,T1_SES_22,T1_SES_23,T1_SES_24,T1_SES_25,T1_SES_26,
    T1_SES_27,T1_SES_28,T1_SES_29,T1_SES_30,T1_SES_31,T1_SES_32).
EXECUTE.

COMPUTE T1_SES_total_SA_V = VARIANCE(T1_SES_1,T1_SES_2,T1_SES_3,T1_SES_4,T1_SES_5R,T1_SES_6,T1_SES_7,
    T1_SES_8,T1_SES_9,T1_SES_10,T1_SES_11,T1_SES_12,T1_SES_13,T1_SES_14R,T1_SES_15R,T1_SES_16R,
    T1_SES_17,T1_SES_18,T1_SES_19,T1_SES_20,T1_SES_21,T1_SES_22,T1_SES_23,T1_SES_24,T1_SES_25,T1_SES_26,
    T1_SES_27,T1_SES_28,T1_SES_29,T1_SES_30,T1_SES_31,T1_SES_32,T1_SES_SA_A1).
EXECUTE.


**** Work Engagement Scale (WES).
COMPUTE T1_WES_total_V=VARIANCE(T1_WES_1,T1_WES_2,T1_WES_3,T1_WES_4,T1_WES_5,T1_WES_6,T1_WES_7,T1_WES_8,T1_WES_9).
EXECUTE.

**** Delinquency scale (DS).
COMPUTE T1_Delinquency_V = VARIANCE (T1_DS_1,T1_DS_2,T1_DS_3,T1_DS_4,T1_DS_5,T1_DS_6).
EXECUTE.


**** Victimisation by Community (subscale of the Exposure to Violence scale): SA & CA have the same 4 items and SA has 3 extra items. (VbC).
COMPUTE T1_VbC_V = VARIANCE (T1_VbC_1,T1_VbC_2,T1_VbC_3,T1_VbC_4).
EXECUTE.

COMPUTE T1_VbC_SA_V = VARIANCE (T1_VbC_1,T1_VbC_2,T1_VbC_3,T1_VbC_4,T1_VbC_SA_A1,T1_VbC_SA_A2,T1_VbC_SA_A3).
EXECUTE.


**** Family Adversity scale: SA & CA have the same 9 items and SA has 1 extra item (FAS).
COMPUTE T1_FAS_V = VARIANCE (T1_FAS_1R,T1_FAS_2R,T1_FAS_3R,T1_FAS_4R,T1_FAS_5R,T1_FAS_6R,T1_FAS_7R,T1_FAS_8R,T1_FAS_9R).
EXECUTE.

COMPUTE T1_FAS_SA_V = VARIANCE (T1_FAS_1R,T1_FAS_2R,T1_FAS_3R,T1_FAS_4R,T1_FAS_5R,T1_FAS_6R,T1_FAS_7R,T1_FAS_8R,T1_FAS_9R,T1_FAS_SA_A1R).
EXECUTE.

**** Child and Youth Resilience Measure: SA and CA have the same 28 items, CA has 1 extra item (CYRM).
COMPUTE T1_CYRM28_total_V=VARIANCE(T1_CYRM_1,T1_CYRM_2,T1_CYRM_3,T1_CYRM_4,T1_CYRM_5,T1_CYRM_6,
    T1_CYRM_7,T1_CYRM_8,T1_CYRM_9,T1_CYRM_10,T1_CYRM_11,T1_CYRM_12,T1_CYRM_13,T1_CYRM_14,T1_CYRM_15,
    T1_CYRM_16,T1_CYRM_17,T1_CYRM_18,T1_CYRM_19,T1_CYRM_20,T1_CYRM_21,T1_CYRM_22,T1_CYRM_23,T1_CYRM_24,
    T1_CYRM_25,T1_CYRM_26,T1_CYRM_27,T1_CYRM_28).
EXECUTE.

COMPUTE T1_CYRM28_total_CA_V=VARIANCE(T1_CYRM_1,T1_CYRM_2,T1_CYRM_3,T1_CYRM_4,T1_CYRM_5,T1_CYRM_6,
    T1_CYRM_7,T1_CYRM_8,T1_CYRM_9,T1_CYRM_10,T1_CYRM_11,T1_CYRM_12,T1_CYRM_13,T1_CYRM_14,T1_CYRM_15,
    T1_CYRM_16,T1_CYRM_17,T1_CYRM_18,T1_CYRM_19,T1_CYRM_20,T1_CYRM_21,T1_CYRM_22,T1_CYRM_23,T1_CYRM_24,
    T1_CYRM_25,T1_CYRM_26,T1_CYRM_27,T1_CYRM_28,T1_CYRM_CA_A1).
EXECUTE.


**** Perception of Neighbourhood scale (PoNS).
COMPUTE T1_PoNS_V = VARIANCE (T1_PoNS_1,T1_PoNS_2,T1_PoNS_3R,T1_PoNS_4,T1_PoNS_5,T1_PoNS_6R,T1_PoNS_7,T1_PoNS_8R).
EXECUTE.

COMPUTE T1_PoNS_CA_V = VARIANCE (T1_PoNS_1,T1_PoNS_2,T1_PoNS_3R,T1_PoNS_4,T1_PoNS_5,T1_PoNS_6R,T1_PoNS_7,T1_PoNS_8R,T1_PoNS_CA_A1R,T1_PoNS_CA_A2R).
EXECUTE.

COMPUTE T1_PoNS_SA_V = VARIANCE (T1_PoNS_1,T1_PoNS_2,T1_PoNS_3R,T1_PoNS_4,T1_PoNS_5,T1_PoNS_6R,T1_PoNS_7,T1_PoNS_8R,T1_PoNS_SA_A1,T1_PoNS_SA_A2).
EXECUTE.


**** Benevolent Childhood Experiences scale (BCE).
COMPUTE T1_BCE_V = VARIANCE (T1_BCE_1R,T1_BCE_2R,T1_BCE_3R,T1_BCE_4R,T1_BCE_5R,T1_BCE_6R,T1_BCE_7R,T1_BCE_8R,T1_BCE_9R,T1_BCE_10R).
EXECUTE.


**** Sensitivity scale (very short version) (SS).
COMPUTE T1_Sensitivity_V = VARIANCE (T1_SS_1,T1_SS_2,T1_SS_3,T1_SS_4,T1_SS_5,T1_SS_6).
EXECUTE.


**** Peer Support scale (PeerSupp).
COMPUTE T1_PeerSupp_V = VARIANCE (T1_PeerSupp_1,T1_PeerSupp_2,T1_PeerSupp_3,T1_PeerSupp_4).
EXECUTE.


**** Substance use/risky behaviours scales. Only CA. (RB, SUS).
COMPUTE T1_CA_SUS_V = VARIANCE (T1_CA_SUS_1,T1_CA_SUS_2,T1_CA_SUS_3,T1_CA_SUS_4,T1_CA_SUS_5,T1_CA_SUS_6,T1_CA_SUS_7).
EXECUTE.

COMPUTE T1_CA_RBS_V = VARIANCE(T1_CA_SUS_1,T1_CA_SUS_2,T1_CA_SUS_3,T1_CA_SUS_4,T1_CA_SUS_5,T1_CA_SUS_6,T1_CA_SUS_7,T1_CA_RB_1).
EXECUTE.


**** Parenting Scale: Parental-caregiver supervision subscale (PCSuper) / Parental-caregiver warmth subscale (PCWarm). Only SA.
COMPUTE T1_SA_PCSuper_V = VARIANCE (T1_SA_PCSuper_1,T1_SA_PCSuper_2,T1_SA_PCSuper_3,T1_SA_PCSuper_4).
EXECUTE.

COMPUTE T1_SA_PCWarm_V = VARIANCE (T1_SA_PCWarm_1,T1_SA_PCWarm_2,T1_SA_PCWarm_3).
EXECUTE.


**** Job satisfaction. Only SA. (JS).
COMPUTE T1_SA_JS_V = VARIANCE (T1_SA_JS_1,T1_SA_JS_2R,T1_SA_JS_3).
EXECUTE.




*************************************************************************************
*********************************SCALE TOTALS*********************************

*All scale totals are sum scores. If you want mean scores just replace SUM with MEAN and change the variable name to not overwrite the sum score.


***** Child Post-Traumatic Stress-Reaction Index (CPTS-RI).
COMPUTE T1_CPTS = SUM.20(T1_CPTS_1,T1_CPTS_2,T1_CPTS_3,T1_CPTS_4,T1_CPTS_5,T1_CPTS_6,T1_CPTS_7,T1_CPTS_8,T1_CPTS_9R,T1_CPTS_10,
                                          T1_CPTS_11R,T1_CPTS_12,T1_CPTS_13,T1_CPTS_14,T1_CPTS_15,T1_CPTS_16,T1_CPTS_17,T1_CPTS_18,T1_CPTS_19,T1_CPTS_20).
VARIABLE LABELS T1_CPTS 'T1 Child Post-Traumatic Stress - Reaction Index (CPTS-RI) scale (sum)'.
EXECUTE.


***** Beck Depression Inventory (BDI_II).
COMPUTE T1_BDI_II=SUM.21(T1_BDI_1,T1_BDI_2,T1_BDI_3,T1_BDI_4,T1_BDI_5,T1_BDI_6,T1_BDI_7,T1_BDI_8,T1_BDI_9,
                                        T1_BDI_10,T1_BDI_11,T1_BDI_12,T1_BDI_13,T1_BDI_14,T1_BDI_15,T1_BDI_16,T1_BDI_17,T1_BDI_18,T1_BDI_19,
                                        T1_BDI_20,T1_BDI_21).
VARIABLE LABELS T1_BDI_II 'T1 Beck Depression Inventory-II (sum)'.
EXECUTE.


***** Short Form Health Survey (SF-15).

RECODE T1_SF15_2 T1_SF15_3 T1_SF15_4 T1_SF15_5 T1_SF15_6 T1_SF15_7 T1_SF15_9 T1_SF15_10 (1=0) 
    (2=50) (3=100) INTO T1_SF15_2_100 T1_SF15_3_100 T1_SF15_4_100 T1_SF15_5_100 T1_SF15_6_100 
    T1_SF15_7_100 T1_SF15_9_100 T1_SF15_10_100.
EXECUTE.

RECODE T1_SF15_1R T1_SF15_12 T1_SF15_13R T1_SF15_14R T1_SF15_15 (1=0) (2=25) (3=50) (4=75) (5=100) 
    INTO T1_SF15_1R_100 T1_SF15_12_100 T1_SF15_13R_100 T1_SF15_14R_100 T1_SF15_15_100.
EXECUTE.

RECODE T1_SF15_8R (1=0) (2=20) (3=40) (4=60) (6=100) (5=80) INTO T1_SF15_8R_100.
EXECUTE.


COMPUTE  T1_SF_14_PHC=(T1_SF15_2_100 + T1_SF15_3_100 + T1_SF15_4_100 + T1_SF15_5_100 + T1_SF15_6_100 + 
    T1_SF15_7_100 + T1_SF15_9_100 + T1_SF15_10_100 + T1_SF15_1R_100 + T1_SF15_12_100 + T1_SF15_13R_100 
    + T1_SF15_14R_100 + T1_SF15_15_100 + T1_SF15_8R_100)/14.
EXECUTE.




**** Physical scale SF-15 for T1 recoding

COMPUTE T1_SF_15_physical = SUM.6 (T1_SF15_2,T1_SF15_3,T1_SF15_4,T1_SF15_5,T1_SF15_6,T1_SF15_7).
VARIABLE LABELS T1_SF_15_physical 'T1 SF-15 physical functioning subscale (sum)'.
EXECUTE.

COMPUTE T1_SF_15_role = SUM.2 (T1_SF15_9,T1_SF15_10).
VARIABLE LABELS T1_SF_15_role 'T1 SF-15 role functioning subscale (sum)'.
EXECUTE.

COMPUTE T1_SF_15_social = SUM.1 (T1_SF15_11).
VARIABLE LABELS T1_SF_15_social 'T1 SF-15 social functioning subscale (sum)'.
EXECUTE.

COMPUTE T1_SF_15_perceptions = SUM.5 (T1_SF15_1R,T1_SF15_12,T1_SF15_13R,T1_SF15_14R,T1_SF15_15).
VARIABLE LABELS T1_SF_15_perceptions 'T1 SF-15 current health perceptions subscale (sum)'.
EXECUTE.

COMPUTE T1_SF_15_pain = SUM.1 (T1_SF15_8R).
VARIABLE LABELS T1_SF_15_pain 'T1 SF-15 pain subscale (sum)'.
EXECUTE.


***** School Engagement Scale: SA and CA have the same 32 items and SA has 1 extra item.
COMPUTE T1_SES_total = SUM.32(T1_SES_1,T1_SES_2,T1_SES_3,T1_SES_4,T1_SES_5R,T1_SES_6,T1_SES_7,
    T1_SES_8,T1_SES_9,T1_SES_10,T1_SES_11,T1_SES_12,T1_SES_13,T1_SES_14R,T1_SES_15R,T1_SES_16R,
    T1_SES_17,T1_SES_18,T1_SES_19,T1_SES_20,T1_SES_21,T1_SES_22,T1_SES_23,T1_SES_24,T1_SES_25,T1_SES_26,
    T1_SES_27,T1_SES_28,T1_SES_29,T1_SES_30,T1_SES_31,T1_SES_32).
VARIABLE LABELS T1_SES_total 'T1 School engagement overall scale (sum) (32 items)'.
EXECUTE.

COMPUTE T1_SES_total_T2 = SUM.31(T1_SES_1,T1_SES_2,T1_SES_3,T1_SES_4,T1_SES_5R,T1_SES_6,T1_SES_7,
    T1_SES_8,T1_SES_9,T1_SES_10,T1_SES_11,T1_SES_12,T1_SES_13,T1_SES_14R,T1_SES_15R,T1_SES_16R,
    T1_SES_17,T1_SES_18,T1_SES_19,T1_SES_20,T1_SES_21,T1_SES_22,T1_SES_23,T1_SES_24,T1_SES_25,T1_SES_26,
    T1_SES_28,T1_SES_29,T1_SES_30,T1_SES_31,T1_SES_32).
VARIABLE LABELS T1_SES_total_T2 'T1 School engagement overall scale (sum), for country comparisons over time (31 items (SA did not assess T1_SES_27 at T2))'.
EXECUTE.

COMPUTE T1_SES_total_SA = SUM.33(T1_SES_1,T1_SES_2,T1_SES_3,T1_SES_4,T1_SES_5R,T1_SES_6,T1_SES_7,
    T1_SES_8,T1_SES_9,T1_SES_10,T1_SES_11,T1_SES_12,T1_SES_13,T1_SES_14R,T1_SES_15R,T1_SES_16R,
    T1_SES_17,T1_SES_18,T1_SES_19,T1_SES_20,T1_SES_21,T1_SES_22,T1_SES_23,T1_SES_24,T1_SES_25,T1_SES_26,
    T1_SES_27,T1_SES_28,T1_SES_29,T1_SES_30,T1_SES_31,T1_SES_32,T1_SES_SA_A1).
VARIABLE LABELS T1_SES_total_SA 'T1 School engagement overall scale (sum), SA specific (33 items)'.
EXECUTE.

COMPUTE T1_SES_total_SA_T2 = SUM.32(T1_SES_1,T1_SES_2,T1_SES_3,T1_SES_4,T1_SES_5R,T1_SES_6,T1_SES_7,
    T1_SES_8,T1_SES_9,T1_SES_10,T1_SES_11,T1_SES_12,T1_SES_13,T1_SES_14R,T1_SES_15R,T1_SES_16R,
    T1_SES_17,T1_SES_18,T1_SES_19,T1_SES_20,T1_SES_21,T1_SES_22,T1_SES_23,T1_SES_24,T1_SES_25,T1_SES_26,
    T1_SES_28,T1_SES_29,T1_SES_30,T1_SES_31,T1_SES_32,T1_SES_SA_A1).
VARIABLE LABELS T1_SES_total_SA_T2 'T1 School engagement overall scale (sum), to compare to SA at T2 (32 items (SA did not assess T1_SES_27 at T2))'.
EXECUTE.

COMPUTE  T1_SES_affective = SUM.9 (T1_SES_1,T1_SES_2,T1_SES_3,T1_SES_4,T1_SES_5R,T1_SES_6,T1_SES_7,T1_SES_8,T1_SES_9).
VARIABLE LABELS  T1_SES_affective 'T1 School engagement affective engagement subscale (sum)'.
EXECUTE.

COMPUTE T1_SES_behavioural = SUM.11 (T1_SES_10,T1_SES_11,T1_SES_12,T1_SES_13,T1_SES_14R,T1_SES_15R,T1_SES_16R,T1_SES_17,T1_SES_18,T1_SES_19,T1_SES_20).
VARIABLE LABELS T1_SES_behavioural 'T1 School engagement behavioural engagement subscale (sum), (11 items)'.
EXECUTE.

COMPUTE T1_SES_behavioural_SA = SUM.12 (T1_SES_10,T1_SES_11,T1_SES_12,T1_SES_13,T1_SES_14R,T1_SES_15R,T1_SES_16R,T1_SES_17,T1_SES_18,T1_SES_19,T1_SES_20, T1_SES_SA_A1).
VARIABLE LABELS T1_SES_behavioural_SA 'T1 School engagement behavioural engagement subscale (sum), SA specific (12 items)'.
EXECUTE.

COMPUTE T1_SES_cognitive = SUM.12 (T1_SES_21,T1_SES_22,T1_SES_23,T1_SES_24,T1_SES_25,T1_SES_26,T1_SES_27,T1_SES_28,T1_SES_29,T1_SES_30,T1_SES_31,T1_SES_32).
VARIABLE LABELS T1_SES_cognitive 'T1 School engagement cognitive engagement subscale (sum), (12 items)'.
EXECUTE.

COMPUTE T1_SES_cognitive_T2 = SUM.11 (T1_SES_21,T1_SES_22,T1_SES_23,T1_SES_24,T1_SES_25,T1_SES_26,T1_SES_28,T1_SES_29,T1_SES_30,T1_SES_31,T1_SES_32).
VARIABLE LABELS T1_SES_cognitive_T2 'T1 School engagement cognitive engagement subscale (sum), for country comparisons over time (11 items (SA did not assess T1_SES_27 at T2))'.
EXECUTE.

***** Work Engagement Scale (UWES-9).
COMPUTE T1_WES_total=SUM.9(T1_WES_1,T1_WES_2,T1_WES_3,T1_WES_4,T1_WES_5,T1_WES_6,T1_WES_7,T1_WES_8,T1_WES_9).
VARIABLE LABELS T1_WES_total 'T1 Work engagement scale overall (sum)'.
EXECUTE.

COMPUTE T1_WES_vigour = SUM.3 (T1_WES_1,T1_WES_2,T1_WES_5).
VARIABLE LABELS T1_WES_vigour 'T1 Work engagement vigour subscale (sum)'.
EXECUTE.

COMPUTE T1_WES_dedication = SUM.3 (T1_WES_3,T1_WES_4,T1_WES_7).
VARIABLE LABELS T1_WES_dedication 'T1 Work engagement dedication subscale (sum)'.
EXECUTE.

COMPUTE T1_WES_absorption = SUM.3 (T1_WES_6,T1_WES_8,T1_WES_9).
VARIABLE LABELS T1_WES_absorption 'T1 Work engagement absorption subscale (sum)'.
EXECUTE.


***** Job satisfaction. Only SA.
COMPUTE T1_SA_JS = SUM.3 (T1_SA_JS_1R,T1_SA_JS_2,T1_SA_JS_3R).
VARIABLE LABELS T1_SA_JS 'T1 Job satisfaction (sum), only SA'.
EXECUTE.


***** Delinquency scale.
COMPUTE T1_Delinquency = SUM.6 (T1_DS_1,T1_DS_2,T1_DS_3,T1_DS_4,T1_DS_5,T1_DS_6).
VARIABLE LABELS T1_Delinquency 'T1 Delinquency scale (sum)'.
EXECUTE.


***** Substance use/risky behaviours scales. Only CA.
COMPUTE T1_CA_SUS = SUM.7 (T1_CA_SUS_1,T1_CA_SUS_2,T1_CA_SUS_3,T1_CA_SUS_4,T1_CA_SUS_5,T1_CA_SUS_6,T1_CA_SUS_7).
VARIABLE LABELS T1_CA_SUS 'T1 Substance use scale (sum), only CA'.
EXECUTE.

COMPUTE T1_CA_RB = SUM.8 (T1_CA_SUS_1,T1_CA_SUS_2,T1_CA_SUS_3,T1_CA_SUS_4,T1_CA_SUS_5,T1_CA_SUS_6,T1_CA_SUS_7,T1_CA_RB_1).
VARIABLE LABELS T1_CA_RB 'T1 Risky behaviours scale (sum), only CA'.
EXECUTE.


***** Victimisation by Community (subscale of the Exposure to Violence scale): SA & CA have the same 4 items and SA has 3 extra items.
COMPUTE T1_VbC = SUM.4 (T1_VbC_1,T1_VbC_2,T1_VbC_3,T1_VbC_4).
VARIABLE LABELS T1_VbC 'T1 Victimisation by Community subscale of the Exposure to Violence scale (sum), (4 items)'.
EXECUTE.

COMPUTE T1_VbC_SA = SUM.7 (T1_VbC_1,T1_VbC_2,T1_VbC_3,T1_VbC_4,T1_VbC_SA_A1,T1_VbC_SA_A2,T1_VbC_SA_A3).
VARIABLE LABELS T1_VbC_SA 'T1 Victimisation by Community subscale of the Exposure to Violence scale (sum), SA specific (7 items)'.
EXECUTE.


***** Family Adversity scale: SA & CA have the same 9 items and SA has 1 extra item.
COMPUTE T1_FAS = SUM.9 (T1_FAS_1R,T1_FAS_2R,T1_FAS_3R,T1_FAS_4R,T1_FAS_5R,T1_FAS_6R,T1_FAS_7R,T1_FAS_8R,T1_FAS_9R).
VARIABLE LABELS T1_FAS 'T1 Family Adversity scale (sum), (9 items)'.
EXECUTE.

COMPUTE T1_FAS_SA = SUM.10 (T1_FAS_1R,T1_FAS_2R,T1_FAS_3R,T1_FAS_4R,T1_FAS_5R,T1_FAS_6R,T1_FAS_7R,T1_FAS_8R,T1_FAS_9R,T1_FAS_SA_A1R).
VARIABLE LABELS T1_FAS_SA 'T1 Family Adversity scale (sum), SA specific (10 items)'.
EXECUTE.


***** Child and Youth Resilience Measure: SA and CA have the same 28 items, CA has 1 extra item. The subscales are based on the original factor structure of the CYRM-28 which might rather fit CA. Hence, EFA or CFA should be done for SA.
COMPUTE T1_CYRM28_total=SUM.28(T1_CYRM_1,T1_CYRM_2,T1_CYRM_3,T1_CYRM_4,T1_CYRM_5,T1_CYRM_6,
    T1_CYRM_7,T1_CYRM_8,T1_CYRM_9,T1_CYRM_10,T1_CYRM_11,T1_CYRM_12,T1_CYRM_13,T1_CYRM_14,T1_CYRM_15,
    T1_CYRM_16,T1_CYRM_17,T1_CYRM_18,T1_CYRM_19,T1_CYRM_20,T1_CYRM_21,T1_CYRM_22,T1_CYRM_23,T1_CYRM_24,
    T1_CYRM_25,T1_CYRM_26,T1_CYRM_27,T1_CYRM_28).
VARIABLE LABELS T1_CYRM28_total 'T1 CYRM-28 total (sum)'.
EXECUTE.

COMPUTE T1_CYRM29_total_CA=SUM.29(T1_CYRM_1,T1_CYRM_2,T1_CYRM_3,T1_CYRM_4,T1_CYRM_5,T1_CYRM_6,
    T1_CYRM_7,T1_CYRM_8,T1_CYRM_9,T1_CYRM_10,T1_CYRM_11,T1_CYRM_12,T1_CYRM_13,T1_CYRM_14,T1_CYRM_15,
    T1_CYRM_16,T1_CYRM_17,T1_CYRM_18,T1_CYRM_19,T1_CYRM_20,T1_CYRM_21,T1_CYRM_22,T1_CYRM_23,T1_CYRM_24,
    T1_CYRM_25,T1_CYRM_26,T1_CYRM_27,T1_CYRM_28,T1_CYRM_CA_A1).
VARIABLE LABELS T1_CYRM29_total_CA 'T1 CYRM-29 total (sum), CA specific (29 items)'.
EXECUTE.

COMPUTE T1_CYRM28_I = SUM.11 (T1_CYRM_2,T1_CYRM_4,T1_CYRM_8,T1_CYRM_11,T1_CYRM_13,T1_CYRM_14,T1_CYRM_15,T1_CYRM_18,T1_CYRM_20,T1_CYRM_21,T1_CYRM_25).
VARIABLE LABELS T1_CYRM28_I 'T1 CYRM-28 individual subscale (sum)'.
EXECUTE.

COMPUTE T1_CYRM28_R = SUM.7 (T1_CYRM_5, T1_CYRM_6, T1_CYRM_7, T1_CYRM_12,T1_CYRM_17, T1_CYRM_24, T1_CYRM_26).
VARIABLE LABELS T1_CYRM28_R 'T1 CYRM-28 relational subscale (sum)'.
EXECUTE.

COMPUTE T1_CYRM28_C = SUM.10 (T1_CYRM_1,T1_CYRM_3,T1_CYRM_9,T1_CYRM_10,T1_CYRM_16,T1_CYRM_19,T1_CYRM_22,T1_CYRM_23,T1_CYRM_27,T1_CYRM_28).
VARIABLE LABELS T1_CYRM28_C 'T1 CYRM-28 contextual subscale (sum)'.
EXECUTE.

COMPUTE T1_CYRM29_C_CA = SUM.11 (T1_CYRM_1,T1_CYRM_3,T1_CYRM_9,T1_CYRM_10,T1_CYRM_16,T1_CYRM_19,T1_CYRM_22,T1_CYRM_23,T1_CYRM_27,T1_CYRM_28,T1_CYRM_CA_A1).
VARIABLE LABELS T1_CYRM29_C_CA 'T1 CYRM-29 contextual subscale (sum), CA specific (11 items)'.
EXECUTE.

COMPUTE T1_CYRM12=SUM.12(T1_CYRM_3,T1_CYRM_5,T1_CYRM_6,T1_CYRM_8,T1_CYRM_9,T1_CYRM_15,T1_CYRM_17,T1_CYRM_21,T1_CYRM_22,T1_CYRM_24,T1_CYRM_25,T1_CYRM_27).
VARIABLE LABELS T1_CYRM12 'T1 CYRM-12 (sum)'.
EXECUTE.

COMPUTE T1_CYRM28_IndPS=SUM.5 (T1_CYRM_2, T1_CYRM_8, T1_CYRM_11, T1_CYRM_13, T1_CYRM_21).
VARIABLE LABELS T1_CYRM28_IndPS 'T1_CYRM28_Individual Personal Skills'.
EXECUTE.
COMPUTE T1_CYRM28_IndPeer= SUM.2 (T1_CYRM_14, T1_CYRM_18).
VARIABLE LABELS T1_CYRM28_IndPeer 'T1_CYRM28_Individual Peer Support'.
EXECUTE.
COMPUTE T1_CYRM28_IndSS= SUM.4 (T1_CYRM_4, T1_CYRM_15, T1_CYRM_20, T1_CYRM_25).
VARIABLE LABELS T1_CYRM28_IndSS 'T1_CYRM28_Individual Social Skills'.
EXECUTE.
*The Caregiver Sub-scale of the CYRM-26 has two sub-clusters of questions*.
COMPUTE T1_CYRM28_CrPhys= SUM.2 (T1_CYRM_5, T1_CYRM_7).
VARIABLE LABELS T1_CYRM28_CrPhys 'T1_CYRM28_Caregivers Physical Care Giving'.
EXECUTE.
COMPUTE T1_CYRM28_CrPsyc= SUM.5 (T1_CYRM_6, T1_CYRM_12, T1_CYRM_17, T1_CYRM_24, T1_CYRM_26).
VARIABLE LABELS T1_CYRM28_CrPsyc 'T1_CYRM28_Caregivers Psychological Care Giving'.
EXECUTE.
*The Contextual Sub-scale of the CYRM-26 has three sub-clusters of questions*.
COMPUTE T1_CYRM28_CntS= SUM.2 (T1_CYRM_22, T1_CYRM_9).
VARIABLE LABELS T1_CYRM28_CntS 'T1_CYRM28_Context Spiritual'.
EXECUTE.
COMPUTE T1_CYRM28_CntEd= SUM.1 (T1_CYRM_3).
VARIABLE LABELS T1_CYRM28_CntEd 'T1_CYRM28_Context Education'.
EXECUTE.
COMPUTE T1_CYRM28_CntC= SUM.7 (T1_CYRM_1, T1_CYRM_10, T1_CYRM_16, T1_CYRM_19, T1_CYRM_23,T1_CYRM_26, T1_CYRM_27,T1_CYRM_28).
VARIABLE LABELS T1_CYRM28_CntC 'T1_CYRM28_Context Cultural'.
EXECUTE.

***** Perception of Neighbourhood scale: SA and CA have the same 8 items, SA has 2 extra items and CA has 2 extra items.
COMPUTE T1_PoNS = SUM.8 (T1_PoNS_1,T1_PoNS_2,T1_PoNS_3R,T1_PoNS_4,T1_PoNS_5,T1_PoNS_6R,T1_PoNS_7,T1_PoNS_8R).
VARIABLE LABELS T1_PoNS 'T1 Perception of Neighbourhood scale (sum), (8 items)'.
EXECUTE.

COMPUTE T1_PoNS_CA = SUM.10 (T1_PoNS_1,T1_PoNS_2,T1_PoNS_3R,T1_PoNS_4,T1_PoNS_5,T1_PoNS_6R,T1_PoNS_7,T1_PoNS_8R,T1_PoNS_CA_A1R,T1_PoNS_CA_A2R).
VARIABLE LABELS T1_PoNS_CA 'T1 Perception of Neighbourhood scale (sum), CA specific (10 items)'.
EXECUTE.

COMPUTE T1_PoNS_SA = SUM.10 (T1_PoNS_1,T1_PoNS_2,T1_PoNS_3R,T1_PoNS_4,T1_PoNS_5,T1_PoNS_6R,T1_PoNS_7,T1_PoNS_8R,T1_PoNS_SA_A1,T1_PoNS_SA_A2).
VARIABLE LABELS T1_PoNS_SA 'T1 Perception of Neighbourhood scale (sum), SA specific (10 items)'.
EXECUTE.


***** Benevolent Childhood Experiences scale.
COMPUTE T1_BCE = SUM.10 (T1_BCE_1R,T1_BCE_2R,T1_BCE_3R,T1_BCE_4R,T1_BCE_5R,T1_BCE_6R,T1_BCE_7R,T1_BCE_8R,T1_BCE_9R,T1_BCE_10R).
VARIABLE LABELS T1_BCE 'T1 Benevolent Childhood Experiences scale (sum)'.
EXECUTE.


***** Sensitivity scale (very short version).
COMPUTE T1_Sensitivity = SUM.6 (T1_SS_1,T1_SS_2,T1_SS_3,T1_SS_4,T1_SS_5,T1_SS_6).
VARIABLE LABELS T1_Sensitivity 'T1 Sensitivity scale (very short version) (sum)'.
EXECUTE.


***** Peer Support scale.
COMPUTE T1_PeerSupp = SUM.4 (T1_PeerSupp_1,T1_PeerSupp_2,T1_PeerSupp_3,T1_PeerSupp_4).
VARIABLE LABELS T1_PeerSupp 'T1 Peer support scale (sum)'.
EXECUTE.

***** Parenting Scale: Only SA.
COMPUTE T1_SA_PCSuper = SUM.4 (T1_SA_PCSuper_1,T1_SA_PCSuper_2,T1_SA_PCSuper_3,T1_SA_PCSuper_4).
VARIABLE LABELS T1_SA_PCSuper 'T1 Parenting Scale: Parental-caregiver supervision subscale (sum), only SA'.
EXECUTE.

COMPUTE T1_SA_PCSuper_T2 = SUM.3 (T1_SA_PCSuper_1,T1_SA_PCSuper_2,T1_SA_PCSuper_3).
VARIABLE LABELS T1_SA_PCSuper_T2 'T1 Parenting Scale: Parental-caregiver supervision subscale (sum), only SA (3 items to be comparable to T2)'.
EXECUTE.

COMPUTE T1_SA_PCWarm = SUM.3 (T1_SA_PCWarm_1,T1_SA_PCWarm_2,T1_SA_PCWarm_3).
VARIABLE LABELS T1_SA_PCWarm 'T1 Parenting Scale: Parental-caregiver warmth subscale (sum), only SA'.
EXECUTE.



***** Social media use total. Only CA.
COMPUTE T1_CA_SM = SUM (T1_CA_SM_hours_Facebook, T1_CA_SM_hours_Instagram, T1_CA_SM_hours_Snapchat, T1_CA_SM_hours_Twitter, T1_CA_SM_hours_Other_hours_1, T1_CA_SM_hours_Other_hours_2).
VARIABLE LABELS T1_CA_SM 'T1 Social media use total (sum), only CA'.
EXECUTE.


***** DHEA & Cortisol.
COMPUTE T1_DHEA_cort_comp = T1_DHEA_pg_mg/T1_Cort_pg_mg.
VARIABLE LABELS T1_DHEA_cort_comp 'T1 DHEA/Cortisol ratio'.
EXECUTE.


VARIABLE LEVEL T1_CPTS T1_BDI_II T1_SF_15_total T1_SF_15_physical T1_SF_15_role T1_SF_15_social T1_SF_15_perceptions T1_SF_15_pain T1_SES_total T1_SES_total_T2 T1_SES_total_SA T1_SES_total_SA_T2 T1_SES_affective
                            T1_SES_behavioural T1_SES_behavioural_SA T1_SES_cognitive T1_SES_cognitive_T2 T1_WES_total T1_WES_vigour T1_WES_dedication T1_WES_absorption T1_SA_JS T1_Delinquency T1_CA_SUS T1_CA_RB
                            T1_VbC T1_VbC_SA T1_FAS T1_FAS_SA T1_CYRM28_total T1_CYRM29_total_CA T1_CYRM28_I T1_CYRM28_R T1_CYRM28_C T1_CYRM29_C_CA T1_CYRM12 T1_PoNS T1_PoNS_CA T1_PoNS_SA T1_BCE T1_Sensitivity
                            T1_PeerSupp T1_SA_PCSuper T1_SA_PCSuper_T2 T1_SA_PCWarm T1_CA_SM T1_DHEA_cort_comp (SCALE).
EXECUTE.


*************************************************************************************************************************************************************************
************************************************************ T1a ****************************************************************************************** T1a *******
*************************************************************************************************************************************************************************


*************************************************************************************
*********************************RECODING**************************************

**CPTS - Child Posttraumtic Stress**
* T1a_CPTS_9.
RECODE T1a_CPTS_9 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1a_CPTS_9R.
EXECUTE.

* T1a_CPTS_11.
RECODE T1a_CPTS_11 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1a_CPTS_11R.
EXECUTE.

**SF15 - Short Form Health Survey 15**
* T1a_CA_SF15_1.
RECODE T1a_CA_SF15_1 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1a_CA_SF15_1R.
EXECUTE.

* T1a_CA_SF15_8.
RECODE T1a_CA_SF15_8 (1=6) (2=5) (3=4) (4=3) (5=2) (6=1) (ELSE=SYSMIS) INTO T1a_CA_SF15_8R.
EXECUTE.

* T1a_CA_SF15_13.
RECODE T1a_CA_SF15_13 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1a_CA_SF15_13R.
EXECUTE.

* T1a_CA_SF15_14.
RECODE T1a_CA_SF15_14 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T1a_CA_SF15_14R.
EXECUTE.

**PoNS - Perception of Neighborhood**
*T1a_SA_PoNS_3.
RECODE T1a_SA_PoNS_3 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1a_SA_PoNS_3R.
EXECUTE.

*T1a_SA_PoNS_6.
RECODE T1a_SA_PoNS_6 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1a_SA_PoNS_6R.
EXECUTE.

*T1a_SA_PoNS_8.
RECODE T1a_SA_PoNS_8 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1a_SA_PoNS_8R.
EXECUTE.

*T1a_SA_PoNS_A5.
RECODE T1a_SA_PoNS_A5 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T1a_SA_PoNS_A5R.
EXECUTE.


**BCE - Benevolent Childhood Experiences**.
RECODE T1a_SA_BCE_1 T1a_SA_BCE_2 T1a_SA_BCE_3 T1a_SA_BCE_4 T1a_SA_BCE_5 T1a_SA_BCE_6 T1a_SA_BCE_7 T1a_SA_BCE_8 T1a_SA_BCE_9 T1a_SA_BCE_10 
    (2=0) (ELSE=Copy) INTO T1a_SA_BCE_1R T1a_SA_BCE_2R T1a_SA_BCE_3R T1a_SA_BCE_4R T1a_SA_BCE_5R T1a_SA_BCE_6R T1a_SA_BCE_7R T1a_SA_BCE_8R T1a_SA_BCE_9R T1a_SA_BCE_10R.
EXECUTE.


***************************** data cleaning *****************************

****** identifying duplicates ******
Go 'Data' > 'Identify Duplicate Cases' > put the variables that you want to use to identify duplicates into 'Define matching cases by'
Note: This will change the order of participants in your dataset since SPSS puts the participants that could be potential duplicates based on the chosen variables at the top of the data set.


****** outliers / extreme values ******
All data has been checked for impossible and extreme values by comparing the value in the dataset with what was written in the participant's survey when the dataset was built,
If an extreme value is still present, then it was chosen by the participant and was therefore kept. It is up to the analyst to decide what will happen with that value.


****** Response bias ******
A variance of 0 indicates that this person has used the same value for each item of a scale. However, have a look at each respective scale if this could be realistic. E.g., scales with 20+ items should have some variance, no matter what's the topic,
However, exceptions could still be possible...  If many participants have a variance of 0 for the same scale (e.g., BCE) that would indicate that it is realistic. If a person has a variance of 0 for two or more scales (like CPTS and BDI), that should 
make suspicious. For this, calculate a new variable that just sums up the variance of the selected scales of interest. Use a frequency table to check how many have variance of 0.

**** Child Posttraumatic Stress - Reaction Index (CPTS).
COMPUTE T1a_CPTS_V = VARIANCE(T1a_CPTS_1,T1a_CPTS_2,T1a_CPTS_3,T1a_CPTS_4,T1a_CPTS_5,T1a_CPTS_6,T1a_CPTS_7,T1a_CPTS_8,T1a_CPTS_9R,T1a_CPTS_10,
                                          T1a_CPTS_11R,T1a_CPTS_12,T1a_CPTS_13,T1a_CPTS_14,T1a_CPTS_15,T1a_CPTS_16,T1a_CPTS_17,T1a_CPTS_18,T1a_CPTS_19,T1a_CPTS_20).
EXECUTE.


**** Beck Depression Inventory (BDI_II).
COMPUTE T1a_BDI_II_V=VARIANCE(T1a_BDI_1,T1a_BDI_2,T1a_BDI_3,T1a_BDI_4,T1a_BDI_5,T1a_BDI_6,T1a_BDI_7,T1a_BDI_8,T1a_BDI_9,
                                        T1a_BDI_10,T1a_BDI_11,T1a_BDI_12,T1a_BDI_13,T1a_BDI_14,T1a_BDI_15,T1a_BDI_16_cleaned,T1a_BDI_17,T1a_BDI_18,T1a_BDI_19,T1a_BDI_20,T1a_BDI_21).
EXECUTE.


**** Child and Youth Resilience Measure: SA and CA have the same 12 items (SF12), CA has the same 29 items as at T1 (CYRM).
COMPUTE T1a_CYRM12_V=VARIANCE(T1a_CYRM_3,T1a_CYRM_5,T1a_CYRM_6,T1a_CYRM_8,T1a_CYRM_9,T1a_CYRM_15,T1a_CYRM_17,T1a_CYRM_21,T1a_CYRM_22,T1a_CYRM_24,T1a_CYRM_25,T1a_CYRM_27).
EXECUTE.

COMPUTE T1a_CA_CYRM28_total_V=VARIANCE(T1a_CYRM_3,T1a_CYRM_5,T1a_CYRM_6,T1a_CYRM_8,T1a_CYRM_9,T1a_CYRM_15,T1a_CYRM_17,T1a_CYRM_21,T1a_CYRM_22,T1a_CYRM_24,T1a_CYRM_25,T1a_CYRM_27,
     T1a_CYRM_1_CA_A1,T1a_CYRM_2_CA_A2,T1a_CYRM_4_CA_A3,T1a_CYRM_7_CA_A4,T1a_CYRM_10_CA_A5,T1a_CYRM_11_CA_A6,T1a_CYRM_12_CA_A7,T1a_CYRM_13_CA_A8,T1a_CYRM_14_CA_A9,T1a_CYRM_16_CA_A10,
     T1a_CYRM_18_CA_A11,T1a_CYRM_19_CA_A12,T1a_CYRM_20_CA_A13,T1a_CYRM_23_CA_A14,T1a_CYRM_26_CA_A15,T1a_CYRM_28_CA_A16).
EXECUTE.

COMPUTE T1a_CA_CYRM28_total_T1CA_V=VARIANCE(T1a_CYRM_3,T1a_CYRM_5,T1a_CYRM_6,T1a_CYRM_8,T1a_CYRM_9,T1a_CYRM_15,T1a_CYRM_17,T1a_CYRM_21,T1a_CYRM_22,T1a_CYRM_24,T1a_CYRM_25,T1a_CYRM_27,
     T1a_CYRM_1_CA_A1,T1a_CYRM_2_CA_A2,T1a_CYRM_4_CA_A3,T1a_CYRM_7_CA_A4,T1a_CYRM_10_CA_A5,T1a_CYRM_11_CA_A6,T1a_CYRM_12_CA_A7,T1a_CYRM_13_CA_A8,T1a_CYRM_14_CA_A9,T1a_CYRM_16_CA_A10,
     T1a_CYRM_18_CA_A11,T1a_CYRM_19_CA_A12,T1a_CYRM_20_CA_A13,T1a_CYRM_23_CA_A14,T1a_CYRM_26_CA_A15,T1a_CYRM_28_CA_A16,T1a_CYRM_CA_A1).
EXECUTE.


**** Peer Support (PeerSupp).
COMPUTE T1a_PeerSupp_V = VARIANCE (T1a_PeerSupp_1,T1a_PeerSupp_2,T1a_PeerSupp_3,T1a_PeerSupp_4).
EXECUTE.


**** Short Form Health Survey 15 (SF-15).
COMPUTE T1a_CA_SF_15_physical_V = VARIANCE (T1a_CA_SF15_2,T1a_CA_SF15_3,T1a_CA_SF15_4,T1a_CA_SF15_5,T1a_CA_SF15_6,T1a_CA_SF15_7).
EXECUTE.

COMPUTE T1a_CA_SF_15_role_V = VARIANCE(T1a_CA_SF15_9,T1a_CA_SF15_10).
EXECUTE.

COMPUTE T1a_CA_SF_15_perceptions_V = VARIANCE(T1a_CA_SF15_1R,T1a_CA_SF15_12,T1a_CA_SF15_13R,T1a_CA_SF15_14R,T1a_CA_SF15_15).
EXECUTE.


**** Benevolent Childhood Experiences scale (BCE).
COMPUTE T1a_SA_BCE_V = VARIANCE (T1a_SA_BCE_1R, T1a_SA_BCE_2R, T1a_SA_BCE_3R, T1a_SA_BCE_4R, T1a_SA_BCE_5R, T1a_SA_BCE_6R, T1a_SA_BCE_7R, T1a_SA_BCE_8R, T1a_SA_BCE_9R, T1a_SA_BCE_10R).
EXECUTE.


****  Parenting Scale: Parental-caregiver supervision subscale (PCSuper) / Parental-caregiver warmth subscale (PCWarm). Only SA.
COMPUTE T1a_SA_PCSuper_V = VARIANCE (T1a_SA_PCSuper_1,T1a_SA_PCSuper_2,T1a_SA_PCSuper_3,T1a_SA_PCSuper_4).
EXECUTE.

COMPUTE T1a_SA_PCWarm_V = VARIANCE(T1a_SA_PCWarm_1,T1a_SA_PCWarm_2,T1a_SA_PCWarm_3).
EXECUTE.


**** Perception of Neighbourhood scale: Only SA (PoNS).
COMPUTE T1a_SA_PoNS_V = VARIANCE (T1a_SA_PoNS_1,T1a_SA_PoNS_2,T1a_SA_PoNS_3R,T1a_SA_PoNS_4,T1a_SA_PoNS_5,T1a_SA_PoNS_6R,T1a_SA_PoNS_7,T1a_SA_PoNS_8R).
EXECUTE.

COMPUTE T1a_SA_PoNS_T1SA_V = VARIANCE (T1a_SA_PoNS_1,T1a_SA_PoNS_2,T1a_SA_PoNS_3R,T1a_SA_PoNS_4,T1a_SA_PoNS_5,T1a_SA_PoNS_6R,T1a_SA_PoNS_7,T1a_SA_PoNS_8R,T1a_SA_PoNS_A1,T1a_SA_PoNS_A2).
EXECUTE.

COMPUTE T1a_SA_PoNS_A_V = VARIANCE (T1a_SA_PoNS_1,T1a_SA_PoNS_2,T1a_SA_PoNS_3R,T1a_SA_PoNS_4,T1a_SA_PoNS_5,T1a_SA_PoNS_6R,T1a_SA_PoNS_7,T1a_SA_PoNS_8R,T1a_SA_PoNS_A1,T1a_SA_PoNS_A2,T1a_SA_PoNS_A3,T1a_SA_PoNS_A4,T1a_SA_PoNS_A5R).
EXECUTE.



*************************************************************************************
*********************************SCALE TOTALS*********************************

*All scale totals are sum scores. If you want mean scores just replace SUM with MEAN and change the variable name to not overwrite the sum score.

***** Child Posttraumatic Stress - Reaction Index.
COMPUTE T1a_CPTS = SUM.20(T1a_CPTS_1,T1a_CPTS_2,T1a_CPTS_3,T1a_CPTS_4,T1a_CPTS_5,T1a_CPTS_6,T1a_CPTS_7,T1a_CPTS_8,T1a_CPTS_9R,T1a_CPTS_10,
                                          T1a_CPTS_11R,T1a_CPTS_12,T1a_CPTS_13,T1a_CPTS_14,T1a_CPTS_15,T1a_CPTS_16,T1a_CPTS_17,T1a_CPTS_18,T1a_CPTS_19,T1a_CPTS_20).
VARIABLE LABELS T1a_CPTS 'T1a Child Post-Traumatic Stress - Reaction Index (CPTS-RI) scale (sum)'.
EXECUTE.


***** Beck Depression Inventory (BDI_II).
COMPUTE T1a_BDI_II=SUM.21(T1a_BDI_1,T1a_BDI_2,T1a_BDI_3,T1a_BDI_4,T1a_BDI_5,T1a_BDI_6,T1a_BDI_7,T1a_BDI_8,T1a_BDI_9,
                                        T1a_BDI_10,T1a_BDI_11,T1a_BDI_12,T1a_BDI_13,T1a_BDI_14,T1a_BDI_15,T1a_BDI_16_cleaned,T1a_BDI_17,T1a_BDI_18,T1a_BDI_19,T1a_BDI_20,T1a_BDI_21).
VARIABLE LABELS T1a_BDI_II 'T1a Beck Depression Inventory-II (sum)'.
EXECUTE.


***** Short Form Health Survey (SF-15). Only CA.
COMPUTE T1a_CA_SF_15_total=SUM.15(T1a_CA_SF15_1R,T1a_CA_SF15_2,T1a_CA_SF15_3,T1a_CA_SF15_4,T1a_CA_SF15_5,T1a_CA_SF15_6,T1a_CA_SF15_7,
    T1a_CA_SF15_8R,T1a_CA_SF15_9,T1a_CA_SF15_10,T1a_CA_SF15_11,T1a_CA_SF15_12,T1a_CA_SF15_13R,T1a_CA_SF15_14R,T1a_CA_SF15_15).
VARIABLE LABELS T1a_CA_SF_15_total 'T1a SF-15 overall health (sum), only CA'.
EXECUTE.

COMPUTE T1a_CA_SF_15_physical = SUM.6 (T1a_CA_SF15_2,T1a_CA_SF15_3,T1a_CA_SF15_4,T1a_CA_SF15_5,T1a_CA_SF15_6,T1a_CA_SF15_7).
VARIABLE LABELS T1a_CA_SF_15_physical 'T1a SF-15 physical functioning subscale (sum)'.
EXECUTE.

COMPUTE T1a_CA_SF_15_role = SUM.2 (T1a_CA_SF15_9,T1a_CA_SF15_10).
VARIABLE LABELS T1a_CA_SF_15_role 'T1a SF-15 role functioning subscale (sum)'.
EXECUTE.

COMPUTE T1a_CA_SF_15_social = SUM.1 (T1a_CA_SF15_11).
VARIABLE LABELS T1a_CA_SF_15_social 'T1a SF-15 social functioning subscale (sum)'.
EXECUTE.

COMPUTE T1a_CA_SF_15_perceptions = SUM.5 (T1a_CA_SF15_1R,T1a_CA_SF15_12,T1a_CA_SF15_13R,T1a_CA_SF15_14R,T1a_CA_SF15_15).
VARIABLE LABELS T1a_CA_SF_15_perceptions 'T1a SF-15 current health perceptions subscale (sum)'.
EXECUTE.

COMPUTE T1a_CA_SF_15_pain = SUM.1 (T1a_CA_SF15_8R).
VARIABLE LABELS T1a_CA_SF_15_pain 'T1a SF-15 pain subscale (sum)'.
EXECUTE.


***** Child and Youth Resilience Measure: SA and CA have the same 12 items (SF12), CA has the same 29 items as at T1.
COMPUTE T1a_CYRM12=SUM.12(T1a_CYRM_3,T1a_CYRM_5,T1a_CYRM_6,T1a_CYRM_8,T1a_CYRM_9,T1a_CYRM_15,T1a_CYRM_17,T1a_CYRM_21,T1a_CYRM_22,T1a_CYRM_24,T1a_CYRM_25,T1a_CYRM_27).
VARIABLE LABELS T1a_CYRM12 'T1a CYRM-12 (sum)'.
EXECUTE.

COMPUTE T1a_CA_CYRM28_total=SUM.28(T1a_CYRM_3,T1a_CYRM_5,T1a_CYRM_6,T1a_CYRM_8,T1a_CYRM_9,T1a_CYRM_15,T1a_CYRM_17,T1a_CYRM_21,T1a_CYRM_22,T1a_CYRM_24,T1a_CYRM_25,T1a_CYRM_27,
     T1a_CYRM_1_CA_A1,T1a_CYRM_2_CA_A2,T1a_CYRM_4_CA_A3,T1a_CYRM_7_CA_A4,T1a_CYRM_10_CA_A5,T1a_CYRM_11_CA_A6,T1a_CYRM_12_CA_A7,T1a_CYRM_13_CA_A8,T1a_CYRM_14_CA_A9,T1a_CYRM_16_CA_A10,
     T1a_CYRM_18_CA_A11,T1a_CYRM_19_CA_A12,T1a_CYRM_20_CA_A13,T1a_CYRM_23_CA_A14,T1a_CYRM_26_CA_A15,T1a_CYRM_28_CA_A16).
VARIABLE LABELS T1a_CA_CYRM28_total 'T1a CYRM-28 total (sum), only CA (original 28 items)'.
EXECUTE.

COMPUTE T1a_CA_CYRM29_total_T1_CA=SUM.29(T1a_CYRM_3,T1a_CYRM_5,T1a_CYRM_6,T1a_CYRM_8,T1a_CYRM_9,T1a_CYRM_15,T1a_CYRM_17,T1a_CYRM_21,T1a_CYRM_22,T1a_CYRM_24,T1a_CYRM_25,T1a_CYRM_27,
     T1a_CYRM_1_CA_A1,T1a_CYRM_2_CA_A2,T1a_CYRM_4_CA_A3,T1a_CYRM_7_CA_A4,T1a_CYRM_10_CA_A5,T1a_CYRM_11_CA_A6,T1a_CYRM_12_CA_A7,T1a_CYRM_13_CA_A8,T1a_CYRM_14_CA_A9,T1a_CYRM_16_CA_A10,
     T1a_CYRM_18_CA_A11,T1a_CYRM_19_CA_A12,T1a_CYRM_20_CA_A13,T1a_CYRM_23_CA_A14,T1a_CYRM_26_CA_A15,T1a_CYRM_28_CA_A16,T1a_CYRM_CA_A1).
VARIABLE LABELS T1a_CA_CYRM29_total_T1_CA 'T1a CYRM-29 total (sum), only CA (29 items)'.
EXECUTE.

COMPUTE T1a_CA_CYRM28_I = SUM.11 (T1a_CYRM_2_CA_A2,T1a_CYRM_4_CA_A3,T1a_CYRM_8,T1a_CYRM_11_CA_A6,T1a_CYRM_13_CA_A8,T1a_CYRM_14_CA_A9,T1a_CYRM_15,T1a_CYRM_18_CA_A11,T1a_CYRM_20_CA_A13,T1a_CYRM_21,T1a_CYRM_25).
VARIABLE LABELS T1a_CA_CYRM28_I 'T1a CYRM-28 individual subscale (sum), only CA'.
EXECUTE.

COMPUTE T1a_CA_CYRM28_R = SUM.7 (T1a_CYRM_5, T1a_CYRM_6, T1a_CYRM_7_CA_A4, T1a_CYRM_12_CA_A7,T1a_CYRM_17, T1a_CYRM_24, T1a_CYRM_26_CA_A15).
VARIABLE LABELS T1a_CA_CYRM28_R 'T1a CYRM-28 relational subscale (sum), only CA'.
EXECUTE.

COMPUTE T1a_CA_CYRM28_C = SUM.10 (T1a_CYRM_1_CA_A1,T1a_CYRM_3,T1a_CYRM_9,T1a_CYRM_10_CA_A5,T1a_CYRM_16_CA_A10,T1a_CYRM_19_CA_A12,T1a_CYRM_22,T1a_CYRM_23_CA_A14,T1a_CYRM_27,T1a_CYRM_28_CA_A16).
VARIABLE LABELS T1a_CA_CYRM28_C 'T1a CYRM-28 contextual subscale (sum), only CA (original 10 items)'.
EXECUTE.

COMPUTE T1a_CA_CYRM29_C_CA = SUM.11 (T1a_CYRM_1_CA_A1,T1a_CYRM_3,T1a_CYRM_9,T1a_CYRM_10_CA_A5,T1a_CYRM_16_CA_A10,T1a_CYRM_19_CA_A12,T1a_CYRM_22,T1a_CYRM_23_CA_A14,T1a_CYRM_27,T1a_CYRM_28_CA_A16,T1a_CYRM_CA_A1).
VARIABLE LABELS T1a_CA_CYRM29_C_CA 'T1a CYRM-29 contextual subscale (sum), only CA (11 items)'.
EXECUTE.

COMPUTE T1a_CYRM28_IndPS=SUM.5 (T1a_CYRM_2, T1a_CYRM_8, T1a_CYRM_11, T1a_CYRM_13, T1a_CYRM_21).
VARIABLE LABELS T1a_CYRM28_IndPS 'T1a_CYRM28_Individual Personal Skills'.
EXECUTE.
COMPUTE T1a_CYRM28_IndPeer= SUM.2 (T1a_CYRM_14, T1a_CYRM_18).
VARIABLE LABELS T1a_CYRM28_IndPeer 'T1a_CYRM28_Individual Peer Support'.
EXECUTE.
COMPUTE T1a_CYRM28_IndSS= SUM.4 (T1a_CYRM_4, T1a_CYRM_15, T1a_CYRM_20, T1a_CYRM_25).
VARIABLE LABELS T1a_CYRM28_IndSS 'T1a_CYRM28_Individual Social Skills'.
EXECUTE.
*The Caregiver Sub-scale of the CYRM-26 has two sub-clusters of questions*.
COMPUTE T1a_CYRM28_CrPhys= SUM.2 (T1a_CYRM_5, T1a_CYRM_7).
VARIABLE LABELS T1a_CYRM28_CrPhys 'T1a_CYRM28_Caregivers Physical Care Giving'.
EXECUTE.
COMPUTE T1a_CYRM28_CrPsyc= SUM.5 (T1a_CYRM_6, T1a_CYRM_12, T1a_CYRM_17, T1a_CYRM_24, T1a_CYRM_26).
VARIABLE LABELS T1a_CYRM28_CrPsyc 'T1a_CYRM28_Caregivers Psychological Care Giving'.
EXECUTE.
*The Contextual Sub-scale of the CYRM-26 has three sub-clusters of questions*.
COMPUTE T1a_CYRM28_CntS= SUM.2 (T1a_CYRM_22, T1a_CYRM_23).
VARIABLE LABELS T1a_CYRM28_CntS 'T1a_CYRM28_Context Spiritual'.
EXECUTE.
COMPUTE T1a_CYRM28_CntEd= SUM.2 (T1a_CYRM_3, T1a_CYRM_16).
VARIABLE LABELS T1a_CYRM28_CntEd 'T1a_CYRM28_Context Education'.
EXECUTE.
COMPUTE T1a_CYRM28_CntC= SUM.5 (T1a_CYRM_1, T1a_CYRM_10, T1a_CYRM_19, T1a_CYRM_26, T1a_CYRM_27).
VARIABLE LABELS T1a_CYRM28_CntC 'T1a_CYRM28_Context Cultural'.
EXECUTE.


***** Perception of Neighbourhood scale: Only SA.
COMPUTE T1a_SA_PoNS = SUM.8 (T1a_SA_PoNS_1,T1a_SA_PoNS_2,T1a_SA_PoNS_3R,T1a_SA_PoNS_4,T1a_SA_PoNS_5,T1a_SA_PoNS_6R,T1a_SA_PoNS_7,T1a_SA_PoNS_8R).
VARIABLE LABELS T1a_SA_PoNS 'T1a Perception of Neighbourhood scale (sum), only SA'.
EXECUTE.

COMPUTE T1a_SA_PoNS_SA = SUM.10 (T1a_SA_PoNS_1,T1a_SA_PoNS_2,T1a_SA_PoNS_3R,T1a_SA_PoNS_4,T1a_SA_PoNS_5,T1a_SA_PoNS_6R,T1a_SA_PoNS_7,T1a_SA_PoNS_8R,T1a_SA_PoNS_A1,T1a_SA_PoNS_A2).
VARIABLE LABELS T1a_SA_PoNS_SA 'T1a Perception of Neighbourhood scale (sum), only SA & SA specific (10 items as at T1)'.
EXECUTE.

COMPUTE T1a_SA_PoNS_T1a_SA = SUM.13 (T1a_SA_PoNS_1,T1a_SA_PoNS_2,T1a_SA_PoNS_3R,T1a_SA_PoNS_4,T1a_SA_PoNS_5,T1a_SA_PoNS_6R,T1a_SA_PoNS_7,T1a_SA_PoNS_8R,T1a_SA_PoNS_A1,T1a_SA_PoNS_A2,T1a_SA_PoNS_A3,T1a_SA_PoNS_A4,T1a_SA_PoNS_A5R).
VARIABLE LABELS T1a_SA_PoNS_T1a_SA 'T1a Perception of Neighbourhood scale (sum), only SA & T1a specific (13 items)'.
EXECUTE.


***** Benevolent Childhood Experiences scale. Only SA.
COMPUTE T1a_SA_BCE = SUM.10 (T1a_SA_BCE_1R, T1a_SA_BCE_2R, T1a_SA_BCE_3R, T1a_SA_BCE_4R, T1a_SA_BCE_5R, T1a_SA_BCE_6R, T1a_SA_BCE_7R, T1a_SA_BCE_8R, T1a_SA_BCE_9R, T1a_SA_BCE_10R).
VARIABLE LABELS T1a_SA_BCE 'T1a Benevolent Childhood Experiences scale (sum), only SA'.
EXECUTE.


***** Peer Support scale.
COMPUTE T1a_PeerSupp = SUM.4 (T1a_PeerSupp_1,T1a_PeerSupp_2,T1a_PeerSupp_3,T1a_PeerSupp_4).
VARIABLE LABELS T1a_PeerSupp 'T1a Peer support scale (sum)'.
EXECUTE.


***** Parenting Scale: Parental-caregiver supervision subscale & Parental-caregiver warmth subscale. Only SA.
COMPUTE T1a_SA_PCSuper = SUM.4 (T1a_SA_PCSuper_1,T1a_SA_PCSuper_2,T1a_SA_PCSuper_3,T1a_SA_PCSuper_4).
VARIABLE LABELS T1a_SA_PCSuper 'T1a Parenting Scale: Parental-caregiver supervision subscale (sum), only SA'.
EXECUTE.

COMPUTE T1a_SA_PCSuper_T2 = SUM.3 (T1a_SA_PCSuper_1,T1a_SA_PCSuper_2,T1a_SA_PCSuper_3).
VARIABLE LABELS T1a_SA_PCSuper_T2 'T1a Parenting Scale: Parental-caregiver supervision subscale (sum), only SA (3 items to be comparable to T2)'.
EXECUTE.

COMPUTE T1a_SA_PCWarm = SUM.3 (T1a_SA_PCWarm_1,T1a_SA_PCWarm_2,T1a_SA_PCWarm_3).
VARIABLE LABELS T1a_SA_PCWarm 'T1a Parenting Scale: Parental-caregiver warmth subscale (sum), only SA'.
EXECUTE.


VARIABLE LEVEL T1a_CPTS T1a_BDI_II T1a_CA_SF_15_total T1a_CA_SF_15_physical T1a_CA_SF_15_role T1a_CA_SF_15_social T1a_CA_SF_15_perceptions
                            T1a_CA_SF_15_pain T1a_CYRM12 T1a_CA_CYRM28_total T1a_CA_CYRM29_total_T1_CA T1a_CA_CYRM28_I T1a_CA_CYRM28_R
                            T1a_CA_CYRM28_C T1a_CA_CYRM29_C_CA T1a_SA_PoNS T1a_SA_PoNS_SA T1a_SA_PoNS_T1a_SA T1a_SA_BCE T1a_PeerSupp
                            T1a_SA_PCSuper T1a_SA_PCSuper_T2 T1a_SA_PCWarm (SCALE).
EXECUTE.





*************************************************************************************************************************************************************************
************************************************************ T2 ****************************************************************************************** T2 *******
*************************************************************************************************************************************************************************

* ParticipantID_CA 36: BCOPE response bias -> deleted responses. 

*************************************************************************************
*********************************RECODING**************************************

**CPTS - Child Posttraumtic Stress**
* T2_CPTS_9.
RECODE T2_CPTS_9 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_CPTS_9R.
EXECUTE.

* T2_CPTS_11.
RECODE T2_CPTS_11 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_CPTS_11R.
EXECUTE.

**SF15 - Short Form Health Survey 15**
* T2_SF15_1.
RECODE T2_SF15_1 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_SF15_1R.
EXECUTE.

* T2_SF15_8.
RECODE T2_SF15_8 (1=6) (2=5) (3=4) (4=3) (5=2) (6=1) (ELSE=SYSMIS) INTO T2_SF15_8R.
EXECUTE.

* T2_SF15_13.
RECODE T2_SF15_13 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_SF15_13R.
EXECUTE.

* T2_SF15_14.
RECODE T2_SF15_14 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_SF15_14R.
EXECUTE.

**FAS - Family Adversity Scale**.
RECODE T2_FAS_1 T2_FAS_2 T2_FAS_3 T2_FAS_4 T2_FAS_5 T2_FAS_6 T2_FAS_7 T2_FAS_8 T2_FAS_9 
    T2_FAS_SA_A1 (2=0) (ELSE=Copy) INTO T2_FAS_1R T2_FAS_2R T2_FAS_3R T2_FAS_4R T2_FAS_5R T2_FAS_6R 
    T2_FAS_7R T2_FAS_8R T2_FAS_9R T2_FAS_SA_A1R.
EXECUTE.


**PoNS - Perception of Neighborhood**
*T2_PoNS_3.
RECODE T2_PoNS_3 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T2_PoNS_3R.
EXECUTE.

*T2_PoNS_6.
RECODE T2_PoNS_6 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T2_PoNS_6R.
EXECUTE.

*T2_PoNS_8.
RECODE T2_PoNS_8 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T2_PoNS_8R.
EXECUTE.

*T2_PoNS_CA_A1.
RECODE T2_PoNS_CA_A1 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T2_PoNS_CA_A1R.
EXECUTE.

*T2_PoNS_CA_A2.
RECODE T2_PoNS_CA_A2 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T2_PoNS_CA_A2R.
EXECUTE.

RECODE T2_PoNS_SA_A5 (1=4) (2=3) (3=2) (4=1) (ELSE=SYSMIS) INTO T2_PoNS_SA_A5R.
EXECUTE.


**BCE - Benevolent Childhood Experiences**.
RECODE T2_BCE_1 T2_BCE_2 T2_BCE_3 T2_BCE_4 T2_BCE_5 T2_BCE_6 T2_BCE_7 T2_BCE_8 T2_BCE_9 T2_BCE_10 
    (2=0) (ELSE=Copy) INTO T2_BCE_1R T2_BCE_2R T2_BCE_3R T2_BCE_4R T2_BCE_5R T2_BCE_6R T2_BCE_7R 
    T2_BCE_8R T2_BCE_9R T2_BCE_10R.
EXECUTE.


**JS - Job Satisfaction**

*T2_SA_JS_2.
RECODE T2_SA_JS_1 T2_SA_JS_3  (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) (ELSE=SYSMIS) INTO T2_SA_JS_1R T2_SA_JS_3R.
EXECUTE.


**SES - School Engagement**
*T2_SES_5.
RECODE T2_SES_5 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_SES_5R.
EXECUTE.

*T2_SES_14.
RECODE T2_SES_14 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_SES_14R.
EXECUTE.

*T2_SES_15.
RECODE T2_SES_15 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_SES_15R.
EXECUTE.

*T2_SES_16.
RECODE T2_SES_16 (1=5) (2=4) (3=3) (4=2) (5=1) (ELSE=SYSMIS) INTO T2_SES_16R.
EXECUTE.


**PSS - Perceived Stress Scale**.
RECODE T2_CA_PSS_4 T2_CA_PSS_5 T2_CA_PSS_7 T2_CA_PSS_8 (1=5) (2=4) (3=3) (4=2) (5=1) INTO 
    T2_CA_PSS_4R T2_CA_PSS_5R T2_CA_PSS_7R T2_CA_PSS_8R.
EXECUTE.




***************************** data cleaning *****************************

****** identifying duplicates ******
Go 'Data' > 'Identify Duplicate Cases' > put the variables that you want to use to identify duplicates into 'Define matching cases by'. Do not only check what you're interested in but also screen other items and look of they are duplicates, too
Note: This will change the order of participants in your dataset since SPSS puts the participants that could be potential duplicates based on the chosen variables at the top of the data set.


****** outliers / extreme values ******
All data has been checked for impossible and extreme values by comparing the values in the dataset with what was written in the participant's survey when the dataset was built (4 times by independent researchers),
If an extreme value is still present, then it was chosen by the participant and was therefore kept. It is up to the analyst to decide what will happen with that value.


****** Response bias ******
A variance of 0 indicates that this person has used the same value for each item of a scale. However, have a look at each respective scale if this could be realistic. E.g., scales with 20+ items should have some variance, no matter what's the topic,
However, exceptions could still be possible...  If many participants have a variance of 0 for the same scale (e.g., BCE) that would indicate that it is realistic. If a person has a variance of 0 for two or more scales (like CPTS and BDI), that should 
make suspicious. For this, calculate a new variable that just sums up the variance of the selected scales of interest. Use a frequency table to check how many have variance of 0.

**** Child Post-Traumatic Stress-Reaction Index (CPTS).
COMPUTE T2_CPTS_V = VARIANCE(T2_CPTS_1,T2_CPTS_2,T2_CPTS_3,T2_CPTS_4,T2_CPTS_5,T2_CPTS_6,T2_CPTS_7,T2_CPTS_8,T2_CPTS_9R,T2_CPTS_10,
                                          T2_CPTS_11R,T2_CPTS_12,T2_CPTS_13,T2_CPTS_14,T2_CPTS_15,T2_CPTS_16,T2_CPTS_17,T2_CPTS_18,T2_CPTS_19,T2_CPTS_20).
EXECUTE.


**** Beck Depression Inventory (BDI_II).
COMPUTE T2_BDI_II_V=VARIANCE(T2_BDI_1,T2_BDI_2,T2_BDI_3,T2_BDI_4,T2_BDI_5,T2_BDI_6,T2_BDI_7,T2_BDI_8,T2_BDI_9,
                                        T2_BDI_10,T2_BDI_11,T2_BDI_12,T2_BDI_13,T2_BDI_14,T2_BDI_15,T2_BDI_16,T2_BDI_17,T2_BDI_18,T2_BDI_19,
                                        T2_BDI_20,T2_BDI_21).
EXECUTE.


**** Short Form Health Survey (SF-15).
COMPUTE T2_SF_15_physical_V = VARIANCE(T2_SF15_2,T2_SF15_3,T2_SF15_4,T2_SF15_5,T2_SF15_6,T2_SF15_7).
EXECUTE.

COMPUTE T2_SF_15_role_V = VARIANCE(T2_SF15_9,T2_SF15_10).
EXECUTE.

COMPUTE T2_SF_15_perceptions_V = VARIANCE(T2_SF15_1R,T2_SF15_12,T2_SF15_13R,T2_SF15_14R,T2_SF15_15).
EXECUTE.


**** School Engagement Scale: SA and CA have the same 32 items and SA has 1 extra item (SES).
COMPUTE T2_SES_total_V = VARIANCE(T2_SES_1,T2_SES_2,T2_SES_3,T2_SES_4,T2_SES_5R,T2_SES_6,T2_SES_7,
    T2_SES_8,T2_SES_9,T2_SES_10,T2_SES_11,T2_SES_12,T2_SES_13,T2_SES_14R,T2_SES_15R,T2_SES_16R,
    T2_SES_17,T2_SES_18,T2_SES_19,T2_SES_20,T2_SES_21,T2_SES_22,T2_SES_23,T2_SES_24,T2_SES_25,T2_SES_26,
    T2_SES_27,T2_SES_28,T2_SES_29,T2_SES_30,T2_SES_31,T2_SES_32).
EXECUTE.

COMPUTE T2_SES_total_SA_V = VARIANCE(T2_SES_1,T2_SES_2,T2_SES_3,T2_SES_4,T2_SES_5R,T2_SES_6,T2_SES_7,
    T2_SES_8,T2_SES_9,T2_SES_10,T2_SES_11,T2_SES_12,T2_SES_13,T2_SES_14R,T2_SES_15R,T2_SES_16R,
    T2_SES_17,T2_SES_18,T2_SES_19,T2_SES_20,T2_SES_21,T2_SES_22,T2_SES_23,T2_SES_24,T2_SES_25,T2_SES_26,
    T2_SES_27,T2_SES_28,T2_SES_29,T2_SES_30,T2_SES_31,T2_SES_32,T2_SES_SA_A1).
EXECUTE.


**** Work Engagement Scale (WES).
COMPUTE T2_WES_total_V=VARIANCE(T2_WES_1,T2_WES_2,T2_WES_3,T2_WES_4,T2_WES_5,T2_WES_6,T2_WES_7,T2_WES_8,T2_WES_9).
EXECUTE.

**** Delinquency scale (DS).
COMPUTE T2_Delinquency_V = VARIANCE (T2_DS_1,T2_DS_2,T2_DS_3,T2_DS_4,T2_DS_5,T2_DS_6).
EXECUTE.


**** Victimisation by Community (subscale of the Exposure to Violence scale): SA & CA have the same 4 items and SA has 3 extra items. (VbC).
COMPUTE T2_VbC_V = VARIANCE (T2_VbC_1,T2_VbC_2,T2_VbC_3,T2_VbC_4).
EXECUTE.

COMPUTE T2_VbC_SA_V = VARIANCE (T2_VbC_1,T2_VbC_2,T2_VbC_3,T2_VbC_4,T2_VbC_SA_A1,T2_VbC_SA_A2,T2_VbC_SA_A3).
EXECUTE.


**** Family Adversity scale: SA & CA have the same 9 items and SA has 1 extra item (FAS).
COMPUTE T2_FAS_V = VARIANCE (T2_FAS_1R,T2_FAS_2R,T2_FAS_3R,T2_FAS_4R,T2_FAS_5R,T2_FAS_6R,T2_FAS_7R,T2_FAS_8R,T2_FAS_9R).
EXECUTE.

COMPUTE T2_FAS_SA_V = VARIANCE (T2_FAS_1R,T2_FAS_2R,T2_FAS_3R,T2_FAS_4R,T2_FAS_5R,T2_FAS_6R,T2_FAS_7R,T2_FAS_8R,T2_FAS_9R,T2_FAS_SA_A1R).
EXECUTE.

**** Child and Youth Resilience Measure: SA and CA have the same 28 items, CA has 1 extra item (CYRM).
COMPUTE T2_CYRM28_total_V=VARIANCE(T2_CYRM_1,T2_CYRM_2,T2_CYRM_3,T2_CYRM_4,T2_CYRM_5,T2_CYRM_6,
    T2_CYRM_7,T2_CYRM_8,T2_CYRM_9,T2_CYRM_10,T2_CYRM_11,T2_CYRM_12,T2_CYRM_13,T2_CYRM_14,T2_CYRM_15,
    T2_CYRM_16,T2_CYRM_17,T2_CYRM_18,T2_CYRM_19,T2_CYRM_20,T2_CYRM_21,T2_CYRM_22,T2_CYRM_23,T2_CYRM_24,
    T2_CYRM_25,T2_CYRM_26,T2_CYRM_27,T2_CYRM_28).
EXECUTE.

COMPUTE T2_CYRM28_total_CA_V=VARIANCE(T2_CYRM_1,T2_CYRM_2,T2_CYRM_3,T2_CYRM_4,T2_CYRM_5,T2_CYRM_6,
    T2_CYRM_7,T2_CYRM_8,T2_CYRM_9,T2_CYRM_10,T2_CYRM_11,T2_CYRM_12,T2_CYRM_13,T2_CYRM_14,T2_CYRM_15,
    T2_CYRM_16,T2_CYRM_17,T2_CYRM_18,T2_CYRM_19,T2_CYRM_20,T2_CYRM_21,T2_CYRM_22,T2_CYRM_23,T2_CYRM_24,
    T2_CYRM_25,T2_CYRM_26,T2_CYRM_27,T2_CYRM_28,T2_CYRM_CA_A1).
EXECUTE.


**** Perception of Neighbourhood scale (PoNS).
COMPUTE T2_PoNS_V = VARIANCE (T2_PoNS_1,T2_PoNS_2,T2_PoNS_3R,T2_PoNS_4,T2_PoNS_5,T2_PoNS_6R,T2_PoNS_7,T2_PoNS_8R).
EXECUTE.

COMPUTE T2_PoNS_CA_V = VARIANCE (T2_PoNS_1,T2_PoNS_2,T2_PoNS_3R,T2_PoNS_4,T2_PoNS_5,T2_PoNS_6R,T2_PoNS_7,T2_PoNS_8R,T2_PoNS_CA_A1R,T2_PoNS_CA_A2R).
EXECUTE.

COMPUTE T2_PoNS_SA_V = VARIANCE (T2_PoNS_1,T2_PoNS_2,T2_PoNS_3R,T2_PoNS_4,T2_PoNS_5,T2_PoNS_6R,T2_PoNS_7,T2_PoNS_8R,T2_PoNS_SA_A1,T2_PoNS_SA_A2).
EXECUTE.


**** Benevolent Childhood Experiences scale (BCE).
COMPUTE T2_BCE_V = VARIANCE (T2_BCE_1R,T2_BCE_2R,T2_BCE_3R,T2_BCE_4R,T2_BCE_5R,T2_BCE_6R,T2_BCE_7R,T2_BCE_8R,T2_BCE_9R,T2_BCE_10R).
EXECUTE.


**** Sensitivity scale (very short version) (SS).
COMPUTE T2_Sensitivity_V = VARIANCE (T2_SS_1,T2_SS_2,T2_SS_3,T2_SS_4,T2_SS_5,T2_SS_6).
EXECUTE.


**** Peer Support scale (PeerSupp).
COMPUTE T2_PeerSupp_V = VARIANCE (T2_PeerSupp_1,T2_PeerSupp_2,T2_PeerSupp_3,T2_PeerSupp_4).
EXECUTE.


**** Substance use/risky behaviours scales. Only CA. (RB, SUS).
COMPUTE T2_CA_SUS_V = VARIANCE (T2_CA_SUS_1,T2_CA_SUS_2,T2_CA_SUS_3,T2_CA_SUS_4,T2_CA_SUS_5,T2_CA_SUS_6,T2_CA_SUS_7).
EXECUTE.

COMPUTE T2_CA_RBS_V = VARIANCE(T2_CA_SUS_1,T2_CA_SUS_2,T2_CA_SUS_3,T2_CA_SUS_4,T2_CA_SUS_5,T2_CA_SUS_6,T2_CA_SUS_7,T2_CA_RB_1).
EXECUTE.


**** Parenting Scale: Parental-caregiver supervision subscale (PCSuper) / Parental-caregiver warmth subscale (PCWarm). Only SA.
COMPUTE T2_SA_PCSuper_V = VARIANCE (T2_SA_PCSuper_1,T2_SA_PCSuper_2,T2_SA_PCSuper_3,T2_SA_PCSuper_4).
EXECUTE.

COMPUTE T2_SA_PCWarm_V = VARIANCE (T2_SA_PCWarm_1,T2_SA_PCWarm_2,T2_SA_PCWarm_3).
EXECUTE.


**** Job satisfaction. Only SA. (JS).
COMPUTE T2_SA_JS_V = VARIANCE (T2_SA_JS_1,T2_SA_JS_2R,T2_SA_JS_3).
EXECUTE.




*************************************************************************************
*********************************SCALE TOTALS*********************************

*All scale totals are sum scores. If you want mean scores just replace SUM with MEAN and change the variable name to not overwrite the sum score.


***** Child Post-Traumatic Stress-Reaction Index (CPTS-RI).
COMPUTE T2_CPTS = SUM.20(T2_CPTS_1,T2_CPTS_2,T2_CPTS_3,T2_CPTS_4,T2_CPTS_5,T2_CPTS_6,T2_CPTS_7,T2_CPTS_8,T2_CPTS_9R,T2_CPTS_10,
                                          T2_CPTS_11R,T2_CPTS_12,T2_CPTS_13,T2_CPTS_14,T2_CPTS_15,T2_CPTS_16,T2_CPTS_17,T2_CPTS_18,T2_CPTS_19,T2_CPTS_20).
VARIABLE LABELS T2_CPTS 'T2 Child Post-Traumatic Stress - Reaction Index (CPTS-RI) scale (sum)'.
EXECUTE.


***** Beck Depression Inventory (BDI_II).
COMPUTE T2_BDI_II=SUM.21(T2_BDI_1,T2_BDI_2,T2_BDI_3,T2_BDI_4,T2_BDI_5,T2_BDI_6,T2_BDI_7,T2_BDI_8,T2_BDI_9,
                                        T2_BDI_10,T2_BDI_11,T2_BDI_12,T2_BDI_13,T2_BDI_14,T2_BDI_15,T2_BDI_16,T2_BDI_17,T2_BDI_18,T2_BDI_19,
                                        T2_BDI_20,T2_BDI_21).
VARIABLE LABELS T2_BDI_II 'T2 Beck Depression Inventory-II (sum)'.
EXECUTE.


***** Short Form Health Survey (SF-15).
COMPUTE T2_SF_15_total=SUM.15(T2_SF15_1R,T2_SF15_2,T2_SF15_3,T2_SF15_4,T2_SF15_5,T2_SF15_6,T2_SF15_7,
    T2_SF15_8R,T2_SF15_9,T2_SF15_10,T2_SF15_11,T2_SF15_12,T2_SF15_13R,T2_SF15_14R,T2_SF15_15).
VARIABLE LABELS T2_SF_15_total 'T2 SF-15 overall health (sum)'.
EXECUTE.

COMPUTE T2_SF_15_physical = SUM.6 (T2_SF15_2,T2_SF15_3,T2_SF15_4,T2_SF15_5,T2_SF15_6,T2_SF15_7).
VARIABLE LABELS T2_SF_15_physical 'T2 SF-15 physical functioning subscale (sum)'.
EXECUTE.

COMPUTE T2_SF_15_role = SUM.2 (T2_SF15_9,T2_SF15_10).
VARIABLE LABELS T2_SF_15_role 'T2 SF-15 role functioning subscale (sum)'.
EXECUTE.

COMPUTE T2_SF_15_social = SUM.1 (T2_SF15_11).
VARIABLE LABELS T2_SF_15_social 'T2 SF-15 social functioning subscale (sum)'.
EXECUTE.

COMPUTE T2_SF_15_perceptions = SUM.5 (T2_SF15_1R,T2_SF15_12,T2_SF15_13R,T2_SF15_14R,T2_SF15_15).
VARIABLE LABELS T2_SF_15_perceptions 'T2 SF-15 current health perceptions subscale (sum)'.
EXECUTE.

COMPUTE T2_SF_15_pain = SUM.1 (T2_SF15_8R).
VARIABLE LABELS T2_SF_15_pain 'T2 SF-15 pain subscale (sum)'.
EXECUTE.


***** School Engagement Scale: SA has 1 extra item (T2_SES_SA_A1) as at T1. However, one item was forgotten in the SA T2 survey (T2_SES_27_CA). Hence, the comparable total score for both countries over time consists of 31 variables.
COMPUTE T2_SES_total = SUM.31(T2_SES_1,T2_SES_2,T2_SES_3,T2_SES_4,T2_SES_5R,T2_SES_6,T2_SES_7,
    T2_SES_8,T2_SES_9,T2_SES_10,T2_SES_11,T2_SES_12,T2_SES_13,T2_SES_14R,T2_SES_15R,T2_SES_16R,
    T2_SES_17,T2_SES_18,T2_SES_19,T2_SES_20,T2_SES_21,T2_SES_22,T2_SES_23,T2_SES_24,T2_SES_25,T2_SES_26,
    T2_SES_28,T2_SES_29,T2_SES_30,T2_SES_31,T2_SES_32).
VARIABLE LABELS T2_SES_total 'T2 School engagement overall scale (sum), T2 specific (31 items since T2_SES_27 was not assessed for SA)'.
EXECUTE.
    
COMPUTE T2_SES_total_CA = SUM.32(T2_SES_1,T2_SES_2,T2_SES_3,T2_SES_4,T2_SES_5R,T2_SES_6,T2_SES_7,
    T2_SES_8,T2_SES_9,T2_SES_10,T2_SES_11,T2_SES_12,T2_SES_13,T2_SES_14R,T2_SES_15R,T2_SES_16R,
    T2_SES_17,T2_SES_18,T2_SES_19,T2_SES_20,T2_SES_21,T2_SES_22,T2_SES_23,T2_SES_24,T2_SES_25,T2_SES_26,
    T2_SES_27_CA,T2_SES_28,T2_SES_29,T2_SES_30,T2_SES_31,T2_SES_32).
VARIABLE LABELS T2_SES_total_CA 'T2 School engagement overall scale (sum), CA specific (original 32 items as at T1)'.
EXECUTE.

COMPUTE T2_SES_total_SA = SUM.32(T2_SES_1,T2_SES_2,T2_SES_3,T2_SES_4,T2_SES_5R,T2_SES_6,T2_SES_7,
    T2_SES_8,T2_SES_9,T2_SES_10,T2_SES_11,T2_SES_12,T2_SES_13,T2_SES_14R,T2_SES_15R,T2_SES_16R,
    T2_SES_17,T2_SES_18,T2_SES_19,T2_SES_20,T2_SES_21,T2_SES_22,T2_SES_23,T2_SES_24,T2_SES_25,T2_SES_26,
    T2_SES_28,T2_SES_29,T2_SES_30,T2_SES_31,T2_SES_32,T2_SES_SA_A1).
VARIABLE LABELS T2_SES_total_SA 'T2 School engagement overall scale (sum), SA T2 specific (32 items since T2_SES_27 was not assessed for SA)'.
EXECUTE.

COMPUTE  T2_SES_affective = SUM.9 (T2_SES_1,T2_SES_2,T2_SES_3,T2_SES_4,T2_SES_5R,T2_SES_6,T2_SES_7,T2_SES_8,T2_SES_9).
VARIABLE LABELS  T2_SES_affective 'T2 School engagement affective engagement subscale (sum)'.
EXECUTE.

COMPUTE T2_SES_behavioural = SUM.11 (T2_SES_10,T2_SES_11,T2_SES_12,T2_SES_13,T2_SES_14R,T2_SES_15R,T2_SES_16R,T2_SES_17,T2_SES_18,T2_SES_19,T2_SES_20).
VARIABLE LABELS T2_SES_behavioural 'T2 School engagement behavioural engagement subscale (sum), (11 items)'.
EXECUTE.

COMPUTE T2_SES_behavioural_SA = SUM.12 (T2_SES_10,T2_SES_11,T2_SES_12,T2_SES_13,T2_SES_14R,T2_SES_15R,T2_SES_16R,T2_SES_17,T2_SES_18,T2_SES_19,T2_SES_20, T2_SES_SA_A1).
VARIABLE LABELS T2_SES_behavioural_SA 'T2 School engagement behavioural engagement subscale (sum), SA specific (12 items)'.
EXECUTE.

COMPUTE T2_SES_cognitive_CA = SUM.12 (T2_SES_21,T2_SES_22,T2_SES_23,T2_SES_24,T2_SES_25,T2_SES_26,T2_SES_27_CA,T2_SES_28,T2_SES_29,T2_SES_30,T2_SES_31,T2_SES_32).
VARIABLE LABELS T2_SES_cognitive_CA 'T2 School engagement cognitive engagement subscale (sum), CA specific (original 12 items as at T1)'.
EXECUTE.

COMPUTE T2_SES_cognitive = SUM.11 (T2_SES_21,T2_SES_22,T2_SES_23,T2_SES_24,T2_SES_25,T2_SES_26,T2_SES_28,T2_SES_29,T2_SES_30,T2_SES_31,T2_SES_32).
VARIABLE LABELS T2_SES_cognitive 'T2 School engagement cognitive engagement subscale (sum), T2 specific (11 items since T2_SES_27 was not assessed for SA)'.
EXECUTE.


***** Work Engagement Scale (UWES-9).
COMPUTE T2_WES_total=SUM.9(T2_WES_1,T2_WES_2,T2_WES_3,T2_WES_4,T2_WES_5,T2_WES_6,T2_WES_7,T2_WES_8,T2_WES_9).
VARIABLE LABELS T2_WES_total 'T2 Work engagement scale overall (sum)'.
EXECUTE.

COMPUTE T2_WES_vigour = SUM.3 (T2_WES_1,T2_WES_2,T2_WES_5).
VARIABLE LABELS T2_WES_vigour 'T2 Work engagement vigour subscale (sum)'.
EXECUTE.

COMPUTE T2_WES_dedication = SUM.3 (T2_WES_3,T2_WES_4,T2_WES_7).
VARIABLE LABELS T2_WES_dedication 'T2 Work engagement dedication subscale (sum)'.
EXECUTE.

COMPUTE T2_WES_absorption = SUM.3 (T2_WES_6,T2_WES_8,T2_WES_9).
VARIABLE LABELS T2_WES_absorption 'T2 Work engagement absorption subscale (sum)'.
EXECUTE.


***** Job satisfaction. Only SA.
COMPUTE T2_SA_JS = SUM.3 (T2_SA_JS_1R,T2_SA_JS_2,T2_SA_JS_3R).
VARIABLE LABELS T2_SA_JS 'T2 Job satisfaction (sum), only SA'.
EXECUTE.


***** Delinquency scale.
COMPUTE T2_Delinquency = SUM.6 (T2_DS_1,T2_DS_2,T2_DS_3,T2_DS_4,T2_DS_5,T2_DS_6).
VARIABLE LABELS T2_Delinquency 'T2 Delinquency scale (sum)'.
EXECUTE.


***** Substance use/risky behaviours scales. Only CA.
COMPUTE T2_CA_SUS = SUM.7 (T2_CA_SUS_1,T2_CA_SUS_2,T2_CA_SUS_3,T2_CA_SUS_4,T2_CA_SUS_5,T2_CA_SUS_6,T2_CA_SUS_7).
VARIABLE LABELS T2_CA_SUS 'T2 Substance use scale (sum), only CA'.
EXECUTE.

COMPUTE T2_CA_RB = SUM.8 (T2_CA_SUS_1,T2_CA_SUS_2,T2_CA_SUS_3,T2_CA_SUS_4,T2_CA_SUS_5,T2_CA_SUS_6,T2_CA_SUS_7,T2_CA_RB_1).
VARIABLE LABELS T2_CA_RB 'T2 Risky behaviours scale (sum), only CA'.
EXECUTE.


***** Victimisation by Community (subscale of the Exposure to Violence scale): SA & CA have the same 4 items and SA has 3 extra items.
COMPUTE T2_VbC = SUM.4 (T2_VbC_1,T2_VbC_2,T2_VbC_3,T2_VbC_4).
VARIABLE LABELS T2_VbC 'T2 Victimisation by Community subscale of the Exposure to Violence scale (sum), (4 items)'.
EXECUTE.

COMPUTE T2_VbC_SA = SUM.7 (T2_VbC_1,T2_VbC_2,T2_VbC_3,T2_VbC_4,T2_VbC_SA_A1,T2_VbC_SA_A2,T2_VbC_SA_A3).
VARIABLE LABELS T2_VbC_SA 'T2 Victimisation by Community subscale of the Exposure to Violence scale (sum), SA specific (7 items)'.
EXECUTE.


***** Family Adversity scale: SA & CA have the same 9 items and SA has 1 extra item.
COMPUTE T2_FAS = SUM.9 (T2_FAS_1R,T2_FAS_2R,T2_FAS_3R,T2_FAS_4R,T2_FAS_5R,T2_FAS_6R,T2_FAS_7R,T2_FAS_8R,T2_FAS_9R).
VARIABLE LABELS T2_FAS 'T2 Family Adversity scale (sum), (9 items)'.
EXECUTE.

COMPUTE T2_FAS_SA = SUM.10 (T2_FAS_1R,T2_FAS_2R,T2_FAS_3R,T2_FAS_4R,T2_FAS_5R,T2_FAS_6R,T2_FAS_7R,T2_FAS_8R,T2_FAS_9R,T2_FAS_SA_A1R).
VARIABLE LABELS T2_FAS_SA 'T2 Family Adversity scale (sum), SA specific (10 items)'.
EXECUTE.


***** Perceived Stress Scale. Only CA.
COMPUTE T2_CA_PSS=SUM.10(T2_CA_PSS_1,T2_CA_PSS_2,T2_CA_PSS_3,T2_CA_PSS_6,T2_CA_PSS_9,T2_CA_PSS_10,
    T2_CA_PSS_4R,T2_CA_PSS_5R,T2_CA_PSS_7R,T2_CA_PSS_8R).
VARIABLE LABELS T2_CA_PSS 'T2 Perceived Stress Scale (sum), only CA'.
EXECUTE.


***** Child and Youth Resilience Measure: SA and CA have the same 28 items, CA has 1 extra item. The subscales are based on the original factor structure of the CYRM-28 which might rather fit CA. Hence, EFA or CFA should be done for SA.
COMPUTE T2_CYRM28_total=SUM.28(T2_CYRM_1,T2_CYRM_2,T2_CYRM_3,T2_CYRM_4,T2_CYRM_5,T2_CYRM_6,
    T2_CYRM_7,T2_CYRM_8,T2_CYRM_9,T2_CYRM_10,T2_CYRM_11,T2_CYRM_12,T2_CYRM_13,T2_CYRM_14,T2_CYRM_15,
    T2_CYRM_16,T2_CYRM_17,T2_CYRM_18,T2_CYRM_19,T2_CYRM_20,T2_CYRM_21,T2_CYRM_22,T2_CYRM_23,T2_CYRM_24,
    T2_CYRM_25,T2_CYRM_26,T2_CYRM_27,T2_CYRM_28).
VARIABLE LABELS T2_CYRM28_total 'T2 CYRM-28 total (sum)'.
EXECUTE.

COMPUTE T2_CYRM29_total_CA=SUM.29(T2_CYRM_1,T2_CYRM_2,T2_CYRM_3,T2_CYRM_4,T2_CYRM_5,T2_CYRM_6,
    T2_CYRM_7,T2_CYRM_8,T2_CYRM_9,T2_CYRM_10,T2_CYRM_11,T2_CYRM_12,T2_CYRM_13,T2_CYRM_14,T2_CYRM_15,
    T2_CYRM_16,T2_CYRM_17,T2_CYRM_18,T2_CYRM_19,T2_CYRM_20,T2_CYRM_21,T2_CYRM_22,T2_CYRM_23,T2_CYRM_24,
    T2_CYRM_25,T2_CYRM_26,T2_CYRM_27,T2_CYRM_28,T2_CYRM_CA_A1).
VARIABLE LABELS T2_CYRM29_total_CA 'T2 CYRM-29 total (sum), CA specific (29 items)'.
EXECUTE.

COMPUTE T2_CYRM28_I = SUM.11 (T2_CYRM_2,T2_CYRM_4,T2_CYRM_8,T2_CYRM_11,T2_CYRM_13,T2_CYRM_14,T2_CYRM_15,T2_CYRM_18,T2_CYRM_20,T2_CYRM_21,T2_CYRM_25).
VARIABLE LABELS T2_CYRM28_I 'T2 CYRM-28 individual subscale (sum)'.
EXECUTE.

COMPUTE T2_CYRM28_R = SUM.7 (T2_CYRM_5, T2_CYRM_6, T2_CYRM_7, T2_CYRM_12,T2_CYRM_17, T2_CYRM_24, T2_CYRM_26).
VARIABLE LABELS T2_CYRM28_R 'T2 CYRM-28 relational subscale (sum)'.
EXECUTE.

COMPUTE T2_CYRM28_C = SUM.10 (T2_CYRM_1,T2_CYRM_3,T2_CYRM_9,T2_CYRM_10,T2_CYRM_16,T2_CYRM_19,T2_CYRM_22,T2_CYRM_23,T2_CYRM_27,T2_CYRM_28).
VARIABLE LABELS T2_CYRM28_C 'T2 CYRM-28 contextual subscale (sum)'.
EXECUTE.

COMPUTE T2_CYRM28_IndPS=SUM.5 (T2_CYRM_2, T2_CYRM_8, T2_CYRM_11, T2_CYRM_13, T2_CYRM_21).
VARIABLE LABELS T2_CYRM28_IndPS 'T2_CYRM28_Individual Personal Skills'.
EXECUTE.
COMPUTE T2_CYRM28_IndPeer= SUM.2 (T2_CYRM_14, T2_CYRM_18).
VARIABLE LABELS T2_CYRM28_IndPeer 'T2_CYRM28_Individual Peer Support'.
EXECUTE.
COMPUTE T2_CYRM28_IndSS= SUM.4 (T2_CYRM_4, T2_CYRM_15, T2_CYRM_20, T2_CYRM_25).
VARIABLE LABELS T2_CYRM28_IndSS 'T2_CYRM28_Individual Social Skills'.
EXECUTE.
*The Caregiver Sub-scale of the CYRM-26 has two sub-clusters of questions*.
COMPUTE T2_CYRM28_CrPhys= SUM.2 (T2_CYRM_5, T2_CYRM_7).
VARIABLE LABELS T2_CYRM28_CrPhys 'T2_CYRM28_Caregivers Physical Care Giving'.
EXECUTE.
COMPUTE T2_CYRM28_CrPsyc= SUM.5 (T2_CYRM_6, T2_CYRM_12, T2_CYRM_17, T2_CYRM_24, T2_CYRM_26).
VARIABLE LABELS T2_CYRM28_CrPsyc 'T2_CYRM28_Caregivers Psychological Care Giving'.
EXECUTE.
*The Contextual Sub-scale of the CYRM-26 has three sub-clusters of questions*.
COMPUTE T2_CYRM28_CntS= SUM.2 (T2_CYRM_22, T2_CYRM_23).
VARIABLE LABELS T2_CYRM28_CntS 'T2_CYRM28_Context Spiritual'.
EXECUTE.
COMPUTE T2_CYRM28_CntEd= SUM.2 (T2_CYRM_3, T2_CYRM_16).
VARIABLE LABELS T2_CYRM28_CntEd 'T2_CYRM28_Context Education'.
EXECUTE.
COMPUTE T2_CYRM28_CntC= SUM.5 (T2_CYRM_1, T2_CYRM_10, T2_CYRM_19, T2_CYRM_26, T2_CYRM_27).
VARIABLE LABELS T2_CYRM28_CntC 'T2_CYRM28_Context Cultural'.
EXECUTE.


COMPUTE T2_CYRM29_C_CA = SUM.11 (T2_CYRM_1,T2_CYRM_3,T2_CYRM_9,T2_CYRM_10,T2_CYRM_16,T2_CYRM_19,T2_CYRM_22,T2_CYRM_23,T2_CYRM_27,T2_CYRM_28,T2_CYRM_CA_A1).
VARIABLE LABELS T2_CYRM29_C_CA 'T2 CYRM-29 contextual subscale (sum), CA specific (11 items)'.
EXECUTE.

COMPUTE T2_CYRM12=SUM.12(T2_CYRM_3,T2_CYRM_5,T2_CYRM_6,T2_CYRM_8,T2_CYRM_9,T2_CYRM_15,T2_CYRM_17,T2_CYRM_21,T2_CYRM_22,T2_CYRM_24,T2_CYRM_25,T2_CYRM_27).
VARIABLE LABELS T2_CYRM12 'T2 CYRM-12 (sum)'.
EXECUTE.



*****BCOPE - Brief Cope. Only CA.
COMPUTE T2_CA_BCOPE_self_distraction = SUM.2 (T2_CA_BCOPE_1,T2_CA_BCOPE_19).
VARIABLE LABELS T2_CA_BCOPE_self_distraction 'T2 Brief COPE self-distraction, only CA'.
COMPUTE T2_CA_BCOPE_active_coping = SUM.2 (T2_CA_BCOPE_2,T2_CA_BCOPE_7).
VARIABLE LABELS T2_CA_BCOPE_active_coping 'T2 Brief COPE active coping, only CA'.
COMPUTE T2_CA_BCOPE_denial = SUM.2 (T2_CA_BCOPE_3,T2_CA_BCOPE_8).
VARIABLE LABELS T2_CA_BCOPE_denial 'T2 Brief COPE denial, only CA'.
COMPUTE T2_CA_BCOPE_substance_use = SUM.2 (T2_CA_BCOPE_4,T2_CA_BCOPE_11).
VARIABLE LABELS T2_CA_BCOPE_substance_use 'T2 Brief COPE substance use, only CA'.
COMPUTE T2_CA_BCOPE_emotional_support = SUM.2 (T2_CA_BCOPE_5,T2_CA_BCOPE_15).
VARIABLE LABELS T2_CA_BCOPE_emotional_support 'T2 Brief COPE emotional support, only CA'.
COMPUTE T2_CA_BCOPE_informational_support = SUM.2 (T2_CA_BCOPE_10,T2_CA_BCOPE_23).
VARIABLE LABELS T2_CA_BCOPE_informational_support 'T2 Brief COPE informational support, only CA'.
COMPUTE T2_CA_BCOPE_behavioral_disengagement = SUM.2 (T2_CA_BCOPE_6,T2_CA_BCOPE_16).
VARIABLE LABELS T2_CA_BCOPE_behavioral_disengagement 'T2 Brief COPE behavioral disengagement, only CA'.
COMPUTE T2_CA_BCOPE_venting = SUM.2 (T2_CA_BCOPE_9,T2_CA_BCOPE_21).
VARIABLE LABELS T2_CA_BCOPE_venting 'T2 Brief COPE venting, only CA'.
COMPUTE T2_CA_BCOPE_positive_reframing = SUM.2 (T2_CA_BCOPE_12,T2_CA_BCOPE_17).
VARIABLE LABELS T2_CA_BCOPE_positive_reframing 'T2 Brief COPE positive reframing, only CA'.
COMPUTE T2_CA_BCOPE_planning = SUM.2 (T2_CA_BCOPE_14,T2_CA_BCOPE_25).
VARIABLE LABELS T2_CA_BCOPE_planning 'T2 Brief COPE planning, only CA'.
COMPUTE T2_CA_BCOPE_humour = SUM.2 (T2_CA_BCOPE_18,T2_CA_BCOPE_28).
VARIABLE LABELS T2_CA_BCOPE_humour 'T2 Brief COPE humour, only CA'.
COMPUTE T2_CA_BCOPE_acceptance = SUM.2 (T2_CA_BCOPE_20,T2_CA_BCOPE_24).
VARIABLE LABELS T2_CA_BCOPE_acceptance 'T2 Brief COPE acceptance, only CA'.
COMPUTE T2_CA_BCOPE_religion = SUM.2 (T2_CA_BCOPE_22,T2_CA_BCOPE_27).
VARIABLE LABELS T2_CA_BCOPE_religion 'T2 Brief COPE religion, only CA'.
COMPUTE T2_CA_BCOPE_self_blame = SUM.2 (T2_CA_BCOPE_13,T2_CA_BCOPE_26).
VARIABLE LABELS T2_CA_BCOPE_self_blame 'T2 Brief COPE self-blame, only CA'.
COMPUTE T2_CA_BCOPE_Avoidant = SUM.12 (T2_CA_BCOPE_1,T2_CA_BCOPE_19,T2_CA_BCOPE_3,T2_CA_BCOPE_8,T2_CA_BCOPE_4,T2_CA_BCOPE_11,
                                                                        T2_CA_BCOPE_6,T2_CA_BCOPE_16,T2_CA_BCOPE_9,T2_CA_BCOPE_21,T2_CA_BCOPE_13,T2_CA_BCOPE_26).
VARIABLE LABELS T2_CA_BCOPE_Avoidant 'T2 Brief COPE avoidant coping, only CA'.
COMPUTE T2_CA_BCOPE_Approach = SUM.12 (T2_CA_BCOPE_2,T2_CA_BCOPE_7,T2_CA_BCOPE_5,T2_CA_BCOPE_15,T2_CA_BCOPE_10,T2_CA_BCOPE_23,
                                                                            T2_CA_BCOPE_12,T2_CA_BCOPE_17,T2_CA_BCOPE_14,T2_CA_BCOPE_25,T2_CA_BCOPE_20,T2_CA_BCOPE_24).
VARIABLE LABELS T2_CA_BCOPE_Approach 'T2 Brief COPE approach coping, only CA'.
EXECUTE.

***** Perception of Neighbourhood scale: SA and CA have the same 8 items, CA has the same 2 extra items at T1 and T2, and SA has 2 extra items at T1 and 5 extra items at T1a & T2.
COMPUTE T2_PoNS = SUM.8 (T2_PoNS_1,T2_PoNS_2,T2_PoNS_3R,T2_PoNS_4,T2_PoNS_5,T2_PoNS_6R,T2_PoNS_7,T2_PoNS_8R).
VARIABLE LABELS T2_PoNS 'T2 Perception of Neighbourhood scale (sum), (8 items)'.
EXECUTE.

COMPUTE T2_PoNS_CA = SUM.10 (T2_PoNS_1,T2_PoNS_2,T2_PoNS_3R,T2_PoNS_4,T2_PoNS_5,T2_PoNS_6R,T2_PoNS_7,T2_PoNS_8R,T2_PoNS_CA_A1R,T2_PoNS_CA_A2R).
VARIABLE LABELS T2_PoNS_CA 'T2 Perception of Neighbourhood scale (sum), CA specific (10 items as at T1)'.
EXECUTE.

COMPUTE T2_PoNS_SA = SUM.10 (T2_PoNS_1,T2_PoNS_2,T2_PoNS_3R,T2_PoNS_4,T2_PoNS_5,T2_PoNS_6R,T2_PoNS_7,T2_PoNS_8R,T2_PoNS_SA_A1,T2_PoNS_SA_A2).
VARIABLE LABELS T2_PoNS_SA 'T2 Perception of Neighbourhood scale (sum), SA specific (10 items as at T1)'.
EXECUTE.

COMPUTE T2_PoNS_T1a_SA = SUM.13 (T2_PoNS_1,T2_PoNS_2,T2_PoNS_3R,T2_PoNS_4,T2_PoNS_5,T2_PoNS_6R,T2_PoNS_7,T2_PoNS_8R,
                                                                    T2_PoNS_SA_A1,T2_PoNS_SA_A2,T2_PoNS_SA_A3,T2_PoNS_SA_A4,T2_PoNS_SA_A5R).
VARIABLE LABELS T2_PoNS_T1a_SA 'T2 Perception of Neighbourhood scale (sum), SA specific (13 items as at T1a)'.
EXECUTE.


***** Benevolent Childhood Experiences scale.
COMPUTE T2_BCE = SUM.10 (T2_BCE_1R,T2_BCE_2R,T2_BCE_3R,T2_BCE_4R,T2_BCE_5R,T2_BCE_6R,T2_BCE_7R,T2_BCE_8R,T2_BCE_9R,T2_BCE_10R).
VARIABLE LABELS T2_BCE 'T2 Benevolent Childhood Experiences scale (sum)'.
EXECUTE.


***** Sensitivity scale (very short version).
COMPUTE T2_SA_Sensitivity = SUM.6 (T2_SA_SS_1,T2_SA_SS_2,T2_SA_SS_3,T2_SA_SS_4,T2_SA_SS_5,T2_SA_SS_6).
VARIABLE LABELS T2_SA_Sensitivity 'T2 Sensitivity scale (very short version) (sum), only SA'.
EXECUTE.


***** Peer Support scale.
COMPUTE T2_PeerSupp = SUM.4 (T2_PeerSupp_1,T2_PeerSupp_2,T2_PeerSupp_3,T2_PeerSupp_4).
VARIABLE LABELS T2_PeerSupp 'T2 Peer support scale (sum)'.
EXECUTE.


***** Parenting Scale: Only SA.
COMPUTE T2_SA_PCSuper = SUM.3 (T2_SA_PCSuper_1,T2_SA_PCSuper_2,T2_SA_PCSuper_3).
VARIABLE LABELS T2_SA_PCSuper 'T2 Parenting Scale: Parental-caregiver supervision subscale (sum), only SA (T2 specific (3 items), one item was not re-assessed (T1_SA_PCSuper_4))'.
EXECUTE.

COMPUTE T2_SA_PCWarm = SUM.3 (T2_SA_PCWarm_1,T2_SA_PCWarm_2,T2_SA_PCWarm_3).
VARIABLE LABELS T2_SA_PCWarm 'T2 Parenting Scale: Parental-caregiver warmth subscale (sum), only SA'.
EXECUTE.


***** Social media use total. Only CA.
COMPUTE T2_CA_SM = SUM (T2_CA_SM_hours_Facebook, T2_CA_SM_hours_Instagram, T2_CA_SM_hours_Snapchat, T2_CA_SM_hours_Twitter, T2_CA_SM_hours_Other_hours_1, T2_CA_SM_hours_Other_hours_2).
VARIABLE LABELS T2_CA_SM 'T2 Social media use total (sum), only CA'.
EXECUTE.


***** DHEA & Cortisol.
COMPUTE T2_DHEA_cort_comp = T2_DHEA_pg_mg/T2_Cort_pg_mg.
VARIABLE LABELS T2_DHEA_cort_comp 'T2 DHEA/Cortisol ratio'.
EXECUTE.


VARIABLE LEVEL T2_CPTS T2_BDI_II T2_SF_15_total T2_SF_15_physical T2_SF_15_role T2_SF_15_social T2_SF_15_perceptions T2_SF_15_pain T2_SES_total
                            T2_SES_total_CA T2_SES_total_SA T2_SES_affective T2_SES_behavioural T2_SES_behavioural_SA T2_SES_cognitive_CA T2_SES_cognitive 
                            T2_WES_total T2_WES_vigour T2_WES_dedication T2_WES_absorption T2_SA_JS T2_Delinquency T2_CA_SUS T2_CA_RB T2_VbC T2_VbC_SA
                            T2_FAS T2_FAS_SA T2_CA_PSS T2_CYRM28_total T2_CYRM29_total_CA T2_CYRM28_I T2_CYRM28_R T2_CYRM28_C T2_CYRM29_C_CA
                            T2_CYRM12 T2_CA_BCOPE_self_distraction T2_CA_BCOPE_active_coping T2_CA_BCOPE_denial T2_CA_BCOPE_substance_use 
                            T2_CA_BCOPE_emotional_support T2_CA_BCOPE_informational_support T2_CA_BCOPE_behavioral_disengagement T2_CA_BCOPE_venting
                            T2_CA_BCOPE_positive_reframing T2_CA_BCOPE_planning T2_CA_BCOPE_humour T2_CA_BCOPE_acceptance T2_CA_BCOPE_religion
                            T2_CA_BCOPE_self_blame T2_CA_BCOPE_Avoidant T2_CA_BCOPE_Approach T2_PoNS T2_PoNS_CA T2_PoNS_SA T2_PoNS_T1a_SA
                            T2_BCE T2_SA_Sensitivity T2_PeerSupp T2_SA_PCSuper T2_SA_PCWarm T2_CA_SM (SCALE).
EXECUTE.


COMPUTE Lockdown_distance=DATEDIFF(T2_Date,COV_19_Lockdown,"days").
VARIABLE LABELS Lockdown_distance 'Days between participation in 2020 and start of first lockdown (neg = participation before lockdown, pos = after lockdown)'.
EXECUTE.

COMPUTE Schools_closed_distance=DATEDIFF(T2_Date,COV_19_Schools_closed,"days").
VARIABLE LABELS Schools_closed_distance 'Days between participation in 2020 and closure of schools due to Cov-19 (neg = participation before closure, pos = after)'.
EXECUTE.


***** Short Form Health Survey (SF-15) for T1A CA only

RECODE T1a_CA_SF15_2 T1a_CA_SF15_3 T1a_CA_SF15_4 T1a_CA_SF15_5 T1a_CA_SF15_6 T1a_CA_SF15_7 T1a_CA_SF15_9 T1a_CA_SF15_10 (1=0) 
    (2=50) (3=100) INTO T1a_CA_SF15_2_100 T1a_CA_SF15_3_100 T1a_CA_SF15_4_100 T1a_CA_SF15_5_100 T1a_CA_SF15_6_100 
    T1a_CA_SF15_7_100 T1a_CA_SF15_9_100 T1a_CA_SF15_10_100.
EXECUTE.

RECODE T1a_CA_SF15_1R T1a_CA_SF15_12 T1a_CA_SF15_13R T1a_CA_SF15_14R T1a_CA_SF15_15 (1=0) (2=25) (3=50) (4=75) (5=100) 
    INTO T1a_CA_SF15_1R_100 T1a_CA_SF15_12_100 T1a_CA_SF15_13R_100 T1a_CA_SF15_14R_100 T1a_CA_SF15_15_100.
EXECUTE.

RECODE T1a_CA_SF15_8R (1=0) (2=20) (3=40) (4=60) (6=100) (5=80) INTO T1a_CA_SF15_8R_100.
EXECUTE.


COMPUTE  T1a_CA_SF15_14_PHC=(T1a_CA_SF15_2_100 + T1a_CA_SF15_3_100 + T1a_CA_SF15_4_100 + T1a_CA_SF15_5_100 + T1a_CA_SF15_6_100 + 
    T1a_CA_SF15_7_100 + T1a_CA_SF15_9_100 +T1a_CA_SF15_10_100 + T1a_CA_SF15_1R_100 + T1a_CA_SF15_12_100 + T1a_CA_SF15_13R_100 
    + T1a_CA_SF15_14R_100 + T1a_CA_SF15_15_100 + T1a_CA_SF15_8R_100)/14.
EXECUTE.

***** Short Form Health Survey (SF-15) for T2

RECODE T2_SF15_2 T2_SF15_3 T2_SF15_4 T2_SF15_5 T2_SF15_6 T2_SF15_7 T2_SF15_9 T2_SF15_10 (1=0) 
    (2=50) (3=100) INTO T2_SF15_2_100 T2_SF15_3_100 T2_SF15_4_100 T2_SF15_5_100 T2_SF15_6_100 
    T2_SF15_7_100 T2_SF15_9_100 T2_SF15_10_100.
EXECUTE.

RECODE T2_SF15_1R T2_SF15_12 T2_SF15_13R T2_SF15_14R T2_SF15_15 (1=0) (2=25) (3=50) (4=75) (5=100) 
    INTO T2_SF15_1R_100 T2_SF15_12_100 T2_SF15_13R_100 T2_SF15_14R_100 T2_SF15_15_100.
EXECUTE.

RECODE T2_SF15_8R (1=0) (2=20) (3=40) (4=60) (6=100) (5=80) INTO T2_SF15_8R_100.
EXECUTE.


COMPUTE  T2_SF_14_PHC=(T2_SF15_2_100 + T2_SF15_3_100 + T2_SF15_4_100 + T2_SF15_5_100 + T2_SF15_6_100 + 
    T2_SF15_7_100 + T2_SF15_9_100 + T2_SF15_10_100 + T2_SF15_1R_100 + T2_SF15_12_100 + T2_SF15_13R_100 
    + T2_SF15_14R_100 + T2_SF15_15_100 + T2_SF15_8R_100)/14.
EXECUTE.




