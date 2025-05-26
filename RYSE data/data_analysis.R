# Cross-sectional data analysis of the data from the RYSE study.

# Package
library(haven)

# Import data
RYSE_master_dataset <- read_sav("RYSE_master_dataset_08082022.sav")

# Separation of the dataset in sub dataset (timepoint and country+site)
## Countries : CA or SA
df_CA <- RYSE_master_dataset[RYSE_master_dataset$Country==1,]
df_SA <- RYSE_master_dataset[RYSE_master_dataset$Country==2 & RYSE_master_dataset$Site != 4,] # we exclude Zamdela

## Timepoints : T1, T1A or T2
df_CA_T1A <- df_CA[df_CA$Assessments %in% c(1, 3), ]
df_CA_T2 <- df_CA[df_CA$Assessments %in% c(2, 3), ]
df_SA_T1A <- df_SA[df_SA$Assessments %in% c(1, 3), ]
df_SA_T2 <- df_SA[df_SA$Assessments %in% c(2, 3), ]

# List of interesting variables
## Outcomes
### SF-15 general health CA -> T1, T1A, T2 / SA -> T1, T2
#T1_SF_14_PHC
#T1a_CA_SF15_14_PHC
#T2_SF_14_PHC

### WES Work Engagement SA and CA -> T1, T2
#T1_WES_total
#T2_WES_total

### SES School Engagement SA and CA -> T1, T2 (but not the same number of questions)


## Risk

# Test
plot(df_CA$T1_CPTS,df_CA$T1_WES_total,xlab="CPTS",ylab="Work engagement")
abline(a=63+20*63/80,b=-63/80,col="red") # "Expected"
lm_cpts_wes <- lm(T1_WES_total~1 + T1_CPTS, data = df_CA)
summary(lm_cpts_wes)
abline(a=lm_cpts_wes$coefficients[1],b=lm_cpts_wes$coefficients[2],col="blue") # "Reality"