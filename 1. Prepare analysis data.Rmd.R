### Load libraries ###
library(tidyverse)

### Load data ###

gm = readRDS("Z:/Emma/Data/GM/gm_data_dp1-4.long.pseudonymised.2025_07_25.rds")


#%% Select analytical sample 

#all YP in year 8 in 2021, in year 9 and did full survey in 2022
#also require gender_identity information at T1

#Gender_Identity:
#1 = Girl (including trans girl); 2 = Boy (including trans boy); 3 = Non-binary; 4 = I describe myself in another way; 5 = Prefer not to say/Not sure

gm_sample1 = gm |> 
  group_by(rand_id) |> 
  filter(!(dp == 1 & (is.na(Gender_Identity) | Gender_Identity == 5))) |> #remove dp1 NA gender/ gender PNS
  filter(!any(dp == 2 & survey_version == "shortened")) |> #remove those who did short survey dp2
  filter(any(dp == 1 & year_group == 8)) |> #year 8 dp1
  ungroup()

# 1730 missing gender identity info at T1, 1303 PNS: 3033 missing gender identity information: remove
# of the year 8s in dp1, 30 did short survey at dp2
#final sample: 17178 YP 
#%% Convert subset data to wide format

gm_wide = gm_sample1 |> 
  select(rand_id, dp, school, Gender_Identity, contains("Emotional_Diff"), 
Sexuality, ethnicity_major_wbri, fsm, sen_orig_recoded, eal,
contains("SRS"), contains("Bullying"),
Disc_Disability, Disc_Gender, Disc_Sexuality, Disc_Race, Disc_Religion,
Caregiving, School_Connection, Attainment, contains("Local_Env"), 
imd_rank, cwi_housing_space_environment) |>
  
  # then do the actual pivot wider
  pivot_wider(

    # which column the data frame is being widened on
    names_from=dp, 
    
    # which columns need repeating for each dp
    values_from=c(
    school, Gender_Identity, contains("Emotional_Diff"), 
Sexuality, ethnicity_major_wbri, fsm, sen_orig_recoded, eal,
contains("SRS"), contains("Bullying"),
Disc_Disability, Disc_Gender, Disc_Sexuality, Disc_Race, Disc_Religion,
Caregiving, School_Connection, Attainment, contains("Local_Env"), 
imd_rank, cwi_housing_space_environment
    ),
    
    # how to rename the repeated columns - e.g., dp1_la, dp1_school etc.
    names_glue = "dp{dp}_{.value}",
    
    # keep columns grouped by dp
    names_vary = "slowest") |> 
  
  #select relevant time variables
  select(dp1_Gender_Identity, contains("dp1_MAMF_Emotional_Diff"), 
dp1_Sexuality, dp1_ethnicity_major_wbri, dp1_fsm, dp1_sen_orig_recoded, dp1_eal,
contains("dp2_SRS"), contains("dp2_Bullying"),
dp2_Disc_Disability, dp2_Disc_Gender, dp2_Disc_Sexuality, dp2_Disc_Race, dp2_Disc_Religion,
dp2_Caregiving, dp2_School_Connection, dp2_Attainment, contains("dp2_Local_Env"), 
dp2_imd_rank, dp2_cwi_housing_space_environment, contains("dp3_MAMF_Emotional_Diff"), 
-dp2_Local_Env_2021_Safe_Area
)


#%% Convert 'non-binary' and 'i describe myself in another way' into one category: define outside of binary

gm_wide_1 = gm_wide |> 
  mutate(Gender_Identity =
         fct_collapse(as.factor(dp1_Gender_Identity),
               "0" = 2, #boys
               "1" = 1, #girls 
               "2" = c(3,4) #outside the binary
               )) |> 
  select(-dp1_Gender_Identity)

#relevel
gm_wide_1$Gender_Identity = fct_relevel(gm_wide_1$Gender_Identity, "0", "1", "2")

#%%