### Load libraries ###
library(tidyverse)
library(quest)

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
contains("SRS"), contains("Bullying"), Loneliness,
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
contains("SRS"), contains("Bullying"), Loneliness,
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
contains("dp2_SRS"), contains("dp2_Bullying"), dp2_Loneliness,
dp2_Disc_Disability, dp2_Disc_Gender, dp2_Disc_Sexuality, dp2_Disc_Race, dp2_Disc_Religion,
dp2_Caregiving, dp2_School_Connection, dp2_Attainment, contains("dp2_Local_Env"), 
dp2_imd_rank, dp2_cwi_housing_space_environment, contains("dp3_MAMF_Emotional_Diff"), 
-dp2_Local_Env_2021_Safe_Area, -dp2_Local_Env_2022_Safe_Area
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

#%% Manipulating variables

gm_wide_2 = gm_wide_1 |> 
  # IMD as a percentage
  mutate(dp2_imd_pct = dp2_imd_rank / 32844,     # converting rank to %
         dp2_imd_pct_inv = 1 - dp2_imd_pct,    # get reciprocal so higher = more deprived
         dp2_imd_pct_inv100 = dp2_imd_pct_inv * 100) |> # rescale from 0-100%
  #binary bullying variables
  mutate(across(starts_with("dp2_Bullying"), #create  bullying binary variable
                  ~case_when(.x == 3 | .x == 4 ~ 1, #quite a lot/ a lot
                            .x == 1 | .x == 2 ~ 0, #not at all/not much
                            is.na(.x) ~NA),
                   .names = "{.col}_binary")) |> 
  #reverse code local environment variables
  #so that higher score is more positive response
    mutate(across(starts_with("dp2_Local_Env_"),   
                ~ 5 - ., .names = "{.col}_rev")) |> 
  #convert don't know on safe area to NA
  mutate(dp2_Local_Env_2022_Safe_Area_recoded_rev =  
     if_else(dp2_Local_Env_2022_Safe_Area_recoded_rev == 2, 
      NA_real_,
      dp2_Local_Env_2022_Safe_Area_recoded_rev)) |> 
  #reverse score loneliness so that 4 = always and never = 0
  mutate(dp2_Loneliness_rev = 5 - dp2_Loneliness) |> 
  #reverse score discrimination variables
  mutate(across(starts_with("dp2_Disc_"),   
                ~ 5 - ., .names = "{.col}_rev")) |>
  #create total score across discrmination items (but not gender_disc)
  mutate(dp2_other_discrimination = rowSums_if(
      across(c(
        dp2_Disc_Disability_rev,
        dp2_Disc_Race_rev,
        dp2_Disc_Religion_rev,
        dp2_Disc_Sexuality_rev
      )),
      ov.min = 1,
      prop = FALSE,
      inclusive = TRUE)) |> 
  #score school belonging so that 0 = not at all and 4 = alot
  mutate(dp2_school_belonging = dp2_School_Connection - 1) |> 
  #recode unclassified ethnicity to NA/ White other to AOEG/ Chinese to Asian
  mutate(dp1_ethnicity = case_when(
    dp1_ethnicity_major_wbri == "Unclassified" ~NA_character_, 
    dp1_ethnicity_major_wbri == "Chinese" ~ "Asian",
    dp1_ethnicity_major_wbri == "White Other" ~ "Any Other Ethnic Group", 
    TRUE ~ dp1_ethnicity_major_wbri
  )) |> 
  #combine sen support and EHC plan
   mutate(dp1_sen =
         fct_collapse(as.factor(dp1_sen_orig_recoded),
               "0" = "No SEN", 
               "1" = c("EHC plan", "SEN Support", "SEN support")
               )) |> 
  #binary sexuality variable
  mutate(dp1_sexual_orientation = 
    fct_collapse(as.factor(dp1_Sexuality), 
  "0" = 3, #heterosexual
  "1" = c("1", "2", "4", "5")))  |> #not heterosexual
  #create total baseline emotional difficulties for those with 2/3 items present (i.e., 7 items)
   mutate(dp1_emotional_difficulties_mean = rowMeans_if(
      across(c(
      "dp1_MAMF_Emotional_Diff_1", "dp1_MAMF_Emotional_Diff_2",
      "dp1_MAMF_Emotional_Diff_3", "dp1_MAMF_Emotional_Diff_4", 
      "dp1_MAMF_Emotional_Diff_5", "dp1_MAMF_Emotional_Diff_6", 
      "dp1_MAMF_Emotional_Diff_7", "dp1_MAMF_Emotional_Diff_8", 
      "dp1_MAMF_Emotional_Diff_9", "dp1_MAMF_Emotional_Diff_10"
      )),
      ov.min = 7,
      prop = FALSE,
      inclusive = TRUE)) |>
  mutate(dp1_emotional_difficulties = round(dp1_emotional_difficulties_mean*10, 2)) |> 
   #create total outcome (dp3) emotional difficulties for those with 2/3 items present (i.e., 7 items)
   mutate(dp3_emotional_difficulties_mean = rowMeans_if(
      across(c(
      "dp3_MAMF_Emotional_Diff_1", "dp3_MAMF_Emotional_Diff_2",
      "dp3_MAMF_Emotional_Diff_3", "dp3_MAMF_Emotional_Diff_4", 
      "dp3_MAMF_Emotional_Diff_5", "dp3_MAMF_Emotional_Diff_6", 
      "dp3_MAMF_Emotional_Diff_7", "dp3_MAMF_Emotional_Diff_8", 
      "dp3_MAMF_Emotional_Diff_9", "dp3_MAMF_Emotional_Diff_10"
      )),
      ov.min = 7,
      prop = FALSE,
      inclusive = TRUE)) |>
  mutate(dp3_emotional_difficulties = round(dp3_emotional_difficulties_mean*10, 2))
  
#relevel SEN 
gm_wide_2$dp1_sen= fct_relevel(gm_wide_2$dp1_sen, "0", "1")
#relevel sexuality
gm_wide_2$dp1_sexual_orientation= fct_relevel(gm_wide_2$dp1_sexual_orientation, "0", "1")


#%% Select relevant analysis variables to create final analysis dataset