# LOAD TOOLS===========================================================================================
library("data.table")
library("plyr")
library("gtools")

# NAME RUN
name_of_current_run <- "selection_second_attempt"

# NAME WHERE DATA IS
input_loc <- "../../paperRW2021/"

# READ IN PARTICIPANT DATA=============================================================================================
participant <-
  fread(paste0(input_loc,
    "ukbDataPrep/inputData/participant_new_nc_20220114.csv"),
    stringsAsFactors = FALSE,
    data.table = FALSE,
    check.names = TRUE,
    tz = ""
  ) # The data.table argument ensures read as data frame, check.names makes sure colnames syntactically valid

# MERGE NEW PARTICIPANT DATA AS OBTAINED FOR THESIS


# PERFORM WITHDRAWALS============================================================================================
w <-
  read.csv(paste0(input_loc,"ukbDataPrep/inputData/withdrawals-feb21.csv"),
           header = FALSE)
participant <- participant[!(participant$eid %in% w$V1), ]
all <- participant

# ADD AND FORMAT SOME DATES
all$CVD_date <- as.Date(all$CVD, format = "%F")
all$approxbirth_date <-
  as.Date(paste(all$YearOfBirth, all$MonthOfBirth, "15", sep = "-"),
          "%Y-%B-%d")
all$assess_date <- as.Date(all$DatAttendAssessCent, format = "%F")

# SETUP TABLE TO DESCRIBE EXCLUSIONS===============================================================================================
exclusions <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(exclusions) <-
  c("Exclusion", "Number_excluded", "Number_remaining")
exclusions <-
  rbind(
    exclusions,
    data.frame(
      "Exclusion" = "Starting number",
      "Number_excluded" = NA,
      "Number_remaining" = nrow(all)
    )
  )

# REMOVE PREVALENT CASES===========================================================================================
## In HES data:
all$CVD_prevalent_baseline <- FALSE
all$CVD_prevalent_baseline[all$CVD != ""] <- (all$CVD_date[all$CVD != ""] < all$assess_date[all$CVD != ""])

exc <- all[all$CVD_prevalent_baseline, ]
all <- all[!(all$CVD_prevalent_baseline), ]

exclusions <-
  rbind(
    exclusions,
    data.frame(
      "Exclusion" = "Prevalent HES-recorded CVD",
      "Number_excluded" = nrow(exc),
      "Number_remaining" = nrow(all)
    )
  )

## Self-reported at baseline:
exc <-
  all[((
    all$Vascular.heartProblemDiagnosDoct_0_0 %in% c("Heart attack", "Stroke")
  ) |
    (
      all$Vascular.heartProblemDiagnosDoct_0_1 %in% c("Heart attack", "Stroke")
    ) |
    (
      all$Vascular.heartProblemDiagnosDoct_0_2 %in% c("Heart attack", "Stroke")
    ) |
    (
      all$Vascular.heartProblemDiagnosDoct_0_3 %in% c("Heart attack", "Stroke")
    )
  ), ]

all <-
  all[!((
    all$Vascular.heartProblemDiagnosDoct_0_0 %in% c("Heart attack", "Stroke")
  ) |
    (
      all$Vascular.heartProblemDiagnosDoct_0_1 %in% c("Heart attack", "Stroke")
    ) |
    (
      all$Vascular.heartProblemDiagnosDoct_0_2 %in% c("Heart attack", "Stroke")
    ) |
    (
      all$Vascular.heartProblemDiagnosDoct_0_3 %in% c("Heart attack", "Stroke")
    )
  ), ]
exclusions <-
  rbind(
    exclusions,
    data.frame(
      "Exclusion" = "Prevalent CVD at baseline",
      "Number_excluded" = nrow(exc),
      "Number_remaining" = nrow(all)
    )
  )


# ADD DATE OF BIRTH===========================================================================================================================================
all$baseline_age <-
  difftime(all$assess_date, all$approxbirth_date,
           units = "days")

# ADD COMPOSITE DIETARY VARS==========================================================================================================
# FRUIT AND VEG
all$FreshFruitIntak[all$FreshFruitIntak == -10] <- 0.5
all$CookVegetIntak[all$CookVegetIntak == -10] <- 0.5
all$SaladRawVegetIntak[all$SaladRawVegetIntak == -10] <- 0.5

all$fruit_and_veg <- NA
all$fruit_and_veg[(all$FreshFruitIntak == -1) |
                    (all$CookVegetIntak == -1) |
                    (all$SaladRawVegetIntak == -1)] <- "DNK"

all$fruit_and_veg[all$FreshFruitIntak == -3 |
                    all$CookVegetIntak == -3 |
                    all$SaladRawVegetIntak == -3] <- "PNA"

all$fruit_and_veg[is.na(all$fruit_and_veg)] <-
  all$FreshFruitIntak[is.na(all$fruit_and_veg)] + all$CookVegetIntak[is.na(all$fruit_and_veg)] + all$SaladRawVegetIntak[is.na(all$fruit_and_veg)]

all$fruit_and_veg[all$fruit_and_veg == "DNK" |
                    all$fruit_and_veg == "PNA"] <- NA

all$fruit_and_veg_cats <-
  cut(
    as.numeric(all$fruit_and_veg),
    c(-10,  2.99,   4.99, 7.99, 1000),
    c(
      "Less than 3 servings/day",
      "3-4.9 servings/day",
      "5-7.9 servings/day",
      "8+ servings/day"
    )
  )

# MEAT
all$lamb <-
  plyr::mapvalues(
    all$Lamb.muttonIntak,
    from = c(
      "Never",
      "Less than once a week",
      "Once a week",
      "2-4 times a week",
      "Do not know",
      "5-6 times a week",
      "",
      "Prefer not to answer",
      "Once or more daily"
    ),
    to =  c(0, 0.5, 1, 3, NA, 5.5, NA, NA, 7)
  )
all$pork <-
  plyr::mapvalues(
    all$PorkIntak,
    from = c(
      "Never",
      "Less than once a week",
      "Once a week",
      "2-4 times a week",
      "Do not know",
      "5-6 times a week",
      "",
      "Prefer not to answer",
      "Once or more daily"
    ),
    to =  c(0, 0.5, 1, 3, NA, 5.5, NA, NA, 7)
  )
all$beef <-
  plyr::mapvalues(
    all$BeefIntak,
    from = c(
      "Never",
      "Less than once a week",
      "Once a week",
      "2-4 times a week",
      "Do not know",
      "5-6 times a week",
      "",
      "Prefer not to answer",
      "Once or more daily"
    ),
    to =  c(0, 0.5, 1, 3, NA, 5.5, NA, NA, 7)
  )
all$red_meat <-
  as.numeric(all$lamb) + as.numeric(all$pork) + as.numeric(all$beef)
all$red_meat_cats <-
  cut(
    all$red_meat,
    c(-10, 0.99, 1.99,   3.99, 30),
    c(
      "Less than 1 time/week",
      "1-1.9 times/week",
      "2-3.9 times/week",
      "4+ times/week"
    )
  )

all$processed_meat_numeric <-
  plyr::mapvalues(
    all$ProcessMeatIntak,
    from = c(
      "Never",
      "Less than once a week",
      "Once a week",
      "2-4 times a week",
      "Do not know",
      "5-6 times a week",
      "",
      "Prefer not to answer",
      "Once or more daily"
    ),
    to =  c(0, 0.5, 1, 3, NA, 5.5, NA, NA, 7)
  )

all$red_and_processed_meat <-
  all$red_meat + as.numeric(all$processed_meat_numeric)
all$red_and_processed_meat_cats <-
  cut(
    all$red_and_processed_meat,
    c(-10, 0.99, 2.99,   4.99, 30),
    c(
      "Less than 1 time/week",
      "1-2.9 times/week",
      "3-4.9 times/week",
      "5+ times/week"
    )
  )

# OILY FISH
all$oily_fish <-
  plyr::mapvalues(
    all$OiliFishIntak,
    from = c(
      "Never",
      "Less than once a week",
      "Once a week",
      "2-4 times a week",
      "Do not know",
      "5-6 times a week",
      "",
      "Prefer not to answer",
      "Once or more daily"
    ),
    to =  c(
      "< 1 time/week",
      "< 1 time/week",
      "1 time/week",
      "2-4 times/ week",
      NA,
      "More than 4 times/week",
      NA,
      NA,
      "More than 4 times/week"
    )
  )

# RECODE ETHNICITY ===============================================================================================================================================
all$ethnicity <-
  plyr::mapvalues(
    all$EthnicBackground,
    c(
      "British" ,
      "Any other white background",
      "Irish",
      "White and Asian",
      "Other ethnic group",
      "Caribbean",
      "Chinese",
      "Indian",
      "Pakistani",
      "White and Black African",
      "Any other mixed background",
      "African",
      "White and Black Caribbean",
      "Prefer not to answer",
      "White",
      "Do not know" ,
      "Any other Black background",
      "Any other Asian background",
      "" ,
      "Bangladeshi",
      "Mixed",
      "Asian or Asian British",
      "Black or Black British"
    ),
    c(
      "White" ,
      "White",
      "White",
      "Mixed_and_other",
      "Mixed_and_other",
      "Black",
      "Mixed_and_other",
      "Asian",
      "Asian",
      "Mixed_and_other",
      "Mixed_and_other",
      "Black",
      "Mixed_and_other",
      NA,
      "White",
      NA ,
      "Black",
      "Asian",
      NA ,
      "Asian",
      "Mixed_and_other",
      "Asian",
      "Black"
    )
  )

# RECODE EDUCATION ==========================================================================
all$education_cats <- "empty"
for (i in 0:5) {
  col <- paste0("Qualif_0_", i)
  print(col)
  print(head(all[, col]))
  all$education_cats[all[, col] == "College or University degree"] <-
    "Higher education"
  all$education_cats[(all$education_cats != "Higher education") &
                       (
                         all[, col] %in% c(
                           "A levels/AS levels or equivalent",
                           "NVQ or HND or HNC or equivalent" ,
                           "Other professional qualifications eg: nursing, teaching"
                         )
                       )] <-
    "Further, professional or vocational education"
  all$education_cats[(!(
    all$education_cats %in% c(
      "Higher education",
      "Further, professional or vocational education"
    )
  )) & (all[, col] %in% c(
    "CSEs or equivalent",
    "None of the above" ,
    "O levels/GCSEs or equivalent"
  ))] <- "School leaver"
}
all$education_cats[all$education_cats == "empty"] <- NA


# RECODE SMOKING, ALCOHOL =====================================================================

all$smoking <- all$SmokeStatus
all$smoking[all$smoking == "" |
              all$smoking == "Prefer not to answer"] <- NA

all$alcohol <-
  plyr::mapvalues(
    all$AlcoholIntakFrequenc,
    from = c(
      "Never",
      "Three or four times a week",
      "Daily or almost daily",
      "Once or twice a week",
      "One to three times a month",
      "Special occasions only",
      "Prefer not to answer",
      "",
      "Do not know"
    ),
    to = c(
      "Never",
      "3+ times per week",
      "3+ times per week",
      "< 3 times per week",
      "< 3 times per week",
      "< 3 times per week",
      NA,
      NA,
      NA
    )
  )
# VARIABLES FOR EXCLUSIONS=============================================================================
meds_list <- c("Blood pressure medication",
               "Cholesterol lowering medication",
               "Insulin")
all$meds <-
  all$MedCholesterolBloodPressDiabet_0_0 %in% meds_list |
  all$MedCholesterolBloodPressDiabet_0_1 %in% meds_list |
  all$MedCholesterolBloodPressDiabet_0_2 %in% meds_list |
  all$MedCholesterolBloodPressDiabetTakExogHormon_0_0 %in% meds_list |
  all$MedCholesterolBloodPressDiabetTakExogHormon_0_1 %in% meds_list |
  all$MedCholesterolBloodPressDiabetTakExogHormon_0_2 %in% meds_list |
  all$MedCholesterolBloodPressDiabetTakExogHormon_0_3 %in% meds_list
all$poor_health <- all$OveralHealthRate == "Poor"

# RELABEL OTHER VARIABLES================================================================================
all$BMI <- all$BodyMassIndex.Bmi.
all$sex <- all$Sex

# LIST COVARIATES IN CONVENIENT WAY ================================================================
# List of covariates except sex and bmi - NB THIS MATCHES ANALYSIS SCRIPT
covs <-
  c(
    "ethnicity",
    "smoking",
    "alcohol",
    "fruit_and_veg_cats",
    "red_and_processed_meat_cats",
    "oily_fish",
    "TDI_quartiles",
    "education_cats"
  )

# List of covariates for descriptive tables
covs_cat <-
  c('age_cats', 'sex', covs, 'BMI_cats')

# CATEGORISE SOME CONTINUOUS VARIABLES======================================================================================================
# Add BMI categories
all$BMI_cats <-
  cut(
    all$BMI,
    breaks = c(0, 18.5, 25, 30, 10000),
    labels = c("Underweight", "Normal weight", "Overweight", "Obese")
  )
all$BMI_cats_coarse <-
  cut(
    all$BMI,
    breaks = c(0, 25, 30, 10000),
    labels = c("Normal Weight or Underweight", "Overweight", "Obese")
  )

# Add age categories
all$baseline_age_cats <-
  cut(
    as.numeric(all$baseline_age),
    breaks = 365.25 * c(0, 50, 60, 70, 80),
    labels = c("40-49", "50-59", "60-69", "70-79")
  )


# Quartiles on included participants
all$TDI_quartiles <-
  quantcut(
    as.numeric(all$TownsendDeprIndexRecruit),
    q = 4,
    labels = c(
      "Least deprived",
      "Second least deprived",
      "Second most deprived",
      "Most deprived"
    )
  )
all$TDI_quartiles <- as.factor(all$TDI_quartiles)

# WRITE FILE DESCRIBING EXCLUSIONS =============================================================
write.csv(exclusions,
          paste0("chapter6_qba/selection-analyses/plots/", name_of_current_run, "exclusions.csv"))

# WRITE TO DATA FILES ===================================================================================
saveRDS(
  all,
  paste0(
    "chapter6_qba/selection-analyses/data/",
    name_of_current_run,
    "_ready_to_use.RDS"
  )
)


