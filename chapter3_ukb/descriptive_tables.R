# USEFUL DOCS, ADVICE

# To format 1000s with commas: formatC(1:10 * 100000, format="d", big.mark=",")
# To format decimal places (?): formatC(, format = "f", digits = num_digits)

# To use xtable:
# newobject<-xtable(object)
# print.xtable(newobject, type=”latex”, file=”filename.tex”)

# xtable:
# https://www.r-bloggers.com/2006/02/getting-tables-from-r-output/
#

# Discussion of how to generate pretty tables in R for Latex: https://stackoverflow.com/questions/51514572/make-a-beautiful-report-on-latex-from-r
# directly with a the tableone wrapper for the kable function
# kableone(tableOne, booktabs = TRUE, format = "latex")

# SETUP
# Run name ===================
run_name <- "_20220614"

## Packages ===================
#library(xtable) xtable and table1 cannot be loaded at the same time
library(gridExtra) # https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
library(table1)
library(kableExtra) # this should allow direct output to LaTeX
library(ggalluvial)

## Other setup ================
source("useful_functions/make_sankey_diagram.R")
source("useful_functions/quantile_cut.R")
source("useful_functions/round_2_dp.R")

# READ DATA ===================
df <-
  data.frame(data.table::fread("data/thesis-phenoData-20220124.csv")) 
df2 <- data.frame(data.table::fread("data/data_vig_20220615.csv")) # This adds in 
df <- merge(df, df2[, ], by = "eid")

df_analysis <-
  readRDS(
    "../../paperRW2021/epiAnalysis/inputData/2021-06-17check_messages_ready_to_use.RDS"
  ) # this is the dataset from the epi analysis. need this to isolate relevant set of IDs# DATA CLEANING AND PREP==============
# There's one participant with no data at all
nbef <- nrow(df)
df <- df[df$p31 != "", ]
naft <- nrow(df)
if ((naft - nbef) != -1)
  stop("Something's changed in input data, wrong number of weird excusions.")

# COLNAMES
colnames_sorted <- colnames(df)
fields <-
  list(
    "sex" = 31,
    "year_birth" = 34,
    "month_birth" = 52,
    "date_assess" = 53,
    "cen_assess" = 54,
    "ethnicity" = 21000,
    "sbp_man" = 93,
    "sbp_auto" = 4080,
    "bmi" = 21001,
    "tdi" = 189,
    "qual" = 6138,
    "sr_mpa" = 894,
    "sr_vpa" = 914,
    "job_walk_stand" = 806,
    "job_activity" = 816,
    "shift_work" = 826,
    "morning_evening_person" = 1180,
    "nap_in_day" = 1190,
    "insomnia" = 1200,
    "night_shift" = 3426 ,
    "urban_rural" = 20118,
    "meds_men" = 6177,
    "meds_women" = 6153,
    "cooked_veg" = 1289,
    "raw_veg" = 1299,
    "fresh_fruit" = 1309,
    "dried_fruit" = 1319,
    "handedness" = 1707,
    "alcohol" = 1558,
    "smoking" = 20116,
    "sr_cvd" = 6150,
    "acc_overall_avg" = 90012,
    "date_acc_start" = 90010,
    "date_acc_end" = 90011,
    "crp" = 30710,
    "hdl" = 30760,
    "triglycerides" = 30870,
    "ldl" = 30780,
    "HbA1c" = 30750,
    "sr_health" = 2178, 
    "employed" = 6142, 
    "usual_walk_pace" = 924,
    "sr_any_mpa_days" = 884, 
    "sr_any_vpa_days"= 904 
  )

for (i in 1:length(fields)) {
  v_name <- names(fields)[i]
  v_code <- paste0("p", fields[i])
  colnames_sorted <- gsub(v_code, v_name, colnames_sorted)
}

colnames(df) <- colnames_sorted

df <- df[, colnames(df)[!grepl("p[0-9]|i[2-3]", colnames(df))]]

# AGE
df$dob_approx <-
  as.Date(paste(df$year_birth, df$month_birth, "15", sep = "-"),
          "%Y-%B-%d")
df$age_entry_main_days <- as.Date(df$date_assess_i0) - df$dob_approx
df$age_entry_main_years <-
  as.double(df$age_entry_main_days) / 365.25
df$age_entry_acc_days <- as.Date(df$date_acc_end) - df$dob_approx
df$age_entry_acc_years <- as.double(df$age_entry_acc_days) / 365.25
df$age_cats <-
  cut(df$age_entry_main_years, c(40, 50, 55, 60, 65, 70, 75, 80))

# SEX
df$Sex <- df$sex

# ASSESS CENTRE
for (i in 0:1) {
  df[, paste0("Assessment_Centre_i", i)] <-
    factor(df[, paste0("cen_assess_i", i)])
}

# ETHNICITY
for (i in 0:1) {
  df[, paste0("Ethnicity_i", i)] <-
    plyr::revalue(
      df[, paste0("ethnicity_i", i)],
      c(
        "British" = "White",
        "Any other white background" = "White",
        "Irish" = "White",
        "White and Asian" = "Mixed/Other",
        "Caribbean" =  "Black or Black British",
        "Chinese"   = "Mixed/Other",
        "Pakistani"  = "Asian or Asian British",
        "White and Black African" = "Mixed/Other",
        "Other ethnic group"  = "Mixed/Other",
        "African"    =  "Black or Black British",
        "Any other mixed background" = "Mixed/Other",
        "White and Black Caribbean" = "Mixed/Other",
        "Prefer not to answer" = NA,
        "Indian"  = "Asian or Asian British",
        "White" = "White",
        "Do not know" = NA,
        "Any other Black background" = "Black or Black British",
        "Any other Asian background"  = "Asian or Asian British",
        "Bangladeshi"  = "Asian or Asian British",
        "Mixed"  = "Mixed/Other",
        "Asian or Asian British"  = "Asian or Asian British",
        "Black or Black British"  = "Black or Black British"
      )
    )
  df[, paste0("Ethnicity_i", i)] <-
    factor(
      df[, paste0("Ethnicity_i", i)],
      levels = c(
        "Asian or Asian British",
        "Black or Black British",
        "Mixed/Other",
        "White"
      )
    )
  
}

# SMOKING
for (i in 0:1) {
  df[, paste0("Smoking_status_i", i)] <-
    plyr::revalue(df[, paste0("smoking_i", i)], replace = c("Prefer not to answer" = NA))
  df[, paste0("Smoking_status_i", i)] <-
    factor(df[, paste0("Smoking_status_i", i)],
           ordered = TRUE,
           levels = c("Never", "Previous", "Current"))
}

# ALCOHOL
for (i in 0:1) {
  df[, paste0("Alcohol_consumption_frequency_i", i)] <-
    plyr::revalue(
      df[, paste0("alcohol_i", i)],
      replace = c(
        "Prefer not to answer" = NA,
        "Three or four times a week" = "3 or more times/week",
        "Special occasions only" = "Less than 3 times/week",
        "One to three times a month" = "Less than 3 times/week",
        "Daily or almost daily" = "3 or more times/week",
        "Once or twice a week" = "Less than 3 times/week"
      )
    )
  df[, paste0("Alcohol_consumption_frequency_i", i)] <-
    factor(
      df[, paste0("Alcohol_consumption_frequency_i", i)] ,
      ordered = TRUE,
      levels = c("Never", "Less than 3 times/week", "3 or more times/week")
    )
}

# EDUCATION
for (i in 0:1) {
  df[, paste0("Qualifications_i", i)] <- NA
  df[, paste0("Qualifications_i", i)][grepl("None of the above|O levels/GCSEs or equivalent|CSEs or equivalent",
                                            df[, paste0("qual_i", i)])] <-
    "School leaver"
  df[, paste0("Qualifications_i", i)][grepl(
    "A levels/AS levels or equivalent|NVQ or HND or HNC or equivalent|Other professional qualifications eg: nursing, teaching",
    df[, paste0("qual_i", i)]
  )] <- "Further education"
  df[, paste0("Qualifications_i", i)][grepl("College or University degree", df[, paste0("qual_i", i)])] <-
    "Higher education"
  df[, paste0("Qualifications_i", i)] <-
    factor(
      df[, paste0("Qualifications_i", i)],
      ordered = TRUE,
      levels = c("School leaver", "Further education", "Higher education")
    )
}

# TDI
df$Quartile_of_Townsend_Deprivation_Index <- qtile(df$tdi)
df$Townsend_Deprivation_Index <- df$tdi

# FRUIT AND VEG
for (j in c(0, 1)) {
  for (i in c("fresh_fruit", "cooked_veg", "raw_veg")) {
    col <-  df[, paste0(i, "_i", j)]
    col <- plyr::mapvalues(col, "", NA)
    df[, paste0(i, "_n_i", j)] <- plyr::revalue(col,
                                                replace = c(
                                                  "Less than one" = 0.5,
                                                  "Do not know" = NA,
                                                  "Prefer not to answer" = NA
                                                  
                                                ))
    df[, paste0(i, "_n_i", j)] <-
      as.double(df[, paste0(i, "_n_i", j)])
  }
  
  df[, paste0("fruit_and_veg_i", j)] <-
    df[, paste0("fresh_fruit_n_i", j)] + df[, paste0("cooked_veg_n_i", j)] + df[, paste0("raw_veg_n_i", j)]
  
  df[, paste0("Daily_fruit_and_vegetable_consumption_i", j)] <-
    cut(
      df[, paste0("fruit_and_veg_i", j)],
      c(0,  2.99,   4.99, 7.99, 1000),
      include.lowest = TRUE,
      c(
        "Less than 3 servings/day",
        "3-4.9 servings/day",
        "5-7.9 servings/day",
        "8+ servings/day"
      )
    )
  df[, paste0("Daily_fruit_and_vegetable_consumption_i", j)] <-
    factor(
      df[, paste0("Daily_fruit_and_vegetable_consumption_i", j)],
      ordered = TRUE,
      levels =   c(
        "Less than 3 servings/day",
        "3-4.9 servings/day",
        "5-7.9 servings/day",
        "8+ servings/day"
      )
    )
}
# Systolic Blood Pressure
df$Systolic_blood_pressure <-
  apply(df[, c("sbp_auto_i0_a0", "sbp_auto_i0_a1")], 1, mean, na.rm = TRUE)
df$Systolic_blood_pressure[is.na(df$Systolic_blood_pressure)] <-
  apply(df[is.na(df$Systolic_blood_pressure), c("sbp_man_i0_a0", "sbp_man_i0_a1")], 1, mean, na.rm = TRUE)

# Biomarkers
for (i in 0:1) {
  df[, paste0("BMI_category_i", i)] <-
    cut(
      df[, paste0("bmi_i", i)],
      c(0, 18.5, 25, 30, 1000),
      labels = c("Underweight", "Normal weight", "Overweight", "Obese")
    )
  df[, paste0("BMI_category_i", i)] <-
    factor(
      df[, paste0("BMI_category_i", i)],
      ordered = TRUE,
      levels = c("Underweight", "Normal weight", "Overweight", "Obese")
    )
}
df$HbA1c <- df$HbA1c_i0
df$LDL_cholesterol <- df$ldl_i0
df$HDL_cholesterol <- df$hdl_i0
df$Triglycerides <- df$triglycerides_i0
df$C_Reactive_Protein <- df$crp_i0

# Meds for prior disease
df$`Medications_for_diabetes,_cholesterol_or_blood_pressure` <-
  grepl('Cholesterol lowering medication|Blood pressure medication|Insulin',
        df$meds_men_i0) |
  grepl(
    'Cholesterol lowering medication|Blood pressure medication|Insulin',
    df$meds_women_i0
  )
# SR health
df$Self_rated_health <-
  factor(
    df$sr_health_i0,
    ordered = TRUE,
    levels = c("Poor", "Fair", "Good", "Excellent")
  )

# SR activity
df$Self_reported_MPA <- df$sr_mpa_i0
df$Self_reported_VPA <- df$sr_vpa_i0
df$Job_involves_activity <- df$job_activity_i0
df$Job_involves_walking_standing <- df$job_walk_stand_i0
df$Self_reported_MPA[df$sr_any_mpa_days_i0 == "0"] <- 0
df$Self_reported_VPA[df$sr_any_vpa_days_i0 == "0"] <- 0
df$Self_reported_usual_walking_pace <- df$usual_walk_pace_i0

# Baseline vals
df$BMI <- df$bmi_i0
df$BMI_category <- df$BMI_category_i0
df$Alcohol_consumption_frequency <-
  df$Alcohol_consumption_frequency_i0
df$Smoking_status <- df$Smoking_status_i0
df$Daily_fruit_and_vegetable_consumption <- df$Daily_fruit_and_vegetable_consumption_i0
df$Ethnicity <- df$Ethnicity_i0
df$Qualifications <- df$Qualifications_i0
df$Age_at_recruitment <- df$age_entry_main_years
df$Age_at_accelerometer_wear <- df$age_entry_acc_years


# Units
units(df$Age_at_recruitment) <- "years"
units(df$Age_at_accelerometer_wear) <- "years"
units(df$HbA1c) <- "mmol/mol"
units(df$C_Reactive_Protein) <- "mg/L"
units(df$BMI) <- "kg/m^2"
units(df$LDL_cholesterol) <- "mmol/L"
units(df$HDL_cholesterol) <- "mmol/L"
units(df$Triglycerides) <- "mmol/L"

# Column labels
for (var in colnames(df)) {
  print(var)
  label(df[, var]) <- gsub("_", " ", var)
}


# SET UP =====================
# Vars to consider in tables
vars_dem <- c("Sex", "Ethnicity")
vars_behav <-
  c(
    "Smoking_status",
    "Alcohol_consumption_frequency",
    "Daily_fruit_and_vegetable_consumption"
  )
vars_sr_act <-
  c(
    "Self_reported_MPA",
    "Self_reported_VPA",
    "Job_involves_walking_standing",
    "Job_involves_activity", 
    "Self_reported_usual_walking_pace"
  )
vars_soc <-
  c("Qualifications", "Townsend_Deprivation_Index") # ADD THINGS TO DO WITH EMPLOYMENT
vars_biom <-
  c(
    "BMI",
    "Systolic_blood_pressure",
    "HbA1c",
    "C_Reactive_Protein",
    "LDL_cholesterol",
    "HDL_cholesterol",
    "Triglycerides"
  )
vars_health <-
  c("Self_rated_health",
    "Medications_for_diabetes,_cholesterol_or_blood_pressure") #, "Prevalent_cardiovascular_disease_at_baseline")

vars_table1 <-
  c("Age_at_recruitment",
    vars_dem,
    vars_behav,
    vars_soc,
    vars_biom,
    vars_health) # @@@ XXX vars_sr_act
vars_table2 <-
  c("Age_at_accelerometer_wear",
    vars_dem,
    vars_behav,
    vars_soc,
    vars_biom,
    vars_health) # @@@ XXX vars_sr_act

# RECODE MISSING ============
for (var in c(vars_table1, vars_sr_act)) {
  print(var)
  df[, var][(df[, var] == "") |
              (df[, var] == "Prefer not to answer") |
              (df[, var] == "Do not know")] <- NA
}

# These vars also need helpful names
# what to do w biomarkers

# DATA QUALITY ================
## Missingness ================
# First recode data as missing where relevant
# Which vars to consider?
# Consider different types of missingness? - Don't think so.
# Consider proportion of sample missing?

## Stability ==================
df_rep1 <- df[(df$cen_assess_i1 != ""), ]
vars_stab <-
  c(vars_dem[vars_dem != "Sex"],
    vars_soc[vars_soc != "Townsend_Deprivation_Index"],
    "BMI_category",
    vars_behav)
df_kap <-
  data.frame(
    "Variable" = vars_stab,
    "n_with_repeat" = NA,
    "Kappa" = NA,
    "FleissCohen" = NA
  )
for (i in 1:length(vars_stab)) {
  # setup
  var <- vars_stab[i]
  print(var)
  cols <- c(paste0(var, "_i0"),  paste0(var, "_i1"))
  print(cols)
  
  print(levels(df_rep1[, cols[1]]))
  
  # sankey
  p <-
    make_sankey(df_rep1,
                var,
                reps = list("Baseline" = "_i0", "First repeat assessment" = "_i1"))
  assign(paste0("p", i), p)
  
  
  # kappa
  cm <-
    caret::confusionMatrix(data = df_rep1[, cols[2]], reference = df_rep1[, cols[1]])
  print(cm$table)
  
  n <- sum(cm$table)
  print(n)
  
  k <- vcd::Kappa(cm$table, weights = "Fleiss-Cohen")
  k2 <- as.data.frame(confint(k))
  # print(k2)
  df_kap[df_kap$Variable == var, "n_with_repeat"] <- n
  df_kap[df_kap$Variable == var, "Kappa"] <-
    paste0(round_2_dp(k$Unweighted["value"]),
           " (",
           round_2_dp(k2["Unweighted", "lwr"]),
           ", ",
           round_2_dp(k2["Unweighted", "upr"]),
           ")")
  
  if (is.ordered(df_rep1[, cols[1]])) {
    df_kap[df_kap$Variable == var, "FleissCohen"] <-
      paste0(round_2_dp(k$Weighted["value"]),
             " (",
             round_2_dp(k2["Weighted", "lwr"]),
             ", ",
             round_2_dp(k2["Weighted", "upr"]),
             ")")
  }
  else {
    df_kap[df_kap$Variable == var, "FleissCohen"] <- "NA"
  }
  
}
rm(df_rep1)

fv_tab_full <- cm$table
fv_close <- cm$table
fv_far <- cm$table
for (i in 1:4) {
  for (j in 1:4) {
    if (abs(i - j) < 2) {
      fv_far[i, j] <- NA
    }
    
    if (abs(i - j) != 1) {
      fv_close[i, j] <- NA
    }
  }
}

k <- vcd::Kappa(fv_tab_full, weights = "Fleiss-Cohen")
k2 <- as.data.frame(confint(k))
print(k)
# print(k2)

sum(fv_far, na.rm = TRUE) / sum(fv_tab_full)
sum(fv_close, na.rm = TRUE) / sum(fv_tab_full)
sum(diag(fv_tab_full)) / sum(fv_tab_full)

png(
  paste0("chapter3_ukb/outputs/stability_", run_name, ".png"),
  width = 1200,
  height = 600
)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

write.csv(df_kap,
          paste0("chapter3_ukb/outputs/kappa_", run_name, ".csv"))

# OVERALL VS ACC SAMPLE =======
df$in_acc_sample <-
  df$eid %in% df_analysis$eid # here I will compare with the people in the actual acc sample
# i.e. after the relevant exclusions
# still illustrates points + more correct
df$in_acc_sample_factor <-
  plyr::mapvalues(df$in_acc_sample,
                  c(TRUE, FALSE),
                  c("Analytic cohort for accelerometry analyses", NA))
df$in_acc_sample_factor <- as.factor(df$in_acc_sample_factor)

# Variables for table
for (j in 1:2) {
  vs <- list(vars_table1, vars_table2)[[j]]
  vars_table_labels <- as.list(rep(NA, times = length(vs)))
  for (i in 1:length(vs)) {
    var <- vs[i]
    label <- label(df[, var])
    unit <- units(df[, var])
    if (!is.null(unit)) {
      desc <- paste0(label, " (", unit, ")")
    }
    else {
      desc <- label
    }
    vars_table_labels[[i]] <- desc
  }
  names(vars_table_labels) <- vs
  assign(paste0("vars_table_labels", j), vars_table_labels)
}

# CATEGORICAL RENDERER
my_render_cat <- function(x) {
  c("", sapply(stats.default(x), function(y)
    with(y,
         paste0(
           formatC(FREQ, big.mark = ","),
           " (",
           format(round(PCT, digits = 0), nsmall = 0),
           "%)"
         ))))
}

# CATEGORICAL RENDERER
my_render_cont <- function(x){
  with(
    stats.apply.rounding(stats.default(x)),
    c(
      "",
      `Mean (SD)` = sprintf("%s (%s)", MEAN, SD),
      `Median [Q1, Q3]` = sprintf("%s [%s, %s]",
                                    MEDIAN, Q1, Q3)
    )
  )
}

# TABLE
tab_all_v_acc <-
  table1(
    c(
      list("Overall UK Biobank cohort" = df),
      split(df, df$in_acc_sample_factor)
    ),
    labels = list(variables = vars_table_labels1),
    render.categorical = my_render_cat, 
    render.cont = my_render_cont
  )
t <-
  t1kable(
    tab_all_v_acc,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE
  )
write(t,
      paste0("chapter3_ukb/outputs/tab_all_v_acc", run_name, ".tex"))

# OTHER CHARACTERISTICS BY ACC CATEGORY ====
df_acc_sample <- df[which(df$in_acc_sample),]
df_acc_sample$acc_quartile_brief <-
  qtile(df_acc_sample[["acc_overall_avg"]],
        probs = seq(0, 1, by = 0.25),
        labels = paste0("Q", 1:4)) # add in acc var
df_acc_sample$acc_quartile <-
  qtile(df_acc_sample[["acc_overall_avg"]])# add in acc var

df_acc_sample$acc_quartile <-
  plyr::mapvalues(
    df_acc_sample$acc_quartile,
    from = levels(df_acc_sample$acc_quartile),
    to =
      paste0("Quartile ", 1:4, ": ", levels(df_acc_sample$acc_quartile), " mg")
  )


tab_acc_quart <-
  table1(
    c(
      list("Accelerometry cohort" = df_acc_sample),
      split(df_acc_sample, df_acc_sample$acc_quartile)
    ),
    labels = list(variables = vars_table_labels2),
    render.categorical = my_render_cat
  )
t2 <-
  t1kable(
    tab_acc_quart,
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    label = "tab:cohort_by_acc",
    caption.short = "Participant characteristics by overall activity."
  )
write(t2,
      paste0("chapter3_ukb/outputs/tab_acc", run_name, ".tex"))

# WRITE DATASET 
saveRDS(df, "chapter3_ukb/outputs/processed_data_for_sel_bias.RDS")
