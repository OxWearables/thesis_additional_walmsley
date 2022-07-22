interact_health_status <- function(rot_list, act_vars_z, df){
 
  for (i in 1:length(rot_list)) {
    # set up
    comp_labels <- rot_list[[i]]
    comp_name <- names(rot_list)[i]
    
    # do transformation
    piv_c <-
      as.data.frame(robCompositions::pivotCoord(act_vars_z[, comp_labels]))
    colnames(piv_c) <- c("piv1", "piv2", "piv3")

    # make new data with these variables
    loc <- cbind(df, piv_c)

    # basic cox model with health statusi included
    cox_mod_simple <- coxph(
      Surv(age_entry, age_exit, CVD_event) ~
        strata(sex) + ethnicity + smoking + alcohol +
        fruit_and_veg_cats + oily_fish +
        red_and_processed_meat_cats + education_cats +
        TDI_quartiles +
        piv1 + piv2 + piv3 + bad_health,
      data = loc
    )
    
    # model with interaction
    cox_mod_interact <- coxph(
      Surv(age_entry, age_exit, CVD_event) ~
        strata(sex) + ethnicity + smoking + alcohol +
      fruit_and_veg_cats + oily_fish +
      red_and_processed_meat_cats + education_cats +
     TDI_quartiles +
    piv1*bad_health + piv2*bad_health + piv3*bad_health,
    data = loc
    )
    
    print(anova(cox_mod_interact, cox_mod_simple))
    print(comp_name)
    print(summary(cox_mod_interact))
  }
}



interact_health_status_centred <- function(rot_list, act_vars_z, df){
  
  for (i in 1:length(rot_list)) {
    # set up
    comp_labels <- rot_list[[i]]
    comp_name <- names(rot_list)[i]
    
    # do transformation
    piv_c <-
      as.data.frame(robCompositions::pivotCoord(act_vars_z[, comp_labels]))
    colnames(piv_c) <- c("piv1", "piv2", "piv3")
    
    # mean centre ilr variables
    m_vec <- apply(piv_c, 2, mean)
    print(m_vec)
    print(head(piv_c))
    m_dat <- as.data.frame(matrix(rep(m_vec, times = nrow(piv_c)), ncol = 3, byrow = TRUE))
    piv_c <- piv_c - m_dat
    print(head(piv_c))
    
    # make new data with these variables
    loc <- cbind(df, piv_c)
    
    # basic cox model with health statusi included
    cox_mod_simple <- coxph(
      Surv(age_entry, age_exit, CVD_event) ~
        strata(sex) + ethnicity + smoking + alcohol +
        fruit_and_veg_cats + oily_fish +
       red_and_processed_meat_cats + education_cats +
        TDI_quartiles +
        piv1 + piv2 + piv3 + bad_health,
      data = loc
    )
    
    # model with interaction
    cox_mod_interact <- coxph(
      Surv(age_entry, age_exit, CVD_event) ~
        strata(sex) + ethnicity + smoking + alcohol +
      fruit_and_veg_cats + oily_fish +
      red_and_processed_meat_cats + education_cats +
     TDI_quartiles +
        piv1*bad_health + piv2*bad_health + piv3*bad_health,
      data = loc
    )
    
    print(anova(cox_mod_interact, cox_mod_simple))
    print(comp_name)
    print(summary(cox_mod_interact))
  }
}









