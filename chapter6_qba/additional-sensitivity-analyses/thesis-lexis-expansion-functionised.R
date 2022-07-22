produce_tp_plot <- function(tp_vec, rot_list, cm, act_vars_z, df){
  
  # Make recording frame for time period ------------------------------------------------------
  rec_dat <- make_rec_dat_frame((length(tp_vec)+1)*length(rot_list))
  names(rec_dat)[names(rec_dat) == "Model"] <- "TimePeriod" # Modifications to data frame to make it more like what is needed here
  rec_dat$pval_overall_interaction <- NA
  
  # Iterate over choice of first pivot coordinate ------------------------------------------
  for (i in 1:length(rot_list)) {
    
    # Set up--------------------------------------------------------------------------------
    comp_labels <- rot_list[[i]]
    comp_name <- names(rot_list)[i]
    
    ## Create values to use in transformation
    diff <- get_diff(comp_name)
    ilr_diff <- create_ref_vals(comp_name, cm)

    ## Do transformation
    piv_c <-
      as.data.frame(robCompositions::pivotCoord(act_vars_z[, comp_labels]))
    colnames(piv_c) <- c("piv1", "piv2", "piv3")

    ## Make new data with these variables
    loc <- cbind(df, piv_c)

    # Basic Cox model-----------------------------------------------------------------------
    cox_mod_simple <- coxph(
      Surv(age_entry, age_exit, CVD_event) ~
        strata(sex) + ethnicity + smoking + alcohol +
        fruit_and_veg_cats + oily_fish +
        red_and_processed_meat_cats + education_cats +
        TDI_quartiles +
        piv1 + piv2 + piv3,
      data = loc
    )
    n_participant <- cox_mod_simple$n
    n_event <- cox_mod_simple$nevent
    
    HRs <- calc_HRs(model = cox_mod_simple, row = "piv1", ilr_diff = ilr_diff)
    rec_dat[(i - 1) * (length(tp_vec)+1)+ 1,] <- c(comp_name, diff, "overall", HRs, n_event, n_participant, NA)
    rm(HRs, n_event)


    # Run Lexis expansion--------------------------------------------------------------------
    lex <- Lexis(
      entry = list(age = age_entry,
                   tos = 0),
      exit = list(age = age_exit),
      exit.status = CVD_event,
      data = loc
    )
    spl <- splitLexis(lex, "tos", breaks = c(tp_vec, 8) * 365.25)
    spl$time_period <- factor(timeBand(spl, "tos", "left") / 365.25)
    spl$lex.exage <- spl$age + spl$lex.dur
    
    print(head(spl[, c("eid", "age_entry", "age_exit", "CVD_event", "lex.dur", "age", "lex.Xst", "time_period")]), 15)

    # Run Cox models with period split -----------------------------------------------------
    # Quickly get different reference time periods by change of reference. This is rather than taking single model 
    # and doing manual calculation to get different rows
    times <- levels(spl$time_period)
    for (j in 1:length(times)) {
      ref <- times[j]
      spl$time_period <- relevel(spl$time_period, ref = ref) # Relevel to use this reference time period
      cox_mod_by_period <- coxph(
        Surv(age, lex.exage, lex.Xst) ~
          strata(sex) + ethnicity + smoking + alcohol +
          fruit_and_veg_cats + oily_fish +
          red_and_processed_meat_cats + education_cats +
          TDI_quartiles +
          piv1 * time_period + piv2 * time_period + piv3 * time_period,
        data = spl
      )
      cox_mod_by_period_no_interact <- coxph(
        Surv(age, lex.exage, lex.Xst) ~
          strata(sex) + ethnicity + smoking + alcohol +
          fruit_and_veg_cats + oily_fish +
          red_and_processed_meat_cats + education_cats +
          TDI_quartiles +
          piv1 + piv2 + piv3 +
          time_period,
        data = spl
      )
      p_val_int <- anova(cox_mod_by_period, cox_mod_by_period_no_interact)$`P(>|Chi|)`[2]
      n_event <- nrow(spl[(spl$time_period == ref) & (spl$lex.Xst ==1), ])
      
      
      print(summary(cox_mod_by_period)) # Do some inspection to examine correct
      
      # Recording results
      HRs <- calc_HRs(model = cox_mod_by_period, row = "piv1", ilr_diff = ilr_diff)
      tpname <- names(tp_vec)[tp_vec == as.numeric(ref)]
      rec_dat[(i - 1) * (length(tp_vec) + 1) + j + 1,] <-
        c(comp_name,
          diff,
          tpname,
          HRs,
          n_event, 
          n_participant,
          p_val_int)
      
      rm(ref, HRs, n_event)
    } # This closes iteration over times
  
    rm(loc, spl)

  } # This closes iteration over variable in first pivot coordinate

  for (var in c("HR", "LowerCI", "UpperCI")) {
    rec_dat[, var] <- as.numeric(rec_dat[, var])
  }
  # Print rec_dat with p vals in 
  print(rec_dat)

  ## Make forest plot --------------------------------------------------------------
  ### This part is admin for labels etc --------------------------------------------
  rec_dat$pval_overall_interaction <- NULL
  rec_dat$Model <- rec_dat$TimePeriod # This is just a quick hack to use the function designed elsewhere here
  rec_dat$TimePeriod <- NULL
  rec_dat <- do_plot_admin(rec_dat, mod_levels = c(rev(names(tp_vec)), "overall"))
  rec_dat$TimePeriod <- rec_dat$Model
  rec_dat$all_p <- as.factor(rec_dat$TimePeriod == "overall")
  my_cols <- get_cols(length(tp_vec) + 1)

  ### Make plot----------------------------------------------------------------------
  p <- # DATA AND AESTHETICS
    ggplot(data = rec_dat,
           aes(
             x = HR,
             y = BehaviourDet ,
             colour = TimePeriod,
             group = TimePeriod,
             label = label,
             shape = all_p
           )) +
    # GEOMS
    geom_pointrange(position = position_dodge(0.6), aes(xmin = LowerCI, xmax = UpperCI), size = 1, shape = 18) +
    geom_text(
      size = 4.5,
      aes(x = 1.18, y = BehaviourDet, label = label),
      position = position_dodge(0.6),
      colour = "black",
      fontface = "bold"
    ) +
    geom_vline(xintercept = 1, size = 0.75) +

    # SCALES
    scale_x_continuous(trans = "log10", breaks = c(0.9, 0.95, 1, 1.05)) +
    scale_color_manual(values = my_cols, guide = guide_legend(reverse = TRUE), na.translate = FALSE) +
  
   # CONSISTENT THEME
    consistent_theme

  ## save plot to file----------------------------------------------------------------------------------------------
  svg(
    paste0("plots/thesis-time-periods-", length(tp_vec), ".svg"), # chapter6_qba/additional-sensitivity-analyses
    width = 10,
    height = 7
  )
  print(p)
  dev.off()

  ## Print some details about plot ---------------------------------------------------------------------------------
  tos <- cut(df$time_in_study[df$CVD_event == 1]/365.25, breaks = c(tp_vec, 8), labels = names(tp_vec))
  print(table(tos))

  ## return
  return(list(rec_dat, cox_mod_by_period))
}










