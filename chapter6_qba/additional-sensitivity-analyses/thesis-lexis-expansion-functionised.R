
source("../../useful_functions/consistent_theme.R")

prep_record <- function(tp_vec, rot_list){
  rec_dat <- data.frame(matrix(nrow = (length(tp_vec)+1)*length(rot_list[[1]]), ncol = 7))
  colnames(rec_dat) <- c("Behaviour",
                                 "Difference",
                                 "TimePeriod",
                                 "HR",
                                 "LowerCI",
                                 "UpperCI",
                                 "pval_overall_interaction")
  return(rec_dat)
}


get_ilr_diff <- function(cm, diff, comp_name){
  pivvar <- cm[, comp_name]
  pivvarnew <- pivvar + diff
  
  othvarname <- colnames(cm)[colnames(cm) != comp_name]
  othvar <- cm[, othvarname]
  othvarnew <- othvar
  
  for (var in othvarname) {
    othvarnew[, var] <-
    othvar[, var] - diff * othvar[, var] / sum(othvar)
    }
  ilr_old <-
    sqrt(3 / 4) * log(pivvar / (othvar[, 1] * othvar[, 2] * othvar[, 3]) ^
                      (1 / 3))
  ilr_new <-
  sqrt(3 / 4) * log(pivvarnew / (othvarnew[, 1] * othvarnew[, 2] * othvarnew[, 3]) ^
                      (1 / 3))
  ilr_diff <- ilr_new - ilr_old
  return(ilr_diff)
}



produce_tp_plot <- function(tp_vec, rot_list, cm, act_vars_z, df){
 
  rec_dat <- prep_record(tp_vec, rot_list)

  for (i in 1:length(rot_list)) {
    # set up
    comp_labels <- rot_list[[i]]
    comp_name <- names(rot_list)[i]
    
    # set up for later looking at transformation
    if (comp_name == "MVPA") {
      diff <- 1 / (24 * 3)
    } else {
      diff <- 1 / 24
    }
    
    ilr_diff <- get_ilr_diff(cm, diff, comp_name)

    # do transformation
    piv_c <-
      as.data.frame(robCompositions::pivotCoord(act_vars_z[, comp_labels]))
    colnames(piv_c) <- c("piv1", "piv2", "piv3")

    # make new data with these variables
    loc <- cbind(df, piv_c)

    # basic cox model
    cox_mod_simple <- coxph(
      Surv(age_entry, age_exit, CVD_event) ~
        strata(sex) + ethnicity + smoking + alcohol +
        fruit_and_veg_cats + oily_fish +
        red_and_processed_meat_cats + education_cats +
        TDI_quartiles +
        piv1 + piv2 + piv3,
      data = loc
    )
    
    vals <- summary(cox_mod_simple)$conf.int["piv1", c(1, 3, 4)]
    HRs <- vals ^ ilr_diff
    rec_dat[(i - 1) * (length(tp_vec)+1)+ 1,] <- c(comp_name, diff, "overall", HRs, NA)


    # run Lexis expansion---------------------------------------------------------------
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

    # Run Cox models with period split ------------------------------------------------
    # Quickly get different reference time periods by change of reference ---------------
    times <- levels(spl$time_period)
    for (j in 1:length(times)) {
      ref <- times[j]
      spl$time_period <- relevel(spl$time_period, ref = ref)
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
      ci <- summary(cox_mod_by_period)$conf.int[, c(1, 3, 4)]
      
      print(summary(cox_mod_by_period))
      
      HRs <- ci["piv1",] ^ ilr_diff

      tpname <- names(tp_vec)[tp_vec == as.numeric(ref)]
      rec_dat[(i - 1) * (length(tp_vec) + 1) + j + 1,] <-
        c(comp_name,
          diff,
          tpname,
          HRs,
          p_val_int)
    }


  }

  for (var in c("HR", "LowerCI", "UpperCI")) {
    rec_dat[, var] <- as.numeric(rec_dat[, var])
  }

  ## Make forest plot --------------------------------------------------------------

  ### This part is admin for labels etc --------------------------------------------
  rec_dat$label <-
    paste0(
      round_2_dp(rec_dat$HR),
      " (",
      round_2_dp(rec_dat$LowerCI),
      ", ",
      round_2_dp(rec_dat$UpperCI),
      ")"
    )

  rec_dat$Behaviour <-
    factor(rec_dat$Behaviour, ordered = TRUE, levels = rot_list[[1]])
  rec_dat$BehaviourDet <- plyr::revalue(
    rec_dat$Behaviour,
    c(
      "MVPA" = "MVPA (extra 20 min/day)",
      "LIPA" = "LIPA (extra 1 hr/day)",
      "SB" = "SB (extra 1 hr/day)",
      "sleep" = "Sleep (extra 1 hr/day)"
    )
  )

  rec_dat$TimePeriod <-
    factor(rec_dat$TimePeriod,
           ordered = TRUE,
           c(rev(names(tp_vec)), "overall"))
  rec_dat$all_p <- as.factor(rec_dat$TimePeriod == "overall")
  
  my_cols <- c(viridis::viridis(n = length(tp_vec)), "black")

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
    scale_color_manual(values = my_cols, guide = guide_legend(reverse = TRUE)) +
    # scale_shape_manual(values = c(18, 20), guide = "none" ) +

   # CONSISTENT THEME
    consistent_theme

  ## save plot to file----------------------------------------------------------------------------------------------
  svg(
    paste0("plots/thesis-time-periods-", length(tp_vec), ".svg"), # chapter6_qba/additional-sensitivity-analyses
    width = 10,
    height = 6
  )
  print(p)
  dev.off()

  ## Print some details about plot ---------------------------------------------------------------------------------
  tos <- cut(df$time_in_study[df$CVD_event == 1]/365.25, breaks = c(tp_vec, 8), labels = names(tp_vec))
  print(table(tos))

  ## return
  return(list(rec_dat, cox_mod_by_period))
}










