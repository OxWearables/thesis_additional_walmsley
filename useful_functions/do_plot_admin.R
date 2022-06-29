do_plot_admin <- function(rec_dat, mod_levels){
  # Label admin-----------------------------------------------------
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
  
  rec_dat$Model <-
    factor(rec_dat$Model,
           ordered = TRUE,
           mod_levels)
  
  
  # Add title row-----------------------------------------------------
  rec_dat <- rbind(data.frame("Behaviour" = " ",  "Model" = NA, "HR"= NA, "LowerCI" = NA, "UpperCI" = NA, "label" = "HR (CI)", "BehaviourDet" = " ") , rec_dat)
  
  # Return final frame-----------------------------------------------------
  return(rec_dat)
}
