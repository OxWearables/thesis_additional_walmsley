# NOTE THIS FUNCTION IS VERY MUCH HARD CODED. 
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
  behav_levs <- unique(rec_dat$Behaviour)
  
  # Add title row-----------------------------------------------------
  rec_dat <- rbind(data.frame("Behaviour" = " ",  "Difference" = NA, "Model" = NA, "HR"= NA, "LowerCI" = NA, "UpperCI" = NA, "label" = "HR (CI)", "n_event" = NA, "n_participant" = NA) , rec_dat)
  
  # Further label admin -------------------------------------------------
  rec_dat$Behaviour <-
    factor(rec_dat$Behaviour, ordered = TRUE, levels = c(behav_levs, " "))
  if ("SB" %in% behav_levs){
    MVPA_diff <- as.double(unique(rec_dat$Difference[rec_dat$Behaviour == "MVPA"]))
    LIPA_diff <- as.double(unique(rec_dat$Difference[rec_dat$Behaviour == "LIPA"]))
    SB_diff <- as.double(unique(rec_dat$Difference[rec_dat$Behaviour == "SB"]))
    sleep_diff <- as.double(unique(rec_dat$Difference[rec_dat$Behaviour == "sleep"]))
      rec_dat$BehaviourDet <- plyr::revalue(
    rec_dat$Behaviour,
    c(
      "MVPA" = paste0("MVPA (extra ", MVPA_diff*24*60, " min/day)"),
      "LIPA" = paste0("LIPA (extra ", LIPA_diff*24," hr/day)"),
      "SB" = paste0("SB (extra ", SB_diff*24," hr/day)"),
      "sleep" = paste0("Sleep (extra ", sleep_diff*24," hr/day)"),
      " " = " "
    )
  )
  }
  if ("sedentary" %in% behav_levs){
    MVPA_diff <- as.double(unique(rec_dat$Difference[rec_dat$Behaviour == "MVPA"]))
    LIPA_diff <- as.double(unique(rec_dat$Difference[rec_dat$Behaviour == "light"]))
    SB_diff <- as.double(unique(rec_dat$Difference[rec_dat$Behaviour == "sedentary"]))
    sleep_diff <- as.double(unique(rec_dat$Difference[rec_dat$Behaviour == "sleep"]))

    rec_dat$BehaviourDet <- plyr::revalue(
      rec_dat$Behaviour,
      c(
        "MVPA" = paste0("MVPA (extra ", MVPA_diff*24*60, " min/day)"),
        "light" = paste0("LIPA (extra ", LIPA_diff*24," hr/day)"),
        "sedentary" = paste0("SB (extra ", SB_diff*24," hr/day)"),
        "sleep" = paste0("Sleep (extra ", sleep_diff*24," hr/day)"),
        " " = " "
      )
    )
  }
  
  # Add n_event to caption if non zero ------------------------------------
  if (!is.na(rec_dat$n_event[nrow(rec_dat)])){
    for ( i in 1:length(mod_levels)){
      this_lev <- mod_levels[i]
      number_events <- formatC(as.numeric(rec_dat$n_event[which(rec_dat$Model == this_lev)][1]), big.mark = ",")
      print(number_events)
      mod_levels[i] <- paste0(this_lev, "\n (", number_events, " events)")
      rec_dat$Model[rec_dat$Model == this_lev] <- mod_levels[i]
  }
  }
  
  # Reorder model
  rec_dat$Model <-
     factor(rec_dat$Model,
            ordered = TRUE,
            mod_levels)
  

  # Return final frame-----------------------------------------------------
  return(rec_dat)
}
