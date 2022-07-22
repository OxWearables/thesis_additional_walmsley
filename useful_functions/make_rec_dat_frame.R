make_rec_dat_frame <- function(nrow){
  rec_dat <- data.frame(matrix(nrow = nrow, ncol = 8))
  colnames(rec_dat) <-
    c("Behaviour",
      "Difference",
      "Model",
      "HR",
      "LowerCI",
      "UpperCI", 
      "n_event", 
      "n_participant")
  return(rec_dat)
}