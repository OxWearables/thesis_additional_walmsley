make_rec_dat_frame <- function(nrow){
  rec_dat <- data.frame(matrix(nrow = nrow, ncol = 6))
  colnames(rec_dat) <-
    c("Behaviour",
      "Difference",
      "Model",
      "HR",
      "LowerCI",
      "UpperCI")
  return(rec_dat)
}