calc_HRs <- function(model, row, ilr_diff){
  vals <- summary(model)$conf.int[row, c(1, 3, 4)]
  HRs <- vals ^ ilr_diff
  return(HRs)
}