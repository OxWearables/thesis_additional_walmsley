get_cols <- function(n){
  if (n == 2){
    return(c("darkred", "black"))
  } 
  if (n == 3){
    return(c(viridis::viridis(n = 3)[2:1], "black"))
  }
  if (n == 4){
    return(c(viridis::viridis(n = 3)[3:1], "black"))
  }
  else{
    return(c(viridis::viridis(n = (n-1))[(n-1):1], "black"))
  }
} 