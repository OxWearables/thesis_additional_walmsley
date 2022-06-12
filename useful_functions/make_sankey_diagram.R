library(ggalluvial)
make_sankey <- function(data, var, reps) {
  if (length(reps) != 2)
    stop("Wrong number of reps")
  vars <- paste0(var, reps)
  for (v in vars) {
    data[, v] <- as.factor(data[, v])
  }
  v1 <- vars[1]
  v2 <- vars[2]
  
  name1 <- names(reps)[1]
  name2 <- names(reps)[2]
  
  data_mini <- data[!is.na(data[[v1]]) & (!is.na(data[[v2]])),]
  
  data_plot <- dplyr::count(data_mini, .data[[v1]], .data[[v2]])
  ggplot(data_plot,
         aes(y = n, axis1 = .data[[v1]], axis2 = .data[[v2]])) +
    geom_alluvium(width = 1 / 12, aes(fill = .data[[v1]])) +
    #geom_stratum(width = 1 / 12) +
    geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
   #geom_text(stat = "stratum", aes(label = scales::percent(after_stat(prop), accuracy = .1))) + 
    scale_x_discrete(limits = c({{name1}}, {{name2}})) +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    ggtitle(gsub("_", " ", var)) +
    theme_classic() +
    theme(
      line = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(), 
      legend.title = element_blank())
}

# Testing 

# col1 <- c("a", "a", "a", "a", "b", "b", "b", "b")
# col2 <- c("c", "d","c", "d", "c", "d", "d", "d")
# df <- data.frame("coll1" = col1, "coll2" = col2)
# make_sankey(df, var = "coll", reps = list("Baseline" = 1, "Repeat" = 2))
# 
