qtile <-
  function(x,
           probs = seq(0, 1, 0.25),
           labels = NULL,
           ordered = FALSE,
           na.rm = TRUE) {
    breaks <- quantile(x = x, probs = probs, na.rm = na.rm)
    out <-
      cut(
        x = x,
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE,
        ordered_result = ordered
      )
    return(out)
  }