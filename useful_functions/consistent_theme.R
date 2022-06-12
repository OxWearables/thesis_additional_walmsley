consistent_theme <-list(# CANVAS
  coord_cartesian(xlim = c(0.85, 1.1), # This focuses the x-axis on the range of interest
                clip = 'off'), # but this allows stuff outside that range (the HRs) to still show
  # THEMES
  theme_classic() , 
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(size = 0.75),
    axis.text.x = element_text(colour = "black", size = 13, margin = margin(t = 5)),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15)),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(colour = "black", size = 16, face = "bold"),
    axis.title.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold"),
    legend.position = "bottom",
    plot.margin = unit(c(1, 20, 1, 1), "lines"))
)
