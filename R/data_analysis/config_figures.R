require(ggplot2)
require(ggsci)

# Sources: 
# https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot
# https://github.com/tidyverse/ggplot2/blob/main/R/theme-defaults.r

# define theme 
theme_ggpr <- function(){ 
  font <- "Arial"  
  # theme_bw(base_size = 14) %+replace%    
  theme_bw(base_size = 12) %+replace%    
    theme(
      legend.background = element_rect(fill = alpha('white', 0.6), color = NA),
      panel.grid.major = element_line(size = 0.3),  
      panel.grid.minor = element_blank(),
      panel.border = element_rect(size = 0.3, color = "grey50", fill = NA),
      axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
      axis.title.y = element_text(color = "grey20", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
      plot.title = element_text(color = "grey20", size = 12, hjust = 0, vjust = 1, margin = margin(t = 6), face = "bold")
    ) 
}
theme_set(theme_ggpr())

message("The file config_figures.R with ggplot2 theme was read.")
