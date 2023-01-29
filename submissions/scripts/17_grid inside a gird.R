library(tidyverse)

n = 55
var_trans <- seq(0,10, length.out = n)
leftover_n <- n^3 - 1000
jitter_trans <- c(rep(0,1000),sample(seq(-003,.003, length.out = 100), leftover_n, replace = TRUE))

list_opts <- list(var_trans,
                  n)

grid_data <- pmap_df(list_opts, ~ tibble(x = seq(0,(.5)*..1, length.out = ..2),
                                         y = x) |> expand.grid())


grid_data |>
  ggplot(aes(x,y))+
  theme_void()+
  theme(plot.background = element_rect(fill = "black"))+
  geom_path(aes(group = x), alpha = .3, color = rev(colorRampPalette(c("#af3918", "#a21152", "#822b75","#612884","#154baf",
                                                                                "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a"))(nrow(grid_data))), position = position_jitter(width = .01, height = .01))+
  geom_path(aes(group = y), alpha =.3, color = rev(colorRampPalette(c("#af3918", "#a21152", "#822b75","#612884","#154baf",
                                                                               "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a"))(nrow(grid_data))), position = position_jitter(width = .01, height = .01))+
  geom_path(aes(group = x), alpha = .03, color = "white", linewidth = .03)+
  geom_path(aes(group = y), alpha = .03, color = "white", linewidth = .03)+
  coord_equal(expand = FALSE)

ggsave("day 17.png",
       device = "png",
       dpi = 300,
       bg = "transparent")