#Diagonal Case Study

library(tidyverse)

n = 150
theta <- seq(0,2*pi, length.out = 1000)
circle <- tibble(x = cos(theta),
                 y = sin(theta))
radi <- seq(1,50, length.out = n)
group_names <- paste0("group", radi)
last_group <- tail(group_names,1)
jitter_trans <- seq(.5,.01, length.out = n)
colors <- colorRampPalette(c("#EB382A","#000000","#EB382A","#000000","#EB382A","#000000","#EB382A"))(n)
text_colors <- colorRampPalette(c("#000000","#ffffff","#000000","#ffffff","#000000","#ffffff","#000000"))(n)
circle_options <- list(radi,
                       group_names,
                       jitter_trans,
                       colors,
                       text_colors)

circles <- pmap_df(circle_options, ~circle |>
                     mutate(x = x * ..1,
                            y = y * ..1,
                            group = ..2) |>
                     mutate(x = if_else(group == last_group, x, jitter(x, amount = ..3)),
                            y = if_else(group == last_group, y, jitter(y, amount = ..3)),
                            fill = ..4,
                            color = ..5))

texture <- tibble(x = seq(-51,51, length.out = 100),
                  y = x) |> expand.grid()

circles |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#E4C8A6"))+
  geom_path(data = texture, linewidth = sample(seq(.01,.9, length.out = 20), nrow(texture), replace = TRUE), aes(group = x), color = "#845244", position = position_jitter(width = .002, height = .003),
            alpha = sample(seq(.01,.7, length.out = 20), nrow(texture), replace = TRUE))+
  geom_path(data = texture, linewidth = sample(seq(.01,.9, length.out = 20), nrow(texture), replace = TRUE), color = "#845244", position = position_jitter(width = .002, height = .003),
            alpha = sample(seq(.01,.7, length.out = 20), nrow(texture), replace = TRUE), aes(group = y))+
  geom_path(linewidth = 15, color = circles$fill)+

  geom_point(data = texture, aes(x,y), inherit.aes = FALSE, size = sample(seq(5,10, length.out = 20), nrow(texture), replace = TRUE), color = "#845244", position = position_jitter(width = .02, height = .03),
             alpha = sample(seq(.01,.08, length.out = 20), nrow(texture), replace = TRUE))+
  geom_path(linewidth = sample(seq(.1, 1.5, length.out = 50), nrow(circles), replace = TRUE), position = position_jitter(width = .05, height = .08),
            color = circles$color)+
  coord_equal(expand = FALSE)
  

ggsave("14 aesemic.png",
       device = "png",
       dpi = 300,
       bg = "transparent")
