library(tidyverse)

theta <- seq(0,2*pi, length = 100)

radi <- seq(0,20, length = 100)
n <- 1:length(radi)
color_pals <- c("#000000", "#ffffff")
fill_pals <- colorRampPalette(c("#000000","#070b40","#290740"))(length(radi))

circles <- pmap_df(list(radi,n,fill_pals), ~tibble(x = cos(theta) * .x + 10,
                                   y = sin(theta) * ..1 + 10,
                                   group = paste0("ring_",..2),
                                   color = sample(color_pals, length(theta), replace = TRUE),
                                   fill = ..3))

square <- tibble(x = c(0,1,1,0,0),
                 y = c(0,0,1,1,0),
                 group = "square_")

xtrans <- 0:19
ytrans <- 0:19
multi <- 1:20

squares <- map2_df(ytrans,multi, ~map_df(xtrans, ~square |>
                    mutate(x = x + .x,
                           group = .x)) |>
                    mutate(y = y + .x,
                           group = group + .y )) |>
  mutate(fill = if_else(group %% 2 == 0, "#ffffff", "#000000")) |>
  filter(x == between(x,0,5) & between(x,15,20))


theta <- seq(0,2*pi, length = 1000)


center <- tibble(x = cos(theta) * 5 + 10,
                 y = sin(theta)* 5 + 10,
                 color = "#ffffff",
                 group = "center")



circles |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"))+
  geom_polygon(data = center, fill = "#ffffff")+
  geom_path(color = circles$color,size = .1)+
  coord_equal(xlim = c(0,20),
              ylim = c(0,20),
              expand = FALSE)

ggsave("day 30.png",
       dpi = 300,
       device = "png",
       bg = "transparent")

