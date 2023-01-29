library(tidyverse)

background <- "#F7F5EB"

texture <- tibble(x = seq(0,10, length = 100),
                  y = seq(0,15, length = 100)) |>
  expand.grid()

theta <- seq(0,2*pi, length = 100)

main_piece <- tibble(x = (cos(theta) * 4) + 5,
                     y = (sin(theta) * 4) + 7.5,
                     group = "circle",
                     fill = "#B23A28")

main_piece_text_shell <- tibble(x = (cos(theta) * 3.5) + 5,
                     y = (sin(theta) * 3.5) + 7.5)

main_texture <- texture |>
  mutate(x = jitter(x, amount = .4),
         y = jitter(y, amount = .4),
         logic = sp::point.in.polygon(x,y, main_piece_text_shell$x, main_piece_text_shell$y)) |>
  filter(logic == 1)

tri_main <- tibble(x = c(2.5,7.5,5,2.5),
                   y = c(5.5,5.5,10.5,5.5)) 

tri_texture <- texture |>
  mutate(x = jitter(x, amount = .4),
         y = jitter(y, amount = .4),
         logic = sp::point.in.polygon(x,y, tri_main$x, tri_main$y)) |>
  filter(logic == 1)

right_circle <- tibble(x = (cos(theta) * 4) + 10,
                       y = (sin(theta) * 4),
                       group = "circle",
                       fill = "#32498D")

left_circle <- tibble(x = (cos(theta) * 4),
                       y = (sin(theta) * 4 + 15),
                       group = "circle",
                       fill = "#D5BD5D")

left_shell <- tibble(x = (cos(theta) * 3.5),
                     y = (sin(theta) * 3.5 + 15))
  
left_texture <- texture |>
  mutate(x = jitter(x, amount = .4),
         y = jitter(y, amount = .4),
         logic = sp::point.in.polygon(x,y, left_shell$x, left_shell$y)) |>
  filter(logic == 1)

right_shell <- tibble(x = (cos(theta) * 3.5 + 10),
                     y = (sin(theta) * 3.5))

right_texture <- texture |>
  mutate(x = jitter(x, amount = .4),
         y = jitter(y, amount = .4),
         logic = sp::point.in.polygon(x,y, right_shell$x, right_shell$y)) |>
  filter(logic == 1)

radis <- c(.8,.5,.4,.8,.5,.4)
borders <- radis + .1
xs <- c(5,3,1,5,7,9)
ys <- c(rep(2,3), rep(13,3))
colors_circles <- colorRampPalette(c("#32498D", "#D5BD5D"))(6)

circle_opts <- list(radis,
                    borders,
                    xs,
                    ys,
                    colors_circles)

blue_circles <- pmap_df(circle_opts, ~tibble(x = (cos(theta)*..1) + ..3,
                                             y = (sin(theta)*..1) + ..4,
                                             fill = ..5,
                                             group = paste0("group_",..5)))


border_circles <- pmap_df(circle_opts, ~tibble(x = (cos(theta)*..2) + ..3,
                                               y = (sin(theta)*..2) + ..4,
                                               color = "#ffffff",
                                               group = paste0("group_",..5,"circle")))



texture |>
  ggplot(aes(x,y))+
  theme_void()+
  theme(plot.background = element_rect(fill = background))+
  geom_point(color = sample(colorRampPalette(c("#EAE0DA","#B7B78A","#F7F5EB"))(10),nrow(texture), replace = TRUE),
             position = position_jitter(width = .6, height = .5),
             size = sample(seq(25,45, length = nrow(texture))),
             alpha = .05)+
  geom_polygon(data = blue_circles, aes(sample(x), sort(y), group = group), fill = NA, color = "white", linetype = 3, size = .1, alpha = .3)+
  geom_polygon(data = main_piece, aes(group = group), fill = main_piece$fill)+
  geom_point(data = main_texture,
             color = sample(colorRampPalette(c("#B23A28","#5C1F15","#BF5836"))(10),nrow(main_texture), replace = TRUE),
             size = sample(seq(2,15, length = nrow(main_texture))),
             alpha = .01)+
  geom_polygon(data = tri_main, fill = "#000000", color = "#000000", size = 5, linejoin = "mitre")+
  geom_path(data = tri_texture, aes(x,sort(y)), color = "#ffffff", alpha = .7, size = .1)+
  geom_polygon(data = right_circle, aes(group = group), fill = right_circle$fill)+
  geom_polygon(data = left_circle, aes(group = group), fill = left_circle$fill)+
  geom_point(data = left_texture,
             color = sample(colorRampPalette(c("#D5BD5D","#649D7A","#377B70", "#32498D"))(10),nrow(left_texture), replace = TRUE),
             size = sample(seq(2,15, length = nrow(left_texture))),
             alpha = .01)+
  geom_point(data = right_texture,
             color = sample(colorRampPalette(rev(c("#D5BD5D","#649D7A","#377B70", "#32498D")))(10),nrow(right_texture), replace = TRUE),
             size = sample(seq(2,15, length = nrow(right_texture))),
             alpha = .01)+
  geom_polygon(data = blue_circles, aes(group = group), fill = blue_circles$fill)+
  geom_polygon(data = border_circles, aes(group = group), color = border_circles$color, fill = NA)+
  coord_equal(expand = FALSE, xlim = c(0,10), ylim = c(0,15))
  
ggsave("day 27.png",
       device = "png",
       dpi = 300,
       bg = "transparent")