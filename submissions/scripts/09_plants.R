library(tidyverse)

n = 150
grass_height = sample(seq(3,6.25, length.out = 50), n, replace = TRUE)
grass_level = 1
waviness = sample(seq(5,50, length.out = 25), n, replace = TRUE)
direction = c("left", "right")
grass_color = c("#A3CB25", "#CEE633", "#88BB1D", "#26610F", "#428115","#0C3607")
border_color = map_chr(grass_color, ~colorRampPalette(c(.x, "#0C3607"))(10)[7])
sky_color = c("#003D59", "#167070", "#44857D", "#2BA8D4")
x_trans <- sample(seq(1,9, length.out = 50), n, replace = TRUE)


list_opts <- list(grass_height,
                  grass_level,
                  waviness,
                  x_trans,
                  colorRampPalette(grass_color)(n),
                  colorRampPalette(border_color)(n),
                  1:n)

theta <- seq(0,2*pi, length.out = 100)

circle <- tibble(x = (cos(theta)*4) + 5,
                 y = (sin(theta)*4) + 5)

grass <- pmap_df(list_opts, ~tibble(y = c(seq(..2,..1,length.out = 1000),
                                          seq(..1,..2, length.out = 1000),
                                          ..2),
                                    x = cos(y)/..3) |>
                   mutate(x = x + c(rep(0,1000),
                                    seq(0,.5, length.out = 1000),
                                    0),
                          group = paste0("plant",..7),
                          fill = ..5,
                          color = ..6,
                          x = x + ..4)) |>
  mutate(logic = sp::point.in.polygon(x,y, circle$x, circle$y)) |>
  filter(logic == 1)




texture <- tibble(crossing(x = seq(0,10, length.out = 50),
                  y = seq(0,10, length.out = 50))) |>
  mutate(logic = sp::point.in.polygon(x,y, circle$x, circle$y)) |>
  filter(logic == 1) 
texture <- texture |>
  arrange(y) |>
  mutate(color = colorRampPalette(c(sky_color, "#ffffff"))(nrow(texture)))

back_texture <- tibble(crossing(x = seq(0,10, length.out = 50),
                                y = seq(0,10, length.out = 50)))

grass |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#751109"))+
  geom_path(data = back_texture, 
            aes(group = y), 
            alpha = .03,
            position = position_jitter(width = .05, height = .04),
            size = sample(seq(1,25, length.out = nrow(back_texture))), 
            color = sample(colorRampPalette(c("#000000", "#ffffff"))(nrow(back_texture))))+
  geom_polygon(data = circle, aes(x,y), inherit.aes = FALSE, fill = sky_color[4], color = "#000000", size =5)+
  geom_point(data = texture, aes(x,y, group = x),
             position = position_jitter(width =.05, height = .06), color = texture$color, inherit.aes = FALSE,  size = sample(1:10, nrow(texture), replace = TRUE), alpha = .04)+
  geom_path(data = circle, aes(x,y), inherit.aes = FALSE, color = "#1a1a1a", size = 10)+
  geom_polygon(fill = grass$fill, color = grass$color, linewidth = .4, position = position_jitter(width = .003, height = .001))+
  coord_equal(xlim = c(0,10), ylim = c(0,10))


ggsave("plants_5.png",
       dev = "png",
       dpi = 300,
       bg = "transparent")