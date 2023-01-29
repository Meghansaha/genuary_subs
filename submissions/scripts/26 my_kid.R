library(tidyverse)

theta <- seq(0,2*pi, length = 1000)

splat2 <- tibble(x = jitter(cos(theta), amount =.02),
                y = jitter(sin(theta)),amount = .02) |>
  mutate(x = x *pi*y,
         y = x*y)


splat <- splat2 |>
  slice_sample(prop= .01) |>
  mutate(x = x + 3,
         y = y + 5)

texture <- tibble(x = seq(0,5, length = 100),
                  y = seq(0,10, length = 100)) |>
  expand.grid()

bluelines <- tibble(x = 0:5)

bluelines_final <- map2_df(seq(0,9, length.out = 19),c(1.2,rep(.8,18)), ~bluelines |>
                            mutate(y = .x,
                                   group = .x,
                                   size = .y))

splat |>
  ggplot(aes(x,y))+
  theme_void()+
  theme(plot.background = element_rect(fill = "white"))+
  geom_point(data = texture, size = sample(seq(20,5, length = 30), nrow(texture), replace = TRUE), alpha = .08, color = "#FCF5F4", position = position_jitter(width = .5, height = .6))+
  geom_path(data = bluelines_final, aes(group = group), color = "#83BEC6", size = rev(bluelines_final$size))+
  geom_vline(xintercept = .6, color = "#BB7867")+
  geom_path(size = sample(seq(.1,3, length = 50), nrow(splat), replace = TRUE), color = "#000f55", lineend = "round") +
  coord_cartesian(xlim = c(0,5), ylim = c(0,10))
