library(tidyverse)

theta <- seq(0,2*pi, length = 100)

circle <- tibble(x = cos(theta)/2,
                 y = sin(theta)/2,
                 group = "circle_")

wavetop <- tibble(x = seq(50,200, length = 10000),
               y = x)

wavebot <- wavetop |>
  mutate(y = y + 60)

wave1 <- rbind(wave2top |> add_row(tail(wavetop,1)), wavebot |> arrange(desc(x)) |> add_row(head(wavebot,1)), head(wavetop,1)) |>
  mutate(group = "wave0") |>
  mutate(y = y - 90,
         x = x - 20)


wave2top <- tibble(x = seq(0,100, length = 10000),
               y = x + sin(cos(x))/6)

wave2bot <- wave2top |>
  mutate(y = y + 60)

wave2 <- rbind(wave2top |> add_row(tail(wave2top,1)), wave2bot |> arrange(desc(x)) |> add_row(head(wave2bot,1)), head(wave2top,1)) |>
  mutate(group = "wave2") |>
  mutate(y = y + 20)



wave3top <- tibble(x = seq(0,100, length = 1000),
                y = -x - sin(cos(x)) + cospi(x))

wave3bot <- wave3top |>
  mutate(y = y + 60)


wave <- rbind(wave3top |> add_row(tail(wave3top,1)), wave3bot |> arrange(desc(x)) |> add_row(head(wave3bot,1)), head(wave3top,1)) |>
  mutate(group = "wave1") |>
  mutate(y = y + 20)

dots <- circle_packer(200,0,100,0,100) |>
  mutate(logic = sp::point.in.polygon(x,y,wave$x, wave$y)) |>
  filter(logic == 1)


dots2 <- circle_packer(200,0,100,0,100) |>
  mutate(logic = sp::point.in.polygon(x,y,wave2$x, wave2$y)) |>
  filter(logic == 1)

dots3 <- circle_packer(200,0,100,0,100) |>
  mutate(logic = sp::point.in.polygon(x,y,wave1$x, wave1$y)) |>
  filter(logic == 1)

dots_back <- circle_packer(80,0,100,0,100) 


wave|>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "white"))+
  geom_polygon(data = dots_back, fill = "black", color = "#1a1a1a", linewidth = .4)+
  geom_polygon(fill = "turquoise")+
  geom_polygon(data = dots, fill = "white", color = "black", linewidth =.6)+
  geom_polygon(data = wave2, fill = "gold")+
  geom_polygon(data = dots2, color = "black", fill = "#ffffff", linewidth = .9)+
  geom_polygon(data = wave1, fill = "darkred")+
  geom_polygon(data = dots3, fill = "white", color = "black", linewidth = .5)+
  coord_equal(xlim = c(0,100), ylim = c(0,100), expand = FALSE)

ggsave("day 25.png",
       device = "png",
       dpi = 300,
       bg = "transparent")