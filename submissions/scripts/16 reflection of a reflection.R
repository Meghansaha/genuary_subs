library(tidyverse)

n = 1000

color = c("#ba3834","#731b18","#6a6a2a","#3d3d12","#634d41",
                        "#9e7057","#EEC373")
                   
background <- "#000000"
  


triangle <- tibble(x = c(0,100,50,0),
                   y = c(100,100,0,100))


all_circles <- all_circles |>
  mutate(logic = point.in.polygon(x,y,triangle$x, triangle$y))|>
  filter(logic == 1)

all_circles1 <- all_circles |>
  mutate(y = y*-1,
         group = paste0(group,"_second"))

all_circles2 <- all_circles |>
  mutate(x = x*-1,
         group = paste0(group,"_third"))

all_circles3 <- all_circles |>
  mutate(x = x*-1,
         y = y*-1,
         group = paste0(group,"_four"))



all_circles |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = background))+
  geom_polygon(fill = all_circles$fills, color = all_circles$borders, size = 1.5)+
  geom_polygon(data = all_circles1, fill = all_circles1$fills, color = all_circles1$borders, size = 1.5)+
  geom_polygon(data = all_circles2, fill = all_circles2$fills, color = all_circles2$borders, size = 1.5)+
  geom_polygon(data = all_circles3, fill = all_circles3$fills, color = all_circles3$borders, size = 1.5)+
  coord_equal(xlim = c(-100,100), ylim = c(-100,100))
  
  ggsave("reflecctions2.png",
         device = "png",
         bg = "transparent",
         dpi = 300)