library(tidyverse)
library(ambient)



update_curl <- function(current_state, step_size = .000008, ...) {
  curl <- curl_noise(
    x = current_state$x, 
    y = current_state$y,
    ...
  )
  next_state <- current_state |>
    mutate(
      x = x + curl$x * step_size,
      y = y + curl$y * step_size,
      time = time + 1
    )
  return(next_state)
}

coords <- seq(0, 1, length.out = 400)
time_1 <- long_grid(x = coords, y = coords) |> 
  mutate(id = row_number(), time = 1)

time_2 <- time_1 |>
  update_curl(
    generator = gen_simplex,
    frequency = 10, 
    seed = 05181990
  )

time_3 <- time_2 |> 
  update_curl(
    generator = gen_simplex,
    frequency = 10, 
    seed = 11172022
  )

dat123 <- bind_rows(time_1,time_3,time_2)

dat123 <- dat123 |>
  update_curl(
    generator = gen_simplex,
    frequency = 5, 
    seed = 411
  )

colorpal <- c("#af3918", "#a21152", "#822b75","#612884","#154baf",
                       "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a")
                       

dat123 |>
  ggplot(aes(x, y, group = id)) + 
  theme_void() +
  theme(plot.background = element_rect(fill = "#000000")) +
  geom_path(colour = colorRampPalette(colorpal)(nrow(dat123)), linewidth = 2) +
  coord_equal()

ggsave("always learn.png",
       bg = "transparent",
       device = "png",
       dpi = 300)


