library(tidyverse)

wave1 <- tibble(x = c(seq(0,100, length.out = 1000),
                     rep(100,1000),
                     seq(100,0, length.out = 1000),
                     rep(0,1000)
                     ),
               y = c(rep(0,1000) + sin(x[1:1000])/2 * cos(x[1:1000]),
                     seq(0,1, length.out = 1000),
                     rep(1,1000) + sin(x[2001:3000])/2 * cos(x[2001:3000]),
                     seq(1,0, length.out = 1000)
                     ),
               group = "wave1_"
)

wave2 <- tibble(y = c(seq(0,100, length.out = 1000),
                      rep(100,1000),
                      seq(100,0, length.out = 1000),
                      rep(0,1000)),
                x = c(rep(0,1000) + sin(y[1:1000])/2 * cospi(y[1:1000]),
                      seq(0,1, length.out = 1000),
                      rep(1,1000) + sin(y[2001:3000])/2 * sinpi(y[2001:3000]),
                      seq(1,0, length.out = 1000)),
                group = "wave2_"
)

wave3 <- tibble(x = c(seq(0,100, length.out = 1000),
                      rep(100,1000),
                      seq(100,0, length.out = 1000),
                      rep(0,1000)),
                y = c(rep(100,1000) + sin(x[1:1000])/2 * cos(x[1:1000]),
                      seq(100,99, length.out = 1000),
                      rep(99,1000) + sin(x[2001:3000])/2 * cos(x[2001:3000]),
                      seq(99,100, length.out = 1000)
                ),
                group = "wave3_"
)

wave4 <- tibble(y = c(seq(0,100, length.out = 1000),
                      rep(100,1000),
                      seq(100,0, length.out = 1000),
                      rep(0,1000)),
                x = c(rep(100,1000) + sin(y[1:1000])/2 * cospi(y[1:1000]),
                      seq(100,99, length.out = 1000),
                      rep(99,1000) + sin(y[2001:3000])/2 * sinpi(y[2001:3000]),
                      seq(99,0, length.out = 1000)),
                group = "wave4_"
)

wave_maker <- function(n, wave_df, direction = c("up","down","left","right")){
  
  if(direction %in% c("up","down")){
    new_waves <- map_df(1:n, ~ wave_df %>%
                          mutate(y = y + ifelse(direction == "up", 1*.x,-1*.x),
                                 group = paste0(group,.x))
    )
  } else {
    new_waves <- map_df(1:n, ~ wave_df %>%
                          mutate(x = x + ifelse(direction == "right", 1*.x,-1*.x),
                                 group = paste0(group,.x))
    )
                        }
  
  
  return(new_waves)
}

n1 <- 25


waves1 <- pmap_df(list(n1,
                       "up"), 
                  ~ wave_maker(..1,wave1, direction = ..2))|>
  mutate(y = y - 1)


waves1$fill <- rep_along(1:nrow(waves1), c(rep("white",4000), rep("black",4000)))

waves2 <- pmap_df(list(n1,
                       "right"), 
                  ~ wave_maker(..1,wave2, direction = ..2)) |>
  mutate(x = x - 1)

waves2$fill <- rep_along(1:nrow(waves2), c(rep("white",4000), rep("black",4000)))

waves3 <- pmap_df(list(n1,
                       "down"), 
                  ~ wave_maker(..1,wave3, direction = ..2))|>
  mutate(y = y + 1)

waves3$fill <- rep_along(1:nrow(waves3), c(rep("white",4000), rep("black",4000)))

waves4 <- pmap_df(list(n1,
                       "left"), 
                  ~ wave_maker(..1,wave4, direction = ..2)) |>
  mutate(x = x + 1)

waves4$fill <- rep_along(1:nrow(waves4), c(rep("white",4000), rep("black",4000)))

center <- tibble(x = c(seq(25,75, length = 100),
                       rep(75, 100),
                       seq(75,25, length = 100),
                       rep(25, 100)),
                 y = c(rep(25,100),
                       seq(25,75, length = 100),
                       rep(75, 100),
                       seq(75,25, length = 100)),
                 group = "center_")

n2 <- 50
trans <- seq(0,25, length.out = n2)
colors_box <- rev(colorRampPalette(c("#ffffff","#000000"))(n2))

center_data <- pmap_df(list(1:n2, 
                             trans,
                             colors_box), ~tibble(x = c(seq(25+..2,75-..2, length = 100),
                                               rep(75-..2, 100),
                                               seq(75-..2,25+..2, length = 100),
                                               rep(25+..2, 100)),
                                         y = c(rep(25+..2,100),
                                               seq(25+..2,75-..2, length = 100),
                                               rep(75-..2, 100),
                                               seq(75-..2,25+..2, length = 100)),
                                         group = paste0("center_",..1),
                                         fill = ..3)) |>
  arrange(group)

waves1 |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#1a1a1a"))+
  geom_polygon(data = center_data,
               position = position_jitter(width = .2, height = .4),
               color = "#1a1a1a",
               fill = center_data$fill,
               alpha = .2) +
  geom_polygon(position = position_jitter(width = .2, height = .4),
               color = "#1a1a1a",
               fill = waves1$fill) +
  geom_polygon(data = waves2,
               position = position_jitter(width = .2, height = .4),
               color = "#1a1a1a",
               fill = waves2$fill) +
  geom_polygon(data = waves3,
               position = position_jitter(width = .2, height = .4),
               color = "#1a1a1a",
               fill = waves3$fill) +
  geom_polygon(data = waves4,
               position = position_jitter(width = .2, height = .4),
               color = "#1a1a1a",
               fill = waves4$fill) +
  # geom_polygon(data = wave2, position = position_jitter(width = .02, height = .004)) +
  # geom_polygon(data = wave3, position = position_jitter(width = .02, height = .004)) +
  # geom_polygon(data = wave4, position = position_jitter(width = .02, height = .004)) +
  coord_equal(xlim = c(0,100), ylim = c(0,100), expand = TRUE)

ggsave("day 19.png",
       device = "png",
       dpi = 300,
       bg = "transparent")