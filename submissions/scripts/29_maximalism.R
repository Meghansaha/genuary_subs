library(tidyverse)

theta <- seq(0,-pi, length = 1000)
n <- 50
xmin <- 0
xmax <- 10
ymin <- 0
ymax <- 10

y_starts <- sample(seq(ymax - 3, ymax, length = n))
x_starts <- sample(seq(xmin, xmax, length = n))

fills <- sort(colorRampPalette(c("#af3918", "#a21152", "#822b75","#612884","#154baf",
                                     "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a"))(n))
                                     

drip_opts <- list(1:n,
                  y_starts,
                  x_starts,
                  fills)

jit_n <- .03



drip <- pmap_df(drip_opts, ~tibble(x = c(..3,
                                         jitter(rep(..3,1000), amount = jit_n),
                                         jitter(cos(theta)/2 + ..3 - .5, amount = jit_n),
                                         jitter(rep(..3 - 1, 1000), amount = jit_n),
                                         ..3,
                                         ..3 + .1,
                                         jitter(rep(..3 ,1000), amount = jit_n) + .1,
                                         jitter(cos(theta)/2 + ..3 - .5, amount = jit_n) + .1,
                                         jitter(rep(..3 - 1, 1000), amount = jit_n) + .1,
                                         ..3 + .1),
                                   y = c(ymax,
                                         jitter(seq(ymax,..2, length = 1000), amount = jit_n),
                                         jitter(sin(theta) + ..2, amount = jit_n),
                                         jitter(seq(..2,ymax, length = 1000), amount = jit_n),
                                         ymax,
                                         ymax - .02,
                                         jitter(seq(ymax,..2, length = 1000), amount = jit_n) - .02,
                                         jitter(sin(theta) + ..2, amount = jit_n) - .02,
                                         jitter(seq(..2,ymax, length = 1000), amount = jit_n) - .02,
                                         ymax - .02),
                                   
                                   group = c(rep(paste0("drip_",..1),3002),
                                             rep(paste0("dripshadow_",..1),3002)),
                                   
                                   fill = c(rep(..4,3002),
                                            rep("#1a1a1a",3002)))
)


y_starts2 <- sample(seq(ymin + 3, ymin, length = n))
x_starts2 <- sample(seq(xmin, xmax, length = n))

fills2 <- colorRampPalette(c("#af3918", "#a21152", "#822b75","#612884","#154baf",
                                          "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a"))(n)

drip2_opts <- list(1:n,
                   y_starts2,
                   x_starts2,
                   fills2)

theta2 <- seq(0,pi, length = 1000)

drip_below <- pmap_df(drip2_opts, ~tibble(x = c(..3,
                                         jitter(rep(..3,1000), amount = jit_n),
                                         jitter(cos(theta2)/2 + ..3 - .5, amount = jit_n),
                                         jitter(rep(..3 - 1, 1000), amount = jit_n),
                                         ..3,
                                         ..3 + .1,
                                         jitter(rep(..3 ,1000), amount = jit_n) + .1,
                                         jitter(cos(theta2)/2 + ..3 - .5, amount = jit_n) + .1,
                                         jitter(rep(..3 - 1, 1000), amount = jit_n) + .1,
                                         ..3 + .1),
                                   y = c(ymin,
                                         jitter(seq(ymin,..2, length = 1000), amount = jit_n),
                                         jitter(sin(theta2) + ..2, amount = jit_n),
                                         jitter(seq(..2,ymin, length = 1000), amount = jit_n),
                                         ymin,
                                         ymin + .02,
                                         jitter(seq(ymin,..2, length = 1000), amount = jit_n) + .02,
                                         jitter(sin(theta2) + ..2, amount = jit_n) - .02,
                                         jitter(seq(..2,ymin, length = 1000), amount = jit_n) + .02,
                                         ymin + .02),
                                   
                                   group = c(rep(paste0("drip_",..1),3002),
                                             rep(paste0("dripshadow_",..1),3002)),
                                   
                                   fill = c(rep(..4,3002),
                                            rep("#1a1a1a",3002)))
)

polka <- tibble(x = seq(xmin,xmax, by = .2),
                y = seq(ymin,ymax, by = .2)) |>
  expand.grid() |>
  mutate(logic = row_number(),
         logic = logic %% 2 != 0) |>
  filter(logic)

texture_top <- tibble(x = seq(xmin,xmax, by = .1),
                  y = seq(ymin,ymax, by = .1)) |>
  expand.grid() |>
  mutate(logic = row_number(),
         logic = logic %% 2 != 0) |>
  filter(logic) |>
  mutate(logic = sp::point.in.polygon(x,y, drip$x, drip$y)) |>
  filter(logic == 1)

square <- tibble(x = c(0,.5,
                       rep(.5,1000),
                       rep(0,1000)),
                 y = c(0,0,
                       seq(0,10, length = 1000),
                       seq(10,0, length = 1000)))

x_trans <- seq(xmin,xmax, by = .5)
y_trans <- seq(ymin,ymax, by = .5)
n_grid <- 1:(length(x_trans))
rect_opts <- list(x_trans,
                  colorRampPalette(c("#000000","#ffffff"))(length(x_trans)),
                  n_grid)
  
grid_texture <- pmap_df(rect_opts, ~square |>
                       mutate(x = x + ..1,
                              group = ..3,
                              fill = ..2))

drip |> 
  ggplot(aes(x,y, group = rev(group)))+
  theme_void()+
  theme(plot.background = element_rect(fill = "white"))+
  geom_polygon(data = grid_texture, fill = grid_texture$fill, color = rev(grid_texture$fill), size = .1, position = position_jitter(width = .05, height = .2) )+
  geom_point(data = polka, aes(x,y), inherit.aes = FALSE, color = "black", size = 2, shape = 21, stroke = 2,
             fill = sample(colorRampPalette(c("#af3918", "#a21152", "#822b75","#612884","#154baf",
                                                       "#0b82b9", "#277e9d","#488e35","#e3a934","#b2336a"))(nrow(polka))))+
  geom_polygon(fill = drip$fill, color = "#1a1a1a")+
  geom_polygon(data = drip_below, fill = drip_below$fill, color = "#1a1a1a")+
  coord_cartesian(xlim = c(xmin, xmax),
                  ylim = c(ymin, ymax),
                  expand = FALSE)

ggsave("day 29.png",
       dpi = 300,
       device = "png")