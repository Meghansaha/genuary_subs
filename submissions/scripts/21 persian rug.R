library(tidyverse)

border_square <- tibble(x = c(0,.5,.5,0,0),
                        y = c(0,0,.5,.5,0))

y_trans <- c(rep(0,20),
             seq(.5,14.5, length = 30),
             rep(14.5, 20),
             seq(14.5, .5, length = 30))

x_trans <- c(seq(0,9.5, length = 20),
             rep(9.5, 30),
             seq(9.5,0, length = 20),
             rep(0, 30))


border_colors <- rep_along(y_trans,c("#301E67","#13005A"))

group_num <- 1:length(y_trans)

border_opts <- list(x_trans, y_trans, border_colors, group_num)

border <- pmap_df(border_opts, ~border_square |>
                    mutate(x = x + ..1,
                           y = y + ..2,
                           fill = ..3,
                           group = paste0("outer_",..4)))

inner_border <- tibble(expand.grid(x = c(seq(.5,9.5, length = 200),
                             rep(9.5,200),
                             seq(9.5, .5, length = 200),
                             rep(.5, 200)),
                       y = c(rep(.5,200),
                             seq(.5,14.5, length = 200),
                             rep(14.5, 200),
                             seq(14.5, .5, length = 200)),
                       group = "inner")) 

center <- tibble(expand.grid(x = c(seq(1,9, length = 200),
                                   rep(9,200),
                                   seq(9, 1, length = 200),
                                   rep(1, 200)),
                             y = c(rep(1,200),
                                   seq(1,14, length = 200),
                                   rep(14, 200),
                                   seq(14, 1, length = 200)),
                             group = "innercenter")) 

diamond_inner <-  tibble(x = c(5,7.5,5,2.5,5),
                         y = c(2.5,7.5,12.5,7.5,2.5),
                         group = "diamond")

diamond_outer <- tibble(x = c(5,8,5,2,5),
                        y = c(2,7.5,13,7.5,2),
                        group = "diamond2")

texture <- tibble(x = seq(0,10, length = 200),
                  y = seq(0,15, lengt= 200)) |>
  expand.grid()

diamond_texture <- texture |>
  mutate(logic = sp::point.in.polygon(x,y,diamond_inner$x, diamond_inner$y),
         group = "diamond") |>
  filter(logic == 1)
  
border |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  geom_polygon(fill = border$fill)+
  geom_point(data = inner_border, fill = "#1a1a1a")+
  geom_path(data = inner_border, position = position_jitter(width = .03, height = .05), linewidth = .1, color = "#333333", alpha = .4)+
  geom_point(data = center, fill = "#13005A", size = .5)+
  geom_path(data = center, position = position_jitter(width = .03, height = .05), linewidth = .1, color = "#301E67", alpha = .4)+
  geom_polygon(data = diamond_inner, fill = "#e8a71c")+
  geom_path(data = diamond_texture, aes(group = y),  linewidth = 1, color = sample(colorRampPalette(c("#c99f42","gold"))(nrow(diamond_texture))))+
  geom_path(data = diamond_outer, linewidth = 2, color = "#e8a71c")+
  geom_path(data = texture, aes(sample(x),sample(y)), inherit.aes = FALSE, alpha = .002, color = "#ffffff", linewidth = sample(seq(.1,.4, length = nrow(texture))))+
  coord_equal(xlim = c(0,10), ylim = c(0,15), expand = FALSE)

ggsave("day 21.png",
       device = "png",
       dpi = 300,
       bg = "transparent")