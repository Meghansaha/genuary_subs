#Genuary 30 2023 - Minimalism
#=============================================================================#
#Library Load-in---------------------------------------------------------------
#=============================================================================#
library(dplyr)
library(purrr)
library(ggplot2)

#=============================================================================#
#Data Set-up-------------------------------------------------------------------
#=============================================================================#
#Setting angles for the circles#
theta <- seq(0,2*pi, length = 100)

#Setting a succession of radi for the circles#
radi <- seq(0,20, length = 100)

#Setting up for iterations#
n <- 1:length(radi)
color_pals <- c("#000000", "#ffffff")
fill_pals <- colorRampPalette(c("#000000","#070b40","#290740"))(length(radi))

#Data Compilation#
circles <- pmap_df(list(radi,n,fill_pals), ~tibble(x = cos(theta) * .x + 10,
                                   y = sin(theta) * ..1 + 10,
                                   group = paste0("ring_",..2),
                                   color = sample(color_pals, length(theta), replace = TRUE),
                                   fill = ..3))

#Setting angles for the center circle#
theta <- seq(0,2*pi, length = 1000)

#Data for the center circle#
center <- tibble(x = cos(theta) * 5 + 10,
                 y = sin(theta)* 5 + 10,
                 color = "#ffffff",
                 group = "center")

#=============================================================================#
#Final Piece-------------------------------------------------------------------
#=============================================================================#

circles |>
  ggplot(aes(x,y, group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#000000"))+
  geom_polygon(data = center, 
               fill = "#ffffff")+
  geom_path(color = circles$color,
            linewidth = .1)+
  coord_equal(xlim = c(0,20),
              ylim = c(0,20),
              expand = FALSE)

#To save the output:
# ggsave("images/30.png",
#        dpi = 300,
#        device = "png",
#        bg = "transparent")
