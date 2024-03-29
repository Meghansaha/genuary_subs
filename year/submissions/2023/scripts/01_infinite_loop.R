#=============================================================================#
#Library Load-in---------------------------------------------------------------
#=============================================================================#

library(dplyr) #For data wrangling/manipulation
library(purrr) #For list wrangling/manipulation/iterations
library(ggplot2) #For plotting
library(gganimate) #For gif creation
library(transformr) #For gif creation assistance

#=============================================================================#
#Data Set up-------------------------------------------------------------------
#=============================================================================#

#Angles for shapes#
theta <- seq(0,2*pi, length.out = 10000)

#Base level of circles to be manipulated#
circles <- tibble(x = cos(theta) * seq(1,100, length.out = 100),
                  y = sin(theta) * seq(1,100, length.out = 100))

#Color palette to use#
color_base <- c("#af3918", "#a21152", "#822b75","#612884", "#154baf",
                "#0b82b9","#277e9d","#488e35","#e3a934","#b2336a")

#Creating indices to create the pattern of which colors will be presented in a loop#            
indices <- list(c(1:10),
                c(2:10,1),
                c(3:10,1:2),
                c(4:10,1:3),
                c(5:10,1:4),
                c(6:10,1:5),
                c(7:10,1:6),
                c(8:10,1:7),
                c(9:10,1:8),
                c(10,1:9))


#=============================================================================#
#Final Data Compilation--------------------------------------------------------
#=============================================================================#

color_picks <- map2_df(indices,1:10, ~circles |>
                        mutate(color = colorRampPalette(color_base[.x])(nrow(circles)),
                               group = paste0("set_",.y))
                      )

#=============================================================================#
#Final Piece-------------------------------------------------------------------
#=============================================================================#

#!!!May take a while to build/render based on your PC stats!!!#

color_picks %>%
  ggplot(aes(x = x*y,
             y = x, 
             group = group))+
  theme_void()+
  theme(plot.background = element_rect(fill = "black", 
                                       linewidth = 20, 
                                       color = "white"))+
  geom_path(linewidth = sample(c(1:4), nrow(color_picks), replace = TRUE),
             position = position_jitter(width = .1), 
            color = color_picks$color)+
  coord_polar()+
  transition_states(
    group,
    transition_length = 10,
    state_length = .1
  ) +
  enter_fade() + 
  exit_fade() +
  ease_aes('bounce-in-out')

#Use to save output locally:
#anim_save("images/01.gif")