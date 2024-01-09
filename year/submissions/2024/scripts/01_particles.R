#=============================================================================#
#Library Load-in---------------------------------------------------------------
#=============================================================================#

library(dplyr) #For data wrangling/manipulation
library(purrr) #For list wrangling/manipulation/iterations
library(ggplot2) #For plotting
library(tidyr) # For some more tidy data manipulation
library(here) # For easier directory navigation

#=============================================================================#
# Image Options----------------------------------------------------------------
#=============================================================================#
# Color Palette Option---------------------------------------------------------
color_pal <-
  c(
    "#4F6F52", "#D9EDBF", 
    "#FFB996", "#FF8080",
    "#FFCF81", "#FFCD4B",
    "#F8FF95", "#FDFFAB"
  )

#=============================================================================#
# Function Creation------------------------------------------------------------
#=============================================================================#

particle_randomizer <-
  function(
    color_pal = color_pal,
    border_color = c("white", "black", "random", "none"),
    crowding = c("light", "middle", "heavy", "random"),
    coordinates = c("cartesian", "polar", "equal", "random")
  ){
    
    # Parameter Lists------------------------------------------------------------
    ## Controls particle border color------------------------------------------
    border_list = list(
      "white" = "#ffffff",
      "black" = "#000000",
      "none" = NA
    )
    
    ## Controls particle crowding amount-----------------------------------------
    crowds_list = list(
      "light" = 100,
      "medium" = 500,
      "heavy" = 1000
    )
    
    ## Controls coordinate systems-----------------------------------------------
    coords_list = list(
      "cartesian" = coord_cartesian(),
      "polar" = coord_polar(),
      "equal" = coord_equal()
    )
    
    # Input Variable Lists-------------------------------------------------------
    ## Color Palette-------------------------------------------------------------
    input_pal = color_pal
    
    ## Crowding/Spacing of points------------------------------------------------
    input_crowding = switch(
      crowding,
      "light" = crowds_list$light,
      "medium" = crowds_list$medium,
      "heavy" = crowds_list$heavy,
      "random" = unlist(sample(crowds_list,1)))
    
    ## Border color of circles/points--------------------------------------------
    input_border = switch(
      border_color,
      "black" = border_list$black,
      "white" = border_list$white,
      "none" = NA,
      "random" = unlist(sample(border_list,1)))
    
    ## Ggplot plot coordinate systems--------------------------------------------
    input_coord = switch(
      coordinates,
      "cartesian" = coords_list$cartesian,
      "polar" = coords_list$polar,
      "equal" = coords_list$equal,
      "random" = unlist(sample(coords_list,1))
    )
    
    ## Randomized and calculated background of the ggplot------------------------
    background_color = sample(colorRampPalette(c(input_pal,"#000000","#ffffff"))(50),1)
    
    # Data set up----------------------------------------------------------------
    
    ## Angle/Theta Transformations-----------------------------------------------
    theta = seq(sample(1:100, 1), sample(1:100, 1)*pi, length.out = 100)
    
    # Base data sets to be transformed-------------------------------------------
    particles <- 
      tibble(
        crossing(
          x = seq(0,10, length.out = input_crowding),
          y = x
        )
      )
    
    # Additions of varied transformations to the x and y variables-------------
    particles_mod <-
      particles |>
      mutate(
        x = sin(theta) * x,
        y = cos(theta) * y,
        color = colorRampPalette(input_pal)(nrow(particles)))
    
    
    # Final work for the ggplot--------------------------------------------------
    particles_mod |>
      ggplot(aes(x,y))+
      theme_void()+
      geom_point(
        shape = 21,
        size = rev(seq(.01, 2, length.out = nrow(particles_mod))),
        stroke = .1,
        alpha = 1,
        fill = rev(sort(particles_mod$color)),
        position = "jitter"
      )+
      theme(plot.background = element_rect(fill = background_color))+
      input_coord
  }

#=============================================================================#
# Final Fx Use-----------------------------------------------------------------
#=============================================================================#
particle_randomizer(
  color_pal = color_pal,
  border_color = "black",
  crowding = "light",
  coordinates = "random"
)

#=============================================================================#
# Image Directory/Saving Work--------------------------------------------------
#=============================================================================#
img_path <-
  here(
    "year",
    "submissions",
    "2024",
    "images",
    "01.png"
  )

## Save the image--------------------------------------------------------------
ggsave(
  img_path,
  dpi = 300,
  device = "png",
  bg = "transparent"
)