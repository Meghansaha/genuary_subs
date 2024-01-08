#=============================================================================#
#Library Load-in---------------------------------------------------------------
#=============================================================================#

library(dplyr) #For data wrangling/Manipulation
library(purrr) #For list wrangling/manipulation/iterations
library(ggplot2) #For Plotting
library(RColorBrewer) #For picking out some brewer colors

#=============================================================================#
#Data Set-up-------------------------------------------------------------------
#=============================================================================#

#Making the grid#
square <- tibble(x = c(0,.1,.1,0,0),
                 y = c(0,0,.1,.1,0))

#Making a single row for the grid#
rowsq <- map_df(seq(0,5, by = .1), ~square |>
                  mutate(x = x + .x))

#Making the full grid#
full <- map_df(seq(0,5, by = .1), ~rowsq |>
                 mutate(y = y + .x))

#Manually calculated the groups for some reason?#
#I don't usually do this like this but I'm too lazy to change it now#
full$group <- rep(paste0("group_",1:2601), each = 5)

#Calculated/Setting some glitch colors
glitch_colors <- sample(c("#ff001e", "#f727f7", "#f7f727", "#27f727", "#001eff", "#000000", "#ffffff", rep(NA, 100)), 2601, replace = TRUE)

#Adding the glitch colors to the data set#
full$fill <- rep(glitch_colors, each = 5)

#Getting some gray colors for the clouds#
grays <- brewer.pal(3, "Greys")

#Setting the colors for the moon/planet/orb thing#
moon_colors <- colorRampPalette(c("#3E497A", "#21325E", "#7858A6", "#548CA8", "#46C2CB", "#2D033B"))(100)

#Setting the sizes for the moon/planet/orb object#
moon_sizes <- seq(250,20,length = 100)

#Making the moon/planet/org thing#
moon <- map2_df(moon_sizes, moon_colors, ~tibble(x = 2.5, y = 2.5) |> 
                  mutate(size = .x, fill = .y )
                )

#Making the clouds#
clouds <- full |> select(y) |> 
  filter(y > 1 & y < 4) |> 
  mutate(x = seq(0, 5, length = length(y)), 
         color = colorRampPalette(grays)(length(y)))|> 
  slice_sample(prop = .80)


#=============================================================================#
#Final Piece-------------------------------------------------------------------
#=============================================================================#

full |>
  ggplot()+
  theme_void()+
  theme(plot.background = element_rect(fill = "#3B185F"))+
  geom_point(data = full, aes(x,y), 
             color = "#1a1a1a", 
             alpha = .2, 
             size = sample(1:15, nrow(full), replace = TRUE), 
             position = position_jitter(width = .01, height = .08))+
  geom_point(data = full |> slice_sample(n = 100), aes(x,y), 
             alpha = sample(seq(.01,.5, length.out = 100)), 
             color = "#ffffff", 
             position = position_jitter(width = .01, height = .02), size = .5)+
  geom_point(data = moon, aes(x,y), 
             size = moon$size, 
             color = moon$fill, 
             position = position_jitter(width = .02, height = .04))+
  geom_point(data = clouds, aes(x,y), 
             color = clouds$color, 
             alpha = .004, 
             size = sample(1:15, nrow(clouds), replace = TRUE), 
             position = position_jitter(width = 1, height = .8))+
  geom_path(aes(x,y, group = group), 
            color = "#1a1a1a", 
            alpha = .1)+
  geom_polygon(aes(x,y, group = group), 
               fill = full$fill, 
               alpha = sample(seq(.1,1, length = 100), nrow(full), replace = TRUE))+
  coord_equal(xlim = c(0,5), 
              ylim = c(0,5), 
              expand = FALSE)

#To save the output:
# ggsave("images/03.png",
#        dpi = 300,
#        device = "png",
#        bg = "transparent",
#        height = 10,
#        width = 10)