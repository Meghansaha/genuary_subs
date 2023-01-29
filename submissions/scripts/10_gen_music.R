#Genuary 10 2023 - Generative Music

#=============================================================================#
#Library Load-In---------------------------------------------------------------
#=============================================================================#
library(dplyr) #Data wrangling
library(purrr) #List Manipulation/iteration work
library(stringr) #String Manipulation
library(ggplot2) #For plotting
library(showtext) #For Custom Fonts

#=============================================================================#
#Font Load-in------------------------------------------------------------------
#=============================================================================#

font_add(family = "tequila", regular = "submissions/helpers/tequila/TEQUILA_.TTF")
showtext_auto()

#=============================================================================#
#Data Set up-------------------------------------------------------------------
#=============================================================================#

#Every word in the song separated by comma#
lyrics <- "I'm,on,my,time,with,everyone,I,have,very,bad,posture,Sit,and,drink,Pennyroyal,Tea,Distill,the,life,that's,inside,of,me,Sit,and,drink,Pennyroyal,Tea,I'm,anemic,royalty,Give,me,a,Leonard,Cohen,afterworld,So,I,can,sigh,eternally,I'm,so,tired,I,can't,sleep,I'm,a,liar,and,a,thief,Sit,and,drink,Pennyroyal,Tea,I'm,anemic,royalty,I'm,on,warm,milk,and,laxatives,Cherry-flavored,antacids"

#A base chord box#
chord_box <- tibble(x = c(rep(1,6), 1:6),
                    y = c(1:6, rep(1,6)),
                    xend = c(rep(6,6), 1:6),
                    yend = c(1:6, rep(6,6)),
                    group = c(rep("vert",6), rep("horiz",6)),
                    size = c(rep(1,5),3,rep(1,6)))

#Creating the set of chord boxes#
boxes <- map2_df(c(0,rep(7,6)), 0:6, ~chord_box|>
                   mutate(x = x + (.x * .y),
                          xend = xend + (.x * .y)))

#Manual calcuation of the chord positions#
chord_points <- tibble(x = c(3,4,5,
                             1,2,6,
                             2,3,6,
                             4,5,6,
                             2,3,
                             3,
                             3,4),
                       y = c(4.5,4.5,5.5,
                             3.5,4.5,3.5,
                             3.5,4.5,5.5,
                             4.5,3.5,4.5,
                             5.5,3.5,
                             4.5,
                             4.5,4.5),
                       group = c(rep("Am",3),
                                 rep("G",3),
                                 rep("C",3),
                                 rep("D",3),
                                 rep("Bb5",2),
                                 rep("A5",1),
                                 rep("Asus2",2))) |>
  mutate(x = x + c(rep(0,3),
                   rep(7*1,3),
                   rep(7*2,3),
                   rep(7*3,3),
                   rep(7*4,2),
                   rep(7*5,1),
                   rep(7*6,2)))

#Adding the chord titles#
chord_annotations <- tibble(chords = c("Am","G","C","D","Bb5","A5","Asus2"),
                            x = c(3.5 + 7*0:6),
                            y = 8.3)

#Adding the muted string symbols#
xes <- tibble(text = "X",
              x = c(1,
                    1 + 14,
                    1 + 21, 2 + 21,
                    1 + 28, 4 + 28, 5 + 28, 6 + 28,
                    1 + 35, 4 + 35, 5 + 35, 6 + 35,
                    1 + 42),
              y = 6.7)

#Adding some overall texture#
texture <- tibble(expand.grid(y = -10:20,
                              x = 0:48))

#Creating a data set for the splt lyrics#
words <- tibble(text = unlist(strsplit(lyrics,","))) |>
  mutate(x = sample(seq(0,48, length.out = 100), 70),
         y = c(sample(seq(-10,5, length.out = 100), 35),
               sample(seq(11,20, length.out = 100), 35)))

boxes |>
  ggplot(aes(x,y, xend = xend, yend = yend))+
  theme_void()+
  theme(plot.background = element_rect(fill = "#FFFCE9"))+
  geom_point(data = texture, aes(x,y), 
             inherit.aes = FALSE, 
             size = sample(1:50, nrow(texture), replace = TRUE), 
             alpha = .04, color = "#F2EDC5", 
             position = position_jitter(width = .9, height = .5))+
  geom_text(data = words, aes(x,y, label = text), 
            inherit.aes = FALSE, 
            family = "tequila", 
            size = sample(5:20, nrow(words), replace = TRUE), 
            color = sample(colorRampPalette(c("#000000","#751109"))(nrow(words))), 
            angle = sample(0:180, nrow(words)))+
  geom_path(data = texture, aes(x,y), 
            inherit.aes = FALSE,  
            alpha = .04, 
            linewidth = 10, color = "#751109", 
            position = position_jitter(width = .9, height = .5))+
  geom_segment(linewidth = boxes$size) +
  geom_point(data = chord_points, aes(x,y), 
             size = 3, 
             inherit.aes = FALSE, 
             shape = 21, 
             color = "#000000", 
             fill = "#751109")+
  geom_text(data = chord_annotations, aes(x,y, label = chords), 
            inherit.aes = FALSE, 
            family = "tequila", 
            size = 12, 
            color = "#751109", 
            fontface = "bold")+
  geom_text(data = xes, aes(x,y, label = text), 
            inherit.aes = FALSE, 
            family = "tequila", 
            size = 6 )+
  coord_equal(ylim = c(-10,20), 
              xlim = c(0,48))

#To save the output:
# ggsave("images/10.png",
#        device = "png",
#        dpi = 300,
#        bg = "transparent",
#        width = 9.98,
#        height = 10.5,
#        )