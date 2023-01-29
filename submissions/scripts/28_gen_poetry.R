library(tidyverse)
library(showtext)
library(ggrepel)

lyrics <- read_csv(here::here("submissions","lyrics.csv"))

#sample(lyrics$lyrics, 6)

font_add(family = "sfpro", regular = "submissions/sfpro.ttf")
showtext_auto()


poem <- list("And I don't know why, he don't ever give up",
             "Please don't say you need me",
             "I might kill my ex, I still love him, though...",
             "Not the one in Los Angeles",
             "The text gon' be evidence, this text is evidence...",
             "I did it all on no drugs")

datetime <- format(Sys.time(), "%a %b %d at %X")

