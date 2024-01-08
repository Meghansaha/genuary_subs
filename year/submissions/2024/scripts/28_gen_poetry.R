#Genuary 28 2023 - Generative Poetry (Final Image made in Canva)
#Canva Link - https://www.canva.com/design/DAFYukT6P28/P9sHbk3ryvsLitmOT_XEsg/view?utm_content=DAFYukT6P28&utm_campaign=designshare&utm_medium=link&utm_source=publishsharelink&mode=preview

#=============================================================================#
#Library Load-in---------------------------------------------------------------
#=============================================================================#
library(readr) #Loading in Data
library(here) #Easier directory movement

#=============================================================================#
#Data Pull/Generation----------------------------------------------------------
#=============================================================================#
lyrics <- read_csv(here("submissions","helpers","lyrics.csv"))

sample(lyrics$lyrics, 6)

#Final Chosen for Canva Design:
poem <- list("And I don't know why, he don't ever give up",
             "Please don't say you need me",
             "I might kill my ex, I still love him, though...",
             "Not the one in Los Angeles",
             "The text gon' be evidence, this text is evidence...",
             "I did it all on no drugs")

#Song Information:
#Frank Black - Los Angeles
#SZA - Kill Bill
#The Game ft. 50 Cent - Hate It or Love It
#BabyFace/Ella Mai - Keeps on Fallin'
#Labrinth - I'm Tired