Zeynep_Birthday<-function(name){ 
  if (!require(dplyr)){ install.packages('dplyr') }
  if (!require(audio)){ install.packages('audio') }  
  notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
  pitch <- paste("D D D",
                 "G D5",
                 "C5 B A G5 D5",
                 "C5 B A G5 D5",
                 "C5 B C5 A D D D",
                 "G D5",
                 "C5 B A G5 D5",
                 "C5 B A G5 D5",
                 "C5 B C5 A D D",
                 "E E C5 B A G",
                 "G A B A E F# D D",
                 "E E C5 B A G",
                 "D5 A D D",
                 "E E C5 B A G",
                 "G A B A E F# D5 D5",
                 "G5 F5 D#5 D5 C5 A# A G",
                 "D5 D D D",
                 "G D5",
                 "C5 B A G5 D5",
                 "C5 B A G5 D5",
                 "C5 B C5 A D D D",
                 "G D5",
                 "C5 B A G5 D5",
                 "G5 F5 D#5 Bb5 A5",
                 "G5 G G G G")
  
  
  duration <- c(0.33, 0.33, 0.33, 
                2, 2, 
                0.33, 0.33, 0.33, 2, 1, 
                0.33, 0.33, 0.33, 2, 1, 
                0.33, 0.33, 0.33, 2, 0.33, 0.33, 0.33, 
                2, 2, 
                0.33, 0.33, 0.33, 2, 1, 
                0.33, 0.33, 0.33, 2, 1, 
                0.33, 0.33, 0.33, 2, 0.75, 0.25,
                1.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                0.33, 0.33, 0.33, 0.75, 0.25, 1, 0.75, 0.25, 
                1.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                1, 2, 0.75, 0.25,
                1.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                0.33, 0.33, 0.33, 0.75, 0.25, 1, 0.75, 0.25, 
                0.75, 0.25, 0.75, 0.25, 0.75, 0.25, 0.75, 0.25, 
                3, 0.33, 0.33, 0.33,   
                2, 2, 
                0.33, 0.33, 0.33, 2, 1, 
                0.33, 0.33, 0.33, 2, 1, 
                0.33, 0.33, 0.33, 2, 0.33, 0.33, 0.33, 
                2, 2, 
                0.33, 0.33, 0.33, 2, 1, 
                0.33, 0.33, 0.33, 2, 1, 
                1,  0.33, 0.33, 0.33, 1)
  
  starwars <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                         duration = duration)
  
  starwars <-
    starwars %>%
    mutate(octave = substring(pitch, nchar(pitch))  %>%
             {suppressWarnings(as.numeric(.))} %>%
             ifelse(is.na(.), 4, .),
           note = notes[substr(pitch, 1, 1)],
           note = note + grepl("#", pitch) -
             grepl("b", pitch) + octave * 12 +
             12 * (note < 3),
           freq = 2 ^ ((note - 60) / 12) * 440)
  
  tempo <- 150
  sample_rate <- 44100
  make_sine <- function(freq, duration) {
    wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                  freq * 2 * pi)
    fade <- seq(0, 1, 50 / sample_rate)
    wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
  }
  
  starwars_wave <-
    mapply(make_sine, starwars$freq, starwars$duration) %>%
    do.call("c", .)
  audio::play(starwars_wave)
  
  par(mar = c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  if (name=="Xen"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("Happy B-day Zeynep!! I hope you have a very sweet day\n",
                               "and an amazing year.\n",
                                 "Thanks a lot for your friendship <3!\n", 
                               "I'm looking forward making lots of game nights\n",
                                 "with guacamole as soon as this ends :P. Lots of love, *hiccups* xen"), 
         cex = 1.5, col = "pink")    
  } else if (name=="Maksim"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("Happy birthday, Zeynep! I wish you all the best. May your special day\n",
                               "be filled with memories and flowers, friendship and happy hours! "), 
         cex = 1.5, col = "blue")
    
  } else if (name=="Chau"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("Happy birthday, girl!!! I'm glad we have gotten much closer in the\n",
                               "weeks before Corona so now I have an extra person to chat with \n",
                               "during this whole quarantine thing. I hope your 24 is going to be\n",
                               "even more awesome than your 23 (which at least in 1 aspect is\n",
                               "looking up *wink wink*)! Love you <3-- from Chau, Nick and Siem --"), 
         cex = 1.5, col = "salmon2")
    
  } else if (name=="Emre"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("I am very happy that our paths crossed in this tiny city of a\n",
                               "distant country. At the very beginning, I didn't think of that\n",
                               "I could like you someday, but you changed it very quickly. Keske\n",
                               "su karantina isleri olmasaydi da kutlayabilseydik dogum gününü;\n",
                               "sen yine de kutlamisiz gibi hayal et ve kendine iyi ki dogdun\n", 
                               "sarkisi söyle! Love and hugs! Even though I smell like a soba.\n",
                               "from Ömer, Emre, and Can."), 
         cex = 1.5, col = "indianred")
    
  } else if (name=="Rodrigo"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("Happy birthday! Hope that despite the pandemic and isolation \n",
                               "you have a great time :D I wish you many more healthy years and\n",
                               "lots of success. See you when the whole shebang is over!"), 
         cex = 1.5, col = "red")
    
  } else if (name=="Nathalie"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("Happy birthday Zeynep! Hopefully, you can enjoy today and \n",
                               "maybe forget a while about the 'intelligent lockdown'. For now,\n", 
                               "enjoy the lovely Spring weather and let's hope we can meet",
                               "again soon! xxx Nathalie"), 
         cex = 1.5, col = "purple4")
    
  } else if (name=="Myrthe"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("Van harte gefeliciteerd met je verjaardag Zeynep! (I hope that\n", 
                               "Duolingo has more to it than olifanten in de kamer haha!) You \n",
                               "are only young once, but you can be immature for a lifetime! ;)\n",
                               "I wish you many more amazing years and I hope that you will \n",
                               "enjoy your special day regardless of these crazy times.\n",
                               "Happy birthday! "), 
         cex = 1.5, col = "green")
    
  } else if (name=="Naana"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("Happy birthday Zeynep. Have a blissful day and may this new\n",
                               "milestone come with more joy, happiness, success and all \n",
                               "that you've wished for"), 
         cex = 1.5, col = "maroon")
    
  } else if (name=="Bengi"){
    plot(x = 1,                 
         xlab = "X Label", 
         ylab = "Y Label",
         xlim = c(0, 100), 
         ylim = c(0, 100),
         main = "",
         type = "n")
    text(x = 50, y = 50, paste("Dogum gunun kutlu olsun!!! I'm glad that I had the chance to\n",
                               "get to know you more. Thank you for being one of the inspiring\n",
                               "and strong women in my life <3 I wish you an amazing new age \n",
                               "with lots of fun and growth. Hope to see and hug you soon :) "), 
         cex = 1.5, col = "palevioletred4")
    
  }
}



