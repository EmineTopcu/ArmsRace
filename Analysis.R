library(ggplot2)
library(tidyverse)

DF.Analysis <- read.csv("Output/StatsBatRange.csv")

plotParam <- function(mode, label)
{
    facetLabel <- function(string) {
        if (label == "")
            return ("")
        paste(label, string)
    }
    
    DF.Sub <- DF.Analysis %>%
    filter(startsWith(DF.Analysis$Mode, mode)) 
    
    pVictim <- DF.Sub %>% 
         ggplot( aes(x=Victim, color=Mode, fill=Mode)) +
         geom_histogram(alpha=0.6) +
         theme(
             legend.position="none",
             strip.text.x = element_text(size = 12)
         ) +
         xlab("") +
         ylab("Victim %") +
        facet_wrap(~Variable, labeller = labeller( Variable = facetLabel))
    ggsave(filename = paste(mode, "Victim.jpg"), plot = pVictim)
     
     pPrey <- DF.Sub %>%
         ggplot( aes(x=Prey, color=Mode, fill=Mode)) +
         geom_histogram(alpha=0.6) +
         theme(
             legend.position="none",
             strip.text.x = element_text(size = 12)
         ) +
         xlab("") +
         ylab("Prey Count") +
         facet_wrap(~Variable, labeller = labeller( Variable = facetLabel))
     ggsave(filename = paste(mode, "Prey.jpg"), plot = pPrey)

    pHunt <- DF.Sub %>%
        ggplot( aes(x=HuntTime, color=Mode, fill=Mode)) +
        geom_histogram(alpha=0.6) +
        theme(
            legend.position="none",
            strip.text.x = element_text(size = 12)
        ) +
        xlab("") +
        ylab("HuntTime") +
        facet_wrap(~Variable, labeller = labeller( Variable = facetLabel))
    ggsave(filename = paste(mode, "Hunt.jpg"), plot = pHunt)
    pHunt
    
    
}


plotParam("RandomHunt", "")
plotParam("BatRangeDistance", "Distance: ")
