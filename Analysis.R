library(ggplot2)
library(tidyverse)
library(Dict)

SAVETOFILE = FALSE
DISPLAY = TRUE

AnalysisInitialize <- function()
{
    labelDict <<- Dict$new(RandomHunt = "", 
                          BatRangeDistance = "Distance: ",
                          BatRangeAngle = "Angle: ",
                          MothRangeDistance = "Distance: ",
                          StartleRange = "Range: ",
                          StartleRecovery = "Recovery time: ",
                          StartleLearning = "Learning: ")

    DF.Analysis <<- read.csv("Output/Stats.csv")
    DF.Analysis <<- cbind(DF.Analysis, DF.Analysis$Mode)
    names(DF.Analysis)[names(DF.Analysis) == 'DF.Analysis$Mode'] <- 'GroupedMode'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'BatRangeDistance'] <- 'BatRange'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'BatRangeAngle'] <- 'BatRange'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'StartleRange'] <- 'Startle'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'StartleRecovery'] <- 'Startle'
    
    DF.Analysis <<- transform(DF.Analysis, Mode = factor(Mode, levels = unique(Mode)))
    DF.Analysis <<- transform(DF.Analysis, GroupedMode = factor(GroupedMode, levels = unique(GroupedMode)))
}


plotHistogram <- function(mode, label)
{
    facetLabel <- function(string) {
        if (label == "")
            return ("")
        paste(label, string)
    }
    
    DF.Sub <- DF.Analysis %>% filter(Mode == mode)
    
    pVictim <- DF.Sub %>%
        ggplot( aes(x = Victim, color = Variable, fill = Variable)) +
        geom_histogram(alpha = 0.6, bins = 30) +
        scale_fill_gradientn(colours = rainbow(4)) +
        theme(
            legend.position = "none",
            strip.text.x = element_text(size = 12)
            ) +
        xlim(0, 1) +
        labs(title = mode, x = "", y = "Victim %") +
        facet_wrap(~Variable, labeller = labeller( Variable = facetLabel))

    pHunt <- DF.Sub %>%
        ggplot( aes(x = HuntTime / Prey, color = Variable, fill = Variable)) +
        geom_histogram(alpha = 0.6, bins = 50) +
        scale_fill_gradientn(colours = rainbow(4)) +
        theme(
            legend.position="none",
            strip.text.x = element_text(size = 12)
            ) +
        labs(title = mode, x = "", y = "HuntTime/Prey") +
        facet_wrap(~Variable, labeller = labeller( Variable = facetLabel))

    if (SAVETOFILE)
    {
        ggsave(filename = str_replace_all(paste("Graphs/", mode, "Victim.png"), " ", ""), plot = pVictim)
        ggsave(filename = str_replace_all(paste("Graphs/", mode, "Hunt.png"), " ", ""), plot = pHunt)
    }
    if (DISPLAY)
    {    
        print(pVictim)
        print(pHunt)
    }
}

plotHistograms <- function()
{
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
        plotHistogram(mode, labelDict[mode])
}

# TODO: colors
plotScatterPlot <- function(mode, label)
{
    facetLabel <- function(string) {
        if (label == "")
            return ("")
        paste(label, string)
    }
    
    DF.Sub <- DF.Analysis %>% filter(Mode == mode)

    pHunt <- DF.Sub %>%
        ggplot( aes(x = Seq, y = HuntTime / Prey, color = Variable))  +
        labs(title = mode, x = "") +
        geom_point(alpha=0.6) +
        scale_fill_gradientn(colours = rainbow(4)) +
        facet_grid(~Variable, labeller = labeller(Variable = facetLabel)) +
        theme(
            legend.position="none",
            strip.text.x = element_text(size = 12)
        ) 
    
    pVictim <- DF.Sub %>%
        ggplot( aes(x = Seq, y = Victim, color = Variable)) +
            labs(title = mode, x = "", y = "Victim %") +
            geom_point(alpha=0.6) +
            facet_grid(~Variable, labeller = labeller(Variable = facetLabel)) +
        theme(
            legend.position="none",
            strip.text.x = element_text(size = 12)
        ) 
    
    if (SAVETOFILE)
    {
        ggsave(filename= str_replace_all(paste("Graphs/", mode, "SPHunt.png"), " ", ""), plot = pHunt)
        ggsave(filename = str_replace_all(paste("Graphs/", mode, "SPVictim.png"), " ", ""), plot = pVictim)
    }
    if (DISPLAY)
    {    
        print(pVictim)
        print(pHunt)
    }
    
}

plotScatterplots <- function()
{
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
        plotScatterPlot(mode, labelDict[mode])
}

plotFlows <- function()
{
    pHunt <- DF.Analysis %>% 
        ggplot( aes(x = Run, y = HuntTime/Prey, color = GroupedMode)) +
        scale_fill_gradientn(colours = rainbow(4)) +
        geom_point(alpha = 0.6)

    pVictim <- DF.Analysis %>% 
        ggplot( aes(x = Run, y = Victim, color = GroupedMode)) +
        scale_fill_gradientn(colours = rainbow(4)) +
        geom_point(alpha = 0.6)

    if (SAVETOFILE)
    {
        ggsave(filename = str_replace_all(paste("Graphs/FlowHunt.png"), " ", ""), plot = pHunt)
        ggsave(filename = str_replace_all(paste("Graphs/FlowVictim.png"), " ", ""), plot = pVictim)
    }
    if (DISPLAY)
    {    
        print(pVictim)
        print(pHunt)
    }    
}

plotViolins <- function()
{
    pHunt <- DF.Analysis %>% 
        ggplot( aes(x = GroupedMode, y = HuntTime/Prey, color = GroupedMode, fill = GroupedMode)) +
        geom_violin(alpha = 0.6) +
        stat_sum()
    
    pVictim <- DF.Analysis %>% 
        ggplot( aes(x = GroupedMode, y = Victim, color = GroupedMode, fill = GroupedMode)) +
        geom_violin(alpha = 0.6) +
        stat_sum()
    
    if (SAVETOFILE)
    {
        ggsave(filename = str_replace_all(paste("Graphs/ViolinHunt.png"), " ", ""), plot = pHunt)
        ggsave(filename = str_replace_all(paste("Graphs/ViolinVictim.png"), " ", ""), plot = pVictim)
    }
    if (DISPLAY)
    {    
        print(pVictim)
        print(pHunt)
    }    
    
}

calcANOVA <- function(mode)
{
    DF.Sub <- DF.Analysis %>% filter(Mode == mode)
    DF.Sub <- transform(DF.Sub, Variable = factor(Variable, levels = unique(Variable)))
    
    if (length(unique(DF.Sub$Variable)) > 1)
    {
        anovaResults <- aov(HuntTime/Prey ~ Variable, data = DF.Sub)
        print(paste(mode, 'HuntTime/Prey'))
        print(summary(anovaResults))
        p_value1 <- summary(anovaResults)[[1]][["Pr(>F)"]][[1]]
        if (p_value1 <= 0.1)
        {
            TukeyResults <- TukeyHSD(anovaResults)
            if (DISPLAY)
            {
                pairwisePlot <- plot(TukeyResults, las = 1)
                print(pairwisePlot)
            }
            if (SAVETOFILE)
            {
                png(str_replace_all(paste("Graphs/", mode, "HuntTimeTukey.png"), " ", ""))
                plot(TukeyResults, las = 1)
                dev.off()
            }
        }
        
        anovaResults <- aov(Victim ~ Variable, data = DF.Sub)
        print(paste(mode, 'Victim %'))
        print(summary(anovaResults))
        p_value2 <- summary(anovaResults)[[1]][["Pr(>F)"]][[1]]
        if (p_value2 <= 0.1)
        {
            TukeyResults <- TukeyHSD(anovaResults)
            if (DISPLAY)
            {
                pairwisePlot <- plot(TukeyResults, las = 1)
                print(pairwisePlot)
            }
            if (SAVETOFILE)
            {
                png(str_replace_all(paste("Graphs/", mode, "VictimTukey.png"), " ", ""))
                plot(TukeyResults, las = 1)
                dev.off()
            }
        }
        return (c(p_value1, p_value2))
    }
    return (c(999, 999))
}

calcANOVAs <- function()
{
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
    {
        p_values <- calcANOVA(mode)
        print(mode)
        print(paste('HuntTime/Prey:', p_values[1]))
        print(paste('Victim %:', p_values[2]))
    }
    
    anovaResults <- aov(HuntTime/Prey ~ GroupedMode, data = DF.Analysis)
    print(mode)
    print(summary(anovaResults))
    print(TukeyHSD(anovaResults))
}

RunAnalysis <- function()
{
    AnalysisInitialize()
    plotHistograms()
    plotScatterplots()
    plotFlows()
    plotViolins()
    calcANOVAs()
}

RunAnalysis()
