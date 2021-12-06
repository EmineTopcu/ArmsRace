library(ggplot2)
library(tidyverse)
library(Dict)

SAVETOFILE = TRUE
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

    DF.Analysis <<- read.csv("Output/StatsFull.csv")
    DF.Analysis <<- cbind(DF.Analysis, DF.Analysis$Mode)
    names(DF.Analysis)[names(DF.Analysis) == 'DF.Analysis$Mode'] <- 'GroupedMode'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'BatRangeDistance'] <- 'BatRange'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'BatRangeAngle'] <- 'BatRange'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'StartleRange'] <- 'Startle'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'StartleRecovery'] <- 'Startle'
    
    DF.Analysis <<- transform(DF.Analysis, Mode = factor(Mode, levels = unique(Mode)))
    DF.Analysis <<- transform(DF.Analysis, GroupedMode = factor(GroupedMode, levels = unique(GroupedMode)))
    DF.Analysis <<- transform(DF.Analysis, Variable = factor(Variable, levels = unique(Variable)))
}


plotHistogram <- function(mode, label, saveToFile = TRUE, display = TRUE)
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
        #scale_fill_distiller(palette = "Dark2") +
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
        labs(title = mode, x = "", y = "HuntTime") +
        facet_wrap(~Variable, labeller = labeller( Variable = facetLabel))

    if (saveToFile)
    {
        ggsave(filename = paste("Graphs/", mode, "Victim.jpg"), plot = pVictim)
        ggsave(filename = paste("Graphs/", mode, "Hunt.jpg"), plot = pHunt)
    }
    if (display)
    {    
        print(pVictim)
        print(pHunt)
    }
}

plotHistograms <- function(saveToFile = TRUE, display = TRUE)
{
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
        plotHistogram(mode, labelDict[mode], saveToFile, display)
}

# TODO: colors
plotScatterPlot <- function(mode, label, saveToFile = TRUE, display = TRUE)
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
        facet_grid(~Variable, labeller = labeller(Variable = facetLabel)) +
        theme(
            legend.position="none",
            strip.text.x = element_text(size = 12)
        ) 
    
    pVictim <- DF.Sub %>%
        ggplot( aes(x = Seq, y = Victim, color = Variable)) +
            labs(title = mode, x = "") +
            geom_point(alpha=0.6) +
            facet_grid(~Variable, labeller = labeller(Variable = facetLabel)) +
        theme(
            legend.position="none",
            strip.text.x = element_text(size = 12)
        ) 
    
    if (saveToFile)
    {
        ggsave(filename = paste("Graphs/", mode, "SPHunt.jpg"), plot = pHunt)
        ggsave(filename = paste("Graphs/", mode, "SPVictim.jpg"), plot = pVictim)
    }
    if (display)
    {    
        print(pVictim)
        print(pHunt)
    }
    
}

plotScatterplots <- function(saveToFile = TRUE, display = TRUE)
{
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
        plotScatterPlot(mode, labelDict[mode], saveToFile, display)
}

plotFlows <- function(saveToFile = TRUE, display = TRUE)
{
    pHunt <- DF.Analysis %>% 
        ggplot( aes(x = Run, y = HuntTime/Prey, color = GroupedMode)) +
        scale_fill_gradientn(colours = rainbow(4)) +
        geom_point(alpha = 0.6)

    pVictim <- DF.Analysis %>% 
        ggplot( aes(x = Run, y = Victim, color = GroupedMode)) +
        scale_fill_gradientn(colours = rainbow(4)) +
        geom_point(alpha = 0.6)

    if (saveToFile)
    {
        ggsave(filename = paste("Graphs/FlowHunt.jpg"), plot = pHunt)
        ggsave(filename = paste("Graphs/FlowVictim.jpg"), plot = pVictim)
    }
    if (display)
    {    
        print(pVictim)
        print(pHunt)
    }    
}

plotViolins <- function(saveToFile = TRUE, display = TRUE)
{
    pHunt <- DF.Analysis %>% 
        ggplot( aes(x = GroupedMode, y = HuntTime/Prey, color = GroupedMode, fill = GroupedMode)) +
        geom_violin(alpha = 0.6) +
        stat_sum()
    
    pVictim <- DF.Analysis %>% 
        ggplot( aes(x = GroupedMode, y = Victim, color = GroupedMode, fill = GroupedMode)) +
        geom_violin(alpha = 0.6) +
        stat_sum()
    
    if (saveToFile)
    {
        ggsave(filename = paste("Graphs/ViolinHunt.jpg"), plot = pHunt)
        ggsave(filename = paste("Graphs/ViolinVictim.jpg"), plot = pVictim)
    }
    if (display)
    {    
        print(pVictim)
        print(pHunt)
    }    
    
}

calcANOVA <- function(mode)
{
    DF.Sub <- DF.Analysis %>% filter(Mode == mode)
    if (length(unique(DF.Sub$Variable)) > 1)
    {
        anovaResults <- aov(HuntTime/Prey ~ Variable, data = DF.Sub)
        print(mode)
        print(summary(anovaResults))
        p_value <- summary(anovaResults)[[1]][["Pr(>F)"]][[1]]
        if (p_value <= 0.1)
        {
            TukeyResults <- TukeyHSD(anovaResults)
            if (DISPLAY)
            {
                pairwisePlot <- plot(TukeyResults, las = 1)
                print(pairwisePlot)
            }
            if (SAVETOFILE)
            {
                png(paste("Graphs/", mode, "Tukey.png"))
                plot(TukeyResults, las = 1)
                dev.off()
            }
        }
        return (p_value)
    }
    return (999)
}

calcANOVAs <- function()
{
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
        print(paste(mode, calcANOVA(mode)))
    
    anovaResults <- aov(HuntTime/Prey ~ GroupedMode, data = DF.Analysis)
    print(mode)
    print(summary(anovaResults))
    print(TukeyHSD(anovaResults))
}

RunAnalysis <- function()
{
    AnalysisInitialize()
    plotHistograms(saveToFile = SAVETOFILE, display = DISPLAY)
    plotScatterplots(saveToFile = SAVETOFILE, display = DISPLAY)
    plotFlows(saveToFile = SAVETOFILE, display = DISPLAY)
    plotViolins(saveToFile = SAVETOFILE, display = DISPLAY)
    calcANOVAs()
}

RunAnalysis()
