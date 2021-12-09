## Anakysis.R
## functions to create plots and run statistical analysis
library(ggplot2)
library(patchwork)
library(tidyverse)
library(Dict)

# Initialize analysis related parameters
AnalysisInitialize <- function(analysisMode = " ")
{
    SAVETOFILE <<- TRUE # whether or not save the plots as image files
    DISPLAY <<- TRUE # whether or not show the plots on screen
    WIDTH <<- 16 # the width of the images in inches
    HEIGHT <<- 10 # the height of the images in inches
    
    # set the output folder to save the files
    if (analysisMode == "SensitivityAnalysis")
        FOLDER <<- "GraphsSens/" 
    else
        FOLDER <<- "Graphs/"
    
    # labelDict keeps the labels to be used in each scenario
    # created to make plotting functions more modular
    labelDict <<- Dict$new(RandomHunt = "", 
                          BatRangeDistance = "Distance: ",
                          BatRangeAngle = "Angle: ",
                          MothRangeDistance = "Distance: ",
                          StartleRange = "Range: ",
                          StartleRecovery = "Rec. time: ",
                          StartleLearning = "Learning: ")

    # read the simulation results to a data frame
    if (analysisMode == "SensitivityAnalysis")
        DF.Analysis <<- read.csv("Output/SensitivityStats.csv")
    else
        DF.Analysis <<- read.csv("Output/Stats.csv")
    
    # add a new column called GroupedMode merging the mode and variable values
    # eg. BatDistance 5, BatDistance 10, etc will be grouped under BatDistance
    DF.Analysis <<- cbind(DF.Analysis, DF.Analysis$Mode)
    names(DF.Analysis)[names(DF.Analysis) == 'DF.Analysis$Mode'] <- 'GroupedMode'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'BatRangeDistance'] <- 'BatRange'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'BatRangeAngle'] <- 'BatRange'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'StartleRange'] <- 'Startle'
    DF.Analysis$GroupedMode[DF.Analysis$Mode == 'StartleRecovery'] <- 'Startle'
    
    # convert Mode and GroupdeMode columns to factors to be able to use in histograms
    DF.Analysis <<- transform(DF.Analysis, Mode = factor(Mode, levels = unique(Mode)))
    DF.Analysis <<- transform(DF.Analysis, GroupedMode = factor(GroupedMode, levels = unique(GroupedMode)))
}

# Create the Victim% and HuntTime/Prey histograms
# for a given mode (scenario)
# subplot: whether the plots will be patched by patchwork
plotHistogram <- function(mode, label, subplot = FALSE)
{
    # the subfunction to generate facet labels
    facetLabel <- function(string) {
        if (label == "")
            return ("")
        paste(label, string)
    }
    
    # generate a sub data frame with the data of the current mode/scenario
    DF.Sub <- DF.Analysis %>% filter(Mode == mode & !is.na(HuntTime))
    
    pVictim <- DF.Sub %>%
        ggplot( aes(x = Victim, color = Variable, fill = Variable)) +
        geom_histogram(alpha = 0.6, bins = 30) +
        scale_fill_gradientn(colours = rainbow(4)) +
        theme(
            legend.position = "none",
            strip.text.x = element_text(size = 12)
            ) +
        xlim(0, 1) +
        labs(title = mode, x = "Victim %", y = "Count") 
        # if the plots will be patched by others, 
        # x-axis labels are displayed horizontally for legibility
        # y-axis is set to a common range for easier comparison
        if (subplot) 
            pVictim <- pVictim + 
                labs(title = mode, y = "") +
                facet_grid(~Variable, labeller = labeller( Variable = facetLabel)) +
                theme(plot.title = element_text(size = 30),
                        axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
                        axis.title = element_text(size = 30)) +
                ylim(0, 50)
        else 
            pVictim <- pVictim + facet_wrap(~Variable, labeller = labeller( Variable = facetLabel))
    
    pHunt <- DF.Sub %>%
        ggplot( aes(x = HuntTime / Prey, color = Variable, fill = Variable)) +
        geom_histogram(alpha = 0.6, bins = 50) +
        scale_fill_gradientn(colours = rainbow(4)) +
        theme(legend.position="none",
            strip.text.x = element_text(size = 12)
            ) +
        labs(title = mode, x = "HuntTime/Prey", y = "Count")
        # if the plots will be patched by others, 
        # x-axis labels are displayed horizontally for legibility
        # y-axis is set to a common range for easier comparison
        if (subplot)
            pHunt <- pHunt + 
                labs(title = mode, y = "") +
                facet_grid(~Variable, labeller = labeller( Variable = facetLabel)) +
                theme(plot.title = element_text(size = 30),
                    axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
                    axis.title = element_text(size = 30)) +
                ylim(0, 40)
        else
            pHunt <- pHunt + facet_wrap(~Variable, labeller = labeller( Variable = facetLabel))

    if (subplot == FALSE) # save or display only standalone plots
    {
        if (SAVETOFILE)
        {
            ggsave(filename = str_replace_all(paste(FOLDER, mode, "Victim.png"), " ", ""), plot = pVictim, width = WIDTH, height = HEIGHT)
            ggsave(filename = str_replace_all(paste(FOLDER, mode, "Hunt.png"), " ", ""), plot = pHunt, width = WIDTH, height = HEIGHT)
        }
        if (DISPLAY)
        {    
            print(pVictim)
            print(pHunt)
        }
    }
    # return the plots generated
    return (list(pVictim, pHunt))
}

# Iterate through the different modes/scenarios
# and create histograms for each
plotHistograms <- function()
{
    # plot individual histograms
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
        plotHistogram(mode, labelDict[mode])

    # generate the patched histograms
    pVictimFull <- NA # the list of Victim % histogram plots to be patched
    pHuntFull <- NA # the list of HuntTime/Prey histogram plots to be patched
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
    {
        if (mode=="RandomHunt") next
        plots <- plotHistogram(mode, labelDict[mode], subplot = TRUE)
        if (is.na(pVictimFull))
            pVictimFull <- plot(plots[[1]])
        else
            pVictimFull <- pVictimFull + plot(plots[[1]])
        if (is.na(pHuntFull))
            pHuntFull <- plot(plots[[2]])
        else
            pHuntFull <- pHuntFull + plot(plots[[2]])
    }

    if (SAVETOFILE)
    {
        ggsave(filename = str_replace_all(paste(FOLDER, "HistVictim.png"), " ", ""), plot = pVictimFull, width = 16, height = 10)
        ggsave(filename = str_replace_all(paste(FOLDER, "HistHunt.png"), " ", ""), plot = pHuntFull, width = 16, height = 10)
    }
    if (DISPLAY)
    {
        print(pVictimFull)
        print(pHuntFull)
    }
}

# Plot the scatter plots to show the data for each run
plotScatterPlot <- function(mode, label)
{
    facetLabel <- function(string) {
        if (label == "")
            return ("")
        paste(label, string)
    }
    
    DF.Sub <- DF.Analysis %>% filter(Mode == mode & !is.na(HuntTime))
    pHunt <- DF.Sub %>%
        ggplot( aes(x = Seq, y = HuntTime / Prey, color = as.factor(Variable)))  +
        labs(title = mode, x = "Runs") +
        geom_point(alpha=0.6) +
        facet_grid(~Variable, labeller = labeller(Variable = facetLabel)) +
        theme(
            legend.position="none",
            strip.text.x = element_text(size = 12)
        ) +
        scale_color_manual(values = rainbow(4))
    
    pVictim <- DF.Sub %>%
        ggplot( aes(x = Seq, y = Victim, color = as.factor(Variable)))  +
            labs(title = mode, x = "Runs", y = "Victim %") +
            geom_point(alpha=0.6) +
            facet_grid(~Variable, labeller = labeller(Variable = facetLabel)) +
        theme(
            legend.position="none",
            strip.text.x = element_text(size = 12)
        ) +
        scale_color_manual(values = rainbow(4))
    
    if (SAVETOFILE)
    {
        suppressMessages(ggsave("plot.png", ggplot()))
        ggsave(filename= str_replace_all(paste(FOLDER, mode, "SPHunt.png"), " ", ""), plot = pHunt, width = WIDTH, height = HEIGHT)
        ggsave(filename = str_replace_all(paste(FOLDER, mode, "SPVictim.png"), " ", ""), plot = pVictim, width = WIDTH, height = HEIGHT)
    }
    if (DISPLAY)
    {    
        print(pVictim)
        print(pHunt)
    }
}

# Iterate through the different modes/scenarios
# and create scatter plots for each
plotScatterplots <- function()
{
    modes = unique(DF.Analysis$Mode)
    for (mode in modes)
        plotScatterPlot(mode, labelDict[mode])
}

# Generate the scatter plot of full model conveying the changes 
# through out different modes/scenarios
plotFlows <- function()
{
    pHunt <- DF.Analysis %>% filter(!is.na(HuntTime)) %>% 
        ggplot( aes(x = Run, y = HuntTime/Prey, color = GroupedMode)) +
        scale_fill_gradientn(colours = rainbow(4)) +
        geom_point(alpha = 0.6) +
        theme(axis.title = element_text(size = 30),
              axis.text.x  = element_text(size = 20),
              legend.text = element_text(size = 20)) 
        
    pVictim <- DF.Analysis %>% filter(!is.na(HuntTime)) %>% 
        ggplot( aes(x = Run, y = Victim, color = GroupedMode)) +
        ylab("Victim %") +
        scale_fill_gradientn(colours = rainbow(4)) +
        geom_point(alpha = 0.6) +
        theme(axis.title = element_text(size = 30),
              axis.text.x  = element_text(size = 20),
              legend.text = element_text(size = 20)) 
        
    if (SAVETOFILE)
    {
        ggsave(filename = str_replace_all(paste(FOLDER, "FlowHunt.png"), " ", ""), plot = pHunt, width = WIDTH, height = HEIGHT)
        ggsave(filename = str_replace_all(paste(FOLDER, "FlowVictim.png"), " ", ""), plot = pVictim, width = WIDTH, height = HEIGHT)
    }
    if (DISPLAY)
    {    
        print(pVictim)
        print(pHunt)
    }    
}

# Generate the violin plot of full model conveying the changes 
# through out different modes/scenarios
plotViolins <- function()
{
    pHunt <- DF.Analysis %>% filter(!is.na(HuntTime)) %>% 
        ggplot( aes(x = GroupedMode, y = HuntTime/Prey, color = GroupedMode, fill = GroupedMode)) +
        xlab("") +
        geom_violin(alpha = 0.6) +
        theme(axis.title = element_text(size = 30),
              axis.text.x  = element_text(size = 12),
              legend.text = element_text(size = 20)) +
        stat_sum()
    
    pVictim <- DF.Analysis %>% filter(!is.na(HuntTime)) %>% 
        ggplot( aes(x = GroupedMode, y = Victim, color = GroupedMode, fill = GroupedMode)) +
        xlab("") +
        ylab("Victim %") +
        geom_violin(alpha = 0.6)+
        theme(axis.title = element_text(size = 30),
              axis.text.x  = element_text(size = 12),
              legend.text = element_text(size = 20)) +
        stat_sum()
    
    if (SAVETOFILE)
    {
        suppressMessages(ggsave("plot.png", ggplot()))
        ggsave(filename = str_replace_all(paste(FOLDER, "ViolinHunt.png"), " ", ""), plot = pHunt, width = WIDTH, height = HEIGHT)
        ggsave(filename = str_replace_all(paste(FOLDER, "ViolinVictim.png"), " ", ""), plot = pVictim, width = WIDTH, height = HEIGHT)
    }
    if (DISPLAY)
    {    
        print(pVictim)
        print(pHunt)
    }    
}

# Statistical analysis using one way ANOVA and TukeyHSD
# to compare whether different variable values cause a difference within a mode
# eg. Mode: BatRangeDistance, Variables: 5, 10, 15, 20
calcANOVA <- function(mode)
{
    DF.Sub <- DF.Analysis %>% filter(Mode == mode & !is.na(HuntTime))
    DF.Sub <- transform(DF.Sub, Variable = factor(Variable, levels = unique(Variable)))
    
    if (length(unique(DF.Sub$Variable)) > 1)
    {
        anovaResults <- aov(HuntTime/Prey ~ Variable, data = DF.Sub)
        print(paste(mode, 'HuntTime/Prey'))
        print(summary(anovaResults))
        p_value1 <- summary(anovaResults)[[1]][["Pr(>F)"]][[1]]
        # Run post-hoc analysis if the there is a difference between the groups
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
                png(str_replace_all(paste(FOLDER, mode, "HuntTimeTukey.png"), " ", ""))
                plot(TukeyResults, las = 1)
                dev.off()
            }
        }
        
        anovaResults <- aov(Victim ~ Variable, data = DF.Sub)
        print(paste(mode, 'Victim %'))
        print(summary(anovaResults))
        p_value2 <- summary(anovaResults)[[1]][["Pr(>F)"]][[1]]
        # Run post-hoc analysis if the there is a difference between the groups
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
                png(str_replace_all(paste(FOLDER, mode, "VictimTukey.png"), " ", ""))
                plot(TukeyResults, las = 1)
                dev.off()
            }
        }
        return (c(p_value1, p_value2))
    }
    return (c(999, 999))
}

# Iterate through the different modes/scenarios
# and run statistical analysis for each
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
    print(paste(mode, 'HuntTime/Prey'))
    print(summary(anovaResults))
    print(TukeyHSD(anovaResults))

    anovaResults <- aov(Victim ~ GroupedMode, data = DF.Analysis)
    print(paste(mode, 'Victim %'))
    print(summary(anovaResults))
    print(TukeyHSD(anovaResults))
}

# statistical analysis on the sensitivity analysis data
calcSensitivityANOVAs <- function()
{
    anovaResults <- aov(HuntTime/Prey ~ GroupedMode, data = DF.Analysis)
    print(summary(anovaResults))
    print(TukeyHSD(anovaResults))
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
            png(str_replace_all(paste(FOLDER, "SensitivityHuntTimeTuykey.png"), " ", ""), width = 800)
            par(mai = c(1, 4, 1, 1))
            plot(TukeyResults, las = 1)
            dev.off()
        }
    }
    
    anovaResults <- aov(Victim ~ GroupedMode, data = DF.Analysis)
    print(summary(anovaResults))
    print(TukeyHSD(anovaResults))
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
            png(str_replace_all(paste(FOLDER, "SensitivityVictimTukey.png"), " ", ""), width = 800)
            par(mai = c(1, 4, 1, 1))
            plot(TukeyResults, las = 1)
            dev.off()
        }
    }
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

RunSensitivityAnalysis <- function()
{
    AnalysisInitialize(analysisMode = 'SensitivityAnalysis')
    plotFlows()
    plotViolins()
    calcSensitivityANOVAs()
}

RunAnalysis()
RunSensitivityAnalysis()
