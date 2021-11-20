source('analyses/shared.R')


#before removing timing outliers ----

#individual plots

plotSuccessRateIndiv <- function (df) {
  
  #compute success rates
  df <- df %>% 
    group_by(expName, participant, Group, Day, tasksNum) %>% 
    summarise(hitPercent = mean(HitMiss, rm.na = T)*100,
              n = n(),
              .groups = 'drop')
  
  pp <- unique(df$participant)
  
  pdf('./docs/SuccRate_indiv.pdf')
  par(mfrow = c(3,3))
  
  for (i in 1:length(pp)) {
    
    for (d in 1:2) {
      
      dataN <- df %>% 
        filter(participant %in% pp[i] & Day == d)
      
      taskV <- unique(dataN$expName)
      
      if (nrow(dataN) == 0) {
        next
      }
      
      plot(dataN$tasksNum, dataN$hitPercent,
           xlab = 'Blocks', ylab = 'Successful trials (%)',
           main = sprintf('%s\nParticipant %s - Day %s', taskV, pp[i], d),
           ylim = c(0, 100), type = 'b', pch = 20)
      if (d == 1) {
        abline(v = 4.5, col = 'red', lty = 2)
      } else if (d == 2) {
        abline(v = c(2.5, 3.5), col = 'red', lty = 2)
      }
      
    }
    
  }
  
  dev.off()
  
}


plotDeltaIndiv <- function (df) {
  
  df %<>% 
    dplyr::select(trialsNum, tasksNum, interceptDelta, participant, Group, Day, expName) %>% 
    mutate(trialsN = trialsNum + (tasksNum-1)*50)
  
  pp <- unique(df$participant)
  
  pdf('./docs/interceptDelta_indiv.pdf')
  par(mfrow = c(2,2))
  
  for (i in 1:length(pp)) {
    
    for (d in 1:2) {
      
      dataN <- df %>% 
        filter(participant %in% pp[i] & Day == d)
      
      taskV <- unique(dataN$expName)
      
      if (nrow(dataN) == 0) {
        next
      }
      
      plot(dataN$trialsN, dataN$interceptDelta,
           xlab = 'trials', ylab = "intercept delta",
           main = sprintf('%s\nParticipant %s - Day %s', taskV, pp[i], d),
           ylim = c(-1, 0.4), pch = 20, type = 'b',
           col = rgb(0,0,0, alpha = 0.5))
      abline(v = seq(50, 499, 50), col = 'blue', lty = 2)
      
    }
    
  }
  
  dev.off()
  
}



#after removing timing outliers ----

plotSuccessRate <- function(df, save.as = 'svg', WxL = c(12,7)) {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/SuccRate_avg.pdf', width=WxL[1], height=WxL[2])
  }
  
  #to plot lines representing changes in conditions (different depending on the day)
  vline <- list(data.frame(x = c(4.5),
                           ymin = rep(0, times = 1, each = 1),
                           ymax = rep(100, times = 1, each = 1)),
                data.frame(x = c(2.5, 3.5),
                           ymin = rep(0, times = 2, each = 1),
                           ymax = rep(100, times = 2, each = 1)))

  for (i in 1:length(expNames)) {

    plist <- list() #empty list to store several plots

    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>% 
      mutate(tasksNum = factor(tasksNum))
    
    # #get last block# for each version of the experiment
    # lastB <- max(as.numeric(dfdata$tasksNum))
    # 
    # #comparisons we want to look at
    # comps <- list(list(c('4','5'), c('4',lastB), c('5',lastB)), #day 1
    #               list(c('2','3'), c('2','4'))) #day 2

    #get the number of days
    d <- unique(df$Day)

    for (j in 1:length(d)) {

      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #to plot the different conditions
      #get maximum number of blocks day 1
      Nblocks <- max(as.numeric(dataD$tasksNum))
      #colors
      Cndcol <- c('#e6e7e8', '#a7a9ac', '#414042') #no_pert, trained_pert and untrained_pert
      #x coordinates
      sp <- 0.4
      if (j == 1) {
        CndXlim <- data.frame(nopert    = c(1-sp, 4.5),
                              trained   = c(4.5, Nblocks+sp),
                              untrained = c(NA, NA))
      } else {
        CndXlim <- data.frame(nopert    = c(3.5, 4+sp),
                              trained   = c(1-sp, 2.5),
                              untrained = c(2.5, 3.5))
      }
      
      #compute summary for that day
      summ_dataD <- dataD %>% 
        group_by(Group, tasksNum) %>% 
        summarise(mn_hitPercent = mean(hitPercent, na.rm = TRUE),
                  sd_hitPercent = sd(hitPercent, na.rm = TRUE),
                  .groups = 'drop')
      
      # #do stats
      # stats <- dataD %>%
      #   group_by(Group) %>%
      #   t_test(hitPercent ~ tasksNum, comparisons = comps[[j]]) %>% 
      #   adjust_pvalue(method = 'bonferroni') %>%
      #   add_significance('p.adj') %>%
      #   add_xy_position(x = 'tasksNum', group = 'Group', fun = 'mean_sd', #add group argument so that dodge works
      #                   dodge = 0.5, scales = 'free')
      # 
      # .GlobalEnv$statsR <- stats #output to global environment

      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)

      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)

      #make plots
      p <- ggplot() + 
        #individual success rates
        geom_jitter(data = dataD, aes(x = tasksNum, y = hitPercent,
                                      group = Group, color = Group),
                    alpha = 0.25, size = 2,
                    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9)) +
        #rectangles w/ conditions at the bottom
        geom_blank() + #otherwise continuous mess up w/ discrete x scale
        annotate('rect',
                 xmin = CndXlim$nopert[1], xmax = CndXlim$nopert[2],
                 ymin = 1, ymax = 12,
                 fill = Cndcol[1]) +
        annotate('text', x = CndXlim$nopert[1]+0.04, y = 7, hjust = 0, size = 2.5,
                 label = 'No\nperturbation', color = 'black', lineheight = 0.75) +
        annotate('rect',
                 xmin = CndXlim$trained[1], xmax = CndXlim$trained[2],
                 ymin = 1, ymax = 12,
                 fill = Cndcol[2]) +
        annotate('text', x = CndXlim$trained[1]+0.04, y = 7, hjust = 0, size = 2.5,
                 label = 'Trained\nperturbation', color = 'black', lineheight = 0.75) +
        annotate('rect',
                 xmin = CndXlim$untrained[1], xmax = CndXlim$untrained[2],
                 ymin = 1, ymax = 12,
                 fill = Cndcol[3]) +
        annotate('text', x = CndXlim$untrained[1]+0.04, y = 7, hjust = 0, size = 2.5,
                 label = 'Untrained\nperturbation', color = 'white', lineheight = 0.75) +
      #success rates averaged across groups
      geom_line(data = summ_dataD, aes(x = tasksNum, y = mn_hitPercent,
                                       group = Group, color = Group),
                position = position_dodge(0.4), size = 0.6) +
        geom_point(data = summ_dataD, aes(x = tasksNum, y = mn_hitPercent,
                                          group = Group, color = Group),
                   position = position_dodge(0.4), size = 2) +
        #error bar with mean +/- SD
        geom_errorbar(data = summ_dataD, aes(x = tasksNum, y = mn_hitPercent,
                                             ymin = mn_hitPercent - sd_hitPercent,
                                             ymax = mn_hitPercent + sd_hitPercent,
                                             group = Group, color = Group),
                      position = position_dodge(0.4), size = 0.6, width = 0.2) +
        geom_segment(data = vline[[j]],
                     mapping = aes(x = x, y = ymin, xend = x, yend = ymax),
                     color = 'black', linetype = 'dashed', size = 0.4) +
        
        theme_classic_article() + 
        #add lots of space under legend box to manually add stats
        theme(legend.box.spacing = unit(c(0,0,80,0), 'pt')) +
        scale_color_discrete(name = 'Group', labels = lbl) + 
        scale_y_continuous(breaks = seq(0, 100, 20), expand = c(0, 0)) + 
        labs(title = title, x = 'Blocks', y = 'Successful trials (%)') +
        coord_cartesian(clip = 'off') #make drawings unconfined to the plot panel

      plist[[j]] = p

    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, widths = c(2,1.2),
                      labels = c('A','B'), font.label = list(size = 18))
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/SuccRate_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=WxL[1], height=WxL[2])
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotScore <- function(df, save.as = 'svg', WxL = c(15,7)) {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/ScorePoints_avg.pdf', width=WxL[1], height=WxL[2])
  }
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum),
             Gp.Pts = paste(Group, points)) #add column to specify different color shades
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      # #keep only last practice block for day 1
      # if(j == 1) {dataD <- dataD %>% filter(as.numeric(tasksNum) > 3)}
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #get number of blocks in current task version
      Nblock = max(as.numeric(dataD$tasksNum))
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      names(lbl) <- c('train_horiz', 'train_tilt')
      
      #set colors for groups and points
      GpPtsCol <- c('#FDD1CE', '#FAA49E', '#F8766D', #trained_horiz 0, 5 and 10 points
                    '#99FCFF', '#00DEE6', '#00BFC4') #trained_tilt 0, 5 and 10 points
      lbls <- rep(levels(dataD$points), time = 2)
      
      # #make plots
      # p <- ggplot(dataD, aes(x = tasksNum, y = percent, fill = points)) +
      #   geom_bar(position = 'fill', stat = 'identity') + 
      #   facet_wrap(~ Group, labeller = labeller(Group = lbl)) + 
      #   
      #   theme_classic_article() + 
      #   #add margin to the bottom of plot title
      #   theme(plot.title = element_text(margin = margin(b = 20, unit = 'pt'))) +
      #   scale_fill_grey() + 
      #   scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20),
      #                      #expand y axis only for the upper limit
      #                      expand = expansion(mult = c(0,0.03))) +
      #   labs(title = title, x = 'Blocks', y = 'Proportion of points earned (%)',
      #        fill = 'Points earned')
      
      #make plots (one layer for each group)
      widthCol <- 0.4
      dodge <- 0.22
      p <- ggplot(dataD %>% filter(Group == 'train_horiz'), 
                  aes(x = as.numeric(tasksNum) - dodge, y = percent, fill = Gp.Pts)) +
        geom_bar(position = 'fill', stat = 'identity', width = widthCol) + 
        geom_bar(data = dataD %>% filter(Group == 'train_tilt'),
                 aes(x = as.numeric(tasksNum) + dodge, y = percent, fill = Gp.Pts),
                 position = 'fill', stat = 'identity', width = widthCol) +
        
        theme_classic_article() + 
        #add margin to the bottom of plot title
        theme(plot.title = element_text(margin = margin(b = 20, unit = 'pt'))) +
        #specify color shades
        scale_fill_manual(name = 'Points earned', labels = lbls, values = GpPtsCol) +
        scale_y_continuous(breaks = seq(0, 1, 0.2), labels = seq(0, 100, 20),
                           expand = expansion(mult = 0)) +
        scale_x_continuous(breaks = seq(1, Nblock, 1)) + 
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        labs(title = '', x = 'Blocks', y = 'Proportion of points earned (%)')
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, widths = c(2,1.2),
                      labels = c('A','B'), font.label = list(size = 18),
                      common.legend = T, legend = 'bottom')
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/Score_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=WxL[1], height=WxL[2])
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotDelta <- function(df, whichY = 'mean', save.as = 'svg', WxL = c(15,7)) {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/interceptDelta_avg.pdf', width=WxL[1], height=WxL[2])
  }
  
  #to represent trials w/ changes in conditions (different depending on the day)
  vline <- list(c(200),
                c(100, 150))
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #to plot paddle length
    pdlL <- c(-params$paddle$x[i], params$paddle$x[i])
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #to plot the size of the perturbation
    maxtrialsD1 <- max(dfdata[which(dfdata$Day == 1), ]$trialsN)
    pertPlt <- data.frame(xstart = c(200, 0), 
                          xend = c(maxtrialsD1, 150), 
                          ystart = rep(max(perturb[[expNames[i]]]$perturb_size)*-1, times = 2), 
                          yend = rep(min(perturb[[expNames[i]]]$perturb_size)*-1, times = 2))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #set y variable for plot (iDelta_mn or iDelta_med)
      if (whichY == 'mean') {
        Yplot <- sym('iDelta_mn')
      } else {
        Yplot <- sym('iDelta_med')
      }
      
      #make plots
      p <- ggplot(dataD, aes(x = trialsN, y = !!Yplot, 
                             color = Group, fill = Group, 
                             group = interaction(Group, tasksNum))) + #group = interaction to dissociate between blocks and groups
        annotate('rect',
                 xmin = pertPlt$xstart[j], xmax = pertPlt$xend[j],
                 ymin = pertPlt$ystart[j], ymax = pertPlt$yend[j],
                 fill = 'grey90') + 
        annotate('text', x = pertPlt$xstart[j], y = pertPlt$yend[j] - 0.012,
                 label = 'Perturbation size', color = 'grey50', hjust = -0.05) + 
        annotate('text', x = 0, y = pdlL[2] + 0.015, label = 'Paddle border', 
                 color = 'black', hjust = +0.1) + 
        geom_hline(yintercept = pdlL, linetype = 'dotted') +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = vline[[j]], linetype = 'dashed', size = 0.4) +
        geom_point(alpha = 0.25, size = 2.6) +
        geom_smooth(se = F, alpha = 0.35, span = 0.9, size = 0.8) +
        
        theme_classic_article() +  
        scale_color_discrete(name = 'Group', labels = lbl) +
        scale_fill_discrete(name = 'Group', labels = lbl) +
        scale_x_continuous(breaks = seq(0, 500, 50)) +
        labs(title = title, x = 'Trials', y = 'Distance between paddle and ball (a.u.)') +
        ylim(-0.26, 0.15)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, widths = c(2,1.1), #width fig1 = 2*fig2 to have same x scale
                      labels = c('A','B'), font.label = list(size = 18))

    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/interceptDelta_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=WxL[1], height=WxL[2])
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }

}


plotDelta_stats <- function(df, whichVersion, WxL = c(10,5)) {
  
  #trialsN to keep for each day
  kp_trialsN <- list(c(200, 201, 250), #day1
                     c(100, 101, 150, 151, 200)) #day2
  
  dfdata <- df %>% 
    dplyr::select(expName, participant, Group, Day, tasksNum, trialsNum, interceptDelta) %>% 
    filter(expName == whichVersion) %>% 
    mutate(trialsN = trialsNum + (tasksNum-1)*50) %>% 
    filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
    convert_as_factor(trialsN) %>% 
    split(., .$Day)
  
  dfdata <- mapply(function(x, y) filter(x, x$trialsN %in% y),
                    x = dfdata, y = kp_trialsN, SIMPLIFY = F)
  
  #to plot paddle length
  pdlL <- c(-params$paddle[whichVersion, 'x'], params$paddle[whichVersion, 'x'])
  
  #to plot the size of the perturbation
  pertPltX <- list(c(1.5, +Inf),
                   c(-Inf, 3.5))
  pertPltY <- data.frame(ystart = max(perturb[[whichVersion]]$perturb_size)*-1,
                        yend = min(perturb[[whichVersion]]$perturb_size)*-1)
  
  #get number of participants in each group (n.gp)
  n.gp <- demoG$N %>%
    filter(expName == whichVersion) %>%
    split(., .$Day)
  
  #set title and labels
  title <- lapply(n.gp, function(x) {sprintf('Session %s', unique(x$Day))})
  lbl <- lapply(n.gp, function(x) {sprintf('%s\n(N = %s)', Gps, x$n)})
  xlbl <- list(c('No perturbation\nLast trial',
                 'Trained perturbation\nTrial 1',
                 'Trained perturbation\nTrial 50'),
               c('Trained perturbation\nLast trial',
                 'Untrained perturbation\nTrial 1',
                 'Untrained perturbation\nTrial 50',
                 'No perturbation\nTrial 1',
                 'No perturbation\nTrial 50'))
  
  #make plots
  mkplot <- function(x, lgd, ttl, newXlbl, pertX) {
    
    #compute means per group
    x_gp <- x %>% 
      group_by(trialsN, Group) %>% 
      summarise(mn_intercept = mean(interceptDelta, na.rm = TRUE),
                sd_intercept = sd(interceptDelta, na.rm = TRUE),
                .groups = 'drop')
    
    ggplot(x, aes(x = trialsN, y = interceptDelta, color = Group)) +
      geom_hline(yintercept = pdlL, linetype = 'dotted', size = 0.3) +
      geom_hline(yintercept = 0, size = 0.3) +
      annotate('rect', 
               xmin = pertX[1], xmax = pertX[2],
               ymin = pertPltY$ystart, ymax = pertPltY$yend,
               fill = 'grey90') +
      #individual data
      geom_jitter(alpha = 0.25, size = 2.5, 
                  position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.9)) +
      #average across participants in the same group
      geom_pointrange(data = x_gp, aes(y = mn_intercept, 
                                       ymin = mn_intercept - sd_intercept,
                                       ymax = mn_intercept + sd_intercept),
                      position = position_dodge(0.4), 
                      size = 0.7, fatten = 3.5, shape = 22, fill = 'white') + 
      
      theme_classic_article() + 
      theme(legend.position = 'none',
            plot.margin = unit(c(100,15,0,15), 'pt')) + #increase top margin to manually add stats
      # scale_color_discrete(name = 'Group', labels = lgd) + 
      scale_x_discrete(labels = paste0('Trial\n', x$trialsN)) + 
      scale_y_continuous(limits = c(-0.45, 0.3), breaks = seq(-0.4, 0.2, 0.2)) + 
      labs(title = NULL,
           x = NULL, y = 'Distance between paddle and ball (a.u.)')
  }
  
  # plist <- lapply(dfdata, lbl, mkplot)
  plist <- mapply(mkplot, dfdata, lbl, title, xlbl, pertPltX, SIMPLIFY = FALSE)
  
  #save plots as svg
  # plot <- ggarrange(plist[[1]], NULL, plist[[2]], ncol = 3, widths = c(0.7,0.6,1),
  #                   labels = c('C','','D'), font.label = list(size = 18))
  plot <- ggarrange(plotlist = plist, ncol = 2, widths = c(0.7,1),
                    labels = c('C','D'), font.label = list(size = 18))
  
  #filename to save figure
  fname = sprintf('./docs/figures/stats_interceptDelta_%s.svg', whichVersion)
    ggsave(file=fname, plot=plot, width=WxL[1], height=WxL[2])
  
}


plotDeltaShift_blocks <- function(df, save.as = 'svg', WxL = c(14,10)) {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/interceptDelta-shift_blocks.pdf', width=WxL[1], height=WxL[2])
  }
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #to plot paddle length
    pdlL <- c(-params$paddle$x[i], params$paddle$x[i])
    #to plot central part of paddle
    pdlC <- c(-params$paddle$MaxPts[i], params$paddle$MaxPts[i])
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      p <- ggplot(dataD, aes(x = interceptDelta, y = fct_rev(tasksNum), fill = Group)) +
        #fct_rev to have descending order in y axis
        geom_vline(xintercept = pdlL, linetype = 'solid') +
        geom_vline(xintercept = pdlC, linetype = 'dashed') +
        geom_density_ridges(alpha = 0.5, size = 0.4, scale = 1) +
        theme_ridges() + 
        theme(legend.position = 'bottom') +
        scale_fill_discrete(name = 'Group', labels = lbl) +
        labs(title = title,
             x = 'Distance between paddle and ball (a.u.)', y = 'Blocks') +
        xlim(-0.5, 0.5)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1)
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/interceptDelta-shift_blocks_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=WxL[1], height=WxL[2])
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotDeltaShift_trials <- function(df, save.as = 'svg', WxL = c(15,10)) {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/interceptDelta-shift_trials.pdf', width=WxL[1], height=WxL[2])
  }
  
  #to plot trials w/ changes in conditions (different depending on the day)
  tr <- list(c(191:200, 201:210),
             c(1:10, 101:110, 151:160))
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #to plot paddle length
    pdlL <- c(-params$paddle$x[i], params$paddle$x[i])
    #to plot central part of paddle
    pdlC <- c(-params$paddle$MaxPts[i], params$paddle$MaxPts[i])
    
    #to plot the size of the perturbation
    pertPlt <- data.frame(start = max(perturb[[expNames[i]]]$perturb_size)*-1, 
                          end = min(perturb[[expNames[i]]]$perturb_size)*-1)
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      # filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
      mutate(trialsN = trialsNum + (tasksNum-1)*50) %>% 
      mutate(trialsN = factor(trialsN))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j] & trialsN %in% tr[[j]])
      
      #perturbation schedule
      if (j==1) {
        pert_sched <- list('No perturbation (Block 4)','Trained perturbation (Block 5)')
      } else {
        pert_sched <- list('Trained perturbation (Block 2)', 
                           'Untrained perturbation (Block 3)', 
                           'No perturbation (Block 4)')
      }
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s (N = %s)', d[j], n)
      } else {
        title <- sprintf('%s, Session %s (N = %s)', expNames[i], d[j], n)
      }
      
      #add perturbation schedule to title
      title_pert <- lapply(pert_sched, function(x) sprintf('%s\n%s', title, x))
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      dataD %<>% split(., .$tasksNum)
      
      mkplot <- function(x, ttl) {
        taskN <- x$tasksNum
        ggplot(x, aes(x = interceptDelta, y = trialsN, fill = Group)) +
          #wrap trialsN with fct_rev() to have descending order in y axis if no coord_flip
          geom_vline(xintercept = pdlL, linetype = 'solid', size = 0.4) +
          geom_vline(xintercept = pdlC, linetype = 'dashed') +
          {if ((j==1 & taskN > 4) || (j==2 & taskN < 4))
            geom_rect(xmin = pertPlt$start, xmax = pertPlt$end, ymin = -Inf, ymax = Inf,
                      fill = 'grey85')} +
          geom_density_ridges(alpha = 0.5, size = 0.4, scale = 1.2) +
          coord_flip() +
          
          theme_ridges(font_size = 12, center_axis_labels = TRUE) +
          theme(plot.margin = unit(c(5,15,10,15), 'pt'),
                plot.title = element_text(margin = margin(0,0,20,0, 'pt')),
                axis.title.y.left = element_text(margin = margin(0,8,0,0, 'pt')),
                axis.text = element_text(color = 'grey40')) +
          scale_fill_discrete(name = 'Group', labels = lbl) +
          xlim(-0.5, 0.4) +
          labs(title = ttl, x = 'Distance between paddle and ball (a.u.)', y = 'Trials')
      }

      p <- mapply(mkplot, dataD, title_pert, SIMPLIFY = F)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(
      ggarrange(plotlist = plist[[1]], ncol = 3,
                common.legend = T, legend = 'bottom',
                labels = c('A','B'), font.label = list(size = 18)), #1st row for day 1
      #add dummy 2nd row for spacing
      NULL,
      ggarrange(plotlist = plist[[2]], ncol = 3,
                common.legend = T, legend = 'bottom',
                labels = c('C','D','E'), font.label = list(size = 18)), #3rd row for day 2
      nrow = 3, heights = c(1, 0.05, 1)
    )

    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/interceptDelta-shift_trials_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=WxL[1], height=WxL[2])
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotDeltaRatio <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/interceptDeltaRatio_avg.pdf', width=17, height=8)
  }
  
  #to represent trials w/ changes in conditions (different depending on the day)
  vline <- list(c(200),
                c(100, 150))
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      # #to plot the size of the perturbation
      # if (j == 1) {
      #   pertPlt = list(x = c(200, 500), y = c(-0.22367, -0.22367))
      # } else {
      #   pertPlt = list(x = c(0, 150), y = c(-0.22367, -0.22367))
      # }
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      p <- ggplot(dataD, aes(x = trialsN, y = iDeltaR_mn, color = Group,
                             group = interaction(Group, tasksNum))) + #group = interaction to dissociate between blocks and groups
        geom_hline(yintercept = c(1), linetype = 'dotted') +
        geom_vline(xintercept = vline[[j]], linetype = 'dashed', size = 0.4, alpha = 0.7) +
        # geom_segment(x = pertPlt$x[1], y = pertPlt$y[1], xend = pertPlt$x[2], yend = pertPlt$y[2],
        #              linetype = 'longdash', color = 'black') +
        geom_point(alpha = 0.3, size = 2.8) +
        geom_smooth(se = T, alpha = 0.35, span = 0.9, size = 1) +
        
        theme_classic() +
        theme(legend.position = 'top',
              text = element_text(size = 15),
              plot.margin = unit(c(0,2,0,2), 'lines')) + #add margins around individual plots (top, right, bottom, left)
        scale_color_discrete(name = 'Group', labels = lbl) +
        scale_x_continuous(breaks = seq(0, 500, 50)) +
        labs(title = title, x = 'Trials', y = 'Distance between paddle and ball (a.u.)') +
        ylim(0.5, 1.3)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1, widths = c(2,1)) #width fig1 = 2*fig2 to have same x scale
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/interceptDeltaRatio_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=17, height=8)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotTimingMidScreen <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/TimingMidScreen_avg.pdf', width=15, height=7)
  }
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      # #make plots
      # p <- ggplot(dataD, aes(x = tasksNum, y = trialMouse.time, color = Group)) +
      #   geom_boxplot() +
      #   theme_classic() +
      #   theme(legend.position = 'top') +
      #   scale_color_discrete(name = 'Group', labels = lbl) +
      #   labs(title = title, x = 'Block #', 
      #        y = 'Time to cross mid-screen (s) (hit trials only)') +
      #   ylim(0.1, 0.9)
      
      #make plots relative to bounce time
      p <- ggplot(dataD, aes(x = tasksNum, y = timeReBounce, color = Group)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_boxplot() +
        theme_classic() +
        theme(legend.position = 'top') +
        scale_color_discrete(name = 'Group', labels = lbl) +
        labs(title = title, x = 'Block #', 
             y = 'Time to cross middle screen re: bounce time (s) (hit trials only)') +
        ylim(-0.4, 1)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1)
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/TimingMidScreen_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=15, height=7)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotTimingWithinIntercept <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/TimingWithinIntercept_avg.pdf', width=15, height=7)
  }
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      # #make plots
      # p <- ggplot(dataD, aes(x = tasksNum, y = trialMouse.time, color = Group)) +
      #   geom_boxplot() +
      #   theme_classic() +
      #   theme(legend.position = 'top') +
      #   scale_color_discrete(name = 'Group', labels = lbl) +
      #   labs(title = title, x = 'Block #', 
      #        y = 'Time to reach intercept zone (s) (hit trials only)') +
      #   ylim(0.1, 0.9)
      
      #make plots with timing relative to bounce time
      p <- ggplot(dataD, aes(x = tasksNum, y = timeReBounce, color = Group)) +
        geom_hline(yintercept = 0, linetype = 'dashed') +
        geom_boxplot() +
        theme_classic() +
        theme(legend.position = 'top') +
        scale_color_discrete(name = 'Group', labels = lbl) +
        labs(title = title, x = 'Block #', 
             y = 'Time to reach intercept zone re: bounce time (s) (hit trials only)') +
        ylim(-0.4, 1)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1)
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/TimingWithinIntercept_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=15, height=7)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotTimingWithinInterceptHist <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/TimingWithinInterceptHist.pdf', width=15, height=7)
  }
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots with timing relative to bounce time
      p <- ggplot(dataD, aes(x = timeReBounce, fill = Group)) + 
        geom_bar() +
        scale_x_binned(n.breaks = 20) + 
        facet_grid(Group ~ tasksNum) + 
        theme_bw() +
        theme(legend.position = 'top') +
        # scale_color_discrete(name = 'Group', labels = lbl) +
        labs(title = title, 
             x = 'Time to reach intercept zone re: bounce time (s) (hit trials only)')
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1)
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/TimingWithinInterceptHist_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=15, height=7)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotDistTraveled <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/DistTraveled.pdf', width=17, height=10)
  }
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)

      #prepare data to make a density plot
      get_density <- function(x, y, ...) {
        density_out <- kde2d(x, y, ...)
        int_x <- findInterval(x, density_out$x)
        int_y <- findInterval(y, density_out$y)
        comb_int <- cbind(int_x, int_y)
        return(density_out$z[comb_int])
      }

      density_map <- dataD %>%
        mutate(density = get_density(timeReConnect, dist_traveled, n = 100))
      
      # #make plots
      # p <- ggplot(dataD, aes(x = timeReConnect, y = dist_traveled, color = Group)) +
      #   geom_point(alpha = 0.005, size = 1) +
      #   facet_grid(Group ~ tasksNum) +
      #   theme_bw() +
      #   theme(legend.position = 'top') +
      #   scale_color_discrete(name = 'Group', labels = lbl) +
      #   scale_x_continuous(breaks = seq(-1,0,0.5)) +
      #   labs(title = title, x = 'Time relative to connect time (s)',
      #        y = '% of distance traveled') +
      #   ylim(-25, 50)
      
      #make density plots
      p <- ggplot(density_map, aes(x = timeReConnect, y = dist_traveled, color = density)) +
        geom_point(alpha = 0.08, size = 1) +
        facet_grid(Group ~ tasksNum) +
        theme_bw() +
        theme(legend.position = 'top',
              text = element_text(size = 15),
              plot.margin = unit(c(0,2,0,2), 'lines')) + #add margins around individual plots (top, right, bottom, left)
        scale_color_gradient(low = 'red', high = 'yellow',
                             limits = c(0, 0.7), breaks = seq(0.2, 0.8, by = 0.2)) +
        scale_x_continuous(breaks = seq(-1,0,0.5)) +
        labs(title = title, x = 'Time relative to connect time (s)',
             y = '% of distance traveled') +
        ylim(-25, 50)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1, widths = c(2,1.2)) #width fig1 = 2*fig2 to have same x scale
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/DistTraveled_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=17, height=10)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotNmvtTime <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/NmvtTime.pdf', width=17, height=10)
  }
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    dfdata <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- dfdata %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Session %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      p <- ggplot(dataD, aes(x = timeReConnect, fill = Group)) +
        # geom_bar() +
        geom_histogram(binwidth = 0.05) +
        facet_grid(Group ~ tasksNum) +
        theme_bw() +
        theme(legend.position = 'top') +
        scale_color_discrete(name = 'Group', labels = lbl) +
        # scale_x_binned(n.breaks = 80) + 
        scale_x_continuous(breaks = seq(-1,0,0.5)) +
        labs(title = title, x = 'Time relative to connect time (s)',
             y = 'Number of cursor movements')
      #   ylim(-25, 50)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1, widths = c(2,1.2)) #width fig1 = 2*fig2 to have same x scale
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/NmvtTime_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=17, height=10)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


#custom theme for ggplot ----

theme_classic_article <- function(){
  
  theme_classic() %+replace%
    
    theme(
      
      #text elements
      text = element_text(size = 12),
      
      #legend elements
      legend.position = 'top',
      
      #margins around plot (top, right, bottom, left)
      plot.margin = unit(c(10,15,0,15), 'pt'),
      
      #axes elements
      axis.title.x.bottom = element_text(margin = margin(10,0,0,0, 'pt')), #add top margin to x label
      axis.title.y.left = element_text(margin = margin(0,10,0,0, 'pt')), #add right margin to y label
      axis.text   = element_text(color = 'grey40'),
      axis.line   = element_line(color = 'grey40'),
      axis.ticks  = element_line(color = 'grey40'),
      axis.ticks.length = unit(5, 'pt')
            
    )
  
}


#methods plots ----

plotPerturbations <- function (dfpert) {
  
  pert <- dfpert %>% 
    mutate(alphaChoice = alphaChoice*-1,
           notperturb = notperturb*-1,
           perturb = perturb*-1)
  
  wallX <- 0.5 #1/2 length wall
  wallY <- 0.0125 #1/2 height wall
  paddleX <- params$paddle$x[3] #1/2 length paddle
  paddleY <- params$paddle$y[3] #1/2 height paddle
  ballR <- params$ball/2 #radius ball
  
  wallPosY <- 0.3
  paddlePosY <- -0.3
  paddlePosX <- -0.5
  
  pertRad <- pi/20
  wallCoord <- data.frame(X = c(-wallX, wallX),
                          Y = rep(wallPosY, 2)) %>% 
    mutate(Xrot = (X-0)*cos(pertRad) - (Y-wallPosY)*sin(pertRad) + 0,
           Yrot = (X-0)*sin(pertRad) + (Y-wallPosY)*cos(pertRad) + wallPosY)
  
  #make plot
  plt <- ggplot() + 
    annotate('rect', 
             xmin = -wallX, xmax = +wallX,
             ymin = wallPosY - wallY, ymax = wallPosY + wallY,
             fill = 'grey50') + #draw wall
    annotate('rect', 
             xmin = paddlePosX - paddleX, xmax = paddlePosX + paddleX,
             ymin = paddlePosY - paddleY, ymax = paddlePosY + paddleY,
             fill = 'grey80') + #draw paddle
    geom_segment(data = wallCoord, aes(x = Xrot[1], y = Yrot[1],
                                       xend = Xrot[2], yend = Yrot[2]),
                 color = 'grey70') + #draw wall tilted
    geom_circle(data = pert, aes(x0 = notperturb*-1, y0 = rep(paddlePosY, nrow(pert)),
                                 r = ballR), 
                lwd = 0.3) + #draw ball launch positions
    geom_circle(data = pert, aes(x0 = notperturb, y0 = rep(paddlePosY, nrow(pert)),
                                 r = ballR), 
                lwd = 0.3) + #draw ball final positions (no perturbation)
    geom_circle(data = pert, aes(x0 = perturb, y0 = rep(paddlePosY, nrow(pert)),
                                 r = ballR), 
                lwd = 0.3, color = 'grey50') + #draw ball final positions (perturbation)
    geom_segment(data = pert, aes(x = notperturb[4]*-1, 
                                  y = paddlePosY + 0.025, 
                                  xend = 0, yend = wallPosY - wallY),
                 lwd = 0.2) + 
    geom_segment(data = pert, aes(x = notperturb[4], 
                                  y = paddlePosY + 0.025, 
                                  xend = 0, yend = wallPosY - wallY),
                 lwd = 0.2) + 
    geom_segment(data = pert, aes(x = perturb[4], 
                                  y = paddlePosY + 0.025, 
                                  xend = 0, yend = wallPosY - wallY),
                 lwd = 0.2, color = 'grey50') + 
    theme_classic() + 
    lims(x = c(-0.8, 0.8), y = c(-0.5, 0.5)) + 
    coord_fixed(ratio = 1)
  
  #save plot
  fname = './docs/plotPerturbations.eps'
  ggsave(file=fname, plot=plt, device='eps')
    
  
}


# plotOutliersTime <- function(df, save.as = 'svg') {
#   
#   #show a message if one of the arguments is missing?
#   
#   #filename to save figure
#   if (save.as == 'pdf') {
#     pdf('./docs/OutliersTime.pdf', width=15, height=7)
#   }
#   
#   for (i in 1:length(expNames)) {
#     
#     plist <- list() #empty list to store several plots
#     
#     #get data for each version of the experiment
#     dfdata <- df %>%
#       filter(expName == expNames[i]) %>%
#       mutate(tasksNum = factor(tasksNum))
#     
#     #get the number of days
#     d <- unique(df$Day)
#     
#     for (j in 1:length(d)) {
#       
#       #get data for each day
#       dataD <- dfdata %>%
#         filter(Day == d[j])
#       
#       #get number of participants for each task version (n) and each group (n.gp)
#       n.gp <- demoG$N %>%
#         filter(expName == expNames[i] & Day == d[j]) %>%
#         .$n
#       n <- sum(n.gp)
#       
#       #set title and labels
#       if (save.as == 'svg') {
#         title <- sprintf('Session %s\nN = %s', d[j], n)
#       } else {
#         title <- sprintf('%s, Session %s\nN = %s', expNames[i], d[j], n)
#       }
#       
#       lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
#       
#       #make plots
#       p <- ggplot(dataD, aes(x = tasksNum, y = trialMouse.time, color = Group)) +
#         geom_boxplot() +
#         theme_classic() +
#         theme(legend.position = 'top') +
#         scale_color_discrete(name = 'Group', labels = lbl) +
#         labs(title = title, x = 'Block #', y = 'Time ball crosses mid-screen (s)') +
#         ylim(0, 2)
#       
#       plist[[j]] = p
#       
#     }
#     
#     #save plots as svg or pdf
#     plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1)
#     
#     #filename to save figure
#     if (save.as == 'svg') {
#       fname = sprintf('./docs/figures/OutliersTime_%s.svg', expNames[i])
#       ggsave(file=fname, plot=plot, width=15, height=7)
#     } else {
#       print(plot)
#     }
#     
#   }
#   
#   if (save.as == 'pdf') {
#     dev.off()
#   }
#   
# }

