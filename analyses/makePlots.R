source('analyses/shared.R')


plotSuccessRate <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/SuccRate_avg.pdf', width=12, height=6)
  }
  
  #to plot lines representing changes in conditions (different depending on the day)
  vline <- list(c(4.5),
                c(2.5, 3.5))

  for (i in 1:length(expNames)) {

    plist <- list() #empty list to store several plots

    #get data for each version of the experiment
    data <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get last block# for each version of the experiment
    lastB <- max(as.numeric(data$tasksNum))
    
    #comparisons we want to look at
    comps <- list(list(c('4','5'), c('4',lastB), c('5',lastB)), #day 1
                  list(c('2','3'), c('3','4'))) #day 2

    #get the number of days
    d <- unique(df$Day)

    for (j in 1:length(d)) {

      #get data for each day
      dataD <- data %>%
        filter(Day == d[j])
      
      #do stats
      stats <- dataD %>%
        group_by(Group) %>%
        t_test(hitPercent ~ tasksNum, comparisons = comps[[j]]) %>% 
        adjust_pvalue(method = 'bonferroni') %>%
        add_significance('p.adj') %>%
        ungroup() %>% 
        add_xy_position(x = 'tasksNum', fun = 'mean_sd', dodge = 0.8)

      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)

      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Day %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Day %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)

      #make plots
      p <- ggline(dataD, x = 'tasksNum', y = 'hitPercent', color = 'Group',
                  add = c('mean_sd'),
                  position = position_dodge(0.5)) +
        geom_vline(xintercept = vline[[j]], linetype = 'dashed', size = 0.4, alpha = 0.7) +
        scale_color_discrete(name = 'Group', labels = lbl)

      p <- ggpar(p,
                 xlab = 'Blocks',
                 ylab = 'Successful trials (%)',
                 ylim = c(0, 100),
                 title = title)
        # stat_pvalue_manual(stats, color = 'Group', step.group.by = 'Group', #add stats results
        #                    bracket.nudge.y = 5,
        #                    tip.length = 0.02, step.increase = 0.1) +
        # scale_y_continuous(expand = expansion(mult = c(0, 0.5)))

      plist[[j]] = p

    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1)
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/SuccRate_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=12, height=6)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotSuccessRateIndiv <- function (df) {
  
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


plotScore <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/ScorePoints_avg.pdf', width=15, height=7)
  }
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    data <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- data %>%
        filter(Day == d[j])
      
      #keep only last practice block for day 1
      if(j == 1) {dataD <- dataD %>% filter(as.numeric(tasksNum) > 3)}
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Day %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Day %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      p <- ggbarplot(dataD, x = 'tasksNum', y = 'percent', color = 'points', fill = 'points',
                     facet.by = 'Group', position = position_stack(),
                     panel.labs = list(Group = lbl),
                     legend.title = 'Points earned') +
        scale_fill_viridis_d() +
        scale_color_viridis_d()
      
      p <- ggpar(p,
                 xlab = 'Blocks',
                 ylab = 'Distribution of points (%)',
                 title = title) +
        theme_classic() +
        theme(plot.margin = unit(c(0,2,0,2), 'lines'),
              text = element_text(size = 15))
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1,
                      common.legend = T, legend = 'right')
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/Score_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=15, height=7)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotDelta <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/interceptDelta_avg.pdf', width=17, height=8)
  }
  
  #to represent trials w/ changes in conditions (different depending on the day)
  vline <- list(c(200),
                c(100, 150))
  
  #to plot paddle length
  pdlL <- c(-params$paddleV1[1], params$paddleV1[1])
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    data <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- data %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #to plot the size of the perturbation
      if (j == 1) {
        pertPlt = list(x = c(200, 500), y = c(-0.22367, -0.22367))
      } else {
        pertPlt = list(x = c(0, 150), y = c(-0.22367, -0.22367))
      }
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Day %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Day %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      p <- ggplot(dataD, aes(x = trialsN, y = iDelta_mn, color = Group,
                           group = interaction(Group, tasksNum))) + #group = interaction to dissociate between blocks and groups
        geom_hline(yintercept = pdlL, linetype = 'dotted') +
        geom_vline(xintercept = vline[[j]], linetype = 'dashed', size = 0.4, alpha = 0.7) +
        geom_segment(x = pertPlt$x[1], y = pertPlt$y[1], xend = pertPlt$x[2], yend = pertPlt$y[2],
                     linetype = 'longdash', color = 'black') +
        geom_point(alpha = 0.35, size = 2.8) +
        # geom_pointrange(aes(ymin = iDelta_mn - iDelta_sd, ymax = iDelta_mn + iDelta_sd), alpha = 0.35) +
        geom_smooth(se = F, span = 0.9, size = 1) +
        
        theme_classic() +
        theme(legend.position = 'top',
              text = element_text(size = 15),
              plot.margin = unit(c(0,2,0,2), 'lines')) + #add margins around individual plots (top, right, bottom, left)
        scale_color_discrete(name = 'Group', labels = lbl) +
        scale_x_continuous(breaks = seq(0, 500, 50)) +
        labs(title = title, x = 'Trials', y = 'Distance between paddle and ball (a.u.)') +
        ylim(-0.23, 0.15)
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1, widths = c(2,1)) #width fig1 = 2*fig2 to have same x scale
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/interceptDelta_avg_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=17, height=8)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }

}


plotDeltaIndiv <- function (df) {
  
  df %<>% 
    select(trialsNum, tasksNum, interceptDelta, participant, Group, Day, expName) %>% 
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


plotDeltaShift_blocks <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/interceptDelta-shift_blocks.pdf', width=12, height=10)
  }
  
  #to plot paddle length
  pdlL <- c(-params$paddleV1[1], params$paddleV1[1])
  #to plot central part of paddle
  pdlC <- c(-params$paddleV1Max, params$paddleV1Max)
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    data <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- data %>%
        filter(Day == d[j])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Day %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Day %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      p <- ggplot(dataD, aes(x = interceptDelta, y = tasksNum, fill = Group)) +
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
      ggsave(file=fname, plot=plot, width=12, height=10)
    } else {
      print(plot)
    }
    
  }
  
  if (save.as == 'pdf') {
    dev.off()
  }
  
}


plotDeltaShift_trials <- function(df, save.as = 'svg') {
  
  #show a message if one of the arguments is missing?
  
  #filename to save figure
  if (save.as == 'pdf') {
    pdf('./docs/interceptDelta-shift_trials.pdf', width=12, height=15)
  }
  
  #to plot trials w/ changes in conditions (different depending on the day)
  tr <- list(c(201:210),
             c(1:10, 101:110, 151:160))
  
  #to plot paddle length
  pdlL <- c(-params$paddleV1[1], params$paddleV1[1])
  #to plot central part of paddle
  pdlC <- c(-params$paddleV1Max, params$paddleV1Max)
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    data <- df %>%
      filter(expName == expNames[i]) %>%
      # filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
      mutate(trialsN = trialsNum + (tasksNum-1)*50) %>% 
      mutate(trialsN = factor(trialsN))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- data %>%
        filter(Day == d[j] & trialsN %in% tr[[j]])
      
      #get number of participants for each task version (n) and each group (n.gp)
      n.gp <- demoG$N %>%
        filter(expName == expNames[i] & Day == d[j]) %>%
        .$n
      n <- sum(n.gp)
      
      #set title and labels
      if (save.as == 'svg') {
        title <- sprintf('Day %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Day %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      p <- ggplot(dataD, aes(x = interceptDelta, y = trialsN, fill = Group)) +
        geom_vline(xintercept = pdlL, linetype = 'solid') +
        geom_vline(xintercept = pdlC, linetype = 'dashed') +
        geom_density_ridges(alpha = 0.5, size = 0.4, scale = 1.5) +
        theme_ridges() + 
        theme(legend.position = 'bottom') +
        scale_fill_discrete(name = 'Group', labels = lbl) +
        labs(title = title,
             x = 'Distance between paddle and ball (a.u.)', y = 'Blocks')
      
      plist[[j]] = p
      
    }
    
    #save plots as svg or pdf
    plot <- ggarrange(plotlist = plist, ncol = 2, nrow = 1)
    
    #filename to save figure
    if (save.as == 'svg') {
      fname = sprintf('./docs/figures/interceptDelta-shift_trials_%s.svg', expNames[i])
      ggsave(file=fname, plot=plot, width=12, height=15)
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
  
  # #to plot paddle length
  # pdlL <- c(-params$paddleV1[1], params$paddleV1[1])
  
  for (i in 1:length(expNames)) {
    
    plist <- list() #empty list to store several plots
    
    #get data for each version of the experiment
    data <- df %>%
      filter(expName == expNames[i]) %>%
      mutate(tasksNum = factor(tasksNum))
    
    #get the number of days
    d <- unique(df$Day)
    
    for (j in 1:length(d)) {
      
      #get data for each day
      dataD <- data %>%
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
        title <- sprintf('Day %s\nN = %s', d[j], n)
      } else {
        title <- sprintf('%s, Day %s\nN = %s', expNames[i], d[j], n)
      }
      
      lbl <- sprintf('%s\n(N = %s)', Gps, n.gp)
      
      #make plots
      p <- ggplot(dataD, aes(x = trialsN, y = iDeltaR_mn, color = Group,
                             group = interaction(Group, tasksNum))) + #group = interaction to dissociate between blocks and groups
        geom_hline(yintercept = c(1), linetype = 'dotted') +
        geom_vline(xintercept = vline[[j]], linetype = 'dashed', size = 0.4, alpha = 0.7) +
        # geom_segment(x = pertPlt$x[1], y = pertPlt$y[1], xend = pertPlt$x[2], yend = pertPlt$y[2],
        #              linetype = 'longdash', color = 'black') +
        geom_point(alpha = 0.35, size = 2.8) +
        # geom_pointrange(aes(ymin = iDeltaR_mn - iDeltaR_sd, ymax = iDeltaR_mn + iDeltaR_sd), alpha = 0.35) +
        geom_smooth(se = F, span = 0.9, size = 1) +
        
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

