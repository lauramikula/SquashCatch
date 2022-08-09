library(sjlabelled)
library(ggpubr) #for plots
library(ggridges) #for density plots
library(cowplot)
library(rstatix)
library(emmeans)
library(afex)
library(lme4)
# library(qqplotr)
library(svglite)
# library(viridis)
library(tidyverse)
library(magrittr)
library(MASS)
# library(ggforce)
library(Skillings.Mack)
library(ggprism)



convertCellToNumVectorFirst <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  # keep only the last value of the vector:
  v <- head(v, 1)
  
  return(v)
  
}


convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}


getPaddleStart <- function (df) {
  
  #extract 1st value in paddlePosX (paddle position when the trial starts)
  paddlePosXstart <- sapply(1:nrow(df),
                             function(x){convertCellToNumVectorFirst(data[x,'paddlePosX'])})
  
  df %<>% 
    mutate(paddlePosX_start = paddlePosXstart) %>% 
    relocate(paddlePosX_start, .before = trialMouse.x)
  
  return(df)
  
}


getDemogr <- function (dfdata, dfsurvey) {
  
  is_pp <- unique(dfdata$participant) #IDs of all participants in pavlovia data
  is_ppN <- length(is_pp) #total number of participants
  
  is_N <- dfdata %>% 
    distinct(expName, participant, Day, .keep_all = T) %>% 
    count(expName, Group, Day)
  
  is_sex <- dfsurvey %>% 
    distinct(taskVersion, id, .keep_all = T) %>% 
    count(taskVersion, group, Q2.3)
  
  is_age <- dfsurvey %>% 
    filter(session == 1) %>% 
    group_by(taskVersion, group) %>% 
    summarise(mean = mean(Q2.2, na.rm=T),
              SD = sd(Q2.2, na.rm=T),
              median = median(Q2.2, na.rm=T),
              min = min(Q2.2, na.rm=T),
              max = max(Q2.2, na.rm=T),
              n = n(),
              .groups = 'drop')
  
  is_hand <- dfsurvey %>% 
    filter(session == 1) %>% 
    distinct(taskVersion, id, .keep_all = T) %>% 
    count(taskVersion, Q2.11)
  
  is_device <- dfsurvey %>% 
    filter(session == 1) %>% 
    distinct(taskVersion, id, .keep_all = T) %>% 
    count(taskVersion, Q6.1)
  
  is_cursor <- dfsurvey %>% 
    filter(session == 1) %>% 
    distinct(taskVersion, id, .keep_all = T) %>% 
    count(taskVersion, Q6.2)
  
  list.demogr <- list('N'=is_N, 'sex'=is_sex, 'age'=is_age, 'hand'=is_hand,
                      'device'=is_device, 'cursor'=is_cursor,
                      'pp'=is_pp, 'ppN'=is_ppN)
  return(list.demogr)
  
}


getParams <- function (dfdata) {
  
  #get the different versions of the experiment
  expNames <- unique(dfdata$expName)
  Gps <- c('Trained horizontal', 'Trained tilt')
  
  param.list <- list('expNames'=expNames, 'Gps'=Gps)
  list2env(param.list, envir = .GlobalEnv)
  
  paddle <- data.frame(x = c(0.075, 0.075, 0.05), #1/2 length of the paddle in different task versions
                       y = c(0.0125, 0.0125, 0.0125), #1/2 height of the paddle in different task versions
                       MaxPts = c(0.025, 0.025, 0.025)) #1/2 length of the paddle where participants get maximum points
  row.names(paddle) <- expNames
  ball <- c(0.025) #diameter of the ball
  
  list.p <- list('ball'=ball, 'paddle'=paddle)
  return(list.p)
  
}


getPerturb <- function(df) {
  
  df %<>% 
    filter(Day == 1 & tasksNum %in% c(4,5)) %>% 
    dplyr::select(expName, pertChoice, alphaChoice, interceptBall) %>% 
    distinct(expName, pertChoice, alphaChoice, .keep_all = T) %>% 
    mutate(pertChoice = recode(pertChoice, '-9' = 'perturb', '9' = 'perturb', 
                               '0' = 'notperturb')) %>% 
    spread(pertChoice, interceptBall) %>% 
    mutate(perturb_size = abs(perturb - notperturb))
  
  #split dataframe
  df.split <- df %>% 
    filter(alphaChoice < 0) %>% 
    split(., .$expName)
  
  return(df.split)
  
}


getScore <- function (df) {
  
  #define breaks to calculate how many points participants got on each trial
  cutoff <- mapply(function(x,y) c(-Inf, -x, -y, y, x, Inf),
                   params$paddle$x, params$paddle$MaxPts, SIMPLIFY = F) #set SIMPLIFY to FALSE to get a list instead of matrix
  
  df %<>%
    dplyr::select(participant, Group, Day, expName, trialsNum, tasksNum, ballSpeed,
           interceptDelta) %>%
    filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
    split(., .$expName) %>% #split into list of dataframes to use mapply
    mapply(function(x,y) mutate(x, points = cut(interceptDelta, breaks = y,
                                                labels = c('0','5','10','5','0'))),
           ., cutoff, SIMPLIFY = F) %>% 
    bind_rows() %>% #combine the list of dataframes (unsplit)
    count(expName, Day, Group, tasksNum, points) %>%
    group_by(expName, Day, Group, tasksNum) %>% #ungroup 'points' to compute percentages across the other variables
    arrange(tasksNum, desc(points)) %>%
    mutate(percent = n/sum(n)*100) %>%
    ungroup()

  return(df)
  
}


getKinematics <- function (df) {

  dfcursor <- df %>%
    separate_rows(ballPosX, ballPosY, paddlePosX, paddlePosY, trialMouse.time,
                  sep = ',|\\[|\\]', convert = T) %>%
    #compute number of frames (= number of rows within each trial)
    group_by(participant, Day, tasksNum, trialsNum) %>%
    mutate(frameNum = row_number(), .before = trialsNum) %>% 
    ungroup() %>% 
    #select columns of interest
    dplyr::select(ballPosX, ballPosY, paddlePosX, paddlePosY, paddlePosX_start, 
           interceptBall, bounceTime, connectTime, trialMouse.time, 
           alphaChoice, pertChoice, horOrTilt, hitOrMiss, frameNum, trialsNum, 
           tasksNum, ballSpeed, participant, Group, Day, expName) %>%
    drop_na(ballPosX)

  return(dfcursor)

}


getSpeedProfile_trial <- function (df) {
  
  dfSpeed <- df %>% 
    filter(hitOrMiss == 'hit') %>% #get hits only
    group_by(expName, participant, Day, tasksNum, trialsNum) %>%
    mutate(velocity = abs(paddlePosX - lag(paddlePosX))) %>% 
    ungroup() %>% 
    #compute mean velocity for each participant and each trial first
    group_by(expName, participant, Day, Group, tasksNum, trialsNum, frameNum) %>% 
    summarise(mn_pp_velocity = mean(velocity, na.rm = TRUE),
              .groups = 'drop') %>% 
    group_by(expName, Group, Day, tasksNum, trialsNum, frameNum) %>% 
    #then average velocity across participants in each group
    summarise(mn_velocity = mean(mn_pp_velocity, na.rm = TRUE),
              sd_velocity = sd(mn_pp_velocity, na.rm = TRUE),
              n = n(),
              velocity_95CI = qt(p = 0.05/2, df = n-1, lower.tail = F) * (sd_velocity / sqrt(n)),
              .groups = 'drop') %>% 
    drop_na()
  
  return(dfSpeed)
  
}


getSpeedProfile_block <- function (df) {
  
  dfSpeed <- df %>% 
    filter(hitOrMiss == 'hit') %>% #get hits only
    group_by(expName, participant, Day, tasksNum, trialsNum) %>%
    mutate(velocity = abs(paddlePosX - lag(paddlePosX))) %>% 
    ungroup() %>% 
    #compute mean velocity for each participant first
    group_by(expName, participant, Day, Group, tasksNum, frameNum) %>% 
    summarise(mn_pp_velocity = mean(velocity, na.rm = TRUE),
              .groups = 'drop') %>% 
    group_by(expName, Group, Day, tasksNum, frameNum) %>% 
    #then average velocity across participants in each group
    summarise(mn_velocity = mean(mn_pp_velocity, na.rm = TRUE),
              sd_velocity = sd(mn_pp_velocity, na.rm = TRUE),
              n = n(),
              velocity_95CI = qt(p = 0.05/2, df = n-1, lower.tail = F) * (sd_velocity / sqrt(n)),
              .groups = 'drop') %>% 
    drop_na()
  
  return(dfSpeed)
  
}


getCursorTimingMidScreen <- function (df) {

  #keep the first frame/timing after the cursor crosses the middle of the screen
  dfcursortime <- df %>%
    group_by(participant, Day, tasksNum, trialsNum) %>%
    filter(sign(paddlePosX) == sign(alphaChoice)) %>%
    slice(1) %>% 
    ungroup()

  #keep only hits
  dfcursortime %<>%
    filter(hitOrMiss == 'hit')
  
  #compare timing to enter intercept zone with bounceTime
  dfcursortime %<>% 
    mutate(timeReBounce = trialMouse.time - bounceTime)

  return(dfcursortime)

}


getCursorTimingWithinIntercept <- function (df) {
  
  #1/2 paddle length is stored in params$paddle$x
  #keep the first frame/timing after the cursor enter intercept zone
  dfcursortime <- df %>% 
    group_by(participant, Day, tasksNum, trialsNum) %>% 
    filter(abs(paddlePosX - interceptBall) <= params$paddle[expName, 'x']) %>% 
    slice(1) %>% 
    ungroup()
  
  #keep only hits
  dfcursortime %<>%
    filter(hitOrMiss == 'hit')
  
  #compare timing to enter intercept zone with bounceTime
  dfcursortime %<>% 
    mutate(timeReBounce = trialMouse.time - bounceTime)
  
  return(dfcursortime)
  
}


getDeltaRatio <- function (df) {

  #extract 1st value in paddlePosX (paddle position when the trial starts)
  paddlePosX_start <- sapply(1:nrow(df),
                             function(x){convertCellToNumVectorFirst(data[x,'paddlePosX'])})

  #compute intercept delta as a ratio (1 > is undershoot, 1 < is overshoot)
  DeltaRatio <- abs(df$interceptPaddle - paddlePosX_start) / abs(df$interceptBall - paddlePosX_start)

  df %<>%
    mutate(deltaRatio = DeltaRatio)
  
  return(df)

}


getDistanceTraveled <- function (df) {
  
  #keep only hit trials
  dfcursor <- df %>% 
    filter(hitOrMiss == 'hit')
  
  #compute % of distance traveled on each frame
  dfcursor %<>%
    #change sign of ball and paddle positions (to get rid of left and right)
    #>0 is in direction of where the ball lands
    #<0 is in the opposite direction
    mutate(ballPosX = ifelse(alphaChoice < 0, ballPosX*-1, ballPosX),
           paddlePosX = ifelse(alphaChoice < 0, paddlePosX*-1, paddlePosX),
           paddlePosX_start = ifelse(alphaChoice < 0, paddlePosX_start*-1, paddlePosX_start),
           interceptBall = ifelse(alphaChoice < 0, interceptBall*-1, interceptBall)) %>% 
    group_by(participant, Day, tasksNum, trialsNum) %>% 
    mutate(dist_traveled = (paddlePosX - dplyr::lag(paddlePosX)) / abs(paddlePosX_start - interceptBall) * 100,
           timeReConnect = trialMouse.time - connectTime) %>% 
    # group_by(participant, Day, tasksNum) %>% 
    # mutate(dist_traveled = dist_traveled / abs(paddlePosX_start - interceptBall) * 100) %>% 
    ungroup() %>% 
    drop_na(dist_traveled) %>% 
    filter(dist_traveled != 0) #remove when there is no cursor movement
  
  return(dfcursor)
  
}


checkPerturbation <- function (df) {
  
  pertRad <- pi/20
  # pertRad <- 0
  noPert <- 0.6 / tan(df * pi/180)
  Pert <- 0.6 * tan(((90 - df) * pi/180) + (pertRad)*2)
  diffPert_noPert <- Pert - noPert
  pertDeg <- pertRad * 180/pi
  
  checkPert <- data.frame(alphaChoice = df,
                          perturb_deg = pertDeg,
                          no_perturb = noPert,
                          perturb = Pert,
                          perturb_size = diffPert_noPert)
  
  return(checkPert)
  
}

