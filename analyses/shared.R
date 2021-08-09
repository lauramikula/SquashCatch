library(sjlabelled)
library(ggpubr) #for plots
library(ggridges) #for density plots
library(rstatix)
# library(emmeans)
library(svglite)
library(viridis)
library(tidyverse)
library(magrittr)



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


getSurvey <- function () {
  
  survey <- read_csv('./data/SquashCatchQualtrics.csv', col_types = cols())
  
  #extract 1st row of the dataframe which contains labels
  labs <- as.character(survey[1,])
  
  #convert columns into numeric and recode groups
  toCvt <- c('Duration (in seconds)',
             'Q2.2',
             'Q2.4',
             'Q2.5',
             'Q3.2_1',
             'Q3.2_2',
             'Q3.8',
             'Q4.3',
             'Q4.5',
             'Q4.6',
             'Q5.1',
             'Q6.4_1',
             'Q8.4_1',
             'Q8.4_2')
  
  # survey <- survey %>%
  #   mutate(across(all_of(toCvt), as.numeric),
  #          group = recode(group, `1` = 'train_horiz', `2` = 'train_tilt'))
  
  #set labels from original csv file
  survey <- survey %>% 
    set_label(label = labs)
  survey <- survey[-1, ]
  
  return(survey)
  
}


getPavlovia <- function () {
  
  data <- read_csv('./data/SquashCatchPavlovia.csv', col_types = cols(participant = 'c'))
  
  #convert columns into numeric
  toCvt <- c('pointsGiven',
             'ballSpeed',
             'interceptBall',
             'interceptPaddle',
             'interceptDelta',
             'wallBounceX',
             'wallBounceY',
             'connectTime',
             'bounceTime',
             'cumulativetime')
  
  data <- data %>% 
    arrange(expName, participant, Day) %>% 
    drop_na(Group) %>% #remove rows with NA in Group
    fill(Browser) %>% 
    mutate(across(all_of(toCvt), as.numeric)) %>%   
    #change interceptDelta (negative is undershoot, positive is overshoot)
    mutate(interceptDelta = ifelse(alphaChoice > 0, interceptDelta*-1, interceptDelta)) %>% 
    #recode variables
    mutate(HitMiss = recode(hitOrMiss, 'miss' = 0, 'hit' = 1), #hit = 1 and miss = 0
           Group = recode(Group, `1` = 'train_horiz', `2` = 'train_tilt')) #Group: 1 = train_horiz and 2 = train_tilt
  
  return(data)
  
}


getCompleteData <- function (dfdata, dfsurvey) {
  
  #remove participants who did session 2 but not session 1
  dfdatasplit <- split(dfdata, dfdata$Day)
  to_rmv <- setdiff(dfdatasplit[[2]]$participant, dfdatasplit[[1]]$participant)
  dfdata %<>% 
    filter(!participant %in% to_rmv)
  dfsurvey %<>% 
    filter(!id %in% to_rmv)
  
  
  df <- dfdata %>% 
    select(participant, Day, tasksNum, expName) %>% 
    unique() %>% 
    group_by(participant, Day, expName) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    arrange(expName, participant, Day)
  
  #extract unique values for expName and Day
  taskV <- unique(df$expName)
  Day <- unique(df$Day)
  
  #number of sessions for each task version and day
  Ns <- c(8,4, #V1 day 1, V1 day 2
          10,4, #V2 day 1, V3 day 2
          8,4) #V2 day 1, V3 day 2

  #find participants who did the required number of blocks for sessions 1 and 2
  Nsess <- data.frame(taskVersion = rep(taskV, each = 2),
                       day = rep(1:2, times = length(taskV), each = 1),
                       N = Ns)
  match_N <- Nsess[match(interaction(df$expName, df$Day), 
                         interaction(Nsess$taskVersion, Nsess$day)), 'N']
  df <- df[which(df$n != match_N), ] #list those who don't match to exclude them
  
  #keep participants who are NOT in df (those who did all blocks for each session)
  dfdata %<>% 
    filter(!participant %in% df$participant)
  
  #find participants with at least 13 consecutive misses
  rm_pp <- dfdata %>% 
    group_by(participant, Day, tasksNum) %>% 
    do({tmp <- sum(with(rle(.$HitMiss == 0), lengths*values) > 12)
    data.frame(Count = tmp)}) %>% 
    filter(Count > 0)
  
  #remove participants who did at least 13 consecutive misses
  dfdata %<>% 
    filter(!participant %in% rm_pp$participant)
  
  #remove participants in dfsurvey who are not in dfdata
  dfsurvey %<>% 
    filter(id %in% dfdata$participant)
  
  df.list <- list('data'=dfdata, 'survey'=dfsurvey)
  list2env(df.list, envir = .GlobalEnv)
  
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
  
  paddle <- data.frame(x = c(0.075, 0.075, 0.05), #length of the paddle in different task versions
                       y = c(0.0125, 0.0125, 0.0125), #height of the paddle in different task versions
                       MaxPts = c(0.025, 0.025, 0.025)) #length of the paddle where participants get maximum points
  ball <- c(0.0125) #diameter of the ball
  
  list.p <- list('ball'=ball, 'paddle'=paddle)
  return(list.p)
  
}


getPerturb <- function(df) {
  
  df %<>% 
    filter(Day == 1 & tasksNum %in% c(4,5)) %>% 
    select(expName, pertChoice, alphaChoice, interceptBall) %>% 
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
    select(participant, Group, Day, expName, trialsNum, tasksNum, ballSpeed,
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


# getKinematics <- function (df) {
#   
#   df <- df %>% 
#     separate_rows(ballPosX, ballPosY, paddlePosX, paddlePosY, sep = ',|\\[|\\]', convert = T) %>% 
#     select(ballPosX, ballPosY, paddlePosX, paddlePosY, alphaChoice, pertChoice, horOrTilt,
#            hitOrMiss, trialsNum, tasksNum, ballSpeed, participant, Group, Day, expName) %>% 
#     drop_na(ballPosX)
#   
#   return(df)
#   
# }


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

