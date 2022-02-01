##Pre-processing and data cleaning

source('analyses/shared.R')
source('analyses/makePlots.R')


#import csv files into R ----


getSurvey <- function () {
  
  survey <- read_csv('./data/SquashCatchQualtrics.csv', col_types = cols())
  
  #import headers labels
  labs <- read_csv('./data/SquashCatchQualtrics_labels.csv', 
                   col_types = cols(.default = 'c'))
  
  # #convert columns into numeric and recode groups
  # toCvt <- c('Duration (in seconds)',
  #            'Q2.2',
  #            'Q2.4',
  #            'Q2.5',
  #            'Q3.2_1',
  #            'Q3.2_2',
  #            'Q3.8',
  #            'Q4.3',
  #            'Q4.5',
  #            'Q4.6',
  #            'Q5.1',
  #            'Q6.4_1',
  #            'Q8.4_1',
  #            'Q8.4_2')
  
  # #suppress warnings because they only appear when running the getSurvey function
  # suppressWarnings(
  #   survey <- survey %>%
  #     mutate(across(all_of(toCvt), as.numeric),
  #            group = recode(group, `1` = 'train_horiz', `2` = 'train_tilt')) #Group: 1 = train_horiz and 2 = train_tilt
  # )
  
  #recode the group column
  survey %<>% 
    mutate(group = recode(group,
                          `1` = 'train_horiz', `2` = 'train_tilt')) %>% 
    mutate(group = factor(group, levels = c('train_horiz', 'train_tilt')))
  
  #set labels for the survey data frame
  survey %<>% 
    set_label(label = as.character(labs[1,])) #need to convert 1st row of labs into character
  
  # #remove rows with NA in group
  # survey %<>% 
  #   drop_na(group)
  
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
    # drop_na(Group) %>% #remove rows with NA in Group
    # fill(Browser) %>% 
    mutate(across(all_of(toCvt), as.numeric)) %>%   
    #change interceptDelta (negative is undershoot, positive is overshoot)
    mutate(interceptDelta = ifelse(alphaChoice > 0, interceptDelta*-1, interceptDelta)) %>% 
    #recode variables
    mutate(HitMiss = recode(hitOrMiss, 'miss' = 0, 'hit' = 1), 
           Group = recode(Group, `1` = 'train_horiz', `2` = 'train_tilt')) %>% 
    mutate(Group = factor(Group, levels = c('train_horiz', 'train_tilt')))
  
  return(data)
  
}


#cleanup, match entries between qualtrics survey and pavlovia data files ----


getCompleteData <- function (dfdata, dfsurvey) {
  
  #session 1 missing ----
  #remove participants w/ data files for session 2 but not session 1
  dfdatasplit <- split(dfdata, dfdata$Day)
  to_rmv <- setdiff(dfdatasplit[[2]]$participant, dfdatasplit[[1]]$participant)
  dfdata %<>% 
    filter(!participant %in% to_rmv)
  
  #all blocks done ----
  #count the number of blocks for each participant and session
  df <- dfdata %>% 
    distinct(participant, Day, tasksNum, expName) %>% 
    count(participant, Day, expName) %>% 
    arrange(expName, participant, Day)
  
  #extract unique values for expName and Day
  taskV <- unique(df$expName)
  Day <- unique(df$Day)
  
  #normal number of blocks for each task version and day
  Ns <- c( 8, 4,  #V1 day 1, V1 day 2
          10, 4, #V2 day 1, V3 day 2
           8, 4)  #V3 day 1, V3 day 2
  
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
  
  #too many misses ----
  #find participants with at least 13 consecutive misses
  rm_pp <- dfdata %>% 
    group_by(participant, Day, tasksNum) %>% 
    do({tmp <- sum(with(rle(.$HitMiss == 0), lengths*values) > 12)
    data.frame(Count = tmp)}) %>% 
    filter(Count > 0)
  
  #remove participants who did at least 13 consecutive misses
  dfdata %<>% 
    filter(!participant %in% rm_pp$participant)
  
  #match qualtrics survey to pavlovia data files ----
  #list all combinations of participant*session in dfdata
  ID.sess_data <- dfdata %>% 
    distinct(participant, Day, expName) %>% 
    arrange(participant, Day)
  
  #keep entries in dfsurvey that are in ID.sess_data
  dfsurvey %<>% 
    semi_join(., ID.sess_data, by = c('id' = 'participant', 'session' = 'Day'))
  
  
  df.list <- list('data'=dfdata, 'survey'=dfsurvey)
  list2env(df.list, envir = .GlobalEnv)
  
}


#remove outliers w/ weird ball timings ----


removeOutlBounceTime_participant <- function (dfdata, dfsurvey) {
  
  dT = 0.1 #timing tolerance relative to median (in s)
  
  #print message in console
  is_N <- dfdata %>% 
    filter(Day == 1) %>% 
    distinct(expName, participant, .keep_all = T) %>% 
    count(expName, Group)
  total_N <- length(unique(dfdata$participant))
  
  cat(
    'Before removing bounce time outliers ------\nTotal number of participants:',
    total_N, '\n'
  )
  print(is_N)
  
  #compute median bounce times for each participant
  bounceT <- dfdata %>% 
    group_by(participant, ballSpeed) %>% 
    summarise(median = median(bounceTime, na.rm = TRUE),
              maxi = max(bounceTime),
              mini = min(bounceTime),
              n = n(),
              .groups = 'drop')
  
  #compare with median bounce time across all participants
  bounceT %<>% 
    group_by(ballSpeed) %>% 
    mutate(overall_med = median(median, na.rm = T),
           within_med_dT = median < overall_med + dT & median > overall_med - dT,
           within = cut(median, breaks = c(-Inf, median(median)-dT, median(median)+dT, Inf),
                        labels = c('faster', 'normal', 'slower'))) %>% 
    ungroup()
  
  #make plot
  bounceTplot <- ggplot(bounceT, aes(x = factor(ballSpeed), y = median)) +
    geom_jitter(alpha = 0.2, color = 'black') +
    geom_pointrange(aes(y = overall_med, 
                        ymin = overall_med - dT, ymax = overall_med + dT),
                    color = 'blue', size = 0.4, shape = 0) +
    theme_classic() +
    labs(y = 'Median bounce time (s)', x = 'Ball speed')
  #save plot
  fname = './docs/OutliersBounceTime.svg'
  ggsave(file=fname, plot=bounceTplot)
  
  #participants to remove
  rm_fast <- unique(bounceT$participant[which(bounceT$within == 'faster')])
  rm_slow <- unique(bounceT$participant[which(bounceT$within == 'slower')])
  
  #print message in console
  cat(
    'Bounce time outliers ------\nNumber of participants removed:',
    length(unique(bounceT$participant[which(bounceT$within != 'normal')])),
    '\n', length(rm_fast), 'had faster timings:', rm_fast,
    '\n', length(rm_slow), 'had slower timings:', rm_slow, '\n'
  )
  
  #remove participants from data and survey
  dfdata %<>%
    filter(!participant %in% rm_fast,
           !participant %in% rm_slow)
  dfsurvey %<>%
    filter(!id %in% rm_fast,
           !id %in% rm_slow)
  
  df.list <- list('data'=dfdata, 'survey'=dfsurvey)
  list2env(df.list, envir = .GlobalEnv)
  
}


removeOutlBounceTime_trial <- function (dfdata, dfsurvey) {
  
  dT = 0.1 #timing tolerance relative to median (in s)
  
  #initial number of trials
  tr_start <- nrow(dfdata)
  
  #compare bounce time on each trial to median bounce time computed for each participant
  bounceT <- dfdata %>% 
    dplyr::select(participant, Day, tasksNum, trialsNum, ballSpeed, bounceTime) %>% 
    group_by(participant, ballSpeed, Day) %>% 
    mutate(med_indiv = median(bounceTime, na.m = T),
           within_med_dT = bounceTime < med_indiv + dT & bounceTime > med_indiv - dT) %>% 
    ungroup()
  
  #remove trials from data
  dfdata <- dfdata[which(bounceT$within_med_dT == T), ]
  
  #final number of trials
  tr_end <- nrow(dfdata)
  
  #percentage of trials removed
  tr_percent <- (tr_start - tr_end)/tr_start*100
  cat(
    'Bounce time outliers ------\nNumber of trials removed:',
    tr_start - tr_end,
    sprintf('(%.3f%% of trials)', tr_percent), '\n'
  )
  
  df.list <- list('data'=dfdata)
  list2env(df.list, envir = .GlobalEnv)
  
  # #see how many trials per block have to be removed
  # bounceT_sum <- bounceT %>% 
  #   group_by(participant, Day, tasksNum) %>% 
  #   filter(within_med_dT == F) %>% 
  #   summarise(n = n(), .groups = 'drop')
  
}

