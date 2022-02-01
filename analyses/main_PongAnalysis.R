source('analyses/shared.R')
source('analyses/cleaningData.R')
source('analyses/makePlots.R')


####
# 
# Main analysis script for the Adaptation to Online Pong Game
# 
# Data can be downloaded here: https://osf.io/54gm2/
# After downloading data files (.csv), put them in an empty "data" folder inside the project
# (path is "./data")
# 
####


#load data files ----

survey <- getSurvey()
data <- getPavlovia()

#do some data cleaning: remove participants who did not complete all blocks 
#within each session and missed on purpose
getCompleteData(data, survey)

#add start X position of the paddle
data <- getPaddleStart(data)


#pre-processing ----

##do some individual plots ----

# #individual success rate
# plotSuccessRateIndiv(data)
# 
# #individual interceptDelta
# plotDeltaIndiv(data)


##remove outliers ----

#timings during experiment (bounceTime)
removeOutlBounceTime_participant(data, survey)
removeOutlBounceTime_trial(data, survey)


##get parameters and demographics ----

params <- getParams(data)
perturb <- getPerturb(data)
demoG <- getDemogr(data, survey)


##get cursor data ----

datacursor <- getKinematics(data)



#data analysis ----

##success rate ----

#get data
dataSucc <- data %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
  group_by(expName, participant, Group, Day, tasksNum) %>% 
  summarise(hitPercent = mean(HitMiss, na.rm = T)*100,
            n = n(),
            .groups = 'drop')


#plots averaged across participants
plotSuccessRate(dataSucc, save.as = 'pdf')
plotSuccessRate(dataSucc, save.as = 'svg', WxL = c(10,5))


###stats ----

dataD <- dataSucc %>%
  filter(expName == 'bounceV3' & Day == 1) %>%
  convert_as_factor(tasksNum, Group, participant)

#normality assumption
dataD %>% 
  group_by(Group, tasksNum) %>% 
  shapiro_test(hitPercent)
ggqqplot(dataD, 'hitPercent') + facet_grid(Group ~ tasksNum)

#homogeneity of variance assumption
dataD %>% 
  group_by(tasksNum) %>% 
  levene_test(hitPercent ~ Group) #assess homogeneity of the between factor

#ANOVA
res.aov <- aov_ez(data = dataD, dv = 'hitPercent', id = 'participant',
                  between = 'Group', within = 'tasksNum',
                  anova_table = list(es = 'pes'), #get partial eta-squared
                  include_aov = TRUE) #get uncorrected degrees of freedom
get_anova_table(res.aov)
#qqplot
ggqqplot(as.numeric(residuals(res.aov$lm)))

#post-hoc tests
#set custom contrasts
contrD1 <- list(
  b4_b5 = c(0, 0, 0, 1, -1, 0, 0, 0),
  b4_b8 = c(0, 0, 0, 1, 0, 0, 0, -1),
  b5_b8 = c(0, 0, 0, 0, 1, 0, 0, -1)
)
contrD2 <- list(
  b2_b3 = c(0, 1, -1, 0),
  b2_b4 = c(0, 1, 0, -1),
  b3_b4 = c(0, 0, 1, -1)
)

#main effect of tasksNum, no interaction
postHoc <- emmeans(res.aov, ~ tasksNum, contr = contrD1, adjust = 'bonferroni')
postHoc$contrasts %>% 
  as.data.frame() %>% 
  mutate(p_val = format.pval(p.value, digits = 3)) %>% 
  add_significance()
#get effect sizes
eff_size(postHoc, sigma = sigma(res.aov$aov$`participant:tasksNum`), 
         edf = df.residual(res.aov$aov$`participant:tasksNum`))

postHoc <- emmeans(res.aov, ~ tasksNum, contr = contrD2, adjust = 'bonferroni')
postHoc$contrasts %>% 
  as.data.frame() %>% 
  mutate(p_val = format.pval(p.value, digits = 3)) %>% 
  add_significance()
#get effect sizes
eff_size(postHoc, sigma = sigma(res.aov$aov$`participant:tasksNum`), 
         edf = df.residual(res.aov$aov$`participant:tasksNum`))

rm(dataD, res.aov, contrD1, contrD2, postHoc)



##proportion of trials with 0, 5 and 10 points ----

dataScore <- getScore(data)

plotScore(dataScore, save.as = 'pdf')
plotScore(dataScore, save.as = 'svg', WxL = c(10,3.5))



##interceptDelta ----

#interceptDelta is the distance between the paddle and the ball in arbitrary units

#get data
dataDelta <- data %>% 
  dplyr::select(participant, Group, Day, trialsNum, tasksNum, ballSpeed, wallOrient,
         pertChoice, alphaChoice, interceptDelta, expName) %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
  mutate(trialsN = trialsNum + (tasksNum-1)*50) %>% 
  group_by(expName, Group, Day, trialsN, tasksNum) %>% 
  summarise(iDelta_mn = mean(interceptDelta, na.rm=T),
            iDelta_med = median(interceptDelta, na.rm=T),
            iDelta_sd = sd(interceptDelta, na.rm=T),
            n = n(),
            .groups = 'drop')


#plots averaged across participants
plotDelta(dataDelta, save.as = 'pdf', WxL = c(15,6))
plotDelta(dataDelta, save.as = 'svg', WxL = c(12,6))


###stats ----

dataD1 <- data %>% 
  filter(expName == 'bounceV3' & Day == 1 & 
           ((tasksNum == 4 & trialsNum == 50) | 
              (tasksNum == 5 & trialsNum %in% c(1, 50)))) %>% 
  dplyr::select(expName, participant, Group, Day, tasksNum, trialsNum, interceptDelta) %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
  mutate(time = case_when(tasksNum == 4 ~ 'Tr200',
                          tasksNum == 5 & trialsNum == 1 ~ 'Tr201',
                          tasksNum == 5 & trialsNum == 50 ~ 'Tr250'),
         trialsN = trialsNum + (tasksNum-1)*50) %>% 
  convert_as_factor(Group, time, participant, trialsN)

dataD2 <- data %>% 
  filter(expName == 'bounceV3' & Day == 2 & 
           ((tasksNum == 1 & trialsNum == 1) |
              (tasksNum == 2 & trialsNum == 50) | 
              (tasksNum == 3 & trialsNum %in% c(1, 50)) | 
              (tasksNum == 4 & trialsNum %in% c(1, 50)))) %>% 
  dplyr::select(expName, participant, Group, Day, tasksNum, trialsNum, interceptDelta) %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
  mutate(time = case_when(tasksNum == 1 ~ 'Tr1',
                          tasksNum == 2 ~ 'Tr100',
                          tasksNum == 3 & trialsNum == 1 ~ 'Tr101',
                          tasksNum == 3 & trialsNum == 50 ~ 'Tr150',
                          tasksNum == 4 & trialsNum == 1 ~ 'Tr151',
                          tasksNum == 4 & trialsNum == 50 ~ 'Tr200'),
         trialsN = trialsNum + (tasksNum-1)*50) %>% 
  convert_as_factor(Group, time, participant, trialsN)

dataD <- dataD2

#normality assumption
dataD %>%
  group_by(Group, time) %>%
  shapiro_test(interceptDelta)
ggqqplot(dataD, 'interceptDelta') + facet_grid(Group ~ time)

#homogeneity of variance assumption
dataD %>%
  group_by(time) %>%
  levene_test(interceptDelta ~ Group) #assess homogeneity of the between factor

#ANOVA
res.aov <- aov_ez(data = dataD, dv = 'interceptDelta', id = 'participant',
                  between = 'Group', within = 'time', type = 3,
                  anova_table = list(es = 'pes'), #get partial eta-squared
                  include_aov = TRUE) #get uncorrected degrees of freedom
get_anova_table(res.aov)
#qqplot
ggqqplot(as.numeric(residuals(res.aov$lm)))

#GLMM because residuals do not seem normally distributed
dataD$intDelta_trans <- dataD$interceptDelta + 1
glmm <- lmer(interceptDelta ~ Group * time + (1|participant), data = dataD)
ggqqplot(residuals(glmm))
anova(glmm)
# print(glmm, corr = FALSE)
# glmm2 <- glmer(intDelta_trans ~ Group * time + (1|participant), data = dataD,
#                family = gaussian(link = 'log'))
# ggqqplot(residuals(glmm2))
# anova(glmm2)

#post-hoc tests
#set custom contrasts
lvls = levels(dataD2$time)
contrD2 <- list()
contrD2[[paste(lvls[1], lvls[2], sep = '-')]] = c(1, -1, 0, 0, 0, 0)
contrD2[[paste(lvls[2], lvls[3], sep = '-')]] = c(0, 1, -1, 0, 0, 0)
contrD2[[paste(lvls[3], lvls[4], sep = '-')]] = c(0, 0, 1, -1, 0, 0)
contrD2[[paste(lvls[2], lvls[5], sep = '-')]] = c(0, 1, 0, 0, -1, 0)
contrD2[[paste(lvls[5], lvls[6], sep = '-')]] = c(0, 0, 0, 0, 1, -1)

#main effect of time, no interaction
postHoc <- emmeans(res.aov, pairwise ~ time, adjust = 'bonferroni') #pairwise to look at all possible combinations
postHoc$contrasts %>%
  as.data.frame() %>%
  mutate(p_val = format.pval(p.value, digits = 3)) %>%
  add_significance()
#get effect sizes
eff_size(postHoc, sigma = sigma(res.aov$aov$`participant:time`), 
         edf = df.residual(res.aov$aov$`participant:time`))

postHoc <- emmeans(res.aov, ~ time, contr = contrD2, adjust = 'bonferroni')
postHoc$contrasts %>%
  as.data.frame() %>%
  mutate(p_val = format.pval(p.value, digits = 3)) %>%
  add_significance()
#get effect sizes
eff_size(postHoc, sigma = sigma(res.aov$aov$`participant:time`), 
         edf = df.residual(res.aov$aov$`participant:time`))

rm(dataD1, dataD2, dataD, res.aov, contrD2, postHoc, lvls)


###savings between sessions 1 and 2 ----
#t-test only taking participants from session 2 (trial 201 session 1 vs trial 1 session 2)
#participants to keep
dataD1 <- data %>% 
  filter(expName == 'bounceV3' & Day == 1 & tasksNum == 5 & trialsNum == 1) %>% 
  dplyr::select(expName, participant, Group, Day, interceptDelta) %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move
  convert_as_factor(Group)
dataD2 <- data %>% 
  filter(expName == 'bounceV3' & Day == 2 & tasksNum == 1 & trialsNum == 1) %>% 
  dplyr::select(expName, participant, Group, Day, interceptDelta) %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move
  convert_as_factor(Group)

datattest <- inner_join(dataD1, dataD2, by = 'participant')
datattest$diff <- datattest$interceptDelta.x - datattest$interceptDelta.y

#normality test on the difference
shapiro_test(datattest$diff)

#compute t-test
res <- t.test(datattest$interceptDelta.x, datattest$interceptDelta.y, paired = TRUE)
res

#do a mixed 2-way ANOVA
datattest <- data %>% 
  filter((expName == 'bounceV3' & Day == 1 & tasksNum == 5 & trialsNum == 1) |
           expName == 'bounceV3' & Day == 2 & tasksNum == 1 & trialsNum == 1) %>% 
  dplyr::select(expName, participant, Group, Day, interceptDelta) %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move
  convert_as_factor(Group)

#normality assumption
datattest %>%
  group_by(Group, Day) %>%
  shapiro_test(interceptDelta)
ggqqplot(datattest, 'interceptDelta') + facet_grid(Group ~ Day)

#homogeneity of variance assumption
datattest %>%
  group_by(Day) %>%
  levene_test(interceptDelta ~ Group) #assess homogeneity of the between factor

#ANOVA
res.aov <- aov_ez(data = datattest, dv = 'interceptDelta', id = 'participant',
                  between = 'Group', within = 'Day', type = 3,
                  anova_table = list(es = 'pes'), #get partial eta-squared
                  include_aov = TRUE) #get uncorrected degrees of freedom
get_anova_table(res.aov)
#qqplot
ggqqplot(as.numeric(residuals(res.aov$lm)))

rm(dataD1, dataD2, datattest, res)


###plot of the stats ----
plotDelta_stats(data, whichVersion = 'bounceV3', WxL = c(12,5)) #same width as interceptDelta plot
plotDelta_stats(data, whichVersion = 'bounceV3', WxL = c(10,5))



##ridgeline plots of interceptDelta ----

#across blocks
plotDeltaShift_blocks(data, save.as = 'pdf')

#across 10 trials after a perturbation (Day 1 block 5; Day 2 blocks 1,3,4)
plotDeltaShift_trials(data, save.as = 'pdf')



##interceptDelta as a ratio ----

data <- getDeltaRatio(data)

#get data
dataDeltaRatio <- data %>% 
  dplyr::select(participant, Group, Day, trialsNum, tasksNum, ballSpeed, wallOrient,
         pertChoice, alphaChoice, deltaRatio, expName) %>% 
  mutate(trialsN = trialsNum + (tasksNum-1)*50) %>% 
  group_by(expName, Group, Day, trialsN, tasksNum) %>% 
  summarise(iDeltaR_mn = mean(deltaRatio, na.rm=T),
            iDeltaR_sd = sd(deltaRatio, na.rm=T),
            n = n(),
            .groups = 'drop')


#plots averaged across participants
plotDeltaRatio(dataDeltaRatio, save.as = 'pdf')



#exploratory analysis ----

##look at timing when crossing mid-screen ----
cursor_midscreen <- getCursorTimingMidScreen(datacursor)

plotTimingMidScreen(cursor_midscreen, save.as = 'pdf')



##look at timing when cursor is within intercept ----

#within intercept = final X position of the ball +/- 1/2 paddle length

cursor_in_intercept <- getCursorTimingWithinIntercept(datacursor)

plotTimingWithinIntercept(cursor_in_intercept, save.as = 'pdf')
plotTimingWithinInterceptHist(cursor_in_intercept, save.as = 'pdf')

#very long to run, to improve!!!!!!!!!!!!!!!!!!!!!!!

# getWithinIntercept <- function(df) {
#   df %<>%
#     mutate(within = abs(paddlePosX - interceptBall) <= paddleL) %>% 
#     slice(1)
# }
# 
# dfcursortime <- datacursor %>%
#     group_by(expName) %>% 
#     nest()
#   dfcursortime$paddleL <- params$paddle$x
#   dfcursortime %<>% 
#     unnest(cols = c(data)) %>% 
#     group_by(participant, Day, tasksNum, trialsNum)
#   apply(dfcursortime, 1, function(x) {
#     x$within <- abs(x$paddlePosX - x$interceptBall) <= x$paddleL; return(x)})
#   fcursortime %<>%
#     filter(within == 'TRUE') %>% 
#     slice(1) %>% 
#     ungroup()
    
  # dfcursortime %<>% 
  #   unnest(cols = c(data)) %>% 
  #   group_by(participant, Day, tasksNum, trialsNum) %>%
  #   # filter(paddlePosX <= abs(paddlePosX - interceptBall)) %>%
  #   filter(abs(paddlePosX - interceptBall) <= paddleL) %>% 
  #   slice(1) %>% 
  #   ungroup()



##look at distance traveled as a function of time relative to connect time ----

dataDistTravel <- getDistanceTraveled(datacursor)

plotDistTraveled(dataDistTravel, save.as = 'svg')

# ggplot(dataDistTravel, aes(x = timeReConnect, y = dist_traveled)) +
#   geom_col() +
#   scale_x_binned(n.breaks = 100) + 
#   scale_x_continuous(breaks = seq(from = -1, to = 0.5, by = 0.25))



##look at number of cursor movement as a function of time relative to connect time ----

plotNmvtTime(dataDistTravel, save.as = 'pdf')
  


##timings depending on OS and browser ----
timings <- data %>% 
  dplyr::select(Browser, alphaChoice, pertChoice, tasksNum, ballSpeed, 
                connectTime, bounceTime, participant, Group, Day, OS, hitOrMiss) %>% 
  # filter(Day == 1 & tasksNum > 2 & tasksNum < 5) %>%
  filter(Day == 1 & tasksNum > 4) %>%
  mutate(Browser = as.factor(Browser), OS = as.factor(OS), ballSpeed = as.factor(ballSpeed)) %>% 
  unite(OS_Browser, c('OS', 'Browser'))

ggboxplot(timings, x = 'alphaChoice', y = 'connectTime', color = 'OS_Browser')


longtimings <- timings %>% 
  filter(participant %in% c('217396', '220291'))
timings2 <- timings[!timings$participant%in%longtimings$participant,]

ggboxplot(timings2, x = 'alphaChoice', y = 'connectTime', color = 'OS_Browser',
          title = 'without pp 217396 and 220291', ylim = c(0.6,0.8))
  

long <- timings %>% 
  filter(connectTime > 0.8)
unique(long$participant)



##look at cursor data ----

testCurs0 <- datacursor %>% 
  filter(alphaChoice == -80 & ballSpeed == 0.05 & pertChoice == 0)
testCurs1 <- datacursor %>% 
  filter(alphaChoice == -80 & ballSpeed == 0.05 & pertChoice == -9)

plot(testCurs0$ballPosX, testCurs0$ballPosY, xlim = c(-0.75, 0.1), ylim = c(-0.37, 0.37))
points(testCurs1$ballPosX, testCurs1$ballPosY, col = 'red')
rect(-0.2 - paddleV1[1], -0.3 - paddleV1[2], -0.2 + paddleV1[1], -0.3 + paddleV1[2])



##look at the ball position at intersection ----
ballEndPos <- data %>%
  mutate_at(c('pertChoice', 'alphaChoice'), as.factor) %>%
  group_by(expName, alphaChoice, pertChoice, ballSpeed) %>%
  summarise(ballEndX_mn = mean(interceptBall, na.rm=T),
            ballEndX_sd = sd(interceptBall),
            .groups = 'drop')

ballEndPos <- ballEndPos %>%
  filter(expName == 'bounceV2' & ballSpeed == 0.04) %>%
  mutate(ballEndY = -0.3)

ballEndPos_wide <- ballEndPos %>% 
  mutate(pertChoice = recode(pertChoice, '-9' = 'perturb', '9' = 'perturb', '0' = 'notperturb')) %>% 
  spread(pertChoice, ballEndX_mn) %>% 
  mutate(perturb_size = abs(perturb - notperturb))

prop_trials <- data %>% 
  dplyr::select(Group, Day, expName, pertChoice, alphaChoice, interceptDelta) %>% 
  filter(abs(interceptDelta) < 0.5 & pertChoice != 0) %>% #remove trials in which participants did not move and start to move late
  count(expName, Day, Group, alphaChoice) %>% 
  group_by(expName, Day, Group) %>%
  mutate(percent = n/sum(n)*100) %>% 
  ungroup()


ori <- ans$ballEndX_mn[2]


ggdotplot(ans, y = 'ballEndX_mn', x = 'ballEndY', color = 'pertChoice', xlim = c(-0.6,0.6), ylim = c(-0.6,0.6)) +
  coord_flip() +
  geom_rect(aes(ymin = 0-paddleV1[1], ymax = 0+paddleV1[1], xmin = -0.3-paddleV1[2], xmax = -0.3+paddleV1[2]))

plot(data = ans, ballEndY ~ ballEndX_mn, xlim = c(-1,1), ylim = c(-1,1), col = pertChoice)
rect(ori - paddleV1[1], -0.35 - paddleV1[2], ori + paddleV1[1], -0.35 + paddleV1[2])

ggplot(ans, aes(ballEndX_mn, ballEndY, col = pertChoice)) +
  geom_point()
