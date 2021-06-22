source('analyses/shared.R')
source('analyses/makePlots.R')



#load data files ----

survey <- getSurvey()
data <- getPavlovia()

#do some data cleaning
#remove participants who did not complete all blocks within each session and missed on purpose
getCompleteData(data, survey)


#parameters and demographics ----

params <- getParams(data)

demoG <- getDemogr(data, survey)



#success rate ----

#get data
dataSucc <- data %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
  group_by(expName, participant, Group, Day, tasksNum) %>% 
  summarise(hitPercent = mean(HitMiss, rm.na=T)*100,
            n = n(),
            .groups = 'drop')


#plots for each individual participant
plotSuccessRateIndiv(dataSucc)

#plots averaged across participants
plotSuccessRate(dataSucc, save.as = 'pdf')


#stats
# res.aov <- aov(hitPercent ~ factor(tasksNum) * Group, data = dataSucc)
# summary(res.aov)
# plot.tukey <- TukeyHSD(res.aov, which = 'tasksNum')
# plot(plot.tukey, las = 1)

# model <- data %>% 
#   filter(Day == 1) %>% 
#   glm(HitMiss ~ Group * factor(tasksNum), family = binomial, data = .)
# summary(model)
# # summary(glht(model, mcp(rank = 'Tukey')))
# emmeans(model, ~ Group * tasksNum, type = 'response')
# anova(model, test='LRT')



#proportion of trials with 0, 5 and 10 points ----

dataScore <- getScore(data)

plotScore(dataScore, save.as = 'pdf')



#interceptDelta ----

#interceptDelta is the distance between the paddle and the ball in arbitrary units

#get data
dataDelta <- data %>% 
  select(participant, Group, Day, trialsNum, tasksNum, ballSpeed, wallOrient,
         pertChoice, alphaChoice, interceptDelta, expName) %>% 
  filter(abs(interceptDelta) < 0.5) %>% #remove trials in which participants did not move and start to move late
  mutate(trialsN = trialsNum + (tasksNum-1)*50) %>% 
  group_by(expName, Group, Day, trialsN, tasksNum) %>% 
  summarise(iDelta_mn = mean(interceptDelta, na.rm=T),
            iDelta_sd = sd(interceptDelta, na.rm=T),
            n = n(),
            .groups = 'drop')


#plots for each individual participant
plotDeltaIndiv(data)

#plots averaged across participants
plotDelta(dataDelta, save.as = 'pdf')

#stats
# dataD1 <- data %>%
#   filter(Day == 1 & tasksNum > 3) %>% 
#   mutate(tasksNum = factor(tasksNum))
# model <- aov(interceptDelta ~ Group * tasksNum, data = dataD1) #factor(Group) doesn't work, try as.factor() instead?
# summary(model)
# plot.tukey <- TukeyHSD(model, which = 'tasksNum')
# plot(plot.tukey)



#ridgeline plots of interceptDelta ----

#across blocks
plotDeltaShift_blocks(data, save.as = 'pdf')

#across 10 trials after a perturbation (Day 1 block 5; Day 2 blocks 1,3,4)
plotDeltaShift_trials(data, save.as = 'pdf')



#interceptDelta as a ratio ----

data <- getDeltaRatio(data)

#get data
dataDeltaRatio <- data %>% 
  select(participant, Group, Day, trialsNum, tasksNum, ballSpeed, wallOrient,
         pertChoice, alphaChoice, deltaRatio, expName) %>% 
  mutate(trialsN = trialsNum + (tasksNum-1)*50) %>% 
  group_by(expName, Group, Day, trialsN, tasksNum) %>% 
  summarise(iDeltaR_mn = mean(deltaRatio, na.rm=T),
            iDeltaR_sd = sd(deltaRatio, na.rm=T),
            n = n(),
            .groups = 'drop')


#plots averaged across participants
plotDeltaRatio(dataDeltaRatio, save.as = 'pdf')



#timings depending on OS and browser ----
timings <- data %>% 
  select(Browser, alphaChoice, pertChoice, tasksNum, ballSpeed, connectTime, bounceTime,
         participant, Group, Day, OS, hitOrMiss) %>% 
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



#look at cursor data ----
cursor.dt <- getKinematics(data)

testCurs0 <- cursor.dt %>% 
  filter(alphaChoice == -80 & ballSpeed == 0.05 & pertChoice == 0)
testCurs1 <- cursor.dt %>% 
  filter(alphaChoice == -80 & ballSpeed == 0.05 & pertChoice == -9)

plot(testCurs0$ballPosX, testCurs0$ballPosY, xlim = c(-0.75, 0.1), ylim = c(-0.37, 0.37))
points(testCurs1$ballPosX, testCurs1$ballPosY, col = 'red')
rect(-0.2 - paddleV1[1], -0.3 - paddleV1[2], -0.2 + paddleV1[1], -0.3 + paddleV1[2])



#look at the ball position at intersection ----
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
  mutate(perturb_size = abs(perturb - notperturb),
         pertSize = abs(notperturb - perturb))

prop_trials <- data %>% 
  select(Group, Day, expName, pertChoice, alphaChoice, interceptDelta) %>% 
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
