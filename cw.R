require(utils)
require(ggplot2)
require(dplyr)

cw = read.csv("~/Documents/cw/cw_marginal.csv") %>% 
  filter(difficulty != 'easy') %>%
  mutate(difficulty = factor(difficulty, levels = c("normal","hard","insane")),
         numCells = ifelse(size=='S',10,
                ifelse(size=='M',15,
                ifelse(size=='L',25,
                ifelse(size=='XL',40,
                ifelse(size=='C60',60,
                ifelse(size=='C80',80,100)))))),
         N.2 = N + 2,
         positive.2 = positive + 1,
         negative.2 = N.2 - positive.2,
         negative = N - positive,
         actual = positive / (positive + negative),
         actual.2 = positive.2 / (positive.2 + negative.2),
         startingNum = ifelse(starting=='solo',1,ifelse(starting=='pairs',2,numCells / 5)),
         startingPct = startingNum / numCells * 100
)

WITH.INTERACTIONS = FALSE

formula = ifelse(WITH.INTERACTIONS, 
                 'cbind(positive.2,negative.2) ~ numCells * difficulty',
                 'cbind(positive.2,negative.2) ~ numCells + difficulty')

fit = glm(data = cw, formula = as.formula(formula), family = binomial())

summary(fit) # display results
confint(fit) # 95% CI for the coefficients
exp(cbind(OR = coef(fit), confint(fit))) # exponentiated coefficients with 95% CI
residuals(fit, type="deviance") # residuals

marginals = cw %>%
  select(
    numCells,
    difficulty,
    positive,
    N
  ) %>%
  group_by(numCells, difficulty) %>%
  mutate(
    sumPositive = sum(positive),
    gamesPlayed = sum(N),
    marginalRate = sumPositive / gamesPlayed
  ) %>%
  select(-positive,-N) %>%
  slice(1)

to.plot.from = 10
to.plot.to = 100

to.plot = expand.grid(numCells = seq(from=to.plot.from,to=to.plot.to), startingMode = c("1", "2", "3"), difficulty = c("normal","hard","insane"))
to.plot$startingNum = as.numeric(to.plot$startingMode)
to.plot$startingNum = ifelse(to.plot$startingNum==3 | (to.plot$startingNum==2 & to.plot$numCells < 10), to.plot$numCells / 5, to.plot$startingNum)
# to.plot$startingMode = ifelse(to.plot$startingMode=="1","Solo",ifelse(to.plot$startingMode=="2","Pairs","Crowded"))
# to.plot$startingPct = to.plot$startingNum / to.plot$numCells

levels(to.plot$difficulty) = c("normal","hard","insane")
# levels(to.plot$startingMode) = c("Solo","Pairs","Crowded")
to.plot = cbind(to.plot, predict(fit,newdata = to.plot, type = "link",se=TRUE))

to.plot = within(to.plot, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.64 * se.fit))  # 1.96 is 95% CI
  UL <- plogis(fit + (1.64 * se.fit))
})

plot.save.prefix = ifelse(WITH.INTERACTIONS, 'with-interactions-', 'no-interactions-')

ggplot(data=to.plot, aes(x = numCells, y = PredictedProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = difficulty), alpha = .2) +
  # geom_line(aes(color=difficulty,linetype=startingMode),numCells=1) + 
  geom_line(aes(color=difficulty),size=1) + 
  scale_y_continuous(breaks=seq(from=0,to=1,by=0.1),minor_breaks = waiver()) +
  scale_x_continuous(breaks=seq(from=to.plot.from,to=to.plot.to,by=10),minor_breaks = waiver()) +
  labs(title = "Chance of victory as a function of map parameters",
       x = "Number of cells", y = "Chance of victory") +
  geom_point(data=marginals, aes(y=marginalRate, x=numCells, color = difficulty, size = gamesPlayed)) +
  ggsave(file=paste(plot.save.prefix,"plot.png",sep = ''), width=7, height = 7)

# total.nodes = cw$numCells * 15
# completed.nodes = ifelse(cw$N > 15, 15, cw$N) * cw$numCells
# uncompleted.nodes = total.nodes - completed.nodes
# print("Percent of nodes unobserved: ")
# print(sum(uncompleted.nodes) / sum(total.nodes))