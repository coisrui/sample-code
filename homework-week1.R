#Problem 0 
#A woman leaves for work between 8:00 AM and 8:30 AM and takes between 40 and 50 minutes to get there. 
#Let the random variable 𝑋 denote her time of departure, and the random variable 𝑌 the travel time. 
#Assuming that these variables are independent and uniformly distributed, find the probability that the woman arrives at work before 9 AM.

set.seed(12345)
trials <- 5000000
leave <- runif(n = trials, min = 0, max = 30)
travel <- runif(n = trials, min = 40, max = 50)
prob.notlate <- mean(leave + travel <= 60)

#Problem 1
#Let 𝑈 = the number of trials needed to get the first head and 𝑉 = the number of trials needed to get two heads in repeated tosses of a fair coin (i.e., where heads and tails are equally likely and exhaust the outcome space). 
#Are 𝑈 and 𝑉 independent random variables? Answer this question with a plot of repeated simulations. 
#You should also report the distribution of the correlation between 𝑈 and 𝑉 in those simulations using R’s cor function (e.g., cor(c(u, v), method="spearman")).

u.out <- c()
v.out <- c()

prob <- txtProgressBar(min = 0, max = 10000, style = 3)
for (j in 1:10000) {
  flips <-c()
  setTxtProgressBar(prob, j)
  flips <- rbinom(1000, size = 1, prob = 0.5)
  i <- 0
  u <- F
  v <- F
  while (u == F | v == F) {
    i <- i + 1
    
    flips[i] <- rbinom(1, 1, 0.5) 
    if (sum(flips[1:i]) == 1 & u == F) {
      u <- i
      } 
    if (sum(flips[1:i]) == 2) {
        v <- i
        }
  }
  u.out <- u
  v.out <- v
}
close(prob)
v.out.prob <- v.out + runif(10000, min = -0.33, max = 0.33)
u.out.prob <- u.out + runif(10000, min = -0.33, max = 0.33)
#Plot
plot(v.out.prob ~ u.out.prob, pch = 16, col = rgb(0, 0, 0, 0.05), 
     ylab = "Flips until two heads (V)", 
     xlab = "Flips until first head (U)")
f.u <- mean(u.out == 4)
f.v <- mean(v.out == 2)

#Problem 2
#The file qog_std_cs_jan23_stata14.dta contains cross-sectional country data from the international system. 
#Develop a model of corruption using the V-Dem Political Corruption Index as the measure of the dependent variable (variable name: vdem_corr) as a function of a dichotomous classification of democracy (variable name: bmr_dem). 
#Use the “recommended statistical modeling workflow” described on pages 7-8 of Matsuura’s Bayesian Statistical Modeling to guide your analysis.
rm(list = ls())
library(haven)
library(tidyverse) 
library(ggplot2)
library(modelsummary) 
qog <- read_dta("Downloads/qog_std_cs_jan24_stata14.dta")
#show V-Dem corruption values
ggplot(qog, aes(x = vdem_corr)) + geom_histogram(binwidth = 0.1) + xlab("V-Dem Political Corruption Index") + 
  scale_y_continuous(breaks = seq(0, 25, 5), labels = seq(0, 25, 5), limits = c(0, 25)) + theme_bw()
#values of democarcy
qog$bmr_dem_lab <- factor(qog$bmr_dem, levels = c(0, 1), labels = c("non-democracy", "democracy"))
tmp <- qog %>%
  select(`BMR Democracy` = bmr_dem_lab) 
datasummary(`BMR Democracy` + 1 ~ N + Percent(), data = tmp,
                                                                 caption = "Democracy Data")
#corruption by democracy
ggplot(qog, aes(y = vdem_corr, x = bmr_dem_lab)) + geom_boxplot() + xlab("BMR Democracy Classification") + 
  ylab("V-Dem Political Corruption Index") + theme_bw()

#corruption by GDP per capita
qog$ln_mad_gdppc <- log(qog$mad_gdppc) ggplot(qog, aes(y = vdem_corr, x = ln_mad_gdppc, group = bmr_dem_lab)) +
  geom_point(aes(col = bmr_dem_lab)) + theme_bw() + geom_text(label = qog$cname, 
                                                              cex = 3, alpha = 0.3, nudge_x = 0.2, nudge_y = 0.02) + 
  labs(col = "Regime Type") + 
  geom_smooth(method = "lm", formula = "y ~ x", aes(col = bmr_dem_lab)) + ylim(0, 1) + xlim(6, 12.3) + 
  labs(x = "log GDP PC", y = "V-Dem Political Corruption")
sigma <- sd(qog$vdem_corr, na.rm = T) 
b1 <- -0.2 
b2 <- -0.05 
r2 = 0.6 
e <- rnorm(length(qog$vdem_corr), sd = sqrt((1 - r2) * sigma^2)) 
xb.o <- b1 * qog$bmr_dem + b2 * log(qog$mad_gdppc) + e 
xb <- xb.o + (0.5 - mean(xb.o, na.rm = T))

plot(qog$vdem_corr ~ xb, ylab = "V-Dem Corruption Index", xlab = "X * beta") 
abline(0, 1, lty = 2, col = "red")

tmp <- qog %>% 
  mutate(log_mad_gdppc = log(mad_gdppc)) %>%
  select(`BMR Democracy` = bmr_dem, `log GDP PC` = log_mad_gdppc, 
         `V-Dem Political Corruption` = vdem_corr) 
lin.mod <- lm(`V-Dem Political Corruption` ~ `BMR Democracy` + 
                `log GDP PC`, data = tmp) 
modelsummary(lin.mod, caption = "basic linear model")
#OBS
obs <- as.numeric(row.names(lin.mod$model))
sim.obs <- simulate(lin.mod, nsim = 1000) 
sim.obs$cname <- qog$cname[obs]
sim.long <- sim.obs %>% 
  pivot_longer(1:1000) 
countries <- unique(sim.long$cname)
# plot actual value vs. predictions 
ggplot(sim.long %>%
         filter(cname %in% countries[1:9]), aes(x = value, group = cname)) + 
  geom_density() + theme_bw() + geom_vline(data = qog %>% 
  filter(cname %in% countries[1:9]), aes(xintercept = vdem_corr), 
  color = "red", linetype = "dashed") + facet_wrap(facets = vars(cname))
ggplot(sim.long %>%
         filter(cname %in% countries[1:50]), aes(y = value, x = cname)) + 
  geom_boxplot() + geom_point(data = qog %>% 
  filter(cname %in% countries[1:50]), aes(y = vdem_corr, x = cname),
  color = "red") + ylab("Predicted V-Dem Corruption") + xlab(NULL) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, 
                                                vjust = 1, hjust = 1, size = 4))
