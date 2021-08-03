df<-read.csv("cleaned_data.csv")
head(df)
hist(df$numdays)
df$numdays <- scale(df$numdays, center = TRUE, scale = TRUE)

basic.lm <- lm(weight ~ numdays, data = df)
summary(basic.lm)

library(ggplot2)  # load the package

(prelim_plot <- ggplot(df, aes(x = numdays, y = weight)) +
    geom_point() +
    geom_smooth(method = "lm"))

plot(basic.lm, which = 1) 
plot(basic.lm, which = 2) 

boxplot(weight ~ THI, data = df) 

(colour_plot <- ggplot(df, aes(x = numdays, y = weight, colour = THI)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))
# weight not only varies by numdays, THI as well

(split_plot <- ggplot(aes(numdays, weight), data = df) + 
    geom_point() + 
    facet_wrap(~ THI) + # create a facet for each mountain range
    xlab("length") + 
    ylab("test score"))

library(lme4)
mixed.lmer <- lmer(weight ~ numdays + (1|THI), data = df)
summary(mixed.lmer)








df<-read.csv("cleaned_data.csv")

aov1 <- aov(weight ~ activity*THI*numdays, data=df)
summary(aov1)


head(df)

#define intercept-only model
intercept_only <- lm(weight ~ 1, data=df)

#define model with all predictors
all <- lm(weight ~ activity + THI + numdays, data=df)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
#view results of forward stepwise regression
forward$anova
forward$coefficients


backward <- step(all, direction='backward', scope=formula(all), trace=0)
#view results of backward stepwise regression
backward$anova
backward$coefficients

summary(df)
df$id = array(1:1, dim=c(30,1))
library(tvem)
model1 <- tvem(data=df,
               formula=weight~THI+activity,
               id=id,
               num_knots=5,
               time=numdays)
plot(model1)
#plot shows activity is time varying but THI is not

model4 <- tvem(data=df,
               formula=weight~activity,
               id=id,
               invar_effect=~THI,
               num_knots=5,
               time=numdays)
plot(model4)
