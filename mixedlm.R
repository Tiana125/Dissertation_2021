df<-read.csv("cleaned_data.csv")
head(df)

basic.lm <- lm(weight ~ numdays, data = df)
summary(basic.lm)

library(ggplot2)  # load the package

(prelim_plot <- ggplot(df, aes(x = numdays, y = weight)) +
    geom_point() +
    geom_smooth(method = "lm"))

plot(basic.lm, which = 1) 
plot(basic.lm, which = 2) 


df <- df[-c(15, 23), ]


aov1 <- aov(weight ~ activity*THI, data=df)
summary(aov1)



#define intercept-only model
intercept_only <- lm(weight ~ 1, data=df)

#define model with all predictors
all <- lm(weight ~ activity + THI + numdays, data=df)

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward',
                scope=formula(all), trace=0,k=log(nrow(df)))
#view results of forward stepwise regression
forward$anova
forward$coefficients


backward <- step(all, direction='backward', scope=formula(all), trace=0,k=log(nrow(df)))
#view results of backward stepwise regression
backward$anova
backward$coefficients



hist(df$numdays)
# df$numdays <- scale(df$numdays, center = TRUE, scale = TRUE)





df$id = array(1:1, dim=c(nrow(df),1))
library(tvem)
set.seed(42)

# select number of knots
model1_selected_knots <- select_tvem(data=df,
                                     formula = weight~1,
                                     id=id,
                                     time=numdays,
                                     use_bic=TRUE,
                                     max_knots=5,
                                     keep_going_if_too_few=TRUE)
plot(model1_selected_knots)
model1 <- tvem(data=df,
               formula=weight~THI+activity,
               id=id,
               num_knots=5,
               time=numdays)

# If this error: Error in plot.new() : figure margins too large
# UNCOMMENT next line and run plot() again
# par(mar = rep(2, 4))
# if still does not work
# clear out plot and maximise plot window
# par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(model1)

#plot shows activity is time varying but THI is not
model2_selected_knots <- select_tvem(data=df,
                                     formula = weight~1,
                                     id=id,
                                     invar_effect=~THI,
                                     time=numdays,
                                     use_bic=TRUE,
                                     max_knots=5,
                                     keep_going_if_too_few=TRUE)

model2 <- tvem(data=df,
               formula=weight~activity,
               id=id,
               invar_effect=~THI,
               num_knots=5,
               time=numdays)
plot(model2)

library(citation)
citation("tvem")




# also tried mixedlm
# turned out doesnt work due to only one level
# (colour_plot <- ggplot(df, aes(x = numdays, y = weight, colour = THI)) +
#     geom_point(size = 2) +
#     theme_classic() +
#     theme(legend.position = "none"))
# # weight not only varies by numdays, THI as well
# 
# (split_plot <- ggplot(aes(numdays, weight), data = df) + 
#     geom_point() + 
#     facet_wrap(~ THI) + 
#     xlab("length") + 
#     ylab("test score"))
# 
# library(lme4)
# mixed.lmer <- lmer(weight ~ numdays + (1|THI), data = df)
# summary(mixed.lmer)