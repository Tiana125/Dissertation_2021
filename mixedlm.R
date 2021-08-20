# read cleaned data from pre-processing by python
df<-read.csv("cleaned_data.csv")
head(df)

### basic linear model: weight vs number of days
basic.lm <- lm(weight ~ numdays, data = df)
summary(basic.lm)

library(ggplot2)  # load ggplot for plotting
# plot linear line with data, 95% confidence interval
prelim_plot <- ggplot(df, aes(x = numdays, y = weight)) +
    geom_point() +
    geom_smooth(method = "lm") 

plot(basic.lm, which = 1) # residual vs fitted
plot(basic.lm, which = 2) # normal q-q

### two-way ANOVA 
# test interaction (hence *, not + between variables)
aov1 <- aov(weight ~ activity*THI, data=df) 
summary(aov1)


### stepwise regression
#define intercept-only model (minimal model)
intercept_only <- lm(weight ~ 1, data=df)
#define model with all predictors (maximal model)
all <- lm(weight ~ activity + THI + numdays, data=df)

#perform forward stepwise 
forward <- step(intercept_only, direction='forward',
                scope=formula(all), trace=0,k=log(nrow(df)))
#view results of forward stepwise regression
forward$anova
forward$coefficients

#perform backward stepwise 
backward <- step(all, direction='backward', scope=formula(all), trace=0,k=log(nrow(df)))
#view results of backward stepwise regression
backward$anova
backward$coefficients


### TVEM
# treat all data as one subject, hence add the same ID
df$id = array(1:1, dim=c(nrow(df),1))
library(tvem)
# for reproducible results
set.seed(25)

# select number of knots
model1_selected_knots <- select_tvem(data=df,
                                     formula = weight~1,
                                     id=id,
                                     time=numdays,
                                     use_bic=TRUE,
                                     max_knots=5,
                                     keep_going_if_too_few=TRUE)
# fit model with number of knots generated above
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

#re-select model with THI as time-invariant 
model2_selected_knots <- select_tvem(data=df,
                                     formula = weight~1,
                                     id=id,
                                     invar_effect=~THI,
                                     time=numdays,
                                     use_bic=TRUE,
                                     max_knots=5,
                                     keep_going_if_too_few=TRUE)
# fit and plot again
model2 <- tvem(data=df,
               formula=weight~activity,
               id=id,
               invar_effect=~THI,
               num_knots=5,
               time=numdays)
plot(model2)


# output citations for library
library(citation)
citation("tvem")



# The rest is not implemented
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