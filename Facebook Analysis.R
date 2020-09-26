install.packages("dplyr")
library(dplyr)

#Load the file, skip 2 rows which contain information about survey questions
Facebook <- read.csv("Facebook.csv", header=TRUE, skip=c(1,3))

#Remove unnecessary rows (contain test responses) and columns that will now be analyzed
Facebook <- Facebook[-c(1,2),-c(1:5,7:17)]

#Check the structure
str(Facebook)

#Force response duration to numeric
Facebook$Duration..in.seconds. <-as.numeric(Facebook$Duration..in.seconds.)

#Check nas and force them to 0
sum(is.na(Facebook))
Facebook[is.na(Facebook)] <- 0
sum(is.na(Facebook))

#Analysis will be done for responses of users with active Facebook account. Subset Facebook dataframe to only keep the rows with active Facebook subscription
FacebookActive <- Facebook %>% filter(Facebook$I.have.an.active.Facebook.subscription. == "Yes")
summary(FacebookActive)

#Remove the column indicating if a user has an active Facebook account
FacebookActive <- FacebookActive[,-2]

#Recode textual responses to numeric values
FacebookActive$Ease.recoded <- recode(FacebookActive$Facebook.is.easy.to.use., "Strongly Agree" = 7, "Agree" = 6, "Somewhat agree" = 5, "Neither agree nor disagree" = 4, "Somewhat disagree" = 3, "Disagree" = 2, "(Other)" = 0)
FacebookActive$Primary.recoded <- recode(FacebookActive$Facebook.is.the.primary.social.network.I.use.to.connect.with.acquaintances..family.and.friends..communities.and.others., "Strongly Agree" = 7, "Agree" = 6, "Somewhat agree" = 5, "Neither agree nor disagree" = 4, "Somewhat disagree" = 3, "Disagree" = 2, "Strongly disagree" = 1, "(Other)" = 0)
FacebookActive$Content.recoded <- recode(FacebookActive$The.content.on.my.Facebook.newsfeed.is.valuable.for.me., "Strongly Agree" = 7, "Agree" = 6, "Somewhat agree" = 5, "Neither agree nor disagree" = 4, "Somewhat disagree" = 3, "Disagree" = 2, "Strongly disagree" = 1, "(Other)" = 0)
FacebookActive$Trust.recoded <- recode(FacebookActive$I.trust.the.content.I.read.on.Facebook., "Strongly Agree" = 7, "Agree" = 6, "Somewhat agree" = 5, "Neither agree nor disagree" = 4, "Somewhat disagree" = 3, "Disagree" = 2, "Strongly disagree" = 1, "(Other)" = 0)
FacebookActive$Privacy.recoded <- recode(FacebookActive$I.am.comfortable.with.my.privacy.while.using.Facebook., "Extremely comfortable" = 7, "Moderately comfortable" = 6, "Slightly comfortable" = 5, "Neither comfortable nor uncomfortable" = 4, "Slightly uncomfortable" = 3, "Moderately uncomfortable" = 2, "Extremely uncomfortable" = 1, "(Other)" = 0)
FacebookActive$Satisfaction.recoded <- recode(FacebookActive$Please.rate.your.overall.satisfaction.with.Facebook., "Extremely satisfied" = 7, "Moderately satisfied" = 6, "Slightly satisfied" = 5, "Neither satisfied nor dissatisfied" = 4, "Slightly dissatisfied" = 3, "Moderately dissatisfied" = 2, "Extremely dissatisfied" = 1, "(Other)" = 0)
FacebookActive$Deactivation.recoded <- recode(FacebookActive$I.consider.deactivating.my.Facebook.account.in.the.next.6.months, "Strongly Agree" = 7, "Agree" = 6, "Somewhat agree" = 5, "Neither agree nor disagree" = 4, "Somewhat disagree" = 3, "Disagree" = 2, "Strongly disagree" = 1, "(Other)" = 0)
FacebookActive$Age.recoded <- recode(FacebookActive$Which.of.the.following.best.represents.your.age., "19 - 24" = 1, "25 - 34" = 2, "35 - 44" = 3, "45 - 54" = 4, "55 or older" = 5, "Prefer not to say" = 0, "(Other)" = 0)

#Also recode satisfaction and likelihood of deactivation to binary for logit analysis
FacebookActive$Satisfaction.recoded.2 <- recode(FacebookActive$Please.rate.your.overall.satisfaction.with.Facebook., "Extremely satisfied" = 1, "Moderately satisfied" = 1, "Slightly satisfied" = 1, "Neither satisfied nor dissatisfied" = 0, "Slightly dissatisfied" = 0, "Moderately dissatisfied" = 0, "Extremely dissatisfied" = 0, "(Other)" = 0)
FacebookActive$Deactivation.recoded.2 <- recode(FacebookActive$I.consider.deactivating.my.Facebook.account.in.the.next.6.months, "Strongly Agree" = 1, "Agree" = 1, "Somewhat agree" = 1, "Neither agree nor disagree" = 0, "Somewhat disagree" = 0, "Disagree" = 0, "Strongly disagree" = 0, "(Other)" = 0)
FacebookActiveSatisfaction <- FacebookActive %>% filter(FacebookActive$Please.rate.your.overall.satisfaction.with.Facebook. != "(Other)")
FacebookActiveSatisfaction$Satisfaction.recoded.2 <- as.factor(FacebookActiveSatisfaction$Satisfaction.recoded.2)

#Check the resulting structure
summary(FacebookActive)
str(FacebookActive)

#boxplot to visually analyze Facebook perception
boxplot(FacebookActive$Ease.recoded, FacebookActive$Primary.recoded, FacebookActive$Content.recoded, FacebookActive$Trust.recoded, FacebookActive$Privacy.recoded, names = c("Ease of Use", "Primary Network", "Value of Content", "Trustworthiness", "Privacy") )

#Linear regression models
##Satsifaction
linearMod <- lm(FacebookActive$Satisfaction.recoded ~ FacebookActive$Ease.recoded + FacebookActive$Primary.recoded + FacebookActive$Content.recoded + FacebookActive$Trust.recoded + FacebookActive$Privacy.recoded)
summary(linearMod)

##Deactivation
linearMod2 <- lm(FacebookActive$Deactivation.recoded ~ FacebookActive$Ease.recoded + FacebookActive$Primary.recoded + FacebookActive$Content.recoded + FacebookActive$Trust.recoded + FacebookActive$Privacy.recoded)
summary(linearMod2)

##Age as predictor of Facebook perception
linearMod3 <- lm(FacebookActive$Privacy.recoded ~ FacebookActive$Age.recoded)
summary(linearMod3)

linearMod4 <- lm(FacebookActive$Trust.recoded ~ FacebookActive$Age.recoded)
summary(linearMod4)

linearMod5 <- lm(FacebookActive$Ease.recoded ~ FacebookActive$Age.recoded)
summary(linearMod5)

linearMod6 <- lm(FacebookActive$Content.recoded ~ FacebookActive$Age.recoded)
summary(linearMod6)

linearMod7 <- lm(FacebookActive$Content.recoded ~ FacebookActive$Age.recoded)
summary(linearMod7)

#Satisfaction amd likelihood of deactivation including demographic variables
linearMod8 <- lm(FacebookActive$Satisfaction.recoded ~ FacebookActive$Ease.recoded + FacebookActive$Primary.recoded + FacebookActive$Content.recoded + FacebookActive$Trust.recoded + FacebookActive$Privacy.recoded + FacebookActive$Age.recoded + FacebookActive$Which.of.the.following.best.describes.your.gender....Selected.Choice)
summary(linearMod8)

linearMod9 <- lm(FacebookActive$Deactivation.recoded ~ FacebookActive$Ease.recoded + FacebookActive$Primary.recoded + FacebookActive$Content.recoded + FacebookActive$Trust.recoded + FacebookActive$Privacy.recoded + FacebookActive$Age.recoded + FacebookActive$Which.of.the.following.best.describes.your.gender....Selected.Choice)
summary(linearMod9)

##Satisfaction and likelihood of deactivation prediction by gender and marital status
linearMod10 <- lm(FacebookActive$Deactivation.recoded ~FacebookActive$Which.of.the.following.best.describes.your.gender....Selected.Choice)
summary(linearMod10)

linearMod11 <- lm(FacebookActive$Satisfaction.recoded ~ FacebookActive$What.is.your.marital.status.)
summary(linearMod11)

linearMod12 <- lm(FacebookActive$Deactivation.recoded ~ FacebookActive$What.is.your.marital.status.)
summary(linearMod12)

##Likelihood of deactivation prediction by satisfaction
linearMod13 <- lm(FacebookActive$Deactivation.recoded ~ FacebookActive$Satisfaction.recoded)
summary(linearMod13)

#Logit models for Satisfaction and deactivation
logitMod <- glm(Satisfaction.recoded.2 ~ Ease.recoded + Primary.recoded + Content.recoded + Trust.recoded + Privacy.recoded, data=FacebookActiveSatisfaction, family=binomial(link="logit"))
summary(logitMod)
exp(coef(logitMod))

logitMod2 <- glm(Deactivation.recoded.2 ~ Ease.recoded + Primary.recoded + Content.recoded + Trust.recoded + Privacy.recoded, data=FacebookActiveSatisfaction, family=binomial(link="logit"))
summary(logitMod)
exp(coef(logitMod))

#Predicting whether a person has Facebook account by age and employment status
Facebook$Age.recoded <- recode(Facebook$Which.of.the.following.best.represents.your.age., "19 - 24" = 1, "25 - 34" = 2, "35 - 44" = 3, "45 - 54" = 4, "55 or older" = 5, "Prefer not to say" = 0, "(Other)" = 0)
Facebook$Account.recoded <-recode(Facebook$I.have.an.active.Facebook.subscription., "Yes" = 1, "No" = 0)

logitMod3 <- glm(Account.recoded ~ Age.recoded, data=Facebook, family=binomial(link="logit"))
summary(logitMod3)
exp(coef(logitMod3))

aov2 <- aov(Account.recoded ~ Age.recoded, data = Facebook)
summary(aov2)
TukeyHSD(aov2)

aov3 <- aov(Account.recoded ~ What.is.your.current.employment.status., data = Facebook)
summary(aov3)
TukeyHSD(aov3)


