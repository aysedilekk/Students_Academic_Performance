library(MASS)
library("stringr")

library("plyr")
library(ggplot2)

education <- read.csv("C:/Users/Ayse/Desktop/Ceng 4.2/StatisticalComputing/uygulama/proje/xAPI-Edu-Data.csv")
education

print(nrow(education))   # row number
print(ncol(education))   # column number

head(education) 
head(education,4)
summary(education)
colnames(education)    # attribute names

#######################################################################################
par(mfrow = c(1,1))  



edu.plot <- ggplot(data=education, aes(x=gender)) +
  xlab("Gender") 
edu.plot + geom_bar(fill=I("red"))



edu.plot <- ggplot(data=education, aes(x=Class)) +
  xlab("Class") 
edu.plot + geom_bar(fill=I("blue"))

########################################



edu.plot <- ggplot(data=education, aes(x=Topic)) +
  xlab("Topic") 
edu.plot + geom_bar(fill=I("red"))



edu.plot <- ggplot(data=education, aes(x=NationalITy)) +
  xlab("NationalITy") 
edu.plot + geom_bar(fill=I("blue"))




# Most of these countries are in the middle east(Islamic states), perhaps this explains the gender disparity


##########################################



edu.plot <- ggplot(data=education, aes(x=Class)) 
edu.plot + geom_bar(fill=I("red")) + facet_wrap(~gender)


edu.plot <- ggplot(data=education, aes(x=Relation)) 
edu.plot + geom_bar(fill=I("blue")) + facet_wrap(~gender)


edu.plot <- ggplot(data=education, aes(x=StudentAbsenceDays)) 
edu.plot + geom_bar(fill=I("green")) + facet_wrap(~gender)


edu.plot <- ggplot(data=education, aes(x=ParentAnsweringSurvey)) 
edu.plot + geom_bar(fill=I("pink")) + facet_wrap(~gender)

# Girls seem to have performed better than boys
# In the case of girls, mothers seem to be more interested in their education than fathers
# Girls had much better attendance than boys


##################################################




edu.plot <- ggplot(data=education, aes(x=Topic, fill = gender)) +
  xlab("Topic") 
edu.plot + geom_bar()




edu.plot <- ggplot(data=education, aes(x=NationalITy, fill = gender)) +
  xlab("NationalITy") 
edu.plot + geom_bar()

# No apparent gender bias when it comes to subject/topic choices, we cannot conclude that girls performed better because they perhaps took less technical subjects
# Gender disparity holds even at a country level. May just be as a result of the sampling.


##############################################3   ??????????????????????????????  buraya kadar yapýldýý

edu.plot <- ggplot(data=education, aes(x=NationalITy, fill = Relation)) +
  xlab("Topic") 
edu.plot + geom_bar()



edu.plot <- ggplot(data=education, aes(x=NationalITy, fill = StudentAbsenceDays)) +
  xlab("Topic") 
edu.plot + geom_bar()

##################################################

edu.plot <- ggplot(data = education, aes(y = mean(education$VisITedResources), x = Class))
edu.plot + geom_bar(stat="identity",fill=I("red")) + 
  ylab("mean(VisITedResources)") + 
  xlab("Class")


edu.plot <- ggplot(data = education, aes(y = mean(education$AnnouncementsView), x = Class))
edu.plot + geom_bar(stat="identity",fill=I("blue")) + 
  ylab("mean(AnnouncementsView)") + 
  xlab("Class")



edu.plot <- ggplot(data = education, aes(y = mean(education$raisedhands), x = Class))
edu.plot + geom_bar(stat="identity",fill=I("green")) + 
  ylab("mean(raisedhands)") + 
  xlab("Class")



edu.plot <- ggplot(data = education, aes(y = mean(education$Discussion), x = Class))
edu.plot + geom_bar(stat="identity",fill=I("pink")) + 
  ylab("mean(Discussion)") + 
  xlab("Class")


# ?????????????   As expected, those that participated more (higher counts in Discussion, raisedhands, AnnouncementViews, RaisedHands), performed better ...that thing about correlation and causation.

########################################################

# RaiseHands according to gender
edu.plot <- ggplot(education, aes(x = raisedhands)) + 
  xlab("Raisedhands") 
edu.plot + geom_histogram(aes(fill = gender), alpha = 0.5)



# VisITedResources according to gender
edu.plot <- ggplot(education, aes(x = VisITedResources)) + 
  xlab("VisITedResources") 
edu.plot + geom_histogram(aes(fill = gender), alpha = 0.5)



# AnnouncementsView according to gender
edu.plot <- ggplot(education, aes(x = AnnouncementsView)) + 
  xlab("AnnouncementsView") 
edu.plot + geom_histogram(aes(fill = gender), alpha = 0.5)


# Discussion according to gender
edu.plot <- ggplot(education, aes(x = Discussion)) + 
  xlab("Discussion") 
edu.plot + geom_histogram(aes(fill = gender), alpha = 0.5)


# Females are more active and more successfull


###################################################################

par(mfrow = c(1,1))  

boxplot(education$Discussion~education$Class) 
boxplot(education$VisITedResources~education$Class)

# The two plots above tell us that visiting the resources may not be as sure a path to performing well as discussions



###############################################################

ggplot(education, aes(x=raisedhands, y=VisITedResources, color=raisedhands, shape=gender)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  ylab("VisITedResources") + 
  xlab("raisedhands")

  

ggplot(education, aes(x=AnnouncementsView, y=Discussion, color=AnnouncementsView, shape=gender)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  ylab("Discussion") + 
  xlab("AnnouncementsView")

# There does not appear to be much of a linear relationship between the numerical features.


###################################################################


# Achievement rate according to Topics     
edu.plot <- ggplot(data=education, aes(x=Class)) 
edu.plot + geom_bar(fill=I("blue")) + facet_wrap(~Topic)



# Achievement rate according to StudentAbsenceDays     
edu.plot <- ggplot(data=education, aes(x=StudentAbsenceDays)) 
edu.plot + geom_bar(fill=I("red")) + facet_wrap(~Topic)


# (Math is the best)
# (StudentAbsenceDays is low for Arabic,Math and Spanish) 
################################################################33

# Relation rate vs Gender     
edu.plot <- ggplot(data=education, aes(x=Relation)) 
edu.plot + geom_bar(fill=I("pink")) + facet_wrap(~gender)

# ParentschoolSatisfaction vs Gender     (Male's ParentschoolSatisfaction is better)
edu.plot <- ggplot(data=education, aes(x=ParentschoolSatisfaction)) 
edu.plot + geom_bar(fill=I("green")) + facet_wrap(~gender)



# (Male's relation is father mostly)
# (Male's ParentschoolSatisfaction is better)
########################################################

cPalette <- c( "blue","red")
edu.plot <- ggplot(data=education, aes(x=Discussion, y=raisedhands, colour = ParentschoolSatisfaction)) 
edu.plot + geom_point() + scale_colour_manual(values=cPalette)
edu.plot + geom_point() + facet_wrap(~ gender)




edu.plot <- ggplot(data=education, aes(x=raisedhands, y=Discussion, colour = raisedhands)) 
edu.plot + geom_point()


##########################################################################


# HYPOTHESIS
colnames(education)

# Regression
RaiseHands <- education$raisedhands

VisitedResource <- education$VisITedResources

AnnouncementsView <- education$AnnouncementsView
AnnouncementsView

Discussion <- education$Discussion

Class <- education$Class


cor(RaiseHands,VisitedResource)    # 0.6915717
cor(RaiseHands,AnnouncementsView)  # 0.6439178

model <- lm(RaiseHands~VisitedResource) 
summary(model)

model <- lm(RaiseHands~AnnouncementsView) 
summary(model)

# Rate of VisitedResource effects the rate of RaiseHands pozitifly
# AnnouncementsView effects the rate of RaiseHands pozitifly

######################################################

# One sample t-test
RaiseHands

#alpha = 0.05
#we dont know sigma
#mü = 70

shapiro.test(RaiseHands) # p-value = 4.005e-16 > alpha  sample is normally distributed

# Ho: Mü <= 70
# H1: Mü > 70

t.test(RaiseHands, mu=70, alternative ="greater",conf.level = 0.95)

# p-value = 1 > alpha=0.05 so you cannot reject the null hyphothesis, it means rate of RaiseHands is not greater than 70.



# Two sample t-test
sample1 <- education[,10]  # RasieHands
sample2 <- education[,11]  # VisitedResource


#alpha = 0.05
# 1) Independent group
# 2) Paired t-test
# Independent sample t-test

mean(sample1)  # 46.775
mean(sample2)  # 54.79792

# Normality Test
shapiro.test(sample1)   # p-value = 4.005e-16 > alpha  normal distributed
shapiro.test(sample2)   # p-value = 2.2e-16 > alpha  normal distributed

# var-test
var.test(sample1,sample2,conf.level = 0.95)   # p-value = 0.115 > alpha so variances are equal

# t-test
t.test(sample1,sample2,conf.level = 0.95, alternative = "less", var.equal = TRUE)    


# p-value = p-value = 5.356e-05 > alpha  reject the null hypotesis
# VisitedResource is not less than RasieHands



