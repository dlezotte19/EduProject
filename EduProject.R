library(readr)
edudata <- read_delim("C:/Users/Dnllz/Downloads/edudata.txt", 
                             "\t", escape_double = FALSE, col_types = cols(Age = col_skip(), 
                                Gender = col_skip(), `Instruction Mode Group` = col_skip(), 
                             `Subject Catalog Nbr` = col_skip(), 
                              X8 = col_skip()), trim_ws = TRUE)
names(edudata) <- c("ethnicity","session","grade")

#Trimming data to only contain the ethnicities we want to test.
edudata2 <- rbind(edudata[edudata$ethnicity == "Asian",], 
                  edudata[edudata$ethnicity == "White",], 
                  edudata[edudata$ethnicity == "Black/African American",])

#getting data to convert to binomial(pass/notPass).
table(edudata2)

#inputting converted binomial data into final data.
edudataF <- data.frame("ethnicity" = rep(c("asian","black","white"),2),
                       "Session" = rep(c("8","14"),each = 3),
                       "Pass" = c(111,156,1467,54,109,863),
                       "notPass" = c(17,139,379,3,84,222))

#building linear regression models
model1 <- glm(cbind(Pass,notPass)~ethnicity*Session,family = binomial,data = edudataF)
model2 <- glm(cbind(Pass,notPass)~ethnicity+Session,family = binomial,data = edudataF)
model3 <- glm(cbind(Pass,notPass)~ethnicity,family = binomial,data = edudataF)
model4 <- glm(cbind(Pass,notPass)~Session,family = binomial,data = edudataF)

#testing for best fit model
anova(model1,model2, test = "Chisq") #P-value = .2077: simpler model(model2) fits better.
anova(model2,model3, test = "Chisq") #P-value = .5135: simpler model(model3) fits better.
anova(model3,model4, test = "Chisq") #P-value = approximately 0: simpler model(model4) does not fit better.
#conclusion: Model3 is best fitting model; session proven insignificant to model.

summary(model3)
#display the slope and intercept parameters for the regession line.
###Model###
#  (Probability of Passing class) = 2.1102 - 1.9377(Black) - .7552(White)
# Where:
# Black {1 = yes, 0 = no}
# White {1 = Yes, 0 = no}

#95% confident that the true slope parameters are within this range.
confint(model3)
#Because 0 does not fall inside any of the parameters, we are 95% confident the variables are significant.


#finding estimated odds ratios
exp(coefficients(model3))
#Conclusion: The odds of a white student passing the class is .47 that of an Asain passing the class.
#The odds of a black student passing the class is .144 that of an Asain passing the class.
#Session and Session*Ethnicity interactions proved to be insignificant.

################MAC2233################

#importing data
macData <- read_csv("C:/Users/Dnllz/Downloads/2014 2015 MAC-2233.csv", 
              col_types = cols(Age = col_skip(), Gender = col_skip(), 
              `Instruction Mode Group` = col_skip(), 
              `Subject Catalog Nbr` = col_skip()))

#Rename column headings
names(macData) <- c("ethnicity", "session", "descript", "grade")
macData$GPA <- ifelse(macData$grade == "A",4,ifelse(macData$grade == "B",3, 
                                                    ifelse(macData$grade == "C",2,
                                                           ifelse(macData$grade == "D",1,0))))

#Trimming data to only contain the ethnicities we want to test.
macData2 <- rbind(macData[macData$ethnicity == "Asian",], 
                  macData[macData$ethnicity == "White",], 
                  macData[macData$ethnicity == "Black/African American",])

#creating models to test
macmodel1 <- glm(GPA~ethnicity*session,family = poisson,data = macData2)
macmodel2 <- glm(GPA~ethnicity+session,family = poisson,data = macData2)
macmodel3 <- glm(GPA~ethnicity,family = poisson,data = macData2)
macmodel4 <- glm(GPA~session,family = poisson,data = macData2)

#testing to find best model
anova(macmodel1,macmodel2,test = "Chisq") #pvalue = .106: model2 is proven a better fit
anova(macmodel2,macmodel3,test = "Chisq") #pvalue = approxiamtely 0: model2 is proven a better fit
anova(macmodel2,macmodel4,test = "Chisq") #pvalue = approximately 0: model2 is proven a better fit

###model2 is the best fit model.
coefficients(macmodel2)
# estimated grade = .986 - .732(Black) -0.339(white) - .0433(Session10W) -0.426(Session12W)
# Where:
# Black {1 = Black, 0 = not Black}
# White {1 = white, 0 = not white}
# Session10W {1 = Session10W, 0 = Not Session12W}
# Session12W {1 = Session12W, 0 = NOt Session12W}

#Determining 95% confidence intervals:
confint(macmodel2)
# 0 is contained within the interval for "Session10W", thus there is not significant evidence to show..
# ..a difference between Session10W and Session1. However, 0 is not found in the interval for any ethnicity..
#..or for session12W, making them significant to grade.

###Conclusion:
#White students obtain an average grade of about 1/3 a letter grade less than Asian students.
#Black students obtain an average grade of about .73 a letter grade less than Asian students.
#Students in Session12W Received a grade of about .42 a letter grade than other sessions.

#########t test to compare two classes#######

#converting letter grade to numeric gpa.
edudata2$GPA <- ifelse(edudata2$grade == "A",4,ifelse(edudata2$grade == "B",3, 
                                                    ifelse(edudata2$grade == "C",2,
                                                           ifelse(edudata2$grade == "D",1,0))))

#average grade in ACG2021
mean(edudata2$GPA, na.rm = T) # 2.607

#average grade in MAC2233
mean(macData2$GPA, na.rm = T) # 1.873

t.test(edudata2$GPA,macData2$GPA)
#data:  edudata2$GPA and macData2$GPA
#t = 17.714, df = 4369.2, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.6526390 0.8150749
#sample estimates:
#  mean of x mean of y 
#2.607103  1.873246

#Conclusion: We are 95% certain students in ACG2021 scored on average between (.653 - .815) higher..
#..than the students taking MAC2233

###############PSY1012##################

#import data
library(readr)
psyData <- read_csv("C:/Users/Dnllz/Downloads/2014- 2015  PSY.csv", 
                           col_types = cols(Age = col_skip(), Gender = col_skip(), 
                                            `Instruction Mode Group` = col_skip(), 
                                            `Subject Catalog Nbr` = col_skip()))

#Rename column headings
names(psyData) <- c("ethnicity", "session", "grade")
psyData$GPA <- ifelse(psyData$grade == "A",4,ifelse(psyData$grade == "B",3, 
                                                    ifelse(psyData$grade == "C",2,
                                                           ifelse(psyData$grade == "D",1,0))))

#Trimming data to only contain the ethnicities we want to test.
psyData2 <- rbind(psyData[psyData$ethnicity == "Asian",], 
                  psyData[psyData$ethnicity == "White",], 
                  psyData[psyData$ethnicity == "Black/African American",])

#creating models to test
psymodel1 <- glm(GPA~ethnicity*session,family = poisson,data = psyData2)
psymodel2 <- glm(GPA~ethnicity+session,family = poisson,data = psyData2)
psymodel3 <- glm(GPA~ethnicity,family = poisson,data = psyData2)
psymodel4 <- glm(GPA~session,family = poisson,data = psyData2)

#testing to find best model
anova(psymodel1,psymodel2,test = "Chisq") #pvalue = .3978: model2 is proven a better fit
anova(psymodel2,psymodel3,test = "Chisq") #pvalue = 0.000278: model2 is proven a better fit
anova(psymodel2,psymodel4,test = "Chisq") #pvalue = approximately 0: model2 is proven a better fit

###model2 is the best fit model.
coefficients(psymodel2)
# estimated grade = 1.046 - .39(Black) -0.071(white) - .015(Session10) -0.0014(Session12)+.083(session120W)..
#..-0.075(Session122W)+0.089(session8)+0.097(Session8W12)-0.11(sessionDD)-0.684(sessionWEK)
# Where:
# Black {1 = Black, 0 = not Black}
# White {1 = white, 0 = not white}
# Session10 {1 = Session10, 0 = Not Session12}
# Session12 {1 = Session12, 0 = NOt Session12}
# Session120W {1 = Session120W, 0 = NOt Session120W}
# Session122W {1 = Session122w, 0 = NOt Session122w}
# Session8 {1 = Session8, 0 = NOt Session8}
# Session8W12 {1 = Session8W12, 0 = NOt Session8W12}
# SessionDD {1 = SessionDD, 0 = NOt SessionDD}
# SessionWEK {1 = SessionWEK, 0 = NOt SessionWEK}

#Determining 95% confidence intervals:
confint(psymodel2)
#sufficient evidence to show a significant difference for: All ethnicities, Session120W, Session122W..
#..sessionWEK

###Conclusion:
#White students obtained an average grade of .07 a letter grade less than asian students, and black students received average grade .39 of a letter grade
#less than asain students. Students in sessionWEK performed the worst(.69 of a letter grade lower than session 1), where as students in session120W performed the best..
#(.083 of a ltter grade higher than session 1 students)

########################EOC data################################


##importing data
library(readr)
eocData <- read_csv("C:/Users/Dnllz/Downloads/ecoData.csv", 
                    col_types = cols(Age = col_skip(), Gender = col_skip(), 
                                     `Instruction Mode Group` = col_skip(), 
                                     `Session Description` = col_character(), 
                                     `Subject Catalog Nbr` = col_skip(), 
                                     X4 = col_skip()))

#changing column headers and adding gpa column.
names(eocData) <- c("ethnicity", "session", "grade")
eocData$GPA <- ifelse(eocData$grade == "A",4,ifelse(eocData$grade == "B",3, 
                                                    ifelse(eocData$grade == "C",2,
                                                           ifelse(eocData$grade == "D",1,0))))
#Trimming data to only contain the ethnicities we want to test.
eocData2 <- rbind(eocData[eocData$ethnicity == "Asian",], 
                  eocData[eocData$ethnicity == "White",], 
                  eocData[eocData$ethnicity == "Black/African American",])

#creating models to test
eocmodel1 <- glm(GPA~ethnicity*session,family = poisson,data = eocData2)
eocmodel2 <- glm(GPA~ethnicity+session,family = poisson,data = eocData2)
eocmodel3 <- glm(GPA~ethnicity,family = poisson,data = eocData2)
eocmodel4 <- glm(GPA~session,family = poisson,data = eocData2)

#testing to find best model
anova(eocmodel1,eocmodel2,test = "Chisq") #pvalue = .6202: model2 is proven a better fit
anova(eocmodel2,eocmodel3,test = "Chisq") #pvalue = appromimately 0: model2 is proven a better fit
anova(eocmodel2,eocmodel4,test = "Chisq") #pvalue = approximately 0: model2 is proven a better fit

###model2 is the best fit model.
coefficients(eocmodel2)
# estimated grade = 1.09 - .329(Black) -0.1459(white) - .006(session10) - 0.045(session12) -.204(session8)..
# -.3901(sessionExpress) + .3088(sessionWeekend)
# Where:
# Black {1 = Black, 0 = not Black}
# White {1 = white, 0 = not white}
# Session10 {1 = Session10, 0 = Not Session12}
# Session12 {1 = Session12, 0 = NOt Session12}
# Session8 {1 = Session8, 0 = NOt Session8}
# SessionExpress {1 = SessionExpress, 0 = NOt SessionEspress}
# SessionWeekend {1 = Sessionweekend, 0 = NOt Session}

#Determining 95% confidence intervals:
confint(eocmodel2)
#sufficient evidence to show a significant difference for: All ethnicities,session8,sessionExpress,sessionWeekend

###Conclusion:
#White students obtained an average grade .14 of a letter grade less than asian students, and black students received am average grade .33 of a letter..
#grade less than asian students. Students in sessionExpression performed the worst(.39 of a letter grade than session 10), where as students in session..
#Weekend performed the best(.309 of a letter grade higher than session10)


##############################ENC data###########################

#import data
library(readr)
encData <- read_csv("C:/Users/Dnllz/Downloads/ENCdata.csv", 
                    col_types = cols(Age = col_skip(), Gender = col_skip(), 
                                     `Instruction Mode Group` = col_skip(), 
                                     `Session Description` = col_character(), 
                                     `Subject Catalog Nbr` = col_skip()))

#convert numberic ethnicity code to descriptive ethnicity.
encData$ethnicity <- ifelse(encData$`Ethnic Group Description` == "1","White",
                            ifelse(encData$`Ethnic Group Description` == "2","Asian", 
                                ifelse(encData$`Ethnic Group Description` == "3", "Black/African American",encData$`Ethnic Group Description`)))

#changing column headers and adding gpa column.
names(encData) <- c("ethnicityD", "session", "grade","ethnicity")
encData$GPA <- ifelse(encData$grade == "A",4,ifelse(encData$grade == "B",3, 
                                                    ifelse(encData$grade == "C",2,
                                                           ifelse(encData$grade == "D",1,0))))
#Trimming data to only contain the ethnicities we want to test.
encData2 <- rbind(encData[encData$ethnicity == "Asian",], 
                  encData[encData$ethnicity == "White",], 
                  encData[encData$ethnicity == "Black/African American",])

#creating models to test
encmodel1 <- glm(GPA~ethnicity*session,family = poisson,data = encData2)
encmodel2 <- glm(GPA~ethnicity+session,family = poisson,data = encData2)
encmodel3 <- glm(GPA~ethnicity,family = poisson,data = encData2)
encmodel4 <- glm(GPA~session,family = poisson,data = encData2)

#testing to find best model
anova(encmodel1,encmodel2,test = "Chisq") #pvalue = .047: model 1 and model 2 fit similiarly well.
anova(encmodel2,encmodel3,test = "Chisq") #pvalue = appromimately 0: model2 is proven a better fit
anova(encmodel2,encmodel4,test = "Chisq") #pvalue = approximately 0: model2 is proven a better fit

###Decided on model 2 for analysis
coefficients(encmodel2)
# estimated grade = .982 - .023(Black) + 0.057(white) - .2195(session12) - 0.363(session6) -.2211(session8)..
# +.152(sessionDynamic)
# Where:
# Black {1 = Black, 0 = not Black}
# White {1 = white, 0 = not white}
# Session12 {1 = Session12, 0 = NOt Session12}
# Session6 {1 = Session6, 0 = NOt Session6}
# Session8 {1 = Session8, 0 = NOt Session8}
# SessionDynamic {1 = SessionDynamic, 0 = NOt SessionDynamic}


#Determining 95% confidence intervals:
confint(encmodel2)
#sufficient evidence to show a significant difference for: White Vs Asian,session8,session12,session6

###Conclusion:
#White students recieved an average grade .05 letter grade higher than Asian students.
#There is not sufficient evidence to show black students scored any differently than Asian or White students.
#Students in session12 recieved a letter grade .21 lower than session10.
#Students in session8 recieved a letter grade .22 lower than session10.
#Students in session6 recieved a letter grade .36 lower than session10.(worst performing session)


###########Comparing classes with a T-test########
t.test(edudata2$GPA,macData2$GPA)

#Conclusion: We are 95% certain students in ACG2021 scored on average between (.653 - .815) higher..
#..than the students taking MAC2233

t.test(edudata2$GPA,psyData2$GPA)
#Conclusion: We are 95% certain students in ACG2021 scored on average between (.026 - .146) higher..
#..than the students taking psy1012

t.test(edudata2$GPA,eocData2$GPA)
#Conclusion: We are 95% certain students in ACG2021 scored on average between (.21 - .35) higher..
#..than the students taking eoc2013

t.test(edudata2$GPA,encData2$GPA)
#Conclusion: We are 95% certain students in ACG2021 scored on average between (.294 - .405) higher..
#..than the students taking enc1101.



t.test(macData2$GPA,psyData2$GPA)
#Conclusion: We are 95% certain students in MAC233 scored on average between (.575 - .721) lower..
#..than the students taking psy1012

t.test(macData2$GPA,eocData2$GPA)
#Conclusion: We are 95% certain students in MAC2233 scored on average between (.368 - .531) lower..
#..than the students taking eoc2013

t.test(macData2$GPA,encData2$GPA)
#Conclusion: We are 95% certain students in MAC2233 scored on average between (.315 - .453) lower..
#..than the students taking enc1101.



t.test(psyData2$GPA,eocData2$GPA)
#Conclusion: We are 95% certain students in PSY1012 scored on average between (.139 - .259) higher..
#..than the students taking eoc2013

t.test(psyData2$GPA,encData2$GPA)
#Conclusion: We are 95% certain students in PSY1012 scored on average between (.222 - .306) higher..
#..than the students taking enc1101.



t.test(eocData2$GPA,encData2$GPA)
#Conclusion: We are 95% certain students in EOC2013 scored on average between (.010 - .121) higher..
#..than the students taking enc1101.



#########################correlation analyis###################

install.packages("ggpubr")
library(ggpubr)

psySample <- sample(psyData2$GPA,3000)
acgSample <- sample(edudata2$GPA,3000)

cor.test(psySample,acgSample, method = c("pearson", "kendall", "spearman")) #Output:NA. workaround below

#Will var based on sample
cor.test(psySample,acgSample, method = "pearson") #Output: -.0279
cor.test(psySample,acgSample, method = "kendall") #Output: -.0234
cor.test(psySample,acgSample, method = "spearman") #Output: -.0284

mergedSamples <- data.frame("psyGrade" = psySample, "acgGrade" = acgSample)

plot(mergedSamples)

ggscatter(mergedSamples,mergedSamples$psyGrade, mergedSamples$acgGrade,
          xlab = "psyGrades", ylab = "acgGrades")