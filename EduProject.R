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