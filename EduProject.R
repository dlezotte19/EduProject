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
