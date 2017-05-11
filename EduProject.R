library(readr)
edudata <- read_delim("C:/Users/Dnllz/Downloads/edudata.txt", 
                        +     "\t", escape_double = FALSE, col_types = cols(X8 = col_skip()), 
                        +     trim_ws = TRUE)
names(edudata) <- c("age","geneder","ethnicity","subjectNbr","group","session","grade")
