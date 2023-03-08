# ============================================================================
# Functions
# ============================================================================

# 1). Function to change likert data to numeric 

factorise <- function(x) {
  case_when(x %in% c("Not familiar at all", "Strongly disagree", "Never", "Definitely no",
                     "Less than once per month", "Less than once a week", "Less frequently than every 14 days") ~ 1,
            x %in% c("Slightly familiar", "Somewhat disagree", "Sometimes", 
                     "Probably not", "About once a month", "Once a week", "Approximately every 10 - 14 days") ~ 2,
            x %in% c("Moderately familiar", "Neither agree nor disagree", "About half the time", "Might or might not",
                     "A few times a month", "Multiple times a week", "Approximately once a week  ") ~ 3,
            x %in% c("Very familiar", "Somewhat agree", "Most of the time", "Probably yes",
                     "A few times a week", "Once a day", "Multiple times a week") ~ 4,
            x %in% c("Extremely familiar", "Strongly agree", "Always", 
                     "Definitely yes", "About once a day", "Multiple times a day", "Daily") ~ 5, 
            x %in% c("Several times a day") ~ 6) 
}

# The end