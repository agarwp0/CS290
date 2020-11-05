library(tidyverse)
library(readxl)
library(xlsx)
# Reading data in from xlsx spreadsheet files
city <- read_xlsx("city_level_SCS.xlsx")
fuzhou <- read_xlsx("SCS_rewards_punishment.xlsx")
ficoC <- read_xlsx("fico_score_city.xlsx")

# Renaming city dataset headers to make my analysis easier
city <- city%>%
  rename(
    System = `Scoring System`,
    Base = `Base Point`
  )

# Renaming fico city dataset headers to make my analysis easier
ficoC <- ficoC %>%
  rename(
    pRank = 'Percentile_Rank',
    avgScore = 'Median_Credit_Score',
    CI_Ratio = 'Credit_Card_Debt_To_Income_Ratio',
    MI_Ratio = 'Mortgage_Debt_To_Income_Ratio',
    CLI_Ratio = 'Car_Loan_Debt_To_Income_Ratio',
    SLI_Ratio = 'Student_Loan_Debt_To_Income_Ratio',
    latePay = 'Late_Payments_Avg'
  )
# knitr::kable(city)
# View(ficoC)

# Renaming fuzhou system dataset headers to make my analysis easier
fuzhou <- fuzhou%>%
  rename(
    Rmin = RMIn
  )
# knitr::kable(fuzhou)


# ficoC Data tidying and manipulation 

# Separating the state data from city column to a separate State column
ficoC <- ficoC %>%
  separate(City, int = c("City", "State"), sep =", ")

# Taking the mean of all cities in the same state, counting the no of cities and
# writing it to a new file.
stateAvg <- ficoC %>%
  group_by(State) %>%
  summarise(meanScore = mean(avgScore), noCity = n())

write.xlsx(stateAvg, file="state_avg.xlsx")


 # Exploratory Analysis of data------- 
  

# Analyzing the city data
#ggplot(city)+
#  geom_freqpoly(aes(Min))+
#  geom_freqpoly(aes(Max))
# ggplot(city, aes())+
#   geom_point(aes(Location, 'Base Point'))
#   geom_boxplot(aes(Location, Min))

# ggplot(ficoC)+
#   geom_histogram(aes(avgScore), binwidth = 1)

# ficoC %>%
#   ggplot()+
#   geom_bar(aes(State))

# stateAvg <- data.frame("State", "meanScore", "noCity")



# Analyzing the fico city data

# Arranging the new data frame of average score in each state in descending
# order. Then taking the top and bottom 5 rows into a new data frame
high_low <- stateAvg %>%
  arrange(desc(meanScore))%>%{
    rbind(head(., 5), tail(., 5))
  }


high_low %>% mutate(Legend = ifelse(meanScore <= 700, "Lowest Score", "Highest Score")) %>%
  ggplot(aes(State, meanScore, color=Legend))+
  geom_point(size= 3)+
  labs(x = "US States", y="Mean Score")


# Showing the different between the Max Penalty and Rewards
ggplot(fuzhou)+
  geom_freqpoly(aes(Pmax, color="Max Penalty"), binwidth=40, na.rm = TRUE)+
  geom_freqpoly(aes(Rmax, color="Max Rewards"), binwidth=40, na.rm = TRUE)+
  labs(x="Reward and Penalty points")

