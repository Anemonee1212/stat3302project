library(ggplot2)
library(lubridate)
library(readxl)
library(tidyverse)

big_5_leagues <- read_excel("data/data1.xlsx")
professional <- read_excel("data/data2.xlsx", range = "A3:F1003")
youth <- read_excel("data/data2.xlsx", sheet = "Data for Study 2", range = "A3:H1211")

# 1st paper: Big Five Leagues =================================================

data1 <- big_5_leagues %>%
  filter(Dom == 1) %>%
  group_by(tBDual) %>%
  summarise(freq = n())

model <- glm(freq ~ tBDual, data = data1, family = "poisson")
summary(model)

fitted_freq <- function(tb) {
  return(exp(model$coefficients[[1]] + model$coefficients[[2]] * tb))
}

data1 %>%
  ggplot(aes(x = tBDual, y = freq)) +
    geom_point() +
    geom_function(fun = fitted_freq, color = "blue", size = 1) +
    ggtitle("RAE bias in 'Big Five' leagues") +
    xlab("time of birth in year (tB)") +
    ylab("Birth frequency")

data_in_catg <- data1 %>%
  mutate(cat = floor(1 + 3 * tBDual)) %>%
  group_by(cat) %>%
  summarise(sum = sum(freq))

data_in_catg %>%
  ggplot(aes(x = cat, y = sum)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = sum), vjust = -0.3, size = 5) +
    ggtitle("Number of births in each period of year") +
    xlab("range of birth time") +
    xlim(c("0 ~ 1/3", "1/3 ~ 2/3", "2/3 ~ 1"))

# 2nd Paper: Professional Players =============================================

data2 <- professional %>%
  mutate(
    nweeks = week(`Date of Birth`),
    wb = ifelse(nweeks == 53, nweeks - 1, nweeks),
    tb = (wb - 0.5) / 52,
    log_value = log(`(Millions of Euros)`)
  ) %>%
  select(Idx, tb, log_value) %>%
  group_by(tb) %>%
  summarise(
    freq = n(),
    mean_log_value = mean(log_value)
  )
data2[52, 2] <- round(data2[52, 2] * 7 / 8.25)

model <- glm(freq ~ tb, data = data2, family = "poisson")
summary(model)

data2 %>%
  ggplot(aes(x = tb, y = freq)) +
    geom_point() +
    geom_function(fun = fitted_freq, color = "blue", size = 1) +
    ggtitle("RAE bias in professional football:\nFREQUENCY analysis") +
    xlab("time of birth in year (tB)") +
    ylab("birth count per week")

model <- glm(mean_log_value ~ tb, data = data2, family = "poisson")
summary(model)

data2 %>%
  ggplot(aes(x = tb, y = mean_log_value)) +
    geom_point() +
    geom_function(fun = fitted_freq, color = "blue", size = 1) +
    ggtitle("RAE bias in professional football:\nVALUE analysis") +
    xlab("time of birth in year (tB)") +
    ylab("mean log value per week")

# 2nd Paper: Youth League =====================================================

data3a <- youth %>%
  filter(`Country of Club` != "ENG") %>%
  filter(Domestic == 1) %>%
  mutate(
    nweeks = week(`Date-of_Birth`),
    wb = ifelse(nweeks == 53, nweeks - 1, nweeks),
    tb = (wb - 0.5) / 52
  ) %>%
  select(Idx, tb, ngames = `Games Played`)
data3b <- youth %>%
  filter(`Country of Club` == "ENG") %>%
  filter(Domestic == 1) %>%
  mutate(
    DOB = ymd(`Date-of_Birth`),
    base_year = ifelse(month(DOB) >= 9, year(DOB), year(DOB) - 1),
    base_date = ymd(paste(base_year, "/09/01", sep = "")),
    nweeks = ceiling(as.double(difftime(DOB, base_date, unit = "weeks"))),
    wb = ifelse(nweeks == 53, nweeks - 1, nweeks),
    tb = (wb - 0.5) / 52
  ) %>%
  select(Idx, tb, ngames = `Games Played`)
data3 <- rbind(data3a, data3b) %>%
  group_by(tb) %>%
  summarise(
    freq = n(),
    mean_games = mean(ngames)
  )
data3[52, 2] <- round(data3[52, 2] * 7 / 8.25)

model <- glm(freq ~ tb, data = data3, family = "poisson")
summary(model)

data3 %>%
  ggplot(aes(x = tb, y = freq)) +
    geom_point() +
    geom_function(fun = fitted_freq, color = "blue", size = 1) +
    ggtitle("RAE bias in youth (U19) football:\nFREQUENCY analysis") +
    xlab("time of birth in year (tB)") +
    ylab("birth count per week")

model <- glm(mean_games ~ tb, data = data3, family = "poisson")
summary(model)

data3 %>%
  ggplot(aes(x = tb, y = mean_games)) +
    geom_point() +
    geom_function(fun = fitted_freq, color = "blue", size = 1) +
    ggtitle("RAE bias in youth (U19) football:\nVALUE analysis") +
    xlab("time of birth in year (tB)") +
    ylab("mean games played per person by birthweek")
