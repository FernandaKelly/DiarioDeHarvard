library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & 
           date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & 
                         hour(date_time) == 8 & 
                         between(minute(date_time), 15, 30), 
                       "inclass","online")) %>%
  select(sex, type)



y <- factor(dat$sex, c("Female", "Male"))
x <- factor(dat$type, c("inclass", "online"))



#1

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

#2

y_pred <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_pred == y)

#3

table( predicted = y_pred, actual = dat$sex)
table(y_pred, y)

#4

sensitivity(y_pred,y )

#5

specificity(y_pred,y)

#6

mean(y == "Female") #em media quantas mulheres tem  


#7

library(caret)
data(iris)
iris = iris[-which(iris$Species=='setosa'),]
y = iris$Species


library("caret")
library(magrittr)
library(dplyr)
library("purrr")
library(ggplot2)
library(plotly)

data("heights")


x = heights$height
y = heights$sex

set.seed(123)
part = createDataPartition(y, times = 1, p = 0.5, list = FALSE)

trei = heights[-part,]
teste = heights[part,]


y_pred = sample(c("Male", "Female"), length(part), replace = TRUE) %>% 
  factor(levels = levels(teste$sex))

mean(y_pred == teste$sex )


heights %>% group_by(sex) %>% summarise(mean(height), sd(height))


y_pred = ifelse(x>62, "Male", "Female") %>%
  factor(levels = levels(teste$sex))

mean(y == y_pred)

###cutoff

cutoff = seq(61,70)
acuracia = map_dbl(cutoff, function(x){
  y_pred = ifelse(trei$height > x, "Male", "Female") %>% 
    factor(levels = levels(teste$sex))
  mean(y_pred == trei$sex)
  
})



ac = data.frame(acuracia)
plot_ac = ggplot(ac, aes(x = 1:10, y = acuracia)) + geom_point()
ggplotly(plot_ac)


max(acuracia)
melhor_cutoff = cutoff[which.max(acuracia)]

y_pred = ifelse(teste$height > melhor_cutoff, "Male", "Female") %>%
  factor(levels = levels(teste$sex))
y_pred = factor(y_pred)
mean(y_pred == teste$sex)


