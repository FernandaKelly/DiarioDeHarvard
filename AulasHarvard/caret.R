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


y_pred = ifelse(x>62, "Male", "Female") %>% factor(levels = levels(teste$sex))

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


#tarefinha

library(dslabs)
mnist = read_mnist()
names(mnist)

str(mnist)
