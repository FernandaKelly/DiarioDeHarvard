# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_pred <- ifelse(trei$height > x, "Male", "Female") %>% 
    factor(levels = levels(teste$sex))
  F_meas(data = y_pred, reference = factor(trei$sex))
})
max(F_1)
plot(F_1)


best_cutoff <- cutoff[which.max(F_1)]
y_pred <- ifelse(teste$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(teste$sex))
sensitivity(data = y_pred, reference = teste$sex)
specificity(data = y_pred, reference = teste$sex)

confusionMatrix(y_pred, reference = teste$sex)