library(tidyverse);library(modelr);library(ggplot2)

prospects <- data.frame(
  name = paste0("Player",1:1000),
  skill = rnorm(1000,0,1),
  bias = rnorm(1000,0,2),
  randomness = runif(1000,-2,2)
) %>%
  mutate(makes_nhl = ifelse(2*bias + skill>1,1,0),
         true_value = skill + bias + randomness)


draft_mod <- glm(makes_nhl ~ skill, 
                 data=prospects, 
                 family="binomial")

value_mod <- glm(true_value ~ skill, 
                 data=prospects %>% 
                   filter(makes_nhl==1))

results <- prospects %>%
  add_predictions(draft_mod,"xDraft",type="response") %>%
  add_predictions(value_mod,"xValue|Draft") %>%
  mutate(xValue = xDraft*`xValue|Draft`,
         error_cond = true_value - `xValue|Draft`,
         error_total = true_value - xValue)

ggplot(results,aes(x=`xValue|Draft`,y=true_value,
                   color = bias)) +
  geom_point() +
  scale_color_gradientn(colours = c("red","grey","blue")) +
  geom_abline(slope = 1,intercept = 0)

ggplot(results,aes(x=xValue,y=true_value,
                   color = bias)) +
  geom_point()+
  scale_color_gradientn(colours = c("red","grey","blue")) +
  geom_abline(slope = 1,intercept = 0)
