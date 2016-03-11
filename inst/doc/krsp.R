## ----options, include=F--------------------------------------------------
knitr::opts_chunk$set(warning = FALSE)
options(knitr.table.format = 'markdown')

## ----st-verbs------------------------------------------------------------
#install.packages("dplyr")
library(dplyr)
mtc <- tbl_df(mtcars)
mtc <- select(mtc, mpg, cyl, horse_power = hp)
mtc
filter(mtc, cyl == 4, horse_power < 70)
arrange(mtc, desc(cyl), horse_power)
mutate(mtc, mpg_per_hp = mpg / horse_power)
summarize(mtc, mean_mpg = mean(mpg), num_records = n())

## ----pipe-example--------------------------------------------------------
mean(sqrt(exp(mtc$mpg)))
mtc$mpg %>% 
  exp %>% 
  sqrt %>% 
  mean

## ----pipe-dplyr----------------------------------------------------------
mtc %>% 
  filter(cyl == 4, horse_power < 70) %>% 
  mutate(mpg_per_hp = mpg / horse_power) %>% 
  arrange(mpg_per_hp)

## ----group-by------------------------------------------------------------
mtc %>% 
  group_by(cyl) %>% 
  summarise(mean_mpg = mean(mpg))

