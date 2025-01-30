#Name: Kaylhan
library(tidyverse)
library(Sleuth3)
library(psych)
data(case0102)

help(case0102)

# Using a pipe describe Salaries by Sex
case0102 %>%
  group_by(Sex) %>%
  summarise(n=n(), Average=mean(Salary), SD=sd(Salary),
            Median=median(Salary), IQR=IQR(Salary),
            Min=min(Salary), Max=max(Salary),
            Q1=quantile(Salary,0.25), Q3=quantile(Salary, 0.75)) %>%
  knitr::kable(digits = 3)


describe(case0102$Salary , skew= FALSE, ranges = TRUE, IQR = TRUE, omit = TRUE)


case0102 <- case0102 %>%
  mutate(Sex = factor(Sex, levels=c('Male', 'Female')))
levels(case0102$Sex)

#Visualizations

ggplot(data = case0102, mapping = aes(y = Salary, x = Sex)) +
  geom_boxplot(fill="blue")+
  ylab("Salary")+
  xlab("Gender")

ggplot(data = case0102, mapping = aes(x = Salary, fill = Sex)) +
  geom_density(alpha = 0.3)+
  xlab("Salary")+
  theme_bw()

#Hypothesis testing 

t.test(Salary ~ Sex, var.equal = TRUE, data = case0102)
