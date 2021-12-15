## REPLICATION CODE FOR: #####################################################################################
#
# Guess, Nagler, and Tucker. "Less than you think: Prevalence and predictors of fake news dissemination
#                             on Facebook." Science Advances 5, eaau4586 (2019).
# DOI: 10.1126/sciadv.aau4586
#
##############################################################################################################

# load data
load("dataset/gnt_replication_data.RData")

# packages 
library(xtable)
library(stargazer)
library(ggplot2)
library(tidyverse)
library( MASS)




## figure 1
dat %>% 
  filter(party != "Other/Not sure") %>%
  ggplot(aes(num_posts, fill = party))  + 
  geom_histogram(bins = 50) +
  scale_fill_manual("", values = c("blue","red","darkgray")) +
  ggtitle("Distribution of News Shares") + ylab("Number of respondents") +
  xlab("Number of web links shared") + theme(legend.position = "bottom", legend.title = element_blank()) + labs(title = NULL) +
  theme_classic()

## figure 2
dat %>% 
  filter(party != "Other/Not sure") %>%
  ggplot(aes(num_fake_shares, fill = party))  + 
  scale_fill_manual("", values = c( "blue", "red"," darkgray")) +
  geom_histogram(bins = 50) +
  ggtitle("Distribution of Fake News Shares") + ylab("Number of respondents") +
  xlab("Number of web links shared") + theme(legend.position = "bottom", legend.title = element_blank()) + labs(title = NULL) +
  theme_classic()


# models 
#table 1
model.cp1a <- glm(num_fake_shares ~ ideology + age + female + black + educ + faminc, 
                  weights = weight_svypop_w3, data = dat, family = "quasipoisson")

model.cp1a1 <- glm.nb(num_fake_shares ~ ideology + age + female + black + educ + faminc, 
                  weights = weight_svypop_w3, data = dat)

#table 2
model.cp1 <- glm(num_fake_shares ~ ideology + age + female + black + educ + faminc + num_posts, 
                 weights = weight_svypop_w3, data = dat, family = "quasipoisson")

model.cp11 <- glm.nb(num_fake_shares ~ ideology + age + female + black + educ + faminc + num_posts, 
                 weights = weight_svypop_w3, data = dat)

# explore interactiion terms 
# interaction model1 1 table 3
model.cp1a_int <- glm(num_fake_shares ~ as.numeric(ideology) + as.numeric(age) + female + black + educ + faminc + num_posts +  as.numeric(ideology) * as.numeric(age), 
                  weights = weight_svypop_w3, data = dat, family = "quasipoisson")

model.cp1a1_int <- glm.nb(num_fake_shares ~ as.numeric(ideology) + as.numeric(age) + female + black + educ + faminc + num_posts +  as.numeric(ideology) * as.numeric(age), 
                  weights = weight_svypop_w3, data = dat)

# interaction model2 2 table 4

model.cp1a_int1 <- glm(num_fake_shares ~ as.numeric(ideology) + as.numeric(age) + female + black + educ + faminc + num_posts + black * as.numeric(ideology) + black * as.numeric(age),  weights = weight_svypop_w3, data = dat, family = "quasipoisson")

model.cp1a1_int1 <- glm.nb(num_fake_shares ~ as.numeric(ideology) + as.numeric(age) + female + black + educ + faminc + num_posts + black * as.numeric(ideology) + black * as.numeric(age), weights = weight_svypop_w3, data = dat)



# rename variable names 
varnames <- c("(Intercept)", "Very Liberal", "Liberal",
              "Moderate", "Conservative", "Very Conservative", 
              "Age: 30-44", "Age: 45-65", "Age: Over 65", "Female", "Black", 
              "Education", "Income", "Number of links shared") 

# interaction term included
varnames1 <- c("(Intercept)", "Ideology (Conservative)", "Age (Old)", "Female", "Black", 
              "Education", "Income", "Number of links shared","Ideology (Conservative) x Age (Old)") 

## table1
stargazer(model.cp1a1,model.cp1a,
          header = FALSE,
          type = "latex",
          digits=2, 
          label = "tab:table1",
          # report = "vc*",
          # column.labels = c("Negative binomial regression model","Quasi-Poisson regression model"),
          style = "apsr",
          # align = TRUE,   
          title = "Determinants of Fake News Sharing on Facebook",
          covariate.labels = c(varnames[c(-1,-14)]),
          dep.var.labels = c("Number of Stories Shared", "Number of Stories Shared (A\\&G)"), 
          keep.stat = c("n", "null.dev", "res.dev"), 
          notes = "Negative binomial regression vs. Quasi-Poisson models.")
##table2
stargazer(model.cp11,model.cp1,
          header = FALSE,
          type = "latex",
          digits=2, 
          label = "tab:table2",
          # report = "vc*",
          # column.labels = c("Negative binomial regression model","Quasi-Poisson regression model"),
          style = "apsr",
          # align = TRUE,   
          title = "Determinants of Fake News Sharing on Facebook",
          covariate.labels = c(varnames[-1]),
          dep.var.labels = c("Number of Stories Shared", "Number of Stories Shared (A\\&G)"), 
          keep.stat = c("n", "null.dev", "res.dev"), 
          notes = "Negative binomial regression vs. Quasi-Poisson models.")
## table 3
stargazer(model.cp1a1_int,model.cp1a_int,
          header = FALSE,
          type = "latex",
          # digits=2, 
          # float = FALSE,
          label = "tab:table3",
          # report = "vc*",
          # column.labels = c("Negative binomial regression model","Quasi-Poisson regression model"),
          style = "apsr",
          # align = TRUE,   
          title = "Determinants of Fake News Sharing on Facebook",
          covariate.labels = c(varnames1[-1]),
          dep.var.labels = c("Number of Stories Shared", "Number of Stories Shared (A\\&G)"), 
          keep.stat = c("n", "null.dev", "res.dev"), 
          notes = "Negative binomial regression vs. Quasi-Poisson models.")

## table 4
stargazer(model.cp1a1_int1,model.cp1a_int1,
          header = FALSE,
          type = "latex",
          # digits=2, 
          # float = FALSE,
          label = "tab:table4",
          # report = "vc*",
          # column.labels = c("Negative binomial regression model","Quasi-Poisson regression model"),
          style = "apsr",
          # align = TRUE,   
          title = "Determinants of Fake News Sharing on Facebook",
          covariate.labels = c("Ideology (Conservative)", "Age (Old)", "Female", "Black", 
              "Education", "Income", "Number of links shared","Ideology (Conservative) x Black", "Age (Old) x Black"),
          dep.var.labels = c("Number of Stories Shared", "Number of Stories Shared (A\\&G)"), 
          keep.stat = c("n", "null.dev", "res.dev"), 
          notes = "Negative binomial regression vs. Quasi-Poisson models.")

