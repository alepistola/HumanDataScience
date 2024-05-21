#install.packages("lme4")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("stargazer")
#install.packages("ggeffects")
#install.packages("sjPlot")
#install.packages("magrittr")
#install.packages("margins")
#install.packages("readxl")
#install.packages("lmerTest")
#install.packages("ggeffects")
#install.packages("dotwhisker")
#install.packages("MuMIn)
#install.packages("merTools")

library(lme4)
library(ggplot2)
library(merTools)
library(dplyr)
library(stargazer)
library(ggeffects)
library(sjPlot)
library(magrittr)
library(margins)
library(readxl)
library(lmerTest)
library(ggeffects)
library(dotwhisker)
library(MuMIn)

## load the data and have a look at it
dataset <- read_excel("final_dataset_2.xlsx")

# nuova feature per explicit nesting
# dataset$countryRegion <- paste(dataset$Country, dataset$Region, sep = "-")
# mixed.lmer3 <- lmer(log(GDPcapita) ~ NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure + (1 + NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure|countryRegion) + (1 + NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure|Year) , data = dataset)

mixed.lmer2 <- lmer(log(GDPcapita) ~ NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure + (1 + NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure|Region/Country) + (1 + NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure|Year) , data = dataset)
summary(mixed.lmer2)

# plotto i residuali del modello
plot(fitted(mixed.lmer2), residuals(mixed.lmer2), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(mixed.lmer2), residuals(mixed.lmer2)))


# conditional variance = variance explained by the entire model
# marginal = variance explained by the fixed effects
# mixed.lmer2null <- lmer(log(GDPcapita) ~ (1 + NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure|Year) + (1 + NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure|Region/Country), data = dataset)
mixed.lmer1 <- lmer(log(GDPcapita) ~  NoSchooling + PrimaryTotal + SecondaryTotal + TertiaryTotal + AvgYearsOfTotalSchooling * EducationalExpenditure + (1|Region), data = dataset)
r.squaredGLMM(mixed.lmer2)
r.squaredGLMM(mixed.lmer1)
# r.squaredGLMM(mixed.lmer2null)

# Visualise random effects 
(re.effects <- plot_model(mixed.lmer2, type = "re", show.values = TRUE))

# tabella effetti fissi
stargazer::stargazer(mixed.lmer2, type = "text", digits = 3, star.cutoffs = c(0.05, 0.01, 0.001), digit.separator = "")

# AME per avgYearsofTotalSchooling
ggpredict(mixed.lmer2, terms = c("AvgYearsOfTotalSchooling", "Year"), type = "re") %>% 
  plot() +
  labs(x = "AvgYearsOfTotalSchooling", y = "GDPcapita", title = "Avg schooling on GDPcapita") + 
  theme_minimal()

# AME per educational exp
ggpredict(mixed.lmer2, terms = c("EducationalExpenditure", "Region"), type = "re") %>% 
  plot() +
  labs(x = "EducationalExpenditure", y = "GDPcapita", title = "Educational expenditure on GDPcapita") + 
  theme_minimal()


# plotto i coefficienti stimati senza l'intercetta che vale 9 e modifica la scala del grafico
coefplot::coefplot(mixed.lmer2, intercept = FALSE)

# stampo gli effetti marginali medi di ogni variabile su GDPcapita
risultati_margins <- margins(mixed.lmer2, data=dataset)
summary(risultati_margins)

# plot AME
dwplot(mixed.lmer2, margins = TRUE)

# stampa i coefficienti, type=pred -> AME
plot_model(mixed.lmer2, type = "eff")

# Effect ranges -> variabilità per gruppo
plotREsim(REsim(mixed.lmer2))

# plot finale per la feature di interesse
# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("EducationalExpenditure"))  # this gives overall predictions for the model
pred.m2 <- ggpredict(mixed.lmer2, terms = c("AvgYearsOfTotalSchooling"))

# Plot the predictions 
(ggplot(pred.mm) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.8) +  # error band
    geom_point(data = dataset,                      # adding the raw data (scaled values)
               aes(x = EducationalExpenditure, y = GDPcapita, colour = Region)) + 
    labs(x = "EducationalExpenditure", y = "GDPcapita", 
         title = "EducationalExpenditure does not affect GDPcapita") + 
    theme_minimal()
)

(ggplot(pred.m2) + 
    geom_line(aes(x = x, y = predicted)) +          # slope
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.8) +  # error band
    geom_point(data = dataset,                      # adding the raw data (scaled values)
               aes(x = AvgYearsOfTotalSchooling, y = GDPcapita, colour = Region)) + 
    labs(x = "AvgYearsOfTotalSchooling", y = "GDPcapita", 
         title = "AvgYearsOfTotalSchooling does not affect GDPcapita") + 
    theme_minimal()
)



##(aov <- anova(mixed.lmer2))

## Anova-like table of random-effect terms using likelihood ratio tests:
## ranova(mixed.lmer2)

## F-tests of 'single term deletions' for all marginal terms:
## drop1(mixed.lmer2)

## backward elimination of non-significant effects:
## step_result <- step(mixed.lmer2)
## Elimination tables for random- and fixed-effect terms:
## step_result
## Extract the model that step found:
## final_model <- get_model(step_result)




