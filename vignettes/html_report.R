## ----init, include = FALSE-----------------------------------------------
# Load packages and add citation information to packages.bib
# By default, packages listed in global.dcf are ignored and environment initiation is handled by .RProfile.
library(repmis)
LoadandCite(pkgs = c("tidyverse", "lubridate", "ProjectTemplate", "repmis", "biometrics", "knitr", "kableExtra", "broom", "GGally", "lme4"),
            file = "assets/bib/packages.bib")

# loads in template hooks
source("assets/R/hooks.R")


## ----hello_world, include = TRUE-----------------------------------------
  print("Hello World!")


## ----echo_false, echo = FALSE--------------------------------------------
  print("In this example, the source code has been hidden and only the output is displayed.")


## ----load_data-----------------------------------------------------------
data(iris)


## ----automatic_table, table = T, results = "hide", eval = F--------------
#  head(iris)
#  

## ----manual_table--------------------------------------------------------
head(iris) %>%
  kable("html") %>%
  kable_styling("hover", full_width = TRUE)


## ----model_summary, comment = ""-----------------------------------------
model <- lmer(Sepal.Length ~ Sepal.Width + Petal.Width + (1|Species), data = iris)

summary(model)


## ----tidy, table = T, results = "hide", warning = F----------------------
tidy(model)


## ----glance, table = T, results = "hide"---------------------------------
glance(model)


## ----augment, table = T, results = "hide"--------------------------------
augment(model) %>%
  head()


## ----ggpairs, out.extra = 'class="img-fluid center"', message = FALSE----
ggpairs(iris, mapping=ggplot2::aes(colour = Species), diag = list(continuous = "densityDiag"),
        axisLabels = "show")


## ----discrete_plot, out.extra = "figure"---------------------------------
ggplot(iris, aes(Petal.Width, Petal.Length, color = Species)) +
  labs(title = "Iris Dataset", x = "Petal Width", y = "Petal Length") +
  geom_point(size = 4) +
  scale_color_telethonkids("light") +
  theme_classic()


## ----continuous_plot, out.extra = "figure"-------------------------------
ggplot(iris, aes(Petal.Width, Petal.Length, color = Petal.Length)) +
  geom_point(size = 4, alpha = .6) +
  scale_color_telethonkids(discrete = FALSE, palette = "dark") +
  theme_classic()


