## ----init, include = FALSE----------------------------------------------------

# Load packages and add citation information to assets/bib/packages.bib
repmis::LoadandCite(pkgs = c("ggplot2", "dplyr", "tidyr", "readr", "purrr",
                             "tibble", "stringr", "forcats", "lubridate",
                             "ProjectTemplate", "repmis", "biometrics", "knitr",
                             "kableExtra", "broom", "GGally", "jtools"),
            file = "bib/packages.bib")
recoder("bib/packages.bib")

# Loads in template hooks

source(system.file("supporting_files/R/hooks.R", package = "biometrics"))


## ----hello_world, include = TRUE----------------------------------------------
  print("Hello World!")


## ----echo_false, echo = FALSE-------------------------------------------------
  print("In this example, the source code has been hidden and only the output is displayed.")


## ----load_data----------------------------------------------------------------
data(iris)


## ----automatic_table, table = TRUE, results = "hide", eval = FALSE------------
#  head(iris)
#  

## ----manual_table-------------------------------------------------------------
head(iris) %>%
  kable("html", escape = FALSE) %>%
  kable_styling("hover", full_width = FALSE)


## ----model_summary, comment = ""----------------------------------------------
model <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)

summary(model)


## ----tidy, table = T, results = "hide", warning = F---------------------------
tidy(model)


## ----glance, table = T, results = "hide"--------------------------------------
glance(model)


## ----augment, table = T, results = "hide"-------------------------------------
augment(model) %>%
  head()


## -----------------------------------------------------------------------------
models <- lapply(c("setosa", "versicolor", "virginica"),
                 function(x) lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris, subset = Species == x))
names(models) <- c("setosa", "versicolor", "virginica")

jtools::export_summs(models,
                     error_format = "CI [{conf.low}, {conf.high}]",
                     exp = FALSE)


## ---- out.extra = "figure", message = FALSE, warning = FALSE------------------
do.call(jtools::plot_summs,
        c(unname(models),
          list(model.names = names(models),
               exp = FALSE,
               colors = unname(telethonkids_cols()[c(2, 4, 8)])))) + 
  labs(title = "Example of plot_summs", subtitle = "Input is a list of models")


## ----ggpairs, out.extra = "figure", message = FALSE---------------------------
ggpairs(iris,
        mapping = ggplot2::aes(colour = Species),
        diag = list(continuous = "densityDiag"),
        axisLabels = "show")


## ----discrete_plot, out.extra = "figure"--------------------------------------
ggplot(iris, aes(Petal.Width, Petal.Length, color = Species)) +
  labs(title = "Iris Dataset", x = "Petal Width", y = "Petal Length") +
  geom_point(size = 4) +
  scale_color_telethonkids("light") +
  theme_classic()


## ----continuous_plot, out.extra = "figure"------------------------------------
ggplot(iris, aes(Petal.Width, Petal.Length, color = Petal.Length)) +
  geom_point(size = 4, alpha = .6) +
  scale_color_telethonkids(discrete = FALSE, palette = "dark") +
  theme_classic()


## ---- eval = FALSE------------------------------------------------------------
#  library(captioner)
#  

## ---- eval = FALSE------------------------------------------------------------
#  figures <- captioner(prefix = "Figure")
#  tables <- captioner(prefix = "Table")
#  

## ---- eval = FALSE------------------------------------------------------------
#  tables(name = "tab1", caption = "Caption")
#  figures(name = "fig1", caption = "Caption.")
#  

