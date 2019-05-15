## ----init, include = FALSE, echo = FALSE---------------------------------
library(biometrics)
library(datasets)
library(knitr)

data(iris)


## ------------------------------------------------------------------------
## Titles | Subtitles


## ---- echo = FALSE, out.extra = 'class="centre" style="width: 500px;"'----
ggplot(iris, aes(Petal.Width, Petal.Length, color = Species)) +
  labs(title = "Iris Dataset", x = "Petal Width", y = "Petal Length") +
  geom_point(size = 4) +
  scale_color_telethonkids("light") +
  theme_classic()


## ---- echo = FALSE, out.extra = 'class="centre" style="width: 300px;"'----
include_graphics("assets/images/logo800.jpg")


## ---- echo = FALSE, out.extra = 'style="float: left; width: 250px; margin: 10px;"'----
include_graphics("assets/images/logo800.jpg")


