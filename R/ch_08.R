#' ---
#' title: "Chapter 08: Exponential smoothing"
#' author: maurogm
#' date: "`r Sys.Date()`"
#' output:
#'   github_document:
#'     toc: true
#' ---


#+ r knitr-global-options, include = FALSE
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE, verbose = TRUE,
  fig.show = "hold", fig.height = 8, fig.width = 10
)
knitr::opts_knit$set(root.dir = normalizePath("../"))

# rmarkdown::render("R/ch_08.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' ## Link
#' https://otexts.com/fpp3/expsmooth.html


#'
#' ## Simple exponential smoothing
#'
#' Es el promedio exponencial simple, donde cada observación tiene un peso alpha
#' y el forecast anterior tiene un peso 1 - alpha.
#' En la formulación que hace, en vez de asignar a la primera observación un peso
#' igual a 1, considera el forecast para t=0 como un hiperparametro a setear, al
#' igual que alpha.
algeria_economy <- global_economy %>%
  filter(Country == "Algeria")
algeria_economy %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")

# Estimate parameters
fit <- algeria_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
fc <- fit %>%
  forecast(h = 5)

fc %>%
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted),
    col = "#D55E00",
    data = augment(fit)
  ) +
  labs(y = "% of GDP", title = "Exports: Algeria") +
  guides(colour = "none")


#' ## Methods with trend
#'
#' ### Holt’s linear trend method
#'
#' Acá en vez de tener un único componente igual al nivel de la serie,
#' se le agrega al forecast una componente de tendencia.
#' Ambas componentes se van actualizando con un promedio exponencial,
#' cada una con su propio decaimiento.
#'
#' Veamos un ejemplo con una serie de crecimiento poblacional:
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)

fit <- aus_economy %>%
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )
fc <- fit %>% forecast(h = 10)

autoplot(aus_economy, Pop) +
  autolayer(fc) +
  labs(y = "Millions", title = "Australian population")

fit %>% report()

#' Vemos que alpha es casi 1, lo que indica que el forecast es prácticaente
#' la observación anterior más la tendencia. El beta también es elevado, lo
#' que indica que la misma se actualiza bastante rápido (lo que tiene sentido
#' dada la aceleración del crecimiento poblacional).

#'
#' ### Damped trend methods
#'
#' Dado que el método de Holt haría que el forecast siga creciendo/decreciendo
#' indefinidamente, se le agrega un factor de amortiguamiento phi para que en
#' el horizonte el forecast se estabilice en una constante.
#' Por cada paso que se adelanta en el horizonte, el factor de amortiguamiento
#' que multiplica a la tendencia se eleva a la potencia del número de pasos.
#'
#' El phi suele estar entre 0.8 (menos hace que la tendencia muera demasiado
#' rápido) y 0.98 (más hace que la tendencia muera muy lentamente).

aus_economy %>%
  model(
    `Holt's method` = ETS(Pop ~ error("A") +
      trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") +
      trend("Ad", phi = 0.9) + season("N"))
  ) %>%
  forecast(h = 15) %>%
  autoplot(aus_economy, level = NULL) +
  labs(
    title = "Australian population",
    y = "Millions"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

#' Acá se seteó phi = 0.9, pero se puede estimar junto al resto de lo parámetros.

#'
#' ### Example: Internet usage
#'
#' Se tiene una serie con la cantidad de usuarios conectados a un server minuto a
#' minuto.
www_usage <- as_tsibble(WWWusage)
www_usage %>% autoplot(value) +
  labs(
    x = "Minute", y = "Number of users",
    title = "Internet usage per minute"
  )

#' Probamos los 3 métodos vistos hasta ahora, y vemos cuál se ajusta mejor
#' usando cross-validation:
www_usage %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") +
      season("N"))
  ) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)

#' Damped Holt es el mejor, así que lo usamos para forecastear 10 minutos más:
fit <- www_usage %>%
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") +
      season("N"))
  )
#' Estimated parameters:
tidy(fit)

#' Vemos que debido a los saltos en la serie histórica, nuestros intervalos de
#' confianza son bastante amplios:
fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(
    x = "Minute", y = "Number of users",
    title = "Internet usage per minute"
  )
