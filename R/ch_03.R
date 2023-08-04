#' ---
#' title: "Chapter 03: Time series decomposition"
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

# rmarkdown::render("R/ch_03.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' ## Link
#' https://otexts.com/fpp3/decomposition.html


#'
#' ## Transformation and adjustments
#'
#' Primero habla de cosas medio básicas como normalizar tomando métricas per cápita
#' o ajustar por inflación.
#'
#' Después explica la Box-Cox transformation, que sirve para reducir la variabilidad
#' de la serie, e idealmente hacer que los efectos estacionales sean parecidos a lo
#' largo de toda la serie.
#'
#' Por ejemplo, acá se tiene la producción de gas histórica de Australia:
aus_production %>%
  autoplot(Gas)

#' Y acá está después de aplicarle la transformación de Box-Cox para un lambda
#' que elije automáticamente usando la función `features`:
lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)
aus_production %>%
  autoplot(box_cox(Gas, lambda)) +
  labs(
    y = "",
    title = latex2exp::TeX(paste0(
      "Transformed gas production with $\\lambda$ = ",
      round(lambda, 2)
    ))
  )


#'
#' ## Time series components
#'
#' Más adelante se verá con mayor detalle, pero acá muestra una forma de
#' descomponer una serie en componentes (estacional + trend-cycle + reminder):
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)
dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp) %>% head()

components(dcmp) %>% autoplot()

#' Está bueno que `components` ya incluye una columna con la serie
#' desestacionalizada, lista para usar:
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

#' ## Clasical decomposition
#'
#' Cuenta un algoritmo rudimentario (data de los 1920s) para descomponer una serie
#' (ya sea en suma o en producto de componentes).
#'
#' La idea es que primero se estima el componente de trend-cycle con promedios
#' móviles, se quita ese componente de la serie original, y del resto se estima
#' el componente estacional como el promedio de cada elemento del período
#' (o sea, el promedio de todos los datos de "viernes" por un lado, de "jueves"
#' por otro, etc.). Lo que sobre es el Reminder.
#'
#' Este método quedó viejo, y da unas cuantas razones al final.
#'
#' - No se adapta a cambios abruptos de tendencia.
#'
#' - No se adapta a cambios en el componente estacional (ej: antes el máximo
#' consumo eléctrico era en invierno para calefaccionar, tras la popularización
#' del aire acondicionado pasó a ser en verano).
#'
#' - No permite aislar datos atípicos.

#'
#' ## Métodos usados por agencias oficiales
#'
#' ### X-11
#'
#' Esta descomposición se parece a la anterior, enmienda varias de sus falencias.
#' Por ejemplo, infiere la tendencia también para los extremos de la serie,
#' y permite lentos cambios en el componente estacional.
#'
#' El siguiente código usa la implementación `X_13ARIMA_SEATS`:

x11_dcmp <- us_retail_employment %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(
    title =
      "Decomposition of total US retail employment using X-11."
  )

#' ### SEATS
#'
#' Significa _Seasonal Extraction in ARIMA Time Series_.
#' No entra en detalles técnicos, aunque da un link a bibliografía adecuada.
#'
#' Para este caso, el resultado es similar al de X-11:

seats_dcmp <- us_retail_employment %>%
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components()
autoplot(seats_dcmp) +
  labs(
    title =
      "Decomposition of total US retail employment using SEATS"
  )

#' ## STL decomposition
#'
#' Tiene ventajas sobre los métodos anteriores:
#'
#' - Unlike SEATS and X-11, STL will handle any type of seasonality, not only monthly and quarterly data.
#' - The seasonal component is allowed to change over time, and the rate of change can be controlled by the user.
#' - The smoothness of the trend-cycle can also be controlled by the user.
#' - It can be robust to outliers (i.e., the user can specify a robust decomposition), so that occasional unusual observations will not affect the estimates of the trend-cycle and seasonal components. They will, however, affect the remainder component.
#'
#' Como desventajas, menciona que _it does not handle trading day or calendar variation automatically_.
#' También que sólo acepta descomposiciones aditivas, pero se puede lograr
#' descomposiciones aditivas tomando el logaritmo de la serie.
#' De quererse algo intermedio, se puede usar una transformación de Box-Cox.
#'
#' Ejemplo:
us_retail_employment %>%
  model(
    STL(Employed ~ trend(window = 7) +
      season(window = "periodic"),
    robust = TRUE
    )
  ) %>%
  components() %>%
  autoplot()
#' Para entender mejor cómo usarlo, hay que meterse a jugar con el código y la
#' documentación.
