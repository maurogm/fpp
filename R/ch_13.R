#' ---
#' title: "Chapter 13: Some practical forecasting issues"
#' author: maurogm
#' date: "`r Sys.Date()`"
#' output:
#'   github_document:
#'     toc: true
#' ---


#+ r knitr-global-options, include = FALSE
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE, verbose = TRUE,
  fig.show = "hold", fig.height = 6, fig.width = 10
)
knitr::opts_knit$set(root.dir = normalizePath("../"))

# rmarkdown::render("R/ch_13.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' ## Link
#' https://otexts.com/fpp3/practical.html


#'
#' ## Weekly, daily and sub-daily data
#'
#' ### Weekly data
#'
#' Hay que usar modelos que puedan lidiar con una periodicidad no entera
#' (52+1/7 semanas en el año), no todos los años tienen la misma cantidad de semanas:
us_gasoline %>%
  group_by(year = year(Week)) %>%
  filter(Week == max(Week))

#' Por ejemplo, un modelo de descomposición:
my_dcmp_spec <- decomposition_model(
  STL(Barrels),
  ETS(season_adjust ~ season("N"))
)
us_gasoline |>
  model(stl_ets = my_dcmp_spec) |>
  forecast(h = "2 years") |>
  autoplot(us_gasoline) +
  labs(
    y = "Millions of barrels per day",
    title = "Weekly US gasoline production"
  )

#' O una regresión armónica:
gas_dhr <- us_gasoline |>
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))

gas_dhr |>
  forecast(h = "2 years") |>
  autoplot(us_gasoline) +
  labs(
    y = "Millions of barrels per day",
    title = "Weekly US gasoline production"
  )


#' ## Método CROSTON para Count data de baja frecuencia
#'
#' Es apropiado cuando tenemos series con muchos ceros, por ejemplo:
j06 <- PBS |>
  filter(ATC2 == "J06") |>
  summarise(Scripts = sum(Scripts))

j06 |> autoplot(Scripts) +
  labs(
    y = "Number of scripts",
    title = "Sales for immune sera and immunoglobulins"
  )

#' El modelo `CROSTON` sólo da forecasts puntuales, y la predicción es constante
#' en el tiempo:
j06 |>
  model(CROSTON(Scripts)) |>
  forecast(h = 6)


#' ## Ensuring forecasts stay within limits
#'
#' ### Positive forecasts
#'
#' Lograr esto es fácil simplemente trabajando en la escala logarítmica:
egg_prices <- prices |> filter(!is.na(eggs))
log_model <- egg_prices |>
  model(ETS(log(eggs) ~ trend("A")))
log_model |>
  forecast(h = 50) |>
  autoplot(egg_prices) +
  labs(
    title = "Annual egg prices",
    y = "$US (in cents adjusted for inflation) "
  )

#' Chequeemos que los residuos parezcan ruido blanco:
log_model %>%
  gg_tsresiduals()

#' ### Forecasts constrained to an interval
#'
#' Si queremos que los forecasts estén entre a y b, usar una transformación
#' logit sobre los datos antes de ajustar el modelo:
#'
#' Volvamos al ejemplo anterior de los precios de los huevos, aunque sea poco
#' realista.
#' Notemos que esta no es una transformación estándar, pero `fabletools` nos
#' permite crearla utilizando la función `new_transformation`, tras lo que
#' podemos incluirla dentro de la fórmula del modelo:
scaled_logit <- function(x, lower = 0, upper = 1) {
  log((x - lower) / (upper - x))
}
inv_scaled_logit <- function(x, lower = 0, upper = 1) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}
my_scaled_logit <- new_transformation(
  scaled_logit, inv_scaled_logit
)
egg_prices |>
  model(
    ETS(my_scaled_logit(eggs, lower = 50, upper = 400)
    ~ trend("A"))
  ) |>
  forecast(h = 50) |>
  autoplot(egg_prices) +
  labs(
    title = "Annual egg prices",
    y = "$US (in cents adjusted for inflation) "
  )


#' ## Forecast combinations
#'
#' ### Forecasts puntuales
#'
#' Entrenando varios modelos y promediando las columnas del `mable` resultante
#' con `mutate` podemos obtener un forecast combinado:
auscafe <- aus_retail |>
  filter(stringr::str_detect(Industry, "Takeaway")) |>
  summarise(Turnover = sum(Turnover))
train <- auscafe |>
  filter(year(Month) <= 2013)
STLF <- decomposition_model(
  STL(log(Turnover) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)
cafe_models <- train |>
  model(
    ets = ETS(Turnover),
    stlf = STLF,
    arima = ARIMA(log(Turnover))
  ) |>
  mutate(combination = (ets + stlf + arima) / 3)
cafe_fc <- cafe_models |>
  forecast(h = "5 years")

cafe_fc |>
  autoplot(auscafe |> filter(year(Month) > 2008),
    level = NULL
  ) +
  labs(
    y = "$ billion",
    title = "Australian monthly expenditure on eating out"
  )

#' El modelo combinado es mejor (en métricas puntuales) que los individuales:
cafe_fc |>
  accuracy(auscafe) |>
  arrange(RMSE)


#'
#' ### Forecast combination distributions
#'
#' Si queremos ver el efecto en los forecasts predictivos, vemos que la combinación
#' no calculó una distribución:
cafe_fc |>
  autoplot(auscafe |> filter(year(Month) > 2008)) +
  facet_wrap(~.model, ncol = 2)
labs(
  y = "$ billion",
  title = "Australian monthly expenditure on eating out, with distribution of forecasts"
)

#' Esto es porque las distribuciones individuales provienen de familias distintas,
#' y por ahora no está implementada su combinación:
cafe_fc |> filter(Month == min(Month))

#' Para obtener una distribución combinada, podemos simular muestras de los
#' modelos individuales y combinarlas.
#' Nótese el uso de la función `distributional::dist_sample` para crear una
#' columna de tipo `dist` que pueda ser reconocida por el `fable`:
cafe_futures <- cafe_models |>
  # Generate 1000 future sample paths
  generate(h = "5 years", times = 1000) |>
  # Compute forecast distributions from future sample paths
  as_tibble() |>
  group_by(Month, .model) |>
  summarise(
    dist = distributional::dist_sample(list(.sim))
  ) |>
  ungroup() |>
  # Create fable object
  as_fable(
    index = Month, key = .model,
    distribution = dist, response = "Turnover"
  )

cafe_futures |>
  autoplot(auscafe |> filter(year(Month) > 2008)) +
  facet_wrap(~.model, ncol = 2) +
  labs(
    y = "$ billion",
    title = "Australian monthly expenditure on eating out"
  )

#' Ahora sí contamos con una distribución combinada, que es de hecho más angosta
#' que todas las demás.
#' 
#' Y al medir la performance, vemos que también en las métricas distribucionales
#' la combinación tiene mejor score:
cafe_futures |>
  accuracy(auscafe, list(crps = CRPS, winkler = winkler_score), level = 95) |>
  arrange(crps)


#' ## Prediction intervals for aggregates
#' 
#' Tal vez hacemos forecasts a nivel diario o mensual, pero nos interesa tener
#' el forecast para todo el año.
#' Lo que hacemos entonces es simular un montón de paths distintos, y luego
#' sumarlos para obtener el forecast agregado.
#' 
#' Aquí es nuevamente práctico el uso de `distributional::dist_sample` para
#' volver a crear un `dist` al que aplicarle métodos varios, como el cálculo
#' de intervalos de confianza con `hilo`:
fit <- auscafe |>
  # Fit a model to the data
  model(ETS(Turnover))
futures <- fit |>
  # Simulate 10000 future sample paths, each of length 12
  generate(times = 10000, h = 12) |>
  # Sum the results for each sample path
  as_tibble() |>
  group_by(.rep) |>
  summarise(.sim = sum(.sim)) |>
  # Store as a distribution
  summarise(total = distributional::dist_sample(list(.sim)))
futures |>
  mutate(
    mean = mean(total),
    pi80 = hilo(total, 80),
    pi95 = hilo(total, 95)
  )

#' ## Backcasting
#' 
#' Si queremos "forecastear hacia atrás", debemos invertir el índice temporal
#' y usar alguno de los métodos habituales.
#' Lo molesto es hacer los malabares para después volver a invertir el índice
#' y que todo quede como queremos:
backcasts <- auscafe |>
  mutate(reverse_time = rev(row_number())) |>
  update_tsibble(index = reverse_time) |>
  model(ets = ETS(Turnover ~ season(period = 12))) |>
  forecast(h = 15) |>
  mutate(Month = auscafe$Month[1] - (1:15)) |>
  as_fable(index = Month, response = "Turnover",
    distribution = "Turnover")
backcasts |>
  autoplot(auscafe |> filter(year(Month) < 1990)) +
  labs(title = "Backcasts of Australian food expenditure",
       y = "$ (billions)")


#' 
#' ## Forecasting on training and test sets
#' 

training <- auscafe |> filter(year(Month) <= 2013)
test <- auscafe |> filter(year(Month) > 2013)
cafe_fit <- training |>
  model(ARIMA(log(Turnover)))

#' ### Multi-step forecasts on training data
#' 
#' Por defecto al extraer valores fitteados de un modelo se toman los fitted
#' values a un paso:
cafe_fit |>
  forecast(h = 60) |>
  autoplot(auscafe) +
  labs(title = "Australian food expenditure",
       y = "$ (billions)")

#' Pero la función `fitted` permite especificar el parámetro `h` para obtener
#' valores fitteados a más de un paso. Por ejemplo, con `h=12`:
fits12 <- fitted(cafe_fit, h = 12)
training |>
  autoplot(Turnover) +
  autolayer(fits12, .fitted, col = "#D55E00") +
  labs(title = "Australian food expenditure",
       y = "$ (billions)")


#' ### One-step forecasts on test data
#' 
#' A veces dejamos un test set fuera, que queremos usar para evaluar el modelo.
#' El problema es que a medida que nos alejamos del último dato conocido, la
#' incertidumbre crece, dado que cada forecast se basa en el anterior. Esto es
#' problemático en el caso de que queramos sumarizar los errores de los forecasts,
#' dado que los mismos tendríąn distintas varianzas.
#' 
#' Lo que podemos hacer en este caso es usar los datos reales para generar
#' cada nuevo forecast, de modo que todos los forecasts sean one-step.
#' Nótese que no estamos re-entrenando el modelo: los parámetros son los calculados
#' con los datos de entrenamiento. Lo que hacemos es usar los datos reales para
#' generar los forecasts, de modo que la varianza se mantenga constante en todo
#' el test set, y podamos sumarizarla con mayor facilidad.
#' 
#' La función `refit` permite hacer exactamente esto, tras lo que podemos usar
#' `accuracy` para calcular las métricas
#' (ignorar el hecho de que .type diga 'Trainig'):
cafe_fit |>
  refit(test) |>
  accuracy()


#'
#'  ## Dealing with outliers and missing values
#' 
#' ### Outliers
#' 
#' Supongamos que tengamos una serie con un outlier, por ejemplo:
tourism |>
  filter(
    Region == "Adelaide Hills", Purpose == "Visiting"
  ) |>
  autoplot(Trips) +
  labs(title = "Quarterly overnight trips to Adelaide Hills",
       y = "Number of trips")


#' Podemos usar `STL` con `robust = TRUE` para que el outlier no afecte la
#' estimación de la tendencia:
ah_decomp <- tourism |>
  filter(
    Region == "Adelaide Hills", Purpose == "Visiting"
  ) |>
  # Fit a non-seasonal STL decomposition
  model(
    stl = STL(Trips ~ season(period = 1), robust = TRUE)
  ) |>
  components()
ah_decomp |> autoplot()

#' Una vez eliminado el efecto de la tendencia (y la estacionalidad, si la hubiera),
#' podemos tratar de identificar los outliers en los residuos con los criterios 
#' tradicionales (ya sea como 1.5 veces el IQR o 3 veces el IQR):
outliers <- ah_decomp |>
  filter(
    remainder < quantile(remainder, 0.25) - 3*IQR(remainder) |
    remainder > quantile(remainder, 0.75) + 3*IQR(remainder)
  )
outliers

#' En este caso, el punto en cuestión es efectivamente identificado como outlier.
#' Si decidimos eliminarlo, entonces surge el problema de qué hacer con el dato
#' faltante.

#' 
#' ### Missing values
#' 
#' Algunos modelos de `fable` pueden lidiar con datos faltantes, como NAIVE,
#' ARIMA, dynamic regression models y NNAR.
#' 
#' Pero otros como STL y ETS no.
#' Si quisiéramos usar uno de estos, podemos entrenar un modelo que admita datos
#' faltantes y usarlo para estimar los puntos que nos falten. Por ejemplo, podemos
#' usar un modelo ARIMA para estimar los valores faltantes en la serie de
#' Adelaide Hills:
ah_miss <- tourism |>
  filter(
    Region == "Adelaide Hills",
    Purpose == "Visiting"
  ) |>
  # Remove outlying observations
  anti_join(outliers) |>
  # Replace with missing values
  fill_gaps()
ah_fill <- ah_miss |>
  # Fit ARIMA model to the data containing missing values
  model(ARIMA(Trips)) |>
  # Estimate Trips for all periods
  interpolate(ah_miss)

#' La función `interpolate` toma un `mable` y devuelve un `tsibble` con los datos
#' originales y los valores estimados para los períodos faltantes.
#' 
#' Efectivamente, comprobamos que `ah_fill` ahora contiene datos para el período
#' del outlier:
ah_fill |>
  # Only show outlying periods
  right_join(outliers |> select(-Trips))

ah_fill |>
  autoplot(Trips) +
  autolayer(ah_fill |> filter_index("2002 Q3"~"2003 Q1"),
    Trips, colour="#D55E00") +
  labs(title = "Quarterly overnight trips to Adelaide Hills",
       y = "Number of trips")

#' Ahora ya podemos usar algún modelo que no admita datos faltantes, como STL:
ah_fill |>
  model(stl = STL(Trips)) |>
  components() |>
  autoplot()
