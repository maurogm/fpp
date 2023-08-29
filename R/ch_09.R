#' ---
#' title: "Chapter 09: ARIMA models"
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

# rmarkdown::render("R/ch_09.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' ## Link
#' https://otexts.com/fpp3/arima.html


#'
#' ## Stationary and differencing
#'
#' Vamos a ver ciertas herramientas de diagnóstico y tests para determinar
#' cuando una serie de tiempo es estacionaria.
google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015)

p1 <- google_2015 |>
  ACF(Close) |>
  autoplot() + labs(subtitle = "Google closing stock price")
p2 <- google_2015 |>
  ACF(difference(Close)) |>
  autoplot() + labs(subtitle = "Changes in Google closing stock price")
gridExtra::grid.arrange(p1, p2, ncol = 2)

#' Como se ve en la serie, un test de ljung_box muesta que hay una clara autocorrelación:
google_2015 |>
  features(Close, ljung_box, lag = 10)

#' Después de diferenciar la serie, el test ya no muestra autocorrelación:
google_2015 |>
  mutate(diff_close = difference(Close)) |>
  features(diff_close, ljung_box, lag = 10)

#' Exploramos una serie de tiempo de ventas de un fármaco, viendo que debemos
#' realizar distintas transformaciones para llevarla a una serie estacionaria.
#'
#' Primero tomamos logaritmo para estabilizar la varianza,
#' luego diferenciamos contra el año anterior para eliminar la estacionalidad (y algo de la tendencia),
#' pero todavía necesitamos una segunda diferenciación para eliminar completamente la tendencia:
PBS_H02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

PBS_H02 |>
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` =
      difference(difference(log(Cost), 12), 1)
  ) |>
  pivot_longer(-Month, names_to = "Type", values_to = "Sales") |>
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"
    ))
  ) |>
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)
#' La necesidad de esta última diferenciación no es completamente obvia,
#' por lo que es útil tener una herramienta algo más formal que nos ayude a determinar esto.

#' La función `unitroot_ndiffs` nos sugiere realizar una diferenciaciones estacional:
PBS_H02 |>
  mutate(log_sales = log(Cost)) |>
  features(log_sales, unitroot_nsdiffs)

#' Tras realizarla, la función `unitroot_ndiffs` nos sugiere realizar una diferenciación adicional:
PBS_H02 |>
  mutate(log_sales_diff12 = difference(log(Cost), 12)) |>
  features(log_sales_diff12, unitroot_ndiffs)

#' Esta última función no está haciendo otra cosa más que aplicar el test de KPSS
#' sucesivamente hasta obtener un p-valor no significativo:
PBS_H02 |>
  mutate(log_sales_diff12 = difference(log(Cost), 12)) |>
  features(log_sales_diff12, unitroot_kpss)

PBS_H02 |>
  mutate(log_sales_diff12 = difference(log(Cost), 12)) |>
  mutate(log_sales_diff12_diff1 = difference(difference(log(Cost), 12), 1)) |>
  features(log_sales_diff12_diff1, unitroot_kpss)

#' ## Non-seasonal ARIMA models
#'
#' Se usa de ejemplo el caso de las exportaciones de Egipto.
global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian exports")

#' Al usar la función `ARIMA`, se busca automáticamente cuáles son los
#' parámetros de p,d,q que mejor se ajustan a la serie.
fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports))
report(fit)
#' Vemos que el modelo elegido es un `ARIMA(2,0,1) w/ mean`.

#'
#' ### ACF and PACF plots
#'
#' Podríamos haber explorado la serie de tiempo usando los gráficos ACF y PACF,
#' para tratar de determinar a ojo los parámetros del modelo.

p_acf <- global_economy |>
  filter(Code == "EGY") |>
  ACF(Exports) |>
  autoplot()
p_pacf <- global_economy |>
  filter(Code == "EGY") |>
  PACF(Exports) |>
  autoplot()
gridExtra::grid.arrange(p_acf, p_pacf, ncol = 2)

#' (Una forma más cómoda de generar esos gráficos es con `gg_tsdisplay(plot_type = "partial")`):
global_economy |>
  filter(Code == "EGY") %>%
  gg_tsdisplay(Exports, plot_type = "partial")

#' Dado que el ACF tiene forma sinusoidal, y el último pico significativo del PACF
#' está en el lag 4, tiene sentido probar un modelo ARIMA(4, 0, 0).

fit2 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4, 0, 0)))
report(fit2)

#' Vemos que el modelo hecho a ojo tiene un desempeño muy similar al modelo elegido por `ARIMA`:
list(fit, fit2) |>
  map(glance) |>
  bind_rows()

#' ## ARIMA modeling in fable
#'
#' ### Example: Central African Republic exports
#'
#' Vamos a ver un ejemplo completo de cómo sería el proceso completo
#' para desarrollar un modelo ARIMA, con y sin las automatizaciones de
#' la función `fable::ARIMA`.
#' Trabajaremos con las exportaciones de la República Centroafricana.

global_economy |>
  filter(Code == "CAF") |>
  autoplot(Exports) +
  labs(
    title = "Central African Republic exports",
    y = "% of GDP"
  )

#' Parece homocedástica, así que no le aplicamos ninguna transformación de potencias.
#'
#' La serie no es estacionaria, así que la diferenciamos:
global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(difference(Exports), plot_type = "partial")
#' La serie diferenciada sí parece estacionaria, por lo que optamos por d=1.
#'
#' El PACF plot sugiere un modelo AR(2), pero el ACF plot sugiere un modelo MA(3).
#' Vamos a probar ambos modelos, junto con modelos elegidos autmáticamente por ARIMA.
#' También probaremos la diferencia entre usar la búsqueda greedy default,
#' y una búsqueda exhaustiva con `stepwise=FALSE`.
caf_fit <- global_economy |>
  filter(Code == "CAF") |>
  model(
    arima210 = ARIMA(Exports ~ pdq(2, 1, 0)),
    arima013 = ARIMA(Exports ~ pdq(0, 1, 3)),
    stepwise = ARIMA(Exports),
    search = ARIMA(Exports, stepwise = FALSE)
  )

caf_fit |> pivot_longer(!Country,
  names_to = "Model name",
  values_to = "Orders"
)

#' Vemos que los modelos elegidos por `ARIMA` son distintos a los que habíamos
#' elegido a ojo.
#'
#' No obstante, al ver como performan, vemos que todos tienen prácticamente el mismo AICc:
glance(caf_fit) |>
  arrange(AICc) |>
  select(.model:BIC)

#' El modelo encontrado por la búqueda exahustiva es el mejor, lo que tiene sentido.
#' Lo curioso es que los dos modelos elegidos a ojo son mejores que el encontrado
#' por la búsqueda greedy.
#'
#' Todavía no terminamos, dado que falta estudiar los residuos del modelo para ver
#' si son razonables.
caf_fit |>
  select(search) |>
  gg_tsresiduals()

#' Los residuos parecen ser ruido blanco, sin autocorrelación y con una distribución
#' razonablemente gaussiana.
#' Por las dudas hacemos un test portmanteau de Ljung-Box con p+q grados de libertad:
augment(caf_fit) |>
  filter(.model == "search") |>
  features(.innov, ljung_box, lag = 10, dof = 3)

#' El p-valor alto apoya la observación que los residuos son ruido blanco,
#' por lo que podemos proceder a forecastear:
forecasts_arima <- caf_fit |>
  forecast(h = 5) |>
  filter(.model == "search")
forecasts_arima |>
  autoplot(global_economy)

#' Es interesante que el forecast puntual es básicamente el de un modelo Naive.
#' La ventaja de usar un modelo ARIMA es que nos da intervalos de confianza
#' bastante más estrechos:
global_economy |>
  filter(Code == "CAF") |>
  model(naive = NAIVE(Exports)) |>
  forecast(h = 5) |>
  bind_rows(forecasts_arima) |>
  autoplot(global_economy, alpha = 0.6)


#' ## Seasonal ARIMA models

#' El capítulo de fpp tiene [una sección](https://otexts.com/fpp3/seasonal-arima.html)
#' en la que se muestran dos ejemplos del procedimiento para ajustar un modelo ARIMA estacional.
#' 
#' No agrega nada nuevo como para mostrarlo acá, más allá del hecho de que
#' en su segundo ejemplo el modelo al que se llega no pasa los tests 
#' de residuos sin autocorrelación (a menos que se amplíe el espacio de
#' búsqueda usando ).


#' 
#' ### Example: Monthly US leisure and hospitality employment
#' 
#' Veamos el workflow completo para ajustar un modelo ARIMA estacional.
leisure <- us_employment |>
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) |>
  mutate(Employed = Employed/1000) |>
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

#' Veamos los gráficos ACF y PACF con una diferenciación estacional:
leisure |>
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

#' Veamos agregando además una diferenciación no estacional:
leisure |>
  gg_tsdisplay(difference(Employed, 12) |> difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

#' A partir de los gráficos esbozamos un par de modelos a ojo, y los comparamos
#' con el modelo elegido por `ARIMA`:
fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit |> pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")

glance(fit) |> arrange(AICc) |> select(.model:BIC)

#' Dan bastante parecidos. Hagamos un análisis de residuos para ver si pasan
#' los tests de ruido blanco:
fit |> select(auto) |> gg_tsresiduals(lag=36)

augment(fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag=24, dof=4)

#' Los residuos parecen ser ruido blanco, por lo que podemos proceder a forecastear:
forecast(fit, h=36) |>
  filter(.model=='auto') |>
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")