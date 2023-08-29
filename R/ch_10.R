#' ---
#' title: "Chapter 10: Dynamic regression models"
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

# rmarkdown::render("R/ch_10.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' ## Link
#' https://otexts.com/fpp3/dynamic.html


#'
#' ## Regression with ARIMA errors using `fable`
#'
#' Queremos predecir la evolución del cambio en el consumo en función del
#' cambio en el ingreso.

us_change |>
  pivot_longer(c(Consumption, Income),
    names_to = "var", values_to = "value"
  ) |>
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(
    title = "US consumption and personal income",
    y = "Quarterly % change"
  )

fit <- us_change |>
  model(ARIMA(Consumption ~ Income))
report(fit)

#' Usando la función `residuals` en el modelo ajustado, podemos obtener
#' tanto los residuos de la regresión como los del modelo ARIMA (que
#' deberían ser ruido blanco).
bind_rows(
  `Regression residuals` =
    as_tibble(residuals(fit, type = "regression")),
  `ARIMA residuals` =
    as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) |>
  mutate(
    type = factor(type, levels = c(
      "Regression residuals", "ARIMA residuals"
    ))
  ) |>
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

#' Si no, `gg_tsresiduals` nos da directamente una visualización de los
#' residuos finales (que son los del modelo ARIMA).
fit |> gg_tsresiduals()

#' Pasando `type = "regression"` a `gg_tsresiduals` podemos ver el análisis
#' de los residuos de la regresión. Muestran una clara autocorrelación, por
#' lo que el modelo ARIMA para los errores fue claramente apropiado.
fit |> gg_tsresiduals(type = "regression")

#' (La comparación no es justa, dado que habría que ver los residuos en un modelo
#' lineal puro para determinar que el modelo ARIMA es apropiado).
#' Si lo hacemos, vemos que efectivamente se llega a un modelo con residuos
#' autocorrelacionados:
us_change %>%
  model(TSLM(Consumption ~ Income)) %>%
  gg_tsresiduals()


#' ## Forecasting
#'
#' Ejemplificamos un proceso de forecasting con un modelo de regresión
#' dinámica. Para ello, usamos los datos de demanda eléctrica de Victoria
#' (Australia) y la temperatura máxima diaria como predictor.

vic_elec_daily <- vic_elec |>
  filter(year(Time) == 2014) |>
  index_by(Date = date(Time)) |>
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) |>
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))
vic_elec_daily |>
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(
    y = "Electricity demand (GW)",
    x = "Maximum daily temperature"
  )

#' La demanda de electricidad es mayor en los días en que la temperatura es
#' muy alta o muy baja. Además, la demanda es mayor en los días de semana
#' que en los fines de semana y feriados.
#' Por lo tanto, usamos un modelo de regresión cuadrático en la temperatura,
#' con una variable indicadora para los días de semana.
fit <- vic_elec_daily |>
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
    (Day_Type == "Weekday")))
fit |> gg_tsresiduals()

augment(fit) |>
  features(.innov, ljung_box, dof = 6, lag = 14)

#' El modelo dista de ser perfecto. Hay autocorrelación en los residuos,
#' el histograma muestra que los residuos tienen colas pesadas,
#' y el test de Ljung-Box muestra que los residuos están lejos de ser
#' ruido blanco.
#' 
#' No obstante, el texto dice que los point forecasts deberían ser razonables
#' (no así los intervalos de confianza).
#' 
#' Para poder forecastear, debemos crear un nuevo dataset con las variables
#' predictoras. En este caso usa un escenario en el que fija la temperatura 
#' para los próximos 14 días en 26 grados, sin feriados a partir del 1 de
#' enero:
vic_elec_future <- new_data(vic_elec_daily, 14) |>
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(fit, vic_elec_future) |>
  autoplot(vic_elec_daily) +
  labs(
    title = "Daily electricity demand: Victoria",
    y = "GW"
  )



#' ## Dymamic Harmonic regression
#'
#' Cuando los períodos son demasiado largos, los modelos ARIMA y ETS
#' no pueden manejarlos bien. ETS se banca hasta longitud 24 (períodos
#' de 24 horas), y ARIMA hasta 350, aunque tener problemas de memoria
#' para más de 200.
#' 
#' En esos casos es mejor usar modelos de regresión dinámica con
#' dejando que la estacionalidad sea manejada por términos de Fourier,
#' y los errores por un modelo ARIMA sin estacionalidad.
#' 
#' ### Example: Australian eating out expenditure
#' 
#' Usamos los datos de gasto en comer afuera en Australia.
#' Como los agrupamos a nivel mensual, la estacionalidad es de 12 meses y
#' podríamos usar tanto un modelo ARIMA como ETS. Pero para ejemplificar
#' el uso de Fourier, usamos un modelo de regresión dinámica, viendo ya
#' que estamos cómo andaría para modelos ARIMA y ETS puros.
aus_cafe <- aus_retail |>
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) |>
  summarise(Turnover = sum(Turnover))

fit <- model(aus_cafe,
  `ARIMA` = ARIMA(log(Turnover)),
  `ETS` = ETS(log(Turnover)),
  `K = 1` = ARIMA(log(Turnover) ~ fourier(K=1) + PDQ(0,0,0)),
  `K = 2` = ARIMA(log(Turnover) ~ fourier(K=2) + PDQ(0,0,0)),
  `K = 3` = ARIMA(log(Turnover) ~ fourier(K=3) + PDQ(0,0,0)),
  `K = 4` = ARIMA(log(Turnover) ~ fourier(K=4) + PDQ(0,0,0)),
  `K = 5` = ARIMA(log(Turnover) ~ fourier(K=5) + PDQ(0,0,0)),
  `K = 6` = ARIMA(log(Turnover) ~ fourier(K=6) + PDQ(0,0,0))
)

fit |>
  forecast(h = "2 years") |>
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250,
        label = paste0("AICc = ", format(AICc))),
    data = glance(fit) %>% filter(.model %>% startsWith("K = "))
  ) +
  labs(title= "Total monthly eating-out expenditure",
       y="$ billions")

#' Modelos elejidos encada caso:
fit %>% pivot_longer(1:8)



