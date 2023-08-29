#' ---
#' title: "Chapter 11: Forecasting hierarchical and grouped time series"
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

# rmarkdown::render("R/ch_11.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' ## Link
#' https://otexts.com/fpp3/dynamic.html


#'
#' ## Hierarchical and grouped time series
#'
#' ### Example: Australian tourism hierarchy (jerarquía anidada)
#'
#' Para mostrar el uso de series temporales jerárquicas y agrupadas,
#' empezamos con datos de turismo australiano agrupados geográficamente
#' por región y estado:
tourism <- tsibble::tourism |>
  mutate(State = recode(State,
    `New South Wales` = "NSW",
    `Northern Territory` = "NT",
    `Queensland` = "QLD",
    `South Australia` = "SA",
    `Tasmania` = "TAS",
    `Victoria` = "VIC",
    `Western Australia` = "WA"
  ))
tourism %>% head()

#' La función `aggregate_key` nos permite crear una jerarquía. La
#' sintaxis `parent/child` se usa para especificar una jerarquía anidada,
#' en este caso creando series nuevas que agrupan los datos a nivel
#'  estatal y nacional:
tourism_hts <- tourism |>
  aggregate_key(State / Region, Trips = sum(Trips))
tourism_hts

tourism_hts %>%
  tsibble::key_data() %>%
  print(n = 100)


#' ### Example: Australian prison population (jerarquía cruzada)
#'
#' Cuando las dimensiones a agrupar no son jerárquicas, se puede usar la
#' sintaxis `Group1 * Group2` para especificar una estructura de grupos
#' cruzados.
#' Por ejemplo, los datos de población carcelaria australiana se pueden
#' agrupar por género, status legal y estado:
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(
    key = c(Gender, Legal, State, Indigenous),
    index = Quarter
  ) |>
  relocate(Quarter)

prison_gts <- prison |>
  aggregate_key(Gender * Legal * State, Count = sum(Count) / 1e3)

#' La función `is_aggregated` es muy conveniente para filtrar datos
#' agregados durante un análisis exploratorio:
prison_gts |>
  filter(
    !is_aggregated(Gender), is_aggregated(Legal),
    is_aggregated(State)
  ) |>
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)")

prison_gts |>
  filter(
    !is_aggregated(Gender), !is_aggregated(Legal),
    !is_aggregated(State)
  ) |>
  mutate(Gender = as.character(Gender)) |>
  ggplot(aes(
    x = Quarter, y = Count,
    group = Gender, colour = Gender
  )) +
  stat_summary(fun = sum, geom = "line") +
  labs(
    title = "Prison population by state and gender",
    y = "Number of prisoners ('000)"
  ) +
  facet_wrap(~ as.character(State),
    nrow = 1, scales = "free_y"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' ### Mixed hierarchical and grouped structure
#'
#' La sintaxis de jararquías anidadas `Parent/Child` y de grupos cruzados
#' `Gropo1 * Grupo2` se pueden combinar para crear estructuras mixtas:
tourism_full <- tourism |>
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))
tourism_full %>% filter(!is_aggregated(Purpose), !is_aggregated(State))

#' ## Single level approaches
#'
#' Vamos a usar los datos de turismo australiano para ilustrar estraegias
#' de conciliación de forecasts de series temporales jerárquicas.
tourism_states <- tourism |>
  aggregate_key(State, Trips = sum(Trips))

#' ### Bottom-up forecasts
#'
#' Si quisiéramos hacer una estrategia bottom-up a mano, deberíamos hacer
#' un forecast para cada serie no-agrupada, y luego sumar los forecasts
#' para cada grupo.
#' Empezamos por hacer los forecasts para cada serie independiente:
fcasts_state <- tourism_states |>
  filter(!is_aggregated(State)) |>
  model(ets = ETS(Trips)) |>
  forecast()
fcasts_state

#' Y construimos los forecasts generales agrupando los individuales:
fcasts_national <- fcasts_state |>
  summarise(value = sum(Trips), .mean = mean(value))
fcasts_national

#' Por suerte no hace falta hacerlo a mano.
#' La función `reconcile` ofrece una API general para conciliar forecasts,
#' donde sólo debemos especificar la estrategia de reconciliación:
reconciled_models <- tourism_states |>
  model(ets = ETS(Trips)) |>
  reconcile(bu = bottom_up(ets))

#' El paso de `recocile` va entre el modelado y el forecasting, y devuelve
#' tanto los modelos originales como los conciliados. El modelo conciliado
#' es una columna de modelos más, y a partir de ahí se puede forecastear
#' normalmente:
reconciled_models |>
  forecast()

#' ### Top-down forecasts
#'
#' Los forecasts top-down se pueden generar usando `top_down()` dentro
#' de la función `reconcile()`:
individual_models <- tourism_states |>
  model(ets = ETS(Trips))
individual_models %>%
  reconcile(td = top_down(ets)) %>%
  forecast()

#' Los distintos criterios para hacer conciliaciones top-down se pueden
#' especificar con el argumento `method` de `top_down()`. El mismo puede
#' aceptar los valores `average_proportions`, `proportion_averages` y
#' `forecast_proportions` (el default):
top_down_reconciliations <- individual_models %>%
  reconcile(
    td_average_proportions = top_down(ets, method = "average_proportions"),
    td_proportion_averages = top_down(ets, method = "proportion_averages"),
    td_forecast_proportions = top_down(ets, method = "forecast_proportions")
  )
top_down_reconciliations %>%
  forecast() %>%
  autoplot(tourism_states)

#' Es interesante como `forecast_proportions` es el único que conserva algo
#' de la forma de las series originales. Particularmente notable en el caso
#' de `NT`, donde hasta los intervalos de confianza son claramente cualquier
#' cosa.

#'
#' ### Middle-out forecasts
#'
#' las conciliaciones middle-out se pueden generar usando `middle_out()`,
#' especificando el nivel a usar como base con el argumento `split`.
#' El default, `split = 1` usa el nivel inmediatamente debajo del Total
#' como base:
tourism_states_region <- tourism |>
  aggregate_key(State / Region, Trips = sum(Trips))

initial_models <- tourism_states_region %>%
  model(ets = ETS(Trips))

reconciled_models <- initial_models %>%
  reconcile(
    mo_from_states = middle_out(ets)
  )
reconciled_models %>%
  forecast() %>%
  filter(!is_aggregated(Region) | is_aggregated(State)) %>%
  autoplot(tourism_states_region, alpha = 0.6) +
  facet_grid(State ~ ., scales = "free_y")

#' ## Ejemplos del uso de Min-trace
#'
#' ### Example: Forecasting Australian domestic tourism
#'
#' Veamos como funciona la conciliación en un caso en que la jerarquía tiene
#' tanto Padre/Hijo como grupos cruzados.
tourism_full <- tourism |>
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))

fit <- tourism_full |>
  filter(year(Quarter) <= 2015) |>
  model(base = ETS(Trips)) |>
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink")
  )

fc <- fit |> forecast(h = "2 years")

#' Se ve que en general los métodos que usan Min-trace son otorgan mejores resultados.
fc |>
  filter(is_aggregated(Region), is_aggregated(Purpose)) |>
  autoplot(
    tourism_full |> filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(State), scales = "free_y")

fc |>
  filter(is_aggregated(State), !is_aggregated(Purpose)) |>
  autoplot(
    tourism_full |> filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(Purpose), scales = "free_y")

#' Al medir la performance hay que tener cuidado, dado que muchas métricas, como el RMSE,
#' dependen de la escala de los datos, y por lo tanto no son comparables entre distintos
#' niveles de agregación. En esos casos se puede filtrar antes de calcular las métricas,
#' y repetir para cada nivel que se quiera estudiar:
fc |>
  filter(is_aggregated(State), is_aggregated(Purpose)) |>
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) |>
  group_by(.model) |>
  summarise(rmse = mean(rmse), mase = mean(mase))



#'
#' ## Reconciled distributional forecasts
#'
#'
#'
#' ### Example: Forecasting Australian prison population
#'
#' Veamos en un nuevo ejemplo como se pueden hacer forecasts distribucionales
#' conciliados.
#' Volvemos al ejemplo de la población carcelaria australiana.

fit <- prison_gts |>
  filter(year(Quarter) <= 2014) |>
  model(base = ETS(Count)) |>
  reconcile(
    bottom_up = bottom_up(base),
    MinT = min_trace(base, method = "mint_shrink")
  )
fc <- fit |> forecast(h = 8)
fc

#' Dado que no agregamos `bootstrap = TRUE` en el paso de `forecast()`, los
#' errores base se asumen normales, y por lo tanto los forecasts conciliados
#' también.
#' 
#' Vemos que para la serie total, los intervalos de confianza del forecast
#' con min_trace es más estrecho y más cercano al valor real que el base y el bottom-up:
fc |>
  filter(
    is_aggregated(State), is_aggregated(Gender),
    is_aggregated(Legal)
  ) |>
  autoplot(prison_gts, alpha = 0.7, level = 90) +
  labs(
    y = "Number of prisoners ('000)",
    title = "Australian prison population (total)"
  )

#' Los forecasts a nivel Estado también suelen ser mejores con min_trace, aunque hay
#' varios casos en los que tendencias novedosas en el set de test quedan por fuera de
#' lo forecasteado:
fc |>
  filter(
    .model %in% c("base", "MinT"),
    !is_aggregated(State), is_aggregated(Legal),
    is_aggregated(Gender)
  ) |>
  autoplot(
    prison_gts |> filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(
    title = "Prison population (by state)",
    y = "Number of prisoners ('000)"
  ) +
  facet_wrap(vars(State), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' Para comparar la precisión de los distintos métodos, está bueno usar métricas
#' que no dependan de la escala de los datos, como el MASE para los puntuales 
#' y el Skill Score (CRPS) para las distribuciones:
fc |>
  filter(
    is_aggregated(State),
    is_aggregated(Gender),
    is_aggregated(Legal)) |>
  accuracy(data = prison_gts,
           measures = list(mase = MASE,
                           ss = skill_score(CRPS)
                           )
           ) |>
  group_by(.model) |>
  summarise(mase = mean(mase), sspc = mean(ss) * 100)

#' Vemos que en la serie total las dos métricas favorecen a min_trace.
#' 
#' Al ver lo que sucede en todas las series de base, vemos que el skill score
#' castiga un poco al min_trace. Probablemente suceda que al estrechar los intervalos
#' de confianza, los casos en los que el valor real queda por fuera de los mismos
#' penalizan mucho al CRPS.
#' A pesar de ello, el MASE indica una mejora en la precisión de los forecasts
#' puntuales:
fc |>
  filter(
    !is_aggregated(State),
    !is_aggregated(Gender),
    !is_aggregated(Legal)) |>
  accuracy(data = prison_gts,
           measures = list(mase = MASE,
                           ss = skill_score(CRPS)
                           )
           ) |>
  group_by(.model) |>
  summarise(mase = mean(mase), sspc = mean(ss) * 100)
