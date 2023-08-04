#' ---
#' title: "Chapter 02: Time series graphics"
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

# rmarkdown::render("R/ch_02.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' ## Link
#' https://otexts.com/fpp3/graphics.html


#'
#' ## `tsibble` objects
#'
#' Es un formato que extiende a tibble para trabajar con series de tiempo.
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)
y

#' Imprime cierta información, como el tiempo que transcurre entre cada observación.
olympic_running

#' La función `distinct` permite obtener los niveles para los que existen distintas series.
olympic_running %>% distinct(Length)
olympic_running %>% distinct(Sex, Length)


#' Ejemplo de conversión de un CSV a tsibble:
prison_csv <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

prison <- prison_csv %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(
    key = c(State, Gender, Legal, Indigenous),
    index = Quarter
  )

prison


#' ## Time plots

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers / 1000)

#' La función `autoplot` toma el input y genera el gráfico que le parece
#' que tiene más sentido:
autoplot(melsyd_economy, Passengers) +
  labs(
    title = "Ansett airlines economy class",
    subtitle = "Melbourne-Sydney",
    y = "Passengers ('000)"
  )


#' ## Time series patterns

#' **Trend**:
#'     A trend exists when there is a long-term increase or decrease in the data. It does not have to be linear. Sometimes we will refer to a trend as “changing direction”, when it might go from an increasing trend to a decreasing trend.
#'
#' **Seasonal**:
#'     A seasonal pattern occurs when a time series is affected by seasonal factors such as the time of the year or the day of the week. Seasonality is always of a fixed and known period.
#'
#' **Cyclic**:
#'     A cycle occurs when the data exhibit rises and falls that are not of a fixed frequency. These fluctuations are usually due to economic conditions, and are often related to the “business cycle”. The duration of these fluctuations is usually at least 2 years.


#'
#'  ## Seasonal plots

a10 <- PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6)

#' Imaginemos que tengo el siguiente autoplot:
autoplot(a10)

#' Puedo usar `gg_season` para examinar mejor el componente estacional de la serie:
a10 %>%
  gg_season(Cost, labels = "both") +
  labs(
    y = "$ (millions)",
    title = "Seasonal plot: Antidiabetic drug sales"
  )

#' ### Multiples períodos
#'
#' Supongamos que tenemos datos de consumo eléctrico cada 30 minutos:
vic_elec %>% head()

#' Usando el parámetro `period` podemos especificar el periodo de
#' tiempo que queremos examinar en busca de efectos estacionales:

vic_elec %>% gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

vic_elec %>% gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

vic_elec %>% gg_season(Demand, period = "year") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

#' ### Seasonal subseries plots
#'
#' Esta opción nos permite tener un gráfico separado para cada periodo de tiempo.
#' Permite ver mejor los efectos que tiene cada estación, así como detectar si
#' estos efectos hay ido variando con el tiempo.

a10 %>%
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )


#' ## Scatterplots

visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
visitors %>%
  pivot_wider(values_from = Trips, names_from = State) %>%
  GGally::ggpairs(columns = 2:9)


#' ## Lag plots
#'
#' `gg_lag` arma por sí sola un gráfico de la variable de interés contra
#' distintos lags de la misma.

recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)
recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")


#' ## Autocorrelation
#'
#' Así como ploteamos los lags de una variable, podemos calcular la correlación
#' entre la variable y sus lags.
#' Esto se hace con la función `ACF`:

recent_production %>% ACF(Beer, lag_max = 9)

#' Y si lo ploteamos:
recent_production %>%
  ACF(Beer) %>%
  autoplot() + labs(title = "Australian beer production")

#' Dado el efecto estacional de la producción de cerveza, se ve la fuerte
#' autocorrelación en los lags de 4 y 8.
#' También se ve que la autocorrelación es más fuerte en los primeros lags,
#' dado que la serie se parece más a lo que pasó el año pasado que a hace 4 años
#' (debido a la presencia de una leve tendencia).

#' ## White noise
#'
#' Cuando no hay estacionalidad ni tendencia, se dice que  la serie se comporta
#' como un proceso de ruido blanco, y se ve así:
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise", y = "")

y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "White noise")
