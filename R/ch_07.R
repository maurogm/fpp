#' ---
#' title: "Chapter 07: Time series regression models"
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

# rmarkdown::render("R/ch_07.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' En este capítulo se va a estudiar el problema de predecir una serie de tiempo
#' usando otras series de tiempo como predictores.
#' Por ejemplo, predecir el consumo eléctrico usando la temperatura como predictor.
#'
#' ## Link
#' https://otexts.com/fpp3/regression.html


#'
#' ## The linear model
#'
#' Introduce el modelo lineal.
#' Siguiendo con el framekowrk de los tsibbles, mables, fables, etc., el modo
#' de fittearlo es con la función `TSLM`. La salida luego se puede ver con
#' la función `report`:
consumption_model_mable <- us_change %>%
  model(TSLM(Consumption ~ Income))
consumption_model_mable %>% report()

#' El resultado que arroja es básicamente el mismo que usando `lm` y `summary`:
lm_clasico <- lm(Consumption ~ Income, data = us_change)
lm_clasico %>%
  summary()

#' ### Assumptions
#'
#' Pego la sección de supuestos porque es concisa y siempre está bueno tenerlos
#' a mano:
#'
#' ```
#' First, we assume that the model is a reasonable approximation to reality;
#' that is, the relationship between the forecast variable and the predictor
#' variables satisfies this linear equation.
#'
#' Second, we make the following assumptions about the errors:
#'
#'    - they have mean zero; otherwise the forecasts will be systematically biased.
#'    - they are not autocorrelated; otherwise the forecasts will be inefficient, as there is more information in the data that can be exploited.
#'    - they are unrelated to the predictor variables; otherwise there would be more information that should be included in the systematic part of the model.
#'
#' It is also useful to have the errors being normally distributed with a constant variance σ2
#' in order to easily produce prediction intervals.
#' ```

#'
#' ### Evaluating the regression model
#'
#' Acá habla del análisis de los residuos para evaluar la salud del modelo.
#' Parecido a los que se hace con `plot(lm)`.
#'
#' Es interesante un patrón que sugiere para detectar correlaciones espurias
#' que se dan cuando dos series que no tienen nada que ver tienen una misma
#' tendencia.
#' En el ejemplo, modela la cantidad de pasajeros de avión en Australia con
#' la producción de arroz en Guinea. Dos cosas que no tienen nada que ver, pero
#' que tienden a aumentar con el paso del tiempo:
fit <- aus_airpassengers %>%
  filter(Year <= 2011) %>%
  left_join(guinea_rice, by = "Year") %>%
  model(TSLM(Passengers ~ Production))
report(fit)

#' El R2 es altísimo. Pero cuando analizamos los residuos, vemos que los mismos
#' están altamente autocorrelacionados, lo que indica que nuestros datos tienen
#' una tendencia que no está capturada por el modelo.
#' Esto, en conjunción con el R2 alto, es un fuerte indicio de que estamos ante
#' una correlación espuria.
fit %>% gg_tsresiduals()

#'
#' ## Some useful predictors
#'
#' Dentro de un modelo que usa `TSLM` se le puede pasar a la fórmula las
#' funciones `trend` y `season`, para que cree automáticamente predictores
#' de tendencia y estacionalidad. El de tendencia crea un parámetro para la
#' variable `t`, y el de estacionalidad crea una dummy variable para cada
#' periodo de la serie.
#'
#' Por ejemplo, para el modelo de producción de cerveza:
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

#' Graficando como series:
augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(
    y = "Megalitres",
    title = "Australian quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Series"))

#' Graficando como scatterplot:
augment(fit_beer) %>%
  ggplot(aes(
    x = Beer, y = .fitted,
    colour = factor(quarter(Quarter))
  )) +
  geom_point() +
  labs(
    y = "Fitted", x = "Actual values",
    title = "Australian quarterly beer production"
  ) +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

#' ### Términos de Fourier
#'
#' En vezde usar dummy variables para la estacionalidad, se puede usar
#' términos de la serie de Fourier para capturar la estacionalidad.
#' Esto es especialmente útil cuando los períodos son muchos (por
#' ejemplo 52 semanas en un año), dado que a menudo alcanza con incluir
#' unos pocos términos de Fourier para capturar la estacionalidad, con
#' lo que tenemos la misma performance con un modelo de menor dimensión.
#'
#' Para el ejemplo de la cerveza:
fourier_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)


#' ## Selecting predictors
#'
#' Introduce como métricas de capacidad predictiva:
#' - R2 ajustado.
#' - Cross-validation.
#' - AIC.
#' - AICc.
#' - BIC.
#'
#' Todos estos (y algunas cosas más) son calculados por `fabletools::glance()`:
fit_consMR <- us_change %>%
  model(tslm = TSLM(Consumption ~ Income + Production +
    Unemployment + Savings))
glance(fit_consMR) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)


#' ## Forecasting with regression
#'
#' Si querés forecastear con un modelo de regresión, tenés que aportar
#' los valores de los predictores para el período de forecasteo, que
#' normalmente no tenés.
#' Si justo tu modelo se basa en eventos de calendario (por ejemplo, si
#' sólo usa features de drift y estacionalidad), entonces no tenés problema.
#' Pero si no, hay que estimar los valores de los predictores: ya sea con
#' otros forecasts, o trabajando bajo distintos escenarios.

fit_consBest <- us_change %>%
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )
future_scenarios <- fabletools::scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income = 1, Savings = 0.5, Unemployment = 0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income = -1, Savings = -0.5, Unemployment = 0),
  names_to = "Scenario"
)

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change %>%
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

#' **Modelo con predictores laggeados**: Si queremos poder predecir, tal
#' vez podemos armar un modelo  que use predictores laggeados. De este
#' modo, al momento realizar una predicción ya vamos a tener disponibles
#' los valores de los predictores que necesitamos.


#' 
#' ## Nonlinear regression
#' 
#' Primero habla de regresiones lineales sobre parámetros transformados.
#' 
#' Después introduce las regresiones no lineales, en particular la opción
#' de usar una función lineal a trozos.
#' 
#' Da el ejemplo de cómo usar una tendencia lienal a trozos:
boston_men <- boston_marathon %>%
  filter(Year >= 1924) %>%
  filter(Event == "Men's open division") %>%
  mutate(Minutes = as.numeric(Time)/60)

#' _Nota_: Dentro de `model()` puedo fitear varios modelos a la vez:
fit_trends <- boston_men %>%
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )
fc_trends <- fit_trends %>% forecast(h = 10)
fc_trends %>% head(15)

boston_men %>%
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes",
       title = "Boston marathon winning times")
