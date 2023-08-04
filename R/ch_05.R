#' ---
#' title: "Chapter 05: The forecastr's toolbox"
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

# rmarkdown::render("R/ch_05.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' ## Link
#' https://otexts.com/fpp3/toolbox.html


#'
#' ## A tidy forecasting workflow
gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)

gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

#' Fitea un modelo usando la función `TSLM` (time series linear model) del
#' paquete `fable` para fittear un linear trend model:
fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))
fit

#' El resultado es un `mable` (model table), que se puede usar para forecastear
#' una `h` cantidad de períodos en el futuro, obteniendo un `fable` (forecast table)`:
fit %>% forecast(h = "3 years")

#' Con el `fable` se puede hacer un `autoplot` para visualizar el forecast:
fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Sweden") %>%
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")


#' ## Some simple forecasting methods
#'
#' Esta sección explora métodos muy simples, a menudo útiles como baseline.
bricks <- aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4") %>% # convenient shorthand for extracting a section of a time series
  select(Bricks)
bricks

#' #### MEAN
#'
#' `MEAN` promedia los valores de la serie de tiempo:
bricks %>%
  model(MEAN(Bricks)) %>%
  forecast(h = "5 years") %>%
  autoplot(bricks)

#' #### Naive
#'
#' `NAIVE` extiende el último valor de la serie de tiempo:
bricks %>%
  model(NAIVE(Bricks)) %>%
  forecast(h = "5 years") %>%
  autoplot(bricks)
#' Este método es óptimo si el fenómeno es un paseo al azar.

#'
#' #### Seasonal Naive
#'
#' Similar al Naive, pero cuando la serie de tiempo tiene una componente
#' estacional, `SNAIVE` extiende el último valor de la serie de tiempo
#' en cada estación:
bricks %>%
  model(SNAIVE(Bricks ~ lag("year"))) %>%
  forecast(h = "5 years") %>%
  autoplot(bricks)
#' El `lag` se necesita porque en algunos casos la serie de tiempo tiene
#' más de un perído estacional, y en esos casos hay que especificar
#' cuál se quiere usar.

#'
#' #### Drift
#'
#' Finalmente este método es similar al NAIVE (extiende el último valor),
#' pero permite ir variando de a poco el forecast. La variación que introduce
#' en cada unidad de tiempo es igual a la variación promedio entre dos puntos
#' sucesivos que observó en la data histórica:
bricks %>%
  model(NAIVE(Bricks ~ drift())) %>%
  forecast(h = "5 years") %>%
  autoplot(bricks)

#' ### Example: Australian quarterly beer production
# Set training data from 1992 to 2006
train <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h = 14)
beer_fc
# Plot forecasts against actual values
beer_fc %>%
  autoplot(train, level = NULL) + # level=NULL avoids plotting ICs
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


#' ## Fitted values and residuals
#'
#' La función `augment` permite agregar a un `fable` los valores ajustados
#' y los residuos. Además incluye los "innovation residuals", que en caso
#' de mostrar un patrón indican que el modelo todavía puede ser mejorado
#' (ver sección siguiente).
fabletools::augment(beer_fit)

#' ## Residual diagnostics
#'
#' Estas propiedades son condición necesaria para que un modelo sea bueno:
#'
#' 1. The innovation residuals are uncorrelated. If there are correlations between
#' innovation residuals, then there is information left in the residuals which
#' should be used in computing forecasts.
#' 2. The innovation residuals have zero mean. If they have a mean other than zero,
#' then the forecasts are biased.
#'
#' Estas otras propiedades son deseables, pero no siempre se cumplen:
#'
#' 3. The innovation residuals have constant variance. This is known as “homoscedasticity”.
#' 4. The innovation residuals are normally distributed.

#'
#' ### Example: Forecasting Google daily closing stock prices
#'
#' Vamos a forecastear los precios de cierre de las acciones de Google:
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)
google_2015 <- google_stock %>% filter(year(Date) == 2015)
autoplot(google_2015, Close) +
  labs(
    y = "$US",
    title = "Google daily closing stock prices in 2015"
  )

#' Para los precios de acciones, pocos modelos suelen ser mejores que
#' el naif (predecir el último valor observado).
#' Fiteamos ese modelo, y estudiamos sus residuos:
aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()
autoplot(aug, .innov) +
  labs(
    y = "$US",
    title = "Residuals from the naïve method"
  )
#' No parece haber autocorrelación en los residuos, y estos parecen centrarse
#' alrededor del 0. Vemos un histograma:
aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")
#' Efectivamente se centran en 0, y son aproximadamente normales, aunque con
#' una cola un poco pesada a derecha.
#'
#' Para ver mejor la autocorrelación, vemos las correlaciones entre los residuos
#' originales y todos sus lagas de 1 a 23:
aug %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "Residuals from the naïve method")
#' Una vez más, no encontramos autocorrelaciones.
#'
#' Estos 3 gráficos son tan útiles que `feasts` tiene la función
#' `gg_tsresiduals` que los genera todos juntos:
google_2015 %>%
  model(NAIVE(Close)) %>%
  feasts::gg_tsresiduals()

#' ### Portmanteau tests for autocorrelation
#'
#' Si se quiere hacer un test más formal de autocorrelación, se puede usar
#' un test de Portmanteau.
#'
#' La idea de estos es similar a la del F-test. Dado que se consideran muchos
#' lags a la vez (por ejemplo en los gráficos de ACF anteriores), la idea es
#' controlar que las aparentes autocorrelaciones que puedan aparecer no sean
#' por azar.
#'
#' Se proponen 2 tests, cuyos estadísticos son explicados en el texto.
#'
#' Para calcular el test de **Box-Pierce**:
aug %>% features(.innov, box_pierce, lag = 10, dof = 0)
#' Para calcular el test de **Ljung-Box**:
aug %>% features(.innov, ljung_box, lag = 10, dof = 0)
#' En ambos casos se obtienen p-valores no significativos.

#'
#' ## Distributional forecasts and prediction intervals
#'
#' Muchos métodos para calcular intervalos de predicción se basan en
#' supuestos sobre la distribución de los residuos, a menudo asumiendo
#' que son normales.
#'
#' De este modo, podemos calcular el desvío estándar de los residuos
#' y pensar que el margen de error de nuestra predicción puntual será
#' proporcional a ese devío.
#' A medida que aumenta el horizonte de predicción, estos errores se
#' acumulan, y por lo tanto el intervalo de predicción se vuelve más
#' amplio.
#'
#' En `fabletools` se puede calcular un intervalo de predicción de
#' este tipo con la función `hilo`:
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo(level = c(80, 95, 99))

#' Si se desea graficarlo, se puede usar `autoplot` directamente,
#' sin pasar por `hilo`:
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015, level = c(80, 95, 99)) +
  labs(title = "Google daily closing stock price", y = "$US")

#' ### Prediction intervals from bootstrapped residuals
#'
#' A veces la asunción de normalidad de los residuos no es razonable.
#' En estos casos, se puede usar un método de bootstrap, que sólo asume
#' que los residuos son independientes y tienen varianza finita.
#'
#' En este caso lo que hacemos es simular nuevos residuos bootstrapeados
#' de los residuos históricos, y usarlos para así ir construyendo una
#' serie de predicciones.
#' La función `generate` nos puede ayudar en esta tarea:
fit <- google_2015 %>%
  model(NAIVE(Close))
sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
sim

google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
    data = sim
  ) +
  labs(title = "Google daily closing stock price", y = "$US") +
  guides(colour = "none")

#' Ahora, para calcular los intervalos de predicción, basta con simular
#' una cantidad arbitrarariamente grande de estas secuencias de predicciones,
#' y calcular los percentiles en función de la confianza que queramos darle.
#'
#' La función `forecast` puede hacer esto por nosotros directamente con
#' el argumento `bootstrap`, ahorrándonos el trabajo de tener que simular:
fc <- fit %>% forecast(h = 30, bootstrap = TRUE, times = 4000)
fc
#' Pasando el objeto `fc` a `autoplot` se grafican los intervalos de predicción:
autoplot(fc, google_2015, level = c(80, 95, 99)) +
  labs(title = "Google daily closing stock price", y = "$US")

#' ## Forecasting using transformations
#'
#' Cuando se hacen forecasts de una serie trasnformada, procedemos igual
#' y después aplicamos la transformación inversa sobre las predicciones y
#' los intervalos.
#' El texto muestra la inversa de la transformación de Box-Cox.
#'
#' Al aplicar la inversa sobre una serie normal aparece un sesgo,
#' en el sentido de que la mediana se aparta de la media. Por más que
#' la inversa de las predicciones puntuales es la mediana, muchas veces
#' es preferible trabajar con las medias.
prices %>%
  filter(!is.na(eggs)) %>%
  model(RW(log(eggs) ~ drift())) %>%
  forecast(h = 50) %>%
  autoplot(prices %>% filter(!is.na(eggs)),
    level = 80, point_forecast = lst(mean, median)
  ) +
  labs(
    title = "Annual egg prices",
    y = "$US (in cents adjusted for inflation) "
  )


#' ## Forecasting with decomposition
#'
#' Queremos hacer forecasts de una serie con estacionalidad.
#' Vamos a descomponer la serie y forecastear sus componentes por separado.
#' A menudo se asume que la estacinalidad es constante, por lo que el
#' "forecast" es simplemente el último valor del período.
#'
#' Tomemos la serie de empleo en el sector retail de los Estados Unidos:
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")
us_retail_employment %>% autoplot()

dcmp <- us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
  components() %>%
  select(-.model)
dcmp
#' Fitteamos la serie desestacionalizada, por ejemplo con un modelo
#' NAIVE:
dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(
    y = "Number of people",
    title = "US retail employment"
  )
#' Y ahora habría que agregarle el componente estacional a los forecasts
#' para tener nuestro forecast final.
#'
#' Todo esto puede hacerse en un solo paso con la función `decomposition_model`.
#' Se le pueden pasar los modelos a usar para cada componente (y en caso de
#' no especificar nada para la componente estacional, se usa `SNAIVE`):
fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment) +
  labs(
    y = "Number of people",
    title = "US retail employment"
  )

#' Es interesante que al ver los residuos, podemos notar que claramente
#' hay una autocirrelación en los mismos. Esto es porque nuestro modelo
#' NAIVE no está capturando el drift de tendencia.
fit_dcmp %>% gg_tsresiduals()

#' ## Evaluating point forecast accuracy
#'
#' Nada muy novedoso.
#'
#' - Habla de train-test split.
#' - Habla de errores como MAE, RMSE, MAPE y sMAPE.
#' - Es interesante el "**MASE**" (Mean Absolute Scaled Error), que lo
#' que hace es dividir a los errores por el error promedio de un modelo
#' baseline (por ejemplo, un modelo NAIVE).
#'
#' En el paquete `forecast` hay una función `accuracy` que nos permite
#' calcular todos estos errores:
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
beer_train <- recent_production %>%
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit %>%
  forecast(h = 10)

beer_fc %>%
  autoplot(
    aus_production %>% filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

accuracy(beer_fc, recent_production)

#' ## Evaluating distributional forecast accuracy
#'
#' En esta sección da scores para evaluar la calidad ya no de predicciones
#' puntuales, sino de forecasts de distribuciones.
#' 
#' - **Quantile score**: Un score que para una cierta probabilidad p, considera
#' la diferencia entre el p-cuantil y el valor real, castigando los errores 
#' cometidos cuando el valor real está más allá del p-cuantil.
#' - **Winkler score**: Un score proporcional a la longitud del intervalo de
#' confianza para un nivel alpha dado, que además castiga cuando el valor
#' observado está por fuera del intervalo.
#' - **CRPS**: Entiendo que es el más razonable... un promedio ponderado de
#' los errores de predicción, pesando los errores por la distribución de
#' probabilidad predicha.
#' - **Skill score**: Es una forma de normalizar el score, parecido a lo que 
#' se hizo con el _MASE_ para las predicciones puntuales. En este caso, se
#' elige una métrica (por ejemplo el _CRPS_) y se calcula la mejora en esa
#' métrica respecto a un modelo baseline (por ejemplo, un modelo NAIVE).
#' 
#' A conticuación mostramos cómo calcular estás métricas con la función
#' `accuracy`.
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit %>%
  forecast(google_jan_2016)

google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock, list(qs=quantile_score), probs=0.10)

google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock,
    list(winkler = winkler_score), level = 80)

google_fc %>%
  accuracy(google_stock, list(crps = CRPS))

google_fc %>%
  accuracy(google_stock, list(skill = skill_score(CRPS)))


#' ## Time series cross-validation
#'
#' Habla de hacer cross-validation para series de tiempo agregando datos
#' de a uno y evaluando el modelo en cada paso.
#' 
#' Lo hace con la función `stretch_tsibble`, de la que podemos setear los
#' parámetros `.init` para indicar cuántos datos usar para como mínimo, y
#' `.step` para indicar cuántos datos agregar en cada paso.

google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  relocate(Date, Symbol, .id)
google_2015_tr

#' Con eso lo que hace es replicar la serie de tiempo varias veces 
#' agregando los datos de a uno, e identificando cada repetición con
#' la variable `.id`.
#' 
#' Con todos estos "datasets" distintos, podemos entrenar un modelo
#' para cada uno y usar `accuracy` para evaluar las métricas sobre
#' todos los modelos:
google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1) %>%
  accuracy(google_2015)

#' Puedo comparar con la métrica sobre el modelo entrenado sobre el
#' dataset de entrenamiento entero:
google_2015 %>%
  model(RW(Close ~ drift())) %>%
  accuracy()

#' Finalmente, veamos que podemos hacer este cross-validation con
#' un horizonte de forecast mayor a 1. A continuación hacemos lo mismo
#' forcasteando hasta 8 pasos más adelante, y vemos cómo el RMSE va
#' empeorando a medida que el horizonte de forecast aumenta:
fc <- google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 8) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Close", distribution = Close)
fc %>%
  accuracy(google_2015, by = c("h", ".model")) %>%
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()
