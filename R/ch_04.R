#' ---
#' title: "Chapter 04: Time series features"
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

# rmarkdown::render("R/ch_04.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

library(tidyverse)
library(fpp3)

#'
#' Más que nada se explorarán las capacidades del paquete `feasts`
#' (FEatures And Statistics from Time Series).
#' 
#' ## Link
#' https://otexts.com/fpp3/features.html


#'
#' ## Some simple statistics
#' 
tourism %>% features(Trips, quantile)

#' ## ACF features
#' 
#' La función `feat_acf` devuelve la siguiente información:
#' 
#' - the first autocorrelation coefficient from the original data;
#' - the sum of squares of the first ten autocorrelation coefficients from the original data;
#' - the first autocorrelation coefficient from the differenced data;
#' - the sum of squares of the first ten autocorrelation coefficients from the differenced data;
#' - the first autocorrelation coefficient from the twice differenced data;
#' - the sum of squares of the first ten autocorrelation coefficients from the twice differenced data;
#' - For seasonal data, the autocorrelation coefficient at the first seasonal lag is also returned.

tourism %>% features(Trips, feat_acf)

#' ## STL features
#' 
#' La función `feat_stl` devuelve la siguiente información:
#' 
#' - trend_strength mide la fuerza del trend component de la descomposición STL.
#' Se mide en función de la diferencia entre la varianza desestacionalizada y
#' la varianza del componente Residual. Si la diferencia es mucha, es porque
#' el componente Trend explica mucha más varianza que el componente de los residuos.
#' - seasonal_strength es análogo, pero midiendo la fuerza del componente estacional,
#' y se calcula comparando la varianza _destrendizada_ con la de los residuos.
#' - seasonal_peak_year indicates the timing of the peaks — which month or quarter contains the largest seasonal component. This tells us something about the nature of the seasonality. In the Australian tourism data, if Quarter 3 is the peak seasonal period, then people are travelling to the region in winter, whereas a peak in Quarter 1 suggests that the region is more popular in summer.
#' - seasonal_trough_year indicates the timing of the troughs — which month or quarter contains the smallest seasonal component.
#' - spikiness measures the prevalence of spikes in the remainder component Rt 
#' of the STL decomposition. It is the variance of the leave-one-out variances of Rt.
#' - linearity measures the linearity of the trend component of the STL decomposition. It is based on the coefficient of a linear regression applied to the trend component.
#' - curvature measures the curvature of the trend component of the STL decomposition. It is based on the coefficient from an orthogonal quadratic regression applied to the trend component.
#' - stl_e_acf1 is the first autocorrelation coefficient of the remainder series.
#' - stl_e_acf10 is the sum of squares of the first ten autocorrelation coefficients of the remainder series.

tourism %>% features(Trips, feat_stl)

#' Esto es útil si se tienen muchas series diferentes y se quiere comparar sus 
#' características. Por ejemplo haciendo lo siguiente se ve que los viajes 
#' vacacionales son los más afectados por la estacionalidad:

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

#' ## Otros features
#' 
#' A continuación se listan muchos más features, muchos de los cuales se discuten
#' en otros lados del libro:
#' 
#' - coef_hurst will calculate the Hurst coefficient of a time series which is a measure of “long memory”. A series with long memory will have significant autocorrelations for many lags.
#' - feat_spectral will compute the (Shannon) spectral entropy of a time series, which is a measure of how easy the series is to forecast. A series which has strong trend and seasonality (and so is easy to forecast) will have entropy close to 0. A series that is very noisy (and so is difficult to forecast) will have entropy close to 1.
#' - box_pierce gives the Box-Pierce statistic for testing if a time series is white noise, and the corresponding p-value. This test is discussed in Section 5.4.
#' - ljung_box gives the Ljung-Box statistic for testing if a time series is white noise, and the corresponding p-value. This test is discussed in Section 5.4.
#' - The kth partial autocorrelation measures the relationship between observations k periods apart after removing the effects of observations between them. So the first partial autocorrelation (k=1) is identical to the first autocorrelation, because there is nothing between consecutive observations to remove. Partial autocorrelations are discussed in Section 9.5. The feat_pacf function contains several features involving partial autocorrelations including the sum of squares of the first five partial autocorrelations for the original series, the first-differenced series and the second-differenced series. For seasonal data, it also includes the partial autocorrelation at the first seasonal lag.
#' - unitroot_kpss gives the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) statistic for testing if a series is stationary, and the corresponding p-value. This test is discussed in Section 9.1.
#' - unitroot_pp gives the Phillips-Perron statistic for testing if a series is non-stationary, and the corresponding p-value.
#' - unitroot_ndiffs gives the number of differences required to lead to a stationary series based on the KPSS test. This is discussed in Section 9.1
#' - unitroot_nsdiffs gives the number of seasonal differences required to make a series stationary. This is discussed in Section 9.1.
#' - var_tiled_mean gives the variances of the “tiled means” (i.e., the means of consecutive non-overlapping blocks of observations). The default tile length is either 10 (for non-seasonal data) or the length of the seasonal period. This is sometimes called the “stability” feature.
#' - var_tiled_var gives the variances of the “tiled variances” (i.e., the variances of consecutive non-overlapping blocks of observations). This is sometimes called the “lumpiness” feature.
#' - shift_level_max finds the largest mean shift between two consecutive sliding windows of the time series. This is useful for finding sudden jumps or drops in a time series.
#' - shift_level_index gives the index at which the largest mean shift occurs.
#' - shift_var_max finds the largest variance shift between two consecutive sliding windows of the time series. This is useful for finding sudden changes in the volatility of a time series.
#' - shift_var_index gives the index at which the largest variance shift occurs.
#' - shift_kl_max finds the largest distributional shift (based on the Kulback-Leibler divergence) between two consecutive sliding windows of the time series. This is useful for finding sudden changes in the distribution of a time series.
#' - shift_kl_index gives the index at which the largest KL shift occurs.
#' - n_crossing_points computes the number of times a time series crosses the median.
#' - longest_flat_spot computes the number of sections of the data where the series is relatively unchanging.
#' - stat_arch_lm returns the statistic based on the Lagrange Multiplier (LM) test of Engle (1982) for autoregressive conditional heteroscedasticity (ARCH).
#' - guerrero computes the optimal λ value for a Box-Cox transformation using the Guerrero method (discussed in Section 3.1).

#' 
#' ## Ejemplo de exploración
#' 
#' El paquete `feasts` tiene muchas más features. Es posible calcularlas todas
#' de este modo:
tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features

#' En total son 48, por lo que mirarlas todas a la vez es difícil.
#' Una opción es mirarlas por categoría, por ejemplo, las relacionadas con
#' la estacionalidad:
library(glue)
tourism_features %>%
  select_at(vars(contains("season"), Purpose)) %>%
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour = Purpose))

#' Otra opción interesante es apllcarles una técnica de reducción de
#' dimensionalidad como PCA:
pcs <- tourism_features %>%
  select(-State, -Region, -Purpose) %>%
  prcomp(scale = TRUE) %>%
  augment(tourism_features)
pcs %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

#' Ahí podemos ver cómo por la componente 2 parece estar separando a los
#' viajes vacacionales del resto. Ytambién cómo hay 4 puntos (los de valores
#' más alto en la componente 1) que parecen estar separados del resto.
#' Si quisiera, podría identificarlos y ver qué tienen de especial:
outliers <- pcs %>%
  arrange(desc(.fittedPC1)) %>% 
  head(4) %>%
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)

outliers %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  mutate(
    Series = glue("{State}", "{Region}", "{Purpose}",
                  .sep = "\n\n")
  ) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")
