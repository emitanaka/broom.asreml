## code to prepare several asreml models

library(asreml)
library(agridat)
library(tidyverse)
devtools::load_all(".")

fit_chickweight <- asreml(weight ~ Diet, random=~ str(~Chick + Time:Chick, ~us(2):id(50)), data = ChickWeight)
fit_chickweight <- converge_asreml(fit_chickweight)
usethis::use_data(fit_chickweight, overwrite = TRUE)

fit_besag_met <- asreml(yield ~ county,
                         random =~ diag(county):block + diag(county):row + diag(county):col +
                           fa(county, 2):gen,
                         residual =~ dsum(~ar1(row):ar1(col) | county),
                         data = besag.met |>
                           mutate(across(c(col, row), as.factor)),
                        aom = TRUE)
fit_besag_met <- converge_asreml(fit_besag_met)


usethis::use_data(fit_besag_met, overwrite = TRUE)
