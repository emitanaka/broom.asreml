
#' A MET model fitted to agridat::besag.met data
#'
#' The following data was fitted.
#'
#' ```
#' asreml(yield ~ county,
#'        random =~ diag(county):block + diag(county):row + diag(county):col +
#'           fa(county, 2):gen,
#'        residual =~ dsum(~ar1(row):ar1(col) | county),
#'        data = agridat::besag.met |>
#'           dplyr::mutate(dplyr::across(c(col, row), as.factor)),
#'        aom = TRUE)
#' ```
#'
"fit_besag_met"



#' A random intercent and random slope model
#'
#' The following model was fitted to the ChickWeight data.
#'
#' ```
#' asreml(weight ~ Diet, random=~ str(~Chick + Time:Chick, ~us(2):id(50)),
#'        data = ChickWeight)
#' ```
"fit_chickweight"
