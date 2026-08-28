augment(fit_besag_met)

coef(fit_besag_met)$fixed
tidy(fit_besag_met, "fixed")

saveRDS(augment(fit_besag_met), test_path("fixtures", "augment_besag_met.rds"))
saveRDS(glance(fit_besag_met), test_path("fixtures", "glance_besag_met.rds"))
saveRDS(tidy(fit_besag_met), test_path("fixtures", "tidy_besag_met_all.rds"))
saveRDS(
  tidy(fit_besag_met, "fixed"),
  test_path("fixtures", "tidy_besag_met_fixed.rds")
)
saveRDS(
  tidy(fit_besag_met, "random"),
  test_path("fixtures", "tidy_besag_met_random.rds")
)
saveRDS(
  tidy(fit_besag_met, "vcomp"),
  test_path("fixtures", "tidy_besag_met_vcomp.rds")
)


# make a model fit with _ in the name

fit_besag_met_hard <- asreml::asreml(
  yield ~ 1,
  sparse = ~county_new,
  random = ~ diag(county_new):block +
    diag(county_new):row +
    diag(county_new):col +
    fa(county_new, 2):gen,
  residual = ~ dsum(~ ar1(row):ar1(col) | county),
  data = agridat::besag.met |>
    dplyr::mutate(dplyr::across(c(col, row), as.factor)) |>
    dplyr::mutate(
      county_new = fct_recode(county, "C_1" = "C1", "C_2_2" = "C2")
    ),
  aom = TRUE
)
fit_besag_met_hard <- converge_asreml(fit_besag_met_hard)
saveRDS(
  fit_besag_met_hard,
  test_path("fixtures", "fit_besag_met_hard.rds")
)
