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
