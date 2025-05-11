library(tidyverse)
library(asreml)
library(testthat)

set.seed(1)
ng <- 400
n <- ng * 10
lvl_gens <- paste0("G", 1:ng)
eff_gens <- setNames(rnorm(length(lvl_gens), 0, 10), lvl_gens)
eff_err <- rnorm(n, 0, 5)
df <- tibble(gen = rep(lvl_gens, each = 10)) |>
  mutate(y = eff_gens[gen] + 20 + eff_err) |>
  mutate(gen = factor(gen))


fit <- asreml_converge(y ~ 1, random=~gen, data = df)
fit$coefficients
fit$vcoeff
vc <- fit$vparameters * fit$sigma2
10^2 / (5^2 + 10^2)
X <- matrix(1, nrow = nrow(df), ncol = 1)
Z <- model.matrix(~ -1 + gen, data = df)
G <- diag(ng) * vc[["gen"]]
R <- diag(n) * fit$sigma2
V <- Z %*% G %*% t(Z) + R
Vi <- solve(V)
P <- Vi - Vi %*% X %*% solve(t(X) %*% Vi %*% X) %*% t(X) %*% Vi
PEV <- G - G %*% t(Z) %*% P %*% Z %*% G
diag(PEV)
fit$vcoeff$random * fit$sigma2
cor(diag(PEV), fit$vcoeff$random * fit$sigma2)
vc <- fit$vparameters * fit$sigma2
vc[["gen"]] / (vc[["gen"]] + fit$sigma2)
mean(sqrt(1 - diag(PEV) / vc[["gen"]]))
