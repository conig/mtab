
skip_if_not_installed("lme4")
library(lme4)

test_that("tab_model() and h_tab() work for glmerMod objects", {
  ## ── Binomial logistic ─────────────────────────────────────────────────────
  data(cbpp, package = "lme4")
  g1_bin <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
                  data = cbpp, family = binomial)
  g2_bin <- update(g1_bin, . ~ . + herd)  # non-nested but good for expect_no_error

  expect_no_error(mtab::tab(g1_bin))
  expect_no_error(mtab::h_tab(g1_bin, g2_bin))

  ## ── Poisson ───────────────────────────────────────────────────────────────
  data(Salamanders, package = "glmmTMB")
  g1_pois <- glmer(count ~ mined + (1 | site),
                   data = Salamanders, family = poisson)

  expect_no_error(mtab::tab(g1_pois))
})
