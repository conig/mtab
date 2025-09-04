skip_if_not_installed("lme4")
library(lme4)

test_that("can print convergence information", {
  ## ── Tiny synthetic set to force a quick non-convergence ───────────────────
  set.seed(1)
  ng <- 20
  ni <- 6
  grp <- factor(rep(seq_len(ng), each = ni))

  # Half the groups are all successes, half all failures (cluster-level separation)
  y <- as.integer(rep(rep(c(0, 1), each = ng / 2), each = ni))
  d_sep <- data.frame(y, grp)

  # Converged (baseline) on the same data: fixed intercept only
  m_ok <- glmer(
    y ~ 1 + (1 | grp),
    data = d_sep,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000))
  )

  # Deliberately starve the optimizer so it fails fast
  m_fail <- glmer(
    y ~ 1 + (1 | grp),
    data = d_sep,
    family = binomial,
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2))
  )

  ## ── Table printing should handle convergence info without throwing errors ─
  output <- mtab::h_tab(m_ok, m_fail, convergence_label = "*")
  expect_equal(grep("\\*", output$Term), 3)
})
