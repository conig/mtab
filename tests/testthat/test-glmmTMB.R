library(glmmTMB)

test_that("Poisson glmmTMB succeeds", {


  m1 <- glmmTMB(y ~ Ed + (1|So), data = MASS::UScrime, family = "poisson")
  m2 <- glmmTMB(y ~ Ed + I(M.F/1000) + (1|So), data = MASS::UScrime, family = "poisson")

  testthat::expect_no_error(mtab::h_tab(m1, m2))


})

test_that("Logistic glmmTMB succeeds", {

  m1 <- glmmTMB(incidence > 0 ~ size + (1|herd), data = lme4::cbpp, family = "binomial")
  m2 <- glmmTMB(incidence > 0 ~ size + period + (1|herd), data = lme4::cbpp, family = "binomial")

  testthat::expect_no_error(mtab::h_tab(m1, m2))

})

test_that("nbinom1 glmmTMB successed",{
  m1 <- glmmTMB(y ~ Ed + (1|So), data = MASS::UScrime, family = nbinom1())
  m2 <- glmmTMB(y ~ Ed + I(M.F/1000) + (1|So), data = MASS::UScrime, family = nbinom1())

  testthat::expect_no_error(mtab::h_tab(m1, m2))
})
