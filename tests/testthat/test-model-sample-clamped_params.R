context("model-sample_clamped_params")

dev_path <- Sys.getenv("CMDSTAN_DEV")
dev_mode <- nchar(dev_path) > 0
if(!dev_mode) {
  message("Use Sys.setenv('CMDSTAN_DEV' = 'your/path/to/cmdstan') first!")
}
skip_if(!dev_mode)
set_cmdstan_path(dev_path)

test_that("a scalar parameter can be clamped", {
  mod <- cmdstan_model(write_stan_file(
    "parameters {
        real x;
        real<lower = 0.0> y;
      }
      model {
        x ~ normal(0.5, 1);
        y ~ normal(0, 1);
      }"
  ))
  Y_VAL <- 0.234
  expect_sample_output(
    fit <-mod$sample(clamped_params = list(y = Y_VAL), chains =  1), 1)
  expect_is(fit, "CmdStanMCMC")

  # JSON file exists
  cfile <- fit$clamped_params_file()
  expect_true(file.exists(cfile))
  ending <- substr(cfile,nchar(cfile)-4,nchar(cfile))
  expect_equal(ending, ".json")

  # Clamped parameter has constant value
  Y_DRAWS <- as.numeric(fit$draws(variables = "y"))
  expect_equal(sum(Y_DRAWS!=Y_VAL), 0)

  # Full clamping (all params) (TODO: what should happen?)
  #expect_sample_output(
  #  fit <- mod$sample(clamped_params = list(x = 3.4, y = 1.0), chains =  1), 1)
  #expect_is(fit, "CmdStanMCMC")

})


test_that("a vector parameter can be clamped", {
  mod <- cmdstan_model(write_stan_file(
    "parameters {
        real x;
        vector<lower = 0.0>[3] y;
     }
     model {
      x ~ normal(0.5, 1);
      y ~ normal(0, 1);
     }"
  ))
  Y_VAL <- c(3,4,5)
  expect_sample_output(
    fit <- mod$sample(clamped_params = list(y = Y_VAL), chains =  1), 1)
  expect_is(fit, "CmdStanMCMC")

  # Clamped vector stays constant
  Y1_DRAWS <- as.numeric(fit$draws(variables = "y[1]"))
  Y2_DRAWS <- as.numeric(fit$draws(variables = "y[2]"))
  Y3_DRAWS <- as.numeric(fit$draws(variables = "y[3]"))
  expect_equal(sum(Y1_DRAWS!=Y_VAL[1]), 0)
  expect_equal(sum(Y2_DRAWS!=Y_VAL[2]), 0)
  expect_equal(sum(Y3_DRAWS!=Y_VAL[3]), 0)

  # Partial clamping of a vector (TODO: what should happen?)
  #Y_VAL <- c(3)
  #expect_sample_output(
  #  fit <- mod$sample(clamped_params = list(y = Y_VAL), chains =  1), 1)
  #expect_is(fit, "CmdStanMCMC")

})
