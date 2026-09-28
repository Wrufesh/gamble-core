if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
} else {
  test_that <- function(desc, code) eval(substitute(code))
  expect_equal <- function(a, b) stopifnot(identical(a, b))
  expect_error <- function(code, ...) tryCatch({ eval(substitute(code)); stop("expected error") }, error = function(e) invisible(NULL))
}

source("tools/gamble_config.R")

test_that("gamble_config handles pixel resolution, intersection, and y_lag", {
  cfg <- gamble_config(task = "flat_design", pixel_res = 5, pixel_intersect = "native", y_lag = 2010)
  expect_equal(cfg$data$pixel_res, 5L)
  expect_equal(cfg$data$pixel_intersect, "native")
  expect_equal(cfg$data$y_lag, "2010")
  env <- compile_to_env(cfg)
  expect_equal(env[["PIXEL_RES"]], "5")
  expect_equal(env[["PIXEL_INTERSECT"]], "native")
  expect_equal(env[["Y_LAG"]], "2010")
})

test_that("gamble_config infers architecture correctly", {
  cfg_flat <- gamble_config(task = "flat_fit")
  expect_equal(cfg_flat$model$architecture, "flat")

  cfg_nested <- gamble_config(task = "nested")
  expect_equal(cfg_nested$model$architecture, "nested")

  cfg_count <- gamble_config(task = "count")
  expect_equal(cfg_count$model$architecture, "count")
})

test_that("gamble_config blocks invalid cross-parameter combinations", {
  expect_error(
    gamble_config(task = "nested", use_bart = TRUE),
    "use_bart = TRUE.*invalid for nested architecture"
  )
})

test_that("gamble_config rejects undeclared overrides", {
  expect_error(
    gamble_config(task = "flat_fit", overrides = list(TOTALLY_FAKE_KNOB_XYZ = "123")),
    "Overrides contain undeclared knobs"
  )
})

test_that("compile_to_env flattens variables properly", {
  cfg <- gamble_config(
    task = "flat_fit",
    classification = "GLOBIOM_subclass",
    nsample = 6000,
    nburn = 2000,
    chains = 4,
    cores = 4,
    use_bart = TRUE,
    overrides = list(BART_COLS = "topo")
  )
  env <- compile_to_env(cfg)

  expect_equal(env[["TASK"]], "flat_fit")
  expect_equal(env[["USE_BART"]], "TRUE")
  expect_equal(env[["NSAMPLE"]], "6000")
  expect_equal(env[["NBURN"]], "2000")
  expect_equal(env[["N_CHAINS"]], "4")
  expect_equal(env[["N_CORES"]], "4")
  expect_equal(env[["EXTRA"]], "BART_COLS=topo")
})

test_that("export_env produces valid formats", {
  cfg <- gamble_config(task = "flat_fit", run_id = "test_01")
  env <- compile_to_env(cfg)

  json_out <- export_env(env, format = "json")
  expect_true(grepl('"TASK": "flat_fit"', json_out))

  docker_out <- export_env(env, format = "docker")
  expect_true(grepl('-e TASK="flat_fit"', docker_out))

  sing_out <- export_env(env, format = "singularity")
  expect_true(grepl('--env TASK="flat_fit"', sing_out))

  bash_out <- export_env(env, format = "bash")
  expect_true(any(grepl('^export TASK="flat_fit"', bash_out)))
})

cat("All gamble_config tests passed successfully!\n")
