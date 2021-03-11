# Title     : Test app runs
# Objective : slightly modified basic tests inspired by https://github.com/ThinkR-open/golem/blob/dev/R/test_helpers.R
# Created by: Joseph Percival
# Created on: 2021/03/09

library(golem)

test_that("app ui", {
  ui <- app_ui()
  expect_shinytaglist(ui)
})

test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
})

# this is too buggy: https://github.com/ThinkR-open/golem/issues/410
# test_that(
#   "app launches",{
#     skip_on_cran()
#     skip_on_travis()
#     skip_on_appveyor()
#     x <- processx::process$new(
#       "R",
#       c(
#         "-e",
#         "pkgload::load_all(here::here());run_app()"
#       )
#     )
#     Sys.sleep(5)
#     expect_true(x$is_alive())
#     x$kill()
#   }
# )








