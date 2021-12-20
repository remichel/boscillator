#' Test the boscillator function 'bosc' that creates an empty bosc-object for further use.

test_that("function creates an object of the 'bosc' class", {
  bosc <- bosc()
  expect_identical(class(bosc), "BOSC-Object")
  expect_type(bosc, "list")
})

test_that("object contains correct list of empty characteristics", {
  bosc <- bosc()
  expect_identical(bosc$timepoints, NULL)
  expect_identical(bosc$tests, NULL)
  expect_identical(bosc$hist, "")
  expect_identical(typeof(bosc$data), "list")
  expect_identical(bosc$data$single_trial, NULL)
  expect_identical(bosc$data$ss, NULL)
  expect_identical(bosc$data$ga, NULL)
})


