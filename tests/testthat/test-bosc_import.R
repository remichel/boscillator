test_that("import works", {

  test = data.frame(sub = sort(rep(seq(1,2), 40)),
                    t = rep(rep(seq(0,.95,.05),2),2),
                    trl = rep(seq(1,40,1),2),
                    r = rbinom(80,1,.5))

  bosc = bosc_import(test,
                     sfreq = 20,
                     n_timepoints = 20,
                     vars = colnames(test),
                     level = "single_trial",
                     aggregate = T)


  expect_equal(sort(unique(test$sub)), sort(unique(bosc$data$single_trial$real$data$subj)))
  # add more tests here
})
