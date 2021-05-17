test_that("multiplication works", {
  params <- tribble(
    ~Var, ~Func, ~Args,
    "X1",  "s",     "bs='cr', k=5",
    "X2",  "s",     NA,
    "X3",  "s",    "",
  )


  construct_smooth_data(params, raw_dat)
})
