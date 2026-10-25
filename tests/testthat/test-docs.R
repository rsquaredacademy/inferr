test_that("ifr_ts_var_test documentation matches the alternative argument", {
  rd <- test_path("..", "..", "man", "ifr_ts_var_test.Rd")
  skip_if_not(file.exists(rd))

  txt <- paste(readLines(rd), collapse = "\n")

  expect_match(txt, 'must be one of "less" \\(default\\), "greater" or "all"')
  expect_false(grepl('"both" \\(default\\)', txt))
})

test_that("README example does not use the removed type argument", {
  rmd <- test_path("..", "..", "README.Rmd")
  md  <- test_path("..", "..", "README.md")
  skip_if_not(file.exists(rmd) && file.exists(md))

  expect_false(any(grepl("type = 'all'", readLines(rmd))))
  expect_false(any(grepl("type = 'all'", readLines(md))))
})
