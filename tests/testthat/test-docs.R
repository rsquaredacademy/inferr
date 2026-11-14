test_that("ifr_ts_var_test documentation matches the alternative argument", {
  rd <- test_path("..", "..", "man", "ifr_ts_var_test.Rd")
  skip_if_not(file.exists(rd))

  txt <- paste(readLines(rd), collapse = "\n")

  expect_match(txt, 'must be one of "less" \\(default\\), "greater" or "all"')
  expect_false(grepl('"both" \\(default\\)', txt))
})

test_that("binomial test documentation identifies the second level as success", {
  rd <- test_path("..", "..", "man", "ifr_binom_calc.Rd")
  skip_if_not(file.exists(rd))

  txt <- paste(readLines(rd), collapse = "\n")

  expect_match(txt, "The success is the \\\\strong\\{second\\} level of the factor")
  expect_match(txt, "\\\\item\\{two_tail\\}")
})

test_that("README example does not use the removed type argument", {
  rmd <- test_path("..", "..", "README.Rmd")
  md  <- test_path("..", "..", "README.md")
  skip_if_not(file.exists(rmd) && file.exists(md))

  expect_false(any(grepl("type = 'all'", readLines(rmd))))
  expect_false(any(grepl("type = 'all'", readLines(md))))
})

test_that("binomial and runs documentation use correct names and meanings (L2)", {
  bin <- test_path("..", "..", "man", "ifr_binom_calc.Rd")
  run <- test_path("..", "..", "man", "ifr_runs_test.Rd")
  skip_if_not(file.exists(bin) && file.exists(run))

  bin_txt <- paste(readLines(bin), collapse = "\n")
  run_txt <- paste(readLines(run), collapse = "\n")

  expect_false(grepl("ifr_binom_cal\\(", bin_txt))
  expect_match(bin_txt, "\\\\item\\{obs_p\\}\\{observed probability of success\\}")
  expect_match(run_txt, "\\\\item\\{threshold\\}\\{threshold used to count runs\\}")
  expect_false(grepl("within group sum of squares", run_txt))
})

test_that("DESCRIPTION uses the release maintainer email (M1)", {
  desc <- test_path("..", "..", "DESCRIPTION")
  skip_if_not(file.exists(desc))

  txt <- paste(readLines(desc), collapse = "\n")
  expect_match(txt, "aravindhebbali@rsquaredacademy\\.com", fixed = FALSE)
  expect_false(grepl("hebbali.aravind@gmail.com", txt))
})

test_that("the exam data source documentation is stable (M3)", {
  rd <- test_path("..", "..", "man", "exam.Rd")
  skip_if_not(file.exists(rd))

  txt <- paste(readLines(rd), collapse = "\n")
  expect_false(grepl("spss-tutorials.com", txt))
  expect_match(txt, "wikipedia.org/wiki/Cochran")
})

test_that("development reports are excluded from the build (L3)", {
  rb <- test_path("..", "..", ".Rbuildignore")
  skip_if_not(file.exists(rb))

  rules <- readLines(rb)
  expect_true(any(grepl("docs-dev", rules)))
})
