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
