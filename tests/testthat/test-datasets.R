test_that("wholesale_customers has the expected dimensions and column types", {
  expect_s3_class(wholesale_customers, "data.frame")
  expect_equal(dim(wholesale_customers), c(440L, 8L))
  expect_named(
    wholesale_customers,
    c("channel", "region", "fresh", "milk", "grocery", "frozen",
      "detergents_paper", "delicassen")
  )

  expect_s3_class(wholesale_customers$channel, "factor")
  expect_s3_class(wholesale_customers$region, "factor")
  expect_equal(nlevels(wholesale_customers$channel), 2L)
  expect_equal(nlevels(wholesale_customers$region), 3L)

  spending <- c("fresh", "milk", "grocery", "frozen",
                "detergents_paper", "delicassen")
  for (col in spending) {
    expect_type(wholesale_customers[[col]], "integer")
    expect_false(anyNA(wholesale_customers[[col]]))
  }
})

test_that("campaign_responses has the expected dimensions and column types", {
  expect_s3_class(campaign_responses, "data.frame")
  expect_equal(dim(campaign_responses), c(200L, 12L))
  expect_named(
    campaign_responses,
    c("customer_id", "segment", "opened_email", "clicked", "purchased",
      "bought_before", "bought_after", "promo_1", "promo_2", "promo_3",
      "daily_orders", "above_target")
  )

  expect_type(campaign_responses$customer_id, "integer")
  expect_s3_class(campaign_responses$segment, "factor")
  expect_equal(levels(campaign_responses$segment), c("new", "returning"))
  expect_type(campaign_responses$daily_orders, "double")
  expect_false(anyNA(campaign_responses))
})

test_that("campaign_responses 0/1 columns are coded 0/1 with both values", {
  binary <- c(
    "opened_email", "clicked", "purchased", "bought_before", "bought_after",
    "promo_1", "promo_2", "promo_3", "above_target"
  )

  for (col in binary) {
    expect_type(campaign_responses[[col]], "integer")
    expect_equal(sort(unique(campaign_responses[[col]])), c(0L, 1L))
  }
})

test_that("campaign_responses above_target matches the daily order counts", {
  expect_equal(
    campaign_responses$above_target,
    as.integer(campaign_responses$daily_orders > 72)
  )
})
