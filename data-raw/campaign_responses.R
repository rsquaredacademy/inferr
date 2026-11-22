# Simulated campaign responses for a fictional online store.
# The data are generated with a fixed seed so that the examples, README and
# vignette produce stable output.

set.seed(20261122)

n <- 200

customer_id <- seq_len(n)

# 120 returning customers and 80 new customers, in random order
segment <- factor(
  sample(rep(c("returning", "new"), c(120, 80))),
  levels = c("new", "returning")
)

# One email campaign: open, click and purchase form a funnel. The open rate is
# deliberately similar in the two segments (a non-significant difference),
# while the overall open rate is clearly above 50%.
p_open <- ifelse(segment == "new", 0.64, 0.61)
opened_email <- rbinom(n, 1, p_open)
clicked <- rbinom(n, 1, ifelse(opened_email == 1, 0.30, 0.03))
purchased <- rbinom(n, 1, ifelse(clicked == 1, 0.35, 0.04))

# Promotion measured before and after: a clear increase in purchases. Most
# discordant customers move from 0 to 1, so McNemar's test is significant.
bought_before <- rbinom(n, 1, 0.30)
p_after <- ifelse(bought_before == 1, 0.65, 0.50)
bought_after <- rbinom(n, 1, p_after)

# Three campaigns measured on the same customers. The response rates are close
# together, so Cochran's Q test is not significant.
latent <- rnorm(n)
promo_1 <- rbinom(n, 1, plogis(qlogis(0.40) + 0.5 * latent))
promo_2 <- rbinom(n, 1, plogis(qlogis(0.39) + 0.5 * latent))
promo_3 <- rbinom(n, 1, plogis(qlogis(0.41) + 0.5 * latent))

# Daily orders for 200 consecutive days. A 28-day seasonal cycle makes the
# sequence cluster above and below the 72-order target, so the runs test
# detects serial dependence.
day <- seq_len(n)
daily_orders <- round(72 + 14 * sin(2 * pi * day / 28) + rnorm(n, 0, 4))
above_target <- as.integer(daily_orders > 72)

campaign_responses <- data.frame(
  customer_id    = customer_id,
  segment        = segment,
  opened_email   = opened_email,
  clicked        = clicked,
  purchased      = purchased,
  bought_before  = bought_before,
  bought_after   = bought_after,
  promo_1        = promo_1,
  promo_2        = promo_2,
  promo_3        = promo_3,
  daily_orders      = daily_orders,
  above_target   = above_target
)

save(campaign_responses, file = "data/campaign_responses.rda",
     compress = "xz", version = 2)
