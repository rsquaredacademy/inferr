library(dplyr)

owanova <- function(data, x, y) UseMethod('owanova')

owanova.default <- function(data, x, y) {

	sample_stats <- anova_split(data, x, y)
	overall_stat <- overall_stats(data, x)

	sample_mean  <- anova_avg(data, x)[1, 1]

	sample_stats <- mutate(sample_stats,
	    sst = length * ((mean - sample_mean) ^ 2),
	    sse = (length - 1) * var
	)

	sstr    <- round(sum(sample_stats$sst), 3)
	ssee    <- round(sum(sample_stats$sse), 3)
	total   <- round(sstr + ssee, 3)
	df_sstr <- nrow(sample_stats) - 1
	df_sse  <- nrow(data) - nrow(sample_stats)
	df_sst  <- nrow(data) - 1
	mstr    <- round(sstr / df_sstr, 3)
	mse     <- round(ssee / df_sse, 3)
	f       <- round(mstr / mse, 3)
	sig     <- round(1- pf(f, df_sstr, df_sse), 3)
	obs     <- nrow(data)
	regs    <- paste(x, '~ as.factor(', y, ')')
	model   <- lm(as.formula(regs), data = data)
	reg     <- summary(model)

	result <- list(between   = sstr,
								 within    = ssee,
		       			 total     = total,
								 df_btw    = df_sstr,
								 df_within = df_sse,
								 df_total  = df_sst,
								 ms_btw    = mstr,
								 ms_within = mse,
								 f         = f,
								 p         = sig,
								 r2        = round(reg$r.squared, 4),
								 ar2       = round(reg$adj.r.squared, 4),
								 sigma     = round(reg$sigma, 4),
								 obs       = obs,
								 tab       = format(sample_stats[, c(1, 2, 3, 5)], nsmall = 3))

	class(result) <- 'owanova'
	return(result)

}

# output
print.owanova <- function(data, ...) {

	# width
	w1 <- nchar('Between Groups')
	w2 <- max(nchar('Squares'), nchar(data$between), nchar(data$within), nchar(data$total))
	w3 <- max(nchar('DF'), nchar(data$df_btw), nchar(data$df_btw), nchar(data$df_within), nchar(data$df_total))
	w4 <- max(nchar('Mean Square'), nchar(data$ms_btw), nchar(data$ms_within))
	w5 <- max(nchar('F'), nchar(data$f))
	w6 <- max(nchar('Sig.'), nchar(format(data$sig, nsmall = 4)))
	w <- sum(w1, w2, w3, w4, w5, w6, 21)
	w7 <- nchar(data$sigma)

	dc <- as.vector(data$tab[, 1])

	w8 <- max(nchar('Category'), max(nchar(dc)))
	w9 <- max(nchar('N'), max(nchar(data$tab[, 2])))
	w10 <- max(nchar('Mean'), max(nchar(format(data$tab[, 3], nsmall = 3))))
	w11 <- max(nchar('Std. Dev.'), max(nchar(data$tab[, 4])))
	wr <- sum(w8, w9, w10, w11, 13)


	p <- format(data$p, nsmall = 4)
	q <- nrow(data$tab)
	s <- length(data$tab)


	cat(fg('ANOVA', w), '\n')
	cat(rep("-", w), sep = "", '\n')
	cat(fg('', w1), fs(), fg('Sum of', w2), fs(), fg('', w3), fs(), fg('', w4), fs(), fg('', w5), fs(), fg('', w6), '\n')
	cat(fg('', w1), fs(), fg('Squares', w2), fs(), fg('DF', w3), fs(), fg('Mean Square', w4), fs(), fg('F', w5), fs(), fg('Sig.', w6), '\n')
	cat(rep("-", w), sep = "", '\n')
	cat(fl('Between Groups', w1), fs(), fg(data$between, w2), fs(), fg(data$df_btw, w3), fs(), fg(data$ms_btw, w4), fs(), fg(data$f, w5), fs(), fg(p, w6), '\n')
	cat(fl('Within Groups', w1), fs(), fg(data$within, w2), fs(), fg(data$df_within, w3), fs(), fg(data$ms_within, w4), fs(), fg('', w5), fs(), fg('', w6), '\n')
	cat(fl('Total', w1), fs(), fg(data$total, w2), fs(), fg(data$df_total, w3), fs(), fg('', w4), fs(), fg('', w5), fs(), fg('', w6), '\n')
	cat(rep("-", w), sep = "", '\n\n')

	cat(fg('Report', wr), '\n')
	cat(rep("-", wr), sep = "", '\n')
	cat(fg('Category', w8), fs(), fg('N', w9), fs(), fg('Mean', w10), fs(), fg('Std. Dev.', w11), '\n')
	cat(rep("-", wr), sep = "", '\n')
	for (i in seq_len(q)) {
		cat(fc(data$tab[i, 1], w8), fs(), fg(data$tab[i, 2], w9), fs(), fg(data$tab[i, 3], w10), fs(), fg(data$tab[i, 4], w11), '\n')
	}
	cat(rep("-", wr), sep = "", '\n\n')

	cat(fl('Number of obs', 13), '=', fl(data$obs, w7), fs(), fl('R-squared', 13), '=', data$r2, '\n')
	cat(fl('Root MSE', 13), '=', data$sigma, fs(), fl('Adj R-squared', 13), '=', data$ar2, '\n\n')
}
