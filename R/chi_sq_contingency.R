# source cross table 
source('helpers/helper.R')


formats <- function() {
    x <- rep("    ")
    return(x)
}

# chi square test of independence
exp_f <- function(x, y) {

    # dimensions
    k <- table(x, y)
    dk <- dim(k)
    ds <- prod(dk)
    nr <- dk[1]
    nc <- dk[2]
    

    if (ds == 4) {

        # pearson chi square
        twoway <- matrix(table(x, y), nrow = 2)
        mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = 2)
        mat2 <- matrix(colSums(twoway), nrow = 1)
        ef <- mat1 %*% mat2
        chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
        df <- (nrow(twoway) - 1) *  (ncol(twoway) - 1)
        sig <- round(pchisq(chi, df, lower.tail = F), 4)
        
        # likelihood ratio chi square
        chilr <- round(2 * sum(matrix(log(twoway / ef), nrow = 1) %*% matrix(twoway, nrow = 4)), 4)
        sig_lr <- round(pchisq(chilr, df, lower.tail = F), 4)
        
        # Yates continuity correction
        way2 <- twoway[, c(2, 1)]
        total <- sum(twoway)
        prods <- prod(diag(twoway)) - prod(diag(way2))
        prod_totals <- prod(rowSums(twoway)) * prod(colSums(twoway))
        chi_y <- round((total *  (abs(prods) - (total / 2)) ^ 2) / prod_totals, 4)
        sig_y <- round(pchisq(chi_y, 1, lower.tail = F), 4)
        
        # mantel haenszel chi square
        num <- twoway[1] - ((rowSums(twoway)[1] * colSums(twoway)[1]) / total)
        den <- prod_totals / ((total ^ 3) - (total ^ 2))
        chimh <- round((num ^ 2) / den, 4)
        sig_mh <- round(pchisq(chimh, 1, lower.tail = F), 4)

    } else {

        twoway <- matrix(table(x, y), nrow = dk[1])
        mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = dk[1])
        mat2 <- matrix(colSums(twoway), ncol = dk[2])
        ef <- mat1 %*% mat2
        chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
        df <- (nrow(twoway) - 1) *  (ncol(twoway) - 1)
        sig <- round(pchisq(chi, df, lower.tail = F), 4)

        # likelihood ratio chi square
        chilr <- round(2 * sum(matrix(twoway, ncol = ds) %*% matrix(log(twoway / ef), nrow = ds)), 4)
        sig_lr <- round(pchisq(chilr, df, lower.tail = F), 4)

    }

    # # pearson chi square
    # twoway <- matrix(table(x, y), nrow = 2)
    # mat1 <- matrix(rowSums(twoway) / sum(twoway), nrow = 2)
    # mat2 <- matrix(colSums(twoway), nrow = 1)
    # ef <- mat1 %*% mat2
    # chi <- round(sum(((twoway - ef) ^ 2) / ef), 4)
    # df <- (nrow(twoway) - 1) *  (ncol(twoway) - 1)
    # sig <- round(pchisq(chi, df, lower.tail = F), 4)
    
    # # likelihood ratio chi square
    # chilr <- round(2 * sum(matrix(log(twoway / ef), nrow = 1) %*% matrix(twoway, nrow = 4)), 4)
    # sig_lr <- round(pchisq(chilr, df, lower.tail = F), 4)
    
    # # Yates continuity correction
    # way2 <- twoway[, c(2, 1)]
    # total <- sum(twoway)
    # prods <- prod(diag(twoway)) - prod(diag(way2))
    # prod_totals <- prod(rowSums(twoway)) * prod(colSums(twoway))
    # chi_y <- round((total *  (abs(prods) - (total / 2)) ^ 2) / prod_totals, 4)
    # sig_y <- round(pchisq(chi_y, 1, lower.tail = F), 4)
    
    # # mantel haenszel chi square
    # num <- twoway[1] - ((rowSums(twoway)[1] * colSums(twoway)[1]) / total)
    # den <- prod_totals / ((total ^ 3) - (total ^ 2))
    # chimh <- round((num ^ 2) / den, 4)
    # sig_mh <- round(pchisq(chimh, 1, lower.tail = F), 4)
    
    
    # phi coefficient
    phi <- round(sqrt(chi / total), 4)
    
    # contingency coefficient
    cc <- round(sqrt(chi / (chi + total)), 4)
    
    # cramer's v
    q <- min(nrow(twoway), ncol(twoway))
    cv <- round(sqrt(chi / (total * (q - 1))), 4)
    
    # format output
    # print(cross_table(x, y))
    # cat("\n\n")

    width1 <- nchar('Likelihood Ratio Chi-Square')
    width2 <- max(nchar(df))
    width3 <- max(nchar(chi), nchar(chilr), nchar(chimh), nchar(chi_y), nchar(phi), 
        nchar(cc), nchar(cv))
    width4 <- 6
    widthn <- sum(width1, width2, width3, width4, 12)

    if (ds == 4) {

        cat(format('Chi Square Statistics', width = widthn, justify = 'centre'), "\n\n")
        cat(format('Statistics', width = width1, justify = 'left'), formats(), format('DF', width = width2, justify = 'centre'), formats(), 
            format('Value', width = width3, justify = 'centre'), formats(), format('Prob', width = width4, justify = 'centre'), "\n", sep = '')
        cat(rep('-', widthn), sep = '', '\n')
        cat(format('Chi-Square', width = width1, justify = 'left'), formats(), format(df, width = width2, justify = 'centre'), formats(), 
            format(chi, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(sig, width = width4, justify = 'right', nsmall = 4, scientific = F), "\n", sep = '')
        cat(format('Likelihood Ratio Chi-Square', width = width1, justify = 'left'), formats(), format(df, width = width2, justify = 'centre'), formats(), 
            format(chilr, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(sig_lr, width = width4, justify = 'right', nsmall = 4, scientific = F), "\n", sep = '')
        cat(format('Continuity Adj. Chi-Square', width = width1, justify = 'left'), formats(), format(df, width = width2, justify = 'right'), formats(), 
            format(chi_y, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(sig_y, width = width4, justify = 'right', nsmall = 4, scientific = F), "\n", sep = '')
        cat(format('Mantel-Haenszel Chi-Square', width = width1, justify = 'left'), formats(), format(df, width = width2, justify = 'right'), formats(), 
            format(chimh, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(sig_mh, width = width4, justify = 'right', nsmall = 4, scientific = F), "\n", sep = '')
        cat(format('Phi Coefficient', width = width1, justify = 'left'), formats(), format(' ', width = width2, justify = 'right'), formats(), 
            format(phi, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(' ', width = width4, justify = 'right'), "\n", sep = '')
        cat(format('Contingency Coefficient', width = width1, justify = 'left'), formats(), format(' ', width = width2, justify = 'right'), formats(), 
            format(cc, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(' ', width = width4, justify = 'right'), "\n", sep = '')
        cat(format("Cramer's V", width = width1, justify = 'left'), formats(), format(' ', width = width2, justify = 'right'), formats(), 
            format(cv, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(' ', width = width4, justify = 'right'), "\n", sep = '')
        cat(rep('-', widthn), sep = '', '\n')

    } else {

        cat(format('Chi Square Statistics', width = widthn, justify = 'centre'), "\n\n")
        cat(format('Statistics', width = width1, justify = 'left'), formats(), format('DF', width = width2, justify = 'centre'), formats(), 
            format('Value', width = width3, justify = 'centre'), formats(), format('Prob', width = width4, justify = 'centre'), "\n", sep = '')
        cat(rep('-', widthn), sep = '', '\n')
        cat(format('Chi-Square', width = width1, justify = 'left'), formats(), format(df, width = width2, justify = 'centre'), formats(), 
            format(chi, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(sig, width = width4, justify = 'right', nsmall = 4, scientific = F), "\n", sep = '')
        cat(format('Likelihood Ratio Chi-Square', width = width1, justify = 'left'), formats(), format(df, width = width2, justify = 'centre'), formats(), 
            format(chilr, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(sig_lr, width = width4, justify = 'right', nsmall = 4, scientific = F), "\n", sep = '')
        cat(format('Phi Coefficient', width = width1, justify = 'left'), formats(), format(' ', width = width2, justify = 'right'), formats(), 
            format(phi, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(' ', width = width4, justify = 'right'), "\n", sep = '')
        cat(format('Contingency Coefficient', width = width1, justify = 'left'), formats(), format(' ', width = width2, justify = 'right'), formats(), 
            format(cc, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(' ', width = width4, justify = 'right'), "\n", sep = '')
        cat(format("Cramer's V", width = width1, justify = 'left'), formats(), format(' ', width = width2, justify = 'right'), formats(), 
            format(cv, width = width3, justify = 'centre', nsmall = 4, scientific = F), formats(), format(' ', width = width4, justify = 'right'), "\n", sep = '')
        cat(rep('-', widthn), sep = '', '\n')

    }
    
}

















