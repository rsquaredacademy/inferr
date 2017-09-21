source('helper/t-test-shiny.R')

observe({
    updateSelectInput(session,
                      inputId = "var_ttest",
                      choices = names(data()),
                      selected = '')

})

observeEvent(input$finalok, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, inputId = "var_ttest",
              choices = names(numdata), selected = names(numdata))
            updateNumericInput(session = session,
                      inputId = 'ttest_mu',
                      value = mean(numdata))
    } else if (ncol(num_data) < 1) {
             updateSelectInput(session, inputId = "var_ttest",
              choices = '', selected = '')
             updateNumericInput(session = session,
                      inputId = 'ttest_mu',
                      value = '')
    } else {
             updateSelectInput(session, inputId = "var_ttest", 
              choices = names(num_data))
             updateNumericInput(session = session,
                      inputId = 'ttest_mu',
                      value = mean(num_data))
    }

})

observeEvent(input$submit_part_train_per, {
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, inputId = "var_ttest",
              choices = names(numdata), selected = names(numdata))
            updateNumericInput(session = session,
                      inputId = 'ttest_mu',
                      value = mean(numdata))
    } else if (ncol(num_data) < 1) {
             updateSelectInput(session, inputId = "var_ttest",
              choices = '', selected = '')
             updateNumericInput(session = session,
                      inputId = 'ttest_mu',
                      value = '')
    } else {
             updateSelectInput(session, inputId = "var_ttest", 
              choices = names(num_data))
             updateNumericInput(session = session,
                      inputId = 'ttest_mu',
                      value = mean(num_data))
    }

})

d_ttest <- eventReactive(input$submit_ttest, {
	# validate(need(input$var_ttest != '', 'Please select a variable.'))
  req(input$ttest_mu)
  data <- final_split$train
  k <- ttest_shiny(data, as.character(input$var_ttest), as.numeric(input$ttest_mu), input$ttest_alpha, input$ttest_type)
  k
})


output$ttest_out <- renderPrint({
  d_ttest()
})
