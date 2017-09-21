observe({
    updateSelectInput(session, 'var_mcnemar1', choices = names(data()))
    updateSelectInput(session, 'var_mcnemar2', choices = names(data()))
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'var_mcnemar1', choices = names(fdata))
    		updateSelectInput(session, 'var_mcnemar2', choices = names(fdata))
    } else {
          updateSelectInput(session, 'var_mcnemar1', choices = names(f_data))
    			updateSelectInput(session, 'var_mcnemar2', choices = names(f_data))
    }
})


observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, 'var_mcnemar1', choices = names(fdata))
        updateSelectInput(session, 'var_mcnemar2', choices = names(fdata))
    } else {
          updateSelectInput(session, 'var_mcnemar1', choices = names(f_data))
          updateSelectInput(session, 'var_mcnemar2', choices = names(f_data))
    }
})

d_mcnemar <- eventReactive(input$submit_mcnemar, {
	# validate(need((input$var_mcnemar1 != '' & input$var_mcnemar2 != ''), 'Please select variables.'))
  req(input$var_mcnemar1)
  req(input$var_mcnemar2)
  data <- final_split$train[, c(input$var_mcnemar1, input$var_mcnemar2)]
  # validate(need((nlevels(data[, 1]) == 2 & nlevels(data[, 2]) == 2), 'Please select binary variables.'))
  k <- table(data[, 1], data[, 2])
  out <- mcnemar_test(k)
  out
})

output$mcnemar_out <- renderPrint({
  d_mcnemar()
})

result2 <- eventReactive(input$submit_mcnemarc, {
  k <- matrix(c(input$mc_00, input$mc_10, input$mc_01, input$mc_11), nrow = 2)
  out <- mcnemar_test(k)
  out
})

output$mcnemarc_out <- renderPrint({
  result2()
})
