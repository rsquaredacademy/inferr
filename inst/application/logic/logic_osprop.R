observe({
    updateSelectInput(session,
                      inputId = "var_osproptest",
                      choices = names(data()),
                      selected = '')
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_osproptest",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_osproptest', choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_osproptest",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_osproptest', choices = names(f_data))
        }
})


d_osproptest <- eventReactive(input$submit_osproptest, {
  req(input$var_osproptest)
  data <- final_split$train
  # validate(need(nlevels(data) == 2, 'Please select a binary variable.'))
  out <- infer_os_prop_test(data, !! sym(as.character(input$var_osproptest)),
                            input$osproptest_prob,
                            input$osproptest_type)
  out
})

output$osproptest_out <- renderPrint({
  d_osproptest()
})

ospropcalc <- eventReactive(input$submit_ospropcalc, {
  infer_os_prop_test(n = input$n_ospropcalc, phat = as.numeric(input$p_ospropcalc), prob = input$prob_ospropcalc,
      alternative = input$ospropcalc_type)
})

output$ospropcalc_out <- renderPrint({
  ospropcalc()
})



