observe({
  updateSelectInput(session,inputId = "var_tsproptest1",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_tsproptestg1",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_tsproptest2",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_tsproptestg2",
    choices = names(data()), selected = '')
})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_tsproptest1", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptest2", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptestg1", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptestg2", choices = names(fdata))
    } else {
          updateSelectInput(session, inputId = "var_tsproptest1", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptest2", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptestg1", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptestg2", choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_tsproptest1", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptest2", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptestg1", choices = names(fdata))
        updateSelectInput(session, inputId = "var_tsproptestg2", choices = names(fdata))
    } else {
          updateSelectInput(session, inputId = "var_tsproptest1", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptest2", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptestg1", choices = names(f_data))
          updateSelectInput(session, inputId = "var_tsproptestg2", choices = names(f_data))
        }
})

d_tsproptest <- eventReactive(input$submit_tsproptest, {
  req(input$var_tsproptest1)
  req(input$var_tsproptest2)
	# validate(need((input$var_tsproptest1 != '' & input$var_tsproptest2 != ''), 'Please select variable.'))
  data <- final_split$train[, c(input$var_tsproptest1, input$var_tsproptest2)]
  out <- infer_ts_prop_test(data[, 1], data[, 2], input$tsproptest_type)
  out
})

d_tsproptestg <- eventReactive(input$submit_tsproptestg, {
  req(input$var_tsproptestg1)
  req(input$var_tsproptestg2)
	# validate(need((input$var_tsproptestg1 != '' & input$var_tsproptestg2 != ''), 'Please select variable.'))
  data <- final_split$train[, c(input$var_tsproptestg1, input$var_tsproptestg2)]
  out <- infer_ts_prop_grp(data[, 1], data[, 2], input$tsproptestg_type)
  out
})

output$tsproptest_out <- renderPrint({
    d_tsproptest()
})

output$tsproptestg_out <- renderPrint({
  d_tsproptestg()
  # validate(need(nlevels(d_tsproptestg()[, 2]) == 2, 'Please select a binary variable.'))
  # ts_prop_grp(d_tsproptestg()[, 1], d_tsproptestg()[, 2], input$tsproptestg_type)
  # if (nlevels(d_tsproptestg()[, 2]) > 2) {
  #   stop('Select a dichotomous variable.')
  # } else {
  #   ts_prop_grp(d_tsproptestg()[, 1], d_tsproptestg()[, 2], input$tsproptestg_type)
  # }
})

tspropcalc <- eventReactive(input$submit_tspropcalc, {
  infer_ts_prop_calc(input$n1_tspropcalc, input$n2_tspropcalc, input$prop_tspropcalc1,
      input$prop_tspropcalc2, input$tspropcalc_type)
})

output$tspropcalc_out <- renderPrint({
  tspropcalc()
})
