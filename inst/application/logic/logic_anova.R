observe({
    updateSelectInput(session,
                      inputId = "var_anova1",
                      choices = names(data()),
                      selected = '')
    updateSelectInput(session,
      inputId = "var_anova2",
      choices = names(data()),
      selected = '')

})

observeEvent(input$finalok, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_anova2",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_anova2', choices = names(f_data))
        }
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_anova1',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_anova1',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_anova1', choices = names(num_data))
        }
})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(f_data))) {
        k <- final_split$train %>% map(is.factor) %>% unlist()
        j <- names(which(k == TRUE))
        fdata <- tibble::as_data_frame(f_data)
        colnames(fdata) <- j
        updateSelectInput(session, inputId = "var_anova2",
            choices = names(fdata))
        } else {
          updateSelectInput(session, 'var_anova2', choices = names(f_data))
        }
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_anova1',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_anova1',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_anova1', choices = names(num_data))
        }
})

d_anova <- eventReactive(input$submit_anova, {
	# validate(need((input$var_anova1 != '' & input$var_anova2 != ''), 'Please select two variables.'))
  req(input$var_anova1)
  req(input$var_anova2)
    data <- final_split$train[, c(input$var_anova1, input$var_anova2)]
    eval(parse(text = paste0("data$", names(data)[2], " <- as.numeric(as.character(data$", names(data)[2], "))")))
    # data
    k <- inferr::infer_oneway_anova(data, as.character(input$var_anova1), as.character(input$var_anova2))  
    k
})

output$anova_out <- renderPrint({
    # inferr::owanova(d_anova(), as.character(input$var_anova1), as.character(input$var_anova2))  
    d_anova()
})
