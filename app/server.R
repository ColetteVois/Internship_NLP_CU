#Server function that implements the back-end 

server <- function(input, output, session){
  
  
#################################################################  DATA  ###############################################################
  
  d_num <- reactive({
    head(subset(original_books_bis, rowname >= input$num_offset_data), input$num_word_data)
  })
  
  # use the key aesthetic/argument to help uniquely identify selected observations
  key_first <- row.names(original_books_bis)
  
  
  
  #Almost all Data that will be given to the analysis part as main data. It depends on the way to choose it (select, numeric input, checkbox). It is firstly done without the checkbox group, then just below, it is done with it.
  original_books_selected_av <- reactive({
    if(input$all == TRUE){
      original_books_bis
    }
    else if(input$all == FALSE){
      if(input$num_check== TRUE){
        d_num()
      }
      else if(input$num_check == FALSE){
        SharedData$new(original_books_bis, ~key_first)
      }
    }
  })
  original_books_sel_use_av <- reactive({
    if(input$all == TRUE){
      original_books_bis
    }
    else if(input$all == FALSE){
      if(input$num_check==TRUE){
        d_num()
      }
      else if(input$num_check ==FALSE){
        original_books_bis[original_books_selected()$selection(),]
      }
    }
    })
  
  #This is the data created by the checkboxgroup
  d_books <- reactive({
    local_data <- data.frame()
    for(b_id in input$book){
      #num_book_input is the number of the book. gsub find the number by removing "Book" from "Booki" and then strtoi converts that to an integer to subset the data
      id_book_input <- gsub("Book", "",b_id)
      #Subesting the data d and then adding this subset to the local data with all books selected
      local_data <- rbind(local_data, subset(original_books_bis, book == id_book_input))
    }
    local_data
  })
  
  
  #All Data that will be given to the analysis part as main data
  original_books_sel_use <- reactive({
    if(length(input$book)){
      d_books()
    }
    else if(!length(input$book)){
      original_books_sel_use_av()
    }
  })
  
  original_books_selected <- reactive({
    if(length(input$book)){
      d_books()
    }
    else if(!length(input$book)){
      original_books_selected_av()
    }
  })
  
  #Creating a new original_books_selected in order to print the text in the app
  original_books_selected_print <- reactive({head(noquote(original_books_selected()$text), 50)})
  
  # use the key aesthetic/argument to help uniquely identify selected observations
  key <- reactive({row.names(original_books_sel_use())})
  
  #length of the selected data with the numeric conditions
  l <- reactive({NROW(original_books_sel_use())})
  
  
  #############################################################  Pre Processing Overview  ######################################################
  
  #Plotting the data in the overview of the preprocessing. A lot of if to cover all the cases. It isn't really possible by plotting the data that will be shared because of the key. I didn't manage to make it work that way so I used a ot of if..
    output$plot_data <- renderPlotly({
      s <- input$rows_selected
      if(input$all==TRUE){
        plot_ly(original_books_bis, x = ~rowname, y = rep(1, n), key = ~key_first, type = 'scatter',source = "select", mode='lines+markers',  color = ~book )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
      }
      else if(input$num_check==TRUE){
        plot_ly(d_num(), x = ~rowname, y = rep(1, NROW(d_num())), key = ~row.names(d_num()), type = 'scatter',source = "select", mode='lines+markers', color = ~book )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
      }
      else if(length(input$book)){
        plot_ly(d_books(), x = ~rowname, y = rep(1, NROW(d_books())), key = ~row.names(d_books()), type = 'scatter',source = "select", mode='lines+markers',color = ~book  )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
      }
      else{
        if(!length(s)){
          plot_ly(original_books_selected(), x = ~rowname, y = rep(1, n), key = ~key_first, type = 'scatter',source = "select", mode='lines+markers',color = ~book  )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")%>% highlight("plotly_selected", 'plotly_deselect',  defaultValues = s,color = I('green'))      
        }
        else if(length(s)){
          plot_ly(original_books_bis, x = ~rowname, y = rep(1, n), key = ~key_first, type = 'scatter',source = "select", mode='lines+markers',color = ~book  )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
        }
      }
    })
    #Deselect the other checkboxs when one is selected
    observeEvent(input$all, {
      if(input$all == TRUE){
        updateCheckboxInput(session, "num_check", value = FALSE)
        updateCheckboxGroupInput(session, "book", selected = character(0))
      }
    })
    
    observeEvent(input$num_check, {
      if(input$num_check == TRUE){
        updateCheckboxInput(session, "all", value = FALSE)
        updateCheckboxGroupInput(session, "book", selected = character(0))
      }
    })
    
    observeEvent(input$book, {
      if(length(input$book)){
        updateCheckboxInput(session, "all", value = FALSE)
        updateCheckboxInput(session, "num_check", value = FALSE)
      }
    })
    
  #Deselecting the check boxes when selecting the plot
     observeEvent(event_data("plotly_selected", source = "select"), {
       updateCheckboxInput(session, "all", value = FALSE)
       updateCheckboxInput(session, "num_check", value = FALSE)
       updateCheckboxGroupInput(session, "book", selected = character(0))
     })
     
  #Printing the number of Lines and the maximum number of Lines 
  output$num_data <- renderUI({
    tagList(renderText({"The number of lines of the input file is"}),
      renderPrint({n}),
      renderText({"and the maximum number of lines you can currently choose is"}),
      renderPrint({n-input$num_offset_data+1}),
      if(input$num_word_data > n-input$num_offset_data+1){
        renderText("You have chosen a number of lines that is too high, it will just pick every line after the chosen offset")
      },
      if(n_act()>=1){
        renderText({"Here are the first 50 lines of the book you selected"})
        renderPrint({original_books_selected_print()})
      }
      )
    })
  
  ################################################################################## Filter Pre Processing  ################################################
  
  #Doing the filter page from the pre processing
  #Creating the data for the boxplots
  lien <- paste(my_path,"/Intership_NLP_CU/token_boxplot.R", sep="")
  source(lien)
  d_token_boxplot <- reactive({token.boxplot(original_books_selected())})
  d_boxplot_1 <- reactive({data.frame(token_sentence_col = unlist(d_token_boxplot()[1]), token_word_ocu_col = d_token_boxplot()[2], token_word_type_col = d_token_boxplot()[3])})
  d_boxplot_2 <- reactive({data.frame(token_word_ocu_col = unlist(d_token_boxplot()[2]))})
  d_boxplot_3 <- reactive({data.frame(token_word_type_col = unlist(d_token_boxplot()[3]))})
  
  #Doing the boxplots
  
  output$box_1 <- renderPlotly({
    plot_ly(d_boxplot_1(),x = rep(0, length(d_boxplot_1()$token_sentence_col)), y=~token_sentence_col, type = "scatter", source = "box1", mode='markers')%>%add_trace(d_boxplot_1(), y=~token_sentence_col, type = "box")%>%layout(title = 'Box plot of the sentence tokenization', yaxis =list(title ='Number of sentences'), titlefont = 'arial', showlegend = FALSE)
                                                                                                                                                      #hoverinfo = 'text', text =~paste("Maximum:", fivenum(test_d)[5], "Q3:", fivenum(test_d)[4]), marker = list(outliercolor = "red"))
  })
  output$box_2 <- renderPlotly({
    plot_ly(d_boxplot_2(),x = rep(0, length(d_boxplot_2()$token_word_ocu_col)), y=~token_word_ocu_col, type = "scatter", source = "box2", mode='markers')%>%add_trace(d_boxplot_2(), y=~token_word_ocu_col, type = "box",  marker = list(outliercolor = "red"))%>%layout(title = 'Box plot of the word tokenization', yaxis =list(title ='Number of words'), titlefont = 'arial', showlegend = FALSE)
  })
  output$box_3 <- renderPlotly({
    plot_ly(d_boxplot_3(),x = rep(0, length(d_boxplot_3()$token_word_type_col)), y=~token_word_type_col, type = "scatter", source = "box3", mode='markers')%>%add_trace(d_boxplot_3(), y=~token_word_type_col, type = "box",  marker = list(outliercolor = "red"))%>%layout(title = 'Box plot of the word type tokenization', yaxis =list(title ='Number of different words'), titlefont = 'arial', showlegend = FALSE)
  })
  output$box_4 <- renderPlotly({
    plot_ly(test_d,x = rep(0, length(test)), y=~test, type = "scatter", source = "box4", mode='markers')%>%add_trace(test_d, y=~test, type = "box",  marker = list(outliercolor = "red"))
  })
  output$box_5 <- renderPlotly({
    plot_ly(test_d,x = rep(0, length(test)), y=~test, type = "scatter", source = "box5", mode='markers')%>%add_trace(test_d, y=~test, type = "box",  marker = list(outliercolor = "red"))
  })
  output$description_token <- renderUI({
    d1 <- event_data("plotly_hover", source = "box1")
    d2 <- event_data("plotly_hover", source = "box2")
    d3 <- event_data("plotly_hover", source = "box3")
    d4 <- event_data("plotly_hover", source = "box4")
    d5 <- event_data("plotly_hover", source = "box5")
    
    n1 <- d1$pointNumber
    n2 <- d2$pointNumber
    n3 <- d3$pointNumber
    n4 <- d4$pointNumber
    n5 <- d5$pointNumber
    
    #The if is a little bit twisted, firstly we need to take away the points of the boxplot which are of length greater than 1 and also we need to avoid having 
    #length = 0. Then we want to do a OR. Indeed, if we hover the point in one of the five boxplots, we want its description. If we do directly an OR, that doesn't work because we don't know all the values yet,
    #so the condition will give NA, which an IF can't take. So we take away this by doing !is.na, an then we do the or we wanted to do in the first place. 
    tagList(
      renderPrint({d1}),
      if(length(n1) == 1 | length(n2) == 1 |length(n3) == 1 |length(n4) == 1 |length(n5) == 1){
        if(!is.na(n1==0 ||n2==0 ||n3==0||n4==0||n5==0)){
          if(n1==0 ||n2==0 ||n3==0||n4==0||n5==0){
            renderText("This tokenisation is based on this package. It does that and this and is better for the sentences.")
          }
        }
        else if(!is.na(n1==1 ||n2==1||n3==1||n4==1||n5==1)){
          if(n1==1 ||n2==1||n3==1||n4==1||n5==1){
          renderText("This tokenisation is based on this package. It does that and this and is better for the words.")
          }
      }
        else if(!is.na(n1==2 ||n2==2||n3==2||n4==2||n5==2)){
          if(n1==2 ||n2==2||n3==2||n4==2||n5==2){
          renderText("This tokenisation is based on this package. It does that and this and is better for the books.")
          }
        }
      }
    )
  })
  
  #Choosing the tokenization and updating the value of the tokenization selected
  #Choice of the tokenization that mixes wether or not the choice was made before or later
  token_sentence_radio_button <- reactive({
    if(input$choice_token_moment == "Now"){
      input$token_sentence_radio_button_now 
    }
    else if(input$choice_token_moment == "Later"){
      input$token_sentence_radio_button_later 
    }
  })
  
  token_word_radio_button <- reactive({
    if(input$choice_token_moment == "Now"){
      input$token_word_radio_button_now 
    }
    else if(input$choice_token_moment == "Later"){
      input$token_word_radio_button_later 
    }
  })
  
  #Warning if Now chosen
  
  output$warning_choose_before <- renderUI({
    tagList(
      if(input$choice_token_moment == "Now"){
        renderText("You have chosen to choose the tokenization at the beginning of the app. So what you will choose here will have no effect on the tokenization used for the analysis. If you want to choose here, you need to go back to the first page (Data) and choose 'Later'")
      }
    )
  })
  
  #Creating the data with the chosen tokenization
  # id_token_sentence_selected <- reactive({strtoi(gsub("TokenizationSentence", "",token_sentence_radio_button()))})
  # id_token_word_selected <- reactive({strtoi(gsub("TokenizationWord", "",token_word_radio_button()))})
  # tokenizer.sentence.i <- reactive({sprintf("tokenizer.sentence.%d(original_books_sel_use())", id_token_sentence_selected())})
  # original_books_tokenized_sentence <- reactive({eval(parse(text=tokenizer.sentence.i()))})
  # tokenizer.word.i <- reactive({sprintf("tokenizer.word.%d(original_books_tokenized_sentence())", id_token_word_selected())})
  # 
  # nb_of_sentences <- reactive({dim(original_books_tokenized_sentence()[1])})
  # original_books_tokenized <- reactive({
  #   original_books_tokenized <- c()
  #   for(k in 1:nb_of_sentences()[1]) {
  #     new_original_books_tokenized <- eval(parse(text=tokenizer.word.i()))
  #     original_books_tokenized <- dplyr::bind_rows(original_books_tokenized,new_original_books_tokenized)
  #   }
  #   original_books_tokenized
  # })
  
  ########################################################################## DATA  ###########################################################
  
  #Shared data between the plot and the datatable of the overview and the wordcloud for the analysis
  d_shared <- reactive({SharedData$new(original_books_sel_use(), ~key())})
  
  ######################################################################### Overview Analysis  ####################################################
  
  #Plotting the scatterplot with plotly
  output$plot_overview <- renderPlotly({
    #s matches the row selected by the user
    s <- input$plot_rows_selected
    #The if for the length doesn't seem very useful, because it works without it, however, I found it on the internet and there is maybe a reason I haven't found yet, so I prefer to let for now.
    #if there are no row selected yet, you can highlight the plot by selecting some points
    if(!length(s)){
      if(input$choice=='Frequency'){
        plot_ly(d_shared(), x = ~rowname, y = ~freq, key = ~key(), type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect',  defaultValues = s,color = I('green'))
      }
      else if(input$choice=='Random'){
        plot_ly(d_shared(), x = ~rowname, y = ~random, key = ~key(), type = 'scatter', mode='markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Random according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Random'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect', defaultValues = s, color = I('green'))
        
      }
    }
    #If there are row selected, you can't higlight the plot because it is already highlighted 
    else if(length(s)){
      if(input$choice=='Frequency'){
        plot_ly(original_books_sel_use(), x = ~rowname, y = ~freq, key = ~key(), type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)
      }
      else if(input$choice=='Random'){
        plot_ly(original_books_sel_use(), x = ~rowname, y = ~random, key = ~key(), type = 'scatter', mode='markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Random according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Random'), titlefont = 'arial', showlegend = FALSE)      
      }
    }
  })
  
  #Plotting the Data Table
  output$table_overview <- DT::renderDataTable({
    #Choosing the data selected in the plot. It is done by crosstalk, see CRAN R Crosstalk SharedData for more details
    dsel <- original_books_sel_use()[d_shared()$selection(),]
    #Creating the data table with the initial data
    dt <-DT::datatable(original_books_sel_use(),options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),pageLength = 5, lengthMenu = c(5, 10, 15, 20)),class = 'display')
    #This condition is whether a data is selected on the plot
    if (NROW(dsel) == 0) {
      dt
    } else {
      #If a data is selected, then we change the style of the table in order to highlight the selected rows, which are dsel$rowname
      DT::formatStyle(dt, "rowname", target = "row",
                      color = DT::styleEqual(dsel$rowname, rep("white", length(dsel$rowname))), backgroundColor = DT::styleEqual(dsel$rowname, rep("black", length(dsel$rowname))))
    }
  })
  
  ################################################################################  DATA  ###############################################
  
  #Choosing the data which is shared with the plot
  d_real_shared <- reactive({original_books_sel_use()[d_shared()$selection(),]})
  #Choosing the data to give to the wordcloud, depending on which data is taken from plotly
  filter_d <- reactive({
    #Changing the data in order to match what the wordcloud takes as an input
    d_prime_reac <- reactive({data.frame(d_real_shared()$word, d_real_shared()$freq)})
    head(subset(d_prime_reac(), d_real_shared...freq <= input$slide_value_freq[2] & d_real_shared...freq >= 
                  input$slide_value_freq[1]), 
         input$slide_value_word)
  })
   
  ################################################################################ Filter Analysis  #########################################################
  
  #Updating the value of the maximum of the slider input for the number of words and for the frequency
  m_act <- reactive({max(original_books_sel_use()$freq)})
  n_act <- reactive({NROW(original_books_sel_use())})
  observeEvent(original_books_sel_use(),{updateSliderInput(session,inputId = "slide_value_freq", label = "Filter the frequency", min = 1, max = m_act(), value = c(1,m_act()), step = 1)})
  observeEvent(original_books_sel_use(),{updateSliderInput(session,inputId = "slide_value_word", label = "Choose the maximum number of words", min = 1, max = n_act(), value = n_act(), step = 1)})
  
  
  #Creating the wordcloud and making it reactive to change in the input values
  output$wordcloud  <- renderWordcloud2(wordcloud2(data = filter_d(),
                                                   shape = 'star', size = 0.8, shuffle =FALSE))
  output$test <- renderPrint({
    filter_d()
  })
  
  ###########################################################################  Report ##############################################################
  
  progress <- reactive({
    capture.output(report, file=NULL)
  })
  

  
  output$report <- downloadHandler(
    filename = function() {
      paste('my_report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      
      withProgress(message ="Generating report", detail =  "it might takes a little while", expr = {tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data_complete = d, data_selected_plot = d[d_shared()$selection(),], 
                     min_freq_wordcloud = input$slide_value_freq[1], max_freq_wordcloud = input$slide_value_freq[2],
                     max_word_wordcloud = input$slide_value_word)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,switch(
        input$format,
        PDF = pdf_document(toc=TRUE), HTML = html_document(toc=TRUE), Word = word_document(toc=TRUE)
      ), output_file = file,
      params = params,
      envir = new.env(parent = globalenv())
      )
      },
      min = 1,
      value = 1
      )
    }
  )
  
  ###############################################################################  Message Menu  ###########################################################
  
  output$warningMenu <- renderMenu({
    # Code to generate each of the messageItems
    war <- list(notificationItem(text = "Everything seems to work", icon("users")))
    l_wc <- reactive({length(filter_d()$d_real_shared...word)})
    if(l_wc()==1){
      list.append(war, notificationItem(
        text = "Only one word is selected on the wordcloud and none appears",
        icon = icon("exclamation-triangle"),
        status = "warning"
      )
      )
    }
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(type = "notifications", .list = war)
  })
  
}
return(server)