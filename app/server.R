#Server function that implements the back-end 

server <- function(input, output, session){
  
################################### LOAD DATA ###########################################
#Link of the observed data
link_file_data_uploaded <-  reactive({input$inputdata$datapath})
#Loading the load_data files 
observeEvent(input$data_type_choice, {source(paste(my_path, sprintf("/Intership_NLP_CU-master/load_data/load_data_%d.R", strtoi(input$data_type_choice)), sep = ""))})

#Creating text to show the options
output$or_a_data_text <- renderText("Or a folder")
output$or_data_1_text <- renderText("Or Austen's books")

#Getting the data from the folder but works only for windows yet
root = c(wd='C:/')
shinyDirChoose(input, "inputfolderfile",roots=root)
link_folder_data_uploaded <- reactive({parseDirPath(roots=root, input$inputfolderfile)})

#Getting the link of the uploaded data
link_data_uploaded <- reactive({
  if(is.null(link_file_data_uploaded())){
    link_folder_data_uploaded()
  }
  else{
    link_file_data_uploaded()
  }
})


#Creating the data for the app

original_books <- reactive({
    #According to the user's choice, changing the load_data that will be used.
  if(input$choice_data_1 == TRUE){
    load.data.1 <- 'load.data.1(link_data_uploaded())'
    local_original_books <- eval(parse(text=load.data.1))
  }  
  else if(input$choice_data_1 == FALSE){
    load.data.i <- sprintf('load.data.%d(link_data_uploaded())', strtoi(input$data_type_choice))
    
    if(is.null(link_data_uploaded()) | length(link_data_uploaded())==0){
      local_original_books <- tibble(text = rep("This is not a text, choose a data!", 200))
    }
    else{
      if(strtoi(input$data_type_choice)==1){
        local_original_books <- eval(parse(text=load.data.i))
      }
      else{
        local_original_books <- eval(parse(text=load.data.i))
        
      }
    }
  }
    local_original_books <- local_original_books %>% mutate(rowname = 1:nrow(local_original_books))
    
    if(is.null(local_original_books$book)) {
      local_original_books <- local_original_books %>% mutate(book = "allthesame")
    }
    local_original_books
})

original_books_bis <- reactive({original_books()})
book_column <- reactive({original_books_bis()$book})

#Removing the spaces from the column book and creating the choices for the books
check_choices <- reactive({
  book_unique <- unique(book_column())
  count = 1
  local_book_column <- as.character(book_unique)
  for(i in book_unique){
    local_book_column[count] <-i
    count = count +1
  }
  local_check_choices <- c()
  for(i in book_unique){
    a_paste_local <- paste("Book", i, sep = "" )
    local_check_choices <- c(local_check_choices, a_paste_local)
  }
  local_check_choices
})


#Updating the radio button for the books
observeEvent(check_choices(),{updateCheckboxGroupInput(session, "book", "Choose one or more book(s)",
                         check_choices(), inline = TRUE )
})

  
################################################################  DATA Tab  ###########################################################


lien <- paste(my_path,"/Intership_NLP_CU-master/description/description_type_data.R", sep="")
source(lien)
  
output$description_type_data_possible_analyzed <- renderUI({
  tagList(
    renderPrint({cat(noquote(load_data_type_description),sep="\n")}),
    renderText({"File/Folder location:"}),
    renderPrint({link_data_uploaded()}),
    renderText({"File/Folder name:"}),
    renderPrint({input$inputdata$name})
  )
})
  
#################################################################  DATA  ###############################################################
  
  d_num <- reactive({
    head(subset(original_books_bis(), rowname >= input$num_offset_data), input$num_word_data)
  })
  
  # use the key aesthetic/argument to help uniquely identify selected observations
  key_first <- reactive({row.names(original_books_bis())})
  
  
  
  #Almost all Data that will be given to the analysis part as main data. It depends on the way to choose it (select, numeric input, checkbox). It is firstly done without the checkbox group, then just below, it is done with it.
  original_books_selected_av <- reactive({
    if(input$all == TRUE){
      original_books_bis()
    }
    else if(input$all == FALSE){
      if(input$num_check== TRUE){
        d_num()
      }
      else if(input$num_check == FALSE){
        SharedData$new(original_books_bis(), ~key_first())
      }
    }
  })
  original_books_selected_used_av <- reactive({
    if(input$all == TRUE){
      original_books_bis()
    }
    else if(input$all == FALSE){
      if(input$num_check==TRUE){
        d_num()
      }
      else if(input$num_check ==FALSE){
        original_books_bis()[original_books_selected()$selection(),]
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
      local_data <- rbind(local_data, subset(original_books_bis(), book == id_book_input))
    }
    local_data
  })
  
  
  #All Data that will be given to the analysis part as main data
  original_books_selected_used <- reactive({
    if(length(input$book)){
      d_books()
    }
    else if(!length(input$book)){
      original_books_selected_used_av()
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
  original_books_selected_print <- reactive({head(noquote(original_books_selected_used()$text), 50)})
  
  
  
  #length of the selected data with the numeric conditions
  l <- reactive({NROW(original_books_selected_used())})
  
  
  #############################################################  Pre Processing Overview  ######################################################
  
  #Plotting the data in the overview of the preprocessing. A lot of if to cover all the cases. It isn't really possible by plotting the data that will be shared because of the key. I didn't manage to make it work that way so I used a ot of if..
    output$plot_data <- renderPlotly({
      s <- input$rows_selected
      if(input$all==TRUE){
        plot_ly(original_books_bis(), x = ~rowname, y = rep(1, NROW(original_books_bis())), key = ~key_first(), type = 'scatter',source = "select", mode='lines+markers',  color = ~book )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
      }
      else if(input$num_check==TRUE){
        plot_ly(d_num(), x = ~rowname, y = rep(1, NROW(d_num())), key = ~row.names(d_num()), type = 'scatter',source = "select", mode='lines+markers', color = ~book )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
      }
      else if(length(input$book)){
        plot_ly(d_books(), x = ~rowname, y = rep(1, NROW(d_books())), key = ~row.names(d_books()), type = 'scatter',source = "select", mode='lines+markers',color = ~book  )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
      }
      # else if((input$all==FALSE)){
      #   plot_ly(original_books_bis(), x = ~rowname, y = rep(1, n), key = ~key_first(), type = 'scatter',source = "select", mode='lines+markers',color = ~book  )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
      # }
      else{
        if(!length(s)){
          plot_ly(original_books_selected(), x = ~rowname, y = rep(1, NROW(original_books_bis())), key = ~key_first(), type = 'scatter',source = "select", mode='lines+markers',color = ~book  )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")%>% highlight("plotly_selected", 'plotly_deselect',  defaultValues = s,color = I('green'))      
        }
        else if(length(s)){
          plot_ly(original_books_bis(), x = ~rowname, y = rep(1, NROW(original_books_bis())), key = ~key_first(), type = 'scatter',source = "select", mode='lines+markers',color = ~book  )%>%layout(title = 'Data plot', xaxis = list(title ='Line'), titlefont = 'arial', dragmode = "select")
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
     
  #Updating the initial values of the numeric input
  observeEvent(NROW(original_books_bis()),{
    updateNumericInput(session, inputId = "num_offset_data", label = "Choose the number of the first line", min = 1, max = NROW(original_books_bis()), value = 1)
    updateNumericInput(session, inputId = "num_word_data", label = "Choose the number of lines follwing the offset", min = 1, max = NROW(original_books_bis()), value = NROW(original_books_bis()))
  })  
  #Printing the number of Lines and the maximum number of Lines 
  output$num_data <- renderUI({
    tagList(renderText({
      paste("The number of lines of the input file is", NROW(original_books_bis()), "and the maximum number of lines you can currently choose is", NROW(original_books_bis())-input$num_offset_data+1, ".")})
      )
    })
  
  output$num_data_highlighted <- renderText({
    if(input$num_word_data > NROW(original_books_bis())-input$num_offset_data+1){
      "You have chosen a number of lines that is too high, it will just pick every line after the chosen offset."
    }
  })
  n_lines_active <- reactive({NROW(original_books_selected_used())})
  output$num_data_text_display <- renderUI({
    tagList(
      if(n_lines_active()>=1){
        renderText({"Here are the first 50 lines of the book you selected:"})
      },
      tags$br(),
      if(n_lines_active()>=1){
    renderPrint({cat(original_books_selected_print(),sep="\n")})
      }
    )})
  
  ################################################################################## Filter Pre Processing  ################################################
  
  #Doing the filter page from the pre processing
  #Creating the data for the boxplots
  lien <- paste(my_path,"/Intership_NLP_CU-master/backend_analysis/token_boxplot.R", sep="")
  source(lien)
  d_token_boxplot <- reactive({token.boxplot(original_books_selected_used())})
  d_boxplot_1 <- reactive({data.frame(token_sentence_col = unlist(d_token_boxplot()[1]))})
  d_boxplot_2 <- reactive({data.frame(token_word_ocu_col = unlist(d_token_boxplot()[2]))})
  d_boxplot_3 <- reactive({data.frame(token_word_type_col = unlist(d_token_boxplot()[3]))})
  d_boxplot_4 <- reactive({data.frame(token_ratio_col = unlist(d_token_boxplot()[2])/unlist(d_token_boxplot()[3]))})
  d_boxplot_5 <- reactive({data.frame(token_normalization = unlist(d_token_boxplot()[4]))})
  
  #Creating the keys for the boxplot to uniquely identify the tokenizations
  # key_1_1 <- reactive({row.names(d_boxplot_1())[-strtoi(token_sentence_radio_button())]})
  # key_1_2 <- reactive({row.names(d_boxplot_1())[strtoi(token_sentence_radio_button())]})
  key_1 <- reactive({row.names(d_boxplot_1())})
  key_2 <- reactive({row.names(d_boxplot_2())})
  key_3 <- reactive({row.names(d_boxplot_3())})
  key_4 <- reactive({row.names(d_boxplot_4())})
  key_5 <- reactive({row.names(d_boxplot_5())})
  
  #Creating a random data to avoid point superposition in the plots of the boxplots
  random_data_avoid_superposition_1 <- reactive({rnorm(length(d_boxplot_1()$token_sentence_col))*0.05})
  random_data_avoid_superposition_2 <- reactive({rnorm(length(d_boxplot_2()$token_word_ocu_col))*0.05})
  random_data_avoid_superposition_3 <- reactive({rnorm(length(d_boxplot_5()$token_normalization))*0.05})
  
  #Doing the boxplots
  
  output$box_1 <- renderPlotly({
    plot_ly(d_boxplot_1(),x = random_data_avoid_superposition_1(), y=~token_sentence_col, key=~key_1(), type = "scatter", mode='markers', source = "box1", marker =list(color="blue"))%>%
      add_trace(d_boxplot_1(), x=0, y=~token_sentence_col, type = "box",  marker = list(outliercolor = "red"))%>%layout(title = 'Box plot of the sentence tokenization', yaxis =list(title ='Number of sentences'), titlefont = 'arial', showlegend = FALSE)
    #hoverinfo = 'text', text =~paste("Maximum:", fivenum(test_d)[5], "Q3:", fivenum(test_d)[4]), marker = list(outliercolor = "red"))
    # add_trace(x = 0, y=~token_sentence_col[strtoi(token_sentence_radio_button())], key  =~ key_1_2(), marker = list(color="yellow"))%>%  
  })
  output$box_2 <- renderPlotly({
    plot_ly(d_boxplot_2(),x = random_data_avoid_superposition_2(), y=~token_word_ocu_col, key  =~ key_2(), type = "scatter",  mode='markers', source = "box2", marker =list(color="blue"))%>%
      add_trace(d_boxplot_2(), x = 0, y=~token_word_ocu_col, type = "box",  marker = list(outliercolor = "red"))%>%layout(title = 'Box plot of the word tokenization', yaxis =list(title ='Number of words'), titlefont = 'arial', showlegend = FALSE)
  })
  output$box_3 <- renderPlotly({
    plot_ly(d_boxplot_3(),x = random_data_avoid_superposition_2(), y=~token_word_type_col, key  =~ key_3(), type = "scatter", mode='markers',source = "box3", marker =list(color="blue"))%>%
      add_trace(d_boxplot_3(),x=0, y=~token_word_type_col, type = "box",  marker = list(outliercolor = "red"))%>%layout(title = 'Box plot of the word type tokenization', yaxis =list(title ='Number of different words'), titlefont = 'arial', showlegend = FALSE)
  })
  output$box_4 <- renderPlotly({
    plot_ly(d_boxplot_4(),x = random_data_avoid_superposition_2(), y=~token_ratio_col, key  =~ key_4(), type = "scatter",  mode='markers',source = "box4", marker =list(color="blue"))%>%
      add_trace(d_boxplot_4(), x=0, y=~token_ratio_col, type = "box",  marker = list(outliercolor = "red"))%>%layout(title = 'Box plot of the ratio', yaxis =list(title ='Ratio'), titlefont = 'arial', showlegend = FALSE)
  })
  output$box_5 <- renderPlotly({
    plot_ly(d_boxplot_5(),x = random_data_avoid_superposition_3(), y=~token_normalization, key  =~ key_5(), type = "scatter", mode='markers', source = "box5", marker =list(color="blue"))%>%
      add_trace(d_boxplot_5(), x=0, y=~token_normalization, type = "box", marker = list(outliercolor = "red"))%>%layout(title = 'Box plot of the normalization', yaxis =list(title ='Number of normalized words'), titlefont = 'arial', showlegend = FALSE)
  })
  
  
  #Doing the hover descritpion
  output$description_token <- renderUI({
    d1 <- event_data("plotly_click", source = "box1")
    d2 <- event_data("plotly_click", source = "box2")
    d3 <- event_data("plotly_click", source = "box3")
    d4 <- event_data("plotly_click", source = "box4")
    d5 <- event_data("plotly_click", source = "box5")
    
    n1 <- d1$key
    n2 <- d2$key
    n3 <- d3$key
    n4 <- d4$key
    n5 <- d5$key
    
    #Loading the description of the tokens and the modulo that works well with the app
    lien <- paste(my_path,"/Intership_NLP_CU-master/description/description_token.R", sep="")
    source(lien)
    lien <- paste(my_path,"/Intership_NLP_CU-master/backend_analysis/modulo_not_null.R", sep="")
    source(lien)
    
    
    if(length(n1)==1){
      text_descri_hover_choosen_1 <- token_sentence_description[modulo.not.null(strtoi(n1), n.tokenizer.sentence)]
    }
    if(length(n2)==1){
      text_descri_hover_choosen_2 <- paste(token_sentence_description[((strtoi(n2)-1) %/% n.tokenizer.word)+1],token_word_description[ modulo.not.null(strtoi(n2), n.tokenizer.word)],sep ='\n')
    }

    if(length(n3)==1){
      text_descri_hover_choosen_3 <- paste(token_sentence_description[((strtoi(n3)-1) %/% n.tokenizer.word)+1],  token_word_description[ modulo.not.null(strtoi(n3), n.tokenizer.word)],sep ='\n')
    }

    if(length(n4)==1){
      text_descri_hover_choosen_4 <- paste(token_sentence_description[((strtoi(n4)-1) %/% n.tokenizer.word)+1],  token_word_description[modulo.not.null(strtoi(n4), n.tokenizer.word)],sep ='\n')
    }

    if(length(n5)==1){
      text_descri_hover_choosen_5 <- paste(token_sentence_description[((strtoi(n5)-1) %/% (n.normalization*n.tokenizer.word))+1],token_word_description[(((strtoi(n5)-1) %/% n.normalization) %% n.tokenizer.word)+1],  token_norma_description[modulo.not.null(strtoi(n5), n.normalization)],sep ='\n')
    }
    
    
    
    #Creating the data that will appear on the app
    
    tagList(
      renderPrint({d3}),
      tags$b(renderText({"Boxplot 1:"})),
      tags$br(),
      if(length(n1)==1){
        renderText({paste("Nb sentences : ",d1$y)})
      },
      if(length(n1)==1){
        renderText({paste("Sentence : ",modulo.not.null(strtoi(n1), n.tokenizer.sentence))})
      },
      tags$br(),
      if(length(n1)==1){
        renderPrint({cat(text_descri_hover_choosen_1)})
      },
      tags$br(),
      
      tags$b(renderText({"Boxplot 2:"})),
      tags$br(),
      if(length(n2)==1){
        renderText({paste("Nb words : ",d2$y)})
      },
      if(length(n2)==1){
        renderText({paste("Sentence : ",((strtoi(n2)-1) %/% n.tokenizer.word)+1)})
      },
      if(length(n2)==1){
        renderText({paste("Word : ",modulo.not.null(strtoi(n2), n.tokenizer.word))})
      },
      tags$br(),
      if(length(n2)==1){
        renderPrint({cat(text_descri_hover_choosen_2)})
      },
      tags$br(),
      
      tags$b(renderText({"Boxplot 3:"})),
      tags$br(),
      if(length(n3)==1){
        renderText({paste("Nb words : ",d3$y)})
      },
      if(length(n3)==1){
        renderText({paste("Sentence : ",((strtoi(n3)-1) %/% n.tokenizer.word)+1)})
      },
      if(length(n3)==1){
        renderText({paste("Word : ",modulo.not.null(strtoi(n3), n.tokenizer.word))})
      },
      tags$br(),
      if(length(n3)==1){
        renderPrint({cat(text_descri_hover_choosen_3)})
      },
      tags$br(),
      
      tags$b(renderText({"Boxplot 4:"})),
      tags$br(),
      if(length(n4)==1){
        renderText({paste("Ratio : ",d4$y)})
      },
      if(length(n4)==1){
        renderText({paste("Sentence : ",((strtoi(n4)-1) %/% n.tokenizer.word)+1)})
      },
      if(length(n4)==1){
        renderText({paste("Word : ",modulo.not.null(strtoi(n4), n.tokenizer.word))})
      },
      tags$br(),
      if(length(n4)==1){
        renderPrint({cat(text_descri_hover_choosen_4)})
      },
      tags$br(),
      
      tags$b(renderText({"Boxplot 5:"})),
      tags$br(),
      if(length(n5)==1){
        renderText({paste("Nb words : ",d5$y)})
      },
      if(length(n5)==1){
        renderText({paste("Sentence : ",((strtoi(n5)-1) %/% (n.normalization*n.tokenizer.word))+1)})
      },
      if(length(n5)==1){
        renderText({paste("Word : ",(((strtoi(n5)-1) %/% n.normalization) %% n.tokenizer.word)+1)})
      },
      if(length(n5)==1){
        renderText({paste("Normalization : ", modulo.not.null(strtoi(n5), n.normalization))})
      },
      tags$br(),
      if(length(n5)==1){
        renderPrint({cat(text_descri_hover_choosen_5)})
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
  
  token_norma_radio_button <- reactive({
    if(input$choice_token_moment == "Now"){
      input$token_norma_radio_button_now 
    }
    else if(input$choice_token_moment == "Later"){
      input$token_norma_radio_button_later
    }
  })
  #Warning if Now chosen
  
  output$choice_tokenizations_reminded <- renderText({
    paste("You have chosen the sentence tokenization ",id_token_sentence_selected(), ", the word tokenization ", id_token_word_selected(),"and the word normalization ", id_token_norma_selected(),".")
  })
      
  output$warning_choose_before <- renderUI({
    tagList(
      if(input$choice_token_moment == "Now"){
        renderText({"You have chosen to choose the tokenization at the beginning of the app. So what you will choose here will have no effect on the tokenization used for the analysis. If you want to choose here, you need to go back to the first page (Data) and choose 'Later'"})
      }
    )
  })
  
  #Creating the data with the chosen tokenization
  # id_token_sentence_selected <- reactive({strtoi(gsub("TokenizationSentence", "",token_sentence_radio_button()))})
  # id_token_word_selected <- reactive({strtoi(gsub("TokenizationWord", "",token_word_radio_button()))})
  # tokenizer.sentence.i <- reactive({sprintf("tokenizer.sentence.%d(original_books_selected_used())", id_token_sentence_selected())})
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
  #Data created using a function taking the koens used as arguments
  lien <- paste(my_path,"/Intership_NLP_CU-master/backend_analysis/after_choose_token.R", sep="")
  source(lien)
  id_token_sentence_selected <- reactive({strtoi(token_sentence_radio_button())})
  id_token_word_selected <- reactive({strtoi(token_word_radio_button())})
  id_token_norma_selected <- reactive({strtoi(token_norma_radio_button())})
  original_books_tokenized <- reactive({after.choose.token(original_books_selected_used(),id_token_sentence_selected(),id_token_word_selected(),id_token_norma_selected())})
  
  #Removing the stopwords from the text if the user wants to
  original_books_tokenized_inter <- reactive({
    if(input$stopword_choice==TRUE){
      if(input$stemming_choice==TRUE){
        arrange(original_books_tokenized()[[6]], desc(freq))
      }
      else if(input$stemming_choice==FALSE){
        arrange(original_books_tokenized()[[5]], desc(freq))
      }
    }
    else if(input$stopword_choice==FALSE){
      if(input$stemming_choice==TRUE){
        arrange(original_books_tokenized()[[4]], desc(freq))
      }
      else if(input$stemming_choice==FALSE){
        arrange(original_books_tokenized()[[3]], desc(freq))
      }  
    }
  })
  original_books_tokenized_freq <- reactive({original_books_tokenized_inter()%>%mutate(rowname = row_number())})
  
  # use the key aesthetic/argument to help uniquely identify selected observations
  key <- reactive({row.names(original_books_tokenized_freq())})
  
  #Shared data between the plot and the datatable of the overview and the wordcloud for the analysis
  original_books_tokenized_freq_shared <- reactive({SharedData$new(original_books_tokenized_freq(), ~key())})
  
  #########################################################################  Details on demand Pre processing  ################################################
  #Heaps law
  lien <- paste(my_path,"/Intership_NLP_CU-master/backend_analysis/heaps_law.R", sep="")
  source(lien)
  
  heaps_law_result <- reactive({heaps.law(original_books_selected_used(), id_token_sentence_selected(), id_token_word_selected())})
  nb.of.word.occu <- reactive({heaps_law_result()[[1]]}) 
  nb.of.stop.word <- reactive({heaps_law_result()[[2]]}) 
  data_heaps_law <- reactive({data.frame(nb.of.word.occu = nb.of.word.occu(),nb.of.stop.word = nb.of.stop.word())})
  reg_lin <- reactive({lm(log(nb.of.stop.word()) ~ log(nb.of.word.occu()))})
  K <- reactive({exp(reg_lin()$coefficients[[1]])})
  beta <- reactive({reg_lin()$coefficients[[2]]})
  output$plot_log_heaps_law <- renderPlot({
    # data_line_log_plotly <- c()
    # for(i in 1:length(nb.of.word.occu())){
    #   data_line_log_plotly <- c(data_line_log_plotly, log_K()*i + log_beta())
    # }
    # data_line_log_plotly <- data.frame(reg_lin_col = data_line_log_plotly)
    # plot_ly(data_heaps_law(), x =~ log(nb.of.word.occu), y =~ log(nb.of.stop.word))%>%add_trace(data_line_log_plotly, y =~ reg_lin_col, type = "scatter", mode = "line")
  
    plot(log(nb.of.word.occu()),log(nb.of.stop.word()),main="Heaps law Log", xlab="Log of number of word occurences", ylab="Log of number of stop words")
    abline(reg_lin()) 
    })
  output$summary_reg_heaps_law <- renderUI(
    tagList(
      tags$b(renderText({"Summary of the Heaps Law regression"})),
      renderPrint({summary(reg_lin())})
    )
  )
  output$plot_heaps_law <- renderPlot({
    plot(nb.of.word.occu(), nb.of.stop.word(),main="Heaps law", xlab="Number of word occurences", ylab="Number of stop words")
    lines(nb.of.word.occu(), K()*nb.of.word.occu()^beta(), col="red")
  })
  
  #zipfs law
  lien <- paste(my_path,"/Intership_NLP_CU-master/backend_analysis/zipfs_law.R", sep="")
  source(lien)
  zipfs_law_result <- reactive({zipfs.law(original_books_tokenized_freq())})
  zipfs_law_data <- reactive({zipfs_law_result()[[1]]})
  lambda <- reactive({zipfs_law_result()[[2]]}) 
  inv <-reactive({zipfs_law_result()[[3]]})
  
  output$summary_reg_zipfs_law <- renderUI({
    tagList(
      tags$b(renderText({"Summary of the Zipf's Law regression"})),
      renderPrint({zipfs_law_result()[[4]]})
    )
  })
  # jpeg(paste(my_path, sprintf('/Intership_NLP_CU-master/backend_analysis/boxplot/zipfs_law_data_%d.jpg',choose_load_data),sep =""))
  # freq_by_rank %>% ggplot(aes(rank, term_frequency)) +
  #   geom_abline(intercept = reg_lin$coefficients[[1]], slope = inv, color = "red") +
  #   geom_line(size = 1.1, alpha = 0.8, show.legend= FALSE) +
  #   scale_x_log10() +
  #   scale_y_log10()
  output$plot_zipfs_law <- renderPlot({
    zipfs_law_data() %>% ggplot(aes(rank, term_frequency)) +
      geom_abline(intercept = log(lambda()), slope = inv(), color = "red") +
      geom_line(size = 1.1, alpha = 0.8, show.legend= FALSE) +
      scale_x_log10() +
      scale_y_log10() + ggtitle("Zipf's law") + xlab("Rank")+ ylab("Term frequency")
  })
  
  #Doing the table with some info about the text
  lien <- paste(my_path,"/Intership_NLP_CU-master/backend_analysis/table_info.R", sep="")
  source(lien)
  table_info_result <- reactive({table.info(original_books_tokenized())})
  output$table_info_details_pre <- renderTable(table_info_result())
  
  ######################################################################### Overview Analysis  ####################################################
  
  
  #Plotting the scatterplot with plotly
  output$plot_overview <- renderPlotly({
    #s matches the row selected by the user
    s <- input$plot_rows_selected
    #The if for the length doesn't seem very useful, because it works without it, however, I found it on the internet and there is maybe a reason I haven't found yet, so I prefer to let for now.
    #if there are no row selected yet, you can highlight the plot by selecting some points
    if(!length(s)){
      if(input$choice=='Frequency'){
        plot_ly(original_books_tokenized_freq_shared(), x = ~rowname, y = ~freq, key = ~key(), type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect',  defaultValues = s,color = I('green'))
      }
      else if(input$choice=='Term Frequency'){
        plot_ly(original_books_tokenized_freq_shared(), x = ~rowname, y = ~tf, key = ~key(), type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Term Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Term Frequency'), titlefont = 'arial', showlegend = FALSE)%>% highlight("plotly_selected", 'plotly_deselect', defaultValues = s, color = I('green'))
      }
    }
    #If there are row selected, you can't higlight the plot because it is already highlighted 
    else if(length(s)){
      if(input$choice=='Frequency'){
        plot_ly(original_books_selected_used(), x = ~rowname, y = ~freq, key = ~key(), type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Frequency'), titlefont = 'arial', showlegend = FALSE)
      }
      else if(input$choice=='Term Frequency'){
        plot_ly(original_books_selected_used(), x = ~rowname, y = ~tf, key = ~key(), type = 'scatter', mode='lines+markers',  marker = list(color = 'blue', opacity=2))%>%layout(title = 'Term Frequency according to the word', xaxis = list(title ='Word'), yaxis =list(title ='Term Frequency'), titlefont = 'arial', showlegend = FALSE)      
      }
    }
  })
  
  #Plotting the Data Table
  output$table_overview <- DT::renderDataTable({
    #Choosing the data selected in the plot. It is done by crosstalk, see CRAN R Crosstalk SharedData for more details
    dsel <- original_books_tokenized_freq()[original_books_tokenized_freq_shared()$selection(),]
    #Creating the data table with the initial data
    dt <-DT::datatable(original_books_tokenized_freq(),options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),pageLength = 5, lengthMenu = c(5, 10, 15, 20)),class = 'display')
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
  d_real_shared <- reactive({original_books_tokenized_freq()[original_books_tokenized_freq_shared()$selection(),]})
  #Choosing the data to give to the wordcloud, depending on which data is taken from plotly
  d_prime_reac <- reactive({data.frame(d_real_shared()$word, d_real_shared()$freq)})
  
  filter_d <- reactive({
    #Changing the data in order to match what the wordcloud takes as an input
    d_prime_reac <- reactive({data.frame(d_real_shared()$word, d_real_shared()$freq)})
    head(subset(d_prime_reac(), d_real_shared...freq <= input$slide_value_freq[2] & d_real_shared...freq >= 
                  input$slide_value_freq[1]), 
         input$slide_value_word)
  })
   
  ################################################################################ Filter Analysis  #########################################################
  
  #Updating the value of the maximum of the slider input for the number of words and for the frequency
  m_act <- reactive({max(original_books_tokenized_freq()$freq)})
  n_act <- reactive({NROW(original_books_tokenized_freq())})
  observeEvent(original_books_selected_used(),{updateSliderInput(session,inputId = "slide_value_freq", label = "Filter the frequency", min = 1, max = m_act(), value = c(1,m_act()), step = 1)})
  observeEvent(original_books_selected_used(),{updateSliderInput(session,inputId = "slide_value_word", label = "Choose the maximum number of words", min = 1, max = n_act(), value = n_act(), step = 1)})
  
  #Creating the title for the word cloud
  
  output$title_wordcloud <- renderUI({
    tagList(
      renderText("Wordcloud of the selected words from the previous plot")
    )
  })
  
  
  #Creating the wordcloud and making it reactive to change in the input values
  output$wordcloud  <- renderWordcloud2(wordcloud2(data = filter_d(),
                                                   shape = 'star', size = 0.8, shuffle =FALSE))
  output$test <- renderPrint({
    # filter_d()
    input$selected_word
  })
  
  ####################################################################  DATA  ################################################################################
  
  word_freq_wordcloud_selected_filter <- reactive({input$selected_word})
  word_wordcloud_selected_filter <- reactive({gsub(":[0-9]+", "", word_freq_wordcloud_selected_filter())})
  list_sentences_wordcloud_filter <- reactive({subset(original_books_tokenized_freq(), word == word_wordcloud_selected_filter())$sentences})
  data_selected_sentences_wordcloud <- reactive({
    local_data_selected_sentences_wordcloud <- c()
    for(i in list_sentences_wordcloud_filter()){
      local_data_selected_sentences_wordcloud <- c(local_data_selected_sentences_wordcloud , original_books_tokenized()[[1]]$sentence[i])
    }
    data.frame(sentence = unlist(local_data_selected_sentences_wordcloud))
    })
  
  ########################################################################## DATA table analysis sentences  ########################################################
  
  output$sentence_table_wordcloud <- DT::renderDataTable({
    DT::datatable(data_selected_sentences_wordcloud(),options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),pageLength = 5, lengthMenu = c(5, 10, 15, 20),searchHighlight = TRUE, search = list(search = word_wordcloud_selected_filter())),class = 'display')
    
  })
  
  #Data fot the wordcloud
  lien <- paste(my_path,"/Intership_NLP_CU-master/backend_analysis/wordcloud_data_func.R", sep="")
  source(lien)
  
  data_wordcloud_freq_tokenized <- reactive({wordcloud.data.func(original_books_tokenized()[[2]], list_sentences_wordcloud_filter()[[1]], word_wordcloud_selected_filter())})
   
  #Creating the title for the wordcloud
  
  output$title_wordcloud2 <- renderUI({
    tagList(
      renderText("fkjfjofrnjnjo"),
      renderPrint(paste("Wordcloud of the words in the same sentences of ",word_wordcloud_selected_filter()))
    )
  })
  #Creating the wordcloud with the sentences
  
  output$wordcloud2_sentences  <- renderWordcloud2(wordcloud2(data = data_wordcloud_freq_tokenized() ,
                                                  shape = 'star', size = 0.8, shuffle =FALSE))
  
  
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
      params <- list(choice_data = input$data_type_choice, time_choice_token = input$choice_token_moment,token_choosen_sentence = token_sentence_radio_button(),
                    token_choosen_word = token_word_radio_button(), token_choosen_norma = token_norma_radio_button(),
                    overview_choice_data_all = input$all, overview_choice_data_num_check = input$num_check,overview_choice_offset =input$num_offset_data,
                    overview_choice_num_word=input$num_word_data, overview_choice_book = input$book, data_selected_lines = original_books_selected_used(),
                    data_boxplot = d_token_boxplot(), data_complete = original_books_tokenized_freq(), 
                    occurence_word = nb.of.word.occu(),occurence_stop_word = nb.of.stop.word(), regression_lin = reg_lin(), result_zipfs_law_passed = zipfs_law_result(),
                    table_info_laws = table_info_result(), data_selected_plot = original_books_tokenized_freq()[original_books_tokenized_freq_shared()$selection(),], 
                    min_freq_wordcloud = input$slide_value_freq[1], max_freq_wordcloud = input$slide_value_freq[2],
                    max_word_wordcloud = input$slide_value_word,key = key(), selected_word_cloud = word_wordcloud_selected_filter(),
                    sentences_selected_cloud = data_selected_sentences_wordcloud())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,switch(
        input$format,
        PDF = pdf_document(toc=TRUE, toc_depth= 4), HTML = html_document(toc=TRUE, toc_depth= 4), Word = word_document(toc=TRUE, toc_depth= 4)
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
  
  # output$warningMenu <- renderMenu({
  #   # Code to generate each of the messageItems
  #   war <- list(notificationItem(text = "Everything seems to work", icon("users")))
  #   l_wc <- reactive({length(filter_d()$d_real_shared...word)})
  #   if(l_wc()==1){
  #     list.append(war, notificationItem(
  #       text = "Only one word is selected on the wordcloud and none appears",
  #       icon = icon("exclamation-triangle"),
  #       status = "warning"
  #     )
  #     )
  #   }
  #   # This is equivalent to calling:
  #   #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
  #   dropdownMenu(type = "notifications", .list = war)
  # })
  
}
return(server)