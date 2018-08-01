#Shiny App
#UI and layout. It implements the front-end
#Creating a navbar page with different tabs, which are created by tabPanel
header <- dashboardHeader(title="NLP App",dropdownMenuOutput("warningMenu"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data", tabName = "data", icon = shiny::icon("th")),
    menuItem("Pre-processing",
             menuSubItem("Overview", tabName = "overview_pre"), icon = icon("fas fa-wrench"),
             menuSubItem("Filter", tabName = "filter_pre", icon = icon("fas fa-binoculars")),
             menuSubItem("Details on demand", tabName = "details_pre", icon = icon("fas fa-sitemap"))
             ),
    menuItem("Analysis", tabName = "ana", 
             menuSubItem("Overview", tabName = "overview_ana"), icon = icon("fab fa-leanpub"),
             menuSubItem("Filter", tabName = "filter_ana", icon = icon("fas fa-binoculars")),
             menuSubItem("Word in context", tabName = "wcontext_ana", icon = icon("fas fa-sitemap"))),
    radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                 inline = TRUE),
    fluidRow(offset = 10,
    downloadButton("report", "Generate report", class = "butt1"),
    #Putting the color of the button in black and the writing in white so it is easier to see and to interact with
    tags$head(tags$style(".butt1{background-color:black;} .butt1{color: white;}"))
  )
)
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "data",
      fluidRow(
        box(width =6 ,
            fileInput("inputdata", "Choose PDF File", multiple = FALSE)
            ),
        box(width = 6,
            radioButtons("data_type_choice", "Which data do you have as an input to upload?",
                         check_choices_load_data, inline = TRUE)
            )
      ),
      fluidRow(
        box(width = 6,
        radioButtons("choice_token_moment", "Choose the tokenization now or in the filter view of the pre processing table",
                         choices = c("Now", "Later"), selected = "Later"),
        radioButtons("token_sentence_radio_button_now", "Tokenization of sentences",
                   check_choices_token_sentence_check, inline = TRUE),
      radioButtons("token_word_radio_button_now", "Tokenization of words",
                   check_choices_token_word_check, inline = TRUE),
      radioButtons("token_norma_radio_button_now", "Normalization of words",
                   check_choices_token_norma_check, inline = TRUE)
        ),
      box(width = 6,
          uiOutput("description_type_data_possible_analyzed")
          )
      )
    ),
    tabItem(
      tabName = "overview_pre",
      h2("Data overview"),
      fluidRow(
        box(
          plotlyOutput("plot_data")
        ),
        box(
          checkboxInput("all", label = "Select all the data", value = TRUE),
          checkboxGroupInput("book", "Choose one or more book(s)",
                             c(), inline = TRUE),
          checkboxInput("num_check", label = "Choose the data with the numeric input, else you can select it on the graph", value = FALSE),
          numericInput(inputId = "num_offset_data", label = "Choose the number of the first line", min = 1, max = n, value = 1),
          numericInput(inputId = "num_word_data", label = "Choose the number of lines follwing the offset", min = 1, max = n, value = n),
          tags$br(),
          uiOutput("num_data"),
          tags$br(),
          tags$b(textOutput("num_data_highlighted")),
          tags$br(),
          uiOutput("num_data_text_display")
        )
      )
    ),
    tabItem(
      tabName = "filter_pre",
      fluidRow(
               box(width = 4,
                 plotlyOutput("box_1")
               ),
               box(width = 4,
                 plotlyOutput("box_2")
               ),
               box(width = 4,
                 plotlyOutput("box_3")
               )),
        fluidRow(
               box(width = 4,
                 plotlyOutput("box_4")
               ),
               box(width = 4,
                 plotlyOutput("box_5")
               ),
              box(width = 4,
                  uiOutput("description_token")
          )),
      fluidRow(
        box(
          radioButtons("token_sentence_radio_button_later", "Tokenization of sentences",
                             check_choices_token_sentence_check, inline = TRUE),
          radioButtons("token_word_radio_button_later", "Tokenization of words",
                             check_choices_token_word_check, inline = TRUE),
          radioButtons("token_norma_radio_button_later", "Normalization of words",
                       check_choices_token_norma_check, inline = TRUE),
          textOutput("choice_tokenizations_reminded"),
          tags$br(),
          tags$b(uiOutput("warning_choose_before"))
        )
      )
    ),
    tabItem(
      tabName = "details_pre",
      fluidRow(
        box(width = 4,
            plotOutput("plot_log_heaps_law")
        ),
        box(width = 4,
            uiOutput("summary_reg_heaps_law")
        ),
        box(width = 4,
            plotOutput("plot_heaps_law")
        )
        ),
      fluidRow(
        box(width = 4,
            uiOutput("summary_reg_zipfs_law")
        ),
        box(width = 4,
            plotOutput("plot_zipfs_law")),
        box(width = 4,
            tableOutput("table_info_details_pre"))
      )),
    tabItem(
      tabName = "overview_ana",
      h2("Plot overview"),
      fluidRow(
              box(width = 4,
               selectInput(inputId = 'choice', label = 'Choose a metric', 
                           choice = c('Frequency', 'Term Frequency')),
               checkboxInput("stemming_choice", label = "Normalization/Stemming", value = FALSE),
               checkboxInput("stopword_choice", label = "Stopwords", value = FALSE)
               ),
            box(width = 8,
               plotlyOutput("plot_overview")
             )
      ),
      fluidRow(
        column(width = 8, offset =4,
               box(
                 DT::dataTableOutput("table_overview")
               ))
      )
    ),
    tabItem(
      tabName = "filter_ana",
      h2("Plot filtering"),
      fluidRow(
        column(width = 8,
               box(
                 wordcloud2Output("wordcloud"),
                 #This HTML script implements the click option for the wordcloud
                 tags$script(HTML(
                   "$(document).on('click', '#canvas', function() {",
                   'word = document.getElementById("wcSpan").innerHTML;',
                   "Shiny.onInputChange('selected_word', word);",
                   "});"
                 )),
                 verbatimTextOutput("test")
               )),
        column(width = 4,
               box(
                 #Slider button to filter the data
                 sliderInput(inputId = "slide_value_freq", label = "Filter the frequency", min = 1, max = m, value = c(1,m), step = 1, dragRange = TRUE),
                 sliderInput(inputId = "slide_value_word", label = "Choose the maximum number of words", min = 1, max = n, value = n, step = 1)
                 
               ))
      )
    ),
    tabItem(
      tabName = "wcontext_ana",
      fluidRow(
        column(width = 8, offset =4,
               box(
                 DT::dataTableOutput("sentence_table_wordcloud")
               )))
  )
 
)
)

ui <- dashboardPage(header, sidebar, body)

return(ui)