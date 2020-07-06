library(shiny)
library(readr)
library(utf8)
library(DT)
library(dplyr)
library(tidytext)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(quanteda)
library(stringr)
library(tm)
library(plotly)
library(tidyr)
library(lda)
library(udpipe)
library(lattice)
library(leaflet)
library(rgdal)
library(rintrojs)
library(shinyalert)

afinn <- readRDS("afinn.rds")

options(shiny.maxRequestSize=100*1024^2)

stripHTML <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

onlyASCII <- function(stringVector){
    return(gsub('[^\x20-\x7E]', '', stringVector))
}


saatchiBigramize <- function(df){
     df %>%
        unnest_tokens(bigram, Comment2, "ngrams", n = 2)
}

gdal.states <- readRDS("StatesShapeFile.rds")

glossary <- data.frame(Tab = c("Tables", "Tables", "Tables",
                               "Visualizations", "Visualizations",
                               "Visualizations", "Visualizations",
                               "Visualizations", "Visualizations",
                               "Context", "Context", "Context", "Context",
                               "Topics", "Topics", "Topics", "Topics", "Topics",
                               "Maps"), 
                       Term = c("Variables", "Tokenize", "Bigramize",
                                "Top N Contributions", 
                                "Top N Positive/Negative",
                                "Max. Positive Words",
                                "Min. Positive Freq.",
                                "Max Negative Words",
                                "Min. Negative Freq.", 
                                "One Word", "Multiple Words", "One Phrase", "Multiple Phrases",
                                "UDPipe NLP", "LDA", "Naive Bayes", "Decision Trees", "SVM",
                                "Variables"),
                       Definition = c("Select a variable to tokenize or bigramize. This variable will be the subject of analysis in all subsequent tabs. Use this selector to update the variable for analysis.", 
                                      "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "", "", ""))

gdal.states <- readRDS("StatesShapeFile.rds")

# options(shiny.maxRequestSize=100*1024^2)



 negative_sentiments <- get_sentiments("bing") %>%
    filter(sentiment == "negative")
 
 positive_sentiments <- get_sentiments("bing") %>%
     filter(sentiment == "positive")

    ui <- navbarPage(title = "Saatchi Text Analyzer", 
                tabPanel(
                  title = introBox("Tables", data.step = 1, data.intro = "Welcome to Saatchi Text Analyzer! Let's start with our first tab, 'Tables'."),
                        introBox(sidebarPanel(
                           actionButton("helpMe", "Help"),
                           
                          introBox(selectInput("fileType", "File Type", choices = c(".csv", ".ftr"), selected = ".csv"),
                                       fileInput("file1", "Choose CSV File",
                                                 accept = c(
                                                     "text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv", ".ftr")
                                       ), data.step = 3, data.intro = "Chhose a file type to upload."),
                                       checkboxInput("header", "Header", TRUE),
                                       tags$hr(),
                                      introBox(actionButton("display", "Display"), data.step = 4, data.intro = "Inspect the file that you uploaded. This doesn't do anything special, but it helps you to confirm that you've got everything you need and that it looks the way it's supposed to."),
                                       tags$hr(),
                                       introBox(uiOutput('variables'), data.step = 5, data.intro = "This step is important! Because we can never predict what our imported data will look like, we have the ability to manually select the variable/column that contains the data that we want to analyse. This is usually the column that contains the full comment or article that we have mined."),
                                       tags$hr(),
                                       introBox(actionButton("tokenize", "Tokenize"), data.step = 6, data.intro = "Use this button ti dusplay a table of the imported data, but with the variable selected for analysis split into individual words."),
                                       tags$hr(),
                                      introBox(actionButton("bigramize", "Bigramize"), data.step = 7, data.intro = "Split the variable selected for analysis into bigrams."), 
                                      introjsUI(), width = 2
                         ), data.step = 2, data.intro = "We'll use this side panel to make changes to what happens in the body of the application."),
                         mainPanel(
                    fluidRow(
                        column(
                            introBox(DT::dataTableOutput("contents"), data.step = 8, data.intro = "The table that we display."),
                            DT::dataTableOutput("tokenized_table"),
                            tableOutput("bigram_table"),
                            textOutput("sanitizeTextInput"), width = 10       
                        )
                    )
                )),
                tabPanel( introBox("Visualizations", data.step = 9, data.intro = "Next we can move on to visualising our data in the Visualizations tab."),
                  introBox(sidebarPanel(width = 2,
                        fluidRow(
                              introBox(shiny::numericInput("topNContributors", "Top N Contributors", value = 50), data.step = 11, data.intro = "Here we can toggle how many results are returned by the top N contributors to sentiment. It often helps to bring this number down to around 25 as the chart starts to crowd and becomes difficult to read."),
                              introBox(shiny::numericInput("topNContributors2", "Top N Positive/Negative", value = 25), data.step = 12, data.intro = "Here we toggle the maximum number of words to appear in the positive and negative sentiment bar charts.")
                        ),
                        fluidRow(
                            introBox(shiny::numericInput("maxPositiveCloud", "Max. Positive Words", value = 300), data.step = 13, data.intro = "Here we can determine the maximum number of words to appear in the positively themed wordclouds. We might want to use this feature if for example there are too many words being plotted by the text data that we upload, and we might want to reduce that number to make the chart easier to read."),
                            introBox(shiny::numericInput("minPositiveFrequency", "Min. Positive Freq.", value = 0), data.step = 14, data.intro = "Here we can select the minimum frequency needed for a word to appear in the positive wordclouds. For example, if we set a value of '5', then only words that appeared 5 or more times will appear in the wordcloud.")
                        ),
                        fluidRow(
                            introBox(shiny::numericInput("maxNegativeCloud", "Max. Negative Words", value = 300), data.step = 15, data.intro = "This is the maximum number of words to be plotted in the negatively themed wordclouds."),
                            introBox(shiny::numericInput("minNegativeFrequency", "Min. Negative Freq.", value = 0), data.step = 16, data.intro = "This is the minimum frequency required for a word to appear in the negatively themed wordclouds.")
                        ), introBox(actionButton("visualize", "Visualize"), data.step = 17, data.intro = "Now that we have set our values, we can use this button to render our visualizations.")
                        
                ), data.step = 10, data.intro = "Here are our toggles and input options for the visualisations we will generate."),
                        mainPanel(
                            useShinyalert(),
                            fluidRow(
                                column(
                                    ## overall sentiment output
                                    h4("  Top Contributions to Sentiment"),
                                    plotOutput("sentiment_plot"), width = 10
                                )
                            ),
                            br(),br(),
                            fluidRow(
                                column(
                                    ## overall sentiment output
                                    h4("  Top Positive/Negative Words"),
                                    br(), br(),
                                    plotOutput("posi_negi_sentiment_plot"), width = 12)),
                            fluidRow(
                                column(
                                    h4("  Positive Wordcloud"),
                                    br(), br(),
                                    plotOutput("positive_wordcloud"), width = 10)),
                            fluidRow(
                                column(
                                    h4("PP WC"),
                                    br(), br(),
                                    plotOutput("positively_preceded_wordcloud"), width = 10)),
                            fluidRow(
                                column(
                                    h4("  Negative Wordcloud"),
                                    br(), br(),
                                    plotOutput("negative_wordcloud"), width = 10)),
                            fluidRow(
                                column(
                                    h4("NP WC"),
                                    br(), br(),
                                    plotOutput("negatively_preceded_wordcloud"), width = 10))
                        )),
            tabPanel("Context",
                     sidebarPanel(
                         selectInput("contextType", "Word/Phrase", choices = c("One Word", "Multiple Words",
                                                                               "One Phrase", "Multiple Phrases")),
                         textInput("contextFilter", "Context", placeholder = "Enter the context filter"),
                         tags$hr(),
                         numericInput("window", "Window", 10),
                         actionButton("get_context", "Get Context"), width = 2
                     ),
                             mainPanel(
                                 fluidRow(
                                     column(
                                         ## overall sentiment output
                                         dataTableOutput("kwic_table"), width = 10,
                                         downloadButton("downloadTable", "Download")
                                     )
                                 )
                             )),
            tabPanel("Topics",
                     sidebarPanel(
                         style = "position:fixed;width:inherit;",
                         selectInput("topicModel", "Algorithm", choices = c("UDPipe NLP", "LDA", 
                                                                            "Naive Bayes",
                                                                            "Decision Trees",
                                                                            "SVM")),
                         actionButton("runModel", "Run Model"), width = 2
                     ),
                     mainPanel(
                         fluidRow(
                             column(
                                 plotOutput("barchartNouns"),
                                 plotOutput("barchartAdjectives"),
                                 plotOutput("barchartWordpairs"),
                                 dataTableOutput("matrixTable"), width = 10
                             )
                         )
                     )),
            tabPanel("Maps",
                     div(class="outer",
                         tags$head(
                             includeCSS("styles.css"),
                             includeScript("gomap.js")
                         ),
                         leafletOutput("leaflet_map", width = "100%", height = "100%"),
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                       width = 330, height = "auto",
                                       h2("Saatchi Maps"),
                                       selectInput("fileType2", "File Type", choices = c(".csv", ".ftr"), selected = ".csv"),
                                       fileInput("file2", "Choose CSV File",
                                                 accept = c(
                                                     "text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv", ".ftr")
                                       ),
                                       uiOutput("variables_maps"),
                                       uiOutput("variables_maps_text"),
                                    #   uiOutput("variables_maps_author"),
                                       selectInput("spatialGranularity", label = "Granularity", choices = c("Postcode", "City", "State", "Country")),
                                       actionButton("renderMap", "Go")
                         )
    )),
           tabPanel("Glossary",
                     mainPanel(dataTableOutput("glossaryTable"), width = 8)
                    )
            )
    
            
    
        

    server <- shinyServer(function(input, output, session) {
    
      
          observeEvent(input$helpMe , {
            introjs(session)  
          })
          
      
            input_data <- reactive({
                    inFile <- input$file1
                    if(input$fileType == ".csv"){
                    temp1 <- read.csv(inFile$datapath, header = input$header)
                    } else if(input$fileType == ".ftr"){
                        temp1 <- read_feather(inFile$datapath)
                    }
                    temp1
            })
        
       

            input_data2 <- reactive({
                var_test <- sym(input$variables2)
                data2 <- mutate(input_data(), Comment2 = as.character((!!!var_test)))
                data2$Comment2 = utf8::utf8_format(data2$Comment2)
                Encoding(data2$Comment2) <- "latin1"
                data2$Comment2 <- stripHTML(data2$Comment2)
                data2$Comment2 <- onlyASCII(data2$Comment2)
                data2
            })
            
            input_data_tokens <- reactive({
                temp <- input_data2() %>%
                    unnest_tokens(word, Comment2)
                Encoding(temp$word) <- "latin1"
                temp
            })
            
        
            
            input_data_bigrams <- reactive({
                input_data2() %>%
                    unnest_tokens(bigram, Comment2, "ngrams", n = 2) %>%
                    separate(bigram, c("word1", "word2"), remove = FALSE)
            })
            
            observeEvent(input$display, {
                output$contents <- DT::renderDataTable({
                    DT::datatable(input_data2())
                })
            })
            
            
              
            output$downloadTable <- downloadHandler(
                filename=function(){
                    paste("Context Table", Sys.Date(), ".csv", sep="")},
                content=function(filename){
                    write.csv(kwiccy(), filename)
                })
        
        
        output$variables = renderUI({
            selectInput('variables2', 'Variables', names(input_data()))
        })
    
    
        
       ## output$sanitizeTextInput <- renderText(input$variables2)
            

        observeEvent(input$tokenize, {
            output$contents = renderDataTable({
                input_data_tokens()
            })
        })
        
    
    
    observeEvent(input$bigramize, {
           output$contents = renderDataTable({ 
               input_data_bigrams()
           })
        })
    
    contributions <- reactive({input_data_tokens() %>%
        inner_join(get_sentiments("afinn"), by = "word") %>%
        group_by(word) %>%
        summarize(occurences = n(),
                  contribution = sum(value))
    })
    
    bing_word_counts <- reactive({
        input_data_tokens() %>%
            inner_join(get_sentiments("bing"), by = "word") %>%
            count(word, sentiment, sort = TRUE) %>%
            ungroup()
    })
    
    acast_positive_negative <- reactive({ 
    step1 <- input_data_tokens() %>%
            inner_join(get_sentiments("bing")) %>%
            count(word, sentiment, sort = TRUE) %>%
            acast(word ~ sentiment, value.var = "n", fill = 0) %>%
            as.data.frame()
            
     tibble::rownames_to_column(step1, "word") 
    })
    
    acast_positively_preceded_bigrams <- reactive({
        step1 <- positively_preceded_bigram() %>%
            count(word2, sentiment, sort = TRUE) %>%
            acast(word2 ~ sentiment, value.var = "n", fill = 0) %>%
            as.data.frame()
        
        tibble::rownames_to_column(step1, "word2")
    })
    
    positively_preceded_bigram <- reactive({
        stopwordz <- get_stopwords() %>%
          filter(!word == "off") 
        input_data_bigrams() %>%
          filter(!word2 %in% stopwordz$word) %>%
            filter(word1 %in% positive_sentiments$word) %>%
            count(word2, sort = TRUE)
    })
    
    negatively_preceded_bigram <- reactive({
        stopwordz <- get_stopwords() %>%
          filter(!word == "off") 
        input_data_bigrams() %>%
            filter(!word2 %in% stopwordz$word) %>%
            filter(word1 %in% negative_sentiments$word) %>%
            count(word2, sort = TRUE)
    })
    
    
    observeEvent(input$visualize, { 
        shinyalert("Visualizing", "Generating visualisations. They will be ready shortly!", type = "info")
        {
    
    output$positively_preceded_wordcloud <- renderPlot(
        wordcloud(words = positively_preceded_bigram()$word2, 
                  freq = positively_preceded_bigram()$n, 
                  min.freq = input$minPositiveFrequency, 
                  max.words = input$maxPositiveCloud,
                  random.order = FALSE, 
                  rot.per = 0.35,
                  colors = brewer.pal(8, "Dark2"))
        , height = 600)
    
    output$negatively_preceded_wordcloud <- renderPlot(
        wordcloud(words = negatively_preceded_bigram()$word2, 
                  freq = 
                      positively_preceded_bigram()$n, 
                  min.freq = input$minNegativeFrequency, 
                  max.words = input$maxNegativeCloud,
                  random.order = FALSE, 
                  rot.per = 0.35,
                  colors = brewer.pal(8, "Dark2"))
        , height = 600)
    

        output$sentiment_plot <- renderPlot({
          topNSentiment <- reactive({ contributions() %>%
                top_n(input$topNContributors, abs(contribution)) %>%
                mutate(word = reorder(word, contribution)) %>%
                ggplot(aes(word, contribution, fill = contribution > 0)) +
                geom_col(show.legend = FALSE) +
                coord_flip()
          })
          topNSentiment()
        }
        )
        
        output$posi_negi_sentiment_plot <- renderPlot({
         plotposneg <- reactive({bing_word_counts() %>%
                group_by(sentiment) %>%
                top_n(input$topNContributors2) %>%
                ungroup() %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n, fill = sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free") +
                labs(y = "Contribution to sentiment", x = NULL) +
                coord_flip()
         })
                plotposneg()
    }) 
            
        
        output$positive_wordcloud <- renderPlot(
            wordcloud(words = acast_positive_negative()$word, 
                      freq = acast_positive_negative()$positive, 
                      min.freq = input$minPositiveFrequency, 
                      max.words = input$maxPositiveCloud,
                      random.order = FALSE, 
                      rot.per = 0.35,
                      colors = brewer.pal(8, "Dark2"))
        , height = 600)
        
        output$negative_wordcloud <- renderPlot(
            wordcloud(words = 
                      acast_positive_negative()$word, 
                      freq = acast_positive_negative()$negative, 
                      min.freq = input$minNegativeFrequency, 
                      max.words = input$maxNegativeCloud,
                      random.order = FALSE, 
                      rot.per = 0.35,
                      colors = brewer.pal(8, "Dark2"))
        , height = 600)
    

        }
    }
        )
    
    
    
    kwiccy <- reactive({
        if(input$contextType == "One Word"){
            
            kwic(tokens(input_data2()$Comment2), input$contextFilter, window = input$window) %>% 
                select(pre, keyword, post)
            
        } else if(input$contextType == "Multiple Words"){
            
            tempContextFilter = sym(input$contextFilter)
            temp <- data.frame(text = as.character(tm::removePunctuation(as.character(input$contextFilter))))
            temp <- temp %>% mutate(texttext = toString(text)) %>% unnest_tokens(word, texttext) %>%
                mutate(type = typeof(word))
            
            kwic(tokens(input_data2()$Comment2), temp$word, window = input$window) %>% select(pre, keyword, post)
        } else if(input$contextType == "One Phrase"){
            kwic(tokens(input_data2()$Comment2), phrase(input$contextFilter), window = input$window) %>% select(pre, keyword, post)
        } else if(input$contextType == "Multiple Phrases"){
            tempContextFilter = sym(input$contextFilter)
            temp <- data.frame(text = as.character(tm::removePunctuation(as.character(input$contextFilter))))
            temp <- temp %>% mutate(texttext = toString(text)) %>% unnest_tokens(bigram, texttext, "ngrams", n = 2) %>%
                mutate(type = typeof(bigram))
            temp2 <- quanteda::phrase(temp$bigram)
            kwic(tokens(input_data2()$Comment2), temp2, window = input$window) %>%
                select(pre, keyword, post)
        }})
    
    
    observeEvent(input$get_context, {
        output$kwic_table <- renderDataTable({
          kwiccy()
        }
 )})
    
    
    output$glossaryTable <- renderDataTable({
        glossary
    })
    
    observeEvent(input$runModel, {
        if(input$topicModel == "UDPipe NLP"){
          shinyalert("Running UDPipe NLP", "The UDPipe Natural Language Processing model is a large machine learning model designed to extract high-value/high-sentiment nouns, adjectives and word pairs from a text document.
                     Because this method is a computationally intensive machine learning task, please allow up to 5 minutes to view the results.", type = "info")
            x <- reactive({
              
                input_data_pipe <- input_data2() %>%
                  filter(!str_detect(Comment2, ".*#.*")) %>%
                  filter(!str_detect(Comment2, ".*travel insurance.*"))
              
                ud_model <- udpipe_download_model(language = "english")
                ud_model <- udpipe_load_model(ud_model$file_model)
                as.data.frame(udpipe_annotate(ud_model, x = input_data_pipe$Comment2))
        })

            nouns <- reactive({
                subset(x(), upos %in% c("NOUN")) 
              })
            
            nouns2 <- reactive({
                temp <- txt_freq(nouns()$token)
                temp$key <- factor(temp$key, levels = rev(temp$key))
                temp
              })
            
            
            output$barchartNouns <- renderPlot({
                barchart(key ~ freq, data = head(nouns2(), 20), col = "cadetblue", 
                         main = "Most occurring nouns", xlab = "Freq")
              })
            
            
            adjectives <- reactive({

                subset(x(), upos %in% c("ADJ"))
                })
            
            adjectives2 <- reactive({
                temp <- txt_freq(adjectives()$token)
                temp$key <- factor(temp$key, levels = rev(temp$key))
                temp
            })
            
            output$barchartAdjectives <- renderPlot({
                barchart(key ~ freq, data = head(adjectives2(), 20), col = "cadetblue", 
                         main = "Most occurring adjectives", xlab = "Freq")
            })
            
            rake_stats <- reactive({
                temp <- keywords_rake(x = x(), term = "lemma", group = "doc_id", 
                                   relevant = x()$upos %in% c("NOUN", "ADJ"))
                
                 temp$key <- factor(temp$keyword, levels = rev(temp$keyword))
                 temp
            })
            
           output$barchartWordpairs <- renderPlot({ 
                barchart(key ~ rake, data = head(subset(rake_stats(), freq > 3), 20), col = "cadetblue", 
                     main = "Keywords identified by RAKE", 
                     xlab = "Rake")
            })
            
            
            
            }
        })
    
    ### End of 'Run Model' ###
    
    ### Maps
    
    output$variables_maps = renderUI({
        selectInput('variables_maps_ui', 'Location', names(maps_data()))
    })
    
    output$variables_maps_text = renderUI({
      selectInput('variables_maps_ui_text', 'Text', names(maps_data()))
    })
    
    # output$variables_maps_author = renderUI({
    #   selectInput('variables_maps_ui_author', 'Author', names(maps_data()))
    # })
    # 
    maps_data <- reactive({
        inFile <- input$file2
        if(input$fileType2 == ".csv"){
            temp1 <- read.csv(inFile$datapath, header = input$header)
        } else if(input$fileType == ".ftr"){
            temp1 <- read_feather(inFile$datapath)
        }
        temp1
    })
    
    maps_data2 <- reactive({
        uploaded_maps_data_variable_selected <- sym(input$variables_maps_ui)
        uploaded_maps_data_text_field <- sym(input$variables_maps_ui_text)
        
        
        maps_data_temp <- mutate(maps_data(), 
                                 Cleaned_Location_Text = 
                                     as.character((!!!uploaded_maps_data_variable_selected)),
                                 Cleaned_Hit_Sentence = 
                                    as.character(!!!uploaded_maps_data_text_field))
        
        
        
        maps_data_temp$Cleaned_Location_Text = utf8::utf8_format(maps_data_temp$Cleaned_Location_Text)
        Encoding(maps_data_temp$Cleaned_Location_Text) <- "latin1"
        maps_data_temp$Cleaned_Location_Text <- stripHTML(maps_data_temp$Cleaned_Location_Text)
        maps_data_temp$Cleaned_Location_Text <- onlyASCII(maps_data_temp$Cleaned_Location_Text)
        
        maps_data_temp$Cleaned_Hit_Sentence = utf8::utf8_format(maps_data_temp$Cleaned_Hit_Sentence)
        Encoding(maps_data_temp$Cleaned_Hit_Sentence) <- "latin1"
        maps_data_temp$Cleaned_Hit_Sentence <- stripHTML(maps_data_temp$Cleaned_Hit_Sentence)
        maps_data_temp$Cleaned_Hit_Sentence <- onlyASCII(maps_data_temp$Cleaned_Hit_Sentence)
        
        
        if(input$spatialGranularity == "State"){
            maps_data_temp <- maps_data_temp %>% 
                mutate(State_Cleaned = 
                          case_when(
                              str_detect(Cleaned_Location_Text, ".*queensland.*") ~ "Queensland",
                              str_detect(Cleaned_Location_Text, ".*Queensland.*") ~ "Queensland",
                              str_detect(Cleaned_Location_Text, ".*Qld.*") ~ "Queensland",
                              str_detect(Cleaned_Location_Text, ".*qld.*") ~ "Queensland",
                              str_detect(Cleaned_Location_Text, ".*QLD.*") ~ "Queensland",
                              str_detect(Cleaned_Location_Text, ".*nsw.*") ~ "New South Wales",
                              str_detect(Cleaned_Location_Text, ".*New.*") ~ "New South Wales",
                              str_detect(Cleaned_Location_Text, ".*new.*") ~ "New South Wales",
                              str_detect(Cleaned_Location_Text, ".*NSW.*") ~ "New South Wales",
                              str_detect(Cleaned_Location_Text, ".*Vic.*") ~ "Victoria",
                              str_detect(Cleaned_Location_Text, ".*VIC.*") ~ "Victoria",
                              str_detect(Cleaned_Location_Text, "ACT") ~ "Australian Capital Territory",
                              str_detect(Cleaned_Location_Text, "Act") ~ "Australian Capital Territory",
                              str_detect(Cleaned_Location_Text, ".*Australian Cap.*") ~ "Australian Capital Territory",
                              str_detect(Cleaned_Location_Text, ".*West.*") ~ "Western Australia",
                              str_detect(Cleaned_Location_Text, "WA") ~ "Western Australia",
                              str_detect(Cleaned_Location_Text, ".*South Au.*") ~ "South Australia",
                              str_detect(Cleaned_Location_Text, ".*SA.*") ~ "South Australia",
                              str_detect(Cleaned_Location_Text, ".*South.*") ~ "South Australia",
                              str_detect(Cleaned_Location_Text, ".*south.*") ~ "South Australia",
                              str_detect(Cleaned_Location_Text, ".*Tas.*") ~ "Tasmania",
                              str_detect(Cleaned_Location_Text, ".*TAS.*") ~ "Tasmania",
                              str_detect(Cleaned_Location_Text, ".*North.*") ~ "Northern Territory",
                              str_detect(Cleaned_Location_Text, ".*NT.*") ~ "Northern Territory", 
                              TRUE ~ "NA"
                          ))  
            
            maps_data_temp %>%
                filter(!State_Cleaned == "NA")
        }
        
    })
        
    percentage_bins = c(0, 1, 5, 10, 20, 40, 60, 80, Inf)
    
    average_sentiment_bins = c(-50, -.5, 0, 10, 30, 50, 100, Inf)
    
    observeEvent(input$renderMap, {
        if(input$spatialGranularity == "State"){
          shinyalert("Mapping", "Your data is being mapped according to Australian states. This should be ready in around 60 seconds!", type = "info")
            maps_data_grouped_subregion <- reactive({
                maps_data2() %>%
                    unnest_tokens(word, Cleaned_Hit_Sentence) %>%
                    inner_join(afinn) %>%
                    mutate(sentiment = case_when(value < 0 ~ "Negative", value >= 0 ~ "Positive")) %>%
                    group_by(State_Cleaned, sentiment) %>%
                    summarise(contribution = sum(value)) %>%
                    dplyr::ungroup() %>%
                pivot_wider(names_from = sentiment, values_from = contribution) %>%
                dplyr::group_by(State_Cleaned) %>%
                mutate(true_sentiment = Positive - Negative) %>% 
                summarise(Ave_Sentiment_per_Author = mean(true_sentiment)) %>%
                    dplyr::mutate(STE_NAME16 = State_Cleaned)
            })
            gdal.states2 <- gdal.states 
            gdal.states2@data <- dplyr::left_join(gdal.states2@data, maps_data_grouped_subregion(), by = "STE_NAME16")
            pal <- colorBin("RdYlGn", domain = maps_data_grouped_subregion()$`Ave_Sentiment_per_Author`, bins = average_sentiment_bins)
            leafletProxy("leaflet_map", data = gdal.states) %>%
                clearControls() %>%
                addPolygons(data = gdal.states2, #,
                            fillColor = ~pal(gdal.states2$`Ave_Sentiment_per_Author`),
                            weight = 1,
                            opacity = 1,
                            color = "black",
                            dashArray = "2",
                            fillOpacity = .32,
                            highlight = highlightOptions(
                                weight = 3.5, 
                                color = "white",
                                dashArray = "4",
                                fillOpacity = 0.32,
                                bringToFront = TRUE),
                            layerId = gdal.states2@data$STE_NAME16,
                            label = sprintf("<strong>%s</strong><br/>%s",
                                            paste("Negative Sentiment: ", gdal.states2$`Ave_Sentiment_per_Author`, sep = ""),
                                            paste("State: ", gdal.states2$STE_NAME16, sep = "")) %>%
                                lapply(htmltools::HTML),
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>%
              addLegend("bottomright", pal = pal, values = ~average_sentiment_bins,
                        title = "Average Sentiment",
                        opacity = 1)
        }
    })

        
    output$leaflet_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = 134.48, lat = -25.73, zoom = 5)
    })
    
    
    }
    )
    
shinyApp(ui, server) 

