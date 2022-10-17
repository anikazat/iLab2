#### SHINY APP ####

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Animal Logic Productivity & Efficiency Analysis",
    
    ### PAGE 1: OVERVIEW ###
    tabPanel("Overview",
             mainPanel(
               # Header 1
               h1("Overview - All Departments"),
               # Header 2
               h4("Count of Reviews"),
               # Output: plot1
               plotOutput(outputId = "plot1"),
               # Header 3
               h4("Value of Reviews"),
               # Output: plot2
               plotOutput(outputId = "plot2")
             ) #mainPanel   
    ), #tabPanel
    
    ### PAGE 2: INDIVIDUAL DEPARTMENTS ###
    tabPanel("Individual Departments", 
             sidebarPanel(
               # Input: Select Department
               selectInput("department", label = "Select Department:", 
                           choices = departments_list, 
                           selected = "Assets"), width = 3
               ), #sidebarPanel
             mainPanel(
               # Header 1
               h1(textOutput(outputId = "txtout")),
               # Header 2
               h3("Count of Reviews Per Month"),
               # Output: plot3 & plot3b
               splitLayout(cellWidths = c("50%", "50%"), 
                           plotOutput(outputId = "plot3"),
                           plotOutput(outputId = "plot3b")),
               # Header 3
               h3("Days Logged Per Month"),
               # Output: plot4
               plotOutput(outputId = "plot4"),
               # Header 4
               h3("Value of Versions Per Month"),
               # Output: plot5
               plotOutput(outputId = "plot5")
             ) #mainPanel
    ), #tabPanel
    
    ### PAGE 3: REVIEW RATIOS ###
    tabPanel("Comparing Reviews",
             mainPanel(
               # Header 1
               h1("Comparing Director Reviews to Internal Reviews"),
               # Header 2
               h4("Average Number of Reviews Per Shot"),
               # Output: plot6b
               plotOutput(outputId = "plot6b"),
               # Output: plot6
               h4("Average Ratio of Director Reviews to Internal Reviews"),
               plotOutput(outputId = "plot6")
             ) #mainPanel
    ), #tabPanel
    
    ### PAGE 4: LINEAR MODELS ###
    tabPanel("Models - Director Reviews",
             sidebarPanel(
               # Input: Select Department
               selectInput("department2", label = "Select Department:", 
                           choices = departments_list, 
                           selected = "Assets"), 
               width = 2
               ), #sidebarPanel
             mainPanel( 
               # Header 1
               h1("Model to Predict Director Review Count"),
               # Header 2
               h3(textOutput(outputId = "txtout2")),
               fluidRow(
                 # Output: linearplot1 & modelSummary
                 splitLayout(cellWidths = c("50%", "50%"), 
                             plotOutput(outputId = "linearplot1"), 
                             verbatimTextOutput(outputId = "modelSummary"))
               ), #fluidRow
               # Output: linearplot2
               plotOutput(outputId = "linearplot2")
             ) #mainPanel 
    ), #tabPanel
    
    ### PAGE 5: LINEAR MODELS ###
    tabPanel("Models - Value",
             sidebarPanel(
               # Input: Select Department
               selectInput("department3", label = "Select Department:", 
                           choices = departments_list, 
                           selected = "Assets"), width = 2
               ), #sidebarPanel
             mainPanel( 
               # Header 1
               h1("Model to Predict Value"),
               # Header 2
               h3(textOutput(outputId = "txtout3")),
               fluidRow(
                 # Output: linearplot3 & modelSummary2
                 splitLayout(cellWidths = c("50%", "50%"),
                             plotOutput(outputId = "linearplot3"),
                             verbatimTextOutput(outputId = "modelSummary2"))
               ), #fluidRow
               # Output: linearplot4
               plotOutput(outputId = "linearplot4")
             ) #mainPanel
    ) # tabPanel
  ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  # Output: plot1
  output$plot1 <- renderPlot({
    df_versions %>% 
      filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>% 
      # filters to include only internal and director reviews - REMOVES SURPLUS REVIEWS
      filter(dirreviewed == "yes" | intreviewed == "yes") %>% 
      group_by(month, department_label) %>%
      count(month) %>% 
      ggplot(aes(month, n)) +
      geom_line(aes(col = department_label)) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
      labs(title = "Count of Reviews (Internal & Director) For Each Department", 
           y = "Count of Reviews", x = "Month", colour = "Department Name:")
  })
  
  # Output: plot2
  output$plot2 <- renderPlot({
    test <- df_versions
    test$weight <- ifelse(test$dirreviewed == "yes", 1, ifelse(test$intreviewed == "yes", 0.75, 0.01))
    test %>% 
      filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>% 
      group_by(month, department_label) %>%
      summarise(value = sum(weight)) %>% 
      ggplot(aes(month, value)) +
      geom_line(aes(col = department_label)) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
      labs(title = "Value of Versions For Each Department", 
           subtitle = "Internal Reviewed, Director Reviewed & Not Reviewed", 
           y = "Value of Versions", x = "Month", colour = "Department Name:")
  })
  
  # Output: txtout
  output$txtout <- renderText({
    paste( "Department:", input$department, sep = " " )
  })
  
  # Output: plot3
  output$plot3 <- renderPlot({
    data_a <- df_versions %>% 
      filter(department_label == input$department & intreviewed == "yes") %>% 
      group_by(month, department_label) %>% 
      summarise(total_count = sum(count))
    data_b <- df_versions %>% 
      filter(department_label == input$department & dirreviewed == "yes") %>% 
      group_by(month, department_label) %>% 
      summarise(total_count = sum(count))
    data_c <- df_versions %>% 
      filter(department_label == input$department & dirreviewed == "no" & intreviewed == "no") %>% 
      group_by(month, department_label) %>% 
      summarise(total_count = sum(count))
    
    data_a$review_type <- "internal"
    data_b$review_type <- "director"
    data_c$review_type <- "surplus"
    
    data_d <- rbind(data_a, data_b, data_c)
    
    data_d %>% 
      ggplot(aes(month, total_count)) +
      geom_line(aes(col = review_type)) +
      scale_colour_manual(values = c("blue", "black", "grey")) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
      labs(title = "Count of Internal, Director & Surplus Reviews Per Month", 
           subtitle = print(paste("Department:", input$department)),  x = "Month", y = "Count", 
           colour = "Review Type:")
  })
  
  # Output: plot3b
  output$plot3b <- renderPlot({
    data_a <- df_versions %>% 
      filter(department_label == input$department & intreviewed == "yes") %>% 
      group_by(month, department_label) %>% 
      summarise(total_count = sum(count))
    data_b <- df_versions %>% 
      filter(department_label == input$department & dirreviewed == "yes") %>% 
      group_by(month, department_label) %>% 
      summarise(total_count = sum(count))
    
    data_a$review_type <- "internal"
    data_b$review_type <- "director"
    
    data_e <- rbind(data_a, data_b)
    
    data_e %>% 
      ggplot(aes(month, total_count)) +
      geom_line(aes(col = review_type)) +
      scale_colour_manual(values = c("blue", "black")) +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
      labs(title = "Count of Internal & Director Reviews Per Month", 
           subtitle = print(paste("Department:", input$department)),
           x = "Month", y = "Count", colour = "Review Type:")
  })
  
  # Output: plot4
  output$plot4 <- renderPlot({
    df_timesheets %>% 
      filter(department_label == input$department) %>% 
      group_by(month) %>% 
      summarise(total_days_logged = sum(days_logged)) %>% 
      ggplot(aes(month, total_days_logged)) +
      geom_line() +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
      labs(title = "Number of Days Logged Per Month", subtitle = print(paste("Department:", input$department)),
           x = "Month", y = "Count")
  })
  
  # Output: plot5
  output$plot5 <- renderPlot({
    test <- df_versions
    test$weight <- ifelse(test$dirreviewed == "yes", 1, ifelse(test$intreviewed == "yes", 0.75, 0.01))
    test %>% 
      filter(department_label == input$department) %>% 
      group_by(month, department_label) %>%
      summarise(value = sum(weight)) %>% 
      ggplot(aes(month, value)) +
      geom_line() +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "top") +
      labs(title = "Value of Versions Per Month (Internal Reviewed, Director Reviewed & Not Reviewed)", 
           subtitle = print(paste("Department Name:", input$department)), y = "Value of Versions", x = "Month")
  })
  
  # Output: plot6
  output$plot6 <- renderPlot({
    #reviews_test <- df_versions
    #reviews_test$intreviewed_num <- ifelse(reviews_test$intreviewed == "yes", 1, 0)
    #reviews_test$dirreviewed_num <- ifelse(reviews_test$dirreviewed == "yes", 1, 0)
    
    # Internal Reviews
    #compare_int_reviews <- reviews_test %>% 
    #  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
    #  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>%
    #  # keep only production shots
    #  #filter(!is.na(shot) & shot != "" & !str_detect(shot, '^z')) %>% 
    #  # remove rows with no department label
    #  filter(!is.na(department_label)) %>% 
    #  # group by shot and department label
    #  group_by(shot, department_label) %>% 
    #  # get count of reviews for each shot for each department
    #  summarise(review_count = sum(intreviewed_num))
    
    ## Director Reviews
    #compare_dir_reviews <- reviews_test %>% 
    #  # to only include main departments - REMOVE THIS IF YOU WANT TO SEE ALL DEPARTMENTS
    #  filter(department_label %in% c("Animation", "Assets", "Lighting", "Character FX", "FX", "Layout")) %>%
    #  # keep only production shots
    #  #filter(!is.na(shot) & shot != "" & !str_detect(shot, '^z')) %>% 
    #  # remove rows with no department label
    #  filter(!is.na(department_label)) %>% 
    #  # group by shot and department label
    #  group_by(shot, department_label) %>% 
    #  # get count of reviews for each shot for each department
    #  summarise(review_count = sum(dirreviewed_num))
    
    # Get ratio of director to internal reviews
    compare_reviews_a <- compare_int_reviews %>% 
      # filter to remove versions that were never reviewed
      filter(review_count > 0) %>% 
      # group by department label
      group_by(department_label) %>% 
      # get average number of reviews per shot for each department
      summarise(avg_int_reviews_per_shot = mean(review_count))
    compare_reviews_b <- compare_dir_reviews %>% 
      # filter to remove versions that were never reviewed
      filter(review_count > 0) %>% 
      # group by department label
      group_by(department_label) %>% 
      # get average number of reviews per shot for each department
      summarise(avg_dir_reviews_per_shot = mean(review_count))
    # join internal and director tables
    compare_reviews_c <- left_join(x=compare_reviews_a, y=compare_reviews_b, by="department_label")
    # create column for ratio of director to internal reviews
    compare_reviews_c$ratio <- compare_reviews_c$avg_dir_reviews_per_shot / compare_reviews_c$avg_int_reviews_per_shot
    # plot
    compare_reviews_c %>% 
      ggplot(aes(department_label, ratio)) +
      geom_col(fill = "#0c024d") +
      theme_minimal() +
      labs(title = "Average Ratio of Director Reviews to Internal Reviews", x = "Department", y = "Ratio")
  })
  
  # Output: plot6b
  output$plot6b <- renderPlot({
    final_plot_06 <- compare_int_reviews %>% 
      # filter to remove versions that were never reviewed
      filter(review_count > 0) %>% 
      # group by department label
      group_by(department_label) %>% 
      # get average number of reviews per shot for each department
      summarise(avg_reviews_per_shot = mean(review_count)) %>% 
      ggplot(aes(department_label, avg_reviews_per_shot)) +
      geom_col(fill = "#0c024d") +
      scale_y_continuous(limits = c(0, 11)) +
      theme_minimal() +
      labs(title = "Average Number of Internal Reviews Per Shot", 
           x = "Department", y = "Average Number of Reviews")
    final_plot_07 <- compare_dir_reviews %>% 
      # filter to remove versions that were never reviewed
      filter(review_count > 0) %>% 
      # group by department label
      group_by(department_label) %>% 
      # get average number of reviews per shot for each department
      summarise(avg_reviews_per_shot = mean(review_count)) %>% 
      ggplot(aes(department_label, avg_reviews_per_shot)) +
      geom_col(fill = "#0c024d") +
      scale_y_continuous(limits = c(0, 11)) +
      theme_minimal() +
      labs(title = "Average Number of Director Reviews Per Shot", 
           x = "Department", y = "Average Number of Reviews")
    grid.arrange(final_plot_06, final_plot_07, ncol = 2, nrow = 1)
  })
  
  # Output: txtout2
  output$txtout2 <- renderText({
    paste( "Department:", input$department2, sep = " " )
  })
  
  # Output: linearplot1
  output$linearplot1 <- renderPlot({
    df_dr <- temp_v %>% filter(department_label == input$department2 & intreviewed=="yes" & dirreviewed=="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","dirreviewed_count","total_days_logged_dr","dirreviewed_version_size"))
    df_ir <- temp_v %>% filter(department_label == input$department2 & intreviewed=="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","intreviewed_count","total_days_logged_ir","intreviewed_version_size"))
    df_nr <- temp_v %>% filter(department_label == input$department2 & intreviewed !="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","notreviewed_count","total_days_logged_nr","notreviewed_version_size"))
    df_department <- df_ir %>%
      left_join(y=df_dr,by=c("month"="month","department"="department")) %>%
      left_join(y=df_nr,by=c("month"="month","department"="department")) %>%
      subset(select=c(month,department,notreviewed_count,notreviewed_version_size,intreviewed_count,intreviewed_version_size,dirreviewed_count,dirreviewed_version_size,total_days_logged_dr)) %>%
      setnames(c("month","department","notreviewed_count","notreviewed_version_size","intreviewed_count","intreviewed_version_size","dirreviewed_count","dirreviewed_version_size","total_days_logged"))
    df_department[is.na(df_department)]<- 0
    df_department_value <- df_department %>% rowwise() %>%
      mutate(not_reviewed_value = notreviewed_count*0.01, 
             internal_review_value = intreviewed_count*0.75,
             dirrector_review_value = dirreviewed_count*1,
             total_version_size = notreviewed_version_size+intreviewed_version_size+dirreviewed_version_size,
             total_value = not_reviewed_value+internal_review_value+dirrector_review_value,
             avg_productivity = total_value/total_days_logged)
    df_department_value <- subset(df_department_value, select = c("month","department","notreviewed_count","notreviewed_version_size","not_reviewed_value","intreviewed_count","intreviewed_version_size","internal_review_value","dirreviewed_count","dirreviewed_version_size","dirrector_review_value","total_version_size","total_value","total_days_logged","avg_productivity"))
    lm_department <- lm(dirreviewed_count ~ notreviewed_count 
                        + notreviewed_version_size + intreviewed_count 
                        + intreviewed_version_size + total_days_logged, 
                        data = df_department_value)
    par(mfrow=c(2,2))
    plot(lm_department)
  })
  
  # Output: modelSummary
  output$modelSummary <- renderPrint({
    df_dr <- temp_v %>% filter(department_label == input$department2 & intreviewed=="yes" & dirreviewed=="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","dirreviewed_count","total_days_logged_dr","dirreviewed_version_size"))
    df_ir <- temp_v %>% filter(department_label == input$department2 & intreviewed=="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","intreviewed_count","total_days_logged_ir","intreviewed_version_size"))
    df_nr <- temp_v %>% filter(department_label == input$department2 & intreviewed !="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","notreviewed_count","total_days_logged_nr","notreviewed_version_size"))
    df_department <- df_ir %>%
      left_join(y=df_dr,by=c("month"="month","department"="department")) %>%
      left_join(y=df_nr,by=c("month"="month","department"="department")) %>%
      subset(select=c(month,department,notreviewed_count,notreviewed_version_size,intreviewed_count,intreviewed_version_size,dirreviewed_count,dirreviewed_version_size,total_days_logged_dr)) %>%
      setnames(c("month","department","notreviewed_count","notreviewed_version_size","intreviewed_count","intreviewed_version_size","dirreviewed_count","dirreviewed_version_size","total_days_logged"))
    df_department[is.na(df_department)]<- 0
    df_department_value <- df_department %>% rowwise() %>%
      mutate(not_reviewed_value = notreviewed_count*0.01, 
             internal_review_value = intreviewed_count*0.75,
             dirrector_review_value = dirreviewed_count*1,
             total_version_size = notreviewed_version_size+intreviewed_version_size+dirreviewed_version_size,
             total_value = not_reviewed_value+internal_review_value+dirrector_review_value,
             avg_productivity = total_value/total_days_logged)
    df_department_value <- subset(df_department_value, select = c("month","department","notreviewed_count","notreviewed_version_size","not_reviewed_value","intreviewed_count","intreviewed_version_size","internal_review_value","dirreviewed_count","dirreviewed_version_size","dirrector_review_value","total_version_size","total_value","total_days_logged","avg_productivity"))
    lm_department <- lm(dirreviewed_count ~ notreviewed_count 
                        + notreviewed_version_size + intreviewed_count 
                        + intreviewed_version_size + total_days_logged, 
                        data = df_department_value)
    summary(lm_department)
  })
  
  # Output: linearplot2
  output$linearplot2 <- renderPlot({
    df_dr <- temp_v %>% filter(department_label == input$department2 & intreviewed=="yes" & dirreviewed=="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","dirreviewed_count","total_days_logged_dr","dirreviewed_version_size"))
    df_ir <- temp_v %>% filter(department_label == input$department2 & intreviewed=="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","intreviewed_count","total_days_logged_ir","intreviewed_version_size"))
    df_nr <- temp_v %>% filter(department_label == input$department2 & intreviewed !="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","notreviewed_count","total_days_logged_nr","notreviewed_version_size"))
    df_department <- df_ir %>%
      left_join(y=df_dr,by=c("month"="month","department"="department")) %>%
      left_join(y=df_nr,by=c("month"="month","department"="department")) %>%
      subset(select=c(month,department,notreviewed_count,notreviewed_version_size,intreviewed_count,intreviewed_version_size,dirreviewed_count,dirreviewed_version_size,total_days_logged_dr)) %>%
      setnames(c("month","department","notreviewed_count","notreviewed_version_size","intreviewed_count","intreviewed_version_size","dirreviewed_count","dirreviewed_version_size","total_days_logged"))
    df_department[is.na(df_department)]<- 0
    df_department_value <- df_department %>% rowwise() %>%
      mutate(not_reviewed_value = notreviewed_count*0.01, 
             internal_review_value = intreviewed_count*0.75,
             dirrector_review_value = dirreviewed_count*1,
             total_version_size = notreviewed_version_size+intreviewed_version_size+dirreviewed_version_size,
             total_value = not_reviewed_value+internal_review_value+dirrector_review_value,
             avg_productivity = total_value/total_days_logged)
    df_department_value <- subset(df_department_value, select = c("month","department","notreviewed_count","notreviewed_version_size","not_reviewed_value","intreviewed_count","intreviewed_version_size","internal_review_value","dirreviewed_count","dirreviewed_version_size","dirrector_review_value","total_version_size","total_value","total_days_logged","avg_productivity"))
    department_time_spent.graph <- ggscatter(df_department_value, x = "total_days_logged", y = "dirreviewed_count", 
                                             add = "reg.line", conf.int = TRUE, 
                                             cor.coef = TRUE, cor.method = "pearson",
                                             xlab = "Number of Days Spent", ylab = "Number of Director Reviews")
    department_ir.graph <- ggscatter(df_department_value, x = "intreviewed_count", y = "dirreviewed_count", 
                                     add = "reg.line", conf.int = TRUE, 
                                     cor.coef = TRUE, cor.method = "pearson",
                                     xlab = "Number of Internal reviews", ylab = "Number of Director Reviews")
    department_ir_bytes.graph <- ggscatter(df_department_value, x = "intreviewed_version_size", y = "dirreviewed_count", 
                                           add = "reg.line", conf.int = TRUE, 
                                           cor.coef = TRUE, cor.method = "pearson",
                                           xlab = "Version Size of Internal reviews", ylab = "Number of Director Reviews")
    department_nr.graph <- ggscatter(df_department_value, x = "notreviewed_count", y = "dirreviewed_count", 
                                     add = "reg.line", conf.int = TRUE, 
                                     cor.coef = TRUE, cor.method = "pearson",
                                     xlab = "Number of Not Reviewed", ylab = "Number of Director Reviews")
    department_nr_bytes.graph <- ggscatter(df_department_value, x = "notreviewed_version_size", y = "dirreviewed_count", 
                                           add = "reg.line", conf.int = TRUE, 
                                           cor.coef = TRUE, cor.method = "pearson",
                                           xlab = "Version Size of Not Reviewed", ylab = "Number of Director Reviews")
    ggarrange(department_time_spent.graph, department_ir.graph, department_ir_bytes.graph, department_nr.graph, department_nr_bytes.graph,
              labels = c(print(paste(input$department2, ": Number of Days Logged vs Director Reviews")),
                         print(paste(input$department2, ": Number of Internal Reviews vs Director Reviews")),
                         print(paste(input$department2, ": Internal Review Version Size vs Director Reviews")),
                         print(paste(input$department2, ": Number of Surplus Version vs Director Reviews")),
                         print(paste(input$department2, ": Surplus Version Size vs Director Reviews"))), 
              ncol = 2, nrow = 3)
  })
  
  # Output: textout3
  output$txtout3 <- renderText({
    paste( "Department:", input$department3, sep = " " )
  })
  
  # Output: linearplot3
  output$linearplot3 <- renderPlot({
    df_dr <- temp_v %>% filter(department_label == input$department3 & intreviewed=="yes" & dirreviewed=="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","dirreviewed_count","total_days_logged_dr","dirreviewed_version_size"))
    df_ir <- temp_v %>% filter(department_label == input$department3 & intreviewed=="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","intreviewed_count","total_days_logged_ir","intreviewed_version_size"))
    df_nr <- temp_v %>% filter(department_label == input$department3 & intreviewed !="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","notreviewed_count","total_days_logged_nr","notreviewed_version_size"))
    df_department <- df_ir %>%
      left_join(y=df_dr,by=c("month"="month","department"="department")) %>%
      left_join(y=df_nr,by=c("month"="month","department"="department")) %>%
      subset(select=c(month,department,notreviewed_count,notreviewed_version_size,intreviewed_count,intreviewed_version_size,dirreviewed_count,dirreviewed_version_size,total_days_logged_dr)) %>%
      setnames(c("month","department","notreviewed_count","notreviewed_version_size","intreviewed_count","intreviewed_version_size","dirreviewed_count","dirreviewed_version_size","total_days_logged"))
    df_department[is.na(df_department)]<- 0
    df_department_value <- df_department %>% rowwise() %>%
      mutate(not_reviewed_value = notreviewed_count*0.01, 
             internal_review_value = intreviewed_count*0.75,
             dirrector_review_value = dirreviewed_count*1,
             total_version_size = notreviewed_version_size+intreviewed_version_size+dirreviewed_version_size,
             total_value = not_reviewed_value+internal_review_value+dirrector_review_value,
             avg_productivity = total_value/total_days_logged)
    df_department_value <- subset(df_department_value, select = c("month","department","notreviewed_count","notreviewed_version_size","not_reviewed_value","intreviewed_count","intreviewed_version_size","internal_review_value","dirreviewed_count","dirreviewed_version_size","dirrector_review_value","total_version_size","total_value","total_days_logged","avg_productivity"))
    lm_department <- lm(dirreviewed_count ~ notreviewed_count 
                        + notreviewed_version_size + intreviewed_count 
                        + intreviewed_version_size + total_days_logged, 
                        data = df_department_value)
    # fit lm to value table
    df_department_value $avg_value_per_hour <- ifelse(is.infinite(df_department_value$avg_productivity),NA,df_department_value$avg_productivity)
    lm_department_value <- lm(total_value ~ total_days_logged + total_version_size, data = df_department_value)
    par(mfrow=c(2,2))
    plot(lm_department_value)
  })
  
  # Output: modelSummary
  output$modelSummary2 <- renderPrint({
    df_dr <- temp_v %>% filter(department_label == input$department3 & intreviewed=="yes" & dirreviewed=="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","dirreviewed_count","total_days_logged_dr","dirreviewed_version_size"))
    df_ir <- temp_v %>% filter(department_label == input$department3 & intreviewed=="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","intreviewed_count","total_days_logged_ir","intreviewed_version_size"))
    df_nr <- temp_v %>% filter(department_label == input$department3 & intreviewed !="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","notreviewed_count","total_days_logged_nr","notreviewed_version_size"))
    df_department <- df_ir %>%
      left_join(y=df_dr,by=c("month"="month","department"="department")) %>%
      left_join(y=df_nr,by=c("month"="month","department"="department")) %>%
      subset(select=c(month,department,notreviewed_count,notreviewed_version_size,intreviewed_count,intreviewed_version_size,dirreviewed_count,dirreviewed_version_size,total_days_logged_dr)) %>%
      setnames(c("month","department","notreviewed_count","notreviewed_version_size","intreviewed_count","intreviewed_version_size","dirreviewed_count","dirreviewed_version_size","total_days_logged"))
    df_department[is.na(df_department)]<- 0
    df_department_value <- df_department %>% rowwise() %>%
      mutate(not_reviewed_value = notreviewed_count*0.01, 
             internal_review_value = intreviewed_count*0.75,
             dirrector_review_value = dirreviewed_count*1,
             total_version_size = notreviewed_version_size+intreviewed_version_size+dirreviewed_version_size,
             total_value = not_reviewed_value+internal_review_value+dirrector_review_value,
             avg_productivity = total_value/total_days_logged)
    df_department_value <- subset(df_department_value, select = c("month","department","notreviewed_count","notreviewed_version_size","not_reviewed_value","intreviewed_count","intreviewed_version_size","internal_review_value","dirreviewed_count","dirreviewed_version_size","dirrector_review_value","total_version_size","total_value","total_days_logged","avg_productivity"))
    lm_department <- lm(dirreviewed_count ~ notreviewed_count 
                        + notreviewed_version_size + intreviewed_count 
                        + intreviewed_version_size + total_days_logged, 
                        data = df_department_value)
    # fit lm to value table
    df_department_value $avg_value_per_hour <- ifelse(is.infinite(df_department_value$avg_productivity),NA,df_department_value$avg_productivity)
    lm_department_value <- lm(total_value ~ total_days_logged + total_version_size, data = df_department_value)
    summary(lm_department_value)
  })
  
  # Output: linearplot4
  output$linearplot4 <- renderPlot({
    df_dr <- temp_v %>% filter(department_label == input$department3 & intreviewed=="yes" & dirreviewed=="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","dirreviewed_count","total_days_logged_dr","dirreviewed_version_size"))
    df_ir <- temp_v %>% filter(department_label == input$department3 & intreviewed=="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","intreviewed_count","total_days_logged_ir","intreviewed_version_size"))
    df_nr <- temp_v %>% filter(department_label == input$department3 & intreviewed !="yes" & dirreviewed !="yes") %>%
      left_join(y=temp_ts,by=c("month"="month","department_label"="department_label")) %>%
      subset(select=c(month,department_label,review_count,total_days_logged,total_bytes)) %>%
      setnames(c("month","department","notreviewed_count","total_days_logged_nr","notreviewed_version_size"))
    df_department <- df_ir %>%
      left_join(y=df_dr,by=c("month"="month","department"="department")) %>%
      left_join(y=df_nr,by=c("month"="month","department"="department")) %>%
      subset(select=c(month,department,notreviewed_count,notreviewed_version_size,intreviewed_count,intreviewed_version_size,dirreviewed_count,dirreviewed_version_size,total_days_logged_dr)) %>%
      setnames(c("month","department","notreviewed_count","notreviewed_version_size","intreviewed_count","intreviewed_version_size","dirreviewed_count","dirreviewed_version_size","total_days_logged"))
    df_department[is.na(df_department)]<- 0
    df_department_value <- df_department %>% rowwise() %>%
      mutate(not_reviewed_value = notreviewed_count*0.01, 
             internal_review_value = intreviewed_count*0.75,
             dirrector_review_value = dirreviewed_count*1,
             total_version_size = notreviewed_version_size+intreviewed_version_size+dirreviewed_version_size,
             total_value = not_reviewed_value+internal_review_value+dirrector_review_value,
             avg_productivity = total_value/total_days_logged)
    df_department_value <- subset(df_department_value, select = c("month","department","notreviewed_count","notreviewed_version_size","not_reviewed_value","intreviewed_count","intreviewed_version_size","internal_review_value","dirreviewed_count","dirreviewed_version_size","dirrector_review_value","total_version_size","total_value","total_days_logged","avg_productivity"))
    department_value.graph <- ggscatter(df_department_value, x = "total_days_logged", y = "total_value", 
                                        add = "reg.line", conf.int = TRUE, 
                                        cor.coef = TRUE, cor.method = "pearson",
                                        xlab = "Number of Days Spent", ylab = "Total Value") #+xlim = c(0, 800))
    department_bytes.graph <- ggscatter(df_department_value, x = "total_version_size", y = "total_value", 
                                        add = "reg.line", conf.int = TRUE, 
                                        cor.coef = TRUE, cor.method = "pearson",
                                        xlab = "Total Size of Versions", ylab = "Total Value") #+xlim = c(0, 800))
    ggarrange(department_value.graph, department_bytes.graph,
              labels = c(print(paste(input$department3, ": Total Days Logged vs Total Value")),
                         print(paste(input$department3, ": Total Size of Versions Produced vs Total Value"))),
              ncol = 2, nrow = 1)
    
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)


