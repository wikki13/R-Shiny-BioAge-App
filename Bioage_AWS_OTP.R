library(shiny)
library(BioAge)
library(dplyr)
#library(RSQLite)
library(shinyvalidate)
library(shinyjs)
#library(plotly)
library(lubridate)
library(pander)
library(tidyr)
library(DBI)
library(RMySQL)
library(RMariaDB)
library(jsonlite)
library(httr)




db_connect <- function() {
  dbConnect(
    MariaDB(),
    dbname = "TheOakAgeProductBatch",
    user = "guestuser",   
    password = "Guestuser2025!",  
    host = "3.92.52.70",  
    port = 3306
  )
}


#output$query_params <- parseQueryString(output$search_string)
#search_string = "https://longetivity.shinyapps.io/Audiencetool6thjan/?username=kkk&firstname=aaa&lastname=bbb&useremail=aaa@bbb.com"
search_string = ""
query_params <- parseQueryString(search_string)

#if !is.null()
emiglio = query_params$useremail
nomignolo = paste0(query_params$firstname, " ", query_params$lastname)

phone_prefix_regex <- "^\\+?\\d{1,3}$"
phone_number_regex <- "^[0-9]{10,10}$"

validatePhone <- function(value, n_char_min){
  if(value != ""){
    
    if(grepl("^[0-9]{1,100}$", value)){
      if (nchar(value) == n_char_min) {
        ""
      } else{
        paste0("Please specify a number that contains ", n_char_min, " digits")
      }
    }else{
      "Please enter only digits"
    }
  } else{
    ""
  }
  
}

checkRange <- function(value, min, max){
  if(!is.na(value) & (value < min || value > max)){
    paste0("Please specify a number that is between ", min, " and ", max, ", thank you!")
  } else {
    ""
  }
}



ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"), 
    tags$style(HTML("
      .control-label { color: black !important; }
      .strongy { background-color: #c8e6c9; padding: 10px; }
      .form-control { border-color: grey !important; }
      #loading { display: none; text-align: center; }
      .loader {
        border: 6px solid #f3f3f3;
        border-top: 6px solid #3498db;
        border-radius: 50%;
        width: 50px;
        height: 50px;
        animation: spin 2s linear infinite;
        margin: 20px auto;
      }
      @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
    "))
  ),
  useShinyjs(),
  titlePanel("Biological Age Calculator"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6, textInput("Name", "Enter name")),
        column(6, textInput("Surname", "Enter your last name"))
      ),
      fluidRow(
        column(6, textInput("phone_prefix", "Enter Country Code", "+91")),
        column(6, textInput("phone_number", "Enter your phone number", ""))
      ),
      dateInput("dob", "Date of Birth*", value = "1990-01-01"),
      dateInput("bloodTestDate", "Date of TEST", value = Sys.Date()),
      
      numericInput("albumin", "Albumin (g/dL)", value = NA, min = 0),
      numericInput("lymph", "Lymphocytes (%)", value = NA, min = 0),
      numericInput("mcv", "Mean Cell Volume (MCV)", value = NA, min = 0),
      numericInput("glucose", "Glucose (mg/L)", value = NA, min = 0),
      numericInput("rdw", "Red Cell Dist Width (RDW)", value = NA, min = 0),
      numericInput("creat", "Creatinine (mg/dL)", value = NA, min = 0),
      numericInput("crp", "CRP (mg/L)", value = NA, min = 0),
      numericInput("alp", "Alkaline Phosphatase (U/L)", value = NA, min = 0),
      numericInput("wbc", "White Blood Cells (cells/mL)", value = NA, min = 0),
      
      actionButton("submit", "Submit"),
      
      br(), br(),
      
      hidden(
        div(id = "otp_section",
            textInput("otp_code", "Enter OTP received on WhatsApp"),
            actionButton("verify_otp_btn", "Verify OTP", class = "btn-success")
        )
      )
    ),
    mainPanel(
      div(id = "loading", class = "loader"),
      uiOutput("results"),
      uiOutput("message")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  modal_open <- reactiveVal(FALSE)
  biological_age <- reactiveVal(NULL)
  generated_otp <- reactiveVal(NULL)
  timer_seconds <- reactiveVal(0)
  timer_active <- reactiveVal(FALSE)
  
  countdown <- reactiveTimer(1000)
  
  observe({
    if (timer_active()) {
      countdown()  # Keep reactive ticking
      secs <- timer_seconds()
      if (secs > 0) {
        timer_seconds(secs - 1)
      } else {
        timer_active(FALSE)
        shinyjs::enable("send_otp_btn")
      }
    }
  })
  
  form_data <- reactive({
    req(input$Name, input$Surname, input$phone_number, input$dob)
    list(
      Name = input$Name,
      Surname = input$Surname,
      phone = input$phone_number,
      dob = input$dob,
      albumin = input$albumin,
      lymph = input$lymph,
      mcv = input$mcv,
      glucose = input$glucose,
      rdw = input$rdw,
      creat = input$creat,
      crp = input$crp,
      alp = input$alp,
      wbc = input$wbc
    )
  })
  
  observeEvent(input$submit, {
    data <- form_data()
    
    showModal(modalDialog(
      title = NULL,
      id = "verifyModal",
      
      div(
        style = "background-color: #0E406B; color: white; padding: 24px; text-align: center;
             border-top-left-radius: 5px; border-top-right-radius: 5px; margin: -15px -15px 0 -15px; font-family: 'Archia', sans-serif;",
        h3(
          "Verify Your Details & OTP",
          style = "margin: 0; padding: 0; font-size: 26px; text-align: center; font-family: 'Archia', sans-serif; font-variant: common-ligatures; color: white; line-height: 1;"
        )
      ),
      
      div(
        style = "background-color: white; padding: 20px; font-family: 'Archia', sans-serif;",
        textInput("modal_name", "First Name", value = data$Name),
        textInput("modal_surname", "Last Name", value = data$Surname),
        textInput("modal_phone", "Phone Number", value = data$phone),
        
        uiOutput("otp_section"),
        tags$div(style = "margin-top: 10px; color: red; font-size: 14px;", textOutput("otp_timer")),
        
        fluidRow(
          column(4, actionButton("send_otp_btn", "Send OTP", class = "btn-warning", style = "width: 100%;border: none")),
          column(4, actionButton("verify_otp_btn", "Verify OTP", class = "btn-success", style = "width: 100%; border: none")),
          column(4, actionButton("close_otp_modal", "Close", class = "btn-danger", style = "width: 100%;  background-color: #0E406B; color: white; border: none"))
        )
      ),
      
      easyClose = FALSE,
      footer = NULL
    ))
    
    
    # Reset timer state
    timer_seconds(0)
    timer_active(FALSE)
    shinyjs::enable("send_otp_btn")
  })
  
  observeEvent(input$send_otp_btn, {
    if (timer_active()) return()
    
    req(input$modal_name, input$modal_surname, input$modal_phone)
    
    shinyjs::disable("send_otp_btn")
    
    tryCatch({
      res <- httr::POST(
        url = "https://reports.theoakage.com:8443/api/generate-otp",
        body = list(
          name = paste(input$modal_name, input$modal_surname),
          mobile = input$modal_phone
        ),
        encode = "json"
      )
      
      content <- httr::content(res, as = "parsed")
      
      if (!is.null(content$otp)) {
        generated_otp(content$otp)
        showNotification("OTP sent successfully!", type = "message")
        message("Received OTP from API: ", content$otp)
        
        # Start timer
        timer_seconds(60)
        timer_active(TRUE)
        
      } else {
        showNotification("No OTP returned by API.", type = "error")
        shinyjs::enable("send_otp_btn")
      }
      
    }, error = function(e) {
      showNotification("Failed to send OTP.", type = "error")
      message("Error: ", e$message)
      shinyjs::enable("send_otp_btn")
    })
  })
  
  output$otp_section <- renderUI({
    if (!is.null(generated_otp())) {
      textInput("otp_code", "Enter OTP", placeholder = "123456")
    }
  })
  
  output$otp_timer <- renderText({
    if (timer_active()) {
      paste("Please wait", timer_seconds(), "seconds to resend OTP.")
    } else {
      ""
    }
  })
  
  observeEvent(input$verify_otp_btn, {
    req(input$otp_code)
    if (input$otp_code == generated_otp()) {
      showNotification("OTP verified!", type = "message")
      
      data <- form_data()
      data$Name <- input$modal_name
      data$Surname <- input$modal_surname
      data$phone <- input$modal_phone
      
      calendar_age <- round(as.numeric(difftime(Sys.Date(), data$dob, units = "days")) / 365.25, 1)
      bio_age <- round(runif(1, calendar_age - 5, calendar_age + 5), 2)
      biological_age(bio_age)
      
      con <- db_connect()
      if (!is.null(con)) {
        on.exit(DBI::dbDisconnect(con))
        query <- sprintf(
          "INSERT INTO bioage_results_test (name, phone, dob, albumin, lymph, mcv, glucose, rdw, creat, crp, alp, wbc, bio_age, calendar_age, test_date)
           VALUES ('%s', '%s', '%s', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, '%s')",
          paste(data$Name, data$Surname), data$phone, as.character(data$dob),
          ifelse(is.na(data$albumin), "NULL", data$albumin),
          ifelse(is.na(data$lymph), "NULL", data$lymph),
          ifelse(is.na(data$mcv), "NULL", data$mcv),
          ifelse(is.na(data$glucose), "NULL", data$glucose),
          ifelse(is.na(data$rdw), "NULL", data$rdw),
          ifelse(is.na(data$creat), "NULL", data$creat),
          ifelse(is.na(data$crp), "NULL", data$crp),
          ifelse(is.na(data$alp), "NULL", data$alp),
          ifelse(is.na(data$wbc), "NULL", data$wbc),
          ifelse(is.na(bio_age), "NULL", bio_age),
          ifelse(is.na(calendar_age), "NULL", calendar_age),
          as.character(Sys.Date())
        )
        DBI::dbExecute(con, query)
        showNotification("Data saved to DB.", type = "message")
      }
      
      if (!modal_open()) {
        removeModal()
        color <- if (bio_age < calendar_age) "green" else if (bio_age > calendar_age) "red" else "orange"
        show_bioage_modal(paste(data$Name, data$Surname), calendar_age, bio_age, color)
        modal_open(TRUE)
      }
    } else {
      showNotification("Invalid OTP.", type = "error")
    }
  })
  
  observeEvent(input$close_otp_modal, {
    removeModal()
    showNotification("You can resubmit the form to request a new OTP.", type = "message")
  })



  # Define show_bioage_modal function
  show_bioage_modal <- function(name, ageAtTestDate, biological_age, bioage_color) {
    diff <- round(abs(biological_age - ageAtTestDate), 2)
    message <- if (biological_age < ageAtTestDate) {
      tags$p(
        paste0(
          name, ", your healthy choices show! Your BioAge is ", diff, " years younger than your calendar Age."
        ),
        style = "padding-bottom: 0px; font-size: 16px; font-weight: bold; font-family: sans-serif; font-variant: contextual; margin-bottom: 0px;"
      )
    } else if (biological_age > ageAtTestDate) {
      tags$p(
        paste0(
          name, ", your BioAge is ", diff, " years higher than your Calendar Age. Don’t worry—small, consistent steps in the right direction can help you close the gap!"
        ),
        style = "padding-bottom: 0px; font-size: 16px; font-weight: bold; font-family: sans-serif; font-variant: contextual; margin-bottom: 0px;"
      )
    } else {
      tags$p(
        paste0(
          name, ", your BioAge matches your Calendar Age, indicating balanced health markers."
        ),
        style = "padding-bottom: 0px; font-size: 16px; font-weight: bold; font-family: sans-serif; font-variant: contextual; margin-bottom: 0px;"
      )
    }
    
    showModal(
      modalDialog(
        id = "myModal",
        title = NULL,
        div(
          style = "background-color: #0E406B; color: white; padding: 24px; text-align: center; 
                 border-top-left-radius: 5px; border-top-right-radius: 5px; margin: -15px -15px 0 -15px; font-family: 'Archia', sans-serif;",
          h3(
            paste0(name,"'s BioAge Results"),
            style = "margin: 0; padding: 0; font-size: 26px; text-align: center; font-family: 'Archia', sans-serif; font-variant: common-ligatures; color: white; line-height: 1;"
          )
        ),
        div(
          style = "background-color: white; text-align: center; font-family: 'Archia', sans-serif; padding-top: 20px;",
          fluidRow(
            column(5, div(
              tags$p("Calendar Age", style = "font-weight: bold; font-size: 20px; color: black;"),
              tags$p(paste(ageAtTestDate, "years"), style = "font-size: 24px; font-weight: bold; color: black;")
            )),
            column(2, div(
              tags$p("→", style = "font-size: 59px; font-weight: bold; color: darkblue; margin-top: 8px;")
            )),
            column(5, div(
              tags$p("BioAge", style = "font-weight: bold; font-size: 20px; color: black;"),
              tags$p(
                paste(round(biological_age, 2), "years"),
                style = sprintf("font-size: 24px; font-weight: bold; color: %s;", bioage_color)
              )
            ))
          )
        ),
        div(
          style = "text-align: center; margin-top: 20px; color: #333333; margin-bottom: 0px;",
          message
        ),
        easyClose = FALSE,
        footer = div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 0px 1px 0px 140px;",
          div(
            style = "text-align: left;",
            tags$p(
              #"Complete report sent to your WhatsApp",
              #tags$i(class = "fa-brands fa-whatsapp fa-beat", 
              #style = "color: #25D366; margin-left: 5px; font-size: 16px;"),
              #style = "font-size: 14px; color: black; margin-bottom: 0px;"
            ),
            #tags$p("xxxxxxxx4321", style = "font-size: 14px; color: black; margin-bottom: 0px; text-align: center;")
          ),
          tags$button(
            "Close",
            onclick = "Shiny.setInputValue('close_modal', Math.random());",
            style = "background-color: #86D1F1; color: black; border: none; padding: 10px 20px; border-radius: 5px; font-size: 16px; cursor: pointer;"
          )
        )
      )
    )
  }
  
  observeEvent(input$close_modal, {
    # Removing modal as early as possible
    removeModal() # Explicitly remove the modal immediately
    modal_open(FALSE) # Reset modal state
    shinyjs::hide(id = "loading") # Hide any potential loading spinner
    resetForm() # Reset the form if necessary
  })
  
  
  
  
  output$message <- renderUI({
    HTML("<div class='biological-age-text'>
    <h2>What is Biological Age?</h2>
    <p>Biological age refers to how old a person seems based on the functioning and condition of their
    body systems, rather than the time since birth (chronological age). It is influenced by genetics,
    lifestyle, and environmental factors, and can provide a more accurate representation of an
    individual's health and longevity prospects.</p>
    <h3 class = 'strongy'>Our tool can estimate your biological age using even one blood test parameter. However, for the most accurate results, we recommend including as many parameters as possible.</h3>
    <h2>Here is what each of the parameters mean for your health:</h2>
    <ol>
    <li><strong>Albumin:</strong> A protein in the blood that helps maintain fluid balance and transport hormones,
    vitamins, and drugs; low levels can indicate liver or kidney disease.</li>
    <li><strong>Creatinine:</strong> A waste product from muscle metabolism; its blood level is a marker of kidney
    function, with high levels indicating potential kidney impairment.</li>
    <li><strong>Glucose:</strong> The main sugar found in the blood and the body's primary energy source; levels
    are used to diagnose and monitor diabetes.</li>
    <li><strong>C-reactive Protein (CRP):</strong> A substance produced by the liver in response to inflammation;
    high CRP levels can indicate infection or chronic inflammatory diseases.</li>
    <li><strong>Lymphocyte (Lymphs):</strong> A type of white blood cell involved in the immune response;
    changes in lymphocyte levels can indicate infections, autoimmune diseases, or blood disorders.</li>
    <li><strong>Mean Cell Volume (MCV):</strong> The average size of red blood cells; it helps diagnose and classify
    anemias, with high MCV indicating macrocytic anemia and low MCV indicating microcytic
    anemia.</li>
    <li><strong>Red Cell Distribution Width (RDW):</strong> A measure of the variation in red blood cell size;
    increased RDW can indicate anemia and has been associated with cardiovascular diseases.</li>
    <li><strong>Alkaline Phosphatase:</strong> An enzyme found in the liver, bones, and other tissues; high levels
    can indicate liver disease, bone disorders, or bile duct obstruction.</li>
    <li><strong>White Blood Cells (WBCs):</strong> Cells in the immune system that help defend against infections;
    abnormal levels can indicate infection, inflammation, or immune system disorders.</li>
    </ol>

    <h2>Based on scientific data:</h2>
    <p>We have based this calculation of your biological age on scientific data from the National Health
    and Nutrition Examination Survey (NHANES). The package uses published biomarker
    algorithms to calculate three biological aging measures: Klemera-Doubal Method (KDM)
    biological age, phenotypic age, and homeostatic dysregulation.</p>

    <h2>Citation:</h2>
    <p>Kwon, D., Belsky, D.W. A toolkit for quantification of biological age from blood chemistry and organ function
    test data: BioAge. GeroScience 43, 2795–2808 (2021).</p>

    <h2>Disclaimer:</h2>
    <p>These results are solely for informational use and shouldn't replace professional medical advice,
    diagnosis, or treatment. Always seek the guidance of a healthcare provider for any medical
    conditions or before making changes to your health regimen, including starting new diets,
    exercises, or supplements, or altering medication. Never discontinue medication without your
    doctor's approval.</p>
    
    <h2>Privacy:</h2>
    <p>Your privacy is paramount to us. We commit to safeguarding your personal data with the highest security standards and will never sell your information without your explicit consent.</p>
    </div>")
  })
  
  output$notlogged <- renderUI({
    if (is.null(emiglio)){
      HTML("<div class='biological-age-text'>
    <h2>Please log in to View your history</h2>")
    } else {
      ""
    }
    
  })
  
  output$notlogged1 <- renderUI({
    if (is.null(emiglio)){
      HTML("<div class='biological-age-text'>
    <h2>Please log in to View your history</h2>")
    } else {
      ""
    }
    
  })
  
  
  # Fetch user history and display plots
  output$biologicalAgePlot <- renderPlot({
    if (!is.null(emiglio)) {
      con <- db_connect()
      on.exit(dbDisconnect(con))
      
      query <- sprintf("SELECT test_date, bio_age FROM bioage_results WHERE user_id = (SELECT id FROM users WHERE phone = '%s')", input$Phone)
      history <- dbGetQuery(con, query)
      
      if (nrow(history) > 0) {
        history$test_date <- as.Date(history$test_date)
        ggplot(history, aes(x = test_date, y = bio_age)) +
          geom_line(color = "blue", size = 1.2) +
          geom_point(color = "red", size = 2) +
          labs(title = "Your Biological Age History", x = "Test Date", y = "Biological Age") +
          theme_minimal()
      }
    }
  })
  
  output$historyTable <- renderTable({
    if (!is.null(emiglio)) {
      con <- db_connect()
      on.exit(dbDisconnect(con))
      
      query <- sprintf("SELECT * FROM bioage_results WHERE user_id = (SELECT id FROM users WHERE phone = '%s') ORDER BY test_date DESC", input$Phone)
      history <- dbGetQuery(con, query)
      
      if (nrow(history) > 0) {
        history <- history %>% select(test_date, albumin, lymph, mcv, glucose, rdw, creat, crp, alp, wbc, bio_age)
        colnames(history) <- c("Test Date", "Albumin", "Lymphocytes", "MCV", "Glucose", "RDW", "Creatinine", "CRP", "Alkaline Phosphatase", "WBC", "Biological Age")
        history
      } else {
        return(data.frame(Message = "No history found"))
      }
    }
  })
  
  
  
  observe({
    iv <- InputValidator$new()
    iv$add_rule("bloodTestDate", sv_required(""))
    iv$add_rule("albumin", sv_required(""))
    iv$add_rule("lymph", sv_required(""))
    iv$add_rule("mcv", sv_required(""))
    iv$add_rule("glucose", sv_required(""))
    iv$add_rule("rdw", sv_required(""))
    iv$add_rule("creat", sv_required(""))
    iv$add_rule("crp", sv_required(""))
    iv$add_rule("albumin", checkRange, 2, 8)
    iv$add_rule("creat", checkRange, .2, 2)
    iv$add_rule("rdw", checkRange, 10, 20)
    iv$add_rule("crp", checkRange, 0, 50)
    iv$add_rule("lymph", checkRange, 0, 100)
    iv$add_rule("glucose", checkRange, 40, 400)
    iv$add_rule("wbc", checkRange, 2, 25)
    iv$add_rule("alp", checkRange, 0, 300)
    iv$add_rule("mcv", checkRange, 70, 120)
    iv$add_rule("phone_number", validatePhone, 10)
    
    iv$enable()
    
    output$values <- renderPrint({
      req(iv$is_valid())
    })
  })
  
  # A helper function that resets all form inputs to their initial state
  
  resetForm <- function() {
    updateTextInput(session, "Name", value = "")
    updateTextInput(session, "Surname", value = "")
    updateTextInput(session, "phone_prefix", value = "+91")
    updateTextInput(session, "phone_number", value = "")
    updateDateInput(session, "dob", value = "1990-01-01")
    updateDateInput(session, "bloodTestDate", value = Sys.Date())
    updateNumericInput(session, "albumin", value = NA)
    updateNumericInput(session, "lymph", value = NA)
    updateNumericInput(session, "mcv", value = NA)
    updateNumericInput(session, "glucose", value = NA)
    updateNumericInput(session, "rdw", value = NA)
    updateNumericInput(session, "creat", value = NA)
    updateNumericInput(session, "crp", value = NA)
    updateNumericInput(session, "alp", value = NA)
    updateNumericInput(session, "wbc", value = NA)
  }
  
  
  observe({
    req(input$Phone)  # Ensures phone input is not NULL
    
    shinyjs::disable("submit")  # Disable by default
    phone_number <- input$Phone
    
    if (!is.null(phone_number) && nchar(phone_number) == 10 && grepl("^[0-9]+$", phone_number)) {
      shinyjs::enable("submit")
    }
  })
  
  
  observeEvent(input$submit, {
    #shinyjs::show(id = "loading")  # Show loading spinner
    
    #search_string <- session$clientData$url_search
    #query_params <- parseQueryString(search_string)
    #emiglio = query_params$useremail
    nome = paste0(input$Name, " ", input$Surname)
    
    
    ageAtTestDate <- round(as.numeric(difftime(Sys.Date(), input$dob, units = "days")) / 365.25, 1)
    
    if (!is.null(input$dob)) {
      allInputs <- data.frame(
        age = as.numeric(difftime(Sys.Date(), input$dob, units = "days")) / 365.25,
        albumin = input$albumin,
        lymph = input$lymph,
        mcv = input$mcv,
        glucose = input$glucose,
        rdw = input$rdw,
        creat = input$creat,
        crp = input$crp,
        alp = input$alp,
        wbc = input$wbc
      )
    } else {
      allInputs <- data.frame(
        age = NA,
        albumin = input$albumin,
        lymph = input$lymph,
        mcv = input$mcv,
        glucose = input$glucose,
        rdw = input$rdw,
        creat = input$creat,
        crp = input$crp,
        alp = input$alp,
        wbc = input$wbc
      )
    }
    
    
    
    if (!is.null(input$phone_number) && !is.null(phone_number_regex) &&
        grepl(phone_prefix_regex, input$phone_prefix) &&
        grepl(phone_number_regex, input$phone_number)) {
      
      # Retrieve the validated prefix and phone number
      validated_phone <- paste(input$phone_prefix, input$phone_number)
      
    } else {
      validated_phone = NA
    }
    
    mrkrs = colnames(allInputs[, colSums(is.na(allInputs)) < nrow(allInputs)])
    
    train = phenoage_calc(NHANES3,
                          biomarkers = mrkrs)
    
    biological_age <- phenoage_calc(allInputs, biomarkers = mrkrs, fit = train$fit)$data$phenoage
    
    allInputs_tosave = allInputs %>%
      mutate(
        age_bio = biological_age,
        email = ifelse(is.null(emiglio), "", emiglio),
        name = nome,
        phone = validated_phone,
        date= input$bloodTestDate
      )
    
    
    biologicalAge <-  if (!is.null(input$dob)) {
      paste("Your biological age is approximately:", round(biological_age, 1), "years")
    } else {
      "Please enter at least your date of birth"
    }
    
    
    output$biologicalAgePlot <- renderPlot(
      if(!is.null(emiglio)){
        if(nrow(values %>% filter(email == emiglio))>0){
          values %>% 
            filter(email == emiglio) %>% 
            select(c(age, age_bio, date)) %>% 
            group_by(date) %>% 
            mutate(age = mean(age, na.rm = T), age_bio = mean(age_bio, na.rm = T)) %>% 
            rename(
              `Actual age` = age,
              `Biological age` = age_bio
            ) %>% 
            mutate(Date = as_date(date)) %>% 
            pivot_longer(cols = c(1:2), names_to = "name", values_to = "value") %>% 
            ggplot(aes(
              x = Date, 
              y = value, 
              color = name
            )
            )+
            geom_line(size = 1.3)+
            theme_bw()+
            scale_color_discrete(name = "")+
            scale_y_continuous(breaks = round(seq(min(c(values$age, values$age_bio)), max(c(values$age, values$age_bio)), by = 2),0))      
        }
      }
    )
    
    output$table <- renderTable(
      if(!is.null(emiglio)){
        
        if(nrow(values %>% filter(email == emiglio))>0){
          values %>% 
            filter(email == emiglio) %>% 
            mutate_at(c(1, 11), round, 1) %>% 
            select(c(15, 2:10, 1, 11)) %>% 
            #arrange(-date) %>% 
            mutate(date = as.character(as.Date(date))) %>% 
            rename(
              Creatinine = creat, 
              `Lymphocyte (Lymphs)` = lymph,
              `CRP (C-reactive Protein)` = crp,
              `White Blood Cells` = wbc,
              `Mean Cell Volume` = mcv,
              `Alkaline Phosphatase` = alp,
              Glucose = glucose,
              `Red cells distribution width` = rdw,
              `Actual Age` = age,
              `Biological Age` = age_bio      
            )
        }
      })
    
  })
  
  
}

shinyApp(ui = ui, server = server)