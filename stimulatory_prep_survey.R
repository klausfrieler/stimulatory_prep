library(tidyverse)
library(psychTestR)
source("stimulatory_survey_resources.R")

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))
debug <- T

make_likert_pages <- function(items = NULL){
  if(is.null(items)){
    items <- likert_items
  }
  else{
    items <- likert_items[items]
  }
  
  imap(likert_items, function(prompt, i){
    psychTestR::NAFC_page( 
      label = sprintf("likert_%s", i),
      shiny::h4(prompt), 
      choices = as.character(1:5), 
      labels = likert_options, 
      button_style = "min-width:250px")
                            
  })  
}

make_dropbox_pages <- function(items = NULL){
  if(is.null(items)){
    items <- dropbox_items
  }
  else{
    items <- dropbox_items[items]
  }
  
  imap(items, function(prompt, i){
    prompt <- shiny::h4(items[[i]]$prompt)
    if("subprompt" %in% names(items[[i]])){
      prompt <- shiny::p(
        shiny::h4(items[[i]]$prompt),
        items[[i]]$subprompt)
    }
    psychTestR::NAFC_page(label = sprintf("question_%s", i),
                          prompt = prompt, 
                          choices = items[[i]]$items, 
                          labels = items[[i]]$items, 
                          button_style = "min-width:320px")
    
  })  
  
}

make_checkbox_pages <- function(items = NULL){
  if(is.null(items)){
    items <- checkbox_items
  }
  else{
    items <- checkbox_items[items]
  }
  imap(items, function(prompt, i){
    prompt <- shiny::h4(items[[i]]$prompt)
    if("subprompt" %in% names(items[[i]])){
      prompt <- shiny::p(
        shiny::h4(items[[i]]$prompt),
        items[[i]]$subprompt)
    }
    psychTestR::checkbox_page(
      label = sprintf("checkbox_%s", i),
      prompt = prompt, 
      choices = items[[i]]$items,
      #choices = as.character(1:length(items[[i]]$items)), 
      labels = items[[i]]$items)
    
  })  
  
}

make_free_text_pages <- function(items = NULL){
  if(is.null(items)){
    items <- free_text_items
  }
  else{
    items <- free_text_items[items]
  }
  imap(items, function(prompt, i){
    cur_item <- items[[i]]
    prompt <- shiny::h4(cur_item$prompt)
    if("subprompt" %in% names(cur_item)){
      prompt <- shiny::div(
        shiny::h4(cur_item$prompt),
        shiny::p(cur_item$subprompt, style =  "margin-left:20%;margin-right:20%;text-align: justify"))
    }
    one_line <- FALSE
    width  <- "600px"
    
    if("one_line" %in% names(cur_item)){
      one_line <- as.logical(cur_item$one_line)
      width <- "400px"
    }
    if("width" %in% names(cur_item)){
      width <- cur_item$width
    }
    psychTestR::text_input_page(
      label = sprintf("text_%s", i),
      prompt = prompt, 
      button_text = "Continue",
      width = width,
      one_line = one_line)
    
  })  
}

make_person_page <- function(label = "person_page",  
                             prompt = "Please enter your personal data", 
                             subprompt = NULL, 
                             width = 400){
  validate <- NULL
  on_complete <- NULL
  admin_ui <- NULL
  save_answer <- TRUE
  button_text <- "Continue"
  first_name <- shiny::textInput("first_name", label = "First Name", placeholder = "",  width = width)
  last_name <- shiny::textInput("last_name", label = "Last Name", placeholder = "",  width = width)
  email <- shiny::textInput("email", label = "E-Mail Address", placeholder = "",  width = width)
  orcid_id   <- shiny::textInput("orcid", label = "ORCID", placeholder = "",  width = width)
  department <- shiny::selectInput("department", label = "Department", 
                                   choice = c("Music", 
                                              "Neuroscience", 
                                              "Literature", 
                                              "Cognitive Neurospsychology",
                                              "Research Groups",
                                              "Services",
                                              "ArtLab",
                                              "Other",
                                              "Not at MPIAE"),
                                   selected = "Other", 
                                   width = 200)
  prof_role <- shiny::selectInput("prof_role", "Professional Role", 
                                  choices = professional_roles,
                                  selected = "Other", 
                                  width = 200)  
  get_answer <- function(input, state, ...) {
    #browser()
    tmp <- input %>% 
      reactiveValuesToList() %>% 
      discard(is.null) 
    
    page_counter <- psychTestR::get_local(sprintf("%s_page_counter", label), state)
    answer <- tmp[c("last_name", "first_name", "email", "orcid", "department", "prof_role")] %>% 
      as_tibble() %>% 
      mutate(counter = page_counter, type = label) %>% 
      filter(nzchar(last_name), nzchar(first_name), nzchar(email))
    
    psychTestR::set_local("repeat", identical(input$last_btn_pressed, "repeat"), state)
    #printf("Repeat = %s", identical(input$last_btn_pressed, "repeat"))
    #print(answer)
    answer
  }
  
  validate <- function(input, state, ...){
    page_counter <- psychTestR::get_local(sprintf("%s_page_counter", label), state)
    if(!is.null(page_counter) && length(page_counter) > 0 && as.integer(page_counter) > 1){
      return(TRUE)
    }
    if(nzchar(input$first_name) && nzchar(input$last_name)) {
      TRUE
    }
    else{
      FALSE
    }
  }
  
  on_complete <- function(state, ...){
    page_counter <- psychTestR::get_local(sprintf("%s_page_counter", label), state)
    psychTestR::set_local(sprintf("%s_page_counter", label), as.integer(page_counter) + 1 , state)
    messagef("Set counter %s to %d", sprintf("%s_page_counter", label), as.integer(page_counter) + 1)
  }
  
  prompt <- shiny::h4(prompt)
  if(!is.null(subprompt)  && nzchar(subprompt)){
    subprompt <- shiny::p(subprompt, 
                          style = "margin-left:20%;margin-right:20%;text-align: justify")
  }
  else subprompt = NULL
  body = shiny::div(onload = "document.getElementById('first_name').value = '';", 
                    psychTestR:::tagify(prompt), 
                    subprompt,
                    first_name, 
                    #middle_name, 
                    last_name, 
                    email,
                    orcid_id, 
                    department, 
                    prof_role)
  ui <- shiny::div(body, shiny::div(trigger_button("repeat", "Another entry"), trigger_button("next", button_text)))
  psychTestR::reactive_page(function(state,...){
    counter <- psychTestR::get_local(sprintf("%s_page_counter", label), state)
    page(ui = ui, label = sprintf("%s%s", label, counter), get_answer = get_answer, save_answer = save_answer, 
         validate = validate, on_complete = on_complete, final = FALSE, 
         admin_ui = admin_ui)      
  })
}

make_numeric_input_pages <- function(items = NULL){
  if(is.null(items)){
    items <- numeric_input_items
  }
  else{
    items <- numeric_input_items[items]
  }
  imap(items, function(prompt, i){
    prompt <- shiny::h4(items[[i]]$prompt)
    if("subprompt" %in% names(items[[i]])){
      prompt <- shiny::p(
        shiny::h4(items[[i]]$prompt),
        items[[i]]$subprompt)
    }
    psychTestR::text_input_page(
      label = sprintf("numeric_%s", i),
      prompt = prompt, 
      button_text = "Continue",
      one_line = TRUE,
      width = 100 )
    
  })  
  
}



stimulatory_survey  <- function(title = "MPIAE Stimulus Sets Survey",
                                documentation = "MSSS",
                                admin_password = "conifer",
                                researcher_email = "klaus.frieler@ae.mpg.de",
                                languages = c("en"),
                                validate_id = "auto",
                                dict = psyquest::dict,
                                ...) {
  elts <- psychTestR::join(
    intro_page(),
    psychTestR::get_p_id(prompt = p_id_prompt, button_text = "Continue"),
    make_free_text_pages(c("name", "general_description", "design_spec", "naming_scheme")),
    make_free_text_pages(c("pub_ref", "location", "mpiea_project_id", "keywords", "version_number")),
    make_numeric_input_pages(c("entities")),
    make_dropbox_pages(),
    make_checkbox_pages(),
    personal_page_info(),
    psychTestR::code_block(
      function(state, ...){
        psychTestR::set_local("repeat", TRUE, state)
        psychTestR::set_local("creator_page_counter", 1, state)
      }
    ),
    psychTestR::while_loop(test = function(state, ...) psychTestR::get_local("repeat", state),
                           logic =
                             make_person_page(label = "creator",
                                              prompt = "Please enter the personal data for the dataset creators",
                                              subprompt = "First and last name and e-mail address are mandatory for the first entry. Click 'Another etnry' to add another creator")
    ),

    psychTestR::code_block(
      function(state, ...){
        psychTestR::set_local("repeat", TRUE, state)
        psychTestR::set_local("owner_page_counter", 1, state)
      }
    ),
    psychTestR::while_loop(test = function(state, ...) psychTestR::get_local("repeat", state),
                           logic =
                             make_person_page(label = "owner",
                                              prompt = "Please enter personal data for the dataset owners",
                                              subprompt = "The owner is a person currently responsible for maintenance and curation of the stimulus set. (First and last name and e-mail are mandatory for first entry, add more owners with 'Another entry')")
    ),
    make_free_text_pages(c("feedback")),
    psychTestR::reactive_page(function(state, ...){
       res <- psychTestR::get_results(state, complete = T, add_session_info = T) %>% as.list()
       if(debug)browser()
       psychTestR::one_button_page(body = shiny::div(
         shiny::h4("Your results"),
         shiny::pre(
           jsonlite::prettify(jsonlite::toJSON(res)), 
           style = general_style))
         , 
         button_text = "Continue")
     }),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::final_page(shiny::p(shiny::h4("Thank you for participating!"),
                                    shiny::h4("You can close the browser tab now."),
                                    shiny::a(href = "http://testing.musikpsychologie.de/stimulatory_prep", 
                                             "Or make another entry.", style = "font-size: large")))
  )
  #browser()  
  
  psychTestR::make_test(
    elts,
    opt = psychTestR::test_options(title = title,
                                   admin_password = admin_password,
                                   researcher_email = researcher_email,
                                   allow_any_p_id_url = FALSE,
                                   problems_info = problems_info,
                                   #force_p_id_from_url = TRUE,
                                   demo = FALSE,
                                   languages = languages))
}