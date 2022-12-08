library(tidyverse)
library(psychTestR)
source("stimulatory_survey_resources.R")

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
    prompt <- shiny::h4(items[[i]]$prompt)
    if("subprompt" %in% names(items[[i]])){
      prompt <- shiny::p(
        shiny::h4(items[[i]]$prompt),
        items[[i]]$subprompt)
    }
    psychTestR::text_input_page(
      label = sprintf("text_%s", i),
      prompt = prompt, 
      button_text = "Continue",
      width = "600px",
      one_line = FALSE)
    
  })  
}

make_person_page <- function(label = "person_page",  id = 1, prompt = "Please enter your personal data", width = 400){
  validate <- NULL
  on_complete <- NULL
  admin_ui <- NULL
  save_answer <- TRUE
  button_text <- "Continue"
  first_name <- shiny::textInput("first_name", label = "First Name", placeholder = "",  width = width)
  middle_name <- shiny::textInput("middle_name", label = "Middle Name", placeholder = "",  width = width)
  last_name <- shiny::textInput("last_name", label = "Last Name", placeholder = "",  width = width)
  orcid_id   <- shiny::textInput("orcid", label = "ORCID", placeholder = "",  width = width)
  department <- shiny::selectInput("department", label = "Department", 
                                   choice = c("Music", 
                                              "Neuroscience", 
                                              "Literature", 
                                              "Cognitive Neuroscience", 
                                              "Not at MPIAE",
                                              "Other"),
                                   selected = "Other", 
                                   width = 200)
  prof_role <- shiny::selectInput("prof_role", "Professional Role", 
                                  choices = professional_roles,
                                  selected = "Other", 
                                  width = 200)  
  get_answer <- function(input, state, ...) {
    browser()
    answer <- input %>% 
      reactiveValuesToList() %>% 
      discard(is.null) %>% 
      as_tibble() %>% 
      select(all_of(c("last_name","middle_name", "first_name", "orcid", "department")))
    
    page_counter <- psychTestR::get_local(sprintf("%s_page_counter", label), state)
    answer <- answer %>% mutate(counter = page_counter, type = label) 
    psychTestR::set_local(sprintf("%s_page_counter", label), as.integer(page_counter) + 1 , state)
    
    psychTestR::set_local("repeat", identical(input$last_btn_pressed, "repeat"), state)
    print(sprintf("Repeat = %s", identical(input$last_btn_pressed, "repeat")))
    print(answer)
    answer
  }
  validate <- function(input, ...){
    if(nzchar(input$first_name) && nzchar(input$last_name)) {
      TRUE
    }
    else{
      FALSE
    }
  }
  prompt <- shiny::h4(prompt)
  body = shiny::div(onload = "document.getElementById('first_name').value = '';", 
                    psychTestR:::tagify(prompt), 
                    first_name, 
                    middle_name, 
                    last_name, 
                    orcid_id, 
                    department, 
                    prof_role)
  ui <- shiny::div(body, shiny::div(trigger_button("repeat", "Another entry"), trigger_button("next", button_text)))
  page(ui = ui, label = sprintf("%s%s", label, id), get_answer = get_answer, save_answer = save_answer, 
       validate = validate, on_complete = on_complete, final = FALSE, 
       admin_ui = admin_ui)      
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
    # psychTestR::get_p_id(prompt = p_id_prompt),
    # make_free_text_pages(c("name", "general_description", "design_spec", "naming_scheme")),
    # make_free_text_pages(c("pub_ref", "location")),
    # make_numeric_input_pages(c("entities")),
    make_dropbox_pages(),
    make_checkbox_pages(),
    # personal_page_info(),
    psychTestR::code_block(
      function(state, ...){
        psychTestR::set_local("repeat", TRUE, state)
        psychTestR::set_local("owner_page_counter", 1, state)
      }
    ),
    psychTestR::while_loop(test = function(state, ...) psychTestR::get_local("repeat", state),
                           logic =
                             make_person_page(label = "owner",
                                              prompt = "Please enter the personal data for the dataset owner")
    ),
    # psychTestR::code_block(
    #   function(state, ...){
    #     psychTestR::set_local("repeat", TRUE, state)
    #     psychTestR::set_local("creator_page_counter", 1, state)
    #   }
    # ),
    # psychTestR::while_loop(test = function(state, ...) psychTestR::get_local("repeat", state),
    #                        logic =
    #                          make_person_page(label = "creator",
    #                                           prompt = "Please enter the personal data for the dataset creator")
    # ),
    # make_free_text_pages(c("feedback")),
    
    psychTestR::reactive_page(function(state, ...){
       res <- psychTestR::get_results(state, complete = T, add_session_info = T) %>% as.list()
       browser()
     }),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::final_page(shiny::p(shiny::h4("Thank you for participating!"),
                                    shiny::h4("You can close the browser tab now."),
                                    shiny::a(href = "http://testing.musikpsychologie.de/MSSS", 
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