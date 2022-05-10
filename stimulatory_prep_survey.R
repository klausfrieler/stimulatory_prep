library(tidyverse)
library(psychTestR)

problems_info = list(
  de = shiny::tags$span("Probleme? Kontaktieren Sie klaus.frieler@ae.mpg.de mit einem Link zu dieser Seite.", 
                        style = "color:#999999",
  ),
  de_f = shiny::tags$span("Probleme? Kontaktieren Sie klaus.frieler@ae.mpg.de mit einem Link zu dieser Seite.", 
                          style = "color:#999999",  
  ),
  en = shiny::tags$span("Problems? Contact klaus.frieler@ae.mpg.de with a link to this page.", 
                        style = "color:#999999",
                        shiny::tags$br(),
  )
)

likert_items <- c()

free_text_items <- list(
  name = list(
    prompt = "Please give a succinct and descriptive name for your stimulus data set"
  ),
  pub_ref = list(
    prompt = "Please enter the DOI of a publication, where the stimulus set was used.",
    subprompt = "If you don't have a DOI, please provide bibliographical information (in any style)."
  ),
  location = list(
    prompt = "Please enter a link or URL, where the stimulus set can be accessed.", 
    subprompt = "Does not need to be publicly available."),
  description = list(
    prompt = "Please describe the stimulus data set shortly."
  ),
  feedback = list(
    prompt = "Would you like to give some feedback on the survey or on the project itself?"
  )
)

numeric_input_items <- list(
  subsets = list(
    prompt = "How many proper subsets does the stimulus set contain?"
    ),
    entities = list(
      prompt = "How many files does the stimulus set contain?"
    )
)

dropbox_items <- list(
  source = list(
    prompt = "What is the origin of the main material in the stimulus set?",
    subprompt = shiny::span("Note: 'Internal' means created by researcher or associates; qualifier 'Experiments' means that stimuli were created in a production experiment.", style = "font-size:small"),
    items = c("External/Commercial", 
              "External/Free",
              "External/Experiment", 
              "Internal/Researcher",
              "Internal/Experiment", 
              "Mixed", 
              "Other")),
  
  modification = list(
    prompt = "For external material, how many modifications have been made?",
    items = c("None", "Some", "Substantial", "Mixed", "Only internal material" )
  ),
  role = list(
    prompt= "What is your professional role?", 
    items = c(
      "PhD",
      "Post-Doc",
      "Senior Researcher", 
      "Research Group Leader",
      "Director", 
      "Other"))
)

checkbox_items <- list(
  scientific_field = list(
    prompt = "In which scientific fields  was the stimulus set used so far?",
    subprompt = "Multiple answers possible",
    items = c("Psychology", "Neuroscience", "Musicology",
              "Literary science", "(Psycho-)Linguistics",
              "Media science", "Sociology", "Genetics", 
              "Other")
  ),
  psych_topic = list(
    prompt = "For which psychological or neuroscience topic was the stimulus set used so far?",
    subprompt = "Multiple answers possible",
    items = c("Non-psychological", 
              "Aesthetics", "Emotion", "Perception", "Cognition", "Memory", "Production", "Physiology", 
              "Brain morphology",
              "Other")
  ),
  art_domain = list(
    prompt = "To which aesthetic or artistic domain does you stimulus set main pertain?",
    subprompt = "Multiple answers possible",
    items = c("Non-aesthetics", "Music", "Literature", 
              "Non-fiction Text", "Visual Arts", 
              "Movies", "Music Videos", "Dance", "Theatre", 
              "Performance", "Other")
  ),
  paradigm = list(
    prompt = "For which experimental paradigm was the stimulus set mainly designed and/or used for?",
    subprompt = "Multiple answers possible",
    items = c("Oddball", "Comparison", "Reaction time", "Judgement/Rating", "fMRI", "EEG", "Eye-tracking", "MEG", 
                "tDCM", "Physiology", "Other")
  ),
  media_types = list(
    prompt = "Please select all media types contained in the stimulus set",
    items = c("Text","Picture", "Video/Live", "Video/Animated", "Audio/Music", "Audio/Speech",
      "Audio/Sounds", "Audio/Mixed", "VR/AR", "Other")
  )
  
)
likert_options <- c("Strongly disagree", "Disagree", "Neither disagree nor agree", "Agree", "Strongly agree") 

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
      one_line = FALSE)
    
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

intro_page <- function(){
  psychTestR::one_button_page(
    body = shiny::div(
      shiny::h3("Dear colleagues,", style = "text-align:left"),
      shiny::p("we like to gather some information on stimulus sets you were using in your past research."),
      shiny::p("This is a pre-screening survey to get an impression on what kind of stimulus data sets are out there,", 
               "Filling in the survey will help the Stimulus Database Working Group (SDB-WG) to set up a system", 
               "allowing researcher the easy re-use of stimuli while serving as a standardized archive and documenting site for stimulus sets at the same time."),
      shiny::p("You can also enter more than one stimulus set using the link at the end of survey"),
      shiny::p("Best wishes,", shiny::tags$br(), 
               "Klaus Frieler", shiny::tags$br(), 
               "(on behalf of the SDB-WG)", style = "font-style:italic"),
      style = "margin-left:20%;margin-right:20%;text-align: justify"
      
      ),
    button_text = "Continue"
  )
  
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
    make_free_text_pages(c("name", "pub_ref", "location")),
    make_numeric_input_pages(c("subsets", "entities")),
    make_free_text_pages(c("description")),
    make_checkbox_pages(),
    make_dropbox_pages(),
    make_free_text_pages(c("feedback")),
    
    # psychTestR::reactive_page(function(state, ...){
    #   res <- psychTestR::get_results(state, complete = T, add_session_info = T) %>% as.list()
    #   browser()
    # }),
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