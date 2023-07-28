general_style <- "margin-left:20%;margin-right:20%;text-align: justify"
subprompt_style <- sprintf("font-size:small;%s", general_style)

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
      style = general_style
      
    ),
    button_text = "Continue"
  )
}

personal_page_info <- function(){
  psychTestR::one_button_page(
    body = shiny::div(
      shiny::h3("Owner and creator information", style = "text-align:left"),
      shiny::p("First, we ask for some  personal information of the creators and owners of the stimulus set"),
      shiny::p("Typically, there are one or more persons involved with the creation of the stimulus set.",
               "Likewise, the are might be one or more persons acting as the owner,",
               "i.e., a person responsible for maintaining and curating the dataset."),
      shiny::p("One the next pages, you should enter some basic information for the  creators and owners.",
               "First and last name are mandatory. Additionaly, a professional role and the department of the MPIAE", 
               "if applicable, should to be selected. ORCID is optional."),
      shiny::p("Press 'Continue' to move on to the next section, press 'Another entry' to enter, surprise, 
               another person's data."),
      style = general_style
    ),
    button_text = "Continue"
  )
}

p_id_prompt <- shiny::div(
  shiny::h4("Stimulus Set ID"),
  shiny::p(
  "Please enter an ID for your stimulus set, which may contain only letters, numbers, and underscores.",
  "You can choose the ID freely, but it should be rather unique. This is **not** the name of the stimulus set, for which you will be asked next.",
  "You can use this ID to interruption your data entry and to get back to the point where you stopped.", 
  "Once you finished data entry for a stimulus set, you cannot use the same ID again. If you need this really, please contact me."),
  style = subprompt_style
)

professional_roles <- c(
  "PhD",
  "Post-Doc",
  "Senior Researcher", 
  "Research Group Leader",
  "Director", 
  "Other")

problems_info = list(
  de = shiny::tags$span("Probleme oder brauchen Sie Hilfe bei der Dateneingabe? Kontaktieren Sie klaus.frieler@ae.mpg.de.", 
                        style = "color:#999999",
  ),
  de_f = shiny::tags$span("Probleme oder brauchen Sie Hilfe bei der Dateneingabe? Kontaktieren Sie klaus.frieler@ae.mpg.de.", 
                          style = "color:#999999",  
  ),
  en = shiny::tags$span("Problems or need help with data entry? Contact klaus.frieler@ae.mpg.de.", 
                        style = "color:#999999",
                        shiny::tags$br(),
  )
)

likert_items <- c()

free_text_items <- list(
  name = list(
    prompt = "Please enter a name for your stimulus data set",
    one_line = T
  ),
  
  pub_ref = list(
    prompt = "Please enter the DOI for a reference publication.",
    subprompt = "If you don't have a DOI, please provide bibliographical information (in any style). You can also enter more than one DOI or citation, separated by blank lines."
  ),
  
  location = list(
    prompt = "Please enter a URL of stimulus", 
    subprompt = "Only if available. The URL needs not  to be publicly available, and is likely to change later anyways."),
  
  mpiea_project_id = list(
    prompt = "Please enter Project IDs", 
    subprompt = "Only if available. Enter all MPIAE Project IDs where the stimulusset (or parts thereof) were used, separated by commas.",
    one_line = T),
  
  general_description = list(
    prompt = "Please describe the stimulus data set shortly",
    subprompt = shiny::p("Just the gist, general idea and concept and of the stimuli, areas of application.",
                         style = "font-size:small;text-align:justify;margin-left:20%;margin-right:20%;width:600px")
  ),
  
  design_spec = list(
    prompt = "Please describe the design of the stimulus",
    subprompt = shiny::p("The design of a stimulus set pertains to the logical axes of systematic manipulations or conditions used for constructing the stimulus set.",
                         "For example, a set of texts consisting three different text types (poems, proverbs, Busch epithets),", 
                         "which are modified each to four different versions (original, no rhyme, no meter, no rhyme and no meter),",
                         " which are avaible in two different media formats (written text and audio).",
                         style = "font-size:small;text-align:justify;margin-left:20%;margin-right:20%;width:600px")
  ),
  naming_scheme = list(
    prompt = "Please describe the naming scheme",
    subprompt = shiny::p("For other to make use of your stimulus set, it is vital to understand the naming scheme of the files. 
                         We recommend be be very consistent here, to use only small letters and underscores and to reflect the general design clearly in the names.",
                         "If the dataset does not contain huge amounts of files, it mighte be advisable to use a maximally flat (shallow) folder structure",
                         "with an expressive and concise naming scheme instead of hierarchies of nested folders. No two files",
                         "should have the same name (including the file extension) while  being discriminated by their containing folders. If your stimulus set",
                         "does not follow these guidelines yet, we suggest considering to change the file organisiation accordingly before you submit your stimuli.",
                         style = "font-size:small;text-align:justify;margin-left:20%;margin-right:20%;width:600px")
  ),
  used_works = list(
    prompt = "Please list all external works (music, films, books etc.) if these were used in the dataset.",
    subprompt = shiny::p("Please separated entries here by a line feed/carriage return or a blank line. You can use any metadata format you like, but the works should be uniquely identifiable. For audio recordings, for exampe, this also means including discographical information of the source the audio was taken from.",
                         style = "font-size:small;text-align:justify;margin-left:20%;margin-right:20%;width:600px")
  ),
  reference_set = list(
    prompt = "Please enter a reference ID",
    subprompt = "If your stimulus set is connected to another stimulus set, please enter the ID here. This allows you split up your stimulus set in cross-referenced subsets.",
    one_line = T
  ),
  keywords = list(
    prompt = "Please enter a list of keywords that apply to the stimulus set",
    one_line = T
    
  ),
  version_number = list(
    prompt = "Please enter a version number", 
    subprompt = "This is optional. Assigning of version numbers is under your discretion, though strongly recommended. No specific format is prescribed, however, a simple <MAJOR>.<MINOR> format is recommended.",
    one_line = T,
    width = "50px"
  ),
  licenses = list(
    prompt = "Please enter the license(s) for the stimuli.", 
    subprompt = "This is mainly applicable if the stimulus set (or parts thereof) is under an open source or creative common license. Please enter all licences that apply.",
    one_line = F,
    width = "600px"
  ),
  feedback = list(
    prompt = "Would you like to give some feedback on the survey or on the project itself?"
  )
)

numeric_input_items <- list(
  # subsets = list(
  #   prompt = "How many proper subsets does the stimulus set contain?"
  # ),
  entities = list(
    prompt = "How many files does the stimulus set contain?"
  )
)

dropbox_items <- list(
  source = list(
    prompt = "What is the origin of the main material in the stimulus set?",
    subprompt = shiny::tags$ul( 
      shiny::tags$li("'External' means that the stimuli are based on foreign source material"), 
      shiny::tags$li("'Commercial' means that the source material was bought from a third party, which holds the copyrights"),
      shiny::tags$li("'Free' means that the source material was free to use (e.g., CC license)"),
      shiny::tags$li("'Experiment' means that stimuli were created in a production experiment either by a third party (External) or by the creators (Internal)."),
      shiny::tags$li("'Internal' means that the stimuli were self-created"),
      shiny::tags$li("'Mixed' means that the stimulus set contains a mixture of the above"),
      shiny::tags$li("'Other' means a currently unimaginable different way of stimulus creation"),
      style = "font-size:small;text-align:justify;margin-left:20%;margin-right:20%;width:400px"),
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
  size = list(
    prompt = "Please select the approximate size category for the complete stimulus set",
    items = c("less than 10 MB",
              "10 - 100 MB",
              "100 MB - 1 GB",
              "more than 1 GB")
  ),
  year_created = list(
    prompt = "Please select the year when the first version of the stimulus set was created",
    items = sprintf("%s", c(seq(2022, 1995, -1), "before 1995"))
  )
  
  
)

checkbox_items <- list(
  # access_level = list(
  #   prompt = "Please select all access levels that apply for the different parts of the stimulus set (as of today).",
  #   subprompt = "",
  #   items = c("Fully restricted",
  #             "Internal use (MPIAE)",
  #             "Restricted (conditional use)",
  #             "Open access")
  # ),
  media_types = list(
    prompt = "Please select all media types contained in the stimulus set",
    items = c("Text","Picture", "Video/Live", "Video/Animated", "Audio/Music", "Audio/Speech",
              "Audio/Sounds", "Audio/Mixed", "VR/AR", "3D-Animation", "Other")
  ),
  file_types = list(
    prompt = "Please select the files types of your stimuli",
    subprompt = "Multiple answers possible",
    items = c("pdf","jpeg","tiff","png","wav","mp3","mp4","doc/x","txt", "Other")
  ),
  access_level = list(
    prompt = "Please select all access levels that apply for the different parts of the stimulus set (as of today).",
    items = c("Project members only", 
              "Department members only",
              "Institute members only (incl. affiliated members)",
              "Available within the Max Planck Society",
              "Available upon request",
              "Open Access")
    
  ),
  copyrights = list(
    prompt = "Please select the applicable copyright or license",
    subprompt = "If the stimuli have different levels of copyright, please select all that apply.",
    items = c("Open Access (e.g., CC-BY 4.0)",
              "Rights obtained by MPIEA - no time limit",
              "Rights have been obtained temporarily: must be cleared / renewed by MPIEA",
              "Author's rights, granted on request",
              "Under external copyright: no reuse possible")
  ),
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
  paradigm = list(
    prompt = "For which paradigm was the stimulus set mainly designed and/or used for?",
    subprompt = "Multiple answers possible",
    items = c("Oddball", 
              "Comparison",  
              "Judgement/Rating", 
              "Passive perception",
              "Sorting",
              "Associations",
              "Prompting",
              "Other")
  ),
  measurement = list(
    prompt = "For which measurement method was the stimulus set mainly designed and/or used for?",
    subprompt = "Multiple answers possible",
    items = c("Questionnaire",
              "Reaction Time",
              "fMRI", "EEG", "MEG", "Eye-Tracking", 
              "tDCM", "Physiology", "Other")
  ),
  art_domain = list(
    prompt = "To which aesthetic or artistic domain does you stimulus set main pertain?",
    subprompt = "Multiple answers possible",
    items = c("Non-aesthetics", "Music", "Literature", 
              "Non-fiction Text", "Visual Arts", 
              "Movies", "Music Videos", "Dance", "Theatre", 
              "Performance", "Other")
  )
)

likert_options <- c("Strongly disagree", "Disagree", "Neither disagree nor agree", "Agree", "Strongly agree") 