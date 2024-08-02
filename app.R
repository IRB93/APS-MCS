library(shiny)
library(shinydashboard)
library(shinyjs)

# Upload pre-calculated cross-validation probabilities
probabilidades <- read.csv("probabilidades.csv")

# Diagnosis_text function definition with pre-calculated probabilities
calcular_probabilidad_precargada <- function(total_score) {
  index <- which.min(abs(probabilidades$total_score - total_score))
  prob_4RT <- probabilidades$probabilidad_4RT[index]
  prob_MSA <- probabilidades$probabilidad_MSA[index]
  return(list(prob_4RT = prob_4RT, prob_MSA = prob_MSA))
}

diagnosis_text <- function(total_score) {
  probs <- calcular_probabilidad_precargada(total_score)
  probabilidad_4RT <- round(probs$prob_4RT, 3)
  probabilidad_MSA <- round(probs$prob_MSA, 3)
  
  if (total_score >= 2.5) {
    return(list(diagnosis = "Diagnosis: 4RT", probability = probabilidad_4RT))
  } else if (total_score >= 1.8 && total_score < 2.5) {
    return(list(diagnosis = "Diagnosis: More likely 4RT", probability = probabilidad_4RT))
  } else if (total_score > 0.5 && total_score < 1.8) {
    return(list(diagnosis = "Diagnosis: Indeterminate", probability = NA))
  } else if (total_score <= -0.2) {
    return(list(diagnosis = "Diagnosis: MSA", probability = probabilidad_MSA))
  } else if (total_score > -0.2 && total_score < 0.5) {
    return(list(diagnosis = "Diagnosis: More likely MSA", probability = probabilidad_MSA))
  }
}

# UI Logic
ui <- dashboardPage(
  dashboardHeader(title = "APS-MCS"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Initial Questions", tabName = "initial", icon = icon("question-circle")),
                menuItem("Frontal Domain", tabName = "frontal", icon = icon("brain"), selected = FALSE),
                menuItem("Parietal Domain", tabName = "parietal", icon = icon("hand-paper"), selected = FALSE),
                menuItem("Cerebellar Domain", tabName = "cerebellar", icon = icon("walking"), selected = FALSE),
                menuItem("Total Scores", tabName = "score", icon = icon("list-alt"), selected = FALSE)
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "initial",
              div(style = "text-align: center;",
                  h2("Initial Questions"),
                  br(),
                  fluidRow(
                    column(12, align = "center",
                           div(class = "question-text",
                               h4("Primary parkinsonism with exclusion criteria for Parkinson's disease/dementia with Lewy bodies or more red flags than supportive criteria?"),
                               radioButtons("q1", NULL, choices = c("Yes" = "yes", "No" = "no"), selected = "no", inline = TRUE)
                           )
                    )
                  ),
                  fluidRow(
                    column(12, align = "center",
                           div(class = "question-text",
                               h4("Low probability criteria for PSP/CBD or MSA, or criteria overlap between both entities?"),
                               radioButtons("q2", NULL, choices = c("Yes" = "yes", "No" = "no"), selected = "no", inline = TRUE)
                           )
                    )
                  ),
                  br(),
                  actionButton("startScale", "Start Scale Assessment", class = "btn-primary")
              )
      ),
      tabItem(tabName = "frontal",
              h3("Frontal Domain Assessments"),
              div(
                h4(strong("Manual Grasping")),
                p("Place your hands under the patient's hands and observe whether the patient grasps the examiner's fingers when the palm is touched."),
                selectInput("frontal_manualgrasping", NULL, choices = c("Select option" = "", "No attempt to grasp" = 0, "Hesitates" = 1, "Grasps" = 2))
              ),
              div(
                h4(strong("Manual Groping")),
                p("Assess whether the patient grabs a pen placed near their hand (without actually touching it)."),
                selectInput("frontal_manualgroping", NULL, choices = c("Select option" = "", "No attempt to grab" = 0, "Hesitates" = 1, "Grabs" = 2))
              ),
              div(
                h4(strong("Visual Grasping")),
                p("During the oculomotor assessment, the patient experiences abrupt episodes of difficulty transitioning from one stimulus to another upon verbal command."),
                selectInput("frontal_visualgrasping", NULL, choices = c("Select option" = "", "No sudden difficulties in shifting gaze" = 0, "Sudden difficulties in shifting gaze" = 1))
              ),
              div(
                h4(strong("Applause Sign")),
                p("The patient is asked to copy the examiner, who gives three claps."),
                selectInput("frontal_applause", NULL, choices = c("Select option" = "", "3 claps" = 0, ">3 claps" = 1))
              ),
              div(
                h4(strong("Stereotypes")),
                p("Repetitive motor or vocal patterns that appear to be purposeless. Identified during the visit or through anamnesis."),
                selectInput("frontal_stereotypes", NULL, choices = c("Select option" = "", "No stereotypical behaviors observed or reported" = 0, "Stereotypical behaviors observed or reported" = 1))
              ),
              div(
                h4(strong("Echopraxia")),
                p("The palms of the patient’s hands are placed on the examiner's hands, and passive horizontal movements in opposite directions are performed for each limb up to three times. After these movements are stopped, it is observed whether the patient continues to perform them. The same procedure is repeated in the vertical plane"),
                selectInput("frontal_echopraxia", NULL, choices = c("Select option" = "", "No continuation of movements" = 0, "Continuation of movements in one plane" = 1, "Continuation of movements in both planes" = 2))
              ),
              div(
                h4(strong("Apraxia of Speech")),
                p("Assess the patient’s speech for inconsistent sound errors, difficulty with mimicking sounds, and prosody alterations through spontaneous conversations and speech tasks."),
                selectInput("frontal_aos", NULL, choices = c("Select option" = "", "No speech production errors or difficulties" = 0, "Speech production errors or difficulties" = 1))
              ),
              div(
                h4(strong("Greed for Food")),
                p("Inquire about the patient’s eating voracity"),
                selectInput("frontal_gff", NULL, choices = c("Select option" = "", "No greed for food" = 0, "Greed for food" = 1))
              )
      ),
      tabItem(tabName = "parietal",
              h3("Parietal Domain Assessments"),
              div(
                h4(strong("Epicritic Touch")),
                p("A clip with both ends bent at a similar distance will be used. With the patient's hand relaxed and facing upwards, the clip will be indented less than 2mm into the skin, once vertically along the arm's axis and once horizontally, in a random order. The patient will be asked which orientation they perceived first. Three attempts will be made on each hand."),
                selectInput("parietal_epicritic", NULL, choices = c("Select option" = "", "Orientation correctly identified in all attempts" = 0, "Orientation incorrectly identified in one or more attempts" = 1))
              ),
              div(
                h4(strong("Graphesthesia")),
                p("Numbers 1 to 3 are randomly drawn on the patient's hand using a clip."),
                selectInput("parietal_graphestesia", NULL, choices = c("Select option" = "", "All numbers identified" = 0, "Unable to identify one or more numbers" = 1))
              ),
              div(
                h4(strong("Stereognosis")),
                p("Select three small objects (e.g., a key, a coin, and a paperclip). With the patient's eyes closed, place one object in their hand and ask them to identify it by touch alone. If the patient is unable to identify the object with one hand, have them try with the other hand before opening their eyes. Repeat the process for the other two objects."),
                selectInput("parietal_stereo", NULL, choices = c("Select option" = "", "All three objects identified" = 0, "Unable to identify one or more objects" = 1))
              ),
              div(
                h4(strong("Limb Kinetic Apraxia")),
                p("The patients are asked to touch the thumb to each finger sequentially, observing for smoothness and accuracy of movements."),
                selectInput("parietal_lkapraxia", NULL, choices = c("Select option" = "", "Smooth and accurate movements or no significant difference with finger tapping test" = 0, "Mild difficulty or inaccuracy in movements or significant difference with finger tapping test" = 1, "Mild difficulty or inaccuracy in movements or difference with finger tapping test" = 1, "Moderate-severe difficulty or inaccuracy in movements or difference with finger tapping test" = 2))
              ),
              div(
                h4(strong("Ideomotor Apraxia")),
                p("The patient is asked to imitate hand gestures ('victory sign', 'thumb-to-ring finger', 'gun sign', and 'thumb-to-index finger joining of both hands')"),
                selectInput("parietal_imapraxia", NULL, choices = c("Select option" = "", "Accurately performs all gestures" = 0, "Mild difficulty to perform one or more gestures" = 1, "Moderate-severe difficulty or inability to perform one or more gestures"))
              ),
              div(
                h4(strong("Body Postural Disturbance")),
                p("Observe the patient during walking and while sitting with arms extended and eyes closed, looking for posterior or lateral displacement affecting the abductor muscles of the upper limbs (includes arm levitation and proximal/distal pseudodystonic postures of the limbs)."),
                selectInput("parietal_bodypost", NULL, choices = c("Select option" = "", "No body postural disturbance" = 0, "Body postural disturbance" = 1))
              )
      ),
      tabItem(tabName = "cerebellar",
              h3("Cerebellar Domain Assessments"),
              div(
                h4(strong("Nystagmus")),
                p("Observe for gaze-evoked continuous, rhythmic eye oscillations. Distinguish from square wave jerks, which are brief, intermittent movements with a return to the starting position. Test by having the patient follow a moving target."),
                selectInput("cerebellar_nystagmus", NULL, choices = c("Select option" = "", "No gaze-evoked nystagmus" = 0, "Gaze-evoked nystagmus" = 1))
              ),
              div(
                h4(strong("Finger-Chase Test")),
                p("The patient is seated comfortably, with trunk and feet supported if necessary. The examiner sits in front of the patient and requests 5 quick, consecutive, unpredictable pointing movements, with an amplitude of 30 cm and a frequency of 1 movement every 2 seconds. The movements must be performed by pointing with the index finger as quickly and accurately as possible. Only the last 3 movements on each side are scored."),
                selectInput("cerebellar_fingerchase", NULL, choices = c("Select option" = "", "Aaccurate pointing" = 0, "Mildly impaired (<5cm)" = 1, "Markedly impaired (>5cm)" = 2))
              ),
              div(
                h4(strong("Finger-Nose Test")),
                p("The patient is seated comfortably, with trunk and feet supported if necessary. Ask the patient to move their finger from their nose to the examiner's finger. The movements should be performed at a moderate speed."),
                selectInput("cerebellar_fingernose", NULL, choices = c("Select option" = "", "No tremor" = 0, "Mild tremor (<2cm)" = 1, "Marked tremor (>2cm)" = 2))
              ),
              div(
                h4(strong("Heel-Knee Test")),
                p("The patient lies supine. Ask the patient to touch the knee of one leg with the heel of the other leg, then slide the heel down the shin to the ankle in less than 1 second, and finally return the leg to the bed."),
                selectInput("cerebellar_heelknee", NULL, choices = c("Select option" = "", "Smooth and accurate movement" = 0, "Difficult or inaccurate movement" = 1))
              ),
              div(
                h4(strong("Scanning Speech")),
                p("Listen to the patient's speech for signs of slow, halting pronunciation with irregular pauses and inappropriate emphasis on syllables. Unlike apraxia of speech, scanning speech does not involve inconsistent errors or difficulty in mimicking speech sounds, but rather a disrupted rhythm and melody"),
                selectInput("cerebellar_scanning", NULL, choices = c("Select option" = "", "No scanning speech" = 0, "Scanning speech" = 1))
              ),
              div(
                h4(strong("Gait Ataxia")),
                p("Evaluate the patient’s gait by asking them to perform tandem walking (heel-to-toe walking) and observing for any widening of the base of their gait."),
                selectInput("cerebellar_ataxia", NULL, choices = c("Select option" = "", "No widening of the base and no difficulty with tandem gait" = 0, "Difficulty with tandem gait without widening of the base" = 1, "Difficulty with tandem gait and widening of the base" = 2))
              )
      ),
      tabItem(tabName = "score",
              h3("Results"),
              verbatimTextOutput("frontalScore"),
              verbatimTextOutput("parietalScore"),
              verbatimTextOutput("cerebellarScore"),
              verbatimTextOutput("totalScore"),
              verbatimTextOutput("diagnosisResult")
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  scores <- reactiveValues()
  
  coefficients <- list(frontal = 1.962078, parietal = 2.084357, cerebellar = -0.4044245)
  
  observe({
    shinyjs::toggleState("q2", condition = input$q1 == "yes")
    shinyjs::toggleState("startScale", condition = input$q1 == "yes" && input$q2 == "yes")
    
    # Enable or disable sidebar menu items
    if (input$q1 == "yes" && input$q2 == "yes") {
      shinyjs::enable("tabs")
    } else {
      shinyjs::disable("tabs")
    }
  })
  
  observeEvent(input$startScale, {
    if (input$q1 == "yes" && input$q2 == "yes") {
      updateTabItems(session, "tabs", "frontal")
    }
  })
  
  # Setting up the reactive values for the scores
  observeEvent(input$frontal_manualgrasping, { scores$frontal_manualgrasping <- input$frontal_manualgrasping })
  observeEvent(input$frontal_manualgroping, { scores$frontal_manualgroping <- input$frontal_manualgroping })
  observeEvent(input$frontal_visualgrasping, { scores$frontal_visualgrasping <- input$frontal_visualgrasping })
  observeEvent(input$frontal_applause, { scores$frontal_applause <- input$frontal_applause })
  observeEvent(input$frontal_stereotypes, { scores$frontal_stereotypes <- input$frontal_stereotypes })
  observeEvent(input$frontal_echopraxia, { scores$frontal_echopraxia <- input$frontal_echopraxia })
  observeEvent(input$frontal_aos, { scores$frontal_aos <- input$frontal_aos })
  observeEvent(input$frontal_gff, { scores$frontal_gff <- input$frontal_gff })
  observeEvent(input$parietal_epicritic, { scores$parietal_epicritic <- input$parietal_epicritic })
  observeEvent(input$parietal_graphestesia, { scores$parietal_graphestesia <- input$parietal_graphestesia })
  observeEvent(input$parietal_stereo, { scores$parietal_stereo <- input$parietal_stereo })
  observeEvent(input$parietal_lkapraxia, { scores$parietal_lkapraxia <- input$parietal_lkapraxia })
  observeEvent(input$parietal_imapraxia, { scores$parietal_imapraxia <- input$parietal_imapraxia })
  observeEvent(input$parietal_bodypost, { scores$parietal_bodypost <- input$parietal_bodypost })
  observeEvent(input$cerebellar_nystagmus, { scores$cerebellar_nystagmus <- input$cerebellar_nystagmus })
  observeEvent(input$cerebellar_fingerchase, { scores$cerebellar_fingerchase <- input$cerebellar_fingerchase })
  observeEvent(input$cerebellar_fingernose, { scores$cerebellar_fingernose <- input$cerebellar_fingernose })
  observeEvent(input$cerebellar_heelknee, { scores$cerebellar_heelknee <- input$cerebellar_heelknee })
  observeEvent(input$cerebellar_scanning, { scores$cerebellar_scanning <- input$cerebellar_scanning })
  observeEvent(input$cerebellar_ataxia, { scores$cerebellar_ataxia <- input$cerebellar_ataxia })
  
  # Calculating the total score with coefficients applied correctly
  observe({
    all_selected <- !is.null(scores$frontal_manualgrasping) && scores$frontal_manualgrasping != "" &&
      !is.null(scores$frontal_manualgroping) && scores$frontal_manualgroping != "" &&
      !is.null(scores$frontal_visualgrasping) && scores$frontal_visualgrasping != "" &&
      !is.null(scores$frontal_applause) && scores$frontal_applause != "" &&
      !is.null(scores$frontal_stereotypes) && scores$frontal_stereotypes != "" &&
      !is.null(scores$frontal_echopraxia) && scores$frontal_echopraxia != "" &&
      !is.null(scores$frontal_aos) && scores$frontal_aos != "" &&
      !is.null(scores$frontal_gff) && scores$frontal_gff != "" &&
      !is.null(scores$parietal_epicritic) && scores$parietal_epicritic != "" &&
      !is.null(scores$parietal_graphestesia) && scores$parietal_graphestesia != "" &&
      !is.null(scores$parietal_stereo) && scores$parietal_stereo != "" &&
      !is.null(scores$parietal_lkapraxia) && scores$parietal_lkapraxia != "" &&
      !is.null(scores$parietal_imapraxia) && scores$parietal_imapraxia != "" &&
      !is.null(scores$parietal_bodypost) && scores$parietal_bodypost != "" &&
      !is.null(scores$cerebellar_nystagmus) && scores$cerebellar_nystagmus != "" &&
      !is.null(scores$cerebellar_fingerchase) && scores$cerebellar_fingerchase != "" &&
      !is.null(scores$cerebellar_fingernose) && scores$cerebellar_fingernose != "" &&
      !is.null(scores$cerebellar_heelknee) && scores$cerebellar_heelknee != "" &&
      !is.null(scores$cerebellar_scanning) && scores$cerebellar_scanning != "" &&
      !is.null(scores$cerebellar_ataxia) && scores$cerebellar_ataxia != ""
    
    if (all_selected) {
      # Convert all input scores to numeric and apply the coefficients
      frontal_score <- sum(as.numeric(scores$frontal_manualgrasping), 
                           as.numeric(scores$frontal_manualgroping),
                           as.numeric(scores$frontal_visualgrasping),
                           as.numeric(scores$frontal_applause),
                           as.numeric(scores$frontal_stereotypes),
                           as.numeric(scores$frontal_echopraxia),
                           as.numeric(scores$frontal_aos),
                           as.numeric(scores$frontal_gff)) * coefficients$frontal
      
      parietal_score <- sum(as.numeric(scores$parietal_epicritic), 
                            as.numeric(scores$parietal_graphestesia),
                            as.numeric(scores$parietal_stereo),
                            as.numeric(scores$parietal_lkapraxia),
                            as.numeric(scores$parietal_imapraxia),
                            as.numeric(scores$parietal_bodypost)) * coefficients$parietal
      
      cerebellar_score <- sum(as.numeric(scores$cerebellar_nystagmus), 
                              as.numeric(scores$cerebellar_fingerchase),
                              as.numeric(scores$cerebellar_fingernose),
                              as.numeric(scores$cerebellar_heelknee),
                              as.numeric(scores$cerebellar_scanning),
                              as.numeric(scores$cerebellar_ataxia)) * coefficients$cerebellar
      
      total_score <- frontal_score + parietal_score + cerebellar_score
      
      # Generate diagnosis based on the total score
      diagnosis <- diagnosis_text(total_score)
      
      output$totalScore <- renderText({
        paste("Total Adjusted Score:", round(total_score, 1))
      })
      
      output$diagnosisResult <- renderText({
        paste(diagnosis$diagnosis, "\nProbability:", round(diagnosis$probability, 3))
      })
      
      output$frontalScore <- renderText({
        paste("Frontal Domain Score:", round(frontal_score/coefficients$frontal, 1))
      })
      
      output$parietalScore <- renderText({
        paste("Parietal Domain Score:", round(parietal_score/coefficients$parietal, 1))
      })
      
      output$cerebellarScore <- renderText({
        paste("Cerebellar Domain Score:", round(cerebellar_score/coefficients$cerebellar, 1))
      })
    } else {
      output$totalScore <- renderText({ "" })
      output$diagnosisResult <- renderText({ "" })
      output$frontalScore <- renderText({ "" })
      output$parietalScore <- renderText({ "" })
      output$cerebellarScore <- renderText({ "" })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)


