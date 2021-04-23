library(taipan)
library(shiny)
library(shinydashboard)

questions <- taipanQuestions(
  scene = sliderInput("quality", label = "Image Quality",
                      min = 0, max = 10, value = 5),
  selection = div(
    radioButtons("hotdog", label = "Hotdog?",
                 choices = list("Hotdog", "Not hotdog")),
    checkboxGroupInput("extra", label = "Condiments",
                       choices = list("Onion", "Tomato (Ketchup)", "Barbeque", "Mustard"))
  )
)

buildTaipan(
  questions = questions,
  images = c("https://raw.githubusercontent.com/srkobakian/taipan/master/sample_images/hotdog.jpg",
             "https://raw.githubusercontent.com/srkobakian/taipan/master/sample_images/not_hotdog.jpg"),
  appdir = file.path(tempdir(), "taipan"), overwrite = TRUE
)

?buildTaipan

tool_questions <- taipanQuestions(
  #scene is needed
  scene = sliderInput("quality", label = "Image Quality", 
                      min = 0, max = 10, value = 5),
  selection = div(
    radioButtons("Tool ID", label = "Hotdog?",
                 choices = list("Tool 1" , "Tool 2" , "Tool 3" , "Tool 4" , "Tool 5", "Tool 6" , "Tool 7" , "Tool 8" , "Tool 9" , "Tool 10", "Unknown")),
    checkboxGroupInput("extra", label = "Condiments",
                       choices = list("Hammer on Anvil", "Hammer off Anvil", "Hammer Transported to Anvil", "Hammer transported from Anvil" , "Hammer Fractures"))
  )
)

image_dir <- list.files("~/Downloads/100RECNX" , full.names=TRUE)
image_dir <- as.vector(image_dir[700:800])

buildTaipan(
  questions = tool_questions,
  #images = c("https://github.com/bjbarrett/taipan_tool/blob/main/images/RCNX0324.JPG" , "https://github.com/bjbarrett/taipan_tool/blob/main/images/RCNX0325.JPG"),
  images = image_dir,
  appdir = file.path("~/Documents", "taipan_tool"), 
  overwrite = TRUE,
  skip_check=TRUE
)
