library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Dynamic selectInput"),
  dashboardSidebar(
    sidebarMenu(
      menuItemOutput("menuitem")
    )
  ),
  dashboardBody(
    numericInput("go_btns_quant","Number of GO buttons",value = 1,min = 1,max = 10),
uiOutput("go_buttons"),
plotOutput("plot")
  )
)