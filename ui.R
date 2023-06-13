# Load the ggplot2 package which provides
# the 'mpg' dataset.
fluidPage(
  titlePanel(tags$strong("미국 투자대가들은 어떤 종목을 사고팔고 있을까?")),
  
  # Create a tabsetPanel
  tabsetPanel(
    # First tab
    tabPanel("순매수 규모 상위 종목",
             fluidRow(
               column(width = 12,
                      tags$h5(tags$span("투자대가들이 가장 많이 매수한 종목 리스트",
                                        tags$br(),
                                        "(최근 분기별 순매수 규모 상위 종목순)", style="color:green;"))
               )
             ),
             fluidRow(
               column(4,
                      selectInput("quarter1",
                                  "Date:",
                                  c("All", unique(as.character(buy$quarter))))
               ),
               column(4,
                      selectInput("Industry1",
                                  "Industry(sector):",
                                  c("All", unique(as.character(buy$Industry))))
               )
             ),
             DT::dataTableOutput("table1")
    ),
    
    tabPanel("순매수 강도 상위 종목",
             fluidRow(
               column(width = 12,
                      tags$h5(tags$span("투자대가들이 상장주식수 대비 많은 물량을 매수한 종목 리스트",tags$br(),"(최근 분기별 순매수 강도(순매수 주식수/전체 상장주식수) 상위 종목순)", style="color:green;"))
               )
             ),
             fluidRow(
               column(4,
                      selectInput("quarter2",
                                  "Date:",
                                  c("All", unique(as.character(buy_percent$quarter))))
               ),
               column(4,
                      selectInput("Industry2",
                                  "Industry(sector):",
                                  c("All", unique(as.character(buy_percent$Industry))))
               )
             ),
             DT::dataTableOutput("table2")
    ),
    
    tabPanel("순매도 규모 상위 종목",
             fluidRow(
               column(width = 12,
                      tags$h5(tags$span("투자대가들이 가장 많이 매도한 종목 리스트",
                                        tags$br(),
                                        "(최근 분기별 순매도 규모 상위 종목순)", style="color:green;"))
               )
             ),
             fluidRow(
               column(4,
                      selectInput("quarter3",
                                  "Date:",
                                  c("All", unique(as.character(sell$quarter))))
               ),
               column(4,
                      selectInput("Industry3",
                                  "Industry(sector):",
                                  c("All", unique(as.character(sell$Industry))))
               )
             ),
             DT::dataTableOutput("table3")
    ),
    
    tabPanel("순매도 강도 상위 종목",
             fluidRow(
               column(width = 12,
                      tags$h5(tags$span("투자대가들이 상장주식수 대비 많은 물량을 매도한 종목 리스트",tags$br(),"(최근 분기별 순매도 강도(순매도 주식수/전체 상장주식수) 상위 종목순)", style="color:green;"))
               )
             ),
             fluidRow(
               column(4,
                      selectInput("quarter4",
                                  "Date:",
                                  c("All", unique(as.character(sell_percent$quarter))))
               ),
               column(4,
                      selectInput("Industry4",
                                  "Industry(sector):",
                                  c("All", unique(as.character(sell_percent$Industry))))
               )
             ),
             DT::dataTableOutput("table4")
    ),
    
    
      # Second tab
    tabPanel("최근 순매수 종목 MAP",
             fluidRow(
               column(width = 12,
                      tags$h5(tags$span("1Q23기준",tags$br(), "박스크기: 전체 순매수 규모, 박스색깔: 순매수 강도", style="color:green;"))
               )
             ),
             
             plotOutput("treemapPlot1")
          
    ),
    
    
    # Second tab
    tabPanel("최근 순매도 종목 MAP",
             fluidRow(
               column(width = 12,
                      tags$h5(tags$span("1Q23기준",tags$br(), "박스크기: 전체 순매도 규모, 박스색깔: 순매도 강도", style="color:green;"))
               )
             ),
             plotOutput("treemapPlot2")
             
    ),
  )
)

