library(readxl)
stock <- read_excel("股指.xlsx", col_types = c("date", 
                                             "numeric", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
nav <- read_excel("股指.xlsx", sheet = "单位净值", 
                  col_types = c("date", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric"))
cnav <- read_excel("股指.xlsx", sheet = "累计净值", 
                   col_types = c("date", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric"))

library(tidyverse)
library(dplyr)
df1=tibble(cnav)
df2=tibble(nav)
df3=tibble(stock)

df=inner_join(df3,df1,by = "日期")
df=inner_join(df,df2,by = "日期")

cnav.data=df[,c(1,8,10,12,14,16,18)]
nav.data=df[,c(1,8,20,22,24,26,28)]
cnav.data=cnav.data[-729,]
nav.data=nav.data[-729,]

name=c("日期","混合基准收益率","建信深证基本面60ETF","工银瑞信深证100ETF","国联安沪深300ETF","景顺长城成长之星股票A","保险主题LOF")
colnames(cnav.data)=name
colnames(nav.data)=name
cnav.name=names(cnav.data[,-1]);cnav.name
nav.name=names(nav.data[,-1]);nav.name
#shinny----
library(shiny)
library(plotly)


#定义UI----
ui <- fluidPage(
  titlePanel("基金收益率随时间变化图"),
  
  # 选择数据框输入
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset", label = "选择指标:", 
                  choices = list("累计净值" = "累计净值", "日净值" = "日净值")),
      
      # 选择基金收益率列
      checkboxGroupInput(inputId = "funds", label = "选择基金名称:", 
                         choices = cnav.name,  
                         selected = cnav.name) 
    ),
    
    # 主页面显示ggplot图表
    mainPanel(
      plotlyOutput("fundPlot")
    )
  )
)

# 服务器部分----
server <- function(input, output) {
  
  # 动态选择数据框
  selectedData <- reactive({
    if (input$dataset == "累计净值") {
      return(cnav.data)
    } else {
      return(nav.data)
    }
  })
  # 检查selectedData是否正确加载
  observe({
    print(head(selectedData()))
  })
  
  # 检查input$funds是否有效
  observe({
    print(input$funds)
  })
  
  # 绘制图形
  output$fundPlot <- renderPlotly({
    # 获取选择的数据
    data <- selectedData()
    
    data |> 
      pivot_longer(cols = input$funds, 
                   names_to = "基金", values_to = "净值增长率") |> 
      ggplot(aes(x = 日期, y = 净值增长率, color = 基金)) +
      geom_line(size=0.1) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

#调用----
shinyApp(ui = ui, server = server)



