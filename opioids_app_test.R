#Setup-- loading in librarys

library(shiny)
library(leaflet)
library(ggmap)
library(ggplot2)
library(dplyr)
library(readxl)
library(corrplot)
library(shinydashboard)
library(plotly)

#state_data from http://www.presidency.ucsb.edu/showelection.php?year=2016
state_data<-read_excel('state_election_data.xlsx',sheet = 2)

#opioids_data from CDC 2015 Data https://www.cdc.gov/drugoverdose/data/statedeaths.html
opioids_data<-read.csv('death_by_state_opioids.csv')
state.name_new<-append(state.name,"Dist. of Col.",9)
opioids_data$State<-state.name_new
state_df<-as.data.frame(state_data)


#Combing data and doing some transformation/cleaning

new_joined_data<-inner_join(state_df,opioids_data)
new_joined_data$lat<-c(32.31823,64.20084,34.04893,35.20105,36.77826,39.55005,41.60322,38.91083,38.90719,27.66483,32.16562,19.89677,44.0682,40.63312,40.26719,41.878,39.0119,37.83933,30.9843,45.25378,39.04575,42.40721,44.31484,46.72955,32.35467,37.96425,46.87968,41.49254,38.80261,43.19385,40.05832,34.51994,40.71278,35.75957,47.55149,40.41729,35.00775,43.80413,41.20332,41.58009,33.83608,43.96951,35.51749,31.9686,39.32098,44.5588,37.43157,47.75107,38.59763,43.78444,43.07597)
new_joined_data$lng<-c(-86.9023,-149.4937,-111.0937,-91.83183,-119.4179,-105.7821,-73.08775,-75.52767,-77.03687,-81.51575,-82.90008,-155.5828,-114.742,-89.39853,-86.1349,-93.0977,-98.48425,-84.27002,-91.96233,-69.44547,-76.64127,-71.38244,-85.60236,-94.6859,-89.39853,-91.83183,-110.3626,-99.90181,-116.4194,-71.5724,-74.40566,-105.8701,-74.00594,-79.0193,-101.002,-82.90712,-97.09288,-120.5542,-77.19452,-71.47743,-81.16372,-99.90181,-86.58045,-99.90181,-111.0937,-72.57784,-78.65689,-120.7401,-80.4549,-88.78787,-107.2903)
new_joined_data$State[9]="Washington DC"
new_joined_data$Standardized_number<-0
new_joined_data$Number<-as.numeric(gsub(",","",as.character(new_joined_data$Number)))


for (i in 1:length(new_joined_data$State)){
  new_joined_data$Standardized_number[i]<-((as.numeric(new_joined_data$Number[i]))/75)
  
}

new_joined_data$State_winner<-0



##No longer needed because it was making everything run too slow, but this is how I got the lat/lng for states
# add_lat_lng<-function(data_to_add){
#   
#  for (i in (1:length(data_to_add$State))){
#     data_to_add$lat[i]<-geocode(data_to_add$State[i])[2]
#     
#   }
#   for (j in (1:length(data_to_add$State))){
#     data_to_add$lng[j]<-geocode(data_to_add$State[j][1])
#   }
#   return(data_to_add)
#   
# }

#new_joined_data<-add_lat_lng(joined_data)



#Making the numbers actual numbers instead of strings

new_joined_data$Trump_votes<-as.numeric(gsub(",","",as.character(new_joined_data$Trump_votes)))

new_joined_data$Clinton_votes<-as.numeric(gsub(",","",as.character(new_joined_data$Clinton_votes)))

new_joined_data$Trump_percentage<-as.numeric(gsub("%","",as.character(new_joined_data$Trump_percentage)))

new_joined_data$Clinton_percentage<-as.numeric(gsub("%","",as.character(new_joined_data$Clinton_percentage)))

#Add who won the state as either 1(Trump) or 0(Clinton)

for(i in 1:length(new_joined_data$State)){
  if(new_joined_data$Clinton_percentage[i]>new_joined_data$Trump_percentage[i]){
    new_joined_data$State_winner[i]<-0
  }else if(new_joined_data$Trump_percentage[i]>new_joined_data$Clinton_percentage[i]){
    new_joined_data$State_winner[i]<-1
  }
}

#Adding the strength of the win-- 0 for Clinton, .5 for a swing (+/- 5%) or 1 for Trump
for(i in 1:length(new_joined_data$State)){
  if(findInterval(new_joined_data$Trump_percentage[i],c(45,55))==1){
    new_joined_data$State_swing[i]<-.5
  }else if((findInterval(new_joined_data$Trump_percentage[i],c(45,55))!=1) & new_joined_data$Clinton_percentage[i]>new_joined_data$Trump_percentage[i]){
    new_joined_data$State_swing[i]<-0
  }else{
    new_joined_data$State_swing[i]<-1
  }
}

#creating continious color palattes to indicate what % of the population voted for the winning candidate of that state--- percent of votes works better than straight votes

pal_clinton<-colorNumeric(palette="Blues",domain= new_joined_data$Clinton_votes)
pal_trump<-colorNumeric(palette = "Reds", domain = new_joined_data$Trump_votes)

pal_clinton_percent<-colorNumeric(palette = "Blues",domain = new_joined_data$Clinton_percentage)
pal_trump_percent<-colorNumeric(palette = "Reds",domain = new_joined_data$Trump_percentage)

##Leaflet creation Function

opioids_election_map<-function(data_4_map,ratevstotal){
  
  leaflet(data_4_map) %>%
    setView(lat=38.33,lng=-101.17,zoom=3)%>%
    addProviderTiles("Stamen.TonerLite")
}

look_at_list<-c('',"Mortality Rate","Number of Overdoses")


#make a Correlations plot and do some transformation our DF
only_numerics<-cbind(new_joined_data$Clinton_votes,new_joined_data$Trump_votes,new_joined_data$Trump_percentage,new_joined_data$Clinton_percentage,new_joined_data$State_winner,new_joined_data$Mortality_Rate,new_joined_data$Number)
colnames(only_numerics)<-c('Clinton Votes','Trump Votes','Trump Percentage','Clinton Percentage','State Winner','Mortality Rate','Number of overdoses')
cor_setup<-cor(only_numerics)


#Make a correlations plot without the two highest cases (OH and CA)
no_outliers<-new_joined_data[c(-5,-36),]

only_numerics_no_outliers<-cbind(no_outliers$Clinton_votes,no_outliers$Trump_votes,no_outliers$Trump_percentage,no_outliers$Clinton_percentage,no_outliers$State_winner,no_outliers$Mortality_Rate,no_outliers$Number)
colnames(only_numerics_no_outliers)<-c('Clinton Votes','Trump Votes','Trump Percentage','Clinton Percentage','State Winner','Mortality Rate','Number of overdoses')
cor_setup_no<-cor(only_numerics_no_outliers)

corr_plottr<-function(outliers){
  if(outliers=="Yes"){
    corrplot(cor_setup_no,method = 'number')
  }else{
    corrplot(cor_setup,method = 'number')
  }
  
}

#Plotting Function for scatter plots
scatter_plotter<-function(data,ratevstotal){
  
  if(ratevstotal=="Mortality Rate"){
    g<-ggplot(data,aes(x=data$State_swing,y=data$Mortality_Rate,text=paste(data$State,'mortalitly rate is',data$Mortality_Rate)))+geom_point(aes(colour=factor(data$State_winner)))+scale_x_continuous(breaks=c(0,.5,1),labels=c("Clinton Safe","Swing State","Trump Safe")) +
      xlab("State Result")+ylab("Mortality Rate")+scale_colour_manual(values = c('blue','red'),breaks = c(0,1),labels = c('Clinton','Trump'),name = ('State Winner'))
  }else if(ratevstotal=="Number of Overdoses"){
    g<-ggplot(data,aes(x=data$State_swing,y=data$Number,text=paste(data$State,'number of overdoses is',data$Number)))+geom_point(aes(colour=factor(data$State_winner)))+scale_x_continuous(breaks=c(0,.5,1),labels=c("Clinton Safe","Swing State","Trump Safe")) +
      xlab("State Result")+ylab("Number of Overdoses")+scale_colour_manual(values = c('blue','red'),breaks = c(0,1),labels = c('Clinton','Trump'),name = ('State Winner'))
  }
  
  ggplotly(g, tooltip='text')
}
#UI

ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Relationship between Opioids and 2016 Election Results",titleWidth = 1200),
  #Sidebar with a slider input for number of bins
  dashboardSidebar(
    sidebarMenu(
      selectInput("ratevstotal",'Please select whether you would like to examine rate or total number of overdoses',look_at_list,selected = '',multiple = FALSE),
      selectInput('outliers','Would you like to remove the top two outliers for the corplot?',c('',"Yes","No"),selected = '',multiple = FALSE)
    )),
  
  # Show a plot of the generated distribution
  dashboardBody(
    fluidRow(
      column(width = 12,
             box("Map",leafletOutput('opioids_map')),
             box("Correlations Plot",plotOutput('corrplot'))
      ),
      column(width = 12,
             plotlyOutput('scatter_plot'))
    )
  )
)



server <- function(input, output) {
  
  output$opioids_map <- renderLeaflet({
    opioids_election_map(new_joined_data,input$ratevstotal)
  })
  
  output$corrplot<-renderPlot({
    corr_plottr(input$outliers)
  })
  
  output$scatter_plot<-renderPlotly({
    validate(
      need(input$ratevstotal,"Please select whether to look at mortality rate or number of overdoses")
    )
    scatter_plotter(new_joined_data,input$ratevstotal)
  })
  
  observeEvent(input$ratevstotal,{
    p=input$ratevstotal
    proxy=leafletProxy('opioids_map')
    if(p=="Mortality Rate"){
      proxy%>%
        clearMarkers()%>%
        addCircleMarkers(lng=as.numeric(new_joined_data$lng),lat=as.numeric(new_joined_data$lat),radius=new_joined_data$Mortality_Rate,
                         color=(ifelse(new_joined_data$Clinton_percentage>new_joined_data$Trump_percentage,pal_clinton_percent(new_joined_data$Clinton_percentage),pal_trump_percent(new_joined_data$Trump_percentage))),
                         popup=(paste(new_joined_data$State,"rate of opioid overdose per 100K", new_joined_data$Mortality_Rate)))
    }else if(p=="Number of Overdoses"){
      proxy%>%
        clearMarkers()%>%
        addCircleMarkers(lng=as.numeric(new_joined_data$lng),lat=as.numeric(new_joined_data$lat),radius=new_joined_data$Standardized_number,
                         color=(ifelse(new_joined_data$Clinton_percentage>new_joined_data$Trump_percentage,pal_clinton_percent(new_joined_data$Clinton_percentage),pal_trump_percent(new_joined_data$Trump_percentage))),
                         popup=(paste(new_joined_data$State,"Number of opioid overdoses is", new_joined_data$Number)))
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

