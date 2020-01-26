#ui.R
#Author: Kadiresan Dhanasekaran
#Date: 22-Jan-2020
#Description: Final Capstone Project - John Shiny Server, Coursera Data Science Capstone Final Project

library(shiny)
library(shinythemes)
library(markdown)
library(dplyr)
library(quanteda)

#ngramToPredict")

shinyUI(
  

  
  
    navbarPage("Next Word Predict",
               theme = shinytheme("spacelab"),
               tabPanel("Home",
                        fluidPage(
                            titlePanel("Home"),
                            sidebarLayout(
                                sidebarPanel(
                                    textInput("inputText",
                                              "Input Your Message:",
                                              value =  "",
                                              placeholder = "Enter text here")
                                ),
                                mainPanel(
                                  h4("Actual Input typed so far",style = "color: coral; font-size: large;"),
                                  h4(textOutput("rawInput"), style = "text-align: left; padding-left: 30px; color: coral; border: double; border-bottom-left-radius: 15px 15px; border-bottom-right-radius: 15px 15px; border-top-right-radius: 15px 15px; border-top-left-radius: 15px 15px;"),                                                                           
                                  h3("Cleansed input phrase after removing profanity words",style = "color: black; font-size: large;"),
                                  h3(textOutput("cleansedInput"), style = "text-align: left; padding-left: 30px; color: black; border: double; border-bottom-left-radius: 15px 15px; border-bottom-right-radius: 15px 15px; border-top-right-radius: 15px 15px; border-top-left-radius: 15px 15px;"),                                   
                                  h2("Ngram used to predict the next word",style = "color: darkblue; font-size: large;"),
                                  h2(textOutput("ngramToPredict"), style = "text-align: center; color: darkblue; border: double; border-bottom-left-radius: 15px 15px; border-bottom-right-radius: 15px 15px; border-top-right-radius: 15px 15px; border-top-left-radius: 15px 15px;"),                                   
                                  h1("Predicted next word", style = "color: green;"),
                                  h1(textOutput("nextWord"), style = "text-align: center; color: darkgreen; border: double; border-bottom-left-radius: 15px 15px; border-bottom-right-radius: 15px 15px; border-top-right-radius: 15px 15px; border-top-left-radius: 15px 15px;")
   
                                )
                            )
                        )
               ),
               tabPanel("About",
                        h3("About Next Word Predict"),
                        br(),
                        div("Next Word Predict is a Shiny app that uses a text
                            prediction algorithm to predict the next word(s)
                            based on text entered by a user.",
                            br(),
                            br(),
                            "The predicted next word is based on bi, tri, quad gram logic where in it takes upto last 3 words typed and uses to predict the next word based on already created model.
                            The app is a reactive app hence as you type, it starts searching the next word. Pls allow couple of seconds for the value to appear",
                            br()
                        ),
                        br(),
                        h3("About Me"),
                        br(),
                        div("My name is Kadiresan Dhanasekaran. I am a Director of Projects at a global consulting firm. I am versatile IT leader with 17+ years of experience, proven track record in delivering major, complex IT initiatives, having hands-on expertise in diverse technologies, leading and mentoring teams. 
I have a keen focus on exceptional service level management and eye for implementing a cost effective and scalable solutions.", 
br(),
"I am also passionate about learning new technologies and products. Machine Learning, AI & Robotics process automation are fascinating to me and I am currently keen in learning & understand them better.
",
                            br()
                        )
               )
    )    
)
