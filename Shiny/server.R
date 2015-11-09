library(shiny)
library(shinyBS)
require(ggplot2)
require(reshape2)
require(plyr)


# Define server logic
shinyServer(function(input, output, session) {

 
  ## PART ONE: Parse search input to get user average

  # get text for header
  output$queryText <- renderUI({
    nullcheck <- parseQueryString(session$clientData$url_search)$curr
    
    if (is.null(nullcheck))
    {
      str1 = paste("<p>Morality scores closer to zero tend to indicate more emotional, 'deontological' decision making, whereas scores near 7 are related with rational utilitarianism.</p><br><br><p> Explore the graphs below to see how Moral Sense Test participants tend to score on our surveys, broken down by demographics.</p>")
      HTML(str1)
    }
    else
    {
      query<-parseQueryString(session$clientData$url_search)
    
          # Get the averages from the search string and convert to char
          query$morav<-round(as.numeric(query$morav), digits=1)
          query$morav<-as.character(query$morav)
          query$empav<-round(as.numeric(query$empav), digits=1)
          query$empav<-as.character(query$empav)
          query$disav<-round(as.numeric(query$disav), digits=1)
          query$disav<-as.character(query$disav)
          
          # which test did they just take? display based on that
          if (query$curr=="moral")
          {
            str1 <- paste("<p>Your average morality judgment was")
            str2 <- paste("out of 7. The <font color='red'>red line</font> represents your score! Scores closer to zero tend to indicate more emotional, 'deontological' decision making, whereas scores near 7 are related with rational utilitarianism.</p><br><br><p> Explore the graphs below to see how your score compares to other participants.</p>")
            HTML(str1,query$morav,str2)
          } 
          else if (query$curr=="empathy")
          {
            str1 <- paste("<p>Your average empathy judgment was")
            str2 <- paste("out of 5. The <font color='red'>red line</font> represents your score! </p><br><br><p>Explore the graphs below to see how your score compares to other participants.</p>")
            HTML(str1,query$empav,str2)
          }
          else if (query$curr=="disgust")
          {
            str1 <- paste("<p>Your average disgust judgment was")
            str2 <- paste("out of 5. The <font color='red'>red line</font> represents your score! </p><br><br><p>Explore the graphs below to see how your score compares to other participants.</p>")
            HTML(str1,query$disav,str2)
          }}
          
          
         
       })
  
  # now, get numeric values for use in the graphs
  moralAv <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    # If we got the average..
    num<-as.numeric(query$morav)
    round(num, digits=2)
    
  })
  
  empAv <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    # If we got the average..
    num<-as.numeric(query$empav)
    round(num, digits=2)
    
  })
  
  disAv <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    # If we got the average..
    num<-as.numeric(query$disav)
    round(num, digits=2)
    
  })

  #get the current test, for alerts
currType <- reactive({
  nullcheck <- parseQueryString(session$clientData$url_search)$curr
  
  if (is.null(nullcheck)) {return("nothing")}
  else {
    query <- parseQueryString(session$clientData$url_search)
    # report the current test type
    query$curr
  }
   
  })

#echo based on whether its a user or not
output$link <- renderUI({
  nullcheck <- parseQueryString(session$clientData$url_search)$curr
  
  if (is.null(nullcheck))
  { return(h4(p("Want to find out how you score on other tests? Return to the",
                strong(a("MST", href="http://www.moralsensetest.com/index.html")),
                "to participate in another study.")))}
  else
  {
      query <- parseQueryString(session$clientData$url_search)
      
      if (query$curr == "profile")
      {return(h4(p("Want to find out how you score on other tests? Scroll down to participate in another study.")))}
      else
      {

        return(h4(p("Want to find out how you score on other tests? Return to the",
                strong(a("MST", href="http://www.moralsensetest.com/user.php")),
                "to participate in another study.")))}
  }
  

  
})

output$debrief <-renderUI({
  type<-input$type;
  query <- parseQueryString(session$clientData$url_search)
  
  if (is.null(query$curr) &&is.null(query$moral)&&is.null(query$emp)&&is.null(query$dis)){return(p())}
  
  else if (type=="moral"&&!is.na(moralAv())) {
    return(h5(p("People usually say that it's morally permissible to divert a trolley on to a different track",
                "such that it kills one person instead of five. This is characteristically utilitarian response.",
                "On the other hand, they also say that it is NOT permissible to push someone on to the tracks in front of the trolley, such that the one person is killed",
                "but the five are saved-- here, they take a deontological perspective. Clearly, moral judgments are inconsistent. Why? This puzzle is known as the trolley problem. In both cases, they are considering saving five lives at the expense of one.",
                "Scientists have suggested that these conflicting responses reflect two competing cognitive processes. The first response (the one we almost always have) is the controlled, reasoned utilitarian judgment",
                "which dictates that it is better to save more lives. The second, which arises only in some cases, is a negative emotional response that rings internal alarm bells. Think about how bad",
                "it would feel to actually push someone off a bridge to their death! Much research has indicated that these two psychological pathways, which are distinct in the brain, are constantly playing tug-of-war when it comes to dictating your moral judgments.",
                "For more information, you can find academic papers on this topic",
                a("here,",href="http://cushmanlab.fas.harvard.edu/docs/hauser&etal_2007.pdf"),
                a("here,",href="https://static.squarespace.com/static/54763f79e4b0c4e55ffb000c/t/5477ccc3e4b01fb132f9bcc3/1417137347517/an-fmri-investigation-of-emotional-engagement-in-moral-judgment.pdf"),
                "and", a("here.",href="https://static.squarespace.com/static/54763f79e4b0c4e55ffb000c/t/5477cd59e4b07cb49aa9009b/1417137497687/the-neural-bases-of-cognitive-conflict-and-control-in-moral-judgment.pdf"))))
  }
  else if (type=="empathy"&&!is.na(empAv())) {
    return(h5(p("Emotions play an important role in dictating moral judgment. In our research, compassion is a particularly relevant personality trait. ",
                "Having empathy for others means that we (usually) don't want to see other people getting hurt. We're able to put ourselves in someone elses shoes and reason about how bad it must feel. The extent to which you feel empathy partially determines ",
                "how you react in tricky ethical situations. For example, a person who is less sensitive to the pain of others is more likely to endorse sacrificing one person ",
                "for the good of the many. On the other hand, those who score high on our empathy scale tend to be more generous and less selfish in dilemmas involving their own personal gain.",
                "You can learn more about this topic",
                a("here",href="http://static1.squarespace.com/static/4ff4905c84aee104c1f4f2c2/t/5084da28e4b066390d161d56/1350883880200/Pizarro+2000.pdf"),"and",
                a("and here.",href="http://www.poptech.org/popcasts/fiery_cushman_studying_harm"))))
  }
  else if (type=="disgust"&&!is.na(disAv())) {
    return(h5(p("Disgust is a visceral response to interacting with things, people or actions that we find to be wrong or bad.",
                "It makes intuitive sense that feelings of disgust factor in to moral decision making. The more disgusted we are by an unethical action,",
                "the more likely we are to judge the action (and its perpetrator) harshly. Conversely, people who score low on disgust scales might",
                "be more willing to endorse utilitarian actions that require doing some harm to bring about a greater good. Some esearchers have theorized that",
                "disgust is the embodiment of the emotional, deontological response we have to moral dilemmas.",
                "You can find more information",
                a("here",href="https://static.squarespace.com/static/54763f79e4b0c4e55ffb000c/t/5477ccf2e4b07347e76a53c9/1417137394112/how-and-where-does-moral-judgment-work.pdf"),
                "and", a("here.",href="http://gruberpeplab.com/teaching/psych131_summer2013/documents/Lecture11_Pizarro2011_DisgustMoralJudgment.pdf"))))
  }
  else {return()}
  
  
  
  
  
})
#redo that to echo into UI for conditional Panel
output$curr <- renderUI({
  nullcheck <- parseQueryString(session$clientData$url_search)$curr
  
  if (is.null(nullcheck))
  { return(radioButtons("type", "Data Type:",
                        c("Morality"="moral",
                          "Empathy"="empathy",
                          "Disgust"="disgust"),selected="moral"))}
  else
  {
    query <- parseQueryString(session$clientData$url_search)
    
    if (query$curr=="moral") {return(radioButtons("type", "Data Type:",
                                                  c("Morality"="moral",
                                                    "Empathy"="empathy",
                                                    "Disgust"="disgust"),selected="moral"))}
    else if (query$curr=="empathy") {return(radioButtons("type", "Data Type:",
                                                         c("Morality"="moral",
                                                           "Empathy"="empathy",
                                                           "Disgust"="disgust"),selected="empathy"))}
    else if (query$curr=="disgust") {return(radioButtons("type", "Data Type:",
                                                         c("Morality"="moral",
                                                           "Empathy"="empathy",
                                                           "Disgust"="disgust"),selected="disgust"))}
    else if (query$curr=="profile") {return(radioButtons("type", "Data Type:",
                                                       c("Morality"="moral",
                                                         "Empathy"="empathy",
                                                         "Disgust"="disgust"),selected="moral"))}
  }
  
  
})
  


  ## PART TWO: The plots themselves
  ## Morality plots
  output$whole_plot <- renderPlot({
    
    
    type<-input$type;
    n <- as.numeric(input$n_bins);
    
    if (type == as.character(currType())||(as.character(currType())=="profile" && ((type=="moral"&&!is.na(moralAv()))||(type=="empathy"&&!is.na(empAv()))||(type=="disgust"&&!is.na(disAv()))))) {
      closeAlert(session,"Alert")
    }
    else {
      if (as.character(currType())=="profile" && ((type=="moral"&&is.na(moralAv()))||(type=="empathy"&&is.na(empAv()))||(type=="disgust"&&is.na(disAv()))))
      {
        createAlert(session, "alert", "Alert", title = "You haven't taken this test yet!", 
                    "Curious? Scroll down to take this test and complete your profile.", append = FALSE);
      }
      else if (as.character(currType())!="nothing")
      {
      createAlert(session, "alert", "Alert", title = "Have you taken this test yet?", 
                  content = paste("Curious? Go back to the", a("MST", href="http://www.moralsensetest.com/user.php"), "to find out how you rank on this measure."), append = FALSE);
      }
      else
      {
        createAlert(session, "alert", "Alert", title = "Have you taken this test yet?", 
                    content = paste("Curious? Go back to the", a("MST", href="http://www.moralsensetest.com/index.html"), "to find out how you rank on this measure."), append = FALSE);
      }
    }
    if (as.character(currType())!="nothing") {
      if (type=="moral") {
        
        ggplot(data=mor,aes(x=moralmeans))+
          geom_histogram(binwidth=5/n,aes(fill=..x..)) +
          geom_density(aes(y=..density..))+
          scale_fill_gradientn(colours=terrain.colors(4), guide=FALSE)+
          geom_vline(xintercept = moralAv(),colour="red",size=2)+
          ggtitle(paste('Population Distribution of Morality'))+
          xlab(paste('Average Morality Score'))+
          ylab(paste('Number of Participants'))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
        
      }
      else if (type=="empathy") {
        ggplot(data=emp,aes(x=empathymeans))+
          geom_histogram(binwidth=5/n,aes(fill=..x..)) +
          geom_density(aes(y=..density..))+
          scale_fill_gradientn(colours=terrain.colors(4),guide=FALSE)+
          geom_vline(xintercept = empAv(),colour="red",size=2)+
          ggtitle(paste('Population Distribution of Empathy'))+
          xlab(paste('Average Empathy Score'))+
          ylab(paste('Number of Participants'))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
        
      }
      else if (type=="disgust"){
        ggplot(data=dis,aes(x=disgustmeans))+
          geom_histogram(binwidth=5/n,aes(fill=..x..)) +
          geom_density(aes(y=..density..))+
          scale_fill_gradientn(colours=terrain.colors(4),guide=FALSE)+
          geom_vline(xintercept = disAv(),colour="red",size=2)+
          ggtitle(paste('Population Distribution of Disgust'))+
          xlab(paste('Average Disgust Score'))+
          ylab(paste('Number of Participants'))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
      }
    }
    else{
    if (type=="moral") {
      
      ggplot(data=mor,aes(x=moralmeans))+
        geom_histogram(binwidth=5/n,aes(fill=..x..)) +
        geom_density(aes(y=..density..))+
        scale_fill_gradientn(colours=terrain.colors(4), guide=FALSE)+
        #geom_vline(xintercept = moralAv(),colour="red",size=2)+
        ggtitle(paste('Population Distribution of Morality'))+
        xlab(paste('More Emotional                                                                              More Rational'))+
        ylab(paste('Number of Participants'))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
      
    }
    else if (type=="empathy") {
      ggplot(data=emp,aes(x=empathymeans))+
        geom_histogram(binwidth=5/n,aes(fill=..x..)) +
        geom_density(aes(y=..density..))+
        scale_fill_gradientn(colours=terrain.colors(4),guide=FALSE)+
        #geom_vline(xintercept = empAv(),colour="red",size=2)+
        ggtitle(paste('Population Distribution of Empathy'))+
        xlab(paste('Average Empathy Score'))+
        ylab(paste('Number of Participants'))+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
      
    }
    else if (type=="disgust"){
      ggplot(data=dis,aes(x=disgustmeans))+
        geom_histogram(binwidth=5/n,aes(fill=..x..)) +
        geom_density(aes(y=..density..))+
        scale_fill_gradientn(colours=terrain.colors(4),guide=FALSE)+
        #geom_vline(xintercept = disAv(),colour="red",size=2)+
        ggtitle(paste('Population Distribution of Disgust'))+
        xlab(paste('Average Disgust Score'))+
        ylab(paste('Number of Participants'))+
       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    }
    
  })
  
  ## Gender plots
  output$gender_plot <- renderPlot({
    type<-input$type;

    n <- as.numeric(input$n_bins);
    
    if (as.character(currType())!="nothing")
    {
    if (type=="moral") {
      
      ggplot(data=mor,aes(x=moralmeans,fill=gender))+
        geom_density(alpha=.3)+
        geom_vline(xintercept = moralAv(),colour="red",size=2)+
        ggtitle(paste('Distribution of Morality by Gender'))+
        xlab(paste('More Emotional                                                                              More Rational'))+
        ylab(paste('Number of Participants'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else if (type=="empathy") {
      ggplot(data=emp,aes(x=empathymeans,fill=gender))+
        geom_density(alpha=.3)+
        geom_vline(xintercept = empAv(),colour="red",size=2)+
        ggtitle(paste('Distribution of Empathy by Gender'))+
        xlab(paste('Average Empathy Score'))+
        ylab(paste('Number of Participants')) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else if (type=="disgust"){
      ggplot(data=dis,aes(x=disgustmeans,fill=gender))+
        geom_density(alpha=.3)+
        geom_vline(xintercept = disAv(),colour="red",size=2)+
        ggtitle(paste('Distribution of Disgust by Gender'))+
        xlab(paste('Average Disgust Score'))+
        ylab(paste('Number of Participants'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    }
    else {
      if (type=="moral") {
        
        ggplot(data=mor,aes(x=moralmeans,fill=gender))+
          geom_density(alpha=.3)+
          #geom_vline(xintercept = moralAv(),colour="red",size=2)+
          ggtitle(paste('Distribution of Morality by Gender'))+
          xlab(paste('More Emotional                                                                              More Rational'))+
          ylab(paste('Number of Participants'))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
      }
      else if (type=="empathy") {
        ggplot(data=emp,aes(x=empathymeans,fill=gender))+
          geom_density(alpha=.3)+
          #geom_vline(xintercept = empAv(),colour="red",size=2)+
          ggtitle(paste('Distribution of Empathy by Gender'))+
          xlab(paste('Average Empathy Score'))+
          ylab(paste('Number of Participants')) +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
      }
      else if (type=="disgust"){
        ggplot(data=dis,aes(x=disgustmeans,fill=gender))+
          geom_density(alpha=.3)+
          #geom_vline(xintercept = disAv(),colour="red",size=2)+
          ggtitle(paste('Distribution of Disgust by Gender'))+
          xlab(paste('Average Disgust Score'))+
          ylab(paste('Number of Participants'))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
      }
    }
  })
  
  ## Religion plots
  output$religion_plot <- renderPlot({
    
    type<-input$type;
   
    n <- as.numeric(input$n_bins);
    
    if (as.character(currType())!="nothing")
      {
    if (type=='moral') {
      ggplot(data=mor,aes(relig1,moralmeans,fill=relig1))+
        geom_violin(aes(colour=relig1)) +
        geom_point(data=mor_relig,aes(x=relig1,y=mean))+
        geom_hline(yintercept = moralAv(),colour="red",size=2)+
        theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
        ggtitle(paste('Distribution of Morality by Primary Religion'))+
        ylab(paste('More Emotional                                                                              More Rational'))+
        xlab(paste('Religion'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else if (type=="empathy") {
      ggplot(data=emp,aes(relig1,empathymeans,fill=relig1))+
        geom_violin(aes(colour=relig1)) +
        geom_point(data=emp_relig,aes(x=relig1,y=mean))+
        geom_hline(yintercept = empAv(),colour="red",size=2)+
        theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
        ggtitle(paste('Distribution of Empathy by Primary Religion'))+
        ylab(paste('Average Empathy Score'))+
        xlab(paste('Religion'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    else if (type=="disgust"){
      ggplot(data=dis,aes(relig1,disgustmeans,fill=relig1))+
        geom_violin(aes(colour=relig1)) +
        geom_point(data=dis_relig,aes(x=relig1,y=mean))+
        geom_hline(yintercept = disAv(),colour="red",size=2)+
        theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
        ggtitle(paste('Distribution of Disgust by Primary Religion'))+
        ylab(paste('Average Disgust Score'))+
        xlab(paste('Religion'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    }
    else{  if (type=='moral') {
      ggplot(data=mor,aes(relig1,moralmeans,fill=relig1))+
        geom_violin(aes(colour=relig1)) +
        geom_point(data=mor_relig,aes(x=relig1,y=mean))+
        #geom_hline(yintercept = moralAv(),colour="red",size=2)+
        theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
        ggtitle(paste('Distribution of Morality by Primary Religion'))+
        ylab(paste('More Emotional                                                                              More Rational'))+
        xlab(paste('Religion'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
      else if (type=="empathy") {
        ggplot(data=emp,aes(relig1,empathymeans,fill=relig1))+
          geom_violin(aes(colour=relig1)) +
          geom_point(data=emp_relig,aes(x=relig1,y=mean))+
          #geom_hline(yintercept = empAv(),colour="red",size=2)+
          theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
          ggtitle(paste('Distribution of Empathy by Primary Religion'))+
          ylab(paste('Average Empathy Score'))+
          xlab(paste('Religion'))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
      }
      else if (type=="disgust"){
        ggplot(data=dis,aes(relig1,disgustmeans,fill=relig1))+
          geom_violin(aes(colour=relig1)) +
          geom_point(data=dis_relig,aes(x=relig1,y=mean))+
          #geom_hline(yintercept = disAv(),colour="red",size=2)+
          theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
          ggtitle(paste('Distribution of Disgust by Primary Religion'))+
          ylab(paste('Average Disgust Score'))+
          xlab(paste('Religion'))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
      }
      }
  })
  
})
