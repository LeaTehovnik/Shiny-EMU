# define the server-side logic of the Shiny application


shinyServer(function(input, output) {
  my.data <- read.table("Eurostat_Table.txt", header = TRUE, sep="\t", dec=",")
  barva = sample(colours(), 30, replace=FALSE)

#_________________________GRAPHICAL COMPARISON__________________________________  
  output$plots<-renderPlot({
    x<-seq(2005,2016,by=1)
    cols.dont.want<-c("geo","Region")
    y<-my.data[my.data$geo==input$Country, ! names(my.data) %in% cols.dont.want, drop=F ]
    y2<-my.data[my.data$geo==input$Country2, ! names(my.data) %in% cols.dont.want, drop=F ]
    y3<-my.data[my.data$geo==input$Country3, ! names(my.data) %in% cols.dont.want, drop=F ]
    minimum <- min(y,y2,y3)
    maksimum <- max(y,y2,y3)
    plot(x,y, 
         main="Analysis of countries throughout years", type='l', ylim = c(minimum,maksimum), xlab = "year", ylab = "interest rate", lwd=3, col="gold",cex.main = 2)
    lines(x,y2, col = "tomato3",lwd=3)
    lines(x,y3, col = "lightseagreen",lwd=3)
    legend("topright", legend = c(input$Country,input$Country2,input$Country3), fill = c("gold","tomato3","lightseagreen")) })
    
#______________________COMPARISON OF COUNTRIES THROUGHOUT THE YEARS________________    
    output$table1 <- renderTable({
      cols.dont.want<-c("geo","Region")
      x <- t(my.data[my.data$geo == input$Country,! names(my.data) %in% cols.dont.want, drop=F])
      colnames(x) <- input$Country
      x2 <- t(my.data[my.data$geo == input$Country2,! names(my.data) %in% cols.dont.want, drop=F])
      colnames(x2) <- input$Country2
      x3 <- t(my.data[my.data$geo == input$Country3,! names(my.data) %in% cols.dont.want, drop=F])
      colnames(x3) <- input$Country3
      table <- cbind("Year"=c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"),x,x2,x3)
      return(table)
    }) 
#_____________________BARPLOT OF COUNTRIES________________________    
    output$hist<-renderPlot({
      drzave<-my.data$geo
      leto1<-input$Year - 2003
      leto <- my.data[,leto1]
      #maksi<-max(leto)
      barplot(leto, names.arg=drzave, main = input$Year, col=barva, las =2, ylab ="interest rate", cex.names= 0.86, cex.main = 2) #, ylim=maksi)
      id<-which(my.data==input$AnnualCountry)
      abline(h=leto[id], col ="red", lty=1)  
      
      
    })
 
# _____________________________COUNTRY ANALYSIS______________________________   
    output$countryTable <- renderTable({
      cols.dont.want<-c("geo","Region")
      values <- as.numeric(my.data[my.data$geo == input$CountryAnalysis,! names(my.data) %in% cols.dont.want, drop=F])
      id <- which(my.data == input$AnnualCountry)
      maxYear <- which(t(values) == max(values)) + 2004
      minYear <- which(t(values) == min(values)) +2004
      
      if(is.null(input$Choices)){return()}
      
      Max      <- "Max"       %in% input$Choices
      Min    <- "Min"     %in% input$Choices
      Mean <- "Mean"  %in% input$Choices
      
      if (Max & Min & Mean){
        return(table <- cbind("\ " = c("Value", "Year"),"Max" = c(max(values), maxYear[1]), "Min" = c(min(values), minYear[1]),
                              "Mean" = c(round(mean(values),2), "\ ")))
       
      } 
      else if (Max & Mean){
        return(table <- cbind("\ " = c("Value", "Year"),"Max" = c(max(values), maxYear[1]), "Mean" = c(round(mean(values),2),  "\ ")))
      }else if (Max & Min){
        return(table <- cbind("\ " = c("Value", "Year"),"Max" = c(max(values), maxYear[1]), "Min" = c(min(values), minYear[1])))
      }
      else if (Min & Mean){
        return(table <- cbind("\ " = c("Value", "Year"), "Min" = c(min(values), minYear[1]), "Mean" = c(round(mean(values),2),  "\ ")))
      }else if (Max){
        return(table <- cbind("\ " = c("Value", "Year"),"Max" = c(max(values), maxYear[1])))
      } else if (Min){
        return(table <- cbind("\ " = c("Value", "Year"), "Min" = c(min(values), minYear[1])))
      } else if (Mean) {
        return(table <- cbind("\ " = c("Value", "Year"),"Mean" = c(round(mean(values),2), "\ ")))
      } 
    })
      
      output$countryPlot<-renderPlot({
        x<-seq(2005,2016,by=1)
        cols.dont.want<-c("geo","Region")
        y<-my.data[my.data$geo==input$CountryAnalysis, ! names(my.data) %in% cols.dont.want, drop=F ]
        plot(x,y, 
             main=paste ("Analysis of", input$CountryAnalysis, sep = " "), 
             type='l', lwd=2, col="navyblue",cex.main = 2,
             
             xlab = "year", ylab = "interest rate")
        y1<-as.numeric(y)
        legend("topright", legend = "Mean", fill = "red")
        
       
        
        if(is.null(input$Choices)){return()}
        
        
        else if ("Mean" %in% input$Choices){return(abline(h=mean(y1), col ="red", lwd=1.5))}
        
        
        
       
      
      
      })
      
     
    
      
      
 
 
})