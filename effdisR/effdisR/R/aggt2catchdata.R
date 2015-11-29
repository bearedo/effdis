aggt2catchdata <-
  function(input=lllf,which.flag="Japan",which.effort='NO.HOOKS',start.year=1950,end.year=2015)
    
  {
    
    #input <- lllf; which.flag='Korea Rep.'; which.effort='NO.HOOKS'; start.year=1990; end.year=2015
    
    input <- input[input$year >= start.year & input$year <= end.year & input$eff1type == which.effort,]
    
    
    #input[input$trend == 517 & input$longitude == -12.5 & input$latitude == -12.5 & input$flagname == "Japan",]
    #input[input$trend == 517 & input$longitude == -12.5 & input$latitude == -2.5 & input$flagname == "Korea Rep.",]
    
    
    
    if(which.flag=='All')
    {
      input1 <- input; print('Modeling all data')
    }
    
    else
    {
      input1 <- input[input$flagname == which.flag,]
      #print(paste('Modeling',which.flag))
    }
    
    
      #input1[input1$trend == 517 & input1$longitude == -17.5 & input1$latitude == -2.5 & input1$flagname == "Korea Rep.",]
    
    
    catch <- aggregate(list(raw_measured_catch=input1$measured_catch), 
                        by=list(year=input1$year,month=input1$month,trend=input1$trend,month=input1$month,
                                longitude=input1$longitude,latitude=input1$latitude,species=input1$species),sum,na.rm=T)
    
    #input2[input2$trend == 517 & input2$longitude == -17.5 & input2$latitude == -2.5,]
    
    catch$flagname <- which.flag
    
    catch <- orderBy(~trend+species,data=catch)
    
    catch
  }
