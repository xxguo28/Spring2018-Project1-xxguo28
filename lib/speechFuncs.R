f.plotsent.len1=function(In.list, InPeriod){
  
  col.use=c("lightgray", "firebrick", "lightblue2", "firebrick4", "firebrick2",  
            "lightblue3", "firebrick1", "blueviolet", "lightblue1", "black", "lightgreen")
  
  In.list$topemotion=apply(select(In.list, 
                                  anger:positive), 
                           1, which.max)
  In.list$topemotion.v=apply(select(In.list,
                                    anger:positive), 
                             1, max)
  In.list$topemotion[In.list$topemotion.v<0.05]=0
  In.list$topemotion=In.list$topemotion+1
  
  temp=In.list$topemotion.v
  In.list$topemotion.v[temp<0.05]=1
  
  df=In.list%>%filter(Period == InPeriod)%>%
    select(sent.id, word.count, 
           topemotion, topemotion.v)
  
  ptcol.use=alpha(col.use[df$topemotion], sqrt(sqrt(df$topemotion.v)))
  
  plot(df$sent.id, df$word.count, 
       col=ptcol.use,
       type="h", #ylim=c(-10, max(In.list$word.count))
       main=paste("Period", InPeriod))
}

