Matrix_Plot<-function(Sql_String=NULL,En_Names,Cn_Names,Out_Process=FALSE,Out_Process_Type=c('IQR')){
  
  #load("F:/dailu/算法项目/硅钢项目/画图/Simu_DfC18.RDATA")
  
  library('sqldf')
  library('RJDBC')
  drv<-JDBC("com.ibm.db2.jcc.DB2Driver","/D:/R/db2jcc.jar",identifier.quote="\"")
  conn<-dbConnect(drv = drv,"jdbc:db2://10.70.16.170:50170/DBPRODE7","sibjy","sibjy" )
  sql_parameter='SELECT LAB_CNAME,SHOW_INDEX from BGRAGGRD.T_WH_PARAMETER WHERE 1=1 '
  parameter_Df<-try(dbGetQuery(conn = conn,sql_parameter))
  performance_list=list(parameter_Df[which(parameter_Df$SHOW_INDEX=="性能指标"),"LAB_CNAME"])
  if(missing(En_Names)|missing(Cn_Names)){
    return("En_Names or Cn_Names miss")
  }
  
  if(length(En_Names)!=length(Cn_Names)){
    return("length En_Names and  Cn_Names must match")
  }
  if(length(En_Names)==1){
    return("params must bigger than 1") 
  }
  
  require("sqldf",quietly = T)
  if(is.null(Sql_String)==1|missing(Sql_String)){
    return("sql missing")
  }else{
    
    Plot_Df<-try(dbGetQuery(conn = conn,Sql_String))
   
  }
  
  dbDisconnect(conn)




  Plot_Df<-as.data.frame(lapply(Plot_Df,as.numeric))
 Plot_Df<-na.omit(Plot_Df)
 if(nrow(Plot_Df)==0){
    return (list(Remind=0,M_P=0,Formula=0)) 
  }
  
  
  
  if(class(Plot_Df)=='type error'){
    return('sql error')
  }else{
    if(!all(En_Names%in%names(Plot_Df))){
      return("En_Names and sql must match")
    }
        Plot_Df<-Plot_Df[En_Names]
    
    # 
    if(Out_Process){
      Plot_Df<-Out_Process(Plot_Df,c(En_Names),type=Out_Process_Type)
      
      if(nrow(Plot_Df)==0){
       # stop("Out_Process return no data")
       return (list(Remind=0,M_P=0,Formula=0))
      }
    }
    
Formula<-c()
  for (i in 1:(length(Cn_Names)-1))
  { for (j in (i+1):length(Cn_Names))
  {x<-Plot_Df[,i]
   y<-Plot_Df[,j]
  fit<-lm(y~x)
  a <- fit$coefficients[2]
  b <- fit$coefficients[1]
  a <- round(a, 3)
  b <- round(b, 3)
 if(a==0){
if(b==0)
  {c<-paste(Cn_Names[j] ,"=" , " ","0", sep = "")}
  else
  { c<-paste(Cn_Names[j] ,"=" ," ", b, sep = "")}
  
  }else{
 if(b==0)
  {c<-paste(Cn_Names[j] ,"=" , " ",a,"*", Cn_Names[i], sep = "")}
  else if(b>0)
  { c<-paste(Cn_Names[j] ,"=" , " ",a,"*", Cn_Names[i] ," + ", b, sep = "")}
  else
  { c<-paste(Cn_Names[j] ,"=" , " ",a,"*", Cn_Names[i] ," ", b, sep = "")} }  # print(c)
  j=j+1
  Formula<-c(Formula,c)
  }
    i=i+1
  }
colnames(Plot_Df)=Cn_Names
cor_plot=as.data.frame(abs(cor(Plot_Df)))
cor_plot=cor_plot[!rownames(cor_plot) %in% performance_list[[1]],]
cor_plot=cor_plot[colnames(cor_plot) %in% performance_list[[1]]]
if(length(cor_plot)!=0){
  plot_flag=1
plot_name=rownames(cor_plot)
cor_plot=cbind(plot_name,cor_plot)
plot_result=data.frame(matrix(numeric(0),nrow=nrow(cor_plot)))

for(i in 2:ncol(cor_plot)){
  cor_plot_1=cor_plot[,c(1,i)]
cor_plot_1=cor_plot_1[order(cor_plot_1[,2],decreasing=TRUE),]
colnames(cor_plot_1)[1]=colnames(cor_plot_1)[2]
plot_result=cbind(plot_result,cor_plot_1)
}
performance_name=colnames(plot_result)
plot_result=rbind(performance_name,plot_result)
row.names(plot_result) <- c(1:nrow(plot_result))
}
  else{
    plot_flag=0
    plot_result=data.frame()
  }

    # names(Plot_Df)<-Cn_Names
    options(bitmapType='cairo')
    
    require("GGally",quietly = T)
    require('uuid',quietly = T)
     library('showtext')
     showtext_auto()
     font_add("SimSun", "/apps/R_PROJECT/fonts/Fonts/simsun.ttc")
    #  
    # Save_Path<-"D:/code/work/SIDS/RD_Simulation/data/"
    # Save_Name<-paste0(Save_Path,UUIDgenerate(TRUE),".png")
    # q<-ggpairs(Plot_Df, lower=list(continuous=wrap("smooth", color="blue")),
    #            diag=list(continuous=wrap("densityDiag",color="blue")), 
    #            upper=list(continuous=wrap("cor",color="blue")),columnLabels=Cn_Names, axisLabels='show')
    # # ,with= P_Width,height = P_Height
    # ggsave(Save_Name,q)
    # M_P<-readBin(Save_Name,"raw")
    # unlink(Save_Name)
    # return(M_P)
    library("Cairo")
    Save_Path<-"/apps/R_PROJECT/SIDS_Sim/data/tempdata/"
    Save_Name<-paste0(Save_Path,UUIDgenerate(TRUE),".png")
    #  par(family='sans')
    #  quartz(family='STKaiti')
    #   old <-theme_light() + theme(text = element_text(family = "SimSun",color="blue",colour="blue",size=20 )) 
    
    old <- theme_gray() + theme(text = element_text(family = "SimSun",color="blue",colour="blue",size=80-5*length(Cn_Names) ,face='bold.italic')) 
    theme_set(old)
    # q<-ggpairs(Plot_Df, lower=list(continuous=wrap("smooth", color="blue",size=0.1)),
    #         diag=list(continuous=wrap("barDiag",color="blue")), 
    #         upper=list(continuous=wrap("cor",color="blue",size=10)),columnLabels=Cn_Names, axisLabels='show')
    q<-ggpairs(Plot_Df, lower=list(continuous=wrap("smooth", color=rgb(43/256,192/256,196/256),size=0.1)),
               diag=list(continuous=wrap("barDiag",color=rgb(43/256,192/256,196/256),fill=rgb(43/256,192/256,196/256))), 
               upper=list(continuous=wrap("cor",color=rgb(43/256,192/256,196/256),size=20-1.2*length(Cn_Names))),columnLabels=Cn_Names, axisLabels='show')
    q<-q+  theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),  panel.background = element_blank(),axis.ticks=element_line(colour='white',color='white'),axis.text=element_text(colour='white'),axis.text.x.bottom=element_text(colour='white',angle=45,vjust=0.5),axis.line = element_line(colour = "white"),plot.background = element_rect(fill = rgb(33/256,80/256,126/256), colour = 'white'),panel.border = element_rect(linetype = "dashed", colour = "black", fill = NA))
    #+ theme(text = element_text(family = "sans",color='black'))
    # q
    
    
    ggsave(Save_Name,q,dpi = 300)
    
    M_P<-readBin(Save_Name,"raw",n=1024*1024)
    # unlink(Save_Name)
         return (list(Remind=1,M_P,Formula,plot_flag,plot_result))
          
  }
}