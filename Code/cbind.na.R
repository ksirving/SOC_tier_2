cbind.na<-function(df1, df2){
  
  #Collect all unique rownames
  total.rownames<-union(x = rownames(x = df1),y = rownames(x=df2))
  
  #Create a new dataframe with rownames
  df<-data.frame(row.names = total.rownames)
  
  #Get absent rownames for both of the dataframe
  absent.names.1<-setdiff(x = rownames(df1),y = rownames(df))
  absent.names.2<-setdiff(x = rownames(df2),y = rownames(df))
  
  #Fill absents with NAs
  df1.fixed<-data.frame(row.names = absent.names.1,matrix(data = NA,nrow = length(absent.names.1),ncol=ncol(df1)))
  colnames(df1.fixed)<-colnames(df1)
  df1<-rbind(df1,df1.fixed)
  
  df2.fixed<-data.frame(row.names = absent.names.2,matrix(data = NA,nrow = length(absent.names.2),ncol=ncol(df2)))
  colnames(df2.fixed)<-colnames(df2)
  df2<-rbind(df2,df2.fixed)
  
  #Finally cbind into new dataframe
  df<-cbind(df,df1[rownames(df),],df2[rownames(df),])
  return(df)
  
}