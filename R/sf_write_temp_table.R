sf_write_temp_table<- function(con, df, table_name, overwrite=FALSE) {
  
  
  data_types_transalation<-data.frame(data_type=c('integer','character', 'numeric', 'Date', 'POSIXct', 'POSIXt'),
                                      sql_data_type=c('int','varchar(255)','float','date','datetime','datetime'))
  
  colnames(df)<-toupper(colnames(df))
  
  table_name<-toupper(table_name)
  columns<-colnames(df)
  data_type<-unlist(lapply(df,function (x) class(x)[1]))
  
  if(length(columns)!=length(data_type)) {print('ERROR'); break}
  
  data_type_df<-merge(data.frame(columns=names(data_type),data_type), data_types_transalation, on = "data_type")
  
  if(overwrite==TRUE) {RJDBC::dbSendQuery(con, paste("drop table if exists",table_name,";"))}
  
  RJDBC::dbSendQuery(con,paste("CREATE TEMP TABLE ", table_name," (",paste(paste(data_type_df$columns,data_type_df$sql_data_type), collapse=','),");"))
  
  n_iter<-ceiling(nrow(df)/15000)
  for(i in 1:n_iter) {
  
    indexes<-(1:15000)+(15000*(i-1))
    indexes<-indexes[indexes<=nrow(df)]
  
    values<-paste("('",paste(apply(df[indexes,,drop=FALSE],1,paste,collapse="','"), collapse="'),('"),"')",sep='')
    RJDBC::dbSendQuery(con, paste("insert into ", table_name," (",paste(columns,collapse=','),") values ",values,";"))
  
  }
  
  cat("TABLE'S DESCRIPTION\n")
  print(RJDBC::dbGetQuery(con,paste("DESC TABLE ",table_name,";",sep='')))
  cat("\n\n\n 10 OBS SAMPLE\n")
  print(RJDBC::dbGetQuery(con,paste("SELECT * FROM ",table_name," LIMIT 10;",sep='')))
  
}
