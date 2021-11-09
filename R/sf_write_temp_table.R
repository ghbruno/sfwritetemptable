sf_write_temp_table<- function(con, df, table_name, overwrite=FALSE) {

  
  data_types_transalation<-data.frame(data_type=c('integer','character', 'numeric', 'Date', 'POSIXct', 'POSIXt'),
                                      sql_data_type=c('int','varchar(255)','float','date','datetime','datetime'))

  colnames(df)<-toupper(colnames(df))
  
  table_name<-toupper(table_name)
  columns<-colnames(df)
  data_type<-unlist(lapply(df,function (x) class(x)[1]))
  
  if(length(columns)!=length(data_types)) {print('ERROR'); break}
  
  data_type_df<-data.frame(columns=names(data_type),data_type) %>% inner_join(data_types_transalation, by = "data_type")
  
  values<-paste("('",paste(apply(df,1,paste,collapse="','"), collapse="'),('"),"')",sep='')
  
  
  if(overwrite==TRUE) {RJDBC::dbSendQuery(con, paste("drop table if exists",table_name,";"))}

  RJDBC::dbSendQuery(con,paste("CREATE TEMP TABLE ", table_name," (",paste(paste(data_type_df$columns,data_type_df$sql_data_type), collapse=','),");"))
  RJDBC::dbSendQuery(con, paste("insert into ", table_name," (",paste(columns,collapse=','),") values ",values,";"))
  cat("TABLE'S DESCRIPTION\n")
  print(RJDBC::dbGetQuery(con,paste("DESC TABLE ",table_name,";",sep='')))
  cat("\n 10 OBS SAMPLE\n")
  print(RJDBC::dbGetQuery(con,paste("SELECT * FROM ",table_name," LIMIT 10;",sep='')))
  
}
