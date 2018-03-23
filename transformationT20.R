file_list =  list.files("C:/Users/RAA090/Documents/Personal/ipl_data/t20_csv_male/")
file_list = file_list[-length(file_list)]
colnames_match_info <- c("match_id","season","city","date","team1_id","team2_id","toss_winner","toss_decision","result","dl_applied","winner","win_by_runs",
                         "win_by_wickets","player_of_match","player_of_match_id","venue","venue_id","umpire1","umpire2")

match_info_final <- data.frame(row.names = colnames_match_info)
match_dat_skip_final <- data.frame()
i = 1
for(my_file in file_list){
  if(i == 265){
    next()
  }
  match_id = substr(my_file,1,nchar(my_file)-4)

  
  match_dat = read.csv(paste("C:/Users/RAA090/Documents/Personal/ipl_data/t20_csv_male/" ,my_file,sep = ""),header = FALSE,skip = 1)
  num_lines_to_skip = max(which(as.character(match_dat$V1) == "info")) +1
  match_dat_skip = read.csv(paste("C:/Users/RAA090/Documents/Personal/ipl_data/t20_csv_male/" ,my_file,sep = ""),header = FALSE,skip = num_lines_to_skip)
  match_dat_skip$V1 <- NULL
  
  
  match_dat_skip$match_id <- match_id
  match_dat_skip$ball <- (match_dat_skip$V3 %% 1) * 10
  match_dat_skip$over <-  floor(match_dat_skip$V3 + 1)
  playing_teams <- unique(as.character(match_dat_skip$V4))
  match_dat_skip$bowling_team <- unlist(lapply(match_dat_skip$V4, function(x) playing_teams[(playing_teams %in% x) == FALSE]))
  colnames(match_dat_skip)[c(1,3:10)] <- c("inning","batting_team","batsman"	,"non_striker","bowler","batsman_runs","extra_runs","dismissal_kind","player_dismissed")
  match_dat_skip$V3 <- NULL
  
  match_info = match_dat[1:num_lines_to_skip-1,]
  match_info$V1 <- NULL
  match_info$V2 <- as.character(match_info$V2)
  match_info$V3 <- as.character(match_info$V3)
  match_info = match_info[!match_info$V2 == "gender",]
  match_info$V2[match_info$V2 == "team"] <- c("team1_id",	"team2_id")
  match_info$V2[match_info$V2 == "umpire"] <- c("umpire1",	"umpire2")
  match_info <- rbind(match_info, c("match_id",match_id))
  dup = which(duplicated(match_info$V2))
  print(dup)
  
  if(length(dup)>0){
    match_info <- match_info[-dup,]
  }
  row.names(match_info) <- match_info$V2
  match_info$V2 <- NULL
  colnames(match_info) <- match_id
  match_info_final = merge(x = match_info_final,y = match_info,by ='row.names' ,all = TRUE)
  rownames(match_info_final) <- match_info_final$Row.names
  match_info_final$Row.names <- NULL
  
  write.csv(match_dat_skip,paste("C:/Users/RAA090/Documents/Personal/ipl_data/t20_csv_male_transformed/",my_file,sep = ""))
  if(i==1){
    match_dat_skip_final <- match_dat_skip
  }else{
    match_dat_skip_final <- rbind(match_dat_skip_final,match_dat_skip)
  }
  
  i = i +1
}

write.csv(match_dat_skip_final,"C:/Users/RAA090/Documents/Personal/ipl_data/t20_csv_male_collated/ball_by_ball_t20.csv")
write.csv(match_info_final,"C:/Users/RAA090/Documents/Personal/ipl_data/t20_csv_male_collated/match_data_t20.csv")
