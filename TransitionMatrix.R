
state_transition_matrix <- function(year, team='ALL', event_file_dir, csv_dir, players=vector()){
   
    if(team!='ALL' & length(players)!=0){
        stop('Pick a team or vector of players, not both.')
    }
    file_list = list.files(event_file_dir)
    file = paste('all', year,'.csv', sep='')
    
    if(!file %in% file_list){
        stop('File or year does not exist. Please check that all event files are in the format
              of all2014.csv')
    }
    
    states = c('0 000','0 100', '0 010', '0 001', '0 110', '0 101', '0 011', '0 111',
               '1 000','1 100', '1 010', '1 001', '1 110', '1 101', '1 011', '1 111',
               '2 000','2 100', '2 010', '2 001', '2 110', '2 101', '2 011', '2 111', '3 000')
    
    #Generates the states and number of runners for all events
    generate_states <- function(EventData){
        start = with(EventData, 
                   paste(OUTS_CT,' ', 
                         ifelse(BASE1_RUN_ID=='',0,1),
                         ifelse(BASE2_RUN_ID=='',0,1),
                         ifelse(BASE3_RUN_ID=='',0,1), sep=''))
        end = with(EventData, 
                    paste(OUTS_CT+EVENT_OUTS_CT,' ', 
                    as.numeric(RUN1_DEST_ID==1 | BAT_DEST_ID==1),
                    as.numeric(RUN1_DEST_ID==2 | RUN2_DEST_ID==2 | BAT_DEST_ID==2),
                    as.numeric(RUN1_DEST_ID==3 | RUN2_DEST_ID==3 |RUN3_DEST_ID==3 | BAT_DEST_ID==3), sep=''))
        end = ifelse(substr(end,1,1)=='3', '3 000', end)
        return(cbind(start,end))
    }
    #subsets the event data for a certain team
    team_subset<-function(EventData, team){
        subset= EventData[(EventData$HOME==team & EventData$BAT_HOME_ID==1)|
                              (EventData$AWAY_TEAM_ID==team &EventData$BAT_HOME_ID==0),]    
    }
    player_to_id<-function(players){
        if(!require('Lahman')){
            install.packages('Lahman')
        }
        require('Lahman')
        player_names = with(Master, paste(nameFirst, nameLast))
        correct_names = players %in% player_names
        if(sum(correct_names)!=length(players)){
            stop(paste(paste(players[which(!correct_names)], collapse=' & '), 'are not in the player database. Try a different spelling'))
        }else{
            ids = Master$retroID[which(player_names==players)]
            names(ids) = player_names[which(players==player_names)]
            return(ids[players])
        }
    }
    prepare_events <- function(file, event_file_dir, csv_dir){
        EventData = read.csv(paste(event_file_dir,'/',file,sep=''), header=FALSE)
        header = read.csv(paste(csv_dir,'EventFields.csv', sep='/'))[,3]
        names(EventData) = header
        EventData$HOME = substr(EventData$GAME_ID,1,3)
        gen_states = generate_states(EventData)
        EventData$START = gen_states[,1]
        EventData$END = gen_states[,2] 
        EventData = subset(EventData, FOUL_FL==FALSE)
        return(EventData)
    }
    team_events<-function(EventData, team){
        if(team!='ALL' & !team %in% EventData$HOME){
            stop('The team you have selected does not exist or has a different abbreviation.')
        }
        else if(team!='ALL'){
            EventData = team_subset(EventData, team)
        }
        return(EventData)
    }
    player_events<-function(EventData, player_ids){
        player_event_list = list()
        for(retroid in player_ids){
            player_event_list[[retroid]] <- subset(EventData, BAT_ID==retroid)
        }
        return(player_event_list)
    }
    team_transition <- function(EventData, states){
        transition_matrix = as.data.frame(matrix(0, nrow=25, ncol=25))
        names(transition_matrix) = states
        row.names(transition_matrix) = states
        
        for(i in 1:(dim(EventData)[1]-1)){
            if(EventData[i,'GAME_END_FL']==TRUE){
                #Game ends so there is no transition
            }
            transition_matrix[EventData$START[i],EventData$END[i]] = transition_matrix[EventData$START[i],EventData$END[i]]+1
        }
        transition_matrix = round(transition_matrix/rowSums(transition_matrix),4)
        transition_matrix[is.na(transition_matrix)]=0
        return(transition_matrix)
    }
    player_transition <- function(player_event_list, player_ids, states){
        player_transition_list = list()
        for(retroid in player_ids){
            transition_matrix = as.data.frame(matrix(0, nrow=25, ncol=25))
            names(transition_matrix) = states
            row.names(transition_matrix) = states
            EventData = player_event_list[[retroid]]
            
            for(i in 1:(dim(EventData)[1]-1)){
                if(EventData[i,'GAME_END_FL']==TRUE){
                    #Game ends so there is no transition
                }
                transition_matrix[EventData$START[i],EventData$END[i]] = transition_matrix[EventData$START[i],EventData$END[i]]+1
            }
            transition_matrix = round(transition_matrix/rowSums(transition_matrix),4)
            transition_matrix[is.na(transition_matrix)]=0
            player_transition_list[[retroid]]=transition_matrix
        }
        return(player_transition_list)
    }
    
    events = prepare_events(file, event_file_dir, csv_dir)
    
    if(length(players)==0){
        events = team_events(events, team)
        transition_matrix = team_transition(events, states)
        return(transition_matrix)
    }else{
        player_ids = player_to_id(players)
        events = player_events(events, player_ids)
        transition_matrix_list = player_transition(events, player_ids, states)
        return(transition_matrix_list)
    } 
}