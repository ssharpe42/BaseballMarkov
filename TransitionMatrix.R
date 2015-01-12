
#generates start and end states for an event file data frame
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

#creates an empty transition matrix
empty_transition_matrix <- function(states){
    matrix = as.data.frame(matrix(0, nrow=25, ncol=25))
    names(matrix) = states
    row.names(matrix) = states
    return(matrix)
}

#Adds necessary variables (states, home team, headers) to the event file data frame
prepare_events <- function(file, eventdir, csv_dir){
    EventData = read.csv(paste(eventdir,'/',file,sep=''), header=FALSE)
    header = read.csv(paste(csv_dir,'EventFields.csv', sep='/'))[,3]
    names(EventData) = header
    EventData$HOME = substr(EventData$GAME_ID,1,3)
    gen_states = generate_states(EventData)
    EventData$START = gen_states[,1]
    EventData$END = gen_states[,2] 
    #We exclude errors that occur on foul balls since they do not lead to any runs or advances
    EventData = subset(EventData, EVENT_CD!=13)
    return(EventData)
}
#Returns events for specified team
team_events<-function(EventData, team){
    if(team!='ALL' & !team %in% EventData$HOME){
        stop('The team you have selected does not exist or has a different abbreviation.')
    }
    else if(team!='ALL'){
        #subset for specified team
        EventData = EventData[(EventData$HOME==team & EventData$BAT_HOME_ID==1)|
                                  (EventData$AWAY_TEAM_ID==team &EventData$BAT_HOME_ID==0),]
    }
    return(EventData)
}
#Returns player retrosheet ids corresponding to vector of player names
player_to_id<-function(players){
    if(!require('Lahman')){
        install.packages('Lahman')
    }
    require('Lahman')
    names = with(Master, paste(nameFirst, nameLast))
    correct_names = players %in% names
    if(sum(players==' ')==length(players)){
        return(players)
    }
    else if(sum(correct_names)!=length(players)){
        stop(paste(paste(players[which(!correct_names)], collapse=' & '), 'are not in the player database. Try a different spelling'))
    }else{
        ids = Master$retroID[which(names %in% players)]
        names(ids) = names[which(names %in% players)]
        return(ids[players])
    }
}
#returns a list of players and their events given the event file df and player retrosheet ids
player_events<-function(EventData, player_ids){
    player_event_list = list()
    for(retroid in player_ids){
        player_event_list[[length(player_event_list)+1]] <- subset(EventData, BAT_ID==retroid)
    }
    names(player_event_list)=player_ids
    return(player_event_list)
}
#Returns the transition matricies for the specified team
team_transition <- function(EventData, states){
    transitions = empty_transition_matrix(states)
    transitions_nonBAT = empty_transition_matrix(states)
    
    if(dim(EventData)[1]>0){
        for(i in 1:(dim(EventData)[1]-1)){
            if(EventData[i,'GAME_END_FL']==TRUE){
                #Game ends so there is no transition
            }
            #If there is a non-batter event (aka SB, wild pitch, pick off) we to the nonBAT transition matrix
            else if(EventData$BAT_EVENT_FL[i]==FALSE){
                transitions_nonBAT[EventData$START[i],EventData$END[i]] = transitions_nonBAT[EventData$START[i],EventData$END[i]] +1 
            #If the batter advances, then we add a transition to the normal matrix
            }else{
                transitions[EventData$START[i],EventData$END[i]] = transitions[EventData$START[i],EventData$END[i]] +1
            }
        }
    }
    temp_transitions = transitions+transitions_nonBAT
    #Calculate batter event probabilities
    transitions = transitions/rowSums(temp_transitions)
    #Calculate batter event probabilities
    transitions_nonBAT = transitions_nonBAT/rowSums(temp_transitions)
    transitions_nonBAT[is.na(transitions_nonBAT)]=0
    transitions[is.na(transitions)]=0
    return(list(transitions,transitions_nonBAT))
}

#Returns a list of transition matricies for the players specified (2 matricies for each player BAT and nonBAT)
player_transition <- function(player_event_list, player_ids, states){
    player_transition_list = list()
    for(retroid in player_ids){
        transitions = empty_transition_matrix(states)
        transitions_nonBAT = empty_transition_matrix(states)
        EventData = player_event_list[[retroid]]
        if(dim(EventData)[1]>0){
            for(i in 1:(dim(EventData)[1]-1)){
                if(EventData[i,'GAME_END_FL']==TRUE){
                    #Game ends so there is no transition
                }#If there is a non-batter event (aka SB, wild pitch, pick off) we to the nonBAT transition matrix
                else if(EventData$BAT_EVENT_FL[i]==FALSE){
                    transitions_nonBAT[EventData$START[i],EventData$END[i]] = transitions_nonBAT[EventData$START[i],EventData$END[i]] +1 
                }#If the batter advances, then we add a transition to the normal matrix
                else{
                    transitions[EventData$START[i],EventData$END[i]] = transitions[EventData$START[i],EventData$END[i]] +1
                }
            }
        }
        temp_transitions = transitions+transitions_nonBAT
        transitions = transitions/rowSums(temp_transitions)
        transitions[is.na(transitions)]=0
        transitions_nonBAT = transitions_nonBAT/rowSums(temp_transitions)
        transitions_nonBAT[is.na(transitions_nonBAT)]=0
        player_transition_list[[length(player_transition_list)+1]]=list(transitions, transitions_nonBAT)
    }
    names(player_transition_list)=player_ids
    return(player_transition_list)
}


#Returns transition matricies for the MLB, a specific team, or a vector of players
state_transition_matrix <- function(year, team, players, event_file_dir, csv_dir){
   
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
    
    #clean event file
    events = prepare_events(file, event_file_dir, csv_dir)
    
    #if no players listed calculate team transition matrix (could be all teams)
    if(length(players)==0){
        events = team_events(events, team)
        transition_matrix = team_transition(events, states)
        return(transition_matrix)
    }else{
    #else calculate transition matricies for players
        player_ids = player_to_id(players)
        events = player_events(events, player_ids)
        transition_matrix_list = player_transition(events, player_ids, states)
        return(transition_matrix_list)
    } 
}

#Normalizes a player's or team's transition probabilities to a specific year MLB's probabilities
#1) It makes the probability of a BAT event the same as the overall MLB probability.
#2) Replaces a player's nonBAT matrix with the MLB nonBAT matrix since the nonBAT transition matrix
#   have little to do with the fact that the specific player is batting.

normalize_nonBAT_perc <- function(BAT_season, nonBAT_season, AB_trans_matrix){
    AB_scalars = ifelse(rowSums(AB_trans_matrix)==0,0,rowSums(BAT_season)/rowSums(AB_trans_matrix))
    AB_matrix_norm = AB_trans_matrix*AB_scalars
    return(list(AB_matrix_norm,nonBAT_season))
}