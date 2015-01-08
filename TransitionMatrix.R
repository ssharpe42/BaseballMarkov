
state_transition_matrix <- function(year, team='ALL', event_file_dir, csv_dir){
    
    file_list = list.files(event_file_dir)
    header = read.csv(paste(csv_dir,'EventFields.csv', sep='/'))[,3]
    file = paste('all', year,'.csv', sep='')
    
    states = c('0 000','0 001', '0 010', '0 100', '0 011', '0 101', '0 110', '0 111',
               '1 000','1 001', '1 010', '1 100', '1 011', '1 101', '1 110', '1 111',
               '2 000','2 001', '2 010', '2 100', '2 011', '2 101', '2 110', '2 111', '3 000')
    
    #Generates the states and number of runners for all events
    generate_states <- function(EventData){
        state_vec = paste(EventData$OUTS_CT, ' ', 
                          ifelse(EventData$BASE1_RUN_ID=='',0,1),
                          ifelse(EventData$BASE2_RUN_ID=='',0,1),
                          ifelse(EventData$BASE3_RUN_ID=='',0,1), sep='')
        return(state_vec)
    }
    #subsets the event data for a certain team
    team_subset<-function(EventData, team){
        subset= EventData[(EventData$HOME==team & EventData$BAT_HOME_ID==1)|
                              (EventData$AWAY_TEAM_ID==team &EventData$BAT_HOME_ID==0),]
        
    }
    
    if(file %in% filelist){
        events = read.csv(file, header=FALSE)
        names(events) = header
        events$HOME = substr(events$GAME_ID,1,3)
        events$STATES = generate_states(events)
        events = subset(events, FOUL_FL==FALSE)
        if(team!='ALL' & !team %in% events$HOME){
            print('The team you have selected does not exist or has a different abbreviation.')
        }else if(team!='ALL'){
            events = team_subset(events, team)
        }
        transition_matrix = as.data.frame(matrix(0, nrow=25, ncol=25))
        names(transition_matrix) = states
        row.names(transition_matrix) = states
        
        for(i in 1:(dim(events)[1]-1)){
            if(events[i,'GAME_END_FL']==TRUE){
                #Game ends so there is no transition
            }
            else if((events[i+1,'BAT_HOME_ID'] != events[i,'BAT_HOME_ID'])|
                        (events[i+1,'INN_CT'] != events[i,'INN_CT'])){
                transition_matrix[events$STATES[i],"3 000"] = transition_matrix[events$STATES[i],"3 000"] + 1
            }else{
                transition_matrix[events$STATES[i],events$STATES[i+1]] = transition_matrix[events$STATES[i],events$STATES[i+1]] +1
            }
        }
        
        transition_matrix = round(transition_matrix/rowSums(transition_matrix),4)
        transition_matrix[is.na(transition_matrix)]=0
        return(transition_matrix)
    
    }else{
        print('File or year does not exist. Please check that all event files are in the format
              of all2014.csv')
    }
}