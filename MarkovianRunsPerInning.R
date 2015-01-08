#EventFields.csv and StateRuns.csv files must be in the csv_dir and the event files must be in the event_file_dir

Exp_Runs_Inning <- function(year, team = 'ALL', event_file_dir, csv_dir){
    require(MASS)
    file_list = list.files(event_file_dir)
    header = read.csv(paste(csv_dir,'EventFields.csv', sep='/'))[,3]
    file = paste('all', year,'.csv', sep='')
    
    states = c('0 000','0 001', '0 010', '0 100', '0 011', '0 101', '0 110', '0 111',
               '1 000','1 001', '1 010', '1 100', '1 011', '1 101', '1 110', '1 111',
               '2 000','2 001', '2 010', '2 100', '2 011', '2 101', '2 110', '2 111', '3 000')
    
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
        
        runner_change = (as.numeric(substr(start,3,3))+as.numeric(substr(start,4,4))+as.numeric(substr(start,5,5))) - (as.numeric(substr(end,3,3))+as.numeric(substr(end,4,4))+as.numeric(substr(end,5,5))) 
        end = ifelse(substr(end,1,1)=='3', '3 000', end)
        return(cbind(start,end,runner_change))
    }
    #Creates a 25 by 25 matrix of 0's
    empty_transition_matrix <- function(states){
        matrix = as.data.frame(matrix(0, nrow=25, ncol=25))
        names(matrix) = states
        row.names(matrix) = states
        return(matrix)
    }
    #subsets the event data for a certain team
    team_subset<-function(EventData, team){
        subset= EventData[(EventData$HOME==team & EventData$BAT_HOME_ID==1)|
                              (EventData$AWAY_TEAM_ID==team &EventData$BAT_HOME_ID==0),]
        
    }
    if(file %in% file_list){
        events = read.csv(file, header=FALSE)
        names(events) = header
        generated_states = generate_states(events)
        events$START = generated_states[,1]
        events$END = generated_states[,2]
        events$RUNNER_CHANGE = as.numeric(generated_states[,3])
        events$HOME = substr(events$GAME_ID,1,3)
        events = subset(events, FOUL_FL==FALSE)
        
        if(team!='ALL' & !team %in% events$HOME){
            print('The team you have selected does not exist or has a different abbreviation.')
        }else if(team!='ALL'){
            events = team_subset(events, team)
        }
        
        transitions = empty_transition_matrix(states)
        transitions_nonAB = empty_transition_matrix(states)

        for(i in 1:(dim(events)[1]-1)){
            if(events[i,'GAME_END_FL']==TRUE){
                #Game ends so there is no transition
            }
            #If the batter doesn't advance and there is no scoring, then we don't want this transition to 
            #have the normal number of runs expected to score
            #EXAMPLE: Stolen base --> 0 100 to 0 010 would usually be construed as a double, but I don't count it this way
            else if(events$BAT_EVENT_FL[i]==FALSE & events$RUNNER_CHANGE[i] == 0){
                transitions_nonAB[events$START[i],events$END[i]] = transitions_nonAB[events$START[i],events$END[i]] +1 
            #If the batter advances, then we add a transition to the normal matrix
            }else{
                transitions[events$START[i],events$END[i]] = transitions[events$START[i],events$END[i]] +1
            }
        }
        temp_transitions = transitions+transitions_nonAB
        #Batter event transition matrix
        transitions = transitions/rowSums(temp_transitions)
        #Total transition matrix 
        temp_transitions = temp_transitions/rowSums(temp_transitions)
        temp_transitions[is.na(temp_transitions)]=0
        transitions[is.na(transitions)]=0
        
        #N = inverse(I - Q) is the average time spent in each state where Q is the transient transition matrix
        N = ginv(as.matrix(diag(24)-temp_transitions[1:24,1:24]))
        
        
        #Avg transitions in each state starting at 0 000 (no outs, nobody on base)
        StateTime = N[1,]
        #Load the matrix of runs scored with each transition
        StateRuns = read.csv(paste(csv_dir,'StateRuns.csv', sep='/'), header=FALSE)
        #Multiply the batter event transition matrix by the runs transition matrix to get expected runs
        Exp_Transition_Runs = rowSums(StateRuns*transitions[-25,-25])
        #Multiply the expected transitions in each state by the expected runs for each state
        Runs_Inning = t(as.matrix(StateTime))%*%as.matrix(Exp_Transition_Runs)
        return(as.numeric(Runs_Inning))
    }else{
        print('File or year does not exist. Please check that all event files are in the format
              of all2014.csv')
    }
}