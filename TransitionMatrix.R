#Make sure the EventFields.csv file is in the same directory as the event files
state_transition_matrix <- function(year,event_file_dir){
    
    file_list = list.files(event_file_dir)
    header = read.csv(paste(event_file_dir,'EventFields.csv', sep='/'))[,3]
    file = paste('all', year,'.csv', sep='')
    
    states = c('0 000','0 001', '0 010', '0 100', '0 011', '0 101', '0 110', '0 111',
               '1 000','1 001', '1 010', '1 100', '1 011', '1 101', '1 110', '1 111',
               '2 000','2 001', '2 010', '2 100', '2 011', '2 101', '2 110', '2 111', '3 000')
    
    generate_states <- function(EventData){
        state_vec = paste(EventData$OUTS_CT, ' ', 
                          ifelse(EventData$BASE1_RUN_ID=='',0,1),
                          ifelse(EventData$BASE2_RUN_ID=='',0,1),
                          ifelse(EventData$BASE3_RUN_ID=='',0,1), sep='')
    }
    
    if(file %in% filelist){
        events = read.csv(file)
        names(events) = header
        events$states = generate_states(events)
        transition_matrix = as.data.frame(matrix(0, nrow=25, ncol=25))
        names(transition_matrix) = states
        row.names(transition_matrix) = states
        
        for(i in 1:(dim(events)[1]-1)){
            if(events[i+1,'OUTS_CT'] < events[i,'OUTS_CT']){
                transition_matrix[events$states[i],"3 000"] = transition_matrix[events$states[i],"3 000"] + 1
            }else{
                transition_matrix[events$states[i],events$states[i+1]] = transition_matrix[events$states[i],events$states[i+1]] +1
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