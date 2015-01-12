
source('TransitionMatrix.R')

#Creates a lineup of players from respective years 
#player_names and player_seasons should be vectors of length 9 of names and corresponding years
#normalize year is the year of MLB transition probabilities that should be used to replace any missing data
#or used to normalize nonBAT events
create_lineup<-function(player_names, player_seasons, event_file_dir, csv_dir, normalize_year = 2014){
    states = c('0 000','0 100', '0 010', '0 001', '0 110', '0 101', '0 011', '0 111',
               '1 000','1 100', '1 010', '1 001', '1 110', '1 101', '1 011', '1 111',
               '2 000','2 100', '2 010', '2 001', '2 110', '2 101', '2 011', '2 111', '3 000')
    transition_matrix_list = list()
    player_ids = player_to_id(player_names)
    years = unique(player_seasons)
    order=vector()
    #calculate normalizing matricies
    norm_matricies = state_transition_matrix(normalize_year, 'ALL', vector(), event_file_dir, csv_dir )
    for(year in years){
        #get players specified for current year and calculate their matricies
        players = player_names[which(player_seasons==year)]
        current_year_list = state_transition_matrix(year, 'ALL',players, event_file_dir, event_file_dir)
        order = c(order,names(current_year_list))
        for(i in 1:length(current_year_list)){
            #if a sum of a row is 0, that means that the state is missing. If so replace with MLB avg from norm_year.
            missing_states = states[which(rowSums(current_year_list[[i]][[1]])[-25]==0)]
            if(length(missing_states)>0){
                warning(paste(names(current_year_list)[i], 'does not have PA in states:', 
                        paste(missing_states, collapse=' & '), '. If you do not replace this player,', 
                        'league average will be used for these state transition probabilities'))
                current_year_list[[i]][[1]][missing_states,] = norm_matricies[[1]][missing_states,]
            }
            #put the normalized matricies into list
            transition_matrix_list[[length(transition_matrix_list)+1]] = 
                                            normalize_nonBAT_perc(norm_matricies[[1]], norm_matricies[[2]], 
                                                    current_year_list[[i]][[1]])
        }
    }
    #return in the order the names are listed
    names(transition_matrix_list)=order
    return(transition_matrix_list[player_ids])
}




