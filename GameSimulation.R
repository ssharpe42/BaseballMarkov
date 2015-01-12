#Function that simulates game and returns runs scored 
simulate_game <- function(lineup_matricies, StateRunsFile){
    states = c('0 000','0 100', '0 010', '0 001', '0 110', '0 101', '0 011', '0 111',
               '1 000','1 100', '1 010', '1 001', '1 110', '1 101', '1 011', '1 111',
               '2 000','2 100', '2 010', '2 001', '2 110', '2 101', '2 011', '2 111', '3 000')
    StateRuns = read.csv(StateRunsFile, header=FALSE)
    names(StateRuns)=states[-25]
    row.names(StateRuns)=states[-25]
    
    #innitialize inning, runs, batter number
    inning = 1
    runs = 0
    end_game = FALSE
    batter = 1
    while(!end_game){
        state = '0 000'
        current_half =TRUE
        while(current_half){
            bat_event = lineup_matricies[[batter]][[1]]
            non_bat_event = lineup_matricies[[batter]][[2]]
            Unif = runif(1)
            #if the random number is greater than the sum of bat event prob, then it is a non bat event
            if(Unif>sum(bat_event[state,])){
                #transition probabilities given current state
                transitions = as.matrix(non_bat_event[state,])
                Unif = Unif - sum(bat_event[state,])
                for(t in 1:25){
                    if(cumsum(transitions)[t] >= Unif){
                        new_state= states[t]
                        break
                    }
                }
                BAT = FALSE
                #otherwise it is a bat event
            }else{
                #transition probabilities given current state
                transitions = as.matrix(bat_event[state,])
                for(t in 1:25){
                    if(cumsum(transitions)[t] >= Unif){
                        new_state= states[t]
                        break
                    }
                }
                BAT = TRUE
            }
            #if 3 outs we end the inning
            if(new_state == '3 000'){
                current_half=FALSE
            }else{
                #if bat event, then add the runs scored starting in current state and ending in new_state
                #if non bat event, then add runs scored starting in current state and ending in new_state minus 1 (batter can't score)
                if(BAT) runs = runs + StateRuns[state, new_state] else runs = runs + max(StateRuns[state,new_state]-1,0)
                #new_state is now the current state
                state = new_state
                #if bat event, we go to the next batter
                if(BAT){
                    if(batter==9) batter=1 else batter=batter+1
                }
            }
        }
        if(inning==9){
            end_game=TRUE 
            #If inning is 8, we advance to 9th inning 65% of the time (Average of 8.65 innings per game in the Wild Card era)
        }else if(inning==8){
            U = runif(1)
            if(U<.65) inning = inning + 1 else end_game=TRUE
        }else{inning = inning+1}
    }
    return(runs)
}

#Simulates season of 162 games and returns vector of games
simulate_season <- function(lineup_matricies, StateRunsFile, games = 162){
    runs = vector()
    for(i in 1:games){
        R = simulate_game(lineup_matricies, StateRunsFile)
        runs = c(runs,R)
    }
    return(runs)
}