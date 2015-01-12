source('TransitionMatrix.R')
source('Lineup.R')
source('GameSimulation.R')


event= "..."
csv = ".../data"

LAA_2014=create_lineup(player_names=c('Kole Calhoun','Mike Trout', 'Albert Pujols','Josh Hamilton',
                                    'Howie Kendrick','Erick Aybar','David Freese', 'Chris Iannetta', 'Collin Cowgill'), 
                       player_seasons=rep(2014,9), 
                       event_file_dir=event, 
                       csv_dir=csv)

BARRY_MVP=create_lineup(player_names=rep('Barry Bonds',9), 
                     player_seasons=rep(2001,9), 
                     event_file_dir=event, 
                     csv_dir=csv)

MLB_AVERAGE_2009=create_lineup(player_names=rep(' ',9), 
                     player_seasons=rep(2009,9), 
                     event_file_dir=event, 
                     csv_dir=csv)


season = simulate_season(LAA_2014,'StateRuns.csv')
season2 = simulate_season(BARRY_MVP,'StateRuns.csv')
season3 = simulate_season(MLB_AVERAGE_2009,'StateRuns.csv')
