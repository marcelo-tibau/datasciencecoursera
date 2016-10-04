# Explorations on baseball Lahman dataset
# It uses the data on pitching, hitting and fielding performance and other tables from 1871 through 2015.
# MLB (American and National Leagues); American Association; Union Association;Players League; Federal League; National Association (1871-1875)

library(Lahman)
head(AllstarFull)
View(AllstarFull)

# find number of appearances by players in the All Star games
data("AllstarFull")
babe_ruth_appearances <- with(AllstarFull, rev(sort(table(playerID == "ruthba0"))))

#find number of appearances by players in the All Star games
player_appearances <- with(AllstarFull, rev(sort(table(playerID))))

# Babe Ruth, Lou Gehrig and Joe DiMaggio appearances 
babe_ruth_appearances <- subset(AllstarFull, playerID == "ruthba01")
lou_gehrig_appearances <- subset(AllstarFull, playerID == "gehrilo01")
dimagg_appearances <- subset(AllstarFull, playerID == "dimagjo01")
        
# density plot of the whole distribution
plot(density(player_appearances), main = "Apperances in All Star Games")
rug(jitter(player_appearances))

# players who has played in more than 10 ASGs
more_than_ten <- player_appearances[player_appearances > 10]
hist(more_than_ten)

# Years that Dimagg played in the ASG:
dimag_years <- with(AllstarFull, yearID[playerID == "dimagjo01"])


