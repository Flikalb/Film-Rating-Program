-- PROCAP - PROGRAMMING LANGUAGE CONCEPTS AND PARADIGMS
-- Haskell program for a film ratings website
-- Lucie Boutou
-- 602543

--
-- Types
--
import Data.List

type Title = String
type Actor = String
type Year = Int
type Fan = String

data Film = Film Title [Actor] Year [Fan]
    deriving(Show,Read,Eq)

instance Ord Film where
    compare f1 f2  
        | numberOfFans f1 > numberOfFans f2   = GT
        | numberOfFans f1 < numberOfFans f2   = LT
        | otherwise                           = EQ
            where numberOfFans (Film _ _ _ fans) = length fans

                
                
                
testDatabase :: [Film]
testDatabase = [
 (Film "Casino Royale" ["Daniel Craig"] 2006 ["Garry", "Dave", "Zoe", "Kevin", "Emma"]), 
 (Film "Blade Runner" ["Harrison Ford", "Rutger Hauer"] 1982 ["Dave", "Zoe", "Amy", "Bill", "Ian", "Kevin", "Emma", "Sam", "Megan"]),
 (Film "Psycho" ["Anthony Perkins", "Janet Leigh"] 1960 ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"]),
 (Film "Body Of Lies" ["Leonardo DiCaprio", "Russell Crowe"] 2008 ["Sam", "Neal", "Kevin", "Chris", "Olga"]),
 (Film "Mamma Mia!" ["Meryl Streep", "Pierce Brosnan"] 2008 ["Kevin", "Jo", "Liz", "Amy", "Sam", "Wally", "Zoe"]),
 (Film "Saving Private Ryan" ["Tom Hanks", "Matt Damon"] 1998 ["Heidi", "Jo", "Megan", "Olga", "Zoe"]),
 (Film "Avatar" ["Sam Worthington", "Zoe Saldana", "Sigourney Weaver"] 2009 ["Olga", "Paula", "Wally", "Megan", "Tim", "Zoe", "Emma"]),
 (Film "Titanic" ["Leonardo DiCaprio", "Kate Winslet"] 1997 ["Zoe", "Amy", "Emma", "Heidi", "Jo", "Megan", "Olga", "Tim"]),
 (Film "Quantum of Solace" ["Daniel Craig"] 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
 (Film "You've Got Mail" ["Meg Ryan", "Tom Hanks"] 1998 ["Dave", "Amy"]),
 (Film "Aliens" ["Sigourney Weaver"] 1986 ["Fred", "Dave", "Amy", "Bill", "Wally", "Paula", "Zoe"]),
 (Film "Kingdom Of Heaven" ["Orlando Bloom", "Liam Neeson"] 2005 ["Chris", "Emma", "Bill", "Dave"]),
 (Film "Inception" ["Leonardo DiCaprio"] 2010 ["Chris", "Emma", "Bill", "Dave", "Liz", "Kevin", "Olga"]),
 (Film "Vertigo" ["James Stewart", "Kim Novak"] 1958 ["Bill", "Emma", "Garry", "Kevin", "Olga", "Tim"]),
 (Film "Up in the Air" ["George Clooney"] 2009 ["Wally", "Liz", "Kevin", "Tim", "Emma"]),
 (Film "The Birds" ["Rod Taylor", "Tippi Hedren"] 1963 ["Kevin", "Olga", "Tim", "Wally", "Kevin"]),
 (Film "The Godfather" ["Marlon Brando", "Al Pacino"] 1972 ["Bill", "Fred", "Garry", "Sam", "Tim", "Zoe"]),
 (Film "The Shawshank Redemption" ["Tim Robbins", "Morgan Freeman"] 1994 ["Jo", "Sam", "Zoe", "Dave", "Emma", "Garry", "Kevin"]),
 (Film "Gladiator" ["Russell Crowe"] 2000 ["Garry", "Ian", "Neal"]),
 (Film "The King's Speech" ["Colin Firth", "Geoffrey Rush"] 2010 ["Garry", "Megan", "Sam", "Wally", "Ian"]),
 (Film "True Lies" ["Arnold Schwarzenegger"] 1994 ["Dave", "Kevin", "Jo", "Zoe"]),
 (Film "Minority Report" ["Tom Cruise"] 2002 ["Dave", "Garry", "Megan", "Sam", "Wally"]),
 (Film "Forrest Gump" ["Tom Hanks"] 1994 ["Ian", "Garry", "Liz", "Sam", "Dave", "Jo"]),
 (Film "127 Hours" ["James Franco"] 2010 ["Liz", "Wally", "Megan"]),
 (Film "The Terminal" ["Tom Hanks", "Catherine Zeta Jones"] 2004 ["Olga", "Heidi", "Bill", "Sam", "Zoe"])
 ]





-- 
--
--  functional code
--
--

addFilm :: Film -> [Film] -> [Film]
addFilm f list = f : list

displayAllFilms :: [Film] -> [Film]
displayAllFilms films = films
    
displayYearFilms year = filter(\(Film _ _ yr _) -> yr==year)

displayFanFilms fan = filter(\(Film _ _ _ fans) -> elem fan fans)

displayActorFilms actor = filter(\(Film _ actors _ _) -> elem actor actors)
 
countTotalFans :: [Film] -> Int
countTotalFans [] = 0
countTotalFans ((Film _ _ _ fans):xs) = length fans + countTotalFans xs

countAveFans :: Actor -> [Film] -> Int
countAveFans actor list = div (countTotalFans (displayActorFilms actor list)) (length (displayActorFilms actor list))

isAlreadyFan :: Fan -> [Fan] -> Bool
isAlreadyFan _ [] = False
isAlreadyFan fan (x:xs)
        | fan == x      = True
        |otherwise      = isAlreadyFan fan xs

becomeFan :: Fan -> Title -> [Film] -> [Film]
becomeFan _ _ [] = []
becomeFan fan tit ((Film title actors yr fans):xs)
        | tit == title  && not(isAlreadyFan fan fans)      = ((Film title actors yr (fan:fans)):xs)
        |otherwise                                         = (Film title actors yr fans) : becomeFan fan tit xs

displayBestFilm :: [Film] -> Film
displayBestFilm = foldr(\film1 film2 -> if film1 > film2 then film1 else film2) (Film "not found" [] 0 [])


displayYearBestFilm :: [Film] -> Int -> Film
displayYearBestFilm listFilm year = displayBestFilm (displayYearFilms year listFilm)

getNFirstFilms :: Int -> [Film] -> [Film]
getNFirstFilms _ []      = []
getNFirstFilms 0 _      = []
getNFirstFilms n (x:xs) = x : getNFirstFilms (n-1) xs                        

displayTopTen :: [Film] -> [Film]
displayTopTen list = getNFirstFilms 10 (reverse (sort list))




-- function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
demo 1  = do putStrLn "All films after adding True Grit with Matt Damon and Jeff Bridges, year : 2010.";
             displayFilms (addFilm (Film "True Grit" ["Matt Damon", "Jeff Bridges"] 2010 []) testDatabase);
demo 2  = do putStrLn "All films in nice format.";
             displayFilms testDatabase;
demo 3  = do putStrLn "All films of year 2010.";
             displayFilms (displayYearFilms 2010 testDatabase);
demo 4  = do putStrLn "All films that Zoe is a fan of.";
             displayFilms (displayFanFilms "Zoe" testDatabase);
demo 5  = do putStrLn "Average fans for actor Tom Hanks.";
             putStrLn ("The average number of fans for the films of " ++ "Tom Hanks" ++ " is : " ++ show (countAveFans "Tom Hanks" testDatabase));
demo 6  = do putStrLn "All films after Zoe becomes fan of Forrest Gump.";
             displayFilms (becomeFan "Zoe" "Forrest Gump" testDatabase);
demo 61 = do putStrLn "All films after Zoe becomes fan of Avatar.";
             displayFilms (becomeFan "Zoe" "Avatar" testDatabase)
demo 7  = do putStrLn "Best film from year 2010.";
             displayFilm (displayYearBestFilm testDatabase 2010);
demo 8  = do putStrLn "Top 10 films.";
             displayFilms (displayTopTen testDatabase);




--
--
-- user interface code
--
--
                                                           
main :: IO()
main = do contents <- readFile "filmsdata.txt";
          films <- return (read contents :: [Film]);
          putStrLn "Enter your name, please : ";
          name <- getLine;
          putStrLn ("Hello " ++ name ++ " !");
          displayChoices name films;


displayChoices :: String -> [Film] -> IO()
displayChoices name list = do putStrLn "What do you want to do ?";
                              putStrLn "1 -> Add a film";
                              putStrLn "2 -> Display all films";
                              putStrLn "3 -> Display all films that were released in a given year";
                              putStrLn "4 -> Display the films that you are fan of";
                              putStrLn "5 -> Display the average number of fans for the films of a particular actor";
                              putStrLn "6 -> Say you are fan of a film";
                              putStrLn "7 -> Display the best film for a given year";
                              putStrLn "8 -> Display the top ten films";
                              putStrLn "9 -> Exit";
                              putStrLn "Enter your choice : ";
                              choice <- getInt;
                              putStrLn "";
                              treatChoice choice name list;

                    
treatChoice :: Int -> String -> [Film] -> IO ()
treatChoice 1 name list             = do film <- getFilm;
                                         displayChoices name (addFilm film list);
treatChoice 2 name list             = do displayFilms list;
                                         displayChoices name list;                                
treatChoice 3 name list             = do putStrLn "Enter the wanted year : ";
                                         yr <- getInt;
                                         putStrLn "";
                                         displayFilms (displayYearFilms yr list);
                                         displayChoices name list;
treatChoice 4 name list             = do putStrLn "You are fan of : ";
                                         putStrLn "";
                                         displayFilms (displayFanFilms name list);
                                         displayChoices name list;
treatChoice 5 name list             = do putStrLn "What is the name of the actor you are looking for ?";
                                         actor <- getLine;
                                         putStrLn ("The average number of fans for the films of " ++ actor ++ " is : " ++ show (countAveFans actor list));
                                         displayChoices name list;
treatChoice 6 name list             = do putStrLn "Enter the title of the wanted film : ";
                                         title <- getLine;
                                         putStrLn "";
                                         displayChoices name (becomeFan name title list);
treatChoice 7 name list             = do putStrLn "Enter the wanted year : ";
                                         yr <- getInt;
                                         putStrLn "";
                                         putStrLn "The best film for this year is : ";
                                         putStrLn "";
                                         displayFilm (displayYearBestFilm list yr);
                                         displayChoices name list;
treatChoice 8 name list             = do putStrLn "The Top Ten Films are : ";
                                         putStrLn "";
                                         displayFilms (displayTopTen list);
                                         displayChoices name list;
treatChoice 9 _ list                = do putStrLn "Exit"; 
                                         writeFile "filmsdata.txt" (show list);



getInt :: IO Int
getInt = do str <- getLine
            return (read str :: Int)

            
            
concatListToString :: [String] -> String
concatListToString [] = []
concatListToString (x:xs) = x ++ " " ++ concatListToString xs



displayFilm :: Film -> IO()
displayFilm (Film title actors yr fans) = do putStrLn title;
                                             putStrLn ("Actors : " ++ concatListToString actors);
                                             putStrLn ("Year : " ++ show yr);
                                             putStrLn ("Fans : " ++ concatListToString fans ++ "\n");
        

        
displayFilms :: [Film] -> IO()
displayFilms []     = putStrLn "";
displayFilms (x:xs) = do displayFilm x;
                         displayFilms xs;

                         
                         
getActors :: [Actor] -> IO [Actor]                     
getActors actors =  do putStrLn "Enter an actor :";
                       actor <- getLine;
                       if actor == ""
                       then return actors
                       else getActors (actor:actors)



getFilm :: IO Film
getFilm = do putStrLn "Enter the title of the film : ";
             title <- getLine;
             putStrLn "Enter the year of the film : ";
             yr <- getInt;
             putStrLn "Enter each actor (one by one) : ";
             actors <- getActors [];
             return (Film title actors yr []);
