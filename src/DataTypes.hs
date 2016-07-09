{-# LANGUAGE NamedFieldPuns #-}
module DataTypes where
    data Client = GovOrg     String
                | Company    String Integer Person String
                | Individual Person Bool
                deriving Show
                
    data Person = Person String String Gender
                deriving Show
    
    data Gender = Male | Female | Unknown
                deriving Show

    data TimeMachine = TimeMachine { model :: Model
                                   , travelType :: TravelType
                                   , cost :: Double }
                     deriving Show

    data Model = Model String Integer String
               deriving Show

    data TravelType = Past | Future | Both
                    deriving Show

    clientName :: Client -> String
    clientName  client = case client of
                GovOrg name         -> name
                Company name _ _ _  -> name
                Individual person _ ->
                    case person of Person fName lName _ -> fName ++ " " ++ lName

    companyName :: Client -> Maybe String
    companyName client = case client of
                Company name _ _ _  -> Just name
                _                   -> Nothing

    clientStatistics :: [Client] -> (Integer, Integer)
    clientStatistics [] = (0, 0)
    clientStatistics (x:xs) = case x of
                     Individual (Person _ _ Male) _     -> (males + 1, females)
                     Individual (Person _ _ Female) _   -> (males, females + 1)
                     _                                  -> (males, females)
                     where tailStatistics = clientStatistics xs
                           males = fst tailStatistics
                           females = snd tailStatistics

    makeDiscount :: [TimeMachine] -> Double -> [TimeMachine]
    makeDiscount [] _ = []
    makeDiscount (t@(TimeMachine { cost }):xs) percent =  let newCost = cost*(1 - percent)
                                                          in t{cost = newCost}:makeDiscount xs percent