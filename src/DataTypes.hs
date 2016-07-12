{-# LANGUAGE NamedFieldPuns, LambdaCase #-}
module DataTypes where

import SimpleFunctions

--data Client = GovOrg     String
--            | Company    String Integer Person String
--            | Individual Person Bool
--            deriving Show

--data Person = Person String String Gender
--            deriving Show

data Client i = GovOrg  { clientId :: i, clientRName :: String}
                 | Company {  clientId :: i
                            , clientRName :: String
                            , companyId :: Integer
                            , person :: Person
                            , duty :: String }
                 | Individual { clientId :: i, person :: Person }
                 deriving Show

data Person = Person {   firstName ::String
                       , lastName :: String
                       , gender :: Gender
                       } deriving Show

data Gender = Male | Female | Unknown
            deriving Show

data TimeMachine = TimeMachine { manufacturer :: String
                               , model :: Integer
                               , name :: String
                               , travelType :: TravelType
                               , cost :: Double }
                 deriving Show

data TravelType = Past | Future | Both
                deriving Show

clientName :: Client a -> String
clientName  client = case client of
            GovOrg {clientRName}         -> clientRName
            Company {clientRName}        -> clientRName
            Individual {person = Person{firstName, lastName}} -> firstName ++ " " ++ lastName

companyName :: Client a -> Maybe String
companyName client = case client of
            Company {clientRName}      -> Just clientRName
            _                   -> Nothing

clientStatistics :: [Client a] -> (Integer, Integer)
clientStatistics [] = (0, 0)
clientStatistics (x:xs) = case x of
                 Individual {person = (Person {gender = Male})}     -> (males + 1, females)
                 Individual {person = (Person {gender = Female})}   -> (males, females + 1)
                 _                                  -> (males, females)
                 where tailStatistics = clientStatistics xs
                       males = fst tailStatistics
                       females = snd tailStatistics

makeDiscount :: [TimeMachine] -> Double -> [TimeMachine]
makeDiscount [] _ = []
makeDiscount (t@(TimeMachine { cost }):xs) percent =  let newCost = cost*(1 - percent)
                                                      in t{cost = newCost}:makeDiscount xs percent

isGovOrg :: Client a -> Bool
isGovOrg (GovOrg {}) = True
isGovOrg _ = False

filterGovOrgs :: [Client a] -> [Client a]
filterGovOrgs = filter isGovOrg

filterGovOrgs' :: [Client a] -> [Client a]
filterGovOrgs' = filter (\case  GovOrg {} -> True
                                _ -> False
                        )

minimumClient :: [Client a] -> Client a
minimumClient = minimumBy $ length . clientName

isIndividual :: Client a -> Bool
isIndividual (Individual {}) = True
isIndividual _ = False

checkIndividualAnalytics :: [Client a] -> (Bool, Bool)
checkIndividualAnalytics clients = (any isIndividual clients, not $ all isIndividual clients)

compareClient :: Client a -> Client a -> Ordering
compareClient (Individual{person = p1}) (Individual{person = p2})
                                = compare (firstName p1) (firstName p2)
compareClient (Individual {}) _ = GT
compareClient _ (Individual {}) = LT
compareClient c1 c2             = compare (clientName c1) (clientName c2)

