module Task8
import StdEnv




:: City = {name :: String , area :: Int , population :: Int }

smallCityCount :: [City] -> Int
smallCityCount [] = 0
smallCityCount list
| (hd list).area < 100 || (hd list).population < 300000 = 1 + smallCityCount (drop 1 list)
= smallCityCount (drop 1 list)

budapest={name="Budapest", area=525, population=1756000}
kutaisi={name="Kutaisi", area=67, population=147000}
debrecen={name="Debrecen", area=461, population=202000}
berlin={name="Berlin", area=891, population=3645000}
pisa={name="Pisa", area=185, population=90000}

// Start = smallCityCount [] // 0
//Start = smallCityCount [budapest,kutaisi,debrecen,berlin,pisa] // 3
// Start = smallCityCount [budapest,berlin] // 0
// Start = smallCityCount [kutaisi] // 1
