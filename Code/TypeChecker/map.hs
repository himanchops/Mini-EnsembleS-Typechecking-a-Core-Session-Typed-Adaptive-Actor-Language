import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type TypeEnv = [(String, String)]

-- checkTypeEnv :: String -> TypeEnv -> Bool
checkTypeEnv value map = Map.member value $ Map.fromList map

-- getTypeEnv :: String -> TypeEnv -> Maybe String
getTypeEnv value map = Map.lookup value $ Map.fromList map

func xs 0 = xs
func xs n = func (("a","b"):xs) (n-1)

main = do
  let x = getTypeEnv "a" $ func [] 3
  let y = getTypeEnv "c" $ func [] 3
  return (x,y)
