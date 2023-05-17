-- $stack run clash -- ./src/GenericPrettyPrintExample.hs -main-is GenericPrettyPrintExample -isrc -o out/main
-- $./out/main
module GenericPrettyPrintExample(main) where

import Text.PrettyPrint.GenericPretty
import Clash.Prelude
import Data.Data

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            deriving (Generic)

instance (Out a) => Out (Tree a)

tree1 :: Tree Int
tree1 = Node (Node (Leaf 333333) (Leaf (-555555)))
             (Node (Node (Node (Leaf 888888) (Leaf 57575757))
                         (Leaf (-14141414)))
                   (Leaf 7777777))

main = pp tree1