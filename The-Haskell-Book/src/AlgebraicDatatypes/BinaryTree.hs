module AlgebraicDatatypes.BinaryTree where
    
data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

instance Foldable BinaryTree where 
    foldr = foldTree

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right 
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = 
    Node (Node Leaf 3 Leaf)
         1
         (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = 
    Node (Node Leaf 4 Leaf)
         2
         (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay = 
    if mapTree (+1) testTree' == mapExpected 
    then putStrLn "yup okay!"
    else putStrLn "test failed!"

testTree :: BinaryTree Integer
testTree = 
    Node (Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)) 1 (Node Leaf 3 Leaf)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testPreorder :: IO ()
testPreorder = if preorder testTree == [1,2,4,5,3] 
               then putStrLn "Preorder fine!"
               else putStrLn "Bad news bears."

testInorder :: IO()
testInorder = if inorder testTree == [4,2,5,1,3] 
              then putStrLn "Inorder fine!"
              else putStrLn "Bad news bears."

testPostorder :: IO()
testPostorder = if postorder testTree == [4,5,2,3,1] 
              then putStrLn "Postorder fine!"
              else putStrLn "Bad news bears."

-- any traversal order is fine
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f i tree = foldr f i (inorder tree)
