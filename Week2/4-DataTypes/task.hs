--task 1
data IntList = Cons Int IntList | Empty
  deriving (Show)

fromList :: [Int] -> IntList
fromList [] = Empty
fromList (x:xs) = Cons x (fromList xs)

toList :: IntList -> [Int]
toList Empty = []
toList (Cons x xs) = x : toList xs

--task 2

type Title = [Char]
type Genre = [Char]
type Length = Int

data Song = Song Title Author Genre Length
    deriving (Show)

isEqualSong :: Song -> Song -> Bool
isEqualSong (Song t1 a1 g1 l1) (Song t2 a2 g2 l2)
    | t1 == t2 && (isEqualAuthor a1 a2) && g1 == g2 && l1 == l2 = True
    | otherwise = False

isSongFromAuthor :: Song -> Author -> Bool
isSongFromAuthor (Song _ a1 _ _) a2 = isEqualAuthor a1 a2

type Name = [Char]
type BirthDay = Int
type RecordLable = [Char]

data Author = Author Name BirthDay RecordLable
    deriving (Show)

isEqualAuthor :: Author -> Author -> Bool
isEqualAuthor (Author n1 b1 r1) (Author n2 b2 r2) = n1 == n2 && b1 == b2 && r1 == r2
data Library = Library [Song]
    deriving (Show)

addSongToLib :: Song -> Library -> Library
addSongToLib s (Library x) = Library (s : x)

removeSongFromLib :: Song -> Library -> Library
removeSongFromLib _ (Library []) = Library []
removeSongFromLib s (Library [x])
    | isEqualSong s x = Library []
    | otherwise = Library [x]
removeSongFromLib s (Library (x:xs))
    | isEqualSong s x = Library xs
    | otherwise = removeSongFromLib s (Library xs)

removeSongsByAuthor :: Author -> Library -> Library
removeSongsByAuthor _ (Library []) = Library []
removeSongsByAuthor a (Library (x:xs))
    | isSongFromAuthor x a = removeSongsByAuthor a (Library xs)
    | otherwise = addSongToLib x (removeSongsByAuthor a (Library xs))

getSongFromTitle :: Title -> Library -> Song
getSongFromTitle _ (Library []) = error "Dont have song with this title in list"
getSongFromTitle t (Library ((Song t1 a1 g1 l1):xs))
    | t == t1 = (Song t1 a1 g1 l1)
    | otherwise = getSongFromTitle t (Library xs)

getSongFromAuthor :: Author -> Library -> Song
getSongFromAuthor _ (Library []) = error "Dont have songs from this author in list"
getSongFromAuthor a (Library ((Song t1 a1 g1 l1):xs))
    | isEqualAuthor a a1 = Song t1 a1 g1 l1
    | otherwise = getSongFromAuthor a (Library xs)

getAuthorInf :: Author -> Library -> Author
getAuthorInf _ (Library []) = error "Dont have this author in list"
getAuthorInf a (Library ((Song _ a1 _ _):xs))
    | isEqualAuthor a a1 = a
    | otherwise = getAuthorInf a (Library xs)

a1 = Author "Alex" 1306 "AlexSongs"
a2 = Author "AC-DC" 1994 "Black Ice"
s2 = Song "Back In Black" a2 "Rock" 400
s1 = Song "Thunderstruck" a1 "Rock" 353
l1 = Library [s1]

--task 3
data List a = ConsList a (List a) | EmptyList
    deriving(Show)

