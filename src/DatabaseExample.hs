module DatabaseExample where

import Control.Applicative
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

-- tables:
-- Users (id, username)
-- Tools (id, name, ...)
-- Checkedout (user_id, tool_id)
--
data Tool = Tool
  { toolId :: Int
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesBorrowed :: Int
  }

instance Show Tool where
  show (Tool id name desc lastReturned timesBorrowed) =
    mconcat
      [ show id
      , ".) "
      , name
      , "\n description: "
      , desc
      , "\n last returned: "
      , show lastReturned
      , "\n times borrowed: "
      , show timesBorrowed
      ]

data User = User
  { userId :: Int
  , userName :: String
  }

instance Show User where
  show (User userId userName) = mconcat [show userId, ".) ", userName]

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

-- add a user to the database
-- The Only constructor is used to create single-element tuples. This is
-- needed because execute expects you to pass in a tuple of a particular
-- size for your values.
--
-- the "\conn ->" function looks like a "ruby block" or callback!
addUser :: String -> IO ()
addUser username =
  withConn "tools.db" $ \conn -> do
    execute conn "insert into users (username) values (?)" (Only username)
    print "user added"

addTool :: String -> String -> IO ()
addTool name description =
  withConn "tools.db" $ \conn -> do
    let q =
          "insert into tools (name, description, lastReturned, timesBorrowed) values (?, ?, ?, ?)"
    day <- utctDay <$> getCurrentTime
    execute conn q (name, description, day, 0 :: Int)
    print "tool added"

checkout :: Int -> Int -> IO ()
checkout userId toolId =
  withConn "tools.db" $ \conn -> do
    execute
      conn
      "insert into checkedout (user_id, tool_id) values (?, ?)"
      (userId, toolId)
    print "checkout added"

instance FromRow User where
  fromRow = User <$> field <*> field

instance FromRow Tool where
  fromRow = Tool <$> field <*> field <*> field <*> field <*> field

printUsers :: IO ()
printUsers =
  withConn "tools.db" $ \conn -> do
    resp <- query_ conn "select * from users;" :: IO [User]
    mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q =
  withConn "tools.db" $ \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "select * from tools;"

printAvailableTools :: IO ()
printAvailableTools =
  printToolQuery $
  mconcat
    [ "select * from tools "
    , "where id not in "
    , "(select tool_id from checkedout);"
    ]

printCheckedout :: IO ()
printCheckedout =
  printToolQuery $
  mconcat
    [ "select * from tools "
    , "where id in "
    , "(select tool_id from checkedout);"
    ]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <-
    query conn "select * from tools where id = (?)" (Only toolId) :: IO [Tool]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  tool {lastReturned = date, timesBorrowed = 1 + timesBorrowed tool}

updateOrWarn :: Connection -> Maybe Tool -> IO ()
updateOrWarn _ Nothing = print "id not found"
updateOrWarn conn (Just tool) = do
  let q =
        mconcat
          [ "update tools set lastReturned = ?, "
          , "timesBorrowed = ? where id = ?;"
          ]
  execute conn q (lastReturned tool, timesBorrowed tool, toolId tool)
  print "tool updated"

-- update lastReturned and increment timesBorrowed by 1
updateToolTable :: Int -> IO ()
updateToolTable toolId =
  withConn "tools.db" $ \conn -> do
    tool <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool <*> pure currentDay
    updateOrWarn conn updatedTool

checkin :: Int -> IO ()
checkin toolId =
  withConn "tools.db" $ \conn -> do
    let q = "delete from checkedout where tool_id = ?"
    execute conn q (Only toolId)
    print "tool checked in"

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

-- putting it all together!
promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new user name"
  userName <- getLine
  addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user"
  userId <- pure read <*> getLine
  print "Enter the id of the tool"
  toolId <- pure read <*> getLine
  checkout userId toolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  print "enter the id of tool"
  toolId <- pure read <*> getLine
  checkinAndUpdate toolId

promptAndAddTool :: IO ()
promptAndAddTool = do
  print "Enter tool name"
  toolName <- getLine
  print "Enter tool description"
  toolDesc <- getLine
  addTool toolName toolDesc

performCommand :: String -> IO ()
performCommand command
  | command == "users" = printUsers >> mainDB
  | command == "tools" = printTools >> mainDB
  | command == "adduser" = promptAndAddUser >> mainDB
  | command == "addtool" = promptAndAddTool >> mainDB
  | command == "checkout" = promptAndCheckout >> mainDB
  | command == "checkin" = promptAndCheckin >> mainDB
  | command == "in" = printAvailableTools >> mainDB
  | command == "out" = printCheckedout >> mainDB
  | command == "quit" = print "bye!"
  | otherwise = print "Sorry command not found" >> mainDB

mainDB :: IO ()
mainDB = do
  print "Enter a command"
  command <- getLine
  performCommand command
