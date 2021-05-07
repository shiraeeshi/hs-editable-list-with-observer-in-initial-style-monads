{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE  ExistentialQuantification #-}
module Main where

import Control.Monad (when, forM_)
import Control.Exception (try)
import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import Control.Monad.State.Strict (StateT, get, modify, runStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map
import ViewUtils (clearScreen, showInRectangle, clearRectangle, showInGrid, drawGrid, highlightCell, printFromBottom)
import Prelude hiding (log)

data RowData = Row { smth :: String } deriving Eq

initialRows = [
  Row "something a"
  , Row "something b"
  , Row "something c"
  , Row "something d"
  , Row "something e"
  ]

data AppStateData m = AppState
  { rows :: [RowData]
  , activeCellY :: Maybe Int
  , debugMessages :: [String]
  , listeners :: AppStateListenersData m
  }

data AppStateListenersData m = AppStateListeners
  { rowsListeners :: [[RowData] -> m ()]
  , activeCellYListeners :: [Maybe Int -> m ()]
  , debugMessagesListeners :: [[String] -> m ()]
  }

addRowsListener :: Monad m => ([RowData] -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addRowsListener listener (AppStateListeners rowsListeners _activeCellYListeners _debugMessagesListeners) =
  AppStateListeners (listener:rowsListeners) _activeCellYListeners _debugMessagesListeners

addActiveCellYListener :: Monad m => (Maybe Int -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addActiveCellYListener listener (AppStateListeners _rowsListeners activeCellYListeners _debugMessagesListeners) =
  AppStateListeners _rowsListeners (listener:activeCellYListeners) _debugMessagesListeners

addDebugMessagesListener :: Monad m => ([String] -> m ()) -> AppStateListenersData m -> AppStateListenersData m
addDebugMessagesListener listener (AppStateListeners _rowsListeners _activeCellYListeners debugMessagesListeners) =
  AppStateListeners _rowsListeners _activeCellYListeners (listener:debugMessagesListeners)

data EditableListOps a = GetList                          ([RowData]   -> EditableListOps a)
                       | GetActiveCellY                   ((Maybe Int) -> EditableListOps a)
                       | GetLogs                          ([String]    -> EditableListOps a)
                       | UpdateList           [RowData]   (EditableListOps a)
                       | UpdateActiveCellY    (Maybe Int) (EditableListOps a)
                       | Log                  String      (EditableListOps a)
                       | forall b. LiftIO     (IO b)      (b -> EditableListOps a)
                       | Done                 a

instance Functor EditableListOps where
  fmap f (GetList k)              = GetList        $ \lst      -> fmap f (k lst)
  fmap f (GetActiveCellY k)       = GetActiveCellY $ \maybeInt -> fmap f (k maybeInt)
  fmap f (GetLogs k)              = GetLogs        $ \logs     -> fmap f (k logs)
  fmap f (UpdateList lst op)      = UpdateList        lst (fmap f op)
  fmap f (UpdateActiveCellY y op) = UpdateActiveCellY y   (fmap f op)
  fmap f (Log msg op)             = Log               msg (fmap f op)
  fmap f (LiftIO a op)            = LiftIO            a   (\k -> fmap f (op k))
  fmap f (Done a)                 = Done (f a)

instance Applicative EditableListOps where
  pure = Done

  (GetList h)             <*> g = GetList        $ \lst  -> (h  lst) <*> g
  (GetActiveCellY h)      <*> g = GetActiveCellY $ \y    -> (h    y) <*> g
  (GetLogs h)             <*> g = GetLogs        $ \logs -> (h logs) <*> g
  (UpdateList lst h)      <*> g = UpdateList        lst (h <*> g)
  (UpdateActiveCellY y h) <*> g = UpdateActiveCellY   y (h <*> g)
  (Log msg h)             <*> g = Log               msg (h <*> g)
  (LiftIO a h)            <*> g = LiftIO              a (\k -> (h k) <*> g)
  (Done a)                <*> g = fmap a g

instance Monad EditableListOps where
  return = Done
  (GetList h)             >>= f = GetList        $ \lst  -> (h  lst) >>= f
  (GetActiveCellY h)      >>= f = GetActiveCellY $ \y    -> (h    y) >>= f
  (GetLogs h)             >>= f = GetLogs        $ \logs -> (h logs) >>= f
  (UpdateList lst h)      >>= f = UpdateList        lst (h >>= f)
  (UpdateActiveCellY y h) >>= f = UpdateActiveCellY   y (h >>= f)
  (Log msg h)             >>= f = Log               msg (h >>= f)
  (LiftIO a h)            >>= f = LiftIO              a (\k -> (h k) >>= f)
  (Done x)                >>= f = f x

getList :: EditableListOps [RowData]
getList = GetList return

getActiveCellY :: EditableListOps (Maybe Int)
getActiveCellY = GetActiveCellY return

getLogs :: EditableListOps [String]
getLogs = GetLogs return

updateList :: [RowData] -> EditableListOps ()
updateList l = UpdateList l $ return ()

updateActiveCellY :: (Maybe Int) -> EditableListOps ()
updateActiveCellY y = UpdateActiveCellY y $ return ()

log :: String -> EditableListOps ()
log msg = Log msg $ return ()

instance MonadIO EditableListOps where
  -- liftIO a = LiftIO a $ \k -> return k
  liftIO a = LiftIO a return

newtype StateHolder a = StateHolder (StateT (AppStateData StateHolder) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

interpret :: EditableListOps a -> StateHolder a
interpret (Done x) = return x
interpret (GetList k) = do lst <- rows <$> (StateHolder get)
                           interpret (k lst)
interpret (GetActiveCellY k) = do y <- activeCellY <$> (StateHolder get)
                                  interpret (k y)
interpret (GetLogs k) = do logs <- debugMessages <$> (StateHolder get)
                           interpret (k logs)
interpret (UpdateList l k) = do StateHolder $ modify $ \s -> s { rows = l }
                                reacts <- (rowsListeners . listeners) <$> (StateHolder get)
                                forM_ reacts ($ l) -- same as forM_ reacts $ \react -> react l
                                interpret k
interpret (UpdateActiveCellY y k) = do StateHolder $ modify $ \s -> s { activeCellY = y }
                                       reacts <- (activeCellYListeners . listeners) <$> (StateHolder get)
                                       forM_ reacts ($ y) -- same as forM_ reacts $ \react -> react y
                                       interpret k
interpret (Log msg k) = do StateHolder $ modify $ \s -> s { debugMessages = take debugLinesCount (msg:(debugMessages s)) }
                           logs <- debugMessages <$> (StateHolder get)
                           reacts <- (debugMessagesListeners . listeners) <$> (StateHolder get)
                           forM_ reacts ($ logs) -- same as forM_ reacts $ \react -> react logs
                           interpret k
interpret (LiftIO a k) = do v <- liftIO a
                            interpret (k v)

performStateAction :: AppStateData StateHolder -> StateHolder a -> IO ()
performStateAction state (StateHolder action) = do
  runStateT action state
  return ()

debugLinesCount = 20

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  --performStateAction initialState (interpret (do ...))
  performStateAction initialState $ interpret $ do
    initRows
    loop
  where
    xUpperLeft = 0
    yUpperLeft = 0
    columnCount = 1
    columnWidth = 14
    rowCount = length initialRows

    initialState :: AppStateData StateHolder
    initialState = AppState [] Nothing [] initListeners

    initRows :: EditableListOps ()
    initRows = updateList initialRows

    initListeners :: AppStateListenersData StateHolder
    -- initListeners =
    --     addRowsListener (interpret . mainRowsListener)
    --     (addActiveCellYListener (interpret . activeCellYListener)
    --     (addDebugMessagesListener (interpret . debugMessagesListener)
    --     (empty)))
    initListeners =
        addRowsListener (interpret . mainRowsListener)
        $ addActiveCellYListener (interpret . activeCellYListener)
        $ addDebugMessagesListener (interpret . debugMessagesListener)
        $ empty
      where
        empty = AppStateListeners [] [] []

    mainRowsListener :: [RowData] -> EditableListOps ()
    mainRowsListener rows = do
      activeCellCoords <- fmap (\y -> (0, y)) <$> getActiveCellY
      liftIO $ showInGrid
                 xUpperLeft
                 yUpperLeft
                 columnCount
                 columnWidth
                 activeCellCoords
                 (map (\row -> [smth row]) rows)
      log "updated rows"

    activeCellYListener :: Maybe Int -> EditableListOps ()
    activeCellYListener activeCellY = do
      let activeCellCoords = fmap (\y -> (0, y)) activeCellY
      liftIO $ drawGrid xUpperLeft yUpperLeft columnWidth columnCount rowCount
      case activeCellCoords of
        Nothing -> return ()
        Just coordsPair -> do
          liftIO $ highlightCell xUpperLeft yUpperLeft columnWidth columnCount rowCount coordsPair
          log "highlighted cell"

    debugMessagesListener :: [String] -> EditableListOps ()
    debugMessagesListener debugMessages = do
      liftIO $ printFromBottom
                 xUpperLeft
                 (yUpperLeft+12+debugLinesCount)
                 debugMessages

    loop :: EditableListOps ()
    loop = do
      key <- liftIO $ getKey
      when (key /= "\ESC") $ do
        case key of
          "\ESC[A" -> do -- up
              activeCellY <- getActiveCellY
              let
                newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ max 0 (y-1)
                    Nothing -> Just 0
              updateActiveCellY newActiveCellY
              log $ "↑ " ++ show(newActiveCellY)
              loop
          "\ESC[B" -> do -- down
              activeCellY <- getActiveCellY
              let
                newActiveCellY =
                  case activeCellY of
                    Just y -> Just $ min (rowCount-1) (y+1)
                    Nothing -> Just 0
              updateActiveCellY newActiveCellY
              log $ "↓ " ++ show(newActiveCellY)
              loop
          "\n" -> do -- enter
              activeCellY <- getActiveCellY
              rows <- getList
                
              let
                  eitherValue :: Either String String
                  eitherValue =
                    case activeCellY of
                      Nothing -> Left "there's no selected cell"
                      Just cellIndex ->
                        if cellIndex < 0 || cellIndex >= (length rows)
                          then Left $ "index out of bounds: " ++ (show cellIndex)
                          else Right $ smth $ rows !! cellIndex

                  showEditField :: String -> EditableListOps ()
                  showEditField value = do
                    let
                      txt = "edit cell value:"
                      lentxt = length txt
                      yPos = 0
                      xPos = (columnCount * (columnWidth + 1)) + 3
                      replaceNth lst idx val = if idx < 1 then val:(tail lst) else (head lst) : (replaceNth (tail lst) (idx - 1) val)
                    liftIO $ showInRectangle xPos yPos lentxt [txt, value]
                    key <- liftIO $ getKey
                    case key of
                      "\n" -> do
                        case activeCellY of
                          Nothing -> return ()
                          Just cellIndex -> do
                            liftIO $ clearRectangle xPos yPos lentxt 2
                            rows <- getList
                            updateList $ replaceNth rows cellIndex (Row value)
                            loop
                      "\DEL" -> showEditField (if (length value) == 0 then value else init value)
                      c -> showEditField (value ++ c)
              case eitherValue of
                Left e -> do
                  log $ "error: " ++ (show e)
                  loop
                Right v -> do
                  showEditField v
          "q" -> return ()
          _ -> return ()

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
