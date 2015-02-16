module Application.Engine where

import Application.Types
import qualified Data.Map as M


evalEvent :: Event -> AppState -> AppState
evalEvent (AddTopic t) as      = addTopic t as
evalEvent (DeleteTopic t) as   = deleteTopic t as
evalEvent (AddRoom r) as       = addRoom r as
evalEvent (DeleteRoom r) as    = deleteRoom r as
evalEvent (AddBlock b) as      = addBlock b as
evalEvent (DeleteBlock b) as   = deleteBlock b as
evalEvent (AssignTopic s t) as = addTimeslot s t as
evalEvent (UnassignTopic t) as = as { timeslots = M.filter (/= t) (timeslots as) }
evalEvent (ReplayEvents as) _ = foldl (flip evalEvent) emptyState as
evalEvent (ShowError _) as     = as
evalEvent NOP as               = as

addTimeslot :: Slot -> Topic -> AppState -> AppState
addTimeslot s t as = let topicslotFilter = M.filter (/= t)
                     in as { timeslots = M.insert s t $ topicslotFilter (timeslots as) }

deleteTimeslot :: Slot -> AppState -> AppState
deleteTimeslot s as = as { timeslots = M.delete s (timeslots as) }

addTopic :: Topic -> AppState -> AppState
addTopic t as = as { topics = t: topics as}

deleteTopic :: Topic -> AppState -> AppState
deleteTopic t as = let topicslotFilter = M.filter (/= t)
                   in as { topics    = filter (/=t) (topics as)
                         , timeslots = topicslotFilter (timeslots as)
                         }

addRoom :: Room -> AppState -> AppState
addRoom r as = as { rooms = r : rooms as }

deleteRoom :: Room -> AppState -> AppState
deleteRoom r as = as { rooms = filter (/= r ) (rooms as)
                     , timeslots = M.fromList $ roomFilter $ M.toList (timeslots as)}
                     where roomFilter = filter (\(s, _) -> room s /= r)

addBlock :: Block -> AppState -> AppState
addBlock b as = as { blocks = b : blocks as }

deleteBlock :: Block -> AppState -> AppState
deleteBlock b as = as { blocks = filter (/= b ) (blocks as)
                      , timeslots = M.fromList $ blockFilter $ M.toList (timeslots as)}
                      where blockFilter = filter (\(s, _) -> block s /= b)

replayEvents :: AppState -> [Event] -> AppState
replayEvents = foldl (flip evalEvent)

generateEvents :: AppState -> [Event]
generateEvents as = concat [t, r, b, ts]
  where t  = reverse $ map AddTopic (topics as)
        r  = reverse $ map AddRoom (rooms as)
        b  = reverse $ map AddBlock (blocks as)
        ts = map (uncurry AssignTopic) (M.toList (timeslots as))
