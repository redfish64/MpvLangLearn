module MpvStructs where

#include <mpv/client.h>

  
import Foreign
import Foreign.C.Types
import Foreign.C.String

newtype MpvEventId = MpvEventId { unMpvEventId :: CInt } deriving (Eq,Show)  

#{enum MpvEventId, MpvEventId
 , mpvEventNone = MPV_EVENT_NONE
 , mpvEventShutdown = MPV_EVENT_SHUTDOWN
 , mpvEventLogMessage = MPV_EVENT_LOG_MESSAGE
 , mpvEventIdle = MPV_EVENT_IDLE
 , mpvEventPlaybackRestart = MPV_EVENT_PLAYBACK_RESTART
 }

instance Storable MpvEventId where
  alignment _ = #{alignment mpv_event_id}
  sizeOf _ = #{size mpv_event_id}
  peek ptr = do
    event_id <- (peekByteOff ptr 0)
    return (MpvEventId event_id)
  poke ptr (MpvEventId event_id) = do
    (pokeByteOff ptr 0  event_id)
  

data MpvEvent = MpvEvent { event_id :: MpvEventId }

instance Storable MpvEvent where
  alignment _ = #{alignment mpv_event}
  sizeOf _ = #{size mpv_event}
  peek ptr = do
    event_id <- #{peek mpv_event, event_id} ptr
    -- b <- #{peek mpv_event, b} ptr
    -- c <- #{peek mpv_event, c} ptr
    return (MpvEvent event_id)
  poke ptr (MpvEvent event_id) = do
    #{poke mpv_event, event_id} ptr event_id
    -- #{poke mpv_event, b} ptr b
    -- #{poke mpv_event, c} ptr c 
