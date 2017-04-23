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
 , mpvEventSeek = MPV_EVENT_SEEK
 }

newtype MpvError = MpvError { unMpvError :: CInt } deriving (Eq,Show)  

#{enum MpvError, MpvError 
    ,mpvErrorSuccess = MPV_ERROR_SUCCESS
    ,mpvErrorEventQueueFull = MPV_ERROR_EVENT_QUEUE_FULL
    ,mpvErrorNomem = MPV_ERROR_NOMEM
    ,mpvErrorUninitialized = MPV_ERROR_UNINITIALIZED
    ,mpvErrorInvalidParameter = MPV_ERROR_INVALID_PARAMETER
    ,mpvErrorOptionNotFound = MPV_ERROR_OPTION_NOT_FOUND
    ,mpvErrorOptionFormat = MPV_ERROR_OPTION_FORMAT
    ,mpvErrorOptionError = MPV_ERROR_OPTION_ERROR
    ,mpvErrorPropertyNotFound = MPV_ERROR_PROPERTY_NOT_FOUND
    ,mpvErrorPropertyFormat = MPV_ERROR_PROPERTY_FORMAT
    ,mpvErrorPropertyUnavailable = MPV_ERROR_PROPERTY_UNAVAILABLE
    ,mpvErroPropertyError = MPV_ERROR_PROPERTY_ERROR
    ,mpvErrorCommand = MPV_ERROR_COMMAND
    ,mpvErrorLoadingFailed = MPV_ERROR_LOADING_FAILED
    ,mpvErrorAoInitFailed = MPV_ERROR_AO_INIT_FAILED
    ,mpvErrorVoInitFailed = MPV_ERROR_VO_INIT_FAILED
    ,mpvErrorNothingToPlay = MPV_ERROR_NOTHING_TO_PLAY
    ,mpvErrorUnknownFormat = MPV_ERROR_UNKNOWN_FORMAT
    ,mpvErrorUnsupported = MPV_ERROR_UNSUPPORTED
    ,mpvErrorNotImplemented = MPV_ERROR_NOT_IMPLEMENTED
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
