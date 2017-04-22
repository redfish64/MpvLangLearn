{-# LANGUAGE ForeignFunctionInterface #-}

-- :set -lmpv

module MpvFFI (mpvCreate,
              mpvSetOptionString,
              mpvSetOptionFlag,
              mpvSetOptionDouble,
              mpvSetPropertyDouble,
              mpvSetPropertyString,
              mpvGetPropertyDouble,
              setupMpvFlags,
              setupMpvOptions,
              mpvInitialize,
              mpvWaitEvent,
              mpvTerminateDestroy,
              setMultipleSubfiles,
              loadFiles,
              Ctx)
              where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import MpvStructs

import Control.Exception
--import Data.Typeable
import Util
import Control.Monad.Reader

data MpvFFIException = MpvFFIException String
    deriving (Show)
--    deriving (Show, Typeable)

instance Exception MpvFFIException

type Ctx = Ptr ()
type Event = ()

data MpvFFIEnv = MpvFFIEnv {
  --called whenever an mpv error occurs
  errorFunc :: Call -> MpvError -> MFM ()
  }

type MFM = ReaderT MpvFFIEnv IO

data Call = CMpvCreate | CMpvInitialize | CMpvTerminateDestroy | CMpvSetOptionString
          | CMpvSetPropertyString | CMpvSetOption | CMpvSetProperty
          | CMpvCommand | CSetMultipleSubfiles | CMpvWaitEvent
          | CMpvGetProperty

foreign import ccall unsafe "mpv/client.h mpv_create"
        c_mpv_create :: IO Ctx

foreign import ccall unsafe "mpv/client.h mpv_initialize"
        c_mpv_initialize :: Ctx -> IO CInt

foreign import ccall unsafe "mpv/client.h mpv_terminate_destroy"
        c_mpv_terminate_destroy :: Ctx -> IO ()

foreign import ccall unsafe "mpv/client.h mpv_set_option_string"
        c_mpv_set_option_string :: Ctx -> CString -> CString -> IO CInt

foreign import ccall unsafe "mpv/client.h mpv_set_property_string"
        c_mpv_set_property_string :: Ctx -> CString -> CString -> IO CInt

 -- * @param name Option name. This is the same as on the mpv command line, but
 -- *             without the leading "--".
 -- * @param format see enum mpv_format.
 -- * @param[in] data Option value (according to the format).
foreign import ccall unsafe "mpv/client.h mpv_set_option"
        c_mpv_set_option :: Ctx -> CString -> CInt -> (Ptr ()) -> IO CInt

foreign import ccall unsafe "mpv/client.h mpv_set_property"
        c_mpv_set_property :: Ctx -> CString -> CInt -> (Ptr ()) -> IO CInt

--Ptr CString must be a null terminated array of strings
foreign import ccall unsafe "mpv/client.h mpv_command"
        c_mpv_command :: Ctx -> (Ptr CString) -> IO CInt

foreign import ccall unsafe "foo.h set_multiple_subfiles"
        c_set_multiple_subfiles :: Ctx -> CInt -> (Ptr CString) -> IO CInt

foreign import ccall unsafe "mpv/client.h mpv_wait_event"
        c_mpv_wait_event :: Ctx -> CDouble -> IO (Ptr MpvEvent)

-- foreign import ccall unsafe "mpv/client.h mpv_observe_property"
--         c_mpv_observe_property Ctx -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "mpv/client.h mpv_get_property"
        c_mpv_get_property :: Ctx -> CString -> CInt -> (Ptr ()) -> IO CInt

--if func returns true, throws an exception
throw_mpve_on :: Show a => IO a -> (a -> Maybe String) -> IO a
throw_mpve_on iv f =
  do
    v <- iv
    case (f v) of
      Just m -> throw $ MpvFFIException m
              -- do putStrLn("Exception! "++(show v))
              --    return v
      Nothing -> return v

--checks for mpv status and throws exception if fails (if status is less than zero)
check_mpv_status :: IO CInt -> IO CInt
check_mpv_status iv = undefined --throw_mpve_on iv (\v -> v < 0)

mpvCreate = throw_mpve_on c_mpv_create $ (\v -> if v == nullPtr then Just "NPE when calling mpv_create" else Nothing)


mpvSetOptionString ctx name value =
  do
    putStrLn $ "mpv_set_option_string "++name ++ " " ++ value
    withCString name
       (\cname ->
         withCString value
           (\cvalue ->
              (check_mpv_status (c_mpv_set_option_string ctx cname cvalue))))
              
mpvSetOptionFlag ctx name v =
  do
    withCString name
       (\cname ->
          alloca
            ((\value ->
               do
                 poke value v
                 voidvalue <- return (castPtr value)
                 --3 == MPV_FORMAT_FLAG TODO: put in enum
                 check_mpv_status (c_mpv_set_option ctx cname 3 voidvalue)) :: (Ptr CInt -> IO CInt))
            
       )

mpvSetOptionDouble ctx name v =
  do
    withCString name
       (\cname ->
          alloca
            ((\value ->
               do
                 poke value v
                 voidvalue <- return (castPtr value)
                 --5 == MPV_FORMAT_DOUBLE TODO: put in enum
                 check_mpv_status (c_mpv_set_option ctx cname 5 voidvalue)) :: (Ptr CDouble -> IO CInt))
            
       )

mpvSetPropertyDouble ctx name v =
  do
    withCString name
       (\cname ->
          alloca
            ((\value ->
               do
                 poke value v
                 voidvalue <- return (castPtr value)
                 --5 == MPV_FORMAT_DOUBLE TODO: put in enum
                 check_mpv_status (c_mpv_set_property ctx cname 5 voidvalue)) :: (Ptr CDouble -> IO CInt))
            
       )

mpvSetPropertyString ctx name value =
  do
    putStrLn $ "mpv_set_property_string "++name ++ " " ++ value
    withCString name
       (\cname ->
         withCString value
           (\cvalue ->
              (check_mpv_status (c_mpv_set_property_string ctx cname cvalue))))
              


--gets a property
--Ex. "time-pos"  position in current file in seconds
mpvGetPropertyDouble :: Ctx -> String -> IO (Maybe CDouble)
mpvGetPropertyDouble ctx name =
  do
    withCString name
       (\cname ->
          alloca
            ((\value ->
               do
                 voidvalue <- return (castPtr value)
                 --TODO: put in enum (5 == MPV_FORMAT_DOUBLE)
                 status <- c_mpv_get_property ctx cname 5 voidvalue
                 if status < 0
                 then
                   return Nothing
                 else
                   ((peek value) >>= return . Just)
             ) :: (Ptr CDouble -> IO (Maybe CDouble)))
            
       )
                       
setupMpvFlags :: Ctx -> [String] -> IO ()
setupMpvFlags ctx xs = recurseMonad xs (\x -> mpvSetOptionFlag ctx x 1 >> return ())
  

setupMpvOptions :: Ctx -> [(String,String)] -> IO ()
setupMpvOptions ctx xs = recurseMonad xs (\(x,y) -> mpvSetOptionString ctx x y >> return ())


       

mpvInitialize ctx = check_mpv_status (c_mpv_initialize ctx)

mpvWaitEvent = c_mpv_wait_event

mpvTerminateDestroy = c_mpv_terminate_destroy
    

--marshalls a list of strings into a c-array of c-strings and deallocates them after
--running the function
marshallCstringArray0 :: [ String ] -> (Ptr CString -> IO x) -> IO x
marshallCstringArray0 array func =
  allocaArray0 (Prelude.length array)
       (allocStringsAndRunFunc array [] func) --partial application  
  where
    --allocates cstrings for each string in array and finally calls function on result
    allocStringsAndRunFunc :: [ String ] -> [ CString ] -> (Ptr CString -> IO x) -> (Ptr CString) -> IO x
    allocStringsAndRunFunc [] cstr_array func ptr =
      do
        pokeArray0 nullPtr ptr (Prelude.reverse cstr_array)
        func ptr
    allocStringsAndRunFunc (s : sa) cstr_array func ptr =
      withCString s
        (\cs -> allocStringsAndRunFunc sa (cs : cstr_array) func ptr)
        

mpvCommand :: Ctx -> [ String ] -> IO CInt                 
mpvCommand ctx array =
  marshallCstringArray0 array
    (\cstr_arr ->
       do
         putStrLn $ "mpv_command: "++ show array
         (check_mpv_status (c_mpv_command ctx cstr_arr)))

setMultipleSubfiles :: Ctx -> [ String ] -> IO CInt                 
setMultipleSubfiles ctx array =
  marshallCstringArray0 array --TODO nullptr at end not needed
    (\cstr_arr ->
       do
         putStrLn $ "set_multiple_subfiles: "++ show array
         (check_mpv_status (c_set_multiple_subfiles ctx (fromIntegral (length array)) cstr_arr)))

loadFiles :: Ctx -> [String] -> IO ()
loadFiles ctx xs = recurseMonad xs (\x -> mpvCommand ctx ["loadfile",x] >> return ())
          

playMovie :: String -> IO ()
playMovie filename =
  do
    --lift some mpv context, since each command may return an error
    ctx <- mpvCreate
    putStrLn "created context"
    mpvSetOptionString ctx "input-default-bindings" "yes"
    mpvSetOptionString ctx "input-vo-keyboard" "yes"
    mpvSetOptionFlag ctx "osc" 1
    putStrLn "set options"
    putStrLn $ "ctx = " ++ (show ctx)
    mpvInitialize ctx
    putStrLn "initialized"
    mpvCommand ctx ["loadfile",filename]
    putStrLn "loaded file"
    eventLoop ctx
    putStrLn "finished event loop"
    mpvTerminateDestroy ctx -- this should be in some sort of failsafe (like java finally)
    return ()
  where
   eventLoop :: Ctx -> IO ()
   eventLoop ctx =
      do
        event <- (mpvWaitEvent ctx 1) >>= peek 
        putStrLn ("mpv_wait_event: " ++ (show (event_id event)))
        time <- mpvGetPropertyDouble ctx "time-pos"
        putStrLn ("time: " ++ (show time))
        case (event_id event) of
          id | id == mpvEventShutdown  -> return ()
          _ -> (eventLoop ctx)
