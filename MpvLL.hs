{-# LANGUAGE ForeignFunctionInterface #-}

-- :set -lmpv

module MpvLL where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import MpvStructs

import Control.Exception
--import Data.Typeable

data MpvException = MpvException
    deriving (Show)
--    deriving (Show, Typeable)

instance Exception MpvException

type Ctx = ()
type Event = ()

foreign import ccall unsafe "mpv/client.h mpv_create"
        c_mpv_create :: IO (Ptr Ctx)

foreign import ccall unsafe "mpv/client.h mpv_initialize"
--foreign import ccall unsafe "mpv/client.h fakeout"
        c_mpv_initialize :: Ptr Ctx -> IO CInt

foreign import ccall unsafe "mpv/client.h mpv_terminate_destroy"
        c_mpv_terminate_destroy :: Ptr Ctx -> IO ()

foreign import ccall unsafe "mpv/client.h mpv_set_option_string"
        c_mpv_set_option_string :: (Ptr Ctx) -> CString -> CString -> IO CInt

 -- * @param name Option name. This is the same as on the mpv command line, but
 -- *             without the leading "--".
 -- * @param format see enum mpv_format.
 -- * @param[in] data Option value (according to the format).
foreign import ccall unsafe "mpv/client.h mpv_set_option"
        c_mpv_set_option :: (Ptr Ctx) -> CString -> CInt -> (Ptr ()) -> IO CInt

--Ptr CString must be a null terminated array of strings
foreign import ccall unsafe "mpv/client.h mpv_command"
        c_mpv_command :: (Ptr Ctx) -> (Ptr CString) -> IO CInt

foreign import ccall unsafe "mpv/client.h mpv_wait_event"
        c_mpv_wait_event :: (Ptr Ctx) -> CDouble -> IO (Ptr MpvEvent)

-- foreign import ccall unsafe "mpv/client.h mpv_observe_property"
--         c_mpv_observe_property (Ptr Ctx) -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "mpv/client.h mpv_get_property"
        c_mpv_get_property :: (Ptr Ctx) -> CString -> CInt -> (Ptr ()) -> IO CInt

--if func returns true, throws an exception
throw_mpve_on :: IO a -> (a -> Bool) -> IO a
throw_mpve_on iv f =
  do
    v <- iv
    case (f v) of
      True -> throw MpvException
      False -> return v

--checks for mpv status and throws exception if fails (if status is less than zero)
check_mpv_status :: IO CInt -> IO CInt
check_mpv_status iv = throw_mpve_on iv (\v -> v < 0)

mpv_create = throw_mpve_on c_mpv_create $ (==) nullPtr

mpv_set_option_string ctx name value =
  do
    withCString name
       (\cname ->
         withCString value
           (\cvalue ->
              (check_mpv_status (c_mpv_set_option_string ctx cname cvalue))))
              
mpv_set_option_flag ctx name v =
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


--gets a property
--Ex. "time-pos"  position in current file in seconds
mpv_get_property_double :: (Ptr Ctx) -> String -> IO (Maybe CDouble)
mpv_get_property_double ctx name =
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
--TODO reminds me of fold... is it related?
recurseMonad :: (Monad b) => [a] -> (a -> b x) -> b ()
recurseMonad [] _ = return ()
recurseMonad (a : as) f = (f a) >> (recurseMonad as f)

                       
setupMpvFlags :: Ptr Ctx -> [String] -> IO ()
setupMpvFlags ctx xs = recurseMonad xs (\x -> mpv_set_option_flag ctx x 1 >> return ())
  

setupMpvOptions :: Ptr Ctx -> [(String,String)] -> IO ()
setupMpvOptions ctx xs = recurseMonad xs (\(x,y) -> mpv_set_option_string ctx x y >> return ())


       

mpv_initialize ctx = check_mpv_status (c_mpv_initialize ctx)

mpv_wait_event = c_mpv_wait_event

mpv_terminate_destroy = mpv_terminate_destroy
    

--marshalls a list of strings into a c-array of c-strings and deallocates them after
--running the function
marshall_cstring_array0 :: [ String ] -> (Ptr CString -> IO x) -> IO x
marshall_cstring_array0 array func =
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
        

mpv_command :: Ptr Ctx -> [ String ] -> IO CInt                 
mpv_command ctx array =
  marshall_cstring_array0 array
    (\cstr_arr ->
       (check_mpv_status (c_mpv_command ctx cstr_arr)))

loadFiles :: Ptr Ctx -> [String] -> IO ()
loadFiles ctx xs = recurseMonad xs (\x -> mpv_command ctx ["loadfile",x] >> return ())
          

play_movie :: String -> IO ()
play_movie filename =
  do
    --lift some mpv context, since each command may return an error
    ctx <- mpv_create
    putStrLn "created context"
    mpv_set_option_string ctx "input-default-bindings" "yes"
    mpv_set_option_string ctx "input-vo-keyboard" "yes"
    mpv_set_option_flag ctx "osc" 1
    putStrLn "set options"
    putStrLn $ "ctx = " ++ (show ctx)
    mpv_initialize ctx
    putStrLn "initialized"
    mpv_command ctx ["loadfile",filename]
    putStrLn "loaded file"
    event_loop ctx
    putStrLn "finished event loop"
    mpv_terminate_destroy ctx -- this should be in some sort of failsafe (like java finally)
    return ()
  where
   event_loop :: Ptr Ctx -> IO ()
   event_loop ctx =
      do
        event <- (mpv_wait_event ctx 1) >>= peek 
        putStrLn ("mpv_wait_event: " ++ (show (event_id event)))
        time <- mpv_get_property_double ctx "time-pos"
        putStrLn ("time: " ++ (show time))
        case (event_id event) of
          id | id == mpvEventShutdown  -> return ()
          _ -> (event_loop ctx)
