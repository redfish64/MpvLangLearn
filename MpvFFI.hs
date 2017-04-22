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
              MFM,
              MpvFFIEnv(..),
              Ctx,
              event_id)
              where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import MpvStructs
import Text.Printf
import Control.Exception
--import Data.Typeable
import Util
import Control.Monad.Reader
import Data.Either

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
          | CMpvSetPropertyString | CMpvSetProperty
          | CMpvCommand | CSetMultipleSubfiles | CMpvWaitEvent
          | CMpvGetProperty | CMpvSetOption
          deriving (Show,Eq)

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


mpvCreate :: MFM Ctx
mpvCreate =
  lift $ throw_mpve_on c_mpv_create $ (\v -> if v == nullPtr then Just "NPE when calling mpv_create" else Nothing)

handleError :: Call -> CInt -> MFM MpvError
handleError call ec =
  do
    let err = MpvError ec
    if ec < 0 then
      do
        env <- ask
        errorFunc env call err
        return err
    else
        return err

withCStringCString :: String -> String -> (CString -> CString -> IO x) -> IO x
withCStringCString a b f=
         withCString a
             (\ca ->
                     withCString b
                          (\cb ->
                             f ca cb))

mpvSetOptionString :: Ctx -> String -> String -> MFM MpvError
mpvSetOptionString ctx name value =
  do
    error <- lift $
      withCStringCString name value (c_mpv_set_option_string ctx)
    handleError CMpvSetOptionString error
              
              
mpvSetOptionFlag :: Ctx -> String -> Int -> MFM MpvError
mpvSetOptionFlag ctx name v =
  do
    error <- lift $ withCString name
       (\cname ->
          alloca
            ((\value ->
               do
                 poke value (fromIntegral v)
                 voidvalue <- return (castPtr value)
                 --3 == MPV_FORMAT_FLAG TODO: put in enum
                 c_mpv_set_option ctx cname 3 voidvalue) :: (Ptr CInt -> IO CInt))
            
       )
    handleError CMpvSetOption error
       


mpvSetOptionDouble :: Ctx -> String -> Double -> MFM MpvError
mpvSetOptionDouble ctx name v =
  do
    error <- lift $ withCString name
       (\cname ->
          alloca
            ((\value ->
               do
                 poke value (realToFrac v)
                 voidvalue <- return (castPtr value)
                 --5 == MPV_FORMAT_DOUBLE TODO: put in enum
                 c_mpv_set_option ctx cname 5 voidvalue) :: (Ptr CDouble -> IO CInt))
            
       )
    handleError CMpvSetOption error

mpvSetPropertyDouble :: Ctx -> String -> Double -> MFM MpvError
mpvSetPropertyDouble ctx name v =
  do
    error <- lift $ withCString name
       (\cname ->
          alloca
            ((\value ->
               do
                 poke value (realToFrac v)
                 voidvalue <- return (castPtr value)
                 --5 == MPV_FORMAT_DOUBLE TODO: put in enum
                 c_mpv_set_property ctx cname 5 voidvalue) :: (Ptr CDouble -> IO CInt))
            
       )
    handleError CMpvSetProperty error

mpvSetPropertyString :: Ctx -> String -> String -> MFM MpvError
mpvSetPropertyString ctx name value =
  do
    --putStrLn $ "mpv_set_property_string "++name ++ " " ++ value
    error <- lift $ withCString name
       (\cname ->
         withCString value
           (\cvalue ->
              c_mpv_set_property_string ctx cname cvalue))
    handleError CMpvSetPropertyString error


--gets a property
--Ex. "time-pos"  position in current file in seconds
mpvGetPropertyDouble :: Ctx -> String -> MFM (Either MpvError Double)
mpvGetPropertyDouble ctx name =
  do
    errorOrResult <- lift $ withCString name
       (\cname ->
          alloca
            ((\value ->
               do
                 voidvalue <- return (castPtr value)
                 --TODO: put in enum (5 == MPV_FORMAT_DOUBLE)
                 status <- c_mpv_get_property ctx cname 5 voidvalue
                 if status < 0
                 then
                   return $ Left status
                 else
                   ((peek value) >>= return . Right)
             ) :: (Ptr CDouble -> IO (Either CInt CDouble)))
            
       )
    case errorOrResult of
         Left error -> handleError CMpvGetProperty (fromIntegral error)
                         >>= (return . Left)
         Right res -> return $ Right (realToFrac res)
                       
setupMpvFlags :: Ctx -> [String] -> MFM ()
setupMpvFlags ctx xs = recurseMonad xs (\x -> mpvSetOptionFlag ctx x 1 >> return ())
  

setupMpvOptions :: Ctx -> [(String,String)] -> MFM ()
setupMpvOptions ctx xs = recurseMonad xs (\(x,y) -> mpvSetOptionString ctx x y >> return ())


       

mpvInitialize :: Ctx -> MFM MpvError
mpvInitialize ctx = lift (c_mpv_initialize ctx) >>= handleError CMpvInitialize 

mpvWaitEvent :: Ctx -> Double -> MFM (Ptr MpvEvent)
mpvWaitEvent ctx waitTime = lift $ c_mpv_wait_event ctx (realToFrac waitTime)

mpvTerminateDestroy :: Ctx -> MFM ()
mpvTerminateDestroy ctx = lift $ c_mpv_terminate_destroy ctx
    

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
        

mpvCommand :: Ctx -> [ String ] -> MFM MpvError            
mpvCommand ctx array =
  do
    error <- lift $ marshallCstringArray0 array
                      (\cstr_arr ->
       do
         putStrLn $ "mpv_command: "++ show array
         c_mpv_command ctx cstr_arr)
    handleError CMpvCommand error

setMultipleSubfiles :: Ctx -> [ String ] -> MFM MpvError
setMultipleSubfiles ctx array =
  do
    error <- lift $ marshallCstringArray0 array --TODO nullptr at end not needed
      (\cstr_arr ->
       do
         putStrLn $ "set_multiple_subfiles: "++ show array
         c_set_multiple_subfiles ctx (fromIntegral (length array)) cstr_arr)
    handleError CSetMultipleSubfiles error

loadFiles :: Ctx -> [String] -> MFM ()
loadFiles ctx xs = recurseMonad xs (\x -> mpvCommand ctx ["loadfile",x] >> return ())
          

playMovie :: String -> IO ()
playMovie filename =
  runReaderT playMovie1 (MpvFFIEnv errorFunc)
  where
    playMovie1 :: MFM ()
    playMovie1 =
      do
        --lift some mpv context, since each command may return an error
        ctx <- mpvCreate
        lift $ putStrLn "created context"
        mpvSetOptionString ctx "input-default-bindings" "yes"
        mpvSetOptionString ctx "input-vo-keyboard" "yes"
        mpvSetOptionFlag ctx "osc" 1
        lift $ putStrLn "set options"
        lift $ putStrLn $ "ctx = " ++ (show ctx)
        mpvInitialize ctx
        lift $ putStrLn "initialized"
        mpvCommand ctx ["loadfile",filename]
        lift $ putStrLn "loaded file"
        eventLoop ctx
        lift $ putStrLn "finished event loop"
        mpvTerminateDestroy ctx -- this should be in some sort of failsafe (like java finally)
        return ()
    errorFunc :: Call -> MpvError -> MFM ()
    errorFunc call mpvError = lift $ putStrLn $
      printf "Error: call %s, status %s" (show call) (show mpvError)
                   
    eventLoop :: Ctx -> MFM ()
    eventLoop ctx =
      do
        event <- (mpvWaitEvent ctx 1) >>= (lift . peek )
        lift $ putStrLn ("mpv_wait_event: " ++ (show (event_id event)))
        time <- mpvGetPropertyDouble ctx "time-pos"
        lift $ putStrLn ("time: " ++ (show time))
        lift $ putStrLn ("event id: " ++ (show $ event_id event))
        case (event_id event) of
          id | id == mpvEventShutdown  -> return ()
          _ -> (eventLoop ctx)
