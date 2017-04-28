module Gui.GuiCore(startGui) where

--import Data.Bits
import Text.Printf (printf)
import Core(commandLine)
import Util(trim)

import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent(forkIO,forkOS)
import Control.Monad.Trans.Either (EitherT,left,right,runEitherT)
import System.Directory (doesFileExist)
import Control.Monad.Trans.Class(lift)
import Data.List.Split(splitOn)
-- import Graphics.UI.WXCore.WxcDefs
-- import Graphics.UI.WXCore.Frame
-- import Graphics.UI.WXCore.WxcClassesAL
-- import Graphics.UI.WXCore.WxcClassesMZ
-- import Graphics.UI.WXCore.WxcTypes

startGui :: IO ()
startGui = start gui


gui :: IO ()
gui = do
    -- hbox <- boxSizerCreate wxVERTICAL
    f <- frame [text := "Mpv Lang Learn"]
    p <- panel f []
    nb <- notebook p []
    sp <- panel nb [] --needed because notebook needs a container for each page
    s <- simple f sp

    ap <- panel nb [] --needed because notebook needs a container for each page
    a <- advanced f ap

    --s <- advanced f f

    -- ap <- panel ab []
    -- a <- advanced f px
    --t <- staticText p1 [text := "Hello World"]
    --    set p [layout := fill $ container p $ margin 10 $ widget t ]
    --return p

    set f [layout :=
             container p $
             column 0
              [ tabs nb
                [
                  tab "Simple" $ container sp $ fill $ widget s,
                  tab "Advanced" $ container ap $ fill $ widget a
                ]
              ],

            clientSize := sz 500 550 ]

              -- t <- staticText p [text := "Hello World"]    
     -- quitButton <- button window [text := "Quit", on command := close window]
    -- exitButton <- button window [text := "Exit", on command := close window]
    -- windowSetSizer window hbox
    -- sizerAddWindow hbox exitButton 1 (wxEXPAND .|. wxALL) 5 ptrNull
    -- sizerAddWindow hbox quitButton 1 (wxEXPAND .|. wxALL) 5 ptrNull
    -- frameCenter window
    return ()

videoFiles
   = [("Video files",["*.mkv","*.mp4","*.avi","*.wmv","*.flv","*.mov"])
     ,("All files",["*.*"])
     ]

srtFiles
   = [("Srt files",["*.srt"])
     ,("All files",["*.*"])
     ]


simple :: Frame x -> Window a -> IO (Panel ())
simple f w =
  do
    p <- panel w []
--    logo <- bitmapButton w "image_barnyard/logo1.png" []


    logop <- panel p []
    bm <- bitmapCreateFromFile "logo.png"
    bmsize <- get bm size
    logo <- panel logop [ on paint := onPaint bm]
    -- f <- fakeout p
    -- set p [ layout := margin 10 $ widget f]

    videoField <- labeledField p "Video" "Video to play" (fileChooser "Open Video" videoFiles)
    -- set p [ layout := margin 10 $ column 4 [ fill $ widget $ fieldPanel videoField] ]
    --(widget (fieldPanel videoField))

    
    nativeLangSrtField <- labeledField p "Native Srt file" "Subtitle file in the native language of the user, srt format" (fileChooser "Open Native Srt" srtFiles)
    foreignLangSrtField <- labeledField p "Foreign Srt file" "Subtitle file in the language of the movie, srt format (optional)" (fileChooser "Open Foreign Srt" srtFiles)
    
    blurb <- staticText p [ fontWeight := WeightBold, text := "* When video is playing, subtitle delay can be adjusted with 'z' and 'x' keys.\nFor more information, see the documentation for mpv"]

    eop <- exitOkErrorPanel f p
           (runSimple videoField nativeLangSrtField foreignLangSrtField)
    set p [layout := margin 10 $ column 4 $
            [fill $ container logop $ floatCentre  $ minsize bmsize $ widget logo ] ++
        (fmap (hfill . widget . fieldPanel) [videoField, nativeLangSrtField,
                                             foreignLangSrtField]) ++
        [hfill $ space 20 1, hfill $ widget blurb, fill $ widget eop]]
        
          
    return p
  where
    speeds = [ "1.5","1.25","1","0.9","0.75","0.5" ]
    speedDefault = 2
    onPaint bm dc viewArea
      = drawBitmap dc bm pointZero False []
    
    
advanced :: Frame x -> Window a -> IO (Panel ())
advanced f w =
  do
    p <- panel w []
    -- f <- fakeout p
    -- set p [ layout := margin 10 $ widget f]

    videoField <- labeledField p "Video" "Video to play" (fileChooser "Open Video" videoFiles)
    -- set p [ layout := margin 10 $ column 4 [ fill $ widget $ fieldPanel videoField] ]
    --(widget (fieldPanel videoField))

    
    nativeLangSrtField <- labeledField p "Native Srt file" "Subtitle file in the native language of the user, srt format" (fileChooser "Open Native Srt" srtFiles)
    foreignLangSrtField <- labeledField p "Foreign Srt file" "Subtitle file in the language of the movie, srt format (optional)" (fileChooser "Open Foreign Srt" srtFiles)

    
    speed0Field <- labeledField p "Speed of Loop 0" "Playback speed of loop without subtitles"
      speedChoice
    loop0  <- checkBox p [text := "Show loop of video without subtitles", checked := True,
                         on command := set (fieldPanel speed0Field) [enabled :~ not]
                         ]
                   
    speed1Field <- labeledField p "Speed of Loop 1" "Playback speed of Foreign Srt loop"
      speedChoice
                   
    speed2Field <- labeledField p "Speed of Loop 2" "Playback speed of Native Srt loop"
      speedChoice

    leadingSecsField <- labeledField p "Leading secs" "Padding before each loop" (textField "1.0")
                   
    trailingSecsField <- labeledField p "Trailing secs" "Padding after each loop" (textField "1.0")

    addMpvOptionsField <- labeledField p "Additional Mpv Options" "Options to pass directly to the mpv video player (options with values must be passed as\n\"-name=val\", not \"-name val\")" (textField "")

                   
    eop <- exitOkErrorPanel f p
        (runAdvanced videoField nativeLangSrtField foreignLangSrtField speed0Field speed1Field speed2Field leadingSecsField trailingSecsField addMpvOptionsField loop0)
    set p [layout := margin 10 $ column 4 $
        (fmap (hfill . widget . fieldPanel) [videoField, nativeLangSrtField,
                                             foreignLangSrtField]) ++
        [hfill . widget $ loop0,
         hfill (row 3 (fmap (hfill . widget . fieldPanel) [speed0Field, speed1Field, speed2Field])),
         hfill (row 3 $ (fmap (hfill . widget . fieldPanel) [leadingSecsField, trailingSecsField]) ++ [ hfill $ space 1 1 ]),
         hfill . widget . fieldPanel $ addMpvOptionsField, fill $ widget eop]]
        
          
    return p
  where
    speeds = [ "1.5","1.25","1","0.9","0.75","0.5" ]
    speedDefault = 2

    speedChoice w =
      do
        p <- panel w []
        c <- choice p [ sorted  := False, items := speeds,
                        selection := speedDefault]
        return $ Field p
          (do
              sel <- get c selection
              return $ speeds !! sel)

-- advanced :: Frame x -> Window a -> IO (Panel ())
-- advanced f w =
--   do
--     p <- panel w []
--     track1CB <- checkBox p [ text := "Enable Empty Loop" ]
--     track1SpeedText <- staticText p [ text := "Speed x" ]
--     track1Speed <- speedDropDown p

--     set p [ layout := hfill $ row 2 [ hfill $ widget track1CB, widget track1Speed] ]
--     return p




exitOkErrorPanel :: Frame x -> Panel () -> ((String -> IO ()) -> IO ()) -> IO (Panel ())
exitOkErrorPanel f w action =
  do
    p <- panel w []
    errorMsgField <- staticText p [ color := red, fontWeight := WeightBold ]
    exitButton <- button p [ text := "Exit", on command := close f]
    okButton <- button p [ text := "OK"]

    set okButton  [on command := do
                      set errorMsgField [ text := "" ]
                      set f [
                        --we do this "on idle" trick, so the button gets unpressed
                        --when the action (playing a movie most likely) is executed
                        on idle :=
                              do
                                action (\msg -> set errorMsgField [ text := msg ])
                                set f [ on idle := return False ]
                                return False
                            ]
                  ]
    set p [ layout := column 3 [ hfill $ space 20 1, fill $ widget $ errorMsgField ,hfill $ row 2 [hfill $ widget exitButton, hfill $ widget okButton ] ]
          ]
    return p


-- advanced :: Frame x -> Window a -> IO (Panel ())
-- advanced f w =
--   do
--     p <- panel w []
--     track1CB <- checkBox p [ text := "Enable Empty Loop" ]
--     track1SpeedText <- staticText p [ text := "Speed x" ]
--     track1Speed <- speedDropDown p

--     set p [ layout := hfill $ row 2 [ hfill $ widget track1CB, widget track1Speed] ]
--     return p


--   where
--     speeds = [ "1.5","1.25","1","0.9","0.75","0.5" ]
--     speedDefault = 2
--     speedDropDown p = choice p [ sorted  := False, items := speeds, selection := speedDefault]
--     trackWidget w widget =
--       do
--         p <- panel w []
--         speedText <- staticText p [ text := "Speed x" ]
--         speedChoice <- speedDropDown p

--         set p [ layout := hfill $ row 2 [ hfill $ widget track1CB, widget track1Speed] ]
        
--     -- track2SrtField <- labeledField p "Native Srt file" "Subtitle file in the native language of the user, srt format" (fileChooser "Open Native Srt" srtFiles)
--     -- foreignLangSrtField <- labeledField p "Foreign Srt file" "Subtitle file in the language of the movie, srt format (optional)" (fileChooser "Open Foreign Srt" srtFiles)

--     -- errorMsgField <- staticText p [ color := red, fontWeight := WeightBold ]

--     exitButton <- button p [ text := "Exit", on command := close f]
--     okButton <- button p [ text := "OK"]

--     set okButton  [on command := do
--                       set f [ on idle :=
--                               do
--                                 runSimple videoField nativeLangSrtField foreignLangSrtField (\msg -> set errorMsgField [ text := msg ])
--                                 set f [ on idle := return False ]
--                                 return False
--                             ]
--                   ]
--     set p [layout := margin 10 $ column 4 $
--         (fmap (hfill. widget . fieldPanel) [videoField, nativeLangSrtField, foreignLangSrtField]) ++
--           [hfill $ space 20 1, fill $ floatCentre $ widget $ errorMsgField ,hfill $ row 2 [hfill $ widget exitButton, hfill $ widget okButton]]
--         ]
          
--     return p


runAdvanced :: Field String -> Field String -> Field String -> Field String -> Field String -> Field String -> Field String -> Field String -> Field String -> (CheckBox ()) -> (String -> IO ()) ->  IO ()
runAdvanced vf nlf flf s0f s1f s2f lsf tsf mpvf l0 errorOut =
  do
      video <- (fieldVal vf)
      nlSrt <-  (fieldVal nlf)
      flSrt <-  (fieldVal flf)
      speed0 <- (fieldVal s0f)
      speed1 <- (fieldVal s1f)
      speed2 <- (fieldVal s2f)
      leadingSecs <- (fieldVal lsf)
      trailingSecs <- (fieldVal tsf)
      mpvOptsStr <- (fieldVal mpvf)
      enableEmptyLoop <- get l0 checked

      let mpvOpts = splitMpvOpts mpvOptsStr

      ca <- runEitherT
        (do
          checkSimpleArgs  (trim video) (trim nlSrt) (trim flSrt)
          if (flSrt == "") && (not enableEmptyLoop) then
            left "If foreign langauge srt is blank, then 'Show loop of video without subtitles' must be checked"
            else return ()
          mapM (\a -> case a of
                        '-' : xs -> return ()
                        _ -> left "All mpv args must start with a '-'. Args contains spaces may be quoted, ex '-foo=abc def'") mpvOpts
          return ()
        )
  
      
      case ca of 
        Left msg -> errorOut msg
        Right () ->
          do
            if (flSrt /= "") then
              commandLine $ ([flSrt,nlSrt,"--"]++
                            (if enableEmptyLoop then 
                               [trackText "none" speed0 leadingSecs trailingSecs]
                              else [])++
                            [trackText "1" speed1 leadingSecs trailingSecs,
                              trackText "2" speed2 leadingSecs trailingSecs,
                              "--"]++mpvOpts++[video])
              else commandLine $ [nlSrt,"--",
                                trackText "none" speed0 leadingSecs trailingSecs,
                                trackText "1" speed1 leadingSecs trailingSecs,"--"]++mpvOpts++[video]
            return ()
      return ()
  where
    trackText :: String -> String -> String-> String-> String
    trackText trk speed ls ts = trk ++ ":" ++ speed ++ ":" ++ ls ++ ":" ++ ts
    splitMpvOpts :: String -> [String]
    splitMpvOpts [] = []
    splitMpvOpts ('\'' : xs) =
                  let (val,rest) = readUntil '\'' xs
                  in
                    val : splitMpvOpts rest
    splitMpvOpts ('"' : xs) =
                  let (val,rest) = readUntil '"' xs
                  in
                    val : splitMpvOpts rest
    splitMpvOpts (' ' : xs) = splitMpvOpts xs
    splitMpvOpts xs =
      let (val, rest) = readUntil ' ' xs
      in val : splitMpvOpts rest

    readUntil :: Char -> String -> (String, String)
    readUntil c [] = ([],[])
    readUntil c (x : xs) | x == c = ([], xs)
                         | otherwise = let (rv, rr) = readUntil c xs
                                       in (x:rv, rr)

  
runSimple :: Field String -> Field String -> Field String -> (String -> IO ()) ->  IO ()
runSimple vf nlf flf errorOut =
    do
      video <- (fieldVal vf)
      nlSrt <-  (fieldVal nlf)
      flSrt <-  (fieldVal flf)

      ca <- runEitherT (checkSimpleArgs  (trim video) (trim nlSrt) (trim flSrt))
      case ca of 
        Left msg -> errorOut msg
        Right () ->
          do
            if (flSrt /= "") then
              commandLine [flSrt,nlSrt,"--",
                           trackText "none" "1",
                           trackText "1" "1",
                           trackText "2" "1",
                           "--",video]
              else
              commandLine [nlSrt,"--",
                           trackText "none" "1",
                           trackText "1" "1","--",video]
            return ()
      return ()
  where
    trackText :: String -> String -> String
    trackText trk speed = trk ++ ":" ++ speed ++ ":1:1"
    
checkSimpleArgs :: String -> String -> String -> EitherT String IO ()
checkSimpleArgs video nlSrt flSrt =
  do
    guardE "Video field can't be blank" (video /= "")
    guardE "Native Srt can't be blank"  (nlSrt /= "")
    (lift . doesFileExist) video >>= (guardE (printf "'%s' doesn't exist" video))
    (lift . doesFileExist) nlSrt >>= guardE (printf "'%s' doesn't exist" nlSrt)
    if (flSrt /= "") then
      (lift . doesFileExist) nlSrt >>= guardE (printf "'%s' doesn't exist" flSrt)
      else
      return ()

guardE :: Monad m => String -> Bool -> EitherT String m ()
guardE msg False = left msg
guardE _ True = right ()


data Field a = Field { fieldPanel :: Panel (), fieldVal :: IO a }

fakeout :: Panel () -> IO (Panel ())
fakeout w = do
               p <- panel w []
               set p [layout := fill $ boxed "fakeout!" $ (space 1 1)]
               return p

labeledField :: Window a -> String -> String -> (Panel () -> IO (Field x)) -> IO (Field x)
labeledField w name desc ff =
  do
    p <- panel w []
    n <- staticText p [text := name]
    d <- staticText p [text := desc]
    field <- ff p
    set n [ fontWeight := WeightBold ]
    set p [ layout := column 3 [ hfill $ widget n, hfill $ widget d, hfill $ widget
       (fieldPanel field)
        ] ]
    return $ field { fieldPanel = p }



fileChooser :: String -> [(String,[String])] -> Panel () -> IO (Field String)
fileChooser title fileTypes w = 
    do
      p <- panel w []
      i <- textEntry p []
      b <- button p [ text := "Browse"]

      set b [on command :=
                 do
                   maybeFile <- fileOpenDialog w False {- change current directory -}
                                               True title fileTypes "" ""
                   case maybeFile of
                      Nothing -> return ()
                      Just f -> set i [text := f] >> return ()
            ]

      set p [layout := hfill $ row 2 [ hfill $ widget i, space 10 1, widget b]]           
                      

      return $ Field p (get i text)


textField :: String -> Panel () -> IO (Field String)
textField def w = 
    do
      p <- panel w []
      i <- textEntry p [ text := def]

      set p [layout := hfill $ row 2 [ hfill $ widget i]]           

      return $ Field p (get i text)
      

-- headerPanel = do
--     p1   <- panel  nb []
--        ok   <- button p1 [text := "Ok", on command := logMessage "ok button pressed"]
--        quit <- button p1 [text := "Quit", on command := close f]

