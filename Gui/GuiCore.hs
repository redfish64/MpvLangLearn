module Gui.GuiCore(startGui) where

import Data.Bits
import Text.Printf (printf)
import Core(commandLine)

import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent(forkIO,forkOS)
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
    px <- panel nb []
    --    s <- fakeout px
    s <- simple f px
    --t <- staticText p1 [text := "Hello World"]
    --    set p [layout := fill $ container p $ margin 10 $ widget t ]
    --return p

    set f [layout :=
             container p $
             column 0
              [ tabs nb
                [ tab "Simple" $ container px $ fill $ widget s ] ],

            clientSize := sz 400 350 ]

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
    -- f <- fakeout p
    -- set p [ layout := margin 10 $ widget f]

    videoField <- labeledField p "Video" "Video to play" (fileChooser "Open Video" videoFiles)
    -- set p [ layout := margin 10 $ column 4 [ fill $ widget $ fieldPanel videoField] ]
    --(widget (fieldPanel videoField))

    
    nativeLangSrtField <- labeledField p "Native Srt file" "Subtitle file in the native language of the user (srt format)" (fileChooser "Open Native Srt" srtFiles)
    foreignLangSrtField <- labeledField p "Foreign Srt file" "Subtitle file in the language of the movie, optional (srt format)" (fileChooser "Open Foreign Srt" srtFiles)

    okButton <- button p [ text := "OK"]

    set okButton  [on command := do
                      set f [ on idle :=
                              do
                                runSimple videoField nativeLangSrtField foreignLangSrtField
                                set f [ on idle := return False ]
                                return False
                            ]
                  ]
    set p [layout := margin 10 $ column 4 $
        (fmap (hfill. widget . fieldPanel) [videoField, nativeLangSrtField, foreignLangSrtField]) ++
          [fill $ space 1 30,hfill $ alignBottomRight $ widget okButton]
        ]
          
    return p

runSimple :: Field String -> Field String -> Field String -> IO ()
runSimple vf nlf flf =
    do
      video <- (fieldVal vf)
      nlSrt <-  (fieldVal nlf)
      flSrt <-  (fieldVal flf)
      putStrLn (printf "runSimple: %s %s %s" video nlSrt flSrt)
      --forkIO $
      commandLine [flSrt,nlSrt,"--","none:1:1:1","1:1:1:1","2:1:1:1","--",video]
      return ()
      


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



fileChooser :: String -> [(String,[String])] -> Panel () -> IO (Field String )
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
    

-- headerPanel = do
--     p1   <- panel  nb []
--        ok   <- button p1 [text := "Ok", on command := logMessage "ok button pressed"]
--        quit <- button p1 [text := "Quit", on command := close f]

