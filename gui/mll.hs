module Main where

import Data.Bits

import Graphics.UI.WX
import Graphics.UI.WXCore
-- import Graphics.UI.WXCore.WxcDefs
-- import Graphics.UI.WXCore.Frame
-- import Graphics.UI.WXCore.WxcClassesAL
-- import Graphics.UI.WXCore.WxcClassesMZ
-- import Graphics.UI.WXCore.WxcTypes

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    -- hbox <- boxSizerCreate wxVERTICAL

    f <- frame [text := "Mpv Lang Learn"]
    p <- panel f []
    nb <- notebook p []
    px <- panel nb []
    s <- simple px
    --t <- staticText p1 [text := "Hello World"]
    --    set p [layout := fill $ container p $ margin 10 $ widget t ]
    --return p

    set f [layout :=
             container p $
             column 0
              [ tabs nb
                [ tab "Simple" $ stretch $ container px $ widget s ] ],

            clientSize := sz 400 300 ]

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


simple :: Window a -> IO (Panel ())
simple w =
  do
    p <- panel w []
    videoField <- labeledField p "Video" "Video to play" (fileChooser "Open Video" videoFiles)
    nativeLangSrtField <- labeledField p "Native Srt file" "Subtitle file in the native language of the user (srt format)" (fileChooser "Open Native Srt" srtFiles)
    foreignLangSrtField <- labeledField p "Foreign Srt file" "Subtitle file in the language of the movie, optional (srt format)" (fileChooser "Open Foreign Srt" srtFiles)

    okButton <- button p [ text := "O-Kay!" ]
    cancelButton <- button p [ text := "Not Want" ]

    set p [layout := margin 10 $ column 4 $
        (fmap (widget . fieldPanel) [videoField, nativeLangSrtField, foreignLangSrtField]) ++
          [space 1 30,alignBottomRight $ row 3 [widget cancelButton,  space 10 1, widget okButton]]
        ]
          
    return p


data Field a = Field { fieldPanel :: Panel (), fieldVal :: IO a }

labeledField :: Window a -> String -> String -> (Panel () -> IO (Field x)) -> IO (Field x)
labeledField w name desc ff =
  do
    p <- panel w []
    n <- staticText p [text := name]
    d <- staticText p [text := desc]
    field <- ff p
    set n [ fontWeight := WeightBold ]
    set p [ layout := column 3 [ widget n, widget d, widget (fieldPanel field) ] ]
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

      set p [layout := hfill $ row 2 [ widget i, space 10 1, widget b]]           
                      

      return $ Field p (get i text)
    

-- headerPanel = do
--     p1   <- panel  nb []
--        ok   <- button p1 [text := "Ok", on command := logMessage "ok button pressed"]
--        quit <- button p1 [text := "Quit", on command := close f]

