module Main where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text (pack, unpack)
import Control.Monad (when)
import Text.Read (readMaybe)

main :: IO ()
main = do
    -- Initialize GTK
    _ <- Gtk.init Nothing

    -- Create a new window
    window <- new Gtk.Window [ #title := "Haskell Calculator" ]
    #setDefaultSize window 300 400

    -- Create a grid layout
    grid <- new Gtk.Grid [ #columnSpacing := 10, #rowSpacing := 10 ]

    -- Create display
    display <- new Gtk.Entry [
        #text := "0",
        #editable := False,
        #xalign := 1,
        #margin := 10
        ]

    -- Create calculator buttons
    let buttonLabels = [
            "7", "8", "9", "/",
            "4", "5", "6", "*",
            "1", "2", "3", "-",
            "C", "0", "=", "+"
            ]

    buttons <- mapM (\label -> new Gtk.Button [ #label := pack label ]) buttonLabels

    -- Attach display to grid
    #attach grid display 0 0 4 1

    -- Attach buttons to grid
    let attachButton (btn, idx) = do
            let col = idx `mod` 4
                row = 1 + idx `div` 4
            #attach grid btn col row 1 1
    mapM_ attachButton (zip buttons [0..])

    -- Set up button logic
    let onButtonClick btn = do
            label <- get btn #label
            currentText <- get display #text
            let current = unpack currentText
                lbl = unpack label
            case lbl of
                "C" -> set display [ #text := "0" ]
                "=" -> case safeEval current of
                    Just result -> set display [ #text := pack (show result) ]
                    Nothing     -> set display [ #text := "Error" ]
                _   -> if current == "0" || current == "Error"
                        then set display [ #text := pack lbl ]
                        else set display [ #text := pack (current ++ lbl) ]

    -- Connect button clicks
    mapM_ (\btn -> on btn #clicked (onButtonClick btn)) buttons

    -- Final window setup
    on window #destroy Gtk.mainQuit
    #add window grid
    #showAll window

    -- Start GTK main loop
    Gtk.main

-- Evaluator
safeEval :: String -> Maybe Double
safeEval expr = case reads expr of
    [(x, "")] -> Just x
    _ -> parseAndCalculate expr

parseAndCalculate :: String -> Maybe Double
parseAndCalculate str = do
    let parts = words str
    when (length parts /= 3) Nothing
    x <- readMaybe (parts !! 0)
    op <- case (parts !! 1) of
        "+" -> Just (+)
        "-" -> Just (-)
        "*" -> Just (*)
        "/" -> if (parts !! 2) == "0" then Nothing else Just (/)
        _   -> Nothing
    y <- readMaybe (parts !! 2)
    return (x `op` y)
