-- TODO: should there be some {-# LANGUAGE #-} ???
module L0C.EscapeColor ( escapeColorize, EscapeColor(..) )
where


data EscapeColor = Black
                | Red
                | Green
                | Yellow
                | Blue
                | Magenta
                | Cyan
                | White
                | BoldBlack
                | BoldRed
                | BoldGreen
                | BoldYellow
                | BoldBlue
                | BoldMagenta
                | BoldCyan
                | BoldWhite

boldToNormal :: EscapeColor -> EscapeColor
boldToNormal BoldBlack = Black
boldToNormal BoldRed = Red
boldToNormal BoldGreen = Green
boldToNormal BoldYellow = Yellow
boldToNormal BoldBlue = Blue
boldToNormal BoldMagenta = Magenta
boldToNormal BoldCyan = Cyan
boldToNormal BoldWhite = White
boldToNormal color = color

isBold :: EscapeColor -> Bool
isBold BoldBlack = True
isBold BoldRed = True
isBold BoldGreen = True
isBold BoldYellow = True
isBold BoldBlue = True
isBold BoldMagenta = True
isBold BoldCyan = True
isBold BoldWhite = True
isBold _ = False

getColorNum :: EscapeColor -> Int
getColorNum Black = 0
getColorNum Red = 1
getColorNum Green = 2
getColorNum Yellow = 3
getColorNum Blue = 4
getColorNum Magenta = 5
getColorNum Cyan = 6
getColorNum White = 7
getColorNum color = getColorNum $ boldToNormal color


escapeColorize :: EscapeColor -> String -> String
escapeColorize color str = "\x1B[3" ++ show (getColorNum color) ++ boldify ++ "m"
                            ++ str ++ "\x1B[m"
                        where boldify = if isBold color then ";1" else ""
