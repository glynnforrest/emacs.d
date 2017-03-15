-- Config { font = "xft:Terminus:pixelsize=12"
 Config { font = "xft:Bitstream Vera Sans Mono:size=10:antialias=true"
        , bgColor = "#defae5"
        , fgColor = "#001110"
        -- , position = Static { xpos = 0 , ypos = 0, width = 1365, height = 20 }
        , position = Top
        , lowerOnStart = False
        , commands = 	[ Run StdinReader
                        , Run Memory ["-t","Memory: <usedratio>%"] 30
                        , Run Battery 	[ "-t", "Battery: <left>%"
                                        -- , "-L", "10", "-H", "50", "-h", "green", "-l", "red"] 50
                                        , "-L", "10", "-H", "50"] 50
                        , Run Com "~/.dotfiles/bin/fuzzy-date-time.py" [] "date" 50
                        , Run Com "~/.dotfiles/bin/volume.sh" [] "volume" 10
                        , Run Com "echo $(xbacklight -get | cut -d . -f 1)%" [] "light" 10
                        ]
        , sepChar = "%"
        , alignSep = "}{"
        -- , template = "%StdinReader% }{ Volume: %volume% <fc=#4e56e4>>></fc> %battery% <fc=#4e56e4>>></fc> Light: %light% <fc=#4e56e4>>></fc> %memory% <fc=#4e56e4>>></fc> %date% "
        , template = "%StdinReader% }{ Volume: %volume% <fc=#4e56e4>>></fc> %battery% <fc=#4e56e4>>></fc> %date% "
        -- , template = "%StdinReader% }{ %date% "
        }
