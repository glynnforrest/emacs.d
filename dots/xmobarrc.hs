Config { font = "xft:Terminus:pixelsize=11"
        , bgColor = "black"
        , fgColor = "#5f9ea0"
        , position = TopW L 100
        , lowerOnStart = False
        , commands = 	[ Run StdinReader
                        , Run Memory ["-t","Memory: <usedratio>%"] 30
                        , Run Battery 	[ "-t", "Battery: <left>%"
                                        , "-L", "10", "-H", "50", "-h", "green", "-l", "red"] 50
                        , Run Com "~/.dotfiles/bin/fuzzy-date-time.py" [] "date" 50
                        , Run Com "~/.dotfiles/bin/volume.sh" [] "volume" 10
                        , Run Com "echo $(xbacklight -get | cut -d . -f 1)%" [] "light" 10
                        ]
        , sepChar = "%"
        , alignSep = "}{"
        , template = "%StdinReader% }{ Volume: %volume% <fc=#FFFFFF>>></fc> %battery% <fc=#FFFFFF>>></fc> Light: %light% <fc=#FFFFFF>>></fc> %memory% <fc=#FFFFFF>>></fc> %date% "
        }
