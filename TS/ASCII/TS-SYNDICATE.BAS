'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-SYNDICATE.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "TS-SYNDICATE"
Cls
Screen 12
Color 8
Locate 13, 14: Print "                       __ __            __"
Locate 14, 14: Print " .-----.--.--.-----.--|  |__.----.---.-|  |_.-----."
Locate 15, 14: Print " |__ --|  |  |     |  _  |  |  __|  _  |   _|  -__|"
Locate 16, 14: Print " |_____|___  |__|__|_____|__|____|___._|____|_____|"
Locate 17, 14: Print " -ts---|_____|------------- s y n d i c a t e . c f"
Color 4: Locate 13, 40: Print "__"
Do
    Color 7
    Locate 13, 14: Print "                       __ __            __"
    Locate 14, 14: Print " .-----.--.--.-----.--|  |__.----.---.-|  |_.-----."
    Locate 15, 14: Print " |__ --|  |  |     |  _  |  |  __|  _  |   _|  -__|"
    Locate 16, 14: Print " |_____|___  |__|__|_____|__|____|___._|____|_____|"
    Locate 17, 14: Print " -ts---|_____|------------- s y n d i c a t e . c f"
    Color 4: Locate 13, 40: Print "__"
    _Delay 0.1
    Color 15
    Locate 13, 14: Print "                       __ __            __"
    Locate 14, 14: Print " .-----.--.--.-----.--|  |__.----.---.-|  |_.-----."
    Locate 15, 14: Print " |__ --|  |  |     |  _  |  |  __|  _  |   _|  -__|"
    Locate 16, 14: Print " |_____|___  |__|__|_____|__|____|___._|____|_____|"
    Locate 17, 14: Print " -ts---|_____|------------- s y n d i c a t e . c f"
    Color 12: Locate 13, 40: Print "__"
    _Delay 0.1
    Color 7
    Locate 13, 14: Print "                       __ __            __"
    Locate 14, 14: Print " .-----.--.--.-----.--|  |__.----.---.-|  |_.-----."
    Locate 15, 14: Print " |__ --|  |  |     |  _  |  |  __|  _  |   _|  -__|"
    Locate 16, 14: Print " |_____|___  |__|__|_____|__|____|___._|____|_____|"
    Locate 17, 14: Print " -ts---|_____|------------- s y n d i c a t e . c f"
    Color 4: Locate 13, 40: Print "__"
    _Delay 0.1
    Color 8

    Locate 13, 14: Print "                       __ __            __"
    Locate 14, 14: Print " .-----.--.--.-----.--|  |__.----.---.-|  |_.-----."
    Locate 15, 14: Print " |__ --|  |  |     |  _  |  |  __|  _  |   _|  -__|"
    Locate 16, 14: Print " |_____|___  |__|__|_____|__|____|___._|____|_____|"
    Locate 17, 14: Print " -ts---|_____|------------- s y n d i c a t e . c f"
    Color 12: Locate 13, 40: Print "__"
    _Delay 0.1

Loop While InKey$ = ""
Color 15
Cls
System
