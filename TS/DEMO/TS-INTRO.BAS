'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-INTRO.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "TS-INTRO"
Cls
Screen 13
'LOCATE 2, 1: COLOR 23: PRINT "ANYKEY = Quit     ALT+ENTER = Fullscreen"
Locate 25, 1: Color 23: Print "(c)2019            thomas.schilb@live.de"
Dim WilliamTell& ' handle to hold sound file
WilliamTell& = _SndOpen("ts-intro.mp3") '        load MP3 file into RAM
_SndLoop WilliamTell&
'_SNDPLAY WilliamTell& '                                   play MP3 file from RAM
'_SNDPLAYFILE "since.mid"
'
' TSC ASCII
'
For i% = 16 To 31
    Color i%
    Locate 11, 5: Print "   ________ ________.________   "
    Locate 12, 5: Print "       |    |______ |           "
    Locate 13, 5: Print "       |    ______| |______     "
    _Delay 0.1
Next i%
For iii% = 16 To 31
    Color iii%
    Locate 15, 5: Print "   -  code. design. music.  -"
    _Delay 0.1
Next iii%

For iiii% = 31 To 16 Step -1
    Color iiii%
    Locate 15, 5: Print "   -  code. design. music.  -"
    _Delay 0.1
Next iiii%

For ii% = 31 To 16 Step -1

    Color ii%
    Locate 11, 5: Print "   ________ ________.________   "
    Locate 12, 5: Print "       |    |______ |           "
    Locate 13, 5: Print "       |    ______| |______     "
    _Delay 0.1
Next ii%
Locate 25, 1: Color 0: Print "(c)2012-2017       thomas.schilb@live.de"
'
' 3d wire cube
'
Cls
Dim a%(500, 2), b&(8000)
Locate 2, 1: Color 23: Print "ANYKEY = Quit            thomasschilb.de"
Locate 25, 1: Color 23: Print "(c)2019           thomas.schilb@live.de"
Locate 20, 18: Color 15: Print ".t.s.c."
Def Seg = VarSeg(b&(0))
For i% = 0 To 1499
    a%(i% \ 3 + 1, i% Mod 3) = (Int(Rnd * 2) * 2 - 1) * Int(50 * Cos(Rnd * 1.570794))
Next i%

Do
    _Delay 0.01
    Get (112, 52)-(209, 149), b&()
    For i& = VarPtr(b&(1)) To VarPtr(b&(1)) + 9603
        Poke i&, (Peek(i&) - 1) * -(Peek(i&) > 17)
    Next i&
    Put (112, 52), b&(), PSet
    z% = (z% + 1) Mod 576
    For i% = 1 To 500
        nx = (a%(i%, 0) * Cos(z% / 91.67325) - a%(i%, 1) * Sin(z% / 91.67325)) * Cos(z% / 30.55775) - a%(i%, 2) * Sin(z% / 30.55775)
        ny = (a%(i%, 0) * Sin(z% / 91.67325) + a%(i%, 1) * Cos(z% / 91.67325)) * Cos(z% / 45.83662) - ((a%(i%, 0) * Cos(z% / 91.67325) - a%(i%, 1) * Sin(z% / 91.67325)) * Sin(z% / 30.55775) + a%(i%, 2) * Cos(z% / 30.55775)) * Sin(z% / 45.83662)
        nz = (a%(i%, 0) * Sin(z% / 91.67325) + a%(i%, 1) * Cos(z% / 91.67325)) * Sin(z% / 45.83662) + ((a%(i%, 0) * Cos(z% / 91.67325) - a%(i%, 1) * Sin(z% / 91.67325)) * Sin(z% / 30.55775) + a%(i%, 2) * Cos(z% / 30.55775)) * Cos(z% / 45.83662)
        If Point(160 + nx * 200 / (ny + 400), 100 - nz * 200 / (ny + 400)) < 31 - (ny + 80) \ 15 Then PSet (160 + nx * 200 / (ny + 400), 100 - nz * 200 / (ny + 400)), 31 - (ny + 80) \ 15
    Next i%
Loop Until InKey$ <> ""

_SndClose WilliamTell& '                                  remove MP3 from RAM
System
