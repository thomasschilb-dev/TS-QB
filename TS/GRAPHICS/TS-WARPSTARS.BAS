'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-WARPSTARS.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "WARPSTARS"
Cls
Const MAXSTARS = 300 ' 50 to 1000 stars
Const WARPSPEED = 10 ' [0=fast to 10=slow]
Screen 12

Type starData
    x As Single ' x increment value
    y As Single ' y increment value
    c As Integer ' star colour
    xx As Single ' x position
    yy As Single ' y position
End Type

Dim i%, s%
Dim star(MAXSTARS) As starData
 
For i% = 1 To MAXSTARS 'initialize star settings
    star(i%).x = ((Rnd * 2) - 1) * 3
    star(i%).y = ((Rnd * 2) - 1) * 3
    Select Case (Abs(star(i%).x) + Abs(star(i%).y))
        Case Is < 2: star(i%).c = 8
        Case Is < 4: star(i%).c = 7
        Case Is <= 6: star(i%).c = 15
    End Select
    star(i%).xx = (((star(i%).x * 215) / 2) + 320)
    star(i%).yy = (((star(i%).y * 135) / 2) + 200)
Next i%
 
Do ' display starfield, main program loop
    For i% = 1 To MAXSTARS
        PSet (star(i%).xx - star(i%).x, star(i%).yy - star(i%).y), 0
        star(i%).xx = star(i%).xx + star(i%).x
        star(i%).yy = star(i%).yy + star(i%).y
        PSet (star(i%).xx, star(i%).yy), star(i%).c
        If star(i%).yy > 483 Or star(i%).yy < -4 Then star(i%).xx = 320: star(i%).yy = 240
        If star(i%).xx > 643 Or star(i%).xx < -4 Then star(i%).xx = 320: star(i%).yy = 240
        For s% = 0 To WARPSPEED * 1000: Next s%
    Next i%
Loop Until InKey$ <> ""
System

