'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-DOSMATRIX1.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "DOSMATRIX1"
Cls
Color 2
1 Def Seg = &HB800
2 For i% = 0 To 159 Step 4
    '  adjust this speed constant for optimal effect
    '             |    0 no speed ;) .05 should be too fast even for a 386
    '             |
    3 If Rnd < .0005 Then j% = 3840 Else j% = -1
    4 If j% > 0 Then Poke j% + i%, Peek(j% - 160 + i%)
    5 If j% > 0 Then j% = j% - 160
    6 If j% > 0 Then GoTo 4
    7 If j% = 0 Then If Rnd > .3 Then Poke i%, 96 * Rnd + 32 Else Poke i%, 32

8 Next

9 If Len(InKey$) = 0 Then GoTo 2
System
