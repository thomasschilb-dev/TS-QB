'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-DCLOCK.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "TS-DCLOCK"
Cls
DECLARE SUB FELT (X%, Y%, XX%, YY%, FARBEN%)
DECLARE SUB KLPRINT (B$, B%, K%, G%, I%)
DECLARE SUB DATUM ()
DECLARE SUB FELD (A%, B%, AA%, BB%, C%)
Screen 12
Width 80, 60
FELT 0, 0, 639, 479, 0
FELT 36, 78, 597, 154, 0

'CONST PI = 3.141592
Dim Shared PI
PI = 3.141592

XCXX = Val(Mid$(Time$, 7, 2))
XCXY = Val(Mid$(Time$, 4, 2))
XCXZ = Val(Mid$(Time$, 1, 2))
1.1
XCXB = PI * 2 / 12 - .0001
XCXD = PI * 2 / 60 - .0001
XCXE = PI * 2 / 60 - .0001
456
If InKey$ <> "" Then End
If XCXX <> Val(Mid$(Time$, 7, 2)) Then XCXX = Val(Mid$(Time$, 7, 2)): GFF = 1 Else GFF = 0: GoTo 456
If XCXY <> Val(Mid$(Time$, 4, 2)) Then XCXY = Val(Mid$(Time$, 4, 2)): GFG = 1 Else GFG = 0
If XCXZ <> Val(Mid$(Time$, 1, 2)) Then XCXZ = Val(Mid$(Time$, 1, 2)): GFH = 1 Else GFH = 0
XCXC = Val(Mid$(Time$, 1, 2))
XCXF = Val(Mid$(Time$, 4, 2))
XCXG = Val(Mid$(Time$, 7, 2))
If XCXC > 12 Then XCXC = XCXC - 12
If XCXC < 4 Then XCXC = XCXC + 12
If XCXC > 3 Then XCXC = XCXC - 3
If XCXF < 16 Then XCXF = XCXF + 60
If XCXF > 15 Then XCXF = XCXF - 15
If XCXG < 16 Then XCXG = XCXG + 60
If XCXG > 15 Then XCXG = XCXG - 15
XCXA = XCXB * XCXC
XCXH = XCXD * XCXF
XCXI = XCXE * XCXG
XCXAA = XCXA
XCXHH = XCXH
XCXII = XCXI
If TEMPTIME$ <> Time$ Then TEMPTIME$ = Time$: DATUM
KLPRINT Mid$(Date$, 4, 2) + ".", 18, 576, 0, 4
KLPRINT Mid$(Date$, 1, 2) + ".", 18, 594, 0, 4
KLPRINT Mid$(Date$, 7, 4), 18, 611, 0, 4
KLPRINT "", 18, 0, 0, 4
Color 15
A = -1
B = -1
C = -1
D = -1
E = -1
F = -1
Locate 27, 27: Print "лл"
Locate 28, 27: Print "лл"
Locate 29, 51: Print "лл"
Locate 30, 51: Print "лл"
20
If A <> Val(Mid$(Time$, 1, 1)) Then A = Val(Mid$(Time$, 1, 1)): GoTo 2 Else GoTo 1
2
Select Case A
    Case 0:
        Locate 25, 7: Print "лллллллл"
        Locate 26, 7: Print "лл    лл"
        Locate 27, 7: Print "лл    лл"
        Locate 28, 7: Print "лл    лл"
        Locate 29, 7: Print "лл    лл"
        Locate 30, 7: Print "лл    лл"
        Locate 31, 7: Print "лллллллл"
    Case 1
        Locate 25, 7: Print "      лл"
        Locate 26, 7: Print "      лл"
        Locate 27, 7: Print "      лл"
        Locate 28, 7: Print "      лл"
        Locate 29, 7: Print "      лл"
        Locate 30, 7: Print "      лл"
        Locate 31, 7: Print "      лл"
    Case 2:
        Locate 25, 7: Print "лллллллл"
        Locate 26, 7: Print "      лл"
        Locate 27, 7: Print "      лл"
        Locate 28, 7: Print "лллллллл"
        Locate 29, 7: Print "лл      "
        Locate 30, 7: Print "лл      "
        Locate 31, 7: Print "лллллллл"
End Select
1
If B <> Val(Mid$(Time$, 2, 1)) Then B = Val(Mid$(Time$, 2, 1)): GoTo 4 Else GoTo 3
4
Select Case B
    Case 0:
        Locate 25, 17: Print "лллллллл"
        Locate 26, 17: Print "лл    лл"
        Locate 27, 17: Print "лл    лл"
        Locate 28, 17: Print "лл    лл"
        Locate 29, 17: Print "лл    лл"
        Locate 30, 17: Print "лл    лл"
        Locate 31, 17: Print "лллллллл"
    Case 1
        Locate 25, 17: Print "      лл"
        Locate 26, 17: Print "      лл"
        Locate 27, 17: Print "      лл"
        Locate 28, 17: Print "      лл"
        Locate 29, 17: Print "      лл"
        Locate 30, 17: Print "      лл"
        Locate 31, 17: Print "      лл"
    Case 2:
        Locate 25, 17: Print "лллллллл"
        Locate 26, 17: Print "      лл"
        Locate 27, 17: Print "      лл"
        Locate 28, 17: Print "лллллллл"
        Locate 29, 17: Print "лл      "
        Locate 30, 17: Print "лл      "
        Locate 31, 17: Print "лллллллл"
    Case 3:
        Locate 25, 17: Print "лллллллл"
        Locate 26, 17: Print "      лл"
        Locate 27, 17: Print "      лл"
        Locate 28, 17: Print "лллллллл"
        Locate 29, 17: Print "      лл"
        Locate 30, 17: Print "      лл"
        Locate 31, 17: Print "лллллллл"
    Case 4:
        Locate 25, 17: Print "лл    лл"
        Locate 26, 17: Print "лл    лл"
        Locate 27, 17: Print "лл    лл"
        Locate 28, 17: Print "лллллллл"
        Locate 29, 17: Print "      лл"
        Locate 30, 17: Print "      лл"
        Locate 31, 17: Print "      лл"
    Case 5:
        Locate 25, 17: Print "лллллллл"
        Locate 26, 17: Print "лл      "
        Locate 27, 17: Print "лл      "
        Locate 28, 17: Print "лллллллл"
        Locate 29, 17: Print "      лл"
        Locate 30, 17: Print "      лл"
        Locate 31, 17: Print "лллллллл"
    Case 6:
        Locate 25, 17: Print "лллллллл"
        Locate 26, 17: Print "лл      "
        Locate 27, 17: Print "лл      "
        Locate 28, 17: Print "лллллллл"
        Locate 29, 17: Print "лл    лл"
        Locate 30, 17: Print "лл    лл"
        Locate 31, 17: Print "лллллллл"
    Case 7:
        Locate 25, 17: Print "лллллллл"
        Locate 26, 17: Print "      лл"
        Locate 27, 17: Print "      лл"
        Locate 28, 17: Print "      лл"
        Locate 29, 17: Print "      лл"
        Locate 30, 17: Print "      лл"
        Locate 31, 17: Print "      лл"
    Case 8:
        Locate 25, 17: Print "лллллллл"
        Locate 26, 17: Print "лл    лл"
        Locate 27, 17: Print "лл    лл"
        Locate 28, 17: Print "лллллллл"
        Locate 29, 17: Print "лл    лл"
        Locate 30, 17: Print "лл    лл"
        Locate 31, 17: Print "лллллллл"
    Case 9:
        Locate 25, 17: Print "лллллллл"
        Locate 26, 17: Print "лл    лл"
        Locate 27, 17: Print "лл    лл"
        Locate 28, 17: Print "лллллллл"
        Locate 29, 17: Print "      лл"
        Locate 30, 17: Print "      лл"
        Locate 31, 17: Print "лллллллл"
End Select
3
If C <> Val(Mid$(Time$, 4, 1)) Then C = Val(Mid$(Time$, 4, 1)): GoTo 6 Else GoTo 5
6
Select Case C
    Case 0:
        Locate 25, 31: Print "лллллллл"
        Locate 26, 31: Print "лл    лл"
        Locate 27, 31: Print "лл    лл"
        Locate 28, 31: Print "лл    лл"
        Locate 29, 31: Print "лл    лл"
        Locate 30, 31: Print "лл    лл"
        Locate 31, 31: Print "лллллллл"
    Case 1
        Locate 25, 31: Print "      лл"
        Locate 26, 31: Print "      лл"
        Locate 27, 31: Print "      лл"
        Locate 28, 31: Print "      лл"
        Locate 29, 31: Print "      лл"
        Locate 30, 31: Print "      лл"
        Locate 31, 31: Print "      лл"
    Case 2:
        Locate 25, 31: Print "лллллллл"
        Locate 26, 31: Print "      лл"
        Locate 27, 31: Print "      лл"
        Locate 28, 31: Print "лллллллл"
        Locate 29, 31: Print "лл      "
        Locate 30, 31: Print "лл      "
        Locate 31, 31: Print "лллллллл"
    Case 3:
        Locate 25, 31: Print "лллллллл"
        Locate 26, 31: Print "      лл"
        Locate 27, 31: Print "      лл"
        Locate 28, 31: Print "лллллллл"
        Locate 29, 31: Print "      лл"
        Locate 30, 31: Print "      лл"
        Locate 31, 31: Print "лллллллл"
    Case 4:
        Locate 25, 31: Print "лл    лл"
        Locate 26, 31: Print "лл    лл"
        Locate 27, 31: Print "лл    лл"
        Locate 28, 31: Print "лллллллл"
        Locate 29, 31: Print "      лл"
        Locate 30, 31: Print "      лл"
        Locate 31, 31: Print "      лл"
    Case 5:
        Locate 25, 31: Print "лллллллл"
        Locate 26, 31: Print "лл      "
        Locate 27, 31: Print "лл      "
        Locate 28, 31: Print "лллллллл"
        Locate 29, 31: Print "      лл"
        Locate 30, 31: Print "      лл"
        Locate 31, 31: Print "лллллллл"
    Case 6:
        Locate 25, 31: Print "лллллллл"
        Locate 26, 31: Print "лл      "
        Locate 27, 31: Print "лл      "
        Locate 28, 31: Print "лллллллл"
        Locate 29, 31: Print "лл    лл"
        Locate 30, 31: Print "лл    лл"
        Locate 31, 31: Print "лллллллл"
    Case 7:
        Locate 25, 31: Print "лллллллл"
        Locate 26, 31: Print "      лл"
        Locate 27, 31: Print "      лл"
        Locate 28, 31: Print "      лл"
        Locate 29, 31: Print "      лл"
        Locate 30, 31: Print "      лл"
        Locate 31, 31: Print "      лл"
    Case 8:
        Locate 25, 31: Print "лллллллл"
        Locate 26, 31: Print "лл    лл"
        Locate 27, 31: Print "лл    лл"
        Locate 28, 31: Print "лллллллл"
        Locate 29, 31: Print "лл    лл"
        Locate 30, 31: Print "лл    лл"
        Locate 31, 31: Print "лллллллл"
    Case 9:
        Locate 25, 31: Print "лллллллл"
        Locate 26, 31: Print "лл    лл"
        Locate 27, 31: Print "лл    лл"
        Locate 28, 31: Print "лллллллл"
        Locate 29, 31: Print "      лл"
        Locate 30, 31: Print "      лл"
        Locate 31, 31: Print "лллллллл"
End Select
5
If D <> Val(Mid$(Time$, 5, 1)) Then D = Val(Mid$(Time$, 5, 1)): GoTo 8 Else GoTo 7
8
Select Case D
    Case 0:
        Locate 25, 41: Print "лллллллл"
        Locate 26, 41: Print "лл    лл"
        Locate 27, 41: Print "лл    лл"
        Locate 28, 41: Print "лл    лл"
        Locate 29, 41: Print "лл    лл"
        Locate 30, 41: Print "лл    лл"
        Locate 31, 41: Print "лллллллл"
    Case 1
        Locate 25, 41: Print "      лл"
        Locate 26, 41: Print "      лл"
        Locate 27, 41: Print "      лл"
        Locate 28, 41: Print "      лл"
        Locate 29, 41: Print "      лл"
        Locate 30, 41: Print "      лл"
        Locate 31, 41: Print "      лл"
    Case 2:
        Locate 25, 41: Print "лллллллл"
        Locate 26, 41: Print "      лл"
        Locate 27, 41: Print "      лл"
        Locate 28, 41: Print "лллллллл"
        Locate 29, 41: Print "лл      "
        Locate 30, 41: Print "лл      "
        Locate 31, 41: Print "лллллллл"
    Case 3:
        Locate 25, 41: Print "лллллллл"
        Locate 26, 41: Print "      лл"
        Locate 27, 41: Print "      лл"
        Locate 28, 41: Print "лллллллл"
        Locate 29, 41: Print "      лл"
        Locate 30, 41: Print "      лл"
        Locate 31, 41: Print "лллллллл"
    Case 4:
        Locate 25, 41: Print "лл    лл"
        Locate 26, 41: Print "лл    лл"
        Locate 27, 41: Print "лл    лл"
        Locate 28, 41: Print "лллллллл"
        Locate 29, 41: Print "      лл"
        Locate 30, 41: Print "      лл"
        Locate 31, 41: Print "      лл"
    Case 5:
        Locate 25, 41: Print "лллллллл"
        Locate 26, 41: Print "лл      "
        Locate 27, 41: Print "лл      "
        Locate 28, 41: Print "лллллллл"
        Locate 29, 41: Print "      лл"
        Locate 30, 41: Print "      лл"
        Locate 31, 41: Print "лллллллл"
    Case 6:
        Locate 25, 41: Print "лллллллл"
        Locate 26, 41: Print "лл      "
        Locate 27, 41: Print "лл      "
        Locate 28, 41: Print "лллллллл"
        Locate 29, 41: Print "лл    лл"
        Locate 30, 41: Print "лл    лл"
        Locate 31, 41: Print "лллллллл"
    Case 7:
        Locate 25, 41: Print "лллллллл"
        Locate 26, 41: Print "      лл"
        Locate 27, 41: Print "      лл"
        Locate 28, 41: Print "      лл"
        Locate 29, 41: Print "      лл"
        Locate 30, 41: Print "      лл"
        Locate 31, 41: Print "      лл"
    Case 8:
        Locate 25, 41: Print "лллллллл"
        Locate 26, 41: Print "лл    лл"
        Locate 27, 41: Print "лл    лл"
        Locate 28, 41: Print "лллллллл"
        Locate 29, 41: Print "лл    лл"
        Locate 30, 41: Print "лл    лл"
        Locate 31, 41: Print "лллллллл"
    Case 9:
        Locate 25, 41: Print "лллллллл"
        Locate 26, 41: Print "лл    лл"
        Locate 27, 41: Print "лл    лл"
        Locate 28, 41: Print "лллллллл"
        Locate 29, 41: Print "      лл"
        Locate 30, 41: Print "      лл"
        Locate 31, 41: Print "лллллллл"
End Select
7
If E <> Val(Mid$(Time$, 7, 1)) Then E = Val(Mid$(Time$, 7, 1)): GoTo 10 Else GoTo 9
10
Select Case E
    Case 0:
        Locate 25, 55: Print "лллллллл"
        Locate 26, 55: Print "лл    лл"
        Locate 27, 55: Print "лл    лл"
        Locate 28, 55: Print "лл    лл"
        Locate 29, 55: Print "лл    лл"
        Locate 30, 55: Print "лл    лл"
        Locate 31, 55: Print "лллллллл"
    Case 1
        Locate 25, 55: Print "      лл"
        Locate 26, 55: Print "      лл"
        Locate 27, 55: Print "      лл"
        Locate 28, 55: Print "      лл"
        Locate 29, 55: Print "      лл"
        Locate 30, 55: Print "      лл"
        Locate 31, 55: Print "      лл"
    Case 2:
        Locate 25, 55: Print "лллллллл"
        Locate 26, 55: Print "      лл"
        Locate 27, 55: Print "      лл"
        Locate 28, 55: Print "лллллллл"
        Locate 29, 55: Print "лл      "
        Locate 30, 55: Print "лл      "
        Locate 31, 55: Print "лллллллл"
    Case 3:
        Locate 25, 55: Print "лллллллл"
        Locate 26, 55: Print "      лл"
        Locate 27, 55: Print "      лл"
        Locate 28, 55: Print "лллллллл"
        Locate 29, 55: Print "      лл"
        Locate 30, 55: Print "      лл"
        Locate 31, 55: Print "лллллллл"
    Case 4:
        Locate 25, 55: Print "лл    лл"
        Locate 26, 55: Print "лл    лл"
        Locate 27, 55: Print "лл    лл"
        Locate 28, 55: Print "лллллллл"
        Locate 29, 55: Print "      лл"
        Locate 30, 55: Print "      лл"
        Locate 31, 55: Print "      лл"
    Case 5:
        Locate 25, 55: Print "лллллллл"
        Locate 26, 55: Print "лл      "
        Locate 27, 55: Print "лл      "
        Locate 28, 55: Print "лллллллл"
        Locate 29, 55: Print "      лл"
        Locate 30, 55: Print "      лл"
        Locate 31, 55: Print "лллллллл"
    Case 6:
        Locate 25, 55: Print "лллллллл"
        Locate 26, 55: Print "лл      "
        Locate 27, 55: Print "лл      "
        Locate 28, 55: Print "лллллллл"
        Locate 29, 55: Print "лл    лл"
        Locate 30, 55: Print "лл    лл"
        Locate 31, 55: Print "лллллллл"
    Case 7:
        Locate 25, 55: Print "лллллллл"
        Locate 26, 55: Print "      лл"
        Locate 27, 55: Print "      лл"
        Locate 28, 55: Print "      лл"
        Locate 29, 55: Print "      лл"
        Locate 30, 55: Print "      лл"
        Locate 31, 55: Print "      лл"
    Case 8:
        Locate 25, 55: Print "лллллллл"
        Locate 26, 55: Print "лл    лл"
        Locate 27, 55: Print "лл    лл"
        Locate 28, 55: Print "лллллллл"
        Locate 29, 55: Print "лл    лл"
        Locate 30, 55: Print "лл    лл"
        Locate 31, 55: Print "лллллллл"
    Case 9:
        Locate 25, 55: Print "лллллллл"
        Locate 26, 55: Print "лл    лл"
        Locate 27, 55: Print "лл    лл"
        Locate 28, 55: Print "лллллллл"
        Locate 29, 55: Print "      лл"
        Locate 30, 55: Print "      лл"
        Locate 31, 55: Print "лллллллл"
End Select
9
If F <> Val(Mid$(Time$, 8, 1)) Then F = Val(Mid$(Time$, 8, 1)): GoTo 12 Else GoTo 1.1
12
Select Case F
    Case 0:
        Locate 25, 65: Print "лллллллл"
        Locate 26, 65: Print "лл    лл"
        Locate 27, 65: Print "лл    лл"
        Locate 28, 65: Print "лл    лл"
        Locate 29, 65: Print "лл    лл"
        Locate 30, 65: Print "лл    лл"
        Locate 31, 65: Print "лллллллл"
    Case 1:
        Locate 25, 65: Print "      лл"
        Locate 26, 65: Print "      лл"
        Locate 27, 65: Print "      лл"
        Locate 28, 65: Print "      лл"
        Locate 29, 65: Print "      лл"
        Locate 30, 65: Print "      лл"
        Locate 31, 65: Print "      лл"
    Case 2:
        Locate 25, 65: Print "лллллллл"
        Locate 26, 65: Print "      лл"
        Locate 27, 65: Print "      лл"
        Locate 28, 65: Print "лллллллл"
        Locate 29, 65: Print "лл      "
        Locate 30, 65: Print "лл      "
        Locate 31, 65: Print "лллллллл"
    Case 3:
        Locate 25, 65: Print "лллллллл"
        Locate 26, 65: Print "      лл"
        Locate 27, 65: Print "      лл"
        Locate 28, 65: Print "лллллллл"
        Locate 29, 65: Print "      лл"
        Locate 30, 65: Print "      лл"
        Locate 31, 65: Print "лллллллл"
    Case 4:
        Locate 25, 65: Print "лл    лл"
        Locate 26, 65: Print "лл    лл"
        Locate 27, 65: Print "лл    лл"
        Locate 28, 65: Print "лллллллл"
        Locate 29, 65: Print "      лл"
        Locate 30, 65: Print "      лл"
        Locate 31, 65: Print "      лл"
    Case 5:
        Locate 25, 65: Print "лллллллл"
        Locate 26, 65: Print "лл      "
        Locate 27, 65: Print "лл      "
        Locate 28, 65: Print "лллллллл"
        Locate 29, 65: Print "      лл"
        Locate 30, 65: Print "      лл"
        Locate 31, 65: Print "лллллллл"
    Case 6:
        Locate 25, 65: Print "лллллллл"
        Locate 26, 65: Print "лл      "
        Locate 27, 65: Print "лл      "
        Locate 28, 65: Print "лллллллл"
        Locate 29, 65: Print "лл    лл"
        Locate 30, 65: Print "лл    лл"
        Locate 31, 65: Print "лллллллл"
    Case 7:
        Locate 25, 65: Print "лллллллл"
        Locate 26, 65: Print "      лл"
        Locate 27, 65: Print "      лл"
        Locate 28, 65: Print "      лл"
        Locate 29, 65: Print "      лл"
        Locate 30, 65: Print "      лл"
        Locate 31, 65: Print "      лл"
    Case 8:
        Locate 25, 65: Print "лллллллл"
        Locate 26, 65: Print "лл    лл"
        Locate 27, 65: Print "лл    лл"
        Locate 28, 65: Print "лллллллл"
        Locate 29, 65: Print "лл    лл"
        Locate 30, 65: Print "лл    лл"
        Locate 31, 65: Print "лллллллл"
    Case 9:
        Locate 25, 65: Print "лллллллл"
        Locate 26, 65: Print "лл    лл"
        Locate 27, 65: Print "лл    лл"
        Locate 28, 65: Print "лллллллл"
        Locate 29, 65: Print "      лл"
        Locate 30, 65: Print "      лл"
        Locate 31, 65: Print "лллллллл"
End Select
GoTo 1.1

DefInt A-Z
Sub DATUM
    Rem KLPRINT TIME$, 18, 5, 0, 4
    If Mid$(Time$, 1, 2) <> "00" Then GoTo 3.1
    If Mid$(Time$, 4, 2) <> "00" Then GoTo 3.1
    If Mid$(Time$, 7, 2) <> "00" Then GoTo 3.1
    KLPRINT Mid$(Date$, 4, 2) + ".", 18, 476, 0, 4
    KLPRINT Mid$(Date$, 1, 2) + ".", 18, 494, 0, 4
    KLPRINT Mid$(Date$, 7, 4), 18, 511, 0, 4
3.1 End Sub

Sub FELT (X, Y, XX, YY, FARBEN)
    Rem LINE (X, Y)-(X, YY), 15
    Rem LINE (X, Y)-(XX, Y), 15
    Rem LINE (X, YY)-(XX, YY), 8
    Rem LINE (XX, Y)-(XX, YY), 8
    View (X + 2, Y + 2)-(XX - 2, YY - 2), FARBEN, FARBEN
    View (0, 0)-(639, 479)
End Sub

Sub KLPRINT (B$, B, K, G, I)
    C = Len(B$)
    B$ = UCase$(B$)
    D = K - 6
    E = B - 14
    If G <> 10 Then E = E - G
    7.1: F = F + 1
    H = F * 6
    If F = C + 1 Then GoTo 198
    If G = 10 Then
        View (D + H, E)-(D + H + 4, E + 6), 7, 7
        View (0, 0)-(639, 479)
    End If
    If Mid$(B$, F, 1) = " " Then
        GoTo 123
    ElseIf Mid$(B$, F, 1) = "A" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "B" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "C" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I
        PSet (D + H, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "D" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "E" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H, E + 4), I
        PSet (D + H, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "F" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I
        PSet (D + H, E + 4), I
        PSet (D + H, E + 5), I
        PSet (D + H, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "G" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 3, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "H" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "I" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H + 2, E + 1), I
        PSet (D + H + 2, E + 2), I
        PSet (D + H + 2, E + 3), I
        PSet (D + H + 2, E + 4), I
        PSet (D + H + 2, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "J" Then
        PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H + 3, E + 1), I
        PSet (D + H + 3, E + 2), I
        PSet (D + H + 3, E + 3), I
        PSet (D + H + 3, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 3, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "K" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 3, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 2, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 2, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 3, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "L" Then
        PSet (D + H, E + 0), I
        PSet (D + H, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I
        PSet (D + H, E + 4), I
        PSet (D + H, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "M" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 1, E + 1), I: PSet (D + H + 3, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 2, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "N" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 1, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 2, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 2, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 3, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "O" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "P" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H, E + 4), I
        PSet (D + H, E + 5), I
        PSet (D + H, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "Q" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 2, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 3, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "R" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 2, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 3, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "S" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H + 4, E + 4), I
        PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "T" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H + 2, E + 1), I
        PSet (D + H + 2, E + 2), I
        PSet (D + H + 2, E + 3), I
        PSet (D + H + 2, E + 4), I
        PSet (D + H + 2, E + 5), I
        PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "U" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "V" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H + 1, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H + 1, E + 4), I: PSet (D + H + 3, E + 4), I
        PSet (D + H + 2, E + 5), I
        PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "W" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 2, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 1, E + 5), I: PSet (D + H + 3, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "X" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H + 1, E + 2), I: PSet (D + H + 3, E + 2), I
        PSet (D + H + 2, E + 3), I
        PSet (D + H + 1, E + 4), I: PSet (D + H + 3, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "Y" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H + 1, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H + 2, E + 4), I
        PSet (D + H + 2, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "Z" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H + 3, E + 2), I
        PSet (D + H + 2, E + 3), I
        PSet (D + H + 1, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "." Then
        PSet (D + H + 1, E + 5), I: PSet (D + H + 2, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "," Then
        PSet (D + H + 1, E + 4), I: PSet (D + H + 2, E + 4), I
        PSet (D + H + 1, E + 5), I: PSet (D + H + 2, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "&" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 3, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 2, E + 2), I
        PSet (D + H + 1, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 2, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 3, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "(" Then
        PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H + 1, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I
        PSet (D + H, E + 4), I
        PSet (D + H + 1, E + 5), I
        PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = ")" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I
        PSet (D + H + 3, E + 1), I
        PSet (D + H + 4, E + 2), I
        PSet (D + H + 4, E + 3), I
        PSet (D + H + 4, E + 4), I
        PSet (D + H + 3, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "1" Then
        PSet (D + H + 3, E + 0), I
        PSet (D + H + 2, E + 1), I: PSet (D + H + 3, E + 1), I
        PSet (D + H + 1, E + 2), I: PSet (D + H + 3, E + 2), I
        PSet (D + H + 3, E + 3), I
        PSet (D + H + 3, E + 4), I
        PSet (D + H + 3, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "2" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H + 3, E + 3), I
        PSet (D + H + 2, E + 4), I
        PSet (D + H + 1, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "3" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H + 4, E + 2), I
        PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "4" Then
        PSet (D + H + 3, E + 0), I
        PSet (D + H + 2, E + 1), I: PSet (D + H + 3, E + 1), I
        PSet (D + H + 1, E + 2), I: PSet (D + H + 3, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H + 3, E + 4), I
        PSet (D + H + 3, E + 5), I
        PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "5" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H + 4, E + 4), I
        PSet (D + H + 4, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "6" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "7" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H + 3, E + 2), I
        PSet (D + H + 3, E + 3), I
        PSet (D + H + 2, E + 4), I
        PSet (D + H + 2, E + 5), I
        PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "8" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "9" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "0" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 3, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 1, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = ":" Then
        PSet (D + H + 1, E + 1), I: PSet (D + H + 2, E + 1), I
        PSet (D + H + 1, E + 2), I: PSet (D + H + 2, E + 2), I
        PSet (D + H + 1, E + 4), I: PSet (D + H + 2, E + 4), I
        PSet (D + H + 1, E + 5), I: PSet (D + H + 2, E + 5), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "-" Then
        PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I
        PSet (D + H + 3, E + 3), I: PSet (D + H + 4, E + 3), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "$" Then
        PSet (D + H + 2, E + 0), I
        PSet (D + H + 1, E + 1), I: PSet (D + H + 2, E + 1), I: PSet (D + H + 3, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 2, E + 2), I
        PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I
        PSet (D + H + 2, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 1, E + 5), I: PSet (D + H + 2, E + 5), I: PSet (D + H + 3, E + 5), I
        PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "*" Then
        PSet (D + H, E + 1), I: PSet (D + H + 2, E + 1), I: PSet (D + H + 4, E + 1), I
        PSet (D + H + 1, E + 2), I: PSet (D + H + 2, E + 2), I: PSet (D + H + 3, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), I: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H + 1, E + 4), I: PSet (D + H + 2, E + 4), I: PSet (D + H + 3, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 2, E + 5), I: PSet (D + H + 4, E + 5), I
    ElseIf Mid$(B$, F, 1) = "/" Then
        PSet (D + H + 4, E + 0), I
        PSet (D + H + 3, E + 1), I
        PSet (D + H + 3, E + 2), I
        PSet (D + H + 2, E + 3), I
        PSet (D + H + 1, E + 4), I
        PSet (D + H + 1, E + 5), I
        PSet (D + H, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "\" Then
        PSet (D + H, E + 0), I
        PSet (D + H + 1, E + 1), I
        PSet (D + H + 1, E + 2), I
        PSet (D + H + 2, E + 3), I
        PSet (D + H + 3, E + 4), I
        PSet (D + H + 3, E + 5), I
        PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "[" Then
        PSet (D + H, E + 0), I: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I
        PSet (D + H, E + 1), I
        PSet (D + H, E + 2), I
        PSet (D + H, E + 3), I
        PSet (D + H, E + 4), I
        PSet (D + H, E + 5), I
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "]" Then
        PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H + 4, E + 1), I
        PSet (D + H + 4, E + 2), I
        PSet (D + H + 4, E + 3), I
        PSet (D + H + 4, E + 4), I
        PSet (D + H + 4, E + 5), I
        PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "_" Then
        PSet (D + H, E + 6), I: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "" Or Mid$(B$, F, 1) = "" Then
        PSet (D + H, E + 0), I: PSet (D + H + 4, E + 0), I
        PSet (D + H, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 4, E + 5), I
        PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I
    ElseIf Mid$(B$, F, 1) = "!" Then
        PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I
        PSet (D + H + 1, E + 1), I: PSet (D + H + 2, E + 1), I: PSet (D + H + 3, E + 1), I
        PSet (D + H + 2, E + 2), I
        PSet (D + H + 2, E + 5), I
        PSet (D + H + 2, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = ">" Then
        PSet (D + H + 1, E + 0), I
        PSet (D + H + 2, E + 1), I
        PSet (D + H + 3, E + 2), I
        PSet (D + H + 4, E + 3), I
        PSet (D + H + 3, E + 4), I
        PSet (D + H + 2, E + 5), I
        PSet (D + H + 1, E + 6), I: GoTo 123
    ElseIf Mid$(B$, F, 1) = "@" Then
        PSet (D + H, E + 0), 7: PSet (D + H + 1, E + 0), I: PSet (D + H + 2, E + 0), I: PSet (D + H + 3, E + 0), I: PSet (D + H + 4, E + 0), 7
        PSet (D + H, E + 1), I: PSet (D + H + 1, E + 1), 7: PSet (D + H + 2, E + 1), 7: PSet (D + H + 3, E + 1), 7: PSet (D + H + 4, E + 1), I
        PSet (D + H, E + 2), I: PSet (D + H + 1, E + 2), 7: PSet (D + H + 2, E + 2), I: PSet (D + H + 3, E + 2), I: PSet (D + H + 4, E + 2), I
        PSet (D + H, E + 3), I: PSet (D + H + 1, E + 3), 7: PSet (D + H + 2, E + 3), I: PSet (D + H + 3, E + 3), I: PSet (D + H + 4, E + 3), I
        PSet (D + H, E + 4), I: PSet (D + H + 1, E + 4), 7: PSet (D + H + 2, E + 4), I: PSet (D + H + 3, E + 4), I: PSet (D + H + 4, E + 4), I
        PSet (D + H, E + 5), I: PSet (D + H + 1, E + 5), 7: PSet (D + H + 2, E + 5), 7: PSet (D + H + 3, E + 5), 7: PSet (D + H + 4, E + 5), 7
        PSet (D + H, E + 6), 7: PSet (D + H + 1, E + 6), I: PSet (D + H + 2, E + 6), I: PSet (D + H + 3, E + 6), I: PSet (D + H + 4, E + 6), 7
    ElseIf Mid$(B$, F, 1) = "<" Then
        PSet (D + H + 3, E + 0), I
        PSet (D + H + 2, E + 1), I
        PSet (D + H + 1, E + 2), I
        PSet (D + H, E + 3), I
        PSet (D + H + 1, E + 4), I
        PSet (D + H + 2, E + 5), I
        PSet (D + H + 3, E + 6), I: GoTo 123
    123 End If
    If F - 1 < C Then GoTo 7.1
198 End Sub

