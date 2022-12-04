'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-SPIRALTEXT.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "TS-SPIRALTEXT"
Cls
DECLARE SUB redraw ()
DECLARE SUB analyse ()

Dim Shared text As String
text$ = "T S C"

Dim Shared word(1 To Len(text$) * 8, 1 To 16)
Call analyse
Cls
Call redraw

Sub analyse
    Cls
    Screen 12

    Color 4: Locate 1, 1: Print text$

    Dim px As Integer, py As Integer, cnt As Integer, ltrcnt As Integer

    px = 1: py = 1

    Do

        word(px, py) = Point(px, py)

        PSet (px, py), 1
        px = px + 1

        If px = Len(text$) * 8 Then

            px = 1
            py = py + 1

        End If

    Loop Until py = 16

End Sub

Sub redraw

    Cls

    Dim row As Integer, cnt As Integer, cstart As Single, cend As Single
    Dim xrot As Integer, yrot As Integer, SCALE As Integer, pan As Integer

    cstart = 0: cend = 6.2

    xrot = 6: yrot = 6: SCALE = 3: pan = 30

    Out &H3C8, 1: Out &H3C9, 10: Out &H3C9, 10: Out &H3C9, 60

    Do

        row = 2

        Do

            Do

                For i = cend To cstart Step -.03

                    x = (SCALE * 60 - (row * xrot / 4)) * Tan(Cos(i))
                    y = Sin(SCALE * 60 - (row * yrot)) * Tan(Sin(i)) * pan

                    cnt = cnt + 1

                    If word(cnt, row) > 0 Then

                        Circle (x + 320, y + 220), SCALE + 1, 4
                        'LINE (x + 320, y + 220)-STEP(12, 12), 1, BF

                    End If

                    If cnt = Len(text$) * 8 Then cnt = 0: Exit Do

                Next

            Loop

            row = row + 1

        Loop Until row = 16

        cend = cend + .1
        cstart = cstart + .1

        now! = Timer

        Do

            newnow! = Timer

        Loop Until newnow! - now! >= .05

        Line (1, 100)-(639, 280), 0, BF

    Loop Until InKey$ = Chr$(27)
    System
End Sub
