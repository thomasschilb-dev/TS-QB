'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-3DFLAMECUBE.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "TS-3DFLAMECUBE"
Cls
DECLARE SUB pal (c%, r%, g%, B%)
DECLARE SUB flam ()
'
Screen 13
'
xoff% = 160 ' Center of the cube in x
yoff% = 100 ' Same but in y
zoff% = 200 ' Same but in z
'
xang = .15 ' don't change these... they control the rotation.
yang = .1
zang = .05
'
points% = 8 ' amount of vertexes in the cube...
points3% = points% * 3 ' this is to eliminate the multiply
'
Dim cube(points% * 3) ' all coordinates of the cube
Dim scr(points% * 2) ' coordinates of where the vertexes should be
' on the screen
'
For a% = 1 To points3%
    Read value%
    cube(a%) = value% * 2
Next
'
For a% = 0 To 63 ' change the palette, colors 0-48
    pal a% / 4, a%, 0, 0
    pal a% / 4 + 16, 63, a%, 0
    pal a% / 4 + 32, 63, 63, a%
Next

inc% = 1

Do
    '
    xp = cube(inc%)
    yp = cube(inc% + 1)
    zp = cube(inc% + 2)
    '
    ' * ** ***** Rotate around z-axis ***** ** *
    '
    rxp = Cos(zang) * xp - Sin(zang) * yp
    ryp = Sin(zang) * xp + Cos(zang) * yp
    '
    xp = rxp
    yp = ryp
    '
    ' * ** ***** Rotate around y-axis ***** ** *
    '
    rxp = Cos(yang) * xp - Sin(yang) * zp
    rzp = Sin(yang) * xp + Cos(yang) * zp
    '
    xp = rxp
    zp = rzp
    '
    ' * ** ***** Rotate around x-axis ***** ** *
    '
    ryp = Cos(xang) * yp - Sin(xang) * zp
    rzp = Sin(xang) * yp + Cos(xang) * zp
    '
    cube(inc%) = rxp
    cube(inc% + 1) = ryp
    cube(inc% + 2) = rzp
    '
    ' * ** ******************************** ** *
    '
    inc% = inc% + 3
    '
    If inc% > points3% Then
        '
        inc% = 1: inc2% = 1
        '
        flam ' Make the flameeffect
        '
        Do
            '
            scr(inc2%) = (cube(inc%) * 256) / (cube(inc% + 2) - zoff%) + xoff%
            scr(inc2% + 1) = (cube(inc% + 1) * 256) / (cube(inc% + 2) - zoff%) + yoff%
            '
            inc2% = inc2% + 2
            inc% = inc% + 3
            '
        Loop While inc% < points3% + 1
        '
        ' Draw Top
        '
        Line (scr(1), scr(2))-(scr(3), scr(4)), 48
        Line -(scr(5), scr(6)), 48
        Line -(scr(7), scr(8)), 48
        Line -(scr(1), scr(2)), 48
        '
        ' Draw Bottom
        '
        Line (scr(9), scr(10))-(scr(11), scr(12)), 48
        Line -(scr(13), scr(14)), 48
        Line -(scr(15), scr(16)), 48
        Line -(scr(9), scr(10)), 48
        '
        ' Draw Sides
        '
        Line (scr(1), scr(2))-(scr(9), scr(10)), 48
        Line (scr(3), scr(4))-(scr(11), scr(12)), 48
        Line (scr(5), scr(6))-(scr(13), scr(14)), 48
        Line (scr(7), scr(8))-(scr(15), scr(16)), 48
        '
        inc% = 1
        '
    End If
    '
    If InKey$ = Chr$(27) Then
        '
        Screen 0: Width 80, 25
        '
        System
        End
        '
    End If
    '
    _Delay 0.005
    '
Loop
'
' * ** coordinates of the cube ** *
' TOP
'      x   y   z
Data -10,10,10
Data -10,10,-10
Data 10,10,-10
Data 10,10,10
' BOTTOM
Data -10,-10,10
Data -10,-10,-10
Data 10,-10,-10
Data 10,-10,10

Sub flam
    '
    For py3% = 10 To 148
        For px3% = 115 To 206
            pol3% = Point(px3%, py3%)
            pol3% = pol3% + Point(px3% + 1, py3%)
            pol3% = pol3% + Point(px3%, py3% + 1)
            pol3% = pol3% + Point(px3% - 1, py3%)
            pol3% = pol3% + Point(px3%, py3% - 1)
            '
            pol3% = pol3% \ 5 - 1
            '
            If pol3% > 48 Then
                pol3% = 48
            Else
                If pol3% < 0 Then pol3% = 0
            End If
            '
            PSet (px3%, py3% - 3), pol3%
            '
        Next
    Next
    '
End Sub

Sub pal (slot As Integer, red As Long, green As Long, blue As Long)
    '
    Dim palcolor As Long
    '
    palcolor = blue * 65536 + green * 256 + red
    '
    Palette slot, palcolor
    '
End Sub
