'
' TODO: TIMER FOR HIGH PERFORMANCE
'

DefInt A-Z

DECLARE SUB AddPart (X%, Y%, Z%, Array() AS ANY)
DECLARE SUB MakePalette (Red%, Green%, Blue%)
DECLARE SUB DrawDOTS ()
DECLARE SUB DrawDOT (aDot AS ANY)

Type Particle3D
    X As Integer
    Y As Integer
    Z As Integer
    Used As Integer
End Type

Const Pi = 3.141593

ReDim Shared Buffer(32001) As Integer

'$STATIC

Dim Shared Cosine(360) As Single
Dim Shared Sine(360) As Single
Dim Shared d4Lut(1024) As Integer
Dim Shared yLut(200) As Integer
Dim Shared cLut(-4 To 255) As Integer
Dim Shared Dot(36) As Integer
Dim Shared Parts(280) As Particle3D
Dim Shared Dots(280) As Particle3D
Dim Shared CAMX, CAMZ, CAMY
 
For i = 0 To 35
    Read Dot(i)
Next i

For i = 1 To 200
    yLut(i) = i * 320
Next i

For i = 0 To 360
    Cosine(i) = Cos(i * (Pi / 180))
    Sine(i) = Sin(i * (Pi / 180))
Next i

For i = 1 To 1024
    d4Lut(i) = i / 4
Next i

For i = 1 To 255
    cLut(i) = i
Next

Screen 13

MakePalette 0, 0, 1
OldColour = 3

Def Seg = VarSeg(Buffer(2))

Cls: Color 1: Text$ = "TS": Locate 1, 1: Print Text$
For X = 0 To Len(Text$) * 8
    For Y = 0 To 7
        If Point(X, Y) Then
            AddPart ((X - ((Len(Text$) * 8 - 8) / 2)) * 12), ((Y - 4) * 12), 6, Parts()
            AddPart ((X - ((Len(Text$) * 8 - 8) / 2)) * 12), ((Y - 4) * 12), -6, Parts()
        End If
    Next
Next

AngleX = 180
AngleY = 180

XSpd = 2

CAMZ = 150: CAMZD = 1
CAMX = 160: CAMXD = -1
CAMY = 100: CAMYD = 1

Time# = Timer
SpinClock# = Timer
ColourClock# = Timer

Do
    CAMZ = CAMZ + CAMZD
    If CAMZ > 500 Then CAMZD = -1
    If CAMZ < 150 Then CAMZD = 1
    CAMX = CAMX + CAMXD
    If CAMX > 200 Then CAMXD = -1
    If CAMX < 120 Then CAMXD = 1
    CAMY = CAMY + CAMYD
    If CAMY > 150 Then CAMYD = -1
    If CAMY < 50 Then CAMYD = 1

    AngleY = (AngleY + 1) Mod 360
    AngleX = (AngleX + XSpd) Mod 360

    If (Timer - SpinClock#) > 2 Then
        XSpd = XSpd + 1
        If XSpd > 7 Then XSpd = 7
        If (Timer - SpinClock#) > 6 Then XSpd = XSpd - 2
        If XSpd < 2 Then XSpd = 2: SpinClock# = Timer
    End If

    If (Timer - ColourClock#) > 10 Then
        Do
            Colour = Int(Rnd * 3) + 1
        Loop Until OldColour <> Colour
        MakePalette Abs(Colour = 1), Abs(Colour = 2), Abs(Colour = 3)
        OldColour = Colour
        ColourClock# = Timer
    End If

    For i = 1 To UBound(Parts, 1)
        If Parts(i).Used Then
            OX = Parts(i).X: OY = Parts(i).Y: OZ = Parts(i).Z
 
            NY = (OY * Cosine(AngleX)) - (OZ * Sine(AngleX))
            NZ = (OZ * Cosine(AngleX)) + (OY * Sine(AngleX))
            OZ = NZ
            NZ = (OZ * Cosine(AngleY)) - (OX * Sine(AngleY))
            NX = (OX * Cosine(AngleY)) + (OZ * Sine(AngleY))
   
            AddPart NX, NY, NZ, Dots()
        End If
    Next

    DrawDOTS

    O = 4
    For i = 1 To 31840
        O = O + 1: Poke (O), cLut((d4Lut((Peek(O + 321) + Peek(O + 319) + Peek(O - 321) + Peek(O - 319))) - 2))
        O = O + 1: Poke (O), cLut((d4Lut((Peek(O + 321) + Peek(O + 319) + Peek(O - 321) + Peek(O - 319))) - 2))
    Next

    Frames& = Frames& + 1

    Buffer(0) = 2560: Buffer(1) = 200
    Put (0, 0), Buffer(0), PSet
Loop Until Inp(&H60) = 1

Screen 0: Width 80

Time# = Timer - Time#

Print "Total Frames:"; Frames&
Print "Total Seconds:"; Time#
Print "Total FPS:"; Frames& / Time#
Print
Print "3DText by Lithium @ TRINTS Online"

Def Seg

Erase Buffer

Data 000,075,075,075,075,000
Data 075,125,125,125,125,075
Data 075,125,175,175,125,075
Data 075,125,175,175,125,075
Data 075,125,125,125,125,075
Data 000,075,075,075,075,000

Sub AddPart (X, Y, Z, Array() As Particle3D)
    For i = 1 To UBound(Array, 1)
        If Array(i).Used = 0 Then
            Array(i).X = X
            Array(i).Y = Y
            Array(i).Z = Z
            Array(i).Used = -1
            Exit Sub
        End If
    Next
End Sub

Sub DrawDOT (aDot As Particle3D)
    ZC = (aDot.Z \ 4) - (CAMZ \ 20)
    If ZC < -75 Then ZC = -75
    Xx2 = ((aDot.X * 256#) / (aDot.Z - 512 - CAMZ)) + CAMX
    Yy2 = ((aDot.Y * 256#) / (aDot.Z - 512 - CAMZ)) + CAMY

    Box = Xx2 + yLut(Yy2 - 1)
    For D = 1 To 6
        Box = Box + 320
        If Dot(Offset) Then Poke (Box), Dot(Offset) + ZC
        If Dot(Offset + 1) Then Poke (Box + 1), Dot(Offset + 1) + ZC
        If Dot(Offset + 2) Then Poke (Box + 2), Dot(Offset + 2) + ZC
        If Dot(Offset + 3) Then Poke (Box + 3), Dot(Offset + 3) + ZC
        If Dot(Offset + 4) Then Poke (Box + 4), Dot(Offset + 4) + ZC
        If Dot(Offset + 5) Then Poke (Box + 5), Dot(Offset + 5) + ZC
        Offset = Offset + 6
    Next
End Sub

Sub DrawDOTS
    For i = 1 To UBound(Dots, 1)
        For j = 1 To UBound(Dots, 1)
            If Dots(i).Z < Dots(j).Z Then Swap Dots(i), Dots(j)
        Next
    Next
    For i = 1 To UBound(Dots, 1)
        If Dots(i).Used Then
            DrawDOT Dots(i)
            Dots(i).Used = 0
        End If
    Next
End Sub

Sub MakePalette (Red, Green, Blue)
    For i = 1 To 255
        Out &H3C8, i
        Out &H3C9, (i * (63 / 255)) * Red
        Out &H3C9, (i * (63 / 255)) * Green
        Out &H3C9, (i * (63 / 255)) * Blue
    Next i
End Sub

