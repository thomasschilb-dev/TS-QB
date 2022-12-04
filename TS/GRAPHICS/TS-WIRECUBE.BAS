'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-WIRECUBE.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "TS-WIRECUBE"
Cls
Dim a%(500, 2), b&(8000)
Screen 13
Color 15: Locate 20, 18: Print "+-------[ www.thomasschilb.tk ]--------+"
Color 15: Locate 4, 18: Print "+-------------[ WireCube ]-------------+"
Color 15: Locate 6: Print "|                                      |"
Color 15: Locate 6: Print "|                                      |"
Color 15: Locate 6: Print "|                                      |"
Color 15: Locate 7: Print "|                                      |"
Color 15: Locate 8: Print "|                                      |"
Color 15: Locate 9: Print "|                                      |"
Color 15: Locate 10: Print "|                                      |"
Color 15: Locate 11: Print "|                                      |"
Color 15: Locate 12: Print "|                                      |"
Color 15: Locate 13: Print "|                                      |"
Color 15: Locate 14: Print "|                                      |"
Color 15: Locate 15: Print "|                                      |"
Color 15: Locate 16: Print "|                                      |"
Color 15: Locate 17: Print "|                                      |"
Color 15: Locate 18: Print "|                                      |"
Color 15: Locate 19: Print "|                                      |"
Color 15: Locate 20: Print "|                                      |"


Def Seg = VarSeg(b&(0))
For i% = 0 To 1499
    _Delay .001
    a%(i% \ 3 + 1, i% Mod 3) = (Int(Rnd * 2) * 2 - 1) * Int(50 * Cos(Rnd * 1.570794))
Next i%
Do
    _Delay 0.001
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
System
