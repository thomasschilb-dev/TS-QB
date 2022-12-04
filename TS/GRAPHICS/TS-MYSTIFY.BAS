'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-MYSTIFY.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
_ScreenMove _Middle
_FullScreen
_Title "TS-MYSTIFY"
Cls
Const Sh = 2, Vx = 4, Ln = 10, MaxSpeed = 6, SV = Sh * Vx, SV2 = SV * 2, Ln1 = Ln + 1
Randomize Timer
Screen 12
Dim Posn%(Sh - 1, Vx - 1, 1, Ln), Vec%(Sh - 1, Vx - 1, 1)
For A% = 0 To 2 * Sh * Vx - 1
    Vec%(A% \ Vx Mod Sh, A% Mod Vx, A% \ SV) = -Int(Rnd * MaxSpeed + 1) * (Int(Rnd * 2) * 2 - 1)
    Posn%(A% \ Vx Mod Sh, A% Mod Vx, A% \ SV, 0) = Int(Rnd * 480)
Next
Do
    For A% = 0 To SV2 * Ln1 - 1
        If A% Mod SV2 = 0 Then Wait &H3DA, 8
        Line (Posn%(A% Mod Sh, A% \ Sh Mod Vx, 0, (A% \ SV2 + (A% \ SV Mod 2) * 2) Mod Ln1) * 1.33, Posn%(A% Mod Sh, A% \ Sh Mod Vx, 1, (A% \ SV2 + (A% \ SV Mod 2) * 2) Mod Ln1))-(Posn%(A% Mod Sh, (A% \ Sh Mod Vx + 1) Mod Vx, 0, (A% \ SV2 + (A% \ SV Mod 2) * 2) Mod Ln1) * 1.33, Posn%(A% Mod Sh, (A% \ Sh Mod Vx + 1) Mod Vx, 1, (A% \ SV2 + (A% \ SV Mod 2) * 2) Mod Ln1)), (Int(Timer \ (3 + (A% Mod Sh) * 1.618)) Mod 15 + 1) * (1 - A% \ SV Mod 2)
        Posn%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2, (A% \ SV2 + 1) Mod Ln1) = Posn%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2, A% \ SV2) + Vec%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2)
        If Posn%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2, (A% \ SV2 + 1) Mod Ln1) < 0 Or Posn%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2, (A% \ SV2 + 1) Mod Ln1) > 479 Then
            Vec%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2) = -Sgn(Vec%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2)) * Int(Rnd * MaxSpeed + 1)
            Posn%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2, (A% \ SV2 + 1) Mod Ln1) = ((Posn%(A% Mod Sh, A% \ Sh Mod Vx, A% \ SV Mod 2, (A% \ SV2 + 1) Mod Ln1) + 240) \ 479) * 479
        End If
    Next
    _Delay 0.01
    '    IF LEN(INKEY$) = 0 THEN GOTO 1
Loop While InKey$ = ""
System
