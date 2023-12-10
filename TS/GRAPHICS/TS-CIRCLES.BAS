'   ______   ______
'  /\__  _\ /\  ___\
'  \/_/\ \/ \ \___  \
'     \ \_\  \/\_____\
'      \/_/   \/_____/
'
' TS-CIRCLES.BAS
'
' (C) 2023 TS. MIT. ALL RIGHTS RESERVED.
'
$ExeIcon:'C:\QB64\TS\TS.ICO'
_Icon
_ScreenMove _Middle
'_FullScreen
_Title "CIRCLES"
Cls
Screen 12
Randomize Timer
'sets the palette
'sets randomness of palette

pal = Int(Rnd * 5) + 1

If pal = 2 Then pal = 256 'green

If pal = 3 Then pal = 257 'yellow

If pal = 4 Then pal = 65536 'blue

If pal = 5 Then pal = 65537 'purple

       

'FOR i = 60 TO 1 STEP -1 'bright in back

For i = 1 To 60 Step 4 'bright in front

    x = x + 1

    Palette x, i * pal

Next

'PALETTE 1, 0



'initiation stuff



size = 23

delay = 5000

winX = 160

winY = 240

accel = .8

maxsp = size / 2

minsp = -maxsp



Dim x(size), y(size)

Dim xx(size), yy(size), nx(size), ny(size)

Dim col(size)

Dim r(size)

Dim t(size), v(size), u(size)





'stuffs the arrays



maxX = 640 - winX

maxY = 480 - winY

x = maxX

y = maxY

For i = 1 To size

    xx(i) = minsp

    yy(i) = minsp

    x = x - 400 / size

    y = y - 400 / size

    x(i) = x

    y(i) = y

    r(i) = i

    col(i) = i

Next



Do



    'delay



    For i = 1 To delay Step 1: Next



    For i = 1 To size



        'circles that erase



        Circle (t(i), v(i)), u(i), 0

        'CIRCLE (640 - t(i), 480 - v(i)), u(i), 0

        'CIRCLE (t(i), 480 - v(i)), u(i), 0

        'CIRCLE (640 - t(i), v(i)), u(i), 0



        'circles that draw



        'this is the real one, the rest are mirrors

        Circle (x(i), y(i)), r(i), col(i)

       

        'CIRCLE (640 - x(i), 480 - y(i)), r(i), col(i)

        'CIRCLE (x(i), 480 - y(i)), r(i), col(i)

        'CIRCLE (640 - x(i), y(i)), r(i), col(i)



        If i < size Then



            'first line is the real one, the rest are mirrors.  No eraser lines are

            'needed because the tail color is black.



            'LINE (x(i), y(i))-(x(i + 1), y(i + 1)), col(i)

            'LINE (640 - x(i), 480 - y(i))-(640 - x(i + 1), 480 - y(i + 1)), col(i)

            'LINE (640 - x(i), y(i))-(640 - x(i + 1), y(i + 1)), col(i)

            'LINE (x(i), 480 - y(i))-(x(i + 1), 480 - y(i + 1)), col(i)



            'connects the lines for some spiffy effects



            'LINE (640 - x(i), y(i))-(640 - x(i + 1), 480 - y(i + 1)), col(i)

            'LINE (x(i), y(i))-(640 - x(i + 1), 480 - y(i + 1)), col(i)

            'LINE (x(i), y(i))-(x(i + 1), 480 - y(i + 1)), col(i)

            'LINE (x(i), 480 - y(i))-(640 - x(i + 1), y(i + 1)), col(i)



        End If



        'sets eraser variables



        t(i) = x(i): v(i) = y(i): u(i) = r(i)



    Next



    'the big bad movement phase



    For i = 1 To size

        'the way this works is the leader object is moved according to these

        'conditions, and then the followers just move along in a line



        If i = size Then

            'accelerates, checking for maxsp and minsp



            xx(i) = xx(i) + nx(i)

            yy(i) = yy(i) + ny(i)

            If xx(i) > maxsp Then xx(i) = maxsp

            If xx(i) < minsp Then xx(i) = minsp

            If yy(i) > maxsp Then yy(i) = maxsp

            If yy(i) < minsp Then yy(i) = minsp



            'moves the leader object, checking to see if it is in the

            'window



            x(i) = x(i) + xx(i)

            y(i) = y(i) + yy(i)

            If x(i) > maxX Then nx(i) = -Rnd * accel - accel

            If x(i) < winX Then nx(i) = Rnd * accel + accel

            If y(i) > maxY Then ny(i) = -Rnd * accel - accel

            If y(i) < winY Then ny(i) = Rnd * accel + accel

        End If



        'lets the followers follow



        If i < size Then

            x(i) = x(i + 1)

            y(i) = y(i + 1)

        End If



    Next


    _Delay 0.1
Loop Until InKey$ <> ""
System


