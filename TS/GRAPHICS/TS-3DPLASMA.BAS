'3D plasma, coded by Ashish  14 June, 2018
'Twitter : @KingOfCoders
'http://lodev.org/cgtutor/plasma.html

_Title "3D Plasma"
Screen _NewImage(600, 600, 32)


Declare Library
    'for camera
    Sub gluLookAt (ByVal eyeX#, Byval eyeY#, Byval eyeZ#, Byval centerX#, Byval centerY#, Byval centerZ#, Byval upX#, Byval upY#, Byval upZ#)
End Declare

Dim Shared mapSize
mapSize = 100
Dim Shared glAllow As _Byte, textureImage&(258), tmp_buffer_image&
Dim Shared tmp_height_map&, height_map_buffer As _MEM 'New height maps

tmp_buffer_image& = _NewImage(mapSize, mapSize, 32)
tmp_height_map& = _NewImage(mapSize, mapSize, 32) 'this image will be treated as height map
height_map_buffer = _MemImage(tmp_height_map&) 'the data in above image will access by this _MEM buffer


_Dest tmp_buffer_image&
'storing calculation in memory for faster rendering
Dim sin1(_Width - 1, _Height - 1), sin2(_Width - 1, _Height - 1), sin3(_Width - 1, _Height - 1)
For y = 0 To _Height - 1
    For x = 0 To _Width - 1
        sin1(x, y) = Sin(Sqr(x ^ 2 + y ^ 2) * .09)
        sin2(x, y) = Sin(y * .03)
        sin3(x, y) = Cos(((_Width / 2 - x) ^ 2 + (_Height / 2 - y) ^ 2) ^ .5 * .07)
Next x, y

Do
    _Dest 0
    Cls
    Print "Generating Textures "; f; "/"; UBound(textureImage&) - 1
    f = f + 1
    For y = 0 To _Height(tmp_buffer_image&) - 1
        For x = 0 To _Width(tmp_buffer_image&) - 1
            col = sin1(x, y) * 64 + sin2(x, y) * 64 + sin3(x, y) * 64 + 255 + f
            col2 = col Mod 255
            _Dest tmp_buffer_image&
            PSet (x, y), hsb(col2, 255, 128, 255)
    Next x, y
    textureImage&(f) = _CopyImage(tmp_buffer_image&)
Loop Until f > UBound(textureImage&) - 1
_Dest 0
_FreeImage tmp_buffer_image&


_GLRender _OnTop

_Dest tmp_height_map&
glAllow = -1
Do
    f = f + 1
    For y = 0 To _Height - 1
        For x = 0 To _Width - 1
            col = sin1(x, y) * 64 + sin2(x, y) * 64 + sin3(x, y) * 64 + 255 + f
            col = Sin(col * .01) * 64 + 128
            PSet (x, y), _RGB(col, col, col)
    Next x, y
Loop

Sub _GL ()
    $Checking:Off
    Static cubeTexture&(257), glSetup
    Static aspect#, frame


    If Not glAllow Then Exit Sub

    If Not glSetup Then
        glSetup = -1
        _glViewport 0, 0, _Width, _Height
        'Convert all images to GL textures
        For i = 1 To UBound(textureImage&) - 1
            _glGenTextures 1, _Offset(cubeTexture&(i))
            Dim m As _MEM
            m = _MemImage(textureImage&(i))

            _glBindTexture _GL_TEXTURE_2D, cubeTexture&(i)
            _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGB, _Width(textureImage&(i)), _Height(textureImage&(i)), 0, _GL_BGRA_EXT, _GL_UNSIGNED_BYTE, m.OFFSET

            _MemFree m

            _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR
            _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
            _FreeImage textureImage&(i)
        Next
        aspect# = _Width / _Height
    End If

    _glClearColor 0, 0, 0, 1
    _glClear _GL_DEPTH_BUFFER_BIT Or _GL_COLOR_BUFFER_BIT

    _glEnable _GL_TEXTURE_2D
    _glEnable _GL_DEPTH_TEST

    _glMatrixMode _GL_PROJECTION
    _glLoadIdentity
    _gluPerspective 45.0, aspect#, 1, 100

    _glMatrixMode _GL_MODELVIEW
    _glLoadIdentity

    _glShadeModel _GL_SMOOTH

    gluLookAt 0, 0, 4, 0, 0, 0, 0, 1, 0

    i = (frame Mod (UBound(textureImage&) - 1)) + 1

    'select our texture
    _glBindTexture _GL_TEXTURE_2D, cubeTexture&(i)

    'rotation
    _glRotatef -45, 1, 0, 0

    drawPlane 2, 2, .05, height_map_buffer

    frame = frame + 1
    $Checking:On
End Sub

Sub drawPlane (w, h, detail, height_map As _MEM)

    'texture coordinates
    tx1 = 0: ty1 = 0
    tx2 = 0: ty2 = 0

    depth1 = 0 'used for depth effect by using height maps
    depth2 = 0

    hx1% = 0: hy1% = 0
    hx2% = 0: hy2% = 0
    _glBegin _GL_TRIANGLE_STRIP
    For y = -h / 2 To h / 2 - detail Step detail
        For x = -w / 2 To w / 2 - detail Step detail
            tx1 = map(x, -w / 2, w / 2, 0, 1)
            ty1 = map(y, -h / 2, h / 2, 1, 0)
            ty2 = map(y + detail, -h / 2, h / 2, 1, 0)

            hx1% = map(tx1, 0, 1, 1, mapSize - 1)
            hy1% = map(ty1, 0, 1, mapSize - 1, 1)
            hy2% = map(ty2, 0, 1, mapSize - 1, 1)

            depth1 = _MemGet(height_map, height_map.OFFSET + memImageIndex(hx1%, hy1%, mapSize), _Unsigned _Byte) / 400
            depth2 = _MemGet(height_map, height_map.OFFSET + memImageIndex(hx1%, hy2%, mapSize), _Unsigned _Byte) / 400

            _glTexCoord2f tx1, ty1
            _glVertex3f x, y, depth1
            _glTexCoord2f tx1, ty2
            _glVertex3f x, y + detail, depth2
        Next x
    Next y

    _glEnd

End Sub

Function memImageIndex& (x, y, w)
    memImageIndex& = (x + y * w) * 4
End Function


Function map! (value!, minRange!, maxRange!, newMinRange!, newMaxRange!)
    map! = ((value! - minRange!) / (maxRange! - minRange!)) * (newMaxRange! - newMinRange!) + newMinRange!
End Function

'method adapted form http://stackoverflow.com/questions/4106363/converting-rgb-to-hsb-colors
Function hsb~& (__H As _Float, __S As _Float, __B As _Float, A As _Float)
    Dim H As _Float, S As _Float, B As _Float

    H = map(__H, 0, 255, 0, 360)
    S = map(__S, 0, 255, 0, 1)
    B = map(__B, 0, 255, 0, 1)

    If S = 0 Then
        hsb~& = _RGBA32(B * 255, B * 255, B * 255, A)
        Exit Function
    End If

    Dim fmx As _Float, fmn As _Float
    Dim fmd As _Float, iSextant As Integer
    Dim imx As Integer, imd As Integer, imn As Integer

    If B > .5 Then
        fmx = B - (B * S) + S
        fmn = B + (B * S) - S
    Else
        fmx = B + (B * S)
        fmn = B - (B * S)
    End If

    iSextant = Int(H / 60)

    If H >= 300 Then
        H = H - 360
    End If

    H = H / 60
    H = H - (2 * Int(((iSextant + 1) Mod 6) / 2))

    If iSextant Mod 2 = 0 Then
        fmd = (H * (fmx - fmn)) + fmn
    Else
        fmd = fmn - (H * (fmx - fmn))
    End If

    imx = _Round(fmx * 255)
    imd = _Round(fmd * 255)
    imn = _Round(fmn * 255)

    Select Case Int(iSextant)
        Case 1
            hsb~& = _RGBA32(imd, imx, imn, A)
        Case 2
            hsb~& = _RGBA32(imn, imx, imd, A)
        Case 3
            hsb~& = _RGBA32(imn, imd, imx, A)
        Case 4
            hsb~& = _RGBA32(imd, imn, imx, A)
        Case 5
            hsb~& = _RGBA32(imx, imn, imd, A)
        Case Else
            hsb~& = _RGBA32(imx, imd, imn, A)
    End Select

End Function


