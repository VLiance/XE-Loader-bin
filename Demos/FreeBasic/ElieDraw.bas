#INCLUDE once "fbgfx.bi"
'#include once "windows.bi"
'#include once "win/mmsystem.bi"



dim shared Siz_Y as integer
Siz_Y = 768'480'680
dim shared Siz_X as integer
Siz_X = 1024'600'1209 
Screenres (Siz_X),(Siz_Y),24,,fb.GFX_FULLSCREEN



Windowtitle "ElieDraw Prototype"

Dim Shared DrawMode As Integer = 0
Dim Shared PencilSize As Integer = 1 
Dim Shared Def As Integer 

Dim CurrentX     As Integer
Dim CurrentY     As Integer

Dim As Ubyte r,g,b
Dim Shared As Uinteger colors(70)
For i As Integer = 0 To 70
    Read r,g,b
    colors(i) = Rgb(r,g,b)
Next i

Dim Shared As Any Ptr  Image_about   'displayed image
    Image_about = Imagecreate(498,117,Rgb(255,255,254))  'save image while screen is being worked on

Dim Shared As Any Ptr Image_set1,Image_set2   'displayed image
Image_set1 = Imagecreate((Siz_X),(Siz_Y),Rgb(255,255,254))  'save image while screen is being worked on
Image_set2 = Imagecreate((Siz_X),(Siz_Y),Rgb(255,255,254))  'saves saved image for UNDO

Dim Shared  As Integer Mekushi_X,Mekushi_Y,OhayoX,OhayoY,YummyB   'mouse variables


Dim Shared ColorPalette As Integer    'id of selected color in palette
ColorPalette = 0                      'black pen default palette#0



Sub object_update()
    Screenlock()
    
    Line (0,0)-(49,165),Rgb(221,139,152),bf
    Draw String (2,8),"DRAW",Rgb(051,051,051)
	Draw String (2,24),"ERASER",Rgb(051,051,051)
	Draw String (2,40),"QUIT",Rgb(051,051,051)
	Draw String (2,56),"ABOUT",Rgb(051,051,051)
	Draw String (2,72),"LOAD",Rgb(051,051,051)
	Draw String (2,88),"SAVE",Rgb(051,051,051)
	Draw String (2,104),"FILL",Rgb(051,051,051)
	Draw String (2,120),"LINE",Rgb(051,051,051)
	Draw String (2,136),"SQUARE",Rgb(051,051,051)
	Draw String (2,152),"",Rgb(051,051,051)
    Line (50,0)-(0,165),Rgb(051,051,051),b '
	
	if Siz_X <= 600 then
	
	
	 For x As Integer = 0 To 35
	 
        Line (x*16,(Siz_Y)-16)-(x*16+15,(Siz_Y)-1),colors(x),bf
        If x = ColorPalette Then
            Line (x*16,(Siz_Y))-(x*16+15,(Siz_Y)-1) ,Rgb(255,255,254),b
            Line (x*16+1,(Siz_Y)-17)-(x*16+15-1,(Siz_Y)-1) ,Rgb(0,0,0),b
        Else
            Line (x*16,(Siz_Y)-16)-(x*16+15,(Siz_Y)) ,Rgb(0,0,0),b
        End If
    Next x
	
	elseif Siz_X >= 601 And Siz_X <=1199 then
	
	
	 For x As Integer = 0 To 50
	 
        Line (x*16,(Siz_Y)-16)-(x*16+15,(Siz_Y)-1),colors(x),bf
        If x = ColorPalette Then
            Line (x*16,(Siz_Y))-(x*16+15,(Siz_Y)-1) ,Rgb(255,255,254),b
            Line (x*16+1,(Siz_Y)-17)-(x*16+15-1,(Siz_Y)-1) ,Rgb(0,0,0),b
        Else
            Line (x*16,(Siz_Y)-16)-(x*16+15,(Siz_Y)) ,Rgb(0,0,0),b
        End If
    Next x
	
	
	elseif Siz_X >= 1200 then
	
	
	 For x As Integer = 0 To 70
	 
        Line (x*16,(Siz_Y)-16)-(x*16+15,(Siz_Y)-1),colors(x),bf
        If x = ColorPalette Then
            Line (x*16,(Siz_Y))-(x*16+15,(Siz_Y)-1) ,Rgb(255,255,254),b
            Line (x*16+1,(Siz_Y)-17)-(x*16+15-1,(Siz_Y)-1) ,Rgb(0,0,0),b
        Else
            Line (x*16,(Siz_Y)-16)-(x*16+15,(Siz_Y)) ,Rgb(0,0,0),b
        End If
    Next x
	
	end if
	
	
	
    Screenunlock()
    Sleep 2
End Sub


Sub LineSize(x1 As Integer,y1 As Integer,x2 As Integer,y2 As Integer,size As Integer,c As Uinteger)
    Dim As Integer x,y
    If x1 = x2 And y1 = y2 Then
        Circle (x1, y1), size, c, , , , f
    Elseif Abs(x2 - x1) >= Abs(y2 - y1) Then
        Dim K As Single = (y2 - y1) / (x2 - x1)
        For I As Integer = x1 To x2 Step Sgn(x2 - x1)
            x = I
            y = K * (I - x1) + y1
            Circle (x,y), size, c, , , , f
        Next I
    Else
        Dim L As Single = (x2 - x1) / (y2 - y1)
        For J As Integer = y1 To y2 Step Sgn(y2 - y1)
            x = L * (J - y1) + x1
            y = J
            Circle (x,y), size,c,,,,f
        Next J
    End If
End Sub

Sub FunctionFill (x As Integer, y As Integer, oldcolour As Integer, newcolour As Integer)
    Dim As Integer Ptr p = New Integer[16*1024 * 1024]
    Dim As Integer n = 0
    Dim As Integer x0, y0

    If oldcolour = newcolour Then Exit Sub
    p[n] = x
    p[n+1] = y
    n = n + 2
    While n > 0
        y0 = p[n-1]
        x0 = p[n-2]
        n = n - 2
        If Point(x0, y0) = oldcolour Then
            Pset (x0, y0), newcolour
            p[n] = x0
            p[n+1] = y0-1
            p[n+2] = x0
            p[n+3] = y0+1
            p[n+4] = x0-1
            p[n+5] = y0
            p[n+6] = x0+1
            p[n+7] = y0
            n = n + 8
        End If
    Wend
    Delete p
End Sub

sub ElieFill()
    FunctionFill  (Mekushi_X,Mekushi_Y,Point(Mekushi_X,Mekushi_Y),colors(ColorPalette))
    object_update()
    'wait for select button release
    While YummyB = 1
                Getmouse Mekushi_X,Mekushi_Y,,YummyB
    Wend
    Put Image_set2,(0,0),Image_set1,Pset    'Image_set2 = Image_set1
    Get (0,0)-((Siz_X)-1,(Siz_Y)-1),Image_set1      'Image_set1 = screen
end sub

sub eliesave()
	        REM Put (0,0),Image_set1,Pset  'copy to screen
			Dim IMG_name As String
            Locate 3,10
            Line (50,0)-(400,32),Rgb(255,255,255),bf
            Line (50,0)-(400,32),Rgb(051,051,051),b
            Line Input "Filename:",IMG_name
	        Bsave IMG_name + ".bmp",Image_set1
	Put (0,0),Image_set1,Pset  'copy to screen
	DrawMode = 0
    object_update()
end sub

Sub PencilDrawing()
    While YummyB=1
        Getmouse Mekushi_X,Mekushi_Y,,YummyB
        If Mekushi_X<>OhayoX Or Mekushi_Y<>OhayoY Then 'mouse has moved so draw erase old draw new
            LineSize(OhayoX,OhayoY,Mekushi_X,Mekushi_Y,PencilSize,colors(ColorPalette)) 'drawline onto screenBuffer
            OhayoX = Mekushi_X
            OhayoY = Mekushi_Y
            object_update()
        End If
        Sleep 1
    Wend
    Put Image_set2,(0,0),Image_set1,Pset    'Image_set2 = Image_set1
    Get (0,0)-((Siz_X)-1,(Siz_Y)-1),Image_set1       'Image_set1 = screen

End Sub

sub intro()
 'var file= "intro.wav"
 'sndPlaySound(file,0) '0 for a single run, 9 for continuous runs
end sub


Sub ElieLine()
Dim As Integer Nsx,Nsy
    Nsx = Mekushi_X
    Nsy = Mekushi_Y
    While YummyB=1
        Getmouse Mekushi_X,Mekushi_Y,,YummyB
        If Mekushi_X<>OhayoX Or Mekushi_Y<>OhayoY Then 'mouse has moved so draw erase old draw new
            Put (0,0),Image_set1,Trans   'restore screen
            LineSize(Nsx,Nsy,Mekushi_X,Mekushi_Y,PencilSize,colors(ColorPalette)) 'drawline onto screenBuffer
            OhayoX = Mekushi_X
            OhayoY = Mekushi_Y
            object_update()
        End If
    Wend
    Put Image_set2,(0,0),Image_set1,Pset    'Image_set2 = Image_set1
    Get (0,0)-((Siz_X)-1,(Siz_Y)-1),Image_set1      'Image_set1 = screen
End Sub

Sub ElieSquare()
    Dim As Integer r
    Dim As String s
    While YummyB=1
        Getmouse Mekushi_X,Mekushi_Y,,YummyB
        If Mekushi_X<>OhayoX Or Mekushi_Y<>OhayoY Then 'mouse has moved so draw erase old draw new
            Screenlock()
            Put (0,0),Image_set1,Trans   'restore screen

            'fill rectangle mode?
            'OhayoX,OhayoY,Mekushi_X,Mekushi_Y
            LineSize(OhayoX,OhayoY,Mekushi_X,OhayoY,PencilSize,colors(ColorPalette))
            LineSize(Mekushi_X,OhayoY,Mekushi_X,Mekushi_Y,PencilSize,colors(ColorPalette))
            LineSize(Mekushi_X,Mekushi_Y,OhayoX,Mekushi_Y,PencilSize,colors(ColorPalette))
            LineSize(OhayoX,Mekushi_Y,OhayoX,OhayoY,PencilSize,colors(ColorPalette))
            Screenunlock()
            object_update()
            Sleep 1
        End If
    Wend
    Put Image_set2,(0,0),Image_set1,Pset    'canvas2 = canvas1
    Get (0,0)-((Siz_X)-1,(Siz_Y)-1),Image_set1       'canvas1 = screen
End Sub

Sub eraser()
    Dim As Double r
    Dim As Integer x,y,cx,cy
    While YummyB=1
        Getmouse Mekushi_X,Mekushi_Y,,YummyB
        If Mekushi_X<>OhayoX Or Mekushi_Y<>OhayoY Then 'mouse has moved so draw erase old draw new
            Line(OhayoX,OhayoY)-(OhayoX+10,OhayoY+10),Rgb(255,255,254),bf  'erase bOhayoX outline
            Line(Mekushi_X,Mekushi_Y)-(Mekushi_X+10,Mekushi_Y+10),Rgb(200,200,200),bf
            Line(Mekushi_X,Mekushi_Y)-(Mekushi_X+10,Mekushi_Y+10),Rgb(051,051,051),b         'draw bOhayoX outline
            OhayoX = Mekushi_X
            OhayoY = Mekushi_Y
            object_update()
        End If
    Wend
    Line(OhayoX,OhayoY)-(OhayoX+10,OhayoY+10),Rgb(255,255,254),bf  'erase bOhayoX outline
    Put Image_set2,(0,0),Image_set1,Pset    'Image_set2 = Image_set1
    Get (0,0)-((Siz_X)-1,(Siz_Y)-1),Image_set1       'Image_set1 = screen
End Sub

Sub about()
    Bload "eliedraw.bmp",Image_about
	 Dim MidX     As Integer = (Siz_X /2) - 250
	 Dim Siz_BOX    As Integer = MidX + 500
	 
    Line (MidX,140)-(Siz_BOX,257),Rgb(255,255,254),bf
	Put (MidX + 1,141),Image_about,Pset
    Line (MidX,140)-(Siz_BOX,257),Rgb(1,1,1),b
	 Draw String (MidX + 150,150),"ElieDraw 0.1.8 Alpha",Rgb(051,051,051)
	 Draw String (MidX + 150,166),"Made by Meaxy76",Rgb(051,051,051)
	 
	 Dim CurrentX     As Integer
     Dim CurrentY     As Integer 
	 Getmouse CurrentX, CurrentY
	    If CurrentY>=140 And CurrentY <=257 Then
			       If CurrentX >= 60 And CurrentX <=560 Then
				   Put (0,0),Image_set1,Pset
				    DrawMode = 0
				    'object_update()
	               end if       
		End If   
	 
End Sub

Sub elieload()
	Dim IMG_name As String
	

    Locate 3,10
    Line (50,0)-(400,32),Rgb(255,255,255),bf
    Line (50,0)-(400,32),Rgb(051,051,051),b
    Line Input "Filename:",IMG_name
	
Image_set1 = Imagecreate((Siz_X),(Siz_Y),Rgb(255,255,254))  'save image while screen is being worked on
	BLOAD IMG_name + ".bmp",Image_set1 
	
	If Image_set1 = False Then
	
	
                   Print "ElieDraw ERR_001";
	
    Else
    Color Rgb(0,0,0),Rgb(255,255,254)  'black ink, white paper
    Cls   'executes the color change
	Put (0,0),Image_set1,Pset  'copy to screen
	'pset (0, 0), 
	DrawMode = 0
	end if
    object_update()
End Sub

Color Rgb(0,0,0),Rgb(255,255,254)  'black ink, white paper
Cls   'executes the color change

object_update()
intro()

Dim As String key
Do

    Getmouse Mekushi_X,Mekushi_Y,,YummyB
	Getmouse CurrentX, CurrentY
    OhayoX = Mekushi_X
    OhayoY = Mekushi_Y


    If YummyB = 1 Then

        'is it over drawing area?
        If  CurrentY<=16 Then
		   If CurrentX >= 0 And CurrentX <=40 Then
				DrawMode = 0
			end if
		end if
	    If CurrentY>=16 And CurrentY <=32 Then
			       If CurrentX >= 0 And CurrentX <=40 Then
				   DrawMode = 1
	               end if       
		End If   
		    
		If CurrentY>=32 And CurrentY <=48 Then
			       If CurrentX >= 0 And CurrentX <=40 Then
                   Locate 12, 30
                   Print "Sayonara!";
				   sleep 2
				   Def = 3
	               end if
		End If   
		
		If CurrentY>=48 And CurrentY <=64 Then
			       If CurrentX >= 0 And CurrentX <=40 Then
				   DrawMode = 2
	               end if
		End If   
		
		If CurrentY>=64 And CurrentY <=80 Then
			       If CurrentX >= 0 And CurrentX <=40 Then
				   DrawMode = 3
	               end if
		End If

        If CurrentY>=80 And CurrentY <=96 Then
			       If CurrentX >= 0 And CurrentX <=40 Then
				   DrawMode = 4
	               end if
		End If	
		
        If CurrentY>=96 And CurrentY <=112 Then
			       If CurrentX >= 0 And CurrentX <=40 Then
				   DrawMode = 5
	               end if
		End If		

        If CurrentY>=112 And CurrentY <=128 Then
			       If CurrentX >= 0 And CurrentX <=40 Then
				   DrawMode = 6
	               end if
		End If				
		
		If CurrentY>=128 And CurrentY <=144 Then
			       If CurrentX >= 0 And CurrentX <=40 Then
				   DrawMode = 7
	               end if
		End If	
		
		    If DrawMode = 0 Then
                PencilDrawing()
            End If
            If DrawMode = 1 Then
                eraser()
            End If
		     If DrawMode = 2 Then
                about()
            End If
			
			 If DrawMode = 3 Then
                elieload()
            End If
			
			If DrawMode = 4 Then
                eliesave()
            End If
			
			If DrawMode = 5 Then
                ElieFill()
            End If
			
			If DrawMode = 6 Then
                ElieLine()
            End If
			
			If DrawMode = 7 Then
                ElieSquare()
            End If
			
			
		
        
		
		If Mekushi_Y<16 Then

            Def = Mekushi_Y\16
			
			While YummyB = 1
                Getmouse Mekushi_X,Mekushi_Y,,YummyB
            Wend
			
			If Def<3 Then
                DrawMode = Def
            End If

            object_update()

        End If
        'is it over pallete?
        If Mekushi_Y>(Siz_Y)-16 Then
            ColorPalette = Mekushi_X\16
            While YummyB = 1
                Getmouse Mekushi_X,Mekushi_Y,,YummyB
            Wend
        End If
        object_update()
    End If
	
Loop Until Def = 3
			

Imagedestroy(Image_set1)
Imagedestroy(Image_set2)



ColorData:

' === ElieDraw default standard colors ===
Data 0  ,0  ,  0  'BLACK
data 051,051,051
Data 127,127,127  'dark gray
Data 195,195,195  'light gray
Data 255,255,254  'WHITE
Data 136,  0, 21  'red brown
Data 185,122, 87  'brown
Data 237, 28, 36  'red
Data 255,174,201  'pink
Data 255,127, 39  'orange
DATA 236,212,165
Data 255,201, 14  'deep yellow gold
Data 255,242,  0  'yellow
Data 239,228,176  'light yellow
Data  34,177, 76  'green
Data 181,230, 29  'lime
Data   0,162,232  'turquoise  medium blue
Data 153,217,234  'light blue
Data  63, 72,204  'indigo dark blue
Data 112,146,190  'blue gray
Data 163, 73,164  'purple
Data 200,191,231  'lavenda
'=====================================
Data 255,128,128
Data 255,  0,  0
Data 128, 64, 64
Data 128,  0,  0

Data 255,255,128 'yellow
Data 255,255,  0
Data 255,128, 64 'orange
Data 255,128,  0
Data 128, 64,  0 'brown
Data 128,128,  0

Data 128,255,128  'green
Data 128,255,  0
Data   0,255,  0
Data   0,128,  0
Data   0, 64,  0
Data 128,128, 64

Data   0,255,128
Data   0,255, 64
Data   0,128,128
Data   0,128, 64
Data   0, 64, 64
Data 128,128,128  'gray

Data 128,255,255  'blue
Data   0,255,255
Data   0, 64,128
Data   0,  0,255
Data   0,  0,128
Data  64,128,128

Data   0,128,255
Data   0,128,192
Data 128,128,255
Data   0,  0,160
Data   0,  0, 64
Data 192,192,192  'gray

Data 255,128,192  'red
Data 128,128,192
Data 128,  0, 64
Data 128,  0,128  'purple
Data  64,  0, 64
Data  64,  0,128  'black

Data 255,128,255
Data 255,  0,255
DATA 238,179,218
Data 255,227,204
Data 255,  0,128
Data 128,  0,255
Data 97,  72,144
Data 64,   0,128
Data 255,255,254  'white

