'  ____                            ____                                    _        
' |  _ \                          |  _ \                                  ( )       
' | |_) |  _   _    __ _   ___    | |_) |  _   _   _ __    _ __    _   _  |/   ___  
' |  _ <  | | | |  / _` | / __|   |  _ <  | | | | | '_ \  | '_ \  | | | |     / __| 
' | |_) | | |_| | | (_| | \__ \   | |_) | | |_| | | | | | | | | | | |_| |     \__ \ 
' |____/   \__,_|  \__, | |___/   |____/   \__,_| |_| |_| |_| |_|  \__, |     |___/ 
'                   __/ |                                           __/ |           
'                  |___/                                           |___/            
'  ____    _          _     _           _                     ____            _   _ 
' |  _ \  (_)        | |   | |         | |                   |  _ \          | | | |
' | |_) |  _   _ __  | |_  | |__     __| |   __ _   _   _    | |_) |   __ _  | | | |
' |  _ <  | | | '__| | __| | '_ \   / _` |  / _` | | | | |   |  _ <   / _` | | | | |
' | |_) | | | | |    | |_  | | | | | (_| | | (_| | | |_| |   | |_) | | (_| | | | | |
' |____/  |_| |_|     \__| |_| |_|  \__,_|  \__,_|  \__, |   |____/   \__,_| |_| |_|
'                                                    __/ |                          
'                                                   |___/                                          
'                                  
'Bugs Bunny's Birthday Ball (Bally 1991)for VP10.6 Final Release or >                                                                                    
'Initial table design and scripting by "Bodydump" and completed by "wrd1972"
'Graphics Remaster by "Brad1X"
'Plastics primitives, mesh playfields and other 3D modeling by "Cyberpez"
'Additional scripting assistance by "cyberpez" and "Rothbauerw"
'Clear ramp primitives and sidewalls by "Flupper", "Bord"
'Lighting scripting, and Looney Tunes dome decals by "nfozzy" 
'Lighting by "wrd1972"
'Physics by "wrd1972"
'Looney Tunes Domes and Henhouse primitives by "Zany"
'DT table scoring reels and DT backdrop by "32Assassin"
'DOF by "Arngrim"
'Sound enhancements by "Rusty Cardores" and "DJRobX"
'Flipper trajectory fix by "nFozzy, Rothbauer"
'Upscaled playfield image by "Sheltemke"
'Positional sounds and additional sound scripting help by Thalamus
'Flipper, rubber, bumper, and other MISC collision sounds by "Fleep, nickbuol, CalleV, Nicolas Mazaleyrat, Jon Osborne"
'VR Room options and misc fixes by "Sixtoe", original minimal room by "Pgheyd"

'Very special thanks to "Bodydump" for allowing me to complete his table
'Many thanks to countless others in the VPF community for helping me with the development of this table.
' 
' - Baseline version from authors above.
'
' Version 2.4 - TastyWasps
' - Completed Fleep sound package
' - Added VPW Rolling Ball sounds
' - Added nFozzy Rolling Ramp sounds
'
' Version 2.5b/c/d/e - TastyWasps, Wylte
' - Fixed left saucer issue
' - Added Dynamic Ball Shadows
' - Quick DBS fixes
' - Fixed DOF for Slingshots
' - Fixed POV for FS
'
' Version 2.6 - Fluffhead, Wylte
' - Brand new, more realistic, ballshadow routines
' - Fixed some rubbers missing materials

' Version 2.7 - Fluffhead
' - Added Trigger to turn off ball reflection that was bleeding through playfield_mesh
' Version 2.8

' - Fixed the spinner so that it gets correct sound and is tied to sw 23
' - Fixed Knocker to remove the knocker position from subroutine call.

' Version 2.8a
' - Added LUT swap for when GI is turned offset
' - Lighting fixes

' Version 2.9
' - Added lighting/flashers to VR Backglass
' - Fixed Alphanumerics in VR
' - Added Cabinet blades
' - Slight fixes to cabinet artwork
' - Animated flipper buttons

'2.9b_UV3 - iaakki - new baked wamps with additive lighting.
'RC3 - iaakki - add AO shadows for both PF's
'RC5 - Primetime5k - Add staged flipper support

'****************************************************************************************************************************************************
'****************************************************************************************************************************************************
Option Explicit
Randomize
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
'****************************************************************************************************************************************************
'****************************************************************************************************************************************************
'  _______           _       _             ____            _     _                       
' |__   __|         | |     | |           / __ \          | |   (_)                      
'    | |      __ _  | |__   | |   ___    | |  | |  _ __   | |_   _    ___    _ __    ___ 
'    | |     / _` | | '_ \  | |  / _ \   | |  | | | '_ \  | __| | |  / _ \  | '_ \  / __|
'    | |    | (_| | | |_) | | | |  __/   | |__| | | |_) | | |_  | | | (_) | | | | | \__ \
'    |_|     \__,_| |_.__/  |_|  \___|    \____/  | .__/   \__| |_|  \___/  |_| |_| |___/
'                                                 | |                                    
'                                                 |_|                                    
'INTRO MUSIC
'   Play Intro Music Snippet = 0
'   No Intro Music Snippet = 1
'Change the value below to set option
IntroMusic = 1


'GI COLOR MOD - Choose your own custom color for General Illumination
	'Primary Colors
		'Red = 255, 0, 0
		'Green = 0, 255, 0
		'Blue = 0, 0, 255
		'Incandescent = 255, 197, 43
		'Warm White = 255, 197, 100
		'Cool White = 255, 255, 255
		'Refer to https://rgbcolorcode.com for customized color codes
'Enter RGB values below for "GI" color
GIColorRed       =  255
GIColorGreen     =  197
GIColorBlue      =  143


'SHADOW OPTIONS
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

'VR Room
'	VR Room Off = 0
'	VR Room On = 1 **TURN OFF SIDE RAILS BELOW**
VRRoom = 0


'SIDE RAILS 
'   Hide Side Rails = 0
'   Show Side Rails = 1
SideRails = 1


'PLAYFIELD SHADOW INTENSITY (adds additional visual depth) 
'Usable range is 0 (lighter) - 100 (darker) 	 	
ShadowIntensity = 1


'----- General Sound Options -----
Const VolumeDial = 0.8				' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 			' Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			' Level of ramp rolling volume. Value between 0 and 1

'****************************************************************************************************************************************************
'****************************************************************************************************************************************************
Const cGameName="bbnny_l2",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="fx_Solenoid",SSolenoidOff="fx_SolOff", SCoin="fx_Coin"
Const ballsize = 25  'radius
Const ballmass = 1
Const tnob = 2	' Total number of balls
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
LoadVPM "01560000", "S11.VBS", 3.26

Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
	pRailLeft.visible = 1
	pRailRight.visible = 1
	'SideWalls.visible = 1
	'SideWalls.Size_y=1.2
Else
	pRailLeft.visible = 0
	pRailRight.visible = 0
	'SideWalls.visible = 0
	'SideWalls.Size_y=2
End if

'****************************************************************************************************************************************************
'Solenoid Call backs
'****************************************************************************************************************************************************
	SolCallback(1) = "SolOuthole"
	SolCallback(2) = "ReleaseBall"
	SolCallBack(5) = "kickSaucer"
	SolCallBack(6) = "ResetDrops" 'Drop Target Reset
	SolCallBack(7) = "SolKnocker"
	SolCallBack(13) = "TopKick"  'ball launcher
	SolCallback(14) = "Sol14_Solenoid"  'Left outlane kickout
	SolCallback(sLRFlipper) = "SolRFlipper"
	SolCallback(sLLFlipper) = "SolLFlipper"
	SolCallback(sURFlipper) = "SolURFlipper"


'GI	
	SolCallback(10) = "SetGI" 'PF GI solenoid
	SolCallback(11) = "SetBBGI"

	'SolCallback(10) = "PlayRelay"
	'SolCallback(11) =  'Insert Backbox GI solenoid

'Looney & Tunes Relays
'	SolCallback(9) = "SetLooney" 'Looney solenoid
'	SolCallback(16) = "SetTunes" 'Tunes solenoid

'Flashers
	'SolCallBack(8) =  					'Right BackPannel Flash
	SolCallBack(9)  = "Flashlooney" 	'Looney solenoid
	SolCallBack(16) = "Flashtunes" 	    'Tunes solenoid
	SolCallback(25) = "SetLamp 125," 	'Left Ramp Flasher
	SolCallback(26) = "SetLamp 126,"    'Left Target Flasher   	BG Top Left Sam Shot
	SolCallBack(27) = "SetLamp 127,"  	'Millions Flasher		BG Cake
	SolCallback(28) = "SetLamp 128," 	'Taz Flasher			BG Speaker Right
	SolCallback(29) = "SetLamp 129," 	'Right Target Flasher	BG Speaker Center
	SolCallBack(30) = "SetLamp 130," 	'Bugs Light				BG Speaker Left  "Sol30"        
	SolCallBack(31) = "SetLamp 131,"    'Back Wall Left			BG Top Right Road Runner
	SolCallBack(32) = "SetLamp 132," 	'Back Wall Right		BG Bottom Right Taz

'***********************************************************
'Gates, Spinner connecting rod prim animations
'***********************************************************
Sub UpdateGatesSpinners
End Sub

Sub GameTimer_Timer()
	pSw15.RotX = Sw15.currentangle
	pSw41.RotX = Sw41.currentangle
	Cor.Update	' Update ball tracking (this sometimes goes in the RDampen_Timer sub)
	RollingUpdate
	FlipperTimer
	DisplayTimer

'Spinner connecting rod animation by cyberpwz
   UpdateGatesSpinners
    pSpinnerRod.TransX = sin( (Spinner1.CurrentAngle+180) * (2*PI/360)) * 5
    pSpinnerRod.TransY = sin( (Spinner1.CurrentAngle- 90) * (2*PI/360)) * 5
	If VRRoom > 0 Then
		dim VRFLobj
		If BGBright.visible = 0 Then
			For each VRFLobj in VRBGFLCenter : VRFLobj.opacity = 95 : Next
			For each VRFLobj in VRBGFLArea : VRFLobj.opacity = 130 : Next
		Else
			For each VRFLobj in VRBGFLCenter : VRFLobj.opacity = 40 : Next
			For each VRFLobj in VRBGFLArea : VRFLobj.opacity = 75 : Next
		End If
	End If
End Sub


'Spinner Brake
sub SpinnerBrake_Hit
	ActiveBall.velx = ActiveBall.velx * .7  '.7 = amount of braking
	ActiveBall.vely = ActiveBall.vely * .7  '.7 = amount of braking
end sub

Sub SolKnocker(Enabled)
	If Enabled Then
		KnockerSolenoid
	End If
End Sub
 


'******************************************************
'					FLIPPERS
'******************************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
        
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If                
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper

		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		UpperFlipper.RotateToEnd
		If UpperFlipper.currentangle > UpperFlipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight UpperFlipper

		Else 
			SoundFlipperUpAttackRight UpperFlipper
			RandomSoundFlipperUpRight UpperFlipper

		End If
	Else
		UpperFlipper.RotateToStart
		If UpperFlipper.currentangle > UpperFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight UpperFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 0.5
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub


'******************************************************
'		FLIPPERS PRIMS & SHADOWS
'******************************************************
sub FlipperTimer()
	pleftFlipper.roty=leftFlipper.CurrentAngle -123
	pUpperFlipper.roty=UpperFlipper.CurrentAngle +60
	prightFlipper.roty=rightFlipper.CurrentAngle +123

'    LFLogo.RotZ = LeftFlipper.CurrentAngle
'	ULFLogo.RotZ = UpperFlipper.CurrentAngle
'    RFLogo.RotZ = RightFlipper.CurrentAngle
end sub




'**********************************************************************************************************
'Initialize Table
'**********************************************************************************************************
Dim bsTrough, bsTopKick, bsHoleKick, PlungerIM, DTbank1
Dim cBall1, cBall2, gBOT
Dim bsDict
Set bsDict = New cvpmDictionary
Const bsNone = "None"
Const bsWire = "Wire"
Const bsRamp = "Ramp"
Const bsRampClear = "Clear"


Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Bugs Bunny Birthday Ball"&chr(13)&""
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
      On Error Resume Next
  
  	  Controller.Run
  	If Err Then MsgBox Err.Description
      On Error Goto 0
      End With

	PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

	'Nudging
    vpmNudge.TiltSwitch= 1
    vpmNudge.Sensitivity=5
    vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

'***Trough Initialize (cyberpez)
	set cball1 = sw11.CreateSizedballWithMass(Ballsize,Ballmass)
	set cball2 = sw12.CreateSizedballWithMass(Ballsize,Ballmass)
		'*** Use gBOT in the script wherever BOT is normally used. Then there is no need for GetBalls calls ***
	gBOT = Array(cball1, cball2)
	bsDict.Add cball1.ID, bsNone
	bsDict.Add cball2.ID, bsNone

	Controller.Switch(11) = 1
	Controller.Switch(12) = 1

'***Drop Target Initialize
Set DTbank1 = New cvpmDropTarget
	DTbank1.InitDrop Array(sw20, sw21, sw22), Array(20,21,22)
	DTbank1.InitSnd "",SoundFX("fx_DropTargetReset",DOFContactors)

'***Captive Ball Location Randomizer (cyberpez)
	Dim RandomBallKick	 
		RandomBallKick = Int(Rnd*2)
		If RandomBallKick = 1 then 
			Kicker2a.CreateBall
			Kicker2a.Kick 0,3
	Else
			Kicker2b.CreateBall
			Kicker2b.Kick 180,3
	End If
			Kicker2a.enabled = 0
			Kicker2b.enabled = 0

	'To determine coordinates
	'debug.print flasher.x
	'debug.print flasher.y
	'debug.print flasher.height





'	Shadow.x = 473.5771
'	Shadow.y = 1080.1
'	Shadow.height = .01

	'***Loney Tunes Domes Decals Positions
	Looney_L.x = 289.1  'Looney L
	Looney_L.y = 1184   
	Looney_L.height = 92
	Looney_L.rotx = -37.36	
	Looney_L.roty = 78.5 
	Looney_L.rotz = -52.64  
	Looney_O.x = 289.41  'Looney O   
	Looney_O.y = 1184  
	Looney_O.height = 92
	Looney_O.rotx = -37.36
	Looney_O.rotY = 78.5 
	Looney_O.rotz = -52.64
	Looney_2ndO.x = 289.9 'Looney O 
	Looney_2ndO.y = 1184  
	Looney_2ndO.height = 92
	Looney_2ndO.rotx = -37.36
	Looney_2ndO.rotY = 78.5 
	Looney_2ndO.rotz = -52.64
	Looney_N.x = 392    'Looney N
	Looney_N.y = 777
	Looney_N.height = 92
	Looney_N.rotx = 0
	Looney_N.rotY = 80
	Looney_N.rotz = -90
	Looney_E.x = 392    'Looney E
	Looney_E.y = 777
	Looney_E.height = 92
	Looney_E.rotx = 0
	Looney_E.rotY = 80
	Looney_E.rotz = -90
	Looney_Y.x = 392    'Looney Y
	Looney_Y.y = 777
	Looney_Y.height = 92
	Looney_Y.rotx = 0
	Looney_Y.rotY = 80
	Looney_Y.rotz = -90
	Tunes_T.x = 789	     'Tunes T
	Tunes_T.y = 1233
	Tunes_T.height = 83
	Tunes_T.rotx = 0
	Tunes_T.rotY = -79.921
	Tunes_T.rotz = 90
	Tunes_U.x = 789	     'Tunes U
	Tunes_U.y = 1233
	Tunes_U.height = 83
	Tunes_U.rotx = 0
	Tunes_U.rotY = -79.921
	Tunes_U.rotz = 90
	Tunes_N.x = 789	     'Tunes N
	Tunes_N.y = 1233
	Tunes_N.height = 83
	Tunes_N.rotx = 0
	Tunes_N.rotY = -79.921
	Tunes_N.rotz = 90
	Tunes_E.x = 789	     'Tunes E
	Tunes_E.y = 1234
	Tunes_E.height = 83
	Tunes_E.rotx = 0
	Tunes_E.rotY = -79.921
	Tunes_E.rotz = 90
	Tunes_S.x = 789	     'Tunes S
	Tunes_S.y = 1233
	Tunes_S.height = 83
	Tunes_S.rotx = 0
	Tunes_S.rotY = -79.921
	Tunes_S.rotz = 90

	PrevGameOver = 0
	SetGIColor
	'center_digits()


	table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"


End Sub


'Sub center_digits()
'
'Dim xoff, yoff, zoff, xcen, ycen, ii, xx, yy, xfact, yfact, obj, xrot, zscale
'
'xoff = 400 ' xoffset of destination (screen coords)
'yoff = 55 ' yoffset of destination (screen coords) 
'zoff = 865 ' zoffset of destination (screen coords)
'xrot = -87
'zscale = .14
'
'xcen =(1133 /2) - (53 / 2)
'ycen =(1183 /2) + (133 /2)
'yfact =80 'y fudge factor (ycen was wrong so fix)
'xfact =80
'
'
'for ii = 0 to 31
'	For Each obj In DigitsVR(ii)
'	xx = obj.x
'		
''	obj.x = (xoff -xcen) + (xx * 0.95) +xfact
'	obj.x = (xoff -xcen) + (xx) +xfact
'	yy = obj.y ' get the yoffset before it is changed
'	obj.y =yoff 
'
'		If(yy < 0.) then
'		yy = yy * -1
'		end if
'
'	obj.height =( zoff - ycen) + yy - (yy * zscale) + yfact
'	
'	obj.rotx = xrot
'	Next
'	Next
'end sub

Dim BIPL: BIPL = 0

'**********************************************************************************************************
' Key Down
'**********************************************************************************************************
Sub Table1_KeyDown(ByVal KeyCode)
'	if keycode = 31 then Kicker4.CreateSizedBallWithMass Ballsize/2, BallMass : Kicker4.Kick t1, t2
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress:'FlipperActivate RightFlipper1, RFPress1

	If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()

	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If keycode = StartGameKey Then soundStartButton()
	If VRRoom > 0 Then
		If keycode = LeftFlipperKey Then
			VR_FB_Left.x = VR_FB_Left.x + 5
		End If
		If keycode = StartGameKey Then		
			VR_StartButton.y = VR_StartButton.y - 2
		End If
		If keycode = RightFlipperKey Then
			VR_FB_Right.x = VR_FB_Right.x - 5
		End If
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

	If KeyDownHandler(keycode) Then Exit Sub
End Sub

'**********************************************************************************************************
' Key Up
'**********************************************************************************************************
Sub Table1_KeyUp(ByVal KeyCode)
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress:'FlipperDeActivate RightFlipper1, RFPress1

	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()    ' Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()  ' Plunger release sound when there is no ball in shooter lane
		End If
	End If

	If VRRoom > 0 Then
		If keycode = LeftFlipperKey Then
			VR_FB_Left.x = VR_FB_Left.x - 5
		End If
		If keycode = RightFlipperKey Then
			VR_FB_Right.x = VR_FB_Right.x + 5
		End If
		If keycode = StartGameKey Then		
			VR_StartButton.y = VR_StartButton.y + 2
		End If
		If keycode = PlungerKey Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If		
	End If

	If KeyUpHandler(keycode) Then Exit Sub
End Sub

'******************************************************
' Trough by cyberpez
'******************************************************
Sub sw11_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub sw11_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub
Sub sw12_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub sw12_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw11.BallCntOver = 0 Then sw12.kick 60, 9
	Me.Enabled = 0
End Sub


'***Drain and release
Sub sw10_Hit() 'Drain
	UpdateTrough
	Controller.Switch(10) = 1
	RandomSoundDrain sw10
End Sub

Sub sw10_UnHit()  'Drain
	Controller.Switch(10) = 0
End Sub

Sub SolOuthole(enabled)
	If enabled Then
		sw10.kick 65,20
		'PlaySoundAt SoundFX("fx_Solenoid",DOFContactors), sw10
	End If
End Sub

Sub ReleaseBall(enabled)
	If enabled Then
		'PlaySoundAt SoundFX("fx_BallRelease",DOFContactors), sw11
		RandomSoundBallRelease sw11
		sw11.kick 60, 12
		UpdateTrough
	End If
End Sub


'**************************************************************
' Kickers
'**************************************************************
'****Saucer kicker
Dim BallSaucer, BallInKicker

Sub kickSaucer(enabled)
	If (enabled) Then
		If sw16.ballcntover > 0 then
			sw16Step = 0
			sw16.timerenabled = 1
		End If
	End If
End Sub

Sub sw16_Hit
	set ballsaucer = activeball
	Controller.switch(16) = 1
End Sub

Sub sw16_Unhit
	Controller.Switch(16) = 0
	BallinKicker = 0
End Sub

Dim SW16Step
Sub SW16_Timer()
	Select Case SW16Step
		Case 0:	pKickerArm.Rotx = 4:PlaySoundat "fx_Solenoid", Primitive162
		Case 2: pKickerArm.Rotx = 8:BallSaucer.velz = 8:BallSaucer.vely = 8:
		Case 3: pKickerArm.Rotx = 8:'BallSaucer.velx = -2
		Case 4:	pKickerArm.Rotx = 4
		Case 5: pKickerArm.Rotx = 0:sw16.timerenabled = 0:SW16Step = -1:
	End Select
	SW16Step = SW16Step + 1
End Sub


'Can this be used to fix the saucer?
'sub Kicker1_Hit()
'	Dim speedx,speedy,finalspeed
'	speedx=activeball.velx
'	speedy=activeball.vely
'	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
'	if TableTilted=true then
'		tKickerTCount=0
'		Kicker1.Timerenabled=1
'		exit sub
'	end if
'	if finalspeed>10 then
'		Kicker1.Kick 0,0
'		activeball.velx=speedx
'		activeball.vely=speedy
'	else
''		mHole.MagnetOn=1
'		KickerHolder1.enabled=1
'	end if
'	
'end sub
'
'
'Sub Kicker1_Timer()
'	tKickerTCount=tKickerTCount+1
'	select case tKickerTCount
'	case 1:
'	'	mHole.MagnetOn=0
'		Pkickarm1.rotz=15
'		Pkickarm2.rotz=15
'		Pkickarm3.rotz=15
'		Pkickarm4.rotz=15
'		Pkickarm5.rotz=15
'		Playsound "saucer"
'		DOF 111, 2
'		Kicker1.kick 165,13
'	case 2:
'		Kicker1.timerenabled=0
'		Pkickarm1.rotz=0
'		Pkickarm2.rotz=0
'		Pkickarm3.rotz=0
'		Pkickarm4.rotz=0
'		Pkickarm5.rotz=0
'
'	end Select
'	
'end sub












'***Left Outlane Kickback Solenoid (Sol14)

Dim EMPos1	
EMPos1 = 0
Sub PlungerTimer_timer()
	EMPos1 = EMPos1 - 15
	pSol14.transY = EMPos1	
	If EMPos1 < 0 then PlungerTimer.Enabled = 0
End Sub

Sub Sol14_Solenoid(enabled)
If Enabled then
plungerIM.AutoFire:pSol14.transY = 90:EMPos1	 = 90:PlungerTimer.Enabled = True
end if
End Sub

Const Sol14_Power = 50 ' Kicker Power
Const Sol14_time = 0.6 ' Time in seconds for Full Plunge
 Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP Sol14_kick, Sol14_Power, Sol14_time
        .Random 0
        .switch 51 'sw51
        .InitExitSnd SoundFX("fx_KickerRelease",DOFContactors), SoundFX("fx_KickerRelease",DOFContactors)
        .CreateEvents "plungerIM"
    End With

'***LPF Kicker  -cp
Sub Sw18_Hit()
     Controller.Switch(18)=1:sw18p.TransY = -5
     vpmtimer.addtimer 10, "Wall5.collidable = True'"
     vpmtimer.addtimer 4000, "Wall5.collidable = False'"
     PlaySoundAt "zCB1",ActiveBall
     vpmtimer.addtimer 100, "Wall65.collidable = True'"
     vpmtimer.addtimer 4000, "Wall65.collidable = False'"
End Sub

Sub Sw18_UnHit()
Controller.Switch(18)=0:sw18p.TransY = 0
End Sub

Dim TKStep
Sub Kicker1_Hit:PlaySoundAtVol "zLPFKickSet",Kicker1,2:Me.Kick 186,95:PlaySoundAtVol SoundFX("zLPFKick",DOFContactors),Kicker1,3:TKStep = 0:pTopKick.TransY = 50:Me.TimerEnabled = true:End Sub
Sub Kicker1_timer()
	Select Case TKStep
		Case 0:pTopKick.TransY = 35
		Case 1:pTopKick.TransY = 18
		Case 2:pTopKick.TransY = 0:me.TimerEnabled = false
	End Select
	TKStep = TKStep + 1
End Sub

Sub TopKick(Enabled)
	If Enabled Then 

		Kicker1.Enabled = True
	Else
		Kicker1.Enabled = False
	End If
End Sub

'**************************************************************
' Pop Bumpers
'**************************************************************
Sub Bumper1_Hit:vpmTimer.PulseSw 54 : RandomSoundBumperBottom Bumper1: End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 53 : RandomSoundBumperMiddle Bumper2: End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 52 : RandomSoundBumperTop Bumper3: End Sub

' Rolling Ramp Sounds
Sub PlasticRampStart_hit: WireRampOn True : bsRampOnClear : End Sub
Sub Trigger1_hit: WireRampOff : bsRampOff ActiveBall.id : End Sub
Sub Ramp45bsShad_hit: bsRampOnClear : End Sub
Sub BallDrop4_hit : bsRampOff ActiveBall.id : End Sub


'**************************************************************
' Switches
'**************************************************************
Sub sw14_Hit():Controller.Switch(14) = 1:Stopsound "zIntro":BIPL=1:End Sub
Sub sw14_UnHit():Controller.Switch(14) = 0:Stopsound "zIntro":BIPL=0:End Sub

Sub sw15_Hit:vpmTimer.PulseSw(15):bsRampOn:End Sub
Sub sw17_Hit:vpmTimer.PulseSw 17:Stopsound "zIntro":End Sub

'***Target 19 and prim animation**********************
Dim zMultiplier
DIM target19step
Sub sw19_Hit
    Select Case Int(Rnd * 4) + 1
        Case 1: zMultiplier = 1.55
        Case 2: zMultiplier = 1.2
        Case 3: zMultiplier = .9
        Case 4: zMultiplier = .7
    End Select
    activeball.velz = activeball.velz*zMultiplier:vpmTimer.PulseSw 19:pSW19.TransY = -3:Target19Step = 1:sw19a.Enabled = 1
End Sub
Sub sw19a_timer()
	Select Case Target19Step
		Case 1:pSW19.TransY = -1
		Case 2:pSW19.TransY = 2
        Case 3:pSW19.TransY = 0:Me.Enabled = 0
     End Select
	Target19Step = Target19Step + 1
End Sub

'***Drop target and Double drop helper functions******
Sub sw20_hit: DTbank1.hit 1: SoundDropTargetDrop sw20:Dh20_21.isdropped = true: LeftDTDown.state = 1 :end sub
Sub sw21_hit: DTbank1.hit 2: SoundDropTargetDrop sw21:Dh20_21.isdropped = true: Dh21_22.isdropped = true: MiddleDTDown.state = 1 :end sub
Sub sw22_hit: DTbank1.hit 3: SoundDropTargetDrop sw22:Dh21_22.isdropped = true: RightDTDown.state = 1 :end sub

Sub Dh20_21_Hit:DTbank1.hit 1:DTbank1.hit 2:SoundDropTargetDrop sw21:Dh20_21.isdropped = true: Dh21_22.isdropped = true :End Sub 'Double Drop
Sub Dh21_22_Hit:DTbank1.hit 2:DTbank1.hit 3:SoundDropTargetDrop sw22:Dh20_21.isdropped = true: Dh21_22.isdropped = true :End Sub 'Double Drop

Sub ResetDrops(Enabled)
	If Enabled Then
		Dh20_21.isdropped = False:Dh21_22.isdropped = False
		DTbank1.DropSol_On
		LeftDTDown.state = 0
		MiddleDTDown.state = 0
		RightDTDown.state = 0
	End if
End Sub

'***Target 24 and prim animation**********************
DIM target24step
Sub sw24_Hit
    Select Case Int(Rnd * 4) + 1
        Case 1: zMultiplier = 1.55
        Case 2: zMultiplier = 1.2
        Case 3: zMultiplier = .9
        Case 4: zMultiplier = .7
    End Select
    activeball.velz = activeball.velz*zMultiplier:vpmTimer.PulseSw 24:pSW24.TransY = -3:Target24Step = 1:sw24a.Enabled = 1
End Sub
Sub sw24a_timer()
	Select Case Target24Step
		Case 1:pSW24.TransY = -1
		Case 2:pSW24.TransY = 2
        Case 3:pSW24.TransY = 0:Me.Enabled = 0
     End Select
	Target24Step = Target24Step + 1
End Sub

Sub sw25_Hit:vpmTimer.PulseSw 25:End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26:End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30:End Sub
 Sub Dh25_26_hit:vpmTimer.PulseSw 25:vpmTimer.PulseSw 26:End Sub 'Double Drop
 Sub Dh26_27_hit:vpmTimer.PulseSw 26:vpmTimer.PulseSw 27:End Sub 'Double Drop
 Sub Dh28_29_hit:vpmTimer.PulseSw 28:vpmTimer.PulseSw 29:End Sub 'Double Drop
 Sub Dh29_30_hit:vpmTimer.PulseSw 29:vpmTimer.PulseSw 30:End Sub 'Double Drop

Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub
Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub
Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub
Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub
 Sub Dh31_32_hit:vpmTimer.PulseSw 31:vpmTimer.PulseSw 32:End Sub 'Double Drop
 Sub Dh32_33_hit:vpmTimer.PulseSw 32:vpmTimer.PulseSw 33:End Sub 'Double Drop
 Sub Dh33_34_hit:vpmTimer.PulseSw 33:vpmTimer.PulseSw 34:End Sub 'Double Drop
 Sub Dh34_35_hit:vpmTimer.PulseSw 34:vpmTimer.PulseSw 35:End Sub 'Double Drop

Sub sw36_Hit:vpmTimer.PulseSw 36:End Sub
Sub sw37_Hit:vpmTimer.PulseSw 37:End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38:End Sub	
Sub sw39_Hit:vpmTimer.PulseSw 39:PlaySoundAt "zCapturedBallSet",ActiveBall:activeball.color = RGB(75,75,75):End Sub
Sub sw40_Hit:vpmTimer.PulseSw 40:PlaySoundAt "zCapturedBallSet",ActiveBall:activeball.color = RGB(75,75,75):End Sub
Sub sw41_Hit:vpmTimer.PulseSw 41:End Sub
Sub sw42_Hit:vpmTimer.PulseSw 42: bsRampOnClear : End Sub

Sub sw44_Hit:vpmTimer.PulseSw 44:End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45:End Sub
 Sub Dh44_45_hit:vpmTimer.PulseSw 44:vpmTimer.PulseSw 45:End Sub 'Double Drop

Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub
Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:End Sub
Sub sw51_Hit:End Sub
Sub sw60_Hit:vpmTimer.PulseSw 60:End Sub
Sub sw61_Hit:vpmTimer.PulseSw 61:End Sub
 Sub sw62_Hit:vpmTimer.PulseSw 62:End Sub
 Sub Dh60_61_hit:vpmTimer.PulseSw 60:vpmTimer.PulseSw 61:End Sub 'Double Drop
 Sub Dh61_62_hit:vpmTimer.PulseSw 61:vpmTimer.PulseSw 62:End Sub 'Double Drop

Sub Spinner1_Spin
	SoundSpinner Spinner1
	vpmTimer.PulseSw 23   
End Sub

'**************************************************************
' Ramp helpers for Spiral Ramp
'**************************************************************
Sub ramphelper1_Hit()
	ActiveBall.velx = Activeball.velx/600
End Sub

Sub ramphelper3_Hit()
	ActiveBall.velx = Activeball.velx*1.2
End Sub

Sub ramphelper4_Hit()
	ActiveBall.vely = Activeball.vely*1.5
End Sub
'
Sub ramphelper5_Hit()
	ActiveBall.velx = Activeball.velx*1.2
End Sub

Sub ramphelper6_Hit()
	ActiveBall.velx = Activeball.velx*1.2
End Sub

Sub ramphelper7_Hit()
	ActiveBall.velx = Activeball.velx*1.2
End Sub

'**************************************************************
' Ball color RGB change
'**************************************************************
Sub RGB1_Hit 'Darkens ball in the shooter lane
	activeball.color = RGB(100,100,100)
End Sub
Sub RGB1_Unhit 
	activeball.color = RGB(150,150,150)
End Sub

Sub RGB2_Hit 'Darkens the ball in the upper areas of the main PF.
	activeball.color = RGB(125,125,125)
End Sub
Sub RGB2_Unhit 
	activeball.color = RGB(150,150,150)
End Sub

Sub RGB3a_Hit 'Darkens the ball entering the lower playfield
	activeball.color = RGB(100,100,100)
End Sub

Sub RGB3b_hit
	if Activeball.Vely > 0 then 
	activeball.color = RGB(150,150,150)
	Else
	activeball.color = RGB(100,100,100)
	End if
End Sub

Sub RGB3b_Unhit
	activeball.color = RGB(150,150,150)
End Sub

Sub RGB4_hit
	if Activeball.Vely > 0 then 
	activeball.color = RGB(150,150,150)
	Else
	activeball.color = RGB(125,125,125)
	End if
End Sub

Sub RGB5_Hit 'Darkens ball in the shooter lane
	activeball.color = RGB(80,80,80)
End Sub

Sub RGB6_Hit 'Darkens ball in the saucer
	activeball.color = RGB(100,100,100)
End Sub

Sub RGB6_UnHit 
	activeball.color = RGB(150,150,150)
End Sub


'***************************************
'* Prim Material Swaps
'***************************************

Dim FadeMaterialRubbersArray: FadeMaterialRubbersArray = Array("RubbersWhiteGIOff", "RubbersWhiteGIOff","RubbersWhiteGIOn","RubbersWhiteGIOn")

Sub MatSwap(pri, group, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
    Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = Collection(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
End Sub


Dim DLintensity
Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
pri.blenddisablelighting = aLvl * DLintensity
End Sub

'*********************************************************************************************************************************************************
' Begin nfozzy lamp handling
'*********************************************************************************************************************************************************

redim GILamps(99) : redim GIFlashers(99)	'new arrays
SortGI GILamps, GIFlashers, GILighting
dim TestString, TestStringAll	'debug strings
Sub SortGI(ByRef aLight,aFlasher, GImixed) 'different method using Arrays instead of scripting dictionary objects
	dim x, CountMe: CountMe = 0
	for x = 0 to (GImixed.Count-1)
		if TypeName(GImixed(x) ) = "Light" Then
			Set aLight(CountMe) = GImixed(x)
			TestString = TestString & "assigned " & GImixed(x).Name & " to aLight(" & CountMe & ")"	& vbnewline 'debug
			CountMe = CountMe+1
			redim Preserve aLight(CountMe)
		end if
	Next
	CountMe = 0
	for x = 0 to (GImixed.Count-1)	'(note: this sub assumes there ARE flashers in the collection!)
		if TypeName(GImixed(x) ) = "Flasher" Then
			Set aFlasher(CountMe) = GImixed(x)
			TestString = TestString & "assigned " & GImixed(x).Name & " to aFlasher(" & CountMe & ")" & vbnewline	'debug
			CountMe = CountMe+1
			redim Preserve aFlasher(CountMe)
		end if
	Next
	redim Preserve aLight(uBound(aLight)-1)	'final trim of the arrays
	redim Preserve aFlasher(uBound(aFlasher)-1)
	'TestSTR(0) = TestSTR(0) & "ubound aLight: " & uBound(aLight) & " uBound aFlashers:" & uBound(aFlasher)	'debug
	'Debug.Print TestString
End Sub

'These arrays contain the following info of all non-GI lights (collected from GetElements via SortLamps sub)
Redim LightsA(999)' Object references
Redim LightsB(999)'	Opacity / Intensity
Redim LightsC(999)'	Fade Up (Light objects)
Redim LightsD(999)'	Fade Down(Light Objects)

SortLamps GILighting, GIOffFlasherCorrection
Sub SortLamps(ByVal GI, aExclude)	'Sorts remaining light and flashers objects (EXCLUDES those in the GI collection)
	dim Counter,x,xx,skipme : skipme = False:Counter = 0 : TestStringAll = "Test String 2"
	for each x in GetElements	'now we're cooking
		'if TypeName(x) = "IDecal" then Continue For 'Decals don't have names. Evil imo D:
		if TypeName(x) = "Light" or TypeName(x) = "Flasher" Then
			SkipMe = False
			for each xx in GI 'Find duplicates and Skip them
				if x.Name = xx.Name then 
					TestStringAll = TestStringAll & x.Name & "found in GI collection, Disregarding & Continuing..." & vbnewline 'debug
					SkipMe = True'Continue For
				End If
			next
			for each xx in aExclude 'Exclude collection
				if x.Name = xx.Name then 
					TestStringAll = TestStringAll & x.Name & "found in exclude collection, Disregarding & Continuing..." & vbnewline 'debug
					SkipMe = True'Continue For
				End If
			next
			for each xx in LCDigits 'Exclude collection
                if x.Name = xx.Name then 
                    TestStringAll = TestStringAll & x.Name & "found in exclude collection, Disregarding & Continuing..." & vbnewline 'debug
                    SkipMe = True'Continue For
                End If
            next
			If Not SkipMe Then
				On Error Resume Next
				'LightsA(Counter) = x.name	'name
				Set LightsA(Counter) = x	'ref
				LightsB(Counter) = x.Opacity
				LightsB(Counter) = x.Intensity
				LightsC(Counter) = x.FadeSpeedUp
				LightsD(Counter) = x.FadeSpeedDown
				On Error Goto 0
				Counter = Counter + 1
				redim Preserve LightsA(Counter)
				redim Preserve LightsB(Counter)
				redim Preserve LightsC(Counter)
				redim Preserve LightsD(Counter)
			End If
		End If
	next
	redim Preserve LightsA(uBound(LightsA)-1)	'final trim of the arrays
	redim Preserve LightsB(uBound(LightsB)-1)
	redim Preserve LightsC(uBound(LightsC)-1)
	redim Preserve LightsD(uBound(LightsD)-1)

	TestStringAll = TestStringAll & "Ubound LightsA = " & UBound(LightsA)	'Debug
	'debug.print TestTwo
End Sub

' Lamp & Flasher Updates
' LampFader object (Lampz) updates on two timers: Logic on 1, Game updates on -1
Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	'Lampz.Update1	'update (fading logic only)
	Lampz.Update2	'update (Pinmame and Fading (for -1, lower latency)
End Sub

function FlashLevelToIndex(Input, MaxSize)
	'FlashLevelToIndex = cInt(Input * (MaxSize-1)+.5)+1
     FlashLevelToIndex = cInt(MaxSize * Input)
end function 

'Lamp Filter
Function LampFilter(aLvl)	
	LampFilter = aLvl^1.6	'exponential curve?
End Function

'Collections to arrays
Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

'Setlamp, etc
	'Solenoid pipeline looks like this:
	'Pinmame Controller -> UseSolenoids -> Solcallback -> intermediate subs (here) -> Lampz fading object -> object updates / more callbacks

	'Lamps, for reference:
	'Pinmame Controller -> UpdateLamps sub -> Lampz Fading Object -> Object Updates / callbacks

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLamps
LampTimer.Interval = -1 '1
LampTimer.Enabled = 1

' Lamp & Flasher Updates
' LampFader object (Lampz) updates on two timers: Logic on 1, Game updates on -1
'Sub LampTimer_Timer()
'	dim x, chglamp
'	chglamp = Controller.ChangedLamps
'	If Not IsEmpty(chglamp) Then
'		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
'			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
'		next
'	End If
'	'Lampz.Update1	'update (fading logic only)
'	Lampz.Update2	'update (Pinmame and Fading (for -1, lower latency)
'End Sub


Dim ColorGradeImage1
ColorGradeImage1 = "ColorGradeLUT256x16_BDDark1"

Sub InitLamps()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensity scale output (no callbacks) through this function before updating
	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/75 : Lampz.FadeSpeedDown(x) = 1/85 : next
	Lampz.FadeSpeedUp(110) = 1/64 'GI

'*********************************************************************************************************************************************************
'End nfozzy lamp handling
'*********************************************************************************************************************************************************

	'Lamp Assignments
	Lampz.MassAssign(1) =L1
	Lampz.MassAssign(2) =Light48a
	Lampz.MassAssign(2) =Light48b
	Lampz.MassAssign(3) =Light49a
	Lampz.MassAssign(3) =Light49b
	Lampz.MassAssign(4) =Light50a
	Lampz.MassAssign(4) =Light50b
	Lampz.MassAssign(5) =l5
	Lampz.MassAssign(6) =l6
	Lampz.MassAssign(7) =l7
	Lampz.MassAssign(8) =l8
	Lampz.MassAssign(9) =l9
	Lampz.MassAssign(10) =l10
	Lampz.MassAssign(11) =l11
	Lampz.MassAssign(12) =l12
	Lampz.MassAssign(13) =l13
	Lampz.MassAssign(14) =L14
	Lampz.MassAssign(15) =L15
	Lampz.MassAssign(16) =L16
	Lampz.MassAssign(17) =L17
	Lampz.MassAssign(18) =L18
	Lampz.MassAssign(19) =L19
	Lampz.MassAssign(20) =L20
	Lampz.MassAssign(21) =L21
	Lampz.MassAssign(22) =L22
	Lampz.MassAssign(23) =L23
	Lampz.MassAssign(24) =L24

	'Tunes plastic dome
	Lampz.MassAssign(25) =L25
	Lampz.MassAssign(25) =L25_bloom
	Lampz.MassAssign(25) =Tunes_T
	Lampz.MassAssign(26) =L26
	Lampz.MassAssign(26) =L26_bloom
	Lampz.MassAssign(26) =Tunes_U
	Lampz.MassAssign(27) =L27
	Lampz.MassAssign(27) =L27_bloom
	Lampz.MassAssign(27) =Tunes_N
	Lampz.MassAssign(28) =L28
	Lampz.MassAssign(28) =L28_bloom
	Lampz.MassAssign(28) =Tunes_E
	Lampz.MassAssign(29) =L29
	Lampz.MassAssign(29) =L29_bloom
	Lampz.MassAssign(29) =Tunes_S

	'Looney Plastic Dome
	Lampz.MassAssign(30) =L30
	Lampz.MassAssign(30) =L30_bloom
	Lampz.MassAssign(30) =Looney_L
	Lampz.MassAssign(31) =L31
	Lampz.MassAssign(31) =L31_bloom
	Lampz.MassAssign(31) =Looney_O
	Lampz.MassAssign(32) =L32
	Lampz.MassAssign(32) =L32_bloom
	Lampz.MassAssign(32) =Looney_2ndO
	Lampz.MassAssign(33) =L33
	Lampz.MassAssign(33) =L33_bloom
	Lampz.MassAssign(33) =Looney_N
	Lampz.MassAssign(34) =L34
	Lampz.MassAssign(34) =L34_bloom
	Lampz.MassAssign(34) =Looney_E
	Lampz.MassAssign(35) =L35
	Lampz.MassAssign(35) =L35_bloom
	Lampz.MassAssign(35) =Looney_Y
	Lampz.MassAssign(36) =L36
	Lampz.MassAssign(37) =L37
	Lampz.MassAssign(38) =L38
	Lampz.Callback(39) = "PlayMusic"
	Lampz.MassAssign(39) =L39
	Lampz.MassAssign(40) =L40
	Lampz.MassAssign(41) =L41
	Lampz.MassAssign(41) =L41a
	Lampz.MassAssign(41) =L41b
	Lampz.MassAssign(42) =L42
	Lampz.MassAssign(42) =L42a
	Lampz.MassAssign(43) =L43
	Lampz.MassAssign(44) =L44
	Lampz.MassAssign(45) =L45
	Lampz.MassAssign(46) =L46
	Lampz.MassAssign(47) =L47
	Lampz.MassAssign(48) =L48
	Lampz.MassAssign(49) =L49
	Lampz.MassAssign(50) =L50
	Lampz.MassAssign(51) =L51
	Lampz.MassAssign(52) =L52
	Lampz.MassAssign(51) =L52a
	Lampz.MassAssign(53) =L53
	Lampz.MassAssign(53) =L53a
	Lampz.MassAssign(54) =L54
	Lampz.MassAssign(63) =l63
	Lampz.MassAssign(64) =F64

	'Set Lampz.obj(111) = 'Sol 11 Insert Relay (Backbox GI)
'	Lampz.MassAssign(111) =BGBright	
	

	'Sol 09 LOONEY Relay
	Lampz.MassAssign(109) =Looney0		
	Lampz.MassAssign(109) =Looney1
	Lampz.MassAssign(109) =Looney2
	Lampz.MassAssign(109) =Looney3
	Lampz.MassAssign(109) =Looney4
	Lampz.MassAssign(109) =Looney5

	'Sol 16 TUNES Relay
	Lampz.MassAssign(116) =Tunes0		
	Lampz.MassAssign(116) =Tunes1
	Lampz.MassAssign(116) =Tunes2
	Lampz.MassAssign(116) =Tunes3
	Lampz.MassAssign(116) =tunes4
	
	'Sol 25 Left Stand Up Flasher Bulb
	Lampz.MassAssign(125) =f125a
	Lampz.MassAssign(125) =f125c
 	Lampz.MassAssign(126) =f125b
	Lampz.MassAssign(125) =F125FS
	Lampz.MassAssign(125) =F125VR
	'Lampz.MassAssign(125) =Flasher002
	'Lampz.Callback(125) = "DisableLighting p125Fil, 5000,"



	'Sol 26 Left Stand Up Flasher Bulb - BG Sam Shot

	Lampz.MassAssign(126) =F126FS
	Lampz.MassAssign(126) =F126VR
	Lampz.Callback(126) = "DisableLighting p126Fil, 1000,"
	Lampz.Callback(126) = "DisableLighting pSW24, 5,"
	Lampz.MassAssign(126) =f126b
	Lampz.MassAssign(126) =VRBGFL26_1
	Lampz.MassAssign(126) =VRBGFL26_2
	Lampz.MassAssign(126) =VRBGFL26_3
	Lampz.MassAssign(126) =VRBGFL26_4
	


    'Sol 27 50 Million Shot - BG Cake

	Lampz.MassAssign(127) = f127
	Lampz.MassAssign(127) =VRBGFL27_1
	Lampz.MassAssign(127) =VRBGFL27_2
	Lampz.MassAssign(127) =VRBGFL27_3
	Lampz.MassAssign(127) =VRBGFL27_4


    'Sol 28 Taz Flasher  - BG Speaker Right
	Lampz.MassAssign(128) = f128
	Lampz.MassAssign(128) =VRBGFL28_1
	Lampz.MassAssign(128) =VRBGFL28_2
	Lampz.MassAssign(128) =VRBGFL28_3
	Lampz.MassAssign(128) =VRBGFL28_4


	'Sol 29 Right Standup Flasher - BG Speaker Center
	Lampz.Callback(129) = "DisableLighting pSW19, 5,"
	Lampz.MassAssign(129) = f129        
	Lampz.MassAssign(129) =VRBGFL29_1
	Lampz.MassAssign(129) =VRBGFL29_2
	Lampz.MassAssign(129) =VRBGFL29_3
	Lampz.MassAssign(129) =VRBGFL29_4


	'Sol 30 Bugs Face Flasher - BG Speaker Left
	Lampz.MassAssign(130) =VRBGFL30_1
	Lampz.MassAssign(130) =VRBGFL30_2
	Lampz.MassAssign(130) =VRBGFL30_3
	Lampz.MassAssign(130) =VRBGFL30_4


    'Sol 31 Back Wall Left - BG Road Runner Top Right
	Lampz.MassAssign(131) = F131
	Lampz.MassAssign(131) =VRBGFL31_1
	Lampz.MassAssign(131) =VRBGFL31_2
	Lampz.MassAssign(131) =VRBGFL31_3
	Lampz.MassAssign(131) =VRBGFL31_4 


    'Sol 32 Back Wall Right - BG Taz Bottom Left
	Lampz.MassAssign(132) = F132
	Lampz.MassAssign(132) =VRBGFL32_1
	Lampz.MassAssign(132) =VRBGFL32_2
	Lampz.MassAssign(132) =VRBGFL32_3
	Lampz.MassAssign(132) =VRBGFL32_4   


	'Sol 10 GI relay and assignments

	Lampz.obj(110) = ColtoArray(GILighting)	
    Lampz.Callback(110) = "FadeMaterialColor Mat_RubbersWhite, 200, 60, "    'fading for white rubbers
    Lampz.Callback(110) = "FadeMaterialColor Mat_RubbersWhite1, 200, 60, "    'fading for white rubbers

	Lampz.Callback(110) = ColorGradeImage1 


	Lampz.Callback(110) = "GIUpdates"
	Lampz.state(110) = 1   'Turn on GI to Start
	Lampz.MassAssign(110) =GuideLight1
	Lampz.MassAssign(110) =GuideLight2
	Lampz.MassAssign(110) =GuideLight3
	Lampz.MassAssign(110) =GuideLight4
	lampz.TurnOnStates	'Set any lamps state to 1. (Object handles fading!)
	lampz.update
End Sub


'*************Scripting to swap materials when GI goes out
Sub FadeMaterialColor(PrimName, LevelMax, LevelMin, aLvl)
	dim rgbLevel
	rgbLevel = ((LevelMax - LevelMin) * aLvl) + LevelMin

    Dim wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    GetMaterial PrimName.material, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, base, glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
    UpdateMaterial PrimName.material, wrapLighting, roughness, glossyImageLerp, thickness, edge, edgeAlpha, opacity, rgb(rgbLevel,rgbLevel,rgbLevel), glossy, clearcoat, isMetal, opacityActive, elasticity, elasticityFalloff, friction, scatterAngle
End Sub

'*********************************************************************************************************************************************************
'Begin lamp helper functions
'*********************************************************************************************************************************************************
'***************************************
'System 11 GI On/Off
'***************************************
Sub GIOn  : SetGI False: End Sub 'These are just debug commands now
Sub GIOff : SetGI True : End Sub

pRampSpiralAdd.color = RGB(255,245,210)


Dim GIoffMult : GIoffMult = 2 'Multiplies all non-GI inserts lights opacities when the GI is off 

Sub GIupdates(ByVal aLvl)	'GI update odds and ends go here
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically

	dim GIscale   'Fade lamps up when GI is off
	GiScale = (GIoffMult-1) * (ABS(aLvl-1 )  ) + 1	'invert
	dim x : for x = 0 to uBound(LightsA) 
		On Error Resume Next
		LightsA(x).Opacity = LightsB(x) * GIscale
		LightsA(x).Intensity = LightsB(x) * GIscale
		'LightsA(x).FadeSpeedUp = LightsC(x) * GIscale
		'LightsA(x).FadeSpeedDown = LightsD(x) * GIscale
		On Error Goto 0
	Next

	pRampSpiralAdd.opacity = 100*aLvl
	pRampTazAdd.opacity = 100*aLvl
	pRampShieldAdd.opacity = 120*aLvl
	pRampSylvestorAdd.opacity = 100*aLvl



'    if aLvl > 0.5 then 'swaps LUTs when GI is turned off 
'        Table1.ColorGradeImage="ColorGradeLUT256x16_ConSat"
'    else 
'        Table1.ColorGradeImage="ColorGradeLUT256x16_BDDark2"
'    end if
End Sub

Dim RelayCounter

Sub PlayRelay(aOn, endpoint, lampnro)
	'iaakki: Routine to play relay events faster than solenoid handler. 
	RelayCounter = RelayCounter + 1		'Counting solenoid clicks. If more than endpoint withing 5s, we will move to timer code
	if RelayCounter < endpoint + 1 then
		if aOn = True Then    
			Playsound "RelayOn"

			if lampnro = 110 Then
				Setlamp lampnro, 0 'inverted for gi
				GiFOPChange 1.25
			Else
				Setlamp lampnro, 1
			end if
		Else
			Playsound "RelayOff"
			if lampnro = 110 Then
				Setlamp lampnro, 5 'inverted for gi
				GiFOPChange 4
			Else
				Setlamp lampnro, 0
			end if
		end If
	end If
	if RelayCounter = endpoint then		'Lots of clicks happened of events detected, jumping to timer code
		if lampnro = 110 Then
			GiFOPChange 4
			GIRelayClickTimer.Interval = 20
			GIRelayClickTimer.Enabled = 1
		Else
			LTRelayClickTimer.Interval = 20
			LTRelayClickTimer.uservalue = lampnro
			LTRelayClickTimer.Enabled = 1
		end if
	end If
	RelayResetStateTimer.enabled = 1	'Enable 5s counter to reset RelayCounter back to 0. 
end Sub

dim ClickCounter
Const FastClickLength = 30

sub LTRelayClickTimer_timer
	'timer to play relay event ending properly
	dim lampNro : lampNro = LTRelayClickTimer.uservalue
	ClickCounter = ClickCounter + 1

	if lampz.state(lampNro) = 0 Then
		Playsound "RelayOn"
		Setlamp lampNro, 1 'inverted for gi
	Else
	
		Playsound "RelayOff"
		Setlamp lampNro, 0 'inverted for gi
	End If
	if ClickCounter = FastClickLength then	'Finishing the clicking
		Setlamp lampNro, 1 'failsafe exit
		ClickCounter = 0
		LTRelayClickTimer.enabled = 0
	end If
end Sub

sub GIRelayClickTimer_timer
	'timer to play relay event ending properly
	Const lampNro = 110
	ClickCounter = ClickCounter + 1
	
	if lampz.state(lampNro) = 0 Then
		Playsound "RelayOn"
		Setlamp lampNro, 5 'inverted for gi
		GiFOPChange 4
	Else
		Playsound "RelayOff"
		Setlamp lampNro, 0 'inverted for gi
		GiFOPChange 1.25
	End If
	if ClickCounter = FastClickLength then	'Finishing the clicking
		Setlamp lampNro, 5 'failsafe exit : inverted for gi
		GiFOPChange 4
		ClickCounter = 0
		GIRelayClickTimer.enabled = 0
	end If
end Sub


sub RelayResetStateTimer_timer
	'relaycounter reset after 5 seconds
	RelayCounter = 0
	RelayResetStateTimer.enabled = 0
end Sub

Dim GiOffFOP
Sub SetGI(aOn)
	PlayRelay aOn, 13, 110		'aOn, endpoint, lamp number
End Sub

Sub SetBBGI(enabled)
	dim BGobj
	If enabled Then
		For each BGobj in VRBGGI : BGobj.visible = 0 : Next
	Else
		For each BGobj in VRBGGI : BGobj.visible = 1 :  Next
	End If
End Sub
	

Sub GiFOPChange(aPower)
	For each GiOffFOP in LampsInserts 'increases falloff power for lamps in "lampinserts" collection, when GI is turned off
		GIOffFOP.falloffpower = aPower 'Sets falloff power to 1.25 when GI is ON
	next
end Sub

'*********************************************************************************************************************************************************
'End lamp helper functions
'*********************************************************************************************************************************************************


'Looney Tunes Domes All Bulbs On
Sub Flashlooney (aOn)
	PlayRelay aOn, 17, 109		'aOn, endpoint, lamp number
End Sub

Sub FlashTunes (aOn)
	PlayRelay aOn, 19, 116		'aOn, endpoint, lamp number
End Sub
'**********************************************************************************************************
'DT View Scoring Display
'**********************************************************************************************************
 Dim Digits(32)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 Digits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
 Digits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
 Digits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
 Digits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
 Digits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
 Digits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
 Digits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
 Digits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
 Digits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
 Digits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
 Digits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
 Digits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
 Digits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
 Digits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
 Digits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
 Digits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
 Digits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
 Digits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
 

 Sub DisplayTimer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
		If DesktopMode = True Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 32) then
				if VRRoom > 0 Then
				For Each obj In DigitsVR(num)
                   If chg And 1 Then obj.visible=stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
				Else
				For Each obj In Digits(num)
                   If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
				End if
			end if
		Next
	   end if
    End If
 End Sub

'******************************************************************************************************************************************************
'VPX  call back functions
'******************************************************************************************************************************************************

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function



'****************************************************************
'  Begin Slingshots
'****************************************************************

' RStep and LStep are the variables that increment the animation
Dim RStep, LStep, UStep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RSling1.Visible = 1
	Sling1.TransZ = -20			'Sling Metal Bracket
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
	vpmTimer.PulseSw 55			'Slingshot Rom Switch
	RandomSoundSlingshotRight Sling1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:Sling1.TransZ = 20
		Case 4:RSLing2.Visible = 0:Sling1.TransY = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	LSling1.Visible = 1
	Sling2.TransZ = -20			'Sling Metal Bracket
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
	vpmTimer.PulseSw 56			'Slingshot Rom Switch
	RandomSoundSlingshotLeft Sling2
End Sub

' 11/13/22 - Duplicate LeftSlingShot_Timer function below - Using the TransZ version since it matches the RightSlingShot coding.
'Sub LeftSlingShot_Timer
'	Select Case LStep
'		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:Sling2.TransY = 20
'		Case 4:LSLing2.Visible = 0:Sling2.TransY = 0:LeftSlingShot.TimerEnabled = 0
'	End Select
'	LStep = LStep + 1
'End Sub

Sub TestSlingShot_Slingshot
	TS.VelocityCorrect(ActiveBall)
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 0:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -30
        Case 2:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub

Sub UpperSlingShot_Slingshot
	vpmTimer.PulseSw 49
    PlaySoundAtVol SoundFX("fx_SlingshotLeft",DOFContactors),SLING3,2
    USling.Visible = 0
    USling1.Visible = 1
    sling3.TransZ = -25
    UStep = 0
    UpperSlingShot.TimerEnabled = 1
End Sub

Sub UpperSlingShot_Timer
    Select Case UStep
        Case 0:USLing1.Visible = 0:USLing2.Visible = 1:sling3.TransZ = -10
        Case 2:USLing2.Visible = 0:USLing.Visible = 1:sling3.TransZ = 0:UpperSlingShot.TimerEnabled = 0:
    End Select
    UStep = UStep + 1
End Sub

'***LPF sling animations***

Sub wall72_Hit:vpmTimer.PulseSw 100:LPF_sling1.visible = 0::LPF_sling1a.visible = 1:wall72.timerenabled = 1:End Sub
Sub wall72_timer:LPF_sling1.visible = 1::LPF_sling1a.visible = 0: wall72.timerenabled= 0:End Sub

'***Rear wall rubber animation***
Sub sw50_Hit:vpmTimer.PulseSw 50:vpmTimer.PulseSw 100:rubber29.visible = 0::rubber29a.visible = 1:sw50.timerenabled = 1:End Sub
Sub sw50_timer:rubber29.visible = 1::rubber29a.visible = 0: sw50.timerenabled= 0:End Sub


'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)

	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
    Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
    Dim rotxy
    rotxy = RotPoint(ax,ay,angle)
    rax = rotxy(0)+px : ray = rotxy(1)+py
    rotxy = RotPoint(bx,by,angle)
    rbx = rotxy(0)+px : rby = rotxy(1)+py
    rotxy = RotPoint(cx,cy,angle)
    rcx = rotxy(0)+px : rcy = rotxy(1)+py
    rotxy = RotPoint(dx,dy,angle)
    rdx = rotxy(0)+px : rdy = rotxy(1)+py

    InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function

'****************************************************************
'  End Slingshots
'****************************************************************



'******************************************************************************************************************************************************************************************************************************
'Begin nfozzy lighting supporting functions scripting
'*****************************************************************************************************************************************************************************************************************************
'====================
'Class jungle nf
'=============

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String) 
		UseCallBack(idx) = True 
		'cCallback(idx) = String 'old execute method				
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _
		
		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False 
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if	
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering				
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property 

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then 
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then 
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x) 
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class


'Helper functions

Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String) 
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub


Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then 
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If		
			Next
		AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then 
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function


' ******************************************************************************************************************************************************************************************************************************
' End nfozzy lighting supporting functions scripting
' ******************************************************************************************************************************************************************************************************************************




' **********************************************************************************
' Options
' **********************************************************************************


'*************************
'Shadow Intensity adjustment
'*************************
Dim ShadowIntensity
BBBBShadow.opacity = ShadowIntensity

'*************************
'Show Side Rails
'*************************
DIM SideRails
If SideRails =1 and VRRoom < 1 Then
		pRailLeft.visible = 1
		pRailRight.visible = 1
Else		pRailLeft.visible = 0
		pRailRight.visible = 0
End if

'*************************
'Hide Desktop LCD
'*************************
DIM VRRoom
If VRRoom = 1 Then
		Dim VRThings
		for each VRThings in DesktopLCD:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 1:Next
		F125VR.Visible=1
		F126VR.Visible=1
		SetBackglass
		For each VRThings in VRBGGI : VRThings.visible = 1 :  Next

Else
		for each VRThings in DesktopLCD:VRThings.visible = 1:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		F125VR.Visible=0
		F126VR.Visible=0


End if

'******************* VR Backglass **********************

Sub SetBackglass()
	Dim obj
	bgdark.visible = True
	BGSpeakersdark.visible = True
	For Each obj In VRBackglass
		obj.x = obj.x
		obj.height = - obj.y - 100
		obj.y = 82 'adjusts the distance from the backglass towards the user
		obj.rotx=-93
	Next
	
		For Each obj In VRDigits
		obj.x = obj.x
		obj.height = - obj.y - 102
		obj.y = 62 'adjusts the distance from the backglass towards the user
		obj.rotx=-93
	Next

	For Each obj In VRBackglassSpeaker
		obj.x = obj.x
		obj.height = - obj.y - 75
		obj.y = 110 'adjusts the distance from the backglass towards the user
		obj.rotx=-92
	Next
End Sub

Set LampCallback = GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps()
	IF VRRoom <> 0 Then
		If Controller.Lamp(55) = 0 Then
			VRBGLA55_1.visible=0: VRBGLA55_2.visible=0: VRBGLA55_3.visible=0: VRBGLA55_4.visible=0
		Else
			VRBGLA55_1.visible=1: VRBGLA55_2.visible=1: VRBGLA55_3.visible=1: VRBGLA55_4.visible=1
		End If
		If Controller.Lamp(56) = 0 Then
			VRBGLA56_1.visible=0: VRBGLA56_2.visible=0: VRBGLA56_3.visible=0: VRBGLA56_4.visible=0
		Else
			VRBGLA56_1.visible=1: VRBGLA56_2.visible=1: VRBGLA56_3.visible=1: VRBGLA56_4.visible=1
		End If
		If Controller.Lamp(57) = 0 Then
			VRBGLA57_1.visible=0: VRBGLA57_2.visible=0: VRBGLA57_3.visible=0: VRBGLA57_4.visible=0
		Else
			VRBGLA57_1.visible=1: VRBGLA57_2.visible=1: VRBGLA57_3.visible=1: VRBGLA57_4.visible=1
		End If
		If Controller.Lamp(58) = 0 Then
			VRBGLA58_1.visible=0: VRBGLA58_2.visible=0: VRBGLA58_3.visible=0: VRBGLA58_4.visible=0
		Else
			VRBGLA58_1.visible=1: VRBGLA58_2.visible=1: VRBGLA58_3.visible=1: VRBGLA58_4.visible=1
		End If
		If Controller.Lamp(59) = 0 Then VRBGGIP1_1.visible=0: VRBGGIP1_2.visible=0: else : VRBGGIP1_1.visible=1: VRBGGIP1_2.visible=1
		If Controller.Lamp(60) = 0 Then VRBGGIP2_1.visible=0: VRBGGIP2_2.visible=0: else : VRBGGIP2_1.visible=1: VRBGGIP2_2.visible=1
		If Controller.Lamp(61) = 0 Then VRBGGIP3_1.visible=0: VRBGGIP3_2.visible=0: else : VRBGGIP3_1.visible=1: VRBGGIP3_2.visible=1
		If Controller.Lamp(62) = 0 Then VRBGGIP4_1.visible=0: VRBGGIP4_2.visible=0: else : VRBGGIP4_1.visible=1: VRBGGIP4_2.visible=1
	End If
End Sub

'******************* VR Plunger **********************


Sub TimerVRPlunger_Timer
  If VRPlunger.Y < 1210 then
		VRPlunger.Y = VRPlunger.Y + 2.5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	VRPlunger.Y = 1080 + (5* Plunger.Position) -20
	timervrplunger2.enabled = 0
End Sub

'*************************
'Intro music
'*************************
Dim IntroMusic, PrevGameOver      
Sub PlayMusic(aLvl)
	If aLvl > 0 and PrevGameOver = 0 Then
		If IntroMusic = 0 Then	
			PlaySound "zintro"
			PrevGameOver = 1
		End If
	else 
'		PrevGameOver = 0
	End If
End Sub

'*************************
'***GI Color Mod***
'*************************
Dim GIxx, ColorModRed, ColorModRedFull, ColorModGreen, ColorModGreenFull, ColorModBlue, ColorModBlueFull
Dim GIColorRed, GIColorGreen, GIColorBlue, GIColorFullRed, GIColorFullGreen, GIColorFullBlue
Sub SetGIColor ()
	for each GIxx in GILighting
	GIxx.Color = rgb(GIColorRed, GIColorGreen, GIColorBlue)
	LeftDTDown.Color = rgb(GIColorRed, GIColorGreen, GIColorBlue)
	MiddleDTDown.Color = rgb(GIColorRed, GIColorGreen, GIColorBlue)
	RightDTDown.Color = rgb(GIColorRed, GIColorGreen, GIColorBlue)
	next
End Sub


'**********************************************************************************************************
'VR Scoring Display
'**********************************************************************************************************
 Dim DigitsVR(32)
 DigitsVR(0)=Array(ax00, ax05, ax0c, ax0d, ax08, ax01, ax06, ax0e, ax02, ax03, ax04, ax07, ax0b, ax0a, ax09, ax0f)
 DigitsVR(1)=Array(ax10, ax15, ax1c, ax1d, ax18, ax11, ax16, ax1e, ax12, ax13, ax14, ax17, ax1b, ax1a, ax19, ax1f)
 DigitsVR(2)=Array(ax20, ax25, ax2c, ax2d, ax28, ax21, ax26, ax2e, ax22, ax23, ax24, ax27, ax2b, ax2a, ax29, ax2f)
 DigitsVR(3)=Array(ax30, ax35, ax3c, ax3d, ax38, ax31, ax36, ax3e, ax32, ax33, ax34, ax37, ax3b, ax3a, ax39, ax3f)
 DigitsVR(4)=Array(ax40, ax45, ax4c, ax4d, ax48, ax41, ax46, ax4e, ax42, ax43, ax44, ax47, ax4b, ax4a, ax49, ax4f)
 DigitsVR(5)=Array(ax50, ax55, ax5c, ax5d, ax58, ax51, ax56, ax5e, ax52, ax53, ax54, ax57, ax5b, ax5a, ax59, ax5f)
 DigitsVR(6)=Array(ax60, ax65, ax6c, ax6d, ax68, ax61, ax66, ax6e, ax62, ax63, ax64, ax67, ax6b, ax6a, ax69, ax6f)
 DigitsVR(7)=Array(ax70, ax75, ax7c, ax7d, ax78, ax71, ax76, ax7e, ax72, ax73, ax74, ax77, ax7b, ax7a, ax79, ax7f)
 DigitsVR(8)=Array(ax80, ax85, ax8c, ax8d, ax88, ax81, ax86, ax8e, ax82, ax83, ax84, ax87, ax8b, ax8a, ax89, ax8f)
 DigitsVR(9)=Array(ax90, ax95, ax9c, ax9d, ax98, ax91, ax96, ax9e, ax92, ax93, ax94, ax97, ax9b, ax9a, ax99, ax9f)
 DigitsVR(10)=Array(axa0, axa5, axac, axad, axa8, axa1, axa6, axae, axa2, axa3, axa4, axa7, axab, axaa, axa9, axaf)
 DigitsVR(11)=Array(axb0, axb5, axbc, axbd, axb8, axb1, axb6, axbe, axb2, axb3, axb4, axb7, axbb, axba, axb9, axbf)
 DigitsVR(12)=Array(axc0, axc5, axcc, axcd, axc8, axc1, axc6, axce, axc2, axc3, axc4, axc7, axcb, axca, axc9, axcf)
 DigitsVR(13)=Array(axd0, axd5, axdc, axdd, axd8, axd1, axd6, axde, axd2, axd3, axd4, axd7, axdb, axda, axd9, axdf)
 DigitsVR(14)=Array(axe0, axe5, axec, axed, axe8, axe1, axe6, axee, axe2, axe3, axe4, axe7, axeb, axea, axe9, axef)
 DigitsVR(15)=Array(axf0, axf5, axfc, axfd, axf8, axf1, axf6, axfe, axf2, axf3, axf4, axf7, axfb, axfa, axf9, axff)

 DigitsVR(16)=Array(bx00, bx05, bx0c, bx0d, bx08, bx01, bx06, bx0e, bx02, bx03, bx04, bx07, bx0b, bx0a, bx09, bx0f)
 DigitsVR(17)=Array(bx10, bx15, bx1c, bx1d, bx18, bx11, bx16, bx1e, bx12, bx13, bx14, bx17, bx1b, bx1a, bx19, bx1f)
 DigitsVR(18)=Array(bx20, bx25, bx2c, bx2d, bx28, bx21, bx26, bx2e, bx22, bx23, bx24, bx27, bx2b, bx2a, bx29, bx2f)
 DigitsVR(19)=Array(bx30, bx35, bx3c, bx3d, bx38, bx31, bx36, bx3e, bx32, bx33, bx34, bx37, bx3b, bx3a, bx39, bx3f)
 DigitsVR(20)=Array(bx40, bx45, bx4c, bx4d, bx48, bx41, bx46, bx4e, bx42, bx43, bx44, bx47, bx4b, bx4a, bx49, bx4f)
 DigitsVR(21)=Array(bx50, bx55, bx5c, bx5d, bx58, bx51, bx56, bx5e, bx52, bx53, bx54, bx57, bx5b, bx5a, bx59, bx5f)
 DigitsVR(22)=Array(bx60, bx65, bx6c, bx6d, bx68, bx61, bx66, bx6e, bx62, bx63, bx64, bx67, bx6b, bx6a, bx69, bx6f)
 DigitsVR(23)=Array(bx70, bx75, bx7c, bx7d, bx78, bx71, bx76, bx7e, bx72, bx73, bx74, bx77, bx7b, bx7a, bx79, bx7f)
 DigitsVR(24)=Array(bx80, bx85, bx8c, bx8d, bx88, bx81, bx86, bx8e, bx82, bx83, bx84, bx87, bx8b, bx8a, bx89, bx8f)
 DigitsVR(25)=Array(bx90, bx95, bx9c, bx9d, bx98, bx91, bx96, bx9e, bx92, bx93, bx94, bx97, bx9b, bx9a, bx99, bx9f)
 DigitsVR(26)=Array(bxa0, bxa5, bxac, bxad, bxa8, bxa1, bxa6, bxae, bxa2, bxa3, bxa4, bxa7, bxab, bxaa, bxa9, bxaf)
 DigitsVR(27)=Array(bxb0, bxb5, bxbc, bxbd, bxb8, bxb1, bxb6, bxbe, bxb2, bxb3, bxb4, bxb7, bxbb, bxba, bxb9, bxbf)
 DigitsVR(28)=Array(bxc0, bxc5, bxcc, bxcd, bxc8, bxc1, bxc6, bxce, bxc2, bxc3, bxc4, bxc7, bxcb, bxca, bxc9, bxcf)
 DigitsVR(29)=Array(bxd0, bxd5, bxdc, bxdd, bxd8, bxd1, bxd6, bxde, bxd2, bxd3, bxd4, bxd7, bxdb, bxda, bxd9, bxdf)
 DigitsVR(30)=Array(bxe0, bxe5, bxec, bxed, bxe8, bxe1, bxe6, bxee, bxe2, bxe3, bxe4, bxe7, bxeb, bxea, bxe9, bxef)
 DigitsVR(31)=Array(bxf0, bxf5, bxfc, bxfd, bxf8, bxf1, bxf6, bxfe, bxf2, bxf3, bxf4, bxf7, bxfb, bxfa, bxf9, bxff)

















'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate 
' how to make these updates. But in summary the following needs to be updated:	
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub


'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj			
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj		
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  END - FLEEP MECHANICAL SOUNDS
'******************************************************







'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5
	AddPt "Polarity", 2, 0.4, -5
	AddPt "Polarity", 3, 0.8, -5	
	AddPt "Polarity", 4, 0.85, -4.5
	AddPt "Polarity", 5, 0.9, -4
	AddPt "Polarity", 6, 0.95, -3.5
	AddPt "Polarity", 7, 1, -3
	AddPt "Polarity", 8, 1.05, -2.5
	AddPt "Polarity", 9, 1.1, -2
	AddPt "Polarity", 10, 1.15, -1.5
	AddPt "Polarity", 11, 1.2, -1
	AddPt "Polarity", 12, 1.25, -0.5
	AddPt "Polarity", 13, 1.3, 0

	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'******************************************************
'			FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
	
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property	
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property
	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
			case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub
	
	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next		
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
	
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'playsound "Knocker_1"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'		FLIPPER POLARITY AND RUBBER DAMPENER
'			SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

' Used for drop targets and flipper tricks
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

'******************************************************
'			FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	'FlipperTricks RightFlipper1, RFPress1, RFCount1, RFEndAngle1, RFState1
end sub


dim LFPress, RFPress, LFCount, RFCount, RFPress1, RFCount1, RFState1, RFEndAngle1
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.030

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
'RFEndAngle1 = RightFlipper1.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST 	
	Flipper.eostorqueangle = EOSA 	
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn

	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b
'		BOT = GetBalls
			
		For b = 0 to UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= -0.4 Then gBOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup			
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST	
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub

Sub LeftFlipper_Collide(parm)
'	debug.print "LF Collide bottom"
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	'FlipperHits
	'LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	'FlipperHits
	'RightFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
	'CheckLiveCatch Activeball, RightFlipper1, RFCount1, parm
	'FlipperHits
End Sub







'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	'TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	'TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class



'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'
Sub RDampen_Timer
	Cor.Update
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b', BOT
'	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			BallShadowA(b).visible = 1
			BallShadowA(b).X = gBOT(b).X + offsetX
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				BallShadowA(b).Y = gBOT(b).Y + offsetY + BallSize/10
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 1.04 + b/1000
				BallShadowA(b).Y = gBOT(b).Y + offsetY
			End If
		End If
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(3,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(3)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function



'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-10000) if you want to see the pf shadow through the ramp

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
' It's recommended that you be selective in which lights go in this collection, as there are limitations:
' 1. The shadows can "pass through" solid objects and other light sources, so be mindful of where the lights would actually able to cast shadows
' 2. If there are more than two equidistant sources, the shadows can suddenly switch on and off, so places like top and bottom lanes need attention
' 3. At this time the shadows get the light on/off from tracking gilvl, so if you have lights you want shadows for that are on at different times you will need to either:
'	a) remove this restriction (shadows think lights are always On)
'	b) come up with a custom solution (see TZ example in script)
' After confirming the shadows work in general, use ball control to move around and look for any weird behavior

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
' Example timer sub:

Sub FrameTimer_Timer()
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top for easy access by players
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' ** Change gBOT to BOT if using existing getballs code
' ** Includes lines commonly found there, for reference:
''	' stop the sound of deleted balls
''	For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
''		...rolling(b) = False
''		...StopSound("BallRoll_" & b)
''	Next
''
'' ...rolling and drop sounds...
''
''		If DropCount(b) < 5 Then
''			DropCount(b) = DropCount(b) + 1
''		End If
''
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			BallShadowA(b).visible = 1
'			BallShadowA(b).X = gBOT(b).X + offsetX
'			If gBOT(b).Z > 30 Then
'				BallShadowA(b).height=gBOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'				BallShadowA(b).Y = gBOT(b).Y + offsetY + BallSize/10
'			Else
'				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 1.04 + b/1000
'				BallShadowA(b).Y = gBOT(b).Y + offsetY
'			End If
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

'Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then 
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 1		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 5		'Offset y position under ball	 (^^for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
dim objrtx1(5), objrtx2(5)
dim objBallShadow(5)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

'Initialization
DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub

Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
		bsRampOff gBOT(num).ID
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim bsRampType
'	Dim gBOT: gBOT=getballs	'Uncomment if you're destroying balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)

	'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then

		'** Above the playfield
			If gBOT(s).Z > 30 Then
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update
				bsRampType = getBsRampType(gBOT(s).id)
'				debug.print bsRampType

				If Not bsRampType = bsRamp Then		'Primitive visible on PF
					objBallShadow(s).visible = 1
					objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
					objBallShadow(s).Y = gBOT(s).Y + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)		'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else								'Opaque, no primitive below
					objBallShadow(s).visible = 0
				End If

				If bsRampType = bsRampClear Or bsRampType = bsRamp Then		'Flasher visible on opaque ramp
					BallShadowA(s).visible = 1
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize/10
					BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Elseif bsRampType = bsWire Then								'Turn it off on wires
					BallShadowA(s).visible = 0
				End If

		'** On pf, primitive only
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then
				If Not OnPF(s) Then BallOnPlayfieldNow True, s
				objBallShadow(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				objBallShadow(s).Y = gBOT(s).Y + offsetY

		'** Under pf, flasher shadow only
			Else
				If OnPF(s) Then BallOnPlayfieldNow False, s
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000
			end if

	'Flasher shadow everywhere
		Elseif AmbientBallShadowOn = 2 Then
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY + BallSize/10
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = gBOT(s).X + (gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height= 1.04 + s/1000
			Else											'Under pf
				BallShadowA(s).X = gBOT(s).X + offsetX
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=gBOT(s).z - BallSize/4 + s/1000
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff Then' And gilvl > 0 Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y
					'objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + offsetY
					'objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

Sub bsRampOnWire()
	If bsDict.Exists(ActiveBall.ID) Then 
		bsDict.Item(ActiveBall.ID) = bsWire
	Else
		bsDict.Add ActiveBall.ID, bsWire
	End If
End Sub

Sub bsRampOn()
	If bsDict.Exists(ActiveBall.ID) Then 
		bsDict.Item(ActiveBall.ID) = bsRamp
	Else
		bsDict.Add ActiveBall.ID, bsRamp
	End If
End Sub

Sub bsRampOnClear()
	If bsDict.Exists(ActiveBall.ID) Then 
		bsDict.Item(ActiveBall.ID) = bsRampClear
	Else
		bsDict.Add ActiveBall.ID, bsRampClear
	End If
End Sub

Sub bsRampOff(idx)
	If bsDict.Exists(idx) Then 
		bsDict.Item(idx) = bsNone
	End If
End Sub

Function getBsRampType(id)
	Dim retValue
	If bsDict.Exists(id) Then
		retValue = bsDict.Item(id)
	Else
		retValue = bsNone
	End If

	getBsRampType = retValue

End Function


'sub Kicker1_Hit()
'	Dim speedx,speedy,finalspeed
'	speedx=activeball.velx
'	speedy=activeball.vely
'	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
'	if TableTilted=true then
'		tKickerTCount=0
'		Kicker1.Timerenabled=1
'		exit sub
'	end if
'	if finalspeed>10 then
'		Kicker1.Kick 0,0
'		activeball.velx=speedx
'		activeball.vely=speedy
'	else
''		mHole.MagnetOn=1
'		KickerHolder1.enabled=1
'	end if
'	
'end sub
'
'
'Sub Kicker1_Timer()
'	tKickerTCount=tKickerTCount+1
'	select case tKickerTCount
'	case 1:
'	'	mHole.MagnetOn=0
'		Pkickarm1.rotz=15
'		Pkickarm2.rotz=15
'		Pkickarm3.rotz=15
'		Pkickarm4.rotz=15
'		Pkickarm5.rotz=15
'		Playsound "saucer"
'		DOF 111, 2
'		Kicker1.kick 165,13
'	case 2:
'		Kicker1.timerenabled=0
'		Pkickarm1.rotz=0
'		Pkickarm2.rotz=0
'		Pkickarm3.rotz=0
'		Pkickarm4.rotz=0
'		Pkickarm5.rotz=0
'
'	end Select
'	
'end sub


sub dbreflect_Hit() : activeball.ReflectionEnabled=false : end Sub
sub dbreflect_UnHit() : activeball.ReflectionEnabled=true : end Sub
