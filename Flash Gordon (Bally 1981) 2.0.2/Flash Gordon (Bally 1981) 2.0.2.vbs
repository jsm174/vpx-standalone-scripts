'*************************************
'Flash Gordon (Bally 1981) - IPD No. 874
'VPX by rothbauerw, bord
'************************************

Option Explicit
Randomize

' ****************************************************
' OPTIONS
' ****************************************************

const VRRoom = 0 					' 0 - desktop or cab mode, 1 - Full VR Room
const Skyline = 0					' 0 - Mountain Top, 1 - Volcano
const cabmode = 0					' 0 - cab and siderails are visible, 1 - cab and siderails are hidden

const ball_image = 0				' 0 - light, 1 - HDR light, 2 H- DR medium, 3 - HDR Dark
const flipper_color = 0				' 0 - yellow, 1 - red

'----- Physics and Game Difficulty Options -----

Const Rubberizer = 1		     	'1 - rubber dampening version (rothbauerw), 2 - velocity and spin correction version (iaakki)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

'----- General Sound Options -----

Const VolumeDial = 0.8		'Values 0-1: global volume multiplier for mechanical sounds 
Const BallRollAmpFactor = 0       		' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)
Const RampRollAmpFactor = 1       		' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's


' ****************************************************
' END OPTIONS
' ****************************************************

' ****************************************************
' STANDARD DEFINITIONS AND INITIALIZATIONS
' ****************************************************

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim FSSMode: FSSMode = Table1.ShowFSS
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "01510000", "Bally.VBS", 3.1

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0

Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

'******************************************************
' 					TABLE INIT
'******************************************************

Const cGameName = "flashgdn"	'Set ROM flashgdn, flashgdv, flashgdf

Dim FGBall

Sub Table1_Init
	VPMInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Flash Gordon (Bally 1981)"
		.Games(cGameName).Settings.Value("rol") = 0 'rotated
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 1
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With
 
	'************  Main Timer init  ********************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	'************  Nudging   **************************

	vpmNudge.TiltSwitch = 7
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, RightSlingShot, LeftSlingShot)
 
	'************  Trough	**************************
	Set FGBall = Drain.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Controller.Switch(8) = 1

	Select Case ball_image
		Case 0: fgball.image = "ball_original"
		Case 1: fgball.image = "ball_light"
		Case 2: fgball.image = "ball_medium"
		Case 3: fgball.image = "ball_dark"
	End Select

	Dim flipcolor

	Select Case flipper_color
		Case 0: flipcolor = "flipperAOyellow"
		Case 1: flipcolor = "flipperAOred"
	End Select

	flipmesh_001.image = flipcolor
	flipmesh_002.image = flipcolor

	Select Case Skyline
		Case 0: skybox.image = "VR_SphereMtTop"
		Case 1: skybox.image = "VR_SphereLava"
	End Select

	GI_Bulbs.blenddisablelighting = 100

	bulb001.blenddisablelighting = 400
	bumpcap_001.blenddisablelighting = 5
	bulb002.blenddisablelighting = 400
	bumpcap_002.blenddisablelighting = 5

	bumpbase002.blenddisablelighting = 3
	

End Sub

'Sub Table1_Paused:Controller.Pause = 1:End Sub
'Sub Table1_unPaused:Controller.Pause = 0:End Sub
'Sub Table1_Exit:Controller.Stop:End Sub

'******************************************************
' 						KEYS
'******************************************************

dim plungerpress

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode = LeftFlipperKey then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress:FlipperActivate2 RightFlipper1, RFPress1
	
	If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull()
		plungerpress = 1
		VRPlunger.y = 2145
	End If

	if KeyCode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
	if KeyCode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
	if KeyCode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	if keycode = StartGameKey then 
		soundStartButton()
		VR_StartButton.transy = 0
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.transx = 8
	End If

	If keycode = RightFlipperKey Then 
		VRFlipperButtonRight.transx = - 8
	End If

	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyCode = PlungerKey Then
		Plunger.Fire
		plungerpress = 0

		If FGBall.x > 890 and FGBall.y > 1750 Then
			SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	If KeyCode = LeftFlipperKey then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress:FlipperDeActivate2 RightFlipper1, RFPress1

	if keycode = StartGameKey then 
		VR_StartButton.transy = -5
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.transx = 0
	End If

	If keycode = RightFlipperKey Then 
		VRFlipperButtonRight.transx = - 0
	End If

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

'******************************************************
'					SOLENOIDS
'******************************************************

SolCallback(1) = "DropLeftReset"					'1 = 6 - 4 Drop Target Reset
SolCallback(2) = "DropTopReset"						'2 = 7 - 3 Drop Target Reset
SolCallback(3) = "DropInlineReset"					'3 = 8 - In Line Drop Target
SolCallback(4) = "TwoWayKicker_Down"           		'4 = 3 - Saucer Kick Down

SolCallback(6) = "SolKnocker"			    		'6 = 2 - Knocker
SolCallback(7) = "SolOuthole"		           		'7 = 1 - OutHole
SolCallback(8) = "TwoWayKicker_Up"					'8 = 4 - Saucer Kick Up
SolCallback(9) = "DropSingleUp"						'9 = 5 - Single Drop Target Reset
'SolCallback(10) =									'10 = 9 - Left Bumper
'SolCallback(11) =									'11 = 10 - Rigth Bumper
SolCallback(12) = "DropSingleDown"					'12 = 11 - Single Drop Target Pull Down
'SolCallback(13) =									'13 = 12 - Top Bumper
'SolCallback(14) =									'14 = 13 - Left Slingshot
'SolCallback(15) =									'15 = 14 - Right Slingshot
'SolCallback(18) =									'18 = 15 - Coin Lockout Door
'SolCallback(19) = "vpmNudge.SolGameOn" 				'19 = 16 - KI Relay (Flipper enabled)

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sURFlipper) = "SolURFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	RandomSoundDrain drain	
	Controller.Switch(8) = 1
End Sub

Sub Drain_UnHit()
	Controller.Switch(8) = 0
End Sub

Sub SolOuthole(enabled)
	If enabled Then 
		Drain.kick 60,20
		RandomSoundBallRelease plunger		
	End If
End Sub

'******************************************************
'					KNOCKER
'******************************************************

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

'******************************************************
'				TWO-WAY KICKER
'******************************************************
Dim kickmov
kickmov = 40

Sub TwoWayKicker_Up(enabled)
	If enabled Then
		If Controller.Switch(30) = True then
			SoundSaucerKick 1, sw30
		Else
			SoundSaucerKick 0, sw30
		End If
		Controller.Switch(30) = 0
		sw30.Kick 30+rnd*1, 22+rnd*3, 0.7
		kickarm.rotx = kickmov * dSin(kickarm.rotz)
		kickarm.roty = -kickmov * dCos(kickarm.rotz)
		vpmTimer.AddTimer 150, "kickarm.rotx = 0:kickarm.roty = 0'"
	End If
End Sub

Sub TwoWayKicker_Down(enabled)
	If enabled Then
		If Controller.Switch(30) = True then
			SoundSaucerKick 1, sw30
		Else
			SoundSaucerKick 0, sw30
		End If
		Controller.Switch(30) = 0
		sw30.Kick 216.5+rnd*1, 21 
		kickarm.rotx = -kickmov * dSin(kickarm.rotz)
		kickarm.roty = kickmov * dCos(kickarm.rotz)
		vpmTimer.AddTimer 150, "kickarm.rotx = 0:kickarm.roty = 0'"
	End If
End Sub

sub sw30_hit()
	SoundSaucerLock
	Controller.Switch(30) = 1
End sub


'******************************************************
'				SLINGSHOTS
'******************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(activeball)
	RandomSoundSlingshotRight slingr
	vpmTimer.PulseSw 35
	rsling001.Visible = 1
	rsling.Visible = 0
	slingr.TransZ = -16
	RStep = 0
	RightSlingShot.TimerEnabled = 1	
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:rsling001.Visible = 0:rsling002.Visible = 1:slingr.TransZ = -8
		Case 4:rsling002.Visible = 0:rsling.Visible = 1:slingr.TransZ = 0:RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(activeball)
	RandomSoundSlingshotLeft slingl
	vpmTimer.PulseSw 36
	lsling001.Visible = 1
	lsling.Visible = 0
	slingl.TransZ = -16
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:lsling001.Visible = 0:lsling002.Visible = 1:slingl.TransZ = -8
		Case 4:lsling002.Visible = 0:lsling.Visible = 1:slingl.TransZ = 0:LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

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

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'    dim rx, ry
'    rx = x*dCos(angle) - y*dSin(angle)
'    ry = x*dSin(angle) + y*dCos(angle)
'    RotPoint = Array(rx,ry)
'End Function

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


'******************************************************
'			BUMPERS AND SKIRT ANIMATION
'******************************************************

Sub Bumper1_Hit
	vpmTimer.PulseSw 40
	RandomSoundBumperBottom Bumper1

	bumperskirt001.roty=skirtAY(me,Activeball)
	bumperskirt001.rotx=skirtAX(me,Activeball)
	me.timerinterval = 150
	me.timerenabled=1
End Sub

sub bumper1_timer
	bumperskirt001.rotx=0
	bumperskirt001.roty=0
	me.timerenabled=0
end sub

Sub Bumper2_Hit
	vpmTimer.PulseSw 39
	RandomSoundBumperMiddle Bumper2

	bumperskirt002.roty=skirtAY(me,Activeball)
	bumperskirt002.rotx=skirtAX(me,Activeball)
	me.timerinterval = 150
	me.timerenabled=1
End Sub

sub bumper2_timer
	bumperskirt002.rotx=0
	bumperskirt002.roty=0
	me.timerenabled=0
end sub

Sub Bumper3_Hit
	vpmTimer.PulseSw 37
	RandomSoundBumperTop Bumper3

	bumperskirt003.roty=skirtAY(me,Activeball)
	bumperskirt003.rotx=skirtAX(me,Activeball)
	me.timerinterval = 150
	me.timerenabled=1
End Sub
 
sub bumper3_timer
	bumperskirt003.rotx=0
	bumperskirt003.roty=0
	me.timerenabled=0
end sub

'******************************************************
'			SKIRT ANIMATION FUNCTIONS
'******************************************************
' NOTE: set bumper object timer to around 150-175 in order to be able
' to actually see the animaation, adjust to your liking

'Const PI = 3.1415926
Const SkirtTilt=5		'angle of skirt tilting in degrees

Function SkirtAX(bumper, bumperball)
	skirtAX=cos(skirtA(bumper,bumperball))*(SkirtTilt)		'x component of angle
	if (bumper.y<bumperball.y) then	skirtAX=skirtAX*-1	'adjust for ball hit bottom half
End Function

Function SkirtAY(bumper, bumperball)
	skirtAY=sin(skirtA(bumper,bumperball))*(SkirtTilt)		'y component of angle
	if (bumper.x>bumperball.x) then	skirtAY=skirtAY*-1	'adjust for ball hit left half
End Function

Function SkirtA(bumper, bumperball)
	dim hitx, hity, dx, dy
	hitx=bumperball.x
	hity=bumperball.y

	dy=Abs(hity-bumper.y)					'y offset ball at hit to center of bumper
	if dy=0 then dy=0.0000001
	dx=Abs(hitx-bumper.x)					'x offset ball at hit to center of bumper
	skirtA=(atn(dx/dy)) '/(PI/180)			'angle in radians to ball from center of Bumper1
End Function


'******************************************************
'						SWITCHES
'******************************************************

'******  Rubber Switches *****
Sub phys_sw5_hit()
	vpmTimer.PulseSw 5
End Sub

Sub phys_sw29_hit()
	vpmTimer.PulseSw 29
End Sub

'*******	Rollover Switches	******************
Sub sw1a_Hit:vpmTimer.PulseSw 1:me.timerenabled = 0:AnimateStar star1a, sw1a, 1:End Sub
Sub sw1a_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw1a_timer:AnimateStar star1a, sw1a, 0:End Sub

Sub sw1b_Hit:vpmTimer.PulseSw 1:me.timerenabled = 0:AnimateStar star1b, sw1b, 1:End Sub
Sub sw1b_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw1b_timer:AnimateStar star1b, sw1b, 0:End Sub

Sub sw1c_Hit:vpmTimer.PulseSw 1:me.timerenabled = 0:AnimateStar star1c, sw1c, 1:End Sub
Sub sw1c_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw1c_timer:AnimateStar star1c, sw1c, 0:End Sub

Sub sw1d_Hit:vpmTimer.PulseSw 1:me.timerenabled = 0:AnimateStar star1d, sw1d, 1:End Sub
Sub sw1d_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw1d_timer:AnimateStar star1d, sw1d, 0:End Sub

Sub sw2a_Hit:vpmTimer.PulseSw 2:me.timerenabled = 0:AnimateStar star2a, sw2a, 1:End Sub
Sub sw2a_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw2a_timer:AnimateStar star2a, sw2a, 0:End Sub

Sub sw2b_Hit:vpmTimer.PulseSw 2:me.timerenabled = 0:AnimateStar star2b, sw2b, 1:End Sub
Sub sw2b_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw2b_timer:AnimateStar star2b, sw2b, 0:End Sub

Sub sw2c_Hit:vpmTimer.PulseSw 2:me.timerenabled = 0:AnimateStar star2c, sw2c, 1:End Sub
Sub sw2c_UnHit:me.timerinterval = 7:me.timerenabled = 1:End Sub
Sub sw2c_timer:AnimateStar star2c, sw2c, 0:End Sub

Sub AnimateStar(prim, sw, action) ' Action = 1 - to drop, 0 to raise
	If action = 1 Then
		RandomSoundRollover
		prim.transz = -6
	Else
		prim.transz = prim.transz + 0.5
		if prim.transz = -3 and Rnd() < 0.05 Then
			sw.timerenabled = 0
		Elseif prim.transz >= 0 Then
			prim.transz = 0
			sw.timerenabled = 0
		End If
	End If
End Sub 

Sub sw4_Hit:vpmTimer.PulseSw 4:AnimateWire wire_sw4, 1:End Sub
Sub sw4_UnHit:AnimateWire wire_sw4, 0:End Sub

Sub sw13_Hit:vpmTimer.PulseSw 13:AnimateWire wire_sw13, 1:End Sub
Sub sw13_UnHit:AnimateWire wire_sw13, 0:End Sub

Sub sw14_Hit:vpmTimer.PulseSw 14:AnimateWire wire_sw14, 1:End Sub
Sub sw14_UnHit:AnimateWire wire_sw14, 0:End Sub

Sub sw31_Hit:vpmTimer.PulseSw 31:AnimateWire wire_sw31, 1:End Sub
Sub sw31_UnHit:AnimateWire wire_sw31, 0:End Sub

Sub sw32_Hit:vpmTimer.PulseSw 32:AnimateWire wire_sw32, 1:End Sub
Sub sw32_UnHit:AnimateWire wire_sw32, 0:End Sub

Sub AnimateWire(prim, action) ' Action = 1 - to drop, 0 to raise)
	If action = 1 Then
		RandomSoundRollover
		prim.transz = -13
	Else
		prim.transz = 0
	End If
End Sub


'***************	Spinners	******************

Sub sw33_Spin():vpmTimer.PulseSw (33):SoundSpinner sw33:End Sub
Sub sw34_Spin():vpmTimer.PulseSw (34):SoundSpinner sw34:End Sub

'******************************************************
'						WALLS
'******************************************************


Sub RightOrbitWall_Hit()
	If me.timerenabled = false Then
		RandomSoundMetal
		me.timerenabled = True
	End If
End Sub

Sub RightOrbitWall_Timer
	me.timerenabled = false
End Sub

Sub LeftOrbitWall_Hit()
	If me.timerenabled = false Then
		RandomSoundMetal
		me.timerenabled = True
	End If
End Sub

Sub LeftOrbitWall_Timer
	me.timerenabled = false
End Sub



'******************************************************
'						TARGETS 
'******************************************************

' stand-ups
Sub sw12_hit():STHit 12:End Sub
Sub sw15_hit():STHit 15:End Sub
Sub sw24_hit():STHit 24:End Sub
Sub sw28_hit():STHit 28:End Sub

' drops
Sub sw3_hit():DTHit 3:End Sub

Sub sw17_hit():DTHit 17:End Sub
Sub sw18_hit():DTHit 18:End Sub
Sub sw19_hit():DTHit 19:End Sub
Sub sw20_hit():DTHit 20:End Sub

Sub sw21_hit():DTHit 21:End Sub
Sub sw22_hit():DTHit 22:End Sub
Sub sw23_hit():DTHit 23:End Sub

Sub sw25_hit():DTHit 25:End Sub
Sub sw26_hit():DTHit 26:End Sub
Sub sw27_hit():DTHit 27:End Sub

Sub DropLeftReset(enabled)
	If enabled Then
		DTRaise 17
		DTRaise 18
		DTRaise 19
		DTRaise 20
		RandomSoundDropTargetReset psw18	
	End If
End Sub

Sub DropTopReset(enabled)
	If enabled Then
		DTRaise 21
		DTRaise 22
		DTRaise 23
		RandomSoundDropTargetReset psw22
	End If
End Sub

Sub DropInlineReset(enabled)
	If enabled Then
		DTRaise 25
		DTRaise 26
		DTRaise 27
		RandomSoundDropTargetReset psw26
	End If
End Sub

Sub DropSingleUp(enabled)
	If enabled Then
		DTRaise 3
		RandomSoundDropTargetReset psw3
	End If
End Sub

Sub DropSingleDown(enabled)
	If enabled Then
		DTDrop 3
		RandomSoundDropTargetReset psw3
	End If
End Sub

'******************************************************
'		DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
	Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped
  
	Public Property Get Primary(): Set Primary = m_primary: End Property
	Public Property Let Primary(input): Set m_primary = input: End Property
  
	Public Property Get Secondary(): Set Secondary = m_secondary: End Property
	Public Property Let Secondary(input): Set m_secondary = input: End Property
  
	Public Property Get Prim(): Set Prim = m_prim: End Property
	Public Property Let Prim(input): Set m_prim = input: End Property
  
	Public Property Get Sw(): Sw = m_sw: End Property
	Public Property Let Sw(input): m_sw = input: End Property
  
	Public Property Get Animate(): Animate = m_animate: End Property
	Public Property Let Animate(input): m_animate = input: End Property
  
	Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
	Public Property Let IsDropped(input): m_isDropped = input: End Property
  
	Public default Function init(primary, secondary, prim, sw, animate, isDropped)
	  Set m_primary = primary
	  Set m_secondary = secondary
	  Set m_prim = prim
	  m_sw = sw
	  m_animate = animate
	  m_isDropped = isDropped
  
	  Set Init = Me
	End Function
  End Class

'Define a variable for each drop target
Dim DT3, DT17, DT18, DT19, DT20, DT21, DT22, DT23, DT25, DT26, DT27

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
'
'	Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 

Set DT3 = (new DropTarget)(sw3, sw3y, psw3, 3, 0, false)
Set DT17 = (new DropTarget)(sw17, sw17y, psw17, 17, 0, false)
Set DT18 = (new DropTarget)(sw18, sw18y, psw18, 18, 0, false)
Set DT19 = (new DropTarget)(sw19, sw19y, psw19, 19, 0, false)
Set DT20 = (new DropTarget)(sw20, sw20y, psw20, 20, 0, false)
Set DT21 = (new DropTarget)(sw21, sw21y, psw21, 21, 0, false)
Set DT22 = (new DropTarget)(sw22, sw22y, psw22, 22, 0, false)
Set DT23 = (new DropTarget)(sw23, sw23y, psw23, 23, 0, false)
Set DT25 = (new DropTarget)(sw25, sw25y, psw25, 25, 0, false)
Set DT26 = (new DropTarget)(sw26, sw26y, psw26, 26, 0, false)
Set DT27 = (new DropTarget)(sw27, sw27y, psw27, 27, 0, false)


'Add all the Drop Target Arrays to Drop Target Animation Array
' DTAnimationArray = Array(DT1, DT2, ....)
Dim DTArray
DTArray = Array(DT3, DT17, DT18, DT19, DT20, DT21, DT22, DT23, DT25, DT26, DT27)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 			'in milliseconds
Const DTDropUpSpeed = 40 			'in milliseconds
Const DTDropUnits = 46 			'VP units primitive drops
Const DTDropUpUnits = 5 			'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 				'max degrees primitive rotates when hit
Const DTDropDelay = 20	 		'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40	 		'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30				'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 1			'Set to 0 to disable bricking, 1 to enable bricking
Const DTMass = 0.2				'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	PlayTargetSound
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
		DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = -1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
	Next
End Function


sub DTBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
	dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
	Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		DTCheckBrick = 4
	Else 
		DTCheckBrick = 0
	End If
End Function

Sub DoDTAnim()
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
	dim transz
	Dim animtime, rangle

	rangle = prim.rotz * 3.1416 / 180

	DTAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If (animate = 1 or animate = 4) and animtime < DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = animate
		Exit Function
	elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If

	if animate = 2 Then
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
		end if

		prim.rotx = DTMaxBend * cos(rangle)/2
		prim.roty = DTMaxBend * sin(rangle)/2

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			'prim.blenddisablelighting = 0.2
			secondary.collidable = 0
			controller.Switch(Switch) = 1
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		end If 
	End If

	If animate = 3 and animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
	elseif animate = 3 and animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If

	if animate = -1 Then
		transz = (1 - (animtime/DTDropUpSpeed)) *  DTDropUnits * -1

		If prim.transz = -DTDropUnits Then
			Dim BOT, b
			BOT = GetBalls

			For b = 0 to UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and BOT(b).z < prim.z+DTDropUnits+25 Then
					BOT(b).velz = 20
				End If
			Next
		End If

		if prim.transz < 0 Then
			'prim.blenddisablelighting = 0.35
			prim.transz = transz
		elseif transz > 0 then
			prim.transz = transz
		end if

		if prim.transz > DTDropUpUnits then 
			prim.transz = DTDropUpUnits
			DTAnimate = -2
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		controller.Switch(Switch) = 0
	End If

	if animate = -2 and animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
		if prim.transz < 0 then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0

			primary.collidable = 1
			secondary.collidable = 0
		end If 
	End If
End Function

'******************************************************
'		DROP TARGET
'		SUPPORTING FUNCTIONS 
'******************************************************

' Used for drop targets
Function Atn2(dy, dx)
	dim pi
	pi = 4*Atn(1)

	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

' Used for drop targets
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


Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
End Function

'******************************************************
'		STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
	Private m_primary, m_prim, m_sw, m_animate
  
	Public Property Get Primary(): Set Primary = m_primary: End Property
	Public Property Let Primary(input): Set m_primary = input: End Property
  
	Public Property Get Prim(): Set Prim = m_prim: End Property
	Public Property Let Prim(input): Set m_prim = input: End Property
  
	Public Property Get Sw(): Sw = m_sw: End Property
	Public Property Let Sw(input): m_sw = input: End Property
  
	Public Property Get Animate(): Animate = m_animate: End Property
	Public Property Let Animate(input): m_animate = input: End Property
  
	Public default Function init(primary, prim, sw, animate)
	  Set m_primary = primary
	  Set m_prim = prim
	  m_sw = sw
	  m_animate = animate
  
	  Set Init = Me
	End Function
  End Class
  

'Define a variable for each stand-up target
Dim ST12, ST15, ST24, ST28

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0

Set ST12 = (new StandupTarget)(sw12, psw12,12, 0)
Set ST15 = (new StandupTarget)(sw15, psw15,15, 0)
Set ST24 = (new StandupTarget)(sw24, psw24,24, 0)
Set ST28 = (new StandupTarget)(sw28, psw28,28, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST12, ST15, ST24, ST28)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit
Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

	PlayTargetSound
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).sw = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180	
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		STCheckHit = 1
	Else 
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime

	STAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If animate = 1 Then
		primary.collidable = 0
		prim.transy = -STMaxOffset
		vpmTimer.PulseSw switch
		STAnimate = 2
		Exit Function
	elseif animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else 
			STAnimate = 2
		End If
	End If	
End Function

'******************************************************
'						FLIPPERS
'******************************************************
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
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
		RightFlipper1.RotateToEnd
		If RightFlipper1.currentangle > RightFlipper1.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper1
		Else 
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If	
    Else
		RightFlipper1.RotateToStart
		If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper1
		End If
	End If
End Sub

'******************************************************
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperTricks2 RightFlipper1, RFPress1, RFCount1, RFEndAngle1, RFState1
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		If Flipper2.currentangle = EndAngle2 Then 
			BOT = GetBalls
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then 
				EOSNudge1 = 0
		end if
	End If
End Sub

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
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount, RFPress1, RFCount1
dim LFState, RFState, RFState1
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim EOST2, EOSA2,FReturn2
dim RFEndAngle, LFEndAngle, RFEndAngle1

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return

Const EOSTnew = 1.5 
Const EOSAnew = 1
Const EOSRampup = 0

EOST2 = rightflipper1.eostorque
EOSA2 = rightflipper1.eostorqueangle
FReturn2 = rightFlipper1.return
Const EOSTnew2 = 2 'EM

Dim SOSRampup:SOSRampup = 2.5

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.045
Const EOSReturn2 = 0.055  'EM's

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle1 = RightFlipper1.endangle

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
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

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

Sub FlipperActivate2(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST2         
	Flipper.eostorqueangle = EOSA2         
End Sub

Sub FlipperDeactivate2(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA2
	Flipper.eostorque = EOST2*EOSReturn2/FReturn2
End Sub

Sub FlipperTricks2 (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

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
			Flipper.eostorque = EOSTnew2
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST2        
			Flipper.eostorqueangle = EOSA2
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
    Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime : CatchTime = GameTime - FCount

    if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
            if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
                    LiveCatchBounce = 0
            else
                    LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
            end If

            If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx= 0
            ball.angmomy= 0
            ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, Rubberizer
    End If
End Sub

'******************************************************
' 				FLIPPER COLLIDE
'******************************************************

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
 	RightFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
 	RightFlipperCollide parm
End Sub

'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
	Next


	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -2.7        
	AddPt "Polarity", 2, 0.33, -2.7
	AddPt "Polarity", 3, 0.37, -2.7        
	AddPt "Polarity", 4, 0.41, -2.7
	AddPt "Polarity", 5, 0.45, -2.7
	AddPt "Polarity", 6, 0.576,-2.7
	AddPt "Polarity", 7, 0.66, -1.8
	AddPt "Polarity", 8, 0.743, -0.5
	AddPt "Polarity", 9, 0.81, -0.5
	AddPt "Polarity", 10, 0.88, 0

	addpt "Velocity", 0, 0,         1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41,         1.05
	addpt "Velocity", 3, 0.53,         1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03,         0.945

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
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
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

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				'playsound "Knocker_1"
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function

'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)

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

'######################### Add Dampenf to Dampener Class 
'#########################    Only applies dampener when abs(velx) < 2 and vely < 0 and vely > -3.75  

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm, ver)
		if ver = 2 Then
			If parm < 10 And parm > 2 And Abs(aball.angmomz) < 15 And aball.vely < 0 then
				aball.angmomz = aball.angmomz * 1.2
				aball.vely = aball.vely * (1.1 + (parm/50))
			Elseif parm <= 2 and parm > 0.2 And aball.vely < 0 Then
				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        	aball.angmomz = aball.angmomz * -0.7
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				aball.vely = aball.vely * (1.2 + (parm/10))
			End if
		Else
			dim RealCOR, DesiredCOR, str, coef
			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
			End If
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub

	Public Sub Report() 	'debug, reports all coords in tbPL.text
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
    public ballvel, ballvelx, ballvely, ballvelz, ballangmomx, ballangmomy, ballangmomz

    Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : redim ballvelz(0) : redim ballangmomx(0) : redim ballangmomy(0): redim ballangmomz(0): End Sub 

    Public Sub Update()    'tracks in-ball-velocity
        dim str, b, AllBalls, highestID : allBalls = getballs

        for each b in allballs
            if b.id >= HighestID then highestID = b.id
        Next

        if uBound(ballvel) < highestID then redim ballvel(highestID)    'set bounds
        if uBound(ballvelx) < highestID then redim ballvelx(highestID)    'set bounds
        if uBound(ballvely) < highestID then redim ballvely(highestID)    'set bounds
        if uBound(ballvelz) < highestID then redim ballvelz(highestID)    'set bounds
        if uBound(ballangmomx) < highestID then redim ballangmomx(highestID)    'set bounds
        if uBound(ballangmomy) < highestID then redim ballangmomy(highestID)    'set bounds
        if uBound(ballangmomz) < highestID then redim ballangmomz(highestID)    'set bounds

        for each b in allballs
            ballvel(b.id) = BallSpeed(b)
            ballvelx(b.id) = b.velx
            ballvely(b.id) = b.vely
            ballvelz(b.id) = b.velz
            ballangmomx(b.id) = b.angmomx
            ballangmomy(b.id) = b.angmomy
            ballangmomz(b.id) = b.angmomz
        Next
    End Sub
End Class

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
    elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		if aball.vely > 3 then	'only hard hits
			Select Case Int(Rnd * 4) + 1
				Case 1: zMultiplier = defvalue+1.1
				Case 2: zMultiplier = defvalue+1.05
				Case 3: zMultiplier = defvalue+0.7
				Case 4: zMultiplier = defvalue+0.3
			End Select
			aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
			'debug.print "----> velz: " & activeball.velz
			'debug.print "conservation check: " & BallSpeed(aBall)/vel
		End If
	end if
end sub

'******************************************************
'	REAL-TIME UPDATES (Ball Rolling, Shadows, Etc)
'******************************************************

' ***** Physics, Animations, Shadows, and Sounds
Sub GameTimer_Timer()
	Cor.Update			'Dampener
	DoDTAnim			'Drop Target Animations
	DoSTAnim			'Stand Up Target Animations
	UpdateMechs			'Flipper, gates, and plunger updates
	SoundUpdates		'Rolling & Drop Sounds
	If DynamicBallShadowsOn Then DynamicBSUpdate

End Sub

Sub UpdateMechs()
	rflip.Rotz = RightFlipper1.currentangle
	rflipr.Rotz = RightFlipper1.currentangle

	flipmesh_001.Rotz = LeftFlipper.currentangle
	flipmesh_002.Rotz = RightFlipper.currentangle

	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

	pSpinner2.rotx = sw33.currentangle
	pSpinnerRod2.transy = -5 * dCos(pSpinner2.rotx)
	pSpinnerRod2.transx = 5 * dSin(pSpinner2.rotx)

	pSpinner1.rotx = sw34.currentangle
	pSpinnerRod1.transy = -5 * dCos(pSpinner1.rotx)
	pSpinnerRod1.transx = 5 * dSin(pSpinner1.rotx) 

	'*******************************************
	'  VR Plunger Code
	'*******************************************
	If plungerpress = 1 then
		If VRPlunger.Y < 2145 + 100 then
			VRPlunger.Y = VRPlunger.Y + 5*10/25
		End If
	Else
		VRPlunger.Y = 2145 + (5* Plunger.Position) - 20
	End If

End Sub

'******************************************************
'      		Rolling Sounds & Ball Shadows
'******************************************************

Const tnob = 2 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Dim ampFactor

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
	Select Case BallRollAmpFactor
		Case 0
			ampFactor = "_amp0"
		Case 1
			ampFactor = "_amp2_5"
		Case 2
			ampFactor = "_amp5"
		Case 3
			ampFactor = "_amp7_5"
		Case 4
			ampFactor = "_amp9"
		Case Else
			ampFactor = "_amp0"
	End Select
End Sub

Sub SoundUpdates()
	Dim b: b=0

	If BallVel(FGBall ) > 1 AND (FGBall.z < 30 or FGBall.z > 75)  Then
		rolling(b) = True
		PlaySound ("BallRoll_" & b & ampFactor), -1, VolPlayfieldRoll(FGBall) * 1.1 * VolumeDial, AudioPan(FGBall), 0, PitchPlayfieldRoll(FGBall), 1, 0, AudioFade(FGBall)
	Else
		If rolling(b) = True Then
			StopSound("BallRoll_" & b & ampFactor)
			rolling(b) = False
		End If
	End If

	' "Static" Ball Shadows
	If AmbientBallShadowOn = 0 Then
		If FGBall.Z > 30 and FGBall.z < 75 Then
			BallShadowA(b).height=FGBall.z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
		Else
			BallShadowA(b).height=FGBall.z - BallSize/2 + 5
		End If
		BallShadowA(b).Y = FGBall.Y + Ballsize/5 + fovY
		BallShadowA(b).X = FGBall.X
		BallShadowA(b).visible = 1

	' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

	ElseIf AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
		If FGBall.Z > 30 and FGBall.z < 75 Then							'The flasher follows the ball up ramps while the primitive is on the pf
			If FGBall.X < tablewidth/2 Then
				objBallShadow(b).X = ((FGBall.X) - (Ballsize/10) + ((FGBall.X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
			Else
				objBallShadow(b).X = ((FGBall.X) + (Ballsize/10) + ((FGBall.X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
			End If
			objBallShadow(b).Y = FGBall.Y + BallSize/10 + fovY
			objBallShadow(b).visible = 0

			BallShadowA(b).X = FGBall.X
			BallShadowA(b).Y = FGBall.Y + BallSize/5 + fovY
			BallShadowA(b).height=FGBall.z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			BallShadowA(b).visible = 1
		Elseif (FGBall.Z <= 30 And FGBall.Z > 10) or (FGBall.Z <= 90 And FGBall.Z => 75)  Then	'On pf, primitive only
			objBallShadow(b).visible = 1
			If FGBall.X < tablewidth/2 Then
				objBallShadow(b).X = ((FGBall.X) - (Ballsize/10) + ((FGBall.X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
			Else
				objBallShadow(b).X = ((FGBall.X) + (Ballsize/10) + ((FGBall.X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
			End If
			objBallShadow(b).Y = FGBall.Y + fovY
			objBallShadow(b).Z = FGBall.Z - 15
			BallShadowA(b).visible = 0
		Else	'Under pf, no shadows
			objBallShadow(b).visible = 0
			BallShadowA(b).visible = 0
		end if

	Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
		If FGBall.Z > 30 and FGBall.z < 75 Then						'In a ramp
			BallShadowA(b).X = FGBall.X
			BallShadowA(b).Y = FGBall.Y + BallSize/5 + fovY
			BallShadowA(b).height=FGBall.z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			BallShadowA(b).visible = 1
		Elseif (FGBall.Z <= 30 And FGBall.Z > 10) or (FGBall.Z <= 90 And FGBall.Z > 75) Then	'On pf
			BallShadowA(b).visible = 1
			If FGBall.X < tablewidth/2 Then
				BallShadowA(b).X = ((FGBall.X) - (Ballsize/10) + ((FGBall.X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
			Else
				BallShadowA(b).X = ((FGBall.X) + (Ballsize/10) + ((FGBall.X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
			End If
			BallShadowA(b).Y = FGBall.Y + Ballsize/10 + fovY
			BallShadowA(b).height=FGBall.z - BallSize/2 + 5
		Else											'Under pf
			BallShadowA(b).visible = 0
		End If
	End If


	'***Ball Drop Sounds***
	If FGBall.VelZ < -1 and FGBall.z < 55 and FGBall.z > 27 Then 'height adjust for ball drop sounds
		If DropCount(b) >= 5 Then
			'DropCount(b) = 0
			If FGBall.velz > -5 Then
				If FGBall.z < 35 Then
					DropCount(b) = 0
					RandomSoundBallBouncePlayfieldSoft FGBall
				End If
			Else
				DropCount(b) = 0
				RandomSoundBallBouncePlayfieldHard FGBall
			End If				
		End If
	End If
	If DropCount(b) < 5 Then
		DropCount(b) = DropCount(b) + 1
	End If
End Sub


'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
 
Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              ' Setup lamp assignments

LampTimer.Interval = -1
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
			if chglamp(x, 0) = 116 then 
				if chglamp(x, 1) = 1 then Lampz.state(chglamp(x, 0)) = 0 else Lampz.state(chglamp(x, 0)) = 1
			end if
		next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0

LampTimer2.Interval = -1
LampTimer2.Enabled = 1

Sub LampTimer2_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	DisplayTimer
	UpdateTextBoxes
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

sub DisableLighting2(pri, DLintensityMax, DLintensityMin, ByVal aLvl)    'cp's script  DLintensity = disabled lighting intesity
    if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)    'Callbacks don't get this filter automatically
    pri.blenddisablelighting = (aLvl * (DLintensityMax-DLintensityMin)) + DLintensityMin
End Sub

Sub MatSwap(pri, group, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 1:pri.Material = group(0) 'Full
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
              Case 4:pri.Material = group(3) 'Off
    End Select
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub FadeMaterial(mat, wrap, roughness, glossy, thickness, edge, edgealpha, opacity, basecol, glossycol, clearcoatcol, isMetal, actopacity, ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	UpdateMaterial mat,wrap,roughness,glossy,thickness,edge,edgealpha,aLvl^3*opacity,basecol,glossycol,clearcoatcol,isMetal,actopacity,0,0,0,0
	if mat = VR_BackglassFlasher.material and aLvl = 1 then Lampz.state(116) = 0
End Sub

Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/7.5 : next

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(1)= l1									' LMiniBonus1
	Lampz.Callback(1) = "DisableLighting2 p1, 0.8, 0.1,"
	Lampz.MassAssign(2)= l2									' LMiniBonus5
	Lampz.Callback(2) = "DisableLighting2 p2, 0.8, 0.1,"
	Lampz.MassAssign(3)= l3									' LMiniBonus9
	Lampz.Callback(3) = "DisableLighting2 p3, 0.8, 0.1,"
	Lampz.MassAssign(4)= l4									' LBonus1
	Lampz.Callback(4) = "DisableLighting2 p4, 0.8, 0.1,"
	Lampz.MassAssign(5)= l5									' LBonus5
	Lampz.Callback(5) = "DisableLighting2 p5, 0.8, 0.1,"
	Lampz.MassAssign(6)= l6									' LBonus9
	Lampz.Callback(6) = "DisableLighting2 p6, 0.8, 0.1,"
	Lampz.MassAssign(7)= l7									' LLower2xBonus
	Lampz.Callback(7) = "DisableLighting2 p7, 30, 1,"
	Lampz.Callback(7) = "DisableLighting2 bulb7, 400, 5,"
	Lampz.MassAssign(8)= l8									' LUpperTripleDropTargetArrow1
	Lampz.Callback(8) = "DisableLighting2 p8, 0.8, 0.1,"
	Lampz.MassAssign(9)= l9									' LLowerDropTragetsOrange
	Lampz.Callback(9) = "DisableLighting2 p9, 30, 1,"
	Lampz.Callback(9) = "DisableLighting2 bulb9, 400, 5,"
	Lampz.MassAssign(10)= l10								' LLowerRightTargetWhite
	Lampz.Callback(10) = "DisableLighting2 p10, 0.8, 0.1,"
	'Const LBGShootAgain = 11
	Lampz.Callback(11) = "FadeMaterial VR_BackglassShootAgain.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	Lampz.MassAssign(12) = L12								' LCenterBonus10K
	Lampz.Callback(12) = "DisableLighting2 p12, 30, 1,"
	Lampz.Callback(12) = "DisableLighting2 bulb12, 400, 5,"
	'Const LBallInPlay	= 13
	Lampz.Callback(13) = "FadeMaterial VR_BackglassBallInPlay.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	Lampz.MassAssign(14) = upperbumperlight001				' LTopBumper
	Lampz.MassAssign(14) = upperbumperlight002
	Lampz.Callback(14) = "DisableLighting2 bulb003, 400, 0,"
	Lampz.Callback(14) = "DisableLighting2 bumpcap_003, 5, 0.5,"
	Lampz.Callback(14) = "DisableLighting2 bumpbase001, 3, 1,"
	Lampz.Callback(14) = "DisableLighting2 bumperskirt003, 2, 1,"

	Lampz.MassAssign(15) = L15								' LRightOutlane
	Lampz.Callback(15) = "DisableLighting2 p15, 0.9, 0,"
	Lampz.MassAssign(17) = L17								' LMiniBonus2
	Lampz.Callback(17) = "DisableLighting2 p17, 0.8, 0.1,"
	Lampz.MassAssign(18) = L18								' LMiniBonus6
	Lampz.Callback(18) = "DisableLighting2 p18, 0.8, 0.1,"
	Lampz.MassAssign(19) = L19								' LMiniBonus10
	Lampz.Callback(19) = "DisableLighting2 p19, 0.8, 0.1,"
	Lampz.MassAssign(20) = L20								' LBonus2
	Lampz.Callback(20) = "DisableLighting2 p20, 0.8, 0.1,"
	Lampz.MassAssign(21) = L21								' LBonus6
	Lampz.Callback(21) = "DisableLighting2 p21, 0.8, 0.1,"
	Lampz.MassAssign(22) = L22								' LBonus10
	Lampz.Callback(22) = "DisableLighting2 p22, 0.8, 0.1,"
	Lampz.MassAssign(23) = L23								' Lower3xBonus
	Lampz.Callback(23) = "DisableLighting2 p23, 30, 1,"
	Lampz.Callback(23) = "DisableLighting2 bulb23, 400, 5,"
	Lampz.MassAssign(24) = L24								' LUpperTripleDropTargetArrow2
	Lampz.Callback(24) = "DisableLighting2 p24, 0.8, 0.1,"
	Lampz.MassAssign(25) = L25								' LLowerDropTragetsYellow
	Lampz.Callback(25) = "DisableLighting2 p25, 0.8, 0.1,"
	Lampz.MassAssign(26) = L26								' LRightFlipperlane
	Lampz.Callback(26) = "DisableLighting2 p26, 0.8, 0.1,"
	'Const LBGMatch	= 27
	Lampz.Callback(27) = "FadeMaterial VR_BackglassMatch.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	Lampz.MassAssign(28) = L28								' LCenterBonus20K
	Lampz.Callback(28) = "DisableLighting2 p28, 30, 1,"
	Lampz.Callback(28) = "DisableLighting2 bulb28, 400, 5,"
	'Const LBGHighScore	= 29
	Lampz.Callback(29) = "FadeMaterial VR_BackglassHighScore.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	Lampz.MassAssign(30) = L30								' LLowerRightExtraBall
	Lampz.Callback(30) = "DisableLighting2 p30, 30, 1,"
	Lampz.Callback(30) = "DisableLighting2 bulb30, 400, 5,"
	Lampz.MassAssign(31) = L31								' LLeftOutlane
	Lampz.Callback(31) = "DisableLighting2 p31, 0.9, 0,"
	Lampz.MassAssign(33) = L33								' LMiniBonus3
	Lampz.Callback(33) = "DisableLighting2 p33, 0.8, 0.1,"
	Lampz.MassAssign(34) = L34								' LMiniBonus7
	Lampz.Callback(34) = "DisableLighting2 p34, 0.8, 0.1,"
	Lampz.MassAssign(35) = L35								' LRightRampArrow
	Lampz.Callback(35) = "DisableLighting2 p35, 1, 0,"
	Lampz.MassAssign(36) = L36								' LBonus3
	Lampz.Callback(36) = "DisableLighting2 p36, 0.8, 0.1,"
	Lampz.MassAssign(37) = L37								' LBonus7
	Lampz.Callback(37) = "DisableLighting2 p37, 0.8, 0.1,"
	Lampz.MassAssign(38) = L38								' LMiniBonus50K
	Lampz.Callback(38) = "DisableLighting2 p38, 0.8, 0.1,"
	Lampz.MassAssign(39) = L39								' LLower4xBonus
	Lampz.Callback(39) = "DisableLighting2 p39, 30, 1,"
	Lampz.Callback(39) = "DisableLighting2 bulb39, 400, 5,"
	Lampz.MassAssign(40) = L40								' LUpperTripleDropTargetArrow3
	Lampz.Callback(40) = "DisableLighting2 p40, 0.8, 0.1,"
	Lampz.MassAssign(41) = L41								' LLowerDropTragetsBlue
	Lampz.Callback(41) = "DisableLighting2 p41, 30, 1,"
	Lampz.Callback(41) = "DisableLighting2 bulb41, 400, 5,"
	Lampz.MassAssign(42) = L42								' LLeftFlipperlane
	Lampz.Callback(42) = "DisableLighting2 p42, 30, 1,"
	Lampz.Callback(42) = "DisableLighting2 bulb42, 400, 5,"
	Lampz.MassAssign(43) = L43								' LLowerShootAgain
	Lampz.Callback(43) = "DisableLighting2 p43, 30, 1,"
	Lampz.Callback(43) = "DisableLighting2 bulb43, 400, 5,"

	Lampz.MassAssign(44) = L44								' LCenterExtraBall
	Lampz.Callback(44) = "DisableLighting2 p44, 30, 1,"
	Lampz.Callback(44) = "DisableLighting2 bulb44, 400, 5,"
	'Lampz.MassAssign(45) = 								' LBGGameOver
	Lampz.Callback(45) = "FadeMaterial VR_BackglassGameOver.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	Lampz.MassAssign(46) = L46								' LCenterBonus30K
	Lampz.Callback(46) = "DisableLighting2 p46, 30, 1,"
	Lampz.Callback(46) = "DisableLighting2 bulb46, 400, 5,"
	Lampz.MassAssign(47) = L47								' LRightRampRollover1
	Lampz.Callback(47) = "DisableLighting2 pstar1a, 2, 0.2,"
	Lampz.MassAssign(49) = L49								' LMiniBonus4
	Lampz.Callback(49) = "DisableLighting2 p49, 0.8, 0.1,"
	Lampz.MassAssign(50) = L50								' LMiniBonus8
	Lampz.Callback(50) = "DisableLighting2 p50, 0.8, 0.1,"
	Lampz.MassAssign(51) = L51								' LLeftRampArrow
	Lampz.Callback(51) = "DisableLighting2 p51, 1, 0,"
	Lampz.MassAssign(52) = L52								' LBonus4
	Lampz.Callback(52) = "DisableLighting2 p52, 0.8, 0.1,"
	Lampz.MassAssign(53) = L53								' LBonus8
	Lampz.Callback(53) = "DisableLighting2 p53, 0.8, 0.1,"
	Lampz.MassAssign(54) = L54								' LBonus100K
	Lampz.Callback(54) = "DisableLighting2 p54, 0.8, 0.1,"
	Lampz.MassAssign(55) = L55								' LLower5xBonus
	Lampz.Callback(55) = "DisableLighting2 p55, 30, 1,"
	Lampz.Callback(55) = "DisableLighting2 bulb55, 400, 5,"
	Lampz.MassAssign(56) = L56								' LUpper4xBonus
	Lampz.Callback(56) = "DisableLighting2 p56, 30, 1,"
	Lampz.Callback(56) = "DisableLighting2 bulb56, 400, 5,"
	Lampz.MassAssign(57) = L57								' LLowerDropTragetsWhite
	Lampz.Callback(57) = "DisableLighting2 p57, 0.8, 0.1,"
	Lampz.MassAssign(58) = L58								' LLowerRightTargetOrange
	Lampz.Callback(58) = "DisableLighting2 p58, 30, 5,"
	Lampz.Callback(58) = "DisableLighting2 bulb58, 400, 5,"
	Lampz.MassAssign(59) = creditlight								' Const LCredit
	Lampz.Callback(59) = "DisableLighting2 credit_lamp, 5, 3,"
	Lampz.MassAssign(60) = L60								' LLowerDropTragets5xBonus
	Lampz.Callback(60) = "DisableLighting2 p60, 30, 1,"
	Lampz.Callback(60) = "DisableLighting2 bulb60, 400, 5,"
	'Const LTilt = 61
	Lampz.Callback(61) = "FadeMaterial VR_BackglassTilt.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	Lampz.MassAssign(62) = L62								' LUpperTargetCollectBonus
	Lampz.Callback(62) = "DisableLighting2 p62, 30, 1,"
	Lampz.Callback(62) = "DisableLighting2 bulb62, 400, 5,"
	Lampz.MassAssign(63) = L63								' LUpperTargetSpecial
	Lampz.Callback(63) = "DisableLighting2 p63, 0.8, 0,"
	Lampz.MassAssign(65) = L65								' LRightRampRollover2
	Lampz.Callback(65) = "DisableLighting2 pstar1b, 2, 0.2,"
	'Const LBGFlashLogo1 = 66
	Lampz.Callback(66) = "FadeMaterial VR_Backglass_FL.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	'Const LBGFlashLogo4 = 67
	Lampz.Callback(67) = "FadeMaterial VR_Backglass_GO.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"

	Lampz.FadeSpeedUp(68) = 1/3 : Lampz.FadeSpeedDown(68) = 1/12
	Lampz.MassAssign(68) = MING_001							' LMingFace1 - Bottom 2
	Lampz.MassAssign(68) = MING_002
	Lampz.MassAssign(68) = MING_003
	Lampz.MassAssign(68) = MING_004
	'FadeMaterial(mat, wrap, roughness, glossy, thickness, edge, edgealpha, opacity, basecol, glossycol, clearcoatcol, isMetal, actopacity, ByVal aLvl)
	Lampz.Callback(68) = "FadeMaterial MINGlayer1.material, 0.25, 0, 0, 0.05, 1, 1, 1, RGB(240,240,240), RGB(100,100,100), RGB(0,0,0), False, True,"
	'Lampz.MassAssign(68) = L001
	Lampz.Callback(68) = "DisableLighting2 MING_bulbs, 100, 0,"

	Lampz.MassAssign(69) = L69								' LRightClockSeconds
	Lampz.Callback(69) = "DisableLighting2 p69, 30, 1,"
	Lampz.Callback(69) = "DisableLighting2 bulb69, 400, 5,"
	Lampz.MassAssign(81) = L81								' LShooterlaneRollover3
	Lampz.Callback(81) = "DisableLighting2 pstar2c, 2, 0.2,"
	Lampz.Callback(81) = "FadeMaterial guide_star81.material, 0.25, 0, 0, 0.05, 1, 1, 1, RGB(240,240,240), RGB(100,100,100), RGB(0,0,0), False, True,"
	'Const LBGFlashLogo2 = 82
	Lampz.Callback(82) = "FadeMaterial VR_Backglass_AS.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	'Const LBGFlashLogo5 = 83
	Lampz.Callback(83) = "FadeMaterial VR_Backglass_RD.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"

	'Lampz.MassAssign(84) = GI15							' LMingFace2 -Top 2
	'Lampz.MassAssign(84) = L002
	Lampz.MassAssign(85) = L85								' LLeftClockSeconds	
	Lampz.Callback(85) = "DisableLighting2 p85, 30, 1,"
	Lampz.Callback(85) = "DisableLighting2 bulb85, 400, 5,"
	Lampz.MassAssign(97) = L97								' LShooterlaneRollover2
	Lampz.Callback(97) = "DisableLighting2 pstar2b, 2, 0.2,"
	Lampz.Callback(97) = "FadeMaterial guide_star97.material, 0.25, 0, 0, 0.05, 1, 1, 1, RGB(240,240,240), RGB(100,100,100), RGB(0,0,0), False, True,"
	'Const LBGFlashLogo3 = 98
	Lampz.Callback(98) = "FadeMaterial VR_Backglass_H.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	'Const LBGFlashLogo6 = 99
	Lampz.Callback(99) = "FadeMaterial VR_Backglass_ON.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	Lampz.MassAssign(100) = L100a							' LLeftTopLaneRollover
	Lampz.MassAssign(100) = L100b
	Lampz.Callback(100) = "DisableLighting2 pstar1c, 2, 0.2,"
	Lampz.Callback(100) = "DisableLighting2 pstar1d, 2, 0.2,"
	Lampz.MassAssign(101) = L101							' LSaucer3xArrowRight
	Lampz.Callback(101) = "DisableLighting2 p101, 0.8, 0.1,"
	Lampz.MassAssign(113) = L113							' LShooterlaneRollover1	
	Lampz.Callback(113) = "DisableLighting2 pstar2a, 2, 0.2,"
	Lampz.Callback(113) = "FadeMaterial guide_star113.material, 0.25, 0, 0, 0.05, 1, 1, 1, RGB(240,240,240), RGB(100,100,100), RGB(0,0,0), False, True,"

	'Const LBGFlasher	= 116
	Lampz.FadeSpeedUp(116) = 1/2 : Lampz.FadeSpeedDown(116) = 1/9	
	Lampz.Callback(116) = "FadeMaterial VR_BackglassFlasher.material, 0, 0, 0, 0.05, 1, 1, 1, RGB(204,204,204), RGB(127,127,127), RGB(0,0,0), False, True,"
	Lampz.MassAssign(117) = L117							' LSaucer2xArrowLeft
	Lampz.Callback(117) = "DisableLighting2 p117, 0.8, 0.1,"
	'Const LLeftBeacon = 195
	'Const LRightBeacon = 196
	'Lampz.MassAssign(197) = GI32							' LLeftBumper
	'Lampz.MassAssign(198) = GI31 							' LRightBumper
	'Const LGI				

	'Flasher Assignments
'	Lampz.Callback(31)= "Flash1"

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

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

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''debug.print Lampz.Locked(100)	'debug
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
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
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
		''debug.print debugstr
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


'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


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

'******************************************************
'****  END LAMPZ
'******************************************************

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

GlobalSoundLevel = 3
CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr									'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel, RelayFlashSoundLevel, RelayGISoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]
RelayFlashSoundLevel = 0.0075 * GlobalSoundLevel * 14						'volume level; range [0, 1];
RelayGISoundLevel = 0.025 * GlobalSoundLevel * 14						'volume level; range [0, 1];

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


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

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
	PlaySoundAtLevelStatic SoundFX("knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
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

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
		PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
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
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 5 then
 		RandomSoundRubberStrong 1 
	End if
	If finalspeed <= 5 then
 		RandomSoundRubberWeak()
 	End If	
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
	TargetBouncer Activeball, 1	
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
	If Activeball.vely < -12 Then RandomSoundLeftArch
End Sub

Sub Arch2_hit()
	If Activeball.vely < -12 Then RandomSoundRightArch
End Sub

Sub Gate_hit()
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
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

'/////////////////////////////  GENERAL ILLUMINATION RELAYS  ////////////////////////////
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

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 400, obj
End Sub


'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

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

Sub RampHelper1_unhit
	If activeball.vely < 0 Then 
		WireRampOn False
	Else
		WireRampOff
	End If
End Sub

Sub RampHelper2_hit
	If activeball.vely > 0 Then 
		WireRampOn False
	Else
		WireRampOff
	End If
End Sub

Sub RampHelper3_unhit
	If activeball.vely < 0 Then 
		WireRampOn False
	Else
		WireRampOff
	End If
End Sub

Sub RampHelper4_unhit
	If activeball.vely < 0 Then 
		WireRampOn False
	Else
		WireRampOff
	End If
End Sub

Sub RampHelper5_unhit
	If activeball.vely < 0 Then 
		WireRampOn False
	Else
		WireRampOff
	End If
End Sub

Sub RampHelper6_hit
	If activeball.vely > 0 Then 
		WireRampOn False
	Else
		WireRampOff
	End If
End Sub

Sub RampHelper7_hit
	If activeball.vely > 0 Then 
		WireRampOn False
	Else
		WireRampOff
	End If
End Sub

Sub RampHelper8_hit
	If activeball.vely > 0 Then 
		WireRampOn False
	Else
		WireRampOff
	End If
End Sub



dim RampMinLoops : RampMinLoops = 4
dim rampAmpFactor

InitRampRolling

Sub InitRampRolling()
	Select Case RampRollAmpFactor
		Case 0
			rampAmpFactor = "_amp0"
		Case 1
			rampAmpFactor = "_amp2_5"
		Case 2
			rampAmpFactor = "_amp5"
		Case 3
			rampAmpFactor = "_amp7_5"
		Case 4
			rampAmpFactor = "_amp9"
		Case Else
			rampAmpFactor = "_amp0"
	End Select
End Sub

dim RampBalls(6,2)
RampBalls(0,0) = False
dim RampType(6)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub

Sub Waddball(input, RampInput)	'Add ball
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

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

Sub WRemoveBall(ID)		'Remove ball
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x & rampAmpFactor)
			StopSound("wireloop" & x & rampAmpFactor)
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
					PlaySound("RampLoop" & x & rampAmpFactor), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x & rampAmpFactor)
				Else
					StopSound("RampLoop" & x & rampAmpFactor)
					PlaySound("wireloop" & x & rampAmpFactor), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x & rampAmpFactor)
				StopSound("wireloop" & x & rampAmpFactor)
			end if
			if (RampBalls(x,0).Z < 30 or RampBalls(x,0).Z > 75) and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x & rampAmpFactor)
				StopSound("wireloop" & x & rampAmpFactor)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x & rampAmpFactor)
			StopSound("wireloop" & x & rampAmpFactor)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

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

Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold

sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, Source

	for iii = 0 to tnob-1									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow0" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		iii = iii + 1
	Next
	numberofsources = iii
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, LSd, currentMat, AnotherSource, iii
	Dim BOT, Source
	
	s = -1
	For each BOT in Array(FGBall)
		s = s + 1
		If BOT.Z < 30 or BOT.Z > 50 Then 'Defining when and where (on the table) you can have dynamic shadows
			For iii = 0 to numberofsources - 1 
				LSd=Distance(BOT.x, BOT.y, DSSources(iii)(0),DSSources(iii)(1))	'Calculating the Linear distance to the Source
				If LSd < falloff Then						    			'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
					currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
					if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
						sourcenames(s) = iii 'ssource.name
						currentMat = objrtx1(s).material
						objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT.X : objrtx1(s).Y = BOT.Y + fovY
						objrtx1(s).Z = BOT.Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT.X, BOT.Y) + 90
						ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
						If AmbientBallShadowOn = 1 Then
							currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
							UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
						Else
							BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
						End If
					Elseif currentShadowCount(s) = 2 Then										'Same logic as 1 shadow, but twice
						currentMat = objrtx1(s).material
						AnotherSource = sourcenames(s)
						objrtx1(s).visible = 1 : objrtx1(s).X = BOT.X : objrtx1(s).Y = BOT.Y + fovY
						objrtx1(s).Z = BOT.Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT.X, BOT.Y) + 90
						ShadowOpacity = (falloff-Distance(BOT.x,BOT.y,DSSources(AnotherSource)(0),DSSources(AnotherSource)(1)))/falloff
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

						currentMat = objrtx2(s).material
						objrtx2(s).visible = 1 : objrtx2(s).X = BOT.X : objrtx2(s).Y = BOT.Y + fovY
						objrtx2(s).Z = BOT.Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT.X, BOT.Y) + 90
						ShadowOpacity2 = (falloff-LSd)/falloff
						objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
						If AmbientBallShadowOn = 1 Then
							currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
							UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
						Else
							BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
						End If
					end if
				Else
					currentShadowCount(s) = 0
					BallShadowA(s).Opacity = 100*AmbientBSFactor
				End If
			Next
		Else									'Hide dynamic shadows everywhere else
			objrtx2(s).visible = 0 : objrtx1(s).visible = 0
		End If
	Next
End Sub

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

'***********************************************************************************
'****					DIP switch routines (parts by scapino) 					****
'***********************************************************************************

'**************
' Edit Dips
'**************

Sub EditDips
 	Dim vpmDips: Set vpmDips = New cvpmDips
 	With vpmDips
 		.AddForm 700,400,"Flash Gordon - DIP switches"
 		.AddFrame 0,0,190,"Maximum credits",&H03000000,Array("10 credits",0,"15 credits",&H01000000,"25 credits",&H02000000,"40 credits",&H03000000)'dip 25&26
 		.AddFrame 0,76,190,"Balls per game",&HC0000000,Array ("2 balls",&HC0000000,"3 balls",0,"4 balls",&H80000000,"5 balls",&H40000000)'dip 31&32
 		.AddFrame 0,152,190,"Saucer 10K adjust",&H00000020,Array("10K is off at start of game",0,"10K is on at start of game",&H00000020)'dip 6
 		.AddFrame 0,198,190,"Special limit",&H10000000,Array("1 replay per game",0,"unlimited replays",&H10000000)'dip 29
 		.AddFrame 0,244,190,"Extra ball limit",&H20000000,Array("1 extra ball per game",0,"1 extra ball per ball",&H20000000)'dip 30
 		.AddChk 205,0,180,Array("Match feature",&H08000000)'dip 28
 		.AddChk 205,20,115,Array("Credits displayed",&H04000000)'dip 27
 		.AddChk 205,40,190,Array("Saucer value in memory",&H00000040)'dip 7
 		.AddChk 205,60,190,Array("Saucer 2X, 3X arrow in memory",&H00000080)'dip 8
 		.AddChk 205,80,190,Array("Outlane special in memory",&H00002000)'dip 14
 		.AddChk 205,100,190,Array("Top target special in memory",&H00004000)'dip 15
 		.AddChk 205,120,190,Array("Bonus multiplier in memory",32768)'dip 16
 		.AddChk 205,140,250,Array("Game over attract says 'Emperor Ming awaits'",&H00100000)'dip 21
 		.AddChk 205,160,250,Array("2 Side targets && flipper feed lane memory",&H00200000)'dip 22
 		.AddChk 205,180,190,Array("4 Drop target bank in memory",&H00400000)'dip 23
 		.AddChk 205,200,190,Array("Top 3 target arrows in memory",&H00800000)'dip 24
 		.AddLabel 40,300,350,20,"Set selftest position 16,17,18 and 19 to 03 for the best gameplay."
 		.AddLabel 50,320,300,20,"After hitting OK, press F3 to reset game with new settings."
 		.ViewDips
 	End With
End Sub

 Set vpmShowDips = GetRef("editDips")

'******************************
' Setup Backglass
'******************************


'*********  Desktop *************
 Dim Digits2(32)
 Dim Patterns(11)
 Dim Patterns2(11)
 
 Patterns(0) = 0     'empty
 Patterns(1) = 63    '0
 Patterns(2) = 6     '1
 Patterns(3) = 91    '2
 Patterns(4) = 79    '3
 Patterns(5) = 102   '4
 Patterns(6) = 109   '5
 Patterns(7) = 125   '6
 Patterns(8) = 7     '7
 Patterns(9) = 127   '8
 Patterns(10) = 111  '9
 
 Patterns2(0) = 128  'empty
 Patterns2(1) = 191  '0
 Patterns2(2) = 134  '1
 Patterns2(3) = 219  '2
 Patterns2(4) = 207  '3
 Patterns2(5) = 230  '4
 Patterns2(6) = 237  '5
 Patterns2(7) = 253  '6
 Patterns2(8) = 135  '7
 Patterns2(9) = 255  '8
 Patterns2(10) = 239 '9
 
 Set Digits2(0) = a0
 Set Digits2(1) = a1
 Set Digits2(2) = a2
 Set Digits2(3) = a3
 Set Digits2(4) = a4
 Set Digits2(5) = a5
 Set Digits2(6) = a6
 
 Set Digits2(7) = b0
 Set Digits2(8) = b1
 Set Digits2(9) = b2
 Set Digits2(10) = b3
 Set Digits2(11) = b4
 Set Digits2(12) = b5
 Set Digits2(13) = b6
 
 Set Digits2(14) = c0
 Set Digits2(15) = c1
 Set Digits2(16) = c2
 Set Digits2(17) = c3
 Set Digits2(18) = c4
 Set Digits2(19) = c5
 Set Digits2(20) = c6
 
 Set Digits2(21) = d0
 Set Digits2(22) = d1
 Set Digits2(23) = d2
 Set Digits2(24) = d3
 Set Digits2(25) = d4
 Set Digits2(26) = d5
 Set Digits2(27) = d6
 
 Set Digits2(28) = e0
 Set Digits2(29) = e1
 Set Digits2(30) = e2
 Set Digits2(31) = e3
 
Sub UpdateTextBoxes()
	NFadeT 11, ShootAgainReel, "DT_SPSA" 		'SAME PLAYER SHOOTS AGAIN
	NFadeT 13, BIPReel, "DT_BALL_IN_PLAY"		'BALL IN PLAY
	NFadeT 27, MatchReel, "DT_MATCH"	 		'MATCH
	NFadeT 29, HighScoreReel, "DT_HSTD" 		'HIGH SCORE TO DATE
	NFadeT 45, GameOverReel, "DT_GAME_OVER"		'GAME OVER
	NFadeT 61, TiltReel, "DT_TILT"				'TILT
End Sub

 Sub NFadeT(nr, a, b)
     Select Case controller.lamp(nr)
         Case False:a.image = ""
         Case True:a.image = b
     End Select
End Sub

dim zz
If Desktopmode = false or FSSMode = true then
    For each zz in DT:zz.Visible = false: Next    
else
    For each zz in DT:zz.Visible = true: Next 
End If

Dim Digits(32)
Digits(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6,LED1x7)
Digits(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6)
Digits(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6)
Digits(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6,LED4x7)
Digits(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6)
Digits(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6)
Digits(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6)

Digits(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6,LED8x7)
Digits(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6)
Digits(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6)
Digits(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6,LED11x7)
Digits(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6)
Digits(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6)
Digits(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6)

Digits(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006,LED1x8)
Digits(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106)
Digits(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206)
Digits(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306,LED1x9)
Digits(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406)
Digits(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506)
Digits(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606)

Digits(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006,LED2x7)
Digits(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106)
Digits(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206)
Digits(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306,LED2x8)
Digits(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406)
Digits(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506)
Digits(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606)


Digits(28) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306)
Digits(29) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406)
Digits(30) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506)
Digits(31) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606)

dim DisplayColor
DisplayColor =  RGB(255,40,1)

Sub DisplayTimer
	Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED)Then
		For ii=0 To UBound(chgLED)
			num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			'VR/FSS
			if (num < 32) then
				For Each obj In Digits(num)
					If chg And 1 Then FadeDisplay obj, stat And 1
					chg=chg\2 : stat=stat\2
				Next
			end if
			num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			For jj = 0 to 10
				If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits2(chgLED(ii, 0)).SetValue jj
			Next
		Next
	End If
End Sub


Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 30
	Else
		Object.Color = RGB(7,7,7)
		Object.Opacity = 10
	End If
End Sub


Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(Digits)
		if IsArray(Digits(x) ) then
			For each obj in Digits(x)
				'obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

InitDigits


Sub center_digits()
	Dim xoff,yoff,zoff,xrot

	xoff =480
	yoff =45
	zoff =555
	xrot = -90

	Dim ii,xx,yy,yfact,xfact,obj,xcen,ycen,zscale

	zscale = 0.0000001

	xcen =(1032 /2) - (74 / 2)
	ycen = (1020 /2 ) + (194 /2)

	yfact =0 'y fudge factor (ycen was wrong so fix)
	xfact =0


	for ii = 0 to 31
		For Each obj In Digits(ii)
		xx = obj.x 
			
		obj.x = (xoff - xcen) + xx + xfact
		yy = obj.y ' get the yoffset before it is changed
		obj.y =yoff 

		If(yy < 0.) then
			yy = yy * -1
		end if

		obj.height =( zoff - ycen) + yy - (yy * zscale) + yfact
		
		obj.rotx = xrot
		Next
	Next

	For each obj in VR_Backglass
		obj.rotx = xrot
		obj.x = xoff
		obj.y = 50
		obj.height = 815
	Next

'	For each obj in VR_Backglass_Prims
'		obj.blenddisablelighting = 4
'	Next

	VR_BackglassFlasher.blenddisablelighting = 4
	VR_BackglassBallInPlay.blenddisablelighting = 2

	Dim BGDL: BGDL = 1.5

	VR_Backglass_FL.blenddisablelighting = BGDL
	VR_Backglass_AS.blenddisablelighting = BGDL
	VR_Backglass_H.blenddisablelighting = BGDL
	VR_Backglass_GO.blenddisablelighting = BGDL
	VR_Backglass_RD.blenddisablelighting = BGDL
	VR_Backglass_ON.blenddisablelighting = BGDL

end sub


'VR Stuff Below.. ****************************************************************************************************

Dim ShipCount:ShipCount = 0

Sub VR_Space_Timer_Timer()

	ShipCount = ShipCount + 1 

	if ShipCount = 50 then 
		VR_Space_FlyingShip.image = "VR_Space_SF_Fighter2"
	elseif ShipCount = 100 then 
		VR_Space_FlyingShip.image = "VR_Space_SF_Fighter3"
	elseif ShipCount >= 150 Then
		VR_Space_FlyingShip.image = "VR_Space_SF_Fighter1"
		ShipCount = 0
	end if

End Sub

'*******************************************
' Hybrid code for VR, Cab, and Desktop
'*******************************************

Dim VRThings

if VRRoom = 0 Then
	for each VRThings in VR_Room:VRThings.visible = 0:Next
Elseif VRRoom = 1 Then
	for each VRThings in VR_Room:VRThings.visible = 1:Next
End If

If cabmode = 1 Then
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
Else
	for each VRThings in VR_Cab:VRThings.visible = 1:Next
	center_digits
End If