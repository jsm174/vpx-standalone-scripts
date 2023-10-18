Option Explicit
Randomize

'Big Betty's Truck Stop (Bally 1988)
'
' Development by agentEighty6 2020 for Visual Pinball X
' Version 2.1.0

' I would like to send a very special thanks to the following.
' - sindbad : For being able to use his VP9 table resources.
' - JPSalas : VERY much improved "Arcade Physics 3.0" materials, script changes, and techniques.
' - BrandonLaw : Playfield and plastics cleanup and enhancement
' - bigus1 : Beta testing and lighting tweeks
' - flupper, nFozzy, and others : Flipper physics, flasher lighting, etc.
' - VPX development team: As always, thanks for your work in keeping VP alive and well
' - To anyone I may have forgotten, please let me know and I'll add you.

' Release notes:
' Version 2.1.0 - Physics overhaul and added dampening plus a bunch of table tweeks/corrections
' Version 2.0.0 - Major update to add JPs Arcade Physics V3 and also to clean up a few issues and tweeks.
' Version 1.1.0 - Minor update to clean up a few small asthetic issues and add Brandon's improved PF and images.
'				 Added changes from Arngrim and Thalamus to improve directional sounds and updates other sounds.
' Version 1.0.0 - New VPX version built from the V9 version graphical resources.
'				Script and lighting was (mostly) rebuilt from scratch.
'				New VPX routines (flipper physics, shadowing, flasher lighting, ball jumping, etc.).
'				Playfield cleanup/optimization.

'Table Components
' Layer1 - Table Mechanics
' Layer2 - Ramps
' Layer3 - Primitives
' Layer4 - Plastics
' Layer5 - Walls
' Layer6 - Misc, timers, outer walls, testing tools, etc.
' Layer7 - Export to Blender, Dampeners
' Layer8 - Flashers
' Layer9 - Table Lamps
' Layer10 - Controlled GI Lights
' Layer11 - GI Lights


' ****************************************************
' OPTIONS
' ****************************************************

' Volume devided by - lower gets higher sound
Const VolDiv = 900    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

' Thalamus 2020 January : Improved directional sounds
Const VolGates  = 1    ' Gates volume.
Const VolTarg   = 1    ' Targets volume.
Const VolSpin   = 1    ' Spinners volume.
Const VolSen    = 1    ' Sensor volume.
Const VolWire   = 1.3   ' wireramp volume.


Dim DynamicLampIntensity, DynamicGIIntensity, DynamicFlasherIntensity, LetTheBallJump, ShowBallShadow, OutlanePos, SolControlledLights, CenterPost, Playtest

' LET THE BALL JUMP A BIT
'	0 = off
'	1 to 6 = ball jump intensity (2 = default)
LetTheBallJump = 2

' DYNAMIC LAMP INTENSITY MULTIPLIER
'	Numeric value to dynamically multiply every playfield lamp's intensity (default=1)
DynamicLampIntensity = 1

' DYNAMIC GI INTENSITY MULTIPLIER
'	Numeric value to dynamically multiply every GI lamp's intensity (default=1, 0=Off)
DynamicGIIntensity = 1

' DYNAMIC FLASHER INTENSITY MULTIPLIER
'	Numeric value to dynamically multiply every flasher's intensity (default=1, 0=Off)
DynamicFlasherIntensity = 1

' SHOW BALL SHADOWS
'	0 = no ball shadows
'	1 = ball shadows are visible
ShowBallShadow = 1

' OUTLANE DIFFICULTY
'   Change size of outlane width
'	0 = Wide (difficult)
'	1 = Normal (default)
'	2 = Narrow (easy)
OutlanePos = 1

' CENTER POST
'   Enable/disable center post between flippers
'	0 = disabled (default)
'	1 = enabled
CenterPost = 0

' SOLENOID CONTROLLED Lights
'	0 = GI lights always on like orig table (default)
'	1 = let the table control the GI lights on/off
SolControlledLights = 0


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const BallSize = 50
Const BallMass = 1

Const cGameName="trucksp3",SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"
Const UseSolenoids 	= 2
Const UseLamps 		= 1
Const UseSync 		= 1
Const HandleMech 	= 0
Const UseGI			= 0

LoadVPM "03020000", "6803.VBS", 3.20

Dim bsTrough, bsSaucerTR, bsKickerTL, bsKickerTR, DropTargetBank

Dim DesktopMode: DesktopMode = Table1.ShowDT


Sub Table1_Init
    vpmInit me
    With Controller
       .GameName=cGameName
       If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description : Exit Sub
       .SplashInfoLine="Truck Stop, Bally 1988"
       .HandleKeyboard=0
       .ShowTitle=0
       .ShowDMDOnly=1
       .ShowFrame=0
       .HandleMechanics=0
       .Hidden= not DesktopMode
       On Error Resume Next
       .Run
       If Err Then MsgBox Err.Description
       On Error Goto 0
    End With

	' Trough handler
	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw swOuthole, swTroughR, swTroughL, 0, 0, 0, 0, 0
		.InitKick BallRelease,90,7
		.Balls = 2
	End With

	' Saucer Top Right
	Set bsSaucerTR = New cvpmBallStack
	With bsSaucerTR
		.InitSaucer SaucerTopRight,swSaucerTopRight,210,30
		.KickForceVar = 2
	End With

	' Kicker Top Right
 	Set bsKickerTR=New cvpmBallStack
 	With bsKickerTR
		.InitSw 0,swLowerDock,swUpperDock,0,0,0,0,0
		.InitKick KickerTopRight,6,35
 		.InitExitSnd SoundFX("SaucerKick",DOFContactors), SoundFX("SaucerKick",DOFContactors)
 	End With

	' Kicker Top Left
 	Set bsKickerTL=New cvpmBallStack
 	With bsKickerTL
 		.InitSaucer KickerTopLeft, swKickerTopLeft,90,50
		.Kickz = 80
 		.InitExitSnd SoundFX("SaucerKick",DOFContactors), SoundFX("SaucerKick",DOFContactors)
 	End With

	'Drop targets bank
	set DropTargetBank = new cvpmDropTarget
		DropTargetBank.InitDrop Array(sw25,sw26,sw27), Array(25,26,27)
		DropTargetBank.InitSnd SoundFX("DropTargetHit",DOFDropTargets),SoundFX("BankReset",DOFContactors)

	vpmNudge.TiltSwitch = swTiltMe
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(SlingLL, SlingLR, SlingUL, SlingUR)

	DisplayTimer.Enabled = DesktopMode

	If DesktopMode = True Then
		RampLT.Visible = 1
		RampRT.Visible = 1
		RampLT2.Visible = 0
		RampRT2.Visible = 0
	Else
		RampLT.Visible = 0
		RampRT.Visible = 0
		RampLT2.Visible = 1
		RampRT2.Visible = 1
	End If

	If CenterPost = 1 Then
		MetalCenterPost.isDropped = False
		MetalCenterPost.visible = True
		RubberCenterPost.collidable=True
		RubberCenterPost.visible=True
	Else
		MetalCenterPost.isDropped = True
		MetalCenterPost.visible = False
		RubberCenterPost.collidable=False
		RubberCenterPost.visible=False
	End If

	'Reset Slings
	Slingshots_Init

	'Set Outlane width/difficulty
	Outlanes_Init

	'Reset Steering wall in upper middle ramp system
	LaneSteeringInit

	'Reset Mushroom Targets in Upper playfield
	MushroomTriggers_Init

	vpmMapLights AllLights
	'not assigned: 14, 28, 62, 77, 93 (backbox top Lights)

	dim xx
    For each xx in GILights
		xx.intensity = xx.intensity * DynamicGIIntensity * 0.5
    Next
	dim zz
	For each zz in AllLights
		zz.intensity = zz.intensity * DynamicLampIntensity
	Next

	l12a.IntensityScale = 0
	l61a.IntensityScale = 0
	l29a.IntensityScale = 0
	l91a.IntensityScale = 0

	If SolControlledLights = 0 Then
		GiON
	End If

	If Playtest = 1 Then
		FlipperTrigger001.Enabled=True
		FlipperTrigger002.Enabled=True
		FlipperTrigger003.Enabled=True
		FlipperTrigger004.Enabled=True
		Wall001.isDropped=false
		Wall002.isDropped=false
		Wall003.isDropped=false
	Else
		FlipperTrigger001.Enabled=False
		FlipperTrigger002.Enabled=False
		FlipperTrigger003.Enabled=False
		FlipperTrigger004.Enabled=False
		Wall001.isDropped=True
		Wall002.isDropped=True
		Wall003.isDropped=True
	End If
End Sub


'***********************************************************************************
'****               	  Keyboard (Input) Handling								****
'***********************************************************************************
Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then Plunger.PullBack
	If keycode = LeftFlipperKey Then lfpress = 1
	If keycode = RightFlipperKey Then rfpress = 1
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then Plunger.Fire
	If keycode = LeftFlipperKey Then
		lfpress = 0
		leftflipper.eostorqueangle = EOSA
		leftflipper.eostorque = EOST
	End If
	If keycode = RightFlipperKey Then
		rfpress = 0
		rightflipper.eostorqueangle = EOSA
		rightflipper.eostorque = EOST
	End If
 	If vpmKeyUp(keycode) Then Exit Sub
End Sub


'***********************************************************************************
'****				        	Solenoid reference      				    	****
'***********************************************************************************
const sKickerTopRight 				= 2  ' mapped in manual to 3
const sSaucerTopRight 				= 4  ' mapped in manual to 2
const sDropTargetsReset				= 6  ' mapped in manual to 4
const sKickerTopLeft 				= 8  ' mapped in manual to 1
const sEjectToShooter 				= 12 ' mapped in manual to 9
const sOuthole						= 14 ' mapped in manual to 10
const sKnocker		 				= 15 ' mapped in manual to 11
const sFlippersEnable				= 19
const sLaneSteering					= 18 ' mapped in manual to 12


'***********************************************************************************
'****								Knocker               						****
'***********************************************************************************
SolCallback(sKnocker) = "HitKnocker"

Sub HitKnocker(enabled)
	If enabled Then PlaySound SoundFX("Knocker",DOFKnocker), 1
End Sub


'***********************************************************************************
'****						  Drains and Kickers           						****
'***********************************************************************************
Dim KickerTopLeftBall, KickerTopLeftPos, BallOnTopRamp
SolCallback(sEjectToShooter)	= "EjectToShooter"
SolCallback(sSaucerTopRight)	= "EjectSaucerTopRight"
SolCallback(sKickerTopLeft) 	= "EjectKickerTopLeft"
SolCallback(sKickerTopRight) 	= "EjectKickerTopRight"
SolCallback(sOutHole)			= "bsTrough.SolIn"

Sub EjectToShooter(enabled)
 	If enabled = True Then
		PlaySoundAtVol SoundFX("BallRelease",DOFContactors), BallRelease, 1
		bsTrough.ExitSol_On
	End If
End Sub

Sub Drain_Hit()
	bsTrough.EntrySol_On
	bsTrough.AddBall Me
	PlaySoundAtVol "Drain", Drain, 1
End Sub

Sub EjectSaucerTopRight(enabled)
	If enabled Then
		PlaySoundAtVol SoundFX("SaucerKick",DOFContactors), SaucerTopRight, 1
		bsSaucerTR.ExitSol_On
	End If
End Sub
Sub EjectKickerTopLeft(enabled)
	If enabled Then
		PlaySoundAtVol SoundFX("SaucerKick",DOFContactors), KickerTopLeft, 1
		If Isobject(KickerTopLeftBall) Then
			BallOnTopRamp = True
			KickerTopLeft.TimerInterval = 1
			KickerTopLeft.TimerEnabled = 1
			KickerTopLeftPos = 0
		End If
	End If
End Sub
Sub EjectKickerTopRight(enabled)
	If enabled Then
		'PlaySoundAtVol SoundFX("SaucerKick",DOFContactors), KickerTopRight, 1
		bsKickerTR.ExitSol_On
	End If
End Sub


Sub KickerTopLeft_Timer
	If KickerTopLeftPos = 0 then
		Controller.Switch(swKickerTopLeft) = 0
	End If
	KickerTopLeftPos = KickerTopLeftPos + 3

	KickerTopLeftBall.z = KickerTopLeftBall.z + 3
	If KickerTopLeftPos > 160 then
		KickerTopLeftBall.x = KickerTopLeftBall.x + 3
	End If
	If KickerTopLeftPos > 185 then
		KickerTopLeft.Kick 90, 6
		'SoundMetalRamp
		Set KickerTopLeftBall = Nothing
		Me.TimerEnabled = 0
	End If

End Sub


'***********************************************************************************
'****							Flippers                 						****
'***********************************************************************************
SolCallback(sLRFlipper)="SolRightFlipper"
SolCallback(sLLFlipper)="SolLeftFlipper"

SolCallback(sFlippersEnable) = "SolEnableBumpers"

' ****************************************************
' flipper subs
' ****************************************************
Sub SolLeftFlipper(Enabled)
	If Enabled Then
		LF.fire
		PlaySoundAtVol SoundFX("fx_flipperup",DOFFlippers), LeftFlipper, 1
		PlaySoundAtVol "fx_flipperup", LeftFlipperTop, 1
		LeftFlipperTop.RotateToEnd
    Else
		PlaySoundAtVol SoundFX("fx_flipperdown",DOFFlippers), LeftFlipper, 1
		PlaySoundAtVol "fx_flipperdown", LeftFlipperTop, 1
		'LeftFlipper.EOSTorque = 0.1
		LeftFlipper.RotateToStart
		LeftFlipperTop.RotateToStart
    End If
End Sub

Sub SolRightFlipper(Enabled)
	If Enabled Then
		RF.fire
		PlaySoundAtVol SoundFX("fx_flipperup",DOFFlippers), RightFlipper, 1
		PlaySoundAtVol "fx_flipperup", RightFlipperTop, 1
		RightFlipperTop.RotateToEnd
    Else
		PlaySoundAtVol SoundFX("fx_flipperdown",DOFFlippers), RightFlipper, 1
		PlaySoundAtVol "fx_flipperdown", RightFlipperTop, 1
		'RightFlipper.EOSTorque = 0.1
		RightFlipper.RotateToStart
		RightFlipperTop.RotateToStart
    End If
End Sub

Sub FlipperTrigger001_hit
		LF.fire
		LeftFlipperTop.RotateToEnd
End Sub
Sub FlipperTrigger001_unhit
		LeftFlipper.RotateToStart
		LeftFlipperTop.RotateToStart
End Sub

Sub FlipperTrigger002_hit
		RF.fire
		RightFlipperTop.RotateToEnd
End Sub
Sub FlipperTrigger002_unhit
		RightFlipper.RotateToStart
		RightFlipperTop.RotateToStart
End Sub

Sub FlipperTrigger003_hit
		LF.fire
		LeftFlipperTop.RotateToEnd
End Sub
Sub FlipperTrigger003_unhit
		LeftFlipper.RotateToStart
		LeftFlipperTop.RotateToStart
End Sub

Sub FlipperTrigger004_hit
		RF.fire
		RightFlipperTop.RotateToEnd
End Sub
Sub FlipperTrigger004_unhit
		RightFlipper.RotateToStart
		RightFlipperTop.RotateToStart
End Sub




'***********************************************************************************
'****							Slingshot and walls        						****
'***********************************************************************************
Dim SLLPos, SLRPos, SULPos, SURPos

Sub SlingLL_Slingshot()
	LLSling1.Visible = 0:LLSling2.Visible = 0:LLSling3.Visible = 0: LLSling4.Visible = 1: LLSling.TransZ = -27
	vpmTimer.PulseSw(swBottomSlingLeft): PlaySoundAtVol SoundFX("SlingL",DOFContactors), LLSling, 1
	SLLPos = 0: Me.TimerInterval = 15:Me.TimerEnabled = 1:
End Sub

Sub SlingLL_Timer
  Select Case SLLPos
    Case 2:	LLSling1.Visible = 0:LLSling2.Visible = 0:LLSling3.Visible = 1: LLSling4.Visible = 0: LLSling.TransZ = -17
    Case 3:	LLSling1.Visible = 0:LLSling2.Visible = 1:LLSling3.Visible = 0: LLSling4.Visible = 0: LLSling.TransZ = -8
    Case 4:	LLSling1.Visible = 1:LLSling2.Visible = 0:LLSling3.Visible = 0: LLSling4.Visible = 0: LLSling.TransZ = 0 :Me.TimerEnabled = 0
  End Select
  SLLPos = SLLPos + 1
End Sub

Sub SlingLR_Slingshot()
	LRSling1.Visible = 0:LRSling2.Visible = 0:LRSling3.Visible = 0: LRSling4.Visible = 1: LRSling.TransZ = -27
	vpmTimer.PulseSw(swBottomSlingRight): PlaySoundAtVol SoundFX("SlingR",DOFContactors), LRSling, 1
	SLRPos = 0: Me.TimerInterval = 15:Me.TimerEnabled = 1:
End Sub

Sub SlingLR_Timer
  Select Case SLRPos
    Case 2:	LRSling1.Visible = 0:LRSling2.Visible = 0:LRSling3.Visible = 1: LRSling4.Visible = 0: LRSling.TransZ = -17
    Case 3:	LRSling1.Visible = 0:LRSling2.Visible = 1:LRSling3.Visible = 0: LRSling4.Visible = 0: LRSling.TransZ = -8
    Case 4:	LRSling1.Visible = 1:LRSling2.Visible = 0:LRSling3.Visible = 0: LRSling4.Visible = 0: LRSling.TransZ = 0 :Me.TimerEnabled = 0
  End Select
  SLRPos = SLRPos + 1
End Sub

Sub SlingUL_Slingshot()
	ULSling1.Visible = 0:ULSling2.Visible = 0:ULSling3.Visible = 0: ULSling4.Visible = 1: ULSling.TransZ = -27
	vpmTimer.PulseSw(swTopSlingLeft): PlaySoundAtVol SoundFX("SlingL",DOFContactors), ULSling, 1
	SULPos = 0: Me.TimerInterval = 15:Me.TimerEnabled = 1:
End Sub

Sub SlingUL_Timer
  Select Case SULPos
    Case 2:	ULSling1.Visible = 0:ULSling2.Visible = 0:ULSling3.Visible = 1: ULSling4.Visible = 0: ULSling.TransZ = -17
    Case 3:	ULSling1.Visible = 0:ULSling2.Visible = 1:ULSling3.Visible = 0: ULSling4.Visible = 0: ULSling.TransZ = -8
    Case 4:	ULSling1.Visible = 1:ULSling2.Visible = 0:ULSling3.Visible = 0: ULSling4.Visible = 0: ULSling.TransZ = 0 :Me.TimerEnabled = 0
  End Select
  SULPos = SULPos + 1
End Sub

Sub SlingUR_Slingshot()
	URSling1.Visible = 0:URSling2.Visible = 0:URSling3.Visible = 0: URSling4.Visible = 1: URSling.TransZ = -27
	vpmTimer.PulseSw(swTopSlingRight): PlaySoundAtVol SoundFX("SlingR",DOFContactors), URSling, 1
	SURPos = 0: Me.TimerInterval = 15:Me.TimerEnabled = 1:
End Sub

Sub SlingUR_Timer
  Select Case SURPos
    Case 2:	URSling1.Visible = 0:URSling2.Visible = 0:URSling3.Visible = 1: URSling4.Visible = 0: URSling.TransZ = -17
    Case 3:	URSling1.Visible = 0:URSling2.Visible = 1:URSling3.Visible = 0: URSling4.Visible = 0: URSling.TransZ = -8
    Case 4:	URSling1.Visible = 1:URSling2.Visible = 0:URSling3.Visible = 0: URSling4.Visible = 0: URSling.TransZ = 0 :Me.TimerEnabled = 0
  End Select
  SURPos = SURPos + 1
End Sub

Sub Slingshots_Init
	LLSling1.Visible = 1:LLSling2.Visible = 0:LLSling3.Visible = 0: LLSling4.Visible = 0: LLSling.TransZ = 0
	LRSling1.Visible = 1:LRSling2.Visible = 0:LRSling3.Visible = 0: LRSling4.Visible = 0: LRSling.TransZ = 0
	ULSling1.Visible = 1:ULSling2.Visible = 0:ULSling3.Visible = 0: ULSling4.Visible = 0: ULSling.TransZ = 0
	URSling1.Visible = 1:URSling2.Visible = 0:URSling3.Visible = 0: URSling4.Visible = 0: URSling.TransZ = 0
End Sub

Sub SolEnableBumpers(enabled)
  If enabled Then
    'turn on bumpers and maybe lights
    SlingLL.hasHitEvent = True
    SlingLR.hasHitEvent = True
    SlingUL.hasHitEvent = True
    SlingUR.hasHitEvent = True
    If SolControlledLights = 1 Then
      GiON
    End If
  Else
    'turn off bumpers and maybe lights
    SlingLL.hasHitEvent = False
    SlingLR.hasHitEvent = False
    SlingUL.hasHitEvent = False
    SlingUR.hasHitEvent = False
    If SolControlledLights = 1 Then
      GiOFF
    End If
  End If
End Sub

Sub GiON
  dim xx
  For each xx in GILights
    xx.State = LightStateOn
  Next
End Sub

Sub GiOFF
  dim xx
  For each xx in GILights
    xx.State = LightStateOff
  Next
End Sub


'***********************************************************************************
'****				        	Switch reference      				  		  	****
'***********************************************************************************
Const swRampEntryC					= 1
Const swRampEntryI					= 2
Const swRampEntryT					= 3
Const swRampEntryY					= 4
Const swOuthole						  = 8
Const swRampExitLeft				= 12
Const swRampExitRight				= 13
Const swTiltMe						  = 15
Const swShooterLane					= 16
Const swBottomSlingLeft			= 17
Const swBottomSlingRight		= 18
Const swTopSlingLeft				= 19
Const swTopSlingRight				= 20
Const swSingleTargetTop			= 21
Const swMushroomLeft				= 22
Const swMushroomRight				= 23
Const swSpinnerTop					= 24
Const swDropTargetBottom		= 25
Const swDropTargetMiddle		= 26
Const swDropTargetTop				= 27
Const swTopWalls					  = 28
Const swKickerTopLeft				= 29
Const swSaucerTopRight			= 30
Const swInlaneLeft					= 31
Const swInlaneRight					= 32
Const swBlueTarget1					= 33
Const swBlueTarget2					= 34
Const swBlueTarget3					= 35
Const swBlueTarget4					= 36
Const swBlueTarget5					= 37
Const swBlueTarget6					= 38
Const swOutlaneLeft					= 39
Const swOutlaneRight				= 40
Const swSingleTargetLeft		= 41
Const swSingleTargetRight		= 42
Const swLowerDock					  = 45
Const swUpperDock					  = 46
Const swTroughL						  = 47
Const swTroughR						  = 48
'***********************************************************************************
'****						   Rollovers and triggers      						****
'***********************************************************************************
Sub sw24_Spin():PlaySoundAtVol "Spinner", sw24, VolSpin:vpmTimer.PulseSw (swSpinnerTop):End Sub
Sub sw1_Hit():vpmTimer.PulseSw(swRampEntryC):PlaySoundAtVol "Gate", ActiveBall, VolGates:End Sub
Sub sw2_Hit():vpmTimer.PulseSw(swRampEntryI):PlaySoundAtVol "Gate", ActiveBall, VolGates:End Sub
Sub sw3_Hit():vpmTimer.PulseSw(swRampEntryT):PlaySoundAtVol "Gate", ActiveBall, VolGates:End Sub
Sub sw4_Hit():vpmTimer.PulseSw(swRampEntryY):PlaySoundAtVol "Gate", ActiveBall, VolGates:End Sub
Sub sw28l_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:vpmTimer.PulseSw(swTopWalls):End Sub
Sub sw28r_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:vpmTimer.PulseSw(swTopWalls):End Sub
Sub sw12a_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swRampExitLeft) = 1:End Sub
Sub sw12a_UnHit():Controller.Switch(swRampExitLeft) = 0:End Sub
Sub sw12b_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swRampExitLeft) = 1:End Sub
Sub sw12b_UnHit():Controller.Switch(swRampExitLeft) = 0:End Sub
Sub sw13a_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swRampExitRight) = 1:End Sub
Sub sw13a_UnHit():Controller.Switch(swRampExitRight) = 0:End Sub
Sub sw13b_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swRampExitRight) = 1:End Sub
Sub sw13b_UnHit():Controller.Switch(swRampExitRight) = 0:End Sub

Sub sw16_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swShooterLane) = 1:End Sub
Sub sw16_UnHit():Controller.Switch(swShooterLane) = 0:End Sub
Sub sw31_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swInlaneLeft) = 1:End Sub
Sub sw31_UnHit():Controller.Switch(swInlaneLeft) = 0:End Sub
Sub sw32_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swInlaneRight) = 1:End Sub
Sub sw32_UnHit():Controller.Switch(swInlaneRight) = 0:End Sub
Sub sw39_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swOutlaneLeft) = 1:End Sub
Sub sw39_UnHit():Controller.Switch(swOutlaneLeft) = 0:End Sub
Sub sw40_Hit():PlaySoundAtVol "Sensor", ActiveBall, VolSen:Controller.Switch(swOutlaneRight) = 1:End Sub
Sub sw40_UnHit():Controller.Switch(swOutlaneRight) = 0:End Sub

'***********************************************************************************
'****						   		Targets		 	     						****
'***********************************************************************************
Sub sw21_Hit:sw21p.TransX = 4:vpmTimer.PulseSw(swSingleTargetTop):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw21_Timer:sw21p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw33_Hit:sw33p.TransX = 4:vpmTimer.PulseSw(swBlueTarget1):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw33_Timer:sw33p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw34_Hit:sw34p.TransX = 4:vpmTimer.PulseSw(swBlueTarget2):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw34_Timer:sw34p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw35_Hit:sw35p.TransX = 4:vpmTimer.PulseSw(swBlueTarget3):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw35_Timer:sw35p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw36_Hit:sw36p.TransX = 4:vpmTimer.PulseSw(swBlueTarget4):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw36_Timer:sw36p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw37_Hit:sw37p.TransX = 4:vpmTimer.PulseSw(swBlueTarget5):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw37_Timer:sw37p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw38_Hit:sw38p.TransX = 4:vpmTimer.PulseSw(swBlueTarget6):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw38_Timer:sw38p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw41_Hit:sw41p.TransX = 4:vpmTimer.PulseSw(swSingleTargetLeft):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw41_Timer:sw41p.TransX = 0:Me.TimerEnabled = 0:End Sub
Sub sw42_Hit:sw42p.TransX = 4:vpmTimer.PulseSw(swSingleTargetRight):StandupTargetHit:Me.TimerEnabled = 1:End Sub
Sub sw42_Timer:sw42p.TransX = 0:Me.TimerEnabled = 0:End Sub

'***********************************************************************************
'****						  Saucers and Kickers         						****
'***********************************************************************************
Sub SaucerTopRight_Hit:PlaySoundAtVol "SaucerHit", ActiveBall, 1:bsSaucerTR.EntrySol_On:bsSaucerTR.AddBall Me:End Sub
Sub KickerTopLeft_Hit: PlaySoundAtVol "SaucerHit", ActiveBall, 1:Controller.Switch(swKickerTopLeft) = 1:Set KickerTopLeftBall = ActiveBall:End Sub
Sub KickerTopRight_Hit:PlaySoundAtVol "SaucerHit", ActiveBall, 1:bsKickerTR.EntrySol_On:bsKickerTR.AddBall Me:BallOnTopRamp = False:End Sub

'***********************************************************************************
'****						  Drop Targets                 						****
'***********************************************************************************

Sub sw25_Hit:DropTargetBank.Hit 1:End Sub    'sw25
Sub sw26_Hit:DropTargetBank.Hit 2:End Sub     'sw26
Sub sw27_Hit:DropTargetBank.Hit 3:End sub		'sw27

SolCallback(sDropTargetsReset) = "DropTargetBank.SolDropUp"


'***********************************************************************************
'****						   Mushroom triggers  	    						****
'***********************************************************************************
Dim sw22step, sw23step
Sub sw22_Hit():sw22step = 0: vpmTimer.PulseSw(swMushroomLeft): PlaySoundAtVol "fx_rubber_hit_1", ActiveBall, 1:Me.TimerInterval = 40:Me.TimerEnabled = 1: End Sub
Sub sw23_Hit():sw23step = 0: vpmTimer.PulseSw(swMushroomRight):PlaySoundAtVol "fx_rubber_hit_2", ActiveBall, 1:Me.TimerInterval = 40:Me.TimerEnabled = 1: End Sub

Sub sw22_Timer()
  Select Case sw22step
    Case 0 : sw22m3.Visible = 1:sw22m2.Visible = 0:sw22m1.Visible = 0
    Case 1 : sw22m2.Visible = 1:sw22m3.Visible = 0:sw22m1.Visible = 0
    Case 2 : sw22m1.Visible = 1:sw22m2.Visible = 0:sw22m3.Visible = 0:Me.TimerEnabled = 0
  End Select
  sw22step = sw22step +1
End Sub

Sub sw23_Timer()
  Select Case sw23step
    Case 0 : sw23m3.Visible = 1:sw23m2.Visible = 0:sw23m1.Visible = 0
    Case 1 : sw23m2.Visible = 1:sw23m3.Visible = 0:sw23m1.Visible = 0
    Case 2 : sw23m1.Visible = 1:sw23m2.Visible = 0:sw23m3.Visible = 0:Me.TimerEnabled = 0
  End Select
  sw23step = sw23step +1
End Sub

Sub MushroomTriggers_Init
	sw22m1.Visible = 1: sw22m2.Visible = 0: sw22m3.Visible = 0
	sw23m1.Visible = 1: sw23m2.Visible = 0: sw23m3.Visible = 0
End Sub

'***********************************************************************************
'****						 	  Lane Steering 	     						****
'***********************************************************************************
Dim LSstep, LSdir
SolCallback(sLaneSteering)		= "SetLaneSteering"

Sub SetLaneSteering(enabled)
  If enabled = True Then
    LSDir = -1: PlaySoundAtVol SoundFX("Solenoid",DOFContactors),Primitive87, 1: LaneSteeringTimer.Enabled = 1
  Else
    LSDir = 1: PlaySoundAtVol SoundFX("Solenoid",DOFContactors),Primitive87, 1: LaneSteeringTimer.Enabled = 1
  End If
End Sub

Sub LaneSteeringTimer_Timer
  If LSdir = 1 Then
    LSstep = LSstep + 1
    Select Case LSstep
      Case 1 : LaneSteeringWall1.IsDropped = 0: LaneSteeringWall2.IsDropped = 1: LaneSteeringWall3.IsDropped = 1: LaneSteeringWallHidden.IsDropped = 0
      Case 2 : LaneSteeringWall2.IsDropped = 0: LaneSteeringWall1.IsDropped = 1: LaneSteeringWall3.IsDropped = 1: LaneSteeringWallHidden.IsDropped = 0
      Case 3 : LaneSteeringWall3.IsDropped = 0: LaneSteeringWall1.IsDropped = 1: LaneSteeringWall2.IsDropped = 1: LaneSteeringWallHidden.IsDropped = 0:Me.Enabled = 0
    End Select
  Else
    LSstep = LSstep - 1
    Select Case LSstep
      Case 2 : LaneSteeringWall3.IsDropped = 1: LaneSteeringWall2.IsDropped = 0: LaneSteeringWall1.IsDropped = 1: LaneSteeringWallHidden.IsDropped = 1
      Case 1 : LaneSteeringWall2.IsDropped = 1: LaneSteeringWall1.IsDropped = 0: LaneSteeringWall3.IsDropped = 1: LaneSteeringWallHidden.IsDropped = 1
      Case 0 : LaneSteeringWall1.IsDropped = 1: LaneSteeringWall2.IsDropped = 1: LaneSteeringWall3.IsDropped = 1: LaneSteeringWallHidden.IsDropped = 1:Me.Enabled = 0
    End Select
  End If
End Sub

Sub LaneSteeringInit
	LaneSteeringWall1.IsDropped = 1: LaneSteeringWall2.IsDropped = 1: LaneSteeringWall3.IsDropped = 1: LaneSteeringWallHidden.IsDropped = 1
	LSDir = 0: LSstep = 0
End Sub

Sub Outlanes_Init
	Dim OutlaneLPosArray: OutlaneLPosArray = Array(45, 1224, 52, 1232.75, 59, 1240.5)
	Dim OutlaneRPosArray: OutlaneRPosArray = Array(815, 1226, 815, 1236.75, 815, 1246.5)
	Dim io: io = OutlanePos * 2
	PegLeftOutLane.X = OutlaneLPosArray(io): PegLeftOutLane.Y = OutlaneLPosArray(io+1)
	PegRightOutLane.X = OutlaneRPosArray(io): PegRightOutLane.Y = OutlaneRPosArray(io+1)
	RubberLeftOutlane1.Visible = 0: RubberLeftOutlane2.Visible = 0: RubberLeftOutlane3.Visible = 0
	RubberLeftOutlane1.Collidable = 0: RubberLeftOutlane2.Collidable = 0: RubberLeftOutlane3.Collidable = 0
	RubberRightOutlane1.Visible = 0: RubberRightOutlane2.Visible = 0: RubberRightOutlane3.Visible = 0
	RubberRightOutlane1.Collidable = 0: RubberRightOutlane2.Collidable = 0: RubberRightOutlane3.Collidable = 0
	Select Case OutlanePos
		Case 0:	RubberLeftOutlane1.Visible = 1: RubberLeftOutlane1.Collidable = 1
				RubberRightOutlane1.Visible = 1: RubberRightOutlane1.Collidable = 1
		Case 1:	RubberLeftOutlane2.Visible = 1: RubberLeftOutlane2.Collidable = 1
				RubberRightOutlane2.Visible = 1: RubberRightOutlane2.Collidable = 1
		Case 2:	RubberLeftOutlane3.Visible = 1: RubberLeftOutlane3.Collidable = 1
				RubberRightOutlane3.Visible = 1: RubberRightOutlane3.Collidable = 1
	End Select
End Sub

Dim Digits(27)
Digits(0)=Array(A1,A2,A3,A4,A5,A6,A7,A10,A8,A9)
Digits(1)=Array(A11,A12,A13,A14,A15,A16,A17,A20,A18,A19)
Digits(2)=Array(A31,A32,A33,A34,A35,A36,A37,A40,A38,A39)
Digits(3)=Array(A41,A42,A43,A44,A45,A46,A47,A50,A48,A49)
Digits(4)=Array(A51,A52,A53,A54,A55,A56,A57,A60,A58,A59)
Digits(5)=Array(A61,A62,A63,A64,A65,A66,A67,A70,A68,A69)
Digits(6)=Array(A71,A72,A73,A74,A75,A76,A77,A80,A78,A79)
Digits(7)=Array(A81,A82,A83,A84,A85,A86,A87,A90,A88,A89)
Digits(8)=Array(A91,A92,A93,A94,A95,A96,A97,A100,A98,A99)
Digits(9)=Array(A101,A102,A103,A104,A105,A106,A107,A110,A108,A109)
Digits(10)=Array(A111,A112,A113,A114,A115,A116,A117,A120,A118,A119)
Digits(11)=Array(A121,A122,A123,A124,A125,A126,A127,A130,A128,A129)
Digits(12)=Array(A131,A132,A133,A134,A135,A136,A137,A140,A138,A139)
Digits(13)=Array(A141,A142,A143,A144,A145,A146,A147,A150,A148,A149)
Digits(14)=Array(A151,A152,A153,A154,A155,A156,A157,A160,A158,A159)
Digits(15)=Array(A161,A162,A163,A164,A165,A166,A167,A170,A168,A169)
Digits(16)=Array(A171,A172,A173,A174,A175,A176,A177,A180,A178,A179)
Digits(17)=Array(A181,A182,A183,A184,A185,A186,A187,A190,A188,A189)
Digits(18)=Array(A191,A192,A193,A194,A195,A196,A197,A200,A198,A199)
Digits(19)=Array(A201,A202,A203,A204,A205,A206,A207,A210,A208,A209)
Digits(20)=Array(A211,A212,A213,A214,A215,A216,A217,A220,A218,A219)
Digits(21)=Array(A221,A222,A223,A224,A225,A226,A227,A230,A228,A229)
Digits(22)=Array(A231,A232,A233,A234,A235,A236,A237,A240,A238,A239)
Digits(23)=Array(A241,A242,A243,A244,A245,A246,A247,A250,A248,A249)
Digits(24)=Array(A251,A252,A253,A254,A255,A256,A257,A260,A258,A259)
Digits(25)=Array(A261,A262,A263,A264,A265,A266,A267,A270,A268,A269)
Digits(26)=Array(A271,A272,A273,A274,A275,A276,A277,A280,A278,A279)
Digits(27)=Array(A281,A282,A283,A284,A285,A286,A287,A290,A288,A289)

Sub LEDs_Timer()
    Dim chgLED, ii, num, chg, stat, obj
	chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(chgLED) Then
		If DesktopMode Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 32) then
					For Each obj In Digits(num)
						If chg And 1 Then obj.State = stat And 1
						chg = chg\2 : stat = stat\2
					Next
				End If
			Next
		End If
    End If
End Sub

'***********************************************************************************
'****				        		Sound routines      				    	****
'***********************************************************************************
Sub RampTriggerC_Hit:PlaySoundAtVol "Gate", ActiveBall, 1:End Sub
Sub RampTriggerI_Hit:PlaySoundAtVol "Gate", ActiveBall, 1:End Sub
Sub RampTriggerT_Hit:PlaySoundAtVol "Gate", ActiveBall, 1:End Sub
Sub RampTriggerY_Hit:PlaySoundAtVol "Gate", ActiveBall, 1:End Sub
Sub LeftKickerEntry_Hit():PlaySoundAtVol "fx_kicker_enter_center", ActiveBall, 1:End Sub


Function OnPlayfield(ball)
  If ball.Z < 30 Then
    OnPlayfield = True
    Exit Function
  End If
  OnPlayfield = False
End Function

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX and Rothbauerw
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.
Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

' requires rampbump1 to 7 in Sound Manager
Sub RandomBump(voladj, freq)
	Dim BumpSnd:BumpSnd= "rampbump" & CStr(Int(Rnd*7)+1)
	PlaySound BumpSnd, 0, Vol(ActiveBall)*voladj, AudioPan(ActiveBall), 0, freq, 0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
    PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

Sub PlaySoundAtBallAbsVol(sound, VolMult)
	PlaySound sound, 0, VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

' set position as table object and Vol + RndPitch manually
Sub PlaySoundAtVolPitch(sound, tableobj, Vol, RndPitch)
	PlaySound sound, 1, Vol, AudioPan(tableobj), RndPitch, 0, 0, 1, AudioFade(tableobj)
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = ball.x * 2 / table1.width-1
  If tmp > 0 Then
    Pan = Csng(tmp ^10)
  Else
    Pan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function BallRollVol(ball) ' Calculates the Volume of the sound based on the ball speed
   BallRollVol = Csng(BallVel(ball) ^2 / (80000 - (79900 * Log(RollVol) / Log(100))))
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
    BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
    VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function

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


'*****************************************
'	Wire Ramp Trigger Processing
'*****************************************
Sub Trigger001_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = True
		end if
	next
End Sub
Sub Trigger002_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = True
		end if
	next
End Sub
Sub Trigger003_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = True
		end if
	next
End Sub
Sub Trigger004_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = True
		end if
	next
End Sub
Sub Trigger005_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = True
		end if
	next
End Sub


Sub Trigger001a_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = False
		end if
	next
End Sub
Sub Trigger002a_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = False
		end if
	next
End Sub
Sub Trigger003a_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = False
		end if
	next
End Sub
Sub Trigger004a_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = False
		end if
	next
End Sub
Sub Trigger005a_hit
    Dim b
    BOT = GetBalls
	For b = 0 to UBound(BOT)
		if BOT(b) is activeball then 
			MRBallArray(b) = False
		end if
	next
End Sub


'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************
Const tnob = 4 ' total number of balls
'Const maxvel = 99 'max ball velocity
ReDim rolling(tnob)

Dim BOT

Dim MRBallArray : MRBallArray = Array (False,False,False,False,False)

Sub RollingTimer_Timer()
    Dim b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob - 1
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
		StopSound("fx_metalrolling" & b)
    Next

	If UBound(BOT) < tnob - 1 Then
		For b = UBound(BOT) + 1 To tnob - 1
			If BallShadow(b).Visible Then BallShadow(b).Visible = False
		Next
	End If

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then 
		For b = 0 to tnob - 1
			MRBallArray(b) = False
		next
		Exit Sub
	End If

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
      If BallVel(BOT(b) ) > 1 Then
			rolling(b) = True
			if MRBallArray(b) = True Then
				StopSound("fx_ballrolling" & b)
				PlaySound("fx_metalrolling" & b), -1, Vol(BOT(b)) * 2 * VolWire, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else
				StopSound("fx_metalrolling" & b)
				if BOT(b).z < 30 Then ' Ball on playfield
					PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
				Else ' Ball on raised ramp
					PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)) * 2, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+15000, 1, 0, AudioFade(BOT(b) )
				End If
			End If
	   Else
			If rolling(b) = True Then
				StopSound("fx_ballrolling" & b)
				StopSound("fx_metalrolling" & b)
				rolling(b) = False
				MRBallArray(b) = False
			End If
		End If

		If BOT(b).VelZ < -1 and BOT(b).z < 75 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySoundAtBOTBallZ "fx_balldrop" & b, BOT(b)
		End If

		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
		Else
			BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
		End If
		BallShadow(b).Y = BOT(b).Y + 12
		BallShadow(b).Visible = True '(BOT(ii).Z > 20)

	Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
  PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'******************************************************
'			STEPS 2-4 (FLIPPER POLARITY SETUP
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity


Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 44
	Next

	'"Polarity" Profile
	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.368, -4
	AddPt "Polarity", 2, 0.451, -3.7
	AddPt "Polarity", 3, 0.493, -3.88
	AddPt "Polarity", 4, 0.65, -2.3
	AddPt "Polarity", 5, 0.71, -2
	AddPt "Polarity", 6, 0.785,-1.8
	AddPt "Polarity", 7, 1.18, -1
	AddPt "Polarity", 8, 1.2, 0


	'"Velocity" Profile
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



'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'******************************************************
'		FLIPPER CORRECTION SUPPORTING FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper -
'ProcessBalls - catches ball data.
' - OR -
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
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
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property

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
				if DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 30 Then
			PartialFlipCoef = 0
		End If
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				if DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				if DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				if DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						if DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			'debug
			if DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				if IsEmpty(PolarityOut(0) ) then
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					if BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if
					if Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					if PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'if DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class


rightFlipper.timerinterval=1
rightflipper.timerenabled=True

sub RightFlipper_timer()

	If leftflipper.currentangle = leftflipper.endangle and LFPress = 1 then
		leftflipper.eostorqueangle = EOSAnew
		leftflipper.eostorque = EOSTnew
		LeftFlipper.rampup = EOSRampup
		if LFCount = 0 Then LFCount = GameTime
		if GameTime - LFCount < LiveCatch Then
			leftflipper.Elasticity = 0.1
			If LeftFlipper.endangle <> LFEndAngle Then leftflipper.endangle = LFEndAngle
		Else
			leftflipper.Elasticity = FElasticity
		end if
	elseif leftflipper.currentangle > leftflipper.startangle - 0.05  Then
		leftflipper.rampup = SOSRampup
		leftflipper.endangle = LFEndAngle - 3
		leftflipper.Elasticity = FElasticity
		LFCount = 0
	elseif leftflipper.currentangle > leftflipper.endangle + 0.01 Then
		leftflipper.eostorque = EOST
		leftflipper.eostorqueangle = EOSA
		LeftFlipper.rampup = Frampup
		leftflipper.Elasticity = FElasticity
	end if

	If rightflipper.currentangle = rightflipper.endangle and RFPress = 1 then
		rightflipper.eostorqueangle = EOSAnew
		rightflipper.eostorque = EOSTnew
		RightFlipper.rampup = EOSRampup
		if RFCount = 0 Then RFCount = GameTime
		if GameTime - RFCount < LiveCatch Then
			rightflipper.Elasticity = 0.1
			If RightFlipper.endangle <> RFEndAngle Then rightflipper.endangle = RFEndAngle
		Else
			rightflipper.Elasticity = FElasticity
		end if
	elseif rightflipper.currentangle < rightflipper.startangle + 0.05 Then
		rightflipper.rampup = SOSRampup
		rightflipper.endangle = RFEndAngle + 3
		rightflipper.Elasticity = FElasticity
		RFCount = 0
	elseif rightflipper.currentangle < rightflipper.endangle - 0.01 Then
		rightflipper.eostorque = EOST
		rightflipper.eostorqueangle = EOSA
		RightFlipper.rampup = Frampup
		rightflipper.Elasticity = FElasticity
	end if

end sub

dim LFPress, RFPress, EOST, EOSA, EOSTnew, EOSAnew
dim FStrength, Frampup, FElasticity, EOSRampup, SOSRampup
dim RFEndAngle, LFEndAngle, LFCount, RFCount, LiveCatch

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
FStrength = LeftFlipper.strength
Frampup = leftFlipper.rampup
FElasticity = leftFlipper.elasticity
EOSTnew = 1.0 'FEOST
EOSAnew = 0.2
EOSRampup = 1.5
SOSRampup = 8.5
LiveCatch = 8

LFEndAngle = leftflipper.endangle
RFEndAngle = rightflipper.endangle

'******************************************************
'		HELPER FUNCTIONS
'******************************************************

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

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

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


Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

' *********************************************************************
' some special physics behaviour
' *********************************************************************
' target or rubber post is hit so let the ball jump a bit
Sub DropTargetHit()
	DropTargetSound
	TargetHit
End Sub

Sub StandupTargetHit()
	StandUpTargetSound
	TargetHit
End Sub

Sub TargetHit()
    ActiveBall.VelZ = ActiveBall.VelZ * (0.5 + (Rnd()*LetTheBallJump + Rnd()*LetTheBallJump + 1) / 6)
End Sub

Sub RubberPostHit()
	ActiveBall.VelZ = ActiveBall.VelZ * (0.9 + (Rnd()*(LetTheBallJump-1) + Rnd()*(LetTheBallJump-1) + 1) / 6)
End Sub

Sub RubberRingHit()
	ActiveBall.VelZ = ActiveBall.VelZ * (0.8 + (Rnd()*(LetTheBallJump-1) + 1) / 6)
End Sub


' *********************************************************************
' more realtime sounds
' *********************************************************************
' ball collision sound

' rubber hit sounds
Sub RubberWalls_Hit(idx)
  PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10
  RubberRingHit
End Sub

Sub RubberPosts_Hit(idx)
  PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10
  RubberPostHit
End Sub

Sub RubberPins_Hit(idx)
  PlaySoundAtBallVol "fx_rubber_hit_" & Int(Rnd*3)+1, 10
  RubberPostHit
End Sub

' metal hit sounds
Sub Metal_Hit(idx)
	PlaySoundAtBallAbsVol "fx_metalhit" & Int(Rnd*3), Minimum(Vol(ActiveBall),0.5)
End Sub

' plastics hit sounds
Sub Plastics_Hit(idx)
	PlaySoundAtBallAbsVol "fx_ball_hitting_plastic", Minimum(Vol(ActiveBall),0.5)
End Sub

' gates sound
Sub Gates_Hit(idx)
	GateSound
End Sub

' sound at ramp rubber at the diverter
Sub RampRubber_Hit()
	PlaySoundAtBallVol "fx_flip_hit_" & Int(Rnd*3)+1, 10
End Sub

' *********************************************************************
' sound stuff
' *********************************************************************
Sub RollOverSound()
	PlaySoundAtVolPitch SoundFX("fx_rollover",DOFContactors), ActiveBall, 0.02, .25
End Sub
Sub DropTargetSound()
	PlaySoundAtVolPitch SoundFX("fx_droptarget",DOFTargets), ActiveBall, 2, .25
End Sub
Sub StandUpTargetSound()
	PlaySoundAtVolPitch SoundFX("fx_target",DOFTargets), ActiveBall, 2, .25
End Sub
Sub GateSound()
	PlaySoundAtVolPitch SoundFX("fx_gate",DOFContactors), ActiveBall, 0.02, .25
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub


'*******************************
'*** Flupper flasher routine ***
'*******************************

Dim FlashLevel12, FlashLevel29, FlashLevel61, FlashLevel91

Sub Flasher12_Timer()
  dim flashx3
  If not Flasher12.TimerEnabled Then
    Flasher12.TimerEnabled = True
    Flasher12.visible = 1
  End If
  flashx3 = FlashLevel12 * FlashLevel12 * FlashLevel12
  Flasher12.opacity = 1000 * flashx3
  l12a.IntensityScale = 0.5 * flashx3
  FlashLevel12 = FlashLevel12 * 0.9 - 0.01
  If FlashLevel12 < 0 Then
    Flasher12.TimerEnabled = False
    Flasher12.visible = 0
  End If
End Sub


Sub Flasher29_Timer()
  dim flashx3
  If not Flasher29.TimerEnabled Then
    Flasher29.TimerEnabled = True
    Flasher29.visible = 1
  End If
  flashx3 = FlashLevel29 * FlashLevel29 * FlashLevel29
  Flasher29.opacity = 1000 * flashx3
  l29a.IntensityScale = 0.5 * flashx3
  FlashLevel29 = FlashLevel29 * 0.9 - 0.01
  If FlashLevel29 < 0 Then
    Flasher29.TimerEnabled = False
    Flasher29.visible = 0
  End If
End Sub


Sub Flasher61_Timer()
	dim flashx3
	If not Flasher61.TimerEnabled Then
		Flasher61.TimerEnabled = True
		Flasher61.visible = 1
	End If
	flashx3 = FlashLevel61 * FlashLevel61 * FlashLevel61
	Flasher61.opacity = 1000 * flashx3
	l61a.IntensityScale = 0.5 * flashx3
	FlashLevel61 = FlashLevel61 * 0.9 - 0.01
	If FlashLevel61 < 0 Then
		Flasher61.TimerEnabled = False
		Flasher61.visible = 0
	End If
End Sub


Sub Flasher91_Timer()
	dim flashx3
	If not Flasher91.TimerEnabled Then
		Flasher91.TimerEnabled = True
		Flasher91.visible = 1
	End If
	flashx3 = FlashLevel91 * FlashLevel91 * FlashLevel91
	Flasher91.opacity = 1000 * flashx3
	l91a.IntensityScale = 0.5 * flashx3
	FlashLevel91 = FlashLevel91 * 0.9 - 0.01
	If FlashLevel91 < 0 Then
		Flasher91.TimerEnabled = False
		Flasher91.visible = 0
	End If
End Sub


'***********************************************************************************
'****							RealTime Updates								****
'***********************************************************************************

Dim BallShadow : BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5)

Sub GameTimer_Timer
  UpdateDropTargetWalls
  UpdateSpinners
  UpdateFlippers
  UpdateFlashers
End Sub

Sub UpdateDropTargetWalls
	sw25a.isdropped = sw25.isdropped
	sw26a.isdropped = sw26.isdropped
	sw27a.isdropped = sw27.isdropped
End Sub

Sub UpdateFlashers
	If (l12.state=lightStateOn) Then
		FlashLevel12 = DynamicFlasherIntensity
		Flasher12_Timer
	End If

	If (l29.state=lightStateOn) Then
		FlashLevel29 = DynamicFlasherIntensity
		Flasher29_Timer
	End If

	Flasher43.visible = (l43.state=lightStateOn)

	If (l44.state=lightStateOn) Then
		Flashlevel(2) = 1 : FlasherFlash2_Timer
	End If

	Flasher45.visible = (l45.state=lightStateOn)
	Flasher57.visible = (l57.state=lightStateOn)
	Flasher58.visible = (l58.state=lightStateOn)
	Flasher59.visible = (l59.state=lightStateOn)
	Flasher60.visible = (l60.state=lightStateOn)

	If (l61.state=lightStateOn) Then
		FlashLevel61 = DynamicFlasherIntensity
		Flasher61_Timer
	End If

	Flasher73.visible = (l73.state=lightStateOn)
	Flasher74.visible = (l74.state=lightStateOn)
	Flasher74a.visible = (l74.state=lightStateOn)
	Flasher75.visible = (l75.state=lightStateOn)

	If (l76.state=lightStateOn) Then
		Flashlevel(1) = 1 : FlasherFlash1_Timer
	End If

	Flasher79.visible = (l79.state=lightStateOn)
	Flasher89.visible = (l89.state=lightStateOn)

	If (l91.state=lightStateOn) Then
		FlashLevel91 = DynamicFlasherIntensity
		Flasher91_Timer
	End If

	Flasher92.visible = (l92.state=lightStateOn)
	Flasher95.visible = (l95.state=lightStateOn)

End Sub

Sub UpdateSpinners
	GateRampCP.RotZ = -(GateRampC.currentangle)
	GateRampC2P.RotZ = -(GateRampC2.currentangle)
	GateRampYP.RotZ = -(GateRampY.currentangle)
	GateRampY2P.RotZ = -(GateRampY2.currentangle)
	GateRampIP.RotZ = -(GateRampI.currentangle)
	GateRampTP.RotZ = -(GateRampT.currentangle)
	TopGateP.RotZ = -(TopGate.currentangle)
	sw24p.RotZ = -(sw24.currentangle)
End Sub

Sub UpdateFlippers

	LeftFlipperShadow.RotY = LeftFlipper.CurrentAngle - 90
	RightFlipperShadow.RotY = RightFlipper.CurrentAngle - 90
	LeftFlipperTopShadow.RotY = LeftFlipperTop.CurrentAngle - 90
	RightFlipperTopShadow.RotY = RightFlipperTop.CurrentAngle - 90

	RightFlipperP.RotY = RightFlipper.CurrentAngle
	LeftFlipperP.RotY = LeftFlipper.CurrentAngle
	RightFlipperTopP.RotY = RightFlipperTop.CurrentAngle
	LeftFlipperTopP.RotY = LeftFlipperTop.CurrentAngle
End Sub



'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub


dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

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

'Tracks ball velocity for judging bounce calculations & angle
'apologies to JimmyFingers is this is what his script does. I know his tracks ball velocity too but idk how it works in particular
dim cor : set cor = New CoRTracker
cor.debugOn = False
'cor.update() - put this on a low interval timer
Class CoRTracker
	public DebugOn 'tbpIn.text
	public ballvel

	Private Sub Class_Initialize : redim ballvel(0) : End Sub 
	'TODO this would be better if it didn't do the sorting every ms, but instead every time it's pulled for COR stuff
	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs
		if uBound(allballs) < 0 then if DebugOn then str = "no balls" : TBPin.text = str : exit Sub else exit sub end if: end if
		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			if DebugOn then 
				dim s, bs 'debug spacer, ballspeed
				bs = round(BallSpeed(b),1)
				if bs < 10 then s = " " else s = "" end if
				str = str & b.id & ": " & s & bs & vbnewline 
				'str = str & b.id & ": " & s & bs & "z:" & b.z & vbnewline 
			end if
		Next
		if DebugOn then str = "ubound ballvels: " & ubound(ballvel) & vbnewline & str : if TBPin.text <> str then TBPin.text = str : end if
	End Sub
End Class

Sub RDampen_Timer()
Cor.Update
End Sub


Dim TestFlashers, TableRef

TestFlashers = 0		' *** set this to 1 to check position of flasher object ***
Set TableRef = Table1   ' *** change this, if your table has another name       ***
Dim FlashLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "red" : 
InitFlasher 2, "red"


Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)

	objflasher(nr).height = objbase(nr).z + 55

	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) ': objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub


Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlashLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlashLevel(nr)^3
	objbase(nr).BlendDisableLighting =  10 * FlashLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * FlashLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,FlashLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	FlashLevel(nr) = FlashLevel(nr) * 0.9 - 0.01
	If FlashLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 


Playtest = 0

''''''''''''''''''''''''
''' Test Kicker
''''''''''''''''''''''''
Sub TestKickerIn_Hit
	TestKickerIn.DestroyBall
	TestKickerOut.CreateBall
	'TestKickerOut.Kick Angle,velocity
	TestKickerOut.Kick 15, 20
End Sub
Sub TestKickerLoop_Hit
	TestKickerLoop.DestroyBall
	TestKickerOut.CreateBall
	'TestKickerOut.Kick Angle,velocity
	TestKickerOut.Kick 330, 20
End Sub
Sub TestKickerOut_Hit
	TestKickerOut.Kick 15, 20
End Sub

''''''''''''''''''''''''''
