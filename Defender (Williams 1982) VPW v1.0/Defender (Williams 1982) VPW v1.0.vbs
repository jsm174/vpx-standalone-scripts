Option Explicit
Randomize

'		____  ___________________   ______  __________ 
'	   / __ \/ ____/ ____/ ____/ | / / __ \/ ____/ __ \
'	  / / / / __/ / /_  / __/ /  |/ / / / / __/ / /_/ /
'	 / /_/ / /___/ __/ / /___/ /|  / /_/ / /___/ _, _/ 
'	/_____/_____/_/   /_____/_/ |_/_____/_____/_/ |_| 
'
'    Williams 1982
'	 System 7
'
'
' DEFENDER TEAM
' -------------
' Apophis - Project lead, scripting, Roth/nFozzy physics, Fleep sound effects, Flupper 3D inserts
' Bord - 3D models and rendering
' Uncle Paulie - VR Room
' Jutsuka - Asset research and testing
' G94 - Playfield and plastics scans
' VPW Team - Testing
'
' Thank you to G94 for providing incredible playfield and plastic scans. Go check out his Defender playfield resotration here:
' https://pinside.com/pinball/forum/topic/defender-williams-1982-nos-playfield-


'********************
' Options
'********************
' Note: press the "R" key to see a zoomed-in view of the instruction card.

'----- Sound Options -----
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.
Const BallRollingVolume = 0.5		' Volume of ball rolling sound. Value ranges between 0 and 1

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

' *** Desktop, VR, or Full Cabinet Options *** This MUST be selected depending on the mode you are playing.

const VR_Room = 0 ' 1 = VR Room; 0 = desktop or cab mode, 

' *** If using VR Room: ***

const CustomWalls = 0 	'set to 0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const WallClock = 1	  	'1 Shows the clock in the VR room only   
const topper = 1		'0 = Off 1= On - Topper visible in VR Room only
const poster = 1		'1 Shows the flyer posters in the VR room only 
const poster2 = 1		'1 Shows the flyer posters in the VR room only 


'**********************************************************************************************************
' Load Table
'**********************************************************************************************************

Dim cab_mode, DesktopMode: DesktopMode = Table1.ShowDT
If Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="dfndr_l4",UseSolenoids=0,UseLamps=0,UseGI=0,SSolenoidOn="",SSolenoidOff="", SCoin=""

LoadVPM "", "S7.VBS", 2.0

Dim GameRun

BallMass = 1
BallSize = 50
Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height


dim VPMversion

'**********************************************************************************************************
' Timers
'**********************************************************************************************************

Sub GameTimer_Timer
	RDampen
	UpdateSolenoids
	UpdateRolling
End Sub


Sub FrameTimer_Timer
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows

	Pgate1.rotx = -max(Gate1.CurrentAngle,0)
	Pgate2.rotx = -max(Gate2.CurrentAngle,0)
	gatewire.rotx = max(Gate3.CurrentAngle,0)
	diverter.roty = Flipper3.currentangle
	LeftBat1.ObjRotZ = LeftFlipper1.CurrentAngle
	LeftBat.ObjRotZ = LeftFlipper.CurrentAngle
	RightBat.ObjRotZ = RightFlipper.CurrentAngle
	batleftshadow1.ObjRotZ = LeftFlipper1.CurrentAngle
	batleftshadow.ObjRotZ = LeftFlipper.CurrentAngle
	batrightshadow.ObjRotZ = RightFlipper.CurrentAngle

	If VR_Room = 1 Then
		VRDisplayTimer
	End If

	If VR_Room = 0 AND cab_mode = 0 Then
		DisplayTimer
	End If
End Sub

'**********************************************************************************************************
' Update Solenoids
'**********************************************************************************************************


Sub UpdateSolenoids
	Dim Changed, Count, funcName, ii, sol11, solNo
	Changed = Controller.ChangedSolenoids
	If Not IsEmpty(Changed) Then
		sol11 = Controller.Solenoid(11)
		Count = UBound(Changed, 1)
		For ii = 0 To Count
			solNo = Changed(ii, CHGNO)
			' multiplex solenoid #11 fixed in VPM 1.52beta and newer
			if VPMversion < "01519901" then
				If SolNo < 11 And sol11 Then solNo = solNo + 32
			else
				' no need to evaluate sol 11 anymore, VPM does it
				if SolNo > 50 then solNo = solNo - 18
			end if
			funcName = SolCallback(solNo)
			If funcName <> "" Then 
				'debug.print solNo
				'debug.print funcName
				Execute funcName & " CBool(" & Changed(ii, CHGSTATE) &")"
			End If
		Next
	End If
End Sub

'**********************************************************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1)     = "SolDTLBankUnhit 13,"
SolCallback(2)     = "SolDTLBankUnhit 14,"
SolCallback(3)     = "SolDTLBankUnhit 15,"
SolCallback(4)     = "SolDTLBankUnhit 16,"
SolCallback(5)     = "SolDTLBankUnhit 17,"
SolCallback(6)     = "SolDTLBankDropDown"
SolCallback(7)     = "SolDTBPodDropUp"
SolCallback(8)     = "SolRelease"
SolCallback(9)     = "SolDTBait1"
SolCallback(10)    = "SolDTBait3"
SolCallback(12)    = "SolDrain"
SolCallback(13)    = "SolAPlunger"
SolCallback(14)    = "SolPFGI" 'PF GI
SolCallback(15)	   = "SolKnocker"
'SolCallback(17)	   = "" 'Left pop bumper
'SolCallback(18)	   = ""	'Right pop bumper
SolCallback(21)    = "SolCenterFlasher"
SolCallback(22)    = "SolFlipperDiverter"
SolCallback(25)    = "SolRun"
SolCallback(33)    = "SolDTRBankUnhit 23,"
SolCallback(34)    = "SolDTRBankUnhit 24,"
SolCallback(35)    = "SolDTRBankUnhit 25,"
SolCallback(36)    = "SolDTRBankUnhit 26,"
SolCallback(37)    = "SolDTRBankUnhit 27,"
SolCallback(38)    = "SolDTRBankDropDown"
SolCallback(39)    = "SolDTTPodDropUp"
SolCallback(40)    = "SolUnlock"
SolCallback(41)    = "SolDTBait2"
SolCallback(42)    = "SolDTBaitDropDown"

SolCallback(sLRFlipper) = ""  			'Fast flip workaround, see keydown and keyup subs 
SolCallback(sLLFlipper) = ""
'SolCallback(sLRFlipper) = "SolRFlipper"
'SolCallback(sLLFlipper) = "SolLFlipper"

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		LeftFlipper1.RotateToEnd

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart
		LeftFlipper1.RotateToStart
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


Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub



'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

dim FlippersEnabled

Sub SolRun(enabled)
	FlippersEnabled = Enabled
	if enabled then
		GameRun = True
		LeftSlingShot.Disabled = 0
		RightSlingShot.Disabled = 0
	Elseif not Enabled Then
		GameRun = false
		LeftSlingShot.Disabled = 1
		RightSlingShot.Disabled = 1
		SolLFlipper 0 
		SolRFlipper 0 
	end if
End Sub

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

dim gilvl : gilvl = 1
Sub SolPFGI(Enabled)
	If Enabled Then
		SetLamp 0,0
		Sound_GI_Relay 0,sw46
		gilvl = 0
		Backglass.ImageA = "Defender Dark"
	Else
		SetLamp 0,1
		Sound_GI_Relay 1,sw46
		gilvl = 1
		Backglass.ImageA = "Defender Illuminated"
	End if

End Sub

Sub SolAPlunger(enabled)
	If enabled Then
		Plunger1.Fire
		SoundPlunger1ReleaseBall
	Else
		Plunger1.PullBack
	End If
End Sub

Sub SolFlipperDiverter(enabled)
	If Enabled Then
		Flipper3.RotateToEnd
	Else
		Flipper3.RotateToStart
	End If
End Sub

Sub SolCenterFlasher(Enabled)
	If Enabled Then
		Lampz.state(100) = 1
	Else
		Lampz.state(100) = 0
	End If
End Sub




'******************************************************
' TROUGH 
'******************************************************

Sub sw47_Hit   : Controller.Switch(47) = 1 : UpdateTrough : End Sub
Sub sw47_UnHit : Controller.Switch(47) = 0 : UpdateTrough : End Sub
Sub sw48_Hit   : Controller.Switch(48) = 1 : UpdateTrough : End Sub
Sub sw48_UnHit : Controller.Switch(48) = 0 : UpdateTrough : End Sub
Sub sw49_Hit   : Controller.Switch(49) = 1 : UpdateTrough : End Sub
Sub sw49_UnHit : Controller.Switch(49) = 0 : UpdateTrough : End Sub

Sub UpdateTrough
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer
	If sw47.BallCntOver = 0 Then sw48.kick 60, 20
	If sw48.BallCntOver = 0 Then sw49.kick 60, 20
	Me.Enabled = 0
End Sub

'******************************************************
' DRAIN & RELEASE
'******************************************************

Sub sw46_Hit
	RandomSoundDrain sw46
	Controller.Switch(46) = 1
End Sub

Sub SolDrain(enabled)
	If enabled Then 
		sw46.kick 60, 20
		Controller.Switch(46) = 0
	End If
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		sw47.kick 56, 9		
		RandomSoundBallRelease sw47
	End If
End Sub


'******************************************************
' LOCKUP 
'******************************************************

Sub sw42_Hit   : Controller.Switch(42) = 1 : UpdateLock : End Sub
Sub sw42_UnHit : Controller.Switch(42) = 0 : UpdateLock : End Sub
Sub sw43_Hit   : Controller.Switch(43) = 1 : UpdateLock : End Sub
Sub sw43_UnHit : Controller.Switch(43) = 0 : UpdateLock : End Sub
Sub sw44_Hit   : Controller.Switch(44) = 1 : UpdateLock :  SoundSaucerLock: End Sub
Sub sw44_UnHit : Controller.Switch(44) = 0 : UpdateLock : End Sub

Sub UpdateLock
	UpdateLockTimer.Interval = 300
	UpdateLockTimer.Enabled = 1
End Sub

Sub UpdateLockTimer_Timer
	If sw42.BallCntOver = 0 Then sw43.kick 180, 10
	If sw43.BallCntOver = 0 Then sw44.kick 180, 10
	Me.Enabled = 0
End Sub

'******************************************************
' RELEASE LOCK
'******************************************************

Sub SolUnlock(enabled)
	If enabled Then 
		sw42.kick 180, 1		
		SoundSaucerKick 1, sw42
	End If
End Sub



'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim DefenderBall1, DefenderBall2, DefenderBall3, gBOT

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Defender Williams"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 1
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With
	On Error Goto 0

	VPMversion=Controller.version

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch = swTilt
	vpmNudge.Sensitivity = 6
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,LeftslingShot,RightslingShot)

	'************  Trough	**************
	Set DefenderBall3 = sw49.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set DefenderBall2 = sw48.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set DefenderBall1 = sw47.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(DefenderBall1,DefenderBall2,DefenderBall3)

	Controller.Switch(49) = 1
	Controller.Switch(48) = 1
	Controller.Switch(47) = 1

	Plunger1.Pullback

	If VR_Room = 1 Then
		SetBackglass
	End If

End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
Dim BIPL : BIPL=0

Sub Table1_KeyDown(ByVal KeyCode)

	If KeyCode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull
		TimerVRPlunger.enabled = true
		TimerVRPlunger2.enabled = False
	End If

	If keycode = RightFlipperKey and GameRun Then 
		Controller.Switch(swLRFlip) = True
		VRFlipperButtonRight.X = VRFlipperButtonRight.X -8
		FlipperActivate RightFlipper, RFPress
		SolRFlipper 1       'emulating fast flips
	End If
	If keycode = LeftFlipperKey and GameRun Then 
		Controller.Switch(swLLFlip) = True
		VRFlipperButtonLeft.X = VRFlipperButtonLeft.X +8
		FlipperActivate LeftFlipper, LFPress
		SolLFlipper 1       'emulating fast flips
	End If
	If keycode = LeftMagnaSave Then 
		Controller.Switch(61) = True
		VRFlipperButtonLeftMagna.X = VRFlipperButtonLeftMagna.X +8
	End If
	If keycode = RightMagnaSave Then 
		Controller.Switch(60) = True
		VRFlipperButtonRightMagna.X = VRFlipperButtonRightMagna.X -8
	End If
	If keycode = LockBarKey Then Controller.Switch(60) = True

	If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter
	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

		End Select
	End If
	if keycode = StartGameKey then 
		soundStartButton
		startbutton.y = startbutton.y -3
	End If



	If keycode = 19 then ScoreCard=1 : CardTimer.enabled=True

	If KeyDownHandler(keycode) Then Exit Sub

End Sub

Sub Table1_KeyUp(ByVal KeyCode)

	If keycode = RightFlipperKey and GameRun Then 
		Controller.Switch(swLRFlip) = False
		VRFlipperButtonRight.X = VRFlipperButtonRight.X +8
		FlipperActivate RightFlipper, RFPress
		SolRFlipper 0       'emulating fast flips
	End If

	If keycode = LeftFlipperKey and GameRun Then 
		Controller.Switch(swLLFlip) = False
		VRFlipperButtonLeft.X = VRFlipperButtonLeft.X -8
		FlipperActivate LeftFlipper, LFPress
		SolLFlipper 0       'emulating fast flips
	End If

	If keycode = LeftMagnaSave Then 
		Controller.Switch(61) = False
		VRFlipperButtonLeftMagna.X = VRFlipperButtonLeftMagna.X -8
	End If

	If keycode = RightMagnaSave Then 
		Controller.Switch(60) = False
		VRFlipperButtonRightMagna.X = VRFlipperButtonRightMagna.X +8
	End If
	If keycode = LockBarKey Then Controller.Switch(60) = False

	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall                        'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall                        'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.enabled = false
		TimerVRPlunger2.enabled = true
	End If

	If Keycode = StartGameKey Then 
		StartButton.y = StartButton.y +3
	End If

	If keycode = 19 then ScoreCard=0

	If KeyUpHandler(keycode) Then Exit Sub

End Sub


'***************************************************************************
' VR Plunger Code
'***************************************************************************

TimerVRPlunger2.enabled = true

Sub TimerVRPlunger_Timer
	if VRPlunger.Y < 2240 then VRPlunger.Y = VRPlunger.y +6  'If the plunger is not fully extend it, then extend it by 5 coordinates in the Y, 
End Sub

Sub TimerVRPlunger2_Timer
	VRPlunger.Y = 2129 + (5* Plunger.Position) -20
end sub


'***************************************************************************
'				Drop Target Controls
'***************************************************************************

Sub sw33_Hit : DTHit 33 : TargetBouncer Activeball, 1 : End Sub
Sub sw34_Hit : DTHit 34 : TargetBouncer Activeball, 1 : End Sub
Sub sw35_Hit : DTHit 35 : TargetBouncer Activeball, 1 : End Sub
Sub sw39_Hit : DTHit 39 : TargetBouncer Activeball, 1 : End Sub
Sub sw40_Hit : DTHit 40 : TargetBouncer Activeball, 1 : End Sub

Sub sw13_Hit : DTHit 13 : TargetBouncer Activeball, 1 : End Sub
Sub sw14_Hit : DTHit 14 : TargetBouncer Activeball, 1 : End Sub
Sub sw15_Hit : DTHit 15 : TargetBouncer Activeball, 1 : End Sub
Sub sw16_Hit : DTHit 16 : TargetBouncer Activeball, 1 : End Sub
Sub sw17_Hit : DTHit 17 : TargetBouncer Activeball, 1 : End Sub

Sub sw23_Hit : DTHit 23 : TargetBouncer Activeball, 1 : End Sub
Sub sw24_Hit : DTHit 24 : TargetBouncer Activeball, 1 : End Sub
Sub sw25_Hit : DTHit 25 : TargetBouncer Activeball, 1 : End Sub
Sub sw26_Hit : DTHit 26 : TargetBouncer Activeball, 1 : End Sub
Sub sw27_Hit : DTHit 27 : TargetBouncer Activeball, 1 : End Sub


' Left Center Drop Target
Sub SolDTBait1(enabled)
	If enabled Then
		DTRaise 33
	End If
End Sub

' Right Center Drop Target
Sub SolDTBait2(enabled)
	If enabled Then
		RandomSoundDropTargetReset sw33p
		DTRaise 34
	End If
End Sub

' Right Top Drop Target
Sub SolDTBait3(enabled)
	If enabled Then
		RandomSoundDropTargetReset sw35p
		DTRaise 35
	End If
End Sub

Sub SolDTBaitDropDown(enabled)
	if enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors), Light56
		DTDrop 33
		DTDrop 34
		DTDrop 35
	end if
End Sub

' Bottom Pod Top Drop Target
Sub SolDTBPodDropUp(enabled)
	If enabled Then
		RandomSoundDropTargetReset sw39p
		DTRaise 39
	End If
End Sub

' Top Pod Top Drop Target
Sub SolDTTPodDropUp(enabled)
	If enabled Then
		RandomSoundDropTargetReset sw40p
		DTRaise 40
	End If
End Sub

' Raise specific target in left bank
Sub SolDTLBankUnhit(sw,enabled)
	If enabled Then
		RandomSoundDropTargetReset sw15p
		DTRaise sw
	End If
End Sub

' Left Bank of Drop Targets
Sub SolDTLBankDropDown(enabled)
	If enabled Then
		DTDrop 13
		DTDrop 14
		DTDrop 15
		DTDrop 16
		DTDrop 17
	End If
End Sub

' Raise specific target in right bank
Sub SolDTRBankUnhit(sw,enabled)
	If enabled Then
		RandomSoundDropTargetReset sw25p
		DTRaise sw
	End If
End Sub


' Right Bank of Drop Targets
Sub SolDTRBankDropDown(enabled)
	If enabled Then
		DTDrop 23
		DTDrop 24
		DTDrop 25
		DTDrop 26
		DTDrop 27
	End If
End Sub



'**********************************************************************************************************


'Wire Triggers
Sub sw9_Hit    : Controller.Switch(9)=1 : End Sub 
Sub sw9_UnHit  : Controller.Switch(9)=0 : End Sub
Sub sw10_Hit   : Controller.Switch(10)=1 : End Sub 
Sub sw10_UnHit : Controller.Switch(10)=0 : End Sub 
Sub sw11_Hit   : Controller.Switch(11)=1 : End Sub 
Sub sw11_UnHit : Controller.Switch(11)=0 : End Sub 
Sub sw12_Hit   : Controller.Switch(12)=1 : End Sub 
Sub sw12_UnHit : Controller.Switch(12)=0 : End Sub
Sub sw45_Hit   : Controller.Switch(45)=1 : End Sub 
Sub sw45_UnHit : Controller.Switch(45)=0 : End Sub
Sub sw50_Hit   : Controller.Switch(50)=1 : BIPL=1 : End Sub 
Sub sw50_UnHit : Controller.Switch(50)=0 : BIPL=0 :End Sub
Sub sw51_Hit   : Controller.Switch(51)=1 : End Sub 
Sub sw51_UnHit : Controller.Switch(51)=0 : End Sub
Sub sw52_Hit   : Controller.Switch(52)=1 : End Sub 
Sub sw52_UnHit : Controller.Switch(52)=0 : End Sub
Sub sw53_Hit   : Controller.Switch(53)=1 : End Sub 
Sub sw53_UnHit : Controller.Switch(53)=0 : End Sub
Sub sw54_Hit   : Controller.Switch(54)=1 : End Sub 
Sub sw54_UnHit : Controller.Switch(54)=0 : End Sub

'Stand Up Targets
Sub sw18_hit : STHit 18 : End Sub 
Sub sw19_hit : STHit 19 : End Sub 
Sub sw20_hit : STHit 20 : End Sub 
Sub sw21_hit : STHit 21 : End Sub 
Sub sw22_hit : STHit 22 : End Sub 
Sub sw28_hit : STHit 28 : End Sub 
Sub sw29_hit : STHit 29 : End Sub 
Sub sw30_hit : STHit 30 : End Sub 
Sub sw31_hit : STHit 31 : End Sub 
Sub sw32_hit : STHit 32 : End Sub 
Sub sw36_hit : STHit 36 : End Sub 
Sub sw37_hit : STHit 37 : End Sub 
Sub sw41_hit : STHit 41 : End Sub 

'Scoring Rubbers
Sub sw38_hit:vpmTimer.pulseSw 38 : playsound"flip_hit_3" : End Sub 

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(55) : RandomSoundBumperTop Bumper1: End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(56) : RandomSoundBumperBottom Bumper2: End Sub




'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************

Dim Digits(32)
' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)

' 2nd Player
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)

' 3rd Player
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)

' 4th Player
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)

' Credits
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)

' Balls
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)


'InitDisplay
'
'Sub InitDisplay
'	Dim num,obj
'	For num = 0 To UBound(Digits)
'		For Each obj In Digits(num)
'			obj.State = 0
'		Next
'	Next
'End Sub

Sub DisplayTimer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 32) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else
			end if
		next
	end if
End Sub


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 58
	RandomSoundSlingshotRight SLING1
	RSling.Visible = 0
	RSling1.Visible = 1
	sling1.TransZ = -20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
		Case 3:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 57
	RandomSoundSlingshotLeft SLING2
	LSling.Visible = 0
	LSling1.Visible = 1
	sling2.TransZ = -20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
		Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
	End Select
	LStep = LStep + 1
End Sub




'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************
Const tnob = 3
Const lob = 0

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

Sub UpdateRolling
	Dim b

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob
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
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * BallRollingVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

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
		If AmbientBallShadowOn = 0 Then
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = gBOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************





'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 15	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","")
currentShadowCount = Array (0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(3), objrtx2(3)
dim objBallShadow(3)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3)

DynamicBSInit

sub DynamicBSInit
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.21
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.22
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.24
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
	Dim s, Source, LSd, currentMat, AnotherSource, iii

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

	'The Magic happens now
	For s = lob to UBound(gBOT)

	' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = gBOT(s).X
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

	' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 Then 'And gBOT(s).Y < (TableHeight - 200) Then 'Or gBOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((gBOT(s).x-DSSources(iii)(0)),(gBOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), gBOT(s).X, gBOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							AnotherSource = sourcenames(s)
							objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), gBOT(s).X, gBOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((gBOT(s).x-DSSources(AnotherSource)(0)),(gBOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), gBOT(s).X, gBOT(s).Y) + 90
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
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************



'**********************************************************************************************************
'**********************************************************************************************************



'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

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
	If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
	Else
		AudioFade = Csng(-((- tmp) ^10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth-1
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
	RndInt = Int(Rnd * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
	RndNum = Rnd * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlunger1ReleaseBall
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger1
End Sub

Sub SoundPlungerReleaseNoBall
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid
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
Sub RandomSoundRollover
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
		RandomSoundRubberWeak
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
Sub RandomSoundRubberWeak
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall      
End Sub

Sub RandomSoundWall
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
Sub RandomSoundMetal
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
Sub RandomSoundBottomArchBallGuide
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
Sub RandomSoundBottomArchBallGuideHardHit
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide
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
Sub RandomSoundTargetHitStrong
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak
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

Sub SoundPlayfieldGate			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock
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
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = gBOT

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


Sub RDampen
	Cor.Update
End Sub



'*******************************************
'     FlippersPol  late 70s to early 80s
'*******************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity
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

Sub TriggerLF_Hit : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit : RF.PolarityCorrect activeball : End Sub





'******************************************************
'           FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
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

	Public Sub Fire 
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

	Public Sub ProcessBalls 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn then 
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

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
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
	Public Sub Reset
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
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
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

Function max(a,b)
	if a > b Then max=a Else max=b
End Function

Function min(a,b)
	if a >= b Then min=b Else min=a
End Function

'*************************************************
' Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

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
' End - Check ball distance from Flipper for Rem
'*************************************************

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

dim LFPress, RFPress, LFCount, RFCount
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
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

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

		For b = 0 to UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= -0.4 Then gBOT(b).vely = -0.4
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
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
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


	Public Sub Report         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class



'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1

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
	end if
end sub






'******************************************************
'		DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_primoff, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get PrimOff(): Set PrimOff = m_primoff: End Property
  Public Property Let PrimOff(input): Set m_primoff = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, primoff, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    Set m_primoff = primoff 
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

'Define a variable for each drop target
Dim DT33, DT34, DT35, DT39, DT40
Dim DT13, DT14, DT15, DT16, DT17
Dim DT23, DT24, DT25, DT26, DT27

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
'
'       isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT33 = (new DropTarget)(sw33, sw33a, sw33p, sw33poff, 33, 0, false)
Set DT34 = (new DropTarget)(sw34, sw34a, sw34p, sw34poff, 34, 0, false)
Set DT35 = (new DropTarget)(sw35, sw35a, sw35p, sw35poff, 35, 0, false)
Set DT39 = (new DropTarget)(sw39, sw39a, sw39p, sw39poff, 39, 0, false)
Set DT40 = (new DropTarget)(sw40, sw40a, sw40p, sw40poff, 40, 0, false)
Set DT13 = (new DropTarget)(sw13, sw13a, sw13p, sw13poff, 13, 0, false)
Set DT14 = (new DropTarget)(sw14, sw14a, sw14p, sw14poff, 14, 0, false)
Set DT15 = (new DropTarget)(sw15, sw15a, sw15p, sw15poff, 15, 0, false)
Set DT16 = (new DropTarget)(sw16, sw16a, sw16p, sw16poff, 16, 0, false)
Set DT17 = (new DropTarget)(sw17, sw17a, sw17p, sw17poff, 17, 0, false)
Set DT23 = (new DropTarget)(sw23, sw23a, sw23p, sw23poff, 23, 0, false)
Set DT24 = (new DropTarget)(sw24, sw24a, sw24p, sw24poff, 24, 0, false)
Set DT25 = (new DropTarget)(sw25, sw25a, sw25p, sw25poff, 25, 0, false)
Set DT26 = (new DropTarget)(sw26, sw26a, sw26p, sw26poff, 26, 0, false)
Set DT27 = (new DropTarget)(sw27, sw27a, sw27p, sw27poff, 27, 0, false)


Dim DTArray
DTArray = Array(DT33, DT34, DT35, DT39, DT40, DT13, DT14, DT15, DT16, DT17, DT23, DT24, DT25, DT26, DT27)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 90 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 50 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.3 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	'PlaySoundAtVol  DTHitSound, Activeball, Vol(Activeball)*22.5
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

'Add Timer name DTAnim to editor to handle drop target animations
DTAnim.interval = 10
DTAnim.enabled = True

Sub DTAnim_Timer
	DoDTAnim
	DoSTAnim
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


Sub DoDTAnim
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).primoff,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, primoff, switch, animate)
	dim transz, switchid
	Dim animtime, rangle

	switchid = switch

	rangle = prim.rotz * PI / 180

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
		primoff.rotx = prim.rotx
		primoff.roty = prim.roty
		DTAnimate = animate
		Exit Function
		elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		primoff.rotx = prim.rotx
		primoff.roty = prim.roty
		animate = 2
		SoundDropTargetDrop prim
	End If

	if animate = 2 Then
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
			primoff.transz = prim.transz
		end if

		prim.rotx = DTMaxBend * cos(rangle)/2
		prim.roty = DTMaxBend * sin(rangle)/2
		primoff.rotx = prim.rotx
		primoff.roty = prim.roty

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			primoff.transz = prim.transz
			secondary.collidable = 0
			controller.Switch(Switchid) = 1
			DTShadowHide switchid
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
		primoff.rotx = prim.rotx
		primoff.roty = prim.roty
	elseif animate = 3 and animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primoff.rotx = prim.rotx
		primoff.roty = prim.roty
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If

	if animate = -1 Then
		transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1

		If prim.transz = -DTDropUnits Then
			Dim b

			For b = 0 to UBound(gBOT)
				If InRotRect(gBOT(b).x,gBOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and gBOT(b).z < prim.z+DTDropUnits+25 Then
					gBOT(b).velz = 20
				End If
			Next
		End If

		if prim.transz < 0 Then
			prim.transz = transz
			primoff.transz = prim.transz
		elseif transz > 0 then
			prim.transz = transz
			primoff.transz = prim.transz
		end if

		if prim.transz > DTDropUpUnits then 
			DTAnimate = -2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primoff.transz = prim.transz
			primoff.rotx = prim.rotx
			primoff.roty = prim.roty
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		controller.Switch(Switchid) = 0
		DTShadowShow switchid
	End If

	if animate = -2 and animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
		primoff.transz = prim.transz
		if prim.transz < 0 then
			prim.transz = 0
			primoff.transz = prim.transz
			primary.uservalue = 0
			DTAnimate = 0

			primary.collidable = 1
			secondary.collidable = 0
		end If 
	End If
End Function

Sub DTShadowHide(switchid)
	Select Case switchid
		Case 13: ShadowDT(0).visible=False
		Case 14: ShadowDT(1).visible=False
		Case 15: ShadowDT(2).visible=False
		Case 16: ShadowDT(3).visible=False
		Case 17: ShadowDT(4).visible=False
		Case 23: ShadowDT(5).visible=False
		Case 24: ShadowDT(6).visible=False
		Case 25: ShadowDT(7).visible=False
		Case 26: ShadowDT(8).visible=False
		Case 27: ShadowDT(9).visible=False
	End Select
End Sub

Sub DTShadowShow(switchid)
	Select Case switchid
		Case 13: ShadowDT(0).visible=True
		Case 14: ShadowDT(1).visible=True
		Case 15: ShadowDT(2).visible=True
		Case 16: ShadowDT(3).visible=True
		Case 17: ShadowDT(4).visible=True
		Case 23: ShadowDT(5).visible=True
		Case 24: ShadowDT(6).visible=True
		Case 25: ShadowDT(7).visible=True
		Case 26: ShadowDT(8).visible=True
		Case 27: ShadowDT(9).visible=True
	End Select
End Sub



'******************************************************
'		DROP TARGET
'		SUPPORTING FUNCTIONS 
'******************************************************


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
Dim ST18, ST19, ST20, ST21, ST22
Dim ST28, ST29, ST30, ST31, ST32
Dim ST36, ST37, ST41

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts

Set ST18 = (new StandupTarget)(sw18, sw18p, 18, 0)
Set ST19 = (new StandupTarget)(sw19, sw19p, 19, 0)
Set ST20 = (new StandupTarget)(sw20, sw20p, 20, 0)
Set ST21 = (new StandupTarget)(sw21, sw21p, 21, 0)
Set ST22 = (new StandupTarget)(sw22, sw22p, 22, 0)
Set ST28 = (new StandupTarget)(sw28, sw28p, 28, 0)
Set ST29 = (new StandupTarget)(sw29, sw29p, 29, 0)
Set ST30 = (new StandupTarget)(sw30, sw30p, 30, 0)
Set ST31 = (new StandupTarget)(sw31, sw31p, 31, 0)
Set ST32 = (new StandupTarget)(sw32, sw32p, 32, 0)
Set ST36 = (new StandupTarget)(sw36, sw36p, 36, 0)
Set ST37 = (new StandupTarget)(sw37, sw37p, 37, 0)
Set ST41 = (new StandupTarget)(sw41, sw41p, 41, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST18, ST19, ST20, ST21, ST22, ST28, ST29, ST30, ST31, ST32, ST36, ST37, ST41)

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

Sub DoSTAnim
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
'		END STAND-UP TARGETS
'******************************************************




'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.


Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = 15
LampTimer.Enabled = 1

Sub LampTimer_Timer
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = 1
Sub LampTimer2_Timer
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub


'Fade material for green, red, yellow colored Bulb prims
Sub FadeMeshMaterial(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub



Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub




Sub InitLampsNF

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/10 : next
	Lampz.FadeSpeedUp(0) = 1/6 : Lampz.FadeSpeedDown(0) = 1/18  	'GI
	Lampz.FadeSpeedUp(100) = 1/2 : Lampz.FadeSpeedDown(0) = 1/8  	'Center flasher

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(7)= Light7	
	Lampz.Callback(7) = "DisableLighting p9, 1.1,"
	Lampz.MassAssign(9)= Light9
	Lampz.MassAssign(9)= Light9h
	Lampz.Callback(9) = "DisableLighting p9, 75,"
	Lampz.MassAssign(10)= Light10
	Lampz.MassAssign(10)= Light10h
	Lampz.Callback(10) = "DisableLighting p10, 75,"
	Lampz.MassAssign(11)= Light11
	Lampz.MassAssign(11)= Light11h
	Lampz.Callback(11) = "DisableLighting p11, 75,"
	Lampz.MassAssign(12)= Light12
	Lampz.MassAssign(12)= Light12h
	Lampz.Callback(12) = "DisableLighting p12, 75,"
	Lampz.MassAssign(13)= Light13
	Lampz.MassAssign(13)= Light13h
	Lampz.Callback(13) = "DisableLighting p13, 75,"
	Lampz.MassAssign(14)= Light14
	Lampz.MassAssign(14)= Light14h
	Lampz.Callback(14) = "DisableLighting p14, 75,"
	Lampz.MassAssign(15)= Light15
	Lampz.MassAssign(15)= Light15h
	Lampz.Callback(15) = "DisableLighting p15, 75,"
	Lampz.MassAssign(16)= Light16
	Lampz.MassAssign(16)= Light16h
	Lampz.Callback(16) = "DisableLighting p16, 75,"
	Lampz.MassAssign(17)= Light17
	Lampz.MassAssign(17)= Light17h
	Lampz.Callback(17) = "DisableLighting p17, 75,"
	Lampz.MassAssign(18)= Light18
	Lampz.MassAssign(18)= Light18h
	Lampz.Callback(18) = "DisableLighting p18, 75,"
	Lampz.MassAssign(19)= Light19
	Lampz.MassAssign(19)= Light19h
	Lampz.Callback(19) = "DisableLighting p19, 150,"
	Lampz.MassAssign(20)= Light20
	Lampz.MassAssign(20)= Light20h
	Lampz.Callback(20) = "DisableLighting p20, 150,"
	Lampz.MassAssign(21)= Light21
	Lampz.MassAssign(21)= Light21h
	Lampz.Callback(21) = "DisableLighting p21, 150,"
	Lampz.MassAssign(22)= Light22
	Lampz.MassAssign(22)= Light22h
	Lampz.Callback(22) = "DisableLighting p22, 150,"
	Lampz.MassAssign(23)= Light23
	Lampz.MassAssign(23)= Light23h
	Lampz.Callback(23) = "DisableLighting p23, 150,"
	Lampz.MassAssign(24)= Light24
	Lampz.MassAssign(24)= Light24h
	Lampz.Callback(24) = "DisableLighting p24, 150,"
	Lampz.MassAssign(25)= Light25
	Lampz.MassAssign(25)= Light25h
	Lampz.Callback(25) = "DisableLighting p25, 150,"
	Lampz.MassAssign(26)= Light26
	Lampz.MassAssign(26)= Light26h
	Lampz.Callback(26) = "DisableLighting p26, 150,"
	Lampz.MassAssign(27)= Light27
	Lampz.MassAssign(27)= Light27h
	Lampz.Callback(27) = "DisableLighting p27, 150,"
	Lampz.MassAssign(28)= Light28
	Lampz.MassAssign(28)= Light28h
	Lampz.Callback(28) = "DisableLighting p28, 150,"
	Lampz.MassAssign(29)= Light29
	Lampz.MassAssign(29)= Light29h
	Lampz.Callback(29) = "DisableLighting p29, 300,"
	Lampz.MassAssign(30)= Light30
	Lampz.MassAssign(30)= Light30h
	Lampz.Callback(30) = "DisableLighting p30, 300,"
	Lampz.MassAssign(31)= Light31
	Lampz.MassAssign(31)= Light31h
	Lampz.Callback(31) = "DisableLighting p31, 300,"
	Lampz.MassAssign(32)= Light32
	Lampz.MassAssign(32)= Light32h
	Lampz.Callback(32) = "DisableLighting p32, 300,"
	Lampz.MassAssign(33)= Light33
	Lampz.MassAssign(33)= Light33h
	Lampz.Callback(33) = "DisableLighting p33, 300,"
	Lampz.MassAssign(34)= Light34
	Lampz.MassAssign(34)= Light34h
	Lampz.Callback(34) = "DisableLighting p34, 50,"
	Lampz.MassAssign(35)= Light35
	Lampz.MassAssign(35)= Light35h
	Lampz.Callback(35) = "DisableLighting p35, 50,"
	Lampz.MassAssign(36)= Light36
	Lampz.MassAssign(36)= Light36h
	Lampz.Callback(36) = "DisableLighting p36, 50,"
	Lampz.MassAssign(37)= Light37
	Lampz.MassAssign(37)= Light37h
	Lampz.Callback(37) = "DisableLighting p37, 50,"
	Lampz.MassAssign(38)= Light38
	Lampz.MassAssign(38)= Light38h
	Lampz.Callback(38) = "DisableLighting p38, 50,"
	Lampz.MassAssign(39)= Light39
	Lampz.MassAssign(39)= Light39h
	Lampz.MassAssign(39)= Light39f
	Lampz.Callback(39) = "DisableLighting p39, 75,"
	Lampz.MassAssign(40)= Light40
	Lampz.MassAssign(40)= Light40h
	Lampz.MassAssign(40)= Light40f
	Lampz.Callback(40) = "DisableLighting p40, 75,"
	Lampz.MassAssign(41)= Light41
	Lampz.MassAssign(41)= Light41h
	Lampz.MassAssign(41)= Light41f
	Lampz.Callback(41) = "DisableLighting p41, 75,"
	Lampz.MassAssign(42)= Light42
	Lampz.MassAssign(42)= Light42h
	Lampz.MassAssign(42)= Light42f
	Lampz.Callback(42) = "DisableLighting p42, 75,"
	Lampz.MassAssign(43)= Light43
	Lampz.MassAssign(43)= Light43h
	Lampz.Callback(43) = "DisableLighting p43, 75,"
	Lampz.MassAssign(44)= Light44
	Lampz.MassAssign(44)= Light44h
	Lampz.Callback(44) = "DisableLighting p44, 75,"
	Lampz.MassAssign(45)= Light45
	Lampz.MassAssign(45)= Light45h
	Lampz.Callback(45) = "DisableLighting p45, 75,"
	Lampz.MassAssign(46)= Light46
	Lampz.MassAssign(46)= Light46h
	Lampz.Callback(46) = "DisableLighting p46, 75,"
	Lampz.MassAssign(47)= Light47
	Lampz.MassAssign(47)= Light47h
	Lampz.Callback(47) = "DisableLighting p47, 75,"
	Lampz.MassAssign(48)= Light48
	Lampz.MassAssign(48)= Light48h
	Lampz.Callback(48) = "DisableLighting p48, 75,"
	Lampz.MassAssign(49)= Light49
	Lampz.MassAssign(49)= Light49h
	Lampz.Callback(49) = "DisableLighting p49, 75,"
	Lampz.MassAssign(50)= Light50
	Lampz.MassAssign(50)= Light50h
	Lampz.Callback(50) = "DisableLighting p50, 75,"
	Lampz.MassAssign(51)= Light51
	Lampz.MassAssign(51)= Light51h
	Lampz.Callback(51) = "DisableLighting p51, 75,"
	Lampz.MassAssign(52)= Light52
	Lampz.MassAssign(52)= Light52h
	Lampz.Callback(52) = "DisableLighting p52, 150,"
	Lampz.MassAssign(53)= Light53
	Lampz.MassAssign(53)= Light53h
	Lampz.Callback(53) = "DisableLighting p53, 150,"
	Lampz.MassAssign(54)= Light54
	Lampz.MassAssign(54)= Light54h
	Lampz.Callback(54) = "DisableLighting p54, 150,"
	Lampz.MassAssign(55)= Light55
	Lampz.MassAssign(55)= Light55h
	Lampz.MassAssign(55)= Light55f
	Lampz.Callback(55) = "DisableLighting p55, 75,"
	Lampz.MassAssign(56)= Light56
	Lampz.MassAssign(56)= Light56h
	Lampz.MassAssign(56)= Light56f
	Lampz.Callback(56) = "DisableLighting p56, 75,"
	Lampz.MassAssign(57)= Light57
	Lampz.MassAssign(57)= Light57h
	Lampz.Callback(57) = "DisableLighting p57, 75,"
	Lampz.MassAssign(58)= Light58
	Lampz.MassAssign(58)= Light58h
	Lampz.Callback(58) = "DisableLighting p58, 75,"
	Lampz.MassAssign(59)= Light59
	Lampz.MassAssign(59)= Light59h
	Lampz.Callback(59) = "DisableLighting p59, 75,"
	Lampz.MassAssign(60)= Light60
	Lampz.MassAssign(60)= Light60h
	Lampz.Callback(60) = "DisableLighting p60, 150,"
	Lampz.MassAssign(61)= Light61
	Lampz.MassAssign(61)= Light61h
	Lampz.Callback(61) = "DisableLighting p61, 150,"
	Lampz.MassAssign(62)= Light62
	Lampz.MassAssign(62)= Light62h
	Lampz.Callback(62) = "DisableLighting p62, 50,"

	'Large center PF flasher
	Lampz.MassAssign(100)= flash1
	Lampz.MassAssign(100)= flash2
	Lampz.MassAssign(100)= flash3
	Lampz.MassAssign(100)= flash4
	Lampz.MassAssign(100)= flashh
	Lampz.Callback(100) = "DisableLighting flasher_on, 50,"

	'GI
	Lampz.obj(0) = ColtoArray(GI)
	Lampz.Callback(0) = "GIUpdates"
	Lampz.state(0) = 1

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback can be assigned multiple times per index
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

	Sub Class_Initialize
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

	Public Sub TurnOnStates	'If obj contains any light objects, set their states to 1 (Fading is our job!)
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

	Public Sub Init	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
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

	Public Sub Update2	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
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

	Public Sub Update	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
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


Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub


'GI related subs

dim giprevalvl
giprevalvl = 0

sub OnPrimsVisible(aValue)
	dim kk
	If aValue then
		For each kk in ON_Prims:kk.visible = 1:next
	Else
		For each kk in ON_Prims:kk.visible = 0:next
	end If
end Sub

sub OffPrimsVisible(aValue)
	dim kk
	If aValue then
		For each kk in OFF_Prims:kk.visible = 1:next
	Else
		For each kk in OFF_Prims:kk.visible = 0:next
	end If
end Sub


'GI callback

Sub GIUpdates(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	'debug.print "aLvl=" & aLvl & " giprevalvl=" & giprevalvl

	if aLvl = 0 then										'GI OFF, let's hide ON prims
		'debug.print "aLvl = 0. OnPrimsVisible False"
		OnPrimsVisible False
		If giprevalvl = 1 Then OffPrimsVisible true
	Elseif aLvl = 1 then									'GI ON, let's hide OFF prims
		'debug.print "aLvl = 1. OffPrimsVisible False"
		OffPrimsVisible False
		If giprevalvl = 0 Then OnPrimsVisible True
	Else
		if giprevalvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
			'debug.print "giprevalvl = 0. OnPrimsVisible True"
			OnPrimsVisible True
		elseif giprevalvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
			'debug.print "giprevalvl = 1. OffPrimsVisible true"
			OffPrimsVisible true
		Else
			'no change
		end if
	end if

	UpdateMaterial "mesh_on",	0.25,1,0.75,0.04705882,0,1, (aLvl)^1,   RGB(200,200,200),0,0,False,True,0,0,0,0	
	UpdateMaterial "Playfield",	0.50,1,0.75,0.04705882,0,1, (aLvl)^1,   RGB(200,200,200),0,0,False,True,0,0,0,0	
	'UpdateMaterial "mesh_off",	0.25,1,0.75,0.04705882,0,1, (1-aLvl)^1, RGB(200,200,200),0,0,False,True,0,0,0,0

	leftbat1.blenddisablelighting = aLvl*0.7+0.3
	leftbat.blenddisablelighting = aLvl*0.7+0.3
	rightbat.blenddisablelighting = aLvl*0.7+0.3
	render5off.blenddisablelighting = aLvl*0.5+0.5

	giprevalvl = alvl
End Sub


'******************************************************
'****  END LAMPZ
'******************************************************




'********************************************
' Hybrid code for VR, Cab, and Desktop
'********************************************

Dim VRThings

if VR_Room = 0 and cab_mode = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 1:Next
Elseif VR_Room = 0 and cab_mode = 1 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = WallClock:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
'Custom Walls, Floor, and Roof
	if CustomWalls = 1 Then
		VR_Wall_Left.image = "VR_Wall_Left"
		VR_Wall_Right.image = "VR_Wall_Right"
		VR_Floor.image = "VR_Floor"
		VR_Roof.image = "VR_Roof"
	end if

	If topper = 1 Then
		Primary_topper.visible = 1
	Else
		Primary_topper.visible = 0
	End If

	If poster = 1 Then
		VRposter.visible = 1
	Else
		VRposter.visible = 0
	End If

	If poster2 = 1 Then
		VRposter2.visible = 1
	Else
		VRposter2.visible = 0
	End If

End If


'********************************************
' VR Clock 
'********************************************

Dim CurrentMinute ' for VR clock

Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
	Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub



' ***************************************************************************
'                                  LAMP CALLBACK
' ****************************************************************************

if VR_Room = 1 Then
	Set LampCallback = GetRef("UpdateMultipleLamps")
End If

Sub UpdateMultipleLamps()
	If Controller.Lamp(1) = 0 Then: l1.visible  =0: else: l1.visible  =1 'Shoot Again
	If Controller.Lamp(2) = 0 Then: l2.visible  =0: else: l2.visible  =1 'Ball In Play
	If Controller.Lamp(3) = 0 Then: l3.visible  =0: else: l3.visible  =1 'Tilt
	If Controller.Lamp(4) = 0 Then: l4.visible  =0: else: l4.visible  =1 'Game Over
	If Controller.Lamp(5) = 0 Then: l5.visible  =0: else: l5.visible  =1 'Match
	If Controller.Lamp(6) = 0 Then: l6.visible  =0: else: l6.visible  =1 'High Score
	If Controller.Lamp(6) = 0 Then: l6a.visible =0: else: l6a.visible =1 'High Score
	If Controller.Lamp(8) = 0 Then: l8.visible  =0: else: l8.visible  =1 'Wave In Play
End Sub


if VR_Room = 0 and cab_mode = 0 Then
	Set LampCallback = GetRef("UpdateDTLamps")
End If


Sub UpdateDTLamps()
	If Controller.Lamp(1) = 0 Then: ShootAgainReel.setValue(0):		Else: ShootAgainReel.setValue(1) 'Shoot Again
	If Controller.Lamp(2) = 0 Then: BIPReel.setValue(0):			Else: BIPReel.setValue(1) 'Ball in Play
	If Controller.Lamp(3) = 0 Then: TiltReel.setValue(0):			Else: TiltReel.setValue(1) 'Tilt
	If Controller.Lamp(4) = 0 Then: GameOverReel.setValue(0):		Else: GameOverReel.setValue(1) 'Game Over
	If Controller.Lamp(5) = 0 Then: MatchReel.setValue(0):			Else: MatchReel.setValue(1) 'Match
	If Controller.Lamp(6) = 0 Then: HighScoreReel.setValue(0):		Else: HighScoreReel.setValue(1) 'High Score
	If Controller.Lamp(8) = 0 Then: WaveInPlayReel.setValue(0):		Else: WaveInPlayReel.setValue(1) 'Wave In Play 
End Sub



' *********************************************************************
' VR Alphanumeric Display
' *********************************************************************


dim DisplayColor
DisplayColor =  RGB(255,40,1)


Sub VRDisplaytimer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In VRDigits(num)
					If chg And 1 Then FadeDisplay obj, stat And 1	
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If
End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 800
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 250
	End If
End Sub

Dim VRDigits(32)

VRDigits(0)=Array(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15)
VRDigits(1)=Array(D16,D17,D18,D19,D20,D21,D22,D23,D24,D25,D26,D27,D28,D29,D30)
VRDigits(2)=Array(D31,D32,D33,D34,D35,D36,D37,D38,D39,D40,D41,D42,D43,D44,D45)
VRDigits(3)=Array(D46,D47,D48,D49,D50,D51,D52,D53,D54,D55,D56,D57,D58,D59,D60)
VRDigits(4)=Array(D61,D62,D63,D64,D65,D66,D67,D68,D69,D70,D71,D72,D73,D74,D75)
VRDigits(5)=Array(D76,D77,D78,D79,D80,D81,D82,D83,D84,D85,D86,D87,D88,D89,D90)
VRDigits(6)=Array(D91,D92,D93,D94,D95,D96,D97,D98,D99,D100,D101,D102,D103,D104,D105)

VRDigits(7)=Array(D106,D107,D108,D109,D110,D111,D112,D113,D114,D115,D116,D117,D118,D119,D120)
VRDigits(8)=Array(D121,D122,D123,D124,D125,D126,D127,D128,D129,D130,D131,D132,D133,D134,D135)
VRDigits(9)=Array(D136,D137,D138,D139,D140,D141,D142,D143,D144,D145,D146,D147,D148,D149,D150)
VRDigits(10)=Array(D151,D152,D153,D154,D155,D156,D157,D158,D159,D160,D161,D162,D163,D164,D165)
VRDigits(11)=Array(D166,D167,D168,D169,D170,D171,D172,D173,D174,D175,D176,D177,D178,D179,D180)
VRDigits(12)=Array(D181,D182,D183,D184,D185,D186,D187,D188,D189,D190,D191,D192,D193,D194,D195)
VRDigits(13)=Array(D196,D197,D198,D199,D200,D201,D202,D203,D204,D205,D206,D207,D208,D209,D210)

VRDigits(14)=Array(D211,D212,D213,D214,D215,D216,D217,D218)
VRDigits(15)=Array(D219,D220,D221,D222,D223,D224,D225,D226)
VRDigits(16)=Array(D227,D228,D229,D230,D231,D232,D233,D234)
VRDigits(17)=Array(D235,D236,D237,D238,D239,D240,D241,D242)
VRDigits(18)=Array(D243,D244,D245,D246,D247,D248,D249,D250)
VRDigits(19)=Array(D251,D252,D253,D254,D255,D256,D257,D258)
VRDigits(20)=Array(D259,D260,D261,D262,D263,D264,D265,D266)

VRDigits(21)=Array(D267,D268,D269,D270,D271,D272,D273,D274)
VRDigits(22)=Array(D275,D276,D277,D278,D279,D280,D281,D282)
VRDigits(23)=Array(D283,D284,D285,D286,D287,D288,D289,D290)
VRDigits(24)=Array(D291,D292,D293,D294,D295,D296,D297,D298)
VRDigits(25)=Array(D299,D300,D301,D302,D303,D304,D305,D306)
VRDigits(26)=Array(D307,D308,D309,D310,D311,D312,D313,D314)
VRDigits(27)=Array(D315,D316,D317,D318,D319,D320,D321,D322)

VRDigits(28)=Array(D323,D324,D325,D326,D327,D328,D329,D330)
VRDigits(29)=Array(D331,D332,D333,D334,D335,D336,D337,D338)
VRDigits(30)=Array(D339,D340,D341,D342,D343,D344,D345,D346)
VRDigits(31)=Array(D347,D348,D349,D350,D351,D352,D353,D354)


Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each obj in VRDigits(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

InitDigits



'**********************************************
'*******	Set Up Backglass Flashers	*******
'**********************************************
' this is for lining up the backglass flashers on top of a backglass image

Sub SetBackglass()
	Dim obj

'	For Each obj In BackglassLow
'		obj.x = obj.x
'		obj.height = - obj.y + -20
'		obj.y = -142 'adjusts the distance from the backglass towards the user
'	Next


	For Each obj In BackglassMid
		obj.x = obj.x
		obj.height = - obj.y + 1150
		obj.y = 80 'adjusts the distance from the backglass towards the user
	Next


'	For Each obj In BackglassHigh
'		obj.x = obj.x
'		obj.height = - obj.y + 40
'		obj.y = -190 'adjusts the distance from the backglass towards the user
'	Next

End Sub

'**********************************************



'**********************************************************************************************************
'* InstructionCard *
'**********************************************************************************************************

Dim CardCounter, ScoreCard
Sub CardTimer_Timer
        If scorecard=1 Then
                CardCounter=CardCounter+2
                If CardCounter>50 Then CardCounter=50
        Else
                CardCounter=CardCounter-4
                If CardCounter<0 Then CardCounter=0
        End If
        InstructionCard.transX = CardCounter*6
        InstructionCard.transY = CardCounter*6
        InstructionCard.transZ = -cardcounter*2
'        InstructionCard.objRotX = -cardcounter/2
        InstructionCard.size_x = 1+CardCounter/25
        InstructionCard.size_y = 1+CardCounter/25
        If CardCounter=0 Then 
                CardTimer.Enabled=False
                InstructionCard.visible=0
        Else
                InstructionCard.visible=1
        End If 
End Sub



' CHANGE LOG
' 001 - apophis - Added Fleep sound package
' 002 - apophis - Added nFozzy flippers and physics
' 003 - apophis - Added Roth drop targts for sw33,34,35,39,40. Implemented a fast flips workaround. Consolidated some timers into the GameTimer.
' 004 - apophis - Updated playfield image to an HD playfield scan. Realigned all table objects. Reformatted script. Updated ball rolling and drop target sounds.
' 005 - apophis - Fixed issue with right lane return. Updated playfield and plastic images with the scan provided by g94.
' 006 - apophis - Added Lampz and 3D inserts. Updated targetbouncer and rubberizer code.
' 007 - apophis - Adjusted some insert lighting. Adjusted slingshot strength.
' 008 - apophis - Updated PF and insert overlay images.
' 009 - apophis - Reworked insert lights and blooms...TBC
' 010 - apophis - Added physical trough and lock. No destroying the balls. Updated some DT textures.
' 011 - apophis - Reworked insert light objects. Changed rubberizer.
' 012 - apophis - Tuned kicker strength. Preliminary center flasher functionality. Updated drop target code to latest.
' 013 - apophis - Tweaked Wall5. Rotated rollovers 180 deg. 
' 014 - apophis - Added plunger lane ramps. Reduced kickback strength. Removed all getball calls. Tuned some inserts.
' 015 - bord - Turned all walls and rubbers to visible=0, removed existing meshes, moved GI beneath pf, small wall tuning to match meshes, added all meshes w/ rendered textures
' 016 - apophis - Created ON_Prim and OFF_Prim collections. Hooked up ON and OFF material fading to GI
' 017 - bord - bracketed gate & wire meshes added, center flasher mesh, flashers and texture, credit light, drop target shadows, new target meshes and textures
' 018 - apophis - Roth drop targets and standup targets added/updated. Wired center flasher to lampz and tuned. Updated rubberizer and flipper nudge.
' 019 - apophis - Dynamic shadows. Drop target shadows.
' 020 - apophis - Fixed issue where playfield not fading properly during GI state changes.
' 021 - UnclePaulie - Added hybrid mode. VR environments, posters, clock, and topper. Animated Backglass for VR and desktop. Changed drop targets to drop at playfield level. Modified groove to avoid stuck ball at diverter.
' 022 - apophis - Animated gatewire. Set plunger pull speed to 0.2. Tied some prim DL values to GI state. Added instruction card zoom feature (press R button). General file cleanup.
' RC1 - apophis - Added a few more ball images, and changed default one. Adjusted wall near upper flipper. Reduced kickback strength a little. Added fire button for Bombs. Updated TimerVRPlunger interval. Updated nudge strengths. Updated DT backdrop.
' RC2 - apophis - Updated opacity of VR alphanumeric display. Added flipper shadows. Updated POV. Tuned some insert halos.
' RC3 - apophis - Adjusted height of wires below flippers.
