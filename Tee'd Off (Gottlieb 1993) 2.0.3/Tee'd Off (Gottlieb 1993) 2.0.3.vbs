'*************************************
'Tee'd Off Premier (Gottlieb) 1993 - IPDB No. 2508
'VPX by rothbauerw
'Script by rothbauerw/jpsalas (VP9)
'Primitives by Dark
'VR by Uncle Paulie
'Physics, Sounds, and Dynamic Shadows updated by fluffhead35
'************************************
 
'******** Revisions done on Hybrid version 0.1 - 2.0 *********
' v0.1	UnclePaulie added hybrid mode, animated plunger and flippers, added VR clock.  Setup for animated backglass later.
' v0.2 Fluffhead35 - added Flipper Physics
' v0.2.1 - Updated ballmass 1.  Adding Rubber Collections.  Added Materials. Adding Rubbers.
' v0.2.2 - Adding collection and turnning off rubber collisions.
' v0.2.2.1 - Changed Back Ball Mass.  Rubber13 logic swapped out.  Playfield Settings fixed.  Changed Coil Rampup
' v0.2.2.2 - Changed Flippers Polariaty
' v0.2.2.3 - Change Plunger release speed from 180 to 120.  Added Rubber Hit Animation.  Locked All Rubers and Posts to table.  Rasied lower height of all rubber walls. 
' v0.2.2.4 - Changed Height of all dPosts and dSleeves Primitives
' v0.2.2.5 - Removed the old enabledFastFlips from the code as well as the leaf switches. Implemented new rubberizers
' v0.2.2.6 - Adding Fleep Sound
' v0.2.2.7 - rothbauerw - fixed customwall code, it was in two places, added thickness to the left sign above three targets, added blenddisable lighting to the apron lights, disabled lut changing with GI
'          - changed kicker code (kickers always enabled with hit height of 8), updated everything to use the new physics materials, adjusted game play difficulty to 56, slope to 6, and adjusted gopher wheel physics accordingly  
'		   - cleaned-up flipper physics and adjusted upper flipper based on and old version, updated kicker/saucer sounds to Fleep 
' v0.2.4   - fluffhead - Adding in dynamic shadow code
' v0.2.4.6 - fluffhead - adding wireramp_drop sounds
' v0.2.4.7 - UnclePaulie - cut holes in the playfield for rollover triggers, slings, targets 
' v0.2.4.8 - Moved VR Plunger code into the UpdateMechs to eliminate two timers, eliminated BIPL and used switch instead, eliminate VPMDMD in cab mode, changed rollover switches to pulseswitch (except sw33 and sw34).
' v2.0.1   - Added blend disable lighting to golf balls when backglass flashes, added VPMInit me, fixed roulette randomness, adjusted for nudge exploit 

Option Explicit
Randomize

'********************
'Options
'********************

'----- Desktop, VR, or Full Cabinet Options -----

const VR_Room = 0 					' 0 is desktop or cab mode, 1 is Tee'd Off VR Room, 2 is Minimal with Wall and Clock Options
const cab_mode = 0 					' 1 for cabinet mode to turn off VR Cabinet side rails, backbox, and lockbar 
const DMDColor = 1					' DMD color for VR and FSS: 1 for default color, 0 for green

' *** If using VR Room = 1:

const PlayThunder = 1				'1 to play thunder sounds, 0 to disable
const LowResGrass = 0				'If you see stutter, try changing this value to 1, it will reduce the resoltution of the grass texture

' *** If using VR Room = 2:

const CustomWalls = 1				'set 0 for Original Minimal Walls, floor, and roof, 1 for wood floor and home walls, 2 for quick golf course setting 
const WallClock = 1					'1 Shows the clock in the VR room only   

Dim VolumeDial, VolcanoLighting, BallRollAmpFactor, RampRollAmpFactor
Dim StagedFlipperMod

VolcanoLighting = 1					'"0" Turns off texture swaps for the volcano (will help performance on low end machines)
VolumeDial = 0.8					'Added Sound Volume Dial (ramps, balldrop, kickers, etc)
BallRollAmpFactor = 0       		' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)
RampRollAmpFactor = 2       		' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)
StagedFlipperMod = 0 				'0 = not staged, 1 - staged (dual leaf switches)

'----- Phsyics Mods -----
Const Rubberizer = 1		     	'1 - rothbauerw version, 2 - iaakki version
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's

'********************
'End Options
'********************

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim UseVPMDMD:If cab_mode <> 1 Then UseVPMDMD = True Else UseVPMDMD = False

If DesktopMode and VR_Room = 0 and Table1.ShowFSS = 0 Then Scoretext.visible = 1 else Scoretext.visible=0
If DMDColor = 1 Then DMD.Color = RGB(255,139,23)

' using table width and height in script slows down the performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "GTS3.VBS", 3.26

'********************
'Standard definitions
'********************
   
Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
 
'Standard Sounds
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

'******************************************************
' 					TABLE INIT
'******************************************************

Dim ii, collobj, GIObj(30), GIUpperObj(30), GICount, GIUpperCount
Dim ttGopherWheel, TOBall1, TOBall2, TOBall3, TOBall4
Const cGameName = "teedoff3"

Sub Table1_Init
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Tee'd Off (Gottlieb 1993)"
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

	vpmNudge.TiltSwitch = 151
	vpmNudge.Sensitivity = 3
	vpmNudge.TiltObj = Array(Bumper1, LeftSlingShot, RightSlingShot, TopSlingShot)
 
	'************  Trough	**************************
	Set TOBall1 = Slot1.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TOBall2 = Slot2.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TOBall3 = Slot3.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TOBall4 = Drain.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Controller.Switch(14) = 1
	Controller.Switch(24) = 1

	Set ttGopherWheel = New cvpmTurntable
		ttGopherWheel.InitTurntable ttGopherWheelTrig, 25 '20
		ttGopherWheel.spinup=100
		ttGopherWheel.spindown=100
		ttGopherWheel.CreateEvents "ttGopherWheel"

	'************  Misc Stuff  ******************
	arm3.isdropped=1
	arm4.isdropped=1
	arm5.isdropped=1

	F13c.intensityscale=0:F13d.intensityscale=0:F13e.intensityscale=0

	Init_Roulette

	If DesktopMode Then
		For each xx in GI_Upper:xx.y=xx.y+12:Next
		For each xx in Flashers_Upper:xx.y=xx.y+12:Next
	End If

	ii = 0
	For Each collObj in GI
		Set GIObj(ii) = collObj
		ii = ii + 1
	Next
	GICount = ii

	ii = 0
	For Each collObj in GI_Upper
		Set GIUpperObj(ii) = collObj
		ii = ii + 1
	Next
	GIUpperCount = ii

	TiltRelay 0
	GIRelay 0
End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_Exit:Controller.Stop:End Sub


'******************************************************
' 						KEYS
'******************************************************
Dim PlungerPress

If StagedFlipperMod = 1 Then	
	keyStagedFlipperR = KeyUpperRight
End If

Sub Table1_KeyDown(ByVal keycode)
	If keycode = StartGameKey Then Controller.Switch(4)= 1:soundStartButton()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

		End Select
	End If

	If KeyCode = LeftFlipperKey then
		FlipperActivate LeftFlipper, LFPress
	End If

	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then FlipperActivate2 RightFlipper2, RFPress1
	End If

	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperRight Then FlipperActivate2 RightFlipper2, RFPress1
	End If

	If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = 2112.186 + 8
	End If

	If keycode = RightFlipperKey Then 
		VRFlipperButtonRight.X = 2089.148 - 8
	End If


 	If keycode = Plungerkey then 
		plunger.PullBack
		SoundPlungerPull()
		Plungerpress = 1
		VR_Primary_plunger.Y = -26.875
	End If

'************************************************************************************************
'VR Test code - REMOVE **************************************************************************
'If keycode = RightMagnasave Then 
'	LightningOn
'	SolBG20 1 
'	vpmTimer.AddTimer 50, "SolBG20 0'"
'	vpmTimer.AddTimer 60, "SolBG21 1'"
'	vpmTimer.AddTimer 110, "SolBG21 0'"
'	vpmTimer.AddTimer 120, "SolBG22 1'"
'	vpmTimer.AddTimer 170, "SolBG22 0'"	
'End If

'If keycode = LeftMagnasave Then RandomSoundRampStop RightRampStop'LightningOff
'************************************************************************************************
'************************************************************************************************


	If vpmKeyDown(keycode) Then Exit Sub

'	if keycode=30 then controller.switch(5)=Not Controller.Switch(5) 'tournament switch "A"
'	if keycode=31 then controller.switch(6)=Not Controller.Switch(6) 'door switch "S"
'	if keycode=33 then EngageRoulette true  ' F test roulette wheel

End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = StartGameKey Then Controller.Switch(4) = 0

	If KeyCode = LeftFlipperKey then
		FlipperDeActivate LeftFlipper, LFPress
	End If

	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then FlipperDeActivate2 RightFlipper2, RFPress1
	End If

	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperRight Then FlipperDeActivate2 RightFlipper2, RFPress1
	End If

	If keycode = plungerkey then 
		plunger.Fire
		Plungerpress = 0

		If Controller.Switch(34) Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	If keycode = LeftFlipperKey Then 
		VRFlipperButtonLeft.X = 2112.186
	End If

	If keycode = RightFlipperKey Then 
		VRFlipperButtonRight.X = 2089.148
	End If

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

'******************************************************
'					SOLENOIDS
'******************************************************

'SolCallback(1) = "" 				           		'1 - Pop Bumper
'SolCallback(2) = ""                           		'2 - Left Sling
'SolCallback(3) = ""                           		'3 - Right Sling
'SolCallback(4) = ""								'4 - Top Kicking Rubber
SolCallback(5) = "MHGKicker"						'5 - Tope Hole
SolCallback(6) = "PlungerGate"						'6 - Plunger Gate
SolCallback(7) = "SolDrop"							'7 - 4 Bank Drop Target
SolCallback(8) = "TopUpKicker"						'8 - Top Up Kicker
SolCallback(9) = "CenterUpKicker"					'9 - Center Up Kicker	
SolCallback(10) = "RightUpKicker"					'10 - Bottom Up Kicker
SolCallback(11) = "Flasher11"						'11 - Left Flipper Flash
SolCallback(12) = "Flasher12"						'12	- Left Center Flash
SolCallback(13) = "Flasher13"						'13 - Top Left Flash
SolCallback(14) = "Flasher14"						'14 - Center Middle Flash
SolCallback(15) = "Flasher15"						'15 - Center Top Flash 	
SolCallback(16) = "Flasher16"						'16 - Top Right Flash
SolCallback(17) = "Flasher17"						'17 - Top Right Flash #2
SolCallback(18) = "Flasher18"						'18 - Right Center Flash
SolCallback(19) = "Flasher19"						'19 - Right Flipper Flash
SolCallback(20) = "SolBG20"								'20 - Light Box #1 Flash
SolCallback(21) = "SolBG21"								'21 - Light Box #2 Flash
SolCallback(22) = "SolBG22"								'22 - Light Box #3 Flash
'SolCallback(23) = ""								'23 - Light Box #4 Flash
'SolCallback(24) = ""								'24 - Light Box Gopher Motor Relay
SolCallback(25) = "EngageRoulette"					'25 - Playfield Gopher Wheel Relay
SolCallback(26) = "GIRelay"							'26 - LightBox GI Relay
SolCallback(28) = "ReleaseBall"						'28 - Ball Release
SolCallback(29) = "SolOuthole"						'29 - Outhole
SolCallback(30) = "SolKnocker"						'30 - Knocker
SolCallback(31) = "TiltRelay"						'31 - Tilt
SolCallback(32) = "vpmNudge.SolGameOn"				'32 - Game Over Relay

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"

'******************************************************
'			TROUGH BASED ON NFOZZY'S
'******************************************************

Sub Slot3_Hit():Controller.Switch(14) = 1:UpdateTrough:End Sub
Sub Slot3_UnHit():Controller.Switch(14) = 0:UpdateTrough:End Sub
Sub Slot2_UnHit():UpdateTrough:End Sub
Sub Slot1_UnHit():UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If Slot1.BallCntOver = 0 Then Slot2.kick 60, 9
	If Slot2.BallCntOver = 0 Then Slot3.kick 60, 9
	Me.Enabled = 0
End Sub

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	RandomSoundDrain drain
	UpdateTrough
	Controller.Switch(24) = 1
End Sub

Sub Drain_UnHit()
	Controller.Switch(24) = 0
End Sub

Sub SolOuthole(enabled)
	If enabled Then 
		Drain.kick 60,20
		'SoundSaucerKick 0, Drain
	End If
End Sub

Sub ReleaseBall(enabled)
	If enabled Then 
		RandomSoundBallRelease Slot1
		Slot1.kick 60, 7
		UpdateTrough
	End If
End Sub

'******************************************************
'					KNOCKER
'******************************************************
Sub SolKnocker(enabled)
	If enabled then 
		KnockerSolenoid
	End If
End Sub

'******************************************************
'				RIGHT UP KICKER (RUK)
'******************************************************

Sub RightUpKicker(enabled)
	If enabled Then
		If Controller.Switch(36) = True Then
			SoundSaucerKick 1, RUKa
		Else
			SoundSaucerKick 0, RUKa
		End If
		RUKa.kick 0,45,1.56
		Controller.Switch(36) = 0
		arm4.isdropped=0
		vpmTimer.AddTimer 150, "arm4.isdropped=1'"
	End If
End Sub

sub RUKa_hit
	SoundSaucerLock
	'PlaySound "kicker_enter_center", 0, 2*VolumeDial
	Controller.Switch(36) = 1
End sub

'******************************************************
'				Mean Hole Green (MHG)
'******************************************************

Sub MHGKicker(enabled)
	If enabled Then
		If Controller.Switch(23) = True Then
			SoundSaucerKick 1, MHG
		Else
			SoundSaucerKick 0, MHG
		End If
		MHG.kickZ 145,25,0,10
		Controller.Switch(23) = 0
		twkicker1.Z = 78
		'MHG.enabled = false
		vpmTimer.AddTimer 150, "twkicker1.Z = 58'"
	End If
End Sub

Sub MHG_hit()
	SoundSaucerLock
	'PlaySound "metalhit2", 0, 2*VolumeDial
	Controller.Switch(23) = 1
End sub


'******************************************************
'				Center Up Kicker (CUK)
'******************************************************

Sub CenterUpKicker(enabled)
	If enabled Then
		If Controller.Switch(26) = True Then
			SoundSaucerKick 1, CUK
		Else
			SoundSaucerKick 0, CUK
		End If
		CUK.kick 0,35,1.56
		Controller.Switch(26) = 0
		SoundSaucerKick 0, CUK		
		arm5.isdropped=0
		vpmTimer.AddTimer 150, "arm5.isdropped=1'"
	End If
End Sub

Sub CUK_hit()
	SoundSaucerLock
	Controller.Switch(26) = 1
End sub

'******************************************************
'				TOP UP KICKER (TUK)
'******************************************************

Sub TopUpKicker(enabled)
	If enabled Then
		If Controller.Switch(16) = True Then
			SoundSaucerKick 1, TUKa
		Else
			SoundSaucerKick 0, TUKa
		End If
		TUKa.kick 0,60,1.56
		Controller.Switch(16) = 0
		arm3.isdropped=0
		vpmTimer.AddTimer 150, "arm3.isdropped=1'"
	End If
End Sub

sub TUKa_hit
	SoundSaucerLock
	Controller.Switch(16) = 1
End sub

'******************************************************
'					Plunger Gate
'******************************************************

Sub PlungerGate(enabled)
	If enabled Then
		BallLockPrim.Z = 26
		BallLockWall.collidable = 0
		'Rubber13.collidable = 0 
		zCol_Rubber_Post_008.collidable = 0
		SoundSaucerKick 0, BallLockPrim		
	Else
		vpmTimer.AddTimer 175, "RaiseGate'" 
	End If
End Sub

Sub RaiseGate()
	BallLockPrim.Z = 70
	BallLockWall.collidable = 1
	'Rubber13.collidable = 1
	zCol_Rubber_Post_008.collidable = 1
	SoundSaucerKick 0, BallLockPrim
End Sub

'******************************************************
'				FLIPPERS
'******************************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
    If Enabled Then
        LF.Fire 'LeftFlipper.RotateToEnd
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
        RF.Fire 'RightFlipper.RotateToEnd
		'RightFlipper2.RotateToEnd
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If	
    Else
        RightFlipper.RotateToStart
		'RightFlipper2.RotateToStart	
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		RightFlipper2.RotateToEnd
		If RightFlipper2.currentangle > RightFlipper2.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper2
		Else 
			SoundFlipperUpAttackRight RightFlipper2
			RandomSoundFlipperUpRight RightFlipper2
		End If	
    Else
		RightFlipper2.RotateToStart
		If RightFlipper2.currentangle > RightFlipper2.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper2
		End If
	End If
End Sub

'******************************************************
'					BUMPERS
'******************************************************

Sub Bumper1_Hit()
	RandomSoundBumperTop Bumper1
	vpmTimer.PulseSw 10
End Sub

'******************************************************
'				SLINGSHOTS
'******************************************************

Dim RStep, Lstep, Tstep

Sub LeftSlingShot_Slingshot
	RandomSoundSlingshotLeft sling1
	vpmTimer.PulseSw 11
    LeftSling.Visible = 0
    LeftSling1.Visible = 1
    sling1.TransZ = -20
    LStep = 0
    Me.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LeftSLing1.Visible = 0:LeftSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:LeftSLing2.Visible = 0:LeftSLing.Visible = 1:sling1.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RandomSoundSlingshotRight sling2
	vpmTimer.PulseSw 12
    RightSling.Visible = 0
    RightSling1.Visible = 10
    sling2.TransZ = -20
    RStep = 0
    Me.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RightSLing1.Visible = 0:RightSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:RightSLing2.Visible = 0:RightSLing.Visible = 1:sling2.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub TopSlingShot_Slingshot
	RandomSoundSlingshotTop sling3
	vpmTimer.PulseSw 13
    TopSling.Visible = 0
    TopSling1.Visible = 10
    sling3.TransZ = -20
    TStep = 0
    Me.TimerEnabled = 1
End Sub

Sub TopSlingShot_Timer
    Select Case TStep
        Case 3:TopSLing1.Visible = 0:TopSLing2.Visible = 1:sling3.TransZ = -10
        Case 4:TopSLing2.Visible = 0:TopSLing.Visible = 1:sling3.TransZ = 0:Me.TimerEnabled = 0
    End Select
    TStep = TStep + 1
End Sub

'******************************************************
'				Animated Rubbers
'******************************************************

Sub Wall_RB009_hit()
	If InRect(ActiveBall.x,ActiveBall.y,176,56,221,56,221,208,176,208) Then
		RubberMHG.visible=0:RubberMHG1.visible=1
		vpmTimer.AddTimer 100, "RubberMHG.visible=1:RubberMHG1.visible=0'"
	End If
End Sub

Sub Wall_RB013_hit()
	If InRect(ActiveBall.x,ActiveBall.y,296,233,346,207,366,250,315,273) Then
		RubberMHGLU.visible=0:RubberMHGLU1.visible=1
		vpmTimer.AddTimer 100, "RubberMHGLU.visible=1:RubberMHGLU1.visible=0'"
	End If
End Sub

Sub Wall_RB007_hit()
	If InRect(ActiveBall.x,ActiveBall.y,318,321,369,298,396,368,347,387) Then
		RubberMHGLL.visible=0:RubberMHGLL1.visible=1
		vpmTimer.AddTimer 100, "RubberMHGLL.visible=1:RubberMHGLL1.visible=0'"
	End If
End Sub

Sub Wall_RB010_hit()
	If InRect(ActiveBall.x,ActiveBall.y,258,479,314,475,326,673,272,679) Then
		RubberMHGPNP.visible=0:RubberMHGPNP1.visible=1
		vpmTimer.AddTimer 100, "RubberMHGPNP.visible=1:RubberMHGPNP1.visible=0'"
	End If
End Sub

Sub Wall_RB006_hit()
	If InRect(ActiveBall.x,ActiveBall.y,472,883,514,857,537,896,495,921) Then
		RubberDT.visible=0:RubberDT1.visible=1
		vpmTimer.AddTimer 100, "RubberDT.visible=1:RubberDT1.visible=0'"
	End If
End Sub

Sub Wall_RB005_hit()
	If InRect(ActiveBall.x,ActiveBall.y,600,559,642,570,625,636,581,623) Then
		RubberCapture.visible=0:RubberCapture1.visible=1
		vpmTimer.AddTimer 100, "RubberCapture.visible=1:RubberCapture1.visible=0'"
	End If
End Sub

'******************************************************
'				SWITCHES
'******************************************************

'*******	Targets		******************
Sub dt1_Hit:DTHit 7:End Sub
Sub dt2_Hit:DTHit 27:End Sub
Sub dt3_Hit:DTHit 37:End Sub
Sub dt4_Hit:DTHit 17:End Sub

'*******	Targets		******************
Sub sw91_Hit:STHit 91:End Sub
Sub sw101_Hit:STHit 101:End Sub
Sub sw111_Hit:STHit 111:End Sub

Sub sw92_Hit:STHit 92:End Sub
Sub sw102_Hit:STHit 102:End Sub
Sub sw112_Hit:STHit 112:End Sub

Sub sw83_Hit:STHit 83:End Sub
Sub sw93_Hit:vpmTimer.PulseSw 93:End Sub
Sub sw103_Hit:STHit 103:End Sub

Sub sw21_Hit:vpmTimer.PulseSw 21:End Sub
Sub sw22_Hit:vpmTimer.PulseSw 22:End Sub

Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub

'*******	Opto Switches		******************
Sub sw80_Hit:Controller.Switch(80) = 1:End Sub
Sub sw80_UnHit:Controller.Switch(80) = 0:End Sub

Sub sw90_Hit:Controller.Switch(90) = 1:End Sub
Sub sw90_UnHit:Controller.Switch(90) = 0:End Sub


'*******	Rollover Switches	******************

Sub sw33_Hit:Controller.Switch(33) = 1:End Sub
Sub sw33_UnHit
	Dim In1, In2, In3, In4

	In1 = inRect(TOBall1.x, TOBall1.y, 374.14, 573, 434.5, 585.5, 421.8, 625, 361, 625)
	In2 = inRect(TOBall2.x, TOBall2.y, 374.14, 573, 434.5, 585.5, 421.8, 625, 361, 625)
	In3 = inRect(TOBall3.x, TOBall3.y, 374.14, 573, 434.5, 585.5, 421.8, 625, 361, 625)
	In4 = inRect(TOBall4.x, TOBall4.y, 374.14, 573, 434.5, 585.5, 421.8, 625, 361, 625)

	if Not In1 and not In2 and not In3 and not In4 Then Controller.Switch(33) = 0
End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub
Sub sw84_Hit:vpmTimer.PulseSw 84:End Sub
Sub sw94_Hit:vpmTimer.PulseSw 94:End Sub
Sub sw104_Hit:vpmTimer.PulseSw 104:End Sub
Sub sw113_Hit:vpmTimer.PulseSw 113:End Sub
Sub sw114_Hit:vpmTimer.PulseSw 114:End Sub

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
Dim DT7, DT27, DT37, DT17

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
'       isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

' Center Bank
Set DT7 = (new DropTarget)(dt1, dt1a, pdt1, 7, 0, false)
Set DT27 = (new DropTarget)(dt2, dt2a, pdt2, 27, 0, false)
Set DT37 = (new DropTarget)(dt3, dt3a, pdt3, 37, 0, false)
Set DT17 = (new DropTarget)(dt4, dt4a, pdt4, 17, 0, false)

'Add all the Drop Target Arrays to Drop Target Animation Array
' DTAnimationArray = Array(DT1, DT2, ....)
Dim DTArray
DTArray = Array(DT7, DT27, DT37, DT17)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 			'in milliseconds
Const DTDropUpSpeed = 40 			'in milliseconds
Const DTDropUnits = 49 			'VP units primitive drops
Const DTDropUpUnits = 5 			'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 				'max degrees primitive rotates when hit
Const DTDropDelay = 20	 		'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40	 		'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30				'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0			'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "targethit"	'Drop Target Hit sound
Const DTDropSound = "DTDrop"		'Drop Target Drop sound
Const DTResetSound = "DTReset"	'Drop Target reset sound

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
			prim.blenddisablelighting = 0.2
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
			prim.blenddisablelighting = 0.35
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
'					DROP TARGETS
'******************************************************
 
Sub SolDrop(enabled)
	If enabled Then
		RandomSoundDropTargetReset pdt4
		DTRaise 7
		DTRaise 27
		DTRaise 37
		DTRaise 17
	End if
End Sub

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
Dim ST83, ST91, ST92, ST101, ST102, ST103, ST111, ST112

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0

Set ST83 = (new StandupTarget)(sw83, psw83,83, 0)
Set ST91 = (new StandupTarget)(sw91, psw91,91, 0)
Set ST92 = (new StandupTarget)(sw92, psw92,92, 0)
Set ST101 = (new StandupTarget)(sw101, psw101,101, 0)
Set ST102 = (new StandupTarget)(sw102, psw102,102, 0)
Set ST103 = (new StandupTarget)(sw103, psw103,103, 0)
Set ST111 = (new StandupTarget)(sw111, psw111,111, 0)
Set ST112 = (new StandupTarget)(sw112, psw112,112, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST83, ST91, ST92, ST101, ST102, ST103, ST111, ST112)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit
Const STHitSound = "targethit"	'Stand-up Target Hit sound

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
'				Lights & Flashers
'******************************************************

'******************************************************
'****  LAMPZ
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
		next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
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


'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity	
End Sub

Sub DisableLighting2(pri, DLintensity, DLOff,ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	If aLvl * DLintensity > DLOff then 
		pri.blenddisablelighting = aLvl * DLintensity
	Else
		pri.blenddisablelighting = DLOff '10
	End If
End Sub

Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/7.5 : next 'FadeSpeedDown Default = 1/10

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(0)= l0			'Golf Again
	Lampz.Callback(0) = "DisableLighting2 p0, 400, 10,"

	'* Skins!
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(7)= l7
	Lampz.Callback(2) = "DisableLighting BulbRed2, 0.65," '0.75
	Lampz.Callback(3) = "DisableLighting BulbRed3, 0.65,"
	Lampz.Callback(4) = "DisableLighting BulbRed4, 0.65,"
	Lampz.Callback(5) = "DisableLighting BulbRed5, 0.65,"
	Lampz.Callback(6) = "DisableLighting BulbRed6, 0.65,"
	Lampz.Callback(7) = "DisableLighting BulbRed7, 0.65,"

	Lampz.MassAssign(10)= l10
	Lampz.MassAssign(11)= l11
	Lampz.Callback(10) = "DisableLighting2 p10, 200, 10,"
	Lampz.Callback(11) = "DisableLighting2 p11, 200, 10,"

	'* GOPHER
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(17)= l17
	Lampz.Callback(12) = "DisableLighting BulbGreen12, 0.65,"
	Lampz.Callback(13) = "DisableLighting BulbGreen13, 0.65,"
	Lampz.Callback(14) = "DisableLighting BulbGreen14, 0.65,"
	Lampz.Callback(15) = "DisableLighting BulbGreen15, 0.65,"
	Lampz.Callback(16) = "DisableLighting BulbGreen16, 0.65,"
	Lampz.Callback(17) = "DisableLighting BulbGreen17, 0.65,"


	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(27)= l27
	Lampz.Callback(22) = "DisableLighting2 p22, 200, 10,"
	Lampz.Callback(24) = "DisableLighting2 p24, 200, 10,"
	Lampz.Callback(25) = "DisableLighting2 p25, 200, 10,"
	Lampz.Callback(26) = "DisableLighting2 p26, 200, 10,"
	Lampz.Callback(27) = "DisableLighting2 p27, 200, 10,"


	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(37)= l37
	Lampz.Callback(31) = "DisableLighting2 p31, 200, 10,"
	Lampz.Callback(32) = "DisableLighting2 p32, 200, 10,"
	Lampz.Callback(33) = "DisableLighting2 p33, 200, 10,"
	Lampz.Callback(35) = "DisableLighting2 p35, 200, 10,"
	Lampz.Callback(36) = "DisableLighting2 p36, 200, 10,"
	Lampz.Callback(37) = "DisableLighting2 p37, 200, 10,"

	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(47)= l47
	Lampz.Callback(40) = "DisableLighting2 p40, 200, 10,"
	Lampz.Callback(41) = "DisableLighting2 p41, 200, 10,"
	Lampz.Callback(42) = "DisableLighting2 p42, 200, 10,"
	Lampz.Callback(43) = "DisableLighting2 p43, 200, 10,"
	Lampz.Callback(44) = "DisableLighting2 p44, 200, 10,"
	Lampz.Callback(45) = "DisableLighting2 p45, 200, 10,"
	Lampz.Callback(46) = "DisableLighting2 p46, 200, 10,"

	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(57)= l57
	Lampz.Callback(51) = "DisableLighting2 p51, 200, 10,"
	Lampz.Callback(52) = "DisableLighting2 p52, 200, 10,"
	Lampz.Callback(53) = "DisableLighting2 p53, 200, 10,"
	Lampz.Callback(54) = "DisableLighting2 p54, 200, 10,"
	Lampz.Callback(55) = "DisableLighting2 p55, 200, 10,"
	Lampz.Callback(56) = "DisableLighting2 p56, 200, 10,"
	Lampz.Callback(57) = "DisableLighting2 p57, 200, 10,"

	Lampz.MassAssign(60)= l60
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(64)= l64
	Lampz.MassAssign(65)= l65
	Lampz.MassAssign(66)= l66
	Lampz.MassAssign(67)= l67
	Lampz.Callback(61) = "DisableLighting2 p61, 200, 10,"
	Lampz.Callback(62) = "DisableLighting2 p62, 200, 10,"
	Lampz.Callback(63) = "DisableLighting2 p63, 200, 10,"
	Lampz.Callback(64) = "DisableLighting2 p64, 200, 10,"
	Lampz.Callback(65) = "DisableLighting2 p65, 200, 10,"
	Lampz.Callback(66) = "DisableLighting2 p66, 200, 10,"
	Lampz.Callback(67) = "DisableLighting2 p67, 200, 10,"


	Lampz.MassAssign(70)= l70
	Lampz.MassAssign(71)= l71
	Lampz.MassAssign(72)= l72
	Lampz.MassAssign(73)= l73
	Lampz.MassAssign(74)= l74
	Lampz.MassAssign(75)= l75
	Lampz.MassAssign(76)= l76
	Lampz.MassAssign(77)= l77
	Lampz.Callback(70) = "DisableLighting2 p70, 200, 10,"
	Lampz.Callback(71) = "DisableLighting2 p71, 200, 10,"
	Lampz.Callback(73) = "DisableLighting2 p73, 100, 10,"
	Lampz.Callback(74) = "DisableLighting2 p74, 100, 10,"
	Lampz.Callback(75) = "DisableLighting2 p75, 200, 10,"
	Lampz.Callback(76) = "DisableLighting2 p76, 200, 10,"
	Lampz.Callback(77) = "DisableLighting2 p77, 200, 10,"

	Lampz.MassAssign(80)= l80
	Lampz.MassAssign(81)= l81
	Lampz.MassAssign(82)= l82
	Lampz.MassAssign(83)= l83
	Lampz.MassAssign(84)= l84
	Lampz.MassAssign(85)= l85
	Lampz.MassAssign(86)= l86
	Lampz.MassAssign(87)= l87
	Lampz.Callback(81) = "DisableLighting2 p81, 200, 10,"
	Lampz.Callback(82) = "DisableLighting2 p82, 200, 10,"
	Lampz.Callback(84) = "DisableLighting2 p84, 200, 10,"
	Lampz.Callback(86) = "DisableLighting2 p86, 200, 10,"
	Lampz.Callback(87) = "DisableLighting2 p87, 300, 10,"

	'* Gopher Wheel Illumination
	Lampz.MassAssign(90)= l90
	Lampz.MassAssign(91)= l91
	Lampz.MassAssign(92)= l92
	Lampz.MassAssign(93)= l93

	Lampz.MassAssign(90)= l90b
	Lampz.MassAssign(91)= l91b
	Lampz.MassAssign(92)= l92b
	Lampz.MassAssign(93)= l93b

	Lampz.MassAssign(90)= l90c
	Lampz.MassAssign(91)= l91c
	Lampz.MassAssign(92)= l92c
	Lampz.MassAssign(93)= l93c

	'* Tee Box - Golf Balls
	Lampz.MassAssign(100)= l100
	Lampz.MassAssign(101)= l101
	Lampz.MassAssign(102)= l102
	Lampz.MassAssign(103)= l103
	Lampz.MassAssign(104)= l104
	Lampz.Callback(100) = "DisableLighting2 GolfBall1, 0.25, .10," 
	Lampz.Callback(101) = "DisableLighting2 GolfBall2, 0.25, .10,"
	Lampz.Callback(102) = "DisableLighting2 GolfBall3, 0.25, .10,"
	Lampz.Callback(103) = "DisableLighting2 GolfBall4, 0.25, .10,"
	Lampz.Callback(104) = "DisableLighting2 GolfBall5, 0.25, .10,"

	If VR_Room = 1 Then
		Lampz.Callback(100) = "DisableLighting VR_GolfBall1, 0.5," 
		Lampz.Callback(101) = "DisableLighting VR_GolfBall2, 0.5,"
		Lampz.Callback(102) = "DisableLighting VR_GolfBall3, 0.5,"
		Lampz.Callback(103) = "DisableLighting VR_GolfBall4, 0.5,"
		Lampz.Callback(104) = "DisableLighting VR_GolfBall5, 0.5,"
	End If

	'Flasher Assignments
'	Lampz.Callback(31)= "Flash1"
'	Lampz.Callback(32)= "Flash2"
'	Lampz.Callback(33)= "Flash3"
'	Lampz.Callback(34)= "Flash4"


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


'*  Flashers

Sub Flasher11(enabled)
	F11.state = enabled
	F11b.state = enabled
	Sound_Flash_Relay enabled, Relay_11
End Sub
Sub Flasher12(enabled)
	F12.state = enabled
	F12b.state = enabled
	Sound_Flash_Relay enabled, Relay_12
End Sub
Sub Flasher13(enabled)
	F13.state = enabled
	F13b.state = enabled
	If enabled then
		F13On = 1
		F13.timerenabled = true
	Else
		F13On = -1
		F13.timerenabled = true
	End If

	Sound_Flash_Relay enabled, Relay_13

End Sub
Sub Flasher14(enabled)
	F14.state = enabled
	F14b.state = enabled
	Sound_Flash_Relay enabled, Relay_14
End Sub 
Sub Flasher15(enabled)
	F15.state = enabled
	F15b.state = enabled
	Sound_Flash_Relay enabled, Relay_15
End Sub 
Sub Flasher16(enabled):F16.state = enabled:F16b.state = enabled:
	If enabled then
		F16On = 1
		F16.timerenabled = true
	Else
		F16On = -1
		F16.timerenabled = true
	End If
	Sound_Flash_Relay enabled, Relay_16
End Sub
Sub Flasher17(enabled)
	F17.state = enabled:F17b.state = enabled:
	Sound_Flash_Relay enabled, Relay_17
End Sub
Sub Flasher18(enabled)
	F18.state = enabled:F18b.state = enabled
	Sound_Flash_Relay enabled, Relay_18
End Sub
Sub Flasher19(enabled)
	F19.state = enabled:F19b.state = enabled
	Sound_Flash_Relay enabled, Relay_19
End Sub


Dim F13On, F13Count
F13Count = 0

Sub F13_Timer()
	F13Count = F13Count + F13On
	If F13Count < 0 Then:F13Count = 0
	If F13Count > 2 Then:F13Count = 2
	
	If VolcanoLighting Then
		Select Case F13Count
			Case 0: Primitive_Volcano.image="VolcanoMap_GION5"
			'Case 1: Primitive_Volcano.image="VolcanoMap_FLSHON0007"
			Case 2: Primitive_Volcano.image="VolcanoMap_FLSHON0010"
		End Select
	End If

	Select Case F13Count
		Case 0: Prim_Dome1.image = "dome3_clear_bright_off":F13c.intensityscale=0:F13d.intensityscale=0:F13e.intensityscale=0:Prim_Dome1.blenddisablelighting=0
		Case 1: Prim_Dome1.image = "dome3_clear_bright":F13c.intensityscale=1:F13d.intensityscale=1:F13e.intensityscale=0.5:Prim_Dome1.blenddisablelighting=1.25
		Case 2: Prim_Dome1.image = "dome3_clear_bright_on":F13c.intensityscale=2:F13d.intensityscale=2:F13e.intensityscale=1:Prim_Dome1.blenddisablelighting=2.5
	End Select


	If F13Count = 0 And F13On = -1 Then:F13.timerenabled = false 
	If F13Count = 2 And F13On = 1 Then:F13.timerenabled = false
End Sub


Dim F16On, F16Count
F16Count = 0

Sub F16_Timer()
	F16Count = F16Count + F16On
	If F16Count < 0 Then:F16Count = 0
	If F16Count > 2 Then:F16Count = 2
	
	Select Case F16Count
		Case 0: Prim_Dome2.image = "dome3_clear_bright_off":Prim_Dome2.blenddisablelighting=0
		Case 1: Prim_Dome2.image = "dome3_clear_bright":Prim_Dome2.blenddisablelighting=1.25
		Case 2: Prim_Dome2.image = "dome3_clear_bright_on":Prim_Dome2.blenddisablelighting=2.5
	End Select

	If F16Count = 0 And F16On = -1 Then:F16.timerenabled = false 
	If F16Count = 2 And F16On = 1 Then:F16.timerenabled = false
End Sub


Sub SolBG20(Enabled)
	If enabled then
		VR_s20.visible = true
		VR_s20.blenddisablelighting = 3
		If VR_Room = 1 and VR_Lightning20.timerenabled=false Then 
			VR_Lightning20.visible = 1
			VR_Lightning20.timerenabled = true
            LightningOn ' For VR Lightning Effect Sub at bottom of script
			If PlayThunder = 1 Then 
				StopSound "thunder1"
				select case Int(rnd*3):
					Case 0:	PlaySoundAt "thunder1", VR_Lightning20
					Case 1:	PlaySoundAt "thunder4", VR_Lightning20
					Case 2:	PlaySoundAt "thunder7", VR_Lightning20
				End Select
			End If
		End If
	Else	
		VR_s20.visible = False
		If VR_Room = 1 Then VR_Lightning20.visible = 0 : LightningOff ' LightningOff For VR Lightning Effect Sub at bottom of script
	End If
	Sound_Flash_Relay enabled, Relay_Backglass_Left
End Sub

Sub SolBG21(Enabled)
	If enabled then
		VR_s21.visible = true
		VR_s21.blenddisablelighting = 3
		If VR_Room = 1 and VR_Lightning21.timerenabled=false Then 
			VR_Lightning21.visible = 1
			VR_Lightning21.timerenabled = true
			LightningOn ' For VR Lightning Effect Sub at bottom of script
			If PlayThunder = 1 Then
				StopSound "thunder2"
				select case Int(rnd*2):
					Case 0:	PlaySoundAt "thunder2", VR_Lightning21
					Case 1:	PlaySoundAt "thunder5", VR_Lightning21					
				End Select
			End If
		End If
	Else	
		VR_s21.visible = False
		If VR_Room = 1 Then VR_Lightning21.visible = 0 : LightningOff ' LightningOff For VR Lightning Effect Sub at bottom of script
	End If
	Sound_Flash_Relay enabled, Relay_Backglass_Center
End Sub

Sub SolBG22(Enabled)
	If enabled then
		VR_s22.visible = true
		VR_s22.blenddisablelighting = 3
		If VR_Room = 1 and VR_Lightning22.timerenabled=false Then
			VR_Lightning22.visible = 1
			VR_Lightning22.timerenabled = true
			LightningOn ' For VR Lightning Effect Sub at bottom of script
			If PlayThunder = 1 Then
				StopSound "thunder3"
				select case Int(rnd*2):
					Case 0:	PlaySoundAt "thunder3", VR_Lightning22
					Case 1:	PlaySoundAt "thunder6", VR_Lightning22					
				End Select
			End If
		End If		
	Else	
		VR_s22.visible = False
		If VR_Room = 1 Then VR_Lightning22.visible = 0:  LightningOff ' LightningOff For VR Lightning Effect Sub at bottom of script
	End If
	Sound_Flash_Relay enabled, Relay_Backglass_Right
End Sub

Sub	VR_Lightning20_Timer()
	me.timerinterval = 3300 + rnd*1300 
	me.timerenabled=false
End Sub

Sub	VR_Lightning21_Timer()
	me.timerinterval = 3300 + rnd*1300
	me.timerenabled=false
End Sub

Sub	VR_Lightning22_Timer()
	me.timerinterval = 3300 + rnd*1300
	me.timerenabled=false
End Sub


'******************************************************
' 			GENERAL ILLUMINATION
'******************************************************

Dim xx

Sub GIRelay(enabled)
	If Enabled Then
		BackglassOff
		LeftTargetDecal.blenddisablelighting = 0.1
		CUKDecal.blenddisablelighting = 0.1
		BackWallDecal.blenddisablelighting = 0
		Primitive_Roundsbox.blenddisablelighting = 0.1
		Primitive_PitchNPutt.blenddisablelighting = 0.1
		Primitive_PlasticRampDecal1.blenddisablelighting = 0
		Primitive_PlasticRampDecal2.blenddisablelighting = 0
		If GolfBall1.blenddisablelighting = 0.1 then GolfBall1.blenddisablelighting = 0
		If GolfBall2.blenddisablelighting = 0.1 then GolfBall2.blenddisablelighting = 0
		If GolfBall3.blenddisablelighting = 0.1 then GolfBall3.blenddisablelighting = 0
		If GolfBall4.blenddisablelighting = 0.1 then GolfBall4.blenddisablelighting = 0
		If GolfBall5.blenddisablelighting = 0.1 then GolfBall5.blenddisablelighting = 0
	Else
		BackglassOn
		LeftTargetDecal.blenddisablelighting = 0.3
		CUKDecal.blenddisablelighting = 0.3
		BackWallDecal.blenddisablelighting = 0.2
		Primitive_Roundsbox.blenddisablelighting = 0.5
		Primitive_PitchNPutt.blenddisablelighting = 0.3
		Primitive_PlasticRampDecal1.blenddisablelighting = 0.2
		Primitive_PlasticRampDecal2.blenddisablelighting = 0.2
		If GolfBall1.blenddisablelighting = 0 then GolfBall1.blenddisablelighting = 0.1
		If GolfBall2.blenddisablelighting = 0 then GolfBall2.blenddisablelighting = 0.1
		If GolfBall3.blenddisablelighting = 0 then GolfBall3.blenddisablelighting = 0.1
		If GolfBall4.blenddisablelighting = 0 then GolfBall4.blenddisablelighting = 0.1
		If GolfBall5.blenddisablelighting = 0 then GolfBall5.blenddisablelighting = 0.1


	End If
	Sound_GI_Relay enabled, Relay_Backglass_Center
End Sub

Sub TiltRelay(enabled)
	Dim i

	If Enabled Then
		for i = 0 to GICount-1:GIObj(i).state=0:Next
		for i = 0 to GIUpperCount-1:GIUpperObj(i).state=0:Next
 		'for each xx in GI:xx.State=0:Next
		'for each xx in GI_Upper:xx.State=0:Next
		numberofsources = 0
 		'Table1.ColorGradeImage = "ColorGrade_8"
		BackglassOff
		LeftTargetDecal.blenddisablelighting = .1
		CUKDecal.blenddisablelighting = 0.1
		BackWallDecal.blenddisablelighting = 0
		Primitive_Roundsbox.blenddisablelighting = .1
		Primitive_PitchNPutt.blenddisablelighting = .1
		Primitive_PlasticRampDecal1.blenddisablelighting = 0
		Primitive_PlasticRampDecal2.blenddisablelighting = 0
		If GolfBall1.blenddisablelighting = 0.1 then GolfBall1.blenddisablelighting = 0
		If GolfBall2.blenddisablelighting = 0.1 then GolfBall2.blenddisablelighting = 0
		If GolfBall3.blenddisablelighting = 0.1 then GolfBall3.blenddisablelighting = 0
		If GolfBall4.blenddisablelighting = 0.1 then GolfBall4.blenddisablelighting = 0
		If GolfBall5.blenddisablelighting = 0.1 then GolfBall5.blenddisablelighting = 0

		If VolcanoLighting Then
			vpmTimer.AddTimer 10, "Primitive_Volcano.image=""VolcanoMap_GION3""'"
			vpmTimer.AddTimer 20, "Primitive_Volcano.image=""VolcanoMap_GION1""'"
			vpmTimer.AddTimer 30, "Primitive_Volcano.image=""VolcanoMap_OFF""'"
		End If
	Else
		for i = 0 to GICount-1:GIObj(i).state=1:Next
		for i = 0 to GIUpperCount-1:GIUpperObj(i).state=1:Next
 		'for each xx in GI:xx.State=1:Next
		'for each xx in GI_Upper:xx.State=1:Next
		numberofsources = numberofsources_hold
 		'Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
		BackglassOn
		LeftTargetDecal.blenddisablelighting = .3
		CUKDecal.blenddisablelighting = 0.3
		BackWallDecal.blenddisablelighting = .2
		Primitive_Roundsbox.blenddisablelighting = .5
		Primitive_PitchNPutt.blenddisablelighting = .3
		Primitive_PlasticRampDecal1.blenddisablelighting = 0.2
		Primitive_PlasticRampDecal2.blenddisablelighting = 0.2
		If GolfBall1.blenddisablelighting = 0 then GolfBall1.blenddisablelighting = 0.1
		If GolfBall2.blenddisablelighting = 0 then GolfBall2.blenddisablelighting = 0.1
		If GolfBall3.blenddisablelighting = 0 then GolfBall3.blenddisablelighting = 0.1
		If GolfBall4.blenddisablelighting = 0 then GolfBall4.blenddisablelighting = 0.1
		If GolfBall5.blenddisablelighting = 0 then GolfBall5.blenddisablelighting = 0.1

		If VolcanoLighting Then
			vpmTimer.AddTimer 10, "Primitive_Volcano.image=""VolcanoMap_GION1""'"
			vpmTimer.AddTimer 20, "Primitive_Volcano.image=""VolcanoMap_GION3""'"
			vpmTimer.AddTimer 30, "Primitive_Volcano.image=""VolcanoMap_GION5""'"
		End If
	End If
	Sound_GI_Relay enabled, Relay_GI
End Sub

Sub BackglassOff()
	VR_Backglass.image = "BG_Off"
	VR_s20.image = "BG_s20off"
	VR_s21.image = "BG_s21off"
	VR_s22.image = "BG_s22off"
End Sub

Sub BackglassOn()
	VR_Backglass.image = "BG_On"
	VR_s20.image = "BG_s20"
	VR_s21.image = "BG_s21"
	VR_s22.image = "BG_s22"
End Sub


'******************************************************
'				Gopher Wheel
'******************************************************

Dim roulette_step, roulette_time, roulette_spin_time, roulette_time_start, roulette_time_prev, roulette_time_step, wheelangle, isOff, holeangle, holeanglemod, prevholeangle
roulette_step = 0
roulette_time = 0
wheelangle = 0 
isOff = True
holeangle = 0 

Dim RouletteBall, CoconutBall

Dim GWHoles
GWHoles = Array (GW0,GW1,GW2,GW3,GW4,GW5,GW6,GW7,GW8,GW9,GW10,GW11)


Sub Init_Roulette()
	GWRamp.collidable=0
	Set RouletteBall = GW0.CreateSizedBallWithMass(21,20) '23,1.0*((23*2)^3)/125000
	RouletteBall.image = "Ball_HDR"
	RouletteSw.enabled = 1

	Set CoconutBall = Kicker2.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Kicker2.kick 180,1
	Kicker2.enabled = false
	cor.Update
End Sub

Sub EngageRoulette(enabled)
	If enabled then
		isOff = True
		roulette_time_start = gametime
		roulette_spin_time = 1000 + Int(rnd*400)
		roulette.enabled = 1

		For each ii in Rswitches:Controller.switch(ii)=0:next
		roulettesw.enabled = false
		For ii = 0 to 11
			GWHoles(ii).kick ((ii+1)*30+60), (rnd(4))+12
			'GWHoles(ii).enabled = false
		Next			
		GWRamp.collidable = 1
		ttGopherWheel.MotorOn = True

		PlaysoundatVol "fx_motor",GopherWheel,2*VolumeDial
	End If
End Sub

Sub Roulette_Timer()
	roulette_time_prev = roulette_time
	roulette_time = gametime - roulette_time_start
	roulette_time_step = roulette_time - roulette_time_prev

	If roulette_step < 0.15 and (roulette_step * roulette_time_step) > 1 Then
		wheelangle = wheelangle + 1
	Else
		wheelangle = wheelangle + (roulette_step * roulette_time_step)
	End If

	If wheelangle > 360 Then
		wheelangle=wheelangle-360
	End If

	holeangle = wheelangle mod 30
	prevholeangle = holeangle

	GopherWheel.RotY =  180 + wheelangle
	GopherCone.RotY =  210 + wheelangle
	GopherKicker.RotY = 180 + wheelangle
	CapGW.RotY = 96 + wheelangle

	if roulette_time < roulette_spin_time Then '1500
		roulette_step = FormatNumber(roulette_step + (0.05 * roulette_time_step), 2)
		If roulette_step > 0.75 Then
			roulette_step = 0.75
		End If
	Else
		if isOff = True Then
			ttGopherWheel.MotorOn = False
			isOff = False
			StopSound "fx_motor"
		End If

		roulette_step = FormatNumber(roulette_step - (0.0005 * roulette_time_step),4)
		If roulette_step < 0.075 then roulette_step = 0.075
	
		If roulette_step < 0.15 and holeangle = 0 Then
			GWRamp.collidable = 0
			roulette_step=0
			roulette.enabled = false
			roulettesw.enabled = true
			roulette_time = 0
		End If			
	End If

	'debug.print roulette_time & " " & roulette_step & " " & holeangle & " " & wheelangle
End Sub


Dim RouletteAtRest, RouletteBallAngle, RouletteX, RouletteY, RouletteAtn2, Rswitches, Rletters(12)
Const RouletteCenterX = 427.8
Const RouletteCenterY = 1324.3
RouletteAtRest = 0

Rswitches = array(40,41,42,43,44,45,50,51,52,53,54,55)

Sub GetRouletteSw()
	RouletteBallAngle = CalcAngle(RouletteBall.X,RouletteBall.Y,RouletteCenterX,RouletteCenterY)
	Rletters(0) = wheelangle + 38
	for ii = 0 to 11
		Rletters(ii) = (Rletters(0) + (ii*30)) mod 360
		If AngleIsNear(RouletteBallAngle,Rletters(ii),15) Then
			Controller.Switch(Rswitches(ii)) = 1
			Exit For
		End If
	next

End Sub


Sub RouletteSw_Timer()
	'debug.print BallVel(RouletteBall)
	If BallVel(RouletteBall) < 2 Then
		For each ii in GWKickers
			ii.enabled = true
		next
	End If
End Sub


Sub GW0_hit():GetRouletteSw:End Sub
Sub GW1_hit():GetRouletteSw:End Sub
Sub GW2_hit():GetRouletteSw:End Sub
Sub GW3_hit():GetRouletteSw:End Sub
Sub GW4_hit():GetRouletteSw:End Sub
Sub GW5_hit():GetRouletteSw:End Sub
Sub GW6_hit():GetRouletteSw:End Sub
Sub GW7_hit():GetRouletteSw:End Sub
Sub GW8_hit():GetRouletteSw:End Sub
Sub GW9_hit():GetRouletteSw:End Sub
Sub GW10_hit():GetRouletteSw:End Sub
Sub GW11_hit():GetRouletteSw:End Sub

 '*****************************************************************
 'Functions
 '*****************************************************************

'*** AngleIsNear returns true if the testangle is within range degrees of the target angle for 0 to 360 degree scale

Function AngleIsNear(testangle, target, range)
	If target <= range Then
		AngleIsNear = ( (testangle + (range*2) ) mod 360 >= target + range ) AND ( (testangle + (range*2) ) mod 360 <= target + (range*3) )
	ElseIf target >= 360 - range Then
		AngleIsNear = ( (testangle - (range*2) ) mod 360 >= target - (range*3) ) AND ( (testangle - (range*2) ) mod 360 <= target - range )
	Else
		AngleIsNear = (testangle >= target - range) AND (testangle <= target + range)
	End If
End Function

'*** CalcAngle returns the angle of a point (x,y) on a circle with center (centerx, centery) with angle 0 at 3 o'clock

Function CalcAngle(x,y,centerX,centerY)
	
	Dim tmpAngle
	
	tmpAngle = Atan2(x - centerX, y - centerY) * 180 / (4*Atn(1))

	If tmpAngle < 0 Then	
		CalcAngle = tmpAngle + 360
	Else	
		CalcAngle = tmpAngle
	End If

End Function

'*** Atan2 returns the Atan2 for a point on a circle (x,y)

Function Atan2(x,y)

	If x > 0 Then
		Atan2 = Atn(y/x)
	ElseIf x < 0 Then
		Atan2 = Sgn(y) * ((4*Atn(1)) - Atn(Abs(y/x)))
	ElseIf y = 0 Then
		Atan2 = 0
	Else
		Atan2 = Sgn(y) * (4*Atn(1)) / 2
	End If

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


'******************************************************
'       		RealTime Updates
'******************************************************

Sub GameTimer_Timer()
    UpdateMechs
	RollingSoundUpdate
	
	DoDTAnim
	DoSTAnim
	Cor.Update

	If VR_Room = 1 Then
		VR_Clouds.ObjRotZ=VR_Clouds.ObjRotZ+0.01
		VR_Clouds2.ObjRotZ=VR_Clouds2.ObjRotZ-0.01
	End If
End Sub

Sub UpdateMechs
	Prim_LeftFlipper.RotY=LeftFlipper.currentangle-90
	Prim_RightFlipper.RotY=RightFlipper.currentangle-90
	FlipperUR.RotY = RightFlipper2.currentangle
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperR2Sh.RotZ = RightFlipper2.currentangle


	'*******************************************
	'  VR Plunger Code
	'*******************************************
	If plungerpress = 1 then
		If VR_Primary_plunger.Y < 66 then
			VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5*10/25
		End If
	Else
		VR_Primary_plunger.Y = -26.875 + (5* Plunger.Position) - 20
	End If
End Sub



'*****************************************
'	Ramp and Drop Sounds
'*****************************************

'PlaySound "name",loopcount,volume,pan,randompitch,pitch,useexisting,restart
Sub TUKRampStart_Hit()
	WireRampOn False
	'PlaySound "wireramp_right", 1, 0.3*volumedial, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub TUKRampStop_Hit()
	WireRampOff
	'StopSound "wireramp_right"
End Sub

Sub RightRampStart_Hit()
	'PlaySound "wireramp_stop", 0, 0.3*volumedial, 0.2
	'PlaySound "wireramp_right2", -1, 0.3*volumedial, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	WireRampOn False
End Sub

Dim RightRampBall

Sub RightRampStop_Hit()
	WireRampOff
	if BallVel(activeball) > 10 Then RandomSoundRampStop RightRampStop
	Set RightRampBall = ActiveBall
	RightRampStop.timerenabled = true
End Sub

Sub RightRampStop_Timer()
	if RightRampBall.VelZ < -1 and RightRampBall.Z < 145 Then
		'PlaySoundAtVol "wireramp_stop", RightRampStop, 0.3*volumedial
	End If
	If RightRampBall.Z < 140 then	
		me.timerenabled = False
	End If
End Sub

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub


Sub CUKRampStart_Hit()
	WireRampOn False
	'PlaySound "wireramp_right3", 1, 0.3*volumedial, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Dim CUKRampBall

Sub CUKRampStop_Hit()
	WireRampOff
	if BallVel(activeball) > 10 Then RandomSoundRampStop CUKRampStop
	Set CUKRampBall = ActiveBall
	CUKRampStop.timerenabled = True
End Sub

Sub CUKRampStop_Timer()
	if CUKRampBall.VelZ < -1 and CUKRampBall.Z < 145 Then
		'PlaySound "wireramp_stop", 0, 0.3*volumedial, 0.2
		'PlaySoundAtVol "wireramp_stop", CUKRampStop, 0.3*volumedial
	End If
	If CUKRampBall.Z < 140 then	
		me.timerenabled = False
	End If
End Sub

Sub RedRampStart_Hit()
	WireRampOn True
	'PlaySound "plasticroll", 1, Vol(ActiveBall)*volumedial, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub RedRampStop_Hit()
	WireRampOff
	'StopSound "plasticroll"
End Sub

Dim RedRampBall

Sub RedRampStop1_Hit()
	WireRampOff
	'StopSound "plasticroll"
	'PlaySound "wireramp_stop", 0, 0.3*volumedial, 0.2
	Set RedRampBall = ActiveBall
	RedRampStop1.timerenabled = true
End Sub

Sub RedRampStop1_Timer()
	If RedRampBall.Z < 140 then	
		me.timerenabled = False
	End If
End Sub

Sub PNPStart_Hit()
	If Activeball.vely < 0 Then
		WireRampOn True
		'PlaySound "plasticroll1", 1, Vol(ActiveBall)*5*volumedial, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End If
End Sub

Sub PNPStart_Unhit()
	If Activeball.vely > 0 Then
		WireRampOff
		'StopSound "plasticroll1"
	End If
End Sub

Sub PNPStop_UnHit()
	If Activeball.vely < 0 Then
		WireRampOff
		'StopSound "plasticroll1"
	End If
End Sub

Sub SLStart_Hit()
	If Activeball.vely < 0 Then
		WireRampOn True
		'PlaySound "plasticroll2", 1, Vol(ActiveBall)*5*volumedial, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End If
End Sub

Sub SLStart_Unhit()
	If Activeball.vely > 0 Then
		WireRampOff
		'StopSound "plasticroll2"
	End If
End Sub

Sub SLStop_UnHit()
	If Activeball.vely < 0 Then
		WireRampOff
		'StopSound "plasticroll2"
	End If
End Sub


'******************************************************
'			Supporting Ball & Sound Functions
'******************************************************


Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / tablewidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function


'******************************************************
'      		Rolling Sounds & Ball Shadows
'******************************************************

Const tnob = 8 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6)

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

Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 120 Then
            rolling(b) = True
            PlaySound ("BallRoll_" & b & ampFactor), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("BallRoll_" & b & ampFactor)
                rolling(b) = False
            End If
        End If

		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 120 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1

		' *** Normal "ambient light" ball shadow
		'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		ElseIf AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(b).Z > 120 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If BOT(b).X < tablewidth/2 Then
					objBallShadow(b).X = ((BOT(b).X) - (Ballsize/10) + ((BOT(b).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(b).X = ((BOT(b).X) + (Ballsize/10) + ((BOT(b).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(b).Y = BOT(b).Y + BallSize/10 + fovY
				objBallShadow(b).visible = 1

				BallShadowA(b).X = BOT(b).X
				BallShadowA(b).Y = BOT(b).Y + BallSize/5 + fovY
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(b).visible = 1
			Elseif BOT(b).Z <= 120 And BOT(b).Z > 100 Then	'On pf, primitive only
				objBallShadow(b).visible = 1
				If BOT(b).X < tablewidth/2 Then
					objBallShadow(b).X = ((BOT(b).X) - (Ballsize/10) + ((BOT(b).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(b).X = ((BOT(b).X) + (Ballsize/10) + ((BOT(b).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(b).Y = BOT(b).Y + fovY
				BallShadowA(b).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(b).visible = 0
				BallShadowA(b).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(b).Z > 120 Then							'In a ramp
				BallShadowA(b).X = BOT(b).X
				BallShadowA(b).Y = BOT(b).Y + BallSize/5 + fovY
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(b).visible = 1
			Elseif BOT(b).Z <= 120 And BOT(b).Z > 100 Then	'On pf
				BallShadowA(b).visible = 1
				If BOT(b).X < tablewidth/2 Then
					BallShadowA(b).X = ((BOT(b).X) - (Ballsize/10) + ((BOT(b).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(b).X = ((BOT(b).X) + (Ballsize/10) + ((BOT(b).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(b).Y = BOT(b).Y + Ballsize/10 + fovY
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(b).visible = 0
			End If
		End If


		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 145 and BOT(b).z > 117 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				'DropCount(b) = 0
				If BOT(b).velz > -5 Then
					If BOT(b).z < 125 Then
						DropCount(b) = 0
						RandomSoundBallBouncePlayfieldSoft BOT(b)
					End If
				Else
					DropCount(b) = 0
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
    Next

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

Sub RightFlipper2_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper2, RFCount1, parm
 	RightFlipperCollide parm
End Sub



'*******************************************
' Hybrid code for VR, Cab, and Desktop
'*******************************************

Dim VRThings, vrx, VRLShadows(30), VRLBDL(60), VRLSCount, VRLBDLCount

if VR_Room = 0 and cab_mode = 0 Then
	for each VRThings in VRMinimal:VRThings.visible = 0:Next
	for each VRThings in VRCabinet:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	for each VRThings in VRGolf:VRThings.visible = 0:Next
Elseif VR_Room = 0 and cab_mode = 1 Then
	for each VRThings in VRMinimal:VRThings.visible = 0:Next
	for each VRThings in VRCabinet:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	for each VRThings in VRGolf:VRThings.visible = 0:Next
Elseif VR_Room = 1 Then
	for each VRThings in VRMinimal:VRThings.visible = 0:Next
	for each VRThings in VRCabinet:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	for each VRThings in VRGolf:VRThings.visible = 1:Next
	If LowResGrass = 1 then VR_Grass.image = "VR_Golf_Grass1024"
	vrx = 0
	For Each VRThings in VRGolfLShadows
		Set VRLShadows(vrx) = VRThings
		vrx = vrx + 1
	Next
	VRLSCount = vrx

	vrx = 0
	For Each VRThings in VRGolfBDL
		Set VRLBDL(vrx) = VRThings
		vrx = vrx + 1
	Next
	VRLBDLCount = vrx
Else
	for each VRThings in VRMinimal:VRThings.visible = 1:Next
	for each VRThings in VRCabinet:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = WallClock:ClockTimer.enabled = WallClock:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
	for each VRThings in VRGolf:VRThings.visible = 0:Next

	'Custom Walls, Floor, and Roof
	if CustomWalls = 0 Then
		VR_Wall_Left.image = "VR_Wall_Left"
		VR_Wall_Right.image = "VR_Wall_Right"
		VR_Floor.image = "VR_Floor"
		VR_Roof.image = "VR_Roof"
	elseif CustomWalls = 1 Then
		VR_Wall_Left.image = "wallpaper left"
		VR_Wall_Right.image = "wallpaper_right"
		VR_Floor.image = "FloorCompleteMap"
		VR_Roof.image = "light gray"
	end if
End If


'*******************************************
'  VR Clock 
'*******************************************

Dim CurrentMinute ' for VR clock

Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
	Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub


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

Sub TBouncer_Hit
	TargetBouncer activeball, 1
	playsound "knocker_1"
End Sub

'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity


'*******************************************
 ' Late 80's early 90's

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5
	AddPt "Polarity", 2, 0.4, -5
	AddPt "Polarity", 3, 0.6, -4.5
	AddPt "Polarity", 4, 0.65, -4.0
	AddPt "Polarity", 5, 0.7, -3.5
	AddPt "Polarity", 6, 0.75, -3.0
	AddPt "Polarity", 7, 0.8, -2.5
	AddPt "Polarity", 8, 0.85, -2.0
	AddPt "Polarity", 9, 0.9,-1.5
	AddPt "Polarity", 10, 0.95, -1.0
	AddPt "Polarity", 11, 1, -0.5
	AddPt "Polarity", 12, 1.1, 0
	AddPt "Polarity", 13, 1.3, 0

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

' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub




'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
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
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperTricks2 RightFlipper2, RFPress1, RFCount1, RFEndAngle1, RFState1
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
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

EOST2 = rightflipper2.eostorque
EOSA2 = rightflipper2.eostorqueangle
FReturn2 = rightFlipper2.return
Const EOSTnew2 = 1.2 'EM

Const EOSTnew = 0.85 '90's and later
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
Const EOSReturn2 = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle1 = RightFlipper2.endangle

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
			Flipper.eostorque = EOSTnew
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
'****  END FLIPPER CORRECTIONS
'******************************************************

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
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
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

Sub RandomSoundSlingshotTop(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_T" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
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

Sub ArchTrig_unhit
	'debug.print activeball.vely
	If activeball.vely < -12 Then
		RandomSoundRightArch
	End If
End Sub

Sub ArchTrig2_hit()
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Wall33_Hit()
	If wall33.timerenabled = false Then
		RandomSoundMetal
		wall33.timerenabled = True
	End If
End Sub

Sub Wall33_Timer
	me.timerenabled = false
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
	If velocity > 2 then
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
	End If
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

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(9,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(9)	

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
			if RampBalls(x,0).Z < 120 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
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
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection
' The easiest way to keep track of this is to start with the group on the left slingshot and move clockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'																E
'	A		 C													B
'	 B		D			your collection should look like		A		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F													C
'																D
'																F
'
'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
'	' stop the sound of deleted balls
'	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		rolling(b) = False
'		StopSound("BallRoll_" & b)
'	Next
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
'Function DistanceFast(x, y)
'	dim ratio, ax, ay
'	ax = abs(x)					'Get absolute value of each vector
'	ay = abs(y)
'	ratio = 1 / max(ax, ay)		'Create a ratio
'	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
'	if ratio > 0 then			'Quickly determine if it's worth using
'		DistanceFast = 1/ratio
'	Else
'		DistanceFast = 0
'	End if
'end Function
'
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

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
'
'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


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

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 90.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 90.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow0" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 90.04
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
	For each BOT in Array(TOBall1, TOBall2, TOBall3, TOBall4)
		s = s + 1
		If BOT.Z < 120 and BOT.z > 110 and BOT.y < 1855 and BOT.x < 850 Then 'Defining when and where (on the table) you can have dynamic shadows
			For iii = 0 to numberofsources - 1 
				LSd=Distance(BOT.x, BOT.y, DSSources(iii)(0),DSSources(iii)(1))	'Calculating the Linear distance to the Source
				If LSd < falloff Then						    			'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
					currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
					if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
						sourcenames(s) = iii 'ssource.name
						currentMat = objrtx1(s).material
						objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT.X : objrtx1(s).Y = BOT.Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
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
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT.X, BOT.Y) + 90
						ShadowOpacity = (falloff-Distance(BOT.x,BOT.y,DSSources(AnotherSource)(0),DSSources(AnotherSource)(1)))/falloff
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

						currentMat = objrtx2(s).material
						objrtx2(s).visible = 1 : objrtx2(s).X = BOT.X : objrtx2(s).Y = BOT.Y + fovY
'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
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

Sub RTXFrameTimer_Timer()
	If DynamicBallShadowsOn Then 
		DynamicBSUpdate 'update ball shadows
	Else
		me.Enabled = False
	End If
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************



' *******************     LOL - They aren't in Collections... :) *****************************************************
Sub LightningOn()
	For vrx = 0 to VRLSCount - 1
		VRLShadows(vrx).visible = True
	Next
	For vrx = 0 to VRLBDLCount - 1
		VRLBDL(vrx).Blenddisablelighting = 1
	Next
End Sub

Sub LightningOff()
	For vrx = 0 to VRLSCount - 1
		VRLShadows(vrx).visible = False
	Next
	For vrx = 0 to VRLBDLCount - 1
		VRLBDL(vrx).Blenddisablelighting = 0
	Next
End Sub

Sub SkyLightTimer1_timer()
	SkyLightTimer2.interval = 40 + rnd(1)*120  ' random between 40 and 160ms
	'VR_Sphere1.image = "VR_Golf_Sky"
	VR_Sphere1.visible = false
	VR_Sphere2.visible = true
	SkyLightTimer1.enabled = false
	SkyLightTimer2.enabled = true
End Sub

Sub SkyLightTimer2_timer()
	SkyLightTimer1.interval = 4000 + rnd(1)*4000  ' random between 4 and 8 seconds
	'VR_Sphere1.image = "VR_Golf_Sky_Dull"
	VR_Sphere1.visible = true
	VR_Sphere2.visible = false
	SkyLightTimer1.enabled = true
	SkyLightTimer2.enabled = false
End Sub


dim preloadCounter
sub preloader_timer
	preloadCounter = preloadCounter + 1
	If VR_Room = 1 Then

		if preloadCounter = 1 then
			VR_Lightning20.visible = 1
			VR_Lightning21.visible = 1
			VR_Lightning22.visible = 1
			LightningOn
		Elseif preloadCounter = 2 then
			VR_Lightning20.visible = 0
			VR_Lightning21.visible = 0
			VR_Lightning22.visible = 0
			LightningOff
			VR_Sphere2.visible = false
			SkyLightTimer1.enabled = true
			me.enabled = false
		end if
	Else
		me.enabled = false
	End If
end sub



