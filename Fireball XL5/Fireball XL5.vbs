Option Explicit


'################ FireballXL5 ########################################################################


' Based on the Fireball XL5 TV series created by Gerry and Sylvia Anderson - 1962

' All music by Barry Gray

' Playfield graphics and models from various web sites


' Table Design, Construction and Code by Sir Random


' Table #001


' Started from the "Completely Blank Table" in the VPX editor


' Big thanks to: 

' Smaug (Marty) at GameClubCentral for playtesting, promoting and launching the game on his channel.

' haggi at VPForums for creating B2S back glasses.

' wiesshund, scutters, digitalarts, and everyone at VPForums for their help and advice.



' HauntFreaks for the GI
' MauiPunter for DOF script & config



' Table Babies: Adopted a kitten halfway through build, named her Venus :)


'*****************************************************************************************************
' Blank Table CREDITS
' Initial table created by fuzzel, jimmyfingers, jpsalas, toxie & unclewilly (in alphabetical order)
' Flipper primitives by zany
' Ball rolling sound script by jpsalas
' Ball shadow by ninuzzu
' Ball control & ball dropping sound by rothbauerw
' DOF by arngrim
' Positional sound helper functions by djrobx
' Plus a lot of input from the whole community (sorry if we forgot you :/)
'*****************************************************************************************************

'#####################################################################################################


'First, try to load the Controller.vbs (DOF), which helps controlling additional hardware like lights, gears, knockers, bells and chimes (to increase realism)
'This table uses DOF via the 'SoundFX' calls that are inserted in some of the PlaySound commands, which will then fire an additional event, instead of just playing a sample/sound effect
On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"

ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

'If using Visual PinMAME (VPM), place the ROM/game name in the constant below,
'both for VPM, and DOF to load the right DOF config from the Configtool, whether it's a VPM or an Original table

Dim Controller	' B2S
Const cGameName = "FireballXL5"

'Set B2SController = CreateObject("B2S.Server")
'B2SController.B2sName = "Fireball XL5"
'B2SController.run
LoadEM

'############################## User Preferences for Desktop/FlexDMD

Dim ShowFlexDMD

If Table1.ShowDT = False Then
	ScoreBoard.Visible = false
	ScoreText001.Visible = false
	ScoreText002.Visible = false
	ScoreText003.Visible = false
	ScorePlayer001.Visible = false
	ScorePlayer002.Visible = false
	ScorePlayer003.Visible = false
	ScorePlayer004.Visible = false
	MovieReel.Visible = false
	MovieReelFlasher.Visible = True		' Apron video
	CreditBox.Visible = false
    ShowFlexDMD = True
Else
    ShowFlexDMD = False
	MovieReelFlasher.Visible = False
End If





Dim EnableRetractPlunger
EnableRetractPlunger = False 'Change to true to enable retracting the plunger at a linear speed; wait for 1 second at the maximum position; move back towards the resting position; nice for button/key plungers

Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys


Sub Table1_KeyDown(ByVal keycode)


	' GNMOD
	If EnteringInitials Then
		CollectInitials(keycode)
		Exit Sub
	End If


	If keycode = PlungerKey Then
		If DMD_VideoMode Then 
			DMD_VideoControlDown = True
		Else
			If EnableRetractPlunger Then
				Plunger.PullBackandRetract
			Else
				Plunger.PullBack
			End If
			SoundPlungerPull
		End If
	End If

	If keycode = LeftFlipperKey And Not Tilted Then
		If SkillShotTarget > 0 Then CycleSkill(-1)
		If DMD_VideoMode Then 
			DMD_VideoControlLeft = True
		Else
			LeftFlipper.TimerEnabled = True 'This line is only for ninuzzu's flipper shadows!
			LeftFlipper.RotateToEnd
			SoundFlippers("LfUp")
		End If
	End If

	If keycode = RightFlipperKey And Not Tilted Then
		If SkillShotTarget > 0 Then	CycleSkill(1)
		If DMD_VideoMode Then 
			DMD_VideoControlRight = True
		Else
			RightFlipper.TimerEnabled = True 'This line is only for ninuzzu's flipper shadows!
			RightFlipper.RotateToEnd
			SoundFlippers("RfUp")
		End If
	End If

	If keycode = LeftTiltKey Then
		Nudge 90, 5
		Check_Tilt
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 5
		Check_Tilt
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 8
		Check_Tilt
	End If

	If keycode = MechanicalTilt Then
		Nudge 0, 6
		Check_Tilt
	End If




    ' Manual Ball Control
	If keycode = 46 Then	 				' C Key
		If EnableBallControl = 1 Then
			EnableBallControl = 0
		Else
			EnableBallControl = 1
		End If
	End If

    If EnableBallControl = 1 Then
		If keycode = 48 Then 				' B Key
			If BCboost = 1 Then
				BCboost = BCboostmulti
			Else
				BCboost = 1
			End If
		End If
		If keycode = 203 Then BCleft = 1	' Left Arrow
		If keycode = 200 Then BCup = 1		' Up Arrow
		If keycode = 208 Then BCdown = 1	' Down Arrow
		If keycode = 205 Then BCright = 1	' Right Arrow
	End If


	If keycode = addcreditkey Then
		If GameReady Then
			playsound "Coin_In_" & Int(Rnd*3)+1, 0.9
			AddCredit
		Else
			PlaySound "Bell10000", 0.5
		End If
	End If

	If keycode = addcreditkey2 Then
		If GameReady Then 
			playsound "Coin_In_" & Int(Rnd*3)+1, 0.9
			AddCredit
			If Not InGame Then
'				keycode = StartGameKey
			End If
		Else
			PlaySound "Bell10000", 0.5
		End If
	End If

	If keycode = StartGameKey Then
		If GameReady And Not InGame Then
			CheckCredits
		Else
			PlaySound "Bell10000", 0.5
		End If
	End If

End Sub




Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
		If Not DMD_VideoMode Then SoundPlungerReleaseBall
		DMD_VideoControlDown = False
	End If

	If keycode = LeftFlipperKey And Not Tilted Then
		LeftFlipper.RotateToStart
		If Not DMD_VideoMode Then SoundFlippers("LfDown")
		DMD_VideoControlLeft = False
	End If

	If keycode = RightFlipperKey And Not Tilted Then
		RightFlipper.RotateToStart
'		UpRightFlipper.RotateToStart
		If Not DMD_VideoMode Then SoundFlippers("RfDown")
		DMD_VideoControlRight = False
	End If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
End Sub




'*************************** Fireball XL5 Code **********

Const HSFileName="Fireball_XL5.txt"
ScoreText.Visible = False


Sub CheckCredits()
	If Credits > 0 Then
		If Tilted Then
			Credits = Credits - 1
			CreditBox.Text = Credits	
			If ShowFlexDMD Then	DMDShowBallCred
			Players = Players + 1
			Start_Game
		ElseIf Players < 4 And Not GameValidated Then
			Credits = Credits - 1
			CreditBox.Text = Credits	
			If ShowFlexDMD Then	DMDShowBallCred
			Players = Players + 1
			ScoreText001.Text = (" " & Players & " PLAYERS ")
			If ShowFlexDMD Then DMDBlinkSplashString Players & " PLAYERS ", 2, False, True
			PlaySound "Bell10_XL5", 0.75
		Else
			PlaySound "Bell1000", 0.5
		End If
	Else
		PlaySound "Bell1000", 0.5
	End If
End Sub

Sub AddCredit()
	Credits = Credits + 1
	If Credits > 9 Then
		Credits = 9
	End If
	CreditBox.Text = Credits
	If ShowFlexDMD Then	DMDShowBallCred
	If Tilted Then
		If ShowFlexDMD And Not DMDscrollsplash.Enabled Then	DMDBlinkSplashString "PRESS START", 10, False, True
		ScoreText002.Text = "PRESS"
		ScoreText003.Text = " START"
	End If
End Sub

Sub AddSpecial()
	SoundKnocker
	AddCredit
	If ShowFlexDMD Then DMD_SwapFrames "Replay", "ReplayA", 20, 100
End Sub



Sub Drain_Hit()
	Dim i
	SoundDrainHit
	Drain.DestroyBall
	TiltBox.Text = ""
	TiltReel.Visible = False
	If BallSaveString > "" Then
		If Ballsaver < BallSaveTime Then
			If Ballsaved < 4 Then
				Ballsaved = Ballsaved + 1
			End If
			BallSave.Enabled = False
			ShowCountdown
			Ballsaver = 99
			ScoreFlash("  ESCAPE POD   ACTIVATED")
			If ShowFlexDMD Then	DMDBlinkSplashString "BALL SAVED " & BallSaveString, 0, False, False
			If BallSaveString = "0.1" Then
				PlaySound "XL5_CloseOne2"
			End If
		End If
		BallRelease.CreateBall
		BallRelease.Kick 90, 10 
	Else
		If BIP = 1 Then

			If Tilted Then
				UnTilt_Game
			Else
				CollectBonus
			End If
			Exit Sub
		End If
		If BIP = 2 Then
			If WizardActive Then Stop_Wizard
			Multiball = False
			For i = 2 to 9
				Eval("Jackpot_Light00" & i).State = 0
			Next
			JackpotLit(Player) = 0
			PlungeBot.Enabled = False
			InMode = False
			VenusActive = False
			FadeOutSound CurrentMusic, 2
			Musictimer.Interval = 1500
			If PlayfieldCounter > 0 Then
				ScoreX = PlayfieldX
			Else
				ScoreX = 1
			End If
		End If
		BIP = BIP - 1
	End If
End Sub



Dim HSScore(5)	
Dim HSName(5)	
Dim MeteorHSName


' Default values for savefile created on first play

Credits = 0

HSScore(1) = 50000000
HSScore(2) = 25000000
HSScore(3) = 10000000
HSScore(4) = 5000000
HSScore(5) = 1000000

HSName(1) = "XL5"
HSName(2) = "STV"
HSName(3) = "VNS"
HSName(4) = "MAT"
HSName(5) = "ROB"

MeteorHS = 10
MeteorHSName = "J90"

Sub Plunger_Init()
	Dim i
	Tilted = True
	HSScore(1) = 50000000
	HSScore(2) = 25000000
	HSScore(3) = 10000000
	HSScore(4) = 5000000
	HSScore(5) = 1000000

	HSName(1) = "XL5"
	HSName(2) = "STV"
	HSName(3) = "VNS"
	HSName(4) = "MAT"
	HSName(5) = "ROB"

	MeteorHS = 10
	MeteorHSName = "J90"

	LoadData		'Load credits & highscores OR create default savefile if none exists (first game)
	Game_init
	AttractModeActive = True
	HighScoreTimer.enabled = True
	CreditBox.Text = Credits
	ScoreBoard.Text = "  FIREBALL XL5"
	ScoreText001.Text = ""
	ScoreText002.Text = "PLEASE"
	ScoreText003.Text = "  WAIT"



	ShowFlexDMD = True							'########################### TEST - REMOVE
	MovieReelFlasher.Visible = True

	If ShowFlexDMD Then 
		FlexDMD_Init
	Else
		FlexDMD = Null
	End If

	For i = 4 to 13
		Eval("Ramp0" & i).Visible = 0
	Next

	If ShowFlexDMD Then DMDBlinkSplashString "PLEASE WAIT", 5, False, False



	Movie_Radio				' normal start
'	CycleFireball
	InitRolling
End Sub





Dim Score(4)
Dim Jackpot(4)
Dim JackpotLit(4)
Dim Lite1(4)
Dim Lite2(4)
Dim Lite3(4) 
Dim EBall(4)
Dim BallNum(4)
Dim LocksLit(4)
Dim BonusX(4)
Dim Bumper1(4)
Dim Bumper2(4)
Dim Bumper3(4)
Dim BonusCollect(4)
Dim Special(4)
Dim SpotCount(4)
Dim Spot1(4)
Dim Spot2(4)
Dim Spot3(4)
Dim Spot4(4)
Dim Spot5(4)
Dim ScoreString(4)
Dim ABCBonus(4)
Dim VenusWon(4)
Dim PowerWon(4)
Dim EBallCollect(4)
Dim SkillShotScore(4)
Dim VenusStarted(4)
Dim PowerStarted(4)
Dim M_PowerWarned(4)
Dim TenBillion(4)
Dim FXCheck(6)
Dim MeteorWon(4)
Dim Warp9Won(4)
Dim Modes(4)


Dim HighScoreString(5)



Dim Player, Players, InGame, GameReady
Dim ScoreX, Credits, HighScore, MeteorHS
Dim Targets_down, Banks_down, Spinner_Lit  
Dim Bonus, Bonus_held, Super_Bonus, BonusTotal, BonX
Dim TextStr, TextStr2
Dim CounterBonusX, CounterKicker, CounterKicker2
Dim FlashText, FlashNum, Flash_bulb, ScrollText, ScrollCounter, GameText, BonusString
Dim BellCount50, BellCount500, BellCount5000, BellCount50000, StarCount
Dim GameBalls, BallsPerGame, BIP, Ballsaver, BallSaveTime, BallSaveString, PlayfieldX, PlayfieldXTime
Dim Tilted, Tiltnum
Dim Multiball, MultiballCounter, Ball_Locked, Ballsaved
Dim PlayfieldCounter, ABC_FlashCounter, AddBonusCounter, Jackpot_TargetCounter, BlinkCounter
Dim FireballCounter, FireballCounter2, FireballCounter3, BallSaveCounter, AttractCounter
Dim M_IntroCounter, M_OutroCounter, MovieCounter, MBallCounter, EballCounter, EballCounter2
Dim CountDownActive, ShieldActive
Dim MovieString, BlinkString, BlinkString2, BlinkFrames, BlinkGap, BlinkOff

Dim M_GammaCounter, M_GammaForward, M_GammaSplice
Dim M_VenusCounter, M_VenusCounter2, M_VenusScene, M_VenusForward, VenusActive, WizardActive
Dim M_PowerForward,	M_PowerCounter, M_PowerCounter2, M_PowerCounterOld, M_PowerHigh, M_PowerScene, PowerActive, PowerCount, M_Powerkicked, M_PowerOn

Dim M_MatCounter, M_MatCounter2, M_MatScene, MatComplete

Dim FPSwap, XL5Loop, InMode, BSwap, ModesTotal
Dim GameValidated, BonusMaxed
Dim RobTalkSound, RobTalkFrames, RobTalking
Dim Gallery, CurrentMusic, CurrentMusicNum, SoundDelay, LoopName, StopLoop
Dim SkillShotTarget
Dim Spots_hit
 

GameReady = False








'### FlexDMD



'*****************************************************************************************************
' UltraDMD constants
'Const UltraDMD_VideoMode_Stretch=0, UltraDMD_VideoMode_Top = 1, UltraDMD_VideoMode_Middle = 2, UltraDMD_VideoMode_Bottom = 3
'Const UltraDMD_Animation_FadeIn = 0, UltraDMD_Animation_FadeOut = 1, UltraDMD_Animation_ZoomIn = 2, UltraDMD_Animation_ZoomOut = 3
'Const UltraDMD_Animation_ScrollOffLeft = 4, UltraDMD_Animation_ScrollOffRight = 5, UltraDMD_Animation_ScrollOnLeft = 6, UltraDMD_Animation_ScrollOnRight = 7,_
'UltraDMD_Animation_ScrollOffUp = 8,UltraDMD_Animation_ScrollOffDown = 9,UltraDMD_Animation_ScrollOnUp = 10,UltraDMD_Animation_ScrollOnDown = 11,UltraDMD_Animation_None = 14



' FlexDMD constants
Const 	FlexDMD_RenderMode_DMD_GRAY = 0, _
		FlexDMD_RenderMode_DMD_GRAY_4 = 1, _
		FlexDMD_RenderMode_DMD_RGB = 2, _
		FlexDMD_RenderMode_SEG_2x16Alpha = 3, _
		FlexDMD_RenderMode_SEG_2x20Alpha = 4, _
		FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num = 5, _
		FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num_4x1Num = 6, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num = 7, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_10x1Num = 8, _
		FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num_gen7 = 9, _
		FlexDMD_RenderMode_SEG_2x7Num10_2x7Num10_4x1Num = 10, _
		FlexDMD_RenderMode_SEG_2x6Num_2x6Num_4x1Num = 11, _
		FlexDMD_RenderMode_SEG_2x6Num10_2x6Num10_4x1Num = 12, _
		FlexDMD_RenderMode_SEG_4x7Num10 = 13, _
		FlexDMD_RenderMode_SEG_6x4Num_4x1Num = 14, _
		FlexDMD_RenderMode_SEG_2x7Num_4x1Num_1x16Alpha = 15, _
		FlexDMD_RenderMode_SEG_1x16Alpha_1x16Num_1x7Num = 16

Const 	FlexDMD_Align_TopLeft = 0, _
		FlexDMD_Align_Top = 1, _
		FlexDMD_Align_TopRight = 2, _
		FlexDMD_Align_Left = 3, _
		FlexDMD_Align_Center = 4, _
		FlexDMD_Align_Right = 5, _
		FlexDMD_Align_BottomLeft = 6, _
		FlexDMD_Align_Bottom = 7, _
		FlexDMD_Align_BottomRight = 8

Dim AlphaChars(255)
AlphaChars(48) = &h443F ' 0
AlphaChars(49) = &h0406 ' 1
AlphaChars(50) = &h085B ' 2
AlphaChars(51) = &h080F ' 3
AlphaChars(52) = &h0866 ' 4
AlphaChars(53) = &h086D ' 5
AlphaChars(54) = &h087D ' 6
AlphaChars(55) = &h2401 ' 7
AlphaChars(56) = &h087F ' 8
AlphaChars(57) = &h086F ' 9
AlphaChars(65) = &h0877 ' A
AlphaChars(66) = &h2A0F ' B
AlphaChars(67) = &h0039 ' C
AlphaChars(68) = &h220F ' D
AlphaChars(69) = &h0879 ' E
AlphaChars(70) = &h0871 ' F
AlphaChars(71) = &h083D ' G
AlphaChars(72) = &h0876 ' H
AlphaChars(73) = &h2209 ' I
AlphaChars(74) = &h001E ' J
AlphaChars(75) = &h1470 ' K
AlphaChars(76) = &h0038 ' L
AlphaChars(77) = &h0536 ' M
AlphaChars(78) = &h1136 ' N
AlphaChars(79) = &h003F ' O
AlphaChars(80) = &h0873 ' P
AlphaChars(81) = &h103F ' Q
AlphaChars(82) = &h1873 ' R
AlphaChars(83) = &h090D ' S
AlphaChars(84) = &h2201 ' T
AlphaChars(85) = &h003E ' U
AlphaChars(86) = &h4430 ' V
AlphaChars(87) = &h5036 ' W
AlphaChars(88) = &h5500 ' X
AlphaChars(89) = &h2500 ' Y
AlphaChars(90) = &h4409 ' Z


Dim FlexDMD, DMDScene, SplashX
Dim DMDSplashAF, DMDSplash_SAF, DMDSplash2AF, DMDSplash2_SAF, DMDSplash3AF, DMDSplash3_SAF
Dim SplashBlink, SplashBlink_S, SplashBlink2, SplashBlink2_S, SplashBlink3, SplashBlink3_S, SplashBlinkFast, SplashBlink_SFast
Dim FontScoreInactive, FontScoreActive, FontScoreActiveShadow, FontScoreActiveSmall
Dim FontXL5, FontXL5Shadow, FontCool, FontCoolShadow, FontTiny
Dim DMDSwap : DMDSwap = True
Dim DMDShowSplash, DMDShowVideo

Dim Frame : Frame = 0
Dim SplashText : SplashText = ""
Dim DMDCounter, DMD_SpinCounter, DMD_EBallCounter, DMD_MBallCounter, DMD_XL5Counter
Dim DMDSwapframecounter, DMDSwapframe1, DMDSwapframe2, DMDSwapframeNum
Dim DMD_IntroCounter, DMD_IntroCounter2, DMD_IntroCounter3, DMD_StarfieldCounter
Dim DMD_RocketPos
Dim DMD_VideoMode, DMD_ShipZ, DMD_ShipXY, DMD_ShipXY_Old, DMD_Ship_Old
Dim DMD_VideoScene, DMD_VideoControlLeft, DMD_VideoControlRight, DMD_VideoControlDown
Dim DMD_VideoCounter, DMD_Mgap, MeteorNum, DMD_VideoScore, DMD_SkillCounter
Dim MeteorSize(5,2)

Dim DMD_ExplosionX, DMD_ExplosionY, DMD_MeteorExplodeCounter, DMDShowSwap
Dim Blank, BlankWhite, BlankBlack, BlankGrey

Dim DMD_Shiphit 
Dim DMD_Ships(4)
Dim DMD_Lasershots(4)

Dim DMD_Splash, DMD_Splash_S, DMD_Splash2, DMD_Splash2_S, DMD_Splash3, DMD_Splash3_S


DMD_XL5Counter = 0

Sub FlexDMD_Init()
	CreateDMD
	CreateDMDScene

	Set Blank = FlexDMD.Stage.GetImage("Blank")
	Set BlankWhite = FlexDMD.Stage.GetImage("BlankWhite")
	Set BlankBlack = FlexDMD.Stage.GetImage("BlankBlack")
	Set BlankGrey = FlexDMD.Stage.GetImage("BlankGrey")

	Set DMD_Splash = FlexDMD.Stage.GetLabel("Splash")
	Set DMD_Splash_S = FlexDMD.Stage.GetLabel("Splash_S")
	Set DMD_Splash2 = FlexDMD.Stage.GetLabel("Splash2")
	Set DMD_Splash2_S = FlexDMD.Stage.GetLabel("Splash2_S")
	Set DMD_Splash3 = FlexDMD.Stage.GetLabel("Splash3")
	Set DMD_Splash3_S = FlexDMD.Stage.GetLabel("Splash3_S")
	Set DMD_OldChar = FlexDMD.Stage.GetImage("DMD_Char_1")
End Sub	

Sub CreateDMD()
	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	If FlexDMD is Nothing Then
		MsgBox "No FlexDMD found. "
		ShowFlexDMD = False
		Exit Sub
	End If
	SetLocale(1033)
	With FlexDMD
		.GameName = cGameName
		.TableFile = Table1.Filename & ".vpx"
		.Color = RGB(255, 88, 32)
		.RenderMode = FlexDMD_RenderMode_DMD_GRAY_4
		.Width = 128
		.Height = 32
		.Clear = True
		.Run = True
	End With
	Dim DMDp
	DMDp = FlexDMD.DmdPixels
	If Not IsEmpty(DMDp) Then
		DMDWidth = FlexDMD.Width
		DMDHeight = FlexDMD.Height
		DMDPixels = DMDp
	End If
End Sub



Sub CreateDMDScene()
	Dim i, j, picnum, frameactor

'##### Fonts 

	Set FontTiny = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(128, 128, 128), vbWhite, 0)
	Set FontScoreActive = FlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", vbWhite, vbWhite, 0)
	Set FontScoreActiveShadow = FlexDMD.NewFont("FlexDMD.Resources.udmd-f7by13.fnt", RGB(16, 16, 16), vbWhite, 0)
	Set FontScoreInactive = FlexDMD.NewFont("FlexDMD.Resources.udmd-f4by5.fnt", RGB(128, 128, 128), vbWhite, 0)	
	Set FontScoreActiveSmall = FlexDMD.NewFont("FlexDMD.Resources.udmd-f4by5.fnt", vbWhite, vbWhite, 0)

	Set FontXL5 = FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", vbWhite, vbWhite, 0)
	Set FontXL5Shadow = FlexDMD.NewFont("FlexDMD.Resources.udmd-f12by24.fnt", RGB(16, 16, 16), vbWhite, 0)
	Set FontCool = FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt", vbWhite, vbWhite, 0)
	Set FontCoolShadow = FlexDMD.NewFont("FlexDMD.Resources.bm_army-12.fnt", RGB(16, 16, 16), vbWhite, 0)


	Set DMDScene = FlexDMD.NewGroup("Score")




	Set frameactor = FlexDMD.NewImage("Blank","VPX.XL5_DMD_Blank")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	For i = 1 to 4
		DMDScene.AddActor FlexDMD.NewLabel("Score_" & i, FontScoreInactive, "" )
	Next
	DMDScene.AddActor FlexDMD.NewLabel("Ball", FontScoreInactive, "")
	DMDScene.AddActor FlexDMD.NewLabel("Credit", FontScoreInactive, "")




	Set frameactor = FlexDMD.NewImage("StarfieldUp","VPX.XL5_DMD_Starsvertical")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 128,  430
	Set frameactor = FlexDMD.NewImage("Starfield2","VPX.XL5_DMD_Starfield2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 846, 32
	frameactor.SetPosition -718, 0



	Set frameactor = FlexDMD.NewImage("Planetoid3","VPX.XL5_DMD_Planetoid3")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 80, 20
	frameactor.SetPosition 40, 6

	Set frameactor = FlexDMD.NewImage("Planetoid4","VPX.XL5_DMD_Planetoid4")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("Planetoid5","VPX.XL5_DMD_Planetoid5")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("Planetoid7","VPX.XL5_DMD_Planetoid7")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	Set frameactor = FlexDMD.NewImage("Planetoid6","VPX.XL5_DMD_Planetoid6")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 52, 26
	frameactor.SetPosition 70, 1
	Set frameactor = FlexDMD.NewImage("Planetoid6A","VPX.XL5_DMD_Planetoid6A")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 52, 30
	frameactor.SetPosition 70, 1



	For i = 0 to 27
		Set frameactor = FlexDMD.NewImage("Speedo" & i,"VPX.XL5_DMD_PowerBar" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetPosition 0, 25
	Next

	Set frameactor = FlexDMD.NewImage("SpeedoBG","VPX.XL5_DMD_PowerBarBG")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 0, 25


'### Main splash text label


	Set frameactor = FlexDMD.NewImage("SplashBG_Stripe","VPX.XL5_DMD_BGSplash_stripe")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition -12, 7

	Set frameactor = FlexDMD.NewImage("SplashBG3_Stripe","VPX.XL5_DMD_BGSplash3_stripe")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition -12, 0

	Set frameactor = FlexDMD.NewImage("SplashBG","VPX.XL5_DMD_BGSplash2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("SplashBG3","VPX.XL5_DMD_BGSplash3")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	For i = 1 to 3
		Set frameactor = FlexDMD.NewImage("VenusEyes" & i,"VPX.XL5_DMD_VenusEyes" & i)
		DMDScene.AddActor frameactor
		frameactor.SetSize 128, 32
		frameactor.Visible = 0
	Next

'	Set frameactor = FlexDMD.NewImage("BumperX","VPX.XL5_DMD_BumperX1")
'	DMDScene.AddActor frameactor
'	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("BumperX2","VPX.XL5_DMD_BumperX2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewLabel("BumperScoreShadow", FontXL5Shadow , "")
	DMDScene.AddActor frameactor
	frameactor.Visible = 1
	frameactor.SetPosition 8, 5
	Set frameactor = FlexDMD.NewLabel("BumperScore", FontXL5, "")
	DMDScene.AddActor frameactor
	frameactor.Visible = 1
	frameactor.SetPosition 6, 3






	Set frameactor = FlexDMD.NewImage("BlankBlack","VPX.XL5_DMD_BlankBlack")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("BlankWhite","VPX.XL5_DMD_BlankWhite")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("BlankGrey","VPX.XL5_DMD_BlankGrey")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	DMDScene.AddActor FlexDMD.NewLabel("Splash_S", FontCoolShadow, "")
	DMDScene.AddActor FlexDMD.NewLabel("Splash", FontCool, "")
	DMDScene.AddActor FlexDMD.NewLabel("Splash3_S", FontScoreActiveShadow, "")
	DMDScene.AddActor FlexDMD.NewLabel("Splash3", FontScoreActive, "")
	DMDScene.AddActor FlexDMD.NewLabel("Splash2_S", FontScoreActiveShadow, "")
	DMDScene.AddActor FlexDMD.NewLabel("Splash2", FontScoreActive, "")



	For i = 1 to 11
		Set frameactor = FlexDMD.NewImage("Bloom" & i,"VPX.XL5_DMD_Bloom" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next

	Set frameactor = FlexDMD.NewImage("Bloom12","VPX.XL5_DMD_BlankWhite")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	Set frameactor = FlexDMD.NewImage("MoonBG","VPX.XL5_DMD_HalfMoon")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 128, 32
	frameactor.SetPosition 0, 0

	Set frameactor = FlexDMD.NewImage("Video2BG","VPX.XL5_DMD_Moonscape")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 128, 32
	frameactor.SetPosition 0, 0


	For i = 1 to 50
		Set frameactor = FlexDMD.NewImage("Starburst_" & i,"VPX.XL5_DMD_Warp9&region=" & ((i-1) * 128) & ",0,128,28")
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetSize 128, 32
		frameactor.SetPosition 0, 0
	Next


	For i = 1 to 6
		Set frameactor = FlexDMD.NewImage("EndStarburst_" & i,"VPX.XL5_DMD_WarpEnd" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next


	Set frameactor = FlexDMD.NewImage("Warp","VPX.XL5_DMD_Warp_9")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("Warp_9","VPX.XL5_DMD_Warp_9")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0



	For i = 1 to 2
		Set frameactor = FlexDMD.NewImage("Laser" & i,"VPX.XL5_DMD_Laser" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetSize 16, 26
		frameactor.SetPosition 56, 4
	Next



	Set frameactor = FlexDMD.NewImage("WarpShip1","VPX.XL5_DMD_EB")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 11, 8		'16, 10
'	frameactor.SetSize 46, 30
	Set frameactor = FlexDMD.NewImage("WarpShip4","VPX.XL5_DMD_WarpBirdofPrey")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 12, 5	'19, 8
'	frameactor.SetSize 76, 32
	Set frameactor = FlexDMD.NewImage("WarpShip3","VPX.XL5_DMD_WarpTB2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 18, 5		'24, 6	'32, 8
'	frameactor.SetSize 113, 32

	Set frameactor = FlexDMD.NewImage("WarpShip2","VPX.XL5_DMD_WarpEnterprise")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 24, 6	'32, 8	'49, 12
'	frameactor.SetSize 132, 32

	Set frameactor = FlexDMD.NewImage("WarpShip1L","VPX.XL5_DMD_EB")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 11, 8	'16, 10
'	frameactor.SetSize 42, 32
	Set frameactor = FlexDMD.NewImage("WarpShip4L","VPX.XL5_DMD_WarpBirdofPreyL")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 12, 5	'19, 8
'	frameactor.SetSize 76, 32
	Set frameactor = FlexDMD.NewImage("WarpShip3L","VPX.XL5_DMD_WarpTB2L")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 18, 5	'24, 6
'	frameactor.SetSize 113, 32
	Set frameactor = FlexDMD.NewImage("WarpShip2L","VPX.XL5_DMD_WarpEnterpriseL")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 24, 6	'66, 16
'	frameactor.SetSize 132, 32




	For i = 1 to 22
		Set frameactor = FlexDMD.NewImage("ShipExplode_" & i, "VPX.XL5_DMD_ShipExplode&region=" & ((i-1) * 64) & ",0,64,64")
		DMDScene.AddActor frameactor
		frameactor.SetSize 128, 64
		frameactor.SetPosition 0, -16
		frameactor.Visible = 0
	Next	


	Set frameactor = FlexDMD.NewImage("VideoBGMoon","VPX.XL5_DMD_BGPlanetMoon")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 65, 15 


	For i = 1 to 42
		Set frameactor = FlexDMD.NewImage("Intro_" & i,"VPX.XL5_DMD_Intro000" & i)
		DMDScene.AddActor frameactor
		frameactor.SetSize 128, 32
		frameactor.SetPosition 0, 0
		frameactor.Visible = 0
	Next

	Set frameactor = FlexDMD.NewImage("IntroFireball","VPX.XL5_DMD_IntroFireball")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("IntroFireball2","VPX.XL5_DMD_IntroFireball2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("IntroFireball3","VPX.XL5_DMD_IntroFireball3")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0





	For i = 10 to 60
		Set frameactor = FlexDMD.NewImage("Skillshot_" & i,"VPX.XL5_DMD_FX000" & i)
		DMDScene.AddActor frameactor
		frameactor.SetSize 128, 32
		frameactor.SetPosition 0, 0
		frameactor.Visible = 0
	Next
	

	Set frameactor = FlexDMD.NewImage("VideoBG","VPX.XL5_DMD_BGPlanet3")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 152, 32



	For i = 1 to 5
		Set frameactor = FlexDMD.NewImage("Meteor" & i,"VPX.XL5_DMD_Meteor" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetSize  4, 4
		frameactor.SetPosition 0, 0
	Next





	Set frameactor = FlexDMD.NewImage("Diagonal","VPX.XL5_DMD_Diagonal")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Diagonal2","VPX.XL5_DMD_Diagonal2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("DiagonalAlpha","VPX.XL5_DMD_DiagonalAlpha")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Diagonal2Alpha","VPX.XL5_DMD_Diagonal2Alpha")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	Set frameactor = FlexDMD.NewImage("Horizontal","VPX.XL5_DMD_Horizontal")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 0, 33

	Set frameactor = FlexDMD.NewImage("Highlight","VPX.XL5_DMD_Highlight")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 72, 32






	For i = 1 to 4
		Set frameactor = FlexDMD.NewImage("MeteorExplode_" & i, "VPX.XL5_DMD_Explode_strip&region=" & ((i-1) * 32) & ",0,32,32")
		DMDScene.AddActor frameactor
		frameactor.SetSize 40,40
		frameactor.Visible = 0

		Set frameactor = FlexDMD.NewImage("SkipPlay" & i,"VPX.XL5_DMD_SkipPlay" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next	

	Set frameactor = FlexDMD.NewImage("LabelWarp9","VPX.XL5_DMD_LabelWarp9")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 38, 18
	Set frameactor = FlexDMD.NewImage("LabelMeteor","VPX.XL5_DMD_LabelMeteor")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 38, 18


	For i = 1 to 3
		Set frameactor = FlexDMD.NewImage("VideoSpaceship" & i, "VPX.XL5_DMD_Spaceship_strip&region=" & ((i-1) * 60) & ",0,60,42")
		DMDScene.AddActor frameactor
		frameactor.SetSize 30,21
		frameactor.SetPosition 48, -21
		frameactor.Visible = 0
	Next	



	For i = 1 to 2
		Set frameactor = FlexDMD.NewImage("ExtraBall_" & i,"VPX.XL5_DMD_ExtraBall_" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetSize 64,16
		frameactor.SetPosition 32, 0

		Set frameactor = FlexDMD.NewImage("MBall_" & i,"VPX.XL5_DMD_MultiBall_" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next



	For i = 1 to 2
		Set frameactor = FlexDMD.NewImage("ShootSpinner" & i,"VPX.XL5_DMD_ShootSpinner" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0

		Set frameactor = FlexDMD.NewImage("SpinnerLit" & i,"VPX.XL5_DMD_SpinnerLit" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next

	For i = 1 to 5
		Set frameactor = FlexDMD.NewImage("Spinner" & i,"VPX.XL5_DMD_Spinner" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetPosition 80, 0
	Next


	For i = 1 to 4
		Set frameactor = FlexDMD.NewImage("Skill_" & i,"VPX.XL5_DMD_Skill_" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next

	For i = 1 to 3
		Set frameactor = FlexDMD.NewImage("Ball" & i,"VPX.XL5_DMD_Ball")
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetSize 20, 20
	Next

	Set frameactor = FlexDMD.NewImage("Balls","VPX.XL5_DMD_Balls")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0




	picnum = 0
	For i = 1 to 3
		For j = 1 to 3
			picnum = picnum + 1
			Set frameactor = FlexDMD.NewImage("MeteorExplode2_" & picnum, "VPX.XL5_DMD_Explode_strip2&region=" & ((j-1) * 79) &"," & ((i-1) * 79) & ",79,79")
			DMDScene.AddActor frameactor
			frameactor.SetSize 40,40
			frameactor.Visible = 0

			Set frameactor = FlexDMD.NewImage("Boom_" & picnum, "VPX.XL5_DMD_Explode_strip2&region=" & ((j-1) * 79) &"," & ((i-1) * 79) & ",79,79")
			DMDScene.AddActor frameactor
			frameactor.SetSize 40,40
			frameactor.Visible = 0

			Set frameactor = FlexDMD.NewImage("Warp1Explode_" & picnum, "VPX.XL5_DMD_Explode_strip2&region=" & ((j-1) * 79) &"," & ((i-1) * 79) & ",79,79")
			DMDScene.AddActor frameactor
			frameactor.SetSize 28,28
			frameactor.Visible = 0
			Set frameactor = FlexDMD.NewImage("Warp2Explode_" & picnum, "VPX.XL5_DMD_Explode_strip2&region=" & ((j-1) * 79) &"," & ((i-1) * 79) & ",79,79")
			DMDScene.AddActor frameactor
			frameactor.SetSize 24,24
			frameactor.Visible = 0
			Set frameactor = FlexDMD.NewImage("Warp3Explode_" & picnum, "VPX.XL5_DMD_Explode_strip2&region=" & ((j-1) * 79) &"," & ((i-1) * 79) & ",79,79")
			DMDScene.AddActor frameactor
			frameactor.SetSize 20,20
			frameactor.Visible = 0
			Set frameactor = FlexDMD.NewImage("Warp4Explode_" & picnum, "VPX.XL5_DMD_Explode_strip2&region=" & ((j-1) * 79) &"," & ((i-1) * 79) & ",79,79")
			DMDScene.AddActor frameactor
			frameactor.SetSize 16,16
			frameactor.Visible = 0
		Next
	Next	






	Set frameactor = FlexDMD.NewImage("Meteor","VPX.XL5_DMD_Meteor")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0






	For i = 1 to 4
		Set frameactor = FlexDMD.NewImage("Cockpit" & i,"VPX.cockpit" & i)
		DMDScene.AddActor frameactor
		frameactor.SetSize 128, 32
		frameactor.Visible = 0
	Next




	Set frameactor = FlexDMD.NewImage("VideoModeBG","VPX.XL5_DMD_Videomode")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("Video","VPX.XL5_DMD_Video")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("Game","VPX.XL5_DMD_Game2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("Arcade","VPX.XL5_DMD_Arcade")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0



	Set frameactor = FlexDMD.NewLabel("VideoScore", FontScoreActiveSmall, "0,000")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 52, 1


	For i = 1 to 3
		Set frameactor = FlexDMD.NewImage("Shipicon_" & i, "VPX.XL5_DMD_Shipmini&region=" & ((i-1) * 26) & ",0,26,32")
		DMDScene.AddActor frameactor
		frameactor.SetSize 10,9
		frameactor.SetPosition ((i-1) * 11), 0
		frameactor.Visible = 0
	Next	

	For i = 1 to 2
		Set frameactor = FlexDMD.NewImage("Photon" & i, "VPX.XL5_DMD_Duke")
		DMDScene.AddActor frameactor
		frameactor.SetSize 12, 12
		frameactor.SetPosition 2, (i-1)*14 + 3
		frameactor.Visible = 0
	Next




	For i = 0 to 9
		Set frameactor = FlexDMD.NewImage("Starring" & i,"VPX.XL5_DMD_Starring" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next




	Set frameactor = FlexDMD.NewLabel("Lasers", FontCool, "")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewLabel("1K", FontScoreActiveSmall , "10K") 
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 76, 12
	Set frameactor = FlexDMD.NewLabel("3K", FontScoreActiveSmall , "30K") 
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 72, 8
	Set frameactor = FlexDMD.NewLabel("5K", FontScoreActiveSmall , "50K") 
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 70, 5

	Set frameactor = FlexDMD.NewLabel("MeteorScore", FontCool, "")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0




	For i = 1 to 2
		Set frameactor = FlexDMD.NewImage("GameOver" & i,"VPX.XL5_DMD_GameOver" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next










	For i = 1 to 4
		Set frameactor = FlexDMD.NewImage("Tunnel_" & (5 - i),"VPX.XL5_DMD_Tunnel&region=" & ((i-1) * 128) & ",0,128,32")	'" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next





	Set frameactor = FlexDMD.NewImage("Multiball","VPX.XL5_DMD_Multiball")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Multiball2","VPX.XL5_DMD_Multiball2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	For i = 1 to 3
		Set frameactor = FlexDMD.NewImage("ExtraBall" & i,"VPX.XL5_DMD_ExtraBall" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next



	Set frameactor = FlexDMD.NewImage("BonusX","VPX.XL5_DMD_BonusX")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("2X","VPX.XL5_DMD_2X")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("2BX","VPX.XL5_DMD_2BX")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("3X","VPX.XL5_DMD_3X")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("3BX","VPX.XL5_DMD_3BX")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("5X","VPX.XL5_DMD_5X")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("5BX","VPX.XL5_DMD_5BX")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	Set frameactor = FlexDMD.NewImage("BonusX5","VPX.XL5_DMD_BonusX5")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("BonusX5B","VPX.XL5_DMD_BonusX5B")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0






	For i = 1 to 4	
		Set frameactor = FlexDMD.NewImage("Jackpot" & i,"VPX.XL5_DMD_Jackpot" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next





	Set frameactor = FlexDMD.NewImage("500K","VPX.XL5_DMD_500K")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("500KA","VPX.XL5_DMD_500KA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("1MIL","VPX.XL5_DMD_1MIL")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("1MILA","VPX.XL5_DMD_1MILA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("1BIL","VPX.XL5_DMD_1BIL")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("1BILA","VPX.XL5_DMD_1BILA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("2BIL","VPX.XL5_DMD_2BIL")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("2BILA","VPX.XL5_DMD_2BILA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0



	Set frameactor = FlexDMD.NewImage("Booster","VPX.XL5_DMD_Booster")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Booster2","VPX.XL5_DMD_Booster2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("Skillshot","VPX.XL5_DMD_Skillshot")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("SkillshotA","VPX.XL5_DMD_SkillshotA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("LocksLit","VPX.XL5_DMD_LocksLit")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("LocksLitA","VPX.XL5_DMD_LocksLitA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	Set frameactor = FlexDMD.NewImage("Replay","VPX.XL5_DMD_Replay")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("ReplayA","VPX.XL5_DMD_ReplayA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("SR","VPX.XL5_DMD_SR")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	For i = 0 to 12
		Set frameactor = FlexDMD.NewImage("FireballWiz_" & i,"VPX.XL5_DMD_FireballWiz_" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next


	Set frameactor = FlexDMD.NewImage("ModesBG0","VPX.XL5_DMD_ModesBG0")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("ModesBG1","VPX.XL5_DMD_ModesBG1")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	
	

	For i = 1 to 6
		Set frameactor = FlexDMD.NewImage("ModeComplete" & i,"VPX.XL5_DMD_ModeComplete" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next



	Set frameactor = FlexDMD.NewImage("VenusClip","VPX.XL5_DMD_VenusClip1")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("SteveClip","VPX.XL5_DMD_SteveClip")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("TeamClip","VPX.XL5_DMD_TeamClip")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0



	For i = 0 to 7
		Set frameactor = FlexDMD.NewImage("VenusCap" & i,"VPX.XL5_DMD_VenusCap" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next



	Set frameactor = FlexDMD.NewImage("Venus","VPX.XL5_DMD_Venusface3")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 26, 32
	frameactor.SetPosition 0, 0

	Set frameactor = FlexDMD.NewImage("Venus2","VPX.XL5_DMD_Venusface2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 26, 32
	frameactor.SetPosition 32, 0

	Set frameactor = FlexDMD.NewImage("VenusCaptured","VPX.XL5_DMD_VenusCaptured")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 32, 32
	frameactor.SetPosition 96, 0


	Set frameactor = FlexDMD.NewImage("Alarm2","VPX.XL5_DMD_Alarm1")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Alarm","VPX.XL5_DMD_Alarm")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Alarm1","VPX.XL5_DMD_Alarm1")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0



	Set frameactor = FlexDMD.NewImage("SuperPops","VPX.XL5_DMD_SuperPops")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("SuperPopsA","VPX.XL5_DMD_SuperPopsA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("RandomAward","VPX.XL5_DMD_RandomAward")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("RandomAwardA","VPX.XL5_DMD_RandomAwardA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0



	Set frameactor = FlexDMD.NewImage("Party","VPX.XL5_DMD_Party")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("PartyA","VPX.XL5_DMD_PartyA")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


'#### Barn Doors

	Set frameactor = FlexDMD.NewImage("ScissorLeft","VPX.XL5_DMD_ScissorLeft")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("ScissorRight","VPX.XL5_DMD_ScissorRight")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("WedgeLeftUp","VPX.XL5_DMD_WedgeLeftUp")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("WedgeRightUp","VPX.XL5_DMD_WedgeRightUp")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("WedgeLeft","VPX.XL5_DMD_WedgeLeft")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("WedgeRight","VPX.XL5_DMD_WedgeRight")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("SawTop","VPX.XL5_DMD_LogoTop")	'"VPX.XL5_DMD_SawTop")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("SawBottom","VPX.XL5_DMD_LogoBottom")	'"VPX.XL5_DMD_SawBottom")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("DiamondTop","VPX.XL5_DMD_DiamondTop")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("DiamondBottom","VPX.XL5_DMD_DiamondBottom")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Diamond4Top","VPX.XL5_DMD_Diamond4Top")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Diamond4Bottom","VPX.XL5_DMD_Diamond4Bottom")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0





	For i = 1 to 46
		Set frameactor = FlexDMD.NewImage("Circle" & i,"VPX.XL5_DMD_Radial" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
	Next

	Set frameactor = FlexDMD.NewImage("Circle0","VPX.XL5_DMD_BlankBlack")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	Set frameactor = FlexDMD.NewImage("LowPower","VPX.XL5_DMD_LowPower")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("LowPower2","VPX.XL5_DMD_LowPower2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0



	Set frameactor = FlexDMD.NewImage("Planetoid","VPX.XL5_DMD_Planetoid")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 128, 32

	Set frameactor = FlexDMD.NewImage("Planetoid2","VPX.XL5_DMD_Planetoid2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 128, 32


	Set frameactor = FlexDMD.NewImage("BallLock","VPX.XL5_DMD_BallLockTest")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetSize 128, 32

	Set frameactor = FlexDMD.NewImage("BlankWhiteSmall","VPX.XL5_DMD_BlankWhite38_30")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 1, 1


	For i = 1 to 11
		Set frameactor = FlexDMD.NewImage("BlankWhitePulse_" & i, "VPX.XL5_DMD_BlankWhitePulse" & i)
		DMDScene.AddActor frameactor
		frameactor.SetPosition 1, 1
		frameactor.Visible = 0
		Set frameactor = FlexDMD.NewImage("BlankWhitePulse2_" & i, "VPX.XL5_DMD_BlankWhitePulse" & i)
		DMDScene.AddActor frameactor
		frameactor.SetPosition 88, 1
		frameactor.Visible = 0
	Next



	For i = 1 to 38
		Set frameactor = FlexDMD.NewImage("Char_" & i, "VPX.DMD_Char_" & i + 9)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetSize 24, 32
		frameactor.SetPosition 12, 0
		Set frameactor = FlexDMD.NewImage("Char2_" & i, "VPX.DMD_Char2_" & i)
		DMDScene.AddActor frameactor
		frameactor.Visible = 0
		frameactor.SetSize 28, 28
		frameactor.SetPosition 8, 2
	Next	

	Set frameactor = FlexDMD.NewImage("Char_BG","VPX.DMD_Char_BG")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("Char_BG2","VPX.DMD_Char_BG2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0

	Set frameactor = FlexDMD.NewImage("HS_BG","VPX.DMD_HS_BG")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	Set frameactor = FlexDMD.NewImage("HS_BG2","VPX.DMD_HS_BG2")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0


	Set frameactor = FlexDMD.NewLabel("HSName_S", FontXL5Shadow, "")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 47, 5
	Set frameactor = FlexDMD.NewLabel("HSName", FontXL5, "")
	DMDScene.AddActor frameactor
	frameactor.Visible = 0
	frameactor.SetPosition 45, 3





	Set DMDSplashAF = DMDScene.GetLabel("Splash").ActionFactory

	Set SplashBlink = DMDSplashAF.Sequence()
	SplashBlink.Add DMDSplashAF.Show(True)
	SplashBlink.Add DMDSplashAF.Wait(0.5)
	SplashBlink.Add DMDSplashAF.Show(False)
	SplashBlink.Add DMDSplashAF.Wait(0.5)

	Set SplashBlinkFast = DMDSplashAF.Sequence()
	SplashBlinkFast.Add DMDSplashAF.Show(True)
	SplashBlinkFast.Add DMDSplashAF.Wait(0.125)
	SplashBlinkFast.Add DMDSplashAF.Show(False)
	SplashBlinkFast.Add DMDSplashAF.Wait(0.125)


	Set DMDSplash_SAF = DMDScene.GetLabel("Splash_S").ActionFactory

	Set SplashBlink_S = DMDSplash_SAF.Sequence()
	SplashBlink_S.Add DMDSplash_SAF.Show(True)
	SplashBlink_S.Add DMDSplash_SAF.Wait(0.5)
	SplashBlink_S.Add DMDSplash_SAF.Show(False)
	SplashBlink_S.Add DMDSplash_SAF.Wait(0.5)

	Set SplashBlink_SFast = DMDSplash_SAF.Sequence()
	SplashBlink_SFast.Add DMDSplash_SAF.Show(True)
	SplashBlink_SFast.Add DMDSplash_SAF.Wait(0.125)
	SplashBlink_SFast.Add DMDSplash_SAF.Show(False)
	SplashBlink_SFast.Add DMDSplash_SAF.Wait(0.125)




	Set DMDSplash2AF = DMDScene.GetLabel("Splash2").ActionFactory

	Set SplashBlink2 = DMDSplash2AF.Sequence()
	SplashBlink2.Add DMDSplash2AF.Show(True)
	SplashBlink2.Add DMDSplash2AF.Wait(0.25)
	SplashBlink2.Add DMDSplash2AF.Show(False)
	SplashBlink2.Add DMDSplash2AF.Wait(0.25)

	Set DMDSplash2_SAF = DMDScene.GetLabel("Splash2_S").ActionFactory

	Set SplashBlink2_S = DMDSplash2_SAF.Sequence()
	SplashBlink2_S.Add DMDSplash2_SAF.Show(True)
	SplashBlink2_S.Add DMDSplash2_SAF.Wait(0.25)
	SplashBlink2_S.Add DMDSplash2_SAF.Show(False)
	SplashBlink2_S.Add DMDSplash2_SAF.Wait(0.25)


	Set DMDSplash3AF = DMDScene.GetLabel("Splash3").ActionFactory

	Set SplashBlink3 = DMDSplash3AF.Sequence()
	SplashBlink3.Add DMDSplash3AF.Show(True)
	SplashBlink3.Add DMDSplash3AF.Wait(0.25)
	SplashBlink3.Add DMDSplash3AF.Show(False)
	SplashBlink3.Add DMDSplash3AF.Wait(0.25)

	Set DMDSplash3_SAF = DMDScene.GetLabel("Splash3_S").ActionFactory

	Set SplashBlink3_S = DMDSplash3_SAF.Sequence()
	SplashBlink3_S.Add DMDSplash3_SAF.Show(True)
	SplashBlink3_S.Add DMDSplash3_SAF.Wait(0.25)
	SplashBlink3_S.Add DMDSplash3_SAF.Show(False)
	SplashBlink3_S.Add DMDSplash3_SAF.Wait(0.25)




	FlexDMD.LockRenderThread
	FlexDMD.RenderMode = FlexDMD_RenderMode_DMD_GRAY_4
	FlexDMD.Stage.RemoveAll
	FlexDMD.Stage.AddActor DMDScene

	FlexDMD.Stage.GetLabel("Ball").SetAlignedPosition 2, 32, FlexDMD_Align_BottomLeft
	FlexDMD.Stage.GetLabel("Credit").SetAlignedPosition 127, 32, FlexDMD_Align_BottomRight
	FlexDMD.Stage.GetLabel("Splash").SetAlignedPosition 63, 18, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Splash_S").SetAlignedPosition 65, 20, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Splash2").SetAlignedPosition 63, 8, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Splash2_S").SetAlignedPosition 65, 10, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Splash3").SetAlignedPosition 63, 8, FlexDMD_Align_Center
	FlexDMD.Stage.GetLabel("Splash3_S").SetAlignedPosition 65, 10, FlexDMD_Align_Center
	FlexDMD.Show = True
	FlexDMD.UnlockRenderThread
End Sub


Sub DMDShowScore()	
	Dim i, label
	For i = 1 to 4
		Set label = FlexDMD.Stage.GetLabel("Score_" & i)
		If i = Player Then
			If Not DMDShowSplash Then
				label.Font = FontScoreActive
			Else
				label.Font = FontScoreActiveSmall
			End If
		Else
			label.Font = FontScoreInactive
		End If
		label.Text = FormatNumber(Score(i), 0, -1, 0, -1)
		label.Visible = True
	Next
	FlexDMD.Stage.GetLabel("Score_1").SetAlignedPosition 2, 1, FlexDMD_Align_TopLeft
	FlexDMD.Stage.GetLabel("Score_2").SetAlignedPosition 126, 1, FlexDMD_Align_TopRight
	FlexDMD.Stage.GetLabel("Score_3").SetAlignedPosition 2, 24, FlexDMD_Align_BottomLeft
	FlexDMD.Stage.GetLabel("Score_4").SetAlignedPosition 126, 24, FlexDMD_Align_BottomRight

End Sub


Sub DMDShowBallCred()		
	Dim label, ballcount
	Set label = FlexDMD.Stage.GetLabel("Credit")
	label.Font = FontScoreInactive
	label.Text = "Credits " & Credits
	label.SetAlignedPosition 127, 32, FlexDMD_Align_BottomRight

	ballcount = GameBalls - BallNum(Player)
	If ballcount > BallsPerGame Then ballcount = 0
	Set label = FlexDMD.Stage.GetLabel("Ball")
	label.Text = "Ball " & ballcount
	label.SetAlignedPosition 2, 32, FlexDMD_Align_BottomLeft

End Sub




'#### FlexDMD movies


Sub DMDPlay_XL5()
	DMD_XL5Counter = 0

'	DMDTimer.Enabled = True
End Sub



'######### FX

'#### Barn doors


Dim CircleOut, CircleExit, CircleDelay, DiamondClose, Diamond4Close
Dim ScissorSwap, WedgeSwap, SawSwap, DiamondSwap, Diamond4Swap, Diamond4Exit
Dim DMD_SawCounter, DMD_CircleCounter, DMD_DiamondCounter, DMD_Diamond4Counter, DMD_ScissorCounter, DMD_WedgeCounter



Sub DMD_OpenDiamond()
	DMD_DiamondCounter = 1
	DiamondSwap = True
	DiamondClose = False
	FlexDMD.Stage.GetImage("DiamondTop").SetPosition 0, 0
	FlexDMD.Stage.GetImage("DiamondBottom").SetPosition 0, -16
	FlexDMD.Stage.GetImage("DiamondTop").Visible = 1
	FlexDMD.Stage.GetImage("DiamondBottom").Visible = 1
	FlexDMD.Stage.GetImage("Circle0").Visible = 0
	DMD_Diamond.Interval = 10
	DMD_Diamond.Enabled = True
End Sub

Sub DMD_CloseDiamond()
	DMD_DiamondCounter = 1
	DiamondSwap = True
	DiamondClose = True
	FlexDMD.Stage.GetImage("DiamondTop").SetPosition 0, -48
	FlexDMD.Stage.GetImage("DiamondBottom").SetPosition 0, 32
	FlexDMD.Stage.GetImage("DiamondTop").Visible = 1
	FlexDMD.Stage.GetImage("DiamondBottom").Visible = 1
	DMD_Diamond.Interval = 50
	DMD_Diamond.Enabled = True
End Sub



Sub	DMD_Diamond_Timer					' 1000
	DMD_Diamond.Interval = 10
	If DMD_DiamondCounter = 50 Then 
		FlexDMD.Stage.GetImage("DiamondTop").Visible = 0
		FlexDMD.Stage.GetImage("DiamondBottom").Visible = 0
		DMD_Diamond.Enabled = False
		Exit Sub
	End If

	If DiamondSwap Then
		If DiamondClose Then
			FlexDMD.Stage.GetImage("DiamondTop").SetPosition 0, DMD_DiamondCounter - 48
		Else	
			FlexDMD.Stage.GetImage("DiamondTop").SetPosition 0, -DMD_DiamondCounter
		End If
		DiamondSwap = False
	Else	
		If DiamondClose Then
			FlexDMD.Stage.GetImage("DiamondBottom").SetPosition 0, 32 - DMD_DiamondCounter
		Else
			FlexDMD.Stage.GetImage("DiamondBottom").SetPosition 0, DMD_DiamondCounter - 16
		End If
		DiamondSwap = True
		DMD_DiamondCounter = DMD_DiamondCounter + 1
	End If
End Sub





Sub DMD_OpenDiamond4()
	DMD_Diamond4Counter = 1
	Diamond4Swap = True
	Diamond4Close = False
	FlexDMD.Stage.GetImage("Diamond4Top").SetPosition 0, 0
	FlexDMD.Stage.GetImage("Diamond4Bottom").SetPosition 0, -16
	FlexDMD.Stage.GetImage("Diamond4Top").Visible = 1
	FlexDMD.Stage.GetImage("Diamond4Bottom").Visible = 1
	FlexDMD.Stage.GetImage("Circle0").Visible = 0
	DMD_Diamond4.Interval = 1
	DMD_Diamond4.Enabled = True
End Sub

Sub DMD_CloseDiamond4(num)
	Diamond4Exit = num
	DMD_Diamond4Counter = 1
	Diamond4Swap = True
	Diamond4Close = True
	FlexDMD.Stage.GetImage("Diamond4Top").SetPosition 0, -48
	FlexDMD.Stage.GetImage("Diamond4Bottom").SetPosition 0, 32
	FlexDMD.Stage.GetImage("Diamond4Top").Visible = 1
	FlexDMD.Stage.GetImage("Diamond4Bottom").Visible = 1
	DMD_Diamond4.Interval = 1
	DMD_Diamond4.Enabled = True
	DMD_Diamond4_Timer
End Sub



Sub	DMD_Diamond4_Timer					' 1000
	DMD_Diamond4.Interval = 10
	If DMD_Diamond4Counter = 50 Then 
		If Diamond4Exit > 0 Then
			DMD_Diamond4.Enabled = False
			Select Case Diamond4Exit
				Case 1
					DMD_OpenCircle
				Case 2
					DMD_OpenWedge
				Case 3
					DMD_OpenScissors
				Case 4
					DMD_OpenSaw
				Case 5
					DMD_OpenDiamond
				Case 6
					DMD_OpenDiamond4
			End Select
			Diamond4Exit = 0
		End If
		FlexDMD.Stage.GetImage("Diamond4Top").Visible = 0
		FlexDMD.Stage.GetImage("Diamond4Bottom").Visible = 0
		DMD_Diamond4.Enabled = False
		Exit Sub
	End If

	If Diamond4Swap Then
		If Diamond4Close Then
			FlexDMD.Stage.GetImage("Diamond4Top").SetPosition 0, DMD_Diamond4Counter - 48
		Else	
			FlexDMD.Stage.GetImage("Diamond4Top").SetPosition 0, -DMD_Diamond4Counter
		End If
		Diamond4Swap = False
	Else	
		If Diamond4Close Then
			FlexDMD.Stage.GetImage("Diamond4Bottom").SetPosition 0, 32 - DMD_Diamond4Counter
		Else
			FlexDMD.Stage.GetImage("Diamond4Bottom").SetPosition 0, DMD_Diamond4Counter - 16
		End If
		Diamond4Swap = True
		DMD_Diamond4Counter = DMD_Diamond4Counter + 1
	End If
End Sub




Sub DMD_OpenSaw()				' 1000
	DMD_SawCounter = 1
	SawSwap = True
	FlexDMD.Stage.GetImage("SawTop").SetPosition 0,0
	FlexDMD.Stage.GetImage("SawBottom").SetPosition 0,0
	FlexDMD.Stage.GetImage("SawTop").Visible = 1
	FlexDMD.Stage.GetImage("SawBottom").Visible = 1
	FlexDMD.Stage.GetImage("Circle0").Visible = 0
	DMD_Saw.Interval = 10
	DMD_Saw.Enabled = True
End Sub

Sub	DMD_Saw_Timer
	DMD_Saw.Interval = 15
	If DMD_SawCounter = 33 Then 
		FlexDMD.Stage.GetImage("SawTop").Visible = 0
		FlexDMD.Stage.GetImage("SawBottom").Visible = 0
		DMD_Saw.Enabled = False
		Exit Sub
	End If
	If SawSwap Then
		FlexDMD.Stage.GetImage("SawTop").SetPosition 0, -DMD_SawCounter
		SawSwap = False
	Else	
		FlexDMD.Stage.GetImage("SawBottom").SetPosition 0, DMD_SawCounter
		SawSwap = True
		DMD_SawCounter = DMD_SawCounter + 1
	End If
End Sub




Sub DMD_OpenScissors()					' 1300
	DMD_ScissorCounter = 1
	ScissorSwap = True
	FlexDMD.Stage.GetImage("ScissorLeft").SetPosition 0,0
	FlexDMD.Stage.GetImage("ScissorRight").SetPosition 0,0
	FlexDMD.Stage.GetImage("ScissorLeft").Visible = 1
	FlexDMD.Stage.GetImage("ScissorRight").Visible = 1
	FlexDMD.Stage.GetImage("Circle0").Visible = 0
	DMD_Scissor.Interval = 10
	DMD_Scissor.Enabled = True
End Sub

Sub	DMD_Scissor_Timer
	DMD_Scissor.Interval = 5
	If DMD_ScissorCounter = 130 Then 
		FlexDMD.Stage.GetImage("ScissorLeft").Visible = 0
		FlexDMD.Stage.GetImage("ScissorRight").Visible = 0
		DMD_Scissor.Enabled = False
		Exit Sub
	End If
	If ScissorSwap Then
		FlexDMD.Stage.GetImage("ScissorLeft").SetPosition -DMD_ScissorCounter, 0
		ScissorSwap = False
	Else	
		FlexDMD.Stage.GetImage("ScissorRight").SetPosition DMD_ScissorCounter, 0
		ScissorSwap = True
		DMD_ScissorCounter = DMD_ScissorCounter + 1
	End If
End Sub



Sub DMD_OpenWedge()					' 660
	DMD_WedgeCounter = 1
	WedgeSwap = True
	FlexDMD.Stage.GetImage("WedgeLeft").SetPosition 0,0
	FlexDMD.Stage.GetImage("WedgeRightUp").SetPosition 0,0
	FlexDMD.Stage.GetImage("WedgeLeft").Visible = 1
	FlexDMD.Stage.GetImage("WedgeRightUp").Visible = 1
	FlexDMD.Stage.GetImage("Circle0").Visible = 0
	DMD_Wedge.Interval = 10 
	DMD_Wedge.Enabled = True
End Sub

Sub	DMD_Wedge_Timer
	DMD_Wedge.Interval = 5
	If DMD_WedgeCounter = 130 Then 
		DMD_Wedge.Enabled = False
		FlexDMD.Stage.GetImage("WedgeLeft").Visible = 0
		FlexDMD.Stage.GetImage("WedgeRightUp").Visible = 0
		Exit Sub
	End If
'	If WedgeSwap Then
		FlexDMD.Stage.GetImage("WedgeLeft").SetPosition -DMD_WedgeCounter, 0
'		WedgeSwap = False
'	Else
		FlexDMD.Stage.GetImage("WedgeRightUp").SetPosition DMD_WedgeCounter, 0
'		WedgeSwap = True
		DMD_WedgeCounter = DMD_WedgeCounter + 1
'	End If
End Sub





Sub DMD_OpenCircle()					' 700
	DMD_CircleCounter = -1
	CircleOut = True
	FlexDMD.Stage.GetImage("Circle0").Visible = 0
	DMD_Circle.Interval = 10
	DMD_Circle.Enabled = True
End Sub

Sub DMD_CloseCircle(num)
	DMD_CircleCounter = -1
	CircleExit = num
	DMD_Circle.Interval = 15
	CircleOut = False
	CircleDelay = False
	DMD_Circle.Enabled = True
End Sub


Sub DMD_Circle_Timer
	If DMD_CircleCounter = 46 Then 
		If CircleExit > 0 Then
			Select Case CircleExit
				Case 1
					DMD_OpenCircle
				Case 2
					DMD_OpenWedge
				Case 3
					DMD_OpenScissors
				Case 4
					DMD_OpenSaw
				Case 5
					DMD_OpenDiamond
				Case 6
					DMD_OpenDiamond4
				Case 7
					FlexDMD.Stage.GetImage("Circle0").Visible = 0
			End Select
			DMD_Circle.Enabled = False
			Exit Sub
		End If
		If CircleOut Then
			FlexDMD.Stage.GetImage("Circle46").Visible = 0
			DMD_Circle.Enabled = False
			Exit Sub
		Else
			DMD_Circle.Interval = 200
			DMD_CircleCounter = 45
		End If
	End If
	DMD_CircleCounter = DMD_CircleCounter + 1
	If CircleOut Then
		FlexDMD.Stage.GetImage("Circle" & DMD_CircleCounter).Visible = 1
		If DMD_CircleCounter > 0 Then FlexDMD.Stage.GetImage("Circle" & DMD_CircleCounter - 1).Visible = 0
	Else
		FlexDMD.Stage.GetImage("Circle" & 46 - DMD_CircleCounter).Visible = 1
		If DMD_CircleCounter > 0 Then FlexDMD.Stage.GetImage("Circle" & 47 - DMD_CircleCounter).Visible = 0
	End If
End Sub




Dim BonusCase, DMD_Bonus, DMD_BonusCounter, DMD_BonusCounter2




Sub DMD_ShowBonus()
	If DMDShowVideo Then Exit Sub
	DMDShowVideo = True
	BonusCase = 1
	DMD_BonusCounter = 1
	ShowBonus.Interval = 5
	ShowBonus.Enabled = True
	ShowBonus_Timer
End Sub

Sub ShowBonus_Timer
	Select Case BonusCase

		Case 1
			ShowBonus.Interval = 8
			BlankBlack.Visible = 1
			BonusCase = 2

		Case 2
			If DMD_BonusCounter > 56 Then ShowBonus.Interval = 8 + (DMD_BonusCounter - 56)^2
			If DMD_BonusCounter < 65 Then
				FlexDMD.Stage.GetImage("BonusX").SetSize DMD_BonusCounter*2, DMD_BonusCounter/2
				FlexDMD.Stage.GetImage("BonusX").SetPosition 64 - DMD_BonusCounter, 16 - DMD_BonusCounter/4
				FlexDMD.Stage.GetImage("BonusX").Visible = 1	
				DMD_BonusCounter = DMD_BonusCounter + 1
			Else
				ShowBonus.Interval = 500
				DMD_BonusCounter = 1
				BonusCase = 3
			End If

		Case 3
			If DMD_BonusCounter < 65 Then
				ShowBonus.Interval = 8
				FlexDMD.Stage.GetImage(DMD_Bonus & "X").SetSize DMD_BonusCounter*2, DMD_BonusCounter/2
				FlexDMD.Stage.GetImage(DMD_Bonus & "X").SetPosition 64 - DMD_BonusCounter, 16 - DMD_BonusCounter/4
				FlexDMD.Stage.GetImage(DMD_Bonus & "X").Visible = 1	
				DMD_BonusCounter = DMD_BonusCounter + 2
			Else
				ShowBonus.Interval = 1500
				DMD_SwapFrames DMD_Bonus & "X", DMD_Bonus & "BX", 15, 100
				BonusCase = 4
			End If

		Case 4
			FlexDMD.Stage.GetImage(DMD_Bonus & "X").Visible = 0
			FlexDMD.Stage.GetImage("BonusX").Visible = 0
			BlankBlack.Visible = 0
			DMDShowVideo = False
			ShowScore
			ShowBonus.Enabled = False

	End Select
End Sub




Sub DMDPlay_Starfield()
	FlexDMD.Stage.GetImage("StarfieldUp").Visible = 1
	DMD_StarfieldCounter = 0
	DMD_Starfield.Enabled = True
End Sub

Sub DMDStop_Starfield()
	FlexDMD.Stage.GetImage("StarfieldUp").Visible = 0
	FlexDMD.Stage.GetImage("Starfield2").Visible = 0
	DMD_Starfield.Enabled = False
	DMD_Starfield2.Enabled = False
End Sub

Sub DMD_Starfield_Timer
	DMD_Starfield.Interval = 200
	FlexDMD.Stage.GetImage("StarfieldUp").SetPosition 0, DMD_StarfieldCounter
	DMD_StarfieldCounter = DMD_StarfieldCounter - 1
	If DMD_StarfieldCounter = -398 Then DMD_StarfieldCounter = 0
End Sub

Sub DMDPlay_Starfield2()
	FlexDMD.Stage.GetImage("Starfield2").Visible = 1
	DMD_StarfieldCounter = -718
	DMD_Starfield2.Enabled = True
End Sub


Sub DMD_Starfield2_Timer
	DMD_Starfield2.Interval = 200
	FlexDMD.Stage.GetImage("Starfield2").SetPosition DMD_StarfieldCounter, 0
	DMD_StarfieldCounter = DMD_StarfieldCounter + 1
	If DMD_StarfieldCounter = 0 Then DMD_StarfieldCounter = -718
End Sub



Dim DMD_VideoIntroCounter, DMD_VideoIntroScene, DMD_StarburstCounter
Dim DMD_Photons, DMD_ShipsLocal
Dim DMD_DanceCounter, DMD_WhooshCounter, DMD_Whoosh2Counter, DMD_LaserCounter, DMD_SkillCase



Sub DMDPlay_Whoosh()
	DMD_WhooshCounter = 0
	DMD_Whoosh.Interval = 60 
	DMD_Whoosh.Enabled = True
End Sub

Sub	DMD_Whoosh_Timer
	DMD_WhooshCounter = DMD_WhooshCounter + 1
	If DMD_WhooshCounter = 37 Then 
		FlexDMD.Stage.GetImage("Intro_36").Visible = 0
		DMD_Whoosh.Enabled = False
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("Intro_" & DMD_WhooshCounter).Visible = 1
	If DMD_WhooshCounter > 1 Then FlexDMD.Stage.GetImage("Intro_" & DMD_WhooshCounter - 1).Visible = 0
End Sub

Sub DMDPlay_Whoosh2()
	DMD_Whoosh2Counter = 9
	DMD_Whoosh2.Interval = 60 
	DMD_Whoosh2.Enabled = True
End Sub

Sub	DMD_Whoosh2_Timer
	DMD_Whoosh2Counter = DMD_Whoosh2Counter + 1
	If DMD_Whoosh2Counter = 49 Then 
		FlexDMD.Stage.GetImage("Skillshot_48").Visible = 0
		DMD_Whoosh2.Enabled = False
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("Skillshot_" & DMD_Whoosh2Counter).Visible = 1
	If DMD_Whoosh2Counter > 10 Then FlexDMD.Stage.GetImage("Skillshot_" & DMD_Whoosh2Counter - 1).Visible = 0
End Sub





Dim BallX

Sub DMDPlay_Skillshot()
	If DMDShowVideo Then Exit Sub
	DMDShowVideo = True
	DMD_SkillCase = 0
	DMD_SkillCounter = 0
	BallX = SkillShotTarget
	SkillShotTarget = 0
	SkillShot.Interval = 1
'	FlexDMD.Stage.GetImage("Skill_" & BallX).Visible = 1
	FlexDMD.Stage.GetImage("Ball2").Visible = 1
	FlexDMD.Stage.GetImage("Ball2").SetSize 20, 20
	FlexDMD.Stage.GetImage("Ball2").SetPosition 18 + 36*(BallX - 1), -24 
	DMD_Skill.Interval = 10
	DMD_Skill.Enabled = True
End Sub

Sub DMD_Skill_Timer
	DMD_Skill.Interval = 6
	DMD_SkillCounter = DMD_SkillCounter + 1
	Select Case DMD_SkillCase
		Case 0
			FlexDMD.Stage.GetImage("Skill_" & BallX).Visible = 1
			FlexDMD.Stage.GetImage("Ball2").SetPosition 19 + 36*(BallX - 1), DMD_SkillCounter - 24 
			If DMD_SkillCounter = 60 Then 
				FlexDMD.Stage.GetImage("Ball2").Visible = 0
				DMD_SkillCounter = 40
				DMD_SkillCase = 1
			End If
		Case 1
			DMD_Skill.Interval = 500
			FlexDMD.Stage.GetImage("Skill_" & BallX).Visible = 0
			DMD_SwapFrames "Skillshot", "SkillshotA", 10, 100
			DMD_SkillCase = 2
		Case 2
			DMDShowVideo = False
'			DMD_SkillCase = 3
			DMD_Skill.Enabled = False
		Case 3
			DMDShowVideo = False
			DMD_Skill.Enabled = False
	End Select
End Sub


Sub DMDPlay_Starburst()
	DMD_StarburstCounter = 0
	DMD_Starburst.Enabled = True
End Sub

Sub DMD_Starburst_Timer
	DMD_Starburst.Interval = 20
	DMD_StarburstCounter = DMD_StarburstCounter + 1
	If DMD_StarburstCounter = 51 Then 
		DMD_StarburstCounter = 1 
		FlexDMD.Stage.GetImage("Starburst_50").Visible = 0
	End If
	FlexDMD.Stage.GetImage("Starburst_" & DMD_StarburstCounter).SetPosition 0, 0
	FlexDMD.Stage.GetImage("Starburst_" & DMD_StarburstCounter).Visible = 1
	If DMD_StarburstCounter > 1 Then FlexDMD.Stage.GetImage("Starburst_" & DMD_StarburstCounter - 1).Visible = 0
End Sub

Sub DMD_Stop_Starburst()

	FlexDMD.Stage.GetImage("Starburst_" & DMD_StarburstCounter).Visible = 0
	DMD_StarburstCounter = 0
	DMD_Starburst.Enabled = False

	DMD_EndStarburst.Interval = 20
	DMD_EndStarburst.Enabled = True
	DMD_EndStarburst_Timer
End Sub

Sub DMD_EndStarburst_Timer
	DMD_EndStarburst.Interval = 100
	DMD_StarburstCounter = DMD_StarburstCounter + 1
	If DMD_StarburstCounter = 7 Then 
		DMD_StarburstCounter = 0 
		FlexDMD.Stage.GetImage("EndStarburst_6").Visible = 0
		DMD_EndStarburst.Enabled = False
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("EndStarburst_" & DMD_StarburstCounter).Visible = 1
	If DMD_StarburstCounter > 1 Then FlexDMD.Stage.GetImage("EndStarburst_" & DMD_StarburstCounter - 1).Visible = 0
End Sub




'############# Games

Sub DMD_VideoGame(vgame)
	Dim i,j
	StopSound CurrentMusic
	MusicTimer.Enabled = False
	If PlayfieldCounter > 1 Then
		PlayfieldTimer.Enabled = False
	End If
	DMD_MenuSwap = True
	Select Case vgame
		Case 1
			DMD_ShipXY = 48
			DMD_ShipXY_Old = DMD_ShipXY
			DMD_ShipZ = -25
			DMD_Ship_Old = "VideoSpaceship1"
			DMD_Shiphit = 0
			DMD_ShipsLocal = DMD_Ships(Player)
			DMD_VideoScene = 0
			DMD_VideoIntroScene = 1
			DMD_VideoCounter = 0
			DMD_VideoIntroCounter = 2
			DMD_Mgap = 30
			MeteorNum = 0
			MaxMeteors = 0
			DMD_MeteorCounter = 0
			DMD_MeteorTotal = 0

			FlexDMD.Stage.GetImage("VideoSpaceship1").Visible = 1
			FlexDMD.Stage.GetImage("VideoSpaceship2").Visible = 1
			FlexDMD.Stage.GetImage("VideoSpaceship3").Visible = 1
			FlexDMD.Stage.GetImage("VideoSpaceship1").SetPosition DMD_ShipXY, DMD_ShipZ

			For i = 1 to 5
				For j = 0 to 2
					MeteorSize(i,j) = 0
				Next
			Next
			DMDPlay_Starfield
			BlankBlack.Visible = 0
			DMD_Video1.Interval = 30
			DMD_Video1.Enabled = True

		Case 2
			DMD_VideoScore = 0
			DMD_VideoScene = 0
			DMD_VideoCounter = 2
			DMD_VideoIntroCounter = 33
			DMD_Photons = 2
			DMD_LaserCounter = DMD_Lasershots(Player)
			DMD_WarpEball = False
			DMDPlay_Starfield
			DMD_Video2.Interval = 30
			DMD_Video2.Enabled = True

	End Select
End Sub








'########################## Warp 9



Dim DMD_VideoCounter2


Sub DMD_Video2_Timer
	Dim i
	Select Case DMD_VideoScene
		Case 0
			DMD_Video2.Interval = 30
			DMD_VideoCounter = DMD_VideoCounter + 2
			If DMD_VideoCounter = 4 Then
				PlaySound "XL5_DMD_8bitShort1"
			ElseIf DMD_VideoCounter < 65 Then
				FlexDMD.Stage.GetImage("Video").SetSize DMD_VideoCounter*2, DMD_VideoCounter/2
				FlexDMD.Stage.GetImage("Video").SetPosition 64 - DMD_VideoCounter, 16 - DMD_VideoCounter/4
				FlexDMD.Stage.GetImage("Video").Visible = 1	
			ElseIf DMD_VideoCounter < 121 Then
				DMD_Video2.Interval = 20
				FlexDMD.Stage.GetImage("Game").SetSize 2 * (DMD_VideoCounter - 64), (DMD_VideoCounter - 64)/2
				FlexDMD.Stage.GetImage("Game").SetPosition 64 - (DMD_VideoCounter - 64), 16 - (DMD_VideoCounter - 64)/4
				FlexDMD.Stage.GetImage("Game").Visible = 1	
			Else
				DMD_Video2.Interval = 1800 
				DMD_VideoCounter = 2
				DMD_VideoScene = 1
			End If
		Case 1
			DMD_CloseCircle(6)
			DMD_Video2.Interval = 750
			DMD_VideoCounter = 0
			DMD_VideoScore = 0
			DMD_VideoMode = True
			DMD_VideoScene = 2
		Case 2
			DMD_Video2.Interval = 50 
			FlexDMD.Stage.GetImage("Diagonal").SetPosition DMD_VideoCounter - 12, 0
			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = 11 Then 
				DMD_VideoCounter = 0
				If DMD_MenuSwap Then
					FlexDMD.Stage.GetImage("SkipPlay1").Visible = 1
					FlexDMD.Stage.GetImage("SkipPlay2").Visible = 0
					DMD_MenuSwap = False
				Else
					FlexDMD.Stage.GetImage("SkipPlay1").Visible = 0
					FlexDMD.Stage.GetImage("SkipPlay2").Visible = 1
					DMD_MenuSwap = True
				End If
			End If
			If DMD_VideoCounter = 1 And DMD_VideoScore = 0 Then
				PlaySound "XL5_DMD_8bitMenu", -1
				FlexDMD.Stage.GetImage("Video").Visible = 0	
				FlexDMD.Stage.GetImage("Game").Visible = 0	
				DMD_VideoScore = 1
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 1
				FlexDMD.Stage.GetImage("LabelWarp9").Visible = 1	
				FlexDMD.Stage.GetImage("Diagonal").Visible = 1
				DMDPlay_Starburst
			End If
			If DMD_VideoControlLeft Then
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 0
				FlexDMD.Stage.GetImage("SkipPlay1").Visible = 1
				StopSound "XL5_DMD_8bitMenu"
				DMD_CloseDiamond4(3)
				DMD_Video2.Interval = 1000
				DMD_VideoScene = 15
			End If	
			If DMD_VideoControlRight Then 
				FlexDMD.Stage.GetImage("SkipPlay1").Visible = 0
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 1
				StopSound "XL5_DMD_8bitMenu"
				DMD_VideoCounter = 2
				DMD_CloseCircle(1)
				DMD_Video2.Interval = 750
				DMD_VideoScene = 3
			End If
		Case 3
			DMD_VideoMode = False
			DMD_Video2.Interval = 20 
			DMD_VideoCounter = DMD_VideoCounter + 2
			If DMD_VideoCounter = 4 Then
				FlexDMD.Stage.GetImage("SkipPlay1").Visible = 0
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 0
				FlexDMD.Stage.GetImage("LabelWarp9").Visible = 0	
				FlexDMD.Stage.GetImage("Diagonal").Visible = 0
				PlaySound "XL5_DMD_8bitShort2", 0.99
			ElseIf DMD_VideoCounter < 65 Then
				FlexDMD.Stage.GetImage("Warp").SetSize DMD_VideoCounter*2, DMD_VideoCounter/2
				FlexDMD.Stage.GetImage("Warp").SetPosition 64 - DMD_VideoCounter, 16 - DMD_VideoCounter/4
				FlexDMD.Stage.GetImage("Warp").Visible = 1	
			Else
				DMD_Video2.Interval = 2000 
				DMD_VideoCounter = 64
				DMD_VideoScore = 0
				DMD_VideoScene = 4
			End If


		Case 4
			DMD_Video2.Interval = 20 
			DMD_VideoCounter = DMD_VideoCounter - 4

			If DMD_VideoCounter > 0 Then
				FlexDMD.Stage.GetImage("Warp").SetSize DMD_VideoCounter*2, DMD_VideoCounter/2
				FlexDMD.Stage.GetImage("Warp").SetPosition 64 - DMD_VideoCounter, 16 - DMD_VideoCounter/4
			Else
				FlexDMD.Stage.GetImage("Warp").Visible = 0
				FlexDMD.Stage.GetImage("Cockpit1").Visible = 1
				PlaySound "XL5_DMD_8bit2", -1
				DMD_Video2.Interval = 1600 
				DMD_VideoScene = 5
			End If

		Case 5
			DMD_Video2.Interval = 400
			DMD_Stop_Starburst
			PlaySound "XL5_Whoosh_LR", 0.99
			DMD_VideoScene = 6

		Case 6
			DMD_Video2.Interval = 80 
			DMD_StartWarp
			FlexDMD.Stage.GetImage("Photon1").Visible = 1
			FlexDMD.Stage.GetImage("Photon2").Visible = 1
			FlexDMD.Stage.GetLabel("VideoScore").Text = "000"
			FlexDMD.Stage.GetLabel("VideoScore").Visible = 1
			FlexDMD.Stage.GetLabel("Lasers").Text = DMD_LaserCounter
			FlexDMD.Stage.GetLabel("Lasers").Visible = 1

			DMD_VideoMode = True
			DMD_VideoScene = 7

		Case 7
			Dim ly, realscore
			DMD_Video2.Interval = 25
			If DMD_VideoControlRight Then 
				DMD_Video2.Interval = 1000
				DMD_LaserCounter = DMD_LaserCounter - 1
				ly = 7
				If Warp2Counter > 50 And Warp2Counter < 80 Then
					DMD_Warp2_Explode
					DMD_VideoScore = DMD_VideoScore + 1
					ly = 12
				ElseIf Warp3Counter > 56 And Warp3Counter < 75 Then
					DMD_Warp3_Explode
					DMD_VideoScore = DMD_VideoScore + 3
					ly = 9
				ElseIf Warp4Counter > 64 And Warp4Counter < 75 Then
					DMD_Warp4_Explode
					DMD_VideoScore = DMD_VideoScore + 5
					ly = 8
				ElseIf Warp1Counter > 55 And Warp1Counter < 75 Then
					DMD_Warp1_Explode
					ly = 7
				End If
				If DMD_VideoScore >= 40 Then DMD_Warp1.Enabled = True
				FlexDMD.Stage.GetImage("Laser1").SetPosition 54, ly
				FlexDMD.Stage.GetImage("Laser2").SetPosition 54, ly
				FlexDMD.Stage.GetImage("Laser1").SetSize 20, 30 - ly
				FlexDMD.Stage.GetImage("Laser2").SetSize 20, 30 - ly

				DMD_SwapFrames "Laser1", "Laser2", 2, 80
				PlaySound "XL5_DMD_Laser", 0.9

			End If

			If DMD_VideoControlLeft And DMD_Photons > 0 Then
				PlaySound "XL5_DMD_Photons", 0.99
'				DMD_SwapFrames "Photon" & DMD_Photons, "Blank", 5, 200
				FlexDMD.Stage.GetImage("Photon" & 3 - DMD_Photons).Visible = 0
				DMD_Photons = DMD_Photons - 1
				If Warp2Counter > -30 And Warp2Counter < 128 Then DMD_VideoScore = DMD_VideoScore + 1
				If Warp3Counter > -30 And Warp3Counter < 128 Then DMD_VideoScore = DMD_VideoScore + 3
				If Warp4Counter > -30 And Warp4Counter < 128 Then DMD_VideoScore = DMD_VideoScore + 5
				DMD_Video2.Interval = 1000
				DMD_PhotonExplode
				If DMD_VideoScore >= 40 Then DMD_Warp1.Enabled = True
			End If

			realscore =	DMD_VideoScore * 10	
			If realscore > 999 Then
				DMD_Scorestring = Left(realscore, Len(realscore) - 3) & "," & Right(realscore, 3)
			Else
				DMD_Scorestring = realscore
			End If
			FlexDMD.Stage.GetLabel("VideoScore").Text = DMD_Scorestring & ",000"
			FlexDMD.Stage.GetLabel("VideoScore").SetAlignedPosition 63, 3, FlexDMD_Align_Center
			FlexDMD.Stage.GetLabel("Lasers").Text = DMD_LaserCounter
			FlexDMD.Stage.GetLabel("Lasers").SetAlignedPosition 126, 28, FlexDMD_Align_BottomRight



			If DMD_LaserCounter = 0 Then 
				DMD_Video2.Interval = 500
				DMD_VideoScene = 8
			End If

		Case 8
			DMD_Video2.Interval = 750
			DMD_VideoCounter = -1
			DMD_VideoCounter2 = 0
			DMD_CloseCircle(5)
			DMD_VideoScene = 10

		Case 10
			DMD_Video2.Interval = 40
			FlexDMD.Stage.GetImage("Diagonal").SetPosition DMD_VideoCounter - 12, 0
			DMD_VideoCounter = DMD_VideoCounter + 1

			If DMD_VideoCounter = 0 Then
				StopSound "XL5_DMD_8bit2"	'"XL5_Incidental_1min"
				PlaySound "XL5_DMD_8bitShort2"
				FlexDMD.Stage.GetImage("Diagonal").Visible = 1
				DMD_SwapFrames "GameOver1", "GameOver2", 30, 100
				DMD_Warp1.Enabled = False
				DMD_Warp2.Enabled = False
				DMD_Warp3.Enabled = False
				DMD_Warp4.Enabled = False
				For i = 1 to 4
					WarpShip(i,1).Visible = 0
					WarpShip(i,2).Visible = 0
				Next
				FlexDMD.Stage.GetLabel("VideoScore").Visible = 0
				FlexDMD.Stage.GetImage("Cockpit1").Visible = 0
				FlexDMD.Stage.GetImage("Photon1").Visible = 0
				FlexDMD.Stage.GetImage("Photon2").Visible = 0
				FlexDMD.Stage.GetLabel("Lasers").Visible = 0
			End If
			If DMD_VideoCounter2 = 5 And DMD_VideoCounter = 2 Then DMD_CloseCircle(2)
			If DMD_VideoCounter = 11 Then
				DMD_VideoCounter = 0
				DMD_VideoCounter2 = DMD_VideoCounter2 + 1
				If DMD_VideoCounter2 = 7 Then DMD_VideoScene = 11
			End If

		Case 11
			DMD_Video2.Interval = 2250
			FlexDMD.Stage.GetImage("Diagonal").Visible = 0
			DMD_VideoScore  = DMD_VideoScore  * ScoreX
			DMDBlinkSplashString DMD_Scorestring & ",000", 3, True, False
			Score(Player) = Score(Player) + DMD_VideoScore * 10000 
			ShowScore
			DMD_VideoScene = 16




		Case 15
			DMD_Video2.Interval = 3000
			FlexDMD.Stage.GetImage("Starburst_" & DMD_StarburstCounter).Visible = 0
			DMD_StarburstCounter = 0
			DMD_Starburst.Enabled = False
			FlexDMD.Stage.GetImage("SkipPlay1").Visible = 0
			FlexDMD.Stage.GetImage("SkipPlay2").Visible = 0
			FlexDMD.Stage.GetImage("LabelWarp9").Visible = 0	
			FlexDMD.Stage.GetImage("Diagonal").Visible = 0
			DMDBlinkSplashString "250,000", 3, True, False
			Score(Player) = Score(Player) + 250000 * ScoreX
			ShowScore
			DMD_VideoScene = 16

		Case 16
			DMD_Video2.Interval = 750
			DMD_CloseCircle(2)
			DMD_VideoScene = 17

		Case 17
			DMD_Video2.Interval = 3000
			PlaySound "XL5_DMD_8bitShort1"
			Modes(Player) = Modes(Player) + 1
			DMDDoubleSplashString "MODE 4", "COMPLETE", True, 3, True
			If Modes(Player) = ModesTotal Then
				Flash4Modes(9)
				FlashSteveVenus 3, 9
				DMD_VideoScene = 31
			Else
				Flash1Mode 4, 9
				FlashSteveVenus 2, 9
				DMD_VideoScene = 18
			End If

		Case 18
			DMD_Video2.Interval = 3550 
			DMD_ShowModeComplete
			DMD_VideoScene = 19

		Case 19
			DMD_Video2.Interval = 450 
			DMDPlay_FlybyLeft
			DMD_VideoScene = 20


		Case 20
			DMD_Video2.Interval = 1500 
			DMDBlinkSplashString "BALL EJECTING", 2, True, True
			DMD_VideoMode = False
			DMD_VideoScene = 21

		Case 21
			InMode = True
			DMD_Video2.Interval = 10000 	'Mode Cooldown
			DMD_VideoScene = 22
			ExitVideoGame

		Case 22
			InMode = False
			DMD_Video2.Enabled = False





		Case 31
			DMD_Video2.Interval = 3550 
			DMD_ShowModeComplete
			DMD_VideoScene = 32


		Case 32
			DMD_Video2.Interval = 450
			FlashSteveVenus 6, 5
			DMDPlay_FlybyLeft
			DMD_VideoScene = 33


		Case 33
			DMD_Video2.Interval = 2000
			DMDDoubleSplashString "W I Z A R D", "MODE", True, 2, True
			DMDStop_Starfield
			PlaySound "XL5_Upbeat"
			DMD_VideoCounter = 1
			DMD_VideoScene = 34

		Case 34
			DMD_Video2.Interval = 250
			If ShowFlexDMD Then DMDDoubleSplashString "TIME", Left(". . . . . . . . ", DMD_VideoCounter * 2), True, 2, True
			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = 9 Then
				DMD_Video1.Interval = 100
				DMD_VideoScene = 35
			End If

		Case 35
			DMD_Video2.Interval = 3550
			If ShowFlexDMD Then DMDDoubleSplashString "TO EARN", "YOUR WINGS", True, 4, True
			DMD_VideoScene = 36

		Case 36
			DMD_Video2.Interval = 450
			DMDPlay_FlybyLeft
			DMD_VideoScene = 44


		Case 44
			DMD_Video2.Interval = 3000
			If ShowFlexDMD Then DMDDoubleSplashString "COLLECT NUMBERS", "1 to 8", False, 3, True
			DMD_VideoCounter = 0
			DMD_VideoScene = 45

		Case 45
			DMD_Video2.Interval = 250
			If ShowFlexDMD Then DMDDoubleSplashString "TO SPELL", Left("FIREBALL", DMD_VideoCounter), True, 1, True
			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = 8 Then
				If ShowFlexDMD Then DMDDoubleSplashString "TO SPELL", "FIREBALL", True, 3, True
				DMD_Video2.Interval = 2550
				DMD_VideoScene = 56
			End If



		Case 56
			DMD_Video2.Interval = 450
			DMDPlay_FlybyLeft
			DMD_VideoScene = 60

		Case 60
			BallRelease.CreateBall
			BallRelease.Kick 90, 10
			Ball_Locked = 2
			Ballsave_Light.state = 0
			BallSave.Enabled = False
			Ballsaver = 99
			SoundBallRelease
			DMD_VideoMode = False
			DMDStop_Starfield
			WizardActive = True
			KickBack.Enabled = True
			KickBack2.Enabled = True
			KickBack_Light.State = 1
			KickBack2_Light.State = 1
			Modes(Player) = 0
			ChaseOn = 1
			Chaser001.TimerEnabled = True
			ChaseCounter = 0
			If PlayfieldCounter > 1 Then PlayfieldTimer.Enabled = True
			MusicTimer.Enabled = True
			DMD_Video2.Enabled = False


	End Select
End Sub


Sub ExitVideoGame()
'	DMD_CloseDiamond
	ExitVGame.Interval = 100
	ExitVGame.Enabled = True
End Sub

Sub ExitVGame_Timer
	MusicTimer.Interval = 1000
	MusicTimer.Enabled = True
	DMDStop_Starfield
	SoundSaucerHit(2)
	Kicker001.Kick 35, 20
	Kicker002.Kick 270, 6 + Int(Rnd*3)
	If PlayfieldCounter > 1 Then
		PlayfieldTimer.Enabled = True
	End If
	ExitVGame.Enabled = False
End Sub


Dim WarpCounter, Warp1Counter, Warp2Counter, Warp3Counter, Warp4Counter 
Dim WarpSwap1, WarpSwap2, WarpSwap3, WarpSwap4
Dim DMD_Warp1ExplodeCounter, DMD_Warp2ExplodeCounter, DMD_Warp3ExplodeCounter, DMD_Warp4ExplodeCounter, DMD_WarpEball
Dim DMD_PhotonExplodeCounter, DMD_MenuSwap
Dim MBallCase, DMD_MBallCounter2, DMD_SpinCounter2, DMD_SpinCase, DMD_FXCase, DMD_FXCounter
Dim DMD_Scorestring, DMD_SplashCheck
Dim DMD_MeteorScore, DMD_MeteorCounter, DMD_MeteorTotal, MeteorHSString
Dim DMD_ShipExplodeCounter, DMD_BoomX, DMD_BoomZ, DMD_ShipInterval, MaxMeteors
Dim DMD_FXCounter2, DMD_FXNum
Dim WarpShip(4,2)


Sub DMD_Warp1_Explode()
	If DMD_WarpEball Then Exit Sub
	DMD_WarpEball = True
	SoundKnocker
	DMD_Warp1ExplodeCounter = 0
	DMD_Warp1Explode.Enabled = True
	DMD_Warp1Explode_Timer
End Sub

Sub DMD_Warp1Explode_Timer
	DMD_Warp1Explode.Interval = 60
	DMD_Warp1ExplodeCounter = DMD_Warp1ExplodeCounter + 1
	If DMD_Warp1ExplodeCounter = 4 Then
		DMD_SwapFrames "ExtraBall_1", "ExtraBall_2", 10, 50
	End If
	If DMD_Warp1ExplodeCounter = 5 Then
		DMD_Warp1.Enabled = False	'Interval = Int(Rnd*2000) + 1000
		WarpShip(1,1).SetPosition 130,0
		WarpShip(1,2).SetPosition 130,0
		Warp1Counter = 0
	End If
	If DMD_Warp1ExplodeCounter = 10 Then
		FlexDMD.Stage.GetImage("Warp1Explode_9").Visible = 0
		DMD_Warp1Explode.Enabled = False
		Extra_Ball
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("Warp1Explode_" & DMD_Warp1ExplodeCounter).SetPosition 48, 8
	FlexDMD.Stage.GetImage("Warp1Explode_" & DMD_Warp1ExplodeCounter).Visible = 1
	If DMD_Warp1ExplodeCounter > 1 Then FlexDMD.Stage.GetImage("Warp1Explode_" & DMD_Warp1ExplodeCounter - 1).Visible = 0
End Sub

Sub DMD_Warp2_Explode()
	PlaySound "XL5_DMD_Explosion1"
	FlexDMD.Stage.GetLabel("1K").Visible = 1
	DMD_Warp2ExplodeCounter = 0
	DMD_Warp2Explode.Enabled = True
	DMD_Warp2Explode_Timer
End Sub

Sub DMD_Warp2Explode_Timer
	DMD_Warp2Explode.Interval = 60
	DMD_Warp2ExplodeCounter = DMD_Warp2ExplodeCounter + 1
	If DMD_Warp2ExplodeCounter = 4 Then
		DMD_SwapFrames "Cockpit3", "Blank", 6, 60
	End If
	If DMD_Warp2ExplodeCounter = 5 Then
		DMD_Warp2.Interval = Int(Rnd*2000) + 1000
		WarpShip(2,1).SetPosition 130,0
		WarpShip(2,2).SetPosition 130,0
		Warp2Counter = 0
	End If
	If DMD_Warp2ExplodeCounter = 10 Then
		FlexDMD.Stage.GetLabel("1K").Visible = 0
		FlexDMD.Stage.GetImage("Warp2Explode_9").Visible = 0
		DMD_Warp2Explode.Enabled = False
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("Warp2Explode_" & DMD_Warp2ExplodeCounter).SetPosition 50, 7
	FlexDMD.Stage.GetImage("Warp2Explode_" & DMD_Warp2ExplodeCounter).Visible = 1
	If DMD_Warp2ExplodeCounter > 1 Then FlexDMD.Stage.GetImage("Warp2Explode_" & DMD_Warp2ExplodeCounter - 1).Visible = 0
End Sub

Sub DMD_Warp3_Explode()
	PlaySound "XL5_DMD_Explosion2"
	FlexDMD.Stage.GetLabel("3K").Visible = 1
	DMD_Warp3ExplodeCounter = 0
	DMD_Warp3Explode.Enabled = True
	DMD_Warp3Explode_Timer
End Sub

Sub DMD_Warp3Explode_Timer
	DMD_Warp3Explode.Interval = 60
	DMD_Warp3ExplodeCounter = DMD_Warp3ExplodeCounter + 1
	If DMD_Warp3ExplodeCounter = 4 Then
		DMD_SwapFrames "Cockpit2", "Blank", 5, 60
	End If
	If DMD_Warp3ExplodeCounter = 5 Then
		DMD_Warp3.Interval = Int(Rnd*2000) + 500
		WarpShip(3,1).SetPosition 130,0
		WarpShip(3,2).SetPosition 130,0
		Warp3Counter = 0
	End If
	If DMD_Warp3ExplodeCounter = 10 Then
		FlexDMD.Stage.GetLabel("3K").Visible = 0
		FlexDMD.Stage.GetImage("Warp3Explode_9").Visible = 0
		DMD_Warp3Explode.Enabled = False
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("Warp3Explode_" & DMD_Warp3ExplodeCounter).SetPosition 52, 5
	FlexDMD.Stage.GetImage("Warp3Explode_" & DMD_Warp3ExplodeCounter).Visible = 1
	If DMD_Warp3ExplodeCounter > 1 Then FlexDMD.Stage.GetImage("Warp3Explode_" & DMD_Warp3ExplodeCounter - 1).Visible = 0
End Sub

Sub DMD_Warp4_Explode()
	PlaySound "XL5_DMD_Explosion3"
	FlexDMD.Stage.GetLabel("5K").Visible = 1
	DMD_Warp4ExplodeCounter = 0
	DMD_Warp4Explode.Enabled = True
	DMD_Warp4Explode_Timer
End Sub

Sub DMD_Warp4Explode_Timer
	DMD_Warp4Explode.Interval = 60
	DMD_Warp4ExplodeCounter = DMD_Warp4ExplodeCounter + 1
	If DMD_Warp4ExplodeCounter = 5 Then
		DMD_Warp4.Interval = Int(Rnd*2000) + 500
		WarpShip(4,1).SetPosition 130,0
		WarpShip(4,2).SetPosition 130,0
		Warp4Counter = 0
	End If
	If DMD_Warp4ExplodeCounter = 10 Then
		FlexDMD.Stage.GetLabel("5K").Visible = 0
		FlexDMD.Stage.GetImage("Warp4Explode_9").Visible = 0
		DMD_Warp4Explode.Enabled = False
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("Warp4Explode_" & DMD_Warp4ExplodeCounter).SetPosition 54, 4
	FlexDMD.Stage.GetImage("Warp4Explode_" & DMD_Warp4ExplodeCounter).Visible = 1
	If DMD_Warp4ExplodeCounter > 1 Then FlexDMD.Stage.GetImage("Warp4Explode_" & DMD_Warp4ExplodeCounter - 1).Visible = 0
End Sub



Sub DMD_PhotonExplode
	Dim i, j
	PlaySound "XL5_Boom1"
	DMDPlay_Whoosh2

	DMD_SwapFrames "Cockpit4", "Cockpit2", 40, 60
	For i = 1 to 4
		For j = 1 to 2
			WarpShip(i,j).SetPosition 130,0
		Next
	Next
	DMD_Warp2.Interval = Int(Rnd*1500) + 2500
	DMD_Warp3.Interval = Int(Rnd*1500) + 3000
	DMD_Warp2.Interval = Int(Rnd*1500) + 3200
	DMD_Warp2_Explode
	DMD_Warp3_Explode
	DMD_Warp4_Explode
'	DMD_PhotonExplodeCounter = 0
'	DMD_Photon.Enabled = True
'	DMD_Photon_Timer
End Sub




Sub DMD_StartWarp()
	Dim i
	WarpCounter = 0

	For i = 1 to 4
		Set WarpShip(i, 1) = FlexDMD.Stage.GetImage("WarpShip" & i)
		Set WarpShip(i, 2) = FlexDMD.Stage.GetImage("WarpShip" & i & "L")
		WarpShip(i,1).SetPosition 130,0
		WarpShip(i,2).SetPosition 130,0
		WarpShip(i,1).Visible = 1
		WarpShip(i,2).Visible = 1
	Next

	WarpSwap1 = False
	WarpSwap2 = True
	WarpSwap3 = False
	WarpSwap4 = True

	DMD_Warp1.Interval = 10
'	DMD_Warp1.Enabled = True
	DMD_Warp2.Interval = 300
	DMD_Warp2.Enabled = True
	DMD_Warp3.Interval = 500
	DMD_Warp3.Enabled = True
	DMD_Warp4.Interval = 100
	DMD_Warp4.Enabled = True
End Sub

Sub DMD_Warp1_Timer
	If DMD_WarpEball Then 
		DMD_Warp1.Enabled = False
		Exit Sub
	End If
	DMD_Warp1.Interval = 20       
	Warp1Counter = Warp1Counter + 1
	WarpShip(1,2).SetPosition Warp1Counter - 6, 4
	If Warp1Counter = 145 Then
		Warp1Counter = 0
		DMD_Warp1.Interval = Int(Rnd*4000) + 2000
	End If
End Sub

Sub DMD_Warp2_Timer
	DMD_Warp2.Interval = 12
	Warp2Counter = Warp2Counter + 1
	If 	WarpSwap2 Then
		WarpShip(2,1).SetPosition 118 - Warp2Counter, 10
	Else
		WarpShip(2,2).SetPosition Warp2Counter - 16, 10	'Warp2Counter - 22
	End If
	If Warp2Counter = 158 Then
		Warp2Counter = 0
		If Int(Rnd*11) > 5 Then WarpSwap2 = Not WarpSwap2
		DMD_Warp2.Interval = Int(Rnd*1500) + 1500
	End If
End Sub

Sub DMD_Warp3_Timer
	DMD_Warp3.Interval = 18
	Warp3Counter = Warp3Counter + 1
	If 	WarpSwap3 Then
		WarpShip(3,1).SetPosition 128 - Warp3Counter, 8
	Else
		WarpShip(3,2).SetPosition Warp3Counter - 15, 8	
	End If
	If Warp3Counter = 160 Then
		Warp3Counter = 0
		If Int(Rnd*11) > 5 Then WarpSwap3 = Not WarpSwap3
		DMD_Warp3.Interval = Int(Rnd*3000) + 500
	End If
End Sub

Sub DMD_Warp4_Timer
	DMD_Warp4.Interval = 24
	Warp4Counter = Warp4Counter + 1
	If 	WarpSwap4 Then
		WarpShip(4,1).SetPosition 128 - Warp4Counter, 6
	Else
		WarpShip(4,2).SetPosition Warp4Counter - 16, 6	
	End If
	If Warp4Counter = 152 Then
		Warp4Counter = 0
		If Int(Rnd*11) > 5 Then WarpSwap4 = Not WarpSwap4
		DMD_Warp4.Interval = Int(Rnd*2000) + 500
	End If
End Sub





'########################### Meteor Attack

Dim DMD_Cooldown



Sub DMD_Video1_Timer
	Dim ship, i
	ship = "VideoSpaceship1"
	Select Case DMD_VideoScene
		Case 0 
			DMD_Video1.Interval = 20
			DMD_VideoCounter = DMD_VideoCounter + 2
			If DMD_VideoCounter = 4 Then PlaySound "XL5_DMD_8bitShort1"
			If DMD_VideoCounter < 65 Then
				FlexDMD.Stage.GetImage("Video").SetSize DMD_VideoCounter*2, DMD_VideoCounter/2
				FlexDMD.Stage.GetImage("Video").SetPosition 64 - DMD_VideoCounter, 16 - DMD_VideoCounter/4
				FlexDMD.Stage.GetImage("Video").Visible = 1	
			ElseIf DMD_VideoCounter < 121 Then
				FlexDMD.Stage.GetImage("Game").SetSize 2 * (DMD_VideoCounter - 64), (DMD_VideoCounter - 64)/2
				FlexDMD.Stage.GetImage("Game").SetPosition 64 - (DMD_VideoCounter - 64), 16 - (DMD_VideoCounter - 64)/4
				FlexDMD.Stage.GetImage("Game").Visible = 1	
			Else
				DMD_Video1.Interval = 1800 
				DMD_VideoScene = 1
			End If
		Case 1
			DMD_CloseCircle(6)
			DMD_Video1.Interval = 712	'475
			DMD_VideoCounter = 0
			DMD_VideoScore = 0
			DMD_VideoMode = True
			DMD_VideoScene = 2
		Case 2
			DMD_Video1.Interval = 50 

			FlexDMD.Stage.GetImage("Diagonal").SetPosition DMD_VideoCounter - 12, 0
			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = 11 Then 
				DMD_VideoCounter = 0

				If DMD_MenuSwap Then
					FlexDMD.Stage.GetImage("SkipPlay1").Visible = 1
					FlexDMD.Stage.GetImage("SkipPlay2").Visible = 0
					DMD_MenuSwap = False
				Else
					FlexDMD.Stage.GetImage("SkipPlay1").Visible = 0
					FlexDMD.Stage.GetImage("SkipPlay2").Visible = 1
					DMD_MenuSwap = True
				End If
			End If
			If DMD_VideoCounter = 1 And DMD_VideoScore = 0 Then
				FlexDMD.Stage.GetImage("Video").Visible = 0	
				FlexDMD.Stage.GetImage("Game").Visible = 0	
				DMD_VideoScore = 1
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 1
				FlexDMD.Stage.GetImage("LabelMeteor").Visible = 1	
				FlexDMD.Stage.GetImage("Diagonal").Visible = 1
				PlaySound "XL5_DMD_8bitMenu", -1
			End If

			If DMD_VideoControlLeft Then
				StopSound "XL5_DMD_8bitMenu"
				FlexDMD.Stage.GetImage("SkipPlay1").Visible = 1
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 0
				DMD_Video1.Interval = 1000
				DMD_CloseDiamond
				DMD_FXCounter2 = 0
				DMD_VideoScene = 16

			End If	
			If DMD_VideoControlRight Then 
				StopSound "XL5_DMD_8bitMenu"
				FlexDMD.Stage.GetImage("SkipPlay1").Visible = 0
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 1
				DMD_VideoCounter = 0
				DMD_CloseCircle(6)
				DMD_Video1.Interval = 750
				DMD_VideoScene = 3
			End If

		Case 3
'			DMD_VideoMode = False
			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = 1 Then 
				DMD_Video1.Interval = 1000
				FlexDMD.Stage.GetImage("SkipPlay1").Visible = 0
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 0
				FlexDMD.Stage.GetImage("LabelMeteor").Visible = 0
				FlexDMD.Stage.GetImage("Diagonal").Visible = 0
				FlexDMD.Stage.GetImage("VideoBG").SetPosition -11, 0
				FlexDMD.Stage.GetImage("VideoBG").Visible = 1
				FlexDMD.Stage.GetImage("VideoBGMoon").SetPosition -11, 0
				FlexDMD.Stage.GetImage("VideoBGMoon").Visible = 1
				DMD_VideoScore = 0
			End If
			If DMD_VideoCounter = 2 Then 

				DMD_VideoScore = DMD_VideoScore + 4
				If DMD_VideoScore = 4 Then PlaySound "XL5_DMD_8bitShort2"
				If DMD_VideoScore < 129 Then
					FlexDMD.Stage.GetImage("Meteor").SetSize DMD_VideoScore, 32	'DMD_VideoScore/4	'32	
					FlexDMD.Stage.GetImage("Meteor").SetPosition 0, 0
					FlexDMD.Stage.GetImage("Meteor").Visible = 1 
					DMD_VideoCounter = 1
					DMD_Video1.Interval = 15
				Else
					DMD_Video1.Interval = 2400
				End If
			End If
			If DMD_VideoCounter = 3 Then 
				DMD_VideoCounter = 4
				DMD_ShipInterval = 60
				DMD_VideoScore = 128
				DMD_VideoMode = True
				DMD_Meteor.Interval = 80
				DMD_Video1.Interval = 10
			End If

			If DMD_VideoCounter = 4 Then 
				If DMD_VideoScore = 128 Then PlaySound "XL5_Whoosh_LR"
				DMD_VideoScore = DMD_VideoScore - 4
				If DMD_VideoScore >= 0 Then
					FlexDMD.Stage.GetImage("Meteor").SetSize DMD_VideoScore, DMD_VideoScore/4
					FlexDMD.Stage.GetImage("Meteor").SetPosition 128 - DMD_VideoScore, 0
					DMD_VideoCounter = 3
					DMD_Video1.Interval = 15
				Else
					FlexDMD.Stage.GetImage("Meteor").Visible = 0
					FlexDMD.Stage.GetLabel("MeteorScore").Text = ""
					FlexDMD.Stage.GetLabel("MeteorScore").Visible = 1
					FlexDMD.Stage.GetLabel("VideoScore").Text = "000"
					DMD_VideoScore = 0
					DMD_VideoScene = 4
				End If
			End If

		Case 4
			DMD_Video1.Interval = 30
			If DMD_ShipZ = -25 Then
				StopSound "XL5_DMD_8bit1"	'"XL5_Incidental_1min"
				PlaySound "XL5_DMD_8bit1", -1
				FlexDMD.Stage.GetLabel("VideoScore").Visible = 1
				FlexDMD.Stage.GetLabel("VideoScore").SetAlignedPosition 56, 1, FlexDMD_Align_TopLeft
				FlexDMD.Stage.GetImage("VideoBG").SetPosition -11, 0
				For i = 1 to 3
					If DMD_ShipsLocal >= i Then
						FlexDMD.Stage.GetImage("Shipicon_" & i).Visible = 1
					Else
						FlexDMD.Stage.GetImage("Shipicon_" & i).Visible = 0
					End If
					If DMD_ShipsLocal = i Then
						DMD_SwapFrames "Shipicon_" & i, "Blank", 20, 100
					End If
				Next	
			End If
			DMD_ShipZ = DMD_ShipZ + 1

			If DMD_VideoCounter < 32 Then DMD_VideoCounter = DMD_VideoCounter + 2
			FlexDMD.Stage.GetImage(ship).SetPosition DMD_ShipXY, DMD_ShipZ
			If DMD_ShipZ > 8 Then 
				DMD_ShipInterval = DMD_ShipInterval + DMD_ShipInterval / 3
				DMD_Video1.Interval = DMD_ShipInterval
			End If
			If DMD_ShipZ > 12 Then
				DMD_Video1.Interval = 30 
				DMD_Mgap = 100
				DMD_ShipXY_Old = DMD_ShipXY
				Randomize 6			'1,2,   4, 5
				DMDPlay_Whoosh2
				DMD_VideoScene = 5
			End If



		Case 5
			DMD_Video1.Interval = 25

		'DMD_ShipZ = DMD_ShipZ + Int(Rnd*3) - 1
			If DMD_ShipZ > 14 Then DMD_ShipZ = 14 
			If DMD_ShipZ < 12 Then DMD_ShipZ = 12

			If DMD_VideoControlRight Then DMD_ShipXY = DMD_ShipXY + 1
			If DMD_VideoControlLeft Then DMD_ShipXY = DMD_ShipXY - 1

			If DMD_ShipXY < 0 Then DMD_ShipXY = 0
			If DMD_ShipXY > 98 Then DMD_ShipXY  = 98
			If DMD_ShipXY < DMD_ShipXY_Old Then ship = "VideoSpaceship3"
			If DMD_ShipXY > DMD_ShipXY_Old Then ship = "VideoSpaceship2"

			DMD_ShipXY_Old = DMD_ShipXY
			FlexDMD.Stage.GetImage(ship).SetPosition DMD_ShipXY, DMD_ShipZ
			FlexDMD.Stage.GetImage("VideoBG").SetPosition Int((102-DMD_ShipXY) / 4) - 24, 0

			If DMD_Ship_Old <> ship Then FlexDMD.Stage.GetImage(DMD_Ship_Old).SetPosition 130, 0
			DMD_Ship_Old = ship

			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = DMD_Mgap And MeteorNum < 5 Then
				DMD_VideoCounter = 0
				DMD_Mgap = Int(Rnd*10) + 30 
				MeteorNum = MeteorNum + 1
				For i = 1 to 5
					if MeteorSize(i,0) = 0 Then 
						MeteorSize(i,0) = 4
						MeteorSize(i,1) = Int(Rnd*95) + 6
						If i = 1 And DMD_MeteorScore = 0  Then MeteorSize(i,1) = Int(Rnd*5) + 70
						If i = 2 And DMD_MeteorScore = 0  Then MeteorSize(i,1) = Int(Rnd*10) + 20
						MeteorSize(i,2) = Int(Rnd*4) + 8
						FlexDMD.Stage.GetImage("Meteor" & i).SetPosition MeteorSize(i,1), MeteorSize(i,2)
						FlexDMD.Stage.GetImage("Meteor" & i).SetSize 4, 4
						FlexDMD.Stage.GetImage("Meteor" & i).Visible = 1
						DMD_Meteor.Enabled = True
						Exit For
					End If
				Next
			End If




		Case 6
			DMD_Video1.Interval = 750
			DMD_CloseCircle(6)
			DMD_VideoCounter = -1
			WarpCounter = 0
			DMD_Meteor.Enabled = False
			DMD_VideoScene = 7
		Case 7
			DMD_Video1.Interval = 40
			FlexDMD.Stage.GetImage("Diagonal").SetPosition DMD_VideoCounter - 12, 0
			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = 0 Then
				FlexDMD.Stage.GetImage("VideoSpaceship1").Visible = 0
				FlexDMD.Stage.GetImage("VideoSpaceship3").Visible = 0
				FlexDMD.Stage.GetImage("VideoSpaceship2").Visible = 0
				For i = 1 to 5
					FlexDMD.Stage.GetImage("Meteor" & i).Visible = 0
				Next
				FlexDMD.Stage.GetLabel("VideoScore").Visible = 0
				FlexDMD.Stage.GetLabel("MeteorScore").Visible = 0
				FlexDMD.Stage.GetImage("VideoBG").Visible = 0
				FlexDMD.Stage.GetImage("VideoBGMoon").Visible = 0

				FlexDMD.Stage.GetImage("Diagonal").Visible = 1
				DMD_SwapFrames "GameOver1", "GameOver2", 30, 100
				StopSound "XL5_DMD_8bit1"
				PlaySound "XL5_DMD_8bitShort2"
			End If
			If DMD_VideoCounter = 11 Then 
				DMD_VideoCounter = 0
				WarpCounter = WarpCounter + 1
				If WarpCounter = 7 Then 
					DMD_Video1.Interval = 1000
					DMD_CloseDiamond4(3)
					DMD_VideoScene = 8
				End If
			End If
		Case 8
			DMD_Video1.Interval = 2500
			PlaySound "XL5_DMD_8bitMenu"
			FlexDMD.Stage.GetImage("Diagonal").Visible = 0
			If DMD_VideoScore > 999 Then
				DMD_Scorestring = Left(DMD_VideoScore, Len(DMD_VideoScore) - 3) & "," & Right(DMD_VideoScore, 3)
			Else
				DMD_Scorestring = DMD_VideoScore
			End If
			DMD_StartSplashBG
			DMD_Splash.Font = FontCool
			DMD_Splash.Text = "Score: " & DMD_Scorestring & ",000"
			DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
			DMD_Splash.Visible = True
			DMD_Splash_S.Font = FontCoolShadow
			DMD_Splash_S.Text = "Score: " & DMD_Scorestring & ",000"
			DMD_Splash_S.SetAlignedPosition 64, 19, FlexDMD_Align_Center
			DMD_Splash_S.Visible = True

			Score(Player) = Score(Player) + DMD_VideoScore * 1000 * ScoreX
			ShowScore
			DMD_VideoScene = 9

		Case 9
			DMD_Video1.Interval = 1000
			DMD_CloseDiamond4(3)
			DMD_VideoScene = 10

		Case 10
			DMD_Video1.Interval = 2000
			DMD_Splash.Text = "Dodged: " & DMD_MeteorTotal
			DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
			DMD_Splash_S.Text = "Dodged: " & DMD_MeteorTotal
			DMD_Splash_S.SetAlignedPosition 64, 19, FlexDMD_Align_Center
			DMD_VideoScene = 11

		Case 11
			DMD_Video1.Interval = 1000
			DMD_CloseDiamond4(3)
			DMD_VideoScene = 12

		Case 12
			DMD_Video1.Interval = 2000
			DMD_OpenWedge	
			DMD_Splash.Text = "Best Run: " & MaxMeteors
			DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
			DMD_Splash_S.Text = "Best Run: " & MaxMeteors
			DMD_Splash_S.SetAlignedPosition 64, 19, FlexDMD_Align_Center
			DMD_VideoCounter = 0
			DMD_VideoScene = 13

		Case 13
			DMD_Video1.Interval = 750
			DMD_CloseCircle(1)	
			DMD_VideoScene = 14

		Case 14
			DMD_Video1.Interval = 15
			StopSound "XL5_DMD_8bitMenu"
			DMD_Splash.Text = ""
			DMD_Splash_S.Text = ""
			DMD_StopSplashBG
			If MaxMeteors <= MeteorHS Then 
				DMD_VideoCounter = 1
				DMD_VideoScene = 18
			Else
				DMD_VideoCounter = DMD_VideoCounter + 1
				If DMD_VideoCounter = 1 Then 
					PlaySound "XL5_Win4"
					DMDPlay_Whoosh
				End If
				If DMD_VideoCounter < 65 Then

					FlexDMD.Stage.GetImage("Arcade").SetSize DMD_VideoCounter*2, DMD_VideoCounter/2
					FlexDMD.Stage.GetImage("Arcade").SetPosition 64 - DMD_VideoCounter, 16 - DMD_VideoCounter/4
					FlexDMD.Stage.GetImage("Arcade").Visible = 1	
				Else
					DMD_SwapFrames "Arcade", "Blank", 20, 100
					FlexDMD.Stage.GetLabel("HSName").Text = ""
					FlexDMD.Stage.GetLabel("HSName").Visible = 1
					FlexDMD.Stage.GetLabel("HSName_S").Text = ""
					FlexDMD.Stage.GetLabel("HSName_S").Visible = 1
					DMD_Video1.Interval = 2000
					MeteorHS = MaxMeteors
					MeteorHSString = ""
					DMD_VideoCounter = 1
					WarpCounter = 0
					DMD_Cooldown = False
					DMD_VideoScene = 15
				End If
			End If


'					###########		Meteor High Score Name input 
		Case 15
			DMD_Video1.Interval = 50
			If Not DMD_VideoControlRight And Not DMD_VideoControlLeft And Not DMD_VideoControlDown Then DMD_Cooldown = False 
			FlexDMD.Stage.GetImage("Char_BG").Visible = 1
			FlexDMD.Stage.GetImage("Char2_" & DMD_VideoCounter).Visible = 1
			DMDPlay_FX 5, -1
			If DMD_VideoCounter = 1 Then 
				FlexDMD.Stage.GetImage("Char2_37").Visible = 0
				FlexDMD.Stage.GetImage("Char2_38").Visible = 0
				FlexDMD.Stage.GetImage("Char2_2").Visible = 0
			ElseIf DMD_VideoCounter = 37 Then 
				FlexDMD.Stage.GetImage("Char2_36").Visible = 0
				FlexDMD.Stage.GetImage("Char2_38").Visible = 0
				FlexDMD.Stage.GetImage("Char2_1").Visible = 0
			ElseIf DMD_VideoCounter = 38 Then 
				FlexDMD.Stage.GetImage("Char2_37").Visible = 0
				FlexDMD.Stage.GetImage("Char2_1").Visible = 0
			Else
				FlexDMD.Stage.GetImage("Char2_" & DMD_VideoCounter + 1).Visible = 0
				FlexDMD.Stage.GetImage("Char2_" & DMD_VideoCounter - 1).Visible = 0
			End If
			If DMD_VideoControlRight And Not DMD_Cooldown Then 
				DMD_Cooldown = True
				PlaySound "XL5_DMD_Pong"
				DMD_VideoCounter = DMD_VideoCounter + 1
				If DMD_VideoCounter > 38 Then DMD_VideoCounter = 1
				DMD_Video1.Interval = 5
				If DMD_VideoCounter = 38 And MeteorHSString = "" Then DMD_VideoCounter = 1
			End If
			If DMD_VideoControlLeft And Not DMD_Cooldown Then 
				DMD_Cooldown = True
				PlaySound "XL5_DMD_Pong"
				DMD_VideoCounter = DMD_VideoCounter - 1
				If DMD_VideoCounter < 1 Then 
					If MeteorHSString = "" Then 
						DMD_VideoCounter = 37
					Else
						DMD_VideoCounter = 38
					End If
				End If
				DMD_Video1.Interval = 5
				If DMD_VideoCounter = 38 And Len(MeteorHSString) < 1 Then DMD_VideoCounter = 37
			End If
			If DMD_VideoControlDown And Not DMD_Cooldown Then 
				DMD_Video1.Interval = 5
				DMD_Cooldown = True
				If DMD_VideoCounter < 27 Then
					PlaySound "Bell10"
					MeteorHSString = MeteorHSString & Chr(DMD_VideoCounter + 64)
				ElseIf DMD_VideoCounter < 37 Then
					PlaySound "Bell10"
					MeteorHSString = MeteorHSString & Chr(DMD_VideoCounter + 21)
				ElseIf DMD_VideoCounter = 37 Then
					PlaySound "Bell10"
					MeteorHSString = MeteorHSString & " "
				ElseIf DMD_VideoCounter = 38 Then
					PlaySound "Bell1000"
					MeteorHSString = Left(MeteorHSString, (Len(MeteorHSString) - 1))
					If MeteorHSString = "" Then DMD_VideoCounter = 1
				End If
				FlexDMD.Stage.GetLabel("HSName").Text = MeteorHSString 
				FlexDMD.Stage.GetLabel("HSName_S").Text = MeteorHSString 
				If Len(MeteorHSString) = 3 Then
					MeteorHSName = MeteorHSString
					SaveData
					FlexDMD.Stage.GetImage("Char_BG2").Visible = 1
					FlexDMD.Stage.GetImage("Char_BG").Visible = 0
					FlexDMD.Stage.GetImage("Char2_" & DMD_VideoCounter).Visible = 0
					DMD_VideoCounter = 0
					DMD_Video1.Interval = 3000
					DMD_VideoScene = 17
				End If
			End If


		Case 16
			Dim vscore, vstring
			DMD_Video1.Interval = 2250
			DMD_FXCounter2 = DMD_FXCounter2 + 1
			If DMD_FXCounter2 = 1 Then
				FlexDMD.Stage.GetImage("SkipPlay1").Visible = 0
				FlexDMD.Stage.GetImage("SkipPlay2").Visible = 0
				FlexDMD.Stage.GetImage("LabelMeteor").Visible = 0
				FlexDMD.Stage.GetImage("Diagonal").Visible = 0
				PlaySound "XL5_DMD_8bitShort1"
				vstring = "100K X " & DMD_Ships(Player) & " (Ships)"
				DMDBlinkSplashString vstring, 0, True, False
			Else
				vscore = 100000 * DMD_Ships(Player) * ScoreX
				vstring = Left(vscore, Len(vscore) - 3) & "," & Right(vscore, 3)
				If vscore > 999999 Then vstring = Left(vscore, Len(vscore) - 6) & "," & Right(vstring, 7)
				DMDBlinkSplashString vstring, 0, False, True
				Score(Player) = Score(Player) + vscore
				ShowScore
				DMD_VideoCounter = 1
				DMD_VideoScene = 17
			End If

		Case 17
			DMD_Video1.Interval = 750
			If DMD_VideoCounter = 0 Then DMDStop_FX(5)
			DMD_CloseCircle(2)
			DMD_VideoScene = 18

		Case 18
			DMD_Video1.Interval = 3000
			FlexDMD.Stage.GetImage("Char_BG2").Visible = 0
			FlexDMD.Stage.GetLabel("HSName").Visible = 0
			FlexDMD.Stage.GetLabel("HSName_S").Visible = 0
			FlexDMD.Stage.GetImage("Diagonal").Visible = 0
			If DMD_VideoCounter = 0 Then DMDStop_FX(5)
			PlaySound "XL5_DMD_8bitShort2"
			Modes(Player) = Modes(Player) + 1
			DMDDoubleSplashString "MODE 2", "COMPLETE", True, 3, True
			If Modes(Player) = ModesTotal Then
				DMD_VideoScene = 19
				Flash4Modes(15)
				FlashSteveVenus 3, 15

			Else
				Flash1Mode 2, 7
				FlashSteveVenus 1, 11
				DMD_VideoScene = 20
			End If

		Case 19
			DMD_Video1.Interval = 3550 
			DMD_ShowModeComplete
			DMD_VideoScene = 21

		Case 20
			DMD_Video1.Interval = 3550 
			DMD_ShowModeComplete
			DMD_VideoScene = 23


		Case 21
			DMD_Video1.Interval = 450
			DMDPlay_FlybyLeft
			DMD_VideoScene = 33




		Case 22
			DMD_Video1.Interval = 750
			DMD_CloseCircle(2)
			DMD_VideoScene = 18



		Case 23
			DMD_Video1.Interval = 450
			DMDPlay_FlybyLeft
			DMD_VideoMode = False
			ShowScore
			DMD_VideoScene = 24

		Case 24
			DMD_Video1.Interval = 1500
			DMDBlinkSplashString "BALL EJECTING", 2, True, True
			DMD_VideoScene = 25

		Case 25
			DMD_Video1.Enabled = False
			ExitVideoGame






		Case 33
			DMD_Video1.Interval = 2000
			If ShowFlexDMD Then
				DMDDoubleSplashString "W I Z A R D", "MODE", True, 2, True
				DMDStop_Starfield
			End If
			PlaySound "Xl5_Upbeat"
			DMD_VideoCounter = 1
			DMD_VideoScene = 34

		Case 34
			DMD_Video1.Interval = 250
			If ShowFlexDMD Then DMDDoubleSplashString "TIME", Left(". . . . . . . . ", DMD_VideoCounter * 2), True, 2, True
			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = 9 Then
				DMD_Video1.Interval = 100
				DMD_VideoScene = 35
			End If

		Case 35
			DMD_Video1.Interval = 3550
			If ShowFlexDMD Then DMDDoubleSplashString "TO EARN", "YOUR WINGS", True, 4, True
			DMD_VideoScene = 36

		Case 36
			DMD_Video1.Interval = 450
			DMDPlay_FlybyLeft
			DMD_VideoScene = 44


		Case 44
			DMD_Video1.Interval = 3000
			If ShowFlexDMD Then DMDDoubleSplashString "COLLECT NUMBERS", "1 to 8", False, 3, True
			DMD_VideoCounter = 0
			DMD_VideoScene = 45

		Case 45
			DMD_Video1.Interval = 250
			If ShowFlexDMD Then DMDDoubleSplashString "TO SPELL", Left("FIREBALL", DMD_VideoCounter), True, 1, True
			DMD_VideoCounter = DMD_VideoCounter + 1
			If DMD_VideoCounter = 8 Then
				If ShowFlexDMD Then DMDDoubleSplashString "TO SPELL", "FIREBALL", True, 3, True
				DMD_Video1.Interval = 2550
				DMD_VideoScene = 56
			End If


		Case 56
			DMD_Video1.Interval = 450
			DMDPlay_FlybyLeft
			DMD_VideoScene = 60



		Case 60
			BallRelease.CreateBall
			BallRelease.Kick 90, 10
			Ball_Locked = 2
			Ballsave_Light.state = 0
			BallSave.Enabled = False
			Ballsaver = 99
			SoundBallRelease
			If ShowFlexDMD Then	DMD_VideoMode = False
			ShowScore
			If PlayfieldCounter > 1 Then PlayfieldTimer.Enabled = True
			WizardActive = True
			KickBack.Enabled = True
			KickBack2.Enabled = True
			KickBack_Light.State = 1
			KickBack2_Light.State = 1

			Modes(Player) = 0

			ChaseOn = 1
			ChaseCounter = 0
			Chaser001.Visible = 1
			Chaser001.TimerEnabled = True

'			MusicTimer.Interval = 1000
			MusicTimer.Enabled = True
			DMD_Video1.Enabled = False
			Exit Sub



	End Select		
End Sub




Sub DMD_Meteor_Explode(met)
	PlaySound "XL5_DMD_Rocks", 0.99
	DMD_ExplosionX = MeteorSize(met,1) - 12
	DMD_ExplosionY = MeteorSize(met,2) - 14
	DMD_MeteorExplodeCounter = 0
	DMD_Meteor.Interval = 80
	DMD_MeteorExplode.Interval = 100
	DMD_MeteorExplode.Enabled = True
End Sub

Sub DMD_MeteorExplode_Timer
	DMD_MeteorExplode.Interval = 100
	DMD_MeteorExplodeCounter = DMD_MeteorExplodeCounter + 1
	If DMD_MeteorExplodeCounter = 5 Then
		FlexDMD.Stage.GetImage("MeteorExplode_4").Visible = 0
		DMD_MeteorExplode.Enabled = False
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("MeteorExplode_" & DMD_MeteorExplodeCounter).SetPosition DMD_ExplosionX, DMD_ExplosionY
	FlexDMD.Stage.GetImage("MeteorExplode_" & DMD_MeteorExplodeCounter).Visible = 1
	If DMD_MeteorExplodeCounter > 1 Then FlexDMD.Stage.GetImage("MeteorExplode_" & DMD_MeteorExplodeCounter - 1).Visible = 0
End Sub

Sub DMD_Meteor_Survive(met)
	DMD_MeteorScore = DMD_MeteorScore + 1
	DMD_MeteorTotal = DMD_MeteorTotal + 1
	If DMD_MeteorScore > MaxMeteors Then MaxMeteors = DMD_MeteorScore
	DMD_Meteor.Interval = 80 - DMD_MeteorScore * 2
	DMD_VideoScore = DMD_VideoScore + DMD_MeteorScore
	If DMD_VideoScore > 999 Then
		DMD_Scorestring = Left(DMD_VideoScore, Len(DMD_VideoScore) - 3) & "," & Right(DMD_VideoScore, 3)
	Else
		DMD_Scorestring = DMD_VideoScore
	End If
	FlexDMD.Stage.GetLabel("VideoScore").Text = DMD_Scorestring & ",000"
	FlexDMD.Stage.GetLabel("VideoScore").SetAlignedPosition 56, 1, FlexDMD_Align_TopLeft
	FlexDMD.Stage.GetLabel("MeteorScore").Text = DMD_MeteorScore
	FlexDMD.Stage.GetLabel("MeteorScore").SetAlignedPosition 128, 1, FlexDMD_Align_TopRight
	FlexDMD.Stage.GetLabel("MeteorScore").Visible = True
	MeteorSize(met,0) = 0
	PlaySound "XL5_DMD_Pong", 0.99
'	DMD_MeteorSurvive.Interval = 500
'	DMD_MeteorSurvive.Enabled = True
End Sub

Sub DMD_MeteorSurvive_Timer
	FlexDMD.Stage.GetLabel("MeteorScore").Text = ""
	DMD_MeteorSurvive.Enabled = False
End Sub


Sub DMDPlay_ShipExplode()
	DMD_ShipExplodeCounter = 0
	PlaySound "XL5_Boom1", 0.99
	DMD_BoomX = DMD_ShipXY - 6
	DMD_BoomZ = DMD_ShipZ - 6
	DMD_ShipExplode.Enabled = True
	DMD_ShipExplode_Timer
End Sub

Sub DMD_ShipExplode_Timer
	DMD_ShipExplode.Interval = 100
	DMD_ShipExplodeCounter = DMD_ShipExplodeCounter + 1
	If DMD_ShipExplodeCounter = 6 Then 
		DMD_ShipZ = -25
		DMD_ShipXY = 48
		FlexDMD.Stage.GetImage("VideoSpaceship1").SetPosition 48, -25
		FlexDMD.Stage.GetImage("VideoSpaceship2").SetPosition 48, -25
		FlexDMD.Stage.GetImage("VideoSpaceship3").SetPosition 48, -25
	End If	
	If DMD_ShipExplodeCounter = 10 Then 	'23
		DMD_ShipExplode.Enabled = False
		FlexDMD.Stage.GetImage("MeteorExplode2_9").Visible = 0

		Exit Sub
	End If


	FlexDMD.Stage.GetImage("MeteorExplode2_" & DMD_ShipExplodeCounter).SetPosition DMD_BoomX, DMD_BoomZ
	FlexDMD.Stage.GetImage("MeteorExplode2_" & DMD_ShipExplodeCounter).Visible = 1
	If DMD_ShipExplodeCounter > 1 Then FlexDMD.Stage.GetImage("MeteorExplode2_" & DMD_ShipExplodeCounter - 1).Visible = 0
End Sub








Sub DMD_Meteor_Timer
	Dim i, j
	For i = 1 to 5
		If MeteorSize(i,0) > 0 Then

			FlexDMD.Stage.GetImage("Meteor" & i).SetSize MeteorSize(i,0), MeteorSize(i,0)
			FlexDMD.Stage.GetImage("Meteor" & i).SetPosition MeteorSize(i,1), MeteorSize(i,2)
			FlexDMD.Stage.GetImage("Meteor" & i).Visible = 1
			MeteorSize(i,0) = MeteorSize(i,0) + 1
			If MeteorSize(i,0) > 16 Then MeteorSize(i,2) = MeteorSize(i,2) + 0.5
			If MeteorSize(i,0) > 24 Then 
				If (DMD_ShipXY >= MeteorSize(i,1) And (DMD_ShipXY - MeteorSize(i,1)) < 22) Or (DMD_ShipXY < MeteorSize(i,1) And (MeteorSize(i,1) - DMD_ShipXY) < 28) Then
					DMD_Meteor_Explode i
					FlexDMD.Stage.GetImage("Meteor" & i).Visible = 0
					MeteorSize(i,0) = 0
					MeteorNum = MeteorNum - 1
					DMD_MeteorScore = 0

					DMD_Shiphit = DMD_Shiphit + 1
					If DMD_Shiphit = 2 Then
						DMD_Shiphit = 0
						DMD_ShipsLocal = DMD_ShipsLocal - 1
						If DMD_ShipsLocal = 0 Then
							DMD_Meteor.Enabled = False
							DMDPlay_ShipExplode
'							FlexDMD.Stage.GetImage("Shipicon_1").Visible = 0
							DMD_Video1.Interval = 500
							DMD_VideoScene = 6
							Exit For
						Else
							DMD_Meteor.Enabled = False
							DMDPlay_ShipExplode
							DMD_VideoCounter = 0
							DMD_Ship_Old = "VideoSpaceship1"
							FlexDMD.Stage.GetImage("VideoSpaceship2").SetPosition 130, 0
							FlexDMD.Stage.GetImage("VideoSpaceship3").SetPosition 130, 0
							DMD_Mgap = 30
							MeteorNum = 0
							DMD_MeteorCounter = 0
							For j = 1 to 5
								MeteorSize(j,0) = 0
								FlexDMD.Stage.GetImage("Meteor" & j).Visible = 0
							Next
							DMD_Video1.Interval = 1000
							DMD_ShipInterval = 30
							DMD_VideoScene = 4

						End If
						Exit For
					End If
				End If
			End If
			If MeteorSize(i,0) > 26 Then 
				DMD_Meteor_Survive(i)
				FlexDMD.Stage.GetImage("Meteor" & i).Visible = 0
				MeteorNum = MeteorNum - 1
			End If
		End If
	Next
	DMD_MeteorCounter = DMD_MeteorCounter + 1
	If DMD_MeteorCounter = 5 Then DMD_MeteorCounter = 0

End Sub










Sub DMDPlay_FX(effect, num)
	If FXCheck(effect) Then Exit Sub
	FXCheck(effect) = True
	DMD_FXNum = num
	DMD_FXCounter = -16
	DMD_FXCase = effect
	If DMD_FXCase = 4 Then DMD_FXCounter2 = 0
	If DMD_FXCase = 5 Then DMD_FXCounter5 = 0

	DMD_FX.Interval = 50
	DMD_FX.Enabled = True
End Sub

Sub DMDStop_FX(num)
	DMD_FX.Enabled = False
	If num = 5 Then 
		FlexDMD.Stage.GetImage("BlankWhitePulse_" & DMD_FXCounter5).Visible = 0
		FlexDMD.Stage.GetImage("BlankWhitePulse2_" & DMD_FXCounter5).Visible = 0
		FlexDMD.Stage.GetImage("Diagonal").Visible = False
		FXCheck(5) = False
	End If
End Sub


Sub DMD_FX_Timer
	Select Case DMD_FXCase
		Case 1						'gradient 200
			DMD_FX.Interval = 5
			FlexDMD.Stage.GetImage("Horizontal").SetPosition 0, DMD_FXCounter
			FlexDMD.Stage.GetImage("Horizontal").Visible = 1
			DMD_FXCase = 2

		Case 2
			DMD_FX.Interval = 5
			FlexDMD.Stage.GetImage("Horizontal").SetPosition 0, DMD_FXCounter
			DMD_FXCounter = DMD_FXCounter + 1
			If DMD_FXCounter = 24 Then DMD_FXCase = 3	'24

		Case 3
			FlexDMD.Stage.GetImage("Horizontal").SetPosition 0, DMD_FXCounter
			DMD_FXCounter = DMD_FXCounter - 1
			If DMD_FXCounter = -56 Then 	'-56
				DMD_FXNum = DMD_FXNum - 1
				If DMD_FXNum < 1 Then
					FlexDMD.Stage.GetImage("Horizontal").Visible = 0
					DMD_FX.Enabled = False
					FXCheck(1) = False
				Else
					DMD_FXCase = 2
				End If
			End If



		Case 4		
			DMD_FX.Interval = 62									' Bloom pulse 750
			DMD_FXCounter2 = DMD_FXCounter2 + 1
			FlexDMD.Stage.GetImage("Bloom12").Visible = 0
			FlexDMD.Stage.GetImage("Bloom" & DMD_FXCounter2).Visible = 1
			If DMD_FXCounter2 > 1 Then FlexDMD.Stage.GetImage("Bloom" & DMD_FXCounter2 - 1).Visible = 0
			If DMD_FXCounter2 = 12 Then
				DMD_FXCounter2 = 0
				DMD_FXNum = DMD_FXNum - 1
				If DMD_FXNum < 1 Then
					FlexDMD.Stage.GetImage("Bloom12").Visible = 0
					DMD_FX.Enabled = False
					FXCheck(4) = False
				End If
			End If

		Case 5		
			DMD_FX.Interval = 50
			FlexDMD.Stage.GetImage("Diagonal").SetPosition DMD_FXCounter5 - 12, 0
			FlexDMD.Stage.GetImage("Diagonal").Visible = 1
			If DMD_FXCounter5 = 11 Then 
				DMD_FXCounter5 = 0
			End If
			DMD_FXCounter5 = DMD_FXCounter5 + 1
			FlexDMD.Stage.GetImage("BlankWhitePulse_" & DMD_FXCounter5).Visible = 1
			FlexDMD.Stage.GetImage("BlankWhitePulse2_" & DMD_FXCounter5).Visible = 1
			If DMD_FXCounter5 > 1 Then 
				FlexDMD.Stage.GetImage("BlankWhitePulse_" & DMD_FXCounter5 - 1).Visible = 0
				FlexDMD.Stage.GetImage("BlankWhitePulse2_" & DMD_FXCounter5 - 1).Visible = 0
			Else
				FlexDMD.Stage.GetImage("BlankWhitePulse_11").Visible = 0
				FlexDMD.Stage.GetImage("BlankWhitePulse2_11").Visible = 0
			End If
			DMD_FXNum = DMD_FXNum - 1
			If DMD_FXNum = 0 Then
				DMD_FX.Enabled = False
				FlexDMD.Stage.GetImage("Diagonal").Visible = False
				FXCheck(5) = False
			End If
	End Select
End Sub


Dim DMD_Diagonal


Sub DMD_PlayBG_Diagonal()
	DMD_Diagonal = 0
	BG_Diagonal.Interval = 50
	BG_Diagonal.Enabled = True
End Sub

Sub DMD_StopBG_Diagonal()
	BG_Diagonal.Enabled = False
	FlexDMD.Stage.GetImage("Diagonal").Visible = 0
End Sub


Sub BG_Diagonal_Timer
	FlexDMD.Stage.GetImage("Diagonal").SetPosition DMD_Diagonal - 12, 0
	FlexDMD.Stage.GetImage("Diagonal").Visible = 1
	DMD_Diagonal = DMD_Diagonal + 1
	If DMD_Diagonal = 11 Then DMD_Diagonal = 0
End Sub



Sub DMDPlay_SpinnerLit()
	If DMDShowVideo Then Exit Sub
	DMDShowVideo = True
	DMD_SpinCounter = 0
	DMD_SpinCounter2 = 0
	DMD_SpinCase = 0
	FlexDMD.Stage.GetImage("Spinner1").Visible = 1
	DMD_SpinnerLit.Interval = 300
	DMD_SpinnerLit.Enabled = True
End Sub


Sub DMD_SpinnerLit_Timer
	DMD_SpinnerLit.Interval = 4
	Select Case DMD_SpinCase
		Case 0
			DMD_SpinCounter = DMD_SpinCounter + 1
			If DMD_SpinCounter = 4 Then PlaySound "XL5_Whoosh_LR", 0.99
			FlexDMD.Stage.GetImage("Ball1").Visible = 1
			FlexDMD.Stage.GetImage("Ball1").SetPosition DMD_SpinCounter - 30, 10
			If DMD_SpinCounter =  140 Then 
				DMD_SpinCase = 1
				FlexDMD.Stage.GetImage("Ball1").Visible = 0
				DMD_SpinCounter = 0
			End If

		Case 1
			DMD_SwapFrames "SpinnerLit1", "SpinnerLit2", 20, 100		
			DMD_SpinCase = 2

		Case 2
			DMD_SpinnerLit.Interval = 20
			DMD_SpinCounter = DMD_SpinCounter + 1
			FlexDMD.Stage.GetImage("Spinner" & DMD_SpinCounter).Visible = 1
			If DMD_SpinCounter > 1 Then 
				FlexDMD.Stage.GetImage("Spinner" & DMD_SpinCounter - 1).Visible = 0
			Else
				FlexDMD.Stage.GetImage("Spinner5").Visible = 0
			End If
			If DMD_SpinCounter = 5 Then 
				DMD_SpinCase = 3
				SoundSpinnerHit
			End If
		Case 3
			DMD_SpinCounter = DMD_SpinCounter - 1
			FlexDMD.Stage.GetImage("Spinner" & DMD_SpinCounter).Visible = 1
			FlexDMD.Stage.GetImage("Spinner" & DMD_SpinCounter + 1).Visible = 0
			If DMD_SpinCounter = 1 Then
				DMD_SpinCounter2 = DMD_SpinCounter2 + 1
				If DMD_SpinCounter2 = 15 Then
					DMD_SpinnerLit.Interval = 1000
					DMD_SpinCase = 4
				Else
					DMD_SpinCase = 2
				End If
			End If

		Case 4
			FlexDMD.Stage.GetImage("Spinner1").Visible = 0
			DMDShowVideo = False
			DMD_SpinnerLit.Enabled = False

	End Select
End Sub




Dim DMD_EBallCase, DMD_EBallCounter2, DMD_FXCounter5




Sub DMDPlay_EBall()
	If DMDShowVideo Then Exit Sub
	DMDShowVideo = True
	FlexDMD.Stage.GetImage("ExtraBall1").Visible = 1
	FlexDMD.Stage.GetImage("ExtraBall1").SetPosition 48, 0
	FlexDMD.Stage.GetImage("ExtraBall1").SetSize 32, 32
	FlexDMD.Stage.GetImage("ExtraBall2").SetSize 1, 32
	FlexDMD.Stage.GetImage("ExtraBall2").Visible = 1
	FlexDMD.Stage.GetImage("ExtraBall3").SetPosition 130, 0
	FlexDMD.Stage.GetImage("ExtraBall3").Visible = 1
	BlankWhite.Visible = 1
	DMD_EBallCounter = 0
	DMD_EBallCounter2 = 0
	DMD_EBallCase = 1
	DMD_EBall.Interval = 10
	DMD_EBall.Enabled = True
End Sub

Sub DMD_EBall_Timer
	DMD_EBall.Interval = 2
	Select Case DMD_EBallCase
		Case 1
			DMD_EBallCounter = DMD_EBallCounter + 1
			FlexDMD.Stage.GetImage("ExtraBall2").SetSize DMD_EBallCounter, DMD_EBallCounter/4
			If DMD_EBallCounter = 128 Then 
				DMD_EBall.Interval = 200
				DMD_EBallCase = 2
			End If

		Case 2
			DMD_EBallCounter = DMD_EBallCounter - 1
			FlexDMD.Stage.GetImage("ExtraBall2").SetSize DMD_EBallCounter, DMD_EBallCounter/4
			FlexDMD.Stage.GetImage("ExtraBall3").SetSize 128 - DMD_EBallCounter, 32 - DMD_EBallCounter/4
			FlexDMD.Stage.GetImage("ExtraBall3").SetPosition DMD_EBallCounter, 0
			If DMD_EBallCounter = 1 Then 
				DMD_EBall.Interval = 200
				DMD_EBallCounter2 = DMD_EBallCounter2 + 1
				If DMD_EBallCounter2 = 2 Then
					DMD_EBallCase = 4
				Else
					DMD_EBallCase = 3
				End If
			End If

		Case 3
			DMD_EBallCounter = DMD_EBallCounter + 1
			FlexDMD.Stage.GetImage("ExtraBall2").SetSize DMD_EBallCounter, DMD_EBallCounter/4
			FlexDMD.Stage.GetImage("ExtraBall3").SetSize 128 - DMD_EBallCounter, 32 - DMD_EBallCounter/4
			FlexDMD.Stage.GetImage("ExtraBall3").SetPosition DMD_EBallCounter, 0
			If DMD_EBallCounter = 50 Then DMD_PlayBoom 48, 0
			If DMD_EBallCounter = 128 Then 
				DMD_EBall.Interval = 200
				DMD_EBallCase = 2
			End If
		Case 4
			DMD_EBallCounter = DMD_EBallCounter + 1
			FlexDMD.Stage.GetImage("ExtraBall3").SetSize 128 - DMD_EBallCounter, 32 - DMD_EBallCounter/4
			FlexDMD.Stage.GetImage("ExtraBall3").SetPosition DMD_EBallCounter, 0
			FlexDMD.Stage.GetImage("ExtraBall2").Visible = 0
			If DMD_EBallCounter = 128 Then 
				DMD_EBallCounter = 0
				FlexDMD.Stage.GetImage("ExtraBall2").Visible = 0
				DMD_EBall.Interval = 250
				DMD_EBallCase = 5
			End If

		Case 5
			DMD_EBall.Interval = 1
			DMD_CloseCircle(2)
			DMD_EBallCase = 6

		Case 6
			DMD_EBall.Interval = 50
			Dim inc
			DMD_EBallCounter = DMD_EBallCounter + 2
			inc = DMD_EBallCounter * 2
			FlexDMD.Stage.GetImage("ExtraBall1").SetSize 32 + inc, 32 + inc
			FlexDMD.Stage.GetImage("ExtraBall1").SetPosition 48 - inc/2, -inc/2
			If DMD_EBallCounter = 40 Then DMD_EBallCase = 7

		Case 7
			FlexDMD.Stage.GetImage("ExtraBall1").Visible = 0
			BlankWhite.Visible = 0
			DMDShowVideo = False
			ShowScore
			DMD_EBall.Enabled = False
	
	End Select
End Sub





Dim MBallCount



Sub DMDPlay_MBall()
	If DMDShowVideo Then Exit Sub
	DMDShowVideo = True
	BlankBlack.Visible = 1

	MBallCount = 0
	MBallCase = 1
	DMD_MBall.Interval = 1
	DMD_MBall.Enabled = True
End Sub


Sub DMD_MBall_Timer
	Select Case MBallCase
		Case 1

			DMD_MBall.Interval = 40
	 		MBallCount = MBallCount + 1
			FlexDMD.Stage.GetImage("Multiball").SetSize MBallCount * 8, MBallCount * 2
			FlexDMD.Stage.GetImage("Multiball").SetPosition 64 - MBallCount * 4, 16 - MBallCount
			FlexDMD.Stage.GetImage("Multiball").Visible = 1
			If MBallCount = 16 Then MBallCase =2

		Case 2
			DMD_MBall.Interval = 1500
			DMD_SwapFrames "Multiball", "Multiball2", 40, 50
			MBallCase =3

		Case 3
			BlankBlack.Visible = 0
			DMDShowVideo = False
			ShowScore
			DMD_MBall.Enabled = False

	End Select
End Sub




Sub DMDPlay_FlybyLeft()
	DMD_RocketPos = 128
	DMDPlanetoid.Interval = 50
	DMDPlanetoid.Enabled = True
End Sub

Sub DMDPlanetoid_Timer
	DMDPlanetoid.Interval = 5
	FlexDMD.Stage.GetImage("Planetoid").Visible = 1
	FlexDMD.Stage.GetImage("Planetoid").SetPosition DMD_RocketPos, 0
	DMD_RocketPos = DMD_RocketPos - 2
	If DMD_RocketPos = 72 Then PlaySound "XL5_Whoosh_LR"
	If DMD_RocketPos < -128 Then
		FlexDMD.Stage.GetImage("Planetoid").Visible = 0
		DMDPlanetoid.Enabled = False
	End If	
End Sub

Sub DMDPlay_FlybyRight()
	DMD_RocketPos = -128
	DMDPlanetoid2.Interval = 50
	DMDPlanetoid2.Enabled = True
End Sub

Sub DMDPlanetoid2_Timer
	DMDPlanetoid2.Interval = 5
	FlexDMD.Stage.GetImage("Planetoid2").Visible = 1
	FlexDMD.Stage.GetImage("Planetoid2").SetPosition DMD_RocketPos, 0
	DMD_RocketPos = DMD_RocketPos + 2
	If DMD_RocketPos = -72 Then PlaySound "XL5_Whoosh_LR", 0.99
	If DMD_RocketPos > 128 Then
		FlexDMD.Stage.GetImage("Planetoid2").Visible = 0
		DMDPlanetoid2.Enabled = False
	End If	
End Sub



Sub DMDPlay_VenusWillDie()
	DMD_XL5Counter = 60
'	DMDShowVideo = True
'	DMD_Starfield.Enabled = False
	DMDTimer.Enabled = True
End Sub


Sub DMDTimer_Timer
	DMD_XL5Counter = DMD_XL5Counter + 1
	Select Case True
		Case DMD_XL5Counter = 61
			FlexDMD.Stage.GetImage("VenusWillDie1").Visible = 1
			DMDTimer.Interval = 32
		Case DMD_XL5Counter = 72
			PlaySound "XL5_SubDie"
			FlexDMD.Stage.GetImage("VenusWillDie" & DMD_XL5Counter - 60).Visible = 1
			FlexDMD.Stage.GetImage("VenusWillDie" & DMD_XL5Counter - 61).Visible = 0
		Case (DMD_XL5Counter > 61 And DMD_XL5Counter < 134)
			FlexDMD.Stage.GetImage("VenusWillDie" & DMD_XL5Counter - 60).Visible = 1
			FlexDMD.Stage.GetImage("VenusWillDie" & DMD_XL5Counter - 61).Visible = 0
		Case DMD_XL5Counter = 134
			DMD_XL5Counter = 0
			DMDShowVideo = False
			FlexDMD.Stage.GetImage("VenusWillDie73").Visible = 0
			DMDTimer.Enabled = False
	End Select
End Sub


Sub DMD_Intro_Timer
	DMD_Intro.Interval = 6
	FlexDMD.Stage.GetImage("IntroFireball").Visible = 1
	FlexDMD.Stage.GetImage("IntroFireball").SetSize DMD_IntroCounter*2, DMD_IntroCounter/2
	FlexDMD.Stage.GetImage("IntroFireball").SetPosition 64 - DMD_IntroCounter, 16 - DMD_IntroCounter/4
	DMD_IntroCounter = DMD_IntroCounter + 2
	If DMD_IntroCounter > 64 Then
		DMD_IntroCounter = 0
		DMD_Intro.Enabled = False 
	End If
End Sub


Sub DMDScrollSplash_Timer()			' Scroll the Attract Mode Splash Text
		Select Case DMDCounter
			Case 0
				DMDScrollSplash.Interval = 60
				DMD_IntroCounter2 = DMD_IntroCounter2 + 1
				If DMD_IntroCounter2 = 32 Then DMD_Intro.Enabled = True
				If DMD_IntroCounter2 = 38 Then
					FlexDMD.Stage.GetImage("Intro_37").Visible = 0
					DMD_IntroCounter2 = 4
					DMDCounter = 1
					Exit Sub
				End If
				If DMD_IntroCounter2 = 1 Then PlaySound "XL5_DMD_Whoosh"
				FlexDMD.Stage.GetImage("Intro_" & DMD_IntroCounter2).Visible = 1
				If DMD_IntroCounter2 > 1 Then FlexDMD.Stage.GetImage("Intro_" & DMD_IntroCounter2 - 1).Visible = 0


			Case 1
				DMD_SwapFrames "IntroFireball", "IntroFireball2", 39, 100
				DMDScrollSplash.Interval = 1000
				DMDCounter = 2


			Case 2
				DMDScrollSplash.Interval = 6
				FlexDMD.Stage.GetImage("IntroFireball3").Visible = 1
				FlexDMD.Stage.GetImage("IntroFireball3").SetSize DMD_IntroCounter2*2, DMD_IntroCounter2/2
				FlexDMD.Stage.GetImage("IntroFireball3").SetPosition 64 - DMD_IntroCounter2, 16 - DMD_IntroCounter2/4
				DMD_IntroCounter2 = DMD_IntroCounter2 + 2
				If DMD_IntroCounter2 > 64 Then
					DMDScrollSplash.Interval = 2000
					DMDCounter = 3
				End If


			Case 3
				DMDScrollSplash.Interval = 750
				DMD_CloseCircle(5)
				DMDCounter = 4
				SplashX = 0

			Case 4
				DMDScrollSplash.Interval = 2550
				FlexDMD.Stage.GetImage("IntroFireball3").Visible = 0
				DMD_SwapFrames "Starring" & SplashX, "Starring" & SplashX + 1, 30, 100
				DMDCounter = 6
				If SplashX = 8 Then
					DMDCounter = 5
					DMDScrollSplash.Interval = 2250
				End If
				SplashX = SplashX + 2

			Case 5
				DMDScrollSplash.Interval = 750
				DMD_CloseCircle(2)	
				DMDCounter = 11

			Case 6
				DMDScrollSplash.Interval = 450
				If SplashX > 3 And SplashX < 8 Then
					DMDPlay_FlybyLeft
				Else
					DMDPlay_FlybyRight
				End If
				DMDCounter = 4




			Case 11
				DMDScrollSplash.Interval = 3000
				DMDPlay_Starfield
				DMD_Splash.Text = ""
				DMDDoubleSplashString "A VPX", "Original", True, 3, False

				DMD_IntroCounter2 = 4
				DMDCounter = 12

			Case 12
				DMDScrollSplash.Interval = 8
				FlexDMD.Stage.GetImage("SR").Visible = 1
				FlexDMD.Stage.GetImage("SR").SetSize DMD_IntroCounter2*2, DMD_IntroCounter2/2
				FlexDMD.Stage.GetImage("SR").SetPosition 64 - DMD_IntroCounter2, 16 - DMD_IntroCounter2/4
				DMD_IntroCounter2 = DMD_IntroCounter2 + 2
				If DMD_IntroCounter2 > 64 Then
					DMDScrollSplash.Interval = 2000
					DMDCounter = 16
				End If



			Case 16
				DMDScrollSplash.Interval = 750
				DMD_CloseCircle(2)	
				DMDCounter = 17

			Case 17
				FlexDMD.Stage.GetImage("SR").Visible = 0
				DMD_Splash.Font = FontScoreActive
				DMD_Splash.Text = "HIGH SCORES"
				DMD_Splash.SetAlignedPosition 63, 15, FlexDMD_Align_Center
				DMD_Splash.Visible = 1
				DMD_Splash2.Font = FontCool
				DMD_Splash2.Visible = 1
				SplashX = 0
				DMDCounter = 18
				DMDScrollSplash.Interval = 3000

			Case 18
				DMDPlay_FlybyLeft
				DMDScrollSplash.Interval = 450
				DMDCounter = 19

			Case 19
				DMDScrollSplash.Interval = 2000
				SplashX = SplashX + 1
				DMD_Splash2.Text = SplashX & ": " & HSName(SplashX)
				DMD_Splash.Text = HSStringNormal(SplashX)
				DMD_Splash.SetAlignedPosition 63, 22, FlexDMD_Align_Center
				DMD_Splash2.SetAlignedPosition 63, 8, FlexDMD_Align_Center
				DMDCounter = 18
				If SplashX  = 5 Then 
					SplashX = 128
					DMDCounter = 20
				End If

			Case 20
				DMDPlay_FlybyRight
				DMDScrollSplash.Interval = 450
				DMDCounter = 21

			Case 21
				DMDScrollSplash.Interval = 3000
				DMD_Splash2.Text = "METEOR MAN"
				DMD_Splash.Text = MeteorHSName & " " & MeteorHS
				DMD_Splash.SetAlignedPosition 63, 22, FlexDMD_Align_Center
				DMD_Splash2.SetAlignedPosition 63, 8, FlexDMD_Align_Center
				SplashX = 128
				DMDCounter = 22

			Case 22
				DMDPlay_FlybyLeft
				DMDScrollSplash.Interval = 450
				DMDCounter = 23

			Case 23
				DMDScrollSplash.Interval = 2000
				DMD_Splash2.Text = ""
				DMD_Splash.Text = "1-4 Players"
				DMD_Splash.SetAlignedPosition 63, 16, FlexDMD_Align_Center
				DMDCounter = 24

			Case 24
				DMDScrollSplash.Interval = 450
				DMDPlay_FlybyLeft
				DMDCounter = 25

			Case 25
				DMDScrollSplash.Interval = 20
				BlankGrey.Visible = True
				DMD_Splash2.Font = FontCool
				DMD_Splash2.SetAlignedPosition SplashX, 11, FlexDMD_Align_TopLeft
				DMD_Splash2.Text = "Created for TV by Gerry & Sylvia Anderson - Music by Barry Gray"

				DMD_Splash.Font = FontCoolShadow
				DMD_Splash.SetAlignedPosition SplashX + 1, 12, FlexDMD_Align_TopLeft
				DMD_Splash.Text = "Created for TV by Gerry & Sylvia Anderson - Music by Barry Gray"
				SplashX = SplashX - 1
				If SplashX  = -500 Then 
					SplashX = 128
					DMDCounter = 30
					DMDScrollSplash.Interval = 100
				End If

			Case 30
				DMDScrollSplash.Interval = 20
				DMD_Splash2.SetAlignedPosition SplashX, 11, FlexDMD_Align_TopLeft
				DMD_Splash.SetAlignedPosition SplashX + 1, 12, FlexDMD_Align_TopLeft
				If SplashX = 128 Then
					DMD_Splash.Text = "Spinner scores 50,000 when Bonus is Maxed"
					DMD_Splash2.Text = "Spinner scores 50,000 when Bonus is Maxed"
				End If
				SplashX = SplashX - 1
				If SplashX  = -340 Then 
					SplashX = 128
					DMDCounter = 31
					DMDScrollSplash.Interval = 450
					DMDPlay_FlybyRight
				End If

			Case 31
				DMDScrollSplash.Interval = 20
				DMD_Splash2.SetAlignedPosition SplashX, 11, FlexDMD_Align_TopLeft
				DMD_Splash.SetAlignedPosition SplashX + 1, 12, FlexDMD_Align_TopLeft
				If SplashX = 128 Then
					DMD_Splash.Text = "Complete 4 Main Modes to start Wizard Mode"
					DMD_Splash2.Text = "Complete 4 Main Modes to start Wizard Mode"
				End If
				SplashX = SplashX - 1
				If SplashX  = -340 Then 
					SplashX = 128
					DMDCounter = 32
					DMDScrollSplash.Interval = 450
					DMDPlay_FlybyLeft
				End If

			Case 32
				DMDScrollSplash.Interval = 20
				DMD_Splash2.SetAlignedPosition SplashX, 11, FlexDMD_Align_TopLeft
				DMD_Splash.SetAlignedPosition SplashX + 1, 12, FlexDMD_Align_TopLeft
				If SplashX = 128 Then
					DMD_Splash.Text = "XL5 Jackpot doubles when collected, up to 50 Million"
					DMD_Splash2.Text = "XL5 Jackpot doubles when collected, up to 50 Million"
				End If
				SplashX = SplashX - 1
				If SplashX  = -400 Then 
					SplashX = 128
					DMDCounter = 33
					DMDScrollSplash.Interval = 450
					DMDPlay_FlybyRight
				End If

			Case 33
				DMDScrollSplash.Interval = 20
				If SplashX = 128 Then 
					DMD_Splash.Text = "When the Main Jackpot is collected, there is a 10 Second window to collect a Super Jackpot"
					DMD_Splash2.Text = "When the Main Jackpot is collected, there is a 10 Second window to collect a Super Jackpot"
				End If
				DMD_Splash2.SetAlignedPosition SplashX, 11, FlexDMD_Align_TopLeft
				DMD_Splash.SetAlignedPosition SplashX + 1, 12, FlexDMD_Align_TopLeft
				SplashX = SplashX - 1
				If SplashX  = -750 Then 
					SplashX = 128
					DMDCounter = 100
					DMDScrollSplash.Interval = 1000
				End If





			Case 100
				DMDCounter = 101
				DMDPlay_FlybyLeft
				DMDScrollSplash.Interval = 450

			Case 101
				DMDStop_Starfield
				BlankGrey.Visible = False
				DMD_Splash.Text = ""
				DMD_Splash2.Text = ""
				DMDScrollSplash.Interval = 500
				DMDCounter = 102

			Case 102
				DMDCounter = 0
				DMD_IntroCounter2 = 0
				DMDShowScore
				DMDScrollSplash.Enabled = False	
		End Select
End Sub



'######### Playfield mode displays

Dim ModeCounter, ModeIntensity, ModeIntensityMax, ModeScale, ModeNum
Dim ModeSVCounter, ModeSVIntensity, ModeSVIntensityMax, ModeSVScale, ModeSVNum, ModeChars
Dim Mode4Counter, Mode4Intensity, Mode4Scale

Sub Flash1Mode(mode, num)			' Odd = On
	ModeCounter = num
	ModeIntensity = 0
	ModeScale = 0.2
	Eval("Mode0" & mode).Visible = True
	ModeNum = mode
	FlashMode.Interval = 25
	FlashMode.Enabled = True
End Sub


Sub FlashMode_Timer
	Eval("Mode0" & ModeNum).IntensityScale = ModeIntensity
	ModeIntensity = ModeIntensity + ModeScale
	If ModeIntensity >= 2 Or ModeIntensity <= 0 Then
		ModeScale = ModeScale * -1
		ModeCounter = ModeCounter - 1
	End If
	If ModeCounter = 0 Then
		Eval("Mode0" & ModeNum).IntensityScale = 1
		FlashMode.Enabled = False
	End If
End Sub



Sub Flash4Modes(num)	'	Odd = On
	Dim i
	Mode4Counter = num
	Mode4Intensity = 0
	Mode4Scale = 0.4
	For i = 1 to 4
		Eval("Mode0" & i).Visible = True
	Next
	Flash4Mode.Interval = 25
	Flash4Mode.Enabled = True
End Sub

Sub Flash4Mode_Timer
	Dim i
	For i = 1 to 4
		Eval("Mode0" & i).IntensityScale = Mode4Intensity
	Next
	Mode4Intensity = Mode4Intensity + Mode4Scale
	If Mode4Intensity >= 4 Or Mode4Intensity <= 0 Then
		Mode4Scale = Mode4Scale * -1
		Mode4Counter = Mode4Counter - 1
	End If
	If Mode4Counter = 0 Then
		If Mode4Scale < 0 Then
		For i = 1 to 4
			Eval("Mode0" & i).IntensityScale = 1
		Next
		End If
		Flash4Mode.Enabled = False
	End If
End Sub





Sub FlashSteveVenus(chars, num)			' num Odd=on
	Dim i
	Modechars = chars	' 1 = Steve, 2 = Venus, 3 = Both
	ModeSVCounter = num

	ModeSVIntensity = 0
	ModeSVIntensityMax = 2
	ModeSVScale = 0.2
	For i = 5 to 8
		Eval("Mode0" & i).Visible = True
	Next
	FlashModes.Interval = 25
	FlashModes.Enabled = True
End Sub


Sub FlashModes_Timer
	Dim i
	Select Case ModeChars
		Case 1
			Mode05.IntensityScale = ModeSVIntensity
			Mode07.IntensityScale = ModeSVIntensity

		Case 2 
			Mode06.IntensityScale = ModeSVIntensity
			Mode08.IntensityScale = ModeSVIntensity

		Case 3
			For i = 5 to 8
				Eval("Mode0" & i).IntensityScale = ModeSVIntensity
			Next

	End Select

	ModeSVIntensity = ModeSVIntensity + ModeSVScale
	If ModeSVIntensity >= ModeSVIntensityMax Or ModeSVIntensity <= 0 Then
		ModeSVScale = ModeSVScale * -1
		ModeSVCounter = ModeSVCounter - 1
	End If
	If ModeSVCounter = 1 And ModeSVScale > 0 Then ModeSVIntensityMax = 1
	If ModeSVCounter = 0 Then
		FlashModes.Enabled = False
	End If
End Sub



Sub FlashAdBonus(num)			' Odd = On
	FlashAdCounter = num
	FlashAdIntensity = 0
	FlashAdScale = 0.2
	BonusFlasher.Visible = True
	BonusFlasher.TimerInterval = 25
	BonusFlasher.TimerEnabled = True
End Sub

Sub BonusFlasher_Timer
	BonusFlasher.IntensityScale = FlashAdIntensity
	FlashAdIntensity = FlashAdIntensity + FlashAdScale
	If FlashAdIntensity >= 2 Or FlashAdIntensity <= 0 Then
		FlashAdScale = FlashAdScale * -1
		FlashAdCounter = FlashAdCounter - 1
	End If
	If FlashAdCounter = 0 Then
		If FlashAdScale < 0 Then BonusFlasher.IntensityScale = 1
		BonusFlasher.TimerEnabled = False
	End If
End Sub

Dim FlashBoosterCounter, FlashBoosterIntensity, FlashBoosterScale, FlashAdCounter, FlashAdIntensity, FlashAdScale 

Sub FlashBooster(num)			' Odd = On
	FlashBoosterCounter = num
	FlashBoosterIntensity = 0
	FlashBoosterScale = 0.2
	BoosterFlasher.Visible = True
	BoosterFlasher.TimerInterval = 25
	BoosterFlasher.TimerEnabled = True
End Sub

Sub BoosterFlasher_Timer
	BoosterFlasher.IntensityScale = FlashBoosterIntensity
	FlashBoosterIntensity = FlashBoosterIntensity + FlashBoosterScale
	If FlashBoosterIntensity >= 2 Or FlashBoosterIntensity <= 0 Then
		FlashBoosterScale = FlashBoosterScale * -1
		FlashBoosterCounter = FlashBoosterCounter - 1
	End If
	If FlashBoosterCounter = 0 Then
		If FlashBoosterScale < 0 Then BoosterFlasher.IntensityScale = 1
		BoosterFlasher.TimerEnabled = False
	End If
End Sub


Dim FlashEBallCounter, FlashEBallIntensity, FlashEBallScale

Sub FlashEBall(num)			' Odd = On
	FlashEBallCounter = num
	FlashEBallIntensity = 0
	FlashEBallScale = 0.2
	EBallFlasher.Visible = True
	EBallFlasher.TimerInterval = 25
	EBallFlasher.TimerEnabled = True
End Sub

Sub EBallFlasher_Timer
	EBallFlasher.IntensityScale = FlashEBallIntensity
	FlashEBallIntensity = FlashEBallIntensity + FlashEBallScale
	If FlashEBallIntensity >= 2 Or FlashEBallIntensity <= 0 Then
		FlashEBallScale = FlashEBallScale * -1
		FlashEBallCounter = FlashEBallCounter - 1
	End If
	If FlashEBallCounter = 0 Then
		If FlashEBallScale < 0 Then EBallFlasher.IntensityScale = 0.6
		EBallFlasher.TimerEnabled = False
	End If
End Sub


Dim FlashWASPCounter, FlashWASPIntensity, FlashWASPScale

Sub FlashWASP(num)			' Odd = On
	FlashWASPCounter = num
	FlashWASPIntensity = 0
	FlashWASPScale = 0.2
	WASPFlasher.Visible = True
	WASPFlasher.TimerInterval = 25
	WASPFlasher.TimerEnabled = True
End Sub

Sub WASPFlasher_Timer
	WASPFlasher.IntensityScale = FlashWASPIntensity
	FlashWASPIntensity = FlashWASPIntensity + FlashWASPScale
	If FlashWASPIntensity >= 2 Or FlashWASPIntensity <= 0 Then
		FlashWASPScale = FlashWASPScale * -1
		FlashWASPCounter = FlashWASPCounter - 1
	End If
	If FlashWASPCounter = 0 Then
		If FlashWASPScale < 0 Then WASPFlasher.IntensityScale = 1
		WASPFlasher.TimerEnabled = False
	End If
End Sub


Dim FlashJackpotCounter, FlashJackpotIntensity, FlashJackpotScale

Sub FlashJackpot(num)			' Odd = On
	FlashJackpotCounter = num
	FlashJackpotIntensity = 0
	FlashJackpotScale = 0.2
	JackpotFlasher.Visible = True
	JackpotFlasher.TimerInterval = 25
	JackpotFlasher.TimerEnabled = True
End Sub

Sub JackpotFlasher_Timer
	JackpotFlasher.IntensityScale = FlashJackpotIntensity
	FlashJackpotIntensity = FlashJackpotIntensity + FlashJackpotScale
	If FlashJackpotIntensity >= 2 Or FlashJackpotIntensity <= 0 Then
		FlashJackpotScale = FlashJackpotScale * -1
		FlashJackpotCounter = FlashJackpotCounter - 1
	End If
	If FlashJackpotCounter = 0 Then
		If FlashJackpotScale < 0 Then JackpotFlasher.IntensityScale = 1
		JackpotFlasher.TimerEnabled = False
	End If
End Sub


Dim FlashShipCounter, FlashShipIntensity, FlashShipScale

Sub FlashShip(num)			' Odd = On
	FlashShipCounter = num
	FlashShipIntensity = 0
	FlashShipScale = 0.2
	ShipFlasher.Visible = True
	ShipFlasher.TimerInterval = 25
	ShipFlasher.TimerEnabled = True
End Sub

Sub ShipFlasher_Timer
	ShipFlasher.IntensityScale = FlashShipIntensity
	FlashShipIntensity = FlashShipIntensity + FlashShipScale
	If FlashShipIntensity >= 2 Or FlashShipIntensity <= 0 Then
		FlashShipScale = FlashShipScale * -1
		FlashShipCounter = FlashShipCounter - 1
	End If
	If FlashShipCounter = 0 Then
		If FlashShipScale < 0 Then ShipFlasher.IntensityScale = 1
		ShipFlasher.TimerEnabled = False
	End If
End Sub







Sub DMDBlinkSplashString(splashstring, numBlinks, fast, override)
	Dim i, label
	
	If (DMDShowVideo Or DMDShowSplash) And Not override Then Exit Sub	
	DMDShowSplash = True
	DMDShowScore
	DMD_StartSplashBG

	DMD_Splash_S.Text = splashstring
	DMD_Splash_S.Font = FontCoolShadow
	DMD_Splash_S.SetAlignedPosition 64, 19, FlexDMD_Align_Center
	DMD_Splash_S.Visible = 1
	DMD_Splash.Font = FontCool
	DMD_Splash.Text = splashstring
	DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
	DMD_Splash.Visible = 1

	If numBlinks > 0 Then
		FlexDMD.LockRenderThread
'		DMD_Splash.ClearActions()
'		DMD_Splash_S.ClearActions()
		If fast Then
			DMD_Splash.AddAction DMDSplashAF.Repeat(SplashBlinkFast, numBlinks * 4)
			DMD_Splash_S.AddAction DMDSplash_SAF.Repeat(SplashBlink_SFast, numBlinks * 4)
		Else
			DMD_Splash.AddAction DMDSplashAF.Repeat(SplashBlink, numBlinks)
			DMD_Splash_S.AddAction DMDSplash_SAF.Repeat(SplashBlink_S, numBlinks)
		End If
		FlexDMD.UnlockRenderThread
	Else
		numBlinks = 2.5
	End If
	DMDBlinkSplash.Interval = numBlinks * 1000
	DMDBlinkSplash.Enabled = True
End Sub

Sub DMDBlinkSplash_Timer()
	DMD_Splash.Text = ""
	DMD_Splash_S.Text = ""
	DMDShowSplash = False
	DMD_StopSplashBG
	DMDShowScore
	DMDBlinkSplash.Enabled = False
End Sub



Dim DMD_SplashBGCounter


Sub DMD_SplashBG_Timer
	DMD_SplashBG.Interval = 50
	DMD_SplashBGCounter = DMD_SplashBGCounter + 1
	If DMD_SplashBGCounter = 11 Then DMD_SplashBGCounter = 0
	FlexDMD.Stage.GetImage("SplashBG_Stripe").SetPosition DMD_SplashBGCounter - 10, 7
End Sub	

Sub DMD_StartSplashBG()
	FlexDMD.Stage.GetImage("SplashBG").Visible = 1
	FlexDMD.Stage.GetImage("SplashBG_Stripe").Visible = 1
	DMD_SplashBG.Enabled = True
End Sub

Sub DMD_StopSplashBG()
	DMD_SplashBG.Enabled = False
	FlexDMD.Stage.GetImage("SplashBG_Stripe").Visible = 0
	FlexDMD.Stage.GetImage("SplashBG").Visible = 0
End Sub


Dim DMDCloseSplash


Sub DMDDoubleSplashString(splashstring, splashstring2, stars, time, override)
	Dim i
'	override = False
	If DMDShowVideo Or (DMDShowSplash and Not override) Then Exit Sub	
	DMDShowSplash = True
'	DMDCloseSplash = True
	DMDShowScore
	If stars Then BlankGrey.Visible = 1
	DMD_Splash3.Text = splashstring
	DMD_Splash3.SetAlignedPosition 63, 9, FlexDMD_Align_Center
	DMD_Splash3_S.Text = splashstring
	DMD_Splash3_S.SetAlignedPosition 65, 11, FlexDMD_Align_Center

	DMD_Splash2.Font = FontScoreActive
	DMD_Splash2.Text = splashstring2
	DMD_Splash2.SetAlignedPosition 63, 23, FlexDMD_Align_Center
	DMD_Splash2.Visible = 1
'	DMD_Splash2_S.Font = FontScoreActiveShadow
	DMD_Splash2_S.Text = splashstring2
	DMD_Splash2_S.SetAlignedPosition 65, 25, FlexDMD_Align_Center
	DMD_Splash2_S.Visible = 1

	FlexDMD.LockRenderThread
	DMD_Splash2.ClearActions()
	DMD_Splash2.AddAction DMDSplash2AF.Repeat(SplashBlink2, time * 2)
	DMD_Splash2_S.ClearActions()
	DMD_Splash2_S.AddAction DMDSplash2_SAF.Repeat(SplashBlink2_S, time * 2)
	FlexDMD.UnlockRenderThread

	DMDDoubleSplash.Interval = time * 1000
	DMDDoubleSplash.Enabled = True
End Sub

Sub DMDDoubleSplash_Timer
	If DMDCloseSplash Then
		DMDCloseSplash = False
		DMDDoubleSplash.Interval = 750
		DMD_CloseDiamond4(2) 
		Exit Sub
	End If
	DMD_Splash3.Text = ""
	DMD_Splash3_S.Text = ""
	DMD_Splash2.Text = ""
	DMD_Splash2_S.Text = ""
	BlankGrey.Visible = 0
	DMDShowSplash = False
	ShowScore
	DMDDoubleSplash.Enabled = False
End Sub




Sub DMD_SwapFrames(frame1, frame2, numswaps, spd)
	If DMDShowSwap Then 
		DMDSwapframe.Enabled = False
		FlexDMD.Stage.GetImage(DMDSwapframe1).Visible = 0
		FlexDMD.Stage.GetImage(DMDSwapframe2).Visible = 0
'		Exit Sub 
	End If
	DMDShowSwap = True
	DMDSwapframecounter = 0
	DMDSwapframe1 = frame1
	DMDSwapframe2 = frame2
	DMDSwapframeNum = numswaps
	DMDSwapframe.Interval = spd
	DMDSwapframe.Enabled = True
	DMDSwapframe_Timer
End Sub

Sub DMDSwapframe_Timer
	If DMDSwapframecounter = DMDSwapframeNum Then 
		FlexDMD.Stage.GetImage(DMDSwapframe1).Visible = 0
		FlexDMD.Stage.GetImage(DMDSwapframe2).Visible = 0
		DMDShowSwap = False
		DMDSwapframe.Enabled = False
		Exit Sub
	End If

	DMDSwapframecounter = DMDSwapframecounter + 1
	If 	DMDSwapframecounter Mod 2 = 0 Then	
		FlexDMD.Stage.GetImage(DMDSwapframe2).Visible = 1
		FlexDMD.Stage.GetImage(DMDSwapframe1).Visible = 0
	Else
		FlexDMD.Stage.GetImage(DMDSwapframe1).Visible = 1
		FlexDMD.Stage.GetImage(DMDSwapframe2).Visible = 0
	End If
End Sub


'########################## FlexDMD End




Sub Game_init()
	Dim i
	Targets_down = 0
	Banks_down = 0
	Spinner_Lit = 0
	ABC_FlashCounter = 0
	Bonus = 0
	Bonus_held = 0
	ScoreX = 1
	InGame = False
	BellCount50 = 0
	BellCount500 = 0
	BellCount5000 = 0
	BellCount50000 = 0
	Gallery = 0
	PlayfieldX = 20			'#### Playfield scoreX for SPECIAL 
	PlayfieldXTime = 60
	PlayfieldCounter = 0
	CounterBonusX = 0
	CounterKicker = 0
	CounterKicker2 = 0
	AddBonusCounter = 0
	Super_Bonus = 0
	BonX = 0
	BallsPerGame = 5		' #### Balls per game 
	FlashText = ""
	FlashNum = 0
	ScrollText = ""
	ScrollCounter = 0
	GameText = "FIREBALL   XL5"
	BonusTotal =0
	BIP = 0
	Multiball = False
	Ballsaver = 99
	BallSaveTime = 7		' #### Ballsaver seconds
	Jackpot_TargetCounter = 0
	MultiballCounter = 0
	BonusString = ""
	StarCount = 0
	BIP = 0
	Ball_Locked = 0
	Flash_bulb = ""
	FireballCounter = 0
	FireballCounter2 = 0
	FireballCounter3 = 0
	CountDownActive = False
	GameValidated = False
	FPSwap = False
	InMode = False
	XL5Loop = 0
	ShieldActive = False
	VenusActive = False
	PowerActive = False
	WizardActive = False
	PowerCount = False
	ModesTotal = 4
	TextStr = ""
	TextStr2 = ""
	BallSaveString = ""
	BallSaveCounter = 0
	AttractCounter = 0
	Players = 0
	Player = 1
	HighScore = 0
	BonusMaxed = False
	Ballsaved = 0
	BSwap = False
	BlinkGap = 0
	BlinkFrames = 0
	BlinkString = ""
	BlinkString2 = ""
	BlinkCounter = 0
	BlinkOff = False
	M_GammaCounter = 0
	M_GammaSplice = 0
	M_GammaForward = False
	M_PowerForward = False
	M_PowerCounter = 0
	M_PowerCounter2 = 0
	M_PowerCounterOld = 0
	M_PowerHigh = 0
	M_PowerScene = 0
	M_Powerkicked = False
	M_VenusCounter = 0
	M_VenusCounter2 = 0
	M_VenusScene = 0
	M_VenusForward = False
	M_MatCounter = 0
	M_MatCounter2 = 0
	M_MatScene = 0
	M_IntroCounter = 0
	M_OutroCounter = 0
	M_PowerOn = False
	EballCounter = 0
	EballCounter2 = 0
	MBallCounter = 0
	MovieCounter = 0
	MovieString = ""
	CurrentMusic = ""
	CurrentMusicNum = 0
	RobTalkSound = ""
	RobTalkFrames = 0
	RobTalking = False
	MatComplete = False
	SoundDelay = 0
	LoopName = ""
	StopLoop = False
	SkillShotTarget = 0
	DMDShowSplash = False
	DMDShowVideo = False
	BumperScore = 25
	DOF 104, DOFPulse


	For i = 1 to 3
		Eval("XL5_Light" & i).state = 0
		Eval("Star_Light00" & i).state = 0
	Next
	For i = 3 to 7
		Eval("Light00" & i).state = 0
	Next
	For i = 10 to 16
		Eval("Light0" & i).state = 0
	Next


	SkillShotScore(0) = 250000
	For i = 1 to 4
		Eval("Spin_Light" & i).state = 0
		Eval("TopTarget_Light00" & i).State = 0
		Eval("Spot_Light00" & i).state = 0
		Score(i) = 0

		SkillShotScore(i) = SkillShotScore(0)
		ABCBonus(i) = 100000
		Jackpot(i) = 10000000

		JackpotLit(i) = 0
		Eball(i) = 0
		BallNum(i) = BallsPerGame	
		LocksLit(i) = False
		BonusX(i) = 1
		Lite1(i) = 0
		Lite2(i) = 0
		Lite3(i) = 0
		Bumper1(i) = 2
		Bumper2(i) = 2
		Bumper3(i) = 2
		Special(i) = 0
		BonusCollect(i) = False
		SpotCount(i) = 0
		Spot1(i) = 0
		Spot2(i) = 0
		Spot3(i) = 0
		Spot4(i) = 0
		Spot5(i) = 0
		Spots_hit = 0
		ScoreString(i) = "0,000,000,000"
		VenusWon(i) = False
		PowerWon(i) = False
		MeteorWon(i) = False
		Warp9Won(i) = False
		VenusStarted(i) = False
		PowerStarted(i) = False
		M_PowerWarned(i) = False
		TenBillion(i) = 0
		EBallCollect(i) = 0
		Modes(i) = 0
		DMD_Lasershots(i) = 20		' Number of shots in Warp-9 video game
		DMD_Ships(i) = 2			' Number of ships in Meteor video game (1-3)
	Next
	For i = 1 to 5
		HighScoreString(i) = ""
	Next

	GameBalls = BallNum(1) + 1

	Target_Reset
	Top_Target_Reset
	TopTarget_Reset
	TopTargets_down = 0

	Lock_Light001.state = 0
	Lock_Light002.state = 0
	Bonus_Light_10X.state = 0
	Bonus_Light_20X.state = 0
	Bonus_Light_30X.state = 0
	Light_2X.state = 0
	Light_3X.state = 0
	Light_5X.state = 0
	Special_Light.state = 0
	CollectBonus_Light.State = 0
	CollectBonus_Light2.State = 0
	Jackpot_Light001.state = 0
	Jackpot_Light002.state = 0
	PlayfieldX10_Light.state = 0
	EB_Light.state = 0
	EB_Light001.State = 0
	EB_Light002.State = 0
	KickBack_Light.State = 0
	KickBack2_Light.State = 0
	For i = 1 to 7
		Eval("SpecialLetter" & i).State = 0
	Next

	If Tilted Then
		If Credits > 0 Then
			ScoreText002.Text = "PRESS"
			ScoreText003.Text = " START"
		Else
			ScoreText002.Text = "INSERT"
			ScoreText003.Text = "  COIN"
		End If
		For i = 1 to 3
			Eval("LightBumper00" & i).state = 0
			Eval("LightBumperLow00" & i).state = 0
		Next
		ScoreText001.Text = GameText
	End If
	PlungeBot.Enabled = False
	KickBack.Enabled = False
	KickBack2.Enabled = False
End Sub


Sub Table1_exit()
	If ShowFlexDMD Then
		If Not FlexDMD is Nothing Then
			FlexDMD.Show = False
			FlexDMD.Run = False
			FlexDMD = NULL
		End If
	End If
	'B2SController.Stop
	SaveData
	If B2SOn Then Controller.Stop
End Sub


'##### AutoPlungers

Sub PlungeBot_Hit()
	PlungeBot.Kick 0, (50 + Int(Rnd*10)+1)
	SoundPlungerReleaseBall
	DOF 104, DOFPulse
End Sub


Sub KickBack_Hit()
	KickBack.Kick 0, 55
	PlaySound SoundFXDOF("XL5_DMD_Photons" ,103,DOFPulse,DOFContactors)
	If ShowFlexDMD Then DMD_SwapFrames "Booster", "Booster2", 20, 50
	FlashBooster(6)
	If Not WizardActive Then
		KickBack_Light.State = 2
		Kick.Interval = 3000
		Kick.Enabled = True
	End If
End Sub

Sub Kick_Timer
'	PlaySound "XL5_DMD_Meteorhit"
	KickBack.Enabled = False
	KickBack_Light.State = 0
	Kick.Enabled = False
End Sub

Sub KickBack2_Hit()
	KickBack2.Kick 0, 55
	PlaySound SoundFXDOF("XL5_DMD_Photons" ,104,DOFPulse,DOFContactors)
	If ShowFlexDMD Then DMD_SwapFrames "Booster", "Booster2", 20, 50
	FlashBooster(6)
	If Not WizardActive Then
		KickBack2_Light.State = 2
		Kick2.Interval = 3000
		Kick2.Enabled = True
	End If
End Sub

Sub Kick2_Timer
'	PlaySound "XL5_DMD_Meteorhit"
	KickBack2.Enabled = False
	KickBack2_Light.State = 0
	Kick2.Enabled = False
End Sub



Sub BallRelease_Hit()
	BallRelease.Kick 90, 10
	DOF 109, DOFPulse
End Sub


Dim SkillShotTargetOld


Sub CycleSkill(leftright)
	Dim i
	SkillShotTargetOld = SkillShotTarget
	SkillShotTarget = SkillShotTarget + leftright
	If SkillShotTarget > 3 Then
		SkillShotTarget = 1
	ElseIf SkillShotTarget < 1 Then
		SkillShotTarget = 3
	End If
'	XL5_Light1.State = Lite1(Player)
'	XL5_Light2.State = Lite2(Player)
'	XL5_Light3.State = Lite3(Player)
	Eval("XL5_Light" & SkillShotTargetOld).State = Eval("Lite" & SkillShotTargetOld & "(Player)")
	Eval("XL5_Light" & SkillShotTarget).State = 2
	If ShowFlexDMD Then 
		For i = 1 to 3
			FlexDMD.Stage.GetImage("Skill_" & i).Visible = 0
		Next
		FlexDMD.Stage.GetImage("Skill_" & SkillShotTarget).Visible = 1
	End If
End Sub

'########### Start New Game ###########

Sub Start_Game()
	Dim i
	LightSeq001.StopPlay()
	Tilted = False
	InGame = True
	Attract.Enabled = False
	GameOver.Enabled = False
	TiltReel.Visible = False
	M_Intro.Enabled = False
	M_Outro.Enabled = False
	M_Radio.Enabled = False
	M_Power.Enabled = False
	SkillShot.Enabled = False
	DMD_VideoMode = False
	StopSound "M_Intro"
	StopSound "M_Outro"
	StopSound "XL5_TowerSnd"
	StopSound CurrentMusic
	MovieReel.image = ""	
	MovieReelFlasher.imageA = "XL5_MovieClear"
	Game_init
	RampUpFlag = False

	ScoreBoard.Text = "0,000,000,000"
	For i = 1 to 4
		Eval("ScorePlayer00" & i).Text = "0,000,000,000"
		Eval("Mode0" & i).Visible = False
		If ShowFlexDMD Then FlexDMD.Stage.GetLabel("Score_" & i).Visible = True
		TenBillion(i) = 0
	Next
	For i = 1 to 3
		Eval("Bumper00" & i).Collidable = True
		Eval("LightBumper00" & i).state = 2
		Eval("LightBumperLow00" & i).state = 2
	Next
	ScoreText001.Text = "Shoot Ball"
	ScoreText002.Text = "Ball 1"
	ScoreText003.Text = ""

	SoundBallRelease
	DOF 104, DOFPulse
	BallRelease.CreateBall
	BallRelease.Kick 90, 10
	BIP = BIP + 1
	Players = 1

	If ShowFlexDMD Then 
		DMDShowScore
		DMDShowBallCred
		If DMDScrollSplash.Enabled Then
			DMDScrollSplash.Enabled = False
			BlankGrey.Visible = 0
			For i = 1 to 37
				FlexDMD.Stage.GetImage("Intro_" & i).Visible = 0
			Next
			For i = 0 to 9
				FlexDMD.Stage.GetImage("Starring" & i).Visible = 0
			Next
			FlexDMD.Stage.GetImage("IntroFireball3").Visible = 0
			FlexDMD.Stage.GetImage("SR").Visible = 0
		End If
		DMDStop_Starfield
		DMDShowSplash = False
		DMDShowVideo = False
		FlexDMD.LockRenderThread

		DMD_Splash.ClearActions()
		FlexDMD.UnlockRenderThread
		DMD_StopSplashBG
		DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
		DMD_Splash.Text = ""
		DMD_Splash_S.Text = ""
		DMD_Splash2.Text = ""
		DMD_Splash2_S.Text = ""
	End If

	ShowScore
	FlashPlayerScore
	Spot_LightBlue.State = 2

	DMD_FireballWiz(Player) = 0
	ChaseOn = 0
	For i = 1 to 8
		Eval("Chaser00" & i).Visible = False
		Eval("Chaser00" & i).TimerEnabled = False
	Next


	SkillShotTarget = Int(Rnd*3) + 1
	Eval("XL5_Light" & SkillShotTarget).State = 2
	If ShowFlexDMD Then FlexDMD.Stage.GetImage("Skill_" & SkillShotTarget).Visible = 1
	SkillShotCount = 0
	SkillshotActive = False
	BallX = 0
	SkillShot.Interval = 100
	SkillShot.Enabled = True


	MusicTimer.Interval = 2000
	MusicTimer.Enabled = True

	DMDPlay_XL5

	RadioMessage
	CycleFireball

	FlashAll(4)

'	LightSequence(-1)


End Sub

Sub FlashAll(num)
	FlashSteveVenus 3, num
	Flash4Modes(num)
	FlashEBall(num)
	FlashBooster(num)
	FlashAdBonus(num)
	FlashJackpot(num)
	FlashWASP(num)
	FlashShip(num)
End Sub

'########### Movies

Sub BlinkFrame(bstring, bstring2, blinks, gap, onoff)
	BlinkString = bstring
	BlinkString2 = bstring2
	BlinkFrames = blinks
	BlinkCounter = 0
	BlinkGap = gap
	BlinkOff = onoff
	Blink.Interval = 1
	Blink.Enabled = True
End Sub

Sub Blink_Timer()
	Blink.Interval = BlinkGap
	If BlinkCounter = BlinkFrames Then
		BlinkCounter = 0
		If BlinkOff Then
			MovieReel.Image = ""
			MovieReelFlasher.imageA = "XL5_MovieClear"
		End If
		Blink.Enabled = False
		Exit Sub
	End If
	If Bswap Then
		MovieReel.Image = BlinkString2
		MovieReelFlasher.imageA = "XL5_MovieClear"
		Bswap = False
		BlinkCounter = BlinkCounter + 1
	Else
		MovieReel.Image = BlinkString
		MovieReelFlasher.imageA = "XL5_MovieClear"
		Bswap = True
	End If
End Sub



' #### Modes complete


Dim ShowModeCompleteCounter, VenusEyes


Sub DMD_ShowModeComplete()
	ShowModeCompleteCounter = 0
	DMD_PlayBG_Diagonal
	FlexDMD.Stage.GetImage("ModesBG0").Visible = 1
	If PowerWon(Player) Then FlexDMD.Stage.GetImage("ModeComplete1").Visible = 1
	If MeteorWon(Player) Then FlexDMD.Stage.GetImage("ModeComplete2").Visible = 1
	If VenusWon(Player) Then FlexDMD.Stage.GetImage("ModeComplete3").Visible = 1
	If Warp9Won(Player) Then FlexDMD.Stage.GetImage("ModeComplete4").Visible = 1
	ShowModeCompleted.Interval = 1000
	ShowModeCompleted.Enabled = True
End Sub

Sub ShowModeCompleted_Timer
	ShowModeCompleted.Interval = 200
	ShowModeCompleteCounter = ShowModeCompleteCounter + 1
	If Modes(Player) = 4 And ShowModeCompleteCounter = 1 Then DMD_SwapFrames "ModeComplete6", "Blank", 10, 300

	If Modes(Player) < 4 And ShowModeCompleteCounter < 16 Then 		
		If ShowModeCompleteCounter Mod 2 = 0 Then
			FlexDMD.Stage.GetImage("ModesBG1").Visible = 1
		Else 
			FlexDMD.Stage.GetImage("ModesBG1").Visible = 0
		End If
	End If

	If ShowModeCompleteCounter = 16 Then
		FlexDMD.Stage.GetImage("ModesBG0").Visible = 0
		DMD_StopBG_Diagonal
		FlexDMD.Stage.GetImage("ModeComplete1").Visible = 0
		FlexDMD.Stage.GetImage("ModeComplete2").Visible = 0
		FlexDMD.Stage.GetImage("ModeComplete3").Visible = 0
		FlexDMD.Stage.GetImage("ModeComplete4").Visible = 0
		FlexDMD.Stage.GetImage("ModeComplete6").Visible = 0
		ShowModeCompleted.Enabled = False
	End If
End Sub


'######################## VENUS MODE

Sub Movie_SavingVenus()
	InMode = True
	VenusActive = True
	VenusEyes = 1
	FadeOutSound CurrentMusic, 1
	M_VenusCounter = 0
	M_VenusCounter2 = 0
	If VenusStarted(Player) Then
		M_VenusScene = 2
		MovieReel.Image = "XL5_VenusX3"
		MovieReelFlasher.imageA = "XL5_VenusX3"
		M_Venus.Interval = 1000
	Else
		M_VenusScene = 1
		M_Venus.Interval = 100
		MovieReel.Image = "XL5_VenusX"
		MovieReelFlasher.imageA = "XL5_VenusX"
		If ShowFlexDMD Then FlexDMD.Stage.GetImage("VenusCap0").Visible = 1
		VenusStarted(Player) = True
		If PlayfieldCounter > 1 Then
			PlayfieldTimer.Enabled = False
		End If
	End If
	M_Venus.Enabled = True
End Sub

Sub M_Venus_Timer()
	Dim i
	Select Case M_VenusScene
	Case 1
		M_Venus.Interval = 2000
		M_VenusCounter = M_VenusCounter + 1
		If M_VenusCounter = 4 Then
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("VenusCap4").Visible = 1
				FlexDMD.Stage.GetImage("VenusCap3").Visible = 0
			End If
			M_VenusCounter = 0
			M_VenusScene = 2
		ElseIf M_VenusCounter = 3 Then
			MovieReel.Image = "XL5_VenusX3"
			MovieReelFlasher.imageA = "XL5_VenusX3"
			M_Venus.Interval = 1000
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("VenusCap3").Visible = 1
				FlexDMD.Stage.GetImage("VenusCap2").Visible = 0
			End If
		ElseIf M_VenusCounter = 2 Then
			BlinkFrame "XL5_VenusX", "XL5_VenusX2", 5, 200, False
			PlaySound "XL5_Klaxon"
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("VenusCap2").Visible = 1
				FlexDMD.Stage.GetImage("VenusCap1").Visible = 0
			End If
			M_Venus.Interval = 2000
		ElseIf M_VenusCounter = 1 Then
			BlinkFrame "XL5_VenusX", "XL5_VenusX1", 5, 200, False
			PlaySound "XL5_Dandandan"
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("VenusCap1").Visible = 1
				FlexDMD.Stage.GetImage("VenusCap0").Visible = 0
			End If
		End If
	Case 2
		M_Venus.Interval = 30
		If M_VenusCounter = 12 Then
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("VenusCap4").Visible = 0
				FlexDMD.Stage.GetImage("VenusEyes1").Visible = 1
			End If
			PlaySound "XL5_SubDie", 0.99
		End If
		If M_VenusCounter < 74 Then
			MovieReel.Image = "XL5_Venus3000" & M_VenusCounter
			MovieReelFlasher.imageA = "XL5_Venus3000" & M_VenusCounter
		End If
		M_VenusCounter = M_VenusCounter + 1

		If M_VenusCounter Mod 12 = 0 And ShowFlexDMD Then
			VenusEyes = VenusEyes + 1
			If VenusEyes > 3 Then VenusEyes = 2
			DMD_SwapFrames	"VenusEyes" & VenusEyes, "Blank", 2, 200
		End If

		If M_VenusCounter = 74 Then
			M_VenusCounter = 12
			M_VenusCounter2 = 0
			M_VenusForward = True
			M_VenusScene = 3
		End If
	Case 3
		M_Venus.Interval = 60
		If M_VenusCounter = 12 Then
			Kicker002.Kick 270, 6 + Int(Rnd*3)
			CurrentMusic = "XL5_Flight"
			PlaySound CurrentMusic
			If PlayfieldCounter > 1 Then
				PlayfieldTimer.Enabled = True
				ShowCountdown
			End If
		End If
		If M_VenusForward Then 
			M_VenusCounter = M_VenusCounter + 1
			If M_VenusCounter = 58 Then
				M_VenusForward = False
			End If
		Else
			M_VenusCounter = M_VenusCounter - 1
			If M_VenusCounter = 13 Then
				M_VenusForward = True
				M_VenusCounter2 = M_VenusCounter2 + 1
			End If
		End If
		If M_VenusCounter Mod 10 = 0 And ShowFlexDMD Then
			VenusEyes = VenusEyes + 1
			If VenusEyes > 3 Then VenusEyes = 2
			DMD_SwapFrames	"VenusEyes" & VenusEyes, "Blank", 2, 200
		End If

		MovieReel.Image = "XL5_Venus000" & 	M_VenusCounter
		MovieReelFlasher.imageA = "XL5_Venus000" & 	M_VenusCounter
		If M_VenusCounter2 = 3 And M_VenusCounter = 56 Then
			M_VenusCounter2 = 0
			M_VenusCounter = 0
			M_VenusScene = 4
		End If	
	Case 4
		M_Venus.Interval = 45
		If M_VenusCounter = 2 Then PlaySound "XL5_SubCountdown"
		M_VenusCounter = M_VenusCounter + 1
		If M_VenusCounter > 58 Then
			MovieReel.Image = "XL5_Venus2000" & (M_VenusCounter - 58)
			MovieReelFlasher.imageA = "XL5_Venus2000" & (M_VenusCounter - 58)
			If M_VenusCounter > 112 Then
				M_VenusCounter = 13				
				M_VenusCounter2 = M_VenusCounter2 + 1
				MovieReel.Image = "XL5_Venus000" & M_VenusCounter
				MovieReelFlasher.imageA = "XL5_Venus000" & M_VenusCounter
			End If
		ElseIf M_VenusCounter < 13 Then
			MovieReel.Image = "XL5_Venus000" & 	(M_VenusCounter + 13)
			MovieReelFlasher.imageA = "XL5_Venus000" & (M_VenusCounter + 13)
		Else
			MovieReel.Image = "XL5_Venus000" & 	M_VenusCounter
			MovieReelFlasher.imageA = "XL5_Venus000" & M_VenusCounter
		End If

		If M_VenusCounter Mod 8 = 0 And ShowFlexDMD Then
			VenusEyes = VenusEyes + 1
			If VenusEyes > 3 Then VenusEyes = 2
			DMD_SwapFrames	"VenusEyes" & VenusEyes, "Blank", 2, 200
		End If

		If M_VenusCounter2 = 3 And M_VenusCounter = 58 Then
			M_Venus.Interval = 2000	
			If ShowFlexDMD Then FlexDMD.Stage.GetImage("VenusEyes1").Visible = 0
			BlinkFrame "XL5_VenusX", "XL5_VenusX4", 5, 200, True
			PlaySound "XL5_Dandandan"
			M_VenusCounter = 0
			M_VenusScene = 5
		End If
	Case 5
		M_Venus.Interval = 10000		'  cooldown
		MovieReel.Image = ""
		MovieReelFlasher.imageA = "XL5_MovieClear"
		VenusActive = False
		MusicTimer.Interval = 2000
		M_VenusScene = 20







	Case 6									'Venus Saved
		If M_VenusCounter = 0 Then
			If PlayfieldCounter > 1 Then
				PlayfieldTimer.Enabled = False
			End If
			VenusWon(Player) = True
			StopSound CurrentMusic
			PlaySound "XL5_Win3"
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("VenusEyes1").Visible = 0
				FlexDMD.Stage.GetImage("VenusCap7").Visible = 1
			End If
			MovieReel.Image = "XL5_VenusX5"
			MovieReelFlasher.imageA = "XL5_VenusX5"
			M_VenusCounter = M_VenusCounter + 1
			M_Venus.Interval = 2600
		Else
			If M_VenusCounter = 1 Then
				PlaySound "XL5_SteveSub2"
				M_Venus.Interval = 45
				If ShowFlexDMD Then
					FlexDMD.Stage.GetImage("VenusCap7").Visible = 0
					Modes(Player) = Modes(Player) + 1
					Flash1Mode 3, 11
					FlashSteveVenus 2, 10
					DMDDoubleSplashString "MODE 3", "COMPLETE", True, 3, True
				End If
			End If
			If M_VenusCounter = 20 Then
				PlaySound "XL5_Upbeat"
			End If

			If M_VenusCounter = 56 And ShowFlexDMD Then DMD_ShowModeComplete

			If M_VenusCounter = 80 Then
				M_Venus.Interval = 1000
				MovieReel.Image = "XL5_DMD_BlankBlack"
				MovieReelFlasher.imageA = "XL5_DMD_BlankBlack"
				M_VenusCounter = 2
				M_VenusScene = 7
				Exit Sub
			End If
			MovieReel.Image = "XL5_Steve1000" & M_VenusCounter
			MovieReelFlasher.imageA = "XL5_Steve1000" & M_VenusCounter
			M_VenusCounter = M_VenusCounter + 1
		End If


	Case 7	
		M_Venus.Interval = 45
		M_VenusCounter = M_VenusCounter + 1
		MovieReel.Image = "XL5_VenusFade" &	M_VenusCounter
		MovieReelFlasher.imageA = "XL5_VenusFade" &	M_VenusCounter
		If M_VenusCounter = 10 Then
			M_VenusCounter = 0
			M_VenusScene = 8
		End If
	Case 8	
		M_Venus.Interval = 45
		M_VenusCounter = M_VenusCounter + 1
		MovieReel.Image = "XL5_Venus4000" & M_VenusCounter
		MovieReelFlasher.imageA = "XL5_Venus4000" & M_VenusCounter
		If M_VenusCounter = 12 Then
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("VenusClip").Visible = 1
				FlexDMD.Stage.GetImage("SteveClip").Visible = 1
				FlexDMD.Stage.GetImage("SteveClip").SetPosition -40, 0
			End If
			PlaySound "XL5_OhSteve"
		End If

		If ShowFlexDMD And M_VenusCounter > 39 And M_VenusCounter < 76 Then
			FlexDMD.Stage.GetImage("SteveClip").SetPosition M_VenusCounter - 75, 0
		End If
		If M_VenusCounter = 35 Then
			CurrentMusic = "XL5_Joe90B"
			PlaySound "XL5_Joe90B"
			MusicTimer.Interval = 78000
		End If
		If M_VenusCounter = 50 Then PlaySound "XL5_ThereThere"

		If M_VenusCounter = 146 And ShowFlexDMD Then DMD_CloseCircle(1)

		If M_VenusCounter = 161 Then
			M_VenusCounter = 1
			M_Venus.Interval = 500
			If ShowFlexDMD Then
				FlexDMD.Stage.GetImage("VenusClip").Visible = 0
				FlexDMD.Stage.GetImage("SteveClip").Visible = 0
				FlexDMD.Stage.GetImage("VenusCap5").Visible = 1
				FlexDMD.Stage.GetImage("VenusCap6").SetPosition 0, 33
				FlexDMD.Stage.GetImage("VenusCap6").Visible = 1
			End If
			Score(Player) = Score(Player) + 1000000 * ScoreX	
			ShowScore
			MovieReel.Image = ""
			MovieReelFlasher.imageA = "XL5_DMD_BlankBlack"
			M_VenusScene = 9
		End If

	Case 9
		If M_VenusCounter = 2 Then
			MovieReel.Image = "XL5_VenusMball2"
			MovieReelFlasher.imageA = "XL5_VenusMball2"
			M_Venus.Interval = 2400
		ElseIf M_VenusCounter < 16 Then
			MovieReel.Image = "XL5_VenusMball" & M_VenusCounter
			MovieReelFlasher.imageA = "XL5_VenusMball" & M_VenusCounter
			M_Venus.Interval = 30
		ElseIf M_VenusCounter = 16 Then
			M_Venus.Interval = 2000
		Else
			PlungeBot.Enabled = True
			BallRelease.CreateBall
			BallRelease.Kick 90, 10
			Ball_Locked = 2
			Ballsave_Light.state = 0
			BallSave.Enabled = False
			Ballsaver = 99
			SoundBallRelease
			M_Venus.Interval = 5000
			M_VenusCounter = 14
			M_VenusCounter2 = 0
			M_VenusScene = 10
			If PlayfieldCounter > 1 Then
				PlayfieldTimer.Enabled = True
				End If
			If ShowFlexDMD Then
				FlexDMD.Stage.GetImage("VenusCap5").Visible = 0
				FlexDMD.Stage.GetImage("VenusCap6").Visible = 0
			End If
			Exit Sub
		End If
		If ShowFlexDMD And M_VenusCounter > 2 Then FlexDMD.Stage.GetImage("VenusCap6").SetPosition 0, 32 - M_VenusCounter*2
		M_VenusCounter = M_VenusCounter + 1

	Case 10
		M_Venus.Interval = 45
		M_VenusCounter = M_VenusCounter + 1
		If M_VenusCounter = 342 And M_VenusCounter2 < 3 Then
			M_VenusCounter = 72
			M_VenusCounter2 = M_VenusCounter2 + 1
		ElseIf M_VenusCounter = 342 And M_VenusCounter2 = 3 Then
			M_VenusCounter = 282
			M_VenusCounter2 = M_VenusCounter2 + 1
		ElseIf M_VenusCounter = 503 Then
			MovieReel.Image = ""
			MovieReelFlasher.imageA = "XL5_MovieClear"
			M_Venus.Interval = 10000
			VenusActive = False
			MusicTimer.Interval = 2000
			M_VenusScene = 20
		Else
			MovieReel.Image = "XL5_Venus500" & 	M_VenusCounter
			MovieReelFlasher.imageA = "XL5_Venus500" & 	M_VenusCounter
		End If


	Case 20
		InMode = False
		M_Venus.Enabled = False

	End Select
End Sub


'#########################


Sub RadioMessage()
	PlaySound "XL5_Launch", 0.99
	BlinkFrame "XL5_Speaker1", "XL5_Speaker2", 1, 1500, True
End Sub



Sub Movie_GammaPower()
	If Not Multiball Then
		StopSound CurrentMusic
		CurrentMusic = "XL5_ZeroG"
		PlaySound CurrentMusic
		MusicTimer.Interval = 133000
	End If

	M_GammaCounter = 0
	M_GammaForward = True
	M_Gamma.Interval = 10
	M_Gamma.Enabled = True
End Sub

Sub M_Gamma_Timer()
	M_Gamma.Interval = 45
	If M_GammaForward Then
		M_GammaCounter = M_GammaCounter + 1
		If M_GammaCounter = 40 Then
			M_GammaSplice = 5 + Int(Rnd*10) * 3
			M_GammaForward = False
		End If	
	Else
		M_GammaCounter = M_GammaCounter - 1
		If M_GammaCounter = M_GammaSplice  Then
			M_GammaForward = True
		End If	
	End If
	If Not RobTalking And Not InMode And MBallCounter = 0 And EballCounter = 0 Then
		MovieReel.Image = "XL5_Gamma000" & M_GammaCounter
		MovieReelFlasher.imageA = "XL5_Gamma000" & M_GammaCounter
	Else
'		StopSound CurrentMusic
	End If
End Sub




Dim glowship


Sub Movie_MorePower()
	PowerActive = True
	InMode = True
	glowship = "6"
	M_PowerCounter = 0
	M_PowerCounter2 = 0
	M_PowerCounterOld = 0
	M_PowerForward = False
	M_PowerWarned(Player) = False
	M_PowerHigh = 15
	M_PowerScene = 0
	If PlayfieldCounter > 1 Then
		PlayfieldTimer.Enabled = False
	End If
	If PowerStarted(Player) Then
		M_PowerScene = 3
		M_PowerCounter = 1
		If ShowFlexDMD Then DMDPlay_Starfield
	End If
	PowerStarted(Player) = True
	M_Powerkicked = False
	M_PowerOn = True
	FadeOutSound CurrentMusic, 1
	M_Power.Interval = 1000
	M_Power.Enabled = True
End Sub

Sub M_Power_Timer()
	Dim i
	Select Case M_PowerScene
		Case 0
			M_Power.Interval = 5000
			BlinkFrame "XL5_Speaker1", "XL5_Speaker2", 1, 2200, False
			PlaySound "XL5_Klaxon"
			If ShowFlexDMD Then 
				DMDPlay_Starfield
				FlexDMD.Stage.GetImage("Alarm2").Visible = 1
				FlexDMD.Stage.GetImage("Planetoid3").Visible = 1
				FlexDMD.Stage.GetImage("Planetoid3").SetPosition 130, 5
				DMD_SwapFrames "Alarm", "Alarm1", 10, 500
			End If
			M_PowerScene = 1

		Case 1
			If ShowFlexDMD Then
				M_Power.Interval = 20
				M_PowerCounter = M_PowerCounter - 1
				FlexDMD.Stage.GetImage("Alarm2").SetPosition 0, M_PowerCounter
				If M_PowerCounter = -32 Then
					FlexDMD.Stage.GetImage("Alarm2").Visible = 0
					M_PowerCounter = 0
					M_PowerScene = 2
				End If
			Else
				M_PowerScene = 2
			End If
		Case 2
			M_Power.Interval = 45
			M_PowerCounter = M_PowerCounter + 1
			If ShowFlexDMD Then FlexDMD.Stage.GetImage("Planetoid3").SetPosition 128 - M_PowerCounter, 5
			MovieReel.Image = "XL5_Power00" & M_PowerCounter
'			MovieReelFlasher.imageA = "XL5_Power00" & M_PowerCounter
			If M_PowerCounter = 1 Then 
				PlaySound "XL5_Power1"
			ElseIf M_PowerCounter = 30 Then
				PlaySound "XL5_Power2"
			ElseIf M_PowerCounter = 110 And ShowFlexDMD Then
				FlexDMD.Stage.GetImage("Planetoid5").Visible = 1
				FlexDMD.Stage.GetImage("Planetoid6").SetSize 52, 26
				FlexDMD.Stage.GetImage("Planetoid6").SetPosition 70, 1 
				FlexDMD.Stage.GetImage("Planetoid6").Visible = 1
				FlexDMD.Stage.GetImage("Planetoid3").Visible = 0
			ElseIf M_PowerCounter = 150 And ShowFlexDMD Then 
				DMD_SwapFrames "LowPower", "LowPower2", 18, 140
'				M_Power.Interval = 100
			ElseIf M_PowerCounter = 190 Then
				M_Power.Interval = 1000
				M_PowerScene = 3
				M_PowerCounter = 1
			End If
		Case 3
			M_Power.Interval = 2000
			If M_PowerCounter = 1 Then
				If ShowFlexDMD Then 
'					FlexDMD.Stage.GetImage("LowPower").Visible = 0
					DMD_SwapFrames "ShootSpinner1", "ShootSpinner2", 20, 150
					FlexDMD.Stage.GetImage("Planetoid6").SetSize 52, 26
					FlexDMD.Stage.GetImage("Planetoid6").SetPosition 70, 1 
					FlexDMD.Stage.GetImage("Planetoid5").Visible = 1
					FlexDMD.Stage.GetImage("Planetoid6").Visible = 1
				End If
				PlaySound "XL5_Mat",-1,0.99
				MovieReel.Image = "XL5_PowerX1"
'				MovieReelFlasher.imageA = "XL5_PowerX1"
				BlinkFrame "XL5_PowerX1", "XL5_PowerX2", 5, 100, False

			Else
				BlinkFrame "XL5_PowerX1", "XL5_PowerX3", 5, 100, False
				For i = 1 to 4
					Eval( "Spin_Light" & i).state = 2
				Next

			End If
			M_PowerCounter = M_PowerCounter + 1
			If M_PowerCounter = 3 Then
				If ShowFlexDMD Then 
					M_Power.Interval = 500
					FlexDMD.Stage.GetImage("SpeedoBG").Visible = 1
					FlexDMD.Stage.GetImage("Speedo27").Visible = 1
				End If
				M_PowerScene = 4
				M_PowerCounter = 28
				M_PowerCounter2 = 0
			End If
		Case 4				
			M_Power.Interval = 100
			M_PowerCounter2 = M_PowerCounter2 + 1
			If M_PowerCounter2 = 40 Then
				M_PowerCounter2 = 0
				M_PowerHigh = M_PowerHigh - 1
			End If

			If M_PowerForward Then
				If Not M_Powerkicked Then
					M_Powerkicked = True
					Kicker001.Kick 35, 20
					SoundSaucerHit(1)
					If PlayfieldCounter > 1 Then
						PlayfieldTimer.Enabled = True
						ShowCountdown
					End If
				End If
				M_PowerCounter = M_PowerCounter + 1
				If M_PowerCounter = 28 Then
					StopSound "XL5_Mat"
					If ShowFlexDMD Then
'						DMD_ShootSpinner.Enabled = False
					End If
					M_PowerCounter = 1
					M_PowerHigh = 0
					M_PowerScene = 5
				ElseIf M_PowerCounter >= M_PowerHigh Then
					M_PowerForward = False
				End If	
			Else
				M_PowerCounter = M_PowerCounter - 1
				If M_PowerCounter = 4 And Not M_PowerWarned(Player) Then
					M_PowerWarned(Player) = True
					PlaySound "XL5_DMD_NotMuchTime"
				End If
				If M_PowerCounter < 0 Then
					StopSound "XL5_Mat"
					If ShowFlexDMD Then
'						DMD_ShootSpinner.Enabled = False
					End If
					M_PowerScene = 10
					Exit Sub
				ElseIf M_PowerCounter < (M_PowerHigh - 2) Then
					M_PowerForward = True
				End If	
			End If
			If MBallCounter = 0 And EballCounter = 0 Then
				MovieReel.Image = "XL5_Speedo" & M_PowerCounter
'				MovieReelFlasher.imageA = "XL5_Speedo" & M_PowerCounter
				If ShowFlexDMD Then 
					FlexDMD.Stage.GetImage("Speedo" & M_PowerCounter).Visible = 1
					FlexDMD.Stage.GetImage("Speedo" & M_PowerCounterOld).Visible = 0
				End If
				M_PowerCounterOld = M_PowerCounter
			End If

		Case 5
			M_Power.Interval = 2000
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("Speedo" & M_PowerCounterOld).Visible = 0
				DMD_SwapFrames "Speedo27", "Speedo0", 20, 100
			End If
			BlinkFrame "XL5_Speedo0", "XL5_Speedo27", 10, 100, False
			PlaySound "XL5_Shock"
			MusicTimer.Interval = 25000	'11500
			PowerActive = False
			SpinLights_Timer
			M_PowerCounter = 20
			M_PowerScene = 6
		Case 6
			If ShowFlexDMD Then FlexDMD.Stage.GetImage("SpeedoBG").Visible = 0
			PowerWon(Player) = True
			If M_PowerCounter = 20 Then
				PlaySound "XL5_Rocket"
				M_Power.Interval = 45
			End If
			MovieReel.Image = "XL5_Power00" & M_PowerCounter
'			MovieReelFlasher.imageA = "XL5_Power00" & M_PowerCounter
			M_PowerCounter = M_PowerCounter + 1
			If M_PowerCounter > 60 And ShowFlexDMD Then	FlexDMD.Stage.GetImage("Planetoid" & glowship).SetPosition 70 - (M_PowerCounter - 60), 1
			If M_PowerCounter = 66 And ShowFlexDMD Then	
				glowship = "6A"
				FlexDMD.Stage.GetImage("Planetoid6").Visible = 0
				FlexDMD.Stage.GetImage("Planetoid6A").Visible = 1
				FlexDMD.Stage.GetImage("Planetoid7").Visible = 1
			End If
			If M_PowerCounter = 72 Then
				If ShowFlexDMD Then	DMDPlay_Whoosh
				PlaySound "XL5_Boom1"
				M_PowerCounter = 5
				M_PowerScene = 7
			End If
		Case 7
			M_Power.Interval = 60
			If ShowFlexDMD Then	
				FlexDMD.Stage.GetImage("Planetoid5").Visible = 0
				FlexDMD.Stage.GetImage("Planetoid6A").Visible = 0
				FlexDMD.Stage.GetImage("Planetoid7").Visible = 0
				DMDShowVideo = False
			End If
			If M_PowerCounter = 30 Then
				PlaySound "XL5_Power3"
				If ShowFlexDMD Then	
					Modes(Player) = Modes(Player) + 1
					Flash1Mode 1, 11
					FlashSteveVenus 1, 6
					DMDDoubleSplashString "MODE 1", "COMPLETE", True, 3, True
				End If
			End If
			MovieReel.Image = "XL5_PowerBoom000" & M_PowerCounter
'			MovieReelFlasher.imageA = "XL5_PowerBoom000" & M_PowerCounter
			M_PowerCounter = M_PowerCounter + 1
			If M_PowerCounter = 44 Then
				MovieReel.Image = ""
				MovieReelFlasher.imageA = "XL5_MovieClear"
				M_Power.Interval = 2160
				PlaySound "XL5_Upbeat"
				M_PowerScene = 8
			End If
		Case 8
			If ShowFlexDMD Then 
				DMDStop_Starfield
				DMD_ShowModeComplete
			End If
			M_Power.Interval = 3550	
			M_PowerScene = 20


		Case 10
			M_Power.Interval = 2000
			PlaySound "XL5_Shock"
			BlinkFrame "XL5_Speedo0", "XL5_Speedo27", 15, 100, True
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("Speedo" & M_PowerCounterOld).Visible = 0
				DMD_SwapFrames "Speedo27", "Speedo0", 20, 100
			End If

			MusicTimer.Interval = 8500
			PowerActive = False
			SpinLights_Timer

			M_PowerCounter = 19
			M_PowerScene = 11
		Case 11
			M_Power.Interval = 60
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetImage("SpeedoBG").Visible = 0
			End If
			M_PowerHigh = 0
			M_PowerScene = 12

		Case 12
			M_Power.Interval = 60
			M_PowerCounter = M_PowerCounter + 1
			MovieReel.Image = "XL5_PowerBoom000" & M_PowerCounter
'			MovieReelFlasher.imageA = "XL5_PowerBoom000" & M_PowerCounter
			If M_PowerCounter > 31 And ShowFlexDMD Then	
				FlexDMD.Stage.GetImage("Planetoid6").Visible = 1
				FlexDMD.Stage.GetImage("Planetoid6").SetPosition 70 - (M_PowerCounter - 31)*5, 1 + (M_PowerCounter - 31)/1.5
				FlexDMD.Stage.GetImage("Planetoid6").SetSize 52 - (M_PowerCounter - 31)*4, 26 - (M_PowerCounter - 31)*2   
			End If
			If M_PowerCounter = 43 Then
				MovieReel.Image = ""
				MovieReelFlasher.imageA = "XL5_MovieClear"
				M_Power.Interval = 500
				If ShowFlexDMD Then	
					DMD_PlayBoom 0, 0 
					PlaySound "XL5_Boom1"
				End If
				M_PowerScene = 13
			End If

		Case 13
			If ShowFlexDMD Then	
				FlexDMD.Stage.GetImage("Planetoid5").Visible = 0
				FlexDMD.Stage.GetImage("Planetoid6").Visible = 0
			End If
			M_Power.Interval = 500
			M_PowerScene = 14

		Case 14
			M_Power.Interval = 1000
			If ShowFlexDMD Then	DMD_CloseDiamond4(2)
			M_PowerScene = 16


		Case 16

			M_PowerCounter = 0
			M_Power.Interval = 10000	
			M_PowerScene = 22

			M_PowerOn = False
			ScoreFlash "   MISSION     FAILED"
			If ShowFlexDMD Then	
				DMDStop_Starfield
				DMDShowVideo = False
				DMDDoubleSplashString "MISSION", "FAILED", True, 4, False
			End If

		Case 20
			M_Power.Interval = 450 
'			FadeOutSound CurrentMusic, 1
			If ShowFlexDMD Then	DMDPlay_FlybyLeft
			M_PowerScene = 21

		Case 21
			M_Power.Interval = 10000 
			M_PowerWin
			M_PowerScene = 22

		Case 22
			InMode = False
			M_Power.Enabled = False

	End Select
End Sub


Dim DMD_BoomCounter


Sub DMD_PlayBoom(boomX, boomY)
	DMD_BoomX = boomX
	DMD_BoomZ = boomY
	DMD_BoomCounter = 0
	DMD_Boom.Interval = 10
	DMD_Boom.Enabled = True
End Sub

Sub DMD_Boom_Timer
	DMD_Boom.Interval = 60
	DMD_BoomCounter = DMD_BoomCounter + 1
	If DMD_BoomCounter = 10 Then 
		DMD_Boom.Enabled = False
		FlexDMD.Stage.GetImage("Boom_9").Visible = 0
		Exit Sub
	End If
	FlexDMD.Stage.GetImage("Boom_" & DMD_BoomCounter).SetPosition DMD_BoomX, DMD_BoomZ
	FlexDMD.Stage.GetImage("Boom_" & DMD_BoomCounter).Visible = 1
	If DMD_BoomCounter > 1 Then FlexDMD.Stage.GetImage("Boom_" & DMD_BoomCounter - 1).Visible = 0
End Sub






Sub M_PowerWin()
	M_PowerHigh = 0
	M_PowerCounter = 0
	PowerActive = False
	M_PowerWarned(Player) = False
	MovieReel.Image = ""
	MovieReelFlasher.imageA= "XL5_MovieClear"
	Score(Player) = Score(Player) + 1000000 * ScoreX
	ShowScore
	ScoreFlash " MODE COMPLETE    ONE MILLION"
	If ShowFlexDMD Then	DMD_SwapFrames "1MIL", "1MILA", 20, 100
	StopSound CurrentMusic
	Movie_RobTalk "Rob_Onemillion", 20
	If Multiball Then
		CurrentMusic = "XL5_UFO"
		PlaySound "XL5_UFO",0,0.99,0,0,0,1,0,0
		MusicTimer.Interval = 70000
	Else
		MusicTimer.Interval = 2000
	End If

End Sub


Dim SoundVol, FadeStep, FadeSound


Sub FadeOutSound(name, delay)
	FadeSound = name
	SoundVol = 0.99
	FadeStep = 0.1/delay
	FadeLoop.Interval = 100
	FadeLoop.Enabled = True
End Sub

Sub FadeLoop_Timer()
	SoundVol = SoundVol - FadeStep
	If SoundVol > 0 Then
		PlaySound FadeSound,0,SoundVol,0,0,0,1,0,0
	Else
		StopSound FadeSound
		FadeLoop.Enabled = False
	End If
End Sub




Sub Movie_Intro()
	MovieReel.image = "XL5MovieBlank"
	MovieReelFlasher.imageA= "XL5_MovieClear"
	LightSequence(-1)
	M_IntroCounter = 0
	M_Intro.Interval = 10
	M_Intro.Enabled = True
End Sub

Sub M_Intro_Timer()
	M_IntroCounter = M_IntroCounter + 1
	If M_IntroCounter = 1 Then
		PlaySound "M_Intro"
		M_Intro.Interval = 100
	ElseIf M_IntroCounter = 7 Then
		M_Intro.Interval = 3000
	ElseIf M_IntroCounter = 25 Then
		M_Intro.Interval = 4500
	ElseIf M_IntroCounter = 108 Then
		MovieReel.image = ""	
		MovieReelFlasher.imageA= "XL5_MovieClear"
		M_Intro.Enabled = False
		Attract.Interval = 1000
		AttractMode
		Exit Sub
	Else
		M_Intro.Interval = 80
	End If
	MovieString = "XL5_Intro000" & M_IntroCounter
	MovieReel.Image = MovieString
	MovieReelFlasher.ImageA = MovieString
End Sub



Sub Movie_Outro()
	RampDown
	ScoreText002.Text = ""
	ScoreText003.Text = ""
	If ShowFlexDMD Then	
		DMD_StopSplashBG
		DMDStop_Starfield
		FlexDMD.Stage.GetImage("Planetoid5").Visible = 0
		FlexDMD.Stage.GetImage("Planetoid6").Visible = 0
		FlexDMD.Stage.GetImage("VenusEyes1").Visible = 0
		FlexDMD.Stage.GetImage("VenusEyes2").Visible = 0
		FlexDMD.Stage.GetImage("VenusEyes3").Visible = 0
		DMDShowBallCred
	End If
	M_Power.Enabled = False
	M_Venus.Enabled = False
	MovieReel.image = "XL5MovieBlank"
	MovieReelFlasher.imageA= "XL5_MovieClear"
	StopSound "XL5_Mat"
	StopSound "XL5_Flight"
	StopSound "XL5_SubDie"
'	FadeOutSound CurrentMusic, 1
	MusicTimer.Enabled = False
	M_OutroCounter = 0
	M_Outro.Interval = 120
	M_Outro.Enabled = True
End Sub

Sub M_Outro_Timer()
	M_OutroCounter = M_OutroCounter + 1
	If M_OutroCounter = 1 Then
		PlaySound "M_Outro"
		LightSequence(-1)
	ElseIf M_OutroCounter = 75 Then
		MovieReel.image = "GameOver"
		MovieReelFlasher.imageA = "XL5_MovieClear"
		GameOver.Interval = 1000
		GameOver.Enabled = True
		M_Outro.Enabled = False
		Exit Sub
	End If
	MovieString = "XL5_Outro000" & M_OutroCounter
	MovieReel.image = MovieString
	MovieReelFlasher.imageA = MovieString
End Sub

Sub Movie_Radio()
	MovieReel.image = "XL5MovieBlank"
	MovieReelFlasher.imageA= "XL5_MovieClear"
	M_OutroCounter = 0
	M_Radio.Interval = 1000
	M_Radio.Enabled = True
End Sub

Sub M_Radio_Timer()
	M_Radio.Interval = 90
	M_OutroCounter = M_OutroCounter + 1
	If M_OutroCounter = 1 Then
		PlaySound "XL5_TowerSnd"
	ElseIf M_OutroCounter = 56 Then
		MovieReel.image = "XL5MovieBlank"
		MovieReelFlasher.imageA= "XL5_MovieClear"
		M_OutroCounter = 0
		M_IntroCounter = 0
		M_Radio.Enabled = False
		GameReady = True
		If Credits > 0 Then
			ScoreText002.Text = "PRESS"
			ScoreText003.Text = " START"
			If ShowFlexDMD Then DMDBlinkSplashString "PRESS START", 10, False, False
		Else 
			ScoreText002.Text = "INSERT"
			ScoreText003.Text = "  COIN"
			If ShowFlexDMD Then DMDBlinkSplashString "INSERT COIN", 10, False, False
		End If
		Movie_Intro
		Exit Sub
	End If
	MovieString = "XL5_SpaceCityTower000" & M_OutroCounter
	MovieReel.image = MovieString
	MovieReelFlasher.imageA = MovieString
End Sub


Sub Movie_MBall()
	MovieReel.image = "XL5MovieBlank"
	MovieReelFlasher.imageA = "XL5_MovieClear"
	MBallCounter = 0
	M_MBall.Interval = 100
	M_MBall.Enabled = True
End Sub

Sub M_MBall_Timer()
	M_MBall.Interval = 45
	MBallCounter = MBallCounter + 1
	If MBallCounter = 1 Then
		PlaySound "XL5_Multiball"
	ElseIf MBallCounter > 85 Then
		BlinkFrame "XL5_MBX", "XL5_MBX1", 24, 45, True
		PlaySound "XL5_Boom1"
		MBallCounter = 0 
		M_MBall.Enabled = False
		Exit Sub
	Else
		MovieString = "XL5_Multiball000" & MBallCounter
		MovieReel.image = MovieString
		MovieReelFlasher.imageA = MovieString
	End If
End Sub

Sub Movie_EBall()
	If Not RobTalking Then
		MovieReel.image = "XL5MovieBlank"
		MovieReelFlasher.imageA = "XL5_MovieClear"
		EballCounter = 170
		M_EBall.Interval = 60
		M_EBall.Enabled = True
	Else
		SoundKnocker
	End If
End Sub


Sub M_EBall_Timer()
	M_EBall.Interval = 60
	EballCounter = EballCounter + 1
	If EballCounter = 187 Then
		PlaySound "XL5_Boom1"
	ElseIf EballCounter = 250 Then
		EballCounter = 0 
		MovieReel.image = ""
		MovieReelFlasher.imageA = "XL5_MovieClear"
		M_EBall.Enabled = False
		Movie_RobTalk "XL5_ExtraballSnd", 19
		Exit Sub
	ElseIf EballCounter < 214 Then
		MovieString = "XL5_EBall00" & EballCounter
		MovieReel.image = MovieString
		MovieReelFlasher.imageA = MovieString
	ElseIf EballCounter = 214 Then
		BlinkFrame "XL5_ExtraBall00050", "XL5_ExtraBall00049", 20, 45, True
	End If
End Sub



'########### Light Sequences

Sub LightSequence(seqnum)
	If seqnum < 0 Then
		LightSeq001.UpdateInterval = 8
		LightSeq001.Play SeqDiagDownLeftOn, 35
		LightSeq001.Play SeqDiagDownRightOn, 35
		LightSeq001.Play SeqUpOn, 25
		LightSeq001.Play SeqDownOn, 25
		LightSeq001.UpdateInterval = 5
		LightSeq001.Play SeqHatch1HorizOn, 25, 2
		LightSeq001.Play SeqCircleOutOn, 25, 5
	End If
	If seqnum = 99 Then LightSeq001.Play SeqAllOff
	If seqnum = 0 Then LightSeq001.Play SeqAllOn

	If seqnum = 1 Then
		LightSeq001.Play SeqDiagDownRightOn, 35
		LightSeqJackpot.UpdateInterval = 8
		LightSeqJackpot.Play SeqDownOn, 25, 3
	End If

	If seqnum = 2 Then LightSeqSpecial.Play SeqRightOn, 25, 3

End Sub



'########## Robert the Robot - Messages

Sub Movie_RobTalk(Strng2, framenum)
	If RobTalking Then
		Exit Sub
	End If
	If VenusActive Then
		RobTalking = True
		PlaySound Strng2
		RobTalk2.Interval = 2000
		RobTalk2.Enabled = True
		Exit Sub
	End If
	If (EballCounter > 0 Or MBallCounter > 0) And Not ShowFlexDMD Then
		RobTalking = True
		PlaySound Strng2
		RobTalk2.Interval = 2000
		RobTalk2.Enabled = True
		Exit Sub
	End If
	RobTalking = True
	MovieReel.image = "XL5MovieBlank"
	MovieReelFlasher.imageA = "XL5_MovieClear"
	RobTalkSound = Strng2
	MovieCounter = 23
	RobTalkFrames = framenum + MovieCounter
	RobTalk.Interval = 10
	RobTalk.Enabled = True
End Sub

Sub RobTalk_Timer()
	RobTalk.Interval = 90
	MovieCounter = MovieCounter + 1
	If MovieCounter = 27 Then
		PlaySound RobTalkSound
		Rob_VidFlash.State = 2		
	ElseIf MovieCounter = (RobTalkFrames - 5) Then
		Rob_VidFlash.State = 0
	ElseIf MovieCounter = RobTalkFrames Then
		MovieReel.image = ""
		MovieReelFlasher.imageA = "XL5_MovieClear"
		RobTalking = False
		RobTalk.Enabled = False
		MovieCounter = 0
		Exit Sub
	End If
	MovieString = "XL5RobTalk-000" & MovieCounter
	If Not InMode And EballCounter = 0 And MBallCounter = 0 Then MovieReel.image = MovieString
	MovieReelFlasher.imageA = MovieString
End Sub

Sub RobTalk2_Timer()
	RobTalking = False
	RobTalk2.Enabled = False
End Sub


'######### TILT


Sub Tilt_Game()
	Dim i
	Tilted = True
	LightSequence(99)
	StopSound CurrentMusic
	Ballsaver = 0
	BallSave.Enabled = False
	Ballsave_Light.state = 0
	BallSaveString = ""
	LeftFlipper.RotateToStart
	SoundFlippers("LfDown")
	RightFlipper.RotateToStart
	SoundFlippers("RfDown")
	For i = 1 to 3
		Eval("Bumper00" & i).Collidable = False
	Next

	DMD_VideoMode = False
	If WizardActive Then Stop_Wizard
End Sub

Sub UnTilt_Game()
	Dim i
	Tilted = False
	LightSeq001.StopPlay()
	CurrentMusicNum = 0 		'psychological 'restart'
	MusicTimer.Interval = 1000
	TiltReel.Visible = False
	TiltBox.Text = ""
	For i = 1 to 3
		Eval("Bumper00" & i).Collidable = True
	Next
	If Bonus > 0 Then 
		TiltBonus.Enabled = True
	Else
		NewBall
	End If
End Sub

Sub TiltBonus_Timer()
	TiltBonus.Interval = 250
	Eval("Bonus_Light00" & Bonus).state = 0
	Eval("Bonus_Light00" & Bonus & "A").state = 0
	SoundSpinnerHit
	Bonus = Bonus - 1
	If Bonus = 0 Then
		NewBall
		TiltBonus.Enabled = False
	End If
End Sub

Sub Check_Tilt()
	Tiltnum = Tiltnum + 1
	If Tiltnum > 3 Then 
		Tilt_Game
		ScoreFlash( "  TILT")
		TiltReel.Visible = True
		TiltBox.Text = "TILT"
	Elseif 	Tiltnum > 2 Then
		ScoreFlash( "WARNING")
		If ShowFlexDMD Then DMDBlinkSplashString "WARNING", 5, True, True
	End If
	Tilt.Interval = 500
	Tilt.Enabled = True
End Sub

Sub Tilt_Timer()
	Tiltnum = Tiltnum - 1
	If Tiltnum = 0 Then
		Tilt.Enabled = False
	End If
End Sub


Sub MusicTimer_Timer()
	If PowerActive Or VenusActive Then 
		MusicTimer.Interval = 3000 
		Exit Sub
	End If
	If CurrentMusic = "XL5_UFO" And Multiball Then
		PlaySound CurrentMusic,0,0.99,0,0,0,1,0,0
		MusicTimer.Interval = 70000
	Else
		CurrentMusicNum = CurrentMusicNum + 1
		StopSound CurrentMusic
		Select Case CurrentMusicNum
			Case 1
				CurrentMusic = "XL5_Theme"
				MusicTimer.Interval = 123000
			Case 2
				CurrentMusic = "XL5_Joe90A"
				MusicTimer.Interval = 85000
			Case 3
				CurrentMusic = "XL5_Jazz2"
				MusicTimer.Interval = 88500
			Case 4
				CurrentMusic = "XL5_ThemeInstr"
				MusicTimer.Interval = 68000
			Case 5
				CurrentMusic = "XL5_Incidental_1min"
				MusicTimer.Interval = 60000 
				CurrentMusicNum = 0
		End Select
		PlaySound CurrentMusic
	End If
End Sub

'######### Flash Active Player Score

Sub FlashPlayerScore()
	FlashPlayer.Interval = 400
	FlashPlayer.Enabled = True
End Sub

Sub FlashPlayer_Timer()
	If FPSwap Then
		Eval("ScorePlayer00" & Player).Text = ""
		If ShowFlexDMD Then FlexDMD.Stage.GetLabel("Score_" & Player).Visible = False
		FPSwap = False
	Else
		Eval("ScorePlayer00" & Player).Text = ScoreString(Player)
		If ShowFlexDMD Then FlexDMD.Stage.GetLabel("Score_" & Player).Visible = True
		FPSwap = True
	End If
End Sub


'############ Flash Text Messages on display ######

Sub ScoreFlash(flash_text)
	FlashText = flash_text
	FlashNum = 6
	ScoreFlashTimer.Interval = 200
	ScoreFlashTimer.Enabled = True
End Sub


Sub ScoreFlashTimer_Timer()
	Select Case FlashNum
		Case 3,5,7
			ScoreText001.Text = ""
		Case 4,6,8
			ScoreText001.Text = FlashText
		Case 2
			ScoreText001.Text = FlashText
			ScoreFlashTimer.Interval = 1000
		Case 1
			ScoreFlashTimer.Enabled = False
			ScoreText001.Text = ""	'GameText
	End Select
	FlashNum = FlashNum - 1
End Sub

'####### Scroll text on SoreText + Flash #####

Sub ScrollDMDText(txt)
	ScrollText = txt
	ScrollCounter = 1
	ScoreText001.Text = ""
	ScrollTextTimer.Interval = 100
	ScrollTextTimer.Enabled = True
End Sub


Sub ScrollTextTimer_Timer()
	ScoreText001.Text = Left(ScrollText, ScrollCounter)
	ScrollCounter = ScrollCounter + 1
	If ScrollCounter > Len(ScrollText) Then
		ScrollTextTimer.Enabled = False	
		ScoreFlash(ScrollText)
	End If
End Sub


'########### Flash FIREBALL Letters ####

Sub Flash_Fireball()
	FireballCounter2 = 1
	FlashFireball.Interval = 40
	FlashFireball.Enabled = True
End Sub

Sub FlashFireball_Timer()
	Dim i
	FlashFireball.Interval = 150
	Select Case FireballCounter2
		Case 1,3,5,7,9
			For i = 1 to 11
				Eval("Fireball00" & i).state = 1
			Next
		Case 2,4,6,8,10
			For i = 1 to 11
				Eval("Fireball00" & i).state = 0
			Next
	End Select
	FireballCounter2 = FireballCounter2 + 1
	If FireballCounter2 = 11 Then
		ResetXL5
		FlashFireball.Enabled = False
	End If
End Sub

Sub Flash_XL5()
	Dim i
	FireballCounter3 = 1
	XL5Loop = 0
	For i = 9 to 11
		Eval("Fireball00" & i).state = 0
		Eval("XL5_Light" & (i - 8)).state = 0
	Next
	FlashXL5.Interval = 30
	FlashXL5.Enabled = True
End Sub

Sub FlashXL5_Timer()
	FlashXL5.Interval = 100

	If FireballCounter3 > 3 Then
		Eval("Fireball00" & (FireballCounter3 + 5)).state = 0
		Eval("XL5_Light" & (FireballCounter3 - 3)).state = 0
	Else
		Eval("Fireball00" & FireballCounter3 + 8).state = 1
		Eval("XL5_Light" & FireballCounter3).state = 1
	End If
	FireballCounter3 = FireballCounter3 + 1
	If FireballCounter3 = 7 Then
		FireballCounter3 = 1
		XL5Loop = XL5Loop + 1
	End If
	If XL5Loop = 3 Then
		ResetXL5
		FlashXL5.Enabled = False
	End If
End Sub

Sub ResetXL5()
	Fireball009.State = Lite1(Player)
	Fireball0010.State = Lite2(Player)
	Fireball0011.State = Lite3(Player)
	XL5_Light1.State = Lite1(Player)
	XL5_Light2.State = Lite2(Player)
	XL5_Light3.State = Lite3(Player)
	If SkillShotTarget > 0 Then
		Dim Strng2
		Strng2 = "XL5_Light" & SkillShotTarget
		Eval(Strng2).State = 2
	End If
End Sub


'#### Cycle Letters Left to Right + Flash 

Sub CycleFireball()
	FireballCounter = 1
	Fireball.Interval = 30
	Fireball.Enabled = True
End Sub

Sub Fireball_Timer()
	If FireballCounter > 8 Then
		Eval("Fireball00" & (FireballCounter - 8)).state = 0
	Else
		Eval("Fireball00" & FireballCounter).state = 1
	End If
	FireballCounter = FireballCounter + 1
	If FireballCounter = 17 Then
		Fireball.Enabled = False
		FireballCounter2 = 1
		FlashWASP(6)
		Flash_XL5

		FlashFireball.Interval = 3000	'delay between cycle and flash 
		FlashFireball.Enabled = True
	End If
End Sub



Sub Flash_Light(LightName)
	Flash_bulb = LightName
	Eval(Flash_bulb).state = 2
	Flash_OneLight.Interval = 3000
	Flash_OneLight.Enabled = True
End Sub

Sub Flash_OneLight_Timer()
	Eval(Flash_bulb).state = 1
	Flash_OneLight.Enabled = False
End Sub



'################## Scores & Chimes ##########


Sub ShowScore()
	If Score(Player) > 99999999 And TenBillion(Player) < 1 Then	
			AddSpecial
			TenBillion(Player) = 1
	ElseIf Score(Player) > 499999999 And TenBillion(Player) < 2 Then
			AddSpecial
			TenBillion(Player) = 2
	ElseIf Score(Player) > 999999999 And TenBillion(Player) < 3 Then
			AddSpecial
			TenBillion(Player) = 3
	ElseIf Score(Player) > 9999999999 And TenBillion(Player) < 4 Then
			AddSpecial
			PlaySound "XL5_OhSteve"
			TenBillion(Player) = 4
	End If

		
	If Score(Player) > 999 Then
		ScoreString(Player) = Left(Score(Player), Len(Score(Player)) - 3) & "," & Right(Score(Player), 3)
	Else
		ScoreString(Player) = Score(Player)
	End If
	If Score(Player) > 999999 Then
		ScoreString(Player) = Left(Score(Player), Len(Score(Player)) - 6) & "," & Right(ScoreString(Player), 7)
	End If
	If Score(Player) > 999999999 Then
		ScoreString(Player) = Left(Score(Player), Len(Score(Player)) - 9) & "," & Right(ScoreString(Player), 11)
	End If
	If Len(ScoreString(Player)) < 13 Then ScoreString(Player) = Left("0,000,000,000", 13-Len(ScoreString(Player))) & ScoreString(Player)	
	ScoreBoard.Text = ScoreString(Player)
	Eval("ScorePlayer00" & Player).Text = ScoreString(Player)

	If ShowFlexDMD Then DMDShowScore	

End Sub




Sub Score_10()
	SkillShotTarget = 0
	If Not Tilted Then
		PlaySound "Bell10_XL5", (MaxSoundLevel * SpinnerSoundFactor)
		Score(Player) = Score(Player) + (10 * ScoreX)
		ShowScore
	End If
End Sub

Sub Score_50()
	SkillShotTarget = 0
	If Not Tilted Then
		BellCount50 = BellCount50 + 5
		BellTimer50.Enabled = 1
	End If
End Sub

Sub BellTimer50_Timer()
	PlaySound "Bell10_XL5", (MaxSoundLevel * SpinnerSoundFactor)
	Score(Player) = Score(Player) + (10 * ScoreX)
	ShowScore
	BellCount50 = BellCount50 - 1
	If BellCount50 = 0 Then
		BellTimer50.Enabled = 0
	End If
End Sub

Sub Score_100()
	SkillShotTarget = 0
	If Not Tilted Then
		PlaySound "Bell100_XL5", (MaxSoundLevel * SpinnerSoundFactor)
		Score(Player) = Score(Player) + (100 * ScoreX)
		ShowScore
	End If
End Sub


Sub Score_500()
	SkillShotTarget = 0
	If Not Tilted Then
		BellCount500 = BellCount500 + 5
		BellTimer500.Enabled = 1
	End If
End Sub

Sub BellTimer500_Timer()
	PlaySound "Bell100_XL5", (MaxSoundLevel * SpinnerSoundFactor)
	Score(Player) = Score(Player) + (100 * ScoreX)
	ShowScore
	BellCount500 = BellCount500 - 1
	If BellCount500 = 0 Then
		BellTimer500.Enabled = 0
	End If
End Sub


Sub Score_1000()
	SkillShotTarget = 0
	If Not Tilted Then
		PlaySound "Bell1000", (MaxSoundLevel)
		Score(Player) = Score(Player) + (1000 * ScoreX)
		ShowScore
	End If
End Sub


Sub Score_5000()
	SkillShotTarget = 0
	If Not Tilted Then
		BellCount5000 = BellCount5000 + 5
		BellTimer5000.Enabled = 1
	End If
End Sub

Sub BellTimer5000_Timer()
	PlaySound "Bell1000", (MaxSoundLevel * SpinnerSoundFactor)
	Score(Player) = Score(Player) + (1000 * ScoreX)
	ShowScore
	BellCount5000 = BellCount5000 - 1
	If BellCount5000 = 0 Then
		BellTimer5000.Enabled = 0
	End If
End Sub

Sub Score_10000()
	SkillShotTarget = 0
	If Not Tilted Then
		PlaySound "Bell10000_XL5", (MaxSoundLevel)
		Score(Player) = Score(Player) + (10000 * ScoreX)
		ShowScore
	End If
End Sub

Sub Score_50000()
	SkillShotTarget = 0
	If Not Tilted Then
		BellCount50000 = BellCount50000 + 5
		BellTimer50000.Enabled = 1
	End If
End Sub

Sub BellTimer50000_Timer()
	PlaySound "Bell10000_XL5", (MaxSoundLevel * SpinnerSoundFactor)
	Score(Player) = Score(Player) + (10000 * ScoreX)
	ShowScore
	BellCount50000 = BellCount50000 - 1
	If BellCount50000 = 0 Then
		BellTimer50000.Enabled = 0
	End If
End Sub

'########## Collect Bonus Lights


Sub CollectBonus()
	Super_Bonus = Bonus_held
	Select Case Super_Bonus
		Case 1
			Bonus_Light_10X.state = 2
		Case 2
			Bonus_Light_20X.state = 2
		Case 3
			Bonus_Light_30X.state = 2
	End Select

	Select Case BonusX(Player)
		Case 2 
			Light_2X.state = 2
		Case 3 
			Light_3X.state = 2
		Case 4 
			Light_5X.state = 2
	End Select

	BonusTimer.Interval = 200	' Fast Count
	BonusTimer.Enabled = True
End Sub

Sub Score_Bonus()	
	Select Case BonusX(Player)
		Case 1 
			Bonus_1000
		Case 2 
			Bonus_2000Fast
		Case 3 
			Bonus_3000Fast
		Case 4 
			Bonus_5000Fast
	End Select
End Sub


Sub Bonus_1000()
	PlaySound "Bell10", (MaxSoundLevel)
	Score(Player) = Score(Player) + (10000 * ScoreX)
	Jackpot(Player) = Jackpot(Player) + (10000 * ScoreX)
	ShowScore
End Sub

'############## Fast Collect Bonus and Increase Jackpot #########

Sub Bonus_2000Fast()
	PlaySound "Bell100", (MaxSoundLevel)
	Score(Player) = Score(Player) + (20000 * ScoreX)
	Jackpot(Player) = Jackpot(Player) + (20000 * ScoreX)
	ShowScore
End Sub

Sub Bonus_3000Fast()
	PlaySound "Bell1000", (MaxSoundLevel)
	Score(Player) = Score(Player) + (30000 * ScoreX)
	Jackpot(Player) = Jackpot(Player) + (30000 * ScoreX)
	ShowScore
End Sub

Sub Bonus_5000Fast()
	PlaySound "Bell10000", (MaxSoundLevel)
	Score(Player) = Score(Player) + (50000 * ScoreX)
	Jackpot(Player) = Jackpot(Player) + (50000 * ScoreX)
	ShowScore
End Sub

' ##### Old school EM bonus (slow)

Sub Bonus_2000()
	CounterBonusX = 2
	BonusTimerX.Interval = 120
	BonusTimerX.Enabled = True
End Sub

Sub Bonus_3000()
	CounterBonusX = 3
	BonusTimerX.Interval = 100
	BonusTimerX.Enabled = True
End Sub

Sub Bonus_5000()
	CounterBonusX = 5
	BonusTimerX.Interval = 80
	BonusTimerX.Enabled = True
End Sub



Sub BonusTimerX_Timer()
	Score_1000
	CounterBonusX = CounterBonusX - 1
	If CounterBonusX = 0 Then
		BonusTimerX.Enabled = False
	End If
End Sub


'############ Bonus Countdown

Sub BonusTimer_Timer
	Dim i
	If Bonus > 0 Then
		Eval("Bonus_Light00" & Bonus).state = 0
		Eval("Bonus_Light00" & Bonus & "A").state = 0
		Score_Bonus
	End If
	If Bonus = 0 Then 
		If Super_Bonus > 0 Then
			Select Case Super_Bonus
				Case 1
					Bonus_Light_10X.state = 0
				Case 2
					Bonus_Light_20X.state = 0
					Bonus_Light_10X.state = 2
				Case 3
					Bonus_Light_30X.state = 0
					Bonus_Light_20X.state = 2
			End Select
			Super_Bonus = Super_Bonus - 1
			Bonus = 10
			Score_Bonus
			For i = 1 to 9
				Eval("Bonus_Light00" & i).state = 1
				Eval("Bonus_Light00" & i & "A").state = 1
			Next
		Else		
			BonusTimer.Enabled = False
			Bonus = 1
			Bonus_held = 0
			Select Case BonusX(Player)
				Case 2 
					Light_2X.state = 1
				Case 3
					Light_3X.state = 1
				Case 4
					Light_5X.state = 1
			End Select
			NewBall
		End If
	End If
	Bonus = Bonus - 1
End Sub

'################# New Ball, Next Player, or End Game ########

Sub NewBall()
	Dim i
	BIP = BIP - 1
	Ballsaved = 0
	Spinner_Lit = 0
	BonusMaxed = False
	Robert_Light001.state = 0
	FlashAdBonus(2)
	Spot_LightBlue.state = 2
	If Not PowerActive Then SpinLights_Timer
	Target_BugCheck
	Top_Target_Reset
	TopTarget_Reset

	If BIP = 0 Then
		If EBall(Player) > 0 Then
			EBall(Player) = EBall(Player) - 1
			ShowCountdown
			If EBall(Player) = 0 Then
				EB_Light.state = 0
			End If
			ScoreFlash(" PLAYER " & Player & " SHOOT AGAIN")
			If ShowFlexDMD Then	DMDDoubleSplashString ("PLAYER " & Player), "SHOOT AGAIN", True, 4, False


'			SkillShotTarget = Int(Rnd*3) + 1
'			Eval("XL5_Light" & SkillShotTarget).State = 2
'			If ShowFlexDMD Then FlexDMD.Stage.GetImage("Skill_" & SkillShotTarget).Visible = 1
'			SkillShotCount = 0
'			SkillshotActive = False
'			BallX = 0
'			SkillShot.Interval = 100
'			SkillShot.Enabled = True


			LaunchBall()
			Exit Sub
		Else
			BallNum(Player) = BallNum(Player) -1
		End If
	End If
	If Player = Players And BallNum(Player) = 0 Then
		Tilt_Game
'		FadeOutSound CurrentMusic, 1
		PlayfieldTimer.Enabled = False
		ScoreText003.Text = ""

		InMode = False
		VenusActive = False
		PowerActive = False
'		WizardActive = False
'		If WizardActive Then Stop_Wizard
		M_Venus.Enabled = False
		If ShowFlexDMD Then
			For i = 0 to 27
				FlexDMD.Stage.GetImage("Speedo" & i).Visible = 0
			Next
			FlexDMD.Stage.GetImage("SpeedoBG").Visible = 0
		End If
		Kicker001.DestroyBall
		Kicker002.DestroyBall
		Movie_Outro
		Exit Sub
	Else
		Player = Player + 1				'Cycle Players
		If Player > Players Then
			Player = 1
		End If
		If Players > 1 Then
			ScoreFlash("  PLAYER " & Player)
			ShowScore

			ResetPlayfield
		End If
		SkillShotTarget = Int(Rnd*3) + 1
		Eval("XL5_Light" & SkillShotTarget).State = 2
		If ShowFlexDMD Then FlexDMD.Stage.GetImage("Skill_" & SkillShotTarget).Visible = 1
		SkillshotActive = False
		BallX = 0
		SkillShot.Interval = 100
		SkillShot.Enabled = True
		LaunchBall()
	End If
End Sub

Sub LaunchBall()
		Ballsaver = 99 
		ScoreText002.Text = "Ball " & (GameBalls - BallNum(Player))

		If ShowFlexDMD Then 
			ShowCountdown
			DMDShowSplash = False
		End If
		ShowScore
		BallRelease.CreateBall
		BallRelease.Kick 90, 7
		SoundBallRelease
		BIP = BIP + 1
		FlashPlayerScore
End Sub


'####### Plunge-lane Star Trigger, activates Ball Save, Multiball, Skillshot and Validates Game


Sub TriggerMain_Hit()
	ScoreText001.Text = ""
	FlashPlayer.Enabled = False


	ShowScore
	If Ballsaver = 99 Then
		Ballsaver = BallSaveTime - Ballsaved
		ShowCountdown
		BallSave.interval = 100
		BallSaveCounter = 10
		BallSave.Enabled = True
		Ballsave_Light.state = 2
	End If
	Check_Multiball
	GameValidated = True
	If SkillShotTarget > 0 Then
		BallX = 0
		SkillShotCount = 0
		Skillswap = True
		SkillshotActive = True
	End If
End Sub


Sub TriggerRamp_Hit()
	PlaySound "WireRamp"
End Sub


Dim SkillShotCount, SkillshotActive, Skillswap



Sub SkillShot_Timer()
	Dim i
	SkillShot.Interval = 10

	If BallX > 0 Then
		If ShowFlexDMD Then FlexDMD.Stage.GetImage("Skill_4").Visible = 0
		XL5_Light1.state = Lite1(Player)
		XL5_Light2.state = Lite2(Player)
		XL5_Light3.state = Lite3(Player)
		SkillShot.Enabled = False
		SkillshotActive = False
		Exit Sub
	End If


	SkillShotCount = SkillShotCount + 1
	If SkillShotCount = 401  Or SkillShotTarget = 0 Then
		If ShowFlexDMD Then 
			FlexDMD.Stage.GetImage("Skill_4").Visible = 0	
			FlexDMD.Stage.GetImage("Skill_1").Visible = 0	
			FlexDMD.Stage.GetImage("Skill_2").Visible = 0	
			FlexDMD.Stage.GetImage("Skill_3").Visible = 0	
		End If
		XL5_Light1.state = Lite1(Player)
		XL5_Light2.state = Lite2(Player)
		XL5_Light3.state = Lite3(Player)
		SkillShot.Enabled = False
		SkillshotActive = False
		Exit Sub
	End If

	If ShowFlexDMD Then 
		If SkillShotCount Mod 20 = 0 Then
			If Skillswap Then
				FlexDMD.Stage.GetImage("Skill_4").Visible = 0	
				Skillswap = False
			Else
				FlexDMD.Stage.GetImage("Skill_4").Visible = 1
				Skillswap = True
			End If
		End If
	End If
	If SkillShotCount = 400 Then
		If SkillshotActive Then
			SkillShotTarget = 0
			SkillShot.Interval = 1000
'			If ShowFlexDMD Then DMD_CloseDiamond4(1)
		Else
			SkillShotCount = 0
		End If
	End If
End Sub


'######### BallSave

Sub BallSave_Timer()
	Dim i
	If Ballsaver > 9 Then
		BallSave.interval = 1000
		BallSaveString = Ballsaver
		ShowCountdown
		Ballsaver = Ballsaver - 1
	Else
		BallSave.interval = 100
		BallSaveCounter = BallSaveCounter - 1
		BallSaveString = Ballsaver &"." & BallSaveCounter
		ShowCountdown
		If BallSaveCounter = 0 Then
			Ballsaver = Ballsaver - 1
			BallSaveCounter = 10
		End If
	End If
	If Ballsaver < 0 Then
		If ShieldActive Then
			ShieldActive = False
			M_Gamma.Enabled = False

			If Not Multiball Then
'				CurrentMusic = "XL5_UFO"
'				PlaySound "XL5_UFO"
'				MusicTimer.Interval = 70000
'			Else
				FadeOutSound CurrentMusic, 1
'				PlaySound "XL5_Upbeat"
				MusicTimer.Interval = 6000
			End If
			For i = 1 to 3
				Eval("LightBumperRed00" & i).state = 0
			Next
			MovieReel.image = ""
			MovieReelFlasher.imageA = "XL5_MovieClear"
		End If
		BallSaveString = ""
		ShowCountdown
		Ballsave_Light.state = 0
		BallSave.Enabled = False
	End If
End Sub

Dim WizardScore


Sub Stop_Wizard()
	Dim i
	WizardActive = False
	FlashSteveVenus 3, 2
	InlaneLeftRed.state = 0
	InlaneRightRed.state = 0
	OutlaneLeftRed.state = 0
	OutlaneRightRed.state = 0
	KickBack_Light.State = 0
	KickBack2_Light.State = 0
	KickBack.Enabled = False
	KickBack2.Enabled = False
	TopTarget_Reset
	For i = 1 to 3
		Eval("LightBumperRed00" & i).state = 0
	Next
	Spot_Light001.state = Spot1(Player)
	Spot_Light002.state = Spot2(Player)
	Spot_Light003.state = Spot3(Player)
	Spot_Light004.state = Spot4(Player)
	Spot_Light005.state = Spot5(Player)
	Spots_hit = Spot1(Player) + Spot2(Player) + Spot3(Player) + Spot4(Player) + Spot5(Player)
	If Spots_hit = 4 Then
		For i = 1 to 5
			If Eval("Spot_Light00" & i).state = 0 Then
				Eval("Spot_Light00" & i).state = 2
			End If
		Next
	End If
	For i = 1 to 6
		Eval("Mode0" & i).Visible = False
	Next


	DMD_FireballWiz(Player) = 0
	ChaseOn = 0
	For i = 1 to 8
		Eval("Chaser00" & i).Visible = False
		Eval("Chaser00" & i).TimerEnabled = False
	Next



	Ambient009.state = 0
	VenusWon(Player) = False
	PowerWon(Player) = False
	MeteorWon(Player) = False
	Warp9Won(Player) = False
	M_PowerOn = False
	InMode = False
	SpinLights_Timer

	Dim WizString

	WizardScore = Score(Player) - WizardScore
	If WizardScore > 999 Then
		WizString = Left(WizardScore, Len(WizardScore) - 3) & "," & Right(WizardScore, 3)
	Else
		WizString = WizardScore
	End If
	If Score(Player) > 999999 Then
		WizString = Left(WizardScore, Len(WizardScore) - 6) & "," & Right(WizString, 7)
	End If
	If WizardScore > 999999999 Then
		WizString = Left(WizardScore, Len(WizardScore) - 9) & "," & Right(WizString, 11)
	End If
	If ShowFlexDMD Then DMDDoubleSplashString "WIZARD TOTAL", WizString, True, 5, True
End Sub


Sub ResetPlayfield()		'For 2+ Players, restore Player's progress 
	Dim i
	RampDown
	Ball_Locked = 0
	For i = 1 to 3
		Eval("LightBumper00" & i).state = 2
		Eval("LightBumperLow00" & i).state = 2
	Next

	For i = 1 to 8
		Eval("Mode0" & i).Visible = False
	Next
	If VenusWon(Player) Then Mode03.Visible = True
	If PowerWon(Player) Then Mode01.Visible = True
	If Warp9Won(Player) Then Mode04.Visible = True: Mode06.Visible = True: Mode08.Visible = True
	If MeteorWon(Player) Then Mode02.Visible = True: Mode05.Visible = True: Mode07.Visible = True


	XL5_Light1.state = Lite1(Player)
	LightBumper001.state = Bumper1(Player)
	LightBumperLow001.state = Bumper1(Player)
	XL5_Light2.state = Lite2(Player)
	LightBumper003.state = Bumper3(Player)
	LightBumperLow003.state = Bumper3(Player)
	XL5_Light3.state = Lite3(Player)
	LightBumper002.state = Bumper2(Player)
	LightBumperLow002.state = Bumper2(Player)

	Lock_Light001.state = 0
	Lock_Light002.state = 0
	If LocksLit(Player) Then
		Lock_Light001.state = 2
		Lock_Light002.state = 2
	End If

	Light_2X.state = 0
	Light_3X.state = 0
	Light_5X.state = 0
	Select Case BonusX(Player)
		Case 2 
			Light_2X.state = 1
		Case 3
			Light_3X.state = 1
		Case 4
			Light_5X.state = 1
	End Select

	If BonusCollect(Player) Then
		CollectBonus_Light.State = 2
		CollectBonus_Light2.State = 2
	Else
		CollectBonus_Light.State = 0
		CollectBonus_Light2.State = 0
	End If


	Top_Target_Reset	
	Target_Reset
	TopTarget_Reset
	TopTargets_down = 0
	Targets_down = 0
	Banks_down = 0
	For i = 3 to 7
		Eval("Light00" & i).state = 0
	Next
	For i = 10 to 16
		Eval("Light0" & i).state = 0
	Next

	If EBallCollect(Player) > 0 Then
		EB_Light001.State = 2
		EB_Light002.State = 2
	Else
		EB_Light001.State = 0
		EB_Light002.State = 0
	End If

	Special_Light.state = Special(Player)
	For i = 1 to 7
		Eval("SpecialLetter" & i).State = 0
	Next
	For i = 2 to 9
		Eval("Jackpot_Light00" & i).State = JackpotLit(Player)
	Next
	Spot_Light001.state = Spot1(Player)
	Spot_Light002.state = Spot2(Player)
	Spot_Light003.state = Spot3(Player)
	Spot_Light004.state = Spot4(Player)
	Spot_Light005.state = Spot5(Player)
	Spots_hit = Spot1(Player) + Spot2(Player) + Spot3(Player) + Spot4(Player) + Spot5(Player)
	If Spots_hit = 4 Then
		For i = 1 to 5
			If Eval("Spot_Light00" & i).state = 0 Then
				Eval("Spot_Light00" & i).state = 2
			End If
		Next
	End If


	InMode = False
	If ShowFlexDMD Then	
		DMD_StopSplashBG
		DMDStop_Starfield
		FlexDMD.Stage.GetImage("Planetoid5").Visible = 0
		FlexDMD.Stage.GetImage("Planetoid6").Visible = 0
		FlexDMD.Stage.GetImage("VenusEyes1").Visible = 0
		FlexDMD.Stage.GetImage("VenusEyes2").Visible = 0
		FlexDMD.Stage.GetImage("VenusEyes3").Visible = 0
	End If
	M_Power.Enabled = False
	M_Venus.Enabled = False
	MovieReel.image = "XL5MovieBlank"
	MovieReelFlasher.imageA = "XL5_MovieClear"
	StopSound "XL5_Mat"
	StopSound "XL5_Flight"

	VenusActive = False
	M_PowerOn = False
	PowerActive = False
	ShieldActive = False
	WizardActive = False

	M_Gamma.Enabled = False
	For i = 1 to 3
		Eval("LightBumperRed00" & i).state = 0
	Next

	MovieReel.image = ""
	MovieReelFlasher.imageA = "XL5_MovieClear"

	SkillShot.Enabled = False

	Kicker001.DestroyBall
	Kicker002.DestroyBall

End Sub


Sub GameOver_Timer()
	Dim i, j
	j = Int(Rnd*10) * 10
	For i = 1 to Players
		If j = Eval(Right(Score(i), 2)) Then
			SoundKnocker
			AddCredit
		End If
	Next
	TextStr = j
	If j = 0 Then
		TextStr = "00"
	End If
	ScoreText001.Text = "                    Match: " & TextStr
	If ShowFlexDMD Then DMDDoubleSplashString "GAME OVER", ("Match: " & TextStr), True, 6, True
	If Credits > 0 Then
		ScoreText002.Text = "PRESS"
		ScoreText003.Text = " START"
 		If ShowFlexDMD Then DMDBlinkSplashString "PRESS START", 6, False, False
	Else
		ScoreText002.Text = "INSERT"
		ScoreText003.Text = "  COIN"
		If ShowFlexDMD Then DMDBlinkSplashString "INSERT COIN", 6, False, False
	End If
	CheckHighScore
	AttractModeActive = False
	HighScoreTimer.interval = 100
	HighScoreTimer.enabled = True
	GameOver.Enabled = False
'	SaveData
	InGame = False

End Sub

Dim HSStringNormal(5)


Sub AttractMode()
	Dim i, label
	For i = 5 to 1 Step -1
		HighScore = HSScore(i)
		If HighScore > 999 Then
			HighScoreString(i) = Left(HighScore, Len(HighScore) - 3) & "," & Right(HighScore, 3)
		End If
		If HighScore > 999999 Then
			HighScoreString(i) = Left(HighScore, Len(HighScore) - 6) & "," & Right(HighScoreString(i), 7)
		End If
		If HighScore > 999999999 Then
			HighScoreString(i) = Left(HighScore, Len(HighScore) - 9) & "," & Right(HighScoreString(i), 11)
		End If
		HSStringNormal(i) = HighScoreString(i) 
		If Len(HighScoreString(i)) < 13 Then HighScoreString(i) = Left("0,000,000,000", 13-Len(HighScoreString(i))) & HighScoreString(i)
	Next

	AttractCounter = 0
	Gallery = 0
	Attract.Enabled = True
End Sub

Sub Attract_Timer()
	Dim i
	Attract.Interval = 1500
	If Gallery = 0 Then
		FPSwap = False
		StopSound CurrentMusic
		CurrentMusic = "XL5_Theme"
		PlaySound CurrentMusic
		LightSequence(-1)
		FlashAll(10)
		If ShowFlexDMD Then
			DMDCounter = 0
			DMD_IntroCounter2 = 0
			DMDScrollSplash.Interval = 10
			DMDScrollSplash.Enabled = True
		End If
	End If	
	If FPSwap Then
		For i = 1 to 4
			Eval("ScorePlayer00" & i).Text = ScoreString(i)
		Next
		FPSwap = False
	Else	
		For i = 1 to 4
			Eval("ScorePlayer00" & i).Text = HighScoreString(1)	
		Next
		FPSwap = True
		Gallery = Gallery + 1
		If Gallery > 48 Then		
			Gallery = 0
'			PlaySound CurrentMusic
		End If
	End If
	MovieReel.image = "XL5_Gallery" & Gallery
	MovieReelFlasher.imageA = "XL5_Gallery" & Gallery

End Sub


'########## Increase Current Bonus 



Sub Increase_Bonus()
	If BonusMaxed Then
		Exit Sub
	End If
	Bonus = Bonus + 1
	If Bonus = 10 then
		If Bonus_held < 3 Then
			Bonus = 0
			Increase_Bonus_held
		Else
			Bonus = 9			' ### Bonus Maxed
			Movie_RobTalk "Rob_BonusMax", 30
			BonusMaxed = True
			If ShowFlexDMD Then DMDDoubleSplashString "BONUS", "AT MAXIMUM", True, 3, False
		End If
	Else
		Eval("Bonus_Light00" & Bonus).state = 1
		Eval("Bonus_Light00" & Bonus & "A").state = 1
	End If
End Sub


Sub Increase_Bonus_held()
	Dim i
	Bonus_held = Bonus_held + 1
	Select Case Bonus_held
		Case 1 
			Bonus_Light_10X.state = 1
		Case 2
			Bonus_Light_20X.state = 1
			Bonus_Light_10X.state = 0
		Case 3
			Bonus_Light_30X.state = 1
			Bonus_Light_20X.state = 0
	End Select
	For i = 1 to 9
		Eval("Bonus_Light00" & i).state = 0
		Eval("Bonus_Light00" & i & "A").state = 0
	Next
End Sub


'############### Give Extra Ball #


Sub Extra_Ball()
	Movie_EBall
	ShowCountdown
End Sub


' ########## Multiball

Sub Check_Multiball()
	Dim i
	If Ball_Locked = 2 Then
		Ball_Locked = 0
		Multiball = True
		RampDown
		For i = 2 to 9
			Eval("Jackpot_Light00" & i).State = 2
		Next
		FlashJackpot(10)
		JackpotLit(Player) = 2
		PlungeBot.Enabled = True
		If Ballsaver < 20 Then Ballsaver = 20
		ShowCountdown
		MultiballCounter = 2
		ScoreX = ScoreX * 2	
		If VenusActive Then
			SoundSaucerHit(1)
			Kicker001.Kick 35, 20
			Kicker002.Kick 270, 6 + Int(Rnd*3)
			BIP = BIP + 1
			Ballsaver = 60
			If ShowFlexDMD Then
				DMDDoubleSplashString "V E N U S", "MULTIBALL", True, 5, True
			End If
			Exit Sub
		End If
		If WizardActive Then
			SoundSaucerHit(1)
			Kicker001.Kick 35, 20
			Kicker002.Kick 270, 6 + Int(Rnd*3)
			BallRelease.CreateBall
			BallRelease.Kick 90, 10
			BIP = BIP + 2
'			Ambient009.state = 1
			If ShowFlexDMD Then
				DMD_SwapFrames "Party", "PartyA", 20, 100
'				DMDDoubleSplashString "W I Z A R D", "MULTIBALL", True, 5, True
			End If
			WizardScore = Score(Player)
			For i = 1 to 3
				Eval("LightBumperRed00" & i).state = 2
				Eval("Spot_Light00" & i).state = 2
			Next
			For i = 1 to 4
				Eval( "Spin_Light" & i).state = 2
			Next
			Flash4Modes(5)
			FlashSteveVenus 3, 5
			Spot_Light004.state = 2
			Spot_Light005.state = 2

			InlaneLeftRed.state = 2
			InlaneRightRed.state = 2
			OutlaneLeftRed.state = 2
			OutlaneRightRed.state = 2
			InlaneLeft.state = 0
			InlaneRight.state = 0
			OutlaneLeft.state = 0
			OutlaneRight.state = 0
			FadeOutSound CurrentMusic, 1
			CurrentMusic = "XL5_Dance"
			PlaySound CurrentMusic
			MusicTimer.Interval = 99000
			Ballsaver = 98
			Ballsave_Light.state = 2
			BallSave.Enabled = True
			Exit Sub

		End If
		LocksLit(Player) = False
		If ShowFlexDMD Then	DMDPlay_MBall
		BlinkFrame "XL5_MBX", "XL5_MBX1", 24, 45, True
		FadeOutSound CurrentMusic, 1
		CurrentMusic = "XL5_UFO"
		PlaySound CurrentMusic
		MusicTimer.Interval = 70000
		MusicTimer.Enabled = True
		Multi_Ball.Interval = 1000
		Multi_Ball.Enabled = True
	End If
End Sub

Sub Multi_Ball_Timer()
	Select Case MultiballCounter
		Case 1
			SoundSaucerHit(1)
			Kicker002.Kick 270, 6 + Int(Rnd*3)
			Lock_Light002.state = 0
			BIP = BIP + 1
			Multi_Ball.Enabled = False
		Case 2
			SoundSaucerHit(1)
			Kicker001.Kick 35, 20
			Lock_Light001.state = 0
			BIP = BIP + 1
	End Select
	MultiballCounter = MultiballCounter - 1
End Sub



'################ Kickers ##########

Sub Kicker001_Hit()
	SoundSaucerHit(2)
	If ChaseOn = 6 Then
		ChaseOn = 7
		Score_50000
		Chaser006.TimerEnabled = False
		Chaser006.Visible = 0
		ChaseCounter = 0
		Chaser007.TimerEnabled = True
		Check_Wizard
	End If
	If WizardActive Then
		SoundSaucerHit(1)
		Score_50000
		Kicker001.Kick 35, 20
		Exit Sub
	End If
	CounterKicker = 4
	Kicker001.TimerInterval = 300
	Kicker001.TimerEnabled = True
End Sub



Sub Kicker001_Timer()
	Dim i
	Kicker001.TimerInterval = 300
	If VenusActive Then
		If Not VenusWon(Player) Then
			FadeOutSound CurrentMusic, 1
			StopSound "XL5_SubCountdown"
			If EBallCollect(Player) > 0 Then
				CollectEBall
				PlaySound "XL5_ExtraBallSnd"
				M_Venus.Interval = 100
			End If
			If Special(Player) Then
				Special_Light.State = 0
				For i = 1 to 7
					Eval("SpecialLetter" & i).State = 0
				Next
				Special(Player) = 0
				LightSequence(2)

				ShowCountdown	
				If PlayfieldCounter > 1 Then 
					PlayfieldCounter = PlayfieldCounter + PlayfieldXTime
					ScoreFlash("PLAYFIELD TIME EXTENDED")
					If ShowFlexDMD Then DMDDoubleSplashString "PLAYFIELD", "TIME EXTENDED", True, 4, True
				Else
					ScoreFlash("PLAYFIELD SCORES X " & PlayfieldX)
					If ShowFlexDMD Then DMDDoubleSplashString "PLAYFIELD", "SCORES X " & PlayfieldX, True, 4, True
					PlayfieldCounter = PlayfieldXTime
					PlayfieldTimer.Interval = 2200
					PlayfieldTimer.Enabled = True
					PlayfieldX10_Light.state = 2
					ShowCountdown
					ScoreX = ScoreX * PlayfieldX
				End If
			End If
			M_VenusCounter = 0
			M_VenusScene = 6
			Kicker001.TimerEnabled = False	
			Exit Sub
		ElseIf EBallCollect(Player) > 0 Then
			CollectEBall
			PlaySound "XL5_ExtraBallSnd"
			Kicker001.TimerInterval = 2000
			CounterKicker = 3
		End If
	ElseIf EBallCollect(Player) > 0 Then
		CollectEBall
		Extra_Ball
		Kicker001.TimerInterval = 4000
		ShowCountdown
		CounterKicker = 3
	End If
	CounterKicker = CounterKicker - 1
	If CounterKicker = 1 Then
		If Special(Player) Then
			LightSequence(2)
			Special_Light.State = 0
			For i = 1 to 7
				Eval("SpecialLetter" & i).State = 0
			Next
			Special(Player) = 0
			If PlayfieldCounter > 1 Then 
				PlayfieldCounter = PlayfieldCounter + PlayfieldXTime
				ScoreFlash("PLAYFIELD TIME EXTENDED")
				If ShowFlexDMD Then DMDDoubleSplashString "PLAYFIELD", "TIME EXTENDED", True, 4, True
			Else
				ScoreFlash("PLAYFIELD SCORES X " & PlayfieldX)
				If ShowFlexDMD Then DMDDoubleSplashString "PLAYFIELD", "SCORES X " & PlayfieldX, True, 4, True
				PlayfieldCounter = PlayfieldXTime
				PlayfieldX10_Light.state = 2
				PlayfieldTimer.Interval = 2200
				PlayfieldTimer.Enabled = True
				PlayfieldX10_Light.state = 2
				ShowCountdown	
				ScoreX = ScoreX * PlayfieldX
			End If
			Kicker001.TimerInterval = 2200
		End If
	End If
	If CounterKicker = 0 Then	
		If LocksLit(Player) And Not Multiball Then
			Lock_Ball
			RampUp
			Lock_Light001.state = 1
		ElseIf Not PowerWon(Player) And Not M_PowerOn And Not InMode And Not Multiball Then
			Movie_MorePower
		ElseIf PowerWon(Player) And Not MeteorWon(Player) And Not InMode And Not Multiball Then
			MeteorWon(Player) = True
			If ShowFlexDMD Then
				DMD_VideoGame(1)
			Else
				ScoreFlash "  VIDEO BONUS " & DMD_Ships(Player) * 100000  * ScoreX
				Score(Player) = Score(Player) + 100000 * DMD_Ships(Player) * ScoreX
				ShowScore
				ModeComplete.Interval = 3000
				ModeComplete.Enabled = True
				Kicker001.TimerEnabled = False		
				Exit Sub
			End If
		Else
			SoundSaucerHit(1)
			Kicker001.Kick 35, 20
		End If
		Kicker001.TimerEnabled = False
	Else
		Score_1000
		Increase_Bonus
	End If
End Sub



Sub Kicker002_Hit()
	SoundSaucerHit(2)
	If ChaseOn = 8 Then
		ChaseOn = 0
		Score_50000
		Chaser008.TimerEnabled = False
		Chaser008.Visible = 0
		ChaseCounter = 0
		Check_Wizard
	End If
	If WizardActive Then
		SoundSaucerHit(1)
		Score_50000
		Kicker002.Kick 270, 6 + Int(Rnd*3)
		Exit Sub
	End If
	CounterKicker2 = 4
	Kicker002.TimerInterval = 300
	Kicker002.TimerEnabled = True
End Sub


Sub Kicker002_Timer()
	Dim i
	Kicker002.TimerInterval = 300
	If VenusActive Then
		If Not VenusWon(Player) Then
			FadeOutSound CurrentMusic, 1
			StopSound "XL5_SubCountdown"
			If EBallCollect(Player) > 0 Then
				CollectEBall
				PlaySound "XL5_ExtraBallSnd"
			End If
			M_VenusCounter = 0
			M_VenusScene = 6
			M_Venus.Interval = 100
			Kicker002.TimerEnabled = False	
			Exit Sub
		ElseIf EBallCollect(Player) > 0 Then
			CollectEBall
			PlaySound "XL5_ExtraBallSnd", 0.99
			Kicker002.TimerInterval = 2000
		End If
	ElseIf EBallCollect(Player) > 0 Then
		CollectEBall
		Extra_Ball
		Kicker002.TimerInterval = 4000
		CounterKicker2 = 2
	End If
	CounterKicker2 = CounterKicker2 - 1
	If CounterKicker2 = 0 Then
		If BonusCollect(Player) Then 
			CollectBonus_Light.State = 0
			CollectBonus_Light2.state = 0
			BonusCollect(Player) = False
			For i = 1 to 3
				Eval("Star_Light00" & i).state = 0
			Next
			AddBonus
		ElseIf LocksLit(Player) And Not Multiball Then
			Lock_Light002.state = 1
			Lock_Ball
		ElseIf Not VenusWon(Player) And Not PowerActive And Not InMode And Not Multiball Then
			Movie_SavingVenus
		ElseIf VenusWon(Player) And Not Warp9Won(Player) And Not InMode And Not PowerActive And Not Multiball Then
			Warp9Won(Player) = True
			If ShowFlexDMD Then
				DMD_VideoGame(2)
			Else
				ScoreFlash "  VIDEO BONUS  " & 100000 * ScoreX
				Score(Player) = Score(Player) + 100000 * ScoreX
				ShowScore
				ModeComplete.Interval = 3000
				ModeComplete.Enabled = True
				Kicker002.TimerEnabled = False		
				Exit Sub
			End If
		Else
			SoundSaucerHit(1)
			Kicker002.Kick 270, 6 + Int(Rnd*3)
		End If
		Kicker002.TimerEnabled = False		
	Else
		Score_1000
		Increase_Bonus
	End If
End Sub


'########## Ramp

Dim RampNum, RampUpFlag


Sub RampUp()
	If RampUpFlag Then Exit Sub
	RampNum = 4
	PlaySound "XL5_Ramp"
	RampMover.Interval = 50
	RampMover.Enabled = True
End Sub

Sub RampDown()
	If Not RampUpFlag Then Exit Sub
	RampNum = 12
	PlaySound "XL5_Ramp"
	RampMover2.Interval = 50
	RampMover2.Enabled = True
End Sub

Sub RampMover_Timer
	Eval("Ramp0" & RampNum).Visible = 1
	Eval("Ramp0" & RampNum - 1).Visible = 0
	RampNum = RampNum + 1
	If RampNum = 14 Then
		RampUpFlag = True
		Ramp03.Collidable = False
		Ramp013.Collidable = True
		RampMover.Enabled = False
	End If

End Sub

Sub RampMover2_Timer
	Eval("Ramp0" & RampNum).Visible = 1
	Eval("Ramp0" & RampNum + 1).Visible = 0
	RampNum = RampNum - 1
	If RampNum = 2 Then
		RampUpFlag = False
		Ramp013.Collidable = False
		Ramp03.Collidable = True
		RampMover2.Enabled = False
	End If
End Sub







Sub ModeComplete_Timer
	If Modes(Player) = 4 Then
		BallRelease.CreateBall
		BallRelease.Kick 90, 10
		Ball_Locked = 2
		Ballsave_Light.state = 0
		BallSave.Enabled = False
		Ballsaver = 99
		SoundBallRelease
		If PlayfieldCounter > 1 Then PlayfieldTimer.Enabled = True
		MusicTimer.Enabled = True

		ScoreFlash "   WIZARD MODE "
		WizardActive = True
		Modes(Player) = 0
		KickBack.Enabled = True
		KickBack2.Enabled = True
		ModeComplete.Enabled = False
		Exit Sub
	Else
		ScoreFlash "  MODE COMPLETE"
		Modes(Player) = Modes(Player) + 1
		If Modes(Player) = 4 Then
			ModeComplete.Interval = 3000
		Else 
			SoundSaucerHit(1)
			Kicker002.Kick 270, 6 + Int(Rnd*3)
			Kicker001.Kick 35, 20
			ModeComplete.Enabled = False
		End If
	End If
End Sub

Sub CollectEBall()
	EBallCollect(Player) = EBallCollect(Player) - 1
	EBall(Player) = EBall(Player) + 1
	ShowCountdown
	FlashEBall(2)
	If ShowFlexDMD And Not VenusActive Then DMDPlay_EBall	
	EB_Light.state = 1
	If EBallCollect(Player) = 0 Then
		EB_Light001.State = 0
		EB_Light002.State = 0
	End If
End Sub

'### Playfield X 

Sub PlayfieldTimer_Timer()
	PlayfieldTimer.Interval = 1000
	PlayfieldCounter = PlayfieldCounter - 1
	ShowCountdown
	If PlayfieldCounter = 0 Then
		If Multiball Then
			ScoreX = 2
		Else
			ScoreX = 1
		End If
		PlayfieldTimer.Enabled = False
		PlayfieldX10_Light.state = 0
	End If
End Sub

'####### Ball Lock 

Sub Lock_Ball()
	Ball_Locked = Ball_Locked + 1
	If Ball_Locked = 2 Then
'		StopSound CurrentMusic
		Movie_RobTalk "Rob_Ball2", 28
		If ShowFlexDMD Then DMDDoubleSplashString "BALL 2", "IS LOCKED", True, 4, True
	Else
		Movie_RobTalk "Rob_Ball1", 28
		If ShowFlexDMD Then DMDDoubleSplashString "BALL 1", "IS LOCKED", True, 4, True
	End If
	If Ballsaver < BallSaveTime Then
		BallSave.Enabled = False
		Ballsave_Light.state = 0
		Ballsaver = 99
	End If
	ShowCountdown
	ShowScore
	BallRelease.CreateBall
	BallRelease.Kick 90, 7
	SoundBallRelease
End Sub



'##################### Fast Bonus Collect #########

Sub AddBonus()
	If Bonus = 0 And Bonus_held = 0 Then
		ScoreFlash(" NO BONUS") 
		AddBonusCounter = 2
		AddBonusTimer.Interval = 2000
		AddBonusTimer.Enabled = True
		Exit Sub
	End If

	BonX = BonusX(Player)
	If BonX = 4 Then
		BonX = 5
	End If
	BonusTotal = Bonus * 10000 + Bonus_held * 100000

	If PowerActive Or ShieldActive Or Multiball Then
		Score(Player) = Score(Player) + BonusTotal * BonX * ScoreX
		ShowScore
		AddBonusCounter = 2
		AddBonusTimer.Interval = 100
		AddBonusTimer.Enabled = True
		If ShowFlexDMD Then DMDDoubleSplashString "BONUS", "COLLECTED", True, 2, False
		SoundKnocker
		Exit Sub
	End If

	If PlayfieldCounter > 1 Then
		PlayfieldTimer.Enabled = False
	End If

	If BonusTotal > 999 Then
		BonusString = Left(BonusTotal, Len(BonusTotal) - 3) & "," & Right(BonusTotal, 3)
		ScoreFlash(" BONUS " & BonusString)
		If ShowFlexDMD Then	'And Not DMDShowSplash Then
			DMDShowSplash = True
			DMD_StartSplashBG
			DMD_Splash.Font = FontCool
			DMD_Splash.Text = BonusString
			DMD_Splash_S.Text = BonusString
			DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
			DMD_Splash_S.SetAlignedPosition 64, 19, FlexDMD_Align_Center
			DMD_Splash.Visible = 1
			DMD_Splash_S.Visible = 1
		End If
	End If

	BonX = BonX * ScoreX
	BonusTotal = BonusTotal * BonX
	If BonusTotal > 999 Then
		BonusString = Left(BonusTotal, Len(BonusTotal) - 3) & "," & Right(BonusTotal, 3)
	End If
	If BonusTotal > 999999 Then
		BonusString = Left(BonusTotal, Len(BonusTotal) - 6) & "," & Right(BonusString, 7)
	End If
	DMDShowSplash = True
	ShowScore
	If BonusX(Player) > 1 Then
		AddBonusCounter = 5
	Else
		AddBonusCounter = 3
	End If
	AddBonusTimer.Interval = 2000
	AddBonusTimer.Enabled = True
End Sub

Sub AddBonusTimer_Timer()
	AddBonusCounter = AddBonusCounter - 1
	Select Case AddBonusCounter
		Case 1
			If ShowFlexDMD Then 
				DMD_StopSplashBG
				DMD_Splash.Text = ""
				DMD_Splash_S.Text = ""
				DMDShowSplash = False
				ShowScore
			End If
			ScoreText001.Text = ""
			If LocksLit(Player) And Not Multiball Then
				Lock_Ball
				Lock_Light002.state = 1
			Else
				SoundSaucerHit(1)
				Kicker002.Kick 270, 6 + Int(Rnd*3)
			End If
			If PlayfieldCounter > 1 Then
				PlayfieldTimer.Enabled = True
			End If
			AddBonusTimer.Enabled = False
		Case 2
			Dim bonusnum
			If BonusTotal > 5000000 Then
				bonusnum = 500000
			ElseIf BonusTotal > 1000000 Then
				bonusnum = 200000
			ElseIf BonusTotal > 200000 Then
				bonusnum = 50000
			ElseIf BonusTotal > 50000 Then
				bonusnum = 20000
			Else 
				bonusnum = 10000
			End If
			BonusTotal = BonusTotal - bonusnum
			If BonusTotal > 0 Then
				BonusString = Left(BonusTotal, Len(BonusTotal) - 3) & "," & Right(BonusTotal, 3)
			Else
				BonusString = "0"
			End If
			If BonusTotal > 999999 Then
				BonusString = Left(BonusTotal, Len(BonusTotal) - 6) & "," & Right(BonusString, 7)
			End If
			ScoreText001.Text = BonusString
			If ShowFlexDMD Then 
				DMD_Splash.Text = BonusString
				DMD_Splash_S.Text = BonusString
				DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
				DMD_Splash_S.SetAlignedPosition 64, 19, FlexDMD_Align_Center
				DMD_Splash.Visible = 1
				DMD_Splash_S.Visible = 1
			End If
			DMDShowSplash = True
			Score(Player) = Score(Player) + bonusnum
			ShowScore
			PlaySound "Bell10"
			If BonusTotal > 0 Then
				AddBonusCounter = AddBonusCounter + 1
				AddBonusTimer.Interval = 40
			Else
				AddBonusTimer.Interval = 500
			End If
		Case 3
			ScoreFlash(BonusString)
			If ShowFlexDMD Then
				DMD_Splash.Text = BonusString
				DMD_Splash_S.Text = BonusString
				DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
				DMD_Splash_S.SetAlignedPosition 64, 19, FlexDMD_Align_Center
				DMD_Splash.Visible = 1
				DMD_Splash_S.Visible = 1
			End If

		Case 4
			ScrollDMDText("X " & BonX)
			If ShowFlexDMD Then
				DMD_Splash.Text = "X " & BonX
				DMD_Splash_S.Text = "X " & BonX
				DMD_Splash.SetAlignedPosition 63, 18, FlexDMD_Align_Center
				DMD_Splash_S.SetAlignedPosition 64, 19, FlexDMD_Align_Center
				DMD_Splash.Visible = 1
				DMD_Splash_S.Visible = 1
			End If

	End Select
End Sub




'############### Left Orbit Star Rollovers #############

Sub Trigger007_Hit()
	SoundRolloverHit
	StarCount = 2
	If ShieldActive Then
		Increase_Bonus
		Score_10000
	ElseIf BonusCollect(Player) Then
		Increase_Bonus
		Score_1000
	Else
		Score_100
	End If
End Sub

Sub Trigger001_Hit()
	SoundRolloverHit
	StarCount = StarCount + 1
	If ShieldActive Then
		Increase_Bonus
		Score_10000
	ElseIf BonusCollect(Player) Then
		Increase_Bonus
		Score_1000
	Else
		Score_100
	End If
End Sub

Sub Trigger005_Hit()
	SoundRolloverHit
	If ShieldActive Then
		Increase_Bonus
		If StarCount = 3 Then 
			StarCount = 0
			Score_50000
		Else
			Score_10000
		End If
	ElseIf BonusCollect(Player) Then
		Increase_Bonus
		If StarCount = 3 Then 
			StarCount = 0
			Score_10000
		Else
			Score_1000
		End If
	Else
		Score_100
	End If
End Sub




'#### Prioritise countdown display

Sub ShowCountdown()
	If BallSaveString > "" Then
		ScoreText003.Text = " Save: " & BallSaveString
		If ShowFlexDMD Then FlexDMD.Stage.GetLabel("Credit").Text = "Ballsave: " & BallSaveString
	ElseIf PlayfieldCounter > 0 Then
		ScoreText003.Text = PlayfieldX & "x:  " & PlayfieldCounter
		If ShowFlexDMD Then FlexDMD.Stage.GetLabel("Credit").Text = "Playfield X" & PlayfieldX & ": " & PlayfieldCounter
	ElseIf EBall(Player) > 0 Then	
		ScoreText003.Text = "Xtra Balls " & EBall(Player)
		If ShowFlexDMD Then FlexDMD.Stage.GetLabel("Credit").Text = "Extra Balls: " & EBall(Player)
	Else
		ScoreText003.Text = ""
		If ShowFlexDMD Then DMDShowBallCred
	End If
	If ShowFlexDMD Then 
		FlexDMD.Stage.GetLabel("Credit").SetAlignedPosition 127, 32, FlexDMD_Align_BottomRight
		FlexDMD.Stage.GetLabel("Credit").Font = FontScoreActiveSmall
	End If
End Sub

'################## X - L - 5 Rollovers #######

Sub Trigger002_Hit()
	If SkillShotTarget = 1 Then
		PlaySound "XL5_Skillshot2"
		If ShowFlexDMD Then DMDPlay_Skillshot()
		SkillDelay.Interval = 1000
		SkillDelay.Enabled = True
	End If
	If Lite1(Player) = 0 Then	
		XL5_Light1.state = 1
		Fireball009.State = 1
		LightBumper001.state = 1
		LightBumperLow001.state = 1
		Bumper1(Player) = 1
		Increase_Bonus
		Lite1(Player) = 1
		Check_ABC
	ElseIf WizardActive Then 
		Score_50000
	Else
		Score_100
	End If
	SoundRolloverHit
End Sub

Sub Trigger003_Hit()
	If SkillShotTarget = 2 Then
		PlaySound "XL5_Skillshot2"
		If ShowFlexDMD Then DMDPlay_Skillshot()
		SkillDelay.Interval = 1000
		SkillDelay.Enabled = True
	End If
	If Lite2(Player) = 0 Then
		XL5_Light2.state = 1
		Fireball0010.State = 1
		LightBumper003.state = 1
		LightBumperLow003.state = 1
		Bumper3(Player) = 1
		Increase_Bonus
		Lite2(Player) = 1
		Check_ABC
	ElseIf WizardActive Then 
		Score_50000
	Else
		Score_100
	End If
	SoundRolloverHit
End Sub

Sub Trigger004_Hit()
	If SkillShotTarget = 3 Then
		PlaySound "XL5_Skillshot2"
		If ShowFlexDMD Then DMDPlay_Skillshot()
		SkillDelay.Interval = 1000
		SkillDelay.Enabled = True
	End If
	If Lite3(Player) = 0 Then
		XL5_Light3.state = 1
		Fireball0011.State = 1
		LightBumper002.state = 1
		LightBumperLow002.state = 1
		Bumper2(Player) = 1
		Increase_Bonus
		Lite3(Player) = 1
		Check_ABC
	ElseIf WizardActive Then 
		Score_50000
	Else
		Score_100
	End If
	SoundRolloverHit
End Sub

Sub SkillDelay_Timer()
	Dim SkillString
	Dim SkillTotal
	SkillDelay.Enabled = False

	SkillTotal = SkillShotScore(Player) * ScoreX
	Score(Player) = Score(Player) + SkillTotal
	ShowScore
	SkillShotScore(Player) = SkillShotScore(Player) * 2

	SkillString = Left(SkillTotal, Len(SkillTotal) - 3) & "," & Right(SkillTotal, 3)
	If SkillTotal > 999999 Then
		If Mid(SkillTotal, Len(SkillTotal) - 5, 1) <> "0" Then
			SkillString = Left(SkillTotal, Len(SkillTotal) - 6) & "." & Mid(SkillTotal, Len(SkillTotal) - 5, 1) & " MILLION"
		Else
			SkillString = Left(SkillTotal, Len(SkillTotal) - 6) & " MILLION"
		End If
	End If
	ScoreFlash "   SKILL SHOT    " & SkillString
	If ShowFlexDMD Then DMDDoubleSplashString "SKILL SHOT", SkillString, True, 2, True

End Sub

Sub Check_ABC()
	Dim ABCTotal, ABCString

	If Lite1(Player) + Lite2(Player) + Lite3(Player) = 3 Then
		Lite1(Player) = 0
		Lite2(Player) = 0
		Lite3(Player) = 0
		BonusX(Player) = BonusX(Player) + 1
		If BonusX(Player) > 4 then
			BonusX(Player) = 4
			ABCTotal = ABCBonus(Player) * ScoreX
			ABCString = Left(ABCTotal, Len(ABCTotal) - 3) & "," & Right(ABCTotal, 3)
			If ABCTotal > 999999 Then
				If Mid(ABCTotal, Len(ABCTotal) - 5, 1) <> "0" Then
					ABCString = Left(ABCTotal, Len(ABCTotal) - 6) & "." & Mid(ABCTotal, Len(ABCTotal) - 5, 1) & " MILLION"
				Else
					ABCString = Left(ABCTotal, Len(ABCTotal) - 6) & " MILLION"
				End If
			End If
			If ABCTotal > 999999999 Then
				ABCString = Left(ABCTotal, Len(ABCTotal) - 9) & " BILLION"
				SoundKnocker
			End If
			ScoreFlash "    XL5 BONUS   " & ABCString
			Score(Player) = Score(Player) + ABCTotal
			If ABCTotal < 1000000 Then
				DMD_Scorestring = "XL5 BONUS " & ABCString
			Else 
				DMD_Scorestring = "  XL5: " & ABCString
			End If

			If ShowFlexDMD Then
'				DMD_PlayJackpot(1)
				If ABCTotal > 999999999 Then
					If Left(ABCTotal, 1) = "1" Then
						DMD_SwapFrames "1BIL", "1BILA", 20, 100
					Else
						DMD_SwapFrames "2BIL", "2BILA", 20, 100
					End If
				Else
					DMDDoubleSplashString "XL5 JACKPOT", ABCString, True, 6, False
				End If
			End If
			ShowScore
			ABCBonus(Player) = ABCBonus(Player) * 2
			If ABCBonus(Player) > 50000000 Then
				ABCBonus(Player) = 50000000
			End If
			Movie_RobTalk "Rob_Jackpot", 18
		Else
			Select Case BonusX(Player)
				Case 2 
					Light_2X.state = 1
				Case 3
					Light_3X.state = 1
					Light_2X.state = 0
				Case 4
					If Light_5X.state = 0 Then
						Light_5X.state = 1
						Light_3X.state = 0
					End If
			End Select
			Movie_RobTalk "Rob_Multiplier", 28
			DMD_Bonus = BonusX(Player)

			If DMD_Bonus = 4 Then DMD_Bonus = 5
			DMD_Scorestring = "     BONUS X " & DMD_Bonus
			If ShowFlexDMD Then DMD_ShowBonus
		End If
		XL5_Light1.state = 2
		XL5_Light2.state = 2
		XL5_Light3.state = 2
		ABC_Flash.Interval = 2000
		ABC_Flash.Enabled = True
		CycleFireball
 	End If
	Score_5000
End Sub

Sub ABC_Flash_Timer()
	XL5_Light1.state = Lite1(Player)
	XL5_Light2.state = Lite2(Player)
	XL5_Light3.state = Lite3(Player)
	ABC_Flash.Enabled = False
End Sub



Dim PlayJackpotCounter, PlayJackpotCounter2, PlayJackpotSpeed, JPot



Sub DMD_PlayJackpot(num)
	If DMDShowVideo Then Exit Sub
	DMDShowVideo = True
	PlayJackpotCounter = 0
	PlayJackpotCounter2 = 0
	JPot = 1
	If num = 2 Then JPot = 5
	PlayJackpot.Interval = 50
	PlayJackpot.Enabled = True
	PlayJackpot_Timer
End Sub


Sub PlayJackpot_Timer
	PlayJackpotCounter = PlayJackpotCounter + 1
	If PlayJackpotCounter > 4 Then PlayJackpotCounter = 1
	FlexDMD.Stage.GetImage("Tunnel_" & PlayJackpotCounter).Visible = 1	
	If PlayJackpotCounter > 1 Then 
		FlexDMD.Stage.GetImage("Tunnel_" & PlayJackpotCounter - 1).Visible = 0
	Else 
		FlexDMD.Stage.GetImage("Tunnel_4").Visible = 0
	End If

	Select Case JPot
		Case 1
			PlayJackpotCounter2 = PlayJackpotCounter2 + 2
			FlexDMD.Stage.GetImage("Jackpot3").SetSize PlayJackpotCounter2*4, PlayJackpotCounter2
			FlexDMD.Stage.GetImage("Jackpot3").SetPosition 64 - PlayJackpotCounter2*2, 16 - PlayJackpotCounter2/2
			FlexDMD.Stage.GetImage("Jackpot3").Visible = 1
			If PlayJackpotCounter2 = 32 Then
				JPot = 2
				DMD_SwapFrames "Jackpot1", "Jackpot3", 31, 50
			End If

		Case 2
			PlayJackpotCounter2 = PlayJackpotCounter2 - 1
			If PlayJackpotCounter2 = 0 Then JPot = 3

		Case 3
			FlexDMD.Stage.GetImage("Tunnel_" & PlayJackpotCounter).Visible = 0	
			FlexDMD.Stage.GetImage("Jackpot3").Visible = 0	
			DMDShowVideo = False
			PlayJackpot.Enabled = False

		Case 5
			PlayJackpotCounter2 = PlayJackpotCounter2 + 2
			FlexDMD.Stage.GetImage("Jackpot4").SetSize PlayJackpotCounter2*4, PlayJackpotCounter2
			FlexDMD.Stage.GetImage("Jackpot4").SetPosition 64 - PlayJackpotCounter2*2, 16 - PlayJackpotCounter2/2
			FlexDMD.Stage.GetImage("Jackpot4").Visible = 1
			If PlayJackpotCounter2 = 32 Then 
				JPot = 6
				DMD_SwapFrames "Jackpot2", "Jackpot4", 31, 50
			End If

		Case 6
			PlayJackpotCounter2 = PlayJackpotCounter2 - 1
			If PlayJackpotCounter2 = 0 Then JPot = 7

		Case 7
			PlayJackpotCounter2 = 0
			FlexDMD.Stage.GetImage("Jackpot4").Visible = 0	
			JPot = 1

	End Select

End Sub

' ###### Bottom Lane rollovers 


Sub SwapLanes()
	If WizardActive Then Exit Sub
	If InlaneLeft.state = 1 then
		InlaneLeft.state = 0
		InlaneRight.state = 1
		OutlaneLeft.state = 1
		OutlaneRight.state = 0
	Else
		InlaneLeft.state = 1
		InlaneRight.state = 0
		OutlaneLeft.state = 0
		OutlaneRight.state = 1
	End If
End Sub



Sub LeftInlane_Hit()
	If ChaseOn = 1 Then
		ChaseOn = 2
		Score_10000
		Chaser001.TimerEnabled = False
		Chaser001.Visible = 0
		ChaseCounter = 0
		Chaser002.TimerEnabled = True
		Check_Wizard
	ElseIf WizardActive Then 
		Score_50000
	ElseIf InlaneLeft.state = 1 then
		Score_5000
		Increase_Bonus
	Else
		Score_500
	End If
	SoundRolloverHit
End Sub

Sub RightInlane_Hit()
	If ChaseOn = 7 Then
		ChaseOn = 8
		Score_50000
		Chaser007.TimerEnabled = False
		Chaser007.Visible = 0
		ChaseCounter = 0
		Chaser008.TimerEnabled = True
		Check_Wizard
	ElseIf WizardActive Then
		Score_50000
	ElseIf InlaneRight.state = 1 then
		Score_5000
		Increase_Bonus
	Else
		Score_500
	End If
	SoundRolloverHit
End Sub

Sub LeftOutlane_Hit()
	If WizardActive Then Score_50000: Exit Sub
	If OutlaneLeft.state = 1 then
		Score_5000
		Increase_Bonus
	Else
		Score_500
	End If
	SoundRolloverHit
End Sub

Sub RightOutlane_Hit()
	If WizardActive Then Score_50000: Exit Sub
	If OutlaneRight.state = 1 then
		Score_5000
		Increase_Bonus
	Else
		Score_500
	End If
	SoundRolloverHit
End Sub





' ####### Left Target Bank

Sub Target003_Hit()
	SoundDropTargetDown
	Targets_down = Targets_down + 1
	If Light004.state = 0 then
		Light004.state = 1
		Score_1000
	ElseIf Light003.state = 0 then
		Light003.state = 1
		Score_5000
	ElseIf Light013.state = 0 then
		Light013.state = 1
		Score_5000
	Else
		Score_50000
		SoundKnocker
	End If
	Target003.TimerInterval = 250
	Target003.TimerEnabled = True
End Sub

Sub Target004_Hit()
	SoundDropTargetDown
	Targets_down = Targets_down + 1
	If Light005.state = 0 then
		Light005.state = 1
		Score_1000
	ElseIf Light010.state = 0 then
		Light010.state = 1
		Score_5000
	ElseIf Light014.state = 0 then
		Light014.state = 1
		Score_50000
	Else
		Score_5000
		SoundKnocker
	End If
	Target004.TimerInterval = 250
	Target004.TimerEnabled = True
End Sub

Sub Target005_Hit()
	SoundDropTargetDown
	Targets_down = Targets_down + 1
	If Light006.state = 0 then
		Light006.state = 1
		Score_1000
	ElseIf Light011.state = 0 then
		Light011.state = 1
		Score_5000
	ElseIf Light015.state = 0 then
		Light015.state = 1
		Score_50000
	Else
		Score_5000
		SoundKnocker
	End If
	Target005.TimerInterval = 250
	Target005.TimerEnabled = True
End Sub

Sub Target006_Hit()
	SoundDropTargetDown
	Targets_down = Targets_down + 1
	If Light007.state = 0 then
		Light007.state = 1
		Score_1000
	ElseIf Light012.state = 0 then
		Light012.state = 1
		Score_5000
	ElseIf Light016.state = 0 then
		Light016.state = 1
		Score_50000
	Else
		Score_5000
		SoundKnocker
	End If
	Target006.TimerInterval = 250
	Target006.TimerEnabled = True
End Sub


Sub Target003_Timer()
	CheckTarget
	Target003.TimerEnabled = False
End Sub

Sub Target004_Timer()
	CheckTarget
	Target004.TimerEnabled = False
End Sub

Sub Target005_Timer()
	CheckTarget
	Target005.TimerEnabled = False
End Sub

Sub Target006_Timer()
	CheckTarget
	Target006.TimerEnabled = False
End Sub

Sub CheckTarget()
'	If Targets_down = 4 then
	Dim i
	For i = 3 to 6
		If Eval("Target00" & i).isDropped = 0 Then Exit Sub
	Next
	Target_Reset
	Banks_down = Banks_down +1
	CheckBank

End Sub

Sub CheckBank()
	Dim i
	Select Case Banks_down
		Case 1
			CollectBonus_Light.State = 2
			CollectBonus_Light2.State = 2
			BonusCollect(Player) = True
			For i = 1 to 3
				Eval("Star_Light00" & i).State = 2
			Next
			ScoreFlash("COLLECT BONUS IS LIT")
			If ShowFlexDMD Then DMDDoubleSplashString "COLLECT BONUS", "IS LIT", True, 5, False
		Case 2
			EBallCollect(Player) = EBallCollect(Player) + 1
			EB_Light001.State = 2
			EB_Light002.State = 2
			FlashEBall(9)
			ScoreFlash " EXTRA BALL IS LIT"
			If ShowFlexDMD Then DMDDoubleSplashString "EXTRA BALL", "IS LIT", True, 5, False
		Case 3
			ScoreFlash(" SPECIAL    IS LIT")
			If ShowFlexDMD Then DMDDoubleSplashString "SPECIAL", "IS LIT", True, 5, False
			Special_Light.state = 2	
			For i = 1 to 7
				Eval("SpecialLetter" & i).State = 2
			Next
			Special(Player) = 2
			SoundKnocker
			LightSequence(2)
			For i = 3 to 7
				Eval("Light00" & i).state = 0
			Next
			For i = 10 to 16
				Eval("Light0" & i).state = 0
			Next
			Banks_down = 0
'			Check_Wizard
	End Select
	Score_5000
	CycleFireball
End Sub




Sub Target_Reset()
	Dim i
	For i = 3 to 6
		Eval("Target00" & i).isDropped = 0
	Next
	SoundDropTargetReset
	SoundDropTargetRelease
	Targets_down = 0
End Sub



Sub Target_BugCheck()
	Dim i
	For i = 3 to 6
		If Eval("Target00" & i).isDropped = 0 Then Exit Sub
	Next
	Target_Reset()
	For i = 3 to 7
		Eval("Light00" & i).state = 0
	Next
	For i = 10 to 16
		Eval("Light0" & i).state = 0
	Next
	Select Case Banks_down
		Case 0
			Exit Sub
		Case 1
			For i = 4 to 7
				Eval("Light00" & i).state = 1
			Next
		Case 2
			For i = 3 to 7
				Eval("Light00" & i).state = 1
			Next
			For i = 10 to 12
				Eval("Light0" & i).state = 1
			Next
	End Select
End Sub


'############# Red Spot Targets

Sub Target007_Hit()
	If Spot1(Player) = 0 Then
		Spot_Light001.state = 1
		Spot1(Player) = 1
		Increase_Bonus
		Check_Spots
	Else
		Score_100
	End If
End Sub

Sub Target008_Hit()
	If Spot2(Player) = 0 Then
		Spot_Light002.state = 1
		Spot2(Player) = 1
		Increase_Bonus
		Check_Spots
	Else
		Score_100
	End If
End Sub

Sub Target009_Hit()
	If ChaseOn = 4 Then
		ChaseOn = 5
		Score_50000
		Chaser004.TimerEnabled = False
		Chaser004.Visible = 0
		ChaseCounter = 0
		Chaser005.TimerEnabled = True
		Check_Wizard
		If ShowFlexDMD Then	DMD_SwapFrames "500K", "500KA", 20, 50
		ScoreFlash "  " & 500000 * ScoreX
		Score(Player) = Score(Player) + 500000 * ScoreX
	End If
	If Spot3(Player) = 0 Then
		Spot_Light003.state = 1
		Spot3(Player) = 1
		Increase_Bonus
		Check_Spots
	Else
		Score_100
	End If
End Sub

Sub Target010_Hit()
	If ChaseOn = 3 Then
		ChaseOn = 4
		Score_50000
		Chaser003.TimerEnabled = False
		Chaser003.Visible = 0
		ChaseCounter = 0
		Chaser004.TimerEnabled = True
		Check_Wizard
		If ShowFlexDMD Then	DMD_SwapFrames "500K", "500KA", 20, 50
		ScoreFlash "  " & 500000 * ScoreX
		Score(Player) = Score(Player) + 500000 * ScoreX
	End If
	If Spot4(Player) = 0 Then
		Spot_Light004.state = 1
		Spot4(Player) = 1
		Increase_Bonus
		Check_Spots
	Else
		Score_100
	End If
End Sub

Sub Target011_Hit()
	If ChaseOn = 5 Then
		ChaseOn = 6
		Score_50000
		Chaser005.TimerEnabled = False
		Chaser005.Visible = 0
		ChaseCounter = 0
		Chaser006.TimerEnabled = True
		Check_Wizard
		If ShowFlexDMD Then	DMD_SwapFrames "1MIL", "1MILA", 30, 50
		ScoreFlash "  " & 1000000 * ScoreX
		Score(Player) = Score(Player) + 1000000 * ScoreX
	End If
	If Spot5(Player) = 0 Then
		Spot_Light005.state = 1
		Spot5(Player) = 1
		Increase_Bonus
		Check_Spots
	Else
		Score_100
	End If
End Sub

Sub Check_Spots()
	Dim SpotTotal, SpotString, i
	Spots_hit = Spot1(Player) + Spot2(Player) + Spot3(Player) + Spot4(Player) + Spot5(Player)
	If Spots_hit = 5 Then
		For i = 1 to 5
			Eval("Spot_Light00" & i).state = 2
		Next
		Spot1(Player) = 0
		Spot2(Player) = 0
		Spot3(Player) = 0
		Spot4(Player) = 0
		Spot5(Player) = 0
		SpotLights.Interval = 2000
		SpotLights.Enabled = True
		SpotCount(Player) = SpotCount(Player) + 1 
		Select Case SpotCount(Player)
			Case 1
				If Not LocksLit(Player) Then
					ScoreFlash("LOCKS ARE LIT")
					If ShowFlexDMD Then DMD_SwapFrames "LocksLit", "LocksLitA", 20, 100
					LocksLit(Player) = True
					Lock_Light001.state = 2
					Lock_Light002.state = 2
					Movie_RobTalk "Rob_Lockballs", 32
				Else
					DMD_RandomAward.Interval = 2000
					DMD_RandomAward.Enabled = True
					ScoreFlash "   LUCKY DIP"
					If ShowFlexDMD Then DMD_SwapFrames "RandomAward", "RandomAwardA", 20, 100
				End If
			Case 2
				ShieldActive = True
				For i = 1 to 3
					Eval("LightBumperRed00" & i).state = 2
				Next

				If Not Multiball And Not VenusActive And Not PowerActive Then
					FadeOutSound CurrentMusic, 1
					CurrentMusic = "XL5_ZeroG"
					PlaySound CurrentMusic
					MusicTimer.Interval = 133000
				End If

				If ShowFlexDMD And Not DMDShowSplash Then DMD_SwapFrames "SuperPops", "SuperPopsA", 30, 100
				Ballsaver = 58
				Ballsave_Light.state = 2
				BallSave.Enabled = True
			Case 3
				Score_50000
				DMD_RandomAward.Interval = 2000
				DMD_RandomAward.Enabled = True
				ScoreFlash "   LUCKY DIP"
				If ShowFlexDMD Then DMD_SwapFrames "RandomAward", "RandomAwardA", 20, 100

			Case 4
				Score_50000
				SpotCount(Player) = 0
				DMD_RandomAward.Interval = 2000
				DMD_RandomAward.Enabled = True
				ScoreFlash "   LUCKY DIP"
				If ShowFlexDMD Then DMD_SwapFrames "RandomAward", "RandomAwardA", 20, 100

		End Select
		Score_5000

		CycleFireball
	ElseIf Spots_hit = 4 Then
		For i = 1 to 5
			If Eval("Spot_Light00" & i).state = 0 Then
				Eval("Spot_Light00" & i).state = 2
			End If
		Next
		Score_1000
	Else
		Score_1000
	End If
End Sub


Sub SpotLights_Timer()
	Spot_Light001.State = Spot1(Player)
	Spot_Light002.State = Spot2(Player)
	Spot_Light003.State = Spot3(Player)
	Spot_Light004.State = Spot4(Player)
	Spot_Light005.State = Spot5(Player)
	SpotLights.Enabled = False
End Sub


Sub DMD_RandomAward_Timer()
	Dim AwardCase
	AwardCase = Int(Rnd*10 + 1)
	Select Case AwardCase
		Case 1,3,5,7
			If ShowFlexDMD And DMD_Ships(Player) < 3 Then
				If Not DMDShowSplash Then DMDDoubleSplashString "Extra Ship", "For METEOR", True, 2, False
				DMD_Ships(Player) = 3
			Else
				ScoreFlash ScoreX &" MILLION"
				Score(Player) = Score(Player) + 1000000 * ScoreX
				If ShowFlexDMD And Not DMDShowSplash Then DMDDoubleSplashString "RANDOM", ScoreX &" MILLION", True, 2, False
			End If


		Case 2,4,8
			ScoreFlash ScoreX &" MILLION"
			Score(Player) = Score(Player) + 1000000 * ScoreX
			If ShowFlexDMD And Not DMDShowSplash Then DMDDoubleSplashString "RANDOM", ScoreX &" MILLION", True, 2, False

		Case Else
			EBallCollect(Player) = EBallCollect(Player) + 1
			EB_Light001.State = 2
			EB_Light002.State = 2
			FlashEBall(7)
			ScoreFlash " EXTRA BALL   IS LIT"
			If ShowFlexDMD And Not DMDShowSplash Then DMDDoubleSplashString "EXTRA BALL", "IS LIT", True, 2, False

	End Select
	DMD_RandomAward.Enabled = False
End Sub



'######## Zoom Targets

Dim TopTargets_down


Sub TopTarget001_Hit()
	SoundDropTargetDown
	TopTarget_Light001.State = 1
	If TopTarget_Light002.State = 1 And TopTarget_Light003.State = 1 And TopTarget_Light004.State = 1 Then TopTargets_down = 4
	TopTarget001.TimerInterval = 250
	TopTarget001.TimerEnabled = True
	Score_500
End Sub

Sub TopTarget002_Hit()
	SoundDropTargetDown
	TopTarget_Light002.State = 1
	If TopTarget_Light001.State = 1 And TopTarget_Light003.State = 1 And TopTarget_Light004.State = 1 Then TopTargets_down = 4
	TopTarget002.TimerInterval = 250
	TopTarget002.TimerEnabled = True
	Score_500
End Sub

Sub TopTarget003_Hit()
	SoundDropTargetDown
	TopTarget_Light003.State = 1
	If TopTarget_Light001.State = 1 And TopTarget_Light002.State = 1 And TopTarget_Light004.State = 1 Then TopTargets_down = 4
	TopTarget003.TimerInterval = 250
	TopTarget003.TimerEnabled = True
	Score_500
End Sub

Sub TopTarget004_Hit()
	SoundDropTargetDown
	TopTarget_Light004.State = 1
	If TopTarget_Light001.State = 1 And TopTarget_Light002.State = 1 And TopTarget_Light003.State = 1 Then TopTargets_down = 4
	TopTarget004.TimerInterval = 250
	TopTarget004.TimerEnabled = True
	Score_500
End Sub




Sub TopTarget001_Timer
	If TopTargets_down = 4 Then TopTarget_Check
	TopTarget001.TimerEnabled = False
End Sub

Sub TopTarget002_Timer
	If TopTargets_down = 4 Then TopTarget_Check
	TopTarget002.TimerEnabled = False
End Sub

Sub TopTarget003_Timer
	If TopTargets_down = 4 Then TopTarget_Check
	TopTarget003.TimerEnabled = False
End Sub

Sub TopTarget004_Timer
	If TopTargets_down = 4 Then TopTarget_Check
	TopTarget004.TimerEnabled = False
End Sub


Sub TopTarget_Check()
	Score_50000
	If KickBack_Light.State = 1 Then
		If KickBack2_Light.State = 1 Then
			Increase_Bonus
		Else
			KickBack2.Enabled = True
			KickBack2_Light.State = 1
			Kick2.Enabled = False
		End If
	Else
		KickBack.Enabled = True
		KickBack_Light.State = 1
		Kick.Enabled = False
	End If
	FlashBooster(10)
	TopTarget_Reset

End Sub

Sub TopTarget_Reset()
	Dim i
	For i = 1 to 4
		Eval("TopTarget00" & i).isDropped = 0
		Eval("TopTarget_Light00" & i).State = 0
	Next
	TopTargets_down = 0
	SoundDropTargetReset
	SoundDropTargetRelease
'	TopTargetBugCheck.Interval = 200
'	TopTargetBugCheck.Enabled = True
End Sub


Sub TopTargetBugCheck_Timer()
	TopTargetBugCheck.Interval = 100
	Dim i
	For i = 1 to 4
		If Eval("TopTarget00" & i).isDropped = 0 Then
			TopTargetBugCheck.Enabled = False
			Exit Sub
		End If
	Next
	TopTarget_Reset
End Sub





Sub TargetBlue_Hit()
	LightSpinner
	If ChaseOn = 2 Then
		ChaseOn = 3
		Score_10000
		Chaser002.TimerEnabled = False
		Chaser002.Visible = 0
		ChaseCounter = 0
		Chaser003.TimerEnabled = True
		Check_Wizard
	Else
		Score_5000
	End If
	Spot_LightBlue.State = 0
End Sub


Sub Chaser001_Timer
	ChaseCounter = ChaseCounter + 1
	If ChaseCounter < 10 Then Exit Sub 
	If ChaseCounter Mod 2 = 0 Then	
		Chaser001.Visible = 1
	Else
		Chaser001.Visible = 0
	End If
End Sub

Sub Chaser002_Timer
	ChaseCounter = ChaseCounter + 1
	If ChaseCounter < 10 Then Exit Sub 
	If ChaseCounter Mod 2 = 0 Then	
		Chaser002.Visible = 1
	Else
		Chaser002.Visible = 0
	End If
End Sub

Sub Chaser003_Timer
	ChaseCounter = ChaseCounter + 1
	If ChaseCounter < 10 Then Exit Sub 
	If ChaseCounter Mod 2 = 0 Then	
		Chaser003.Visible = 1
	Else
		Chaser003.Visible = 0
	End If
End Sub

Sub Chaser004_Timer
	ChaseCounter = ChaseCounter + 1
	If ChaseCounter < 10 Then Exit Sub 
	If ChaseCounter Mod 2 = 0 Then	
		Chaser004.Visible = 1
	Else
		Chaser004.Visible = 0
	End If
End Sub

Sub Chaser005_Timer
	ChaseCounter = ChaseCounter + 1
	If ChaseCounter < 10 Then Exit Sub 
	If ChaseCounter Mod 2 = 0 Then	
		Chaser005.Visible = 1
	Else
		Chaser005.Visible = 0
	End If
End Sub

Sub Chaser006_Timer
	ChaseCounter = ChaseCounter + 1
	If ChaseCounter Mod 2 = 0 Then	
		Chaser006.Visible = 1
	Else
		Chaser006.Visible = 0
	End If
End Sub

Sub Chaser007_Timer
	ChaseCounter = ChaseCounter + 1
	If ChaseCounter Mod 2 = 0 Then	
		Chaser007.Visible = 1
	Else
		Chaser007.Visible = 0
	End If
End Sub

Sub Chaser008_Timer
	ChaseCounter = ChaseCounter + 1
	If ChaseCounter Mod 2 = 0 Then	
		Chaser008.Visible = 1
	Else
		Chaser008.Visible = 0
	End If
End Sub




Dim DMD_Fireball, DMD_FireballWizCounter, DMD_WizCase, DMD_WizHit, ChaseCounter, ChaseOn




Dim DMD_FireballWiz(4)




Sub Check_Wizard()
	Score_10000
	PlaySound "XL5_WizLetter"
	FlashShip(8)
	DMD_FireballWiz(Player) = DMD_FireballWiz(Player) + 1
	If DMD_FireballWiz(Player) = 9 Then DMD_FireballWiz(Player) = 1

	If ShowFlexDMD Then
		If DMD_FireballWiz(Player) < 8 Then 
			DMD_SwapFrames "FireballWiz_" & DMD_FireballWiz(Player), "FireballWiz_0", 20, 100
			Exit Sub
		End If
	End If
	ScoreFlash Left( "F I R E B A L L ", DMD_FireballWiz(Player)*2)
	If ShowFlexDMD Then 
		DMDShowVideo = True
		DMD_FireballWizCounter = 33
		DMD_WizCase = 1
		FlexDMD.Stage.GetImage("FireballWiz_0").SetPosition 0, 33
		FlexDMD.Stage.GetImage("FireballWiz_0").Visible = 1
		PlaySound "XL5_Whoosh_LR"
		FireballWiz.Interval = 20
		FireballWiz.Enabled = True
		FireballWiz_Timer
	Else
		DMD_WizHit = False
	End If
End Sub

Sub FireballWizBG_Timer
	DMDShowVideo = False
	DMD_SwapFrames "1BIL", "1BILA", 20, 100
	Score(Player) = Score(Player) +1000000000
	SoundKnocker
	FireballWizBG.Enabled = False

End Sub

Sub FireballWiz_Timer
	Dim i
	Select Case DMD_WizCase
		Case 1
			FireballWiz.Interval = 6
			DMD_FireballWizCounter = DMD_FireballWizCounter - 1
			If DMD_FireballWizCounter < 0 Then
				DMD_FireballWizCounter = 0
				FireballWiz.Interval = 250
				DMD_WizCase = 2
			Else
				FlexDMD.Stage.GetImage("FireballWiz_0").SetPosition 0, DMD_FireballWizCounter
			End If

		Case 2
			FireballWiz.Interval = 250
			DMD_FireballWizCounter = DMD_FireballWizCounter + 1
			If DMD_FireballWizCounter <= DMD_FireballWiz(Player) Then
				PlaySound "XL5_WizLetter"
				Eval("Fireball00" & DMD_FireballWizCounter).State = 1
				FlexDMD.Stage.GetImage("FireballWiz_" & DMD_FireballWizCounter).Visible = 1
				FlexDMD.Stage.GetImage("FireballWiz_" & DMD_FireballWizCounter - 1).Visible = 0
			Else
				If DMD_FireballWiz(Player) = 8 Then
					DMD_WizCase = 5
					FireballWiz.Interval = 1000
				Else
					FireballWiz.Interval = 200
					DMD_WizCase = 3
				End If
			End If

		Case 3
			FireballWiz.Interval = 2000
			DMD_SwapFrames "FireballWiz_" & DMD_FireballWiz(Player), "FireballWiz_0", 20, 100
			DMD_FireballWizCounter = 0
			DMD_WizCase = 18



		Case 5
			FireballWiz.Interval = 500
			DMD_FireballWizCounter = DMD_FireballWizCounter + 1
			If DMD_FireballWizCounter = 12 Then 
				DMD_WizCase = 6
			Else
				PlaySound "Bell10000"
				SoundKnocker
				FlexDMD.Stage.GetImage("FireballWiz_" & DMD_FireballWizCounter).Visible = 1
				FlexDMD.Stage.GetImage("FireballWiz_" & DMD_FireballWizCounter - 1).Visible = 0
			End If

		Case 6
			FireballWiz.Interval = 1600
			DMD_SwapFrames "FireballWiz_11", "FireballWiz_0", 30, 50
			DMD_WizCase = 7

		Case 7
			FireballWiz.Interval = 750
			DMD_FireballWizCounter = 0
			FlexDMD.Stage.GetImage("FireballWiz_11").Visible = 0
'			SoundKnocker
'			Extra_Ball
			DMD_WizCase = 9

		Case 8
			FireballWiz.Interval = 1500
			DMD_SwapFrames "FireballWiz_8", "FireballWiz_0", 30, 50
			DMD_FireballWizCounter = 0
			DMD_WizCase = 9

		Case 9
			FireballWiz.Interval = 20
			If DMD_FireballWizCounter = 0 Then PlaySound "XL5_Whoosh_LR"
			DMD_FireballWizCounter = DMD_FireballWizCounter - 1
			If DMD_FireballWizCounter < -32 Then
				FlexDMD.Stage.GetImage("FireballWiz_8").Visible = 0
				FlexDMD.Stage.GetImage("FireballWiz_8").SetPosition 0, 0
				DMD_WizCase = 10
			Else
				FlexDMD.Stage.GetImage("FireballWiz_8").SetPosition 0, DMD_FireballWizCounter
				FlexDMD.Stage.GetImage("FireballWiz_8").Visible = 1
				FlexDMD.Stage.GetImage("FireballWiz_12").SetPosition 0, 32 + DMD_FireballWizCounter
				FlexDMD.Stage.GetImage("FireballWiz_12").Visible = 1
			End If



		Case 10
			FireballWiz.Interval = 5000
			DMD_SwapFrames "FireballWiz_12", "FireballWiz_8", 10, 500
'			DMDBlinkSplashString " CONGRATULATIONS", 6, False, True
			FireballWizBG.Interval = 5000
			FireballWizBG.Enabled = True
			DMD_WizCase = 20

		Case 11
			FireballWiz.Interval = 5000
			DMD_SwapFrames "FireballWiz_12", "FireballWiz_8", 20, 250
'			DMDBlinkSplashString " CONGRATULATIONS", 6, False, True
			DMD_WizCase = 20



		Case 18
			FireballWiz.Interval = 10
			DMD_FireballWizCounter = DMD_FireballWizCounter + 1
			If DMD_FireballWizCounter > 33 Then
				FlexDMD.Stage.GetImage("FireballWiz_" & DMD_FireballWiz(Player)).Visible = 0
				FlexDMD.Stage.GetImage("FireballWiz_" & DMD_FireballWiz(Player)).SetPosition 0, 0
				DMD_WizCase = 20
			Else
				FlexDMD.Stage.GetImage("FireballWiz_" & DMD_FireballWiz(Player)).SetPosition 0, DMD_FireballWizCounter
				FlexDMD.Stage.GetImage("FireballWiz_" & DMD_FireballWiz(Player)).Visible = 1
			End If



		Case 20
			For i = 1 to 8
				Eval("Fireball00" & i).State = 0
			Next
			If DMD_FireballWiz(Player) = 8 Then DMD_FireballWiz(Player) = 0
			DMDShowVideo = False
			DMD_WizHit = False
			FireballWiz.Enabled = False

	End Select
End Sub





'########### In-line Targets & Jackpot ###


Sub Top_Target001_Hit()
	Score_5000
End Sub

Sub Top_Target002_Hit()
	Jackpot_Target.TimerEnabled = False
	Score_5000
End Sub

Sub Top_Target003_Hit()
	Jackpot_Target.TimerEnabled = False
	Score_5000
End Sub

Sub Top_Target_Reset()
	Dim i
	For i = 1 to 3
		Eval("Top_Target00" & i).isDropped = 0
	Next
	SoundDropTargetRelease
End Sub


Sub Jackpot_Target_Hit()
	Dim JackpotTotal
	Dim JackpotString
	If JackpotLit(Player) = 2 Then
		JackpotTotal = Jackpot(Player) * ScoreX
		If Jackpot_TargetCounter = 3 Then
			JackpotTotal = JackpotTotal * 5		' SuperJackpot 
		End If
		JackpotString = Left(JackpotTotal, Len(JackpotTotal) - 3) & "," & Right(JackpotTotal, 3)
		If JackpotTotal > 999999 Then
			JackpotString = Left(JackpotTotal, Len(JackpotTotal) - 6) & "," & Right(JackpotString, 7)
		End If
		If JackpotTotal > 999999999 Then
			JackpotString = Left(JackpotTotal, Len(JackpotTotal) - 9) & "," & Right(JackpotString, 11)
		End If
		If Jackpot_TargetCounter = 3 Then
			ScoreFlash "SUPER JACKPOT " & JackpotString
			PlaySound "XL5_OhSteve"
			FlashJackpot(6)
			If ShowFlexDMD Then DMD_PlayJackpot(2)
		Else
			ScoreFlash "   JACKPOT     " & JackpotString
			FlashJackpot(2)
			If ShowFlexDMD Then DMD_PlayJackpot(1)
		End If
		LightSeq001.Play SeqDiagDownRightOn, 25, 3
		Score(Player) = Score(Player) + JackpotTotal
		ShowScore
		Jackpot(Player) = Jackpot(Player) * 2
		If Jackpot(Player) > 64000000 Then
			Jackpot(Player) = 64000000
		End If
		SoundKnocker
		Jackpot_Light001.state = 2
		Jackpot_TargetCounter = 3
		Jackpot_Target.TimerInterval = 10000
		Jackpot_Target.TimerEnabled = True
'		CycleFireball
		LightSequence(1)
		Movie_RobTalk "Rob_Jackpot", 18
	Else
		Score_5000
	End If
End Sub

Sub Jackpot_Target_Timer()
	Jackpot_Target.TimerInterval = 5000
	Jackpot_Light001.State = 0
	Eval("Top_Target00" & Jackpot_TargetCounter).isDropped = False
	SoundDropTargetUp
	Jackpot_TargetCounter = Jackpot_TargetCounter - 1
	If 	Jackpot_TargetCounter = 0 Then
		Jackpot_Target.TimerEnabled = False
	End If
End Sub


' ##############  Spinner

Sub LightSpinner()
	Dim i
	If Spinner_Lit = 0 Then
		Spinner_Lit = 1
		FlashAdBonus(11)
		If Not PowerActive And Not WizardActive Then
			For i = 1 to 4
				Eval("Spin_Light" & i).State = 2
			Next
			SpinLights.Interval = 3000
			SpinLights.Enabled = True
		End If
		Flash_Light("Robert_Light001")
		Movie_Robtalk "Rob_Spinnerlit", 22
		If ShowFlexDMD Then DMDPlay_SpinnerLit
	End If
End Sub


Sub SpinDelayed_Timer
	Dim i
	Movie_Robtalk "Rob_Spinnerlit", 22
	If ShowFlexDMD Then DMDPlay_SpinnerLit
	SpinDelayed.Enabled = False
End Sub


Sub Spinner001_Spin()
	SoundSpinnerHit
	PowerCount = Not PowerCount
	If PowerActive Then
		If PowerCount Then 
			M_PowerHigh = M_PowerHigh + 1
			PlaySound "XL5_DMD_Laser"	'"Bell100"
		End If
		Exit Sub
	End If
	If WizardActive Then
		If PowerCount Then 
			PlaySound "XL5_DMD_Laser"
			Score(Player) = Score(Player) + 500000 * ScoreX
			If ShowFlexDMD Then DMD_SwapFrames "500K", "500KA", 5,50
		End If
		Exit Sub
	End If
	If Spinner_Lit = 1 Then
'		If PowerCount Then PlaySound "Bell10"
		If Spin_Light1.state > 0 Then
			Spin_Light1.state = 0
			Spin_Light2.state = 1
'			PlaySound "Bell1000"
			Score_1000
		Elseif Spin_Light2.state > 0 Then
			Spin_Light2.state = 0
			Spin_Light3.state = 1
'			PlaySound "Bell1000"
			Score_1000
		Elseif Spin_Light3.state > 0 Then
			Spin_Light3.state = 0
			Spin_Light4.state = 1
'			PlaySound "Bell1000"
			Score_1000
		Elseif Spin_Light4.state > 0 Then
			Spin_Light1.state = 1
			Spin_Light4.state = 0
			Flash_Light("Robert_Light001")
			FlashAdBonus(1)
			Increase_Bonus

			If BonusMaxed Then
				Score_50000
			Else
				Score_10000
			End If
		End If
	End If
End Sub

Sub SpinLights_Timer
	If Not PowerActive And Not WizardActive Then
		Dim i
		For i = 1 to 4
			Eval("Spin_Light" & i).State = 0
		Next
		If Spinner_Lit = 1 Then Spin_Light1.State = 1
	End If
	SpinLights.Enabled = False
End Sub





' #################### The Bumpers, Mario, the Bumpers!


Sub Bumper001_Hit()
	SoundBumperHit("Top")
	If ShieldActive Then
		BumperXCounter = 0
		DMD_StopBumperX.Interval = 100
		DMD_StopBumperX.Enabled = True
	Else
		If LightBumper001.state = 1 Then
			Score_1000
		Else
			Score_100
		End If
		LightBumperRed001.Duration 1, 100, 0
	End If
End Sub



Sub Bumper003_Hit()
	SoundBumperHit("Bottom")
	If ShieldActive Then
		BumperXCounter = 0
		DMD_StopBumperX.Interval = 100
		DMD_StopBumperX.Enabled = True
	Else
		If LightBumper003.state = 1 Then
			Score_1000
		Else
			Score_10
		End If
		LightBumperRed002.Duration 1, 100, 0
	End If
End Sub

Sub Bumper002_Hit()
	SoundBumperHit("Middle")
	If ShieldActive Then
		BumperXCounter = 0
		DMD_StopBumperX.Interval = 100
		DMD_StopBumperX.Enabled = True
	Else
		If LightBumper002.state = 1 Then
			Score_1000
		Else
			Score_10
		End If
		LightBumperRed003.Duration 1, 100, 0
	End If
End Sub




Dim BumperXCounter, BumperScore



Sub DMD_StopBumperX_Timer
	Dim bumperstring
	BumperXCounter = BumperXCounter + 1
	If BumperXCounter = 1 Then 
		Score_10000
		BumperScore = BumperScore - 1 
		If ShowFlexDMD Then
			DMD_StopBumperX.Interval = 2000
			FlexDMD.Stage.GetImage("BumperX2").Visible = 1
			bumperstring = BumperScore
			If BumperScore < 10 Then bumperstring = "0" & bumperstring
			FlexDMD.Stage.GetLabel("BumperScore").Text = bumperstring
			FlexDMD.Stage.GetLabel("BumperScoreShadow").Text = bumperstring
		End If
		If BumperScore = 0 Then
			BumperScore = 25
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetLabel("BumperScore").Text = ""
				FlexDMD.Stage.GetLabel("BumperScoreShadow").Text = ""
				FlexDMD.Stage.GetImage("BumperX2").Visible = 0
				DMD_SwapFrames "1MIL", "1MILA", 20, 100
			End If
			Score(Player) = Score(Player)  + 1000000 * ScoreX
'			DMD_StopBumperX.Enabled = False
		End If
	ElseIf BumperXCounter = 2 Then 
		If ShowFlexDMD Then
			FlexDMD.Stage.GetImage("BumperX2").Visible = 0
			FlexDMD.Stage.GetLabel("BumperScore").Text = ""
			FlexDMD.Stage.GetLabel("BumperScoreShadow").Text = ""
		End If
		DMD_StopBumperX.Enabled = False
	End If
End Sub






'############## Save Credits & High Score


Sub SaveData
	' Based on Black's Highscore routines
	Dim FileObj, ScoreFile, i
	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then 
		Exit Sub
	End If
	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & HSFileName, True)

	ScoreFile.WriteLine Credits
	For i = 1 to 5
		scorefile.WriteLine HSScore(i)
		scorefile.WriteLine HSName(i)
	Next
	ScoreFile.WriteLine MeteorHS
	ScoreFile.WriteLine MeteorHSName

	ScoreFile.Close
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub


'##### Load Credits & High Scores 

Sub LoadData
	Dim FileObj, ScoreFile, i

    Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) Then Exit Sub

	If Not FileObj.FileExists(UserDirectory & HSFileName) Then
		Set FileObj = Nothing
		SaveData
		Exit Sub
	End if

	Set ScoreFile = FileObj.GetFile(UserDirectory & HSFileName)
	Set TextStr = ScoreFile.OpenAsTextStream(1,0)
	If TextStr.AtEndOfStream Then Exit Sub

	Credits = cdbl(textstr.readline)
	If Credits > 0 Then DOF 125, DOFOn
	For i = 1 to 5
		HSScore(i) = cdbl(textstr.readline)
		HSName(i)  = textstr.readline
	Next
	If Not TextStr.AtEndOfStream Then MeteorHS = Int(textstr.readline)
	If Not TextStr.AtEndOfStream Then MeteorHSName = textstr.readline
	TextStr.Close
	Set ScoreFile = Nothing
    Set FileObj = Nothing
	HighScore =	HSScore(1)
End Sub


sub CheckHighScore
	dim si, sj, i
	dim stemp
	dim stempplayers
	for i=1 to 4
		sortscores(i)=0
		sortplayers(i)=0
	next
	for i = 1 to Players
		sortscores(i) =	Score(i)
		sortplayers(i)=i
	next

	for si = 1 to Players
		for sj = 1 to Players-1
			if sortscores(sj)>sortscores(sj+1) then
				stemp=sortscores(sj+1)
				stempplayers=sortplayers(sj+1)
				sortscores(sj+1)=sortscores(sj)
				sortplayers(sj+1)=sortplayers(sj)
				sortscores(sj)=stemp
				sortplayers(sj)=stempplayers
			end if
		next
	next
	ScoreChecker=4
	CheckAllScores=1
	NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)	
end sub



' ============================================================================================
' GNMOD - Multiple High Score Display and Collection
' ============================================================================================
Dim EnteringInitials		' Normally zero, set to non-zero to enter initials
EnteringInitials = 0

Dim PlungerPulled
PlungerPulled = 0

Dim SelectedChar			' character under the "cursor" when entering initials

Dim HSTimerCount			' Pass counter for HS timer, scores are cycled by the timer
HSTimerCount = 5			' Timer is initially enabled, it'll wrap from 5 to 1 when it's displayed

Dim InitialString			' the string holding the player's initials as they're entered

Dim AlphaString				' A-Z, 0-9, space (_) and backspace (<)
Dim AlphaStringPos			' pointer to AlphaString, move forward and backward with flipper keys
AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"

Dim HSNewHigh				' The new score to be recorded


Dim DMD_OldChar, DMD_HSNameString, AttractModeActive




dim ScoreChecker
dim CheckAllScores
dim sortscores(4)
dim sortplayers(4)



Sub HighScoreTimer_Timer
	if EnteringInitials then
		if HSTimerCount = 1 then
			SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
			HSTimerCount = 2
		else
			SetHSLine 3, InitialString
			HSTimerCount = 1
		end if
	elseif Not Tilted then
		SetHSLine 1, "HIGH SCORE"
		SetFormatLine HSScore(1)
		SetHSLine 3, HSName(1)
		HSTimerCount = 5	' set so the highest score will show after the game is over
		HighScoreTimer.enabled=false
	elseif CheckAllScores then
		NewHighScore sortscores(ScoreChecker),sortplayers(ScoreChecker)

	else
		If Not AttractModeActive Then
			AttractModeActive = True
			Attract.Interval = 5000
			AttractMode
		End If

		' cycle through high scores
		HighScoreTimer.interval=2000
		HSTimerCount = HSTimerCount + 1
		if HsTimerCount > 5 then
			HSTimerCount = 1
		End If

		SetHSLine 1, "HI SCORE "+FormatNumber(HSTimerCount,0)
		SetFormatLine HSScore(HSTimerCount)
		SetHSLine 3, HSName(HSTimerCount)

	end if
End Sub

Function GetHSChar(String, Index)
	dim ThisChar
	dim FileName
	ThisChar = Mid(String, Index, 1)
	FileName = "XL5_"
	if ThisChar = " " or ThisChar = "" then
		FileName = FileName & "BL"
	elseif ThisChar = "<" then
		FileName = FileName & "LT"
	elseif ThisChar = "_" then
		FileName = FileName & "SP"
	elseif ThisChar = "," then
		FileName = FileName & "CM"
	else
		FileName = FileName & ThisChar
	End If
	GetHSChar = FileName
End Function

Sub SetHsLine(LineNo, String)
	dim Letter
	dim ThisDigit
	dim ThisChar
	dim StrLen
	dim LetterLine
	dim Index
	dim StartHSArray
	dim EndHSArray
	dim LetterName
	dim xfor
	StartHSArray=array(0,1,11,25)
	EndHSArray=array(0,10,24,31)
	StrLen = len(string)
	Index = 1

	for xfor = StartHSArray(LineNo) to EndHSArray(LineNo)
		Eval("HS"&xfor).image = GetHSChar(String, Index)
		Index = Index + 1
	next

End Sub

Sub SetFormatLine(String)
	Dim Index
	Dim xfor
	Dim HString
	Index = 1

	If Len(String) < 12 Then
		If String > 999 Then
			HString = Left(String, Len(String) - 3) & "," & Right(String, 3)
		End If
		If String > 999999 Then
			HString = Left(String, Len(String) - 6) & "," & Right(HString, 7)
		End If
		If String > 999999999 Then
			HString = Left(String, Len(String) - 9) & "," & Right(HString, 11)
		End If
		HString = Left("              ", 14 - Len(HString)) & HString
	Else 
		HString = String
	End If

	for xfor = 11 to 24
		Eval("HS"&xfor).image = GetHSChar(HString, Index)
		Index = Index + 1
	next
End Sub

Sub NewHighScore(NewScore, PlayNum)
	If NewScore > HSScore(5) Then
		HighScoreTimer.interval = 500
		HSTimerCount = 1
		AlphaStringPos = 1		' start with first character "A"
		EnteringInitials = 1	' intercept the control keys while entering initials
		InitialString = ""		' initials entered so far, initialize to empty
		SetHSLine 1, " PLAYER "+FormatNumber(PlayNum,0)
		SetHSLine 2, " ENTER NAME"
		SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
		HSNewHigh = NewScore
		If ShowFlexDMD Then 
			FlexDMD.Stage.GetImage("HS_BG").Visible = 1
			DMDPlay_FX 5, -1
			Set DMD_OldChar = FlexDMD.Stage.GetImage("Char2_" & AlphaStringPos)
			DMD_OldChar.Visible = 1
		End If
		AddSpecial
	End if
	ScoreChecker=ScoreChecker-1
	if ScoreChecker=0 then
		CheckAllScores=0
	end if
End Sub

Sub CollectInitials(keycode)
	If keycode = LeftFlipperKey Then
		' back up to previous character
		AlphaStringPos = AlphaStringPos - 1
		If AlphaStringPos < 1 Then
			AlphaStringPos = len(AlphaString)		' handle wrap from beginning to end
			If InitialString = "" Then
				' Skip the backspace if there are no characters to backspace over
				AlphaStringPos = AlphaStringPos - 1
			End if
		End If
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		If ShowFlexDMD Then 
			DMD_OldChar.Visible = 0
			Set DMD_OldChar = FlexDMD.Stage.GetImage("Char2_" & AlphaStringPos)
			DMD_OldChar.Visible = 1
		End If
		PlaySound "Bell10"
	ElseIf keycode = RightFlipperKey Then
		' advance to next character
		AlphaStringPos = AlphaStringPos + 1
		If AlphaStringPos > len(AlphaString) or (AlphaStringPos = len(AlphaString) and InitialString = "") Then
			' Skip the backspace if there are no characters to backspace over
			AlphaStringPos = 1
		End If
		SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
		If ShowFlexDMD Then 
			DMD_OldChar.Visible = 0
			Set DMD_OldChar = FlexDMD.Stage.GetImage("Char2_" & AlphaStringPos)
			DMD_OldChar.Visible = 1
		End If
		PlaySound "Bell10"
	ElseIf keycode = StartGameKey or keycode = PlungerKey Then
		SelectedChar = MID(AlphaString, AlphaStringPos, 1)
		If SelectedChar = "_" Then
			InitialString = InitialString & " "
			PlaySound("Bell100")
		ElseIf SelectedChar = "<" Then
			InitialString = MID(InitialString, 1, len(InitialString) - 1)
			If Len(InitialString) = 0 Then
				' If there are no more characters to back over, don't leave the < displayed
				AlphaStringPos = 1
			End If
			PlaySound("Bell1000")
		Else
			InitialString = InitialString & SelectedChar
			PlaySound("Bell100")
		End If
		If Len(InitialString) < 3 Then
			SetHSLine 3, InitialString & SelectedChar
			If ShowFlexDMD Then 
				FlexDMD.Stage.GetLabel("HSName").Text = InitialString
				FlexDMD.Stage.GetLabel("HSName").Visible = 1
				FlexDMD.Stage.GetLabel("HSName_S").Text = InitialString
				FlexDMD.Stage.GetLabel("HSName_S").Visible = 1
			End If
		End If
	End If
	if len(InitialString) = 3 Then
		' save the score
		Dim i
		for i = 5 to 1 step -1
			If i = 1 or (HSNewHigh > HSScore(i) and HSNewHigh <= HSScore(i - 1)) Then
				' Replace the score at this location
				If i < 5 Then
					HSScore(i + 1) = HSScore(i)
					HSName(i + 1) = HSName(i)
				End If
				EnteringInitials = 0
				HSScore(i) = HSNewHigh
				HSName(i) = InitialString
				HSTimerCount = 5
				AttractModeActive = False
				HighScoreTimer_Timer
				HighScoreTimer.interval = 2000
				PlaySound("Bell1000")
				SaveData
				FadeOutSound CurrentMusic, 1
				If ShowFlexDMD Then
					DMD_HSClose
				Else
					Attract.Interval = 5000
					AttractMode
				End If

				Exit Sub
			ElseIf i < 5 then
				HSScore(i + 1) = HSScore(i)
				HSName(i + 1) = HSName(i)
			End If
		Next
	End If
End Sub
' END GNMOD


Dim HSSwap, HSSCount



Sub DMD_HSClose
	DMD_HS.Interval = 500
	FlexDMD.Stage.GetLabel("HSName").Text = InitialString
	FlexDMD.Stage.GetLabel("HSName_S").Text = InitialString
	HSSCount = 0
	HSSwap = True
	DMD_HS.Enabled = True
End Sub


Sub DMD_HS_Timer
	DMD_HS.Interval = 50
	HSSCount = HSSCount + 1
	If HSSCount = 1 Then
		FlexDMD.Stage.GetImage("HS_BG2").Visible = 1
		FlexDMD.Stage.GetImage("HS_BG").Visible = 0
		DMD_OldChar.Visible = 0
	End If

'	If HSSwap Then
'		FlexDMD.Stage.GetLabel("HSName").SetPosition 49, 7
'		FlexDMD.Stage.GetLabel("HSName_S").SetPosition 47, 5
'		HSSwap = False
'	Else
'		FlexDMD.Stage.GetLabel("HSName_S").SetPosition 49, 7
'		FlexDMD.Stage.GetLabel("HSName").SetPosition 47, 5
'		HSSwap = True
'	End If

	If HSSCount = 20 Then DMD_CloseCircle(7)

	If HSSCount = 36 Then
		FlexDMD.Stage.GetLabel("HSName").Visible = 0
		FlexDMD.Stage.GetLabel("HSName_S").Visible = 0
		FlexDMD.Stage.GetImage("HS_BG2").Visible = 0
		DMDStop_FX(5)
		Attract.Interval = 5000
		AttractMode
		DMD_HS.Enabled = False
	End If
End Sub




'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************

Dim RStep, Lstep, UStep, URStep, MRStep, LRStep, MLStep


Sub RightSlingShot_Slingshot
	If Not Tilted Then
		SoundSlingshotHit("SlingR")
		DOF 104, DOFPulse
		RSling.Visible = 0
		RSling1.Visible = 1
		sling1.rotx = 16
		RStep = 0
		RightSlingShot.TimerEnabled = 1
		SwapLanes
		Score_10
	End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2
			RSLing1.Visible = 0
			RSLing2.Visible = 1
			sling1.rotx = 7

        Case 3
			RSLing2.Visible = 0
			RSLing.Visible = 1
			sling1.rotx = 0
			RightSlingShot.TimerEnabled = 0

    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	If Not Tilted Then
		SoundSlingshotHit("SlingL")
		DOF 103, DOFPulse
		LSling.Visible = 0
		LSling3.Visible = 1
		sling2.rotx = 10
		LStep = 0
		LeftSlingShot.TimerInterval = 10
		LeftSlingShot.TimerEnabled = 1
		SwapLanes
		Score_10
	End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
		Case 0
			LSLing1.Visible = 1
			LSLing3.Visible = 0
			sling2.rotx = 20
			LeftSlingShot.TimerInterval = 10

		Case 1
			LSLing1A.Visible = 1
			LSLing1.Visible = 0
			sling2.rotx = 20
			LeftSlingShot.TimerInterval = 20

		Case 2
			LSLing2.Visible = 1
			LSLing1A.Visible = 0
			sling2.rotx = 15
			LeftSlingShot.TimerInterval = 10

		Case 3
			LSLing2.Visible = 0
			LSLing3.Visible = 1
			sling2.rotx = 10
        Case 4
			LSLing3.Visible = 0
			LSLing4.Visible = 1
			sling2.rotx = 5

        Case 5
			LSLing4.Visible = 0
			LSLing.Visible = 1
			sling2.rotx = 0

        Case 6
			LSLing.Visible = 0
			LSLing4A.Visible = 1
			LeftSlingShot.TimerInterval = 20

        Case 7
			LSLing4A.Visible = 0
			LSLing.Visible = 1
			LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


Sub LowRightSlingShot_Slingshot
	If Not Tilted Then
		SoundSlingshotHit("SlingR")
		DOF 116, DOFPulse
		LRSling.Visible = 0
		LRSling1.Visible = 1
		LowRightSling.rotx = 16
		LRStep = 0
		LowRightSlingShot.TimerEnabled = 1
		SwapLanes
		Score_10
	End If
End Sub

Sub LowRightSlingShot_Timer
    Select Case LRStep
        Case 2
			LRSLing1.Visible = 0
			LRSLing2.Visible = 1
			LowRightSling.rotx = 8

        Case 3
			LRSLing2.Visible = 0
			LRSLing.Visible = 1
			LowRightSling.rotx = 0
			LowRightSlingShot.TimerEnabled = 0

    End Select
    LRStep = LRStep + 1
End Sub

Sub MidRightSlingShot_Slingshot
	If Not Tilted Then
		SoundSlingshotHit("SlingR")
		DOF 120, DOFPulse
		MRSling2.Visible = 1
		MRSLing.Visible = 0
		MidRightSling.rotx = 10
		MRStep = 0
		MidRightSlingShot.TimerInterval = 10
		MidRightSlingShot.TimerEnabled = 1
		Score_100
	End If
End Sub

Sub MidRightSlingShot_Timer
    Select Case MRStep
		Case 0
			MRSLing1.Visible = 1
			MRSLing2.Visible = 0
			MidRightSlingShot.TimerInterval = 20
			MidRightSling.rotx = 16
		Case 3
			MRSLing1.Visible = 0
			MRSLing2.Visible = 1
			MidRightSlingShot.TimerInterval = 10
			MidRightSling.rotx = 12
		Case 4
			MRSLing2.Visible = 0
			MRSling3.Visible = 1
			MidRightSling.rotx = 8
		Case 5
			MRSLing3.Visible = 0
			MRSLing.Visible = 1
			MidRightSling.rotx = 0
			MidRightSlingShot.TimerEnabled = 0
    End Select
    MRStep = MRStep + 1
End Sub


Sub UpRightSlingShot_Slingshot
	If Not Tilted Then
		SoundSlingshotHit("SlingR")
		DOF 110, DOFPulse
		URSling2.Visible = 1
		URSLing.Visible = 0
		UpRightSling.rotx = 10
		URStep = 0
		UpRightSlingShot.TimerInterval = 10
		UpRightSlingShot.TimerEnabled = 1
		Score_100
	End If
End Sub

Sub UpRightSlingShot_Timer

    Select Case URStep
		Case 0
			URSLing1.Visible = 1
			URSLing2.Visible = 0
			UpRightSlingShot.TimerInterval = 20
			UpRightSling.rotx = 16
		Case 3
			URSLing1.Visible = 0
			URSLing2.Visible = 1
			UpRightSlingShot.TimerInterval = 10
			UpRightSling.rotx = 12
		Case 4
			URSLing2.Visible = 0
			URSling3.Visible = 1
			UpRightSling.rotx = 8
		Case 5
			URSLing3.Visible = 0
			URSLing.Visible = 1
			UpRightSling.rotx = 0
			UpRightSlingShot.TimerEnabled = 0
    End Select
    URStep = URStep + 1
End Sub




Sub UpperSlingShot_Slingshot
	If Not Tilted Then
		SoundSlingshotHit("SlingR")
		DOF 107, DOFPulse
		USling1.Visible = 1
		USling.Visible = 0
		UpperSling.rotx = 20
		UStep = 0
		UpperSlingShot.TimerEnabled = 1
		Score_10
	End If
End Sub

Sub UpperSlingShot_Timer
    Select Case UStep
		Case 3
			UpperSling.rotx = 10
			USling2.Visible = 1
			USling1.Visible = 0
		Case 4
			UpperSling.rotx = 0
			USling.Visible = 1
			USling2.Visible = 0

			UpperSlingShot.TimerEnabled = 0
    End Select
    UStep = UStep + 1
End Sub



Sub MidLeftSlingShot_Slingshot
	If Not Tilted Then
		SoundSlingshotHit("SlingR")
		DOF 115, DOFPulse
		MLSling.Visible = 0
		MLSling1.Visible = 3
		MidLeftSling.rotx = 8
		MLStep = 0
		MidLeftSlingShot.TimerInterval = 25
		MidLeftSlingShot.TimerEnabled = 1
		Score_10
	End If
End Sub

Sub MidLeftSlingShot_Timer
    Select Case MLStep
		Case 0
			MLSLing1.Visible = 1
			MLSLing3.Visible = 0
			MidLeftSling.rotx = 16

		Case 2
			MLSLing2.Visible = 1
			MLSLing1.Visible = 0
			MidLeftSling.rotx = 12

		Case 3
			MLSLing2.Visible = 0
			MLSLing3.Visible = 1
			MidLeftSling.rotx = 8

        Case 4
			MLSLing3.Visible = 0
			MLSLing4.Visible = 1
			MidLeftSling.rotx = 4
        Case 5
			MLSLing4.Visible = 0
			MLSLing.Visible = 1
			MidLeftSling.rotx = 0
			MidLeftSlingShot.TimerEnabled = 0


'		Case 3
'			MLSling1.Visible = 0
'			MLSling2.Visible = 1
'			MidLeftSling.rotx = 8
'		Case 4
'			MidLeftSling.rotx = 0
'			MLSling2.Visible = 0
'			MLSling.Visible = 1
'			MidLeftSlingShot.TimerEnabled = 0
    End Select
    MLStep = MLStep + 1
End Sub


'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
'	Vol = Vol * (1 - (ball.y / table1.height) ' Modifies the Volume based on the ball distance/position
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub


'********************************************************************
'      JP's VP10 Rolling Sounds (+rothbauerw's Dropping Sounds)
'********************************************************************


Dim towerY


Const tnob = 5 ' total number of balls
ReDim rolling(tnob)

Sub InitRolling
    Dim i
    For i = 1 to tnob
        rolling(i) = False
    Next
	RollingTimer.Enabled = 1
End Sub


Sub RollingTimer_Timer()
    Dim BOT, b

	towerY = towerY - 0.25
	If towerY < 0 Then towerY = 359.75
	SpaceTower.RotY = towerY

    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
		StopSound("RampLoop" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    For b = 0 to UBound(BOT)
        ' play the rolling sound for each ball
        If BallVel(BOT(b) ) > 0.75 AND BOT(b).z < 30 Then
				rolling(b) = True
				StopSound("RampLoop" & b)
				PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        ElseIf BallVel(BOT(b) ) > 0.25 AND BOT(b).z > 30 Then
				rolling(b) = True
		        StopSound("fx_ballrolling" & b)
				PlaySound "RampLoop" & b, -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
				StopSound("RampLoop" & b)
                rolling(b) = False
            End If
        End If

        ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 40 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop" & b, 0, ABS(BOT(b).Velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound ("Ball_Collide_" & Int(Rnd*7)+1), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


Sub Pins_Hit (idx)
	SoundMetalHit
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	'PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	SoundMetalHit
End Sub

Sub Metals_Medium_Hit (idx)
	'PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	SoundMetalHit
End Sub

Sub Metals2_Hit (idx)
	'PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	SoundMetalHit
End Sub

Sub Gates_Hit (idx)
	SoundGateHit("Heavy")
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		SoundRubbersHit
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		SoundRubbersHit
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		SoundWallHit
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		SoundWallHit
 	End If
End Sub




Sub Walls_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		SoundWallHit
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		SoundWallHit
 	End If
End Sub



Sub LeftFlipper_Collide(parm)
	SoundBallOnFlipperHit
End Sub

Sub RightFlipper_Collide(parm)
 	SoundBallOnFlipperHit
End Sub





' 		 --- Fleep Sounds --- '
' --- Ported to FP by AnonTet --- '
'adddebugtext "Fleep Sound Code loaded"
' -- OPTIONS -- '

' Lets Avoid distortion by playing too loud (Not really needed for Fleep sounds but might help "normalize" all table sounds
Const MaxSoundLevel = 0.90 
Dim FinalSpeed, SoundFactor

Dim StartButtonSoundFactor, CoinSoundFactor
Dim PlungerReleaseSoundFactor, PlungerPullSoundFactor 
Dim NudgeLeftSoundFactor,  NudgeCenterSoundFactor, NudgeRightSoundFactor


' Values must not be zero and <= 1
CoinSoundFactor = 0.65											
StartButtonSoundFactor = 0.65

NudgeLeftSoundFactor = 1										
NudgeRightSoundFactor = 1										
NudgeCenterSoundFactor = 1										

PlungerReleaseSoundFactor = 0.80 								
PlungerPullSoundFactor = 0.80									

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpSoundFactor, FlipperDownSoundFactor
Dim SlingshotSoundFactor, BumperSoundFactor, KnockerSoundFactor

' Values must not be zero and <= 1
FlipperUpSoundFactor = 0.40         
FlipperDownSoundFactor = 0.30       

SlingshotSoundFactor = 0.70			

BumperSoundFactor = 0.70			

KnockerSoundFactor = 1 					

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberSoundFactor
Dim SaucerSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor
Dim WallImpactSoundFactor, MetalImpactSoundFactor
Dim BallOnFlipperSoundEnabled, FlipperRubberSoundFactor

' Values must not be zero and <= 1
'BallBouncePlayfieldSoftFactor = 0.025		
'BallBouncePlayfieldHardFactor = 0.025		
WallImpactSoundFactor = 0.27				
MetalImpactSoundFactor = 0.3		

RubberSoundFactor = 0.45						

SaucerSoundFactor = 0.70							

BallOnFlipperSoundEnabled = 1		 	' 1=On (Default), 0=Off 
FlipperRubberSoundFactor  = 0.40		


'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundFactor, TargetSoundFactor, SpinnerSoundFactor
Dim RolloverSoundFactor, StarTriggerSoundFactor

' Values must not be zero and <= 1
GateSoundFactor = 0.20				

TargetSoundFactor = 1.0			

SpinnerSoundFactor = 0.50         

RolloverSoundFactor = 0.40       

StarTriggerSoundFactor = 0.25

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundFactor, BallReleaseSoundFactor
Dim BottomArchBallGuideSoundFactor ', FlipperBallGuideSoundFactor 

' Values must not be zero and <= 1
DrainSoundFactor = 0.70			

BallReleaseSoundFactor = 0.70

BottomArchBallGuideSoundFactor = 0.50


' -- SOUND SUPPORT ROUTINES -- '

' - Ball release from drain
Sub SoundBallRelease()
	PlaySound("BallRelease" & Int(Rnd*7)+1), BallReleaseSoundFactor
End Sub
' - Coin sounds
Sub SoundCoinIn()
	 Select Case Int(rnd*3)
		 Case 0: PlaySound "Coin_In_1" , (MaxSoundLevel * CoinSoundFactor)
		 Case 1: PlaySound "Coin_In_2" , (MaxSoundLevel * CoinSoundFactor)
		 Case 2: PlaySound "Coin_In_3" , (MaxSoundLevel * CoinSoundFactor)
	 End Select
End Sub


' - Rubber sounds
Sub SoundRubbersHit()
	 FinalSpeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)	'
	 SoundFactor = FormatNumber((FinalSpeed / 1000) * (MaxSoundLevel * RubberSoundFactor),4)
	 
	 If FinalSpeed >= 1100 then PlaySound ("Rubber_1_Hard"  ), MaxSoundLevel : End If
	 If (FinalSpeed <= 1100 and FinalSpeed >= 800) then PlaySound ("Rubber_Strong_" & Int(Rnd*9)+1 ), SoundFactor : End If
	 If FinalSpeed <800 Then PlaySound ("Rubber_" & Int(Rnd*9)+1), SoundFactor +0.1: End If
End Sub


' - Bumper sounds
Sub SoundBumperHit(bumper)
	Select Case bumper
		Case "Top"   : 
			PlaySound SoundFXDOF("Bumpers_Top_" 	 & Int(Rnd*5)+1,109,DOFPulse,DOFContactors), (MaxSoundLevel * BumperSoundFactor)
		
		Case "Middle": 
			PlaySound SoundFXDOF("Bumpers_Middle_" & Int(Rnd*5)+1,107,DOFPulse,DOFContactors), (MaxSoundLevel * BumperSoundFactor)
		
		Case "Bottom": 
			PlaySound SoundFXDOF("Bumpers_Bottom_" & Int(Rnd*5)+1,110,DOFPulse,DOFContactors), (MaxSoundLevel * BumperSoundFactor)
	End Select
End Sub


' - Drain sounds
Sub SoundDrainHit()
	 PlaySound "Drain_" & Int(Rnd*9)+1 , (MaxSoundLevel * DrainSoundFactor)
End Sub


' - Flippers sounds
Sub SoundFlippers(state)
	 SoundFactor = MaxSoundLevel * FlipperUpSoundFactor
	 Select Case state
		 Case "LfUp": 	 
			PlaySound SoundFXDOF("Flipper_L0" & Int(Rnd*9)+1,101,DOFOn,DOFContactors), SoundFactor

		 Case "RfUp": 	 
			PlaySound SoundFXDOF("Flipper_R0" & Int(Rnd*9)+1,102,DOFOn,DOFContactors), SoundFactor

		 Case "LfDown": 
			PlaySound SoundFXDOF("Flipper_Left_Down_" & Int(Rnd*7)+1,101,DOFOff,DOFContactors), SoundFactor

		 Case "RfDown": 
			PlaySound SoundFXDOF("Flipper_Right_Down_" & Int(Rnd*8)+1,102,DOFOff,DOFContactors), SoundFactor
	 End Select
End Sub


' - Slingshot sounds
Sub SoundSlingshotHit(sling)
	 SoundFactor = MaxSoundLevel * FlipperUpSoundFactor
	 Select Case sling
		 Case "SlingL": PlaySound "Sling_L" & Int(Rnd*10)+1, SoundFactor
		 Case "SlingR": PlaySound "Sling_R" & Int(Rnd*8)+1 , SoundFactor
	 End Select
End Sub


' - Nudge Sounds
Sub SoundNudgeLeft()
	 PlaySound "Nudge_" & Int(Rnd*2)+1 , (MaxSoundLevel * NudgeLeftSoundFactor)
End Sub

Sub SoundNudgeRight()
	 PlaySound "Nudge_" & Int(Rnd*2)+1 , (MaxSoundLevel * NudgeRightSoundFactor)
End Sub

Sub SoundNudgeCenter()
	 PlaySound "Nudge_" & Int(Rnd*2)+1 , (MaxSoundLevel * NudgeCenterSoundFactor)
End Sub


' - Plunger Sounds
Sub SoundPlungerPull()
	 PlaySound "Plunger_Pull_1", (MaxSoundLevel * PlungerPullSoundFactor)
End Sub

Sub SoundPlungerReleaseBall()
	 PlaySound "Plunger_Release_Ball", (MaxSoundLevel * PlungerReleaseSoundFactor)
End Sub

Sub SoundPlungerReleaseNoBall()
	 PlaySound "Plunger_Release_No_Ball", (MaxSoundLevel * PlungerReleaseSoundFactor)
End Sub


' - Target Sounds
''- Target hit - Use for ball hit the target
Sub SoundTargetsHit()
	 FinalSpeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)	'1200	'xBAM.Ball.HitSpeed
	 SoundFactor = FormatNumber((FinalSpeed / 1000) * (MaxSoundLevel * TargetSoundFactor))
	 If FinalSpeed >= 1100 Then 
			PlaySound ("Target_Hit_" & Int(Rnd*4)+5), SoundFactor
		 Else
			PlaySound ("Target_Hit_" & Int(Rnd*4)+1), SoundFactor
	 End If
End Sub

''-Target drop - drop target drops into table
Sub SoundDropTargetDown()
	 PlaySound ("Drop_Target_Down_" & Int(Rnd*6)+1), (MaxSoundLevel)' * TargetSoundFactor)
End Sub

''- Target reset - target pops up
Sub SoundDropTargetUp()
	 PlaySound "droptarget_up", MaxSoundLevel
End Sub

''- targets all down
Sub SoundDropTargetRelease()
	 PlaySound "droptarget_down",  (MaxSoundLevel * TargetSoundFactor)
End Sub 

''- targets all up
Sub SoundDropTargetReset()
	 PlaySound ("Drop_Target_Reset_" & Int(Rnd*6)+1), MaxSoundLevel' * (TargetSoundFactor + 0.1))
End Sub 


' - Rollover Sounds
Sub SoundRolloverHit()
	 PlaySound ("Rollover_" & Int(Rnd*4)+1), (MaxSoundLevel * RolloverSoundFactor)
End Sub


' Kicker sounds
Sub SoundSaucerHit(Scenario)
	Select Case scenario
		 ' no ball in kicker. Ex: table startup
		 Case 0: PlaySound "Saucer_Empty", (MaxSoundLevel * SaucerSoundFactor) 
		 ' kick ball out
		 Case 1: PlaySound "Saucer_Kick", (MaxSoundLevel * SaucerSoundFactor)  
		 ' ball enters kicker
		 Case 2: PlaySound ("Saucer_Enter_" & Int(Rnd*2)+1), (MaxSoundLevel * SaucerSoundFactor)
	End Select
End Sub


' - Gate Sounds
Sub SoundGateHit(GateType)
	 FinalSpeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)	'1200	'xBAM.Ball.HitSpeed
	 SoundFactor = FormatNumber((FinalSpeed / 1000) * (MaxSoundLevel * GateSoundFactor))
	 If FinalSpeed > 1100 Then
			 Select Case GateType
				 Case "Heavy": 
					 Playsound ("Gate_"& Int(Rnd*2)+1), SoundFactor
				 Case "Light":
					 Playsound ("Gate_FastTrigger_"& Int(Rnd*2)+1), SoundFactor
			 End Select
		 Else
			 Select Case GateType
				Case "Heavy": 
					 Playsound ("Gate_"& Int(Rnd*2)+1), GateSoundFactor
				 Case "Light":
					 Playsound ("Gate_FastTrigger_"& Int(Rnd*2)+1), GateSoundFactor
			 End Select
	 End If
End Sub


' - Knocker Sound 
Sub SoundKnocker()
	 PlaySound "Knocker_1", (MaxSoundLevel * KnockerSoundFactor)
End Sub


' - Spinner Sound  
Sub SoundSpinnerHit()
	PlaySound "Spinner", (MaxSoundLevel * SpinnerSoundFactor)
End Sub 




' Sound of the ball hitting the flipper
Sub SoundBallOnFlipperHit()
	 FinalSpeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)	'1200	'xBAM.Ball.HitSpeed
	 SoundFactor = FormatNumber((FinalSpeed / 1000) * (MaxSoundLevel * FlipperRubberSoundFactor))
	 
	 If (BallOnFlipperSoundEnabled = 1 and FinalSpeed > 300) then PlaySound ("Flipper_Rubber_" & Int(Rnd*7)+1), SoundFactor
End Sub 


' - Sound for ball coming down the outlane
Sub SoundBottomArchHit()
	 FinalSpeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)	'180	'xBAM.Ball.HitSpeed
	 SoundFactor = FormatNumber((FinalSpeed / 1000) * (MaxSoundLevel * BottomArchBallGuideSoundFactor))
	 If FinalSpeed >=200 Then PlaySound ("Apron_Hard_Hit_" & Int(Rnd*3)+1), SoundFactor
	 If FinalSpeed > 150 And FinalSpeed < 200 Then PlaySound ("Apron_Bounce_"& Int(Rnd*2)+1), SoundFactor
	 If FinalSpeed >= 100 And FinalSpeed <= 150 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySound ("Apron_Bounce_1"), SoundFactor
			Case 2 : PlaySound ("Apron_Bounce_Soft_1"), SoundFactor
		End Select
	 End If
	 If FinalSpeed < 150 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySound ("Apron_Bounce_Soft_1"), SoundFactor
			Case 2 : PlaySound ("Apron_Medium_3"), SoundFactor
		End Select
	 End if
End Sub


' - Sound for Metal hits
Sub SoundMetalHit()
	 FinalSpeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)	'500	'xBAM.Ball.HitSpeed
	 SoundFactor = FormatNumber((FinalSpeed / 1000) * (MaxSoundLevel * MetalImpactSoundFactor))
	 If FinalSpeed > 100 Then PlaySound ("Metal_Touch_" & Int(Rnd*13)+1), SoundFactor
End Sub


' - Sound for Walls hits
Sub SoundWallHit()
	 PlaySound ("Wall_Hit_" & Int(Rnd*9)+1), (MaxSoundLevel * WallImpactSoundFactor)
End Sub

