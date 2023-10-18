'************************************************************
'*															*
'*						Doctor Who							*
'*					   VPW Mod v1.0							*
'*				RothbauerW, Sixtoe, Apophis					*
'*															*
'*					Original Table by						*
'*			 Sliderpoint and Rothbauerw 2020				*
'*			 oooPLAYER1ooo & Unclewilly 2010				*
'*		with help from Bord, Fleep, ClarkKent, Fuzzel		*
'*				  Knorr, Thalamus, Sixtoe					*
'************************************************************

Option Explicit
Randomize
 
'************************************************************************
'* TABLE OPTIONS ********************************************************
'************************************************************************

'***********	VR Room and Cabinet Options   *****************

Const VRRoom = 0					'0 - VR Room off, 1 - Minimal Room, 2 - Ultra Minimal
Const CabinetMode = 0				'1 - hide the rails and scale the side panels higher for cabinet mode, 0 - cab mode off
Const SideLogo = 1					'0 - Side Logo Off, 1 - On

'***********	Outlane gap drain difficulty adjustment ***************** 'AXS

Const OutlaneDifficulty = 2			'1 = EASY, 2 = MEDIUM (Factory), 3 = HARD

'***********	Use staged flippers (dual leaf switches)	*******************************

Const StagedFlipperMod = 0			'0 = not staged, 1 - staged (dual leaf switches)

'----- Phsyics Mods -----
Const Rubberizer = 1				'1 - rothbauerw version, 2 - iaakki version
Const FlipperCoilRampupMode = 0		'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7		'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's

'----- General Sound Options -----
'VolumeDial is the actual global volume multiplier for the mechanical sounds.
'Values smaller than 1 will decrease mechanical sounds volume.
'Recommended values should be no greater than 1.
Const VolumeDial = 0.8
Const BallRollAmpFactor = 0			'0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)
Const RampRollAmpFactor = 0			'0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)

' Graphics Options
Const GI_Color = "Off"				'Mixed - Red - Blue - White - Off
Const SideWallFlashers = 1			'1 On / 0 Off
Const GISideWalls = 1				'1 On / 0 Off
Const Ball_Brightness = 0.95		'1 Full brightness / 0 Black

'************************************************************************
'* END OF TABLE OPTIONS *************************************************
'************************************************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const UseVPMColoredDMD = false 
Const UseVPMModSol = 1

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode

LoadVPM "01560000", "WPC.VBS", 3.26 
 
'********************
'Standard definitions
'********************

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Set GICallback2 = GetRef("UpdateGI")

Const cSingleLFlip = True
Const cSingleRFlip = False

' Standard Sounds
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

'******************************************************
' 					TABLE INIT
'******************************************************

'Rom Name
Const cGameName = "dw_l2"

' using table width and height in script slows down the performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

Dim mMinipf,plungerIM
Dim DWBall1, DWBall2, DWBall3
Dim PFPos, xx, DayNight
Dim ballbrightness

'Collections to Arrays
Dim GIGArr2(20), GIGArr3(20), GIGArr4(20)
Dim GIBGArr0(20), GIBGArr1(20), GIBGArr2(20), GIBGArr3(20), GIBGArr4(20)
Dim GIFArr2(20), GIFArr3(20), GIFArr4(20)
Dim GArrCnt2, GArrCnt3, GArrCnt4
Dim BGArrCnt0, BGArrCnt1, BGArrCnt2, BGArrCnt3, BGArrCnt4
Dim FArrCnt2, FArrCnt3, FArrCnt4
Dim FLArr2(20), FLArr3(20), FLArr4(20)
Dim FLArrCnt2, FLArrCnt3, FLArrCnt4
Dim MPFArr(20), MPFArr2(20), MPFLArr(20)
Dim MPFArrCnt, MPFArrCnt2, MPFLArrCnt

Dim TempArr

Sub Table1_Init
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Doctor Who" & vbNewLine & "VPW Mod"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 0
		.dip(0)=&h00  'Set to usa
		On Error Resume Next
		.Run 

		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	Controller.Switch(22) = 1 'close coin door
	Controller.Switch(24) = 1 'always closed
	Controller.Switch(82) = 1 'pfglass switch

	'************  Trough init
	Set DWBall1 = sw25.CreateSizedBallWithMass (Ballsize/2, BallMass)
	Set DWBall2 = sw26.CreateSizedBallWithMass (Ballsize/2, BallMass)
	Set DWBall3 = sw27.CreateSizedBallWithMass (Ballsize/2, BallMass)

	Controller.Switch(25) = 1
	Controller.Switch(26) = 1
	Controller.Switch(27) = 1

	ballbrightness = CInt(Ball_Brightness*255)
	if ballbrightness > 255 Then ballbrightness=255
	if ballbrightness < 0 Then ballbrightness=0

	DWBall1.color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
	DWBall2.color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
	DWBall3.color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)

	' Impulse Plunger
	Const IMPowerSetting = 45
	Const IMTime = 0.7
	Set plungerIM = New cvpmImpulseP
	With plungerIM
		.InitImpulseP swplunger, IMPowerSetting, IMTime
		'.InitExitSnd SoundFX("Solenoid",DOFContactors), SoundFX("plunger",DOFContactors)
		.CreateEvents "plungerIM"
	End With

	set mMinipf=new cvpmmech
	with mMinipf
		.sol1=28
		.sol2=27
		.mtype=vpmMechOneDirSol+vpmmechlinear
		.length=270
		.steps=360
		.callback=getref("UpdateMiniPF")
		.start
	end with

	'Other Init
	sw71.isDropped = 1
	sw72.isDropped = 1
	sw73.isDropped = 1
	sw74.isDropped = 1
	sw75.isDropped = 1
	sw71a.isDropped = 1
	sw72a.isDropped = 1
	sw73a.isDropped = 1
	sw74a.isDropped = 1
	sw75a.isDropped = 1

	DayNight = table1.NightDay
	Intensity 'sets GI brightness depending on day/night slider settings

	If not DesktopMode then
		l23bg.visible=0
		l36bg.visible=0
		l48bg.visible=0
		l67bg.visible=0
		l71bg.visible=0
		l72bg.visible=0
		l82bg.visible=0
	End If

	'Init switches
	Controller.Switch(22) = 1 'close coin door
	Controller.Switch(24) = 1 'always closed
	Controller.Switch(82) = 1 'pfglass switch

	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 4
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper4, LeftSling, RightSling)  

	TDDown.collidable = False
	TDUp.collidable = True

	sw76wall.collidable = False
	sw77wall.collidable  = False

	'Graphic Variables
	If GI_Color = "Mixed" then
		for each xx in GIG2:xx.Color = RGB(255, 0, 0):next
		for each xx in GIG3:xx.Color = RGB(255, 255, 0):next
		for each xx in GIG4:xx.Color = RGB(0, 0, 255):next
		for each xx in GIG2:xx.ColorFull = RGB(255, 0, 0):next
		for each xx in GIG3:xx.ColorFull = RGB(255, 255, 0):next
		for each xx in GIG4:xx.ColorFull = RGB(0, 0, 255):next
		for each xx in GIF2:xx.Color = RGB(255, 255, 128):Next
		for each xx in GIF3:xx.Color = RGB(255, 255, 0):Next
		for each xx in GIF4:xx.Color = RGB(0, 0, 255):Next
	End If

	If GI_Color = "Red" then
		for each xx in GIG2:xx.Color = RGB(255, 0, 0):next
		for each xx in GIG3:xx.Color = RGB(255, 0, 0):next
		for each xx in GIG4:xx.Color = RGB(255, 0, 0):next
		for each xx in GIG2:xx.ColorFull = RGB(255, 0, 0):next
		for each xx in GIG3:xx.ColorFull = RGB(255, 0, 0):next
		for each xx in GIG4:xx.ColorFull = RGB(255, 0, 0):next
		for each xx in GIF2:xx.Color = RGB(255, 0, 0):next
		for each xx in GIF3:xx.Color = RGB(255, 0, 0):next
		for each xx in GIF4:xx.Color = RGB(255, 0, 0):next
	End If

	If GI_Color = "White" then
		for each xx in GIG2:xx.Color = RGB(255, 255, 0):next
		for each xx in GIG3:xx.Color = RGB(255, 255, 0):next
		for each xx in GIG4:xx.Color = RGB(255, 255, 0):next
		for each xx in GIF2:xx.Color = RGB(255, 255, 128):Next
		for each xx in GIF3:xx.Color = RGB(255, 255, 128):Next
		for each xx in GIF4:xx.Color = RGB(255, 255, 128):Next
	End If

	If GI_Color = "Blue" then
		for each xx in GIG2:xx.Color = RGB(0, 0, 255):next
		for each xx in GIG3:xx.Color = RGB(0, 0, 255):next
		for each xx in GIG4:xx.Color = RGB(0, 0, 255):next
		for each xx in GIG2:xx.ColorFull = RGB(0, 0, 255):next
		for each xx in GIG3:xx.ColorFull = RGB(0, 0, 255):next
		for each xx in GIG4:xx.ColorFull = RGB(0, 0, 255):next
		for each xx in GIF2:xx.Color = RGB(0, 0, 255):Next
		for each xx in GIF3:xx.Color = RGB(0, 0, 255):Next
		for each xx in GIF4:xx.Color = RGB(0, 0, 255):Next
	End If

	xx = 0
	For Each TempArr in GIG2 
		Set GIGArr2(xx) = TempArr
		xx = xx + 1
	Next
	GArrCnt2 = xx

	xx = 0
	For Each TempArr in GIG3
		Set GIGArr3(xx) = TempArr
		xx = xx + 1
	Next
	GArrCnt3 = xx

	xx = 0
	For Each TempArr in GIG4 
		Set GIGArr4(xx) = TempArr
		xx = xx + 1
	Next
	GArrCnt4 = xx 

	xx = 0
	For Each TempArr in GIBG0 
		Set GIBGArr0(xx) = TempArr
		xx = xx + 1
	Next
	BGArrCnt0 = xx

	xx = 0
	For Each TempArr in GIBG1 
		Set GIBGArr1(xx) = TempArr
		xx = xx + 1
	Next
	BGArrCnt1 = xx

	xx = 0
	For Each TempArr in GIBG2 
		Set GIBGArr2(xx) = TempArr
		xx = xx + 1
	Next
	BGArrCnt2 = xx

	xx = 0
	For Each TempArr in GIBG3 
		Set GIBGArr3(xx) = TempArr
		xx = xx + 1
	Next
	BGArrCnt3 = xx

	xx = 0
	For Each TempArr in GIBG4 
		Set GIBGArr4(xx) = TempArr
		xx = xx + 1
	Next
	BGArrCnt4 = xx

	xx = 0
	For Each TempArr in GIF2 
		Set GIFArr2(xx) = TempArr
		xx = xx + 1
	Next
	FArrCnt2 = xx

	xx = 0
	For Each TempArr in GIF3 
		Set GIFArr3(xx) = TempArr
		xx = xx + 1
	Next
	FArrCnt3 = xx

	xx = 0
	For Each TempArr in GIF4 
		Set GIFArr4(xx) = TempArr
		xx = xx + 1
	Next
	FArrCnt4 = xx

	xx = 0
	For Each TempArr in FlasherTest02 
		Set FLArr2(xx) = TempArr
		xx = xx + 1
	Next
	FLArrCnt2 = xx

	xx = 0
	For Each TempArr in FlasherTest03 
		Set FLArr3(xx) = TempArr
		xx = xx + 1
	Next
	FLArrCnt3 = xx

	xx = 0
	For Each TempArr in FlasherTest04 
		Set FLArr4(xx) = TempArr
		xx = xx + 1
	Next
	FLArrCnt4 = xx

	xx = 0
	For Each TempArr in MiniPF
		Set MPFArr(xx) = TempArr
		xx = xx + 1
	Next
	MPFArrCnt = xx

	xx = 0
	For Each TempArr in MiniPF2
		Set MPFArr2(xx) = TempArr
		xx = xx + 1
	Next
	MPFArrCnt2 = xx

	xx = 0
	For Each TempArr in MiniPFLights
		Set MPFLArr(xx) = TempArr
		xx = xx + 1
	Next
	MPFLArrCnt = xx

	SetBackGlass
	Flash06 0
	Flash08 0
	Flash17 0
	Flash18 0
	Flash19 0
	Flash20 0
	Flash21 0
	who_h 0	
	who_o 0
	Flash24 0

End Sub
	
PFPos=-1

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub 
'Sub Table1_exit:Controller.Stop: End Sub

'*******	TABLE OPTIONS SETUP	***********************************

If OutlaneDifficulty = 1 Then  
	OutlaneLeft1.Collidable = True :OutlaneLeft1.Visible = True
	OutlaneLeft2.Collidable = False:OutlaneLeft2.Visible  = False
	OutlaneLeft3.Collidable = False:OutlaneLeft3.Visible = False
	OutlaneLeft1a.Collidable = True 
	OutlaneLeft2a.Collidable = False
	OutlaneLeft3a.Collidable = False


	OutlaneRight1.Collidable = True:OutlaneRight1.Visible  = True
	OutlaneRight2.Collidable = False:OutlaneRight2.Visible  = False
	OutlaneRight3.Collidable = False:OutlaneRight3.Visible  = False
	OutlaneRight1a.Collidable = True
	OutlaneRight2a.Collidable = False
	OutlaneRight3a.Collidable = False
End If

If OutlaneDifficulty = 2 Then 
	OutlanePegL.transY = -12.5
	OutLanePegR.transY = -12.5

	OutlaneLeft1.Collidable = False:OutlaneLeft1.Visible  = False
	OutlaneLeft2.Collidable = True:OutlaneLeft2.Visible  = True
	OutlaneLeft3.Collidable = False:OutlaneLeft3.Visible  = False
	OutlaneLeft1a.Collidable = False
	OutlaneLeft2a.Collidable = True
	OutlaneLeft3a.Collidable = False

	OutlaneRight1.Collidable = False:OutlaneRight1.Visible  = False
	OutlaneRight2.Collidable = True:OutlaneRight2.Visible  = True
	OutlaneRight3.Collidable = False:OutlaneRight3.Visible = False
	OutlaneRight1a.Collidable = False
	OutlaneRight2a.Collidable = True
	OutlaneRight3a.Collidable = False
End If

If OutlaneDifficulty = 3 Then 
	OutlanePegL.transY = -25
	OutLanePegR.transY = -25

	OutlaneLeft1.Collidable = False:OutlaneLeft1.Visible = False
	OutlaneLeft2.Collidable = False:OutlaneLeft2.Visible = False
	OutlaneLeft3.Collidable = True:OutlaneLeft3.Visible  = True
	OutlaneLeft1a.Collidable = False 
	OutlaneLeft2a.Collidable = False
	OutlaneLeft3a.Collidable = True

	OutlaneRight1.Collidable = False:OutlaneRight1.Visible  = False
	OutlaneRight2.Collidable = False:OutlaneRight2.Visible  = False
	OutlaneRight3.Collidable = True:OutlaneRight3.Visible  = True
	OutlaneRight1a.Collidable = False
	OutlaneRight2a.Collidable = False
	OutlaneRight3a.Collidable = True
End If

If StagedFlipperMod = 1 Then
	keyStagedFlipperL = KeyUpperLeft
	keyStagedFlipperR = KeyUpperRight
End If

'*******	Set Up Backglass	***********************

Sub SetBackglass()
	Dim obj

	For Each obj In BackGlass
		obj.x = obj.x
		obj.height = - obj.y + 475
		obj.y = 71
	Next
End Sub

'******************************************************
' 						KEYS
'******************************************************

Sub Table1_KeyDown(ByVal Keycode)
	if keycode = plungerkey then controller.switch(34)=True
	
	If keycode = PlungerKey Then 
		LaunchButton.Y = 642.4715 - 4
		Flasher7.Y = 2329.471 - 4
	End If

	If keycode = StartGameKey Then soundStartButton()

	If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()
 
	If keycode = LeftFlipperKey Then 
		FlipperActivate LeftFlipper, LFPress
		If StagedFlipperMod <> 1 Then 
			FlipperActivate LeftFlipper2, LFPress1
		End If
	End If
	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress 
	End If
	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperLeft Then 
			FlipperActivate LeftFlipper2, LFPress1
		End If
	End If

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySoundAtLevelStatic ("Coin_In_1"), CoinSoundLevel, sw28
			Case 1: PlaySoundAtLevelStatic ("Coin_In_2"), CoinSoundLevel, sw28
			Case 2: PlaySoundAtLevelStatic ("Coin_In_3"), CoinSoundLevel, sw28
		End Select
	End If


	If vpmKeyDown(keycode) Then Exit Sub 
End Sub
   
Sub Table1_KeyUp(ByVal Keycode)
	If keycode = plungerkey then controller.switch(34)=false

	If keycode = PlungerKey Then 
		LaunchButton.Y = 642.4715
		Flasher7.Y = 2329.471
	End if

	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		If StagedFlipperMod <> 1 Then 
			FlipperDeActivate LeftFlipper2, LFPress1
		End If
	End If
	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress 
	End If
	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperLeft Then 
			FlipperDeActivate LeftFlipper2, LFPress1
		End If
	End If
	If vpmKeyUp(keycode) Then Exit Sub
	If keyuphandler(keycode) Then Exit Sub
End Sub
 
'************************************************************************
'						 SOLENOIDS
'************************************************************************

Solcallback(1)="SolTrapDoor"
Solcallback(2)="SolAutoFire"
Solcallback(3)="TardisExit"
Solcallback(4)="solmpfl" 
Solcallback(5)="solmpfr"
SolModcallback(6)= "Flash06"
Solcallback(7)="SolKnocker"
SolModCallback(8)= "Flash08"'doctor 3 flasher, in backbox
'solcallback(11)="bpr1"
'solcallback(12)="bpr2"
'solcallback(13)="bpr3"
Solcallback(15)="SolOutHole"
Solcallback(16)="SolBallRelease"
SolModcallback(17)="Flash17"
SolModcallback(18)="Flash18"
SolModcallback(19)="Flash19"
SolModcallback(20)="flash20"
SolModcallback(21)="Flash21"
SolModCallback(22)= "who_h"
SolModCallback(23)= "who_o"
SolModcallback(24)="Flash24"
SolCallback(28)="TEOnOff"

'******************************************************
'				TRAP DOOR
'******************************************************

Dim TDDir

sub soltrapdoor(Enabled)
 	if enabled then 		
		TDDown.collidable = True
		TDUp.collidable = False
		TDDir = 5
		TDUpTimer.Enabled = 1		
 	else
		TDDown.collidable = False
		TDUp.collidable = True
		TDDir = -5 
		TDUpTimer.Enabled = 1
	end if
  end sub

Sub TDUP_Hit()
	activeball.velz = -abs(activeball.vely)
	activeball.velx = 0
	if activeball.x > 58 then activeball.x = 58
	WireRampOff
End Sub

Sub TDUpTimer_timer
		TD.RotX = TD.RotX + TDDir
		If TD.RotX = -55 Then
			controller.switch(57)= False
			Me.Enabled = 0
			PlaySoundAtVol SoundFX("FlapOpen", DOFContactors), 0.25, TD
		ElseIf TD.RotX = 0 Then
			controller.switch(57)= True
			Me.Enabled = 0
			PlaySoundAtVol SoundFX("FlapClos", DOFContactors), 0.25, TD
		End If
End Sub


'******************************************************
'				PLUNGER
'******************************************************

Sub solAutofire(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
		If CheckBallInPlunger(DWBall1) or CheckBallInPlunger(DWBall2) or CheckBallInPlunger(DWBall3) Then
			SoundAutoPlunger 1
		Else
			SoundAutoPlunger 0
		End If
	End If
End Sub

Function CheckBallInPlunger(ball)
	If ball.x > 870 and ball.y > 1875 Then
		CheckBallInPlunger = True
	Else
		CheckBallInPlunger = False
	End If	
End Function

'******************************************************
'					KNOCKER
'******************************************************

Sub SolKnocker(enabled)
	If enabled Then
		KnockerSolenoid()
	End If
End Sub

'******************************************************
'					VUK
'******************************************************

  
Sub TardisExit(enabled)
	If Enabled Then
		TardisEntrance.KickZ 180, 35, 92, 0
		SoundSaucerKick 1, TardisEntrance		
		Controller.Switch (31) = 0
	End If
End Sub

'******************************************************
'					MINI PLAYFIELD KICKERS
'******************************************************

Sub solmpfl(enabled)
 	If enabled then
		sw76wall.collidable = False
		sw76.kick 171 + (Rnd * 2), 8 + (Rnd*2)
		Kickerplunger1.Z = -30
		If Not IsEmpty (LockedBall1) Then
			SoundSaucerKick 1, sw76
			vpmtimer.addtimer 50, "RandomSoundDelayedBallDropFromKicker sw76 '"
			LockedBall1 = Empty
		Else
			SoundSaucerKick 0, sw76
		End If
	Else
		Kickerplunger1.Z = -50
 	End If
End Sub
 
Sub solmpfr(enabled)
	If enabled then
		sw77wall.collidable  = False
		sw77.kick 189 - (Rnd * 2), 8 + (Rnd*2)
		Kickerplunger2.Z = -30
		If Not IsEmpty (LockedBall2) Then
			SoundSaucerKick 1, sw77
			vpmtimer.addtimer 50, "RandomSoundDelayedBallDropFromKicker sw77 '"
			LockedBall2 = Empty
		Else
			SoundSaucerKick 0, sw77
		End If
	Else
		Kickerplunger2.Z = -50
	End If
End Sub

'******************************************************
'					FLASHERS
'******************************************************

Sub Flash06(Level)
	If FL6bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn sw28
	If FL6bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff sw28

	If Level > 0 Then
		FL06.IntensityScale = (Level / 2.55) / 100
		FL06b.IntensityScale = (Level / 2.55) / 100
		FL06h.IntensityScale = (Level / 2.55) / 100
		FL06.State = 1
		FL06b.State = 1
		FL06h.State = 1
	Else
		FL06.State = 0
		FL06b.State = 0
		FL06h.State = 0
	End If
	FL6bg.IntensityScale = (Level / 2.55) / 100
	DisableLighting pFL06, 300, (Level / 2.55) / 100
End Sub


Sub Flash08(Level)
	If FL8bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn KnockerPosition
	If FL8bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff KnockerPosition

	FL8bg.IntensityScale = (Level / 2.55) / 100
End Sub

Sub Flash17(Level)
	If FL17bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn sw78
	If FL17bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff sw78

	If Level > 0 Then
		FL17.IntensityScale = (Level / 2.55) / 100
		FL17.State = 1
		TEFlashP.image = "TopFlasherRedOn"		
	Else
		FL17.State = 0
		TEFlashP.image = "TopFlasherRed"
	End If
	FL17bg.IntensityScale = (Level / 2.55) / 100
	FL17f.IntensityScale = (Level / 2.55) / 100
	TEFlashP.blenddisablelighting = (Level / 2.55) / 100 * 0.6
End Sub

Sub Flash18(Level)
	If FL18bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn sw78
	If FL18bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff sw78

	If Level > 0 Then
		FL18.IntensityScale = (Level / 2.55) / 100
		FL18.State = 1
	Else
		FL18.State = 0
	End If
	FL18bg.IntensityScale = (Level / 2.55) / 100
	DisableLighting pFL18, 200, (Level / 2.55) / 100
End Sub

Sub Flash19(Level)
	If FL19bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn sw78
	If FL19bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff sw78

	If Level > 0 Then
		FL19.IntensityScale = (Level / 2.55) / 100

		FL19.State = 1
	Else
		FL19.State = 0
	End If
	FL19bg.IntensityScale = (Level / 2.55) / 100
	DisableLighting pFL19, 200, (Level / 2.55) / 100
End Sub

Sub Flash20(Level)
	If FL20bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn Bumper1
	If FL20bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff Bumper1

	If Level > 0 Then
		FL20.Opacity = (Level / 2.55)
		FL20.visible = 1
	 else
		FL20.Visible = 0
	 end if
	FL20bg.IntensityScale = (Level / 2.55) / 100
end sub

Sub Flash21(Level)
	If FL21bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn LeftMiddle
	If FL21bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff LeftMiddle

	If Level > 0 Then	
		FL21.IntensityScale = (Level / 2.55) / 100
		FL21b.IntensityScale = (Level / 2.55) / 100
		FL21.State = 1
		FL21b.State = 1
		RepairFlash.visible = 1
		If SideWallFlashers = 1 then
			FL21c.visible = 1
			FL21d.visible = 1
		End If
	Else
		FL21.State = 0
		FL21b.State = 0
		FL21c.visible = 0
		FL21d.visible = 0
		RepairFlash.visible = 0
	End If
	FL21bg.IntensityScale = (Level / 2.55) / 100
End Sub

Sub who_h(Level)
	If FL22bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn LeftMiddle
	If FL22bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff LeftMiddle

	If Level > 0 Then
		FL22.IntensityScale = (Level / 2.55) / 100
		FL22h.IntensityScale = (Level / 2.55) / 100
		FL22.State = 1
		FL22h.State = 1
	 else
		FL22.State = 0
		FL22h.State = 0
	end if
	FL22bg.IntensityScale = (Level / 2.55) / 100
	DisableLighting pFL22, 200, (Level / 2.55) / 100
End Sub
 
Sub who_o(Level)
	If FL23bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn sw18
	If FL23bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff sw18

	If Level > 0 Then
		FL23.IntensityScale = (Level / 2.55) / 100
		FL23h.IntensityScale = (Level / 2.55) / 100
		FL23.State = 1
		FL23h.State = 1
	 else
		FL23.State = 0
		FL23h.State = 0
	 end if
	FL23bg.IntensityScale = (Level / 2.55) / 100
	DisableLighting pFL23, 200, (Level / 2.55) / 100
End Sub

Sub Flash24(Level)
	If FL24bg.IntensityScale = 0 and Level > 0 Then SoundFlashRelayOn sw43
	If FL24bg.IntensityScale > 0 and Level = 0 Then SoundFlashRelayOff sw43

	If Level > 0 Then
		FL24.IntensityScale = (Level / 2.55) / 100
		FL24.State = 1
		EscapeFlash.visible = 1
	Else
		FL24.State = 0
		EscapeFlash.visible = 0
	End If
	FL24bg.IntensityScale = (Level / 2.55) / 100
End Sub

'******************************************************
'			TROUGH BASED ON FOZZY'S
'******************************************************

Sub sw25_Hit():Controller.Switch(25) = 1:UpdateTrough: End Sub
Sub sw25_UnHit():Controller.Switch(25) = 0:UpdateTrough:End Sub
Sub sw26_Hit():Controller.Switch(26) = 1:UpdateTrough:End Sub
Sub sw26_UnHit():Controller.Switch(26) = 0:UpdateTrough:End Sub
Sub sw27_Hit():Controller.Switch(27) = 1:UpdateTrough:End Sub
Sub sw27_UnHit():Controller.Switch(27) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw25.BallCntOver = 0 Then sw26.kick 65, 10
	If sw26.BallCntOver = 0 Then sw27.kick 65, 10	
	Me.Enabled = 0
End Sub

'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub sw28_Hit()
	RandomSoundDrain sw28
	Controller.Switch(28) = 1
	UpdateTrough
End Sub

Sub sw28_UnHit()
	Controller.Switch(28) = 0
End Sub

Sub SolBallRelease(enabled)
	If enabled Then 
		RandomSoundBallRelease sw25
		sw25.kick 65, 10
		UpdateTrough
	End If
End Sub

Sub SolOutHole(enabled)
	If enabled Then 
		sw28.kick 65, 10
		UpdateTrough
	End If
End Sub

'RandomSoundBottomArchBallGuide - Soft Bounces
Sub Wall014_Hit() : RandomSoundBottomArchBallGuide() : End Sub
Sub Wall015_Hit() : RandomSoundBottomArchBallGuide() : End Sub

'RandomSoundBottomArchBallGuide - Hard Bounces
Sub Ramp021_Hit() : RandomSoundBottomArchBallGuideHardHit() : End Sub
Sub Ramp022_Hit() : RandomSoundBottomArchBallGuideHardHit() : End Sub

'RandomSoundFlipperBallGuide
Sub bgleft_Hit() : RandomSoundFlipperBallGuide() : End Sub
Sub bgright_Hit() : RandomSoundFlipperBallGuide() : End Sub

'******************************************************
'				SLINGSHOTS
'******************************************************

Dim LStep, RStep

Sub LeftSling_Slingshot
	RandomSoundSlingshotLeft()	
	vpmTimer.PulseSw 15
	SlingL1.Visible = 0
	SlingL3.Visible = 1
	SlingL.TransZ = -10
	LStep = 0
	Me.TimerEnabled = 1
End Sub

Sub LeftSling_Timer
    Select Case LStep
	Case 0:SLingL2.Visible = 1:SLingL3.Visible = 0:slingL.TransZ = -20
        Case 3:SLingL2.Visible = 0:SLingL3.Visible = 1:slingL.TransZ = -10
        Case 4:SLingL3.Visible = 0:SLingL1.Visible = 1:slingL.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSling_Slingshot
	RandomSoundSlingshotRight()	
	vpmTimer.PulseSw 16
	SlingR1.Visible = 0
	SlingR3.Visible = 1
	slingR.TransZ = -10
	RStep = 0
	Me.TimerEnabled = 1
End Sub

Sub RightSling_Timer
    Select Case RStep
	Case 0:SlingR2.Visible = 1:SlingR3.Visible = 0:SlingR.TransZ = -20
        Case 3:SlingR2.Visible = 0:SlingR3.Visible = 1:SlingR.TransZ = -10
        Case 4:SlingR3.Visible = 0:SlingR1.Visible = 1:SlingR.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'******************************************************
'				BUMPERS
'******************************************************

Sub Bumper2_Hit():vpmTimer.PulseSw 61:RandomSoundBumperB():End Sub
Sub Bumper1_Hit():vpmTimer.PulseSw 62:RandomSoundBumperA():End Sub
Sub Bumper4_Hit():vpmTimer.PulseSw 63:RandomSoundBumperC():End Sub

 'Ball events
sub sw76_hit():controller.switch(76) = 1
	SoundSaucerLock
	Set LockedBall1= ActiveBall
	vpmtimer.addtimer 10, "LockedBall1.Z = (ZPosTrans + 25)'"
	sw76wall.collidable = True
end sub 
Sub sw76_unhit:controller.switch(76) = 0:end Sub

Sub sw76wall_hit()
	OnBallBallCollision activeball, Empty, BallSpeed(Activeball)*1.5	
End Sub

sub sw77_hit()
	controller.switch(77) = 1
	SoundSaucerLock
	Set LockedBall2= ActiveBall
	vpmtimer.addtimer 10, "LockedBall2.Z = (ZPosTrans + 25)'"
	sw77wall.collidable  = True
end sub
Sub sw77_unhit:controller.Switch(77) = 0:end sub

Sub sw77wall_hit()
	OnBallBallCollision activeball, Empty, BallSpeed(Activeball)*1.5
End Sub


Sub TardisEntrance_hit:Controller.Switch(31) = 1:end sub
Sub ShooterLane_Hit:Controller.Switch(17)=1:End Sub
Sub ShooterLane_Unhit:Controller.Switch(17)=0:End Sub

  'MiniPF Door Switches
sub sw68s_hit:vpmTimer.PulseSw 68:RandomSoundScoopEntry:gate68p.Rotx = -42: End Sub
sub sw38s_Hit:vpmTimer.PulseSw 38:RandomSoundScoopEntry:gate38p.Rotx = -42: End Sub
sub sw88s_Hit:vpmTimer.PulseSw 88:RandomSoundScoopEntry:gate88p.Rotx = -42: End Sub

sub sw68s_unhit:RandomSoundSubway: End Sub
sub sw38s_unHit:RandomSoundSubway: End Sub
sub sw88s_unHit:RandomSoundSubway: End Sub


 'MiniPF Standup
Sub sw78_Hit():STHit 78:End Sub
 
 'MiniPf Buttons
Sub sw71_Hit:STHit 71:End Sub
Sub sw72_Hit:STHit 72:End Sub
Sub sw73_Hit:STHit 73:End Sub
Sub sw74_Hit:STHit 74:End Sub
Sub sw75_Hit:STHit 75:End Sub

 'ramp gates
Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub
Sub sw36_Hit:vpmTimer.PulseSw 36:SoundRampGate:End Sub
Sub gate3_Hit:vpmTimer.PulseSw 37:SoundPlayfieldGate:End Sub
Sub gate5_Hit:vpmTimer.PulseSw 35:SoundPlayfieldGate:End Sub
 
'Playfield Gates
Sub Gate1_hit():SoundHeavyGate:End Sub
Sub Gate4_hit():SoundHeavyGate:End Sub

 ' Activate transmat
Sub sw58_Hit:STHit 58:End Sub
Sub sw58a_Hit:TargetBouncer activeball, 1:End Sub
 

 ' Escape targets
Sub sw41_Hit:STHit 41:End Sub
Sub sw42_Hit:STHit 42:End Sub
Sub sw43_Hit:STHit 43:End Sub
Sub sw44_Hit:STHit 44:End Sub
Sub sw45_Hit:STHit 45:End Sub
Sub sw46_Hit:STHit 46:End Sub

'Sub sw41a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw42a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw43a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw44a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw45a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw46a_Hit:TargetBouncer activeball, 1:End Sub


 'repair targets
Sub sw51_Hit:STHit 51:End Sub
Sub sw52_Hit:STHit 52:End Sub
Sub sw53_Hit:STHit 53:End Sub
Sub sw54_Hit:STHit 54:End Sub
Sub sw55_Hit:STHit 55:End Sub
Sub sw56_Hit:STHit 56:End Sub

'Sub sw51a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw52a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw53a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw54a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw55a_Hit:TargetBouncer activeball, 1:End Sub
'Sub sw56a_Hit:TargetBouncer activeball, 1:End Sub
 
 ' lane rollovers
Sub RightOutlane_Hit:Controller.Switch(67) = 1:RandomSoundRollover():End Sub
Sub RightOutlane_UnHit:Controller.Switch(67) = 0:End Sub
Sub RightInlane_Hit:Controller.Switch(66) = 1:RandomSoundRollover():End Sub
Sub RightInlane_UnHit:Controller.Switch(66) = 0:End Sub
Sub LeftOutlane_Hit:Controller.Switch(64) = 1:RandomSoundRollover():End Sub
Sub LeftOutlane_UnHit:Controller.Switch(64) = 0:End Sub
Sub LeftInlane_Hit:Controller.Switch(65) = 1:RandomSoundRollover():End Sub
Sub LeftInlane_UnHit:Controller.Switch(65) = 0:End Sub
Sub LeftMiddle_Hit:Controller.Switch(47) = 1:RandomSoundRollover():End Sub
Sub LeftMiddle_UnHit:Controller.Switch(47) = 0:End Sub
   
 'hidden rollovers
Sub sw18_Hit:vpmTimer.PulseSw 18:RandomSoundRollover():End Sub
Sub sw48_Hit:vpmTimer.PulseSw 48:RandomSoundRollover():End Sub


'******************************************************
'		STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate, m_target

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get Target(): Target = m_target: End Property
  Public Property Let Target(input): m_target = input: End Property

  Public default Function init(primary, prim, sw, animate, target)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_target = target

    Set Init = Me
  End Function
End Class


'Define a variable for each stand-up target
Dim ST41, ST42, ST43, ST44, ST45, ST46, ST51, ST52, ST53, ST54, ST55, ST56, ST58, ST71, ST72, ST73, ST74, ST75, ST78

Set ST41 = (new StandupTarget)(sw41, psw41,41, 0, 79)
Set ST42 = (new StandupTarget)(sw42, psw42,42, 0, 79)
Set ST43 = (new StandupTarget)(sw43, psw43,43, 0, 79)
Set ST44 = (new StandupTarget)(sw44, psw44,44, 0, 79)
Set ST45 = (new StandupTarget)(sw45, psw45,45, 0, 79)
Set ST46 = (new StandupTarget)(sw46, psw46,46, 0, 79)

Set ST51 = (new StandupTarget)(sw51, psw51,51, 0, -88)
Set ST52 = (new StandupTarget)(sw52, psw52,52, 0, -88)
Set ST53 = (new StandupTarget)(sw53, psw53,53, 0, -88)
Set ST54 = (new StandupTarget)(sw54, psw54,54, 0, -88)
Set ST55 = (new StandupTarget)(sw55, psw55,55, 0, -88)
Set ST56 = (new StandupTarget)(sw56, psw56,56, 0, -88)
Set ST58 = (new StandupTarget)(sw58, psw58,58, 0, 84)

Set ST71 = (new StandupTarget)(sw71, sw71p,71, 0, 0)
Set ST72 = (new StandupTarget)(sw72, sw72p,72, 0, 0)
Set ST73 = (new StandupTarget)(sw73, sw73p,73, 0, 0)
Set ST74 = (new StandupTarget)(sw74, sw74p,74, 0, 0)
Set ST75 = (new StandupTarget)(sw75, sw75p,75, 0, 0)
Set ST78 = (new StandupTarget)(sw78, ANIM_target_TE,78, 0, 0)


Dim STArray
STArray = Array(ST41, ST42, ST43, ST44, ST45, ST46, ST51, ST52, ST53, ST54, ST55, ST56, ST58, ST71, ST72, ST73, ST74, ST75, ST78)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  0.75 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 15 			'max vp units target moves when hit
Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

	PlayTargetSound
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary,STArray(i).target)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).target, STMass
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
Function STCheckHit(aBall, target, orientation) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (orientation - 90) * 3.1416 / 180	
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
	Elseif orientation = 0 Then 'specific to dr who, TE buttons should always trigger a hit.
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

'******************************************************
'				NFOZZY'S FLIPPERS
'******************************************************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sULFlipper) = "SolULFlipper"
 
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire 'LeftFlipper.RotateToEnd

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			'Play partial flip sound
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			'Play full flip sound
			'LeftFlipper.RotateToEnd
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

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			'Play partial flip sound
			RandomSoundReflipUpRight RightFlipper
		Else 
			'Play full flip sound
			'RightFlipper.RotateToEnd
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

Sub SolULFlipper(Enabled)
	If Enabled Then
		LeftFlipper2.RotateToEnd

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			'Play partial flip sound
			RandomSoundReflipUpLeft LeftFlipper2
		Else 
			'Play full flip sound
			'LeftFlipper.RotateToEnd
			SoundFlipperUpAttackLeft LeftFlipper2
			RandomSoundFlipperUpLeft LeftFlipper2
		End If
	Else
		LeftFlipper2.RotateToStart
	
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper2
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks LeftFlipper2, LFPress1, LFCount1, LFEndAngle1, LFState1
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
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


dim LFPress, LFPress1, RFPress, LFCount, LFCount1, RFCount
dim LFState, RFState, LFState1
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle, LFEndAngle1

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 	'1.0 FEOST
Const EOSAnew = 1		'0.2
Const EOSRampup = 0	'0.5 
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn2 = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
LFEndAngle1 = Leftflipper2.endangle
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

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub LeftFlipper2_Collide(parm)	
	LeftFlipperCollide parm
End Sub


Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************

'******************************************************
'				FLIPPER AND RUBBER CORRECTION
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

	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.1, 	1.07
	addpt "Velocity", 2, 0.2, 	1.15
	addpt "Velocity", 3, 0.3, 	1.25
	addpt "Velocity", 4, 0.41, 1.05
	addpt "Velocity", 5, 0.65, 	1.0'0.982
	addpt "Velocity", 6, 0.702, 0.968
	addpt "Velocity", 7, 0.95,  0.968
	addpt "Velocity", 8, 1.03, 	0.945

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5.5
	AddPt "Polarity", 2, 0.4, -5.5
	AddPt "Polarity", 3, 0.8, -5.5	
	AddPt "Polarity", 4, 0.85, -5.25
	AddPt "Polarity", 5, 0.9, -4.25
	AddPt "Polarity", 6, 0.95, -3.75
	AddPt "Polarity", 7, 1, -3.25
	AddPt "Polarity", 8, 1.05, -2.25
	AddPt "Polarity", 9, 1.1, -1.5
	AddPt "Polarity", 10, 1.15, -1
	AddPt "Polarity", 11, 1.2, -0.5
	AddPt "Polarity", 12, 1.25, 0
	AddPt "Polarity", 13, 1.3, 0

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
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
End Sub

 
'**********************************************************
'     MiniPF Animation 
 '**********************************************************
Dim ZPos, MPFTime, LockedBall1, LockedBall2, OldLevel
Dim ZOffset
ZOffset = 0.7843

Dim TERampArray
TERampArray = Array (MiniPFDown, TEUp001, TEUp002,TEUp003,TEUp004,TEUp005,TEUp006,TEUp007,TEUp008,TEUp009,_
	TEUp010,TEUp011,TEUp012,TEUp013,TEUp014,TEUp015,TEUp016, TEUp017, TEUp018, TEUp019,_
	TEUp020,TEUp021,TEUp022,TEUp023,TEUp024,TEUp025,TEUp026, TEUp027, TEUp028, TEUp029,_
	TEUp030,TEUp031,TEUp032,TEUp033,TEUp034,TEUp035,TEUp036, TEUp037)', TEUp038, TEUp039,_
	'TEUp040,TEUp041,TEUp042,TEUp043,TEUp044,TEUp045,TEUp046, TEUp047, TEUp048, TEUp049)

Sub UpdateMiniPF(aCurrPos,aSpeed,aLast)
	If aCurrPos > 180 Then
		ZPos = (((aCurrPos - 180)* -1) +180)
	Else 
		ZPos = aCurrPos
	End If

	'debug.print ZPos
	
  	If aCurrPos < 35 then Controller.Switch(32) = false
 	If aCurrPos > 35 and aCurrPos < 90 then Controller.Switch(32) = true
 	If aCurrPos > 90 and aCurrPos < 145 then Controller.Switch(32) = false
 	If aCurrPos > 145 and aCurrPos < 180 then Controller.Switch(32) = true
 	If aCurrPos > 180 and aCurrPos < 190 then Controller.Switch(32) = false
 	If aCurrPos > 190 and aCurrPos < 270 then Controller.Switch(32) = true
 	If aCurrPos > 270 and aCurrPos < 350 then Controller.Switch(32) = false
 	If aCurrPos > 350then Controller.Switch(32) = true

	PlaysoundatVol SoundFX("Motor-Old1" ,DOFGear),0.1, sw78

	For XX=0 to MPFArrCnt-1
		MPFArr(XX).TransZ = ZPosTrans
	Next

	miniPFSH.Height = ZPosTrans + 0.1

	For XX=0 to MPFArrCnt2-1
		MPFArr2(XX).TransZ = (ZPosTrans - 84.18)
	Next

	For XX=0 to MPFLArrCnt-1
		MPFLArr(XX).BulbHaloHeight = ZPosTrans
	Next
	FL17f.Height = ZPosTrans+110

	If Not IsEmpty (LockedBall1) Then
		LockedBall1.Z = (ZPosTrans + 25)
	End If
	If Not IsEmpty (LockedBall2) Then
		LockedBall2.Z = (ZPosTrans + 25)
	End If

	Dim TERampCount
	TERampCount = Int(ZPosTrans/4)

	Dim b
	For b = 0 to UBound(TERampArray)
		If b = TERampCount Then 				'raise the time expander playfield
			TERampArray(b).collidable = True
		ElseIf b = TERampCount + 18 or b = TERampCount - 2 or b = TERampCount - 4 then	'put a roof to prevent flying balls and add some trailing surfaces to give Mini Playfield some thickness
			TERampArray(b).collidable = True
		ElseIf TERampArray(b).collidable = True Then 
			TERampArray(b).collidable = False
		End If
	Next

	If TERampCount > 0 Then
		Scoop1.Collidable = 0
		Scoop2.Collidable = 0
	Else
		Scoop1.Collidable = 1
		Scoop2.Collidable = 1
	End If

	'Raise the ball with the mini playfield
	If InRect(DWBall1.x, DWBall1.y, 275, 264, 590, 264, 590, 462, 275, 462) and abs(DWBall1.z - ZPosTrans - 25) < 10 Then
		DWBall1.z = ZPosTrans + 25
	End If

	If InRect(DWBall2.x, DWBall2.y, 275, 264, 590, 264, 590, 462, 275, 462) and abs(DWBall2.z - ZPosTrans - 25) < 10 Then
		DWBall2.z = ZPosTrans + 25
	End If

	If InRect(DWBall3.x, DWBall3.y, 275, 264, 590, 264, 590, 462, 275, 462) and abs(DWBall3.z - ZPosTrans - 25) < 10 Then
		DWBall3.z = ZPosTrans + 25		
	End If

	OldLevel = PFPos

	If ZPos <= 64 Then'Ground level
		PFPos = 0
'	Elseif ZPos > 5 and ZPos <= 64 Then
'		PFPos = 1
	Elseif ZPos > 64 and ZPos <= 128 Then  
		PFPos = 2
	Elseif ZPos > 128 Then
		PFPos = 3
	End If

	If OldLevel <> PFPos Then	
		Select Case PFPos
			Case 0
				gate68p.Rotx = 0
				gate38p.Rotx = 0
				gate88p.Rotx = 0

				sw68s.Enabled = 0
				sw38s.Enabled = 0
				sw88s.Enabled = 0

				sw71.isDropped = 1
				sw72.isDropped = 1
				sw73.isDropped = 1
				sw74.isDropped = 1
				sw75.isDropped = 1
				sw71a.isDropped = 1
				sw72a.isDropped = 1
				sw73a.isDropped = 1
				sw74a.isDropped = 1
				sw75a.isDropped = 1


				Sleeve1.Collidable = 1
				Sleeve4.Collidable = 1

				Pin1.Collidable = 0
				Pin2.Collidable = 0
				Pin3.Collidable = 0.
				Pin4.Collidable = 0

				Wall20.IsDropped = 1
				Wall21.IsDropped = 1
				Wall202.IsDropped = 1
				Wall212.IsDropped = 1

				Scoop3.Collidable = 0
				Scoop4.Collidable = 0
				Scoop5.Collidable = 0

				MiniPFDown.collidable = 1
			Case 2
				gate68p.Rotx = 0
				gate38p.Rotx = 0
				gate88p.Rotx = 0

				sw68s.Enabled = 0
				sw38s.Enabled = 0
				sw88s.Enabled = 0

				sw71.isDropped = 0
				sw72.isDropped = 0
				sw73.isDropped = 0
				sw74.isDropped = 0
				sw75.isDropped = 0
				sw71a.isDropped = 0
				sw72a.isDropped = 0
				sw73a.isDropped = 0
				sw74a.isDropped = 0
				sw75a.isDropped = 0


				Sleeve1.Collidable = 0
				Sleeve4.Collidable = 0

				Pin1.Collidable = 0
				Pin2.Collidable = 0
				Pin3.Collidable = 0
				Pin4.Collidable = 0

				Wall20.IsDropped = 1
				Wall21.IsDropped = 1
				Wall202.IsDropped = 1
				Wall212.IsDropped = 1

				Scoop3.Collidable = 0
				Scoop4.Collidable = 0
				Scoop5.Collidable = 0

				MiniPFDown.collidable = 0
			Case 3
				gate68p.Rotx = 0
				gate38p.Rotx = 0
				gate88p.Rotx = 0

				sw68s.Enabled = 1
				sw38s.Enabled = 1
				sw88s.Enabled = 1

				sw71.isDropped = 1
				sw72.isDropped = 1
				sw73.isDropped = 1
				sw74.isDropped = 1
				sw75.isDropped = 1
				sw71a.isDropped = 1
				sw72a.isDropped = 1
				sw73a.isDropped = 1
				sw74a.isDropped = 1
				sw75a.isDropped = 1

				Sleeve1.Collidable = 0
				Sleeve4.Collidable = 0

				Pin1.Collidable = 1
				Pin2.Collidable = 1
				Pin3.Collidable = 1
				Pin4.Collidable = 1

				Wall20.IsDropped = 0
				Wall21.IsDropped = 0
				Wall202.IsDropped = 0
				Wall212.IsDropped = 0

				Scoop3.Collidable = 1
				Scoop4.Collidable = 1
				Scoop5.Collidable = 1

				MiniPFDown.collidable = 0
		End Select
	End If
End Sub

Dim TEOn

Sub TEOnOff(Enabled)
	TEOn = Enabled
End Sub

Function ZPosTrans
	ZPosTrans = ZPos * ZOffset
End Function 

Sub TEHit_hit (idx)
	TEShake
End Sub

Sub TEShake
	Dim displacey, displacex

	displacey = -abs(activeball.vely)/30
	displacex = -activeball.velx/60

	For XX=0 to MPFArrCnt-1		
		MPFArr(XX).TransY = displacey
		MPFArr(XX).TransX = displacex
	Next

	For XX=0 to MPFArrCnt2-1		
		MPFArr2(XX).TransY = displacey
		MPFArr2(XX).TransX = displacex
	Next

	If Not IsEmpty (LockedBall1) Then
		LockedBall1.Y = sw76.Y +  displacey
		LockedBall1.X = sw76.X +  displacex
	End If
	If Not IsEmpty (LockedBall2) Then
		LockedBall2.Y = sw77.Y +  displacey
		LockedBall2.X = sw77.X +  displacex
	End If
	ResetMPF.Enabled = 1
End Sub

ResetMPF.interval = 35

Sub ResetMPF_timer
	For XX=0 to MPFArrCnt-1		
		MPFArr(XX).TransY = 0
		MPFArr(XX).TransX = 0
	Next

	For XX=0 to MPFArrCnt2-1		
		MPFArr2(XX).TransY = 0
		MPFArr2(XX).TransX = 0
	Next

	If Not IsEmpty (LockedBall1) Then
		LockedBall1.Y = sw76.Y
		LockedBall1.X = sw76.X
	End If
	If Not IsEmpty (LockedBall2) Then
		LockedBall2.Y = sw77.Y
		LockedBall2.X = sw77.X
	End If
	me.Enabled = 0
End Sub


  '**********************G.I STRING********************************
Sub UpdateGI(giNo, status)
	Dim ii, Trans

	Trans = Status * .125
	
	Select Case giNo
		Case 0  'BackBox 1,Insert in ROM
			For xx=0 to BGArrCnt0 - 1 
				If Status > 0 Then
					GIBGArr0(xx).IntensityScale = (Status-1) / 7
				Else
					GIBGArr0(xx).IntensityScale = 0					
				End If
			Next
			If Status = 8 Then SoundGIRelayOn KnockerPosition
			If Status < 2 Then SoundGIRelayOff KnockerPosition
		Case 1  'BackBox 2,Insert in ROM
			For xx=0 to BGArrCnt1 - 1 
				If Status > 2 Then
					GIBGArr1(xx).IntensityScale = (Status-1) / 7					
				Else
					GIBGArr1(xx).IntensityScale = 0
				End If
			Next
			If Status = 8 Then SoundGIRelayOn KnockerPosition
			If Status < 2 Then SoundGIRelayOff KnockerPosition
		Case 2  'String 3,PFa/Insert in ROM
			If Status > 2 Then
				For xx=0 to GArrCnt2 - 1				
					GIGArr2(xx).State = 1
					GIGArr2(xx).IntensityScale = Status * .25:
				Next
				If GISideWalls = 1 then
					For xx=0 to FArrCnt2 - 1
						GIFArr2(xx).visible = 1: 
					Next
				end if
			Else
				For xx=0 to GArrCnt2 - 1 				
					GIGArr2(xx).State = 0
				Next
				For xx=0 to FArrCnt2 - 1
					GIFArr2(xx).visible = 0: 
				Next
			End If

			For xx=0 to BGArrCnt2  - 1
				If Status > 0 Then
					GIBGArr2(xx).IntensityScale = (Status-1) / 7
				Else
					GIBGArr2(xx).IntensityScale = 0					
				End If
			Next

			If Status = 8 Then SoundGIRelayOn LeftMiddle
			If Status < 2 Then SoundGIRelayOff LeftMiddle
			UpdateMaterial "GIPlastics", 0, 1, 1, 1, 0, 0, Trans, 16777215, 16777215 , 4539717, 0, 1, 0,0,0,0
			
			For xx=0 to FLArrCnt2-1: FLArr2(xx).Opacity = (status) * 14.5:Next

		Case 3 'String 4,PFb/Insertb in ROM
			If Status > 2 Then
				For xx=0 to GArrCnt3 - 1 				
					GIGArr3(xx).State = 1
					GIGArr3(xx).IntensityScale = Status * .25:
				Next
				If GISideWalls = 1 then
					For xx=0 to FArrCnt3 - 1
						GIFArr3(xx).visible = 1: 
					Next
				end if
			Else
				For xx=0 to GArrCnt3 - 1 				
					GIGArr3(xx).State = 0
				Next
				For xx=0 to FArrCnt3 - 1
					GIFArr3(xx).visible = 0: 
				Next
			End If

			For xx=0 to BGArrCnt3 - 1 
				If Status > 0 Then
					GIBGArr3(xx).IntensityScale = (Status-1) / 7
				Else
					GIBGArr3(xx).IntensityScale = 0					
				End If
			Next

			If Status = 8 Then SoundGIRelayOn sw18
			If Status < 2 Then SoundGIRelayOff sw18

			For xx=0 to FLArrCnt3-1: FLArr3(xx).Opacity = (status) * 14.5:Next			
		Case 4  'String 5,PFc/Insertc in ROM
			If Status > 2 Then
				For xx=0 to GArrCnt4 - 1 				
					GIGArr4(xx).State = 1
					GIGArr4(xx).IntensityScale = Status * .25:
				Next
				If GISideWalls = 1 then
					For xx=0 to FArrCnt4 - 1
						GIFArr4(xx).visible = 1: 
					Next
				end if
			Else
				For xx=0 to GArrCnt4 - 1 				
					GIGArr4(xx).State = 0
				Next
				For xx=0 to FArrCnt4 - 1
					GIFArr4(xx).visible = 0: 
				Next
			End If

			For xx=0 to BGArrCnt4 - 1 
				If Status > 0 Then
					GIBGArr4(xx).IntensityScale = (Status-1) / 7
				Else
					GIBGArr4(xx).IntensityScale = 0					
				End If
			Next

			If Status = 8 Then SoundGIRelayOn sw78
			If Status < 2 Then SoundGIRelayOff sw78

			For xx=0 to FLArrCnt4-1: FLArr4(xx).Opacity = (status) * 14.5:Next			
'		Case 5 	'never passed from ROM 'PFc/Insertc in ROM
	End Select
End Sub

Dim GILevel

Sub Intensity
	If DayNight <= 20 Then 
			GILevel = .5
	ElseIf DayNight <= 40 Then 
			GILevel = .4125
	ElseIf DayNight <= 60 Then 
			GILevel = .325
	ElseIf DayNight <= 80 Then 
			GILevel = .2375
	Elseif DayNight <= 100  Then 
			GILevel = .15
	End If

	For each xx in GIG2: xx.Intensity = xx.Intensity * GILevel: Next
	For each xx in GIG3: xx.Intensity = xx.Intensity * GILevel: Next
	For each xx in GIG4: xx.Intensity = xx.Intensity * GILevel: Next
End Sub




'******************************************************
'****  LAMPZ by nFozzy
'******************************************************



Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments
LampTimer.Interval = 12		' Using fixed value so the fading speed is same for every fps
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

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub



Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/15 : next

	'Lampz Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= l11h
	Lampz.Callback(11) = "DisableLighting p11, 200,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= l12h
	Lampz.Callback(12) = "DisableLighting p12, 200,"
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(13)= l13h
	Lampz.Callback(13) = "DisableLighting p13, 200,"
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= l14h
	Lampz.Callback(14) = "DisableLighting p14, 200,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15h
	Lampz.Callback(15) = "DisableLighting p15, 200,"
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= l16h
	Lampz.Callback(16) = "DisableLighting p16, 200,"
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(17)= l17h
	Lampz.Callback(17) = "DisableLighting p17, 200,"
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= l18h
	Lampz.Callback(18) = "DisableLighting p18, 200,"
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= l21h
	Lampz.Callback(21) = "DisableLighting p21, 200,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22h
	Lampz.Callback(22) = "DisableLighting p22, 200,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= l23h
	Lampz.MassAssign(23)= fl_23

	Lampz.Callback(23) = "DisableLighting p23, 200,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24h
	Lampz.Callback(24) = "DisableLighting p24, 200,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= l25h
	Lampz.Callback(25) = "DisableLighting p25, 200,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26h
	Lampz.Callback(26) = "DisableLighting p26, 200,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27h
	Lampz.Callback(27) = "DisableLighting p27, 200,"
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= l28h
	Lampz.Callback(28) = "DisableLighting p28, 200,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= l31h
	Lampz.Callback(31) = "DisableLighting p31, 200,"
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(32)= l32h
	Lampz.Callback(32) = "DisableLighting p32, 200,"
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= l33h
	Lampz.Callback(33) = "DisableLighting p33, 200,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34h
	Lampz.Callback(34) = "DisableLighting p34, 200,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35h
	Lampz.Callback(35) = "DisableLighting p35, 200,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36h
	Lampz.MassAssign(36)= fl36

	Lampz.Callback(36) = "DisableLighting p36, 200,"
	Lampz.MassAssign(37)= l37
	Lampz.Callback(37) = "DisableLighting p37, 200,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= l38h
	Lampz.Callback(38) = "DisableLighting p38, 200,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= l41h
	Lampz.Callback(41) = "DisableLighting p41, 200,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42h
	Lampz.Callback(42) = "DisableLighting p42, 200,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= l43h
	Lampz.Callback(43) = "DisableLighting p43, 200,"
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(44)= l44h
	Lampz.Callback(44) = "DisableLighting p44, 200,"
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(45)= l45h
	Lampz.Callback(45) = "DisableLighting p45, 200,"
	Lampz.MassAssign(46)= l46
	Lampz.Callback(46) = "DisableLighting p46, 200,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= l47f
	Lampz.Callback(47) = "DisableLighting l47p, 5,"
	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= l48h
	Lampz.MassAssign(48)= fl48

	Lampz.Callback(48) = "DisableLighting p48, 200,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51h
	Lampz.Callback(51) = "DisableLighting p51, 200,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= l52h
	Lampz.Callback(52) = "DisableLighting p52, 200,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53h
	Lampz.Callback(53) = "DisableLighting p53, 200,"
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= l54h
	Lampz.Callback(54) = "DisableLighting p54, 200,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= l55h
	Lampz.Callback(55) = "DisableLighting p55, 200,"
	Lampz.MassAssign(56)= l56
'	Lampz.Callback(56) = "DisableLighting p56, 200,"   '<-- needs prims. moves vertically
	Lampz.MassAssign(57)= l57
'	Lampz.Callback(57) = "DisableLighting p57, 200,"   '<-- needs prims. moves vertically
	Lampz.MassAssign(58)= l58
	Lampz.Callback(58) = "DisableLighting l58p, 0.5,"
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(61)= L61h
	Lampz.Callback(61) = "DisableLighting p61, 200,"
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(62)= l62h
	Lampz.Callback(62) = "DisableLighting p62, 200,"
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= l63h
	Lampz.Callback(63) = "DisableLighting p63, 200,"
	Lampz.MassAssign(64)= l64
	Lampz.MassAssign(64)= l64h
	Lampz.Callback(64) = "DisableLighting p64, 200,"
	Lampz.MassAssign(65)= l65
	Lampz.MassAssign(65)= l65h
	Lampz.Callback(65) = "DisableLighting p65, 200,"
	Lampz.MassAssign(66)= l66
	Lampz.MassAssign(66)= l66h
	Lampz.Callback(66) = "DisableLighting p66, 200,"
	Lampz.MassAssign(67)= fl67

	Lampz.MassAssign(68)= l68
	Lampz.MassAssign(68)= l68h
	Lampz.Callback(68) = "DisableLighting p68, 200,"
	Lampz.MassAssign(71)= l71
	Lampz.MassAssign(71)= l71h
	Lampz.MassAssign(71)= fl71

	Lampz.Callback(71) = "DisableLighting p71, 200,"
	Lampz.MassAssign(72)= l72
	Lampz.MassAssign(72)= l72h
	Lampz.MassAssign(72)= fl72

	Lampz.Callback(72) = "DisableLighting p72, 200,"
	Lampz.MassAssign(73)= l73
	Lampz.MassAssign(73)= l73h
	Lampz.Callback(73) = "DisableLighting p73, 160,"
	Lampz.MassAssign(74)= l74
	Lampz.MassAssign(74)= l74h
	Lampz.Callback(74) = "DisableLighting p74, 160,"
	Lampz.MassAssign(75)= l75
	Lampz.MassAssign(75)= l75h
	Lampz.Callback(75) = "DisableLighting p75, 160,"
	Lampz.MassAssign(76)= l76
	Lampz.MassAssign(76)= l76h
	Lampz.Callback(76) = "DisableLighting p76, 160,"
	Lampz.MassAssign(77)= l77
	Lampz.MassAssign(77)= l77h
	Lampz.Callback(77) = "DisableLighting p77, 160,"
	Lampz.MassAssign(78)= l78
	Lampz.MassAssign(78)= l78h
	Lampz.Callback(78) = "DisableLighting p78, 200,"
	Lampz.MassAssign(81)= l81
	Lampz.MassAssign(81)= l81h
	Lampz.Callback(81) = "DisableLighting p81, 200,"
	Lampz.MassAssign(82)= l82
	Lampz.MassAssign(82)= fl82

	Lampz.Callback(82) = "DisableLighting p82, 200,"
	Lampz.MassAssign(83)= l83
	Lampz.MassAssign(83)= l83h
	Lampz.Callback(83) = "DisableLighting p83, 150,"
	Lampz.MassAssign(84)= l84
	Lampz.MassAssign(84)= l84h
	Lampz.Callback(84) = "DisableLighting p84, 150,"
	Lampz.MassAssign(85)= l85
	Lampz.MassAssign(85)= l85h
	Lampz.Callback(85) = "DisableLighting p85, 150,"

	Lampz.MassAssign(86)= l86a
	Lampz.MassAssign(86)= l86b

	Lampz.MassAssign(86)= l86ah
	Lampz.MassAssign(86)= l86bh

	Lampz.Callback(86) = "DisableLighting p86a, 200,"
	Lampz.Callback(86) = "DisableLighting p86b, 200,"

	if VRRoom < 1 then 
		f87.visible = 1
		f88.visible = 1
		f87d.visible = 1
		f88d.visible = 1
		f88f.visible = 0
		Lampz.MassAssign(87)= f87	' #Launchbutton
		Lampz.MassAssign(88)= f88	' #Startbutton

		if DesktopMode then 
			Lampz.MassAssign(23)= l23bg	' Desktop DMD Lights
			Lampz.MassAssign(36)= l36bg	' Desktop DMD Lights 
			Lampz.MassAssign(48)= l48bg	' Desktop DMD Lights
			Lampz.MassAssign(67)= l67bg	' Desktop DMD Lights
			Lampz.MassAssign(71)= l71bg	' Desktop DMD Lights
			Lampz.MassAssign(72)= l72bg	' Desktop DMD Lights
			Lampz.MassAssign(82)= l82bg	' Desktop DMD Lights
		end if

	else
		f87.visible = 0
		f88.visible = 0
		f87d.visible = 0
		f88d.visible = 0
		f88f.visible = 1
		Lampz.Callback(87)= "DisableLighting LaunchButton, 0.2,"
		Lampz.MassAssign(88)= f88f
	end if 

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




' misc 
Sub PrimTimer_Timer
	batleftshadow.RotZ = LeftFlipper.CurrentAngle
	batleftuppershadow.RotZ = LeftFlipper2.CurrentAngle
	batrightshadow.RotZ = RightFlipper.CurrentAngle

	FlipperL.RotZ = LeftFlipper.CurrentAngle
	FlipperR.RotZ = RightFlipper.CurrentAngle
	FlipperL2.RotZ = LeftFlipper2.CurrentAngle
	If Controller.Lamp(67) = 0 Then
		l67on.visible = 0
			If SideWallFlashers = 1 then
			l67onb.visible = 0
			End If
	Else
		L67on.visible = 1
			If SideWallFlashers = 1 then		
			l67onb.visible = 1
			End If
	End If
		If SideWallFlashers = 1 then
			If GI33.State = 1 Then
				GI33b.visible = 1
			Else
				GI33b.visible =0
			End If
			If GI32.State = 1 Then
				GI32b.visible = 1
			Else
				GI32b.visible = 0
			End If
		End if
	If Light3.state = 1 Then
		Flasher4.visible = 1
	Else
		Flasher4.visible = 0
	End If

	If Light2.state = 1 Then
		Flasher5.visible = 1
		Flasher6.visible = 1
	Else
		Flasher5.visible = 0
		Flasher6.visible = 0
	End If
'TextBox2.Text = ABS(gi11.Intensity)
'TextBox1.Text = 
End Sub



'
'' ***************************************************************************
''                                  LAMP CALLBACK
'' ****************************************************************************
'
'Set LampCallback = GetRef("UpdateMultipleLamps")
'
'Sub UpdateMultipleLamps()
'
'	if l48.state = 1 then: fl48.visible = 1: else: fl48.visible = 0: end if ' #1
'	if l36.state = 1 then: fl36.visible = 1: else: fl36.visible = 0: end if ' #2
'	if l82.state = 1 then: fl82.visible = 1: else: fl82.visible = 0: end if ' #3
'	if l71.state = 1 then: fl71.visible = 1: else: fl71.visible = 0: end if ' #4
'
'	if l72.state = 1 then: fl72.visible = 1: else: fl72.visible = 0: end if ' #6
'	if l23.state = 1 then: fl_23.visible = 1: else: fl_23.visible = 0: end if ' #7
'
'	'#5
'	If Controller.Lamp(67) = 0 Then
'		fl67.visible = 0
'	else 
'		fl67.visible = 1
'	end if
'
'	if l87.state = 1 and VRRoom < 1 then: f87.visible = 1: else: f87.visible = 0: end if ' #Launchbutton
'	if l87.state = 1 and VRRoom > 0 then: LaunchButton.blenddisablelighting = 0.2: else: LaunchButton.blenddisablelighting = 0: end if ' #3
'
'	if l88.state = 1 and VRRoom < 1 then: f88.visible = 1: else: f88.visible = 0: end if ' #StartButton
'	if l88.state = 1 and VRRoom > 0 then: f88f.visible = 1: else: f88f.visible = 0: end if ' #3
'
'End Sub
'
'






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
'    Rolling Sounds & Sliderpoint's Ball Shadows
'*****************************************

Const tnob = 4 ' maximum number of balls on the table (including locked/newton Balls)
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

Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
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
			If BOT(b).Z > 30 Then
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
			If BOT(b).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
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
			Elseif BOT(b).Z <= 30 And BOT(b).Z > 10 Then	'On pf, primitive only
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
			If BOT(b).Z > 30 Then							'In a ramp
				BallShadowA(b).X = BOT(b).X
				BallShadowA(b).Y = BOT(b).Y + BallSize/5 + fovY
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(b).visible = 1
			Elseif BOT(b).Z <= 30 And BOT(b).Z > 10 Then	'On pf
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
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				'DropCount(b) = 0
				If BOT(b).velz > -5 Then
					If BOT(b).z < 35 Then
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

Sub GameTimer_Timer()
	SetBlendDisableLighting
	RollingSoundUpdate
	If DynamicBallShadowsOn Then DynamicBSUpdate
	cor.update	
	DoSTAnim
End Sub

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
'
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold

sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, Source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
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
	Dim falloff:	falloff = 250			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, LSd, currentMat, AnotherSource, iii
	Dim BOT, Source
	
	s = -1
	For each BOT in Array(DWBall1, DWBall2, DWBall3)
		s = s + 1
		If BOT.Z < 30 and BOT.z > 20 and BOT.y < 1880 and BOT.x < 865 Then 'Defining when and where (on the table) you can have dynamic shadows
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

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


' Blend Disable Lighting for Tardis and Time Expander

Sub SetBlendDisableLighting()
End Sub


	'ball drops
Sub RHelp_Hit:WireRampOff:End Sub' 'ActiveBall.VelY=0
Sub RHelp2_Hit:WireRampOff:End Sub 

Sub LaneEnd1_Hit
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 3 then
 		RandomSoundRubberStrong 5*5 
	End if
	If finalspeed <= 3 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

Sub LaneEnd2_Hit
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 3 then
 		RandomSoundRubberStrong 5*5 
	End if
	If finalspeed <= 3 then
 		RandomSoundRubberWeak()
 	End If	
End Sub

Sub WireRamp_Hit:WireRampOff:WireRampOn False:RandomSoundMetal:end Sub


'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

GlobalSoundLevel = 3													'relative sound level sound relays only; This is not used for global sound effects volume.
CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8/3 '1 wjr											'volume level; range [0, 1]
'PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim RelayFlashSoundLevel, RelayGISoundLevel
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010/3          						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635/3								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0/3                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45/3                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
RelayFlashSoundLevel = 0.0025 * GlobalSoundLevel * 14/3						'volume level; range [0, 1];
RelayGISoundLevel = 0.025 * GlobalSoundLevel * 1.5/3							'volume level; range [0, 1];
SlingshotSoundLevel = 0.95/3												'volume level; range [0, 1]
BumperSoundFactor = 4.25/3												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim BallWithBallCollisionSoundFactor
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2/5									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025/5									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025/5									'volume multiplier; must not be zero
PlasticRampDropToPlayfieldSoundLevel = 0.9/5									'volume level; range [0, 1]
WireRampDropToPlayfieldSoundLevel = 0.9/5								'volume level; range [0, 1]
DelayedBallDropOnPlayfieldSoundLevel = 0.8/5									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075/5											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/5
SubwaySoundLevel = 1/5	
SubwayEntrySoundLevel = 1/5
ScoopEntrySoundLevel = 1/5
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10/5											'volume multiplier; must not be zero
'SpinnerSoundLevel = 0.002												'volume level; range [0, 1]
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]


'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8/5														'volume level; range [0, 1]
BallReleaseSoundLevel = 1/5												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2/5									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015/5										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim LaneLoudImpactMinimumSoundLevel, LaneLoudImpactMaximumSoundLevel, LaneSoundFactor, LaneEnterSoundFactor
Dim LaneLoudImpactSoundLevel
'InnerLoopSoundFactor = 0.25												'volume multiplier; must not be zero
'LaneSoundFactor = 0.15													'volume multiplier; must not be zero
'LaneEnterSoundFactor = 0.3												'volume multiplier; must not be zero
'LaneLoudImpactMinimumSoundLevel = 0										'volume level; range [0, 1]
'LaneLoudImpactMaximumSoundLevel = 0.3									'volume level; range [0, 1]


'///////////////////////-----Ramps-----///////////////////////
'///////////////////////-----Plastic Ramps-----///////////////////////
Dim RampCornerSoundLevel, RampFallbackSoundLevel

RampCornerSoundLevel = 0.1												'volume level; range [0, 1]
'RampFallbackSoundLevel = 0.2											'volume level; range [0, 1]

'///////////////////////-----Wire Ramps-----///////////////////////
Dim WireRampSoundLevel

WireRampSoundLevel = 0.75										'volume level; range [0, 1]
'///////////////////////-----Ramp Flaps-----///////////////////////
Dim FlapSoundLevel

FlapSoundLevel = 0.8/10													'volume level; range [0, 1]

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
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, aVol, tableobj)
    PlaySound soundname, 1, aVol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub


'/////////////////////////////  SUPPORTING BALL & SOUND FUNCTIONS  ////////////////////////////
Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp

	tmp = tableobj.y * 2 / tableheight-1
	'tmp = tableobj.x * 2 / table1.height-1
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

Function VolPlasticRampRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlasticRampRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
    PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function PitchPlasticRamp(ball) ' Calculates the pitch of the sound based on the ball speed - used for plastic ramps roll sound
    PitchPlasticRamp = BallVel(ball) * 20
End Function

Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function RndInt(min, max)
    RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySoundAtLevelStatic ("Start_Button"), StartButtonSoundLevel, sw28
End Sub

Sub SoundNudgeLeft()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
	End Select
End Sub

Sub SoundNudgeRight()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
	End Select
End Sub

Sub SoundNudgeCenter()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic ("Nudge_1"), NudgeCenterSoundLevel * VolumeDial, sw28
		Case 2 : PlaySoundAtLevelStatic ("Nudge_2"), NudgeCenterSoundLevel * VolumeDial, sw28
		Case 3 : PlaySoundAtLevelStatic ("Nudge_3"), NudgeCenterSoundLevel * VolumeDial, sw28
	End Select
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic ("Drain_1"), DrainSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic ("Drain_2"), DrainSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic ("Drain_3"), DrainSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic ("Drain_4"), DrainSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic ("Drain_5"), DrainSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic ("Drain_6"), DrainSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic ("Drain_7"), DrainSoundLevel, drainswitch
		Case 8 : PlaySoundAtLevelStatic ("Drain_8"), DrainSoundLevel, drainswitch
		Case 9 : PlaySoundAtLevelStatic ("Drain_9"), DrainSoundLevel, drainswitch
		Case 10 : PlaySoundAtLevelStatic ("Drain_10"), DrainSoundLevel, drainswitch
		Case 11 : PlaySoundAtLevelStatic ("Drain_11"), DrainSoundLevel, drainswitch
	End Select
End Sub


'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBallRelease(brswitch)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Ball_Release_1",DOFContactors), BallReleaseSoundLevel, brswitch
		Case 2 : PlaySoundAtLevelStatic SoundFX("Ball_Release_2",DOFContactors), BallReleaseSoundLevel, brswitch
		Case 3 : PlaySoundAtLevelStatic SoundFX("Ball_Release_3",DOFContactors), BallReleaseSoundLevel, brswitch
		Case 4 : PlaySoundAtLevelStatic SoundFX("Ball_Release_4",DOFContactors), BallReleaseSoundLevel, brswitch
		Case 5 : PlaySoundAtLevelStatic SoundFX("Ball_Release_5",DOFContactors), BallReleaseSoundLevel, brswitch
		Case 6 : PlaySoundAtLevelStatic SoundFX("Ball_Release_6",DOFContactors), BallReleaseSoundLevel, brswitch
		Case 7 : PlaySoundAtLevelStatic SoundFX("Ball_Release_7",DOFContactors), BallReleaseSoundLevel, brswitch
	End Select
End Sub

'/////////////////////////////  AUTO PLUNGER  SOLENOID SOUNDS  ////////////////////////////
Sub SoundAutoPlunger(scenario)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Plunger_Release_Ball",DOFContactors), PlungerReleaseSoundLevel, swPlunger
		Case 1
			PlaySoundAtLevelStatic SoundFX("Plunger_Auto_Release_No_Ball",DOFContactors), PlungerReleaseSoundLevel, swPlunger
	End Select
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft()
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_L1",DOFContactors), SlingshotSoundLevel, SlingL
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_L2",DOFContactors), SlingshotSoundLevel, SlingL
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_L3",DOFContactors), SlingshotSoundLevel, SlingL
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_L4",DOFContactors), SlingshotSoundLevel, SlingL
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_L5",DOFContactors), SlingshotSoundLevel, SlingL
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_L6",DOFContactors), SlingshotSoundLevel, SlingL
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_L7",DOFContactors), SlingshotSoundLevel, SlingL
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_L8",DOFContactors), SlingshotSoundLevel, SlingL
		Case 9 : PlaySoundAtLevelStatic SoundFX("Sling_L9",DOFContactors), SlingshotSoundLevel, SlingL
		Case 10 : PlaySoundAtLevelStatic SoundFX("Sling_L10",DOFContactors), SlingshotSoundLevel, SlingL
	End Select
End Sub

Sub RandomSoundSlingshotRight()
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_R1",DOFContactors), SlingshotSoundLevel, SlingR
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_R2",DOFContactors), SlingshotSoundLevel, SlingR
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_R3",DOFContactors), SlingshotSoundLevel, SlingR
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_R4",DOFContactors), SlingshotSoundLevel, SlingR
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_R5",DOFContactors), SlingshotSoundLevel, SlingR
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_R6",DOFContactors), SlingshotSoundLevel, SlingR
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_R7",DOFContactors), SlingshotSoundLevel, SlingR
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_R8",DOFContactors), SlingshotSoundLevel, SlingR
	End Select
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperA()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper1
	End Select
End Sub

Sub RandomSoundBumperB()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper2
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper2
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper2
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper2
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper2
	End Select
End Sub

Sub RandomSoundBumperC()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper4
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper4
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper4
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper4
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bumper4
	End Select
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
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_L01",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_L02",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_L07",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_L08",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_L09",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_L10",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_L12",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_L14",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_L18",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_L20",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_L26",DOFFlippers), FlipperLeftHitParm, Flipper
	End Select
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_R01",DOFFlippers), FlipperRightHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_R02",DOFFlippers), FlipperRightHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_R03",DOFFlippers), FlipperRightHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_R04",DOFFlippers), FlipperRightHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_R05",DOFFlippers), FlipperRightHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_R06",DOFFlippers), FlipperRightHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_R07",DOFFlippers), FlipperRightHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_R08",DOFFlippers), FlipperRightHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_R09",DOFFlippers), FlipperRightHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_R10",DOFFlippers), FlipperRightHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_R11",DOFFlippers), FlipperRightHitParm, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpRight(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_8",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_1"), parm / 10 * RubberFlipperSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_2"), parm / 10 * RubberFlipperSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_3"), parm / 10 * RubberFlipperSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_4"), parm / 10 * RubberFlipperSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_5"), parm / 10 * RubberFlipperSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_6"), parm / 10 * RubberFlipperSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_7"), parm / 10 * RubberFlipperSoundFactor
	End Select
End Sub


'/////////////////////////////  JP'S VP10 BALL COLLISION SOUND  ////////////////////////////
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

'/////////////////////////////  THEATRE OF MAGIC SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rollover_1"), RolloverSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Rollover_2"), RolloverSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Rollover_3"), RolloverSoundLevel
		Case 4 : PlaySoundAtLevelActiveBall ("Rollover_4"), RolloverSoundLevel
	End Select
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
'/////////////////////////////  POSTS - EVENTS  ////////////////////////////
Sub RubberPosts_Hit(idx)
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
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor
	End Select
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
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
	Select Case Int(Rnd*13)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Metal_Touch_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Metal_Touch_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Metal_Touch_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Metal_Touch_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Metal_Touch_5"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Metal_Touch_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Metal_Touch_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Metal_Touch_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Metal_Touch_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall ("Metal_Touch_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall ("Metal_Touch_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall ("Metal_Touch_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall ("Metal_Touch_13"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Medium_Hit (idx)
	RandomSoundMetal
End Sub

Sub Metals2_Hit (idx)
	RandomSoundMetal
End Sub


'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_2"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Drain_On_Metal_Under_Apron_1"), BottomArchBallGuideSoundFactor * 0.25
		Case 2 : PlaySoundAtLevelActiveBall ("Drain_On_Metal_Under_Apron_2"), BottomArchBallGuideSoundFactor * 0.25
		Case 3 : PlaySoundAtLevelActiveBall ("Drain_On_Metal_Under_Apron_3"), BottomArchBallGuideSoundFactor * 0.25
	End Select
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
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Medium_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*7)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Soft_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Soft_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Soft_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall ("Apron_Soft_4"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall ("Apron_Soft_5"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 6 : PlaySoundAtLevelActiveBall ("Apron_Soft_6"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 7 : PlaySoundAtLevelActiveBall ("Apron_Soft_7"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_5",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_6",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_7",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_8",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
End Sub

Sub PlayTargetSound()
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 10 then
 		RandomSoundTargetHitStrong()
	Else 
 		RandomSoundTargetHitWeak()
 	End If	
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

'/////////////////////////////  RAMPS BALL DROP TO PLAYFIELD SOUNDS  ////////////////////////////
'/////////////////////////////  PLASTIC RAMP - EXIT HOLE - TO PLAYFIELD - SOUNDS  ////////////////////////////
Sub RandomSoundPlasticRampDropToPlayfield(rswitch)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_From_Plastic_Ramp_1"), PlasticRampDropToPlayfieldSoundLevel, rswitch
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_From_Plastic_Ramp_2"), PlasticRampDropToPlayfieldSoundLevel, rswitch
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_From_Plastic_Ramp_3"), PlasticRampDropToPlayfieldSoundLevel, rswitch
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_From_Plastic_Ramp_4"), PlasticRampDropToPlayfieldSoundLevel, rswitch
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_From_Plastic_Ramp_5"), PlasticRampDropToPlayfieldSoundLevel, rswitch
	End Select
End Sub

'/////////////////////////////  WIRE RAMP - EXIT HOLE - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundWireRampDropToPlayfield(rswitch)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_From_Wire_Ramp_1"), WireRampDropToPlayfieldSoundLevel, rswitch
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_From_Wire_Ramp_2"), WireRampDropToPlayfieldSoundLevel, rswitch
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_From_Wire_Ramp_3"), WireRampDropToPlayfieldSoundLevel, rswitch
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_From_Wire_Ramp_4"), WireRampDropToPlayfieldSoundLevel, rswitch
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_From_Wire_Ramp_5"), WireRampDropToPlayfieldSoundLevel, rswitch
	End Select
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, ActiveBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, ActiveBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, ActiveBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, ActiveBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, ActiveBall
	End Select
End Sub


'/////////////////////////////  SUBWAY  ////////////////////////////
Sub RandomSoundSubway()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Subway_1",DOFContactors), SubwaySoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic SoundFX("Subway_2",DOFContactors), SubwaySoundLevel, Activeball
		Case 3 : PlaySoundAtLevelStatic SoundFX("Subway_3",DOFContactors), SubwaySoundLevel, Activeball
		Case 4 : PlaySoundAtLevelStatic SoundFX("Subway_4",DOFContactors), SubwaySoundLevel, Activeball
	End Select
End Sub

Sub TardisTrough_hit:RandomSoundSubwayEntry:End Sub


'/////////////////////////////  SUBWAY ENTRY AND SCOOP ENTRY ////////////////////////////
Sub RandomSoundSubwayEntry()
	Select Case Int(Rnd*6)+1
		Case 1 : PlaySoundAtLevelStatic ("Subway_Entry_1"), SubwayEntrySoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Subway_Entry_2"), SubwayEntrySoundLevel, Activeball
		Case 3 : PlaySoundAtLevelStatic ("Subway_Entry_3"), SubwayEntrySoundLevel, Activeball
		Case 4 : PlaySoundAtLevelStatic ("Subway_Entry_4"), SubwayEntrySoundLevel, Activeball
		Case 5 : PlaySoundAtLevelStatic ("Subway_Entry_5"), SubwayEntrySoundLevel, Activeball
		Case 6 : PlaySoundAtLevelStatic ("Subway_Entry_6"), SubwayEntrySoundLevel, Activeball
	End Select
End Sub

Sub RandomSoundSubwayEntry2()
	PlaySoundAtLevelStatic ("Subway_Enter_1"), SubwayEntrySoundLevel, Activeball
End Sub

Sub RandomSoundScoopEntry()
	Select Case Int(Rnd*6)+1
		Case 1 : PlaySoundAtLevelStatic ("Scoop_Entry_1"), ScoopEntrySoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Scoop_Entry_2"), ScoopEntrySoundLevel, Activeball
		Case 3 : PlaySoundAtLevelStatic ("Scoop_Entry_3"), ScoopEntrySoundLevel, Activeball
		Case 4 : PlaySoundAtLevelStatic ("Scoop_Entry_4"), ScoopEntrySoundLevel, Activeball
		Case 5 : PlaySoundAtLevelStatic ("Scoop_Entry_5"), ScoopEntrySoundLevel, Activeball
		Case 6 : PlaySoundAtLevelStatic ("Scoop_Entry_6"), ScoopEntrySoundLevel, Activeball
		Case y : PlaySoundAtLevelStatic ("Scoop_Entry_7"), ScoopEntrySoundLevel, Activeball
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////
Sub SoundRampGate()
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_Small_1"), GateSoundLevel * 0.2 , Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_Small_2"), GateSoundLevel * 0.2 , Activeball
	End Select
End Sub

Sub SoundPlayfieldGate()
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_FastTrigger_1"), GateSoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_FastTrigger_2"), GateSoundLevel, Activeball
	End Select
End Sub

Sub SoundHeavyGate()
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_1"), GateSoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
	End Select
End Sub

'/////////////////////////////  WIRE RAMP TIMERS  ////////////////////////////

Dim WireRampBall

Sub WireRamp_Timer()
	If WireRampBall.z > 60 Then
		PlaySoundAtLevelTimerExistingActiveBall ("Wire_Ramp"), RollingSoundFactor * 0.00025 * Csng(BallVel(WireRampBall)^3) * VolumeDial, WireRampBall
	Else
		me.timerenabled = false
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, Activeball
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, Activeball
	End Select
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropFromKicker(saucer)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, saucer
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, saucer
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, saucer
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, saucer
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  GENERAL ILLUMINATION RELAYS  ////////////////////////////
Sub SoundGIRelayOn(giloc)
	PlaySoundAtLevelStatic ("GI_On"), 0.025*RelayGISoundLevel, giloc
End Sub

Sub SoundGIRelayOff(giloc)
	PlaySoundAtLevelStatic ("GI_Off"), 0.025*RelayGISoundLevel, giloc
End Sub

'/////////////////////////////  PLAYFIELD FLASH RELAYS  ////////////////////////////
Sub SoundFlashRelayOn(flashloc)
	PlaySoundAtLevelStatic ("Relay_On"), 0.025*RelayGISoundLevel, flashloc
End Sub

Sub SoundFlashRelayOff(flashloc)
	PlaySoundAtLevelStatic ("Relay_Off"), 0.025*RelayFlashSoundLevel, flashloc
End Sub


'/////////////////////////////  PLASTIC RAMPS FLAPS - SOUNDS  ////////////////////////////
Sub RandomSoundRampFlapUp()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Ramp_Flap_Up_1"), FlapSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Ramp_Flap_Up_2"), FlapSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Ramp_Flap_Up_3"), FlapSoundLevel
	End Select
End Sub

Sub RandomSoundRampFlapDown()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Ramp_Flap_Down_1"), FlapSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Ramp_Flap_Down_2"), FlapSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Ramp_Flap_Down_3"), FlapSoundLevel
	End Select
End Sub

'/////////////////////////////  PLASTIC RAMPS FLAPS - EVENTS  ////////////////////////////
Sub RampFlap1_Hit()
	If ActiveBall.VelY > 0  Then 
		'ball is traveling down the playfield
		RandomSoundRampFlapDown()		
		WireRampOff
	ElseIf ActiveBall.VelY < 0 Then
		'ball is traveling up the playfield
		RandomSoundRampFlapUp()		
		WireRampOn True
	End If
End Sub

Dim RampFlap2Count

Sub RampFlap2_Hit()
	If ActiveBall.VelX < 0 and RampFlap2Count = 0 Then 
		RampFlap2Count = 3
		me.timerenabled = true
		'ball is traveling down the playfield
		RandomSoundRampFlapDown()
		WireRampOff
	ElseIf ActiveBall.VelX > 0  and RampFlap2Count = 0 Then
		RampFlap2Count = 3
		me.timerenabled = true
		RandomSoundRampFlapUp()
		WireRampOn True
	End If
End Sub

Sub RampFlap2_Timer()
	RampFlap2Count = RampFlap2Count - 1
	If RampFlap2Count = 0 Then me.timerenabled = false
End Sub

'/////////////////////////////  PLASTIC RAMPS CORNER - SOUNDS  ////////////////////////////

Sub SoundRampCorner1()
	PlaySoundAtLevelStatic ("Ramp_Corner_1"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner2()
	PlaySoundAtLevelStatic ("Ramp_Corner_2"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner3()
	PlaySoundAtLevelStatic ("Ramp_Corner_3"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner4()
	PlaySoundAtLevelStatic ("Ramp_Corner_4"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner5()
	PlaySoundAtLevelStatic ("Ramp_Corner_5"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner6()
	PlaySoundAtLevelStatic ("Ramp_Corner_6"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner7()
	PlaySoundAtLevelStatic ("Ramp_Corner_7"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner8()
	PlaySoundAtLevelStatic ("Ramp_Corner_8"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner9()
	PlaySoundAtLevelStatic ("Ramp_Corner_9"), RampCornerSoundLevel, Activeball
End Sub 

Sub SoundRampCorner10()
	PlaySoundAtLevelStatic ("Ramp_Corner_10"), RampCornerSoundLevel, Activeball
End Sub 

'/////////////////////////////  PLASTIC RAMPS CORNER - EVENTS  ////////////////////////////

Sub Corner001_Hit()
	SoundRampCorner1
End Sub

Sub Corner002_Hit()
	SoundRampCorner2
End Sub

Sub Corner003_Hit()
	SoundRampCorner3
End Sub

Sub Corner004_Hit()
	SoundRampCorner4
End Sub

Sub Corner005_Hit()
	SoundRampCorner5
End Sub

Sub Corner006_Hit()
	SoundRampCorner6
End Sub

Sub Corner007_Hit()
	SoundRampCorner7
End Sub

Sub Corner008_Hit()
	SoundRampCorner8
End Sub

Sub Corner009_Hit()
	SoundRampCorner9
End Sub

Sub Corner010_Hit()
	SoundRampCorner10
End Sub

Sub Corner011_Hit()
	SoundRampCorner2
End Sub

Sub Corner012_Hit()
	SoundRampCorner1
End Sub

'/////////////////////////////  LEFT LOOP ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_L1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_L2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_L3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_L4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub

Sub RandomSoundRightArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_R1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_R2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_R3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_R4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub


Sub Arch_hit()
	'debug.print activeball.vely
	If activeball.vely < -15 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_unhit()
'	StopSound "Arch_L1"
'	StopSound "Arch_L2"
'	StopSound "Arch_L3"
'	StopSound "Arch_L4"

	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

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
dim RampBalls(5,2)
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
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
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

'*********************
'Cabinet Mode
'*********************

if CabinetMode = 1 and VRRoom < 1 then
		PinCab_Blades.Size_y=2000
		Pincab_Rails.visible = 0
Else
		PinCab_Blades.Size_y=1000
end If

'**************************** VR Stuff ******************************
DIM VRThings
If VRRoom > 0 Then
	Scoretext.visible = 0
	DMD.visible = 1
	If VRRoom = 1 Then
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VRStuff:VRThings.visible = 1:Next
		for each VRThings in DTStuff:VRThings.visible = 0:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		for each VRThings in DTStuff:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
		Pincab_Backbox.image = "Pincab_Backbox_Min"
	End If
Else
		Scoretext.visible = 1
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRStuff:VRThings.visible = 0:Next
		for each VRThings in DTStuff:VRThings.visible = 1:Next
End if

if SideLogo = 1 then
		LogoSide.visible = 1
Else
		LogoSide.visible = 0
end If
