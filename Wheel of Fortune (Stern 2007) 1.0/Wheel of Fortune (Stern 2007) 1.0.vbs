Option explicit
Randomize

'************************************************************************
'							TABLE OPTIONS
'************************************************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName = "wof_500"

Const Ballsize = 52
Const BallMass = 1.5

Const UseVPMModSol = True
Const UseModulatedMiniDMD = True
Dim UseVPMDMD,CustomDMD,DesktopMode
DesktopMode = Table1.ShowDT : CustomDMD = False
If Right(cGamename,1)="c" Then CustomDMD=True
If CustomDMD OR (NOT DesktopMode AND NOT CustomDMD) Then UseVPMDMD = False		'hides the internal VPMDMD when using the color ROM or when table is in Full Screen and color ROM is not in use
If DesktopMode AND NOT CustomDMD Then UseVPMDMD = True							'shows the internal VPMDMD when in desktop mode and color ROM is not in use
Scoretext.visible = NOT CustomDMD												'hides the textbox when using the color ROM

LoadVPM "02800000", "Sam.VBS", 3.54

'********************
'Standard definitions
'********************

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 1

'Standard Sounds
Const SSolenoidOn = "solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_coin"

'************************************************************************
'						 INIT TABLE
'************************************************************************

Dim bsTrough, PlungerIM, DTBank

Sub Table1_Init
	vpmInit Me
    With Controller
        .GameName = cGameName
        .SplashInfoLine = "Wheel Of Fortune (Stern 2007)"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        If NOT CustomDMD Then .Hidden = DesktopMode				'hides the external DMD when in desktop mode and color ROM is not in use
        .HandleMechanics = 1
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

'Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

'Nudging
    vpmNudge.TiltSwitch = -7
    vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

'Trough
    Set bsTrough = New cvpmTrough
    With bsTrough
		.Size = 4
		.InitSwitches Array(21, 20, 19, 18)
		.InitExit BallRelease, 70, 15
		.InitEntrySounds "fx_drain", SoundFX(SSolenoidOn,DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
		.InitExitSounds  SoundFX(SSolenoidOn,DOFContactors), SoundFX("ballrelease",DOFContactors)
		.Balls = 4
		.CreateEvents "bsTrough", Drain
    End With

'Impulse Plunger
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP sw23, 60, 0.5
        .Switch 23
        .Random 1.5
        .InitExitSnd SoundFX("AutoPlunger",DOFContactors), SoundFX(SSolenoidOn,DOFContactors)
        .CreateEvents "plungerIM"
    End With

'Targets
	Set DTBank = New cvpmDropTarget  
   	  With DTBank
   		.InitDrop Array(sw39, sw40, sw41), Array(39,40,41)
        .Initsnd SoundFX("TotemDropDown", DOFDropTargets), SoundFX("TotemDropUp", DOFDropTargets)
       End With

	BallLock1.IsDropped = 1
	InitWheelLights

End Sub

'************************************************************************
'							KEYS
'************************************************************************

Sub Table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 3:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 3:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 4:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
	If KeyCode = PlungerKey Then Plunger.Pullback: PlaySoundAt "fx_PlungerPull", Plunger
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal Keycode)
	If KeyCode = PlungerKey Then Plunger.Fire: StopSound "fx_PlungerPull":PlaysoundAt "fx_Plunger", Plunger
    If vpmKeyUp(keycode) Then Exit Sub
'	if keycode = LeftMagnaSave Then			'debug
'		On Error Resume Next
'		KickerTEST.CreateBall
'		KickerTEST.Kick 0, 45.-3
'		On Error Goto 0 
'	end if 
End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_UnPaused:Controller.Pause = 0:End Sub
Sub Table1_Exit:Controller.Stop:End Sub

'************************************************************************
'						 SOLENOIDS MAP
'************************************************************************

SolCallBack(1) = "SolTrough"						'Trough-Up Kicker
SolCallBack(2) = "SolAutoPlungerIM"					'AutoLaunch
SolCallback(3) = ""
'4-7 												'Wheel Motor
SolCallback(8) = "DTBank.SolDropUp"					'Drop Target
SolCallback(12) = "solLonnie"						'lonnie
SolCallback(13) = "solMaria"						'Maria 
SolCallback(14) = "solKeith"						'Keith
SolCallback(15)= "SolLFlipper"						'Left Flipper
SolCallback(16)= "SolRFlipper"						'Right Flipper
SolModCallback(19) = "SetLampMod 129,"   			'Flasher:Red Contestant         
SolModCallback(20) = "SetLampMod 130,"				'Flasher:Yellow Contestant
SolModCallback(21) = "SetLampMod 131,"				'Flasher:Blue Contestant
SolCallback(22) = "solBallLock" 					'MiniRamp Down Post
SolCallback(23) = "solBallLock1" 					'Left Ramp Up Post
SolCallback(24) = "vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker)," 		'Knocker  
SolModCallback(25) = "SetLampMod 135,"   			'Flasher:Wheel 
SolModCallBack(26) = "SetLampMod 136,"				'Flasher:Backpanel Left
SolModCallback(27) = "SetLampMod 137,"				'Flasher:Left Orbit
SolModCallback(28) = "SetLampMod 138,"				'Flasher:Right Ramp
SolModCallback(29) = "SetLampMod 139,"				'Flasher:Right Orbit
SolModCallback(30) = "SetLampMod 132,"				'Flasher:Pop Bumper           
SolModCallback(31) = "SetLampMod 133,"				'Flasher:Mini Ramp
SolModCallBack(32) = "SetLampMod 134,"				'Flasher:Backpanel Right

'************************************************************************
'						 SOLENOIDS SUBS
'************************************************************************

' *** BALL TROUGH
Sub SolTrough(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		If BsTrough.Balls Then vpmTimer.PulseSw 22
	End If
End Sub

' *** AUTOPLUNGER
Sub SolAutoPlungerIM(Enabled)
	If Enabled Then
		PlungerIM.AutoFire
	End If
End Sub

' *** FLIPPERS
Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), light20
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), light20
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), light21
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), light21
        RightFlipper.RotateToStart
    End If
End Sub

' *** MINIRAMP DOWN POST
Sub SolBallLock(enabled)
	If enabled then
		BallLock.IsDropped = 1
		BallLockP.Z = 0
		PlaySoundAt SoundFX("TotemDropDown", DOFContactors), BallLockP
	else
		BallLock.IsDropped = 0
		BallLockP.Z = 35
		PlaySoundAt SoundFX("TotemDropUp", DOFContactors), BallLockP
	End If
  End Sub

' *** LEFT RAMP UP POST
Sub SolBallLock1(enabled)
	If enabled then
		BallLock1.IsDropped = 0
		BallLock1P.Z = 45
		PlaySoundAt SoundFX("TotemDropUp", DOFContactors), BallLock1P
	else
		BallLock1.IsDropped = 1
		PlaySoundAt SoundFX("TotemDropDown", DOFContactors), BallLock1P
		BallLock1P.Z = 0
	End If
End Sub

' *** LONNIE
Sub solLonnie(enabled)
	If Enabled then
		Lonnie.RotX= 0
		PlaySoundAt SoundFX(SSolenoidOn, DOFContactors), Lonnie
	 Else
		Lonnie.RotX= 10
	End If
End Sub

' *** MARIA
  Sub solMaria(enabled)
	If Enabled then
		Maria.RotX= 0
		PlaySoundAt SoundFX(SSolenoidOn, DOFContactors), Maria
	 Else
		Maria.RotX= 10
	End If
  End Sub

' *** KEITH
  Sub solKeith(enabled)
		If Enabled then
		Keith.RotX= 0
		PlaySoundAt SoundFX(SSolenoidOn, DOFContactors), Keith
	 Else
		Keith.RotX= 10
	End If
  End Sub

'************************************************************************
'						 	SWITCHES
'************************************************************************

' *** SLINGSHOTS
Dim LStep, RStep

Sub LeftSlingShot_Slingshot()
	PlaySoundAt SoundFX("leftslingshot",DOFContactors), gi3
    LSling1.Visible = 1
    Lemk.TransZ = -20
    LStep = 0
    vpmTimer.PulseSw 26
    Me.TimerInterval = 20:Me.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer()
    Select Case LStep
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:Lemk.TransZ = -10
        Case 3:LSLing2.Visible = 0:Lemk.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot()
	PlaySoundAt SoundFX("rightslingshot",DOFContactors), gi1
    RSling1.Visible = 1
    Remk.TransZ = -20
    RStep = 0
    vpmTimer.PulseSw 27
    Me.TimerInterval = 20:Me.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer()
    Select Case RStep
        Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:Remk.TransZ = -10
        Case 3:RSLing2.Visible = 0:Remk.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' *** BUMPERS
 Sub Bumper1_Hit():vpmTimer.PulseSw 31:End Sub
 Sub Bumper2_Hit():vpmTimer.PulseSw 30:End Sub
 Sub Bumper3_Hit():vpmTimer.PulseSw 32:End Sub

' *** ROLLOVERS
   Sub sw1_Hit:Controller.Switch(1) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw1_UnHit:Controller.Switch(1) = 0:End Sub
   Sub sw2_Hit:Controller.Switch(2) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw2_UnHit:Controller.Switch(2) = 0:End Sub
   Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub
   Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
   Sub sw28_Hit:Controller.Switch(28) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
   Sub sw29_Hit:Controller.Switch(29) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw29_UnHit:Controller.Switch(29) = 0:End Sub
   Sub sw42_Hit:Controller.Switch(42) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub
   Sub sw43_Hit:Controller.Switch(43) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub
   Sub sw45_Hit:Controller.Switch(45) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw45_UnHit:Controller.Switch(45) = 0:End Sub
   Sub sw57_Hit:Controller.Switch(57) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
   Sub sw58_Hit:Controller.Switch(58) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
   Sub sw59_Hit:Controller.Switch(59) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw59_UnHit:Controller.Switch(59) = 0:End Sub
   Sub sw60_Hit:Controller.Switch(60) = 1:PlaySoundAt "fx_sensor", ActiveBall:End Sub
   Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub

   Sub sw33_Hit:Controller.Switch(33) = 1:End Sub
   Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub

' *** OPTOS
   Sub sw6_Hit:Controller.Switch(6) = 1:End Sub
   Sub sw6_UnHit:Controller.Switch(6) = 0:End Sub
   Sub sw50_Hit:Controller.Switch(50) = 1:End Sub
   Sub sw50_UnHit:Controller.Switch(50) = 0:End Sub
   Sub sw51_Hit:Controller.Switch(51) = 1:PlaySoundAt "fx_ramp_enter", ActiveBall:End Sub
   Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub
   Sub sw52_Hit:Controller.Switch(52) = 1:End Sub
   Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub
   Sub sw53_Hit:Controller.Switch(53) = 1:PlaySoundAt "fx_ramp_metal", ActiveBall:End Sub
   Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub


' *** STANDUP TARGETS
  Sub sw7_Hit:vpmTimer.PulseSw 7:End Sub
  Sub sw8_Hit:vpmTimer.PulseSw 8:End Sub
  Sub sw9_Hit:vpmTimer.PulseSw 9:End Sub
  Sub sw10_Hit:vpmTimer.PulseSw 10:End Sub
  Sub sw11_Hit:vpmTimer.PulseSw 11:End Sub
  Sub sw12_Hit:vpmTimer.PulseSw 12:End Sub
  Sub sw13_Hit:vpmTimer.PulseSw 13:End Sub
  Sub sw14_Hit:vpmTimer.PulseSw 14:End Sub
  Sub sw36_Hit:vpmTimer.PulseSw 36:End Sub
  Sub sw37_Hit:vpmTimer.PulseSw 37:End Sub
  Sub sw38_Hit:vpmTimer.PulseSw 38:End Sub
  Sub sw61_Hit:vpmTimer.PulseSw 61:End Sub

' *** DROP TARGETS
  Sub sw39_dropped:DTBank.Hit 1:End Sub
  Sub sw40_dropped:DTBank.Hit 2:End Sub
  Sub sw41_dropped:DTBank.Hit 3:End Sub

'*****************************************
'           REALTIME UPDATES
'*****************************************

Sub GameTimer_Timer()
	UpdateWheel
	UpdateMiniDMD
	UpdateGates
	UpdateFlipperShadow
	UpdateBallShadow
	RollingSoundUpdate
End Sub

' *** WHEEL
Sub UpdateWheel()
	WheelP.RotY = Controller.GetMech(0)
End Sub

' *** FLIPPER SHADOWS
sub UpdateFlipperShadow()
    FlipperLSh.RotZ = LeftFlipper.currentangle
    FlipperRSh.RotZ = RightFlipper.currentangle
End Sub
' *** GATES PRIMITIVES
Sub UpdateGates
	Primitive15.RotX = Gate1.currentangle
	Primitive17.RotX = Gate2.currentangle
	Primitive12.RotX = Gate3.currentangle
	Primitive10.RotX = Gate4.currentangle
	Primitive16.RotX = Gate5.currentangle
	Primitive14.RotX = Gate6.currentangle
	Primitive13.RotX = Gate7.currentangle
	Primitive18.RotX = Gate8.currentangle
End Sub

' *** ROLLING SOUND
Const tnob = 4
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls
	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 Then
            rolling(b) = True
			if BOT(b).z < 30 then 
				If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
					PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b) ) * .5 , Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b))	
				Else
					PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b) ) * .5 , Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
				End If
			Else ' ball in air, probably on plastic.  
				If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
					PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b) ) * .2  , Pan(BOT(b) ), 0, Pitch(BOT(b) ) + 40000, 1, 0, AudioFade(BOT(b))	
				Else
					PlaySound "fx_ballrolling" & b, -1, Vol(BOT(b) ) * .2 , Pan(BOT(b) ), 0, Pitch(BOT(b) ) + 40000, 1, 0
				End If
			End If
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

' *** BALL SHADOW
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4)

Sub UpdateBallShadow()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (table1.Width/2))/7)) + 10
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (table1.Width/2))/7)) - 10
        End If
        ballShadow(b).Y = BOT(b).Y + 20
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

' *** Mini-DMD

Dim LED(69)

LED(0)=Array(D1,D2,D3,D4,D5)
LED(1)=Array(D6,D7,D8,D9,D10)
LED(2)=Array(D11,D12,D13,D14,D15)
LED(3)=Array(D16,D17,D18,D19,D20)
LED(4)=Array(D21,D22,D23,D24,D25)
LED(5)=Array(D26,D27,D28,D29,D30)
LED(6)=Array(D31,D32,D33,D34,D35)
LED(7)=Array(D36,D37,D38,D39,D40)
LED(8)=Array(D41,D42,D43,D44,D45)
LED(9)=Array(D46,D47,D48,D49,D50)
LED(10)=Array(D51,D52,D53,D54,D55)
LED(11)=Array(D56,D57,D58,D59,D60)
LED(12)=Array(D61,D62,D63,D64,D65)
LED(13)=Array(D66,D67,D68,D69,D70)
LED(14)=Array(D71,D72,D73,D74,D75)
LED(15)=Array(D76,D77,D78,D79,D80)
LED(16)=Array(D81,D82,D83,D84,D85)
LED(17)=Array(D86,D87,D88,D89,D90)
LED(18)=Array(D91,D92,D93,D94,D95)
LED(19)=Array(D96,D97,D98,D99,D100)
LED(20)=Array(D101,D102,D103,D104,D105)
LED(21)=Array(D106,D107,D108,D109,D110)
LED(22)=Array(D111,D112,D113,D114,D115)
LED(23)=Array(D116,D117,D118,D119,D120)
LED(24)=Array(D121,D122,D123,D124,D125)
LED(25)=Array(D126,D127,D128,D129,D130)
LED(26)=Array(D131,D132,D133,D134,D135)
LED(27)=Array(D136,D137,D138,D139,D140)
LED(28)=Array(D141,D142,D143,D144,D145)
LED(29)=Array(D146,D147,D148,D149,D150)
LED(30)=Array(D151,D152,D153,D154,D155)
LED(31)=Array(D156,D157,D158,D159,D160)
LED(32)=Array(D161,D162,D163,D164,D165)
LED(33)=Array(D166,D167,D168,D169,D170)
LED(34)=Array(D171,D172,D173,D174,D175)

Sub UpdateMiniDMD
    dim ii
	IF UseModulatedMiniDMD Then
		dim jj, kk, target, tobj
		for jj = 0 to 4
			for ii = 0 to 6
				for kk = 0 to 4
					target = (kk * 35) + (jj * 7) + ii + 141
					LED(jj * 7 + ii)(4-kk).IntensityScale = (FadingLevel(target)/255) ^ 1.5
					LED(jj * 7 + ii)(4-kk).State = LampState(target)
				Next
			Next   
		Next
	Else
		Dim ChgLED,num,chg,stat,obj
		ChgLED=Controller.ChangedLEDs(&Hffffffff,&Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii,0):chg=chgLED(ii,1):stat=chgLED(ii,2)
				For Each obj In LED(num)
					If chg And 1 Then 
						if stat And 1 Then
							obj.state = 1
						Else
							obj.state = 0
						End If
					End If
					chg=chg\2:stat=stat\2
				Next
			Next
		End If
    End If
End Sub


'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' 		With Modulated Flasher Routines (ninuzzu)
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************
Dim LampState(610), FadingLevel(610)
Dim FlashSpeedUp(610), FlashSpeedDown(610), FlashMin(610), FlashMax(610), FlashLevel(610)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
	UpdateLamps
End Sub

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4       ' used to track the fading state
        FlashSpeedUp(x) = 0.5    ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.35 ' slower speed when turning off the flasher
        FlashMax(x) = 1          ' the maximum value when on, usually 1
        FlashMin(x) = 0          ' the minimum value when off, usually 0
        FlashLevel(x) = 0        ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub UpdateLamps
'Inserts
'	nFadeL 1, l1		'start
'	nFadeL 2, l2		'tournament
	NFadeL 3, l3
	NFadeL 4, l4
	NFadeL 5, l5
	NFadeL 6, l6
	NFadeL 7, l7
	NFadeL 8, l8
	NFadeL 9, l9
	NFadeL 10, l10
	NFadeL 11, l11
	NFadeL 12, l12
	NFadeL 13, l13
	NFadeL 14, l14
	NFadeL 15, l15
	NFadeL 16, l16
	NFadeL 17, l17
	NFadeL 18, l18
	NFadeL 19, l19
	NFadeL 20, l20
	NFadeL 21, l21
	NFadeL 22, l22
	NFadeL 23, l23
	NFadeL 24, l24
	NFadeL 25, l25
	NFadeL 26, l26
	NFadeL 27, l27
	NFadeL 28, l28
	NFadeL 29, l29
	NFadeL 30, l30
	NFadeL 31, l31
	NFadeL 32, l32
	NFadeL 33, l33
	NFadeL 34, l34
	NFadeL 35, l35
	NFadeL 36, l36
	NFadeL 37, l37
	NFadeL 38, l38
	NFadeL 39, l39
	NFadeL 40, l40
	Flash 41, F41
	LampMod 41, bulb11
	Flash 42, F42
	LampMod 42, bulb12
	Flash 43, F43
	LampMod 43, bulb13
	Flash 44, F44
	LampMod 44, bulb14
	NFadeL 45, l45
	NFadeL 46, l46
	NFadeL 47, l47
	NFadeL 48, l48
	NFadeL 49, l49
	NFadeL 50, l50
	NFadeL 51, l51
	NFadeL 52, l52
	NFadeL 53, l53
	NFadeL 54, l54
	NFadeL 55, l55
	NFadeL 56, l56
	NFadeL 57, l57
	NFadeL 58, l58
	NFadeL 59, l59
	NFadeLm 60, l60
	NFadeLm 60, l60a
	NFadeL 60, l60b
	NFadeLm 61, l61
	NFadeLm 61, l61a
	NFadeL 61, l61b
	NFadeLm 62, l62
	NFadeLm 62, l62a
	NFadeL 62, l62b
	NFadeL 63, l63
	NFadeL 64, l64
	NFadeL 65, l65
	NFadeL 66, l66
	NFadeL 67, l67
	NFadeL 68, l68
	NFadeL 69, l69
	NFadeL 70, l70
	NFadeL 71, l71
	NFadeL 72, l72
	NFadeL 73, l73
	NFadeL 74, l74
	NFadeL 75, l75
	NFadeL 76, l76
	NFadeL 77, l77
	NFadeL 78, l78
	NFadeL 79, l79
	NFadeL 80, l80

'Wheel LEDs
'	Flash 81,		'?????????
	Flash 82, WLedRedWedge '#1
'	Flash 83, WLedRedWedge '#2
'	Flash 84, WLedRedWedge '#3
	Flash 85, WLedGIL		'Left GI
	Flash 86, WLedGIR		'Right GI
'	Flash 87,		'?????????
'	Flash 89, WLedYellowWedge '#1
	Flash 90, WLedBlueWedge '#1
'	Flash 91, WLedBlueWedge '#2
'	Flash 92, WLedBlueWedge '#3

'	Flash 93, WLedYellowWedge '#2
	Flash 94, WLedYellowWedge '#3
'	Flash 95,		'?????????
	Flash 105, WLedRedContestant
	Flash 109, WLedYellowContestant
	Flash 110, WLedBlueContestant
'	Flash 111,		'?????????

'Wheel Border LEDs
	Flashm 106,WLed1:Flash 106,WBorder1
	Flashm 100,WLed2:Flash 100,WBorder2
	Flashm 99,WLed3:Flash 99,WBorder3
	Flashm 98,WLed4:Flash 98,WBorder4
	Flashm 97,WLed5:Flash 97,WBorder5
	Flashm 101,WLed6:Flash 101,WBorder6
	Flashm 102,WLed7:Flash 102,WBorder7
	Flashm 103,WLed8:Flash 103,WBorder8
	Flashm 124,WLed9:Flash 124,WBorder9
	Flashm 123,WLed10:Flash 123,WBorder10
	Flashm 122,WLed11:Flash 122,WBorder11
	Flashm 121,WLed12:Flash 121,WBorder12
	Flashm 125,WLed13:Flash 125,WBorder13
	Flashm 126,WLed14:Flash 126,WBorder14
	Flashm 127,WLed15:Flash 127,WBorder15
	Flashm 116,WLed16:Flash 116,WBorder16
	Flashm 115,WLed17:Flash 115,WBorder17
	Flashm 114,WLed18:Flash 114,WBorder18
	Flashm 113,WLed19:Flash 113,WBorder19
	Flashm 117,WLed20:Flash 117,WBorder20
	Flashm 118,WLed21:Flash 118,WBorder21
	Flashm 119,WLed22:Flash 119,WBorder22
	Flashm 108,WLed23:Flash 108,WBorder23
	Flashm 107,WLed24:Flash 107,WBorder24

'Flashers
	LampMod 129, FL19			'Flasher:Red Contestant  
	LampMod 130, FL20			'Flasher:Yellow Contestant
	LampMod 131, FL21			'Flasher:Blue Contestant
	LampMod 135, FL25a			'Flasher:Wheel #1
	LampMod 135, FL25b			'Flasher:Wheel #2
	LampMod 135, FL25c			'Flasher:Wheel #3
	LampMod 135, FL25d			'Flasher:Wheel #4
	LampMod 136, FL26			'Flasher:Backpanel Left #1
	LampMod 136, FL26a			'Flasher:Backpanel Left #2
	LampMod 136, FL26b			'Flasher:Backpanel Left #3
	LampMod 137, FL27			'Flasher:Left Orbit
	LampMod 138, FL28			'Flasher:Right Ramp
	LampMod 139, FL29			'Flasher:Right Orbit
	LampMod 132, FL30			'Flasher:Pop Bumper #1
	LampMod 132, FL30a			'Flasher:Pop Bumper #2
	LampMod 133, FL31			'Flasher:Mini Ramp
	LampMod 134, FL32			'Flasher:Backpanel Right #1
	LampMod 134, FL32a			'Flasher:Backpanel Right #2
	LampMod 134, FL32b			'Flasher:Backpanel Right #3

End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1            'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0	'off
        Case 5:object.image = a:FadingLevel(nr) = 1		'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = FlashLevel(nr)
End Sub

' Modulated Flasher and Lights objects

Sub SetLampMod(nr, value)
    If value > 0 Then
		LampState(nr) = 1
	Else
		LampState(nr) = 0
	End If
	FadingLevel(nr) = value
End Sub

Sub LampMod(nr, object)
	If TypeName(object) = "Light" Then
		Object.IntensityScale = FadingLevel(nr)/128
		Object.State = LampState(nr)
	End If
	If TypeName(object) = "Flasher" Then
		Object.IntensityScale = FadingLevel(nr)/128
		Object.visible = LampState(nr)
	End If
	If TypeName(object) = "Primitive" Then
		Object.DisableLighting = LampState(nr)
	End If
End Sub

' *********************************************************************
'                 General Illumination
' *********************************************************************
set GICallback = GetRef("UpdateGI")

Sub UpdateGI(nr,enabled)
	Dim xx
	Select Case nr
		Case 0
		If Enabled Then
			DOF 101, DOFOn
			Table1.ColorGradeImage= "ColorGrade_8"
			For Each xx in GI:xx.State=1:Next
			For Each xx in BWGI:xx.visible=1:Next
			For xx = 1 to 10
				ExecuteGlobal "bulb" & xx & ".image = ""bulb_orange"":bulb" & xx & ".DisableLighting = True"
			Next
			Spot1.visible=1:Spot2.visible=1:Spot3.visible=1:Spot4.visible=1
			Playsound "fx_relay_on"
		Else
			DOF 101, DOFOff
			Table1.ColorGradeImage= "ColorGrade_1"
			For Each xx in GI:xx.State=0:Next
			For Each xx in BWGI:xx.visible=0:Next
			For xx = 1 to 10
				ExecuteGlobal "bulb" & xx & ".image = ""bulb"":bulb" & xx & ".DisableLighting = False"
			Next
			Spot1.visible=0:Spot2.visible=0:Spot3.visible=0:Spot4.visible=0
			Playsound "fx_relay_off"
		End If
	End Select
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 5000)
	'Debug.Print Vol
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

function AudioFade(ball)
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Sub PlaySoundAt(sound, tableobj)
	If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
		PlaySound sound, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
	Else
		PlaySound sound, 1, 1, Pan(tableobj)
	End If
End Sub

Sub PlaySoundAtBall(sound)
	If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
		PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
	Else
		PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
	End If
End Sub

'*****************************************
'      		COLLISON SOUND EFFECTS
'*****************************************

' left ramp sounds
Sub LRHit0_Hit:PlaySoundAt "fx_lr1", ActiveBall:End Sub
Sub LRHit1_Hit:PlaySoundAt "fx_lr2", ActiveBall:End Sub
Sub LRHit2_Hit:PlaySoundAt "fx_lr3", ActiveBall:End Sub
Sub LRHit3_Hit:PlaySoundAt "fx_lr4", ActiveBall:End Sub
Sub LRHit4_Hit:PlaySoundAt "fx_lr5", ActiveBall:End Sub
Sub LRHit5_Hit:PlaySoundAt "fx_lr6", ActiveBall:End Sub
Sub LRHit6_Hit:PlaySoundAt "fx_lr7", ActiveBall:End Sub

'right ramp sounds
Sub RRHit0_Hit:PlaySoundAt "fx_rr1", ActiveBall:End Sub
Sub RRHit1_Hit:PlaySoundAt "fx_rr2", ActiveBall:End Sub
Sub RRHit2_Hit:PlaySoundAt "fx_rr3", ActiveBall:End Sub
Sub RRHit3_Hit:PlaySoundAt "fx_rr4", ActiveBall:End Sub
Sub RRHit4_Hit:PlaySoundAt "fx_rr5", ActiveBall:End Sub
Sub RRHit5_Hit:PlaySoundAt "fx_rr6", ActiveBall:End Sub
Sub RRHit6_Hit:PlaySoundAt "fx_lr1", ActiveBall:End Sub
Sub RRHit7_Hit:PlaySoundAt "fx_lr1", ActiveBall:End Sub
Sub RRHit8_Hit:PlaySoundAt "fx_rr7", ActiveBall:End Sub

Sub RampExit1_Hit:Me.TimerInterval=100:Me.TimerEnabled=1:End Sub
Sub RampExit1_timer:Me.TimerEnabled=0:PlaySoundAt "balldrop",RampExit1:End Sub
Sub RampExit2_Hit:Me.TimerInterval=100:Me.TimerEnabled=1:End Sub
Sub RampExit2_timer:Me.TimerEnabled=0:PlaySoundAt "balldrop", RampExit2:End Sub
Sub RampExit3_Hit:StopSound "fx_ramp_metal":Me.TimerInterval=100:Me.TimerEnabled=1:End Sub
Sub RampExit3_timer:Me.TimerEnabled=0:PlaySoundAt "fx_balldrop_ramp", RampExit3:End Sub
Sub RampExit4_Hit:Me.TimerInterval=100:Me.TimerEnabled=1:End Sub
Sub RampExit4_timer:Me.TimerEnabled=0:PlaySoundAt "balldrop", RampExit4:End Sub

Sub OnBallBallCollision(ball1, ball2, velocity)
	If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
		PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
	else
		PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 500, Pan(ball1), 0, Pitch(ball1), 0, 0
	end if
End Sub

Sub Bumpers_Hit (idx)
PlaySoundAt SoundFX("PopBumper",DOFContactors), ActiveBall
End Sub

Sub Targets_Hit (idx)
PlaySoundAt SoundFX("target",DOFTargets), ActiveBall
End Sub

Sub Gates_Hit (idx)
	PlaySoundAt "fx_gate4", ActiveBall
End Sub

Sub PrimitiveURamps_Hit
	PlaySoundAt "fx_gate4", ActiveBall
End Sub

Sub BallLock_Hit
	PlaySoundAt "fx_sensor", ActiveBall
End Sub

Sub BallLock1_Hit
	PlaySoundAt "fx_sensor", ActiveBall
End Sub


Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySoundAtBall "fx_rubber"
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySoundAtBall "fx_rubber"
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtBall "fx_rubber_hit_1"
		Case 2 : PlaySoundAtBall "fx_rubber_hit_2"
		Case 3 : PlaySoundAtBall "fx_rubber_hit_3"
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtBall "fx_flip_hit_1"
		Case 2 : PlaySoundAtBall "fx_flip_hit_2"
		Case 3 : PlaySoundAtBall "fx_flip_hit_3"
	End Select
End Sub

'*****************************************
'      	INIT WHEEL LIGHTS POSITION
'*****************************************

Const Pi=3.1415926535
Const LedRadius = 141.75

Sub InitWheelLights
dim xx
'Wheel Border LEDs #1
For xx = 1 to 24
WheelLEDs(xx-1).X = WheelP.X + LedRadius * Sin((xx-1) * 15 * Pi/180) 
WheelLEDs(xx-1).Y = WheelP.Y - LedRadius * Cos((xx-1) * 15 * Pi/180) * sin (WheelP.objrotx * Pi/180) -5
WheelLEDs(xx-1).Height = WheelP.Z + LedRadius * Cos((xx-1) * 15 * Pi/180) * cos (WheelP.objrotx * Pi/180)
WheelLEDs(xx-1).RotX = WheelP.objrotx -90
Next
'Wheel Border LEDs #2
For xx = 1 to 24
WheelBorder(xx-1).X = WheelP.X + LedRadius * Sin((xx-1) * 15 * Pi/180) 
WheelBorder(xx-1).Y = WheelP.Y - LedRadius * Cos((xx-1) * 15 * Pi/180) * sin (WheelP.objrotx * Pi/180)-5
WheelBorder(xx-1).Height = WheelP.Z + LedRadius * Cos((xx-1) * 15 * Pi/180) * cos (WheelP.objrotx * Pi/180)
WheelBorder(xx-1).RotX = WheelP.objrotx -90
Next
'Wheel Wedge R-Y-B LEDs
WLedRedWedge.X = WheelP.X+10:WLedRedWedge.Y=WheelP.Y +1:WLedRedWedge.height = WheelP.Z:WLedRedWedge.RotX = WheelP.objrotx -90
WLedYellowWedge.X = WheelP.X:WLedYellowWedge.Y=WheelP.Y +1:WLedYellowWedge.height = WheelP.Z:WLedYellowWedge.RotX = WheelP.objrotx -90
WLedBLueWedge.X = WheelP.X-10:WLedBLueWedge.Y=WheelP.Y +1:WLedBLueWedge.height = WheelP.Z:WLedBLueWedge.RotX = WheelP.objrotx -90
'Wheel Contenstant R-Y-B LEDs
WLedRedContestant.X = WheelP.X+10:WLedRedContestant.Y=WheelP.Y +15*cos(WheelP.objrotx*Pi/180):WLedRedContestant.height = WheelP.Z+15*(1+sin(WheelP.objrotx*Pi/180)):WLedRedContestant.RotX = WheelP.objrotx -90
WLedYellowContestant.X = WheelP.X:WLedYellowContestant.Y=WheelP.Y +15*cos(WheelP.objrotx*Pi/180):WLedYellowContestant.height = WheelP.Z+15*(1+sin(WheelP.objrotx*Pi/180)):WLedYellowContestant.RotX = WheelP.objrotx -90
WLedBlueContestant.X = WheelP.X-10:WLedBlueContestant.Y=WheelP.Y +15*cos(WheelP.objrotx*Pi/180):WLedBlueContestant.height = WheelP.Z+15*(1+sin(WheelP.objrotx*Pi/180)):WLedBlueContestant.RotX = WheelP.objrotx -90
'GI:Wheel Left
WLedGIR.X = WheelP.X + 80 * Sin(150 * Pi/180) 
WLedGIR.Y = WheelP.Y - 80 * Cos(150 * Pi/180) * sin (WheelP.objrotx * Pi/180) + 5
WLedGIR.Height = WheelP.Z + 90 * Cos(150 * Pi/180) * cos (WheelP.objrotx * Pi/180)
WLedGIR.RotX = WheelP.objrotx -90
'GI:Wheel Right
WLedGIL.X = WheelP.X + 80 * Sin(210 * Pi/180) 
WLedGIL.Y = WheelP.Y - 80 * Cos(210 * Pi/180) * sin (WheelP.objrotx * Pi/180) + 5
WLedGIL.Height = WheelP.Z + 90 * Cos(210 * Pi/180) * cos (WheelP.objrotx * Pi/180)
WLedGIL.RotX = WheelP.objrotx -90
'Flasher:Wheel #1
FL25a.X = WheelP.X + 90 * Sin(-90 * Pi/180) 
FL25a.Y = WheelP.Y - 90 * Cos(-90 * Pi/180) * sin (WheelP.objrotx * Pi/180) + 5
FL25a.Height = WheelP.Z + 90 * Cos(-90 * Pi/180) * cos (WheelP.objrotx * Pi/180)
FL25a.RotX = WheelP.objrotx -90
'Flasher:Wheel #2
FL25b.X = WheelP.X + 90 * Sin(90 * Pi/180) 
FL25b.Y = WheelP.Y - 90 * Cos(90 * Pi/180) * sin (WheelP.objrotx * Pi/180) + 5
FL25b.Height = WheelP.Z + 90 * Cos(90 * Pi/180) * cos (WheelP.objrotx * Pi/180)
FL25b.RotX = WheelP.objrotx -90
'Flasher:Wheel #3
FL25c.X = WheelP.X + 90 * Sin(0 * Pi/180) 
FL25c.Y = WheelP.Y - 90 * Cos(0 * Pi/180) * sin (WheelP.objrotx * Pi/180) + 5
FL25c.Height = WheelP.Z + 90 * Cos(0 * Pi/180) * cos (WheelP.objrotx * Pi/180)
FL25c.RotX = WheelP.objrotx -90
'Flasher:Wheel #4
FL25d.X = WheelP.X + 90 * Sin(180 * Pi/180) 
FL25d.Y = WheelP.Y - 90 * Cos(180 * Pi/180) * sin (WheelP.objrotx * Pi/180) + 5
FL25d.Height = WheelP.Z + 90 * Cos(180 * Pi/180) * cos (WheelP.objrotx * Pi/180)
FL25d.RotX = WheelP.objrotx -90
End Sub

