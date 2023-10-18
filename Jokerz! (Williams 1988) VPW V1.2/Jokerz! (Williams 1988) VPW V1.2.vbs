'.------..------..------..------..------..------..------.
'|J.--. ||O.--. ||K.--. ||E.--. ||R.--. ||Z.--. ||!.--. |
'| :(): || :/\: || :/\: || (\/) || :(): || :(): || (\/) |
'| ()() || :\/: || :\/: || :\/: || ()() || ()() || :\/: |
'| '--'J|| '--'O|| '--'K|| '--'E|| '--'R|| '--'Z|| '--'!|
'`------'`------'`------'`------'`------'`------'`------'
'
'Jokerz! (Williams 1988)
'https://www.ipdb.org/machine.cgi?id=1308
'
'************
' VPW Jokerz
'************
'Project Lead & Rendering - Tomate
'VPX Monkey - Sixtoe
'Center Ramp Mech - Apophis
'Physics Tweaks - iaakki, bord, clarkkent
'Random Scripting Stuff - Apophis, iaakki
'Fluppers LUT Selector - fluffhead35
'VR Cabinet from VR version by Sixtoe, Walter Whitmer & DJRobX
'VR Backglass Card Deck from VR version by DJRobX
'VR Backbox - Leojreimroc
'Desktop Card Deck from JP's Jokerz
'
Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'///////////////////// ---- VR Mode ---- /////////////////////
Const VRRoom = 0					'0 - Off, 1 - Minimal Room, 2 - Ultra Minimal

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'----- General Sound Options -----
Const VolumeDial = 0.8				'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'Level of ramp rolling volume. Value between 0 and 1

' ****** LUT overall contrast & brightness setting **************************************************************************************
Dim luts, lutpos
luts = array("ColorGradeLUT256x16_jokerz", "ColorGradeLUT256x16_ConSat", "ColorGradeLUT256x16_ConSatgam")
lutpos = 0						'  set the nr of the LUT you want to use (0 = first in the list above, 1 = second, etc); 0 is the default
Const EnableMagnasave = 1		' 1 - on; 0 - off; if on then the magnasave button let's you rotate all LUT's

'*******************************************
'  Constants and Global Variables
'*******************************************
Const BallSize = 50			'Ball size must be 50
Const BallMass = 1			'Ball mass must be 1
Const tnob = 5				'Total number of balls
Const lob = 0				'Locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim CabinetMode, DesktopMode: DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode
If Not DesktopMode and VRRoom=0 Then CabinetMode=1 Else CabinetMode=0

LoadVPM "01530000", "S11.VBS", 3.10

'**********************
' Standard definitions
'**********************
Const cGameName = "jokrz_l6"
Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SCoin = ""

'********
' Timers
'********
Sub Gametimer_Timer
	RollingUpdate
	CheckBallLocations
End Sub

Sub Frametimer_Timer 
	LeftFlipper_on.RotY = LeftFlipper.currentangle
	RightFlipper_on.RotY = RightFlipper.currentangle
	LeftFlipper_off.RotY = LeftFlipper.currentangle
	RightFlipper_off.RotY = RightFlipper.currentangle

	If f122a.IntensityScale > 0 then Drawbridge_on_flash.visible = 1 else Drawbridge_on_flash.visible = 0
	Spinner1_Mount.RotZ = -(sw48.currentangle)
	Spinner_On.RotZ = -(sw48.currentangle)
	Spinner_Off.RotZ = -(sw48.currentangle)

	UpdateBallBrightness
	Displaytimer
	
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows

	gate2p.RotZ = -(gate2.currentangle)
	gate3p.RotZ = -(gate3.currentangle)
	gate5p.RotZ = -(gate5.currentangle)
	gate6p.RotZ = -(gate6.currentangle)
	GateUpperLp.RotZ = -(GateUpperL.currentangle)
	GateUpperRp.RotZ = -(GateUpperR.currentangle)

	sw25p_off.transz = sw25p.transz
	sw25p_off.rotx = sw25p.rotx
	sw25p_off.roty = sw25p.roty

	sw26p_off.transz = sw26p.transz
	sw26p_off.rotx = sw26p.rotx
	sw26p_off.roty = sw26p.roty

	sw27p_off.transz = sw27p.transz
	sw27p_off.rotx = sw27p.rotx
	sw27p_off.roty = sw27p.roty

	sw41p_off.transz = sw41p.transz
	sw41p_off.rotx = sw41p.rotx
	sw41p_off.roty = sw41p.roty

	sw42p_off.transz = sw42p.transz
	sw42p_off.rotx = sw42p.rotx
	sw42p_off.roty = sw42p.roty

	sw43p_off.transz = sw43p.transz
	sw43p_off.rotx = sw43p.rotx
	sw43p_off.roty = sw43p.roty

	sw36p_off.transz = sw36p.transz
	sw36p_off.rotx = sw36p.rotx
	sw36p_off.roty = sw36p.roty

	sw37p_off.transz = sw37p.transz
	sw37p_off.rotx = sw37p.rotx
	sw37p_off.roty = sw37p.roty

	sw38p_off.transz = sw38p.transz
	sw38p_off.rotx = sw38p.rotx
	sw38p_off.roty = sw38p.roty

End Sub

'************
' Table init.
'************
Dim x
Dim bsLEject, mWheel
Dim JKBall1, JKBall2, JKBall3, gBOT
Dim DisplayColor, DisplayColorG
DisplayColor =  RGB(255,40,1)

Sub table1_Init()
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game: " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Jokerz! (Williams 1988)" & vbNewLine & "VPW"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

	'************  Trough	**************
	Set JKBall3 = sw11.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set JKBall2 = sw12.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set JKBall1 = sw13.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(JKBall1,JKBall2,JKBall3)

	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1

    ' Nudging
    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    vpmTimer.InitTimer FastTimer, true

If vrroom > 0 then
    Set mWheel = new cvpmMyMech
    With mWheel
        .Length = 200
        .Steps = 360
        .mType = vpmMechStepSol + vpmMechCircle + vpmMechLinear + vpmMechFast
        .Sol1 = 14
        .Sol2 = 13
        .Addsw 59, 0, 180
        .Callback = GetRef("UpdateWheel")
        .Start
    End With
Else
    Set mWheel = new cvpmMech
    With mWheel
        .Length = 180
		if DesktopMode then
			.Steps = 32
		Else
			.Steps = 16
		end if
        .mType = vpmMechStepSol + vpmMechCircle + vpmMechLinear + vpmMechFast
        .Sol1 = 14
        .Sol2 = 13
        .Addsw 59, 0, 10
        .Callback = GetRef("UpdateWheel")
        .Start
    End With
End If

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    InitCenterRamp

	'Top Left Eject
    Set bsLEject = new cvpmSaucer
    With bsLEject
        .InitKicker Sw44, 44, 20, 26, 0
		.InitSounds "Saucer_Enter_2", SoundFX("LeftEject",DOFContactors), SoundFX("LeftEject",DOFContactors)
        .CreateEvents "bsLEject", Sw44
    End With

Dim six
	For each six in off_prims:six.blenddisablelighting = 1 : Next
	If desktopmode and vrroom = 0 then
		For each six in DT_LED:six.visible = 1 : Next
		pokercards.visible = 1
	Else 
		For each six in DT_LED:six.visible = 0 : Next
		pokercards.visible = 0
	End If
End Sub

'Right VUK
Dim KickerBall47

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180

	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

Sub sw47_Hit
    set KickerBall47 = activeball
	Controller.Switch(47) = 1
    SoundSaucerLock
End Sub

Sub bsREject(Enable)
    If Enable then
		If Controller.Switch(47) <> 0 Then
			KickBall KickerBall47, 300, 40, 50, 60
			SoundSaucerKick 1, sw47
			Controller.Switch(47) = 0
'			g01_rightpost.z=40
'			g01_rightpost_off.z=40
'			RVUKAnim.Enabled = True
		End If
	End If
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

Dim RMSEnable: RMSEnable = False

Sub Table1_KeyDown(ByVal keycode)
	If Keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If Keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	If keycode = PlungerKey Then Plunger.Pullback : SoundPlungerPull
	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter
	If keycode = StartGameKey Then SoundStartButton
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
	If VRRoom > 0 Then
		If Keycode = LeftFlipperKey Then
			VRFlipperButtonLeft.x = VRFlipperButtonLeft.x + 5
		End If
		If Keycode = RightFlipperKey Then
			VRFlipperButtonRight.x = VRFlipperButtonRight.x - 5
		End If
		If Keycode = StartGameKey Then
			VR_StartButton.y = -4
		End If
		If Keycode = PlungerKey Then
			TimerVRPlunger.Enabled = True
			TimerVRPlunger1.Enabled = False
		End If			
	End If

	If keycode = RightMagnaSave and EnableMagnasave = 1 and RMSEnable = True then
		lutpos = lutpos + 1 : If lutpos > ubound(luts) Then lutpos = 0 : end if
        call myChangeLut
		playsound "LutChange"
	End if

	If keycode = LeftMagnaSave and EnableMagnasave = 1 then
		RMSEnable = True
'		lutpos = lutpos - 1 : If lutpos < 0 Then lutpos = ubound(luts) : end if 
'        call myChangeLut
		playsound "LUT_Toggle_Down_Front"
    end if

	If vpmKeyDown(keycode) Then Exit Sub
End Sub
 
Sub Table1_KeyUp(ByVal keycode)
	If Keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If Keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
'	If Keycode = StartGameKey Then Controller.Switch(16) = 0
	If keycode = PlungerKey Then Plunger.Fire : SoundPlungerReleaseBall
	If VRRoom > 0 Then
		If Keycode = LeftFlipperKey Then
			VRFlipperButtonLeft.x = 2107.845
		End If
		If Keycode = RightFlipperKey Then
			VRFlipperButtonRight.x = 2098.562
		End If
		If Keycode = StartGameKey Then
			VR_StartButton.y = 0
		End If
		If Keycode = PlungerKey Then
			TimerVRPlunger.Enabled = False
			TimerVRPlunger1.Enabled = True
		End If			
	End If
	If keycode = LeftMagnaSave and EnableMagnasave = 1 Then
		RMSEnable = False
		playsound "LUT_Toggle_Down_Front"
	End If
	If vpmKeyUp(keycode) Then Exit Sub
End Sub


'******************************************
'	Solenoids
'******************************************

SolCallback(2) = "SolRelease"
SolCallback(3) = "SolBLDropTgt"
SolCallback(4) = "SolBRDropTgt"
SolCallback(5) = "bsLEject.SolOut"
SolCallback(6) = "SolTLDropTgt"
SolCallback(7) = "SolBLKicker"
SolCallback(8) = "vpmSolSound SoundFX(""Knocker_1"",DOFKnocker),"
SolCallback(10) = "SolGi"
'SolCallback(16) = "bsREject.SolOut"
SolCallback(16) = "bsREject"
SolCallback(15) = "SolRamp"

' Flashers
SolCallback(9) = "FlashSol109"		'F09 Main Flasher x2 f09 central ramp

If desktopmode and vrroom > 0 then	'"Wheel Flashers" (Backbox)
	SolCallback(11) = "FlashSol111"
	else
	SolCallback(11) = "SetLamp 111,"
End If

SolCallback(22) = "SetLamp 122,"	'"Lifter Ramp Flashers"
SolCallback(25) = "Flashsol125" 	'"SetLamp 125,"	'Back Wall "JO"
SolCallback(26) = "Flashsol126" 	'"SetLamp 126,"	'Back Wall "KE"
SolCallback(27) = "Flashsol127" 	'"SetLamp 127,"	'Back Wall "RZ"
SolCallback(28) = "Flashsol128" 	'"SetLamp 128,"	'Back Wall "!"
SolCallback(29) = "FlashSol129"		'F29 "Double Scores Flashers"
SolCallback(30) = "FlashSol130"		'Main Flasher Top Left
SolCallback(31) = "FlashSol131"		'Main Flasher Left
SolCallback(32) = "FlashSol132"		'Main Flasher Right

'********
' GI
'********

dim gilvl:gilvl = 1				'this will be updated in giupdates sub and will hold the current fading level of the GI
Sub SolGi(Enabled)
    If Enabled Then
        'GIOff
		Sound_GI_Relay 0,sw52
		SetLamp 0,0
		bgbright.imageA = "backglassimage"
		bgbright.imageB = "backglassimage"
'these below should be moved to giupdates sub if still needed
'		For each x in GIBackwall:DisableLighting x, 0, 1:Next
		For each x in BGGI:x.opacity = 5:Next
		BgBright.opacity = 50
    Else
        'GiOn
		Sound_GI_Relay 1,sw52
		SetLamp 0,1
		bgbright.imageA = "bgbrighttest2"
		bgbright.imageB = "bgbrighttest2"
'these below should be moved to giupdates sub if still needed
'		For each x in GIBackwall:DisableLighting x, 20, 1:Next
		For each x in BGGI:x.opacity = 50:Next
		BgBright.opacity = 300  
    End If
End Sub

Sub SolBLKicker(Enabled)
    If Enabled Then
		SoundSaucerKick 1, sw46
        Controller.Switch(46) = 0
        Controller.Switch(45) = 0
        sw46.Kick 40, 50 + RND(1) * 5
        sw45.TimerEnabled = 1
    End If
End Sub

Sub Updatewheel(aNewPos, aSpeed, aLastPos)
    Cardwheel.Rotz = -aNewPos + 180 + 16 + 6
	Dim tmp
	tmp = aNewPos \ 2
		If tmp> 9 then tmp = tmp -1
		pokercards.SetValue(tmp)
End Sub

'****************************
' Center Ramp  
'****************************

Dim RampPos, RampDir, RampIdx, LastRampIdx, RampDelayCnt
Const RampEndstopDelay = 100

' This is called from table1_Init
Sub InitCenterRamp
	Controller.Switch(16) = 1
	RampPos = 0
	RampIdx = -1
	LastRampIdx = -1
	RampDelayCnt = 0
	dim x : for each x in CenterRamp : x.collidable = false : Next
	'vpmTimer.AddTimer 3000, "LowerRamp '"
End Sub

Sub SolRamp(Enabled)
	'debug.print "SolRamp Enabled=" & Enabled
	RampTimer.Enabled = Enabled
    If Enabled Then
		PlaySound SoundFX("Motor", DOFGear), -1
	Else
		StopSound "Motor"
    End If
End Sub

Sub RiseRamp
	Controller.Switch(16) = 0
	RampDir = 1
End Sub

Sub LowerRamp
	Controller.Switch(15) = 0
	RampDir = -1
End Sub

Sub RampIsUp
    Controller.Switch(15) = 1
	RampPos = 100
	RampDelayCnt = RampDelayCnt + 1
	If RampDelayCnt > RampEndstopDelay Then LowerRamp
End Sub

Sub RampIsDown
	Controller.Switch(16) = 1
	RampPos = 0
	RampDelayCnt = RampDelayCnt + 1
	If RampDelayCnt > RampEndstopDelay Then RiseRamp
End Sub

Sub RampTimer_Timer
    RampPos = RampPos + RampDir
    If RampPos >= 100 Then 
		RampIsUp
    ElseIf RampPos =< 0 Then 
		RampIsDown
	Else
		RampDelayCnt = 0
	End If
	'update visible ramp
	Drawbridge_on.objrotx = -1 - RampPos / 4.47
	Drawbridge_off.objrotx = -1 - RampPos / 4.47
	Drawbridge_on_flash.objrotx = -1 - RampPos / 4.47
	G02_DrawbridgeProt_On.objrotx = -1 - RampPos / 4.47
	G02_DrawbridgeProt_Off.objrotx = -1 - RampPos / 4.47
	'update collidable ramp
	RampIdx = INT(RampPos/10)-1
	If RampIdx <> LastRampIdx Then 
		'debug.print "RampIdx=" & RampIdx & " LastRampIdx=" & LastRampIdx
		If RampIdx >= 0 Then CenterRamp(RampIdx).collidable = True
		If LastRampIdx >= 0 Then CenterRamp(LastRampIdx).collidable = False
		LastRampIdx = RampIdx
	End If
End Sub



'******************************************
'	Flipppers
'******************************************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

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
		RF.Fire
		If RightFlipper.currentangle > RightFlipper.endangle - ReflipAngle Then
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

'******************************************************
'						TROUGH 
'******************************************************

Sub sw11_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub sw11_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub
Sub sw12_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub sw12_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub sw13_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub sw13_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If sw11.BallCntOver = 0 Then sw12.kick 60, 9
	If sw12.BallCntOver = 0 Then sw13.kick 60, 9
	Me.Enabled = 0
End Sub

'******************************************************
'					DRAIN & RELEASE
'******************************************************

Sub Drain_Hit() 
	RandomSoundDrain drain
	UpdateTrough
	vpmTimer.PulseSw 10
	vpmTimer.AddTimer 500, "Drain.kick 60, 20'"
End Sub

Sub SolRelease(enabled)
	If enabled Then 
'		vpmTimer.PulseSw 15
		sw11.kick 60, 9	
		RandomSoundBallRelease sw11
	End If
End Sub

'*******************
' Switches & Targets
'*******************

' Saucers
Sub sw44_UnHit
	SoundSaucerKick 1, sw44
End Sub

Sub sw45_Hit():Controller.Switch(45) = 1:sw46.Enabled = True:End Sub
Sub sw45_Timer:sw45.TimerEnabled = 0:sw45.Kick 40, 26 + RND(1) * 5:End Sub

Sub sw46_Hit():Controller.Switch(46) = 1:sw46.Enabled = False:End Sub

'**********************************************
' Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'**********************************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw(64)
	RandomSoundSlingshotRight Sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
    RightSlingShot.TimerInterval = 10
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw(63)
	RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval  = 10
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub sw53_Hit():vpmtimer.pulsesw 53:End Sub
Sub sw56_Hit():vpmtimer.pulsesw 56:End Sub

 'Bumpers
Dim bump1, bump2, bump3

Sub Bumper1_Hit : vpmTimer.PulseSw(60) : RandomSoundBumperTop(Bumper1) :bump1 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper1_Timer()
    Select Case bump1
        Case 1:bump1 = 2: bumperring_left_on.Z = -30 : bumperring_left_off.Z = -30
        Case 2:bump1 = 3: bumperring_left_on.Z = -20 : bumperring_left_off.Z = -20
        Case 3:bump1 = 4: bumperring_left_on.Z = -10 : bumperring_left_off.Z = -10
        Case 4:Me.TimerEnabled = 0 : bumperring_left_on.Z = 0 : bumperring_left_off.Z = 0
    End Select
End Sub

Sub Bumper2_Hit : vpmTimer.PulseSw(61) : RandomSoundBumperMiddle(Bumper2) :bump2 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper2_Timer()
    Select Case bump2
        Case 1:bump2 = 2 : bumperring_right_on.Z = -30 : bumperring_right_off.Z = -30
        Case 2:bump2 = 3 : bumperring_right_on.Z = -20 : bumperring_right_off.Z = -20
        Case 3:bump2 = 4 : bumperring_right_on.Z = -10 : bumperring_right_off.Z = -10
        Case 4:Me.TimerEnabled = 0 : bumperring_right_on.Z = 0 : bumperring_right_off.Z = 0
    End Select
End Sub

Sub Bumper3_Hit : vpmTimer.PulseSw(62) : RandomSoundBumperBottom(Bumper3) :bump3 = 1:Me.TimerEnabled = 1:End Sub
Sub Bumper3_Timer()
    Select Case bump3
        Case 1:bump3 = 2: bumperring_lower_on.Z = -30 : bumperring_lower_off.Z = -30
        Case 2:bump3 = 3: bumperring_lower_on.Z = -20 : bumperring_lower_off.Z = -20
        Case 3:bump3 = 4: bumperring_lower_on.Z = -10 : bumperring_lower_off.Z = -10
        Case 4:Me.TimerEnabled = 0 : bumperring_lower_on.Z = 0 : bumperring_lower_off.Z = 0
    End Select
End Sub

' lanes
Sub sw14_Hit():Controller.Switch(14) = 1:End Sub
Sub sw14_UnHit():Controller.Switch(14) = 0:End Sub

Sub sw20_Hit():Controller.Switch(20) = 1:End Sub
Sub sw20_UnHit():Controller.Switch(20) = 0:End Sub

Sub sw21_Hit():Controller.Switch(21) = 1:End Sub
Sub sw21_UnHit():Controller.Switch(21) = 0:End Sub

Sub sw22_Hit():Controller.Switch(22) = 1:End Sub
Sub sw22_UnHit():Controller.Switch(22) = 0:End Sub

Sub sw23_Hit():Controller.Switch(23) = 1:End Sub
Sub sw23_UnHit():Controller.Switch(23) = 0:End Sub

' top lanes
Sub sw17_Hit():Controller.Switch(17) = 1:End Sub
Sub sw17_UnHit():Controller.Switch(17) = 0:End Sub

Sub sw18_Hit():Controller.Switch(18) = 1:End Sub
Sub sw18_UnHit():Controller.Switch(18) = 0:End Sub

Sub sw19_Hit():Controller.Switch(19) = 1:End Sub
Sub sw19_UnHit():Controller.Switch(19) = 0:End Sub

Sub sw28_Hit():Controller.Switch(28) = 1:End Sub
Sub sw28_UnHit():Controller.Switch(28) = 0:End Sub

Sub sw29_Hit():Controller.Switch(29) = 1:End Sub
Sub sw29_UnHit():Controller.Switch(29) = 0:End Sub

Sub sw30_Hit():Controller.Switch(30) = 1:End Sub
Sub sw30_UnHit():Controller.Switch(30) = 0:End Sub

Sub sw31_Hit():Controller.Switch(31) = 1:End Sub
Sub sw31_UnHit():Controller.Switch(31) = 0:End Sub

Sub sw32_Hit():Controller.Switch(32) = 1:End Sub
Sub sw32_UnHit():Controller.Switch(32) = 0:End Sub

' ramps
Sub sw49_Hit():Controller.Switch(49) = 1: sw49p.TransY = -15 :End Sub
Sub sw49_UnHit():Controller.Switch(49) = 0: sw49p.TransY = 0 :End Sub

Sub sw50_Hit():Controller.Switch(50) = 1: sw50p.TransY = -15 :End Sub
Sub sw50_UnHit():Controller.Switch(50) = 0: sw50p.TransY = 0 :End Sub

'****************************************************************
'		Drop Target Controls
'****************************************************************

Sub Sw25_Hit:DTHit 25:TargetBouncer Activeball, 1.5:End Sub
Sub Sw26_Hit:DTHit 26:TargetBouncer Activeball, 1.5:End Sub
Sub Sw27_Hit:DTHit 27:TargetBouncer Activeball, 1.5:End Sub

Sub Sw36_Hit:DTHit 36:TargetBouncer Activeball, 1.5:End Sub
Sub Sw37_Hit:DTHit 37:TargetBouncer Activeball, 1.5:End Sub
Sub Sw38_Hit:DTHit 38:TargetBouncer Activeball, 1.5:End Sub

Sub Sw41_Hit:DTHit 41:TargetBouncer Activeball, 1.5:End Sub
Sub Sw42_Hit:DTHit 42:TargetBouncer Activeball, 1.5:End Sub
Sub Sw43_Hit:DTHit 43:TargetBouncer Activeball, 1.5:End Sub

Sub SolBLDropTgt(enabled)
	if enabled then
		RandomSoundDropTargetReset sw26p
		DTRaise 25
		DTRaise 26
		DTRaise 27
	end if
End Sub

Sub SolBRDropTgt(enabled)
	if enabled then
		RandomSoundDropTargetReset sw37p
		DTRaise 36
		DTRaise 37
		DTRaise 38
	end if
End Sub

Sub SolTLDropTgt(enabled)
	if enabled then
		RandomSoundDropTargetReset sw42p
		DTRaise 41
		DTRaise 42
		DTRaise 43
	end if
End Sub

' targets

Sub sw24_Hit:vpmTimer.PulseSw 24:End Sub
Sub sw52_Hit: vpmTimer.PulseSw 52: End Sub

' spinners

Sub sw48_Spin():vpmTimer.pulsesw 48:End Sub


'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.
'
Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps
InitLampsNF              ' Setup lamp assignments
LampTimer.Interval = 16
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	chgLamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
	ModLampz.Update1
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	ModLampz.Update
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 1 to 140 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/10 : next
	for x = 0 to 5 : ModLampz.FadeSpeedUp(x) = 1/5 : ModLampz.FadeSpeedDown(x) = 1/40 : Next
	for x = 6 to 28 : ModLampz.FadeSpeedUp(x) = 1/10 : ModLampz.FadeSpeedDown(x) = 1/40 : Next
	'GI speed
	Lampz.FadeSpeedUp(0) = 1/30 : Lampz.FadeSpeedDown(0) = 1/80

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(1) = l1
	Lampz.MassAssign(1) = l1a
	Lampz.Callback(1) = "DisableLighting p1, 50,"
	Lampz.MassAssign(2) = l2
	Lampz.MassAssign(2) = l2a
	Lampz.Callback(2) = "DisableLighting p2, 50,"
	Lampz.MassAssign(3) = l3
	Lampz.MassAssign(3) = l3a
	Lampz.Callback(3) = "DisableLighting p3, 50,"
	Lampz.MassAssign(4) = l4
	Lampz.MassAssign(4) = l4a
	Lampz.Callback(4) = "DisableLighting p4, 50,"
	Lampz.MassAssign(5) = l5
	Lampz.MassAssign(5) = l5a
	Lampz.Callback(5) = "DisableLighting p5, 50,"
	Lampz.MassAssign(6) = l6
	Lampz.MassAssign(6) = l6a
	Lampz.Callback(6) = "DisableLighting p6, 50,"
	Lampz.MassAssign(7) = l7
	Lampz.MassAssign(7) = l7a
	Lampz.Callback(7) = "DisableLighting p7, 50,"
	Lampz.MassAssign(8) = l8
	Lampz.MassAssign(8) = l8a
	Lampz.Callback(8) = "DisableLighting p8, 50,"
	Lampz.MassAssign(9) = l9
	Lampz.MassAssign(9) = l9a
	Lampz.Callback(9) = "DisableLighting p9, 50,"
	Lampz.MassAssign(10) = l10
	Lampz.MassAssign(10) = l10a
	Lampz.Callback(10) = "DisableLighting p10, 50,"
	Lampz.MassAssign(11) = l11
	Lampz.MassAssign(11) = l11a
	Lampz.Callback(11) = "DisableLighting p11, 50,"
	Lampz.MassAssign(12) = l12
	Lampz.MassAssign(12) = l12a
	Lampz.Callback(12) = "DisableLighting p12, 50,"
	Lampz.MassAssign(13) = l13
	Lampz.MassAssign(13) = l13a
	Lampz.Callback(13) = "DisableLighting p13, 50,"
	Lampz.MassAssign(14) = l14
	Lampz.MassAssign(14) = l14a
	Lampz.Callback(14) = "DisableLighting p14, 50,"
	Lampz.MassAssign(15) = l15
	Lampz.MassAssign(15) = l15a
	Lampz.Callback(15) = "DisableLighting p15, 50,"
	Lampz.MassAssign(16) = l16
	Lampz.MassAssign(16) = l16a
	Lampz.Callback(16) = "DisableLighting p16, 50,"
	Lampz.MassAssign(17) = l17
	Lampz.MassAssign(17) = l17a
	Lampz.Callback(17) = "DisableLighting p17, 50,"
	Lampz.MassAssign(18) = l18
	Lampz.MassAssign(18) = l18a
	Lampz.Callback(18) = "DisableLighting p18, 50,"
	Lampz.MassAssign(19) = l19
	Lampz.MassAssign(19) = l19a
	Lampz.Callback(19) = "DisableLighting p19, 50,"
	Lampz.MassAssign(20) = l20
	Lampz.MassAssign(20) = l20a
	Lampz.Callback(20) = "DisableLighting p20, 50,"
	Lampz.MassAssign(21) = l21
	Lampz.MassAssign(21) = l21a
	Lampz.Callback(21) = "DisableLighting p21, 50,"
	Lampz.MassAssign(22) = l22
	Lampz.MassAssign(22) = l22a
	Lampz.Callback(22) = "DisableLighting p22, 50,"
	Lampz.MassAssign(23) = l23
	Lampz.MassAssign(23) = l23a
	Lampz.Callback(23) = "DisableLighting p23, 50,"
	Lampz.MassAssign(24) = l24
	Lampz.MassAssign(24) = l24a
	Lampz.Callback(24) = "DisableLighting p24, 50,"
	Lampz.MassAssign(25) = l25
	Lampz.MassAssign(25) = l25a
	Lampz.Callback(25) = "DisableLighting p25, 50,"
	Lampz.MassAssign(26) = l26
	Lampz.MassAssign(26) = l26a
	Lampz.Callback(26) = "DisableLighting p26, 50,"
	Lampz.MassAssign(27) = l27
	Lampz.MassAssign(27) = l27a
	Lampz.Callback(27) = "DisableLighting p27, 50,"
	Lampz.MassAssign(28) = l28
	Lampz.MassAssign(28) = l28a
'	Lampz.Callback(28) = "DisableLighting p28, 50,"
	Lampz.MassAssign(29) = l29
	Lampz.MassAssign(29) = l29a
'	Lampz.Callback(29) = "DisableLighting p29, 50,"
	Lampz.MassAssign(30) = l30
	Lampz.MassAssign(30) = l30a
'	Lampz.Callback(30) = "DisableLighting p30, 50,"
	Lampz.MassAssign(31) = l31
	Lampz.MassAssign(31) = l31a
'	Lampz.Callback(31) = "DisableLighting p31, 50,"
	Lampz.MassAssign(32) = l32
	Lampz.MassAssign(32) = l32a
'	Lampz.Callback(32) = "DisableLighting p32, 50,"
	Lampz.MassAssign(33) = l33
	Lampz.MassAssign(33) = l33a
	Lampz.Callback(33) = "DisableLighting p33, 50,"
	Lampz.MassAssign(34) = l34
	Lampz.MassAssign(34) = l34a
	Lampz.Callback(34) = "DisableLighting p34, 50,"
	Lampz.MassAssign(35) = l35
	Lampz.MassAssign(35) = l35a
	Lampz.Callback(35) = "DisableLighting p35, 50,"
	Lampz.MassAssign(36) = l36
	Lampz.MassAssign(36) = l36a
	Lampz.Callback(36) = "DisableLighting p36, 50,"
	Lampz.MassAssign(37) = l37
	Lampz.MassAssign(37) = l37a
	Lampz.Callback(37) = "DisableLighting p37, 50,"
	Lampz.MassAssign(38) = l38
	Lampz.MassAssign(38) = l38a
	Lampz.Callback(38) = "DisableLighting p38, 50,"
	Lampz.MassAssign(39) = l39
	Lampz.MassAssign(39) = l39a
	Lampz.Callback(39) = "DisableLighting p39, 50,"
	Lampz.MassAssign(40) = l40
	Lampz.MassAssign(40) = l40a
	Lampz.Callback(40) = "DisableLighting p40, 50,"
	Lampz.MassAssign(41) = l41
	Lampz.MassAssign(41) = l41a
	Lampz.Callback(41) = "DisableLighting p41, 50,"
	Lampz.MassAssign(42) = l42
	Lampz.MassAssign(42) = l42a
	Lampz.Callback(42) = "DisableLighting p42, 50,"
	Lampz.MassAssign(43) = l43
	Lampz.MassAssign(43) = l43a
	Lampz.Callback(43) = "DisableLighting p43, 50,"
	Lampz.MassAssign(44) = l44
	Lampz.MassAssign(44) = l44a
	Lampz.Callback(44) = "DisableLighting p44, 50,"
	Lampz.MassAssign(45) = l45
	Lampz.MassAssign(45) = l45a
	Lampz.MassAssign(45) = l45f
	Lampz.Callback(45) = "DisableLighting p45, 30,"
	Lampz.MassAssign(46) = l46
	Lampz.MassAssign(46) = l46a
	Lampz.MassAssign(46) = l46f
	Lampz.Callback(46) = "DisableLighting p46, 30,"
	Lampz.MassAssign(47) = l47
	Lampz.MassAssign(47) = l47a
	Lampz.Callback(47) = "DisableLighting p47, 30,"
	Lampz.MassAssign(48) = l48
	Lampz.MassAssign(48) = l48a
	Lampz.Callback(48) = "DisableLighting p48, 30,"
	Lampz.MassAssign(49) = l49
	Lampz.MassAssign(49) = l49a
	Lampz.Callback(49) = "DisableLighting p49, 50,"
	Lampz.MassAssign(50) = l50
	Lampz.MassAssign(50) = l50a
	Lampz.Callback(50) = "DisableLighting p50, 30,"
	Lampz.MassAssign(51) = l51
	Lampz.Callback(51) = "DisableLighting p51, 30,"
	Lampz.MassAssign(52) = l52
	Lampz.Callback(52) = "DisableLighting p52, 30,"
	Lampz.MassAssign(53) = l53
	Lampz.Callback(53) = "DisableLighting p53, 30,"
	Lampz.MassAssign(54) = l54
	Lampz.Callback(54) = "DisableLighting p54, 30,"
	Lampz.MassAssign(55) = l55
	Lampz.Callback(55) = "DisableLighting p55, 30,"
	Lampz.MassAssign(56) = l56
	Lampz.Callback(56) = "DisableLighting p56, 30,"

if DesktopMode and vrroom = 0 then
	Lampz.MassAssign(49) = l49dt
	Lampz.MassAssign(57) = l57
	Lampz.MassAssign(58) = l58
	Lampz.MassAssign(59) = l59
	Lampz.MassAssign(60) = l60
	Lampz.MassAssign(61) = l61
	Lampz.MassAssign(62) = l62
	Lampz.MassAssign(63) = l63
	Lampz.MassAssign(64) = l64
	Lampz.MassAssign(111) = f111	'"Desktop Wheel"
end If

'********************************************************

'	Flashers

	Lampz.MassAssign(122) = f122a	'"Lifter Ramp Flashers"
	Lampz.MassAssign(122) = f122b	'"Lifter Ramp Flashers"
	Lampz.Callback(122) = "DisableLighting Drawbridge_on_flash, 1,"

	Lampz.obj(0) = ColtoArray(GI)
	Lampz.Callback(0) = "GIUpdates"
	Lampz.state(0) = 1

	'Turn off all lamps on startup
	lampz.Init	'This just turns state of any lamps to 1
	ModLampz.Init

	'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub

'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
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

Sub SetModLamp(aNr, aInput)
	ModLampz.state(aNr) = abs(aInput)/255
End Sub


'**********************************************************************
'Class jungle nf
'**********************************************************************

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


Class DynamicLamps 'Lamps that fade up and down. GI and Flasher handling
	Public Loaded(50), FadeSpeedDown(50), FadeSpeedUp(50)
	Private Lock(50), SolModValue(50)
	Private UseCallback(50), cCallback(50)
	Public Lvl(50)
	Public Obj(50)
	Private UseFunction, cFilter
	private Mult(50)
	Public Name

	Public FrameTime
	Private InitFrame

	Private Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(Obj)
			FadeSpeedup(x) = 0.01
			FadeSpeedDown(x) = 0.01
			lvl(x) = 0.0001 : SolModValue(x) = 0
			Lock(x) = True : Loaded(x) = False
			mult(x) = 1
			Name = "DynamicFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'debugstr = debugstr & x & "=" & tmp(x) & vbnewline
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out
	End Property


	Public Property Let State(idx,Value)
		'If Value = SolModValue(idx) Then Exit Property ' Discard redundant updates
		If Value <> SolModValue(idx) Then ' Discard redundant updates
			SolModValue(idx) = Value
			Lock(idx) = False : Loaded(idx) = False
		End If
	End Property

	Public Property Get state(idx) : state = SolModValue(idx) : end Property

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

	'solcallback (solmodcallback) handler
	Sub SetLamp(aIdx, aInput) : state(aIdx) = aInput : End Sub	'0->1 Input
	Sub SetModLamp(aIdx, aInput) : state(aIdx) = aInput/255 : End Sub	'0->255 Input
	Sub SetGI(aIdx, ByVal aInput) : if aInput = 8 then aInput = 7 end if : state(aIdx) = aInput/7 : End Sub	'0->8 WPC GI input

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline

				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline

			end if
		Next
		'debug.print debugstr
	End Sub

	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'just call turnonstates for now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all numeric fading. If done fading, Lock(x) = True
		'dim stringer
		dim x : for x = 0 to uBound(Lvl)
			'stringer = "Locked @ " & SolModValue(x)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					'stringer = "Fading Up " & lvl(x) & " + " & FadeSpeedUp(x)
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					'stringer = "Fading Down " & lvl(x) & " - " & FadeSpeedDown(x)
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		'tbF.text = stringer
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(Lvl)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx
		for x = 0 to uBound(Lvl)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(abs(Lvl(x))*mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(abs(Lvl(x))*mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)*mult(x)
					End If
				end if
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x)*mult(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					Loaded(x) = True
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


const BallBrightMax = 255			'Brightness setting when GI is on (max of 255). Only applies for Normal ball.
const BallBrightMin = 100			'Brightness setting when GI is off (don't set above the max). Only applies for Normal ball.
Sub UpdateBallBrightness
	Dim b, brightness
	For b = 0 to UBound(gBOT)
		If NOT bBallInTrough(b) then
			if ballbrightness >=0 Then gBOT(b).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
			if b = UBound(gBOT) then 'until last ball brightness is set, then reset to -1
				if ballbrightness = ballbrightMax Or ballbrightness = ballbrightMin then ballbrightness = -1
			end if
		End If
	Next
End Sub

Dim bBallInTrough(3)
Sub CheckBallLocations
	Dim b
	For b = 0 to UBound(gBOT)
		'Check if ball is in the trough
		If InRect(gBOT(b).X, gBOT(b).Y, 847,1742,847,1840,507,2082,507,1973) Then
			bBallInTrough(b) = True
		Else
			bBallInTrough(b) = False
		End If
	Next
End Sub

'******************
'		GI
'******************

dim kk, ballbrightness

sub OnPrimsVisible(aValue)
	If aValue then
		For each kk in ON_Prims:kk.visible = 1:next
		if cabinetmode = 0 then rails_on.visible = 1
	Else
		For each kk in ON_Prims:kk.visible = 0:next
		rails_on.visible = 0
	end If
end Sub

sub OffPrimsVisible(aValue)
	If aValue then
		For each kk in OFF_Prims:kk.visible = 1:next
		if cabinetmode = 0 then rails_off.visible = 1
	Else
		For each kk in OFF_Prims:kk.visible = 0:next
		rails_off.visible = 0
	end If
end Sub

'GI callback
Const PFGIOFFOpacity = 100

Sub GIUpdates(ByVal aLvl)	'argument is unused
	dim x

	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	'debug.print "aLvl=" & aLvl & " giprevalvl=" & giprevalvl

	if aLvl = 0 then										'GI OFF, let's hide ON prims
		'debug.print "aLvl = 0. OnPrimsVisible False"
		OnPrimsVisible False
'		Flasherlight1.intensity = 7
		If gilvl = 1 Then OffPrimsVisible true
		if ballbrightness <> -1 then ballbrightness = ballbrightMin
	Elseif aLvl = 1 then									'GI ON, let's hide OFF prims
		'debug.print "aLvl = 1. OffPrimsVisible False"
		OffPrimsVisible False
'		Flasherlight1.intensity = 2
		If gilvl = 0 Then OnPrimsVisible True
		if ballbrightness <> -1 then ballbrightness = ballbrightMax
	Else
		if gilvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
			'debug.print "giprevalvl = 0. OnPrimsVisible True"
			OnPrimsVisible True
			ballbrightness = ballbrightMin + 1
		elseif gilvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
			'debug.print "giprevalvl = 1. OffPrimsVisible true"
			OffPrimsVisible true
			ballbrightness = ballbrightMax - 1
		Else
			'no change
		end if
	end if

	UpdateMaterial "OpaqueON",			0,0,0,0,0,0,aLvl,RGB(255,255,255),0,0,False,True,0,0,0,0		'let transparency be only 0.25 and 1.

	Playfield_OFF.opacity = PFGIOFFOpacity - (PFGIOFFOpacity * alvl^3)
	plasticRamps.blenddisablelighting = 1.5 * alvl + 0.5

	FlashOffDL = FlasherOffBrightness*(4/5*aLvl + 1/5)
	Flasherbase1.blenddisablelighting = FlashOffDL
	Flasherbase2.blenddisablelighting = FlashOffDL
	Flasherbase3.blenddisablelighting = FlashOffDL
	Flasherbase4.blenddisablelighting = FlashOffDL
	Flasherbase5.blenddisablelighting = FlashOffDL

	'ball
	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)

	gilvl = alvl

End Sub


'******************************************************
'*****   FLUPPER DOMES 
'******************************************************

dim SolDrawSpeed : SolDrawSpeed = 1.5


Sub FlashSol130(level)
'	debug.print "level status: " & level
	if level Then
		ObjTargetLevel(1) = 1
		FlasherFlash1_Timer
	Else
		ObjTargetLevel(1) = 0
	end if
End Sub

Sub FlashSol131(level)
	if level Then
		ObjTargetLevel(2) = 1
		FlasherFlash2_Timer
	Else
		ObjTargetLevel(2) = 0
	end if
End Sub

Sub FlashSol109(level)
	if level Then
		ObjTargetLevel(3) = 1
		ObjTargetLevel(4) = 1
		FlasherFlash3_Timer
		FlasherFlash4_Timer
	Else
		ObjTargetLevel(3) = 0
		ObjTargetLevel(4) = 0
	end if
End Sub

Sub FlashSol132(level)
	if level Then
		ObjTargetLevel(5) = 1
		FlasherFlash5_Timer
	Else
		ObjTargetLevel(5) = 0
	end if
End Sub


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1			' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.1		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 1.8		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
Dim FlashOffDL : FlashOffDL = FlasherOffBrightness

InitFlasher 1, "orange"
InitFlasher 2, "orange"
InitFlasher 3, "orange"
InitFlasher 4, "orange"
InitFlasher 5, "orange"

'rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
'		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "orange" : objlight(nr).color = RGB(230,130,30) : objflasher(nr).color = RGB(230,130,30) : objbloom(nr).color = RGB(230,130,30)
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True
		objflasher(nr).visible = 1
		objbloom(nr).visible = 1
		objlit(nr).visible = 1
		Select Case nr
			Case 1:
				If VRRoom > 0 Then
					VRBGFL30_1.visible = 1
					VRBGFL30_2.visible = 1
					VRBGFL30_3.visible = 1
					VRBGFL30_4.visible = 1
					BGBulb19.opacity = 20
				End If
				FlasherLL_L.visible = 1
				FlasherLL_BL.visible = 1
				FlasherLL_R.visible = 1
				FlasherLL_BT.visible = 1
				FlasherLL_P.visible = 1
			Case 2:
				If VRRoom > 0 Then
					VRBGFL31_1.visible = 1
					VRBGFL31_2.visible = 1
					VRBGFL31_3.visible = 1
					VRBGFL31_4.visible = 1
				End If
				FlasherL_L.visible = 1
				FlasherL_BL.visible = 1
				FlasherL_R.visible = 1
				FlasherL_BT.visible = 1
				FlasherL_P.visible = 1
			Case 3:
				If VRRoom > 0 Then
					VRBGFL9_1.visible = 1
					VRBGFL9_2.visible = 1
					VRBGFL9_3.visible = 1
					VRBGFL9_4.visible = 1
				End If
				FlasherC_L.visible = 1
				FlasherC_BL.visible = 1
				FlasherC_R.visible = 1
				FlasherC_BT.visible = 1
				FlasherC_P.visible = 1
			Case 5:
				If VRRoom > 0 Then
					VRBGFL32_1.visible = 1
					VRBGFL32_2.visible = 1
					VRBGFL32_3.visible = 1
					VRBGFL32_4.visible = 1
				End If
				FlasherR_L.visible = 1
				FlasherR_BL.visible = 1
				FlasherR_R.visible = 1
				FlasherR_BT.visible = 1
				FlasherR_P.visible = 1
		End Select
	End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting = FlashOffDL + 10 * ObjLevel(nr)^3
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 

	if ObjTargetLevel(nr) = 1 and ObjLevel(nr) < ObjTargetLevel(nr) Then			'solenoid ON happened
		ObjLevel(nr) = (ObjLevel(nr) + 0.15) * RndNum(1.05, 1.15)					'fadeup speed. ~4-5 frames * 30ms
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif ObjTargetLevel(nr) = 0 and ObjLevel(nr) > ObjTargetLevel(nr) Then		'solenoid OFF happened
		ObjLevel(nr) = ObjLevel(nr) * 0.8 - 0.01									'fadedown speed. ~16 frames * 30ms
		if ObjLevel(nr) < 0.02 then ObjLevel(nr) = 0								'slight perf optimization to cut the very tail of the fade
	Else																			'do nothing here
		ObjLevel(nr) = ObjTargetLevel(nr)
		'debug.print objTargetLevel(nr) &" = " & ObjLevel(nr)
	end if

Dim gionflash, gioffflash
gionflash = 44
gioffflash = 50

	select case nr
		Case 1:
			If VRRoom > 0 Then
				VRBGFL30_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL30_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL30_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL30_4.opacity = 100 * ObjLevel(nr)^3
			End If
			'debug.print ObjLevel(nr)
			FlasherLL_L.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^1
			FlasherLL_BL.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^1
			FlasherLL_R.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
			FlasherLL_BT.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
			FlasherLL_P.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
		Case 2:
			If VRRoom > 0 Then
				VRBGFL31_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL31_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL31_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL31_4.opacity = 100 * ObjLevel(nr)^3
			End If
			FlasherL_L.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^1
			FlasherL_BL.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^1
			FlasherL_R.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
			FlasherL_BT.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
			FlasherL_P.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
		Case 3:
			If VRRoom > 0 Then
				VRBGFL9_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL9_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL9_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL9_4.opacity = 100 * ObjLevel(nr)^3
			End If
			FlasherC_L.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^1
			FlasherC_BL.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^1
			FlasherC_R.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
			FlasherC_BT.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
			FlasherC_P.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
		Case 5:
			If VRRoom > 0 Then
				VRBGFL32_1.opacity = 100 * ObjLevel(nr)^2
				VRBGFL32_2.opacity = 100 * ObjLevel(nr)^2
				VRBGFL32_3.opacity = 100 * ObjLevel(nr)^2
				VRBGFL32_4.opacity = 100 * ObjLevel(nr)^3
			End If
			FlasherR_L.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^1
			FlasherR_BL.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^1
			FlasherR_R.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
			FlasherR_BT.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
			FlasherR_P.opacity = (gioffflash - gionflash*gilvl) * ObjLevel(nr)^2
	End Select
	If ObjLevel(nr) < 0.2 Then
		Select Case nr
			Case 1:
			 BGBulb19.opacity = 50
		End Select
	End If
	If ObjLevel(nr) <= 0 Then 
		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objbloom(nr).visible = 0
		objlit(nr).visible = 0
		Select Case nr
			Case 1:
				If VRRoom > 0 Then
					VRBGFL30_1.visible = 0
					VRBGFL30_2.visible = 0
					VRBGFL30_3.visible = 0
					VRBGFL30_4.visible = 0
'					BGBulb19.opacity = 50
				End If
				FlasherLL_L.visible = 0
				FlasherLL_BL.visible = 0
				FlasherLL_R.visible = 0
				FlasherLL_BT.visible = 0
				FlasherLL_P.visible = 0
			Case 2:
				If VRRoom > 0 Then
					VRBGFL31_1.visible = 0
					VRBGFL31_2.visible = 0
					VRBGFL31_3.visible = 0
					VRBGFL31_4.visible = 0
				End If
				FlasherL_L.visible = 0
				FlasherL_BL.visible = 0
				FlasherL_R.visible = 0
				FlasherL_BT.visible = 0
				FlasherL_P.visible = 0
			Case 3:
				If VRRoom > 0 Then
					VRBGFL9_1.visible = 0
					VRBGFL9_2.visible = 0
					VRBGFL9_3.visible = 0
					VRBGFL9_4.visible = 0
				End If
				FlasherC_L.visible = 0
				FlasherC_BL.visible = 0
				FlasherC_R.visible = 0
				FlasherC_BT.visible = 0
				FlasherC_P.visible = 0
			Case 5:
				If VRRoom > 0 Then
					VRBGFL32_1.visible = 0
					VRBGFL32_2.visible = 0
					VRBGFL32_3.visible = 0
					VRBGFL32_4.visible = 0
				End If
				FlasherR_L.visible = 0
				FlasherR_BL.visible = 0
				FlasherR_R.visible = 0
				FlasherR_BT.visible = 0
				FlasherR_P.visible = 0
		End Select
	End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub

'******************************************************
'******  END FLUPPER DOMES
'******************************************************


'**********************************************************
'**************   VR Backbox Display Driver   *************
'**********************************************************

Dim Digits(32)
Digits(0) = Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
Digits(1) = Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
Digits(2) = Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
Digits(3) = Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
Digits(4) = Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
Digits(5) = Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
Digits(6) = Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
Digits(7) = Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
Digits(8) = Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
Digits(9) = Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
Digits(10) = Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
Digits(11) = Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
Digits(12) = Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
Digits(13) = Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
Digits(14) = Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
Digits(15) = Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)

Digits(16) = Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
Digits(17) = Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
Digits(18) = Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
Digits(19) = Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
Digits(20) = Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
Digits(21) = Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
Digits(22) = Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
Digits(23) = Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
Digits(24) = Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
Digits(25) = Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
Digits(26) = Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
Digits(27) = Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
Digits(28) = Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
Digits(29) = Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
Digits(30) = Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
Digits(31) = Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)


Sub DisplayTimer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 32) then
				if VRRoom > 0 Then
				For Each obj In DigitsVR(num)
                   If chg And 1 Then FadeDisplay obj, stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
				end If
				if DesktopMode = True and VRRoom < 1 Then
				For Each obj In Digits(num)
                   If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
				end If
				else
			end if
		Next
    End If
End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
	Else
		Object.Color = RGB(1,1,1)
	End If
End Sub

Dim DigitsVR(32)
DigitsVR(0)=Array(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15)
DigitsVR(1)=Array(D16,D17,D18,D19,D20,D21,D22,D23,D24,D25,D26,D27,D28,D29,D30)
DigitsVR(2)=Array(D31,D32,D33,D34,D35,D36,D37,D38,D39,D40,D41,D42,D43,D44,D45)
DigitsVR(3)=Array(D46,D47,D48,D49,D50,D51,D52,D53,D54,D55,D56,D57,D58,D59,D60)
DigitsVR(4)=Array(D61,D62,D63,D64,D65,D66,D67,D68,D69,D70,D71,D72,D73,D74,D75)
DigitsVR(5)=Array(D76,D77,D78,D79,D80,D81,D82,D83,D84,D85,D86,D87,D88,D89,D90)
DigitsVR(6)=Array(D91,D92,D93,D94,D95,D96,D97,D98,D99,D100,D101,D102,D103,D104,D105)
DigitsVR(7)=Array(D106,D107,D108,D109,D110,D111,D112,D113,D114,D115,D116,D117,D118,D119,D120)
DigitsVR(8)=Array(D121,D122,D123,D124,D125,D126,D127,D128,D129,D130,D131,D132,D133,D134,D135)
DigitsVR(9)=Array(D136,D137,D138,D139,D140,D141,D142,D143,D144,D145,D146,D147,D148,D149,D150)
DigitsVR(10)=Array(D151,D152,D153,D154,D155,D156,D157,D158,D159,D160,D161,D162,D163,D164,D165)
DigitsVR(11)=Array(D166,D167,D168,D169,D170,D171,D172,D173,D174,D175,D176,D177,D178,D179,D180)
DigitsVR(12)=Array(D181,D182,D183,D184,D185,D186,D187,D188,D189,D190,D191,D192,D193,D194,D195)
DigitsVR(13)=Array(D196,D197,D198,D199,D200,D201,D202,D203,D204,D205,D206,D207,D208,D209,D210)
DigitsVR(14)=Array(D211,D212,D213,D214,D215,D216,D217,D218,D219,D220,D221,D222,D223,D224,D225)
DigitsVR(15)=Array(D226,D227,D228,D229,D230,D231,D232,D233,D234,D235,D236,D237,D238,D239,D240)

DigitsVR(16)=Array(D241,D242,D243,D244,D245,D246,D247,D248,D249,D250,D251,D252,D253,D254,D255)
DigitsVR(17)=Array(D256,D257,D258,D259,D260,D261,D262,D263,D264,D265,D266,D267,D268,D269,D270)
DigitsVR(18)=Array(D271,D272,D273,D274,D275,D276,D277,D278,D279,D280,D281,D282,D283,D284,D285)
DigitsVR(19)=Array(D286,D287,D288,D289,D290,D291,D292,D293,D294,D295,D296,D297,D298,D299,D300)
DigitsVR(20)=Array(D301,D302,D303,D304,D305,D306,D307,D308,D309,D310,D311,D312,D313,D314,D315)
DigitsVR(21)=Array(D316,D317,D318,D319,D320,D321,D322,D323,D324,D325,D326,D327,D328,D329,D330)
DigitsVR(22)=Array(D331,D332,D333,D334,D335,D336,D337,D338,D339,D340,D341,D342,D343,D344,D345)
DigitsVR(23)=Array(D346,D347,D348,D349,D350,D351,D352,D353,D354,D355,D356,D357,D358,D359,D360)
DigitsVR(24)=Array(D361,D362,D363,D364,D365,D366,D367,D368,D369,D370,D371,D372,D373,D374,D375)
DigitsVR(25)=Array(D376,D377,D378,D379,D380,D381,D382,D383,D384,D385,D386,D387,D388,D389,D390)
DigitsVR(26)=Array(D391,D392,D393,D394,D395,D396,D397,D398,D399,D400,D401,D402,D403,D404,D405)
DigitsVR(27)=Array(D406,D407,D408,D409,D410,D411,D412,D413,D414,D415,D416,D417,D418,D419,D420)
DigitsVR(28)=Array(D421,D422,D423,D424,D425,D426,D427,D428,D429,D430,D431,D432,D433,D434,D435)
DigitsVR(29)=Array(D436,D437,D438,D439,D440,D441,D442,D443,D444,D445,D446,D447,D448,D449,D450)
DigitsVR(30)=Array(D451,D452,D453,D454,D455,D456,D457,D458,D459,D460,D461,D462,D463,D464,D465)
DigitsVR(31)=Array(D466,D467,D468,D469,D470,D471,D472,D473,D474,D475,D476,D477,D478,D479,D480)

Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(DigitsVR)
		if IsArray(DigitsVR(x) ) then
			For each obj in DigitsVR(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

InitDigits

Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : vpmTimer.addResetObj Me
	End Sub

	Public Sub AddSw(aSwNo, aStart, aEnd)
		mSw(mNextSw) = Array(aSwNo, aStart, aEnd, 0)
		mNextSw = mNextSw + 1
	End Sub

	Public Sub AddPulseSwNew(aSwNo, aInterval, aStart, aEnd)
		If Controller.Version >= "01200000" Then
			mSw(mNextSw) = Array(aSwNo, aStart, aEnd, aInterval)
		Else
			mSw(mNextSw) = Array(aSwNo, -aInterval, aEnd - aStart + 1, 0)
		End If
		mNextSw = mNextSw + 1
	End Sub

	Public Sub Start
		Dim sw, ii
		With Controller
			.Mech(1) = Sol1 : .Mech(2) = Sol2 : .Mech(3) = Length
			.Mech(4) = Steps : .Mech(5) = MType : .Mech(6) = Acc : .Mech(7) = Ret
			ii = 10
			For Each sw In mSw
				If IsArray(sw) Then
					.Mech(ii) = sw(0) : .Mech(ii+1) = sw(1)
					.Mech(ii+2) = sw(2) : .Mech(ii+3) = sw(3)
					ii = ii + 10
				End If
			Next
			.Mech(0) = mMechNo
		End With
		If IsObject(mCallback) Then mCallBack 0, 0, 0 : mLastPos = 0 : vpmTimer.EnableUpdate Me, True, True  ' <------- All for this.
	End Sub

	Public Property Get Position : Position = Controller.GetMech(mMechNo) : End Property
	Public Property Get Speed    : Speed = Controller.GetMech(-mMechNo)   : End Property
	Public Property Let Callback(aCallBack) : Set mCallback = aCallBack : End Property

	Public Sub Update
		Dim currPos, speed
		currPos = Controller.GetMech(mMechNo)
		speed = Controller.GetMech(-mMechNo)
		If currPos < 0 Or (mLastPos = currPos And mLastSpeed = speed) Then Exit Sub
		mCallBack currPos, speed, mLastPos : mLastPos = currPos : mLastSpeed = speed
	End Sub

	Public Sub Reset : Start : End Sub
	
End Class


'**********************************************************************
'/////////////////////////---   Physics  ---//////////////////////////'
'**********************************************************************

'***********************************************************************
' Begin NFozzy Physics Scripting:  Flipper Tricks and Rubber Dampening '
'***********************************************************************

'****************************************************************
'	Flipper Collision Subs
'****************************************************************

Sub LeftFlipper_Collide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipper_Collide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
 	RandomSoundRubberFlipper(parm)
End Sub

'****************************************************************
'	FLIPPER CORRECTION INITIALIZATION
'****************************************************************

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

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
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
         :                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

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

'****************************************************************
'	FLIPPER TRICKS
'****************************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
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
                                        gBOT(b).velx = gBOT(b).velx / 1.5
                                        gBOT(b).vely = gBOT(b).vely - 0.5
										debug.print "nudge"
                                end If
                        Next
                End If
        Else 
                If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 15 then 
                        EOSNudge1 = 0
                end if
        End If
End Sub


'****************************************************************
'	Maths
'****************************************************************
Const PI = 3.1415927

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
'        dim pi
'        pi = 4*Atn(1)

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
' Check ball distance from Flipper for Rem
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

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'****************************************************************
'	End - Check ball distance from Flipper for Rem
'****************************************************************

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
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.018

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

'######################### Add new dampener to CheckLiveCatch 
'#########################    Note the updated flipper angle check to register if the flipper gets knocked slightly off the end angle

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
			debug.print "catch bounce: " & LiveCatchBounce
            If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
            ball.vely = LiveCatchBounce * (16 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx= 0
            ball.angmomy= 0
            ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, 2
    End If
End Sub






'******************************************************
'  SLINGSHOT CORRECTION
'******************************************************

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
	AddSlingsPt 0, 0.00,	-5
	AddSlingsPt 1, 0.45,	-5
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	5
	AddSlingsPt 5, 1.00,	5

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
'			debug.print " BallPos=" & BallPos &" Angle=" & Angle 
'			debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
'			debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
'			debug.print " " 
		End If
	End Sub

End Class





'****************************************************************
'	PHYSICS DAMPENERS
'****************************************************************
'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64		'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
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
		if cor.ballvel(aBall.id) = 0 then
			RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.001) 'hack
		Else
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		end If
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm, ver)
		If ver = 1 Then
			dim RealCOR, DesiredCOR, str, coef
			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			if cor.ballvel(aBall.id) = 0 then
                RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.001) 'hack
            Else
                RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
            end If
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
			End If
		Elseif ver = 2 Then
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

'****************************************************************
'	TRACK ALL BALL VELOCITIES FOR RUBBER DAMPENER AND DROP TARGETS
'****************************************************************

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

Sub RDampen_Timer()
	Cor.Update
End Sub

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

'****************************************************************
'	iaakki's TargetBouncer for targets and posts
'****************************************************************

sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel
	vel = BallSpeed(aBall)
'	debug.print "bounce"
	if aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * 1.1
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

'****************************************************************
'	END nFozzy Physics
'****************************************************************




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
	Dim b
	
	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob
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
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

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


' Ramp triggers 

Sub trigRamp1_Hit : WireRampOn True : End Sub
Sub trigRamp1_UnHit : if Activeball.vely > 0 Then :WireRampOff :End If: End Sub

Sub trigRamp2_Hit : WireRampOn True : End Sub
Sub trigRamp2_UnHit : if Activeball.vely > 0 Then :WireRampOff  :End If: End Sub

Sub trigRamp3_Hit: WireRampOff : End Sub

Sub trigRamp4_Hit: WireRampOn True : End Sub

Sub trigRamp5_Hit: WireRampOff : End Sub

Sub trigRamp6_Hit : WireRampOn False : End Sub
Sub trigRamp6_UnHit : if Activeball.vely > 0 Then :WireRampOff  :End If: End Sub

Sub trigRamp7_Hit : WireRampOff : End Sub


'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************





'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

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
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

'****************************************************************
'		DROP TARGETS INITIALIZATION
'****************************************************************

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
Dim DT25, DT26, DT27, DT36, DT37, DT38, DT41, DT42, DT43

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

Set DT25 = (new DropTarget)(sw25, sw25o, sw25p, 25, 0, false)
Set DT26 = (new DropTarget)(sw26, sw26o, sw26p, 26, 0, false)
Set DT27 = (new DropTarget)(sw27, sw27o, sw27p, 27, 0, false)
Set DT36 = (new DropTarget)(sw36, sw36o, sw36p, 36, 0, false)
Set DT37 = (new DropTarget)(sw37, sw37o, sw37p, 37, 0, false)
Set DT38 = (new DropTarget)(sw38, sw38o, sw38p, 38, 0, false)
Set DT41 = (new DropTarget)(sw41, sw41o, sw41p, 41, 0, false)
Set DT42 = (new DropTarget)(sw42, sw42o, sw42p, 42, 0, false)
Set DT43 = (new DropTarget)(sw43, sw43o, sw43p, 43, 0, false)

Dim DTArray
DTArray = Array(DT25, DT26, DT27, DT36, DT37, DT38, DT41, DT42, DT43)

	'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 0 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "" 'Drop Target Drop sound
Const DTResetSound = "" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'  DROP TARGETS FUNCTIONS
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
			secondary.collidable = 0
			controller.Switch(Switchid) = 1
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
		elseif transz > 0 then
			prim.transz = transz
		end if

		if prim.transz > DTDropUpUnits then 
			DTAnimate = -2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		controller.Switch(Switchid) = 0

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

Sub DTAnim_Timer()
	DoDTAnim
	DoSTAnim
'	If psw1.transz < -DTDropUnits/2 Then drop1.visible = 0 else drop1.visible = 1
'	If psw2.transz < -DTDropUnits/2 Then drop2.visible = 0 else drop2.visible = 1
'	If psw3.transz < -DTDropUnits/2 Then drop3.visible = 0 else drop3.visible = 1
'	If psw4.transz < -DTDropUnits/2 Then drop4.visible = 0 else drop4.visible = 1
End Sub


'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
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



'****************************************************************
'		STAND-UP TARGET INITIALIZATION
'****************************************************************

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
Dim ST24, ST52

'Set array with stand-up target objects

'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'						****IMPORTANT!!!****
'	------	transy must be used to offset the target animation  ------
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instructions, set to 0
'	target identifier:	The target

Set ST24 = (new StandupTarget)(sw24, sw24p, 24, 0, 24)
Set ST52 = (new StandupTarget)(sw52, sw52p, 52, 0, 52)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST24, ST52)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit
'Const STHitSound = "target"	'Stand-up Target Hit sound - **Replaced with Fleep Code
Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'****************************************************************
'		STAND-UP TARGETS FUNCTIONS
'****************************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

'	PlayTargetSound		'Replaced with Fleep Code
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).target = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 

	If perpvel <= 0 or perpvelafter >= 0 Then
		STCheckHit = 0 
	Else
		STCheckHit = 1
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch, animate)
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


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************


Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


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
	Dim iii, source

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

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
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
	Dim s, Source, LSd, currentMat, AnotherSource, iii

'	'Hide shadow of deleted balls
'	For s = UBound(gBOT) + 1 to tnob
'		objrtx1(s).visible = 0
'		objrtx2(s).visible = 0
'		objBallShadow(s).visible = 0
'		BallShadowA(s).visible = 0
'	Next
'
'	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit
'
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


'****************************************************************
'		Cabinet Mode
'****************************************************************

'If CabinetMode Then
'	Cab_Sides.size_y = 1500
'	Cab_Sides_Off.size_y = 1500
'Else
'	Cab_Sides.size_y = 1000
'	Cab_Sides_Off.size_y = 1000
'End If

'****************************************************************
'		VR Mode
'****************************************************************
DIM VRThings
If VRRoom > 0 Then
	SetBackGlass
	For each VRThings in VRDigits:VRThings.visible = 1: Next
	For each VRThings in VRDigits2:VRThings.visible = 1: Next
	For each VRThings in BGGI:VRThings.visible = 1: Next
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
	End If
Else
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
End if


Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x
		obj.height = - obj.y + 335
		obj.y = 135 'adjusts the distance from the backglass towards the user
		obj.rotx = -87
	Next

	For Each obj In VRDigits
		obj.x = obj.x
		obj.height = - obj.y + 335
		obj.y = 195 'adjusts the distance from the backglass towards the user
		obj.rotx = -90
	Next
	For Each obj In VRDigits2
		obj.x = obj.x
		obj.height = - obj.y + 335
		obj.y = 200 'adjusts the distance from the backglass towards the user
		obj.rotx = -90
	Next

End Sub

' ******************************************************************************************
'      LAMP CALLBACK for VR Backglass Flashers
' ******************************************************************************************

Set LampCallback = GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps()
  IF VRRoom > 0 Then
	If Controller.Lamp(57) = 0 Then: VRBGFLMeter1.visible=0: VRBGFLMeter9.visible=0 : else: VRBGFLMeter1.visible=1 : VRBGFLMeter9.visible=1
	If Controller.Lamp(58) = 0 Then: VRBGFLMeter2.visible=0: VRBGFLMeter10.visible=0 : else: VRBGFLMeter2.visible=1 : VRBGFLMeter10.visible=1
	If Controller.Lamp(59) = 0 Then: VRBGFLMeter3.visible=0: VRBGFLMeter11.visible=0 : else: VRBGFLMeter3.visible=1 : VRBGFLMeter11.visible=1
	If Controller.Lamp(60) = 0 Then: VRBGFLMeter4.visible=0: VRBGFLMeter12.visible=0 : else: VRBGFLMeter4.visible=1 : VRBGFLMeter12.visible=1
	If Controller.Lamp(61) = 0 Then: VRBGFLMeter5.visible=0: VRBGFLMeter13.visible=0 : else: VRBGFLMeter5.visible=1 : VRBGFLMeter13.visible=1
	If Controller.Lamp(62) = 0 Then: VRBGFLMeter6.visible=0: VRBGFLMeter14.visible=0 : else: VRBGFLMeter6.visible=1 : VRBGFLMeter14.visible=1
	If Controller.Lamp(63) = 0 Then: VRBGFLMeter7.visible=0: VRBGFLMeter15.visible=0 : else: VRBGFLMeter7.visible=1: VRBGFLMeter15.visible=1
	If Controller.Lamp(64) = 0 Then: VRBGFLMeter8.visible=0: VRBGFLMeter16.visible=0 : else: VRBGFLMeter8.visible=1: VRBGFLMeter16.visible=1
  End If
End Sub


dim sol11lvl, sol25lvl, sol26lvl, sol27lvl, sol28lvl, sol29lvl

sub Flashsol125(level)
	sol25lvl = 1
	sol25timer_timer
end sub

sub sol25timer_timer							
	if Not sol25timer.enabled then
		If VRRoom > 0 Then
			VRBGFL25_1.visible = true
			VRBGFL25_2.visible = true
			VRBGFL25_3.visible = true
			VRBGFL25_4.visible = true
		End If
		f125.visible = true
		sol25timer.enabled = true
	end if
	sol25lvl = 0.8 * sol25lvl - 0.01
	if sol25lvl < 0 then sol25lvl = 0
		If VRRoom > 0 Then
			VRBGFL25_1.opacity = 100 * sol25lvl^1.5
			VRBGFL25_2.opacity = 100 * sol25lvl^1.5
			VRBGFL25_3.opacity = 100 * sol25lvl^1.5
			VRBGFL25_4.opacity = 100 * sol25lvl^2
		End If
	f125.opacity = 100 * sol25lvl^2
	if sol25lvl =< 0 Then
		If VRRoom > 0 Then
			VRBGFL25_1.visible = false
			VRBGFL25_2.visible = false
			VRBGFL25_3.visible = false
			VRBGFL25_4.visible = false
		End If
		f125.visible = false
		sol25timer.enabled = false
	end if
end sub

sub Flashsol126(level)
		sol26lvl = 1
		sol26timer_timer
end sub

sub sol26timer_timer							
	if Not sol26timer.enabled then
		If VRRoom > 0 Then
			VRBGFL26_1.visible = true
			VRBGFL26_2.visible = true
			VRBGFL26_3.visible = true
			VRBGFL26_4.visible = true
		End If
		f126.visible = true
		sol26timer.enabled = true
	end if
	sol26lvl = 0.8 * sol26lvl - 0.01
	if sol26lvl < 0 then sol26lvl = 0
		If VRRoom > 0 Then
			VRBGFL26_1.opacity = 100 * sol26lvl^1.5
			VRBGFL26_2.opacity = 100 * sol26lvl^1.5
			VRBGFL26_3.opacity = 100 * sol26lvl^1.5
			VRBGFL26_4.opacity = 100 * sol26lvl^2
		End If
	f126.opacity = 100 * sol26lvl^2
	if sol26lvl =< 0 Then
		If VRRoom > 0 Then
			VRBGFL26_1.visible = false
			VRBGFL26_2.visible = false
			VRBGFL26_3.visible = false
			VRBGFL26_4.visible = false
		End If
		f126.visible = false
		sol26timer.enabled = false
	end if
end sub

sub Flashsol127(level)
		sol27lvl = 1
		sol27timer_timer
end sub

sub sol27timer_timer							
	if Not sol27timer.enabled then
		If VRRoom > 0 Then
			VRBGFL27_1.visible = true
			VRBGFL27_2.visible = true
			VRBGFL27_3.visible = true
			VRBGFL27_4.visible = true
		End If
		f127.visible = true
		sol27timer.enabled = true
	end if
	sol27lvl = 0.8 * sol27lvl - 0.01
	if sol27lvl < 0 then sol27lvl = 0
		If VRRoom > 0 Then
			VRBGFL27_1.opacity = 100 * sol27lvl^1.5
			VRBGFL27_2.opacity = 100 * sol27lvl^1.5
			VRBGFL27_3.opacity = 100 * sol27lvl^1.5
			VRBGFL27_4.opacity = 100 * sol27lvl^2
		End If
	f127.opacity = 100 * sol27lvl^2
	if sol27lvl =< 0 Then
		If VRRoom > 0 Then
			VRBGFL27_1.visible = false
			VRBGFL27_2.visible = false
			VRBGFL27_3.visible = false
			VRBGFL27_4.visible = false
		End If
		f127.visible = false
		sol27timer.enabled = false
	end if
end sub

sub Flashsol128(level)
		sol28lvl = 1
		sol28timer_timer
end sub

sub sol28timer_timer							
	if Not sol28timer.enabled then
		If VRRoom > 0 Then
			VRBGFL28_1.visible = true
			VRBGFL28_2.visible = true
			VRBGFL28_3.visible = true
			VRBGFL28_4.visible = true
		End If
		f128.visible = true
		sol28timer.enabled = true
	end if
	sol28lvl = 0.8 * sol28lvl - 0.01
	if sol28lvl < 0 then sol28lvl = 0
		If VRRoom > 0 Then
			VRBGFL28_1.opacity = 100 * sol28lvl^1.5
			VRBGFL28_2.opacity = 100 * sol28lvl^1.5
			VRBGFL28_3.opacity = 100 * sol28lvl^1.5
			VRBGFL28_4.opacity = 100 * sol28lvl^2
		End If
	f128.opacity = 100 * sol28lvl^2
	if sol28lvl =< 0 Then
		If VRRoom > 0 Then
			VRBGFL28_1.visible = false
			VRBGFL28_2.visible = false
			VRBGFL28_3.visible = false
			VRBGFL28_4.visible = false
		End If
		f128.visible = false
		sol28timer.enabled = false
	end if
end sub

sub Flashsol129(level)
	If VRRoom > 0 Then
		sol29lvl = 1
		sol29timer_timer
	End If
end sub

sub sol29timer_timer							
	if Not sol29timer.enabled then
		VRBGFL29_1.visible = true
		VRBGFL29_2.visible = true
		VRBGFL29_3.visible = true
		VRBGFL29_4.visible = true
		VRBGFL29_5.visible = true
		VRBGFL29_6.visible = true
		VRBGFL29_7.visible = true
		VRBGFL29_8.visible = true
		sol29timer.enabled = true
	end if
	sol29lvl = 0.8 * sol29lvl - 0.01
	if sol29lvl < 0 then sol29lvl = 0
	VRBGFL29_1.opacity = 100 * sol29lvl^1.5
	VRBGFL29_2.opacity = 100 * sol29lvl^1.5
	VRBGFL29_3.opacity = 100 * sol29lvl^1.5
	VRBGFL29_4.opacity = 100 * sol29lvl^2
	VRBGFL29_5.opacity = 100 * sol29lvl^1.5
	VRBGFL29_6.opacity = 100 * sol29lvl^1.5
	VRBGFL29_7.opacity = 100 * sol29lvl^1.5
	VRBGFL29_8.opacity = 100 * sol29lvl^2
	if sol29lvl =< 0 Then
		VRBGFL29_1.visible = false
		VRBGFL29_2.visible = false
		VRBGFL29_3.visible = false
		VRBGFL29_4.visible = false
		VRBGFL29_5.visible = false
		VRBGFL29_6.visible = false
		VRBGFL29_7.visible = false
		VRBGFL29_8.visible = false
		sol29timer.enabled = false
	end if
end sub

sub Flashsol111(level)
	If VRRoom > 0 Then
		sol11lvl = 1
		sol11timer_timer
	End If
end sub

sub sol11timer_timer			
	dim obj
	if Not sol11timer.enabled then
		for each obj in VRBGFL11 : obj.visible = 1 : Next	
		sol11timer.enabled = true
	end if
	sol11lvl = 0.8 * sol11lvl - 0.01
	if sol11lvl < 0 then sol11lvl = 0
	for each obj in VRBGFL11 : obj.opacity = 100 * sol11lvl^2 : Next
	if sol11lvl =< 0 Then
		for each obj in VRBGFL11 : obj.visible = 0 : Next
		sol11timer.enabled = false
	end if
end sub


'*******************************************
'  VR Plunger Code
'*******************************************

Sub TimerVRPlunger_Timer
  If VRPlunger.Y < 2385.993 then
       VRPlunger.Y = VRPlunger.Y + 6
  End If
End Sub

Sub TimerVRPlunger1_Timer
  VRPlunger.Y = 2268.993 + (5* Plunger.Position) -20
End Sub

'********************************************
'*   LUT Selector
'********************************************																																				"

dim textindex : textindex = 1
dim charobj(55), glyph(201)
InitDisplayText

Sub myChangeLut
	table1.ColorGradeImage = luts(lutpos)
	DisplayText lutpos, luts(lutpos)
	vpmTimer.AddTimer 2000, "If lutpos = " & lutpos & " then for anr = 10 to 54 : charobj(anr).visible = 0 : next'"
End Sub

Sub InitDisplayText
	Dim anr
	For anr = 10 to 54 : set charobj(anr) = eval("text0" & anr) : charobj(anr).visible = 0 : Next
	For anr = 32 to 96 : glyph(anr) = anr : next
	For anr = 0 to 31 : glyph(anr) = 32 : next
	for anr = 97 to 122 : glyph(anr)  = anr - 32 : next
	for anr = 123 to 200 : glyph(anr) = 32 : next
End Sub

Sub DisplayText(nr, luttext)
	dim tekst, anr
	for anr = 10 to 54 : charobj(anr).imageA = 32 : charobj(anr).visible = 1 : next
	If nr > -1 then
		tekst = "lutpos:" & nr
		For anr = 1 to len(tekst) : charobj(43 + anr).imageA = glyph(asc(mid(tekst, anr, 1))) : Next
	End If
	For anr = 1 to len(luttext)
		charobj(9 + anr).imageA = glyph(asc(mid(luttext, anr, 1)))
		If nr = -1 Then
			charobj(9 + anr).y = 1500 + sin(((textindex * 4 + anr)/20)*3.14) * 100
			charobj(9 + anr).height = 150 + cos(((textindex * 4 + anr)/20)*3.14) * 100
		End If
	Next
End Sub


'===============
'	Changelog
'===============
'001 tomate - new LP ramps added
'002 tomate - pf lenght fixed
'003 tomate - new fantastic scanned PF added, courtesy of bord & Chris 
'004 tomate - new prims imported at layer 4 (ON prims), low poly ramp and upper PF prims imported at layer 5, bunch of things aligned at layer 1,
'005 Sixtoe - Completely rebuilt VPX from scratch, rebuilding it with new dimensions and layout. Added physics objects and materials. Modified collidable ramps. Script heavily butchered.
'006 Sixtoe - Added nfozzy physics and fleep sound, fixed some other issues. Physics are...funky? (Based on Indi code), balls bounce off pops back up into lanes a lot.
'007 iaakki - Various physics fixes
'008 Sixtoe - More physics fixes, added ramp roofs, added flupper flashers, 
'009 Sixtoe - Nfozzy Lamps, Primitive Inserts, Insert Blooms, Flasher fixes / assignments, physics settings, various other table fixes.
'010 tomate - old prims erased, all VPX elements set to not visible, new prims added, new baked textures added
'011 tomate - flippers, drop targets, standup targets, ramp triggers and up/down ramp separated. Some other fixes
'012 Sixtoe - Changed Physics to Late 80's, added astro's text overlay, added custom temp playfield to test inserts (hybrid of astro's and tomates at low res), hooked up prim flippers, adjusted and hooked up ramp insert flashers, dropped GI, messed around with insert lighting in general.
'013 tomate - flippers length corrected, flippers triggers fixed, new flippers primitives added, plastic ramps and upper PF textures and prims added, right ramp Roof added to prevent ball stuck, new GION_pf texture added, low poly plunger ramp added, new plastics texture added, some other minor fixes
'014 Sixtoe - Ramp fixed by turning material active (doh, thanks unclepaulie), main prim flashers respositioned, walls and rubbers adjusted to remove ball traps, split l51 - l56 inlane lights out from upper playfield, relit them and changed colour from red to incandescent, fixed vr cab to be non-static and set primitves to toys, adjusted top gates to stop ball trap, turned off the transmit from the underplayfield GI, made VR collections, added textures for upperpf flashers, hooked up primtive drawbridge and drawbridge protector so they move and deleted old ramp one, brightened ramp texture, adjusted upper pf l28 - l32 lights to correct position, added rebaked textures from tomate to remove some unwanted reflections, tried to relight drawbridge as it lets white light through (need to work on this more), other minor tweaks, adjustments and cleanup
'015 tomate - new triggers prims and texture added, g01 texture redone, upperPF texture redone, g02 texture fixed
'016 tomate - new g02 texture
'017 Sixtoe - Redid target & spinner primitives, added spinner code and roth droptarget & standup target code, added bloom to upper lights.
'018 tomate - PF mesh added, slightly retouched right VUK area
'019 Sixtoe - Fixed sw49 & sw50 heights, connected sw49 & sw50 primitives up to move, changed spinner wire primtive material, redid drawbridge flasher (needs fading script & more tweaking, will wait until gi-off finished), changed top right kicker to a physics kicker and rebuilt the surrounding area to cope, replaced top right target with stopper and hat, fixed ball trap up there, redid upper playfield flashers, rebuilt VR cab to (mostly) use common origin point and raised to match table, added VRRoom switch and control, lowered standup target positions, added collidable metalpost near top right kicker as ball sometimes jumps on that plastic, changes rectangle inserts because old ones were acting weird, fixed VR plunger, deleted top right target, added kicker cup prim, 
'020 tomate - added VUK stoper primitive to g02, drop targets reflections removed from plastic ramps texture, some anoying reflections removed from upper PF texture
'021 tomate - Plastics cutouts and flippers texture fixed, added GIOFF textures and asigned to new prims at layer 5
'022 Sixtoe - Something, can't remember.
'023 apophis - GI material fades Lampz integration.
'024 tomate - PF_off added to GI
'025 leojreimroc - VR Backglass.  Changed VR Backbox.  Fade timers added to flashers.  Animated VR Plunger and buttons.  Minor VR tweaks
'026 Sixtoe - Fixed right flipper post past, fixed GI on loads of prims, separated and hooked up moving gates, sorted out drop targets, hooked up other moving "gi_off" objects including flippers, hooked up flasher blooms, fixed all transparent images, fixed centre ramp roof texture, 
'027 Sixtoe - Added dynamic shadows, fixed plastics depth bias issue, fixed rear flashers broken by backglass flashers, updated physics to latest version (from blood machines?), 
'028 Sixtoe - Updated flipper physics to enable post passing (thanks Clark Kent), added top left kicker exit sound, lightend GI off playfield
'029 tomate - drawbridge_flash texture remplaced, GIOFF_pfcutout texture remplaced, bloom strength lowered to 0.1, ball texture remplaced
'030 bord - flipper length/shape/strength changes, added custom LUT
'031 tomate - fiddled a little with the upper PF hole
'032 Sixtoe - Minor flipper position correction (thanks ClarkKent), realigned and adjusted drawbridge flasher ramp including materials, repositioned flasher and lights and flares to try and stop the light cutoff, brightened flasher base, adjusted top of drawbridge ramp to stop flyballs into ramp, adjusted centre ramp end stopper to be less bouncy, resaved pf_off to remove white lines.
'033 Sixtoe - Split pop bumper primitives and animated them, optimised biiger textures a bit (shaved 100meg off, can probably do more)
'034 apophis - Updated ball and ramp rolling scripts and sounds. Added ramp rolling triggers. Updated Roth DT script to latest version including new fleep sound effects. Added motor sound effect. Updated relay sound effects. Removed all GetBalls calls, updated to gBOT. Updated DynamicShadows collection. Automated CabinetMode setting. Updated sw52_Hit to handle only enable switch if ramp is down (fixes a bug?)
'035 Sixtoe - Added drain resiliance (thanks Apophis), fixed jackpot and draw poker inserts, adjusted tilt amount and sensitivity,
'036 apophis - Made flasher bases DL react to GI level.
'037 Sixtoe - Added flasher objects to layer 10, sideblade flasher names;
'038 fluffheaed35 - Added Lut Selector
'039 iaakki - tied sideblade flashers to domes and reworked dome code to have some fadeup speed. This table seems to flicker the solenoids differently than many other.
'040 Sixtoe - Turned on dtanim timer again, added playfield flashers, tweaked sideblade flashers, still quirky.
'041 iaakki - reworked solgi sub (there are still few items that should be moved to giupdates and tied to alvl value), modified prims to fade between 0-1: it used to be 0.25-1, Reduced GI fading speed a lot and now the fading is also working correctly
'042 apophis - Rebuilt center ramp. Reverted to original sw52_hit sub. Fixed ballbrightness script. Made LampsTimer a fixed interval.
'043 Sixtoe - Added GI adjuster, changed collidable centre ramp so it's hittable from outside, hooked up ramp flasher.
'044 apophis - Fixed the confused ramp issue.
'045 Sixtoe - Redid flashers, changed primitves to ear ones and dropped them below the plastics, created new Orange textures, redid the sideblade flashers in orange, tweaked and fiddled a bit more with flasher settings. Tweaked top right vuk area to remove potential edge case ball trap.
'046 Sixtoe - Optimised textures, added new flasher prims and textures to bottom visible flasher bases (thanks Bord), turned up the playfield flashers, some script tidying.
'047 Tomate - Skewed central ramp texture fixed
'048 apophis - Rewrote ramp again, fixing issues with tilt and slamtilt, removed timer for sw52.
'049 Sixtoe - Added fully working desktop background including lighting, rebuilt right physical ramp, adjust visible ramp to compensate, cleaned playfield mesh, adjusted top right vuk, added spinner to lighting system, added drop holes, adjusted red lights, tidied up script a bit more
'V1.0 Release
'V1.0.1 Bugfix Release for desktop components showing through on cabinet mode.
'v1.0.2 Added blocker wall around ramps to stop ball traps, tweaked lots of little things to make some texture fading better, fixed some textures not swapping correctlly, fixed left ramp flasher bug, added some automated script to make card deck code change between VR, Cabinet and Desktop (they all need different settings to work!), fixed auto cabinetmode code, shrunk legs on flashers.
'v1.1 Release
'v1.1.1 tomate - Visible ramp through plastic fixed, collidable ramp raised too high and caused jams fixed, small tweaks to plastic ramp textures
'v1.1.2 tomate - roof at the back right and back left sides to prevent ball stucks added, collidable roof at the end of the right ramp added (the one that exists in the real machine), slingshots plastics texture slightly modified
'v1.1.3 apophis - Fixed GIOFF and GION _pastics05 and _decals texture alpha issues. Added slingshot corrections. Added wall under playfield_mesh near flippers. Fixed inlane wall physics material setting.
'v1.2 Release
