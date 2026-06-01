' Taito's Space Shuttle / IPD No. 4583 / 4 Players
' VPX - version by JPSalas 2019, version 2.0.0

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1.1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "Taito.vbs", 3.26

Dim bsTrough, dtCbank, dtRbank, bsLeftSaucer, bsRightSaucer, x, musicplaying, ballnum
musicplaying = False
ballnum = 1

Const cGameName = "sshuttle"

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0
Const vpmhidden = 1 'hide the vpinmame window
'----- FlexDMD Options -----
Dim UseFlexDMD:UseFlexDMD = 0   	' 0 = off, 1 = on. Replaces external dmd when enabled 
Const FlexScoreColour = 0			' 0 = blue (default), 1 = violet, 2 = red, 3 = Green, 4 = yellow
Const FlexGameStatusColour = 1		' 0 = blue, 1 = violet (default), 2 = red, 3 = Green, 4 = yellow


If Table1.ShowDT = true then
    For each x in aReels
        x.Visible = 1
    Next
else
    For each x in aReels
        x.Visible = 0
    Next
end if

' Standard Sounds
Const SSolenoidOn = "fx_SolenoidOn"
Const SSolenoidOff = "fx_SolenoidOff"
Const SCoin = "fx_Coin"

'******************
' Realtime Updates
'******************

Set Motorcallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates
    GIUpdate
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
    Diverter.RotZ = DiverterFlipper.CurrentAngle
	Popup.Z = PostFlipper.CurrentAngle
End Sub

'************
' Table init.
'************

Sub table1_Init

	' initalise the FlexDMD display
    If UseFlexDMD Then FlexDMD_Init

'NVramPatchLoad
    vpmInit me

	' Ball texture
	Table1.BallFrontDecal = "sphere"
	Table1.BallDecalMode = True
	'Table1.BallImage = "Ball_Final"
	Table1.DefaultBulbIntensityScale = 0
	Table1.BallPlayfieldReflectionScale = .25

    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Space Shuttle - Taito 1984" & vbNewLine & "VPX table by JPSalas v.2.0.0"
		.Games(cGameName).Settings.Value("sound") = 0
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = vpmhidden
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
		If UseFlexDMD Then ExternalEnabled = .Games(cGameName).Settings.Value("showpindmd")
		If UseFlexDMD Then .Games(cGameName).Settings.Value("showpindmd") = 0
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With
	
	'PlaySound "gq_commander",-0
	'IntroAudio

    ' Nudging
    vpmNudge.TiltSwitch = 30
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 1, 11, 21, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 3
    End With

    ' Saucers
    Set bsRightSaucer = New cvpmBallStack
    bsRightSaucer.InitSaucer sw3, 3, 180, 8
    bsRightSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsRightSaucer.KickForceVar = 3

    Set bsLeftSaucer = New cvpmBallStack
    bsLeftSaucer.InitSaucer sw2, 2, 180, 8
    bsLeftSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsLeftSaucer.KickForceVar = 3

    ' Drop targets

    set dtCbank = new cvpmdroptarget
    dtCbank.InitDrop sw43, 43
    dtCbank.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtCbank.CreateEvents "dtCbank"

    set dtRbank = new cvpmdroptarget
    dtRbank.InitDrop Array(sw4, sw14, sw24), Array(4, 14, 24)
    dtRbank.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtRbank.CreateEvents "dtRbank"

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Turn on Gi
    vpmtimer.addtimer 2000, "GiOn '"
    solpost 0
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit
	'NVramPatchExit
	If UseFlexDMD then
		If IsObject(FlexDMD) Then 
			If Not FlexDMD is Nothing Then
				FlexDMD.Show = False
				FlexDMD.Run = False
				FlexDMD = NULL
			End If
		End if
		Controller.Games(cGameName).Settings.Value("showpindmd") = ExternalEnabled
	End if	
	Controller.stop
End Sub



'**************

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySoundAt "fx_PlungerPull", Plunger:Plunger.Pullback
    If keycode = RightFlipperKey Then Controller.Switch(31) = 1
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = RightFlipperKey Then Controller.Switch(31) = 0
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
    DOF 101, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 42
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    DOF 102, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 41
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 35:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 55:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper2:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 45:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper3:End Sub



' Drain & Saucers
'Sub Drain_Hit:random_drain:PlaysoundAt "fx_drain", Drain:bsTrough.AddBall Me:End Sub

Sub Drain_Hit()   
	If LampState(GameOverLampID)Then

	Else
		If musicplaying=true Then
			Dim rDrain
			rDrain=0
			rDrain = INT(8 * RND(1) )
			Select Case rDrain
			Case 0:PlaySound "gq_extermination",-0
			Case 1:PlaySound "gq_notright",-0
			Case 2:PlaySound "gq_grabthar",-0
			Case 3:PlaySound "gq_nevergiveup",-0
			Case 4:PlaySound "gq_onejob",-0
			Case 5:PlaySound "gq_episode",-0
			Case 6:PlaySound "gq_notright",-0
			Case 7:PlaySound "gq_notright",-0
			End Select
		End If
		
	End If
	PlaysoundAt "fx_drain", Drain
	vpmTimer.addTimer 2500, "bsTrough.addball Drain '"
	
End Sub

Sub sw3_Hit:PlaysoundAt "fx_kicker_enter", sw3:vpmTimer.addTimer 1500, "bsRightSaucer.AddBall 0 '":End Sub
Sub sw2_Hit:PlaysoundAt "fx_kicker_enter", sw2:vpmTimer.addTimer 1500, "bsLeftSaucer.AddBall 0 '":End Sub

' Rollovers
Sub sw51_Hit:Controller.Switch(51) = 1:PlaySoundAt "fx_sensor", sw51:End Sub
Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:PlaySoundAt "fx_sensor", sw61:PlaySound "gq_drain":End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub

'Sub sw101_Hit:PlaySound "gq_grabthar":Controller.Switch(101)=1:End Sub	'101
'Sub sw101_unHit:Controller.Switch(101)=0:End Sub
'Sub sw101_Hit:PlaySound "gq_grabthar":Controller.Switch(101)=1:End Sub	'101
'Sub sw101_unHit:Controller.Switch(101)=0:End Sub

Sub sw52_Hit:Controller.Switch(52) = 1:PlaySoundAt "fx_sensor", sw52:End Sub
Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub

Sub sw62_Hit:Controller.Switch(62) = 1:PlaySoundAt "fx_sensor", sw62:PlaySound "gq_drain":End Sub
Sub sw62_UnHit:Controller.Switch(62) = 0:End Sub


Sub sw102_Hit:Controller.Switch(102) = 1:PlaySoundAt "gq_flyby", sw102:End Sub
Sub sw102_unHit:Controller.Switch(102)=0:End Sub

Sub sw105_Hit:Controller.Switch(105) = 1:PlaySoundAt "gq_zoom", sw105:End Sub
Sub sw105_unHit:Controller.Switch(105)=0:End Sub


'Sub sw5_Hit:Controller.Switch(5) = 1:PlaySoundAt "fx_sensor", sw5:End Sub
'Sub sw5_UnHit:Controller.Switch(5) = 0:End Sub
Sub sw5_Hit:PlaySound "gq_alien":Controller.Switch(5)=1:End Sub	'5
Sub sw5_unHit:Controller.Switch(5)=0:End Sub

'Sub sw15_Hit:Controller.Switch(15) = 1:PlaySoundAt "fx_sensor", sw15:End Sub
'Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub
Sub sw15_Hit:PlaySound "gq_alien2":Controller.Switch(15)=1:End Sub	'15
Sub sw15_unHit:Controller.Switch(15)=0:End Sub

'Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "fx_sensor", sw25:End Sub
'Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw25_Hit:PlaySound "gq_alien":Controller.Switch(25)=1:End Sub	'25
Sub sw25_unHit:Controller.Switch(25)=0:End Sub

Sub sw44_Hit:Controller.Switch(44) = 1:PlaySoundAt "fx_sensor", sw44:End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub

Sub sw64_Hit:Controller.Switch(64) = 1:PlaySoundAt "fx_sensor", sw64:End Sub
Sub sw64_UnHit:Controller.Switch(64) = 0:End Sub

'Sub sw64_Hit:PlaySound "gq_commander":Controller.Switch(64)=1:End Sub	'64
'Sub sw64_unHit:Controller.Switch(64)=0:End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:PlaySoundAt "gq_crush", sw34:End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub

Sub sw103_Hit:Controller.Switch(103) = 1:PlaySoundAt "gq_rocks", sw103:End Sub
Sub sw103_UnHit:Controller.Switch(103) = 0:End Sub

Sub sw104_Hit:Controller.Switch(104) = 1:PlaySoundAt "gq_rocks", sw104:End Sub
Sub sw104_UnHit:Controller.Switch(104) = 0:End Sub

' Right Ramp Exit
Sub sw65_Hit:PlaySound "gq_shieldsUp":vpmTimer.PulseSw 65:End Sub

Sub sw65_unHit:Controller.Switch(65)=0:End Sub

' Droptargets (only sound effect)
Sub sw4_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), sw4:End Sub
Sub sw14_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), sw14:End Sub
Sub sw24_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), sw24:End Sub
Sub sw43_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), sw43:End Sub

' Spinners
Sub Spinner1_Spin:vpmTimer.PulseSw 71:PlaySoundAt "fx_spinner", Spinner1:End Sub

' Targets
Sub sw13_Hit:vpmTimer.PulseSw 13:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw23_Hit:vpmTimer.PulseSw 23:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw33_Hit:vpmTimer.PulseSw 33:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw63_Hit:vpmTimer.PulseSw 63:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw73_Hit:vpmTimer.PulseSw 73:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
'Sub sw34_Hit:vpmTimer.PulseSw 34:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub

'Rubber animations
Dim Rub1, Rub2

Sub sw22_Hit: vpmTimer.PulseSw 22:Rub1 = 1:sw22_Timer:End Sub
Sub sw22_Timer
    Select Case Rub1
        Case 1:r12.Visible = 1:sw22.TimerEnabled = 1
        Case 2:r12.Visible = 0:r13.Visible = 1
        Case 3:r13.Visible = 0:sw22.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub sw72_Hit: vpmTimer.PulseSw 72:Rub2 = 1:sw72_Timer:End Sub
Sub sw72_Timer
    Select Case Rub2
        Case 1:r10.Visible = 1:sw72.TimerEnabled = 1
        Case 2:r10.Visible = 0:r11.Visible = 1
        Case 3:r11.Visible = 0:sw72.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

' Rubbers
Sub sw12_Hit:vpmTimer.PulseSw 12:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub
Sub sw54_Hit:vpmTimer.PulseSw 54:End Sub
Sub sw72_Hit:vpmTimer.PulseSw 72:End Sub

'*********
'Solenoids
'*********
SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "bsLeftSaucer.SolOut"
SolCallback(3) = "bsRightSaucer.SolOut"
SolCallback(4) = "SetLamp 104,"
SolCallback(5) = "SolPost"
SolCallback(6) = "SolGate"
SolCallback(7) = "dtCbank.SolDropUp"
SolCallback(8) = "dtRbank.SolDropUp"
SolCallback(17) = "SolGi" '17=relay
SolCallback(18) = "vpmNudge.SolGameOn"

Sub SolGi(enabled)
    If enabled Then
        GiOff
    Else
        GiOn
    End If
End Sub

Sub SolPost(Enabled)
    If Enabled Then
        PostFlipper.RotateToStart
		Popupwall.IsDropped = 0
		PlaySound"fx_SolenoidOn"
    Else
        PostFlipper.RotateToEnd
		Popupwall.IsDropped = 1
		PlaySound"fx_SolenoidOff"
    End If
End Sub

Sub SolGate(Enabled)
    If Enabled Then
		PlaySound"fx_SolenoidOn"
        DiverterFlipper.RotateToEnd
    Else
		PlaySound"fx_SolenoidOff"
        DiverterFlipper.RotateToStart
    End If
	PlaySound "gq_airlock",-0
End Sub

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
PlaySound "fx_rubber_flipper", 0, parm/60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
PlaySound "fx_rubber_flipper", 0, parm/60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'*****************
'   Gi Effects
'*****************

Dim OldGiState
OldGiState = -1 'start witht he Gi off

Sub GiON
    For each x in aGiLights
        x.State = 1
    Next
End Sub

Sub GiOFF
    For each x in aGiLights
        x.State = 0
    Next
End Sub

Sub GiEffect(enabled)
    If enabled Then
        For each x in aGiLights
            x.Duration 2, 1000, 1
        Next
    End If
End Sub

Sub GIUpdate
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            GiOn
        End If
		If LampState(GameOverLampID)Then

		Else
			If musicplaying=false Then
				vpmTimer.addTimer 1000, "MusicStart '"			
			End If
		End If
	End If
	If LampState(GameOverLampID)Then
		If musicplaying=true Then
			MusicStop		
			PlaySound "gq_outro",-0
		End If
	End If

	
End Sub
'**********
' Music
'**********


'Start the music
Sub MusicStart
	If musicplaying<>true Then
		DIM music
		musicplaying = true
		music = "GQTheme.mp3"
		PlayMusic "GQTheme.mp3" 
		IntroAudio
	End If
End Sub

Sub MusicStop
	EndMusic 
	StopSound "gq_commander"
	StopSound "gq_playwar"
	StopSound "gq_presence"
	musicplaying = false
	
End Sub

Sub Table1_MusicDone()
PlayMusic "GQTheme.mp3"
End Sub 

Sub IntroAudio()   
	
		Dim iAudio
		iAudio=0
		iAudio = INT(3 * RND(1) )
		Select Case iAudio
		Case 0:PlaySound "gq_commander",-0
		Case 1:PlaySound "gq_playwar",-0
		Case 2:PlaySound "gq_presence",-0
		End Select
	
End Sub

'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
'  just 4 steps, like in Pacdude's original script.
' Included the new Modulated flashers & Lights for WPC
'**********************************************************

Dim LampState(200), FadingState(200), FlashLevel(200)

InitLamps() ' turn off the lights and flashers and reset them to the default parameters

' vpinmame Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3 'fading step
        Next
    End If
    UpdateLeds
    UpdateLamps
    'NVramPatchKeyCheck
End Sub

Sub UpdateLamps()
    Lampm 0, li0
    Flash 0, li0a
    Lampm 1, li1
    Flash 1, li1a
    Lamp 10, li10
    Lamp 100, li100
    Lamp 101, li101
    Lamp 102, li102
    Lamp 103, li103
    Lamp 109, li109
    Lamp 11, li11
    Lamp 110, li110
    Lamp 111, li111
    Lamp 112, li112
    Lamp 113, li113
    Lamp 119, li119
    Lamp 12, li12
    Lamp 120, li120
    Lamp 121, li121
    Lamp 122, li122
    Lamp 123, li123
    Lamp 129, li129
    Lamp 130, li130
    Lampm 131, li131a
    Lamp 131, li131
    Lamp 132, li132
    Lampm 133, li133a
    Lamp 133, li133
    Lamp 143, li143
    Lamp 153, li153
    Lamp 2, li2
    Lamp 20, li20
    Lamp 21, li21
    Lamp 22, li22
    Lamp 30, li30
    Lamp 31, li31
    Lamp 32, li32
    Lamp 40, li40
    Lamp 41, li41
    Lamp 42, li42
    Lamp 50, li50
    Lamp 51, li51
    Lamp 52, li52
    Lamp 60, li60
    Lamp 61, li61
    Lamp 62, li62
    Lamp 70, li70
    Lamp 71, li71
    Lamp 72, li72
    Lamp 79, li79
    Lamp 80, li80
    Lamp 81, li81
    Lamp 82, li82
    Lampm 83, li83
    Flash 83, li83a
    Lamp 89, li89
    Lamp 90, li90
    Lamp 91, li91
    Lamp 92, li92
    Lamp 93, li93
    Lamp 99, li99
    'backdrop lights
        Lamp 139, li139
        Lamp 140, li140
        Lamp 141, li141
        Lamp 142, li142
        Lamp 149, li149
        Lamp 150, li150
        Lamp 151, li151
        Lamp 152, li152
    'Flasher
    Lampm 104, f4
    Lampm 104, f4a
    Lampm 104, f4b
    Lampm 104, f4c
    Lampm 104, f4d
    Lamp 104, f4e
End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 25 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
        LampState(x) = 0
        FadingState(x) = 3 ' used to track the fading state
        FlashLevel(x) = 0
    Next
End Sub

Sub SetLamp(nr, value) ' 0 is off, 1 is on
    FadingState(nr) = abs(value) + 3
End Sub

' Lights: used for VPX standard lights, the fading is handled by VPX itself, they are here to be able to make them work together with the flashers

Sub Lamp(nr, object)
    Select Case FadingState(nr)
        Case 4:object.state = 1:FadingState(nr) = 0
        Case 3:object.state = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Lampm(nr, object) ' used for multiple lights, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:object.state = 1
        Case 3:object.state = 0
    End Select
End Sub

' Flashers: 4 is on,3,2,1 fade steps. 0 is off

Sub Flash(nr, object)
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1:FadingState(nr) = 0
        Case 3:Object.IntensityScale = 0.66:FadingState(nr) = 2
        Case 2:Object.IntensityScale = 0.33:FadingState(nr) = 1
        Case 1:Object.IntensityScale = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1
        Case 3:Object.IntensityScale = 0.66
        Case 2:Object.IntensityScale = 0.33
        Case 1:Object.IntensityScale = 0
    End Select
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub Reel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 0
        Case 3:object.SetValue 2:FadingState(nr) = 2
        Case 2:object.SetValue 3:FadingState(nr) = 1
        Case 1:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub Reelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 2
        Case 2:object.SetValue 3
        Case 1:object.SetValue 0
    End Select
End Sub

Sub NFadeReel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 1
        Case 3:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub NFadeReelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 0
    End Select
End Sub

'Texts

Sub Text(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message:FadingState(nr) = 0
        Case 3:object.Text = "":FadingState(nr) = 0
    End Select
End Sub

Sub Textm(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message
        Case 3:object.Text = ""
    End Select
End Sub

' Modulated Subs for the WPC tables

Sub SetModLamp(nr, level)
    FlashLevel(nr) = level / 150 'lights & flashers
End Sub

Sub LampMod(nr, object)          ' modulated lights used as flashers
    Object.IntensityScale = FlashLevel(nr)
    Object.State = 1             'in case it was off
End Sub

Sub FlashMod(nr, object)         'sets the flashlevel from the SolModCallback
    Object.IntensityScale = FlashLevel(nr)
End Sub

'Walls and mostly Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'fading to off...
        Case 3:object.image = b:FadingState(nr) = 2
        Case 2:object.image = c:FadingState(nr) = 1
        Case 1:object.image = d:FadingState(nr) = 0
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
        Case 2:object.image = c
        Case 1:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'off
        Case 3:object.image = b:FadingState(nr) = 0 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
    End Select
End Sub

'Modulated lights & Flashers

Sub SetModLamp(nr, level)
    FlashLevel(nr) = level / 150 'lights & flashers
End Sub

Sub LightMod(nr, object)         ' modulated lights used as flashers
    Object.IntensityScale = FlashLevel(nr)
    Object.State = 1
End Sub

Sub FlashMod(nr, object) 'sets the flashlevel from the SolModCallback
    Object.IntensityScale = FlashLevel(nr)
End Sub

'************************************
'          LEDs Display
'     Based on Scapino's LEDs
'************************************

Dim Digits(32)
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

'Assign 6-digit output to reels
Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5

Set Digits(6) = b0
Set Digits(7) = b1
Set Digits(8) = b2
Set Digits(9) = b3
Set Digits(10) = b4
Set Digits(11) = b5

Set Digits(12) = c0
Set Digits(13) = c1
Set Digits(14) = c2
Set Digits(15) = c3
Set Digits(16) = c4
Set Digits(17) = c5

Set Digits(18) = d0
Set Digits(19) = d1
Set Digits(20) = d2
Set Digits(21) = d3
Set Digits(22) = d4
Set Digits(23) = d5

Set Digits(24) = e0
Set Digits(25) = e1

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, num, chg, stat
	If UseFlexDMD then FlexDMD.LockRenderThread
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            num = chgLED(ii, 0) : chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            If UseFlexDMD then UpdateFlexChar num, stat
			For jj = 0 to 10
                If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
            Next
        Next
    End IF
	If UseFlexDMD then
		With FlexDMDScene
			If LampState(152) Then 'High score
				.GetLabel("LineText").Text = "HIGH SCORE"
			ElseIf LampState(150) Then 'Tilt
				.GetLabel("LineText").Text = "   TILT    "
			ElseIf LampState(149) Then 'game over
				.GetLabel("LineText").Text = "GAME OVER"
			ElseIf LampState(139) Then 'player 1
				.GetLabel("LineText").Text = " PLAYER 1  "
			ElseIf LampState(140) Then 'player 2
				.GetLabel("LineText").Text = " PLAYER 2  "
			ElseIf LampState(141) Then 'player 3
				.GetLabel("LineText").Text = " PLAYER 3  "
			ElseIf LampState(142) Then 'player 4
				.GetLabel("LineText").Text = " PLAYER 4  "
			End If
		End With
		FlexDMD.UnlockRenderThread
	End If
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 700)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'********************************************
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)

		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz)/17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' =============================================================================================================
'                 NVram patch for Taito do Brasil tables by Pmax65
'
' NVramPatchExit	' Must be placed before the Controler.Stop statement into the Table1_Exit Sub
' NVramPatchLoad	' Must be placed before the VPinMAME controller initialization
' NVramPatchKeyCheck' Must be placed in the lamptimer timer
' =============================================================================================================

Const GameOverLampID = 149 ' set this constant to the ID number of the game-over lamp

Dim NVramPatchCoinCnt

Function GetNVramPath()
    Dim WshShell
    Set WshShell = CreateObject("WScript.Shell")
    GetNVramPath = WshShell.RegRead("HKCU\Software\Freeware\Visual PinMame\globals\nvram_directory")
End function

Function FileExists(FileName)
    DIM FSO
    FileExists = False
    Set FSO = CreateObject("Scripting.FileSystemObject")
    FileExists = FSO.FileExists(FileName)
    Set FSO = Nothing
End Function

Sub Kill(FileName)
    Dim ObjFile, FSO
    On Error Resume Next
    Set FSO = CreateObject("Scripting.FileSystemObject")
    Set ObjFile = FSO.GetFile(FileName)
    ObjFile.Delete
    On Error Goto 0
    Set FSO = Nothing
End Sub

Sub Copy(SourceFileName, DestFileName)
    Dim FSO
    On Error Resume Next
    Set FSO = CreateObject("Scripting.FileSystemObject")
    FSO.CopyFile SourceFileName, DestFileName, True
    On Error Goto 0
    Set FSO = Nothing
End Sub

Sub NVramPatchLoad
    NVramPatchCoinCnt = 0
    If FileExists(GetNVramPath + "\" + cGameName + ".nvb")Then
        Copy GetNVramPath + "\" + cGameName + ".nvb", GetNVramPath + "\" + cGameName + ".nv"
    Else
        Copy GetNVramPath + "\" + cGameName + ".nv", GetNVramPath + "\" + cGameName + ".nvb"
    End If
End Sub

Sub NVramPatchExit
    If LampState(GameOverLampID)Then
        Kill GetNVramPath + "\" + cGameName + ".nvb"
        Do
            LampTimer_Timer          ' This loop is needed to avoid the NVram reset (losing the hi-score and credits)
        Loop Until LampState(20) = 1 ' when the game is over but the match procedure isn't still ended
	End If
End Sub

' =============================================================================================================
' To completely erase the NVram file keep the Start Game button pushed while inserting
' two coins into the first coin slit (this resets the high scores too)
' =============================================================================================================

Sub NVramPatchKeyCheck
    If Controller.Switch(swStartButton)then
        If Controller.Switch(swCoin1)then
            If NVramPatchCoinCnt = 2 Then
                Controller.Stop
                Kill GetNVramPath + "\" + cGameName + ".nv"
                Kill GetNVramPath + "\" + cGameName + ".nvb"
                QuitPlayer 2
            Else
                NVramPatchCoinCnt = 1
            End If
        Else
            If NVramPatchCoinCnt = 1 Then
                NVramPatchCoinCnt = 2
            End If
        End If
    Else
        NVramPatchCoinCnt = 0
    End If
End Sub

'*****************************
' Flex DMD Display - scutters
'*****************************
'**********************************************************
'  4*6 score + 2*1 game indicators numeric segment to flexdmd display conversion
'**********************************************************
Dim FlexDMD
DIm FlexDMDDict, FlexDMDDictComma
Dim FlexDMDScene
Dim ExternalEnabled

Sub FlexDMD_Init() 'default/startup values

	' populate the lookup dictionary for mapping display characters
	FlexDictionary_Init

	Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
	If Not FlexDMD is Nothing Then

		Dim FlexDMDFont
	
		FlexDMD.GameName = cGameName
		FlexDMD.TableFile = Table1.Filename & ".vpx"
		FlexDMD.RenderMode = 2
		FlexDMD.Width = 128
		FlexDMD.Height = 32
		FlexDMD.Clear = True
		FlexDMD.Run = True

		FlexDMD.LockRenderThread

		Set FlexDMDScene = FlexDMD.NewGroup("Scene")
		

		With FlexDMDScene

			'normal background
			.AddActor FlexDMD.NewImage("BackG", "FlexDMD.Resources.dmds.black.png")
			'populate background 
			.AddActor FlexDMD.NewFrame("Frame")
			.GetFrame("Frame").Visible = True
			Select Case FlexScoreColour
			'RGB R & B reversed in flex
			Case 2 'red
				.GetFrame("Frame").FillColor = vbBlue
				.GetFrame("Frame").BorderColor = vbBlue
			Case 3 'green
				.GetFrame("Frame").FillColor = vbGreen
				.GetFrame("Frame").BorderColor = vbGreen
			Case 4 'yellow
				.GetFrame("Frame").FillColor = vbCyan
				.GetFrame("Frame").BorderColor = vbCyan
			Case 1 'violet
				.GetFrame("Frame").FillColor = RGB(184,70,170)			
				.GetFrame("Frame").BorderColor = RGB(184,70,170)
			Case Else 'blue
				.GetFrame("Frame").FillColor = RGB(255,100,0)
				.GetFrame("Frame").BorderColor = RGB(255,100,0)
			End Select
			.GetFrame("Frame").Height = 26
			.GetFrame("Frame").Width= 128
			.GetFrame("Frame").Fill= True
			.GetFrame("Frame").Thickness= 1
		
			.AddActor FlexDMD.NewImage("Back","VPX.DMD_Frame")

			'24 score segment display holders
			dim i, pos, char

			For i = 0 to 11
				Select Case i
					Case 0,1,2
						pos = (i * 9) + 4
					Case 3,4,5
						pos = (i * 9) + 6
					Case 6,7,8
						pos = (i * 9) + 14
					Case 9,10,11
						pos = (i * 9) + 16
				End Select
		
				Select Case i
				Case 2,8
					char = "VPX.DMD_SpaceC"
				Case Else
					char = "VPX.DMD_Space"
				End Select
				'line 1 
				.AddActor FlexDMD.NewImage("Seg" & cstr(i), char)
				.GetImage("Seg" & cstr(i)).SetAlignedPosition pos,0 ,0
				'line 2
				.AddActor FlexDMD.NewImage("Seg" & cstr(i + 12), char)
				.GetImage("Seg" & cstr(i + 12)).SetAlignedPosition pos,13,0

			Next 

			Select Case FlexGameStatusColour 
			'RGB R & B not reversed in flex when using font!
			Case 2 'red
				Set FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbRed, vbBlue, 0)
			Case 3 'green
				Set FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbGreen, vbBlue, 0)
			Case 4 'yellow
				Set FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbYellow, vbBlue, 0)
			Case 0	'blue
				Set FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(0,100,255), vbBlue, 0)
			Case Else 'violet
				Set FlexDMDFont = FlexDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(170,70,184), vbBlue, 0)
			End Select

			FlexDMDScene.AddActor(FlexDMD.NewLabel("LineCredit", FlexDMDFont, "CREDITS 0"))
			FlexDMDScene.GetLabel("LineCredit").SetAlignedPosition 88, 27,0 
	
			FlexDMDScene.AddActor(FlexDMD.NewLabel("LineBall", FlexDMDFont, "BALL 0"))
			FlexDMDScene.GetLabel("LineBall").SetAlignedPosition 5,27,0

			FlexDMDScene.AddActor(FlexDMD.NewLabel("LineText", FlexDMDFont,"           "))
			FlexDMDScene.GetLabel("LineText").SetAlignedPosition 38,27,0


		End With
	
		FlexDMD.Stage.AddActor FlexDMDScene
		
		FlexDMD.Show = True
		FlexDMD.UnlockRenderThread

	Else
		
		UseFlexDMD = 0
	
	End If

End Sub


Sub FlexDictionary_Init

	Set FlexDMDDict = CreateObject("Scripting.Dictionary")

	FlexDMDDict.Add 0, "VPX.DMD_Space"
	FlexDMDDict.Add 63, "VPX.DMD_0"
	FlexDMDDict.Add 6, "VPX.DMD_1"
	FlexDMDDict.Add 91, "VPX.DMD_2"
	FlexDMDDict.Add 79, "VPX.DMD_3"
	FlexDMDDict.Add 102, "VPX.DMD_4"
	FlexDMDDict.Add 109, "VPX.DMD_5"
	FlexDMDDict.Add 125, "VPX.DMD_6"
	FlexDMDDict.Add 7, "VPX.DMD_7"
	FlexDMDDict.Add 127,"VPX.DMD_8"
	FlexDMDDict.Add 111,"VPX.DMD_9"

	'mapped as actual display char for using inbuilt flex font
	'offset the look up value by 1000
	FlexDMDDict.Add 1000, " "
	FlexDMDDict.Add 1063, "0"
	FlexDMDDict.Add 1006, "1"
	FlexDMDDict.Add 1091, "2"
	FlexDMDDict.Add 1079, "3"
	FlexDMDDict.Add 1102, "4"
	FlexDMDDict.Add 1109, "5"
	FlexDMDDict.Add 1125, "6"
	FlexDMDDict.Add 1007, "7"
	FlexDMDDict.Add 1127,"8"
	FlexDMDDict.Add 1111,"9"

	Set FlexDMDDictComma = CreateObject("Scripting.Dictionary")

	FlexDMDDictComma.Add 0, "VPX.DMD_SpaceC"
	FlexDMDDictComma.Add 63, "VPX.DMD_0C"
	FlexDMDDictComma.Add 6, "VPX.DMD_1C"
	FlexDMDDictComma.Add 91, "VPX.DMD_2C"
	FlexDMDDictComma.Add 79, "VPX.DMD_3C"
	FlexDMDDictComma.Add 102, "VPX.DMD_4C"
	FlexDMDDictComma.Add 109, "VPX.DMD_5C"
	FlexDMDDictComma.Add 125, "VPX.DMD_6C"
	FlexDMDDictComma.Add 7, "VPX.DMD_7C"
	FlexDMDDictComma.Add 127,"VPX.DMD_8C"
	FlexDMDDictComma.Add 111,"VPX.DMD_9C"
	
End sub


Sub UpdateFlexChar(id, value)

	if FlexDMDDict.Exists (value) then
		With FlexDMDScene
			Select Case id
			Case 2,8,14,20
				.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", FlexDMDDictComma.Item (value)).Bitmap
			Case Else
				If id < 24 Then
					.GetImage("Seg" & id).Bitmap = FlexDMD.NewImage("", FlexDMDDict.Item (value)).Bitmap
				ElseIf id < 26 Then
					Dim newvalue
					newvalue = value + 1000		'offset for different dictionary value
					If id = 25 Then
						.GetLabel("LineCredit").Text = "CREDITS " & FlexDMDDict.Item (newvalue)
					Else
						.GetLabel("LineBall").Text = "BALL " & FlexDMDDict.Item (newvalue)
					End If
				End If
			End Select
		End With
	End If

End Sub