' Vortex - Taito 1983
' IPD No. 4576 / 1983 / 4 Players
' VPX - version by JPSalas 2018, version 1.0.2

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "Taito.vbs", 3.26

Dim bsTrough, dtbank1, dtbank2, dtbank3, bsRightSaucer, x

Const cGameName = "Vortex"

Const UseSolenoids = 1
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0

Dim VarHidden
If Table1.ShowDT = true then
    VarHidden = 1
    For each x in aReels
        x.Visible = 1
    Next
else
    VarHidden = 0
    For each x in aReels
        x.Visible = 0
    Next
    lrail.Visible = 0
    rrail.Visible = 0
end if

if B2SOn = true then VarHidden = 1

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_Coin"

'************
' Table init.
'************

Sub table1_Init
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Alfred Hitchcock's Psycho (TBA 2019)" & vbNewLine & "VPX table by IVANTBA"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        .Games(cGameName).Settings.Value("sound") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

PlaySound "Music",-1
PlaySound "Intro 2",-0
PlaySound "Intro",-0

    ' Nudging
    vpmNudge.TiltSwitch = 30
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 1, 0, 0, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 1
    End With

    ' Saucers
    Set bsRightSaucer = New cvpmBallStack
    bsRightSaucer.InitSaucer sw2, 2, 220, 15
    bsRightSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsRightSaucer.KickForceVar = 6

    ' Drop targets
    set dtbank1 = new cvpmdroptarget
    dtbank1.InitDrop Array(sw35,sw45,sw55,sw65,sw75), Array(35,45,55,65,75)
    dtbank1.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)

    set dtbank2 = new cvpmdroptarget
    dtbank2.InitDrop Array(sw41,sw51,sw61), Array(41,51,61)
    dtbank2.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)

    set dtbank3 = new cvpmdroptarget
    dtbank3.InitDrop Array(sw11,sw21,sw31), Array(11,21,31)
    dtbank3.initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Turn on Gi
    GiOn
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:Controller.stop:End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySound "fx_PlungerPull", 0, 1, 0.1, 0.25:Plunger.Pullback
    if keycode = rightflipperkey then controller.switch(74) = 1
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    if keycode = rightflipperkey then controller.switch(74) = 0
    If vpmKeyUp(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySound "fx_plunger", 0, 1, 0.1, 0.25:Plunger.Fire
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("fx_slingshot", DOFContactors), 0, 1, -0.05, 0.05
    DOF 101, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 71
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
    PlaySound SoundFX("fx_slingshot2", DOFContactors), 0, 1, 0.05, 0.05
    DOF 102, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 64
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

' Scoring rubbers
Sub sw44_Hit:vpmTimer.PulseSw 44:PlaySound SoundFX("fx_rubber_band", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

Sub sw54_Hit:vpmTimer.PulseSw 54:PlaySound SoundFX("fx_rubber_band", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub


' Rubber animations
Dim Rub1, Rub2

Sub RubberBand10_Hit:Rub1 = 1:RubberBand10_Timer:End Sub
Sub RubberBand10_Timer
    Select Case Rub1
        Case 1:r2.Visible = 0:r7.Visible = 1:RubberBand10.TimerEnabled = 1
        Case 2:r7.Visible = 0:r8.Visible = 1
        Case 3:r8.Visible = 0:r2.Visible = 1:RubberBand10.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub RubberBand15_Hit:Rub2 = 1:RubberBand15_Timer:End Sub
Sub RubberBand15_Timer
    Select Case Rub2
        Case 1:r6.Visible = 0:r9.Visible = 1:RubberBand15.TimerEnabled = 1
        Case 2:r9.Visible = 0:r10.Visible = 1
        Case 3:r10.Visible = 0:r6.Visible = 1:RubberBand15.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 3:PlaySound SoundFX("fx_bumper", DOFContactors), 0, 1, -0.05:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 13:PlaySound SoundFX("fx_bumper2", DOFContactors), 0, 1, 0.05:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 23:PlaySound SoundFX("fx_bumper3", DOFContactors), 0, 1, 0:End Sub

' Drain & Saucers
Sub Drain_Hit:Playsound "fx_drain":bsTrough.AddBall Me:End Sub
Sub sw2_Hit::PlaySound "fx_kicker_enter", 0, 1, 0.05:bsRightSaucer.AddBall 0:End Sub

' Rollovers
Sub sw4_Hit:Controller.Switch(4) = 1:PlaySound "fx_sensorsw4", 0, 1, pan(ActiveBall):End Sub
Sub sw4_UnHit:Controller.Switch(4) = 0:End Sub

Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "fx_sensorsw14", 0, 1, pan(ActiveBall):End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:PlaySound "fx_sensorsw24", 0, 1, pan(ActiveBall):End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:PlaySound "fx_sensorsw34", 0, 1, pan(ActiveBall):End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub

Sub sw42_Hit:Controller.Switch(42) = 1:PlaySound "fx_sensorsw42", 0, 1, pan(ActiveBall):End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub

Sub sw52_Hit:Controller.Switch(52) = 1:PlaySound "fx_sensorsw52", 0, 1, pan(ActiveBall):End Sub
Sub sw52_UnHit:Controller.Switch(52) = 0:End Sub

Sub sw62_Hit:Controller.Switch(62) = 1:PlaySound "fx_sensorsw62", 0, 1, pan(ActiveBall):End Sub
Sub sw62_UnHit:Controller.Switch(62) = 0:End Sub


' Droptargets
Sub sw35_Hit:PlaySound SoundFX("fx_droptarget", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw45_Hit:PlaySound SoundFX("fx_droptarget2", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw55_Hit:PlaySound SoundFX("fx_droptarget3", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw65_Hit:PlaySound SoundFX("fx_target4", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw75_Hit:PlaySound SoundFX("fx_droptarget5", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw41_Hit:PlaySound SoundFX("fx_target6", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw51_Hit:PlaySound SoundFX("fx_target3", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw61_Hit:PlaySound SoundFX("fx_droptarget8", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw11_Hit:PlaySound SoundFX("fx_droptarget9", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw21_Hit:PlaySound SoundFX("fx_droptarget10", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw31_Hit:PlaySound SoundFX("fx_droptarget11", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub

Sub sw35_Dropped:dtbank1.Hit 1:End Sub
Sub sw45_Dropped:dtbank1.Hit 2:End Sub
Sub sw55_Dropped:dtbank1.Hit 3:End Sub
Sub sw65_Dropped:dtbank1.Hit 4:End Sub
Sub sw75_Dropped:dtbank1.Hit 5:End Sub
Sub sw41_Dropped:dtbank2.Hit 1:End Sub
Sub sw51_Dropped:dtbank2.Hit 2:End Sub
Sub sw61_Dropped:dtbank2.Hit 3:End Sub
Sub sw11_Dropped:dtbank3.Hit 1:End Sub
Sub sw21_Dropped:dtbank3.Hit 2:End Sub
Sub sw31_Dropped:dtbank3.Hit 3:End Sub

' Spinners
Sub Spinner1_Spin: vpmTimer.PulseSw 22: PlaySound SoundFX("fx_spinner", DOFContactors), 0, 1, 0.05:End Sub
Sub Spinner2_Spin: vpmTimer.PulseSw 12: PlaySound SoundFX("fx_spinner", DOFContactors), 0, 1, -0.05:End Sub
Sub Spinner3_Spin: vpmTimer.PulseSw 32: PlaySound SoundFX("fx_spinner", DOFContactors), 0, 1, 0.05:End Sub

'Targets
Sub sw33_Hit:vpmTimer.PulseSw 33:PlaySound SoundFX("fx_target", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw43_Hit:vpmTimer.PulseSw 43:PlaySound SoundFX("fx_target2", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:PlaySound SoundFX("fx_droptarget7", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw63_Hit:vpmTimer.PulseSw 63:PlaySound SoundFX("fx_droptarget4", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw73_Hit:vpmTimer.PulseSw 73:PlaySound SoundFX("fx_target5", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub
Sub sw72_Hit:vpmTimer.PulseSw 72:PlaySound SoundFX("fx_droptarget6", DOFDropTargets), 0, 1, pan(ActiveBall):End Sub


'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "bsRightSaucer.SolOut"
SolCallback(3) = "dtbank2.SolDropUp"
SolCallback(4) = "dtbank3.SolDropUp"
SolCallback(5) = "dtbank1.SolDropUp"
'SolCallback(6) = ""
SolCallback(7) = "dtbank1.SolHit 1,"
SolCallback(8) = "dtbank1.SolHit 2,"
SolCallback(9) = "dtbank1.SolHit 3,"
SolCallback(10) = "dtbank1.SolHit 4,"
SolCallback(11) = "dtbank1.SolHit 5,"

SolCallback(12) = "SolLeftGi" 'left flash effect
SolCallback(13) = "SolRightGi" 'right flash effect

SolCallback(17) = "SolGi" '17=relay
SolCallback(18) = "vpmNudge.SolGameOn"

Sub SolGi(enabled)
    If enabled Then
        GiOff
    Else
        GiOn
    End If
End Sub

Sub SolLeftGi(enabled)
    If enabled Then
    For each x in aGiLeftLights
        x.State = 0
    Next
    Else
    For each x in aGiLeftLights
        x.State = 1
    Next
    End If
End Sub

Sub SolRightGi(enabled)
    If enabled Then
    For each x in aGiRightLights
        x.State = 0
    Next
    Else
    For each x in aGiRightLights
        x.State = 1
    Next
    End If
End Sub

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, -0.1, 0.05
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, -0.1, 0.05
        LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup", DOFFlippers), 0, 1, 0.1, 0.05
        RightFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown", DOFFlippers), 0, 1, 0.1, 0.05
        RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
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
    End If
End Sub

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200), FlashRepeat(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 10 ' lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0)) = chgLamp(ii, 1) + 4 'actual fading step
        Next
    End If
    If VarHidden Then
        UpdateLeds
    End If
    UpdateLamps
    GIUpdate
    RollingUpdate
End Sub

Sub UpdateLamps()
    NFadeL 0, li0
    NFadeL 1, li1
    NFadeL 10, li10
    NFadeLm 100, li100
    Flash 100, li100a
    NFadeLm 101, li101
    Flash 101, li101a
    NFadeL 102, li102
    NFadeL 103, li103
    NFadeL 109, li109
    NFadeL 11, li11
    NFadeLm 110, LiBumper1a
    NFadeLm 110, LiBumper1b
    NFadeL 110, LiBumper1c
    NFadeLm 111, LiBumper3a
    NFadeLm 111, LiBumper3b
    NFadeL 111, LiBumper3c
    NFadeLm 112, LiBumper2a
    NFadeLm 112, LiBumper2b
    NFadeL 112, LiBumper2c
    NFadeL 113, li113
    NFadeL 12, li12
    NFadeL 123, li123
    NFadeL 133, li133
    NFadeL 143, li143
    NFadeL 153, li153
    NFadeL 2, li2
    NFadeL 20, li20
    NFadeL 21, li21
    NFadeL 22, li22
    NFadeL 30, li30
    NFadeL 31, li31
    NFadeL 32, li32
    NFadeL 40, li40
    NFadeL 41, li41
    NFadeL 42, li42
    NFadeL 50, li50
    NFadeL 51, li51
    NFadeL 52, li52
    NFadeL 60, li60
    NFadeLm 61, li61
    NFadeLm 61, li61a
    Flashm 61, li61b
    Flash 61, li61c
    NFadeL 62, li62
    NFadeLm 70, li70
    Flash 70, li70a
    NFadeLm 71, li71
    Flash 71, li71a
    NFadeLm 72, li72
    Flash 72, li72a
    NFadeL 79, li79
    NFadeL 80, li80
    NFadeL 81, li81
    NFadeLm 82, li82
    Flash 82, li82a
    NFadeL 83, li83
    NFadeL 89, li89
    NFadeL 90, li90
    NFadeL 91, li91
    NFadeLm 92, li92
    Flash 92, li92a
    NFadeL 93, li93
    NFadeLm 99, li99
    Flash 99, li99a
    'backdrop lights
    'If VarHidden Then
        'NFadeL 139, li139
        'NFadeL 140, li140
        'NFadeL 141, li141
        'NFadeL 142, li142
        'NFadeL 149, li149
        'NFadeL 150, li150
        'NFadeL 151, li151
        'NFadeL 152, li152
    'End If
End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.2   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.1 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
        FlashRepeat(x) = 20     ' how many times the flash repeats
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr)Then
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
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
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
            FlashLevel(nr) = FlashLevel(nr)- FlashSpeedDown(nr)
            If FlashLevel(nr) <FlashMin(nr)Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr)> FlashMax(nr)Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change anything, it just follows the main flasher
    Select Case FadingLevel(nr)
        Case 4, 5
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub FlashBlink(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr)- FlashSpeedDown(nr)
            If FlashLevel(nr) <FlashMin(nr)Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 0 AND FlashRepeat(nr)Then 'repeat the flash
                FlashRepeat(nr) = FlashRepeat(nr)-1
                If FlashRepeat(nr)Then FadingLevel(nr) = 5
            End If
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr)> FlashMax(nr)Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
            If FadingLevel(nr) = 1 AND FlashRepeat(nr)Then FadingLevel(nr) = 4
    End Select
End Sub

' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.SetValue 2:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1          'wait
        Case 13:object.SetValue 3:FadingLevel(nr) = 0                  'Off
    End Select
End Sub

Sub FadeRm(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1
        Case 5:object.SetValue 0
        Case 9:object.SetValue 2
        Case 3:object.SetValue 3
    End Select
End Sub

'Texts

Sub NFadeT(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = "":FadingLevel(nr) = 0
        Case 5:object.Text = message:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeTm(nr, object, message)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
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
'Set Digits(0) = a0
'Set Digits(1) = a1
'Set Digits(2) = a2
'Set Digits(3) = a3
'Set Digits(4) = a4
'Set Digits(5) = a5

'Set Digits(6) = b0
'Set Digits(7) = b1
'Set Digits(8) = b2
'Set Digits(9) = b3
'Set Digits(10) = b4
'Set Digits(11) = b5

'Set Digits(12) = c0
'Set Digits(13) = c1
'Set Digits(14) = c2
'Set Digits(15) = c3
'Set Digits(16) = c4
'Set Digits(17) = c5

'Set Digits(18) = d0
'Set Digits(19) = d1
'Set Digits(20) = d2
'Set Digits(21) = d3
'Set Digits(22) = d4
'Set Digits(23) = d5

'Set Digits(24) = e0
'Set Digits(25) = e1

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
            Next
        Next
    End IF
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit2", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber_post", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_rubber_pin", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

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
    Dim BOT, b, ballpitch
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 15000 'increase the pitch on a ramp or elevated surface
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), Pan(BOT(b)), 0, ballpitch, 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub

Sub Table1_Exit():Controller.Games(cGameName).Settings.Value("sound") = 1:Controller.Stop:End Sub