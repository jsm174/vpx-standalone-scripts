' Shark - Taito 1982
' IPD No. 4582 / 1982 / 4 Players
' VPX - version by JPSalas 2019, version 4.0.0

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1.7

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "Taito.vbs", 3.26

Dim bsTrough, bsSaucer, dtL, dtR, dtR2, mVLock, x

Const cGameName = "sharkt"

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0
Const vpmhidden = 1 'hide the vpinmame window

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

Sub RealTime_Timer
    GIUpdate
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
End Sub

'************
' Table init.
'************

Sub table1_Init
	' NVramPatchLoad
	vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Shark - Taito 1982" & vbNewLine & "VPX table by JPSalas v.4.0.0"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = vpmhidden
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        .Games(cGameName).Settings.Value("sound") = 1
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = 30
    vpmNudge.Sensitivity = 3
    vpmNudge.TiltObj = Array(Bumper1, LeftSlingshot, RightSlingshot, LeftSlingshot1)

    ' Trough
    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 0, 1, 11, 21, 0, 0, 0, 0
    bsTrough.InitKick BallRelease, 90, 4
    bsTrough.InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsTrough.Balls = 3

    ' Drop targets
    Set dtL = New cvpmDropTarget
    dtL.InitDrop Array(sw52, sw62, sw72), Array(52, 62, 72)
    dtL.InitSnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtL.CreateEvents "dtL"

    Set dtR = New cvpmDropTarget
    dtR.InitDrop Array(sw31, sw41, sw51, sw61, sw71), Array(31, 41, 51, 61, 71)
    dtR.InitSnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtR.CreateEvents "dtR"

    Set dtR2 = New cvpmDropTarget
    dtR2.InitDrop Array(sw5, sw15, sw25), Array(5, 15, 25)
    dtR2.InitSnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtR2.CreateEvents "dtR2"

    ' Saucer
    Set bsSaucer = New cvpmBallStack
    bsSaucer.InitSaucer sw2, 2, 0, 21
    bsSaucer.InitExitSnd SoundFX("fx_kicker", DOFDropTargets), SoundFX("fx_solenoid", DOFContactors)
    bsSaucer.KickForceVar = 4

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' Init some switches
    Controller.Switch(60) = 1
    Controller.Switch(70) = 1
    RealTime.Enabled = 1

    ' Turn on Gi
    vpmtimer.addtimer 1500, "GiOn '"

    LoadLUT
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit
    'NVramPatchExit
    Controller.stop
End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If
    If vpmKeyDown(keycode) Then Exit Sub
    If keycode = PlungerKey Then PlaySoundat "fx_PlungerPull",Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""
    If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", Plunger:Plunger.Fire
End Sub

'***************************
'   LUT - Darkness control 
'***************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 15:UpdateLUT:SaveLUT:Lutbox.text = "level of darkness " & LUTImage + 1:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0":GiIntensity = 1:ChangeGIIntensity 1
        Case 1:table1.ColorGradeImage = "LUT1":GiIntensity = 1.05:ChangeGIIntensity 1
        Case 2:table1.ColorGradeImage = "LUT2":GiIntensity = 1.1:ChangeGIIntensity 1
        Case 3:table1.ColorGradeImage = "LUT3":GiIntensity = 1.15:ChangeGIIntensity 1
        Case 4:table1.ColorGradeImage = "LUT4":GiIntensity = 1.2:ChangeGIIntensity 1
        Case 5:table1.ColorGradeImage = "LUT5":GiIntensity = 1.25:ChangeGIIntensity 1
        Case 6:table1.ColorGradeImage = "LUT6":GiIntensity = 1.3:ChangeGIIntensity 1
        Case 7:table1.ColorGradeImage = "LUT7":GiIntensity = 1.35:ChangeGIIntensity 1
        Case 8:table1.ColorGradeImage = "LUT8":GiIntensity = 1.4:ChangeGIIntensity 1
        Case 9:table1.ColorGradeImage = "LUT9":GiIntensity = 1.45:ChangeGIIntensity 1
        Case 10:table1.ColorGradeImage = "LUT10":GiIntensity = 1.5:ChangeGIIntensity 1
        Case 11:table1.ColorGradeImage = "LUT11":GiIntensity = 1.55:ChangeGIIntensity 1
        Case 12:table1.ColorGradeImage = "LUT12":GiIntensity = 1.6:ChangeGIIntensity 1
        Case 13:table1.ColorGradeImage = "LUT13":GiIntensity = 1.65:ChangeGIIntensity 1
        Case 14:table1.ColorGradeImage = "LUT14":GiIntensity = 1.7:ChangeGIIntensity 1
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1   'used for the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep, LStep1

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 23
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
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 33
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

Sub LeftSlingShot1_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk1
    UpperSling4.Visible = 1
    Lemk1.RotX = 26
    LStep1 = 0
    vpmTimer.PulseSw 75
    LeftSlingShot1.TimerEnabled = 1
End Sub

Sub LeftSlingShot1_Timer
    Select Case LStep1
        Case 1:UpperSling4.Visible = 0:UpperSling3.Visible = 1:Lemk1.RotX = 14
        Case 2:UpperSling3.Visible = 0:UpperSling2.Visible = 1:Lemk1.RotX = 2
        Case 3:UpperSling2.Visible = 0:Lemk1.RotX = -10:LeftSlingShot1.TimerEnabled = 0
    End Select
    LStep1 = LStep1 + 1
End Sub

' Scoring rubbers
Dim Rub1, Rub2

Sub sw43_Hit:vpmTimer.PulseSw 43:Rub1 = 1:sw43_Timer:End Sub

Sub sw43_Timer
    Select Case Rub1
        Case 1:rubber14.Visible = 0:rubber16.Visible = 1:sw43.TimerEnabled = 1
        Case 2:rubber16.Visible = 0:rubber17.Visible = 1
        Case 3:rubber17.Visible = 0:rubber14.Visible = 1:sw43.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub sw53_Hit:vpmTimer.PulseSw 53:Rub2 = 1:sw53_Timer:End Sub
Sub sw53_Timer
    Select Case Rub2
        Case 1:rubber8.Visible = 0:rubber18.Visible = 1:sw53.TimerEnabled = 1
        Case 2:rubber18.Visible = 0:rubber19.Visible = 1
        Case 3:rubber19.Visible = 0:rubber8.Visible = 1:sw53.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

Sub sw63_Hit:vpmTimer.PulseSw 63:End Sub

' Saucer
Sub sw2_Hit:bsSaucer.AddBall 0:PlaySoundAt SoundFX("fx_kicker_enter", DOFContactors), sw2:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 73:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:End Sub

' Drain hole
Sub Drain_Hit:PlaysoundAt "fx_drain",Drain:bsTrough.AddBall Me:End Sub

' Spinner
Sub Spinner1_Spin:vpmTimer.PulseSw 13:PlaySoundAt "fx_spinner",Spinner1:End Sub

' Rollovers

Sub sw3_Hit:Controller.Switch(3) = 1:PlaySoundAt "fx_sensor",sw3:End Sub
Sub sw3_UnHit:Controller.Switch(3) = 0:End Sub

Sub sw4_Hit:Controller.Switch(4) = 1:PlaySoundAt "fx_sensor",sw4:End Sub
Sub sw4_UnHit:Controller.Switch(4) = 0:End Sub

Sub sw12_Hit:Controller.Switch(12) = 1:PlaySoundAt "fx_sensor",sw12:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub

Sub sw14_Hit:Controller.Switch(14) = 1:PlaySoundAt "fx_sensor",sw14:End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:PlaySoundAt "fx_sensor",sw24:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:PlaySoundAt "fx_sensor",sw34:End Sub
Sub sw34_UnHit:Controller.Switch(34) = 0:End Sub

Sub sw22_Hit:Controller.Switch(22) = 1:PlaySoundAt "fx_sensor",sw22:End Sub
Sub sw22_UnHit:Controller.Switch(22) = 0:End Sub

Sub sw32_Hit:Controller.Switch(32) = 1:PlaySoundAt "fx_sensor",sw32:End Sub
Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub

Sub sw42_Hit:Controller.Switch(42) = 1:PlaySoundAt "fx_sensor",sw42:End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:End Sub

Sub sw64_Hit:Controller.Switch(64) = 1:PlaySoundAt "fx_sensor",sw64:End Sub
Sub sw64_UnHit:Controller.Switch(64) = 0:End Sub

Sub sw54_Hit:Controller.Switch(54) = 1:PlaySoundAt "fx_sensor",sw54:End Sub
Sub sw54_UnHit:Controller.Switch(54) = 0:End Sub

Sub sw44_Hit:Controller.Switch(44) = 1:PlaySoundAt "fx_sensor",sw44:End Sub
Sub sw44_UnHit:Controller.Switch(44) = 0:End Sub

Sub sw74_Hit:Controller.Switch(74) = 1:PlaySoundAt "fx_sensor",sw74:End Sub
Sub sw74_UnHit:Controller.Switch(74) = 0:End Sub

Sub sw65_Hit:Controller.Switch(65) = 1:PlaySoundAt "fx_sensor",sw65:End Sub
Sub sw65_UnHit:Controller.Switch(65) = 0:End Sub

'Targets
Sub sw35_Hit:vpmTimer.PulseSw 35:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "SolSaucer" '"bsSaucer.SolOut"
SolCallback(3) = "SolLockRelease"
SolCallback(4) = "dtR2.SolDropUp"
SolCallback(7) = "dtR.SolDropUp"
SolCallback(8) = "dtL.SolDropUp"
SolCallback(9) = "GiEffect" 'Strobe mostly the backdrop, but I add a Gi effect
SolCallback(10) = "dtR2.SolHit 1,"
SolCallback(11) = "dtR2.SolHit 2,"
SolCallback(12) = "dtR2.SolHit 3,"
SolCallback(17) = "SolGi"
SolCallback(18) = "vpmNudge.SolGameOn"

Sub SolGi(enabled)
    If enabled Then
        GiOff
    Else
        GiOn
    End If
End Sub

Dim SStep
Sub SolSaucer(Enabled)
If Enabled Then
    Lemk2.RotX = 26
    SStep = 0
	sw2.TimerEnabled = 1
	bsSaucer.ExitSol_On
End If
End Sub

Sub sw2_Timer 'kicker animation
    Select Case SStep
        Case 1:Lemk2.RotX = 14
        Case 2:Lemk2.RotX = 2
        Case 3:Lemk2.RotX = -10:sw2.TimerEnabled = 0
    End Select
    SStep = SStep + 1
End Sub

Sub SolLockRelease(Enabled)
    If Enabled Then
        Lock1a.Isdropped = 1
        Lock2a.Isdropped = 1
        Lock3a.Isdropped = 1
        PlaySoundAt "fx_solenoidon",sw40a2
    Else
        Lock1a.Isdropped = 0
        Lock2a.Isdropped = 0
        Lock3a.Isdropped = 0
        PlaySoundAt "fx_solenoidoff",sw40a2
    End If
End Sub

'*******************
' Flipper Subs v3.0
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.65:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.15:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.65:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.15:RightFlipper.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
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
	strobe.Duration 2, 1000, 0 'backglass effect
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

'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame v3.0
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
    Lamp 0, l0
    Lamp 1, l1
    Lamp 10, l10
    Lamp 100, l100
    Lamp 101, l101
    Lamp 103, l103
    Lamp 109, l109
    Lamp 11, l11
    Lampm 110, l110
    Flash 110, l110a
    Lampm 111, l111
    Flash 111, l111a
    Lampm 112, l112
    Flash 112, l112a
    Lamp 113, l113
    Lamp 119, L119
    Lamp 12, l12
    Lampm 120, l120
	Flash 120, l120a
    Lampm 121, l121
	Flash 121, l121a
    Lampm 122, l122
	Flash 122, l122a
    Lamp 123, l123
    Lamp 129, l129
    Lampm 133, l133
    Flash 133, l133a
    Lamp 140, l140
    Lamp 141, l141
    Lamp 142, l142
    Lampm 143, l143
	Flash 143, l143a
    Lamp 153, l153
    Lamp 2, l2
    Lamp 20, l20
    Lamp 30, l30
    Lampm 31, l31
	Flash 31, l31a
    Lampm 32, l32
	Flash 32, l32a
    Lamp 40, l40
    Lamp 41, l41
    Lamp 42, l42
    Lampm 50, l50
	Flash 50, l50a
    Lampm 51, l51
	Flash 51, l51a
    Lampm 52, l52
	Flash 52, l52a
    Lampm 60, l60
	Flash 60, l60a
    Lampm 61, l61
	Flash 61, l61a
    Lampm 62, l62
	Flash 62, l62a
    Lamp 70, l70
    Lamp 71, l71
    Lamp 72, l72
    Lamp 79, l79
    Lamp 80, l80
    Lamp 81, l81
    Lampm 82, l82a
    Lamp 82, l82
    Lamp 83, l83
    Lamp 89, l89
    Lamp 90, l90
    Lamp 91, l91
    Lamp 92, l92
    Lamp 93, l93
    Lamp 99, l99

    'backdrop lights
        Lamp 139, l139
        Lamp 140, l140
        Lamp 141, l141
        Lamp 142, l142
        Lamp 149, l149
        Lamp 150, l150
        Lamp 151, l151
        Lamp 152, l152
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

'Assign 7-digit output to reels
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
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED) Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj) OR stat = Patterns2(jj) then Digits(chgLED(ii, 0) ).SetValue jj
            Next
        Next
    End IF
End Sub

'************************************
' Diverse Collection Hit Sounds v3.0
'************************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
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
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls
Const lob = 0     'number of locked balls
Const maxvel = 32 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'***********************
' Ball Collision Sound
'***********************

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

Const GameOverLampID = 149	' set this constant to the ID number of the game-over lamp

Dim NVramPatchCoinCnt

Function GetNVramPath()
	Dim WshShell
	Set WshShell = CreateObject("WScript.Shell")
	GetNVramPath = WshShell.RegRead ("HKCU\Software\Freeware\Visual PinMame\globals\nvram_directory")
End function

Function FileExists(FileName)
	DIM FSO
	FileExists = False
	Set FSO = CreateObject("Scripting.FileSystemObject")
	FileExists =FSO.FileExists(FileName)
	Set FSO = Nothing
End Function

Sub Kill(FileName)
	Dim ObjFile,FSO
	On Error Resume Next
	Set FSO = CreateObject("Scripting.FileSystemObject")
	Set ObjFile = FSO.GetFile (FileName)
	ObjFile.Delete
	On Error Goto 0
	Set FSO = Nothing
End Sub

Sub Copy(SourceFileName,DestFileName)
	Dim FSO
	On Error Resume Next
	Set FSO = CreateObject("Scripting.FileSystemObject")
	FSO.CopyFile SourceFileName,DestFileName,True
	On Error Goto 0
	Set FSO = Nothing
End Sub

Sub NVramPatchLoad	
	NVramPatchCoinCnt = 0
	If FileExists(GetNVramPath + "\" + cGameName + ".nvb") Then
		Copy GetNVramPath + "\" + cGameName + ".nvb",GetNVramPath + "\" + cGameName + ".nv"
	Else
		Copy GetNVramPath + "\" + cGameName + ".nv",GetNVramPath + "\" + cGameName + ".nvb"
	End If
End Sub

Sub NVramPatchExit
	If LampState(GameOverLampID) Then 
		Kill GetNVramPath + "\" + cGameName + ".nvb"
		Do
			LampTimer_Timer				' This loop is needed to avoid the NVram reset (losing the hi-score and credits) 
		Loop Until LampState(20) = 1	' when the game is over but the match procedure isn't still ended
	End If
End Sub

' =============================================================================================================
' To completely erase the NVram file keep the Start Game button pushed while inserting 
' two coins into the first coin slit (this resets the high scores too)
' =============================================================================================================

Sub NVramPatchKeyCheck
	If Controller.Switch(swStartButton) then
		If Controller.Switch(swCoin1) then
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