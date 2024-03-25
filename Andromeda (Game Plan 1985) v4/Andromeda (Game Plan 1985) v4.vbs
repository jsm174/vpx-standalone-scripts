' Andromeda / IPD No. 73 / Game Plan / September, 1985 / 4 Players
' VPX - version by JPSalas 2020, version 4.0.0. Graphics by Siggi
' vpinmame script based on destruk script

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "01120100", "GamePlan.VBS", 3.1

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn = "fx_solenoidon"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = "fx_Coin"

Dim VarHidden:VarHidden = Table1.ShowDT
if B2SOn = true then VarHidden = 1

Dim x

' Remove desktop items in FS mode
If Table1.ShowDT then
    For each x in aReels
        x.Visible = 1
    Next
Else
    For each x in aReels
        x.Visible = 0
    Next
End If

'************
' Table init.
'************

Dim bsTrough, dtL, dtR, bsLeft
Const cGameName = "andromed"

Sub Table1_Init
    With Controller
        .GameName = cGameName
        'If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Andromeda, GamePlan, 1985" & vbnewline & "Table by jpsalas v4.0"
        .Games(cGameName).Settings.Value("rol") = 0   '1= rotated display, 0= normal
        .Games(cGameName).Settings.Value("sound") = 1 '1 enabled rom sound
        .HandleMechanics = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .ShowTitle = 0
        .Hidden = VarHidden
        On Error Resume Next
        .SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 5
    vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, bumper4, RightSlingshot)

    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 0, 10, 0, 0, 0, 0, 0, 0
    bsTrough.InitKick BallRelease, 90, 8
    bsTrough.InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsTrough.Balls = 1

    Set dtL = New cvpmDropTarget
    dtL.InitDrop dt7, 23
    dtL.InitSnd SoundFX("", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    dtL.CreateEvents "dtL"

    Set dtR = New cvpmDropTarget
    dtR.InitDrop Array(dt1, dt2, dt3, dt4, dt5, dt6), Array(17, 18, 19, 20, 21, 22)
    dtR.InitSnd SoundFX("", DOFContactors), SoundFX("fx_resetdrop", DOFContactors)
    dtR.CreateEvents "dtR"

    Set bsLeft = New cvpmBallStack
    bsLeft.InitSaucer sw24, 24, 50, 10
    bsLeft.InitExitSnd SoundFX("fx_Solenoidon", DOFContactors), SoundFX("fx_Solenoidon", DOFContactors)

    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

    ' turn the RealTimeUpdates timer
    RealTimeUpdates.Enabled = 1

    ' Init through
    Drain2.CreateBall
    Drain2.Kick 180, 1

    LoadLUT
End Sub

'******************
' RealTime Updates
'******************

'Set MotorCallback = GetRef("RealTimeUpdates")

Sub RealTimeUpdates_Timer
    RollingUpdate
End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If KeyCode = RightFlipperKey Then Controller.Switch(40) = 1
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If
    If KeyDownHandler(KeyCode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_PlungerPull", Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""
    If KeyCode = RightFlipperKey Then Controller.Switch(40) = 0
    If KeyUpHandler(KeyCode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_plunger", plunger:Plunger.Fire
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
GiIntensity = 1   'used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'********************
'     Flippers
'********************

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

'*********
'Solenoids
'*********

SolCallback(1) = "dtL.SolDropUp"
SolCallback(2) = "dtR.SolDropUp"
SolCallback(3) = "vpmSolSound SoundFX(""fx_Knocker"",DOFKnocker),"
'4'bumper2
'5'bumper3
'6'bumper4
'7'bumper1
'8'slingshot
SolCallback(9) = "bsLeft.SolOut"
SolCallback(10) = "bsTrough.SolOut"
SolCallback(15) = "SolShift"
SolCallback(16) = "vpmNudge.SolGameOn"
SolCallback(19) = "SetLamp 119,"
SolCallback(32) = "SolGi"

Sub SolGi(Enabled) 'Gi effect
    If Enabled Then
        GiOn
    Else
        GiOff
    End If
End Sub

Dim DrainBalls:DrainBalls = 0

Sub SolShift(Enabled)
    If Enabled Then
        If DrainBalls = 1 Then
            Drain.DestroyBall
            Controller.Switch(11) = 0
            bsTrough.AddBall 0
            DrainBalls = 0
        End If
    End If
End Sub

'*********
' Switches
'*********

' Slings
Dim RStep

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 16
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'Rubbers -scoring and animation
Dim Rub1, Rub2, Rub3, Rub4, Rub5, Rub6, Rub7, Rub8

Sub RubberBand018_Hit:vpmTimer.PulseSw 9:Rub1 = 1:RubberBand018_Timer
End Sub

Sub RubberBand018_Timer
    Select Case Rub1
        Case 1:r030.Visible = 0:r032.Visible = 1:RubberBand018.TimerEnabled = 1
        Case 2:r032.Visible = 0:r033.Visible = 1
        Case 3:r033.Visible = 0:r030.Visible = 1:RubberBand018.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub RubberBand020_Hit:vpmTimer.PulseSw 9:Rub2 = 1:RubberBand020_Timer
End Sub

Sub RubberBand020_Timer
    Select Case Rub2
        Case 1:r034.Visible = 0:r035.Visible = 1:RubberBand020.TimerEnabled = 1
        Case 2:r035.Visible = 0:r036.Visible = 1
        Case 3:r036.Visible = 0:r034.Visible = 1:RubberBand020.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

Sub RubberBand021_Hit:vpmTimer.PulseSw 9:Rub3 = 1:RubberBand021_Timer
End Sub

Sub RubberBand021_Timer
    Select Case Rub3
        Case 1:r030.Visible = 0:r032.Visible = 1:RubberBand021.TimerEnabled = 1
        Case 2:r032.Visible = 0:r033.Visible = 1
        Case 3:r033.Visible = 0:r030.Visible = 1:RubberBand021.TimerEnabled = 0
    End Select
    Rub3 = Rub3 + 1
End Sub

Sub RubberBand010_Hit:vpmTimer.PulseSw 9:Rub4 = 1:RubberBand010_Timer
End Sub

Sub RubberBand010_Timer
    Select Case Rub4
        Case 1:r019.Visible = 0:r023.Visible = 1:RubberBand010.TimerEnabled = 1
        Case 2:r023.Visible = 0:r024.Visible = 1
        Case 3:r024.Visible = 0:r019.Visible = 1:RubberBand010.TimerEnabled = 0
    End Select
    Rub4 = Rub4 + 1
End Sub

Sub RubberBand016_Hit:vpmTimer.PulseSw 9:Rub5 = 1:RubberBand016_Timer
End Sub

Sub RubberBand016_Timer
    Select Case Rub5
        Case 1:r021.Visible = 0:r025.Visible = 1:RubberBand016.TimerEnabled = 1
        Case 2:r025.Visible = 0:r026.Visible = 1
        Case 3:r026.Visible = 0:r021.Visible = 1:RubberBand016.TimerEnabled = 0
    End Select
    Rub5 = Rub5 + 1
End Sub

Sub RubberBand007_Hit:vpmTimer.PulseSw 9:Rub6 = 1:RubberBand007_Timer
End Sub

Sub RubberBand007_Timer
    Select Case Rub6
        Case 1:r011.Visible = 0:r017.Visible = 1:RubberBand007.TimerEnabled = 1
        Case 2:r017.Visible = 0:r018.Visible = 1
        Case 3:r018.Visible = 0:r011.Visible = 1:RubberBand007.TimerEnabled = 0
    End Select
    Rub6 = Rub6 + 1
End Sub

Sub RubberBand004_Hit:vpmTimer.PulseSw 9:Rub7 = 1:RubberBand004_Timer
End Sub

Sub RubberBand004_Timer
    Select Case Rub7
        Case 1:r022.Visible = 0:r027.Visible = 1:RubberBand004.TimerEnabled = 1
        Case 2:r027.Visible = 0:r028.Visible = 1
        Case 3:r028.Visible = 0:r022.Visible = 1:RubberBand004.TimerEnabled = 0
    End Select
    Rub7 = Rub7 + 1
End Sub

Sub RubberBand009_Hit:Rub8 = 1:RubberBand009_Timer
End Sub

Sub RubberBand009_Timer
    Select Case Rub8
        Case 1:r013.Visible = 0:r015.Visible = 1:RubberBand009.TimerEnabled = 1
        Case 2:r015.Visible = 0:r016.Visible = 1
        Case 3:r016.Visible = 0:r013.Visible = 1:RubberBand009.TimerEnabled = 0
    End Select
    Rub8 = Rub8 + 1
End Sub

'Drain & Kickers
Sub Drain_Hit:PlaySoundAt "fx_drain", Drain:DrainBalls = 1:Controller.Switch(11) = 1:End Sub
Sub sw24_Hit:PlaySoundAt "fx_kicker_enter", sw24:bsLeft.AddBall 0:End Sub

'Spinner
Sub sw4_Spin:vpmTimer.PulseSw(4):PlaysoundAt "fx_spinner", sw4:End Sub

'Lanes
Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "fx_sensor", sw25:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub

Sub sw37_Hit:Controller.Switch(37) = 1:PlaySoundAt "fx_sensor", sw37:End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub

Sub sw38_Hit:Controller.Switch(38) = 1:PlaySoundAt "fx_sensor", sw38:End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:End Sub

Sub sw39_Hit:Controller.Switch(39) = 1:PlaySoundAt "fx_sensor", sw39:End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub

Sub sw30_Hit:Controller.Switch(30) = 1:PlaySoundAt "fx_sensor", sw30:End Sub
Sub sw30_UnHit:Controller.Switch(30) = 0:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:PlaySoundAt "fx_sensor", sw31:End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub

Sub sw32_Hit:Controller.Switch(32) = 1:PlaySoundAt "fx_sensor", sw32:End Sub
Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub

Sub sw35_Hit:Controller.Switch(35) = 1:PlaySoundAt "fx_sensor", sw35:End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:End Sub

'Targets
Sub sw27_Hit:vpmTimer.PulseSw(27):PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw28_Hit:vpmTimer.PulseSw(28):PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw29_Hit:vpmTimer.PulseSw(29):PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub
Sub sw36_Hit:vpmTimer.PulseSw(36):PlaySoundAtBall SoundFX("fx_target", DOFTargets):End Sub

'Bumpers
Sub bumper1_Hit:vpmTimer.PulseSw(15):PlaySoundAt SoundFX("fx_bumper", DOFContactors), bumper1:End Sub
Sub bumper2_Hit:vpmTimer.PulseSw(12):PlaySoundAt SoundFX("fx_bumper", DOFContactors), bumper2:End Sub
Sub bumper3_Hit:vpmTimer.PulseSw(13):PlaySoundAt SoundFX("fx_bumper", DOFContactors), bumper3:End Sub
Sub bumper4_Hit:vpmTimer.PulseSw(14):PlaySoundAt SoundFX("fx_bumper", DOFContactors), bumper4:End Sub

'Droptargets only hit sound
Sub dt1_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), dt1:End Sub
Sub dt2_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), dt2:End Sub
Sub dt3_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), dt3:End Sub
Sub dt4_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), dt4:End Sub
Sub dt5_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), dt5:End Sub
Sub dt6_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), dt6:End Sub
Sub dt7_Hit:PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), dt7:End Sub

'**********************************************************
'     JP's Lamp Fading for VPX and Vpinmame v4.0
' FadingStep used for all kind of lamps
' FlashLevel used for modulated flashers
' LampState keep the real lamp state in a array 
'**********************************************************

Dim LampState(200), FadingStep(200), FlashLevel(200)

InitLamps() ' turn off the lights and flashers and reset them to the default parameters

' vpinmame Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingStep(chgLamp(ii, 0)) = chgLamp(ii, 1)
        Next
    End If
    UpdateLeds
    UpdateLamps
End Sub

Sub UpdateLamps()
    Lamp 1, li1
    Lamp 2, li2
    Lamp 3, li3
    Lamp 4, li4
    Lamp 5, li5
    Lamp 6, li6
    Lamp 7, li7
    Lamp 8, li8
    Lamp 9, li9
    Lamp 10, li10
    Lamp 11, li11
    Lamp 12, li12
    Lamp 13, li13
    Lamp 14, li14
    Lamp 15, li15
    Lamp 16, li16
    Lamp 17, li17
    Lamp 18, li18
    Lamp 19, li19
    Lamp 20, li20
    Lamp 21, li21
    Lamp 22, li22
    Lamp 23, li23
    Lamp 24, li24
    Lamp 25, li25
    Lamp 26, li26
    Lamp 27, li27
    Lamp 28, li28
    Lamp 29, li29
    Lamp 30, li30
    Lamp 31, li31
    Lamp 33, li33
    Lamp 34, li34
    Lamp 35, li35
    Lamp 36, li36
    Lamp 37, li37
    Lamp 38, li38
    Lamp 39, li39
    NFReel 40, TiltReel   '"TILT"
    NFReel 41, HighscoreR '"High Score to date"
    Lamp 42, li42
    Lamp 43, li43
    NFReel 44, GameOverR   '"Game Over"
    NFReel 45, BallinPlayR '"Ball in Play"
    Lamp 46, li46
    Lamp 47, li47
    Lamp 48, li48
    Lamp 49, li49
    Lamp 50, li50
    Lamp 51, li51
    NFReelm 52, ShootAgainR
    Lamp 52, li52
    Lamp 53, li53
    Lamp 54, li54
    Lamp 55, li55
    Lamp 56, li56

    'Flashers
    Lampm 119, f11a
    Lampm 119, f11b
    Lamp 119, f11
End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 10
    LampTimer.Enabled = 1
    For x = 0 to 200
        FadingStep(x) = 0
        FlashLevel(x) = 0
    Next
End Sub

Sub SetLamp(nr, value) ' 0 is off, 1 is on
    FadingStep(nr) = abs(value)
End Sub

' Lights: used for VPX standard lights, the fading is handled by VPX itself, they are here to be able to make them work together with the flashers

Sub Lamp(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.state = 1:FadingStep(nr) = -1
        Case 0:object.state = 0:FadingStep(nr) = -1
    End Select
End Sub

Sub Lampm(nr, object) ' used for multiple lights, it doesn't change the fading state
    Select Case FadingStep(nr)
        Case 1:object.state = 1
        Case 0:object.state = 0
    End Select
End Sub

' Flashers:  0 starts the fading until it is off

Sub Flash(nr, object)
    Dim tmp
    Select Case FadingStep(nr)
        Case 1:Object.IntensityScale = 1:FadingStep(nr) = -1
        Case 0
            tmp = Object.IntensityScale * 0.85 - 0.01
            If tmp > 0 Then
                Object.IntensityScale = tmp
            Else
                Object.IntensityScale = 0
                FadingStep(nr) = -1
            End If
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the fading state
    Dim tmp
    Select Case FadingStep(nr)
        Case 1:Object.IntensityScale = 1
        Case 0
            tmp = Object.IntensityScale * 0.85 - 0.01
            If tmp > 0 Then
                Object.IntensityScale = tmp
            Else
                Object.IntensityScale = 0
            End If
    End Select
End Sub

' Desktop Objects: Reels & texts

' Reels - 4 steps fading
Sub Reel(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.SetValue 1:FadingStep(nr) = -1
        Case 0:object.SetValue 2:FadingStep(nr) = 2
        Case 2:object.SetValue 3:FadingStep(nr) = 3
        Case 3:object.SetValue 0:FadingStep(nr) = -1
    End Select
End Sub

Sub Reelm(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.SetValue 1
        Case 0:object.SetValue 2
        Case 2:object.SetValue 3
        Case 3:object.SetValue 0
    End Select
End Sub

' Reels non fading
Sub NfReel(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.SetValue 1:FadingStep(nr) = -1
        Case 0:object.SetValue 0:FadingStep(nr) = -1
    End Select
End Sub

Sub NfReelm(nr, object)
    Select Case FadingStep(nr)
        Case 1:object.SetValue 1
        Case 0:object.SetValue 0
    End Select
End Sub

'Texts

Sub Text(nr, object, message)
    Select Case FadingStep(nr)
        Case 1:object.Text = message:FadingStep(nr) = -1
        Case 0:object.Text = "":FadingStep(nr) = -1
    End Select
End Sub

Sub Textm(nr, object, message)
    Select Case FadingStep(nr)
        Case 1:object.Text = message
        Case 0:object.Text = ""
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

'Walls, flashers, ramps and Primitives used as 4 step fading images
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingStep(nr)
        Case 1:object.image = a:FadingStep(nr) = -1
        Case 0:object.image = b:FadingStep(nr) = 2
        Case 2:object.image = c:FadingStep(nr) = 3
        Case 3:object.image = d:FadingStep(nr) = -1
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingStep(nr)
        Case 1:object.image = a
        Case 0:object.image = b
        Case 2:object.image = c
        Case 3:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingStep(nr)
        Case 1:object.image = a:FadingStep(nr) = -1
        Case 0:object.image = b:FadingStep(nr) = -1
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingStep(nr)
        Case 1:object.image = a
        Case 0:object.image = b
    End Select
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
Const maxvel = 30 'max ball velocity
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

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'************************************
'          LEDs Display
'************************************

Dim Digits(32)
Digits(0) = Array(D1, D2, D3, D4, D5, D6, D7)
Digits(1) = Array(D11, D12, D13, D14, D15, D16, D17)
Digits(2) = Array(D21, D22, D23, D24, D25, D26, D27)
Digits(3) = Array(D31, D32, D33, D34, D35, D36, D37)
Digits(4) = Array(D41, D42, D43, D44, D45, D46, D47)
Digits(5) = Array(D51, D52, D53, D54, D55, D56, D57)
Digits(6) = Array(D61, D62, D63, D64, D65, D66, D67)
Digits(7) = Array(D71, D72, D73, D74, D75, D76, D77)
Digits(8) = Array(D81, D82, D83, D84, D85, D86, D87)
Digits(9) = Array(D91, D92, D93, D94, D95, D96, D97)
Digits(10) = Array(D101, D102, D103, D104, D105, D106, D107)
Digits(11) = Array(D111, D112, D113, D114, D115, D116, D117)
Digits(12) = Array(D121, D122, D123, D124, D125, D126, D127)
Digits(13) = Array(D131, D132, D133, D134, D135, D136, D137)
Digits(14) = Array(D141, D142, D143, D144, D145, D146, D147)
Digits(15) = Array(D151, D152, D153, D154, D155, D156, D157)
Digits(16) = Array(D161, D162, D163, D164, D165, D166, D167)
Digits(17) = Array(D171, D172, D173, D174, D175, D176, D177)
Digits(18) = Array(D181, D182, D183, D184, D185, D186, D187)
Digits(19) = Array(D191, D192, D193, D194, D195, D196, D197)
Digits(20) = Array(D201, D202, D203, D204, D205, D206, D207)
Digits(21) = Array(D211, D212, D213, D214, D215, D216, D217)
Digits(22) = Array(D221, D222, D223, D224, D225, D226, D227)
Digits(23) = Array(D231, D232, D233, D234, D235, D236, D237)
Digits(24) = Array(D241, D242, D243, D244, D245, D246, D247)
Digits(25) = Array(D251, D252, D253, D254, D255, D256, D257)
Digits(26) = Array(D261, D262, D263, D264, D265, D266, D267)
Digits(27) = Array(D271, D272, D273, D274, D275, D276, D277)
Digits(28) = Array(D281, D282, D283, D284, D285, D286, D287)
Digits(29) = Array(D291, D292, D293, D294, D295, D296, D297)
Digits(30) = Array(D301, D302, D303, D304, D305, D306, D307)
Digits(31) = Array(D311, D312, D313, D314, D315, D316, D317)

Sub UpdateLeds
    Dim ChgLED, ii, num, chg, stat, obj
    ChgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(chgLED)
            num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For Each obj In Digits(num)
                If chg And 1 Then obj.State = stat And 1
                chg = chg \ 2:stat = stat \ 2
            Next
        Next
    End If
End Sub

'*******************
' GI Lights
'*******************

Sub GiOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

'Gameplan Andromeda
'thanks to Albert for this settings.
Sub editDips
    Dim vpmDips:Set vpmDips = New cvpmDips
    With vpmDips
        .AddForm 700, 400, "Gameplan Andromeda - DIP switches"
        .AddFrame 2, 0, 190, "Credits per coin", &H0000001F, Array("1 credit - 2 coins", &H00000031, "1 credit - 1 coin", &H00000002, "2 credits - 1 coin", &H00000004, "5 credits - 1 coin", &H0000000A) 'dip 1-5
        .AddFrame 2, 76, 190, "Maximum credits", &H06000000, Array("10 credits", 0, "20 credits", &H02000000, "30 credits", &H04000000, "40 credits", &H06000000)                                          'dip 26&27
        .AddFrame 2, 152, 190, "High game to date award", &HC0000000, Array("no award", 0, "1 credit", &H40000000, "2 credits", &H80000000, "3 credits", &HC0000000)                                       'dip 31&32
        .AddFrame 210, 0, 190, "Replay or free ball award", &H18000000, Array("no award", 0, "50,000 points", &H08000000, "extra ball", &H10000000, "replay", &H18000000)                                  'dip 28+29
        .AddFrame 210, 76, 190, "Balls per game", &H00C00000, Array("1 ball", 0, "2 balls", &H00400000, "3 balls", &H00800000, "5 balls", &H00C00000)                                                      'dip 23+24
        .AddFrame 210, 152, 190, "Specials per game", &H00004000, Array("1 special only", 0, "2 specials", &H00004000)                                                                                     'dip 15***
        .AddFrame 210, 198, 190, "Spinner points", &H01000000, Array("start at 100 points", 0, "start at 1,000 points", &H01000000)                                                                        'dip 25***                                                                                                                                           'dip 16***
        .AddChk 2, 250, 150, Array("Match feature", &H20000000)                                                                                                                                            'dip 30                                                                                                                                    'dip 14
        .AddLabel 30, 320, 300, 20, "After hitting OK, press F3 to reset game with new settings."
        .ViewDips
    End With
End Sub
Set vpmShowDips = GetRef("editDips")