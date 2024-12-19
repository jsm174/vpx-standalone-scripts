' Snake Machine - Taito 1982
' IPD No. 4585 / 4 Players
' Some playfield design elements were copied from Stern's 1981 'Viper'..
' VPX - version by JPSalas 2020, version 4.0.0

Option Explicit
Randomize

Const BallSize = 50
Const BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "Taito.vbs", 3.26

Dim bsTrough, bsLeftSaucer,bsTopSaucer, dtbankL, dtbankC, dtbankT, plungerIM, x

Const cGameName = "snake"

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
    RightFlipperTop1.RotZ = RightFlipper1.CurrentAngle
End Sub

'************
' Table init.
'************

Sub table1_Init
'NVramPatchLoad
    vpmInit me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Snake Machine - Taito 1982" & vbNewLine & "VPX table by JPSalas v4.0.0"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = vpmhidden
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        .Games(cGameName).Settings.Value("sound") = 0
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
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, LeftSlingshot, RightSlingshot, sw22, sw42, sw52)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 1, 0, 0, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 1
    End With

    ' Saucers
    Set bsLeftSaucer = New cvpmBallStack
    bsLeftSaucer.InitSaucer sw2, 2, 180, 15
    bsLeftSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsLeftSaucer.KickForceVar = 6

    Set bsTopSaucer = New cvpmBallStack
    bsTopSaucer.InitSaucer sw45, 45, 0, 25
    bsTopSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsTopSaucer.KickForceVar = 6

	' Drop targets

	Set dtbankT=New cvpmDropTarget
	dtbankT.InitDrop Array(sw24,sw34,sw44),Array(24,34,44)
	dtbankT.Initsnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtbankT.CreateEvents "dtbankT"

	Set dtbankL=New cvpmDropTarget
	dtbankL.InitDrop Array(sw54,sw64,sw74),Array(54,64,74)
	dtbankL.Initsnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtbankL.CreateEvents "dtbankL"

	Set dtbankC=New cvpmDropTarget
	dtbankC.InitDrop Array(sw55,sw65,sw75),Array(55,65,75)
	dtbankC.Initsnd SoundFX("fx_droptarget", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
    dtbankC.CreateEvents "dtbankC"

    ' Impulse Plunger, used as the kickback
	' the vp kickback plunger will never touch the ball
    Const IMPowerSetting = 38 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP sw11, IMPowerSetting, IMTime
        .Random 0.3
		.switch 11
        .InitExitSnd SoundFX("fx_popper",DOFContactors), SoundFX("fx_popper",DOFContactors)
        .CreateEvents "plungerIM"
    End With
	kickback.pullback

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
    RealTime.Enabled = 1

    ' Turn on Gi
    vpmtimer.addtimer 1500, "GiOn '"

    LoadLUT

MusicOn

End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit
    'NVramPatchExit
    Controller.stop
End Sub



'**********************************************************************************************************
'Bob Marley
'**********************************************************************************************************

Sub MusicOn
	
    Dim FileSystemObject, folder, r, ct, file, musicPath, myMusicFolder
    ' myMusicFolder = "Bob Marley" 
    ' Set FileSystemObject = CreateObject("Scripting.FileSystemObject")
    ' musicPath = FileSystemObject.GetAbsolutePathName(".") ' get path to Visual Pinball\table
    ' musicPath = Left(musicPath, Len(musicPath) - 6) + "music\" 'get path to Visual Pinball\music
    
	' Set folder = FileSystemObject.GetFolder(musicPath + myMusicFolder) 'Comment out if using custom path

	'*****************************************************************************************************************
	'NOTE- If you use a non-standard folder structure {IE not using "VisualPinball\Tables" and "VisualPinball\Music"} 
	'please enter in the direct path to music sub-folder on the "Set Folder=" below and comment out "Set Folder=" Line above.
	'*****************************************************************************************************************
	
	'Set custom path below
	'Set folder= FileSystemObject.GetFolder("C:\vPinball\VisualPinball\Music\Bob Marley")

    Dim music(9)
    music(0) = "Music/Bob Marley/Could You Be Loved - Bob Marley.mp3"
    music(1) = "Music/Bob Marley/I Know - Bob Marley.mp3"
    music(2) = "Music/Bob Marley/Is This Love - Bob Marley.mp3"
    music(3) = "Music/Bob Marley/No Woman No Cry - Bob Marley.mp3"
    music(4) = "Music/Bob Marley/One Love - Bob Marley.mp3"
    music(5) = "Music/Bob Marley/Stiff Necked Fools - Bob Marley.mp3"
    music(6) = "Music/Bob Marley/Stir It Up - Bob Marley.mp3"
    music(7) = "Music/Bob Marley/Sun Is Shining - Bob Marley.mp3"
    music(8) = "Music/Bob Marley/Waiting In Vain - Bob Marley.mp3"
    music(9) = "Music/Bob Marley/Zion Train Lyrics - Bob Marley.mp3"


    Randomize
    r = INT(UBound(music) * Rnd + 1)
    ct=1
    For Each file in music
        if ct = r Then 
            if (LCase(Right(file,4))) = ".mp3" Then ' can only play mp3 files
               PlayMusic file
            End If 
       End If
   ct = ct + 1
   Next
 End Sub
 

 Sub Table1_MusicDone()
    MusicOn
 End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
	If keycode = LeftMagnaSave Then EndMusic
    If keycode = RightMagnaSave Then MusicOn 'Changes tracks
    If vpmKeyDown(keycode)Then Exit Sub
    If keycode = PlungerKey Then PlaySoundAt "fx_PlungerPull", Plunger:Plunger.Pullback
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""
    If vpmKeyUp(keycode)Then Exit Sub
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
Dim LStep, RStep
Dim s22, s42, s52

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
    LeftSling1.Visible = 0
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 32
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:LeftSling1.Visible = 1:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
    RightSling1.Visible = 0
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 12
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:RightSling1.Visible = 1:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub sw22_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk1
    rubber11.Visible = 0
    rubber27.Visible = 1
    Remk1.RotX = 26
    s22 = 0
    vpmTimer.PulseSw 22
    sw22.TimerEnabled = 1
End Sub

Sub sw22_Timer
    Select Case s22
        Case 1:rubber27.Visible = 0:rubber26.Visible = 1:Remk1.RotX = 14
        Case 2:rubber26.Visible = 0:rubber25.Visible = 1:Remk1.RotX = 2
        Case 3:rubber25.Visible = 0:rubber11.Visible = 1:Remk1.RotX = -10:sw22.TimerEnabled = 0
    End Select
    s22 = s22 + 1
End Sub

Sub sw42_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk2
    rubber22.Visible = 0
    rubber33.Visible = 1
    Lemk2.RotX = 26
    s42 = 0
    vpmTimer.PulseSw 42
    sw42.TimerEnabled = 1
End Sub

Sub sw42_Timer
    Select Case s42
        Case 1:rubber33.Visible = 0:rubber32.Visible = 1:Lemk2.RotX = 14
        Case 2:rubber32.Visible = 0:rubber31.Visible = 1:Lemk2.RotX = 2
        Case 3:rubber31.Visible = 0:rubber22.Visible = 1:Lemk2.RotX = -10:sw42.TimerEnabled = 0
    End Select
    s42 = s42 + 1
End Sub

Sub sw52_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk2
    rubber17.Visible = 0
    rubber24.Visible = 1
    Remk2.RotX = 26
    s52 = 0
    vpmTimer.PulseSw 52
    sw52.TimerEnabled = 1
End Sub

Sub sw52_Timer
    Select Case s52
        Case 1:rubber24.Visible = 0:rubber23.Visible = 1:Remk2.RotX = 14
        Case 2:rubber23.Visible = 0:rubber15.Visible = 1:Remk2.RotX = 2
        Case 3:rubber15.Visible = 0:rubber17.Visible = 1:Remk2.RotX = -10:sw52.TimerEnabled = 0
    End Select
    s52 = s52 + 1
End Sub


' Rubber animations
Dim Rub1, Rub2, Rub3

Sub rlband007_Hit:Rub1=1:rlband007_Timer:End Sub
Sub rlband007_Timer
    Select Case Rub1
        Case 1:rubber4.Visible = 0:rubber34.Visible = 1:rlband007.TimerEnabled = 1
        Case 2:rubber34.Visible = 0:rubber35.Visible = 1
        Case 3:rubber35.Visible = 0:rubber4.Visible = 1:rlband007.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub rlband010_Hit:Rub2 = 1:rlband010_Timer:End Sub
Sub rlband010_Timer
    Select Case Rub2
        Case 1:rubber21.Visible = 0:rubber29.Visible = 1:rlband010.TimerEnabled = 1
        Case 2:rubber29.Visible = 0:rubber30.Visible = 1
        Case 3:rubber30.Visible = 0:rubber21.Visible = 1:rlband010.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

Sub rlband006_Hit:Rub3 = 1:rlband006_Timer:End Sub
Sub rlband006_Timer
    Select Case Rub3
        Case 1:rubber12.Visible = 0:rubber36.Visible = 1:rlband006.TimerEnabled = 1
        Case 2:rubber36.Visible = 0:rubber37.Visible = 1
        Case 3:rubber37.Visible = 0:rubber12.Visible = 1:rlband006.TimerEnabled = 0
    End Select
    Rub3 = Rub3 + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 63:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 73:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper2:End Sub

' Drain & Sausers
Sub Drain_Hit:PlaysoundAt "fx_drain", Drain:bsTrough.AddBall Me:End Sub
Sub sw2_Hit:PlaysoundAt "fx_kicker_enter", sw2:bsLeftSaucer.AddBall 0:End Sub
Sub sw45_Hit:PlaysoundAt "fx_kicker_enter", sw45:bsTopSaucer.AddBall 0:End Sub

' Rollovers
Sub sw21_Hit:Controller.Switch(21) = 1:PlaySoundAt "fx_sensor", sw21:End Sub
Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:PlaySoundAt "fx_sensor", sw31:End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub

Sub sw41_Hit:Controller.Switch(41) = 1:PlaySoundAt "fx_sensor", sw41:End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:End Sub

Sub sw51_Hit:Controller.Switch(51) = 1:PlaySoundAt "fx_sensor", sw51:End Sub
Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub

'Targets
Sub sw4_Hit:vpmTimer.PulseSw 4:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw14_Hit:vpmTimer.PulseSw 14:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw3_Hit:vpmTimer.PulseSw 3:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw13_Hit:vpmTimer.PulseSw 13:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw23_Hit:vpmTimer.PulseSw 23:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub
Sub sw33_Hit:vpmTimer.PulseSw 33:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):End Sub

' Spinners
Sub Spinner1_Spin:vpmTimer.PulseSw 61:PlaySoundAt "fx_spinner", Spinner1:End Sub
Sub Spinner2_Spin:vpmTimer.PulseSw 71:PlaySoundAt "fx_spinner", Spinner2:End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "SolLeftSaucer"
SolCallback(3) = "dtbankT.SolDropUp"
SolCallback(4) = "dtbankL.SolDropUp"
SolCallback(5) = "dtbankC.SolDropUp"
SolCallback(6) = "SolTopSaucer"
SolCallback(17) = "Auto_Plunger"
SolCallback(18) = "vpmNudge.SolGameOn"

Dim sw2Pos, sw45Pos

Sub SolLeftSaucer(enabled)
    If enabled Then
        bsLeftSaucer.ExitSol_On
        sw2pos = 1:sw2.TimerEnabled = 1
    End If
End Sub

Sub sw2_Timer
    Select Case sw2Pos
        Case 1:sw2p.rotx = -30
        Case 2:sw2p.rotx = -20
        Case 3:sw2p.rotx = -10
        Case 4:sw2p.rotx = 0:sw2.TimerEnabled = 0
    End select
    sw2Pos = sw2Pos + 1
End Sub

Sub SolTopSaucer(enabled)
    If enabled Then
        bsTopSaucer.ExitSol_On
        sw45pos = 1:sw45.TimerEnabled = 1
    End If
End Sub

Sub sw45_Timer
    Select Case sw45Pos
        Case 1:sw45p.rotx = 30
        Case 2:sw45p.rotx = 20
        Case 3:sw45p.rotx = 10
        Case 4:sw45p.rotx = 0:sw4.TimerEnabled = 0
    End select
    sw45Pos = sw45Pos + 1
End Sub

Sub Auto_Plunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
		kickback.Fire
		kickback.pullback
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
        RightFlipper1.EOSTorque = 0.65:RightFlipper1.RotateToEnd
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.15:RightFlipper.RotateToStart
        RightFlipper1.EOSTorque = 0.15:RightFlipper1.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
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
    Lamp 0, li0
    Lamp 1, li1
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
    Lampm 119, LiBumper1a 
    Lampm 119, LiBumper1b
    Lamp 119, LiBumper1c
    Lamp 12, li12
    Lampm 120, LiBumper2a
    Lampm 120, LiBumper2b 
    Lamp 120, LiBumper2c
    Lamp 122, li122
    Lamp 123, li123
    Lamp 133, li133
    Lamp 143, li143
    Lamp 153, li153
    Lamp 163, li163
    Lamp 2, li2
    'Lamp 20, li20
    Lamp 21, li21
    Lamp 22, li22
    Lamp 30, li30
    Lamp 31, li31
    Lamp 32, li32
    Lamp 40, li40
    Lamp 41, li41
    Lampm 42, li42
    Flash 42, li42a
    Lamp 50, li50
    Lamp 51, li51
    Lamp 52, li52
    Lamp 60, li60
    Lamp 61, li61
    Lamp 62, li62
    Lamp 70, li70
    Lamp 71, li71
    Lamp 72, li72
    'Lamp 79, li79
    Lamp 80, li80
    Lamp 81, li81
    Lamp 82, li82
    Lamp 83, li83
    Lampm 89, li89
    Flash 89, li89a
    Lampm 90, li90
    Flash 90, li90a
    Lampm 91, li91
    Flash 91, li91a
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
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
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
