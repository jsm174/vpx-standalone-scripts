' Fire Action De Luxe / IPD No. 4552 / 1983 / 4 Players
' VPX - version by JPSalas 2018, version 4.0.0

Option Explicit
Randomize

'--------------------------------------------------------------------------------------------------------------------
'VR ROOM SELECTION
'--------------------------------------------------------------------------------------------------------------------
Dim VRroom
VRroom = 1 'Set to 0 for arcade sphere and 1 for trailer park/gas station general store

'GLASS WITH SCRATCHES
'--------------------------------------------------------------------------------------------------------------------
Dim Scratches
Scratches = 1 'Set to 0 for no glass with scratches and 1 for glass with scratches
'--------------------------------------------------------------------------------------------------------------------

Const BallSize = 50
Const BallMass = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01550000", "Taito.vbs", 3.26

Dim bsTrough, bsSaucer, bsSaucer2, bsSaucer3, plungerIM, x

Const cGameName = "fireactd"

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0 'set it to 1 if the table runs too fast
Const HandleMech = 0
Const vpmhidden = 1 'hide the vpinmame window

'Hybrid view settings below (automatic, nothing for the user to Set)
'--------------------------------------------------
If Table1.ShowDT = true and RenderingMode <>2 then 'Desktop mode
    For each x in aReels
        x.Visible = True
    Next
	gi060.y = 100
	gi061.y = 100
	gi062.y = 100
	gi063.y = 100
	gi064.y = 100
	gi065.y = 100
	gi066.y = 100
end if

If RenderingMode = 2 or Table1.ShowFSS = -1 or Table1.ShowDT = False then
	For Each x in aReels
		x.Visible = False
	Next
	If RenderingMode = 2 or Table1.ShowFSS = -1 Then
		Flasher001.TimerEnabled = True
		Flasher002.TimerEnabled = True
		DisplayTimer.Enabled = True
	End If
	If RenderingMode = 2 Then
		If VRroom = 0 Then 
			Primary_environment.Visible = True
			Platform1.Visible = True
			Platform2.Visible = True
			Platform3.Visible = True
			Platform4.Visible = True
		End If
		If VRroom = 1 Then
			For Each x in gasstation
				x.Visible = True
			Next
			sign.TimerEnabled = True
			Timer001.Enabled = True
			Timer003.Enabled = True
		End If
	End If
End If

If Scratches = 1 then WindowGlass.Visible = True:GlassImpurities.Visible = True: GlassImpurities1.Visible = True

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
    OrbitGate.Rotz = flippergate.Currentangle
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
        .SplashInfoLine = "Last Starfighter, The (Original 2023) v1.01" & vbNewLine & "VPX table by JPSalas v.4.0.0" & vbNewLine & "Graphics by HiRez00 - Sounds by Xenonph"
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
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 1, 11, 0, 0, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 2
    End With

    ' Saucers
    Set bsSaucer = New cvpmBallStack
    bsSaucer.InitSaucer sw2, 2, 170, 14
    bsSaucer.InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
    bsSaucer.KickForceVar = 3

    ' Impulse Plunger
    Const IMPowerSetting = 31 ' Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP sw21, IMPowerSetting, IMTime
        .Random 0.3
        .switch 21
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_kicker", DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1:PlayMusic"LStarfighter\0lsa01.ogg":a01.enabled=true:a01.interval=93000
    RealTime.Enabled = 1

    ' Turn on Gi
    vpmtimer.addtimer 1500, "GiOn '"
    LoadBWall
    LoadLUT

	setup_backglass
End Sub

Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit:NVramPatchExit:Controller.stop:Controller.Games(cGameName).Settings.Value("sound") = 1:End Sub

'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25:til01.enabled=True
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25:til01.enabled=True
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25:til01.enabled=True
    If KeyCode = RightFlipperKey Then
		flipperbuttonright.x=flipperbuttonright.x-5
		Controller.Switch(74) = 1
	End If
	If KeyCode = LeftFlipperKey Then flipperbuttonleft.x=flipperbuttonleft.x+5
    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If
    If keycode = AddCreditKey or keycode = AddCreditKey2 then c01.enabled=True
    If keycode = StartGameKey Then 
		startb.y=startb.y-5
		s01.enabled=True
	End If
    If Keycode = 3 Then NextBWall
    If vpmKeyDown(keycode) Then Exit Sub
    If keycode = PlungerKey Then 
		PlaySoundAt "fx_kicker", sw21
		Controller.Switch(65) = 1
		launchbutton1.Visible = False:launchbutton2.Visible = True
	End If
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""
    If KeyCode = RightFlipperKey Then
		flipperbuttonright.x=flipperbuttonright.x+5
		Controller.Switch(74) = 0
	End If
	If KeyCode = LeftFlipperKey Then flipperbuttonleft.x=flipperbuttonleft.x-5
	If keycode = StartGameKey Then startb.y=startb.y+5
    If vpmKeyUp(keycode) Then Exit Sub
    If keycode = PlungerKey Then 
		Controller.Switch(65) = 0
		launchbutton1.Visible = True:launchbutton2.Visible = False
	End If	
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

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
    DOF 104, DOFPulse
    LeftSling4.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    vpmTimer.PulseSw 72
    LeftSlingShot.TimerEnabled = 1
FlashLevel5 = 1 : FlasherFlash5_Timer:b01.enabled=True:FlashLevel7 = 1 : FlasherFlash7_Timer
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
    DOF 105, DOFPulse
    RightSling4.Visible = 1
    Remk.RotX = 26
    RStep = 0
    vpmTimer.PulseSw 71
    RightSlingShot.TimerEnabled = 1
FlashLevel6 = 1 : FlasherFlash6_Timer:b05.enabled=True:FlashLevel8 = 1 : FlasherFlash8_Timer
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

' Scoring rubbers & Rubber animations
Dim Rub1, Rub2, Rub3, Rub4, Rub5

Sub sw55_Hit:vpmTimer.PulseSw 55:End Sub

Sub sw45_Hit:vpmTimer.PulseSw 45:Rub1 = 1:sw45_Timer:End Sub
Sub sw45_Timer
    Select Case Rub1
        Case 1:r6.Visible = 0:r15.Visible = 1:sw45.TimerEnabled = 1
        Case 2:r15.Visible = 0:r16.Visible = 1
        Case 3:r16.Visible = 0:r6.Visible = 1:sw45.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub sw44_Hit:vpmTimer.PulseSw 44:Rub2 = 1:sw44_Timer:End Sub
Sub sw44_Timer
    Select Case Rub2
        Case 1:r3.Visible = 0:r1.Visible = 1:sw44.TimerEnabled = 1
        Case 2:r1.Visible = 0:r8.Visible = 1
        Case 3:r8.Visible = 0:r3.Visible = 1:sw44.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

Sub rsband001_Hit:Rub3 = 1:rsband001_Timer:End Sub
Sub rsband001_Timer
    Select Case Rub3
        Case 1:r9.Visible = 0:r11.Visible = 1:rsband001.TimerEnabled = 1
        Case 2:r11.Visible = 0:r12.Visible = 1
        Case 3:r12.Visible = 0:r9.Visible = 1:rsband001.TimerEnabled = 0
    End Select
    Rub3 = Rub3 + 1
End Sub

Sub rsband004_Hit:Rub4 = 1:rsband004_Timer:End Sub
Sub rsband004_Timer
    Select Case Rub4
        Case 1:r10.Visible = 0:r18.Visible = 1:rsband004.TimerEnabled = 1
        Case 2:r18.Visible = 0:r19.Visible = 1
        Case 3:r19.Visible = 0:r10.Visible = 1:rsband004.TimerEnabled = 0
    End Select
    Rub4 = Rub4 + 1
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 15:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper1:FlashLevel1 = 1 : FlasherFlash1_Timer:b01.enabled=True:FlashLevel7 = 1 : FlasherFlash7_Timer:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 5:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper2:FlashLevel2 = 1 : FlasherFlash2_Timer:b03.enabled=True:FlashLevel8 = 1 : FlasherFlash8_Timer:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 25:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper3:FlashLevel3 = 1 : FlasherFlash3_Timer:b04.enabled=True:FlashLevel7 = 1 : FlasherFlash7_Timer:End Sub
Sub Bumper4_Hit:vpmTimer.PulseSw 35:PlaySoundAt SoundFX("fx_bumper", DOFContactors), Bumper4:FlashLevel4 = 1 : FlasherFlash4_Timer:b05.enabled=True:FlashLevel8 = 1 : FlasherFlash8_Timer:End Sub

' Drain & Saucers
Sub Drain_Hit:d01.enabled=True:PlaysoundAt "fx_drain", Drain:bsTrough.AddBall Me:End Sub
Sub sw2_Hit:PlaysoundAt "fx_kicker_enter", sw2:bsSaucer.AddBall 0:StopIGSounds:k01.enabled=True:BL=1:End Sub
Sub sw2_UnHit:BL=0:End Sub

' Rollovers
Sub sw51_Hit:Controller.Switch(51) = 1:PlaySoundAt "fx_sensor", sw51:sd01.enabled=True:End Sub
Sub sw51_UnHit:Controller.Switch(51) = 0:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:PlaySoundAt "fx_sensor", sw61:r01.enabled=true:End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub

Sub sw54_Hit:Controller.Switch(54) = 1:PlaySoundAt "fx_sensor", sw54:roc01.enabled=True:End Sub
Sub sw54_UnHit:Controller.Switch(54) = 0:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:PlaySoundAt "fx_sensor", sw31:sd01.enabled=True:End Sub
Sub sw31_UnHit:Controller.Switch(31) = 0:End Sub

Sub sw41_Hit:Controller.Switch(41) = 1:PlaySoundAt "fx_sensor", sw41:r03.enabled=true:End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:End Sub

Sub sw64_Hit:Controller.Switch(64) = 1:PlaySoundAt "fx_sensor", sw64:grats01.enabled=True:End Sub
Sub sw64_UnHit:Controller.Switch(64) = 0:End Sub

Sub sw33_Hit:Controller.Switch(33) = 1:PlaySoundAt "fx_sensor", sw33:r01.enabled=true:End Sub
Sub sw33_UnHit:Controller.Switch(33) = 0:End Sub

Sub sw43_Hit:Controller.Switch(43) = 1:PlaySoundAt "fx_sensor", sw43:r02.enabled=true:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:End Sub

Sub sw53_Hit:Controller.Switch(53) = 1:PlaySoundAt "fx_sensor", sw53:r02.enabled=true:End Sub
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub

Sub sw63_Hit:Controller.Switch(63) = 1:PlaySoundAt "fx_sensor", sw63:r03.enabled=true:End Sub
Sub sw63_UnHit:Controller.Switch(63) = 0:End Sub

Sub sw73_Hit:Controller.Switch(73) = 1:PlaySoundAt "fx_sensor", sw73:ct01.enabled=True:End Sub
Sub sw73_UnHit:Controller.Switch(73) = 0:End Sub

' and orbit gate
Sub sw14_Hit:Controller.Switch(14) = 1:PlaySoundAt "fx_sensor", sw14
    wallgate.IsDropped = 1
    flippergate.RotateToEnd
    vpmtimer.AddTimer 1000, "CloseOrbitGate '"
End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub

Sub CloseOrbitGate():wallgate.IsDropped = 0:flippergate.RotateToStart:End Sub

' Spinners
Sub sw24_Spin:vpmTimer.PulseSw 24:PlaySoundAt "fx_spinner", sw24:FlashLevel7 = 1 : FlasherFlash7_Timer:FlashLevel8 = 1 : FlasherFlash8_Timer:End Sub

'Targets
Sub sw12_Hit:vpmTimer.PulseSw 12:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r01.enabled=true:End Sub
Sub sw22_Hit:vpmTimer.PulseSw 22:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r01.enabled=true:End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r02.enabled=true:End Sub
Sub sw42_Hit:vpmTimer.PulseSw 42:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r02.enabled=true:End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r03.enabled=true:End Sub
Sub sw62_Hit:vpmTimer.PulseSw 62:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r03.enabled=true:End Sub
Sub sw3_Hit:vpmTimer.PulseSw 3:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r03.enabled=true:End Sub
Sub sw13_Hit:vpmTimer.PulseSw 13:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r03.enabled=true:End Sub
Sub sw23_Hit:vpmTimer.PulseSw 23:PlaySoundAtBall SoundFX("fx_target", DOFDropTargets):r03.enabled=true:End Sub

'*********
'Solenoids
'*********

SolCallback(1) = "bsTrough.SolOut"
SolCallback(2) = "bsSaucer.SolOut"
SolCallback(3) = "Auto_Plunger"

SolCallback(13) = "SolFireAction"

SolCallback(18) = "vpmNudge.SolGameOn"

Sub Auto_Plunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
    End If
End Sub

Sub SolFireAction(enabled)
    If enabled Then
        LightSeqFA.Play SeqBlinking, , 5, 50
        li3.Duration 2, 1000, 0
        li4.Duration 2, 1000, 0
    End If
End Sub

Sub SolGi(enabled)
    If enabled Then
        GiOff
    Else
        GiOn
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
    If RenderingMode <> 2 and Table1.ShowFSS <> -1 then UpdateLeds
    UpdateLamps
    NVramPatchKeyCheck
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
    Lamp 110, Li110
    Lamp 111, Li111
    Lamp 112, Li112
    Lamp 113, li113
    Lampm 119, li119a
    Lampm 119, li119b
    Lamp 119, li119
    Lamp 12, li12
    Lampm 120, li120a
    Lampm 120, li120b
    Lamp 120, li120
    Lampm 121, li121a
    Lampm 121, li121b
    Lamp 121, li121
    Lampm 122, li122a
    Lampm 122, li122b
    Lamp 122, li122
    Lamp 123, li123
    Lamp 129, li129
    Lamp 130, li130
    Lamp 131, li131
    Lamp 132, li132
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
    Lamp 83, li83
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
        aBallShadow(b).Y = 1930
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2+1

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
	Exit Sub
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

'**********************************************
'Fluppers Flashers

Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6, FlashLevel7, FlashLevel8, FlashLevel9, FlashLevel10
FlasherLight1.IntensityScale = 0
FlasherLight2.IntensityScale = 0
FlasherLight3.IntensityScale = 0
FlasherLight4.IntensityScale = 0
FlasherLight5.IntensityScale = 0
FlasherLight6.IntensityScale = 0
FlasherLight7.IntensityScale = 0
FlasherLight8.IntensityScale = 0
'FlasherLight9.IntensityScale = 0
'FlasherLight10.IntensityScale = 0

'*** blue flasher ***
Sub FlasherFlash1_Timer()
	dim flashx3, matdim
	If not Flasherflash1.TimerEnabled Then 
		Flasherflash1.TimerEnabled = True
		Flasherflash1.visible = 1
		'Flasherlit1.visible = 1
	End If
	flashx3 = FlashLevel1 * FlashLevel1 * FlashLevel1
	Flasherflash1.opacity = 8000 * flashx3
	'Flasherlit1.BlendDisableLighting = 10 * flashx3
	'Flasherbase1.BlendDisableLighting =  flashx3
	Flasherlight1.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel1)
	'Flasherlit1.material = "domelit" & matdim
	FlashLevel1 = FlashLevel1 * 0.85 - 0.01
	If FlashLevel1 < 0.15 Then
		'Flasherlit1.visible = 0
	Else
		'Flasherlit1.visible = 1
	end If
	If FlashLevel1 < 0 Then
		Flasherflash1.TimerEnabled = False
		Flasherflash1.visible = 0
	End If
End Sub

'*** blue flasher ***
Sub FlasherFlash2_Timer()
	dim flashx3, matdim
	If not Flasherflash2.TimerEnabled Then 
		Flasherflash2.TimerEnabled = True
		Flasherflash2.visible = 1
		'Flasherlit2.visible = 1
	End If
	flashx3 = FlashLevel2 * FlashLevel2 * FlashLevel2
	Flasherflash2.opacity = 8000 * flashx3
	'Flasherlit2.BlendDisableLighting = 10 * flashx3
	'Flasherbase2.BlendDisableLighting =  flashx3
	Flasherlight2.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel2)
	'Flasherlit2.material = "domelit" & matdim
	FlashLevel2 = FlashLevel2 * 0.85 - 0.01
	If FlashLevel2 < 0.15 Then
		'Flasherlit2.visible = 0
	Else
		'Flasherlit2.visible = 1
	end If
	If FlashLevel2 < 0 Then
		Flasherflash2.TimerEnabled = False
		Flasherflash2.visible = 0
	End If
End Sub

'*** blue flasher ***
Sub FlasherFlash3_Timer()
	dim flashx3, matdim
	If not Flasherflash3.TimerEnabled Then 
		Flasherflash3.TimerEnabled = True
		Flasherflash3.visible = 1
		'Flasherlit3.visible = 1
	End If
	flashx3 = FlashLevel3 * FlashLevel3 * FlashLevel3
	Flasherflash3.opacity = 8000 * flashx3
	'Flasherlit3.BlendDisableLighting = 10 * flashx3
	'Flasherbase3.BlendDisableLighting =  flashx3
	Flasherlight3.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel3)
	'Flasherlit3.material = "domelit" & matdim
	FlashLevel3 = FlashLevel3 * 0.85 - 0.01
	If FlashLevel3 < 0.15 Then
		'Flasherlit3.visible = 0
	Else
		'Flasherlit3.visible = 1
	end If
	If FlashLevel3 < 0 Then
		Flasherflash3.TimerEnabled = False
		Flasherflash3.visible = 0
	End If
End Sub

'*** blue flasher ***
Sub FlasherFlash4_Timer()
	dim flashx3, matdim
	If not Flasherflash4.TimerEnabled Then 
		Flasherflash4.TimerEnabled = True
		Flasherflash4.visible = 1
		'Flasherlit4.visible = 1
	End If
	flashx3 = FlashLevel4 * FlashLevel4 * FlashLevel4
	Flasherflash4.opacity = 8000 * flashx3
	'Flasherlit4.BlendDisableLighting = 10 * flashx3
	'Flasherbase4.BlendDisableLighting =  flashx3
	Flasherlight4.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel4)
	'Flasherlit4.material = "domelit" & matdim
	FlashLevel4 = FlashLevel4 * 0.85 - 0.01
	If FlashLevel4 < 0.15 Then
		'Flasherlit4.visible = 0
	Else
		'Flasherlit4.visible = 1
	end If
	If FlashLevel4 < 0 Then
		Flasherflash4.TimerEnabled = False
		Flasherflash4.visible = 0
	End If
End Sub

'*** blue flasher ***
Sub FlasherFlash5_Timer()
	dim flashx3, matdim
	If not Flasherflash5.TimerEnabled Then 
		Flasherflash5.TimerEnabled = True
		Flasherflash5.visible = 1
		'Flasherlit5.visible = 1
	End If
	flashx3 = FlashLevel5 * FlashLevel5 * FlashLevel5
	Flasherflash5.opacity = 8000 * flashx3
	'Flasherlit5.BlendDisableLighting = 10 * flashx3
	'Flasherbase5.BlendDisableLighting =  flashx3
	Flasherlight5.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel5)
	'Flasherlit5.material = "domelit" & matdim
	FlashLevel5 = FlashLevel5 * 0.85 - 0.01
	If FlashLevel5 < 0.15 Then
		'Flasherlit5.visible = 0
	Else
		'Flasherlit5.visible = 1
	end If
	If FlashLevel5 < 0 Then
		Flasherflash5.TimerEnabled = False
		Flasherflash5.visible = 0
	End If
End Sub


'*** blue flasher ***
Sub FlasherFlash6_Timer()
	dim flashx3, matdim
	If not Flasherflash6.TimerEnabled Then 
		Flasherflash6.TimerEnabled = True
		Flasherflash6.visible = 1
		'Flasherlit6.visible = 1
	End If
	flashx3 = FlashLevel6 * FlashLevel6 * FlashLevel6
	Flasherflash6.opacity = 8000 * flashx3
	'Flasherlit6.BlendDisableLighting = 10 * flashx3
	'Flasherbase6.BlendDisableLighting =  flashx3
	Flasherlight6.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel6)
	'Flasherlit6.material = "domelit" & matdim
	FlashLevel6 = FlashLevel6 * 0.85 - 0.01
	If FlashLevel6 < 0.15 Then
		'Flasherlit6.visible = 0
	Else
		'Flasherlit6.visible = 1
	end If
	If FlashLevel6 < 0 Then
		Flasherflash6.TimerEnabled = False
		Flasherflash6.visible = 0
	End If
End Sub

'*** blue flasher ***
Sub FlasherFlash7_Timer()
	dim flashx3, matdim
	If not Flasherflash7.TimerEnabled Then 
		Flasherflash7.TimerEnabled = True
		Flasherflash7.visible = 1
		Flasherlit7.visible = 1
	End If
	flashx3 = FlashLevel7 * FlashLevel7 * FlashLevel7
	Flasherflash7.opacity = 8000 * flashx3
	Flasherlit7.BlendDisableLighting = 10 * flashx3
	Flasherbase7.BlendDisableLighting =  flashx3
	Flasherlight7.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel7)
	Flasherlit7.material = "domelit" & matdim
	FlashLevel7 = FlashLevel7 * 0.85 - 0.01
	If FlashLevel7 < 0.15 Then
		Flasherlit7.visible = 0
	Else
		Flasherlit7.visible = 1
	end If
	If FlashLevel7 < 0 Then
		Flasherflash7.TimerEnabled = False
		Flasherflash7.visible = 0
	End If
End Sub

'*** blue flasher ***
Sub FlasherFlash8_Timer()
	dim flashx3, matdim
	If not Flasherflash8.TimerEnabled Then 
		Flasherflash8.TimerEnabled = True
		Flasherflash8.visible = 1
		Flasherlit8.visible = 1
	End If
	flashx3 = FlashLevel8 * FlashLevel8 * FlashLevel8
	Flasherflash8.opacity = 8000 * flashx3
	Flasherlit8.BlendDisableLighting = 10 * flashx3
	Flasherbase8.BlendDisableLighting =  flashx3
	Flasherlight8.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel8)
	Flasherlit8.material = "domelit" & matdim
	FlashLevel8 = FlashLevel8 * 0.85 - 0.01
	If FlashLevel8 < 0.15 Then
		Flasherlit8.visible = 0
	Else
		Flasherlit8.visible = 1
	end If
	If FlashLevel8 < 0 Then
		Flasherflash8.TimerEnabled = False
		Flasherflash8.visible = 0
	End If
End Sub

'***********

Sub cf01_Timer
    Dim xaa
    xaa = INT(8 * RND(1) )
    Select Case xaa
    Case 0:FlashLevel1 = 1 : FlasherFlash1_Timer
    Case 1:FlashLevel2 = 1 : FlasherFlash2_Timer
    Case 2:FlashLevel3 = 1 : FlasherFlash3_Timer
    Case 3:FlashLevel4 = 1 : FlasherFlash4_Timer
    Case 4:FlashLevel5 = 1 : FlasherFlash5_Timer
    Case 5:FlashLevel6 = 1 : FlasherFlash6_Timer
    Case 6:FlashLevel6 = 1 : FlasherFlash6_Timer:FlashLevel5 = 1 : FlasherFlash5_Timer
    Case 7:FlashLevel7 = 1 : FlasherFlash7_Timer:FlashLevel8 = 1 : FlasherFlash8_Timer
    End Select
end sub

Sub cf02_Timer
cf01.enabled=False
cf02.enabled=False
end sub

'**********************************
'  MUSIC & SOUNDS TIMERS
'**********************************
'attract timers

Sub a01_Timer
    Dim xa
    xa = INT(15 * RND(1) )
    Select Case xa
    Case 0:PlayMusic"LStarfighter\0lsa09.ogg":a02.enabled=true:a02.interval=31500
    Case 1:PlayMusic"LStarfighter\0lsa10.ogg":a02.enabled=true:a02.interval=41200
    Case 2:PlayMusic"LStarfighter\0lsa11.ogg":a02.enabled=true:a02.interval=32400
    Case 3:PlayMusic"LStarfighter\0lsa12.ogg":a02.enabled=true:a02.interval=18400
    Case 4:PlayMusic"LStarfighter\0lsa13.ogg":a02.enabled=true:a02.interval=24200
    Case 5:PlayMusic"LStarfighter\0lsa14.ogg":a02.enabled=true:a02.interval=40000
    Case 6:PlayMusic"LStarfighter\0lsa15.ogg":a02.enabled=true:a02.interval=31200
    Case 7:PlayMusic"LStarfighter\0lsa16.ogg":a02.enabled=true:a02.interval=68600
    Case 8:PlayMusic"LStarfighter\0lsa17.ogg":a02.enabled=true:a02.interval=27000
    Case 9:PlayMusic"LStarfighter\0lsa18.ogg":a02.enabled=true:a02.interval=23500
    Case 10:PlayMusic"LStarfighter\0lsa19.ogg":a02.enabled=true:a02.interval=17000
    Case 11:PlayMusic"LStarfighter\0lsa20.ogg":a02.enabled=true:a02.interval=18300
    Case 12:PlayMusic"LStarfighter\0lsa21.ogg":a02.enabled=true:a02.interval=29500
    Case 13:PlayMusic"LStarfighter\0lsa22.ogg":a02.enabled=true:a02.interval=15700
    Case 14:PlayMusic"LStarfighter\0lsa23.ogg":a02.enabled=true:a02.interval=19700
    End Select
a01.enabled=False
end sub

Sub a02_Timer
    PlayMusic"LStarfighter\0lsa39.ogg":a03.enabled=true:a03.interval=15500
a02.enabled=False
end sub

Sub a03_Timer
    Dim xb
    xb = INT(15 * RND(1) )
    Select Case xb
    Case 0:PlayMusic"LStarfighter\0lsa24.ogg":a04.enabled=true:a04.interval=21000
    Case 1:PlayMusic"LStarfighter\0lsa25.ogg":a04.enabled=true:a04.interval=27500
    Case 2:PlayMusic"LStarfighter\0lsa26.ogg":a04.enabled=true:a04.interval=47900
    Case 3:PlayMusic"LStarfighter\0lsa27.ogg":a04.enabled=true:a04.interval=24000
    Case 4:PlayMusic"LStarfighter\0lsa28.ogg":a04.enabled=true:a04.interval=33800
    Case 5:PlayMusic"LStarfighter\0lsa29.ogg":a04.enabled=true:a04.interval=30000
    Case 6:PlayMusic"LStarfighter\0lsa30.ogg":a04.enabled=true:a04.interval=28400
    Case 7:PlayMusic"LStarfighter\0lsa31.ogg":a04.enabled=true:a04.interval=18900
    Case 8:PlayMusic"LStarfighter\0lsa32.ogg":a04.enabled=true:a04.interval=31500
    Case 9:PlayMusic"LStarfighter\0lsa33.ogg":a04.enabled=true:a04.interval=34000
    Case 10:PlayMusic"LStarfighter\0lsa34.ogg":a04.enabled=true:a04.interval=30000
    Case 11:PlayMusic"LStarfighter\0lsa35.ogg":a04.enabled=true:a04.interval=20000
    Case 12:PlayMusic"LStarfighter\0lsa36.ogg":a04.enabled=true:a04.interval=28300
    Case 13:PlayMusic"LStarfighter\0lsa37.ogg":a04.enabled=true:a04.interval=17400
    Case 14:PlayMusic"LStarfighter\0lsa38.ogg":a04.enabled=true:a04.interval=25000
    End Select
a03.enabled=False
end sub

Sub a04_Timer
    Dim xc
    xc = INT(8 * RND(1) )
    Select Case xc
    Case 0:PlayMusic"LStarfighter\0lsa01.ogg":a01.enabled=true:a01.interval=93000
    Case 1:PlayMusic"LStarfighter\0lsa02.ogg":a01.enabled=true:a01.interval=73000
    Case 2:PlayMusic"LStarfighter\0lsa03.ogg":a01.enabled=true:a01.interval=112000
    Case 3:PlayMusic"LStarfighter\0lsa04.ogg":a01.enabled=true:a01.interval=95000
    Case 4:PlayMusic"LStarfighter\0lsa05.ogg":a01.enabled=true:a01.interval=133800
    Case 5:PlayMusic"LStarfighter\0lsa06.ogg":a01.enabled=true:a01.interval=90500
    Case 6:PlayMusic"LStarfighter\0lsa07.ogg":a01.enabled=true:a01.interval=141000
    Case 7:PlayMusic"LStarfighter\0lsa08.ogg":a01.enabled=true:a01.interval=77500
    End Select
a04.enabled=False
end sub

'*********************************************
'In Game Music Timers

Dim AA
AA=0

Sub p01_Timer
    Dim xd
    xd = INT(2 * RND(1) )
    Select Case xd
    Case 0:PlayMusic"LStarfighter\0lsp01.ogg":p02.enabled=true:p02.interval=90500
    Case 1:PlayMusic"LStarfighter\0lsp02.ogg":p02.enabled=true:p02.interval=67500

    End Select
p01.enabled=False
end sub

Sub p02_Timer
    Dim xe
    xe = INT(2 * RND(1) )
    Select Case xe
    Case 0:PlayMusic"LStarfighter\0lsp01.ogg":p01.enabled=true:p01.interval=90500
    Case 1:PlayMusic"LStarfighter\0lsp02.ogg":p01.enabled=true:p01.interval=67500

    End Select
p02.enabled=False
end sub

Sub v01_Timer
    AA=0
v01.enabled=False
end sub

Dim IGC
IGC=0

Sub r01_Timer
     If AA=1 Then b01.enabled=true:r01.enabled=False
     If AA=0 Then
     IGC=IGC+1
     If IGC = 1 Then
	 Dim xpa
	 xpa = INT(9 * RND(1) )
	 Select Case xpa
     Case 0:PlaySound"0lsi01":v01.enabled=true:v01.interval=1800:AA=1
     Case 1:PlaySound"0lsi02":v01.enabled=true:v01.interval=2200:AA=1
     Case 2:PlaySound"0lsi03":v01.enabled=true:v01.interval=2050:AA=1
     Case 3:PlaySound"0lsi04":v01.enabled=true:v01.interval=1770:AA=1
     Case 4:PlaySound"0lsi05":v01.enabled=true:v01.interval=2070:AA=1
     Case 5:PlaySound"0lsi06":v01.enabled=true:v01.interval=1750:AA=1
     Case 6:PlaySound"0lsi07":v01.enabled=true:v01.interval=1740:AA=1
     Case 7:PlaySound"0lsi08":v01.enabled=true:v01.interval=2000:AA=1
     Case 8:PlaySound"0lsi09":v01.enabled=true:v01.interval=3000:AA=1
     End Select
     End If
     If IGC = 2 Then
	 Dim xpb
	 xpb = INT(9 * RND(1) )
	 Select Case xpb
     Case 0:PlaySound"0lsi10":v01.enabled=true:v01.interval=2400:AA=1
     Case 1:PlaySound"0lsi11":v01.enabled=true:v01.interval=1750:AA=1
     Case 2:PlaySound"0lsi12":v01.enabled=true:v01.interval=2000:AA=1
     Case 3:PlaySound"0lsi13":v01.enabled=true:v01.interval=2200:AA=1
     Case 4:PlaySound"0lsi14":v01.enabled=true:v01.interval=2350:AA=1
     Case 5:PlaySound"0lsi15":v01.enabled=true:v01.interval=2200:AA=1
     Case 6:PlaySound"0lsi16":v01.enabled=true:v01.interval=3600:AA=1
     Case 7:PlaySound"0lsi17":v01.enabled=true:v01.interval=2370:AA=1
     Case 8:PlaySound"0lsi18":v01.enabled=true:v01.interval=2250:AA=1
     End Select
     End If
     If IGC = 3 Then
	 Dim xpc
	 xpc = INT(9 * RND(1) )
	 Select Case xpc
     Case 0:PlaySound"0lsi19":v01.enabled=true:v01.interval=1200:AA=1
     Case 1:PlaySound"0lsi20":v01.enabled=true:v01.interval=2070:AA=1
     Case 2:PlaySound"0lsi21":v01.enabled=true:v01.interval=1200:AA=1
     Case 3:PlaySound"0lsi22":v01.enabled=true:v01.interval=2000:AA=1
     Case 4:PlaySound"0lsi23":v01.enabled=true:v01.interval=1700:AA=1
     Case 5:PlaySound"0lsi24":v01.enabled=true:v01.interval=1700:AA=1
     Case 6:PlaySound"0lsi25":v01.enabled=true:v01.interval=1500:AA=1
     Case 7:PlaySound"0lsi26":v01.enabled=true:v01.interval=1400:AA=1
     Case 8:PlaySound"0lsi27":v01.enabled=true:v01.interval=1100:AA=1
     End Select
     End If
     If IGC = 4 Then
	 Dim xpd
	 xpd = INT(8 * RND(1) )
	 Select Case xpd
     Case 0:PlaySound"0lsi28":v01.enabled=true:v01.interval=1000:AA=1
     Case 1:PlaySound"0lsi29":v01.enabled=true:v01.interval=1450:AA=1
     Case 2:PlaySound"0lsi30":v01.enabled=true:v01.interval=1100:AA=1
     Case 3:PlaySound"0lsi31":v01.enabled=true:v01.interval=1000:AA=1
     Case 4:PlaySound"0lsi32":v01.enabled=true:v01.interval=1900:AA=1
     Case 5:PlaySound"0lsi33":v01.enabled=true:v01.interval=1800:AA=1
     Case 6:grats01.enabled=True
     Case 7:PlaySound"0lsfxilL":v01.enabled=true:v01.interval=2000:AA=1
     End Select
     End If
     If IGC=4 then IGC=0
     End If
r01.enabled=False
end sub

Sub r02_Timer
     If AA=1 Then b02.enabled=true:r02.enabled=False
     If AA=0 Then
     IGC=IGC+1
     If IGC = 1 Then
	 Dim xra
	 xra = INT(9 * RND(1) )
	 Select Case xra
     Case 0:PlaySound"0lsi01":v01.enabled=true:v01.interval=1800:AA=1
     Case 1:PlaySound"0lsi02":v01.enabled=true:v01.interval=2200:AA=1
     Case 2:PlaySound"0lsi03":v01.enabled=true:v01.interval=2050:AA=1
     Case 3:PlaySound"0lsi04":v01.enabled=true:v01.interval=1770:AA=1
     Case 4:PlaySound"0lsi05":v01.enabled=true:v01.interval=2070:AA=1
     Case 5:PlaySound"0lsi06":v01.enabled=true:v01.interval=1750:AA=1
     Case 6:PlaySound"0lsi07":v01.enabled=true:v01.interval=1740:AA=1
     Case 7:PlaySound"0lsi08":v01.enabled=true:v01.interval=2000:AA=1
     Case 8:PlaySound"0lsi09":v01.enabled=true:v01.interval=3000:AA=1
     End Select
     End If
     If IGC = 2 Then
	 Dim xrb
	 xrb = INT(9 * RND(1) )
	 Select Case xrb
     Case 0:PlaySound"0lsi10":v01.enabled=true:v01.interval=2400:AA=1
     Case 1:PlaySound"0lsi11":v01.enabled=true:v01.interval=1750:AA=1
     Case 2:PlaySound"0lsi12":v01.enabled=true:v01.interval=2000:AA=1
     Case 3:PlaySound"0lsi13":v01.enabled=true:v01.interval=2200:AA=1
     Case 4:PlaySound"0lsi14":v01.enabled=true:v01.interval=2350:AA=1
     Case 5:PlaySound"0lsi15":v01.enabled=true:v01.interval=2200:AA=1
     Case 6:PlaySound"0lsi16":v01.enabled=true:v01.interval=3600:AA=1
     Case 7:PlaySound"0lsi17":v01.enabled=true:v01.interval=2370:AA=1
     Case 8:PlaySound"0lsi18":v01.enabled=true:v01.interval=2250:AA=1
     End Select
     End If
     If IGC = 3 Then
	 Dim xrc
	 xrc = INT(9 * RND(1) )
	 Select Case xrc
     Case 0:PlaySound"0lsi19":v01.enabled=true:v01.interval=1200:AA=1
     Case 1:PlaySound"0lsi20":v01.enabled=true:v01.interval=2070:AA=1
     Case 2:PlaySound"0lsi21":v01.enabled=true:v01.interval=1200:AA=1
     Case 3:PlaySound"0lsi22":v01.enabled=true:v01.interval=2000:AA=1
     Case 4:PlaySound"0lsi23":v01.enabled=true:v01.interval=1700:AA=1
     Case 5:PlaySound"0lsi24":v01.enabled=true:v01.interval=1700:AA=1
     Case 6:PlaySound"0lsi25":v01.enabled=true:v01.interval=1500:AA=1
     Case 7:PlaySound"0lsi26":v01.enabled=true:v01.interval=1400:AA=1
     Case 8:PlaySound"0lsi27":v01.enabled=true:v01.interval=1100:AA=1
     End Select
     End If
     If IGC = 4 Then
	 Dim xrd
	 xrd = INT(9 * RND(1) )
	 Select Case xrd
     Case 0:PlaySound"0lsi28":v01.enabled=true:v01.interval=1000:AA=1
     Case 1:PlaySound"0lsi29":v01.enabled=true:v01.interval=1450:AA=1
     Case 2:PlaySound"0lsi30":v01.enabled=true:v01.interval=1100:AA=1
     Case 3:PlaySound"0lsi31":v01.enabled=true:v01.interval=1000:AA=1
     Case 4:PlaySound"0lsi32":v01.enabled=true:v01.interval=1900:AA=1
     Case 5:PlaySound"0lsi33":v01.enabled=true:v01.interval=1800:AA=1
     Case 6:grats01.enabled=True
     Case 7:PlaySound"0lsfxilL":v01.enabled=true:v01.interval=2000:AA=1
     Case 8:PlaySound"0lsfxilR":v01.enabled=true:v01.interval=1000:AA=1
     End Select
     End If
     If IGC=4 then IGC=0
     End If
r02.enabled=False
end sub

Sub r03_Timer
     If AA=1 Then b03.enabled=true:r03.enabled=False
     If AA=0 Then
     IGC=IGC+1
     If IGC = 1 Then
	 Dim xsa
	 xsa = INT(9 * RND(1) )
	 Select Case xsa
     Case 0:PlaySound"0lsi01":v01.enabled=true:v01.interval=1800:AA=1
     Case 1:PlaySound"0lsi02":v01.enabled=true:v01.interval=2200:AA=1
     Case 2:PlaySound"0lsi03":v01.enabled=true:v01.interval=2050:AA=1
     Case 3:PlaySound"0lsi04":v01.enabled=true:v01.interval=1770:AA=1
     Case 4:PlaySound"0lsi05":v01.enabled=true:v01.interval=2070:AA=1
     Case 5:PlaySound"0lsi06":v01.enabled=true:v01.interval=1750:AA=1
     Case 6:PlaySound"0lsi07":v01.enabled=true:v01.interval=1740:AA=1
     Case 7:PlaySound"0lsi08":v01.enabled=true:v01.interval=2000:AA=1
     Case 8:PlaySound"0lsi09":v01.enabled=true:v01.interval=3000:AA=1
     End Select
     End If
     If IGC = 2 Then
	 Dim xsb
	 xsb = INT(9 * RND(1) )
	 Select Case xsb
     Case 0:PlaySound"0lsi10":v01.enabled=true:v01.interval=2400:AA=1
     Case 1:PlaySound"0lsi11":v01.enabled=true:v01.interval=1750:AA=1
     Case 2:PlaySound"0lsi12":v01.enabled=true:v01.interval=2000:AA=1
     Case 3:PlaySound"0lsi13":v01.enabled=true:v01.interval=2200:AA=1
     Case 4:PlaySound"0lsi14":v01.enabled=true:v01.interval=2350:AA=1
     Case 5:PlaySound"0lsi15":v01.enabled=true:v01.interval=2200:AA=1
     Case 6:PlaySound"0lsi16":v01.enabled=true:v01.interval=3600:AA=1
     Case 7:PlaySound"0lsi17":v01.enabled=true:v01.interval=2370:AA=1
     Case 8:PlaySound"0lsi18":v01.enabled=true:v01.interval=2250:AA=1
     End Select
     End If
     If IGC = 3 Then
	 Dim xsc
	 xsc = INT(9 * RND(1) )
	 Select Case xsc
     Case 0:PlaySound"0lsi19":v01.enabled=true:v01.interval=1200:AA=1
     Case 1:PlaySound"0lsi20":v01.enabled=true:v01.interval=2070:AA=1
     Case 2:PlaySound"0lsi21":v01.enabled=true:v01.interval=1200:AA=1
     Case 3:PlaySound"0lsi22":v01.enabled=true:v01.interval=2000:AA=1
     Case 4:PlaySound"0lsi23":v01.enabled=true:v01.interval=1700:AA=1
     Case 5:PlaySound"0lsi24":v01.enabled=true:v01.interval=1700:AA=1
     Case 6:PlaySound"0lsi25":v01.enabled=true:v01.interval=1500:AA=1
     Case 7:PlaySound"0lsi26":v01.enabled=true:v01.interval=1400:AA=1
     Case 8:PlaySound"0lsi27":v01.enabled=true:v01.interval=1100:AA=1
     End Select
     End If
     If IGC = 4 Then
	 Dim xsd
	 xsd = INT(8 * RND(1) )
	 Select Case xsd
     Case 0:PlaySound"0lsi28":v01.enabled=true:v01.interval=1000:AA=1
     Case 1:PlaySound"0lsi29":v01.enabled=true:v01.interval=1450:AA=1
     Case 2:PlaySound"0lsi30":v01.enabled=true:v01.interval=1100:AA=1
     Case 3:PlaySound"0lsi31":v01.enabled=true:v01.interval=1000:AA=1
     Case 4:PlaySound"0lsi32":v01.enabled=true:v01.interval=1900:AA=1
     Case 5:PlaySound"0lsi33":v01.enabled=true:v01.interval=1800:AA=1
     Case 6:grats01.enabled=True
     Case 7:PlaySound"0lsfxilR":v01.enabled=true:v01.interval=1000:AA=1
     End Select
     End If
     If IGC=4 then IGC=0
     End If
r03.enabled=False
end sub

Sub ct01_Timer
     If BL=1 Then grats01.enabled=True:ct01.enabled=False:BL=3:End If
     If BL=0 Then roc01.enabled=True:ct01.enabled=False:End If
end sub

Dim BL
BL=0

Sub k01_Timer
	 Dim xt
	 xt = INT(11 * RND(1) )
	 Select Case xt
     Case 0:PlaySound"0lsk01":v01.enabled=true:v01.interval=3000:AA=1
     Case 1:PlaySound"0lsk02":v01.enabled=true:v01.interval=3000:AA=1
     Case 2:PlaySound"0lsk03":v01.enabled=true:v01.interval=3000:AA=1
     Case 3:PlaySound"0lsk04":v01.enabled=true:v01.interval=2500:AA=1
     Case 4:PlaySound"0lsk05":v01.enabled=true:v01.interval=5000:AA=1
     Case 5:PlaySound"0lsk06":v01.enabled=true:v01.interval=5000:AA=1
     Case 6:PlaySound"0lsk07":v01.enabled=true:v01.interval=4000:AA=1
     Case 7:PlaySound"0lsk08":v01.enabled=true:v01.interval=3000:AA=1
     Case 8:PlaySound"0lsk09":v01.enabled=true:v01.interval=2400:AA=1
     Case 9:PlaySound"0lsk10":v01.enabled=true:v01.interval=4000:AA=1
     Case 10:PlaySound"0lsk11":v01.enabled=true:v01.interval=2000:AA=1
     End Select
k01.enabled=False
end sub

Sub sd01_Timer
			Dim xu
			xu = INT(2 * RND(1) )
			Select Case xu
			Case 0:StopIGSounds:PlaySound("0lsfxsd01")
			Case 1:StopIGSounds:PlaySound("0lsfxsd02")
    End Select
sd01.enabled=False
end sub

Sub grats01_Timer
			Dim xv
			xv = INT(6 * RND(1) )
			Select Case xv
			Case 0:PlaySound"0lsi34":v01.enabled=true:v01.interval=1400:AA=1
			Case 1:PlaySound"0lsi34":v01.enabled=true:v01.interval=1400:AA=1
			Case 2:PlaySound"0lsi35":v01.enabled=true:v01.interval=1000:AA=1
			Case 3:PlaySound"0lsi36":v01.enabled=true:v01.interval=1000:AA=1
            Case 4:PlaySound"0lsk01":v01.enabled=true:v01.interval=3000:AA=1
			Case 5:PlaySound"0lsi34":v01.enabled=true:v01.interval=1400:AA=1
    End Select
grats01.enabled=False
end sub

Sub g01_Timer
			Dim xy
			xy = INT(3 * RND(1) )
			Select Case xy
			Case 0:PlaySound"0lsg01"
			Case 1:PlaySound"0lsg01"
			Case 2:PlaySound"0lsg03"
    End Select
g01.enabled=False
end sub

Sub roc01_Timer
			Dim xw
			xw = INT(3 * RND(1) )
			Select Case xw
			Case 0:PlaySound"0lsfxr01"
			Case 1:PlaySound"0lsfxr02"
			Case 2:PlaySound"0lsfxr03"
    End Select
roc01.enabled=False
end sub

Sub til01_Timer
			Dim xz
			xz = INT(5 * RND(1) )
			Select Case xz
			Case 0:PlaySound"0lsg02"
			Case 1:PlaySound"0lsT":PlaySound"0lsg02":v01.enabled=true:v01.interval=4000:AA=1
			Case 2:PlaySound"0lsg02"
			Case 3:PlaySound"0lsg02"
			Case 4:PlaySound"0lsg02"
    End Select
til01.enabled=False
end sub

Sub Gate2_Hit()
roc01.enabled=True
End Sub

Sub Gate4_Hit()
roc01.enabled=True
End Sub

'******************
'Coin Insert Sound FX Timer

Sub c01_Timer
			Dim xj
			xj = INT(4 * RND(1) )
			Select Case xj
			Case 0:StopCSounds:PlaySound("0lsc01")
			Case 1:StopCSounds:PlaySound("0lsc02")
			Case 2:StopCSounds:PlaySound("0lsc03")
			Case 3:StopCSounds:PlaySound("0lsc04")
    End Select
cf01.enabled=true:cf01.interval=100
cf02.enabled=true:cf02.interval=1000
c01.enabled=False
end sub


'******************
'Player Start Sound FX Timer

Sub s01_Timer
           If BIP=1 Then g01.enabled=True:s01.enabled=False:End If
           If BIP=0 Then
            EndMusic:a01.enabled=False:a02.enabled=False:a03.enabled=False:a04.enabled=False
            a04.enabled=true:a04.interval=8000
			Dim xl
			xl = INT(12 * RND(1) )
			Select Case xl
			Case 0:StopPSSounds:PlaySound("0lss01")
			Case 1:StopPSSounds:PlaySound("0lss02")
			Case 2:StopPSSounds:PlaySound("0lss03")
			Case 3:StopPSSounds:PlaySound("0lss04")
			Case 4:StopPSSounds:PlaySound("0lss05")
			Case 5:StopPSSounds:PlaySound("0lss06")
			Case 6:StopPSSounds:PlaySound("0lss07")
			Case 7:StopPSSounds:PlaySound("0lss08")
			Case 8:StopPSSounds:PlaySound("0lss09")
			Case 9:StopPSSounds:PlaySound("0lss10")
			Case 10:StopPSSounds:PlaySound("0lss11")
			Case 11:StopPSSounds:PlaySound("0lss12")
            End Select
           End If
s01.enabled=False
end sub

'******************
' Drain Sound FX Timer

Sub d01_Timer:
    If BL=3 then 
       BIP=1:ExAnimation.Enabled = True
       PlaySound ("0lsfxe01"):BL=0:d01.enabled=False
    End If
    If BIP=0 then Exit Sub
    If BIP=1 Then
       BIP=0:ExAnimation.Enabled = True
       PlaySound ("0lsfxe01"):StopIGSounds
       p01.enabled=False:p02.enabled=False
	   Dim xm
	   xm = INT(11 * RND(1) )
       Select Case xm
	   Case 0:EndMusic:PlaySound"0lsd01":d02.interval=3500:d02.enabled=1
	   Case 1:EndMusic:PlaySound"0lsd02":d02.interval=3500:d02.enabled=1
	   Case 2:EndMusic:PlaySound"0lsd03":d02.interval=3500:d02.enabled=1
	   Case 3:EndMusic:PlaySound"0lsd04":d02.interval=2100:d02.enabled=1
	   Case 4:EndMusic:PlaySound"0lsd05":d02.interval=2250:d02.enabled=1
	   Case 5:EndMusic:PlaySound"0lsd06":d02.interval=2900:d02.enabled=1
	   Case 6:EndMusic:PlaySound"0lsd07":d02.interval=2700:d02.enabled=1
	   Case 7:EndMusic:PlaySound"0lsd08":d02.interval=5900:d02.enabled=1
	   Case 8:EndMusic:PlaySound"0lsd09":d02.interval=5900:d02.enabled=1
	   Case 9:EndMusic:PlaySound"0lsd10":d02.interval=4700:d02.enabled=1
	   Case 10:EndMusic:PlaySound"0lsd11":d02.interval=5800:d02.enabled=1
       End Select
    End If
d01.enabled=False
end sub

Sub d02_Timer
		 PlaySound"0lsd00":a01.enabled=true:a01.interval=12700
d02.enabled=False
end sub

Dim BIP
BIP=0
Sub Gate5_Hit():
     If BL=1 Then Exit Sub
     BIP=1:EndMusic:StopSound"0lsd00":a04.enabled=False:a03.enabled=False:a02.enabled=False:a01.enabled=False
     d01.enabled=False:d02.enabled=False:g01.enabled=True
	 Dim xf
	 xf = INT(2 * RND(1) )
	 Select Case xf
     Case 0:PlaySound"0lspa01":Gate5.timerenabled=1:Gate5.timerinterval=8300
     Case 1:PlaySound"0lspa10":Gate5.timerenabled=1:Gate5.timerinterval=7300
	 End Select
End Sub

Sub Gate5_timer
	 Dim xg
	 xg = INT(16 * RND(1) )
	 Select Case xg
     Case 0:PlaySound"0lspa12":PlaySound"0lspa01"
     Case 1:PlaySound"0lspa11":PlaySound"0lspa01"
     Case 2:PlaySound"0lspa03":PlaySound"0lspa01"
     Case 3:PlaySound"0lspa04":PlaySound"0lspa01"
     Case 4:PlaySound"0lspa05":PlaySound"0lspa01"
     Case 5:PlaySound"0lspa06":PlaySound"0lspa01"
     Case 6:PlaySound"0lspa07":PlaySound"0lspa01"
     Case 7:PlaySound"0lspa08":PlaySound"0lspa01"
     Case 8:PlaySound"0lspa09":PlaySound"0lspa01"
     Case 9:PlaySound"0lspa13":PlaySound"0lspa01"
     Case 10:PlaySound"0lspa14":PlaySound"0lspa01"
     Case 11:PlaySound"0lspa15":PlaySound"0lspa01"
     Case 12:PlaySound"0lspa16":PlaySound"0lspa01"
     Case 13:PlaySound"0lspa17":PlaySound"0lspa01"
     Case 14:PlaySound"0lspa18":PlaySound"0lspa01"
     Case 15:PlaySound"0lspa19":PlaySound"0lspa01"
	 End Select
Gate5.timerenabled=0
End Sub

Sub Trigger001_Hit
    Gate5.timerenabled=0:a04.enabled=False
    StopPSSounds
    StopPASounds
    PlaySound"0lspa02"
    Dim xd
    xd = INT(2 * RND(1) )
    Select Case xd
    Case 0:PlayMusic"LStarfighter\0lsp01.ogg":p02.enabled=true:p02.interval=90500
    Case 1:PlayMusic"LStarfighter\0lsp02.ogg":p02.enabled=true:p02.interval=67500
    End Select
End Sub

Sub Trigger002_Hit
    grats01.enabled=True
    roc01.enabled=True
    Ex2Animation.Enabled = True
    PlaySound"0lsfxe01"
End Sub


Sub b01_Timer
	Dim xn
	xn = INT(2 * RND(1) )
	Select Case xn
	Case 0:PlaySound"0lsfx04"
	Case 1:PlaySound"0lsfx05"
    End Select
b01.enabled=False
end sub

Sub b02_Timer
	Dim xo
	xo = INT(2 * RND(1) )
	Select Case xo
	Case 0:PlaySound"0lsfx03"
	Case 1:PlaySound"0lsfx06"
    End Select
b02.enabled=False
end sub

Sub b03_Timer
	Dim xq
	xq = INT(2 * RND(1) )
	Select Case xq
	Case 0:PlaySound"0lsfx01"
	Case 1:PlaySound"0lsfx02"
    End Select
b03.enabled=False
end sub

Sub b04_Timer
	Dim xoa
	xoa = INT(4 * RND(1) )
	Select Case xoa
	Case 0:PlaySound"0lsfx03"
	Case 1:PlaySound"0lsfx06"
	Case 2:PlaySound"0lsfx04"
	Case 3:PlaySound"0lsfx05"
    End Select
b04.enabled=False
end sub

Sub b05_Timer
	Dim xob
	xob = INT(4 * RND(1) )
	Select Case xob
	Case 0:PlaySound"0lsfx03"
	Case 1:PlaySound"0lsfx06"
	Case 2:PlaySound"0lsfx01"
	Case 3:PlaySound"0lsfx02"
    End Select
b05.enabled=False
end sub


Sub StopCSounds()

   StopSound"0lsc01"
   StopSound"0lsc02"
   StopSound"0lsc03"
   StopSound"0lsc04"

End Sub

Sub StopPSSounds()

   StopSound"0lss01"
   StopSound"0lss02"
   StopSound"0lss03"
   StopSound"0lss04"
   StopSound"0lss05"
   StopSound"0lss06"
   StopSound"0lss07"
   StopSound"0lss08"
   StopSound"0lss09"
   StopSound"0lss10"
   StopSound"0lss11"
   StopSound"0lss12"

End Sub

Sub StopPASounds()

   StopSound"0lspa01"
   StopSound"0lspa02"
   StopSound"0lspa03"
   StopSound"0lspa04"
   StopSound"0lspa05"
   StopSound"0lspa06"
   StopSound"0lspa07"
   StopSound"0lspa08"
   StopSound"0lspa09"
   StopSound"0lspa10"
   StopSound"0lspa11"
   StopSound"0lspa12"
   StopSound"0lspa13"
   StopSound"0lspa14"
   StopSound"0lspa15"
   StopSound"0lspa16"
   StopSound"0lspa17"
   StopSound"0lspa18"
   StopSound"0lspa19"

End Sub

Sub StopIGSounds()

   StopSound"0lsfxsd01"
   StopSound"0lsfxsd02"
   StopSound"0lsi01"
   StopSound"0lsi02"
   StopSound"0lsi03"
   StopSound"0lsi04"
   StopSound"0lsi05"
   StopSound"0lsi06"
   StopSound"0lsi07"
   StopSound"0lsi08"
   StopSound"0lsi09"
   StopSound"0lsi10"
   StopSound"0lsi11"
   StopSound"0lsi12"
   StopSound"0lsi13"
   StopSound"0lsi14"
   StopSound"0lsi15"
   StopSound"0lsi16"
   StopSound"0lsi17"
   StopSound"0lsi18"
   StopSound"0lsi19"
   StopSound"0lsi20"
   StopSound"0lsi21"
   StopSound"0lsi22"
   StopSound"0lsi23"
   StopSound"0lsi24"
   StopSound"0lsi25"
   StopSound"0lsi26"
   StopSound"0lsi27"
   StopSound"0lsi28"
   StopSound"0lsi29"
   StopSound"0lsi30"
   StopSound"0lsi31"
   StopSound"0lsi32"
   StopSound"0lsi33"
   StopSound"0lsi34"
   StopSound"0lsi35"
   StopSound"0lsi36"
   StopSound"0lsk01"
   StopSound"0lsk02"
   StopSound"0lsk03"
   StopSound"0lsk04"
   StopSound"0lsk05"
   StopSound"0lsk06"
   StopSound"0lsk07"
   StopSound"0lsk08"
   StopSound"0lsk09"
   StopSound"0lsk10"
   StopSound"0lsk11"

End Sub

'*************
'   BACKWALL CHANGE
'*************

Dim BWall
Dim xa
Sub LoadBWall
    xa = LoadValue(cGameName, "BWall")
    If(xa <> "") Then BWall = xa Else BWall = 0
	UpdateBWall
End Sub

Sub SaveBWall
    SaveValue cGameName, "BWall", BWall
End Sub

Sub NextBWall: BWall = (BWall +1 ) MOD 3: UpdateBWall: SaveBWall: End Sub

Sub UpdateBWall
Select Case BWall
Case 0: Wall4.sideimage = "lsf-bwall-1"
Case 1: Wall4.sideimage = "lsf-bwall-2"
Case 2: Wall4.sideimage = "lsf-bwall-3"
End Select
End Sub

'******************************************************
'                  Flasher Animation
'******************************************************

Dim ExCnt
ExCnt = 0
ExAnimation.Interval=17*2  '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub ExAnimation_Timer
	'Initialize the animation
	If ExFlasher.visible = False Then 
		ExFlasher.visible = True
		ExCnt = 0
        PlaySound "EX2"
	End If

	'Select the correct frame
	If ExCnt > 99 Then
		ExFlasher.imageA = "EX1_" & ExCnt
	Elseif ExCnt > 9 Then
		ExFlasher.imageA = "EX1_0" & ExCnt
	Else
		ExFlasher.imageA = "EX1_00" & ExCnt
	End If
	ExCnt = ExCnt + 1

    If ExCnt = 4 Then EXPLight1.state=1

    If ExCnt > 21 Then EXPLight1.state=0: End If

	'Finish animation
	If ExCnt > 38 Then 
		ExCnt = 0
		ExAnimation.Enabled = False
		ExFlasher.visible = False
	End If
End Sub

Dim Ex2Cnt
Ex2Cnt = 0
Ex2Animation.Interval=17*2  '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub Ex2Animation_Timer
	'Initialize the animation
	If Ex2Flasher.visible = False Then 
		Ex2Flasher.visible = True
		Ex2Cnt = 0
        EXPLight2.state=1
        PlaySound "EX2"
	End If

	'Select the correct frame
	If Ex2Cnt > 99 Then
		Ex2Flasher.imageA = "EX2_" & Ex2Cnt
	Elseif Ex2Cnt > 9 Then
		Ex2Flasher.imageA = "EX2_0" & Ex2Cnt
	Else
		Ex2Flasher.imageA = "EX2_00" & Ex2Cnt
	End If
	Ex2Cnt = Ex2Cnt + 1

    If Ex2Cnt > 21 Then EXPLight2.state=0: End If

	'Finish animation
	If Ex2Cnt > 38 Then 
		Ex2Cnt = 0
		Ex2Animation.Enabled = False
		Ex2Flasher.visible = False
	End If
End Sub

Dim cabctr:cabctr = 1
Sub Flasher001_Timer()
	starfightercab.Image = "cabbake"
	If Int(5*Rnd+1) = 5 then
		tct.Visible = not tct.Visible
		star.Visible = not star.Visible
		gameglow.Visible = not gameglow.Visible
		starfightercab.Image = "cabbake"&cabctr
		cabctr = cabctr + 1
		If cabctr = 6 then cabctr = 1
	End If
	Randomize
	If Int(2*Rnd+1) = 2 then
		sb.Visible = not sb.Visible
		If VRroom = 1 then
			Flasher001.Visible = not Flasher001.Visible
			If Flasher001.Visible = True then station.Image = "bake2" Else station.Image = "bake"
		End If
	End If	
End Sub

Dim gunstarmov:gunstarmov=30
Sub Timer001_Timer()
	Timer001.Interval = 0
	If gunstar.z < 3000 then Timer001.Interval = 10
	gunstar.z = gunstar.z + gunstarmov
	Flasher002.Height = Flasher002.Height + gunstarmov
	Flasher003.Height = Flasher003.Height + gunstarmov
	Flasher004.Height = Flasher004.Height + gunstarmov
	Flasher005.Height = Flasher005.Height + gunstarmov
	If gunstar.z <= 800 then gunstarmov = 30:Timer001.Interval = 10000
	If gunstar.z >= 500000 then gunstarmov = -30:Timer001.Interval = 30000
End Sub

Sub Flasher002_Timer()
	sl.Visible = not sl.Visible
	If VRroom = 1 then
		Randomize
		If Int(2*Rnd+1) = 2 then
			Flasher002.Visible = not Flasher002.Visible
			Flasher003.Visible = not Flasher003.Visible
			Flasher004.Visible = not Flasher004.Visible
			Flasher005.Visible = not Flasher005.Visible
		End If
	End If
End Sub

Sub Timer002_Timer() 'Backglass lights
	If li139.State = 1 then bipl139.Visible = True:p1l139.Visible = True Else bipl139.Visible = False:p1l139.Visible = False
	If li140.State = 1 then p2l140.Visible = True Else p2l140.Visible = False
	If li141.State = 1 then p3l141.Visible = True Else p3l141.Visible = False
	If li142.State = 1 then p4l142.Visible = True Else p4l142.Visible = False
	If li149.State = 1 then gameoverl149.Visible = True Else gameoverl149.Visible = False
	If li150.State = 1 then tiltl150.Visible = True Else tiltl150.Visible = False
	If li152.State = 1 then highscorel152.Visible = True Else highscorel152.Visible = False
End Sub

Dim FDigits(27)
FDigits(0)=Array(FLight5,FLight6,FLight7,FLight8,FLight9,FLight10,FLight11)
FDigits(1)=Array(FLight12,FLight13,FLight14,FLight15,FLight16,FLight17,FLight18)
FDigits(2)=Array(FLight19,FLight20,FLight21,FLight22,FLight23,FLight24,FLight25)
FDigits(3)=Array(FLight26,FLight27,FLight28,FLight29,FLight30,FLight31,FLight32)
FDigits(4)=Array(FLight33,FLight34,FLight35,FLight36,FLight37,FLight38,FLight39)
FDigits(5)=Array(FLight40,FLight41,FLight42,FLight43,FLight44,FLight45,FLight46)
FDigits(6)=Array(FLight47,FLight48,FLight49,FLight50,FLight51,FLight52,FLight53)
FDigits(7)=Array(FLight54,FLight55,FLight56,FLight57,FLight58,FLight59,FLight60)
FDigits(8)=Array(FLight61,FLight62,FLight63,FLight64,FLight65,FLight66,FLight67)
FDigits(9)=Array(FLight68,FLight69,FLight70,FLight71,FLight72,FLight73,FLight74)
FDigits(10)=Array(FLight75,FLight76,FLight77,FLight78,FLight79,FLight80,FLight81)
FDigits(11)=Array(FLight82,FLight83,FLight84,FLight85,FLight86,FLight87,FLight88)
FDigits(12)=Array(FLight89,FLight90,FLight91,FLight92,FLight93,FLight94,FLight95)
FDigits(13)=Array(FLight96,FLight97,FLight98,FLight99,FLight100,FLight101,FLight102)
FDigits(14)=Array(FLight103,FLight104,FLight105,FLight106,FLight107,FLight108,FLight109)
FDigits(15)=Array(FLight110,FLight111,FLight112,FLight113,FLight114,FLight115,FLight116)
FDigits(16)=Array(FLight117,FLight118,FLight119,FLight120,FLight121,FLight122,FLight123)
FDigits(17)=Array(FLight124,FLight125,FLight126,FLight127,FLight128,FLight129,FLight130)
FDigits(18)=Array(FLight131,FLight132,FLight133,FLight134,FLight135,FLight136,FLight137)
FDigits(19)=Array(FLight138,FLight139,FLight140,FLight141,FLight142,FLight143,FLight144)
FDigits(20)=Array(FLight145,FLight146,FLight147,FLight148,FLight149,FLight150,FLight151)
FDigits(21)=Array(FLight152,FLight153,FLight154,FLight155,FLight156,FLight157,FLight158)
FDigits(22)=Array(FLight159,FLight160,FLight161,FLight162,FLight163,FLight164,FLight165)
FDigits(23)=Array(FLight166,FLight167,FLight168,FLight169,FLight170,FLight171,FLight172)
FDigits(24)=Array(FLight173,FLight174,FLight175,FLight176,FLight177,FLight178,FLight179)
FDigits(25)=Array(FLight180,FLight181,FLight182,FLight183,FLight184,FLight185,FLight186)
FDigits(26)=Array(FLight187,FLight188,FLight189,FLight190,FLight191,FLight192,FLight193)
FDigits(27)=Array(FLight194,FLight195,FLight196,FLight197,FLight198,FLight199,FLight200)

Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLED=Controller.ChangedLEDs(&Hffffffff,&Hffffffff)
	If Not IsEmpty(ChgLED) Then
		For ii=0 To UBound(chgLED)
		num=chgLED(ii,0):chg=chgLED(ii,1):stat=chgLED(ii,2)
			For Each obj In FDigits(num)
				If chg And 1 Then obj.Visible=stat And 1
				chg=chg\2:stat=stat\2
			Next
		Next
	End If
End Sub

'*******************************************
' Setup Backglass
'*******************************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen, ix, xx, yy, xobj

Sub setup_backglass()

	xoff = 0
	yoff = -60
	zoff = 382
	xrot = -90
	zscale = 0.0000001

	xcen = 0  '(130 /2) - (92 / 2)
	ycen = (780 /2 ) + (203 /2)

	for ix = 0 to 27
		For Each xobj In FDigits(ix)

			xx = xobj.x  
				
			xobj.x = (xoff - xcen) + xx
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y = yoff 

			If (yy < 0.) then
				yy = yy * -1
			end if

			xobj.height = (zoff - ycen) + yy - (yy * (zscale))
			xobj.rotx = xrot
		Next
	Next
end sub

Dim catctr:catctr=0
Dim catinc:catinc=1
Dim catitems
Sub Timer003_Timer()
	For Each catitems in cats
		catitems.Visible = False
	Next
	Select Case catctr
		Case 0: cat0.Visible = True
		Case 1: cat1.Visible = True
		Case 2: cat2.Visible = True
		Case 3: cat3.Visible = True
	End Select
	catctr = catctr+catinc
	If catctr = 3 then catinc=-1
	If catctr = 0 then catinc=1
End Sub

Sub sign_Timer()
	If sign.ImageA = "sign" then sign.ImageA = "sign1" Else sign.ImageA = "sign"
End Sub
