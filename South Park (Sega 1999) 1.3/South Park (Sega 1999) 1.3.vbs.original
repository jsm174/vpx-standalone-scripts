'*********************************************************************
'1999 South Park by Sega Pinball
'*********************************************************************
'*********************************************************************
'Mechanics by:	Joe Balcer, Rob Hurtado
'Software by:	Neil Falconer, Orin Day
'*********************************************************************
'*********************************************************************
'recreated for Visual Pinball by Knorr
'*********************************************************************
'*********************************************************************
'I would like to give my sincere thanks to
'Mfuegemann, Freneticamnesic, Toxie and the VPdevs, Clark Kent,Arngrim
'Gigalula, Kiwi and JP for helping me while building this table
'*********************************************************************

'V1.2
'Rebuild KennyKickers
'Rebuild CartmanKickers
'Changed Rolling Sound
'Added RampRollingSounds
'Bug Fixes
'Small Changes With Light

'V1.1
'Fix for "do while" error (thx jp!)

'V1.0
'First Release For VP10.0



Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "sega.VBS", 3.36

'********************
'Standard definitions
'********************

Const cGameName = "sprk_103"
Const UseSolenoids = 1
Const UseLamps = 1
Const SSolenoidOn = "SolOn"
Const SSolenoidOff = "SolOff"
Const SFlipperOn = "FlipperUp"
Const SFlipperOff = "FlipperDown"
Const SCoin = "Coin5"

'************************************
'Switch SlingShotPlastics to Original
'************************************


SlingPlastics = 1			'Change to 1 for "Yes, i hate Timmy and Butters"





Dim bsTrough, PlungerIM, TopVuk, SuperVuk, nBall, aBall, SlingPlastics


Set LampCallback = GetRef("UpdateMultipleLamps")
Set MotorCallback = GetRef("RealTimeUpdates")
Set nBall = ckicker.createball
	ckicker.Kick 0, 0
If SlingPlastics = 1 then
	PlasticSlingshotLinks.Image = "PlasticSlingShotStan"
	PlasticSlingShotRechts.Image = "PlasticSlingShotKenny"
End if

'************
' Table init.
'************

Sub table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .Games(cGameName).Settings.Value("rol") = 0
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        '.SetDisplayPosition 0, 0, GetPlayerHWnd 'uncomment this line if you don't see the DMD
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
'        .Switch(22) = 1 'close coin door
'        .Switch(24) = 1 'and keep it close
        PinMAMETimer.Interval = PinMAMEInterval
        PinMAMETimer.Enabled = true
        vpmNudge.TiltSwitch = swTilt
        vpmNudge.Sensitivity = 2
    End With

    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 14, 13, 12, 11, 10, 0, 0
        .InitKick BallRelease, 80, 5
        .InitExitSnd SoundFX("BallRelease", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .Balls = 5
        .IsTrough = 1
    End With


	Set TopVuk = New cvpmBallStack
	With TopVuk
		.InitSaucer sw46, 46, -90, 100
		.InitExitSnd SoundFX("SafeHouseKick", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
		.KickForceVar = 5
		.KickZ = 1.4
        .KickBalls = 1
	End With

    Set SuperVuk = New cvpmBallStack
    With SuperVuk
        .InitSw 0, 45, 0, 0, 0, 0, 0, 0
        .InitKick sw45, -80, 70
        .InitExitSnd SoundFX("KickandWire", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
        .KickZ = 1.4
    End With

    If table1.ShowDT = False then
        Ramp16.WidthTop = 0
        Ramp16.WidthBottom = 0
        Ramp15.WidthTop = 0
        Ramp15.WidthBottom = 0
        Korpus.Size_Y = 1.7
		Korpus.Z = 60
    End if
End Sub


'******
'Trough
'******
Dim BallCount

Sub SolRelease(Enabled)
    If Enabled Then
'        If bsTrough.Balls = 5 Then vpmTimer.PulseSw 15
        If bsTrough.Balls > 0 Then bsTrough.ExitSol_On
    End If
End Sub

Sub BallRelease_UnHit()
	BallCount = BallCount +1
	End Sub

Sub Drain_Hit
	BallCount = BallCount -1
    PlaySound "Balltruhe", 0, 0.5, 0
    bsTrough.AddBall Me
    If BallCount <1 then GiOff:GiOffState = True
End Sub

'Autoplunger

Sub AutoLaunch(Enabled)
	If Enabled Then
		Plunger.Autoplunger = True
		Plunger.Fire
		PlaySound SoundFX("AutoPlunger", DOFContactors)
		Else
		Plunger.Autoplunger = False
	End If
 End Sub

'********
'ChefVuk
'********

Sub sw46_Hit()
	PlaySound "SafeHouseHit"
	TopVuk.AddBall Me
End Sub

Sub sw46_UnHit()
	GiOn
End Sub

'*********
'CartmanVuk
'*********

Dim cBall, cBall1, cBall2, cBall3, cBall4, cBall5, cBall6, cBall7

Sub CartmanKickerHole_Hit()
    RandomSoundCartman
    Set cBall = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole1_Hit()
    RandomSoundCartman
    Set cBall1 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole2_Hit()
    RandomSoundCartman
    Set cBall2 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole3_Hit()
    RandomSoundCartman
    Set cBall3 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole4_Hit()
    RandomSoundCartman
    Set cBall4 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole5_Hit()
    RandomSoundCartman
    Set cBall5 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole6_Hit()
    RandomSoundCartman
    Set cBall6 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub CartmanKickerHole7_Hit()
    RandomSoundCartman
    Set cBall7 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 48
    vpmTimer.AddTimer 1200, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub


Sub SuperVukAddBall()
	SuperVuk.AddBall 1
End Sub


Sub CartmanKickerHole_Timer
    Do While cBall.Z > 0
        cBall.Z = cBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole1_Timer
    Do While cBall1.Z > 0
        cBall1.Z = cBall1.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole2_Timer
    Do While cBall2.Z > 0
        cBall2.Z = cBall2.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole3_Timer
    Do While cBall3.Z > 0
        cBall3.Z = cBall3.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole4_Timer
    Do While cBall4.Z > 0
        cBall4.Z = cBall4.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole5_Timer
    Do While cBall5.Z > 0
        cBall5.Z = cBall5.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole6_Timer
    Do While cBall6.Z > 0
        cBall6.Z = cBall6.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub CartmanKickerHole7_Timer
    Do While cBall7.Z > 0
        cBall7.Z = cBall7.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub sw45_UnHit()
	BallinToilet = 0
End Sub

'*********
'KennyVuk
'*********

Dim KennySolLeft
Dim KennySolRight

Dim kBall, kBall1, kBall2, kBall3, kBall4, kBall5

Sub KennyLeft(Enabled)
	If Enabled then
		UpdateKennyLeft.Enabled = 1
		KennySolLeft = True
		RandomSoundKennyMove
		Else
		UpdateKennyLeft.Enabled = 0
		Kenny.RotZ = 0
		KennyL.RotZ = 0
	End if
End Sub

Sub KennyRight(Enabled)
	If Enabled then
		UpdateKennyRight.Enabled = 1
		KennySolRight = True
		RandomSoundKennyMove
		Else
		UpdateKennyright.Enabled = 0
		Kenny.RotZ = 0
		KennyL.RotZ = 0
	End if
End Sub

Sub KennyDead(Enabled)
	If Enabled then
		KennyFlipper.RotatetoEnd
		PlaySound SoundFX("TrapDoorLow",DOFContactors)
		Else
		KennyFlipper.RotatetoStart
		PlaySound SoundFX("FlipperDown",DOFContactors)
	End if
End Sub

Sub updatekennydead_Timer()
	    Kenny.RotX = KennyFlipper.CurrentAngle
End Sub


'Kenny Animation
'RotX -1 = KennyDead
'RotY +1 = rotate clockwise
'RotZ +1 = KennyLeft
'RotZ -1 = KennyRight

Sub UpdateKennyLeft_Timer()
	If KennySolLeft = True And Kenny.RotZ < 8 then Kenny.RotZ = Kenny.RotZ +1
	If KennysolLeft = False And Kenny.RotZ > 0 then Kenny.RotZ = Kenny.RotZ -1
	If Kenny.RotZ >= 8 Then KennySolLeft = False

	If KennySolLeft = True And KennyL.RotZ < 8 then KennyL.RotZ = KennyL.RotZ +1
	If KennysolLeft = False And KennyL.RotZ > 0 then KennyL.RotZ = KennyL.RotZ -1
	If KennyL.RotZ >= 8 Then KennySolLeft = False
End Sub

Sub UpdateKennyRight_Timer()
	If KennySolRight = True And Kenny.RotZ > -8 then Kenny.RotZ = Kenny.RotZ -1
	If KennySolRight = False And Kenny.RotZ < 0 then Kenny.RotZ = Kenny.RotZ +1
	If Kenny.RotZ <= -8 Then KennySolRight = False

	If KennySolRight = True And KennyL.RotZ > -8 then KennyL.RotZ = KennyL.RotZ -1
	If KennySolRight = False And KennyL.RotZ < 0 then KennyL.RotZ = KennyL.RotZ +1
	If KennyL.RotZ <= -8 Then KennySolRight = False
End Sub




Sub KennyKickerHole_Hit()
    RandomSoundKenny
    Set kBall = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole_Timer()
    Do While kball.Z > 0
        kBall.Z = kBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub KennyKickerHole1_Hit()
    RandomSoundKenny
    Set kBall1 = ActiveBall:Me.TimerEnabled = 1
	vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub


Sub KennyKickerHole1_Timer()
    Do While kBall1.Z > 0
        kBall1.Z = kBall1.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub KennyKickerHole2_Hit()
    RandomSoundKenny
    Set kBall2 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole2_Timer()
    Do While kBall2.Z > 0
        kBall2.Z = kBall2.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

Sub KennyKickerHole3_Hit()
    RandomSoundKenny
    Set kBall3 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole3_Timer()
    Do While kBall3.Z > 0
        kBall3.Z = kBall3.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub


Sub KennyKickerHole4_Hit()
    RandomSoundKenny
    Set kBall4 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole4_Timer()
    Do While kBall4.Z > 0
        kBall4.Z = kBall4.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub


Sub KennyKickerHole5_Hit()
    RandomSoundKenny
    Set kBall5 = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 44
    vpmTimer.AddTimer 2000, "SuperVukAddBall'"
    Me.Enabled = 0
End Sub

Sub KennyKickerHole5_Timer()
    Do While kBall5.Z > 0
        kBall5.Z = kBall5.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub


Sub BallTrigger_Hit()
	ActiveBall.VelZ = +15
End Sub


'*********
'Toilet
'*********
Dim FlasherF7Visible


'Seat
Sub ToiletSeatLid(Enabled)
	If Enabled then
		ToiletFlipper.RotatetoEnd
		ToiletKicker.Enabled = True
		PlaySound SoundFX("FlipperDown",DOFContactors)
	Else
		ToiletFlipper.RotatetoStart
		ToiletKicker.Enabled = False
		PlaySound SoundFX("FlipperDown",DOFContactors)
	End if
End Sub


'MrHankeyUp
Sub MrHankeyup(Enabled)
	If Enabled then
	MrHankeyFlipper.RotatetoEnd
	CisternFlipper.RotatetoEnd
	FlasherF7Visible = True
	PlaySound SoundFX("TrapDoorLow",DOFContactors)
	End if
End Sub

'MrHankey
Sub MrHankeyDown(Enabled)
	If Enabled then
	MrHankeyFlipper.RotatetoStart
	CisternFlipper.RotatetoStart
	FlasherF7Visible = False
	End if
End Sub

Dim BallinToilet

Sub ToiletKicker_Hit
	BallinToilet = 1
	sw26spinner.timerenabled = False
    RandomSoundCartman
    Set aBall = ActiveBall:Me.TimerEnabled = 1
    vpmTimer.PulseSw 43
    vpmTimer.AddTimer 2400, "SuperVukAddBall'"
    GiBlinking
	if BallCount < 2 then
		vpmTimer.AddTimer 2400, "GiOff'"
		else
		vpmTimer.AddTimer 2400, "GiOn'"
	End if
End Sub


Sub ToiletKicker_Timer
    Do While aBall.Z > 0
        aBall.Z = aBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
End Sub

'''*****************************************************************************************
'''*freneticamnesic level nudge script, based on rascals nudge bobble with help from gtxjoe*
'''*     add timers and "Nudgebobble(keycode)" to left and right tilt keys to activate     *
'''*****************************************************************************************


dim Shaketime, keycode

ShakeTime = 20


Sub LeftHit_Hit()
	nudgebobble2(keycode)
	nudgebobble3(keycode)
	nudgebobble4(keycode)
End Sub

Sub RightHit_Hit()
	nudgebobble2(keycode)
	nudgebobble3(keycode)
	nudgebobble4(keycode)
End Sub



Dim bgcharctr2a:bgcharctr2a = 3
Dim bgcharctr2b:bgcharctr2b = 3
Dim bgcharctr3a:bgcharctr3a = 3
Dim bgcharctr3b:bgcharctr3b = 3
Dim bgcharctr4a:bgcharctr4a = 3
Dim bgcharctr4b:bgcharctr4b = 3

Dim centerlocation2:centerlocation2 = 0
Dim bgdegree2a:bgdegree2a = 5 'move +/- 8 degrees
Dim bgdegree2b:bgdegree2b = 5 'move +/- 8 degrees
Dim bgdurationctr2a:bgdurationctr2a = 0
Dim bgdurationctr2b:bgdurationctr2b = 0
Dim centerlocation3:centerlocation3 = 0
Dim bgdegree3a:bgdegree3a = 5 'move +/- 8 degrees
Dim bgdegree3b:bgdegree3b = 5 'move +/- 8 degrees
Dim bgdurationctr3a:bgdurationctr3a = 0
Dim bgdurationctr3b:bgdurationctr3b = 0
Dim centerlocation4:centerlocation4 = 0
Dim bgdegree4a:bgdegree4a = 5 'move +/- 8 degrees
Dim bgdegree4b:bgdegree4b = 5 'move +/- 8 degrees
Dim bgdurationctr4a:bgdurationctr4a = 0
Dim bgdurationctr4b:bgdurationctr4b = 0

Sub LevelT2_Timer()
	eric.RotAndTra7 = eric.RotAndTra7 + bgcharctr2a  'change rotation value by bgcharctr
	If eric.RotAndTra7 >= bgdegree2a + centerlocation2 then bgcharctr2a = -1:bgdurationctr2a = bgdurationctr2a + 1   'if level moves past max degrees, change direction and increate durationctr
	If eric.RotAndTra7 <= -bgdegree2a + centerlocation2 then bgcharctr2a = 1  'if level moves past min location, change direction
	eric.ObjRotX = eric.ObjRotX + bgcharctr2b  'change rotation value by bgcharctr
	If eric.ObjRotX >= bgdegree2b + centerlocation2 then bgcharctr2b = -1:bgdurationctr2b = bgdurationctr2b + 1   'if level moves past max degrees, change direction and increate durationctr
	If eric.ObjRotX <= -bgdegree2b + centerlocation2 then bgcharctr2b = 1  'if level moves past min location, change direction

	If bgdurationctr2a = ShakeTime then bgdegree2a = bgdegree2a - 2:bgdurationctr2a = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdurationctr2b = ShakeTime then bgdegree2b = bgdegree2b - 2:bgdurationctr2b = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdegree2a <= 0 then LevelT2.Enabled = False:bgdegree2a = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
	If bgdegree2b <= 0 then LevelT2.Enabled = False:bgdegree2b = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
End Sub

Sub LevelT3_Timer()
	'Dim loopctr
	Kyle.RotAndTra7 = Kyle.RotAndTra7 + bgcharctr3a  'change rotation value by bgcharctr
	If Kyle.RotAndTra7 >= bgdegree3a + centerlocation3 then bgcharctr3a = -1:bgdurationctr3a = bgdurationctr3a + 1   'if level moves past max degrees, change direction and increate durationctr
	If Kyle.RotAndTra7 <= -bgdegree3a + centerlocation3 then bgcharctr3a = 1  'if level moves past min location, change direction
	Kyle.ObjRotX = Kyle.ObjRotX + bgcharctr3b  'change rotation value by bgcharctr
	If Kyle.ObjRotX >= bgdegree3b + centerlocation3 then bgcharctr3b = -1:bgdurationctr3b = bgdurationctr3b + 1   'if level moves past max degrees, change direction and increate durationctr
	If Kyle.ObjRotX <= -bgdegree3b + centerlocation3 then bgcharctr3b = 1  'if level moves past min location, change direction

	If bgdurationctr3a = ShakeTime then bgdegree3a = bgdegree3a - 2:bgdurationctr3a = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdurationctr3b = ShakeTime then bgdegree3b = bgdegree3b - 2:bgdurationctr3b = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdegree3a <= 0 then LevelT3.Enabled = False:bgdegree3a = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
	If bgdegree3b <= 0 then LevelT3.Enabled = False:bgdegree3b = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
End Sub

Sub LevelT4_Timer()
	'Dim loopctr
	Stan.RotAndTra7 = Stan.RotAndTra7 + bgcharctr4a  'change rotation value by bgcharctr
	If Stan.RotAndTra7 >= bgdegree4a + centerlocation4 then bgcharctr4a = -1:bgdurationctr4a = bgdurationctr4a + 1   'if level moves past max degrees, change direction and increate durationctr
	If Stan.RotAndTra7 <= -bgdegree4a + centerlocation4 then bgcharctr4a = 1  'if level moves past min location, change direction
	Stan.ObjRotX = Stan.ObjRotX + bgcharctr4b  'change rotation value by bgcharctr
	If Stan.ObjRotX >= bgdegree4b + centerlocation4 then bgcharctr4b = -1:bgdurationctr4b = bgdurationctr4b + 1   'if level moves past max degrees, change direction and increate durationctr
	If Stan.ObjRotX <= -bgdegree4b + centerlocation4 then bgcharctr4b = 1  'if level moves past min location, change direction

	If bgdurationctr4a = ShakeTime then bgdegree4a = bgdegree4a - 2:bgdurationctr4a = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdurationctr4b = ShakeTime then bgdegree4b = bgdegree4b - 2:bgdurationctr4b = 0 'if level has moved back and forth 4 times, decrease amount of movement by -2 and repeat by resetting durationctr
	If bgdegree4a <= 0 then LevelT4.Enabled = False:bgdegree4a = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
	If bgdegree4b <= 0 then LevelT4.Enabled = False:bgdegree4b = 5 'if amount of movement is 0, turn off LevelT timer and reset movement back to max 8 degrees
End Sub


Sub Nudgebobble2(keycode)
	LevelT2.Enabled = True:bgdurationctr2a = 0:bgdegree2a = 7:bgdurationctr2b = 0:bgdegree2b = 7
	'LevelT3.Enabled = True:bgdurationctr3 = 0:bgdegree3 = 7
	'LevelT4.Enabled = True:bgdurationctr4 = 0:bgdegree4 = 7
End Sub

Sub Nudgebobble3(keycode)
	'LevelT2.Enabled = True:bgdurationctr2 = 0:bgdegree2 = 7
	LevelT3.Enabled = True:bgdurationctr3a = 0:bgdegree3a = 7:bgdurationctr3b = 0:bgdegree3b = 7
	'LevelT4.Enabled = True:bgdurationctr4 = 0:bgdegree4 = 7
End Sub

Sub Nudgebobble4(keycode)
	'LevelT2.Enabled = True:bgdurationctr2 = 0:bgdegree2 = 7
	'LevelT3.Enabled = True:bgdurationctr3 = 0:bgdegree3 = 7
	LevelT4.Enabled = True:bgdurationctr4a = 0:bgdegree4a = 7:bgdurationctr4b = 0:bgdegree4b = 7
End Sub

'Sub bobblesome_Timer()  'This looks like a free running timer that 1 out of ten times will start movement 
'	Dim chance
'	chance = Int(10*Rnd+1)
'	If chance = 5 then Nudgebobble(CenterTiltKey)
'End Sub




'*********
'Solenoids
'*********

SolCallback(1) = "SolRelease"
SolCallback(2) = "AutoLaunch"
SolCallback(3) = "SuperVuk.SolOut"
SolCallback(4) = "TopVuk.SolOut"
SolCallback(5) = "ToiletSeatLid"
SolCallback(6) = "MrHankeyUp"
SolCallback(13) = "MrHankeyDown"
SolCallBack(14) = "KennyDead"
SolCallBack(19) = "KennyLeft"
SolCallback(20) = "KennyRight"
'SolCallback(24) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"



'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If KeyCode = MechanicalTilt Then
        vpmTimer.PulseSw vpmNudge.TiltSwitch
        Exit Sub
    End if
	If keycode = PlungerKey Then Plunger.Pullback:Playsound "plungerpull":End if
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = PlungerKey Then Plunger.Fire: PlaySound "Plunger"
    If vpmKeyUp(keycode) Then Exit Sub
End Sub


'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("FlipperUpLeft", DOFContactors):FlipperLeft.RotateToEnd
    Else
        PlaySound SoundFX("FlipperDown", DOFContactors):FlipperLeft.RotateToStart:FlipperLeft.return = returnspeed * 0.5
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("FlipperUpRightBoth", DOFContactors):FlipperRight.RotateToEnd
    Else
        PlaySound SoundFX("FlipperDown", DOFContactors):FlipperRight.RotateToStart:FlipperRight.return = returnspeed * 0.5
    End If
End Sub

dim returnspeed, lfstep, rfstep
returnspeed = FlipperLeft.return
lfstep = 1
rfstep = 1

sub leftflipper_timer()
	select case lfstep
		Case 1: leftflipper.return = returnspeed * 0.6 :lfstep = lfstep + 1
		Case 2: leftflipper.return = returnspeed * 0.7 :lfstep = lfstep + 1
		Case 3: leftflipper.return = returnspeed * 0.8 :lfstep = lfstep + 1
		Case 4: leftflipper.return = returnspeed * 0.9 :lfstep = lfstep + 1
		Case 5: leftflipper.return = returnspeed * 1 :lfstep = lfstep + 1
		Case 6: leftflipper.timerenabled = 0 : lfstep = 1
	end select
end sub

sub rightflipper_timer()
	select case rfstep
		Case 1: rightflipper.return = returnspeed * 0.6 :rfstep = rfstep + 1
		Case 2: rightflipper.return = returnspeed * 0.7 :rfstep = rfstep + 1
		Case 3: rightflipper.return = returnspeed * 0.8 :rfstep = rfstep + 1
		Case 4: rightflipper.return = returnspeed * 0.9 :rfstep = rfstep + 1
		Case 5: rightflipper.return = returnspeed * 1 :rfstep = rfstep + 1
		Case 6: rightflipper.timerenabled = 0 : rfstep = 1
	end select
end sub


'*********
' Switches
'*********

Sub sw16_Hit:Controller.Switch(16) = 1:sw16wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'shooterlane'
Sub sw16_UnHit:Controller.Switch(16) = 0:sw16wire.RotX = 0:GiOn:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0: End Sub

Sub sw26_Hit:Controller.Switch(26) = 1: sw26spinner.timerenabled = True:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0: End Sub

Sub sw26spinner_Timer()
	If (sw26spinner.currentangle > 9 Or  sw26spinner.currentangle < -9) then
	GiBlinking
	Else
	if Ballcount > 0 then GiOn
'	if Ballcount < 1 then GiOff
	End if
End Sub

Sub sw42_Hit: Controller.Switch(42) = 1:ActiveBall.VelZ = 0: End Sub
Sub sw42_UnHit: Controller.Switch(42) = 0: End Sub
'Sub Wall50_Hit: vpmTimer.PulseSw 42: ActiveBall.VelZ = 0:End Sub

Sub sw47_Hit:Controller.Switch(47) = 1:sw47wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'left orbit'
Sub sw47_UnHit:Controller.Switch(47) = 0:sw47wire.RotX = 0:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:GiOff:sw57wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'left outlane'
Sub sw57_UnHit:Controller.Switch(57) = 0:GiOn::sw57wire.RotX = 0:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:sw58wire.RotX = 15:sw26spinner.timerenabled = False:PlaySound "metalhit_thin":End Sub 'left inlane'
Sub sw58_UnHit:Controller.Switch(58) = 0:sw58wire.RotX = 0:End Sub

Sub sw60_Hit:Controller.Switch(60) = 1:GiOff:sw60wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'right outlane'
Sub sw60_UnHit:Controller.Switch(60) = 0:GiOn:sw60wire.RotX = 0:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:sw61wire.RotX = 15:PlaySound "metalhit_thin":End Sub 'right inlane'
Sub sw61_UnHit:Controller.Switch(61) = 0:sw61wire.RotX = 0:End Sub

'*********
' Targets
'*********


Sub T17_Hit:vpmTimer.PulseSw 17: DTGrandpa.RotX = DTGrandpa.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T17_Timer:DTGrandpa.RotX = DTGrandpa.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T18_Hit:vpmTimer.PulseSw 18: DTMsCartman.RotX = DTMsCartman.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T18_Timer:DTMsCartman.RotX = DTMsCartman.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T19_Hit:vpmTimer.PulseSw 19: DTMayor.RotX = DTMayor.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T19_Timer:DTMayor.RotX = DTMayor.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T20_Hit:vpmTimer.PulseSw 20: DTBarbrady.RotX = DTBarbrady.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T20_Timer:DTBarbrady.RotX = DTBarbrady.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T21_Hit:vpmTimer.PulseSw 21: DTHugo.RotX = DTHugo.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T21_Timer:DTHugo.RotX = DTHugo.RotX -5:Me.TimerEnabled = 0:End Sub


Sub T22_Hit:vpmTimer.PulseSw 22: DTJ.RotX = DTJ.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T22_Timer:DTJ.RotX = DTJ.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T23_Hit:vpmTimer.PulseSw 23: DTSirJohn.RotX = DTSirJohn.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T23_Timer:DTSirJohn.RotX = DTSirJohn.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T24_Hit:vpmTimer.PulseSw 24: DTH.RotX = DTH.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T24_Timer:DTH.RotX = DTH.RotX -5:Me.TimerEnabled = 0:End Sub


Sub T34_Hit:vpmTimer.PulseSw 34: DTWendy.RotX = DTWendy.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T34_Timer:DTWendy.RotX = DTWendy.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T35_Hit:vpmTimer.PulseSw 35: DTMrGarrison.RotX = DTMrGarrison.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T35_Timer:DTMrGarrison.RotX = DTMrGarrison.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T36_Hit:vpmTimer.PulseSw 36: DTMrMackey.RotX = DTMrMackey.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T36_Timer:DTMrMackey.RotX = DTMrMackey.RotX -5:Me.TimerEnabled = 0:End Sub


Sub T37_Hit:vpmTimer.PulseSw 37: DTNed.RotX = DTNed.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T37_Timer:DTNed.RotX = DTNed.RotX -5:Me.TimerEnabled = 0:End Sub
Sub T38_Hit:vpmTimer.PulseSw 38: DTMephisto.RotX = DTMephisto.RotX +5:Playsound SoundFX("target", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub T38_Timer:DTMephisto.RotX = DTMephisto.RotX -5:Me.TimerEnabled = 0:End Sub


'*********
' Gates
'*********

Sub GateUpdate()
	WireBumpers.RotX = BumperGate.currentangle +90
	WirePlungerLane.RotX = PlungerLaneGate.currentangle +90
	sw25wire.RotX = sw25spinner.currentangle +90
	sw26wire.RotX = sw26spinner.currentangle +90
End Sub


'*********
' Bumper
'*********

Sub LeftBumper_hit:vpmTimer.pulseSw 49:Playsound SoundFX("BumperLeft", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub LeftBumper_Timer:Me.Timerenabled = 0:End Sub

Sub RightBumper_hit:vpmTimer.pulseSw 50:Playsound SoundFX("BumperMiddle", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub RightBumper_Timer:Me.Timerenabled = 0:End Sub

Sub BottomBumper_hit:vpmTimer.pulseSw 51:Playsound SoundFX("BumperRight", DOFContactors):Me.TimerEnabled = 1:End Sub
Sub BottomBumper_Timer:Me.Timerenabled = 0:End Sub



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************


Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySound SoundFX("SlingshotLeft", DOFContactors), 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 62
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySound SoundFX("SlingshotRight", DOFContactors), 0, 1, -0.05, 0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    vpmTimer.PulseSw 59
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


'*****************************************
 '  JP's Fading Lamps 3.4 VP9 Fading only
 '      Based on PD's Fading Lights
 ' SetLamp 0 is Off
 ' SetLamp 1 is On
 ' LampState(x) current state
 '****************************************
 
InitLamps

Sub InitLamps()
    Set Lights(1) = l1
    Set Lights(2) = l2
    Set Lights(3) = l3
    Set Lights(4) = l4
    Set Lights(5) = l5
    Set Lights(6) = l6
    Set Lights(7) = l7
    Set Lights(8) = l8
    Set Lights(9) = l9
    Set Lights(10) = l10
    Set Lights(11) = l11
    Set Lights(12) = l12
    Set Lights(13) = l13
    Set Lights(14) = l14
    Set Lights(15) = l15
    Set Lights(16) = l16
    Set Lights(17) = l17
    Set Lights(18) = l18
    Set Lights(19) = l19
    Set Lights(20) = l20
    Set Lights(21) = l21
    Set Lights(22) = l22
    Set Lights(23) = l23
    Set Lights(24) = l24
    Set Lights(25) = l25
    Set Lights(26) = l26
    Set Lights(27) = l27
    Set Lights(28) = l28
    Set Lights(29) = l29
    Set Lights(30) = l30
    Set Lights(33) = l33
    Set Lights(34) = l34
    Set Lights(35) = l35
    Set Lights(36) = l36
    Set Lights(37) = l37
    Set Lights(38) = l38
    Set Lights(39) = l39
    Set Lights(40) = l40
    Set Lights(41) = l41
    Set Lights(42) = l42
    Set Lights(43) = l43
    Set Lights(44) = l44
    Set Lights(45) = l45
    Set Lights(46) = l46
    Set Lights(48) = l48
    Set Lights(50) = l50
    Set Lights(51) = l51
    Set Lights(52) = l52
    Set Lights(53) = l53
    Set Lights(54) = l54
    Set Lights(55) = l55
    Set Lights(56) = l56
    Set Lights(57) = l57
    Set Lights(58) = l58
    Set Lights(59) = l59
    Set Lights(60) = l60
    Set Lights(61) = l61
    Set Lights(62) = l62
    Set Lights(63) = l63
    Set Lights(64) = l64
End Sub
 
  Sub UpdateMultipleLamps()
		If l30.state = 1 then Kenny.Visible = False:KennyL.Visible = True: else Kenny.Visible = True: KennyL.Visible = False
		If l22.state = 1 then BulbCoverKennyRed.visible = False:BulbCoverKennyRedL.visible = True:BulbCoverKennyRedF.visible = True:BulbCoverKennyRedF1.Visible = True: else BulbCoverKennyRed.visible = True: BulbCoverKennyRedL.visible = False:BulbCoverKennyRedF.visible = False: BulbCoverKennyRedF1.Visible = False
		If l23.state = 1 then BulbCoverKennyYellow.visible = False:BulbCoverKennyYellowL.visible = True:BulbCoverKennyYellowF.visible = True: BulbCoverKennyYellowF1.Visible = True: else BulbCoverKennyYellow.visible = True: BulbCoverKennyYellowL.visible = False: BulbCoverKennyYellowF.visible = False:BulbCoverKennyYellowF1.Visible = False
		If l46.state = 1 then bulbgreenleft.visible = False:bulbgreenleftL.visible = True:bulbgreenleftF.Visible = True: Else bulbgreenleft.visible = True:bulbgreenleftL.visible = False:bulbgreenleftF.visible = False
		If l48.state = 1 then bulbgreenright.visible = False:bulbgreenrightL.visible = True:bulbgreenrightf.visible = True: Else bulbgreenright.visible = True:bulbgreenrightL.visible = False:bulbgreenrightF.visible = False
		If l30.state = 1 then l30a.state = 1:FlasherF6.Visible = True: Else FlasherF6.Visible = False:l30a.state = 0
		If l38.state = 1 then BumperLeftLight:BumperFlasher.Visible = 1: Else BumperLeftLightOff:BumperFlasher.Visible = 0
		If l39.state = 1 then BumperRightLight: Else BumperRightLightOff
		If l40.state = 1 then BumperBottomLight: Else BumperBottomLightOff
End Sub

'RainbowLight

Dim RGBStep, RGBFactor, Red, Green, Blue

RGBStep = 0
RGBFactor = 1
Red = 255
Green = 0
Blue = 0

Sub RGBTimer_timer 'rainbow light color changing
    Select Case RGBStep
        Case 0 'Green
            Green = Green + RGBFactor
            If Green > 255 then
                Green = 255
                RGBStep = 1
            End If
        Case 1 'Red
            Red = Red - RGBFactor
            If Red < 0 then
                Red = 0
                RGBStep = 2
            End If
        Case 2 'Blue
            Blue = Blue + RGBFactor
            If Blue > 255 then
                Blue = 255
                RGBStep = 3
            End If
        Case 3 'Green
            Green = Green - RGBFactor
            If Green < 0 then
                Green = 0
                RGBStep = 4
            End If
        Case 4 'Red
            Red = Red + RGBFactor
            If Red > 255 then
                Red = 255
                RGBStep = 5
            End If
        Case 5 'Blue
            Blue = Blue - RGBFactor
            If Blue < 0 then
                Blue = 0
                RGBStep = 0
            End If
    End Select
    'Light1.color = RGB(Red\10, Green\10, Blue\10)
    l14.colorfull = RGB(Red, Green, Blue)
'    light2.color = RGB(Red, Green, Blue)
'    textbox1.text = Red
'    textbox2.text = Green
'    textbox3.text = Blue
End Sub


'*********************
'Generell Illumination
'*********************

Dim bulb, fbulb, GiOnState, GiOffState

Sub GIOff()
	GiOffState = True
	for each bulb in GI
	bulb.state = 0
	Table1.ColorGradeImage = "-70"
	for each fbulb in GiFlasher
	fbulb.visible = False
	next
	next
End Sub

Sub GIOn()
	GiOnState = True
	for each bulb in GI
	bulb.state = 1
	for each fbulb in GiFlasher
	fbulb.visible = True
	Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
	next
	next
End Sub

Sub GiBlinking()
	GiOnState = False
	GiOffState = False
	FlasherTimer1.Enabled = True
	Table1.ColorGradeImage = "-30"
End Sub


'GiFlasherBlinking

Sub FlasherTimer1_Timer()
	for each fbulb in GiFlasher
	fbulb.visible = 0
	for each bulb in Gi
	bulb.state = 0
	FlasherTimer1.Enabled = 0
	If GiOffState = False then FlasherTimer2.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer2_Timer()
	for each fbulb in GiFlasher
	fbulb.visible = 1
	for each bulb in Gi
	bulb.state = 1
	If GiOnState = False then FlasherTimer1.Enabled = 1: Else 
	FlasherTimer2.Enabled = 0
	next
	next
End Sub


'*********************
'BumperRingLights
'*********************

Sub BumperLeftLight()
	for each bulb in RingLightLeftBumper
	bulb.state = 1
	next
End Sub

Sub BumperLeftLightOff()
	for each bulb in RingLightLeftBumper
	bulb.state = 0
	next
End Sub

Sub BumperRightLight()
	for each bulb in RingLightRightBumper
	bulb.state = 1
	next
End Sub

Sub BumperRightLightOff()
	for each bulb in RingLightRightBumper
	bulb.state = 0
	next
End Sub


Sub BumperBottomLight()
	for each bulb in RingLightBottomBumper
	bulb.state = 1
	next
End Sub

Sub BumperBottomLightOff()
	for each bulb in RingLightBottomBumper
	bulb.state = 0
	next
End Sub



'*********
'Flasher
'*********

SolCallback(7) = "FlashPops"
SolCallback(18) = "FlashTopVuk"
SolCallback(25) = "FlashStan"
SolCallback(26) = "FlashChef"
SolCallback(27) = "FlashKenny"
SolCallback(28) = "FlashKyle"
SolCallback(29) = "FlashCartman"
SolCallback(30) = "FlashKennyAndBack"
SolCallback(31) = "FlashMrHankeyToilet"
SolCallback(32) = "FlashSuperVuk"


Sub FlashMrHankeyToilet(Enabled)
	If Enabled Then
	BulbCoverToiletteLeft.Visible = False
	BulbCoverToiletteLeftOn.Visible = True
	BulbCoverToiletteRight.Visible = False
	BulbcoverToiletteRightOn.Visible = True
	F7l.State = 1
	F7r.State = 1
	If FlasherF7Visible = True then FlasherF7.Visible = True
	If FlasherF7Visible = True then FlasherF7a.Visible = True
	Else
	BulbCoverToiletteLeft.Visible = True
	BulbcoverToiletteLeftOn.Visible = False
	BulbCoverToiletteRight.Visible = True
	BulbCoverToiletteRightOn.Visible = False
	F7l.State = 0
	F7r.State = 0
	FlasherF7.Visible = False
	FlasherF7a.Visible = False
	End if
End Sub

Sub FlashStan(Enabled)
	If Enabled Then
	F1.State = 1
	F1a.State = 1
	Else
	F1.State = 0
	F1a.State = 0
	End if
End Sub

Sub FlashChef(Enabled)
	If Enabled Then
	F2.State = 1
	F2a.State = 1
	Else
	F2.State = 0
	F2a.State = 0
	End if
End Sub

Sub FlashKenny(Enabled)
	If Enabled Then
	F3.State = 1
	F3a.State = 1
	Else
	F3.State = 0
	F3a.State = 0
	End if
End Sub

Sub FlashKyle(Enabled)
	If Enabled Then
	F4.State = 1
	F4a.State = 1
	Else
	F4.State = 0
	F4a.State = 0
	End if
End Sub

Sub FlashCartman(Enabled)
	If Enabled Then
	F5.State = 1
	F5a.State = 1
	Else
	F5.State = 0
	F5a.State = 0
	End if
End Sub

Sub FlashSuperVuk(Enabled)
	If Enabled Then
	F8.State = 1
	FlasherCapRed.Image = "dome3_redOn"
	FlasherF8.Visible = True
	if Ballcount > 0 And BallinToilet = 0 then GiBlinking
	Else
	F8.State = 0
	FlasherCapRed.Image = "dome3_red"
	FlasherF8.Visible = False
	If Ballcount > 0 And BallinToilet = 0 then GiOn
	End if
End Sub

Sub FlashPops(Enabled)
	If Enabled Then
	FPa.State = 1
	FPb.State = 1
	FPc.State = 1
	FPd.State = 1
	FlasherCapYellow.Image = "dome3_yellowOn"
	FlasherFPa.Visible = True
	Else
	FPa.State = 0
	FPb.State = 0
	FPc.State = 0
	FPd.State = 0
	FlasherCapYellow.Image = "dome3_yellow"
	FlasherFPa.Visible = False
	End if
End Sub

Sub FlashKennyandBack(Enabled)
	If Enabled Then
	F6b.State = 1
	F6a.Visible = False
	F6a1.Visible = True
	Flasherf6.Visible = True
	FlasherF6a.Visible = True
	Kenny.Visible = False
	KennyL.Visible = True
	Else
	F6b.State = 0
	F6a.Visible = True
	F6a1.Visible = False
	FlasherF6.Visible = False
	FlasherF6a.Visible = False
	Kenny.Visible = True
	KennyL.Visible = False
	End if
End Sub

Sub FlashTopVuk(Enabled)
	If Enabled Then
	F18.State = 1
	FlasherCapChef.Image = "dome3_redOn"
	FlasherF18.Visible = True
	FlasherF18a.Visible = True
	If BallCount > 0 And BallinToilet = 0 then GiBlinking
	Else
	F18.State = 0
	FlasherCapChef.Image = "dome3_red"
	FlasherF18.Visible = False
	FlasherF18a.Visible = False
	If BallCount > 0 And BallinToilet = 0 then GiOn
	End if
End Sub



	


' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 1800)
End Function

Function Vol2(ball1, ball2) ' Calculates the Volume of the sound based on the speed of two balls
    Vol2 = (Vol(ball1) + Vol(ball2) ) / 2
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

'*****************************************
'    JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 9 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = FALSE
    Next
End Sub

Sub RollingSoundUpdate()
    Dim BOT, b
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = FALSE
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = TRUE
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
        Else
            If rolling(b) = TRUE Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = FALSE
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

'*******************
' WireRampDropSounds
'*******************

Dim SoundBall

Sub CartmanWireStart_Hit
	GiOn
End Sub

Sub CartmanWireEnd_Hit
	PlaySound "BallDrop"
End Sub

Sub Trigger3_Hit()
	StopSound "plasticrolling"
	PlaySound "Balldrop"
End Sub

Sub Trigger4_Hit()
	StopSound "plasticrolling"
	PlaySound "Balldrop"
End Sub

Sub Trigger5_Hit()
	Playsound "Balldrop"
End Sub

Sub Trigger6_Hit()
	Stopsound "plasticrolling"
End Sub

Sub RightRampColl_Hit()
	Set SoundBall = ActiveBall
	PlaySound "plasticrolling", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub RightRampColl_UnHit()
	StopSound "plasticrolling"
End Sub

Sub LeftRampColl_UnHit()
	StopSound "plasticrolling"
End Sub

Sub LeftRampColl_Hit()
	Set SoundBall = ActiveBall
	PlaySound "plasticrolling", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub StopsoundRamp_Hit()
	StopSound "plasticrolling"
End Sub

Sub StopsoundRamp1_Hit()
	StopSound "plasticrolling"
End Sub


'****************************
'     Realtime Updates
' called by the MotorCallBack
'****************************

Sub RealTimeUpdates
    'flippers
    FlipperLeftP.RotY = FlipperLeft.CurrentAngle
    FlipperRightP.RotY = FlipperRight.CurrentAngle		
    ToiletSeat.RotX = ToiletFlipper.CurrentAngle
    Cistern.RotX = CisternFlipper.CurrentAngle
    MrHankey.TransY = MrHankeyFlipper.CurrentAngle
    ' rolling sound
    RollingSoundUpdate
    'Gates
	GateUpdate
End Sub




'******************************
' Diverse Collection Hit Sounds
'******************************


Sub Metals_Medium_Hit(idx)
    PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit(idx)
    PlaySound "metalhit2", 0, 0.5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit(idx)
    PlaySound "gate", 0, 0.9, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Hit(idx)
    PlaySound "gate4", 0, 0.7, 0, 0.25
End Sub

Sub Rubbers_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

Sub Posts_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 16 then
        PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End if
    If finalspeed >= 6 AND finalspeed <= 16 then
        RandomSoundRubber()
    End If
End Sub

Sub RandomSoundRubber()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "rubber_hit_1", 0, 0.5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "rubber_hit_2", 0, 0.5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "rubber_hit_3", 0, 0.5, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub

Sub FlipperLeft_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub FlipperRight_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "flip_hit_1", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "flip_hit_2", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "flip_hit_3", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub

Sub RandomSoundCartman()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "CartmanHole1", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "CartmanHole2", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "CartmanHole4", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub

Sub RandomSoundKenny()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "KennyHole1", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 2:PlaySound "KennyHole2", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
        Case 3:PlaySound "KennyHole3", 0, 0.6, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
    End Select
End Sub

Sub RandomSoundKennyMove()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "KennyMove1", 0, 0.2, 0, 0, 0, 1, 0
        Case 2:PlaySound "KennyMove2", 0, 0.2, 0, 0, 0, 1, 0
        Case 3:PlaySound "KennyMove3", 0, 0.2, 0, 0, 0, 1, 0
    End Select
End Sub