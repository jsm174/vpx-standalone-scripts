'*****************************************************************************************************
'
'							  Gladiators Premier Gattlieb 1993 VPX v1.1.1
'								http://www.ipdb.org/machine.cgi?id=1011
'
'										   Created by Kiwi
'
'*****************************************************************************************************

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'******************************************** OPTIONS **********************************************

'****************************************** Volume Settings **********************************

Const RolVol = 1	'Ball Rolling
Const MroVol = 1	'Wire Ramps Rolling
Const ProVol = 1	'Plastic Ramps Rolling
Const ColVol = 1	'Ball Collision
Const DroVol = 1	'Ball drop (3)
Const RubVol = 1	'Rubbers Collision
Const MhiVol = 1	'Metals Hit
Const NudVol = 1	'Nudge
Const TrfVol = 1	'Trough fire
Const PlpVol = 1	'Plunger pull
Const PlfVol = 1	'Plunger fire
Const SliVol = 1	'Kicking Targets (2)
Const BumVol = 1	'Bumpers (3)
Const SwiVol = 1	'Rollovers (6)
Const TarVol = 1	'Targets (2)
Const DtaVol = 1	'Droptargets (3)
Const DtrVol = 1	'Droptarget reset
Const GatVol = 1	'Gates (5)
Const SpiVol = 1	'Spinner
Const KicVol = 0.5	'Kicker catch
Const KieVol = 1	'Kicker eject
Const KidVol = 1	'Kicker Drain
Const FluVol = 0.6	'Flippers up
Const FldVol = 0.6	'Flippers down
Const KnoVol = 1	'Knocker
Const PgcVol = 1	'Plunger Gate Catapult
Const MocVol = 0.1	'Motor Catapult
Const ShaVol = 0.3	'Shaker

'************************ ROM

Const cGameName = "gladiatr"

'************************ Ball: 50 unit is standard ball size ** Mass=(50^3)/125000 ,(BallSize^3)/125000

Const BallSize = 50

Const BallMass = 1.07

'************************ Max ball momentum ** Momentum limiter: 0 off, 1 on.

Const Maxmom = 400

Const MomOn = 0

'************************ Ball Shadow: 0 hidden , 1 visible

Const BallSHW = 1

'************************ CabRails and rail lights Hidden/Visible in FS mode: 0 hidden , 1 visible

Const RailsLights = 0

'************************ Flashers Intensity

Const Lumen = 10

'************************ Color Grading LUT: 1 = Active, any other value = disabled

Const LutEnabled = 1

'************************ DMD: 0 for VPinMAME DMD visible in DT mode, 1 for Text DMD visible in DT mode

Const VPMorTextDMD = 1

'************************ 0 Backglass on in FSS, 1 Backglass on and Backdrop off in DT, 2 Backglass on in FS

Const BackG = 0

'************************ Vel Catapult

Const Moltiplicatore = 3

'************************ Ombre fori PF

Dim o
For each o in aOmbre
	o.IntensityScale = o.IntensityScale * 0.95
	o.Height = -24
Next

'******************************************** OPTIONS END **********************************************

Dim ModePlay, VarHidden, UseVPMColoredDMD
ModePlay = Table1.ShowDT + Table1.ShowFSS - BackG

'******************************************** DMD

 If ModePlay < 0 Then
    UseVPMColoredDMD = VPMorTextDMD
    VarHidden = VPMorTextDMD
Else
    UseVPMColoredDMD = 0
    VarHidden = 0		'Put 1 if you whant DMD Hidden in FS mode
    TextBoxDMD.Visible = 0
End If

' If B2SOn = True Then VarHidden = 1

'******************************************** FSS Init

 If ModePlay = -2 Then
	TextBoxDMD.Visible = False
End If

 If ModePlay > -2 Then
	FlasherDMD.Visible = False
	l112.Visible = 0
	l113.Visible = 0
	l114.Visible = 0
	l115.Visible = 0
	l116.Visible = 0
	l117.Visible = 0

	l184.Visible = 0
	l184a.Visible = 0
	l182.Visible = 0
	l182a.Visible = 0
	l188.Visible = 0
	l188a.Visible = 0
	l189.Visible = 0
	l189a.Visible = 0

	lb1.Visible = 0
	lb2.Visible = 0
	lb3.Visible = 0
	lb4.Visible = 0
	lb5.Visible = 0
	lb6.Visible = 0
	lb7.Visible = 0
	lb8.Visible = 0
	lb9.Visible = 0
	lb10.Visible = 0
	lb11.Visible = 0
	lb12.Visible = 0
	lb13.Visible = 0
	lb14.Visible = 0
	lb15.Visible = 0
	lb16.Visible = 0
	lb17.Visible = 0
	lb18.Visible = 0
	lb19.Visible = 0
	lb20.Visible = 0
	lb21.Visible = 0
	lb22.Visible = 0
	lb23.Visible = 0
	lb24.Visible = 0
	lb25.Visible = 0
	lb26.Visible = 0
	lb27.Visible = 0
	lb28.Visible = 0
	lb29.Visible = 0
	lb30.Visible = 0
	lb31.Visible = 0
	lb32.Visible = 0
	lb33.Visible = 0
	lb34.Visible = 0
	lb35.Visible = 0
	lb36.Visible = 0
	lb37.Visible = 0
	lb38.Visible = 0
	lb39.Visible = 0
	lb40.Visible = 0
	lb41.Visible = 0
	lb42.Visible = 0
	lb43.Visible = 0
	lb44.Visible = 0
End If

'********************************************

LoadVPM "01560000", "gts3.VBS", 3.26

'Set Controller = CreateObject("b2s.server")

Dim bsTrough, bsTP, bsLP, bsCP, bsRP, dtBank, mRMotor, x, FxSDrain
FxSDrain = 0

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 0
Const HandleMech = 0

' Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = "coin3"


Const swLCoin = 0
Const swRCoin = 1
Const swCCoin = 2
Const swCoinShuttle = 3
Const swStartButton = 4
Const swTournament = 5
Const swFrontDoor = 6

'************
' Table init.
'************

Sub Table1_Init
	vpmInit me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Gladiators Premier (Gottlieb 1993)" & vbNewLine & "VPX table by Kiwi 1.1.1"
		.HandleMechanics = 0
		.HandleKeyboard = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.ShowTitle = 0
		.Hidden = VarHidden
'		.Games(cGameName).Settings.Value("dmd_pos_x")=0
'		.Games(cGameName).Settings.Value("dmd_pos_y")=0
'		.Games(cGameName).Settings.Value("dmd_width")=400
'		.Games(cGameName).Settings.Value("dmd_height")=92
'		.Games(cGameName).Settings.Value("rol") = 0
'		.Games(cGameName).Settings.Value("sound") = 1
'		.Games(cGameName).Settings.Value("ddraw") = 1
		.Games(cGameName).Settings.Value("dmd_red")=255
		.Games(cGameName).Settings.Value("dmd_green")=20
		.Games(cGameName).Settings.Value("dmd_blue")=20
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With

	' Nudging
	vpmNudge.TiltSwitch = 151
	vpmNudge.Sensitivity = 2
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingShot, RightSlingShot)

	' Trough
	Set bsTrough = New cvpmBallStack
	With bsTrough
		.InitSw 23, 0, 33, 0, 0, 0, 0, 0
		.InitKick BallRelease, 67, 5
'		.InitEntrySnd "Solenoid", "Solenoid"
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		.Balls = 4
	End With

	' Top Eject Hole
	Set bsTP = New cvpmBallStack
	With bsTP
		.InitSaucer sw21, 21, 150, 20
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		.KickAngleVar = 2
		.KickForceVar = 2
		.KickZ = 1.56
	End With

	' Left Eject Hole
	Set bsLP = New cvpmBallStack
	With bsLP
		.InitSaucer sw31, 31, 105, 14
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		.KickAngleVar = 2
		.KickForceVar = 2
		.KickZ = 1
	End With

	' Center Eject Hole (VUK)
	Set bsCP = New cvpmBallStack
	With bsCP
		.InitSaucer sw22, 22, 0, 38
		.KickZ = 1.56
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
	End With

	' Right Eject Hole
	Set bsRP = New cvpmBallStack
	With bsRP
		.InitSaucer sw32, 32, 182, 24
'		.InitExitSnd SoundFX("popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
		.KickAngleVar = 0.5
		.KickForceVar = 2
		.KickZ = 1.56
	End With

	' Drop targets
	set dtBank = new cvpmdroptarget
	With dtBank
		.initdrop array(sw15, sw25, sw35), array(15, 25, 35)
	End With

	' RMotor mech
    Set mRMotor = New cvpmMech
    With mRMotor
        .MType = vpmMechOneSol + vpmMechReverse + vpmMechNonLinear
        .Sol1 = 23
        .Length = 34 * Moltiplicatore
        .Steps = 136
        .Acc = 10
        .Ret = 1
        .AddSw 30, 0, 0
        .Callback = GetRef("UpdateRMotor")
        .Start
    End With

	' Init Ramp Motor
	For each x in MotRamp:x.Collidable = 0:Next

' Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

' Init Targets Walls
	LeftSling.IsDropped=1:LeftSling1.IsDropped=1:LeftSling2.IsDropped=1
	RightSling.IsDropped=1:RightSling1.IsDropped=1:RightSling2.IsDropped=1

' GI Delay Timer
	GIDelay.Enabled = 1

' Init drain
     DrainInit.CreateBall:DrainInit.Kick 180, 1:DrainInit.TimerEnabled = 1

	If ModePlay < 0 Then
		RailSx.visible=1
		RailDx.visible=1
		'Trim.visible=1
		TrimS1.visible=1
		TrimS2.visible=1
		TrimS3.visible=1
		TrimS4.visible=1
		f14.visible=0
		f14a.visible=1
		f14b.visible=1
		f15a.visible=1
		f17a.visible=1
	Else
		RailSx.visible=RailsLights
		RailDx.visible=RailsLights
		'Trim.visible=RailsLights
		TrimS1.visible=RailsLights
		TrimS2.visible=RailsLights
		TrimS3.visible=RailsLights
		TrimS4.visible=RailsLights
		f14.visible= ABS(ABS(RailsLights) -1)
		f14a.visible=RailsLights
		f14b.visible=RailsLights
		f15a.visible=RailsLights
		f17a.visible=RailsLights
	End If

' Backbox Flashers Y Position

Const Fpy = 16.2

l112.y = Fpy
l113.y = Fpy
l114.y = Fpy
l115.y = Fpy
l116.y = Fpy
l117.y = Fpy

l184.y = Fpy
l184a.y = Fpy
l182.y = Fpy
l182a.y = Fpy
l188.y = Fpy
l188a.y = Fpy
l189.y = Fpy
l189a.y = Fpy

lb1.y = Fpy
lb2.y = Fpy
lb3.y = Fpy
lb4.y = Fpy
lb5.y = Fpy
lb6.y = Fpy
lb7.y = Fpy
lb8.y = Fpy
lb9.y = Fpy
lb10.y = Fpy
lb11.y = Fpy
lb12.y = Fpy
lb13.y = Fpy
lb14.y = Fpy
lb15.y = Fpy
lb16.y = Fpy
lb17.y = Fpy
lb18.y = Fpy
lb19.y = Fpy
lb20.y = Fpy
lb21.y = Fpy
lb22.y = Fpy
lb23.y = Fpy
lb24.y = Fpy
lb25.y = Fpy
lb26.y = Fpy
lb27.y = Fpy
lb28.y = Fpy
lb29.y = Fpy
lb30.y = Fpy
lb31.y = Fpy
lb32.y = Fpy
lb33.y = Fpy
lb34.y = Fpy
lb35.y = Fpy
lb36.y = Fpy
lb37.y = Fpy
lb38.y = Fpy
lb39.y = Fpy
lb40.y = Fpy
lb41.y = Fpy
lb42.y = Fpy
lb43.y = Fpy
lb44.y = Fpy

	TextLUT.Visible = 0
	TextLUT.Text = Table1.ColorGradeImage
	LoadLut
	TextBoxDMD.Width=Int(TextBoxDMD.Width * (4/WindowWidth)*(WindowHeight/3))
'	TextBoxDMD.Width=300	' 128 x 32 = 4 x 1    320 x 80
'	TextBoxDMD.Width=250	' DMD 16-10 X * 0,83  266 x 80
'	TextBoxDMD.Width=225	' DMD 16-9  X * 0,75  240 x 80

End Sub

Sub Table1_Exit():Controller.Stop:End Sub
Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub

' GI Init
Sub GIDelay_Timer()
	SetLamp 150, 1
	SetLamp 151, 1
	SetLamp 156, 1
	GIDelay.Enabled = 0
	mr.Collidable = True
End Sub

Sub DrainInit_Timer()
	FxSDrain = 1
	RollingTimer.Enabled = 1
	DrainInit.TimerEnabled = 0
End Sub

'**********
' Keys
'**********

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode = LeftFlipperKey Then Controller.Switch(82) = 1:BumpersDisable
	If KeyCode = RightFlipperKey Then Controller.Switch(83) = 1:BumpersDisable
	If keycode = LeftMagnaSave Then bLutActive = True:TextLUT.Visible = 1
	If keycode = RightMagnaSave Then
	If bLutActive And LutEnabled = 1 Then NextLUT: End If
	End If
	If keycode = PlungerKey Then Plunger.Pullback:PlaySoundAtVol "fx_plungerpull", Plunger, PlpVol
	If keycode = LeftTiltKey Then Nudge 90, 4:PlaySound SoundFX("fx_nudge",0), 0, NudVol, -0.1, 0.25
	If keycode = RightTiltKey Then Nudge 270, 4:PlaySound SoundFX("fx_nudge",0), 0, NudVol, 0.1, 0.25
	If keycode = CenterTiltKey Then Nudge 0, 5:PlaySound SoundFX("fx_nudge",0), 0, NudVol, 0, 0.25
	If vpmKeyDown(keycode) Then Exit Sub
    'debug key
	If KeyCode = "3" And Sw15.IsDropped = 1 And Sw25.IsDropped = 1 And Sw35.IsDropped = 1 Then
	Sw15.IsDropped = 0:Sw25.IsDropped = 0:Sw35.IsDropped = 0:dtbank.DropSol_On
End If
'    If KeyCode = "3" Then
'        SetLamp 190, 1
'        SetLamp 191, 1
'        SetLamp 194, 1
'        SetLamp 195, 1
'        SetLamp 196, 1
'        SetLamp 197, 1
'        SetLamp 199, 1
'    End If
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyCode = LeftFlipperKey Then Controller.Switch(82) = 0:BumpersDisable
	If KeyCode = RightFlipperKey Then Controller.Switch(83) = 0:BumpersDisable
	If keycode = LeftMagnaSave Then bLutActive = False:TextLUT.Visible = 0
	If keycode = PlungerKey Then Plunger.Fire:PlaySoundAtVol "plunger2", Plunger, PlfVol * (Plunger.Position/25)
	If vpmKeyUp(keycode) Then Exit Sub
    'debug key
'    If KeyCode = "3" Then
'        SetLamp 190, 0
'        SetLamp 191, 0
'        SetLamp 194, 0
'        SetLamp 195, 0
'        SetLamp 196, 0
'        SetLamp 197, 0
'        SetLamp 199, 0
'    End If
End Sub

Sub BumpersDisable
 If Controller.Switch(82) And Controller.Switch(83) Then
	Bumper1.HasHitEvent=0
	Bumper2.HasHitEvent=0
	Bumper3.HasHitEvent=0
Else
	Bumper1.HasHitEvent=1
	Bumper2.HasHitEvent=1
	Bumper3.HasHitEvent=1
End If
End Sub

' Update RMotor

Dim RampDir

Sub UpdateRMotor(aNewPos, aSpeed, aLastPos)
	RampDir = aNewPos / 4
	RMotor.RotY = RampDir - 4
	MotRamp(aLastPos\4).Collidable = False
	MotRamp(aNewPos\4).Collidable = True
 If aSpeed > 0 Then
	PlaySoundAtVolLoop SoundFX("MotorNoise",DOFGear), RMotor, MocVol
Else
	StopSound "MotorNoise"
End If
End Sub

'*********
'   LUT
'*********

Dim bLutActive, LUTImage

Sub LoadLUT
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage +1) MOD 9:UpdateLUT:SaveLUT:End Sub

Sub UpdateLUT
Select Case LutImage
Case 0:Table1.ColorGradeImage = "LUT0"
Case 1:Table1.ColorGradeImage = "LUT1"
Case 2:Table1.ColorGradeImage = "LUT2"
Case 3:Table1.ColorGradeImage = "LUT3"
Case 4:Table1.ColorGradeImage = "LUT4"
Case 5:Table1.ColorGradeImage = "LUT5"
Case 6:Table1.ColorGradeImage = "LUT6"
Case 7:Table1.ColorGradeImage = "LUT7"
Case 8:Table1.ColorGradeImage = "LUT8"
End Select
TextLUT.Text = Table1.ColorGradeImage
End Sub

'*********
' Switches
'*********

' Slings
Dim LStep, RStep, KickingSxFire, KickingDxFire

Sub LeftSlingshot_Slingshot:vpmTimer.PulseSw 13:TimerKSx.Enabled=0:LeftKicking.RotY = 10:LeftSlingshot.IsDropped=1:LeftSling.IsDropped=0:LStep=0:Me.TimerEnabled=1:PlaySoundAtVol SoundFX("bumper5",DOFContactors), LeftKicking, SliVol:KickingSxFire = 1:End Sub
Sub LeftSlingshot_Timer
	Select Case LStep
		Case 0:LeftSling.IsDropped = 0:LeftKicking.RotY = 10
		Case 1: 'pause
		Case 2:LeftSling.IsDropped = 1:LeftSling1.IsDropped = 0:LeftKicking.RotY = 7
		Case 3:LeftSling1.IsDropped = 1:LeftSling2.IsDropped = 0:LeftKicking.RotY = 4
		Case 4:LeftSling2.IsDropped = 1:LeftSlingshot.IsDropped=0:Me.TimerEnabled = 0:TimerKSx.Enabled=1:KickingSxFire = 0:LeftKicking.RotY = 0
	End Select
	LStep = LStep + 1
End Sub

Sub TimerKSx_Timer()
 If KickingSxFire = 0 Then
	LeftKicking.RotY = -(-1+(0.16*LeftKickingServo.CurrentAngle))
End If
End Sub 

Sub RightSlingshot_Slingshot:vpmTimer.PulseSw 14:TimerKDx.Enabled=0:RightKicking.RotY = 10:RightSlingshot.IsDropped=1:RightSling.IsDropped=0:RStep=0:Me.TimerEnabled=1:PlaySoundAtVol SoundFX("bumper5",DOFContactors), RightKicking, SliVol:KickingDxFire = 1:End Sub
Sub RightSlingshot_Timer
	Select Case RStep
		Case 0:RightSling.IsDropped = 0:RightKicking.RotY = 10
		Case 1: 'pause
		Case 2:RightSling.IsDropped = 1:RightSling1.IsDropped = 0:RightKicking.RotY = 7
		Case 3:RightSling1.IsDropped = 1:RightSling2.IsDropped = 0:RightKicking.RotY = 4
		Case 4:RightSling2.IsDropped = 1:RightSlingshot.IsDropped=0:Me.TimerEnabled = 0:TimerKDx.Enabled=1:KickingDxFire = 0:RightKicking.RotY = 0
	End Select
	RStep = RStep + 1
End Sub

Sub TimerKDx_Timer()
 If KickingDxFire = 0 Then
	RightKicking.RotY = -(-1+(0.16*RightKickingServo.CurrentAngle))
End If
End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 10:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper1, BumVol:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 11:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper2, BumVol:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 12:PlaySoundAtVol SoundFX("jet1",DOFContactors), Bumper3, BumVol:End Sub

' Spinner
Sub sw20_Spin:vpmTimer.PulseSw 20:PlaySoundAtVol "spinner", sw20, SpiVol:End Sub

' Eject holes
Sub Drain_Hit
	bsTrough.AddBall Me
 If FxSDrain = 1 Then:PlaysoundAtVol "drain1a", Drain, KidVol:End If
End Sub
Sub sw21_Hit:PlaysoundAtVol "fx_kicker_enter1", sw21, KicVol:bsTP.AddBall 0:End Sub
Sub sw31_Hit:PlaysoundAtVol "fx_kicker_enter1", sw31, KicVol:bsLP.AddBall 0:End Sub
Sub sw22_Hit:PlaysoundAtVol "fx_kicker_enter1", sw22, KicVol:bsCP.AddBall 0:sw22Wire.RotX = 0:End Sub	'VUK
Sub sw32_Hit:PlaysoundAtVol "fx_kicker_enter1", sw32, KicVol:bsRP.AddBall 0:sw32Wire.RotX = 0:End Sub

' Rollovers
Sub sw24_Hit:Controller.Switch(24) = 1:Switch24wire.RotX=-15:PlaySoundAtVol "sensor", sw24, SwiVol:End Sub
Sub sw24_UnHit:Controller.Switch(24) = 0:Switch24wire.RotX=0:End Sub
Sub sw80_Hit:Controller.Switch(80) = 1:End Sub
Sub sw80_UnHit:Controller.Switch(80) = 0:End Sub
Sub sw81_Hit:Controller.Switch(81) = 1:End Sub
Sub sw81_UnHit:Controller.Switch(81) = 0:End Sub
Sub sw94_Hit:Controller.Switch(94) = 1:PlaySoundAtVol "sensor", sw94, SwiVol:End Sub
Sub sw94_UnHit:Controller.Switch(94) = 0:End Sub
Sub sw100_Hit:Controller.Switch(100) = 1:End Sub
Sub sw100_UnHit:Controller.Switch(100) = 0:End Sub
Sub sw92_Hit:Controller.Switch(92) = 1:sw92wire.RotX=15:PlaySoundAtVol "sensor", sw92, SwiVol:End Sub
Sub sw92_UnHit:Controller.Switch(92) = 0:sw92wire.RotX=0:End Sub
Sub sw102_Hit:Controller.Switch(102) = 1:PlaySoundAtVol "sensor", sw102, SwiVol:End Sub
Sub sw102_UnHit:Controller.Switch(102) = 0:End Sub
Sub sw93_Hit:Controller.Switch(93) = 1:sw93wire.RotX=15:PlaySoundAtVol "sensor", sw93, SwiVol:End Sub
Sub sw93_UnHit:Controller.Switch(93) = 0:sw93wire.RotX=0:End Sub
Sub sw103_Hit:Controller.Switch(103) = 1:PlaySoundAtVol "sensor", sw103, SwiVol:End Sub
Sub sw103_UnHit:Controller.Switch(103) = 0:End Sub

' Ramp sensors
Sub Gate90_Hit():vpmTimer.PulseSw 90:End Sub
'Sub sw90_Hit():Controller.Switch(90) = 1:End Sub
'Sub sw90_UnHit():Controller.Switch(90) = 0:End Sub
Sub sw91_Hit():Controller.Switch(91) = 1:End Sub
Sub sw91_UnHit():Controller.Switch(91) = 0:End Sub
Sub sw101_Hit():Controller.Switch(101) = 1:End Sub
Sub sw101_UnHit():Controller.Switch(101) = 0:End Sub

' Droptargets
Sub sw15_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw15, DtaVol:End Sub
Sub sw15_Dropped:dtBank.hit 1
	Bulb8T15.Visible=1
End Sub

Sub sw25_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw25, DtaVol:End Sub
Sub sw25_Dropped:dtBank.hit 2
	Bulb8T25.Visible=1
End Sub

Sub sw35_Hit:PlaySoundAtVol SoundFX("DROPTARG",DOFDropTargets), sw35, DtaVol:End Sub
Sub sw35_Dropped:dtBank.hit 3
	Bulb8T35.Visible=1
End Sub

' Targets
Sub sw95_Hit:vpmTimer.PulseSw 95:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub
Sub sw104_Hit:vpmTimer.PulseSw 104:PlaySoundAtBallVol SoundFX("target",DOFTargets), TarVol:End Sub

' Fx Sounds
Sub Gate_Hit():PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub Gate2_Hit():PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub Gate3a_Hit():PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub GateInSx2_Hit()PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub GateInDx2_Hit()PlaySoundAtBallVol "Gate51", GatVol:End Sub
Sub Perno_Hit():PlaySoundAtBallVol "WireHit", MhiVol:End Sub
Sub CatapStop_Hit():PlaySoundAtBallVol "WireHit", MhiVol:End Sub
Sub FlapStop_Hit():PlaySoundAtBallVol "WireHit", MhiVol:End Sub
Sub FlapStop1_Hit():PlaySoundAtBallVol "WireHit", MhiVol:End Sub
Sub FlapStopTop_Hit():PlaySoundAtBallVol "WireHit", MhiVol:End Sub
Sub Fx1_Hit:PlaySoundAtBallVol "fx_InMetalrolling", MhiVol:End Sub
Sub Fx2_Hit:PlaySoundAtBallVol "fx_InMetalrolling", MhiVol:End Sub
Sub Fx3_Hit:PlaySoundAtBallVol "fx_InMetalrolling", MhiVol:End Sub

'*********
'Solenoids
'*********

SolCallback(6)  = "SolBallReleaseLP"
SolCallback(7)  = "SolBallReleaseTP"
SolCallback(8)  = "SetLamp 188,"
SolCallback(9)  = "SetLamp 189,"
SolCallback(10) = "dtcbank"
SolCallback(11) = "SolBallReleaseCP"
SolCallback(12) = "SolBallReleaseRP"
SolCallback(13) = "PlungerGate"
SolCallback(14) = "SetLamp 194,"
SolCallback(15) = "SetLamp 195,"
SolCallback(16) = "SetLamp 196,"
SolCallback(17) = "SetLamp 197,"
SolCallback(18) = "SetLamp 198,"
SolCallback(19) = "SetLamp 199,"
SolCallback(20) = "SetLamp 190,"
SolCallback(21) = "SetLamp 191,"
SolCallback(22) = "SetLamp 182,"
'SolCallback(23) = "RampMotorRelay"
SolCallback(24) = "SetLamp 184,"
SolCallback(25) = "MShaker"
SolCallback(26) = "GIBackBox"
SolCallback(28) = "bsTroughSolOut"	'"bsTrough.SolOut"
SolCallback(29) = "bsTrough.SolIn"
SolCallback(30) = "KnockerSound"
SolCallback(31) = "GIRelay"
Solcallback(32) = "SolRun"

Sub bsTroughSolOut(Enabled)
 If Enabled Then
	bsTrough.ExitSol_On
	PlaySoundAtVol SoundFX("popper",DOFContactors), BallRelease, TrfVol
End If
End Sub

Sub PlungerGate(Enabled)
 If Enabled Then
	Perno.IsDropped=1
	ServoPerno.Open=1
	PlaySoundAtVol SoundFX("fx_solenoidon",DOFContactors), PernoP1, PgcVol
Else
	Perno.TimerEnabled=1
End If
End Sub

Sub Perno_Timer
	Perno.IsDropped=0
	Perno.TimerEnabled=0
	ServoPerno.Open=0
	PlaySoundAtVol SoundFX("fx_solenoidoff",DOFContactors), PernoP1, PgcVol
End Sub

Sub dtcbank(Enabled)
 If Enabled Then
	dtbank.DropSol_On
	PlaySoundAtVol SoundFX("DTResetB",DOFContactors), sw25, DtrVol
End If
	Bulb8T15.TimerEnabled=1
End Sub

Sub Bulb8T15_Timer()
	Bulb8T15.Visible=0
	Bulb8T25.Visible=0
	Bulb8T35.Visible=0
	Bulb8T15.TimerEnabled=0
End Sub

Sub SolBallReleaseLP(Enabled)
 If Enabled Then
	bsLP.ExitSol_On
	EjectTimer.Enabled=1
	EjectArm.RotY = -20
	PlaySoundAtVol SoundFX("popper",DOFContactors), sw31, KieVol
End If
End Sub

Sub EjectTimer_Timer
	EjectArm.RotY = EjectArm.RotY +2
 If EjectArm.RotY = 0 Then:EjectTimer.Enabled = 0
End Sub

Sub SolBallReleaseTP(Enabled)
 If Enabled Then
	bsTP.ExitSol_On
	PlaySoundAtVol SoundFX("popper",DOFContactors), sw21, KieVol
End If
End Sub

Sub SolBallReleaseCP(Enabled)
 If Enabled Then
	bsCP.ExitSol_On
	sw22Wire.RotX = -10
	EjectCP.Z = -31
	EjectCPT.Enabled=1
	PlaySoundAtVol SoundFX("popper",DOFContactors), sw22, KieVol
End If
End Sub

Sub EjectCPT_Timer
	sw22Wire.RotX = -12
	EjectCP.Z = -71
	EjectCPT.Enabled=0
End Sub

Sub SolBallReleaseRP(Enabled)
 If Enabled Then
	bsRP.ExitSol_On
	sw32Wire.RotX = -10
	EjectRP.Z = -31
	EjectRPT.Enabled=1
	PlaySoundAtVol SoundFX("popper",DOFContactors), sw32, KieVol
End If
End Sub

Sub EjectRPT_Timer
	sw32Wire.RotX = -12
	EjectRP.Z = -71
	EjectRPT.Enabled=0
End Sub

Sub MShaker(Enabled)
	PlaySoundAtVol SoundFX("ShakerSoftPulsing",DOFShaker), l50, ShaVol
End Sub

Sub KnockerSound(Enabled)
 If Enabled Then
	PlaySoundAtVol SoundFX("Knocker",DOFKnocker), l30, KnoVol
End If
End Sub

'**************
' GI
'**************

Sub GIRelay(Enabled)
	Dim GIoffon
	GIoffon = ABS(ABS(Enabled) -1)
	SetLamp 150, GIoffon
	SetLamp 151, GIoffon
End Sub

Sub GIBackBox(Enabled)
	Dim GIBBoffon
	GIBBoffon = ABS(ABS(Enabled) -1)
	SetLamp 156, GIBBoffon
End Sub

Sub SolRun(Enabled)
	vpmNudge.SolGameOn Enabled
 If Enabled Then
'	PinPlay=1
Else
'	PinPlay=0
'	Bumper1.HasHitEvent = ABS(Enabled)
'	Bumper2.HasHitEvent = ABS(Enabled)
'	Bumper3.HasHitEvent = ABS(Enabled)
'	LeftSlingshot.Disabled = ABS(Enabled)
'	RightSlingShot.Disabled = ABS(Enabled)
End If
End Sub

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
	If Enabled Then
		PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), LeftFlipper, FluVol:LeftFlipper.RotateToEnd:LeftFlipper1.RotateToEnd
	Else
		If LeftFlipper.CurrentAngle < LeftFlipper.StartAngle - 5 Then
			PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), LeftFlipper, FldVol
		End If	
        LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		PlaySoundAtVol SoundFX("flipperup1",DOFFlippers), RightFlipper, FluVol:RightFlipper.RotateToEnd:RightFlipper1.RotateToEnd
	Else
   		If RightFlipper.CurrentAngle > RightFlipper.StartAngle + 5 Then
			PlaySoundAtVol SoundFX("flipperdown1",DOFFlippers), RightFlipper, FldVol
		End If
		RightFlipper.RotateToStart:RightFlipper1.RotateToStart
	End If
End Sub

Sub LeftFlipper_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper1_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
	PlaySound "rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'**********************************
'       JP's VP10 Fading Lamps & Flashers v2
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200), DLSpeedUp(200), DLSpeedDown(200)

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

Sub UpdateLamps
	NFadeL 0, l0
	NFadeLm 2, l2
	FlashDL 2, 1, LEDR2
	NFadeLm 3, l3
	FlashDL 3, 1, LEDR3
	NFadeLm 4, l4
	FlashDL 4, 1, LEDR4
	NFadeLm 5, l5
	FlashDL 5, 1, LEDR5
	NFadeLm 6, l6
	FlashDL 6, 1, LEDR6
	NFadeLm 7, l7
	FlashDL 7, 1, LEDR7
	NFadeL 10, l10
	NFadeL 11, l11
	NFadeLm 12, l12
	FlashDL 12, 1, LEDR12
	NFadeLm 13, l13
	FlashDL 13, 1, LEDR13
	NFadeLm 14, l14
	FlashDL 14, 1, LEDR14
	NFadeLm 15, l15
	FlashDL 15, 1, LEDR15
	NFadeLm 16, l16
	FlashDL 16, 1, LEDR16
	NFadeLm 17, l17
	FlashDL 17, 1, LEDR17
	NFadeL 20, l20
	NFadeL 21, l21
	NFadeL 22, l22
	NFadeL 23, l23
	NFadeL 24, l24
	NFadeL 25, l25
	NFadeL 26, l26
	NFadeL 27, l27
	NFadeL 30, l30
	NFadeL 31, l31
	NFadeL 32, l32
	NFadeL 33, l33
	NFadeL 34, l34
	NFadeL 35, l35
	NFadeL 36, l36
	NFadeL 37, l37
	NFadeL 40, l40
	NFadeL 41, l41
	NFadeL 42, l42
	NFadeL 43, l43
	NFadeL 44, l44
	NFadeL 45, l45
	NFadeL 46, l46
	NFadeL 47, l47
	NFadeL 50, l50
	NFadeL 51, l51
	NFadeL 52, l52
	NFadeL 53, l53
	NFadeL 54, l54
	NFadeL 55, l55
	NFadeL 56, l56
	NFadeL 57, l57
	NFadeL 60, l60
	NFadeL 61, l61
	NFadeL 62, l62
	NFadeL 63, l63
	NFadeL 64, l64
	NFadeL 65, l65
	NFadeL 66, l66
	NFadeL 67, l67
	NFadeL 70, l70
	NFadeL 71, l71
	NFadeL 72, l72
	NFadeL 73, l73
	NFadeL 74, l74
	NFadeL 75, l75
	NFadeL 76, l76
	NFadeL 77, l77
	NFadeL 81, l81
	NFadeL 82, l82
	NFadeL 83, l83
	NFadeL 91, l91
	NFadeL 92, l92
	NFadeL 93, l93
	NFadeL 100, l100
	NFadeL 101, l101
	NFadeL 102, l102
	NFadeL 103, l103
	NFadeL 104, l104
	NFadeL 105, l105
	NFadeL 106, l106
	NFadeL 107, l107

'Flashers

	Flashm 84, f84
	FlashDL 84, 2, LEDR84
	Flashm 85, f85
	FlashDL 85, 2, LEDR85
	Flashm 86, f86
	FlashDL 86, 2, LEDR86
	Flashm 87, f87
	FlashDL 87, 2, LEDR87
	Flashm 94, f94
	FlashDL 94, 0.5, LEDR94
	Flashm 95, f95
	FlashDL 95, 0.5, LEDR95
	Flashm 96, f96
	FlashDL 96, 0.5, LEDR96
	Flashm 97, f97
	FlashDL 97, 0.5, LEDR97

	NFadeLm 150, Bulb1
	NFadeLm 150, Bulb1a
	NFadeLm 150, Bulb2
	NFadeLm 150, Bulb2a
	NFadeLm 150, Bulb3
	NFadeLm 150, Bulb3a
	NFadeLm 150, Bulb4
	NFadeLm 150, Bulb4a
	NFadeLm 150, Bulb5
	NFadeLm 150, Bulb5a
	NFadeLm 150, Bulb6
	NFadeLm 150, Bulb6a
	NFadeLm 150, Bulb7
	NFadeLm 150, Bulb7a
	NFadeLm 150, Bulb8
	NFadeLm 150, Bulb8a
	NFadeLm 150, Bulb8T15
	NFadeLm 150, Bulb8T25
	NFadeLm 150, Bulb8T35
	NFadeLm 150, Bulb9
	NFadeLm 150, Bulb9a
	NFadeLm 150, Bulb10
	NFadeLm 150, Bulb10a
	NFadeLm 150, Bulb11
	NFadeLm 150, Bulb12
	NFadeLm 150, Bulb13
	NFadeLm 150, Bulb13a
	NFadeLm 150, Bulb14
	NFadeLm 150, Bulb15
	NFadeLm 150, Bulb16
	NFadeLm 150, Bulb17
	NFadeLm 150, Bulb17a
	NFadeLm 150, Bulb18
	NFadeLm 150, Bulb18a
	NFadeLm 150, Bulb19
	NFadeLm 150, Bulb19a
	NFadeLm 150, LR1
	NFadeLm 150, LR2
	NFadeLm 150, LR3
	NFadeLm 150, Bulb8T15
	NFadeLm 150, Bulb8T25
	NFadeLm 150, Bulb8T35
	NFadeLm 150, gib1		' Bumper1
	NFadeLm 150, gib2		' Bumper2
	NFadeLm 150, gib3		' Bumper3
	FlashDL 150, 3, Dome1	' Bumper3

	Flashm 151, fgit1
	Flashm 151, fgit2
	Flashm 151, fgit3
	Flashm 151, fgit4
	Flashm 151, fgit5
	Flashm 151, fgit6
	Flashm 151, fgit7
	Flashm 151, fgit8
	Flashm 151, fgit9
	Flashm 151, fgit10

	Flashm 151, fgit1a
	Flashm 151, fgit2a
	Flashm 151, fgit3a
	Flashm 151, fgit4a
	Flashm 151, fgit8a
	Flashm 151, fgit10a
	Flashm 151, fgit12a
	Flashm 151, fgit13a
	Flashm 151, fgit14a
	Flashm 151, fgit17a
	Flashm 151, fgit18a
	Flash 151, fgit19a

	NFadeLm 194, fl14
	Flashm 194, f14
	Flashm 194, f14a
	Flashm 194, f14b
	FlashDL 194, 0.5, Flasher24B14
	NFadeLm 195, fl15
	Flashm 195, f15
	Flashm 195, f15a
	FlashDL 195, 0.5, Flasher24B15
	NFadeLm 196, fl16
	Flashm 196, f16
	FlashDL 196, 0.5, Flasher24B16
	NFadeLm 197, fl17
	Flashm 197, f17
	Flashm 197, f17a
	FlashDL 197, 0.5, Flasher24B17
	NFadeLm 198, l197
	NFadeL 198, l197a
	Flash 199, f19
	Flash 190, f20
	Flash 191, f21

' BackBox

	Flash 112, l112
	Flash 113, l113
	Flash 114, l114
	Flash 115, l115
	Flash 116, l116
	Flash 117, l117

	Flashm 182, l182
	Flash 182, l182a
	Flashm 184, l184
	Flash 184, l184a
	Flashm 188, l188
	Flash 188, l188a
	Flashm 189, l189
	Flash 189, l189a

	Flashm 156, lb1
	Flashm 156, lb2
	Flashm 156, lb3
	Flashm 156, lb4
	Flashm 156, lb5
	Flashm 156, lb6
	Flashm 156, lb7
	Flashm 156, lb8
	Flashm 156, lb9
	Flashm 156, lb10
	Flashm 156, lb11
	Flashm 156, lb12
	Flashm 156, lb13
	Flashm 156, lb14
	Flashm 156, lb15
	Flashm 156, lb16
	Flashm 156, lb17
	Flashm 156, lb18
	Flashm 156, lb19
	Flashm 156, lb20
	Flashm 156, lb21
	Flashm 156, lb22
	Flashm 156, lb23
	Flashm 156, lb24
	Flashm 156, lb25
	Flashm 156, lb26
	Flashm 156, lb27
	Flashm 156, lb28
	Flashm 156, lb29
	Flashm 156, lb30
	Flashm 156, lb31
	Flashm 156, lb32
	Flashm 156, lb33
	Flashm 156, lb34
	Flashm 156, lb35
	Flashm 156, lb36
	Flashm 156, lb37
	Flashm 156, lb38
	Flashm 156, lb39
	Flashm 156, lb40
	Flashm 156, lb41
	Flashm 156, lb42
	Flashm 156, lb43
	Flash 156, lb44

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.2   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.1 ' slower speed when turning off the flasher
        DLSpeedUp(x) = 0.4   	' faster speed when turning on the objects DisableLighting
        DLSpeedDown(x) = 0.2	' slower speed when turning off the objects DisableLighting
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
End Sub

Sub SetModLamp(nr, level)
	FlashLevel(nr) = level /150 'lights & flashers
End Sub

' Lights: old method, using 4 images

Sub FadeL(nr, light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:light.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:light.image = a:light.State = 1:FadingLevel(nr) = 1   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1           'wait
        Case 9:light.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1        'wait
        Case 13:light.image = d:Light.State = 0:FadingLevel(nr) = 0  'Off
    End Select
End Sub

Sub FadeLm(nr, light, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:light.image = b
        Case 5:light.image = a
        Case 9:light.image = c
        Case 13:light.image = d
    End Select
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

Sub LightMod(nr, object) ' modulated lights used as flashers
    Object.IntensityScale = FlashLevel(nr)
	Object.State = 1
End Sub

'Ramps & Primitives used as 4 step fading lights
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
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = Lumen*FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = Lumen*FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
    Object.IntensityScale = Lumen*FlashLevel(nr)
End Sub

Sub FlashMod(nr, object) 'sets the flashlevel from the SolModCallback
    Object.IntensityScale = Lumen*FlashLevel(nr)
End Sub

Sub FastFlash(nr, object)
    Select Case FadingLevel(nr)
		Case 4:object.Visible = 0:FadingLevel(nr) = 0 'off
		Case 5:object.Visible = 1:FadingLevel(nr) = 1 'on
		Object.IntensityScale = Lumen*FlashMax(nr)
    End Select
End Sub

' Objects DisableLighting

Sub FlashDL(nr, Limite, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - DLSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.BlendDisableLighting = Limite*FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + DLSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.BlendDisableLighting = Limite*FlashLevel(nr)
    End Select
End Sub

Sub FlashDLm(nr, Limite, object) 'multiple objects, it just sets the flashlevel
    Object.BlendDisableLighting = Limite*FlashLevel(nr)
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

Sub NFadeTm(nr, object, b)
    Select Case FadingLevel(nr)
        Case 4:object.Text = ""
        Case 5:object.Text = message
    End Select
End Sub

' *********************************************************************
' 					Wall, rubber and metal hit sounds
' *********************************************************************

Sub Rubbers_Hit(idx):PlaySoundAtBallVol "rubber1", RubVol:End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Table1.width-1
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
'PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, Vol) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, Vol, Pan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVolLoop(soundname, tableobj, Vol) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, -1, Vol, Pan(tableobj), 0, 0, 1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVolPitch(soundname, tableobj, Vol, Pitch) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, Vol, Pan(tableobj), 0, Pitch, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVol(soundname, VolMult) ' play a sound at the ball position, like rubbers, targets
    PlaySound soundname, 0, Vol(ActiveBall) * VolMult, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

'*******************************************
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'*******************************************

Const tnob = 19 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b, momfactorx, momfactory, momfactorz
    BOT = GetBalls
'	TextBox001.Text = (UBound(BOT))+1

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
		rolling(b) = False
		aBallShadow(b).Visible = 0
		StopSound("fx_ballrolling" & b)
		StopSound("fx_Rolling_Plastic" & b)
		StopSound("fx_Rolling_Metal" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)

		aBallShadow(b).X = BOT(b).X
		aBallShadow(b).Y = BOT(b).Y
		aBallShadow(b).Height = BOT(b).Z - (BallSize / 2)+1

        If BallVel(BOT(b)) > 1 Then
            rolling(b) = True

'Playfield
			If BOT(b).z < 30 Then
					StopSound("fx_Rolling_Metal" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*RolVol, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else

'Wire Ramps
'Catapulta
				If InRect(BOT(b).x, BOT(b).y, 218,209,286,209,465,575,190,655) Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'Centrale
			ElseIf InRect(BOT(b).x, BOT(b).y, 490,415,550,415,550,990,490,990) Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )
'Destra
			ElseIf InRect(BOT(b).x, BOT(b).y, 635,875,925,875,925,1265,776,1265) Then
					StopSound("fx_ballrolling" & b):StopSound("fx_Rolling_Plastic" & b)
					PlaySound("fx_Rolling_Metal" & b), -1, Vol(BOT(b) )*3*MroVol, Pan(BOT(b) ), 0, Pitch(BOT(b) )*0.1, 1, 0, AudioFade(BOT(b) )

'Plastic Ramps
			Else 
					StopSound("fx_Rolling_Metal" & b):StopSound("fx_ballrolling" & b)
					PlaySound("fx_Rolling_Plastic" & b), -1, Vol(BOT(b) )*3*ProVol, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) ) 
			End If
			End If

        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
				StopSound("fx_Rolling_Plastic" & b)
				StopSound("fx_Rolling_Metal" & b)
                rolling(b) = False
            End If
        End If

		' play ball drop sounds
		If InRect(BOT(b).x, BOT(b).y, 200,628,460,570,790,1350,400,1350) And BOT(b).VelZ < -8 And BOT(b).z < 50 And BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySound ("fx_ballhit" & b), 0, (ABS(BOT(b).velz)/17)*DroVol, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

		' Ball Shadow
		If BallSHW = 1 Then
			aBallShadow(b).Visible = 1
		Else
			aBallShadow(b).Visible = 0
		End If

        ' ball momentum control
        If BOT(b).AngMomX AND BOT(b).AngMomY AND BOT(b).AngMomZ <> 0 And MomOn = 1 Then
            momfactorx = ABS(Maxmom / BOT(b).AngMomX)
            momfactory = ABS(Maxmom / BOT(b).AngMomY)
            momfactorz = ABS(Maxmom / BOT(b).AngMomZ)
            If momfactorx < 1 And MomOn = 1 Then
                BOT(b).AngMomX = BOT(b).AngMomX * momfactorx
                BOT(b).AngMomY = BOT(b).AngMomY * momfactorx
                BOT(b).AngMomZ = BOT(b).AngMomZ * momfactorx
            End If
            If momfactory < 1 And MomOn = 1 Then
                BOT(b).AngMomX = BOT(b).AngMomX * momfactory
                BOT(b).AngMomY = BOT(b).AngMomY * momfactory
                BOT(b).AngMomZ = BOT(b).AngMomZ * momfactory
            End If
            If momfactorz < 1 And MomOn = 1 Then
                BOT(b).AngMomX = BOT(b).AngMomX * momfactorz
                BOT(b).AngMomY = BOT(b).AngMomY * momfactorz
                BOT(b).AngMomZ = BOT(b).AngMomZ * momfactorz
            End If
        End If

    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(ColVol*((velocity) ^2 / 200)), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'******************
' RealTime Updates
'******************
Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
	UpdateGates
	UpdateFlipperLogos
End Sub

Sub UpdateFlipperLogos
	FlipperSx.RotZ = LeftFlipper.CurrentAngle
	FlipperDx.RotZ = RightFlipper.CurrentAngle
	FlipperSx1.RotZ = LeftFlipper1.CurrentAngle
	FlipperDx1.RotZ = RightFlipper1.CurrentAngle
	PernoP1.TransZ = - ServoPerno.CurrentAngle
End Sub

Dim PI
PI = Round(4 * Atn(1), 6) '3.1415926535897932384626433832795

Sub UpdateGates
	GateInSx.RotX = - GateInSx1.CurrentAngle -23
	GateInDx.RotX = - GateInDx1.CurrentAngle -23
	Gate3.RotX = - Gate3a.CurrentAngle -23
	GateDx1.RotX = - (13+(0.9*Gate.CurrentAngle))
' If KickingSxFire = 0 Then
'	LeftKicking.RotY = -(-5+(0.15*LeftKickingServo.CurrentAngle))
'End If

'	LeftKicking.RotY = -(-8+(0.19*LeftKickingServo.CurrentAngle))
'	RightKicking.RotY = -(-8+(0.19*RightKickingServo.CurrentAngle))
	pSpinnerRod.TransX = sin( (sw20.CurrentAngle+180) * (PI/180)) * 8
	pSpinnerRod.TransZ = sin( (sw20.CurrentAngle- 90) * (PI/180)) * 8
	pSpinnerRod.RotY = sin( (sw20.CurrentAngle-180) * (PI/180)) * 6

End Sub
