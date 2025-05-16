'*************************************
'Wipe Out Premier 1993
'http://www.ipdb.org/machine.cgi?id=2799

'*************************************

	Option Explicit 
	Randomize

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 800    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolPi     = 1    ' Rubber pins volume.flipper
Const VolTarg   = 1    ' Targets volume.
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = .15  ' Flipper volume.
Const VolSling  = 1    ' Slingshot volume.
Const VolWire   = 1.3  ' Wireramp volume.
Const VolBump   = 4    ' Bumpers volume.
Const VolRH     = 1    ' Rubber volume.
Const VolPo     = 1    ' Posts volume.

'*******************************************************************************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'********************************************** OPTIONS ************************************************

Dim cGameName
cGameName= "wipeout"

'Const BallSize    = 50

'Const BallMass    = 1.025		'Mass=(53.11^3)/125000 ,(BallSize^3)/125000

'************ DMD

Const VPMorTextDMD = 1	'0 for VPinMAME DMD visible in DT mode, 1 for Text DMD visible in DT mode

Dim VarHidden, UseVPMColoredDMD
 If Table1.ShowDT = True Then
    UseVPMColoredDMD = VPMorTextDMD
    VarHidden = VPMorTextDMD
Else
    UseVPMColoredDMD = 0
    VarHidden = 0
    TextBoxDMD.Visible = 0
End If

 If B2SOn = True Then VarHidden = 1

'************ Rails and rail lights Hidden/Visible in FS mode : 0 hidden , 1 visible

Const RailsLights = 0

'************ Slingshot mode : 0 Walls , 1 Flippers

Const SlingM = 0

'************ Slingshot hit threshold, with flippers (parm)

Const ThSling = 3

'************ Slalom Options

Const SlalomTHD = 1.5

Const SlalomForce = 2.5

Const SidesStrength = 2500

'************

'******************************************** FSS Init

 If Table1.ShowFSS = True Then
	TextBoxDMD.Visible = False
End If

 If Table1.ShowFSS = False Then
	FlasherDMD.Visible = False
End If

'********************************************

BRubber.Threshold = SlalomTHD:BRubber.Force = SlalomForce
BRubberSX1.Threshold = SlalomTHD:BRubberSX1.Force = SlalomForce
BRubberSX2.Threshold = SlalomTHD:BRubberSX2.Force = SlalomForce
BRubberSX3.Threshold = SlalomTHD:BRubberSX3.Force = SlalomForce
BRubberSX4.Threshold = SlalomTHD:BRubberSX4.Force = SlalomForce
BRubberSX5.Threshold = SlalomTHD:BRubberSX5.Force = SlalomForce
BRubberSX6.Threshold = SlalomTHD:BRubberSX6.Force = SlalomForce
BRubberSX7.Threshold = SlalomTHD:BRubberSX7.Force = SlalomForce
BRubberSX8.Threshold = SlalomTHD:BRubberSX8.Force = SlalomForce
BRubberSX9.Threshold = SlalomTHD:BRubberSX9.Force = SlalomForce
BRubberSX10.Threshold = SlalomTHD:BRubberSX10.Force = SlalomForce
BRubberSX11.Threshold = SlalomTHD:BRubberSX11.Force = SlalomForce

BRubberDX1.Threshold = SlalomTHD:BRubberDX1.Force = SlalomForce
BRubberDX2.Threshold = SlalomTHD:BRubberDX2.Force = SlalomForce
BRubberDX3.Threshold = SlalomTHD:BRubberDX3.Force = SlalomForce
BRubberDX4.Threshold = SlalomTHD:BRubberDX4.Force = SlalomForce
BRubberDX5.Threshold = SlalomTHD:BRubberDX5.Force = SlalomForce
BRubberDX6.Threshold = SlalomTHD:BRubberDX6.Force = SlalomForce
BRubberDX7.Threshold = SlalomTHD:BRubberDX7.Force = SlalomForce
BRubberDX8.Threshold = SlalomTHD:BRubberDX8.Force = SlalomForce
BRubberDX9.Threshold = SlalomTHD:BRubberDX9.Force = SlalomForce
BRubberDX10.Threshold = SlalomTHD:BRubberDX10.Force = SlalomForce
BRubberDX11.Threshold = SlalomTHD:BRubberDX11.Force = SlalomForce

FlipperSxS.Strength = SidesStrength
FlipperDxS.Strength = SidesStrength

'******************************************** OPTIONS END **********************************************

LoadVPM "01210000","GTS3.VBS",3.10

'Set Controller = CreateObject("b2s.server")

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0

'Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SCoin = "coin3"

'Gottlieb switches
Const swLCoin = 0 
Const swRCoin = 1
Const swCCoin = 2
Const swCoinShuttle = 3
Const swStartButton = 4
Const swTournament = 5
Const swFrontDoor = 6
Const swDrop1 = 7

Dim bsAPlunger, bsTrough, bsLKick, bsRKick, bsUkick, dtDrop, RSlalomb, LSlalomb, mHole, PinPlay

'**************************************************************
'************   Table Init   **********************************
'**************************************************************

Sub Table1_Init
    With Controller
        .GameName = cGameName
        .SplashInfoLine = "Wipe Out 1.1.0 (Premier 1993)" & vbNewLine & "VPX table by Edizzle & Kiwi"
        .HandleMechanics = 0
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .Hidden = VarHidden
'		.Games(cGameName).Settings.Value("dmd_pos_x")=0
'		.Games(cGameName).Settings.Value("dmd_pos_y")=0
'		.Games(cGameName).Settings.Value("dmd_width")=256
'		.Games(cGameName).Settings.Value("dmd_height")=92
'		.Games(cGameName).Settings.Value("rol")=0
'		.Games(cGameName).Settings.Value("sound") = 1
'		.Games(cGameName).Settings.Value("ddraw") = 1
		.Games(cGameName).Settings.Value("dmd_red")=255
		.Games(cGameName).Settings.Value("dmd_green")=64
		.Games(cGameName).Settings.Value("dmd_blue")=0
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

' Nudging

    vpmNudge.TiltSwitch = 151
    vpmNudge.Sensitivity = 2
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingShot, RightSlingShot, RightTopSlingShot)

' Trough
       Set bsTrough = New cvpmBallStack
       With bsTrough
           .InitSw 24,0,0,34,0,0,0,0	
           .InitKick BallRelease, 63, 8
           .InitEntrySnd "Drain", "Solenoid"
           .InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
           .Balls = 3	
           .Addball 0
       End With 

' Lower Droptarget
       Set dtDrop = new cvpmDropTarget
       With dtDrop
			.InitDrop Array(sw7,sw17,sw27,sw37),Array(7,17,27,37)
'			.Initsnd "target", "resetdrop"
		End With

' Main Timer init

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	SlingFSx1.Enabled = SlingM
	SlingFSx2.Enabled = SlingM
	SlingFDx1.Enabled = SlingM
	SlingFDx2.Enabled = SlingM
	SlingFDx3.Enabled = SlingM
	SlingFDx4.Enabled = SlingM

	LeftSlingshot.IsDropped = SlingM
	RightSlingshot.IsDropped = SlingM
	RightTopSlingShot.IsDropped = SlingM

KSLIn1.Enabled= 0
KSLIn2.Enabled= 0
KSLIn3.Enabled= 0

BL2.IsDropped = 0
GateF1.RotateToStart


FlipperSxS.RotateToEnd
FlipperDxS.RotateToEnd

BRubberSX1.Collidable = 1
BRubberSX2.Collidable = 1
BRubberSX3.Collidable = 1
BRubberSX4.Collidable = 1
BRubberSX5.Collidable = 1
BRubberSX6.Collidable = 1
BRubberSX7.Collidable = 1
BRubberSX8.Collidable = 1
BRubberSX9.Collidable = 1
BRubberSX10.Collidable = 1
BRubberSX11.Collidable = 1

BRubberDX1.Collidable = 0
BRubberDX2.Collidable = 0
BRubberDX3.Collidable = 0
BRubberDX4.Collidable = 0
BRubberDX5.Collidable = 0
BRubberDX6.Collidable = 0
BRubberDX7.Collidable = 0
BRubberDX8.Collidable = 0
BRubberDX9.Collidable = 0
BRubberDX10.Collidable = 0
BRubberDX11.Collidable = 0

 If ShowDT=True Then
	RailSx.visible=1
	RailDx.visible=1
	'Trim.visible=1
	TrimS1.visible=1
	TrimS2.visible=1
	TrimS3.visible=1
	TrimS4.visible=1
Else
	RailSx.visible=RailsLights
	RailDx.visible=RailsLights
	'Trim.visible=RailsLights
	TrimS1.visible=RailsLights
	TrimS2.visible=RailsLights
	TrimS3.visible=RailsLights
	TrimS4.visible=RailsLights
End If

	f123.Y = 20
	f123a.Y = 20
	f123b.Y = 20

	f124.Y = 20
	f124a.Y = 20
	f124b.Y = 20

	fbb1.Y = 20
	fbb2.Y = 20
	fbb3.Y = 20
	fbb4.Y = 20
	fbb5.Y = 20
	fbb6.Y = 20
	fbb7.Y = 20
	fbb8.Y = 20
	fbb9.Y = 20
	fbb10.Y = 20
	fbb11.Y = 20
	fbb12.Y = 20
	fbb13.Y = 20
	fbb14.Y = 20
	fbb15.Y = 20
	fbb16.Y = 20
	fbb17.Y = 20
	fbb18.Y = 20
	fbb19.Y = 20
	fbb20.Y = 20
	fbb21.Y = 20
	fbb22.Y = 20
	fbb23.Y = 20
	fbb24.Y = 20
	fbb25.Y = 20
	fbb26.Y = 20
	fbb27.Y = 20
	fbb28.Y = 20
	fbb29.Y = 20
	fbb30.Y = 20
	fbb31.Y = 20
	fbb32.Y = 20
	fbb33.Y = 20
	fbb34.Y = 20
	fbb35.Y = 20
	fbb36.Y = 20
	fbb37.Y = 20
	fbb38.Y = 20
	fbb39.Y = 20
	fbb40.Y = 20
	fbb41.Y = 20

	GiL7T7.Visible=0
	GiL7T17.Visible=0
	GiL7T27.Visible=0
	GiL7T37.Visible=0
	GiL8T7.Visible=0
	GiL8T17.Visible=0
	GiL8T27.Visible=0
	GiL8T37.Visible=0

'****************   Init GI   ******************

	UpdateGI 0, 8
	SetLamp 166, 1

End Sub

Sub Table1_Exit():Controller.Stop:End Sub
Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub

'************************************************
'*******************Keys*************************
'************************************************
Sub Table1_KeyDown(ByVal keycode)
	If KeyCode = LeftFlipperKey Then Controller.Switch(81) = 1
	If KeyCode = RightFlipperKey Then Controller.Switch(82) = 1
	If keycode = LeftTiltKey Then Nudge 90, 4:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
	If keycode = RightTiltKey Then Nudge 270, 4:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
	If keycode = CenterTiltKey Then Nudge 0, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
	If keycode = Plungerkey then plunger.PullBack:PlaySoundAtVol "plungerpull", Plunger, 1
	If vpmKeyDown(keycode) Then Exit Sub
    'debug key
'	If KeyCode = "3" Then
'	SLTimer.Enabled = 1
'	SLTimer.Interval = 50
'        SetLamp 158, 1
'        SetLamp 159, 1
'        SetLamp 160, 1
'        SetLamp 161, 1
'End If
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If KeyCode = LeftFlipperKey Then Controller.Switch(81) = 0
	If KeyCode = RightFlipperKey Then Controller.Switch(82) = 0
	If keycode = plungerkey then plunger.Fire:PlaySoundAtVol "plunger", Plunger, 1
	If vpmKeyUp(keycode) Then Exit Sub
    'debug key
'	If KeyCode = "3" Then
'	SLTimer.Enabled = 0
'        SetLamp 158, 0
'        SetLamp 159, 0
'        SetLamp 160, 0
'        SetLamp 161, 0
'End If
End Sub

'**************
' Flipper Subs
'**************

   SolCallback(sLRFlipper) = "SolRFlipper"
   SolCallback(sLLFlipper) = "SolLFlipper"

   Sub SolLFlipper(Enabled)
	If Enabled Then
        PlaySoundAtVol SoundFX("fx_flipper1",DOFFlippers), LeftFlipper, VolFlip
        LeftFlipper.RotateToEnd
	    Else
        PlaySoundAtVol SoundFX("fx_flipperdown",DOFFlippers), LeftFlipper, VolFlip
        LeftFlipper.RotateToStart
	End If
End Sub

   Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAtVol SoundFX("fx_flipper1",DOFFlippers), RightFlipper, VolFlip
        RightFlipper.RotateToEnd
    Else
        PlaySoundAtVol SoundFX("fx_flipperdown",DOFFlippers), RightFlipper, VolFlip
        RightFlipper.RotateToStart
    End If
End Sub

'************************************************
'******************Kickers***********************
'************************************************

Dim BIP:BIP=0

Sub RightKicker(enabled)
 If enabled Then
	sw35.kick 0,30,1.56
	Controller.Switch(35) = 0
'	vpmTimer.AddTimer 2000, "sw35.enabled=1'"
	PlaysoundAtVol SoundFX("scoopexit",DOFContactors), sw35, 1
	VUKEject.Z = -53
	sw35.TimerEnabled = 1
End If
End Sub

Sub sw35_Timer
	VUKEject.Z=-70
	WireSwVUK.RotX = -10
	sw35.TimerEnabled = 0
End Sub

Sub sw35_hit
	PlaySoundAtVol "fx_kicker_enter", sw35, 0.8
	Controller.Switch(35) = 1
'	sw35.enabled= 0
	WireSwVUK.RotX = 0
End sub

Sub Drain_Hit()
  Me.Destroyball:bsTrough.AddBall Me:PlaysoundAtVol "fx_drain", Drain, 1
    BIP = BIP-1
End Sub
Sub BRT_Hit():BIP=BIP+1:PlaysoundAtVol SoundFX("fx_ballrel",DOFContactors), BRT, 1:End Sub

'************************************************
'************** Bumpers *****************
'************************************************

Sub Bumper1_Hit:vpmTimer.PulseSw 10:PlaySoundAtBallVol SoundFX("fx_bumper1",DOFContactors), VolBump:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 12:PlaySoundAtBallVol SoundFX("fx_bumper2",DOFContactors), VolBump:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 11:PlaySoundAtBallVol SoundFX("fx_bumper3",DOFContactors), VolBump:End Sub

'************************************************
'**********Sling Shot Animations*****************
'************************************************

Dim RStep, Lstep, RTStep

Sub LeftSlingShot_Slingshot
 If PinPlay=1 Then
	PlaySoundAtVol SoundFX("SlingshotLeft",DOFContactors), Sling1, 1
	vpmTimer.PulseSw 13
    LSling.Visible = 0
    LSling1.Visible = 1
    Sling1.TransZ = -22
    LStep = 0
    Me.TimerEnabled = 1
End If
End Sub

Sub SlingFSx1_Collide(parm)
 If Sling1.TransZ=0 And PinPlay=1 And parm > ThSling Then
	SlingFSx1.RotateToEnd:SlingFSx2.RotateToEnd:vpmTimer.PulseSw 13:PlaySoundAtVol SoundFX("SlingshotLeft",DOFContactors), sling1, 1
	LSling.Visible = 0:LSling1.Visible = 1:Sling1.TransZ = -22:LeftSlingshot.TimerEnabled=1:LStep=0
End If
End Sub
Sub SlingFSx2_Collide(parm)
 If Sling1.TransZ=0 And PinPlay=1 And parm > ThSling Then
	SlingFSx1.RotateToEnd:SlingFSx2.RotateToEnd:vpmTimer.PulseSw 13:PlaySoundAtVol SoundFX("SlingshotLeft",DOFContactors), sling1, 1
	LSling.Visible = 0:LSling1.Visible = 1:Sling1.TransZ = -22:LeftSlingshot.TimerEnabled=1:LStep=0
End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:Sling1.TransZ = -11:SlingFSx1.RotateToStart:SlingFSx2.RotateToStart
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:Sling1.TransZ = 0:Me.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
 If PinPlay=1 Then
	PlaySoundAtVol SoundFX("SlingshotRight",DOFContactors), Sling2, 1
	vpmTimer.PulseSw 14
    RSling.Visible = 0
    RSling1.Visible = 1
    Sling2.TransZ = -22
    RStep = 0
    Me.TimerEnabled = 1
End If
End Sub

Sub SlingFDx1_Collide(parm)
 If Sling2.TransZ=0 And PinPlay=1 And parm > ThSling Then
	SlingFDx1.RotateToEnd:SlingFDx2.RotateToEnd:vpmTimer.PulseSw 14:PlaySoundAtVol SoundFX("SlingshotLeft",DOFContactors), sling2, 1
	RSling.Visible = 0:RSling1.Visible = 1:Sling2.TransZ = -22:RightSlingshot.TimerEnabled=1:RStep=0
End If
End Sub
Sub SlingFDx2_Collide(parm)
 If Sling2.TransZ=0 And PinPlay=1 And parm > ThSling Then
	SlingFDx1.RotateToEnd:SlingFDx2.RotateToEnd:vpmTimer.PulseSw 14:PlaySoundAtVol SoundFX("SlingshotLeft",DOFContactors), sling2, 1
	RSling.Visible = 0:RSling1.Visible = 1:Sling2.TransZ = -22:RightSlingshot.TimerEnabled=1:RStep=0
End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:Sling2.TransZ = -11:SlingFDx1.RotateToStart:SlingFDx2.RotateToStart
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:Sling2.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub RightTopSlingShot_Slingshot
	PlaySoundAtVol SoundFX("SlingshotRight",DOFContactors), Sling3, 1
	vpmTimer.PulseSw 15
    RSling3.Visible = 0
    RSling4.Visible = 1
    Sling3.TransZ = -22
    RTStep = 0
    Me.TimerEnabled = 1
End Sub

Sub SlingFDx3_Collide(parm)
 If Sling3.TransZ=0 And PinPlay=1 And parm > ThSling Then
	SlingFDx3.RotateToEnd:SlingFDx4.RotateToEnd:vpmTimer.PulseSw 15:PlaySoundAtVol SoundFX("SlingshotLeft",DOFContactors), sling3, 1
	RSling3.Visible = 0:RSling4.Visible = 1:Sling3.TransZ = -22:RightTopSlingShot.TimerEnabled=1:RTStep=0
End If
End Sub
Sub SlingFDx4_Collide(parm)
 If Sling3.TransZ=0 And PinPlay=1 And parm > ThSling Then
	SlingFDx3.RotateToEnd:SlingFDx4.RotateToEnd:vpmTimer.PulseSw 15:PlaySoundAtVol SoundFX("SlingshotLeft",DOFContactors), sling3, 1
	RSling3.Visible = 0:RSling4.Visible = 1:Sling3.TransZ = -22:RightTopSlingShot.TimerEnabled=1:RTStep=0
End If
End Sub

Sub RightTopSlingShot_Timer
    Select Case RTStep
        Case 3:RSLing4.Visible = 0:RSLing5.Visible = 1:Sling3.TransZ = -11:SlingFDx3.RotateToStart:SlingFDx4.RotateToStart
        Case 4:RSLing5.Visible = 0:RSLing3.Visible = 1:Sling3.TransZ = 0:Me.TimerEnabled = 0
    End Select
    RTStep = RTStep + 1
End Sub

'******************
' Targets
'******************

Sub sw16_Hit:vpmTimer.PulseSw 16:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw26_Hit:vpmTimer.PulseSw 26:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw36_Hit:vpmTimer.PulseSw 36:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw105_Hit:vpmTimer.PulseSw 105:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw115_Hit:vpmTimer.PulseSw 115:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw92_Hit:vpmTimer.PulseSw 92:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw93_Hit:vpmTimer.PulseSw 93:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw94_Hit:vpmTimer.PulseSw 94:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw95_Hit:vpmTimer.PulseSw 95:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw23_Hit:Controller.Switch(23)=1:KickingTInsw23.RotY=8:PlaysoundAtVol SoundFX("metalhit_thin",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw23_Unhit:Controller.Switch(23)=0:KickingTInsw23.RotY=0:End Sub
Sub sw23_Timer:KickingTsw33.RotY=0:Me.TimerEnabled=0:End Sub
Sub sw33_Hit:vpmTimer.PulseSw 33:KickingTsw33.RotY=8:Me.TimerEnabled=1:PlaysoundAtVol SoundFX("target",DOFTargets), ActiveBall, VolTarg:End Sub
Sub sw33_Timer:KickingTsw33.RotY=0:Me.TimerEnabled=0:End Sub

'******************
' Drop Targets
'******************

Sub sw7_Hit():PlaysoundAtVol SoundFX("DTDrop",DOFDropTargets), ActiveBall, VolTarg:End Sub
Sub sw7_Dropped:dtDrop.Hit 1:GiL7T7.Visible=1:GiL8T7.Visible=1:End Sub
Sub sw17_Hit():PlaysoundAtVol SoundFX("DTDrop",DOFDropTargets), ActiveBall, VolTarg:End Sub
Sub sw17_Dropped:dtDrop.Hit 2:GiL7T17.Visible=1:GiL8T17.Visible=1:End Sub
Sub sw27_Hit():PlaysoundAtVol SoundFX("DTDrop",DOFDropTargets), ActiveBall, VolTarg:End Sub
Sub sw27_Dropped:dtDrop.Hit 3:GiL7T27.Visible=1:GiL8T27.Visible=1:End Sub
Sub sw37_Hit():PlaysoundAtVol SoundFX("DTDrop",DOFDropTargets), ActiveBall, VolTarg:End Sub
Sub sw37_Dropped:dtDrop.Hit 4:GiL7T37.Visible=1:GiL8T37.Visible=1:End Sub

'******************
' Rollovers
'******************

Sub sw20_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(20)=1:Psw20.Z=118:End Sub
Sub sw20_unhit:Controller.Switch(20)=0:Psw20.Z=135:End Sub
Sub sw21_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(21)=1:Psw21.Z=118:End Sub
Sub sw21_unhit:Controller.Switch(21)=0:Psw21.Z=135:End Sub
Sub sw22_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(22)=1:Psw22.Z=118:End Sub
Sub sw22_unhit:Controller.Switch(22)=0:Psw22.Z=135:End Sub
Sub sw30_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(30)=1:Psw30.Z=118:End Sub
Sub sw30_unhit:Controller.Switch(30)=0:Psw30.Z=135:End Sub
Sub sw31_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(31)=1:Psw31.Z=118:End Sub
Sub sw31_unhit:Controller.Switch(31)=0:Psw31.Z=135:End Sub
Sub sw32_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(32)=1:Psw32.Z=118:End Sub
Sub sw32_unhit:Controller.Switch(32)=0:Psw32.Z=135:End Sub

Sub sw25_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(25)=1:Psw25.Z=-44:End Sub
Sub sw25_unhit:Controller.Switch(25)=0:Psw25.Z=-27:End Sub

Sub sw80_hit:PlaysoundAtVol "wireramp", ActiveBall, VolWire:Controller.Switch(80)=1:End Sub
Sub sw80_unhit:Controller.Switch(80)=0:End Sub

Sub sw85_hit:PlaysoundAtVol "gate", ActiveBall, 1:Controller.Switch(85)=1:End Sub
Sub sw85_unhit:Controller.Switch(85)=0:End Sub
Sub sw100_hit:Controller.Switch(100)=1:End Sub
Sub sw100_unhit:Controller.Switch(100)=0:End Sub
Sub sw102_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(102)=1:Psw102.Z=-44:End Sub
Sub sw102_unhit:Controller.Switch(102)=0:Psw102.Z=-27:End Sub
Sub sw112_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(112)=1:Psw112.Z=-44:End Sub
Sub sw112_unhit:Controller.Switch(112)=0:Psw112.Z=-27:End Sub
Sub sw113_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(113)=1:Psw113.Z=-44:End Sub
Sub sw113_unhit:Controller.Switch(113)=0:Psw113.Z=-27:End Sub
Sub sw103_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(103)=1:Psw103.Z=-44:End Sub
Sub sw103_unhit:Controller.Switch(103)=0:Psw103.Z=-27:End Sub
Sub sw104_hit:PlaysoundAtVol "rollover", ActiveBall, 1:Controller.Switch(104)=1:Psw104.Z=-44:Bisl = Bisl + 1:End Sub
Sub sw104_unhit:Controller.Switch(104)=0:Psw104.Z=-27:End Sub

Sub sw110_hit:Controller.Switch(110)=1:End Sub		'Starting Gate Opto
Sub sw110_unhit:Controller.Switch(110)=0:End Sub

'*********
'Solenoids												' 1 - Left Pop Bumper
'*********												' 2 - Top Pop Bumper			
														' 3 - Bottom Pop Bumper
														' 4 - Left Sling
														' 5 - Right Sling
'	SolCallback(6) = "Top Sling"						' 6 - Top Sling
	SolCallback(7) = "skiliftgate"						' 7 - Ski Lift Gate
'	SolCallback(7) = "vpmSolDiverter Flipper1,True,"
	SolCallback(8) = "LSlalom"							' 8 - Left Slalom
	SolCallback(9) = "RSlalom"							' 9 - Right Slalom	
	SolCallback(10) = "RightKicker"						'10 - UpKicker
	SolCallback(11) = "dtDropSDUp"						'11 - 4 Bank Reset
														'12	- Not Used
	SolCallback(13) = "startgate"						'13 - Starting Gate Plunger
	SolCallback(14) = "SetLamp 154,"					'14 - Top Bumper Flash	
	SolCallback(15) = "SetLamp 155,"					'15 - Center Bumper Flash
	SolCallback(16) = "SetLamp 156,"					'16 - Bottom Bumper Flash
	SolCallback(17) = "Setlamp 157,"					'17 - Left Bottom Flash
	SolCallback(18) = "SetLamp 158,"					'18 - Left Center Flash
	SolCallback(19) = "SetLamp 159,"					'19 - Left Top Flash
	SolCallback(20) = "SetLamp 160,"					'20 - Right Top
	SolCallback(21) = "SetLamp 161,"					'21 - Right Center Flash
   	SolCallback(22) = "SetLamp 162,"					'22 - Right Bottom Flash
	SolCallback(23) = "SetLamp 163,"					'23 - Lightbox (3)
	SolCallback(24) = "SetLamp 164,"					'24 - LightBox (3)
	SolCallback(25) = "SkiLiftMotor"					'25 - Ski Lift Motor Relay
	SolCallback(26) = "LightBox"						'26 - LightBox Relay (A)
	SolCallback(28) = "bsTrough.SolOut"					'28 - Ball Release
	SolCallback(29) = "bsTrough.SolIn"					'29 - Outhole
	SolCallback(30) = "vpmSolSound ""Knocker"","		'30 - Knocker
	SolCallback(31) = "TiltRelay"						'31 - Tilt												
	Solcallback(32) = "SolRun"							'32 - Game Over Relay

Sub LightBox(Enabled)
	Dim GIoffon
	GIoffon = ABS(ABS(Enabled) -1)
	SetLamp 166, GIoffon
End Sub

'******************
' Ski Lift Gate
'******************

Sub skiliftgate(Enabled)
	If Enabled Then
	GateF1.RotateToEnd
	PlaySoundAt SoundFX("solenoid",DOFContactors), GateF1
Else
	GateF1.RotateToStart
	PlaySoundAt SoundFX("solenoid",DOFContactors), GateF1
End If
End Sub

Sub GateF1_Collide(parm)
	PlaySoundAtVol "metalhit_thin", ActiveBall, 1
End Sub

'******************
' Start Gate
'******************

Sub startgate(Enabled)
 If Enabled Then
	BL2.Isdropped = 1
	PlaySoundAtVol SoundFX("solenoid",DOFContactors), sw110, 1
Else
	BL2.Isdropped = 0
End If
End Sub

'******************
' Ski Lift Motor
'******************

Sub SkiLiftMotor(Enabled)
 If Enabled Then
	SLTimer.Enabled = 1
	SLTimer.Interval = 42
 If Enabled And bsteps1 <> 0 And bsteps1 < 106 Then
	KSLIn1.TimerEnabled = 1
End If
 If Enabled And bsteps2 <> 0 And bsteps2 < 106 Then
	KSLIn2.TimerEnabled = 1
End If
 If Enabled And bsteps3 <> 0 And bsteps3 < 106 Then
	KSLIn3.TimerEnabled = 1
End If
Else
	KSLIn1.Enabled = 0
	KSLIn2.Enabled = 0
	KSLIn3.Enabled = 0
	SLTimer.Enabled = 0
	KSLIn1.TimerEnabled = 0
	KSLIn2.TimerEnabled = 0
	KSLIn3.TimerEnabled = 0
End If
End Sub

Dim SLsteps, KSLE, Bisl
SLsteps = 0:KSLE = 1:Bisl = 0

Sub SLTimer_Timer()
	Select Case SLsteps
		Case 0:SLBelt.Image = "BeltU4":SLBelt1.Image = "BeltU4":SLBelt2.Image = "BeltD4":SLBelt3.Image = "BeltD4":SLsteps = 1

			If KSL1on = 1 And bsteps1 = 0 Then:KSLIn1.TimerEnabled = 1:KSLIn1.TimerInterval = 40:End If
			If KSL2on = 1 And bsteps2 = 0 Then:KSLIn2.TimerEnabled = 1:KSLIn2.TimerInterval = 40:End If
			If KSL3on = 1 And bsteps3 = 0 Then:KSLIn3.TimerEnabled = 1:KSLIn3.TimerInterval = 40:End If

		Case 1:SLBelt.Image = "BeltU5":SLBelt1.Image = "BeltU5":SLBelt2.Image = "BeltD5":SLBelt3.Image = "BeltD5":SLsteps = 2
		Case 2:SLBelt.Image = "BeltU6":SLBelt1.Image = "BeltU6":SLBelt2.Image = "BeltD6":SLBelt3.Image = "BeltD6":SLsteps = 3
		Case 3:SLBelt.Image = "BeltU7":SLBelt1.Image = "BeltU7":SLBelt2.Image = "BeltD7":SLBelt3.Image = "BeltD7":SLsteps = 4
		Case 4:SLBelt.Image = "BeltU8":SLBelt1.Image = "BeltU8":SLBelt2.Image = "BeltD8":SLBelt3.Image = "BeltD8":SLsteps = 5
		Case 5:SLBelt.Image = "BeltU9":SLBelt1.Image = "BeltU9":SLBelt2.Image = "BeltD9":SLBelt3.Image = "BeltD9":SLsteps = 6
		Case 6:SLBelt.Image = "BeltU10":SLBelt1.Image = "BeltU10":SLBelt2.Image = "BeltD10":SLBelt3.Image = "BeltD10":SLsteps = 7
		Case 7:SLBelt.Image = "BeltU11":SLBelt1.Image = "BeltU11":SLBelt2.Image = "BeltD11":SLBelt3.Image = "BeltD11":SLsteps = 8
		Case 8:SLBelt.Image = "BeltU12":SLBelt1.Image = "BeltU12":SLBelt2.Image = "BeltD12":SLBelt3.Image = "BeltD12":SLsteps = 9
		Case 9:SLBelt.Image = "BeltU13":SLBelt1.Image = "BeltU13":SLBelt2.Image = "BeltD13":SLBelt3.Image = "BeltD13":SLsteps = 10
		Case 10:SLBelt.Image = "BeltU14":SLBelt1.Image = "BeltU14":SLBelt2.Image = "BeltD14":SLBelt3.Image = "BeltD14":SLsteps = 11
		Case 11:SLBelt.Image = "BeltU15":SLBelt1.Image = "BeltU15":SLBelt2.Image = "BeltD15":SLBelt3.Image = "BeltD15":SLsteps = 12
		Case 12:SLBelt.Image = "BeltU16":SLBelt1.Image = "BeltU16":SLBelt2.Image = "BeltD16":SLBelt3.Image = "BeltD16":SLsteps = 13
		Case 13:SLBelt.Image = "BeltU1":SLBelt1.Image = "BeltU1":SLBelt2.Image = "BeltD1":SLBelt3.Image = "BeltD1":SLsteps = 14
		Case 14:SLBelt.Image = "BeltU2":SLBelt1.Image = "BeltU2":SLBelt2.Image = "BeltD2":SLBelt3.Image = "BeltD2":SLsteps = 15
		Case 15:SLBelt.Image = "BeltU3":SLBelt1.Image = "BeltU3":SLBelt2.Image = "BeltD3":SLBelt3.Image = "BeltD3":SLsteps = 0

			If KSLE = 1 And Bisl > 0 Then:KSLIn1.Enabled = 1:End If
			If KSLE = 2 And Bisl > 0 Then:KSLIn2.Enabled = 1:End If
			If KSLE = 3 And Bisl > 0 Then:KSLIn3.Enabled = 1:End If

	End Select

		If bsteps1 > 13 Then:KSLE = 2:End If
		If bsteps2 > 13 Then:KSLE = 3:End If
		If bsteps3 > 13 Then:KSLE = 1:End If

		PulegiaSL.RotX=PulegiaSL.RotX + 8
End Sub

Dim liftball1, xball1, yball1, zball1, KSL1on
Dim bsteps1: bsteps1 = 0

Sub KSLIn1_Hit
Set liftball1 = ActiveBall
	KSL1on=1
	xball1 = 164
	yball1 = 746
	zball1 = 20
End Sub

Sub KSLIn1_Timer()
 If NOT IsEmpty(liftball1) Then
	liftball1.x = xball1
	liftball1.y = yball1
	liftball1.z = zball1
	yball1 = yball1 - 4		'424/106
	zball1 = zball1 + 2.15	'227,9/106
	bsteps1 = bsteps1 + 1
End If
 If bsteps1 > 106 Then
	Me.TimerEnabled = 0
	bsteps1 = 0
	Me.DestroyBall
	KSL1on=0				'Kicker1 fine trasporto
	KSLOut.createball
	KSLOut.Kick 0,2
	Bisl = Bisl - 1
End If
	KSLIn1.Enabled = 0
End Sub

Dim liftball2, xball2, yball2, zball2, KSL2on
Dim bsteps2: bsteps2 = 0

Sub KSLIn2_Hit
Set liftball2 = ActiveBall
	KSL2on=1
	xball2 = 164
	yball2 = 746
	zball2 = 20
End Sub

Sub KSLIn2_Timer()
 If NOT IsEmpty(liftball2) Then
	liftball2.x = xball2
	liftball2.y = yball2
	liftball2.z = zball2
	yball2 = yball2 - 4		'424/106
	zball2 = zball2 + 2.15	'227,9/106
	bsteps2 = bsteps2 + 1
End If
 If bsteps2 > 106 Then
	Me.TimerEnabled = 0
	bsteps2 = 0
	Me.DestroyBall
	KSL2on=0				'Kicker2  fine trasporto
	KSLOut.createball
	KSLOut.Kick 0,2
	Bisl = Bisl - 1
End If
	KSLIn2.Enabled = 0
End Sub

Dim liftball3, xball3, yball3, zball3, KSL3on
Dim bsteps3: bsteps3 = 0

Sub KSLIn3_Hit
Set liftball3 = ActiveBall
	KSL3on=1
	xball3 = 164
	yball3 = 746
	zball3 = 20
End Sub

Sub KSLIn3_Timer()
 If NOT IsEmpty(liftball3) Then
	liftball3.x = xball3
	liftball3.y = yball3
	liftball3.z = zball3
	yball3 = yball3 - 4		'424/106
	zball3 = zball3 + 2.15	'227,9/106
	bsteps3 = bsteps3 + 1
End If
 If bsteps3 > 106 Then
	Me.TimerEnabled = 0
	bsteps3 = 0
	Me.DestroyBall
	KSL3on=0				'Kicker3  fine trasporto
	KSLOut.createball
	KSLOut.Kick 0,2
	Bisl = Bisl - 1
End If
	KSLIn3.Enabled = 0
End Sub

'******************
' Slalom
'******************

Sub LSlalom(enabled)
	If Enabled Then
	Primitive_SkiiHill.RotZ=-8
	SlalomRubbers.RotZ=-8
	SlalomMetals.RotZ=-8
	Psw20.RotZ=-8:Psw21.RotZ=-8:Psw22.RotZ=-8:Psw30.RotZ=-8:Psw31.RotZ=-8:Psw32.RotZ=-8
	GuidaGialla.RotZ=-8:GuidaBlu.RotZ=-8:GuidaVerde.RotZ=-8
	l42p.RotZ=-8:l43p.RotZ=-8:l44p.RotZ=-8:l72p.RotZ=-8:l73p.RotZ=-8:l74p.RotZ=-8

FlipperSxS.RotateToEnd
FlipperDxS.RotateToEnd

BRubberSX1.Collidable = 1
BRubberSX2.Collidable = 1
BRubberSX3.Collidable = 1
BRubberSX4.Collidable = 1
BRubberSX5.Collidable = 1
BRubberSX6.Collidable = 1
BRubberSX7.Collidable = 1
BRubberSX8.Collidable = 1
BRubberSX9.Collidable = 1
BRubberSX10.Collidable = 1
BRubberSX11.Collidable = 1

PlastSxS1.Collidable = 1
PlastSxS2.Collidable = 1
PlastSxS3.Collidable = 1

BRubberDX1.Collidable = 0
BRubberDX2.Collidable = 0
BRubberDX3.Collidable = 0
BRubberDX4.Collidable = 0
BRubberDX5.Collidable = 0
BRubberDX6.Collidable = 0
BRubberDX7.Collidable = 0
BRubberDX8.Collidable = 0
BRubberDX9.Collidable = 0
BRubberDX10.Collidable = 0
BRubberDX11.Collidable = 0

PlastDxS1.Collidable = 0
PlastDxS2.Collidable = 0
PlastDxS3.Collidable = 0

	PlaySoundAtVol SoundFX("metalhit2",DOFContactors), sw110, 1
	End If
End Sub

Sub RSlalom(enabled)
	If Enabled Then
	Primitive_SkiiHill.RotZ=-7
	SlalomRubbers.RotZ=-7
	SlalomMetals.RotZ=-7
	Psw20.RotZ=-7:Psw21.RotZ=-7:Psw22.RotZ=-7:Psw30.RotZ=-7:Psw31.RotZ=-7:Psw32.RotZ=-7
	GuidaGialla.RotZ=-7:GuidaBlu.RotZ=-7:GuidaVerde.RotZ=-7
	l42p.RotZ=-7:l43p.RotZ=-7:l44p.RotZ=-7:l72p.RotZ=-7:l73p.RotZ=-7:l74p.RotZ=-7

FlipperSxS.RotateToStart
FlipperDxS.RotateToStart

BRubberSX1.Collidable = 0
BRubberSX2.Collidable = 0
BRubberSX3.Collidable = 0
BRubberSX4.Collidable = 0
BRubberSX5.Collidable = 0
BRubberSX6.Collidable = 0
BRubberSX7.Collidable = 0
BRubberSX8.Collidable = 0
BRubberSX9.Collidable = 0
BRubberSX10.Collidable = 0
BRubberSX11.Collidable = 0

PlastSxS1.Collidable = 0
PlastSxS2.Collidable = 0
PlastSxS3.Collidable = 0

BRubberDX1.Collidable = 1
BRubberDX2.Collidable = 1
BRubberDX3.Collidable = 1
BRubberDX4.Collidable = 1
BRubberDX5.Collidable = 1
BRubberDX6.Collidable = 1
BRubberDX7.Collidable = 1
BRubberDX8.Collidable = 1
BRubberDX9.Collidable = 1
BRubberDX10.Collidable = 1
BRubberDX11.Collidable = 1

PlastDxS1.Collidable = 1
PlastDxS2.Collidable = 1
PlastDxS3.Collidable = 1

	PlaySoundAtVol SoundFX("metalhit2",DOFContactors), sw110, 1
	End If
End Sub

Sub FlipperSxS_Collide(parm)
	PlaySoundAtVol "metalhit_thin", ActiveBall, 1
End Sub
Sub FlipperDxS_Collide(parm)
	PlaySoundAtVol "metalhit_thin", ActiveBall, 1
End Sub

Sub dtDropSDUp(Enabled)
 If Enabled Then
	dtDrop.DropSol_On
End If
	GiL7T7.TimerEnabled=1
	PlaySoundAtVol SoundFX("resetdrop",DOFContactors), sw26, 1
End Sub

Sub GiL7T7_Timer()
	GiL7T7.Visible=0
	GiL7T17.Visible=0
	GiL7T27.Visible=0
	GiL7T37.Visible=0
	GiL8T7.Visible=0
	GiL8T17.Visible=0
	GiL8T27.Visible=0
	GiL8T37.Visible=0
	GiL7T7.TimerEnabled=0
End Sub

'****************** GI

Sub TiltRelay(enabled)
	If Enabled Then
	UpdateGI 0, 1
	Playsound "rollover"
Else
	UpdateGI 0, 8
	End If
End Sub

Sub SolRun(Enabled)
	vpmNudge.SolGameOn Enabled
 If Enabled Then
	PinPlay=1
Else
	PinPlay=0
	SlingFSx1.RotateToStart:SlingFSx2.RotateToStart
	SlingFDx1.RotateToStart:SlingFDx2.RotateToStart
	SlingFDx3.RotateToStart:SlingFDx4.RotateToStart
End If
End Sub

'******************
' RealTime Updates
'******************

Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
    UpdateMechs
End Sub

Sub UpdateMechs
	Prim_LeftFlipper.RotZ=LeftFlipper.currentangle
	Prim_RightFlipper.RotZ=RightFlipper.currentangle
	GatePrim.ObjRotZ=GateF1.Currentangle
End Sub

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 20 'lamp fading speed
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

	NFadeL 0, l0
	
	NFadeLm 11, l11
	Flash 11, f11
	NFadeL 12, l12
	NFadeL 13, l13
	NFadeL 14, l14
	NFadeL 15, l15
	NFadeL 16, l16
	NFadeL 17, l17

	NFadeL 23, l23
	NFadeL 24, l24
	NFadeL 25, l25
	NFadeL 26, l26
	NFadeL 27, l27

	NFadeL 33, l33
	NFadeL 34, l34
	NFadeL 35, l35
	NFadeL 36, l36
	NFadeL 37, l37

	FadeObj 42, l42P, "bulbcover1_greenOn", "bulbcover1_greenA", "bulbcover1_greenB", "bulbcover1_green"
	FadeObj 43, l43p, "bulbcover1_blueOn", "bulbcover1_blueA", "bulbcover1_blueB", "bulbcover1_blue"
	FadeObj 44, l44p, "bulbcover1_yellowOn", "bulbcover1_yellowA", "bulbcover1_yellowB", "bulbcover1_yellow"
	NFadeL 45, l45
	NFadeL 46, l46
	NFadeL 47, l47

	NFadeL 54, l54
	NFadeL 55, l55
	NFadeL 56, l56
	NFadeL 57, l57

	NFadeL 63, l63
	NFadeL 64, l64
	NFadeL 65, l65
	NFadeL 66, l66
	NFadeL 67, l67

	FadeObj 72, l72P, "bulbcover1_greenOn", "bulbcover1_greenA", "bulbcover1_greenB", "bulbcover1_green"
	FadeObj 73, l73p, "bulbcover1_blueOn", "bulbcover1_blueA", "bulbcover1_blueB", "bulbcover1_blue"
	FadeObj 74, l74p, "bulbcover1_yellowOn", "bulbcover1_yellowA", "bulbcover1_yellowB", "bulbcover1_yellow"
	NFadeL 75, l75
	NFadeL 76, l76
	NFadeL 77, l77

	NFadeL 80, l80
	NFadeL 81, l81
	NFadeL 82, l82
	NFadeL 83, l83

	NFadeL 85, l85
	NFadeL 86, l86
	NFadeL 87, l87
	
	
	NFadeL 90, l90
	NFadeL 91, l91
	NFadeL 92, l92
	NFadeL 93, l93
	NFadeL 94, l94
	NFadeL 95, l95
	NFadeL 96, l96
	NFadeL 97, l97
	
	NFadeL 100, l100
	NFadeL 101, l101
	NFadeL 102, l102
	NFadeL 103, l103
	NFadeL 104, l104
	NFadeL 105, l105
	NFadeL 106, l106
	NFadeL 107, l107

	Flash 154, f114
	NFadeL 155, l115
	NFadeL 156, l116
	NFadeL 157, l117
	NFadeLm 158, l118
	Flash 158, f118
	NFadeLm 159, l119
	Flash 159, f119
	NFadeLm 160, l120
	Flash 160, f120
	NFadeLm 161, l121
	Flash 161, f121
	NFadeL 162, L122

' Backbox lights

	Flashm 163, f123
	Flashm 163, f123a
	Flash 163, f123b

	Flashm 164, f124
	Flashm 164, f124a
	Flash 164, f124b

	Flashm 166, fbb1
	Flashm 166, fbb2
	Flashm 166, fbb3
	Flashm 166, fbb4
	Flashm 166, fbb5
	Flashm 166, fbb6
	Flashm 166, fbb7
	Flashm 166, fbb8
	Flashm 166, fbb9
	Flashm 166, fbb10
	Flashm 166, fbb11
	Flashm 166, fbb12
	Flashm 166, fbb13
	Flashm 166, fbb14
	Flashm 166, fbb15
	Flashm 166, fbb16
	Flashm 166, fbb17
	Flashm 166, fbb18
	Flashm 166, fbb19
	Flashm 166, fbb20
	Flashm 166, fbb21
	Flashm 166, fbb22
	Flashm 166, fbb23
	Flashm 166, fbb24
	Flashm 166, fbb25
	Flashm 166, fbb26
	Flashm 166, fbb27
	Flashm 166, fbb28
	Flashm 166, fbb29
	Flashm 166, fbb30
	Flashm 166, fbb31
	Flashm 166, fbb32
	Flashm 166, fbb33
	Flashm 166, fbb34
	Flashm 166, fbb35
	Flashm 166, fbb36
	Flashm 166, fbb37
	Flashm 166, fbb38
	Flashm 166, fbb39
	Flashm 166, fbb40
	Flash 166, fbb41
	
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

'*********
'Update GI
'*********

Dim gistep

Sub UpdateGI(no, step)
    Dim xx
    If step = 0 then exit sub 'only values from 1 to 8 are visible and reliable. 0 is not reliable and 7 & 8 are the same so...
    gistep = (step-1) / 7
    'Select Case no
    '    Case 0
            For each xx in GI:xx.IntensityScale = gistep:next   'Playfield
  
    ' change the intensity of the flasher depending on the gi to compensate for the gi lights being off
    For xx = 0 to 200
        FlashMax(xx) = 6 - gistep * 3 ' the maximum value of the flashers
    Next
End Sub

'************************************************
'******Supporting Ball & Sound Functions*********
'************************************************

Function RndNum(min, max)
  RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
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

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
  BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
  VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
End Function

'************************************************
'***********JP's VP10 Rolling Sounds*************
'************************************************

Const tnob = 5 ' total number of balls
Const lob = 0  'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
Dim BOT, b, ballpitch, ballvol
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
        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 15000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b))
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'************************************************
'*******JP's VP10 Ball Collision Sound***********
'************************************************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ActiveBall)
End Sub

'************************************************
'***************JF Sound Routines****************
'************************************************

Sub BRubbersSlalom_Hit (idx)
	PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 20 then
    PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End if
  If finalspeed >= 6 AND finalspeed <= 20 then
    RandomSoundRubber()
  End If
End Sub

Sub RandomSoundRubber()
  Select Case Int(Rnd*3)+1
    Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRH, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
    Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End Select
End Sub

Sub RRampEnter_Hit():PlaysoundAtVol "ramp_enter", ActiveBall, 1:End Sub
Sub RRampEnter1_Hit():PlaysoundAtVol "ramp_enter", ActiveBall, 1:End Sub

Sub LMRampRolling_Hit():PlaysoundAtVol "WireRamp", ActiveBall, 1:End Sub
Sub RRampRolling_Hit():Playsound "ramp_rolling", 0, 0.5, pan(ActiveBall), 0, 8000, 0, 0, AudioFade(ActiveBall):End Sub

Sub LMRamp_Hit()
     PlaySoundAtVol "fx_balldrop100", LMRamp, 1
	 StopSound "WireRamp"
 End Sub

Sub RRamp_Hit()
     PlaySoundAtVol "fx_balldrop100", RRamp, 1
	 StopSound "ramp_rolling"
 End Sub

Sub wirerampsound_Hit:PlaysoundAtVol "fx_metalrolling", ActiveBall, VolWire:End Sub
Sub wirerampsound2_Hit:PlaysoundAtVol "Wireramp1", ActiveBall, VolWire:End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX and Rothbauerw
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
  PlaySound sound, 0, ABS(BOT.velz)/17, AudioPan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub
