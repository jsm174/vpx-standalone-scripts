Option Explicit
Randomize

' Thalamus 2018-07-24
' Added/Updated "Positional Sound Playback Functions" and "Supporting Ball & Sound Functions"
' Thalamus 2018-08-16 : Improved directional sounds
' Added InitVpmFFlipsSAM

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 400
Const VolCol = 10

Const VolBump   = 2    ' Bumpers multiplier.
Const VolRol    = 1    ' Rollovers volume multiplier.
Const VolGates  = 1    ' Gates volume multiplier.
Const VolMetals = 1    ' Metals volume multiplier.
Const VolRB     = 4    ' Rubber bands multiplier.
Const VolRPi    = 1    ' Rubber pins multiplier.
Const VolTarg   = 1    ' Targets multiplier.

Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName = "rsn_110h"

If Table1.ShowDT = false then
Ramp16.Visible = 0
Ramp15.Visible = 0
Flasher001.rotx = -20
Flasher001.y = 415
End If

LoadVPM "01560000", "sam.VBS", 3.10

Const UseSolenoids=1,UseLamps=1,UseSync=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="FlipperUp",SFlipperOff="FlipperDown",sCoin="coin3"

SolCallback(1)				= "SolTrough"
SolCallback(2)				= "SolAutofire"
SolCallback(3)				= "SolCenterLock"
SolCallback(4)				= "SolCenterLockLatch"
SolCallback(5)				= "MagLeft"
SolCallback(6)				= "SolControlGate"
SolCallback(7)				= "MagRight"
'SolCallBack(9)			    = "vpmSolSound ""bumper2"","
'SolCallBack(10)			= "vpmSolSound ""bumper2"","
'SolCallBack(11)			= "vpmSolSound ""bumper2"","
'SolCallback(13)            = "vpmSolSound ""left_slingshot_new"","
'SolCallback(14)            = "vpmSolSound ""right_slingshot_new"","

SolCallback(15) 			= "SolLFlipper"
SolCallback(16) 			= "SolRFlipper"

SolCallback(17)				= "SolLUD"
SolCallback(18)             = "MickRelayLeft2"
SolCallback(19)             = "MickRelayRight2"

SolCallBack(20)     		= "Sol20"
SolCallBack(21)     		= "Sol21"
SolCallBack(22)     		= "Sol22"
SolCallBack(23)     		= "Sol23"
SolCallBack(25)     		= "Sol25"
SolCallBack(26)     		= "Sol26"
SolCallBack(27)     		= "Sol27"
SolCallBack(28)     		= "Sol28"
SolCallBack(29)     		= "Sol29"
SolCallBack(31)     		= "Sol31"

SolCallback(30)				= "SolCUD"
SolCallback(32)				= "SolRUD"

Dim bsTrough, x, xx, Mag1, Mag2, BIP

Sub Table1_Init
    Controller.GameName= cGameName
	Controller.SplashInfoLine="Rolling Stones (Stern 2011)"
	Controller.Games(cGameName).Settings.Value("rol")=0
	Controller.ShowTitle=0
	Controller.ShowDMDOnly=1
	Controller.ShowFrame=0
	Controller.HandleMechanics=0
    'Controller.Hidden = varhidden
	On Error Resume Next
		Controller.Run
		If Err Then MsgBox Err.Description
	On Error Goto 0
	PinMAMETimer.Interval=PinMAMEInterval:PinMAMETimer.Enabled=1:vpmNudge.TiltSwitch=-7
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
	vpmNudge.Sensitivity=5

    'Set bsTrough=New cvpmBallStack
	'bsTrough.InitSw 0,21,20,19,18,17,0,0
	'bsTrough.InitSw 0,21,20,19,18,0,0,0
	'bsTrough.InitKick BallRelease,110,5
	'bsTrough.InitExitSnd "BallRel","Solon"
	'bsTrough.KickBalls = 1
	'bsTrough.Balls=5

    Set mag1= New cvpmMagnet
		mag1.InitMagnet Magnet1, 16 '16
		mag1.GrabCenter = False

    Set mag2= New cvpmMagnet
		mag2.InitMagnet Magnet2, 16 '16
		mag2.GrabCenter = False

    Plunger1.Pullback
    Controller.Switch(71) = 1
    W17.isdropped = 1
	W18.isdropped = 1
	W19.isdropped = 1
	W20.isdropped = 1
	W21.isdropped = 1
    LUD.isdropped = 1
    RUD.isdropped = 1
    CUD.isdropped = 1
    MW1.isdropped = 1
	MW2.isdropped = 1
	MW3.isdropped = 1
	MW4.isdropped = 1
	MW5.isdropped = 1
	MW6.isdropped = 1
	MW_PARK.isdropped = 1
    Mick_PRIM.RotZ = 38
	MW1a.isdropped = 1
	MW2a.isdropped = 1
	MW3a.isdropped = 1
	MW4a.isdropped = 1
	MW5a.isdropped = 1
	MW6a.isdropped = 1
	MW_PARKa.isdropped = 1
    GI_On
  InitVpmFFlipsSAM
End Sub


Sub Table1_KeyDown(ByVal KeyCode)
 	If vpmKeyDown(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then PlaySoundAtVol "PlungerPull", plunger, 1:Plunger.Pullback
    	If KeyCode=LeftFlipperKey Then Controller.Switch(73)=1
		If KeyCode=RightFlipperKey Then Controller.Switch(75)=1
		If keycode=LeftMagnaSave Then Controller.Switch(88)=1
        If keycode=RightMagnaSave Then Controller.Switch(86)=1
        If keycode = 3 Then Controller.Switch(88)=1:Controller.Switch(86)=1
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If vpmKeyUp(KeyCode) Then Exit Sub
	If keycode = PlungerKey Then StopSound "PlungerPull":Plunger.Fire
	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If
	If KeyCode=LeftFlipperKey Then Controller.Switch(73)=0
	If KeyCode=RightFlipperKey Then Controller.Switch(75)=0
    If keycode = LeftMagnaSave Then Controller.Switch(88)=0
    If keycode=RightMagnaSave Then Controller.Switch(86)=0
    If keycode = 3 Then Controller.Switch(88)=0:Controller.Switch(86)=0
End Sub

Set MotorCallback=GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps

FGI1.visible = Light40.State
FGI2.visible = Light40.State
FGI3.visible = Light40.State
FGI4.visible = Light40.State
FGI5.visible = Light40.State

'Flasher1.visible = Light40.State
'Flasher2.visible = Light40.State

F1.visible = Light40.State
F2.visible = Light26.State

End Sub

Set Lights(3)=L3
Set Lights(4)=L4
Set Lights(5)=L5
Set Lights(6)=L6
Set Lights(7)=L7
Set Lights(8)=L8
Set Lights(9)=L9
Set Lights(10)=L10
Set Lights(11)=L11
Set Lights(12)=L12
Set Lights(13)=L13
Set Lights(14)=L14
Set Lights(15)=L15
Set Lights(16)=L16
Set Lights(17)=L17
Set Lights(18)=L18
Set Lights(19)=L19
Set Lights(20)=L20
Set Lights(21)=L21
Set Lights(22)=L22
Set Lights(23)=L23
Set Lights(26)=L26
Set Lights(27)=L27
Set Lights(29)=L29
Set Lights(30)=L30
Set Lights(37)=L37
Set Lights(38)=L38
Set Lights(39)=L39
Set Lights(40)=L40
Set Lights(42)=L42
Set Lights(43)=L43
Set Lights(44)=L44
Set Lights(45)=L45
Set Lights(46)=L46
Set Lights(47)=L47
Set Lights(25)=L25
Set Lights(27)=L27
Set Lights(28)=L28
Set Lights(41)=L41
Set Lights(24)=L24
Set Lights(28)=L28
Set Lights(31)=L31
Set Lights(32)=L32
Set Lights(33)=L33
Set Lights(34)=L34
Set Lights(35)=L35
Set Lights(36)=L36
Set Lights(48)=L48
Set Lights(49)=L49
Set Lights(50)=L50
Set Lights(51)=L51
Set Lights(52)=L52
Set Lights(53)=L53
Set Lights(58)=L58
Set Lights(60)=L60
Set Lights(61)=L61
Set Lights(62)=L62

'Switch Subs

Sub SW50_Hit()
Controller.Switch(50) = 1
End Sub

Sub SW50_UnHit()
Controller.Switch(50) = 0
End Sub

Sub SW23_Hit()
Controller.Switch(23) = 1
Gi_Off
End Sub

Sub Gi_Start_Hit()
Gi_On
End Sub

Sub SW23_UnHit()
Controller.Switch(23) = 0
If Controller.Switch(71) = False And ActiveBall.Uservalue = 666 Then
Controller.Switch(71) = True
End If
End Sub

Sub SW6_Hit():Controller.Switch(6) = 1:End Sub
Sub SW6_UnHit():Controller.Switch(6) = 0:End Sub
Sub SW7_Hit():Controller.Switch(7) = 1:End Sub
Sub SW7_UnHit():Controller.Switch(7) = 0:End Sub
Sub SW8_Hit():Controller.Switch(8) = 1:End Sub
Sub SW8_UnHit():Controller.Switch(8) = 0:End Sub
Sub SW9_Hit():Controller.Switch(9) = 1:End Sub
Sub SW9_UnHit():Controller.Switch(9) = 0:End Sub


Sub SW45_Hit():vpmTimer.PulseSw 45:Gi_Off:End Sub
Sub SW48_Hit():vpmTimer.PulseSw 48:End Sub

Sub SW29_Hit():vpmTimer.PulseSw 29:End Sub
Sub SW28_Hit():vpmTimer.PulseSw 28:End Sub

Sub SW25_Hit():vpmTimer.PulseSw 25:End Sub
Sub SW24_Hit():vpmTimer.PulseSw 24:End Sub
Sub SW41_Spin():vpmTimer.PulseSw 41:PlaySoundAtVol "fx_spinner", SW41, VolSpin:End Sub

Sub SW42_Hit():vpmTimer.PulseSw 42:Gi_Off:End Sub
Sub SW43_Hit():vpmTimer.PulseSw 43:End Sub

Sub SW44_Hit():vpmTimer.PulseSw 44:End Sub

Sub SW53_Hit():Controller.Switch(53) = 1:gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1:End Sub
Sub SW53_UnHit():Controller.Switch(53) = 0:End Sub

Sub SW46_Hit():Controller.Switch(46) = 1:End Sub
Sub SW46_UnHit():Controller.Switch(46) = 0:End Sub

Sub SW51_Hit():vpmTimer.PulseSw 51:Sw51p.transx = -10:me.timerenabled = 1:End Sub
Sub SW51_Timer():Sw51p.transx = 0:me.timerenabled = 0:End Sub

Sub SW56_Hit():vpmTimer.PulseSw 56:Sw56p.transx = -10:me.timerenabled = 1:End Sub
Sub SW56_Timer():Sw56p.transx = 0:me.timerenabled = 0:End Sub

Sub SW57_Hit():vpmTimer.PulseSw 57:Sw57p.transx = -10:me.timerenabled = 1:End Sub
Sub SW57_Timer():Sw57p.transx = 0:me.timerenabled = 0:End Sub

Sub SW1_Hit():vpmTimer.PulseSw 1:Sw1p.transx = -10:me.timerenabled = 1:End Sub
Sub SW1_Timer():Sw1p.transx = 0:me.timerenabled = 0:End Sub

Sub SW2_Hit():vpmTimer.PulseSw 2:Sw2p.transx = -10:me.timerenabled = 1:End Sub
Sub SW2_Timer():Sw2p.transx = 0:me.timerenabled = 0:End Sub

Sub SW3_Hit():vpmTimer.PulseSw 3:Sw3p.transx = -10:me.timerenabled = 1:End Sub
Sub SW3_Timer():Sw3p.transx = 0:me.timerenabled = 0:End Sub

Sub SW10_Hit():vpmTimer.PulseSw 10:Sw10p.transx = -10:me.timerenabled = 1:End Sub
Sub SW10_Timer():Sw10p.transx = 0:me.timerenabled = 0:End Sub

Sub SW11_Hit():vpmTimer.PulseSw 11:Sw11p.transx = -10:me.timerenabled = 1:End Sub
Sub SW11_Timer():Sw11p.transx = 0:me.timerenabled = 0:End Sub

Sub SW12_Hit():vpmTimer.PulseSw 12:Sw12p.transx = -10:me.timerenabled = 1:End Sub
Sub SW12_Timer():Sw12p.transx = 0:me.timerenabled = 0:End Sub

Sub SW54_Hit():vpmTimer.PulseSw 54:Sw54p.transx = -10:me.timerenabled = 1:gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1:End Sub
Sub SW54_Timer():Sw54p.transx = 0:me.timerenabled = 0:End Sub

Sub SW55_Hit():vpmTimer.PulseSw 55:Sw55p.transx = -10:me.timerenabled = 1:gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1:End Sub
Sub SW55_Timer():Sw55p.transx = 0:me.timerenabled = 0:End Sub


Sub Magnet1_Hit()
If NOT ActiveBall.Uservalue = 666 Then
Mag1.AddBall ActiveBall
End If
End Sub

Sub Magnet1_UnHit()
Mag1.RemoveBall ActiveBall
End Sub

Sub Magnet2_Hit()
If NOT ActiveBall.Uservalue = 666 Then
Mag2.AddBall ActiveBall
End If
End Sub

Sub Magnet2_UnHit()
Mag2.RemoveBall ActiveBall
End Sub


'Subs for Various Things.

Sub SolTrough(Enabled)
	If Enabled Then
		SW21.kick 37,30
		vpmTimer.PulseSw 22
        PlaySoundAt SoundFX("ballrelease",DOFContactors), sw21
        BIP = BIP + 1
	End If
 End Sub

Sub Bumper1_Hit
	PlaySoundAtVol SoundFX("fx_bumper4",DOFContactors), Bumper1, VolBump
	'B1L1.State = 1:B1L2. State = 1
	Me.TimerEnabled = 1
    vpmTimer.PulseSw 30
End Sub

Sub Bumper1_Timer
	'B1L1.State = 0:B1L2. State = 0
	Me.Timerenabled = 0
End Sub

Sub Bumper2_Hit
	PlaySoundAtVol SoundFX("fx_bumper4",DOFContactors), Bumper2, VolBump
	'B2L1.State = 1:B2L2. State = 1
	Me.TimerEnabled = 1
    vpmTimer.PulseSw 31
End Sub

Sub Bumper2_Timer
	'B2L1.State = 0:B2L2. State = 0
	Me.Timerenabled = 0
End Sub

Sub Bumper3_Hit
	PlaySoundAtVol SoundFX("fx_bumper4",DOFContactors), Bumper3, VolBump
	'B3L1.State = 1:B3L2. State = 1
	Me.TimerEnabled = 1
    vpmTimer.PulseSw 32
End Sub

Sub Bumper3_Timer
	'B3L1.State = 0:B3L2. State = 0
	Me.Timerenabled = 0
End Sub

Sub SolLFLIPPER(enabled)
  If enabled Then
    LeftFlipper.RotateToEnd
    PlaySoundAtVol SoundFX("fx_flipperup",DOFContactors), LeftFlipper, VolFlip
  else
    LeftFlipper.RotateToStart

    PlaySoundAtVol SoundFX("fx_flipperdown",DOFContactors), LeftFlipper, VolFlip
  End If
End Sub

Sub SolRFLIPPER(enabled)
  If enabled Then
    RightFlipper.RotateToEnd
    PlaySoundAtVol SoundFX("fx_flipperup",DOFContactors), RightFlipper, VolFlip
  else
    RightFlipper.RotateToStart
    PlaySoundAtVol SoundFX("fx_flipperdown",DOFContactors), RightFlipper, VolFlip
  End If
End Sub

Sub SolLUD(enabled)
  If enabled Then
    LUD.isdropped = 0
    PlaySoundAtVol SoundFX("fx_solenoid",DOFContactors), Primitive96, 1
  else
    LUD.isdropped = 1
  End If
End Sub

Sub SolRUD(enabled)
  If enabled Then
    RUD.isdropped = 0
    PlaySoundAtVol SoundFX("fx_solenoid",DOFContactors), Primitive97, 1
  else
    RUD.isdropped = 1
  End If
End Sub

Sub SolCUD(enabled)
  If enabled Then
    CUD.isdropped = 0
    PlaySound SoundFX("fx_solenoid",DOFContactors) ' TODO
    CUD_Lamp.State = 2
  else
    CUD.isdropped = 1
    CUD_Lamp.State = 0
  End If
End Sub

Sub MagLeft(enabled)
  If enabled Then
    Mag1.MagnetOn = 1
  else
    Mag1.MagnetOn = 0
  End If
End Sub

Sub MagRight(enabled)
  If enabled Then
    Mag2.MagnetOn = 1
  else
    Mag2.MagnetOn = 0
  End If
End Sub

Sub SolControlGate(enabled)
  If enabled Then
    UCG.open = 1
    PlaySoundAtVol SoundFX("fx_solenoid",DOFContactors), UCG, 1
  else
    UCG.open = 0
  End If
End Sub

Sub SolCenterLock(enabled)
  If enabled Then
    BL1.isdropped = 0
    BL2.isdropped = 0
    PlaySoundAtVol SoundFX("fx_solenoid",DOFContactors), Ronny, 1
  End If
End Sub

Sub SolCenterLockLatch(enabled)
  If enabled Then
    BL1.isdropped = 0
    BL2.isdropped = 0
  else
    BL1.isdropped = 1
    BL2.isdropped = 1
  End If
End Sub

Sub SolAutoFire(enabled)
  If enabled Then
    Plunger1.Fire
    PlaySoundAtVol SoundFX("fx_solenoid",DOFContactors), Plunger, 1
  else
    Plunger1.Pullback
  End If
End Sub

Sub Drain_Hit()
  If ActiveBall.Uservalue = 666 Then
    me.destroyball
    Set wball = Kicker_Load.createball
    wball.image = "Powerball2"
    wball.Uservalue = 666
    Kicker_Load.kick 45,5
    PlaySoundAt "drain", drain
    BIP = BIP - 1
    Drain.enabled = 0
    TDrain.enabled = 1
    If BIP < 1 Then
      Gi_Off
    End If
  else
    me.destroyball
    Kicker_Load.createball
    Kicker_Load.kick 45,5
    PlaySoundAt "drain", drain
    BIP = BIP - 1
    Drain.enabled = 0
    TDrain.enabled = 1
    If BIP < 1 Then
      Gi_Off
    End If
  End If
End Sub

Sub TDrain_Timer()
  Drain.enabled = 1
  me.enabled = 0
End Sub

Sub UpdateFlipperLogos
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
if L60.state=1 then P60.blenddisablelighting=10
if L60.state=0 then P60.blenddisablelighting=0
if L61.state=1 then P61.blenddisablelighting=10
if L61.state=0 then P61.blenddisablelighting=0
if L62.state=1 then P62.blenddisablelighting=10
if L62.state=0 then P62.blenddisablelighting=0
End Sub


'Mick Left / Right Direction Handler.

Sub MickRelayLeft2(enabled)
If enabled Then
'LL.State = 1
micky_left.enabled = 1
else
'LL.State = 0
micky_left.enabled = 0
End If
End Sub

Sub MickRelayRight2(enabled)
If enabled Then
micky_right.enabled = 1
'LR.State = 1
else
'LR.State = 0
micky_right.enabled = 0
End If
End Sub

Sub micky_right_timer()
Mick_Prim.RotZ = Mick_Prim.RotZ + 1
If Mick_Prim.RotZ > 38 then Mick_Prim.RotZ = 38
End Sub

Sub micky_left_timer()
Mick_Prim.RotZ = Mick_Prim.RotZ - 1
If Mick_Prim.RotZ < -30 then Mick_Prim.RotZ = -30
End Sub

'Shake the Primitive when the ball hits the wall behind Mick.

Dim mshake

Sub micky_shake_timer()
Select Case mshake
Case 1:Mick_Prim.TransY = -15:mshake = 2
Case 2:Mick_Prim.TransY = 0:mshake = 3
Case 3:Mick_Prim.TransY = -7:mshake = 4
Case 4:Mick_Prim.TransY = 0:me.enabled = 0
End Select
End Sub

'Mick on a Stick (TM) :) Watchdog Timer.

Sub Move_Mick_Timer()
If Mick_Prim.RotZ => 35 AND Mick_Prim.RotZ <= 38 Then
Controller.Switch(33) = 1
MW1.isdropped = 0
MW1a.isdropped = 0
else
Controller.Switch(33) = 0
MW1.isdropped = 1
MW1a.isdropped = 1
End If
If Mick_Prim.RotZ => 23 AND Mick_Prim.RotZ <= 26 Then
Controller.Switch(34) = 1
MW2.isdropped = 0
MW2a.isdropped = 0
else
Controller.Switch(34) = 0
MW2.isdropped = 1
MW2a.isdropped = 1
End If
If Mick_Prim.RotZ => 12 AND Mick_Prim.RotZ <= 15 Then
Controller.Switch(35) = 1
MW3.isdropped = 0
MW3a.isdropped = 0
else
Controller.Switch(35) = 0
MW3.isdropped = 1
MW3a.isdropped = 1
End If
If Mick_Prim.RotZ => 1  AND Mick_Prim.RotZ <= 4 Then
Controller.Switch(36) = 1
MW4.isdropped = 0
MW4a.isdropped = 0
else
Controller.Switch(36) = 0
MW4.isdropped = 1
MW4a.isdropped = 1
End If
If Mick_Prim.RotZ => -9 AND Mick_Prim.RotZ <= -6 Then
Controller.Switch(39) = 1
MW_Park.isdropped = 0
MW_Parka.isdropped = 0
else
Controller.Switch(39) = 0
MW_Park.isdropped = 1
MW_Parka.isdropped = 1
End If
If Mick_Prim.RotZ => -18 AND Mick_Prim.RotZ <= -15 Then
Controller.Switch(37) = 1
MW5.isdropped = 0
MW5a.isdropped = 0
else
Controller.Switch(37) = 0
MW5.isdropped = 1
MW5a.isdropped = 1
End If
If Mick_Prim.RotZ => -30 AND Mick_Prim.RotZ <= -27 Then
Controller.Switch(38) = 1
MW6.isdropped = 0
MW6a.isdropped = 0
else
Controller.Switch(38) = 0
MW6.isdropped = 1
MW6a.isdropped = 1
End If
End Sub

'Mick on a Stick Target Handler.

Sub MW1_Hit()
vpmTimer.PulseSw 72
mshake = 1
micky_shake.enabled = 1
gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1
End Sub

Sub MW2_Hit()
vpmTimer.PulseSw 72
mshake = 1
micky_shake.enabled = 1
gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1
End Sub

Sub MW3_Hit()
vpmTimer.PulseSw 72
mshake = 1
micky_shake.enabled = 1
gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1
End Sub

Sub MW4_Hit()
vpmTimer.PulseSw 72
mshake = 1
micky_shake.enabled = 1
gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1
End Sub

Sub MW5_Hit()
vpmTimer.PulseSw 72
mshake = 1
micky_shake.enabled = 1
gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1
End Sub

Sub MW_Park_Hit()
vpmTimer.PulseSw 72
mshake = 1
micky_shake.enabled = 1
gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1
End Sub

Sub MW6_Hit()
vpmTimer.PulseSw 72
mshake = 1
micky_shake.enabled = 1
gflash = 1:gi_flash.enabled = 1:gi_flash_stop.enabled = 1
End Sub


'Flasher Handlers - Launches A Timer to work around the constant state ON SAM issue.

Dim f20time

Sub Sol20(enabled)
If enabled Then
F20.state = 1
'Mick_PRIM.image = "Mick_Bright"
f20time = 1
Fade20.enabled = 1
else
F20.state = 0
Fade20.enabled = 0
'Mick_PRIM.image = "Mick"
End If
End Sub

Sub Fade20_Timer()
Select Case f20time
Case 1:
F20.state = 0
'Mick_PRIM.image = "Mick"
f20time = 2
Case 2:
F20.state = 1
'Mick_PRIM.image = "Mick_Bright"
f20time = 1
End Select
End Sub

Dim f21time

Sub Sol21(enabled)
If enabled Then
F21.state = 1
'Mick_PRIM.image = "Mick_Bright"
f21time = 1
Fade21.enabled = 1
else
F21.state = 0
'Mick_PRIM.image = "Mick"
Fade21.enabled = 0
End If
End Sub

Sub Fade21_Timer()
Select Case f21time
Case 1:
F21.state = 0
'Mick_PRIM.image = "Mick"
f21time = 2
Case 2:
F21.state = 1
'Mick_PRIM.image = "Mick_Bright"
f21time = 1
End Select
End Sub

Sub Sol31(enabled)
If enabled Then
f31.state = 1
else
f31.state = 0
End If
End Sub

Sub Sol26(enabled)
If enabled Then
Light26.state = 1
Light26a.state = 1
else
Light26.state = 0
Light26a.state = 0
End If
End Sub

Dim f25time

Sub Sol25(enabled)
If enabled Then
F25.state = 1
F25a.state = 1
F25b.state = 1
F25pf.state = 1
'Mick_PRIM.image = "Mick_Blue"
f25time = 1
Fade25.enabled = 1
else
F25.state = 0
F25a.state = 0
F25b.state = 0
F25pf.state = 0
'Mick_PRIM.image = "Mick"
Fade25.enabled = 0
End If
End Sub

Sub Fade25_Timer()
Select Case f25time
Case 1:
F25.state = 0
F25a.state = 0
F25b.state = 0
F25pf.state = 0
'Mick_PRIM.image = "Mick"
f25time = 2
Case 2:
F25.state = 1
F25a.state = 1
F25b.state = 1
F25pf.state = 1
'Mick_PRIM.image = "Mick_Blue"
f25time = 1
End Select
End Sub


Dim f27time

Sub Sol27(enabled)
If enabled Then
F27.state = 1
F27a.state = 1
F27b.state = 1
F27pf.state = 1
'Mick_PRIM.image = "Mick_Red"
f27time = 1
Fade27.enabled = 1
else
F27.state = 0
F27a.state = 0
F27b.state = 0
F27pf.state = 0
'Mick_PRIM.image = "Mick"
Fade27.enabled = 0
End If
End Sub

Sub Fade27_Timer()
Select Case f27time
Case 1:
F27.state = 0
F27a.state = 0
F27b.state = 0
F27pf.state = 0
'Mick_PRIM.image = "Mick"
f27time = 2
Case 2:
F27.state = 1
F27a.state = 1
F27b.state = 1
F27pf.state = 1
'Mick_PRIM.image = "Mick_Red"
f27time = 1
End Select
End Sub

Dim f29time

Sub Sol29(enabled)
If enabled Then
f29.state = 1
f29a.state = 1
f29time = 1
Fade29.enabled = 1
else
f29.state = 0
f29a.state = 0
Fade29.enabled = 0
End If
End Sub

Sub Fade29_Timer()
Select Case f29time
Case 1:
f29.state = 0
f29a.state = 0
f29time = 2
Case 2:
f29.state = 1
f29a.state = 1
f29time = 1
End Select
End Sub

Dim f28time

Sub Sol28(enabled)
If enabled Then
F28.state = 1
F28a.state = 1
F28b.state = 1
F28pf.state = 1
'Mick_PRIM.image = "Mick_Blue"
f28time = 1
Fade28.enabled = 1
else
F28.state = 0
F28a.state = 0
F28b.state = 0
F28pf.state = 0
'Mick_PRIM.image = "Mick"
Fade28.enabled = 0
End If
End Sub

Sub Fade28_Timer()
Select Case f28time
Case 1:
F28.state = 0
F28a.state = 0
F28b.state = 0
F28pf.state = 0
'Mick_PRIM.image = "Mick"
f28time = 2
Case 2:
F28.state = 1
F28a.state = 1
F28b.state = 1
F28pf.state = 1
'Mick_PRIM.image = "Mick_Blue"
f28time = 1
End Select
End Sub

Dim f22time

Sub Sol22(enabled)
If enabled Then
Light22.state = 1
Light22a.state = 1
Charlie.Image = "drummer_bright"
Keith.Image = "guitar1b_bright"
'Mick_PRIM.image = "Mick_Bright"
f22time = 1
Fade22.enabled = 1
else
Light22.state = 0
Light22a.state = 0
Fade22.enabled = 0
Charlie.Image = "drummer"
Keith.Image = "guitar1b"
'Mick_PRIM.image = "Mick"
End If
End Sub

Sub Fade22_Timer()
Select Case f22time
Case 1:
Light22.state = 0
Light22a.state = 0
Charlie.Image = "drummer"
Keith.Image = "guitar1b"
'Mick_PRIM.image = "Mick"

f22time = 2
Case 2:
Light22.state = 1
Light22a.state = 1
Charlie.Image = "drummer_bright"
Keith.Image = "guitar1b_bright"
'Mick_PRIM.image = "Mick_Bright"
f22time = 1
End Select
End Sub

Dim f23time

Sub Sol23(enabled)
If enabled Then
Light23.state = 1
Light23a.state = 1
Light23a1.state = 1
f23time = 1
Fade23.enabled = 1
'Mick_PRIM.image = "Mick_Bright"
Ronny.image = "Guitar2_Bright"
else
Light23.state = 0
Light23a.state = 0
Light23a1.state = 0
Fade23.enabled = 0
'Mick_PRIM.image = "Mick"
Ronny.image = "Guitar2"
End If
End Sub

Sub Fade23_Timer()
Select Case f23time
Case 1:
Light23.state = 0
Light23a.state = 0
Light23a1.state = 0
Ronny.image = "Guitar2"
f23time = 2
Case 2:
Light23.state = 1
Light23a.state = 1
Light23a1.state = 1
f23time = 1
Ronny.image = "Guitar2_Bright"
End Select
End Sub

'****Targets

Sub RRD_Hit()
  PlaySoundAtVol "Ball_Bounce", RRD, 1
  gi_on
End Sub

Sub LRD_Hit()
  PlaySoundAtVol "Ball_Bounce", LRD, 1
  gi_on
End Sub


' Virtual Spring - The real game has a small spring just to the left of the back entrance to the ball lock.
' If the ball is moving fast enough it will overpower and pass by the spring into the top lanes, if it is moving too slow the spring will
' knock the ball to the right down the back entrance to the ball lock.

Dim virtdir
Sub Virt_Setup_Hit()
virtdir = 1
virt_reset.enabled = 1
End Sub

Sub Virt_Reset_Timer()
virtdir = 0
End Sub

Sub Virt_Spring_Hit()
If ActiveBall.VelX > -10 AND Virtdir = 1 Then
ActiveBall.VelX = 2
End If
End Sub

'Manual Trough System (Switches) to handle the White Ceramic Ball
'and activate the White Ball Detection Opto.

Sub SW21_Hit()
If ActiveBall.Uservalue = 666 Then
Controller.Switch(71) = 0
End If
Controller.Switch(21) = 1
W21.isdropped = 0
End Sub

Sub SW21_Unhit()
W21.isdropped = 1
Controller.Switch(21) = 0
End Sub

Sub SW20_Hit()
W20.isdropped = 0
Controller.Switch(20) = 1
End Sub

Sub SW20_Unhit()
W20.isdropped = 1
Controller.Switch(20) = 0
End Sub

Sub SW19_Hit()
W19.isdropped = 0
Controller.Switch(19) = 1
End Sub

Sub SW19_Unhit()
W19.isdropped = 1
Controller.Switch(19) = 0
End Sub

Sub SW18_Hit()
W18.isdropped = 0
Controller.Switch(18) = 1
End Sub

Sub SW18_Unhit()
W18.isdropped = 1
Controller.Switch(18) = 0
End Sub

Sub SW17_Hit()
'W17.isdropped = 0
Controller.Switch(17) = 1
End Sub

Sub SW17_Unhit()
'W17.isdropped = 1
Controller.Switch(17) = 0
End Sub

'Intial Load Timer to populate the Trough.

Dim wball,tball
tball = 1

Sub load_trough_timer()
Select Case tball
Case 1:Kicker_Load.createball:Kicker_Load.kick 45,5:tball = 2
Case 2:Kicker_Load.createball:Kicker_Load.kick 45,5:tball = 3
Case 3:Set wball = Kicker_Load.createball:wball.image = "powerball2":wball.Uservalue = 666:Kicker_Load.kick 45,5:tball = 4
Case 4:Kicker_Load.createball:Kicker_Load.kick 45,5:tball = 5
Case 5:Kicker_Load.createball:Kicker_Load.kick 45,5:tball = 6
Case 6:me.enabled = 0
End Select
End Sub

'*****GI Lights On/Off Subs

dim rsxx

For each rsxx in GI:rsxx.State = 1: Next

Sub GI_on()
For each rsxx in GI:rsxx.State = 1: Next
Table1.colorgradeimage = "ColorGradeLUT256x16_ConSat"
LFLogo.image = "flipper-l2"
RFLogo.image = "flipper-r2"
'Mick_PRIM.image = "Mick_Bright"
Mick_PRIM.blenddisablelighting = 0.25
Wall4.blenddisablelighting = 0.2
Charlie.blenddisablelighting = 0.1
Keith.blenddisablelighting = 0.1
Ronny.blenddisablelighting = 0.1
'Flasher1.visible=1
'Flasher2.visible=1
Flasher001.color = rgb(97,97,97)
Flasher002.color = rgb(150,150,150)
Flasher003.color = rgb(120,120,120)
End Sub

Sub GI_off()
For each rsxx in GI:rsxx.State = 0: Next
Table1.colorgradeimage = "ColorGrade_4"
LFLogo.image = "flipper-l2d"
RFLogo.image = "flipper-r2d"
'Mick_PRIM.image = "Mick_Dark"
Mick_PRIM.blenddisablelighting = 0.1
Wall4.blenddisablelighting = 0
Charlie.blenddisablelighting = 0
Keith.blenddisablelighting = 0
Ronny.blenddisablelighting = 0
'Flasher1.visible=0
'Flasher2.visible=0
Flasher001.color = rgb(48,48,48)
Flasher002.color = rgb(48,48,48)
Flasher003.color = rgb(48,48,48)
End Sub

Dim gflash

Sub GI_Flash_Timer()
Select Case gflash
Case 1:gi_on:gflash = 2
Case 2:gi_off:gflash = 1
End Select
End Sub

Sub Gi_Flash_Stop_Timer()
gi_flash.enabled = 0
gi_on
me.enabled = 0
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    PlaySoundAt SoundFX("left_slingshot",DOFContactors), sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    RightSlingShot.TimerInterval = 10
    vpmTimer.PulseSw 27
	'gi1.State = 0:Gi2.State = 0
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    PlaySoundAt SoundFX("right_slingshot",DOFContactors), sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    LeftSlingShot.TimerInterval = 10
    vpmTimer.PulseSw 26
	'gi3.State = 0:Gi4.State = 0
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolRPi, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetals, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetals, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetals, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rollovers_Hit (idx)
	PlaySound "rollover", 0, Vol(ActiveBall)*VolRol, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRB, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRB, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRB, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRB, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRB, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*VolFlip, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*VolFlip, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*VolFlip, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
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

Sub PlaySoundAtVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioFade(ball) ' Can this be together with the above function ?
  Dim tmp
  tmp = ball.y * 2 / Table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 8 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
   Dim BOT, b
    BOT = GetBalls

	' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

    For b = 0 to UBound(BOT)
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/10, AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )/15, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
 ' play ball drop sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

Sub RealTime_timer
UpdateFlipperLogos
BallShadowUpdate
End Sub

Sub BallShadowUpdate()
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8)
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
if BOT(b).z > 30 Then 
ballShadow(b).height = BOT(b).Z - 20
ballShadow(b).opacity = 80
Else
ballShadow(b).height = BOT(b).Z - 24
ballShadow(b).opacity = 90
End If
    Next	
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


' Thalamus : Exit in a clean and proper way
Sub Table1_exit()
  Controller.Pause = False
  Controller.Stop
End Sub