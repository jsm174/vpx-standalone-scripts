' Twister / IPD No. 3976 / April, 1996 / 6 Players
' Original Twister Sega Table build by 32assassin and francisco666 with code rebuild by 32assassin and arngrim scripted DOF and test
' based on Twister Sega by xio, TheWool and JimmyFingers on VP, francisco666 on FP; Contains zany models of bumper caps

' Rebuild and modification done by DJRobX on version prior to 1.0.

' UnclePaulie did various VR Rooms on versions 1.0 through 1.25 as well as Fleep sounds and early VPW physics
' UnclePaulie updates to version 2.0 to include latest VPW sounds, physics, lighting, Lampz, bumpers, shadows, ball and multiball trough and gBOT performance, VR fan, and lots of other fixes reported


'******** Revisions done by UnclePaulie on Hybrid version 1.0 - 2.0 *********
' ALL Modifications documented at the bottom of the script.


Option Explicit
Randomize

Dim TargetsdeVPX, GILightsColor, decalonflippers, Primitivetransplastics, FanSound, FanSoundLevel

'*******************************************
' Desktop, Cab, and VR OPTIONS
'*******************************************

' Desktop, Cab, and VR Room are automatically selected.  However if in VR Room mode, you can change the environment with the magna save buttons.

'Gi and bumper lights option: 0 = normal white, 1 = blue.
GILightsColor = 0 

const BallLightness = 1 '0 = dark, 1 = not as dark, 2 = bright, 3 = brightest 
'const cabsideblades = 1 '0 = off, 1 = on;  some users want sideblades in the cabinet, some don't.

'Flipper decal image: 0 = standard flippers or 1 = flippers with decal
decalonflippers = 0

'set 0 to turn off fan sound if you have a real fan (automatic if you also have contactors)
FanSound = 1
FanSoundLevel = 0.5 'Level of the sound for the fan if chosen (0-1)

' Add playfield shadow map
const PFShadow = 1 ' 0 = off, 1 = On

'set 0 for primitives non visibles and 1 to make them visibles instead of VPX surfaces (recommendation is "1")  
Primitivetransplastics=1


' *** If using VR Room:

const WallClock = 1	  		'1 Shows the clock in the VR minimal rooms only
const VRFanON = 1			'1 Uses the VR Fan; 0 = turns off (if you turn off, you can instead use the topper)
const topper = 0			'0 = Off 1= On - Topper Picture visible in VR Room only  (NOTE:  this is only a simple picture, not the Fan: and disabled if VRFanON is selected)
const poster = 1		 	'1 Shows the flyer posters in the VR room only 
const poster2 = 1		 	'1 Shows the flyer posters in the VR room only 
const VRGI = 0				'1 Dims the backglass with GI changes.  Real table is constantly lit, no dimming: so default is "0".
const DisableVRSelector = 0	'Diables the ability to change the VR room option with the magna saves in your game when set to 1
'						' ** NOTE ** will need to ensure the VR selector is on the first time you play at least, so you can select your VR room.

'VR Room Options you can change via magnasave buttons ONLY in VR mode

' ******  NOTE:  it will default to desktop mode the first time you play.  Use magna save buttons (or control keys) to select VR Room.  ******

	' 0 - VR Room off (desktop mode, or same as ultra minimal)
	' 1 - Minimal Room
	' 2 - Minimal with modern home walls
	' 3 - Minimal with Tornado Walls
	' 4 - VR Sphere Room w/ 360 Pano image of field/farmstead
	' 5 - VR Sphere Room with a cool nasty storm pasted into a sphere (not a 360 pano)
	' 6 - VR Sphere Room with another storm pasted in (not a 360 pano)

' ****************************************************

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's)
									'2 = flasher image shadow, but it moves like ninuzzu's

' *** These define the appearance of shadows in your table
'Ambient (Room light source)
Const AmbientBSFactor         = 0.9    '0 to 1, higher is darker
Const AmbientMovement        = 2        '1 to 4, higher means more movement as the ball moves left and right
Const offsetX                = 0        'Offset x position under ball    (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY                = 5        'Offset y position under ball     (for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor         = 0.95    '0 to 1, higher is darker
Const Wideness                = 20    'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness                = 5        'Sets minimum as ball moves away from source

'----- General Sound Options -----
Const VolumeDial = 0.8				'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'Level of ramp rolling volume. Value between 0 and 1

'----- Phsyics Mods -----
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7


' ****************************************************

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0


'*******************************************
' Constants and Global Variables
'*******************************************


Dim UseVPMDMD, VRRoom, VR_Room, cab_mode, DesktopMode: DesktopMode = table1.ShowDT
If RenderingMode = 2 Then VR_Room=1 Else VR_Room=0      'VRRoom set based on RenderingMode in version 10.72
If Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0


Const UsingROM = True				'The UsingROM flag is to indicate code that requires ROM usage. Mostly for instructional purposes only.

Const BallSize = 50
Const BallMass = 1
Const tnob = 5 ' total number of balls
Const lob = 0   'number of locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim BIPL : BIPL = False				'Ball in plunger lane


Const cGameName="twst_405",UseSolenoids=2,UseLamps=0,UseGI=0

if VR_Room = 1 Then
	LoadVRRoom
End If

If VR_Room = 1 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode

LoadVPM "01120100", "SEGA.VBS", 3.02    


'********************************************* 
' CODE BELOW IS FOR THE Playfield Shadow Map 
'*********************************************

if PFShadow = 1 Then
	PF_shadow.visible = 1
Else
	PF_shadow.visible = 0
End If

'**********************************************************************************************************
'Solenoids
'**********************************************************************************************************

SolCallBack(1) = "SolBallRelease"
SolCallback(2) = "Auto_Plunger"
SolCallback(3) = "DropTargetReset"
SolCallback(4) = "DropTargetDown"
SolCallback(5) = "SolFan"'Fan Motor Relay
SolCallback(6) = "SolSpinWheelsMotor"
SolCallback(8) = "SolKnocker"
SolCallback(14) = "SolMBKicker"
SolCallback(17) = "SolTroughLock"
SolCallBack(22)= "Flash22"
SolCallBack(23)= "Flash23"
SolCallBack(25)= "Flash25" 'F1 s125a, b, F125b1, b1
SolCallBack(26)= "Flash2"  'F2 s126
SolCallBack(27)= "Flash27" 'F3 s127a, s127b
SolCallBack(28)= "Flash28" 'F4 s128a, a1, b, b1, c, c1
SolCallBack(29)= "Flash5"  'F5 s129
SolCallBack(30)= "Flash6"  'F6 s130
SolCallBack(31)= "Flash7"  'F7
SolCallBack(32)= "Flash8"  'F8
SolCallback(33)= "MagnetDiverter.MagnetOn="
SolCallback(34)= "SolDiscMagnet"


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolDiscMagnet(Enabled)
	magnetdisco.MagnetOn=Enabled
	if not Enabled Then	
		magnetdisco.GrabCenter = False
	end if
End Sub

'*******************************************
'  Timers
'*******************************************


' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
	DoDTAnim 						'handle drop target animations
	DoSTAnim						'handle stand up target animations
End Sub


' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	UpdateBallBrightness
End Sub


' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
End Sub


'*******************************************
' Flippers 
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub


'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

Sub Auto_Plunger(Enabled)
    If Enabled Then
        PlungerIM.AutoFire
		PlungerIM.random 2
		PlungerIM.strength = 30
    End If
End Sub   

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub

'******************************************************
'						FAN
'******************************************************


Sub SolFan(Enabled)
If enabled Then
	If FanSound=1 Then
		PlaySoundAtLevelStatic ("Topper_Fan"), FanSoundLevel, BackwallFanPosition
	End If
	If VR_Room =1 AND VRFanON = 1then BlowerTimer.enabled = true
Else
	If FanSound = 1 Then
		stopsound "Topper_Fan"
	end If
	If VR_Room = 1 AND VRFanON = 1 then BlowerRampDownTimer.enabled = true
End If

End Sub

Dim VRFanSpeed
VRFanSpeed = 15

sub BlowerTimer_Timer()
	VR_Fan.RotY = VR_Fan.RotY + VRFanSpeed
end sub

sub BlowerRampDownTimer_Timer()
	BlowerTimer.interval = BlowerTimer.interval + 5
	VRFanSpeed = VRFanSpeed - 1
	if BlowerTimer.interval > 100 then 
		BlowerTimer.enabled = false
		BlowerTimer.interval = 11
		VRFanSpeed = 15
		BlowerRampDownTimer.enabled = false
		exit sub
	end If
End Sub


'*******************************************
' GI
'*******************************************

dim gilvl:gilvl = 1

set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	If Enabled Then
		dim xx
		For each xx in GI:xx.State = 1:	Next
		gilvl=1
		Sound_GI_Relay 1, Relay_GI
		SetLamp 101, 1 'Controls GI
		SetLamp 111, 1 'Prim and Ball GI Updates
		if VRGI = 1 Then
			PinCab_Backglass.BlendDisableLighting = 1.5
			PinCab_Backglass.image = "BackglassVRIlluminated"
		end If
	Else 
		For each xx in GI:xx.State = 0: Next
		gilvl=0
		Sound_GI_Relay 0, Relay_GI
		SetLamp 101, 0 'Controls GI
		SetLamp 111, 0 'Prim and Ball GI Updates
		if VRGI = 1 Then
			PinCab_Backglass.BlendDisableLighting = 0.5
			PinCab_Backglass.image = "BackglassVRDark"
		end If
	End If
End Sub


'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim TSPINA, magnetdisco, MagnetDiverter

Dim gBOT, tBall1, tBall2, tBall3,tBall4, tBall5, bl41, bl42, bl43, bl44, bl45

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Sega Twister"&chr(13)&"by UnclePaulie"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
		.Games(cGameName).Settings.Value("sound") = 1 ' Set sound (0=OFF, 1=ON)	
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled  = True

 ' Nudging
	vpmNudge.TiltSwitch=56
	vpmNudge.Sensitivity=2
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)


'Set the GI and bumper lights color
	select case GILightsColor 
		case 0 'Normal
			changetonormal
		case 1 'Blue
			changetoblue
	end Select
	
'Flippers
	if decalonflippers = 0 Then
		RightFlipper.image = "flipper_w_red_Right"
		LeftFlipper.image = "flipper_w_red_Right"
	end if

'Surfaces
	Primitive22.visible = Primitivetransplastics

	dim plast
	For each plast in transparentsurfaces : plast.visible = (-Primitivetransplastics + 1 ) : Next

'Ball initializations needed for physical trough

	Set tBall1 = Drain.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set tBall2 = sw11.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set tBall3 = sw12.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set tBall4 = sw13.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set tBall5 = sw14.CreateSizedballWithMass(BallSize/2,Ballmass)
	gBOT = Array(tBall1, tBall2, tBall3, tBall4, tBall5)

' Trough Switches and Multiball Ball lock variables
	Controller.Switch(10) = 1
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1
	Controller.Switch(14) = 1
	Controller.Switch(41) = 0
	Controller.Switch(42) = 0
	Controller.Switch(43) = 0
	Controller.Switch(44) = 0
	Controller.Switch(45) = 0

' ball lock variables to control multiball trough
	bl41 = 0
	bl42 = 0
	bl43 = 0
	bl44 = 0
	bl45 = 0

	Set TSPINA = New cvpmTurntable
		TSPINA.InitTurntable SpinTrigger, 25
		TSPINA.SpinDown = 10
		TSPINA.CreateEvents "TSPINA"

	Set magnetdisco = New cvpmMagnet
		magnetdisco.InitMagnet iman, 15
		magnetdisco.strength = 10
		magnetdisco.GrabCenter = false
		magnetdisco.MagnetOn = False
		magnetdisco.CreateEvents "magnetdisco"


	Set MagnetDiverter=new cvpmMagnet
		MagnetDiverter.initMagnet TrMagDiv,20

	PinCab_Backglass.BlendDisableLighting = 1.5
	PinCab_Backglass.image = "BackglassVRIlluminated"

	if VR_Room = 1 Then
		VRChangeRoom
	End If

' initialize the attirbutes of the bumpers
	initbumpers

end sub


Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit
	if VR_Room = 1 Then
		SaveVRRoom
	End If
	Controller.stop
End Sub


'**********************************************************************************************************
' Keys and Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If keycode = PlungerKey Then 
		Plunger.Pullback : SoundPlungerPull
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter

	If keycode = StartGameKey Then 
		SoundStartButton
		StartButton.y = 898.5 - 4
	End If

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		VR_CabFlipperLeft.X = VR_CabFlipperLeft.X +10
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		VR_CabFlipperRight.X = VR_CabFlipperRight.X - 10
	End If

' Change VR Room with magna-save buttons; only in VR mode

	if VR_Room = 1 Then
		if DisableVRSelector = 0 then
			If keycode = leftmagnasave then
				vrroom = vrroom -1
			if vrroom < 0 then vrroom = 7
				VRChangeRoom
			End If

			If keycode = rightmagnasave then
				vrroom = vrroom +1
			if vrroom > 7 then vrroom = 0
				VRChangeRoom
			End If
		End If
	End If

    If vpmKeyDown(keycode) Then Exit Sub

End Sub

Sub Table1_KeyUp(ByVal KeyCode)

	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		VR_Primary_plunger.Y = -75

	End If
	If keycode = LeftFlipperKey Then
		FlipperDeActivate LeftFlipper, LFPress
		VR_CabFlipperLeft.X = VR_CabFlipperLeft.X -10	
	End If
	
	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		VR_CabFlipperRight.X = VR_CabFlipperRight.X +10
	End If

	If keycode = StartGameKey Then
		StartButton.y = 898.5
	End If

	If KeyUpHandler(keycode) Then Exit Sub

End Sub

	Dim plungerIM
    Const IMPowerSetting = 24 'Plunger Power
    Const IMTime = 0.6        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 0.3
        .switch 16
        .CreateEvents "plungerIM"
    End With

Sub TimerPlunger_Timer
  If VR_Primary_plunger.Y < 30 then
  		VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
  VR_Primary_plunger.Y = -75 + (5* Plunger.Position) -20
End Sub


'********************************************
' Drain hole and saucer kickers
'********************************************

' Main Trough

Sub Drain_Hit():Controller.Switch(10) = 1:RandomSoundDrain Drain:UpdateTrough:End Sub
Sub Drain_UnHit():Controller.Switch(10) = 0:UpdateTrough:End Sub
Sub sw11_Hit():Controller.Switch(11) = 1:UpdateTrough:End Sub
Sub sw11_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub
Sub sw12_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub sw12_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub sw13_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub sw13_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub
Sub sw14_Hit():Controller.Switch(14) = 1:UpdateTrough:End Sub
Sub sw14_UnHit():Controller.Switch(14) = 0:UpdateTrough:End Sub
Sub sw15_Hit():Controller.Switch(15) = 1:UpdateTrough:End Sub
Sub sw15_UnHit():Controller.Switch(15) = 0:UpdateTrough:End Sub

Sub UpdateTrough
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer
	If sw14.BallCntOver = 0 Then sw13.kick 60, 9
	If sw13.BallCntOver = 0 Then sw12.kick 60, 9
	If sw12.BallCntOver = 0 Then sw11.kick 60, 9
	If sw11.BallCntOver = 0 Then drain.kick 60, 12
	Me.Enabled = 0
End Sub

Sub SolBallRelease(Enabled)
	If Enabled Then
		RandomSoundBallRelease sw15
		sw15.Kick 70,12
	End If
End Sub

Sub SolTroughLock(Enabled)
	If Enabled Then
		sw14.Kick 60,12
		UpdateTrough
	End If
End Sub


' MultiBall Trough

Sub sw45_Hit()
	Controller.Switch(45) = 1
	bl45 = 1
	UpdateMBTrough
End Sub

Sub sw41_Hit()
	If bl45 = 1 Then
		Controller.Switch(41) = 1
		bl41 = 1
	End If
	UpdateMBTrough
End Sub

Sub sw42_Hit()
	If bl41 = 1 Then
		Controller.Switch(42) = 1
		bl42 =1 
	End If
	UpdateMBTrough
End Sub

Sub sw43_Hit()
	If bl42 = 1 Then
		Controller.Switch(43) = 1
		bl43 = 1
	End If
	UpdateMBTrough
End Sub

Sub sw44_Hit()
	If bl43 = 1 Then
		Controller.Switch(44) = 1
		bl44 = 1
	End If
	UpdateMBTrough
End Sub

Sub sw41_UnHit():Controller.Switch(41) = 0:bl41=0:UpdateMBTrough:End Sub
Sub sw42_UnHit():Controller.Switch(42) = 0:bl42=0:UpdateMBTrough:End Sub
Sub sw43_UnHit():Controller.Switch(43) = 0:bl43=0:UpdateMBTrough:End Sub
Sub sw44_UnHit():Controller.Switch(44) = 0:bl44=0:UpdateMBTrough:End Sub
Sub sw45_UnHit():Controller.Switch(45) = 0:bl45=0:UpdateMBTrough:End Sub


Sub UpdateMBTrough
	UpdateMBTroughTimer.Interval = 100
	UpdateMBTroughTimer.Enabled = 1
End Sub

Sub UpdateMBTroughTimer_Timer
	If sw45.BallCntOver = 0 Then sw41.kick 340, 8
	If sw41.BallCntOver = 0 Then sw42.kick 340, 8
	If sw42.BallCntOver = 0 Then sw43.kick 340, 8
	If sw43.BallCntOver = 0 Then sw44.kick 340, 8
	Me.Enabled = 0
End Sub

Sub SolMBKicker(Enabled)
	IF Enabled Then
		RandomSoundBallRelease sw45
		sw45.Kick 0,40, 1.56
		UpdateMBTrough
	End If
End Sub

'********************************************
'  Targets
'********************************************

'Drop Targets

Sub sw9_Hit
	DTHit 9
End Sub

Sub DropTargetReset(enabled)
	if enabled then
		RandomSoundDropTargetReset sw9p
		DTRaise 9
	end if
End Sub

Sub DropTargetDown(enabled)
	if enabled then
		SoundDropTargetDrop sw9p
		DTDrop 9
	end if
End Sub

'Wire Triggers
Sub sw16_Hit
	Controller.Switch(16) = 1 
	BIPL = True
End Sub 

Sub sw16_UnHit
	Controller.Switch(16) = 0
	BIPL = False
End Sub

Sub sw17_Hit:Controller.Switch(17) = 1 : End Sub 
Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub
Sub sw18_Hit:Controller.Switch(18) = 1 : End Sub 
Sub sw18_UnHit:Controller.Switch(18) = 0:End Sub
Sub sw19_Hit:Controller.Switch(19) = 1 : End Sub 
Sub sw19_UnHit:Controller.Switch(19) = 0:End Sub
Sub sw21_Hit:Controller.Switch(21) = 1 : End Sub 
Sub sw21_UnHit:Controller.Switch(21) = 0:End Sub
Sub sw23_Hit:Controller.Switch(23) = 1 : End Sub 
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub
Sub sw25_Hit:Controller.Switch(25) = 1 : End Sub 
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw57_Hit:Controller.Switch(57) = 1 : End Sub 
Sub sw57_UnHit:Controller.Switch(57) = 0:End Sub
Sub sw58_Hit:Controller.Switch(58) = 1 : End Sub 
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub sw60_Hit:Controller.Switch(60) = 1 : End Sub 
Sub sw60_UnHit:Controller.Switch(60) = 0:End Sub
Sub sw61_Hit:Controller.Switch(61) = 1 : End Sub 
Sub sw61_UnHit:Controller.Switch(61) = 0:End Sub


'Spinners
Sub sw20_Spin:vpmTimer.PulseSw 20 : SoundSpinner sw20 : End Sub
Sub sw22_Spin:vpmTimer.PulseSw 22 : SoundSpinner sw22 : End Sub

'Ramp Triggers sound by collection
Sub sw24_Hit:vpmTimer.PulseSw 24:End Sub 

Sub sw40_Hit:vpmTimer.PulseSw 40:End Sub 


'*******************************************
'  Ramp Triggers
'*******************************************
Sub Trigger5_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub Trigger2_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub Trigger2_unhit()
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub Trigger6_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub Trigger6_unhit()
	PlaySoundAt "WireRamp_Stop", Trigger6
End Sub

Sub Trigger4_hit()
	WireRampOn False ' On Wire Ramp Pay Wire Ramp Sound
End Sub

Sub Trigger1_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
	magnetdisco.GrabCenter = True
End Sub

Sub Trigger1_unhit()
	PlaySoundAt "WireRamp_Stop", Trigger1
End Sub


'Stand Up Targets sound by collection cardinal points
Sub sw26_Hit:vpmTimer.PulseSw 26:PlayTargetSound:TargetBouncer Activeball, 1:End Sub
Sub sw27_Hit:vpmTimer.PulseSw 27:PlayTargetSound:TargetBouncer Activeball, 1:End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30:PlayTargetSound:TargetBouncer Activeball, 1:End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:PlayTargetSound:TargetBouncer Activeball, 1:End Sub

'Mini Targets
Sub sw28_Hit:vpmTimer.PulseSw 28:PlayTargetSound:TargetBouncer Activeball, 1:End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:PlayTargetSound:TargetBouncer Activeball, 1:End Sub

'Round Targets
Sub sw33_Hit
	STHit 33
End Sub

Sub sw33o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw34_Hit
	STHit 34
End Sub

Sub sw34o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw35_Hit
	STHit 35
End Sub

Sub sw35o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw36_Hit
	STHit 36
End Sub

Sub sw36o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw37_Hit
	STHit 37
End Sub

Sub sw37o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw38_Hit
	STHit 38
End Sub

Sub sw38o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw39_Hit
	STHit 39
End Sub

Sub sw39o_Hit
	TargetBouncer Activeball, 1
End Sub

'*******************************************
' Bumpers Fleep sounds
'*******************************************

Sub Bumper1_Hit 
	vpmTimer.PulseSw(51) 
	RandomSoundBumperTop Bumper1
End Sub

Sub Bumper2_Hit
	vpmTimer.PulseSw(50) 
	RandomSoundBumperBottom Bumper2
End Sub

Sub Bumper3_Hit
	vpmTimer.PulseSw(49)
	RandomSoundBumperMiddle Bumper3
End Sub


'********************  Spinning Discs Animation Timer ****************************
Dim SpinnerMotorOff, SpinnerStep, ss

Sub SolSpinWheelsMotor(enabled)
	If enabled Then
		TSPINA.MotorOn = True
		SpinnerStep = 10
		SpinnerMotorOff = False
		SpinnerTimer.Interval = 10
		SpinnerTimer.enabled = True
	Else
		SpinnerMotorOff = True
		TSPINA.MotorOn = False
	end If
End Sub

Sub SpinnerTimer_Timer()
	If Not(SpinnerMotorOff) Then
		spina.ObjRotZ  = ss
		ss = ss + SpinnerStep
	Else
		if SpinnerStep < 0 Then
			SpinnerTimer.enabled = False
		Else
		'slow the rate of spin by decreasing rotation step
			SpinnerStep = SpinnerStep - 0.05
			
			spina.ObjRotZ  = ss
			ss = ss + SpinnerStep
		End If
	End If
	if ss > 360 then ss = ss - 360
End Sub


'MAGNET : DIVERTER (AT THE TOP OF THE PF)
Sub TrMagDiv_Hit
	Controller.Switch(32)=1
	ActiveBall.AngMomZ=0
	ActiveBall.AngMomY=0
	ActiveBall.AngMomX=0

	MagnetDiverter.AddBall ActiveBall
	MagnetDiverter.AttractBall ActiveBall
End Sub	

Sub TrMagDiv_unHit
	Controller.Switch(32)=0
	MagnetDiverter.RemoveBall ActiveBall
End Sub

'*************************************************************************
'*************************************************************************


'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 62
	RandomSoundSlingshotRight SLING1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 12
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 59
	RandomSoundSlingshotLeft SLING2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 12
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


Dim NextOrbitHit:NextOrbitHit = 0 


sub changetonormal
	dim luz
	For each luz in GI:luz.color = RGB(255,157,111):	Next
end sub 

sub changetoblue
	dim luz
	For each luz in GI:luz.color = RGB(0,0,255):	Next
end sub 


'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************


' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function


' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(5), objrtx2(5)
dim objBallShadow(5)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

Dim ClearSurface:ClearSurface = True		'Variable for hiding flasher shadow on wire and clear plastic ramps
									'Intention is to set this either globally or in a similar manner to RampRolling sounds

'Initialization
DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub


Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
		If Not ClearSurface Then
			BallShadowA(num).visible = 1
			objBallShadow(num).visible = 0
		Else
			objBallShadow(num).visible = 1
		End If
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update

				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					If gBOT(s).X < tablewidth/2 Then
						objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
					Else
						objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
					End If
					objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				End If

			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s

				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf

			Else												'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY
				End If
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=0.1
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y
					'objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + offsetY
					'objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer activeball, 1
End Sub



'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Early 90's and after
'
Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub



'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub

	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class


'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class


'******************************************************
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************


Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper2) Then
					gBOT(b).velx = gBOT(b).velx / 1.3
					gBOT(b).vely = gBOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST         
	Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b

		For b = 0 to UBound(gBOT)
			If Distance(gBOT(b).x, gBOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If gBOT(b).vely >= -0.4 Then gBOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR



Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class



'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class


'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
'****  DROP TARGETS by Rothbauerw
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class


'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

'Define a variable for each drop target

Dim DT9

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
'
'	Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 
'
'       isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT9 = (new DropTarget)(sw9, sw9a, sw9p, 9, 0, false)

Dim DTArray
DTArray = Array(DT9)


'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	PlayTargetSound
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
		DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
	End If
	DoDTAnim
End Sub

Sub DTRaise(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = -1
	DoDTAnim
End Sub

Sub DTDrop(switch)
	Dim i
	i = DTArrayID(switch)

	DTArray(i).animate = 1
	DoDTAnim
End Sub

Function DTArrayID(switch)
	Dim i
	For i = 0 to uBound(DTArray) 
		If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
	Next
End Function


sub DTBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3
End Sub


'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
	dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
	Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
			DTCheckBrick = 3
		Else
			DTCheckBrick = 1
		End If
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		DTCheckBrick = 4
	Else 
		DTCheckBrick = 0
	End If
End Function


Sub DoDTAnim()
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
	dim transz, switchid
	Dim animtime, rangle

	switchid = switch

	rangle = prim.rotz * PI / 180

	DTAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If (animate = 1 or animate = 4) and animtime < DTDropDelay Then
		primary.collidable = 0
	If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = animate
		Exit Function
		elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
		primary.collidable = 0
		If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		animate = 2
		SoundDropTargetDrop prim
	End If

	if animate = 2 Then
		transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
		if prim.transz > -DTDropUnits  Then
			prim.transz = transz
		end if

		prim.rotx = DTMaxBend * cos(rangle)/2
		prim.roty = DTMaxBend * sin(rangle)/2

		if prim.transz <= -DTDropUnits Then 
			prim.transz = -DTDropUnits
			secondary.collidable = 0
			if UsingROM then 
				controller.Switch(Switchid) = 1
			else 
				'do nothing 'DTAction switchid
			end if
			primary.uservalue = 0
			DTAnimate = 0
			Exit Function
		Else
			DTAnimate = 2
			Exit Function
		end If 
	End If

	If animate = 3 and animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
	elseif animate = 3 and animtime > DTDropDelay Then
		primary.collidable = 1
		secondary.collidable = 0
		prim.rotx = 0
		prim.roty = 0
		primary.uservalue = 0
		DTAnimate = 0
		Exit Function
	End If

	if animate = -1 Then
		transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1

		If prim.transz = -DTDropUnits Then
			Dim b
			For b = 0 to UBound(gBOT)
				If InRotRect(gBOT(b).x,gBOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and gBOT(b).z < prim.z+DTDropUnits+25 Then
					gBOT(b).velz = 20
				End If
			Next
		End If

		if prim.transz < 0 Then
			prim.transz = transz
		elseif transz > 0 then
			prim.transz = transz
		end if

		if prim.transz > DTDropUpUnits then 
			DTAnimate = -2
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		if UsingROM then controller.Switch(Switchid) = 0

	End If

	if animate = -2 and animtime > DTRaiseDelay Then
		prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
		if prim.transz < 0 then
			prim.transz = 0
			primary.uservalue = 0
			DTAnimate = 0

			primary.collidable = 1
			secondary.collidable = 0
		end If 
	End If
End Function


'******************************************************
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************


' Used for drop targets
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

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
    Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
    Dim rotxy
    rotxy = RotPoint(ax,ay,angle)
    rax = rotxy(0)+px : ray = rotxy(1)+py
    rotxy = RotPoint(bx,by,angle)
    rbx = rotxy(0)+px : rby = rotxy(1)+py
    rotxy = RotPoint(cx,cy,angle)
    rcx = rotxy(0)+px : rcy = rotxy(1)+py
    rotxy = RotPoint(dx,dy,angle)
    rdx = rotxy(0)+px : rdy = rotxy(1)+py

    InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function


'******************************************************
'****  END DROP TARGETS
'******************************************************


'******************************************************
'		STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public default Function init(primary, prim, sw, animate)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class

'Define a variable for each stand-up target


Dim ST33, ST34, ST35, ST36, ST37, ST38, ST39
Dim PT26, PT27, PT28, PT29, PT30, PT31	'Rectangle Stand Up Targets


'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts



Set ST33 = (new StandupTarget)(sw33, psw33,33, 0)
Set ST34 = (new StandupTarget)(sw34, psw34,34, 0)
Set ST35 = (new StandupTarget)(sw35, psw35,35, 0)
Set ST36 = (new StandupTarget)(sw36, psw36,36, 0)
Set ST37 = (new StandupTarget)(sw37, psw37,37, 0)
Set ST38 = (new StandupTarget)(sw38, psw38,38, 0)
Set ST39 = (new StandupTarget)(sw39, psw39,39, 0)


PT26 = Array(sw26,26, 0)
PT27 = Array(sw27,27, 0)
PT28 = Array(sw28,28, 0)
PT29 = Array(sw29,29, 0)
PT30 = Array(sw30,30, 0)
PT31 = Array(sw31,31, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)



Dim STArray
STArray = Array(ST33, ST34, ST35, ST36, ST37, ST38, ST39)
Dim PTArray
PTArray = Array(PT26, PT27, PT28, PT29, PT30, PT31)



'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance
Const PTMass = 0.15				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

	PlayTargetSound
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).sw = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180	
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		STCheckHit = 1
	Else 
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime

	STAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If animate = 1 Then
		primary.collidable = 0
		prim.transy = -STMaxOffset
		if UsingROM then 
			vpmTimer.PulseSw switch
		else
			'do nothing 'STAction switch
		end if
		STAnimate = 2
		Exit Function
	elseif animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else 
			STAnimate = 2
		End If
	End If	
End Function


'******************************************************
'		END STAND-UP TARGETS
'******************************************************



'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If				
			End If
		End If

		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height=0.1
			End If
			BallShadowA(b).Y = gBOT(b).Y + offsetY
			BallShadowA(b).X = gBOT(b).X + offsetX
			BallShadowA(b).visible = 1
		End If
	Next
End Sub


'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************




'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function



'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************



'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate 
' how to make these updates. But in summary the following needs to be updated:	
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub


Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub


'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////
Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub


'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 16 then 
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 10 then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()			
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)	
	SoundPlayfieldGate	
End Sub	

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
	If Activeball.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If activeball.velx < -8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If Activeball.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If activeball.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj			
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj		
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'*******************************************
'  Ball brightness code
'*******************************************

if BallLightness = 0 Then
	table1.BallImage="ball-dark"
	table1.BallFrontDecal="JPBall-Scratches"
elseif BallLightness = 1 Then
	table1.BallImage="ball-HDR"
	table1.BallFrontDecal="Scratches"
elseif BallLightness = 2 Then
	table1.BallImage="ball-light-hf"
	table1.BallFrontDecal="g5kscratchedmorelight"
else
	table1.BallImage="ball-lighter-hf"
	table1.BallFrontDecal="g5kscratchedmorelight"
End if

' ****************************************************


'*******************************************
' Hybrid code for VR, Cab, and Desktop
'*******************************************


DIM VRThings

' Desktop Mode or Cabinet Mode:

if VR_Room = 0 and cab_mode = 0 Then  ' Desktop Mode
		for each VRThings in DTRails:VRThings.visible = 1:Next
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 0:Next
		for each VRThings in VRMinRoom:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in VRFan:VRThings.visible = 0:Next
		PinCab_Blades.visible=0
		Primary_topper.visible = 0
		VRposter.visible = 0
		VRposter2.visible = 0
		primitive23.visible=0
		ScoreText.visible = 1

Elseif VR_Room = 0 and cab_mode = 1 Then  ' Cabinet Mode
		for each VRThings in DTRails:VRThings.visible = 0:Next
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 0:Next
		for each VRThings in VRMinRoom:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in VRFan:VRThings.visible = 0:Next
		Primary_topper.visible = 0
		VRposter.visible = 0
		VRposter2.visible = 0
		PinCab_Blades.visible=1
		primitive23.visible=1
		ScoreText.visible = 0
Else
		ScoreText.visible = 0	' VR mode
End If

Sub VRChangeRoom()

	If VRRoom="" Then  ' If this is the first run of the table, there will be no value saved, we default it to 2 here.
		VRRoom = 2
	End if

	If VRRoom = 0 Then   ' Desktop Mode
		for each VRThings in DTRails:VRThings.visible = 1:Next
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 0:Next
		for each VRThings in VRMinRoom:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in VRFan:VRThings.visible = 0:Next
		PinCab_Blades.visible=0
		primitive23.visible=0
		Primary_topper.visible = 0
		VRposter.visible = 0
		VRposter2.visible = 0
	End If

	If VRRoom = 1 Then
		for each VRThings in DTRails:VRThings.visible = 0:Next
		VR_Wall_Left.image = "Wall_Left_Min"
		VR_Wall_Right.image = "Wall_Right_Min"
		VR_Floor.image = "Floor_Min"
		VR_Roof.image = "Roof_Min"
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 0:Next
		for each VRThings in VRMinRoom:VRThings.visible = 1:Next
		for each VRThings in VRClock:VRThings.visible = WallClock:Next
		for each VRThings in VRFan:VRThings.visible = VRFanON:Next
		PinCab_Blades.visible=1
		primitive23.visible=1

		If topper = 1 AND VRFanON = 0 Then
			Primary_topper.visible = 1
		Else
			Primary_topper.visible = 0
		End If

		If poster = 1 Then
			VRposter.visible = 1
		Else
			VRposter.visible = 0
		End If

		If poster2 = 1 Then
			VRposter2.visible = 1
		Else
			VRposter2.visible = 0
		End If
	End If

	If VRRoom = 2 Then
		for each VRThings in DTRails:VRThings.visible = 0:Next
		VR_Wall_Left.image = "wallpaper_left"
		VR_Wall_Right.image = "wallpaper_right"
		VR_Floor.image = "Floorcompletemap"
		VR_Roof.image = "light_gray"
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 0:Next
		for each VRThings in VRMinRoom:VRThings.visible = 1:Next
		for each VRThings in VRClock:VRThings.visible = WallClock:Next
		for each VRThings in VRFan:VRThings.visible = VRFanON:Next
		PinCab_Blades.visible=1
		primitive23.visible=1

		If topper = 1 AND VRFanON = 0 Then
			Primary_topper.visible = 1
		Else
			Primary_topper.visible = 0
		End If

		If poster = 1 Then
			VRposter.visible = 1
		Else
			VRposter.visible = 0
		End If

		If poster2 = 1 Then
			VRposter2.visible = 1
		Else
			VRposter2.visible = 0
		End If

	End If

	If VRRoom = 3 Then
		for each VRThings in DTRails:VRThings.visible = 0:Next
		VR_Wall_Left.image = "Wall_Left"
		VR_Wall_Right.image = "Wall_Right"
		VR_Floor.image = "Floor"
		VR_Roof.image = "Roof"
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 0:Next
		for each VRThings in VRMinRoom:VRThings.visible = 1:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in VRFan:VRThings.visible = VRFanON:Next
		PinCab_Blades.visible=1
		primitive23.visible=1

		If topper = 1 AND VRFanON = 0 Then
			Primary_topper.visible = 1
		Else
			Primary_topper.visible = 0
		End If

		VRposter.visible = 0
		VRposter2.visible = 0
	End If

	If VRRoom = 4 Then
		for each VRThings in DTRails:VRThings.visible = 0:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 1:Next
		Primary_environment.image="Sphere1"
		Primary_environment.roty=40
		for each VRThings in VRMinRoom:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VRFan:VRThings.visible = VRFanON:Next
		PinCab_Blades.visible=1
		primitive23.visible=1

		If topper = 1 AND VRFanON = 0 Then
			Primary_topper.visible = 1
		Else
			Primary_topper.visible = 0
		End If

		VRposter.visible = 0
		VRposter2.visible = 0
	End If

	If VRRoom = 5 Then
		for each VRThings in DTRails:VRThings.visible = 0:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 1:Next
		Primary_environment.image="Sphere2"
		Primary_environment.roty=270
		for each VRThings in VRMinRoom:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VRFan:VRThings.visible = VRFanON:Next
		PinCab_Blades.visible=1
		primitive23.visible=1

		If topper = 1 AND VRFanON = 0 Then
			Primary_topper.visible = 1
		Else
			Primary_topper.visible = 0
		End If

		VRposter.visible = 0
		VRposter2.visible = 0
	End If

	If VRRoom = 6 Then
		for each VRThings in DTRails:VRThings.visible = 0:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 1:Next
		Primary_environment.image="Sphere"
		Primary_environment.roty=300
		for each VRThings in VRMinRoom:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in VRCab:VRThings.visible = 1:Next
		for each VRThings in VRFan:VRThings.visible = VRFanON:Next
		PinCab_Blades.visible=1
		primitive23.visible=1

		If topper = 1 AND VRFanON = 0 Then
			Primary_topper.visible = 1
		Else
			Primary_topper.visible = 0
		End If

		VRposter.visible = 0
		VRposter2.visible = 0
	End If

	If VRRoom = 7 Then  'Cabinet Mode
		for each VRThings in DTRails:VRThings.visible = 0:Next
		for each VRThings in VRCab:VRThings.visible = 0:Next
		for each VRThings in VRSphereRoom:VRThings.visible = 0:Next
		for each VRThings in VRMinRoom:VRThings.visible = 0:Next
		for each VRThings in VRClock:VRThings.visible = 0:Next
		for each VRThings in VRFan:VRThings.visible = 0:Next
		PinCab_Blades.visible=1
		primitive23.visible=1
		Primary_topper.visible = 0
		VRposter.visible = 0
		VRposter2.visible = 0
	End If

End Sub



'*************************************************************************************** 
' CODE BELOW IS FOR THE VR CLOCK.  This was originally taken from Rascal VP9 clock table 
'***************************************************************************************

Dim CurrentMinute ' for VR clock

Sub ClockTimer_Timer()

    'ClockHands Below *********************************************************
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())

End Sub


Sub SaveVRRoom
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if VRRoom = "" then VRRoom = 2 'failsafe to modern walls

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "Twister.txt",True)
	ScoreFile.WriteLine VRRoom
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub


Sub LoadVRRoom
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		VRRoom=2
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "Twister.txt") then
		VRRoom=2
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "Twister.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			VRRoom=2
			Exit Sub
		End if
		VRRoom = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub



'******************************************************
'*****   FLUPPER DOMES 
'******************************************************

 Sub Flash22(Enabled)
	If Enabled Then		
		ObjTargetLevel(1) = 1
		ObjTargetLevel(3) = 1
	Else
		ObjTargetLevel(1) = 0
		ObjTargetLevel(3) = 0
	End If
	FlasherFlash1_Timer
	Sound_Flash_Relay enabled, Flasherbase1
	FlasherFlash3_Timer
	Sound_Flash_Relay enabled, Flasherbase3
 End Sub


 Sub Flash23(Enabled)
	If Enabled Then		
		ObjTargetLevel(4) = 1
	Else
		ObjTargetLevel(4) = 0
	End If
	FlasherFlash4_Timer
	Sound_Flash_Relay enabled, Flasherbase4
 End Sub

 Sub Flash2(Enabled)
	If Enabled Then		
		ObjTargetLevel(2) = 1
	Else
		ObjTargetLevel(2) = 0
	End If
	SetLamp 126,Enabled
	FlasherFlash2_Timer
	Sound_Flash_Relay enabled, Flasherbase2
 End Sub

 Sub Flash5(Enabled)
	If Enabled Then		
		ObjTargetLevel(5) = 1
		Lampz.state(105) = 1
	Else
		ObjTargetLevel(5) = 0
		Lampz.state(105) = 0
	End If
	SetLamp 129,Enabled
	FlasherFlash5_Timer
	Sound_Flash_Relay enabled, Flasherbase5
 End Sub

 Sub Flash6(Enabled)
	If Enabled Then		
		ObjTargetLevel(6) = 1
		Lampz.state(106) = 1
	Else
		ObjTargetLevel(6) = 0
		Lampz.state(106) = 0
	End If
	SetLamp 130,Enabled
	FlasherFlash6_Timer
	Sound_Flash_Relay enabled, Flasherbase6
 End Sub

 Sub Flash7(Enabled)
	If Enabled Then		
		ObjTargetLevel(7) = 1
	Else
		ObjTargetLevel(7) = 0
	End If
	FlasherFlash7_Timer
	Sound_Flash_Relay enabled, Flasherbase7
 End Sub

 Sub Flash8(Enabled)
	If Enabled Then		
		ObjTargetLevel(8) = 1
	Else
		ObjTargetLevel(8) = 0
	End If
	FlasherFlash8_Timer
	Sound_Flash_Relay enabled, Flasherbase8
 End Sub


's125 init
dim Sol125level, Sol125factor, Sol125TmrInterval
Sol125factor = 10
Sol125TmrInterval = 20

Sol125flash.interval = Sol125TmrInterval
S125a.IntensityScale = 0
S125b.IntensityScale = 0
F125b1.IntensityScale = 0
F125b2.IntensityScale = 0

 Sub Flash25(Enabled)
	If Enabled Then		
		Lampz.state(121) = 1
		Sol125level = 1
		Sol125flash_Timer
	Else
		Lampz.state(121) = 0
		Sol125level = Sol125level * 0.6 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, zCol_Rubber_Post011
 End Sub


sub Sol125flash_timer()
    If not Sol125flash.Enabled Then 
		Sol125flash.Enabled = True  
    End If

	S125a.IntensityScale  = sol125factor * Sol125level^3
	S125b.IntensityScale  = sol125factor * Sol125level^3
	F125b1.IntensityScale  = sol125factor * Sol125level
	F125b2.IntensityScale  = sol125factor * Sol125level

	Sol125level = Sol125level * 0.85 - 0.01

    If Sol125level < 0 Then 
		Sol125flash.Enabled = False 
    End If

end sub

's127 init
dim Sol127level, Sol127factor, Sol127TmrInterval
Sol127factor = 10
Sol127TmrInterval = 20

Sol127flash.interval = Sol127TmrInterval
S127a.IntensityScale = 0
S127b.IntensityScale = 0

 Sub Flash27(Enabled)
	If Enabled Then		
		Lampz.state(103) = 1
		Sol127level = 1
		Sol127flash_Timer
	Else
		Lampz.state(103) = 0
		Sol127level = Sol127level * 0.6 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, zCol_Rubber_Post034
 End Sub

sub Sol127flash_timer()
    If not Sol127flash.Enabled Then 
		Sol127flash.Enabled = True  
    End If

	S127a.IntensityScale  = sol127factor * Sol127level^1.2
	S127b.IntensityScale  = sol127factor * Sol127level^1.2

	Sol127level = Sol127level *  0.85 - 0.01

    If Sol127level < 0 Then 
		Sol127flash.Enabled = False 
    End If

end sub

's128 init
dim Sol128level, Sol128factor, Sol128TmrInterval
Sol128factor = 10
Sol128TmrInterval = 20

Sol128flash.interval = Sol128TmrInterval
S128a.IntensityScale = 0
S128b.IntensityScale = 0
S128c.IntensityScale = 0
S128a1.IntensityScale = 0
S128b1.IntensityScale = 0
S128c1.IntensityScale = 0

 Sub Flash28(Enabled)
	If Enabled Then		
		Lampz.state(104) = 1
		Sol128level = 1
		Sol128flash_Timer
	Else
		Lampz.state(104) = 0
		Sol128level = Sol128level * 0.6 'minor tweak to force faster fade
	End If
	Sound_Flash_Relay enabled, p_F4a
 End Sub


sub Sol128flash_timer()
    If not Sol128flash.Enabled Then 
		Sol128flash.Enabled = True  
    End If

	S128a.IntensityScale  = sol128factor * Sol128level^3
	S128b.IntensityScale  = sol128factor * Sol128level^3
	S128c.IntensityScale  = sol128factor * Sol128level^3
	S128a1.IntensityScale  = sol128factor * Sol128level
	S128b1.IntensityScale  = sol128factor * Sol128level
	S128c1.IntensityScale  = sol128factor * Sol128level

	Sol128level = Sol128level *  0.85 - 0.01

    If Sol128level < 0 Then 
		Sol128flash.Enabled = False 
    End If

end sub


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.3		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.2		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "white"
InitFlasher 2, "white"
InitFlasher 3, "white"
InitFlasher 4, "white"
InitFlasher 5, "white"
InitFlasher 6, "white"
InitFlasher 7, "white"
InitFlasher 8, "white"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 


Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 40
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	'rothbauerw
	'Adjust the position of the flasher object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	If objbase(nr).roty > 135 then
		objflasher(nr).y = objbase(nr).y + 50
		objflasher(nr).height = objbase(nr).z + 20
	Else
		objflasher(nr).y = objbase(nr).y + 20
		objflasher(nr).height = objbase(nr).z + 50
	End If
	objflasher(nr).x = objbase(nr).x

	'rothbauerw
	'Adjust the position of the light object to align with the flasher base.
	'Comment out these lines if you want to manually adjust the flasher object
	objlight(nr).x = objbase(nr).x
	objlight(nr).y = objbase(nr).y
	objlight(nr).bulbhaloheight = objbase(nr).z -10

	'rothbauerw
	'Assign the appropriate bloom image basked on the location of the flasher base
	'Comment out these lines if you want to manually assign the bloom images
	dim xthird, ythird
	xthird = tablewidth/3
	ythird = tableheight/3

	If objbase(nr).x >= xthird and objbase(nr).x <= xthird*2 then
		objbloom(nr).imageA = "flasherbloomCenter"
		objbloom(nr).imageB = "flasherbloomCenter"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperLeft"
		objbloom(nr).imageB = "flasherbloomUpperLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird then
		objbloom(nr).imageA = "flasherbloomUpperRight"
		objbloom(nr).imageB = "flasherbloomUpperRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterLeft"
		objbloom(nr).imageB = "flasherbloomCenterLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*2 then
		objbloom(nr).imageA = "flasherbloomCenterRight"
		objbloom(nr).imageB = "flasherbloomCenterRight"
	elseif objbase(nr).x < xthird and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerLeft"
		objbloom(nr).imageB = "flasherbloomLowerLeft"
	elseif  objbase(nr).x > xthird*2 and objbase(nr).y < ythird*3 then
		objbloom(nr).imageA = "flasherbloomLowerRight"
		objbloom(nr).imageB = "flasherbloomLowerRight"
	end if

	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	if round(ObjTargetLevel(nr),1) > round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) + 0.3
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif round(ObjTargetLevel(nr),1) < round(ObjLevel(nr),1) Then
		ObjLevel(nr) = ObjLevel(nr) * 0.85 - 0.01
		if ObjLevel(nr) < 0 then ObjLevel(nr) = 0
	Else
		ObjLevel(nr) = round(ObjTargetLevel(nr),1)
		objflasher(nr).TimerEnabled = False
	end if
	'ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub 
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub 

'******************************************************
'******  END FLUPPER DOMES
'******************************************************




'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.




Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments
LampTimer.Interval = -1
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	if UsingROM then chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update2	'update (fading logic only)
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub SetModLamp(id, val)
	Lampz.state(id) = val
End Sub

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub



const flatprim = 300

Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (max level / full MS fading time). The Modulate property must be set to 1 / max level if lamp is modulated.
'	dim x : for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/120 : Lampz.Modulate(x) = 1 : next
	dim x : for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/4 : Lampz.FadeSpeedDown(x) = 1/10 : Lampz.Modulate(x) = 1 : next

	' Fading for Flasher lamps on Playfield
	Lampz.FadeSpeedUp(126) = 1/4 : Lampz.FadeSpeedDown(126) = 1/16 : Lampz.Modulate(126) = 1
	Lampz.FadeSpeedUp(129) = 1/4 : Lampz.FadeSpeedDown(129) = 1/16 : Lampz.Modulate(129) = 1
	Lampz.FadeSpeedUp(130) = 1/4 : Lampz.FadeSpeedDown(130) = 1/16 : Lampz.Modulate(130) = 1

	Lampz.FadeSpeedUp(103) = 1/4 : Lampz.FadeSpeedDown(103) = 1/16 : Lampz.Modulate(103) = 1
	Lampz.FadeSpeedUp(104) = 1/4 : Lampz.FadeSpeedDown(104) = 1/16 : Lampz.Modulate(104) = 1
	Lampz.FadeSpeedUp(105) = 1/4 : Lampz.FadeSpeedDown(105) = 1/16 : Lampz.Modulate(105) = 1
	Lampz.FadeSpeedUp(106) = 1/4 : Lampz.FadeSpeedDown(106) = 1/16 : Lampz.Modulate(106) = 1
	Lampz.FadeSpeedUp(121) = 1/4 : Lampz.FadeSpeedDown(121) = 1/16 : Lampz.Modulate(121) = 1


' GI Fading
	Lampz.FadeSpeedUp(101) = 1/4 : Lampz.FadeSpeedDown(101) = 1/16 : Lampz.Modulate(101) = 1
	Lampz.FadeSpeedUp(111) = 1/4 : Lampz.FadeSpeedDown(111) = 1/16 : Lampz.Modulate(111) = 1

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(1)= l1a
	Lampz.Callback(1) = "DisableLighting p1, flatprim,"    'flat prim
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= l2a
	Lampz.Callback(2) = "DisableLighting p2, 150,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= l3a
	Lampz.Callback(3) = "DisableLighting p3, 150,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(5)= l5
	Lampz.Callback(6) = "DisableLighting p6, flatprim,"
	Lampz.Callback(9) = "DisableLighting p9, flatprim,"
	Lampz.MassAssign(17)= l17b001 'Dorthy LED
	Lampz.MassAssign(18)= l18b001 'Dorthy LED
	Lampz.MassAssign(19)= l19b001 'Dorthy LED
	Lampz.MassAssign(20)= l20b001 'Dorthy LED
	Lampz.MassAssign(21)= l21b001 'Dorthy LED
    Lampz.MassAssign(17)= l17b 'Dorthy LED
    Lampz.MassAssign(18)= l18b 'Dorthy LED
    Lampz.MassAssign(19)= l19b 'Dorthy LED
    Lampz.MassAssign(20)= l20b 'Dorthy LED
    Lampz.MassAssign(21)= l21b 'Dorthy LED
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22a
	Lampz.Callback(22) = "DisableLighting p22, 150,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= l23a
	Lampz.Callback(23) = "DisableLighting p23, 150,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24a
	Lampz.Callback(24) = "DisableLighting p24, 150,"
	Lampz.MassAssign(25)= l25a
	Lampz.Callback(25) = "DisableLighting p25, flatprim,"
	Lampz.MassAssign(26)= l26a
	Lampz.Callback(26) = "DisableLighting p26, flatprim,"
	Lampz.MassAssign(27)= l27a
	Lampz.Callback(27) = "DisableLighting p27, flatprim,"
	Lampz.MassAssign(28)= l28a
	Lampz.Callback(28) = "DisableLighting p28, flatprim,"
	Lampz.MassAssign(29)= l29a
	Lampz.Callback(29) = "DisableLighting p29, flatprim,"
	Lampz.MassAssign(30)= l30a
	Lampz.Callback(30) = "DisableLighting p30, flatprim,"
	Lampz.MassAssign(31)= l31a
	Lampz.Callback(31) = "DisableLighting p31, flatprim,"
	Lampz.MassAssign(32)= l32a
	Lampz.Callback(32) = "DisableLighting p32, flatprim,"
	Lampz.Callback(33) = "DisableLighting p33, flatprim,"
	Lampz.Callback(34) = "DisableLighting p34, flatprim,"
	Lampz.Callback(35) = "DisableLighting p35, flatprim,"
	Lampz.Callback(36) = "DisableLighting p36, flatprim,"
	Lampz.Callback(37) = "DisableLighting p37, flatprim,"
	Lampz.Callback(38) = "DisableLighting p38, flatprim,"
	Lampz.Callback(39) = "DisableLighting p39, flatprim,"
	Lampz.Callback(40) = "DisableLighting p40, flatprim,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= l41a
	Lampz.Callback(41) = "DisableLighting p41, flatprim,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42a
	Lampz.Callback(42) = "DisableLighting p42, flatprim,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= l43a
	Lampz.Callback(43) = "DisableLighting p43, 150,"
	Lampz.Callback(44) = "DisableLighting p44, flatprim,"
	Lampz.Callback(45) = "DisableLighting p45, flatprim,"
	Lampz.Callback(46) = "DisableLighting p46, flatprim,"
	Lampz.Callback(47) = "DisableLighting p47, flatprim,"
	Lampz.Callback(48) = "DisableLighting p48, flatprim,"
	Lampz.MassAssign(49)= l49
	Lampz.MassAssign(49)= l49a
	Lampz.Callback(49) = "DisableLighting p49, 150,"
	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(50)= l50a
	Lampz.Callback(50) = "DisableLighting p50, 150,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51a
	Lampz.Callback(51) = "DisableLighting p51, 150,"

' bumpers
	Lampz.MassAssign(57)= bumpersmalllight3
	Lampz.MassAssign(57)= bumperbiglight3
	Lampz.MassAssign(57)= bumpershadow3
	Lampz.MassAssign(57)= bumperhighlight3
	Lampz.Callback(57) = "bumperbulb3flash"
	Lampz.MassAssign(58)= bumpersmalllight2
	Lampz.MassAssign(58)= bumperbiglight2
	Lampz.MassAssign(58)= bumpershadow2
	Lampz.MassAssign(58)= bumperhighlight2
	Lampz.Callback(58) = "bumperbulb2flash"
	Lampz.MassAssign(59)= bumpersmalllight1
	Lampz.MassAssign(59)= bumperbiglight1
	Lampz.MassAssign(59)= bumpershadow1
	Lampz.MassAssign(59)= bumperhighlight1
	Lampz.Callback(59) = "bumperbulb1flash"

	Lampz.MassAssign(60)= l60
	Lampz.MassAssign(60)= l60a
	Lampz.Callback(60) = "DisableLighting p60, 150,"
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(61)= l61a
	Lampz.Callback(61) = "DisableLighting p61, 20,"
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(62)= l62a
	Lampz.Callback(62) = "DisableLighting p62, 150,"
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= l63a
	Lampz.Callback(63) = "DisableLighting p63, 150,"
	Lampz.MassAssign(64)= l64a
	Lampz.Callback(64) = "DisableLighting p64, flatprim,"
	Lampz.MassAssign(65)= l65a
	Lampz.Callback(65) = "DisableLighting p65, flatprim,"
	Lampz.MassAssign(66)= l66a
	Lampz.Callback(66) = "DisableLighting p66, flatprim,"
	Lampz.MassAssign(67)= l67a
	Lampz.Callback(67) = "DisableLighting p67, flatprim,"
	Lampz.MassAssign(68)= l68a
	Lampz.Callback(68) = "DisableLighting p68, flatprim,"
	Lampz.MassAssign(69)= l69a
	Lampz.Callback(69) = "DisableLighting p69, flatprim,"
	Lampz.MassAssign(70)= l70a
	Lampz.Callback(70) = "DisableLighting p70, flatprim,"
	Lampz.MassAssign(71)= l71a
	Lampz.Callback(71) = "DisableLighting p71, flatprim,"
	Lampz.MassAssign(72)= l72a
	Lampz.Callback(72) = "DisableLighting p72, flatprim,"

' Flasher Lamps on Playfield
	Lampz.MassAssign(126) = S126
	Lampz.MassAssign(129) = S129
	Lampz.MassAssign(130) = S130

	Lampz.Callback(121) = "DisableLighting p_F1, 250,"
	Lampz.Callback(121) = "DisableLighting p_F1a, 250,"
	Lampz.Callback(103) = "DisableLighting p127a, 250,"
	Lampz.Callback(103) = "DisableLighting p127b, 250,"
	Lampz.Callback(104) = "DisableLighting p_F4a, 250,"
	Lampz.Callback(104) = "DisableLighting p_F4b, 250,"
	Lampz.Callback(104) = "DisableLighting p_F4c, 250,"
	Lampz.Callback(105) = "DisableLighting p_F5, 250,"
	Lampz.Callback(106) = "DisableLighting p_F6, 250,"


	for each x in GI
		Lampz.MassAssign(101)= x
	Next

	Lampz.Callback(111) = "GIUpdates"


	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks
'Version 0.14 - Updated to support modulated signals - Niwak

Class LampFader
	Public FadeSpeedDown(150), FadeSpeedUp(150)
	Private Lock(150), Loaded(150), OnOff(150)
	Public UseFunction
	Private cFilter
	Public UseCallback(150), cCallback(150)
	Public Lvl(150), Obj(150)
	Private Mult(150)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = 0
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if TypeName(input) <> "Double" and typename(input) <> "Integer"  and typename(input) <> "Long" then
			If input Then
				input = 1
			Else
				input = 0
			End If
		End If
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		''debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) > 0 then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= OnOff(x) then Lvl(x) = OnOff(x) : Lock(x) = True
				else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) > 0 then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= OnOff(x) then Lvl(x) = OnOff(x) : Lock(x) = True
				else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx, aLvl : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				aLvl = Lvl(x)*Mult(x)
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(aLvl) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = aLvl : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(aLvl)
					Else
						obj(x).Intensityscale = aLvl
					End If
				end if
				'if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" and typename(lvl(x)) <> "Long" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,aLvl	'Proc
				If Lock(x) Then
					if Lvl(x) = OnOff(x) or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class


'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

'******************************************************
'****  END LAMPZ
'******************************************************


'******************************************************
'****  Ball GI Brightness Level Code
'******************************************************

const BallBrightMax = 255			'Brightness setting when GI is on (max of 255). Only applies for Normal ball.
const BallBrightMin = 100			'Brightness setting when GI is off (don't set above the max). Only applies for Normal ball.

Sub UpdateBallBrightness
	Dim b, brightness
	For b = 0 to UBound(gBOT)
		if ballbrightness >=0 Then gBOT(b).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
			if b = UBound(gBOT) then 'until last ball brightness is set, then reset to -1
			if ballbrightness = ballbrightMax Or ballbrightness = ballbrightMin then ballbrightness = -1
		end if
	Next
End Sub


'******************
' GI fading
'******************

dim ballbrightness

'GI callback

Sub GIUpdates(ByVal aLvl)	'argument is unused
	dim x, gispinnercolor, girubbercolor, giflippercolor ', giaproncolor, girampcolor

	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically

	if aLvl = 0 then										'GI OFF, let's hide ON prims
		if ballbrightness <> -1 then ballbrightness = ballbrightMin

	Elseif aLvl = 1 then									'GI ON, let's hide OFF prims
		if ballbrightness <> -1 then ballbrightness = ballbrightMax
	Else
		if gilvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
			ballbrightness = ballbrightMin + 1
		elseif gilvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
			ballbrightness = ballbrightMax - 1
		Else
			'no change
		end if

	end if

' flippers
	LFLogo.blenddisablelighting = 0.05 * alvl + .025
	RFLogo.blenddisablelighting = 0.05 * alvl + .025

' targets
	For each x in GITargets
		x.blenddisablelighting = 0.25 * alvl + .1
	Next

' transparent targets
	For each x in GITargetstrans
		x.blenddisablelighting = 0.075 * alvl + .025
	Next

' prims (Dorothy and Truck, and other prims)
	For each x in GIPrims
		x.blenddisablelighting = .1 * alvl + .05
	Next

	p9Jackpot.blenddisablelighting = 100*aLvl 'jackpot prim before ramp
	p9JackpotWall.blenddisablelighting = 2.5*aLvl + 1 'jackpot prim before ramp

' plastics for apron, and around edges
	Primitive17.blenddisablelighting = .5 * alvl + .1

' Spinners
	gispinnercolor = 140*alvl + 115
	MaterialColor "Metal2",RGB(gispinnercolor,gispinnercolor,gispinnercolor)

' rubbers '                   and White screw caps
	girubbercolor = 128*alvl + 64
	MaterialColor "Rubber White",RGB(girubbercolor,girubbercolor,girubbercolor)

' metal ramps, rails, and walls

	wires.blenddisablelighting = 0.5 * alvl + 0.5

' ball
	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)

	gilvl = alvl

End Sub



'******************
' Bumper fading
'******************

sub initbumpers()

if gilightscolor = 1 Then
	bumperbiglight1.color = RGB(32,80,255)
	bumperbiglight1.colorfull = RGB(32,80,255)
	bumpersmalllight1.color = RGB(32,80,255)
	bumpersmalllight1.colorfull = RGB(32,80,255)
	bumperhighlight1.color = RGB(255,16,8)
	bumpersmalllight1.TransmissionScale = 0
	bumpersmalllight1.BulbModulateVsAdd = 1

	bumperbiglight2.color = RGB(32,80,255)
	bumperbiglight2.colorfull = RGB(32,80,255)
	bumpersmalllight2.color = RGB(32,80,255)
	bumpersmalllight2.colorfull = RGB(32,80,255)
	bumperhighlight2.color = RGB(255,16,8)
	bumpersmalllight2.TransmissionScale = 0
	BumperSmallLight2.BulbModulateVsAdd = 1

	bumperbiglight3.color = RGB(32,80,255)
	bumperbiglight3.colorfull = RGB(32,80,255)
	bumpersmalllight3.color = RGB(32,80,255)
	bumpersmalllight3.colorfull = RGB(32,80,255)
	bumperhighlight3.color = RGB(255,16,8)
	bumpersmalllight3.TransmissionScale = 0
	bumpersmalllight3.BulbModulateVsAdd = 1
Else
	bumperbiglight1.color = RGB(255,230,190)
	bumperbiglight1.colorfull = RGB(255,230,190)
	bumpersmalllight1.color = RGB(255,255,255)
	bumpersmalllight1.colorfull = RGB(255,255,255)
	bumperhighlight1.color = RGB(255,180,100)
	bumpersmalllight1.TransmissionScale = 0
	BumperSmallLight1.BulbModulateVsAdd = 0.99

	bumperbiglight2.color = RGB(255,230,190)
	bumperbiglight2.colorfull = RGB(255,230,190)
	bumpersmalllight2.color = RGB(255,255,255)
	bumpersmalllight2.colorfull = RGB(255,255,255)
	bumperhighlight2.color = RGB(255,180,100)
	bumpersmalllight2.TransmissionScale = 0
	BumperSmallLight2.BulbModulateVsAdd = 0.99

	bumperbiglight3.color = RGB(255,230,190)
	bumperbiglight3.colorfull = RGB(255,230,190)
	bumpersmalllight3.color = RGB(255,255,255)
	bumpersmalllight3.colorfull = RGB(255,255,255)
	bumperhighlight3.color = RGB(255,180,100)
	bumpersmalllight3.TransmissionScale = 0
	BumperSmallLight3.BulbModulateVsAdd = 0.99
End If
End Sub

Sub bumperbulb1flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim Z
	Z = aLvl
if gilightscolor = 1 Then
	bumperbase1.BlendDisableLighting = 0.5*Z
	bumperdisk1.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat1", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(0,130 - 100 * Z, 255 - 150 * Z), RGB(0,0,255), RGB(0,0,32), false, true, 0, 0, 0, 0
	bumpersmalllight1.intensity = 30 + 30 * Z
	bumperbiglight1.intensity = 10 * Z
	bumpertop1.BlendDisableLighting = Z *.15 + .05
	MaterialColor "bumpertopmat1", RGB(255-55*Z,235 - Z*35,220 + Z*55)
	bumperbulb1.BlendDisableLighting = 10*Z + 3
	bumperhighlight1.opacity = 1000 * (Z^3)
Else
	BumperBase1.BlendDisableLighting = 0.5*Z
	BumperDisk1.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat1", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
	BumperSmallLight1.intensity = 30 + 30 * Z
	bumperbiglight1.intensity = 10 * Z
	BumperTop1.BlendDisableLighting = Z *.15 + .05
	MaterialColor "bumpertopmat1", RGB(255,235 - Z*36,220 - Z*90)
	BumperBulb1.BlendDisableLighting = 10*Z + 3
	BumperHighlight1.opacity = 1000 * (Z^3)
	BumperSmallLight1.color = RGB(255,255 - 20*Z,255-65*Z)
	BumperSmallLight1.colorfull = RGB(255,255 - 20*Z,255-65*Z)
End If
End Sub

Sub bumperbulb2flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim Z
	Z = aLvl
if gilightscolor = 1 Then
	BumperBase2.BlendDisableLighting = 0.5*Z
	BumperDisk2.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat2", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(0,130 - 100 * Z, 255 - 150 * Z), RGB(0,0,255), RGB(0,0,32), false, true, 0, 0, 0, 0
	BumperSmallLight2.intensity = 30 + 30 * Z
	bumperbiglight2.intensity = 10 * Z
	BumperTop2.BlendDisableLighting = Z *.15 + .05
	MaterialColor "bumpertopmat2", RGB(255-55*Z,235 - Z*35,220 + Z*55)
	BumperBulb2.BlendDisableLighting = 10*Z + 3
	BumperHighlight2.opacity = 1000 * (Z^3)
Else
	BumperBase2.BlendDisableLighting = 0.5*Z
	BumperDisk2.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat2", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
	BumperSmallLight2.intensity = 30 + 30 * Z
	bumperbiglight2.intensity = 10 * Z
	BumperTop2.BlendDisableLighting = Z *.15 + .05
	MaterialColor "bumpertopmat2", RGB(255,235 - Z*36,220 - Z*90)
	BumperBulb2.BlendDisableLighting = 10*Z + 3
	BumperHighlight2.opacity = 1000 * (Z^3)
	BumperSmallLight2.color = RGB(255,255 - 20*Z,255-65*Z)
	BumperSmallLight2.colorfull = RGB(255,255 - 20*Z,255-65*Z)
End If
End Sub

Sub bumperbulb3flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim Z
	Z = aLvl
if gilightscolor = 1 Then
	bumperbase3.BlendDisableLighting = 0.5*Z
	bumperdisk3.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat3", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(0,130 - 100 * Z, 255 - 150 * Z), RGB(0,0,255), RGB(0,0,32), false, true, 0, 0, 0, 0
	bumpersmalllight3.intensity = 30 + 30 * Z
	bumperbiglight3.intensity = 10 * Z
	bumpertop3.BlendDisableLighting = Z *.15 + .05
	MaterialColor "bumpertopmat3", RGB(255-55*Z,235 - Z*35,220 + Z*55)
	bumperbulb3.BlendDisableLighting = 10*Z + 3
	bumperhighlight3.opacity = 1000 * (Z^3)
Else
	BumperBase3.BlendDisableLighting = 0.5*Z
	BumperDisk3.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat3", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
	BumperSmallLight3.intensity = 30 + 30 * Z
	bumperbiglight3.intensity = 10 * Z
	BumperTop3.BlendDisableLighting = Z *.15 + .05
	MaterialColor "bumpertopmat3", RGB(255,235 - Z*36,220 - Z*90)
	BumperBulb3.BlendDisableLighting = 10*Z + 3
	BumperHighlight3.opacity = 1000 * (Z^3)
	BumperSmallLight3.color = RGB(255,255 - 20*Z,255-65*Z)
	BumperSmallLight3.colorfull = RGB(255,255 - 20*Z,255-65*Z)
End If
End Sub

'**************************************
' Speedupball on ramp corner condition
'**************************************


Sub SpeedUpBall(actBall)
	With actBall
		If .VelY < abs(8) AND .VelX < abs(8) AND .VelZ < abs(1) Then .VelY = .VelY * 3 : .VelX = .VelX * 3: .VelZ = .VelZ * 3
	End With
End Sub


Sub TriggerSpeedUp_Hit()
	SpeedUpBall ActiveBall
'        debug.print "velx: " & ActiveBall.velx & " vely: " & ActiveBall.vely & " velz: " & ActiveBall.velz
End Sub



'******** Revisions done by UnclePaulie on Hybrid version 1.0 - 2.0 *********

' v1.0	VR Room added by Uncle Paulie in version 1.0
'		Two minimal room environments added
'		Small lighting changes
'		Sixtoe modified the magnetic spinner to increase magnet slightly
' v1.1	Main changes were adding Fleep's sound package 
' 		Tweaked the left sling position slightly to make it a little more sensitive, as it didn't trigger enough
' 		Added ball cor tracker, and updated ball rolling sounds
' 		Had to change some of the ramp triggers to get Fleep sounds working
' 		Changed the desktop settings to: 52,50,0,0,1,1,1,0,200,-390 (it was: 52,50,0,0,1,1,1,0,80,-120)
' 		Added animated flipper buttons, and animated the plunger when using a keyboard.
' 		Added a sphere room (coundn't find good tornado 360 pano images.  Found a field image prior to storm.)  Sphere used from Rajo Joey.
' 		Added a couple generic pictures and inserted into a sphere.  Not pano images, but still look cool.
' 		Adjusted some of the fleep sound levels
' 		Added an optional Wall Clock from Rajo Joey, and moved to side wall.
' 		VR Room can be changed with magnasave buttons
' v1.2.4 Added nFozzy physics and Roth flipper physics, rubber and post dampeners
' 		Implemented other recommended physics updates to bumpers, walls, targets, slings, etc.  
' 		Moved the topper fan sound to the backglass for those with SSF
' 		Ensured the ball reflections were on
' 		Modified the autoplunger strength and randomness to more closely match the real table.
' 		Adjusted flipper physics to match that of a mid/late '90s table.
' 		Adjusted the size of the flippers based off nFozzy / roth's guiedance.  Reduced length from 125 to 118.
' 		Changed the playfield friction to 0.13
' 		Adjusted the sound levels of the flipper rubbers
' 		VR Backglass will dim with the GI 
' 		Added an invisible wall (but collidable) next to the plunger.  In cabinet mode the ball would not always come out of plunger lane correctly on autofire 
' 		Sixtoe found a few updates:  
'   	Physics on the sleeves needed to change to zCol_sleeves, not rubberposts
'    	Bumper lights (i57-59a) - turned off static (lights were swimming)
'    	Post and metal material changed to meltal0.8 (30 primatives).  The nuts on top and posts under slings looked too white.
'    	Also original primatives on posts were still collidable.  Turned them off as the new primative overlays do the colliding now.  
' v1.2.5:  Tomate's Updates
' 		- metal walls hit threshold changed  1 ---> 2
' 		- new plastic ramp prim and texture added
' 		- new lowPoly collidable plastic ramp added (to match with ramp's shape)
' 		- new wire ramps textures added
' 		- some tweaks on playfield texture
' 		- bumpers cap materials changed
' 		- new sideblades texture added
' 		- new LUT added
' v1.2.6: Added a 2nd rubberizer effect based off the VPW taxi table.
' 		Tomate added a playfield shadow flasher to add depth to playfield.
' v1.2.7 Changed the pov of the desktop to 44.6 incline (was 52).  Also changed disable lighting on spina primative to 0.2 (was 0).  Some PCs, the disc looked too dark.
' v1.2.8 Received feedback that sometimes the ball would bounce over the apron, or get stuck under left orbit or the ramp.  
'		Changed the transparent plastics to collidable.  (all but top truck one)
'		Increased the height of invisible ramp walls to match the visible primatives
'		Added invisible glass to stop going over apron 
'		Adjusted a few GI light locations to align with playfield
' v1.2.9 Added desktop rails and lockbar in for desktop mode
' v1.3	Lots of feedback from VP competition and others; also in dire need to be updated to latest VPW physics, etc.
'		Rebuilt Main Trough to hold all balls, and changed to no longer destroying balls. Should fix some inconsistencies found in competition.
'		Added an apron physics walls to be higher, and block balls from potentially jumping over.
' v1.31	Added real trough for multiball (switches 41-45, and solenoid 14 for VUK) had to control via balllock variable (bl41-45)
'		Added a VUK ramp and physics walls to shield the ball.
' v1.32	Updated to latest Fleep sounds, and changed to new fleep relay sounds
'		Converted to gBOT thoughout... no destroying balls. Should improve performance and correct some VP Competition issues.
' 		Added Apophis sling corrections.
'		Added lastest VPW code for DT, ST, slingshot corrections, flippers, physics, dampeners, shadows
'		Added VPW Dynamic Ball Shadows to the solution done Iakki, Apophis, and Wylte.
'		Added groove to plunger lane, and small ball release ramp to get up.  
'		Turned off ingame AO and Screen Space Reflections.  (Not needed, and is a performance Hit)
'		Changed all wall and metal sound hit thresholds to 2.  
'		Added several walls for physics on plastics and dorothy
' v1.33	Adjusted flipper size slightly... too long before.
' 		The playfield friction was set to .13. Recommendation is 0.15 to 0.25.Changed to 0.18
' 		Changed the desktop pov
' v1.34	Added new drop targets to the solution done by Rothbauerw
' v1.35	Added new stand up targets to the solution done by Rothbauerw
'		The right sling and right outlane switchs are reversed from the manual... 60 and 62.
' 		The lights for the T and W in TWISTER were swapped.  l65 and l66
'		Adjusted the disable lighting for targets (and sign), truck, and dorothy.  And created GI collecions.
'		Added different sling primitives
' 		Added separate materials on every target, to ensure they animate
' 		Adjusted the size of rollovers and targets.  Also adjusted the heights.
' 		Lowered the pincab and backbox disable lighting in VR, they were too bright.
' v1.36	Modified hybrid mode... automatic for desktop and cabinet users; and VR users can still change environments with magna save.
'		VR Room environment will automatically save to twister.txt file in the visual pinbal / user directory (if in VR mode)
'		Raised the Plastics Physics Wall 3 up by 5 VPU.  Ball could get stuck behind the Ball Lock target.
'		Added flyers and topper prim for VR options
' v1.37	Updated to Lampz lighting routine by VPW.
'		Added flupper domes
' v1.38	Playfield flashers changed to flash appropriately, code similar to VPW TFTC code.
' v1.39	Significantly modified GI shapes, added bulb primitives, bulb GI, and plastic top GI.  Default is now "normal", not blue or violet.
' 		Changed the duration of playfield flashers to be a little longer...
' 		Changed the material of the plastics to allow GI light to go through.
' 		Changed sideblades to black, closer to real table.
' 		Created a basic background for DesktopMode, and added a scoretext dmd
'		Changed material and removed image on sidewalls for more reflectivity
'		Removed NoUpperLeftFlipper and right... old way to do fastflips.
'		Added GI to lampz, control the ball brightness, prims, targets, rubbers, etc. for GI
' 		Adjusted top area GI sizes and shapes, as couldn't see bonus lights at the top when that lightning effect is on, and was going through walls.
'		Added new top lane prims and screws with different materials, and adjusted the size and shapes.
'		Changed the bumper caps to be active, to see the light show through.  Also adjusted the height scale up by 15 VPU.
'		Lowered the flasher domes.  Real table the flares are mounted underneath.
'		Changed the bumpers extensively.  Reused a lot of Flupper bumpers, but needed to tie into lampz.
' v1.40	Implemented some equations from flupper bumper fading routines
'		Removed the violet color option, and only can choose from normal (white) or blue.  
'		Updated bumper light color if blue lights chosen.
'		Fixed the rubber sizes, shapes, and locations.  Also reduced the circumference of the sleeves
' 		Fixed the material/transparency on the big cannister and truck
' v1.41	New playfield with rollovers and target holes cutout, and blocked some areas you could see under top orbit.
'		Added inside pincab walls, lowered the cabinet bottom for VR and seeing through holes.  Updated VR Backbox image.
'		Added another GI by left gate.  Also adjusted the material for the transparent stand up targets
'		Animated the start button
'		Added a playfield mesh for a bevel to the subway trough
' v1.42	Modified the playfield mesh to include hole for end of trough.  Also redid the trough, added walls, and modified location and strength.
'		Changed the physical walls coming into the multiball trough.
'		Added 4 different ball brigthness options, as well as a disable VR Room magna save option.
' v1.43	Changed the playfield friction, and the flipper physics and sizes.  Also the ramp physics, and a ramp physics wall to prevent ball jumping out of ramp.
'		Added a ramp speed up trigger and subroutine at top of plastic ramp.  A corner condition was reported that a ball stoppped in middle of ramp on small shot
'		Updated the VR Backglass dark and Illuminated image.  Did not animate, as real one doesn't.  Just turn on and off with GI.
' v1.44	Added updated playfield with all inserts cutout, and a text overlay for insert prims.
'		Added 3D insert prims
'		Added a wall around the jackpot triangle to control the underlighting with GI.
'		Added glow effects around all the insert prims (not the small flat prims, however).
' v1.45	Added VR Fan elements from Flupper's Whirlwind, changed the housing primitive, and modified the code slightly.
'		Slight mods to the triangle flasher 
'		Updated the primitive20 mesh, as there were screws floating, and 4 bolts in the middle of the ramp.  Looked weird in VR.
'		Cabinet POV updates.
' v1.46	Bord recommended reducing the amount of sling push out from rotx 20 to 12.
'		Bord had a ball go missing in the top right, Sixtoe recommended to extend physics walls.  Also added a wall around perimeter.
'		Leojreimroc recommended having an option for GI on backglass.(real table is static). Also changed material on the VR cab.  (too reflective, used a _noextrashade one).  
'		Slight adjustment to wall7 coming into the ball lock.
' v1.47	Apophis recommended going to 10.72 and automate the VR_Room call.
'		Adjusted the rubbers on slings slightly.
'		Added flipper primitives and new images.
'		Added the Twister speaker grills for VR.
' v2.0	Release
	
' thanks to the VPW team for testing, especially tastywasps, apophis, bord, sixtoe, leojreimroc
