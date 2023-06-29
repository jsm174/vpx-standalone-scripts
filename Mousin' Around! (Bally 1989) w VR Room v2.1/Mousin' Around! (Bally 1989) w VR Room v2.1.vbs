Option Explicit
Randomize
'
' MM      MM   OOOOOOO   UU     UU   SSSSSSS   II  NN      NN   ''      AAAAA    RRRRRRRR    OOOOOOO   UU     UU  NN      NN  DDDDDDDD   !!
' MMM    MMM  OO     OO  UU     UU  SS     SS  II  NNN     NN   ''     AA   AA   RR     RR  OO     OO  UU     UU  NNN     NN  DD     DD  !!
' MMMM  MMMM  OO     OO  UU     UU  SS         II  NNNN    NN  ''     AA     AA  RR     RR  OO     OO  UU     UU  NNNN    NN  DD     DD  !!
' MM MMMM MM  OO     OO  UU     UU  SS         II  NN NN   NN         AA     AA  RR     RR  OO     OO  UU     UU  NN NN   NN  DD     DD  !!
' MM  MM  MM  OO     OO  UU     UU   SSSSSSS   II  NN NN   NN         AAAAAAAAA  RRRRRRRR   OO     OO  UU     UU  NN NN   NN  DD     DD  !!
' MM      MM  OO     OO  UU     UU         SS  II  NN   NN NN         AA     AA  RR RR      OO     OO  UU     UU  NN   NN NN  DD     DD  !!
' MM      MM  OO     OO  UU     UU         SS  II  NN    NNNN         AA     AA  RR  RR     OO     OO  UU     UU  NN    NNNN  DD     DD  !!
' MM      MM  OO     OO  UU     UU  SS     SS  II  NN     NNN         AA     AA  RR    RR   OO     OO  UU     UU  NN     NNN  DD     DD
' MM      MM   OOOOOOO    UUUUUUU    SSSSSSS   II  NN      NN         AA     AA  RR     RR   OOOOOOO    UUUUUUU   NN      NN  DDDDDDDD   !!  by Bally/Midway, 1989
'
'

' - Herweh: Original Development by Herweh 2019 for Visual Pinball 10
' 	- Those that helped Herweh in original release:  Schreibi34, OldSkoolGamer, Cosmic80, Flupper, JPSalas, 32assassin, Sliderpoint, Ninuzzu, Dark, DJRobX, tttttwii
' - UnclePaulie:  Version 1.0: VR room (Thanks to Sixtoe for feedback, and come corrections)
' - UnclePaulie:  Version's through 2.1: Updates to VPW physics, slings, dynamic shadows, lighting, hybrid table, gBOT, performance, Lampz, 3D inserts, Fleep sounds, ballrolling and ramp sounds.
'   - Thank you to Rothbauerw, PinStratsDan, Bord, Tomate, Sixtoe, Thalamus, and Benji for improvements, testing, and feedback!

' Detailed updates included at the end of the script 


'******************************************************************************************

Dim GIColorMod, FlipperColorMod, EnableGI, EnableFlasher, ShadowOpacityGIOff, ShadowOpacityGIOn
dim orbit, orbit2

'*******************************************
' Desktop, Cab, and VR OPTIONS
'*******************************************

' Desktop, Cab, and VR Room are automatically selected.  However if in VR Room mode, you can change the environment with the magna save buttons.


const BallLightness = 1 '0 = dark, 1 = bright, 2 = brighter, 3 = brightest

' *** If using VR Room: ***

const CustomWalls = 0 	  	' set to 0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const WallClock = 1	  	  	' 1 Shows the clock in the VR room only   
const topper = 1		  	' 0 = Off 1= On - Topper visible in VR Room only
const poster = 1		  	' 1 Shows the flyer posters in the VR room only 
const poster2 = 1		  	' 1 Shows the flyer posters in the VR room only 
const sideblades = False  	' True uses custom sideblades and False uses generic black sideblades in VR only
const BGFlashers = 1	  	' 1 = Flashers on the backglass for VR mode.
const VRBGFlashHigh = 300 	' VR Backglass Opacity for when GI off (default is 300)
const VRBGFlashLow = 75		' VR Backglass Opacity for when GI on (default is 75)

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's
Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)

'Ambient (Room light source)
Const AmbientBSFactor 		= 1 	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 0		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor 		= 1		'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

'----- General Sound Options -----
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.
Const BallRollVolume = 0.5 			'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5 			'Level of ramp rolling volume. Value between 0 and 1

'----- Phsyics Mods -----
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled


'************************************************
'************************************************

' ****************************************************
' OPTIONS
' ****************************************************

' GI COLOR MOD
'   0 = White bulbs and GI (default)
'   1 = Yellow bulbs and GI
'   2 = Red bulbs and GI
'   3 = Blue bulbs and GI
GIColorMod = 0

' MAGNA SAVE MOD ADJUSTMENTS 
' Ability to adjust Color or Flipper Mod with Left / Right Magna Save buttons during gameplay
const MagnaOn = 0  '1 = Ability On, 0 = Ability Off

' FLIPPER COLOR MOD
'	0 = Yellow/red
'	1 = Yellow/red/Bally (default, like the real pin)
'   2 = Yellow/white/Williams
'   3 = Red/white/Williams
'	4 = Blue/white/Williams
FlipperColorMod = 1

' ENABLE/DISABLE GI (general illumination)
'	0 = GI is off
'	1 = GI is on (value is a multiplicator for GI intensity - decimal values like 0.7 or 1.33 are valid too)
EnableGI = 1

' ENABLE/DISABLE flasher
'	0 = Flashers are off
'	1 = Flashers are on
EnableFlasher = 1

' PLAYFIELD SHADOW INTENSITY DURING GI OFF OR ON (adds additional visual depth) 
' usable range is 0 (lighter) - 100 (darker)  
ShadowOpacityGIOff = 90
ShadowOpacityGIOn  = 60


'***************************************************************************************************************************************************************
'***************************************************************************************************************************************************************


' ****************************************************
' standard definitions
' ****************************************************

Dim VR_Room, cab_mode, DesktopMode: DesktopMode = MousinAround.ShowDT
If RenderingMode = 2 Then VR_Room=1 Else VR_Room=0      'VRRoom set based on RenderingMode in version 10.72
If Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0

Const UseSolenoids 	= 2
Const UseLamps 		= 0
Const UseSync 		= 0
Const HandleMech 	= 0
Const UseGI			= 1

Const cDMDRotation 	= -1 			'-1 for No change, 0 - DMD rotation of 0?, 1 - DMD rotation of 90?
Const cGameName 	= "mousn_l4"	'ROM name
Const ballsize 		= 50
Const ballmass 		= 1
Const UsingROM = True				'The UsingROM flag is to indicate code that requires ROM usage. 

'***********************

Const tnob = 3						'Total number of balls
Const lob = 0						'Locked balls

Dim tablewidth: tablewidth = MousinAround.width
Dim tableheight: tableheight = MousinAround.height
Dim i, MBall1, MBall2, MBall3, gBOT
Dim BIPL : BIPL = False				'Ball in plunger lane

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package."
On Error Goto 0

LoadVPM "01560000", "S11.VBS", 3.26


' ****************************************************
' table init
' ****************************************************
Sub MousinAround_Init()
	vpmInit Me 
	With Controller
        .GameName 			= cGameName
        .SplashInfoLine 	= "Mousin' Around (Bally/Midway 1989)"
		.Games(cGameName).Settings.Value("sound") = 1 ' Set sound (0=OFF, 1=ON)	
		.Games(cGameName).Settings.Value("showpindmd") = 0 ' Use External DMD (0=OFF,1=ON)
		.HandleMechanics 	= False
		.HandleKeyboard 	= False
		.ShowDMDOnly 		= True
		.ShowFrame 			= False
		.ShowTitle 			= False
		.Hidden 			= DesktopMode
		If cDMDRotation >= 0 Then .Games(cGameName).Settings.Value("rol") = cDMDRotation
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

	' tilt
	vpmNudge.TiltSwitch		= 1
	vpmNudge.Sensitivity	= 6
	vpmNudge.TiltObj		= Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

	' initialize the traps to transz = 0... solves a corner condition such that they can go below table if there's a VPX crash.
	pRightTrap.transz = 0
	pLeftTrap.transz = 0

	'************  Trough	**************
	Set MBall1 = BallRelease.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall2 = sw12.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set MBall3 = sw13.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(MBall1,MBall2, MBall3)

	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1

	' init lights, flippers, traps, mouse hole diverter and target motor bank

	InitFlasher
	InitGI True
	InitFlippers
	InitTraps
	InitMouseHoleDiverter
	InitMotorBank

	' Turn on the Flupper bumper lights at game launch
	FlBumperFadeTarget(1) = .9
	FlBumperFadeTarget(2) = .9
	FlBumperFadeTarget(3) = .9

	' backdrop objects
	r57.Visible = DesktopMode
	r58.Visible = DesktopMode
	r59.Visible = DesktopMode
	r60.Visible = DesktopMode
	r61.Visible = DesktopMode
	r62.Visible = DesktopMode
	r63.Visible = DesktopMode
	L_DT_LadyMouse.Visible = DesktopMode
	L_DT_Mouse.Visible = DesktopMode

If VR_Room = 1 Then
    SetBackglass
End If

	' VR Backglass lighting
	PinCab_Backglass.blenddisablelighting = .7

' control orbit variables
	orbit = 0
	orbit2 = 0

End Sub

Sub MousinAround_Paused() 	: Controller.Pause = True : End Sub
Sub MousinAround_UnPaused() : Controller.Pause = False : End Sub
Sub MousinAround_Exit()
	Controller.Pause = False
	Controller.Stop
End Sub

' save the insert intensities so they can be updated when GI is off

InitInsertIntensities

Sub InitInsertIntensities
	dim bulb
	for each bulb in InsertLightsInPrim
		bulb.uservalue = bulb.intensity
	next
End sub

'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	UpdateBallBrightness
	LampTimer

	If VR_Room = 1 Then
		DisplayTimer
	End If

	If VR_Room = 0 AND cab_mode = 0 Then
		DisplayTimerDT
	End If

End Sub


'***************************************************************************
' VR Plunger Code
'***************************************************************************

Sub TimerVRPlunger_Timer
  If VR_Primary_plunger.Y < 50 then
       VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  VR_Primary_plunger.Y = -35 + (5* Plunger.Position) -20
End Sub


' ****************************************************
' keys
' ****************************************************
Sub MousinAround_KeyDown(ByVal keycode)

    If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		VR_Primary_plunger.Y = -35
	End If

	If keycode = LeftFlipperKey  Then 
		FlipperActivate LeftFlipper, LFPress
		Controller.Switch(58) = True
		VR_FB_Left.X = VR_FB_Left.X +10
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		Controller.Switch(57) = True
		VR_FB_Right.X = VR_FB_Right.X - 10
	End If

	if keycode = StartGameKey then 
		Primary_start_button.y= 1091.341 - 5
		SoundStartButton
	End If

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter

If Magnaon = 1 Then
	If keycode = LeftMagnaSave Then GIColorMod = (GIColorMod + 1) MOD 4 : ResetGI
	If keycode = RightMagnaSave Then FlipperColorMod = (FlipperColorMod + 1) MOD 5 : ResetFlippers
End If

    If KeyDownHandler(keycode) Then Exit Sub

End Sub

Sub MousinAround_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then 
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.Enabled = False
		TimerVRPlunger1.Enabled = True
		VR_Primary_plunger.Y = -35
	end if

	If keycode = LeftFlipperKey  Then
		FlipperDeActivate LeftFlipper, LFPress
		Controller.Switch(58) = False
		VR_FB_Left.X = VR_FB_Left.X -10
	End If
	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		Controller.Switch(57) = False
		VR_FB_Right.X = VR_FB_Right.X +10
	End If

	if keycode = StartGameKey then 
		Primary_start_button.y= 1091.341
	End If

    If KeyUpHandler(keycode) Then Exit Sub

End Sub

' ****************************************************
' *** solenoids
' ****************************************************
SolCallback(1)          = "SolOuthole"
SolCallback(2)	       	= "SolBallRelease"
SolCallback(3)			= "SolRightTrapUp"
SolCallback(4)			= "SolLeftTrapUp"
SolCallback(5)			= "SolRightTrapDown"
SolCallback(8)			= "SolLeftTrapDown"
SolCallback(7)			= "SolKnocker"
SolCallback(10)			= "SolGI"
SolCallback(11)			= "SolMotorBank"
SolCallback(13) 		= "SolKickback"
SolCallback(14)			= "SolMouseHoleDiverter"
SolCallback(16)			= "SolMouseHoleRelease"
SolCallback(22)			= "SolOrbitGate"

SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sLRFlipper) = "SolRFlipper"


SolCallback(15)			= "Flash 15,"		' center flashers
SolCallback(25) 		= "Flash 25,"		' right flipper flasher
SolCallback(26)			= "Flash 26,"		' left flipper flasher
SolCallback(27) 		= "Flash 27,"		' left side flasher
SolCallback(28) 		= "Flash28"			' backboard dome flasher
SolCallback(29)  		= "Flash 29,"		' top right flasher
SolCallback(30) 		= "Flash 30,"		' right ramp flasher
SolCallback(31) 		= "Flash 31,"		' left ramp flasher
SolCallback(32)	 		= "Flash 32,"		' timer flasher


'******************************************************
'			TROUGH BASED ON FOZZY'S
'******************************************************

Sub BallRelease_Hit():Controller.Switch(11) = 1:UpdateTrough: End Sub
Sub BallRelease_UnHit():Controller.Switch(11) = 0:UpdateTrough:End Sub
Sub sw12_Hit():Controller.Switch(12) = 1:UpdateTrough:End Sub
Sub sw12_UnHit():Controller.Switch(12) = 0:UpdateTrough:End Sub
Sub sw13_Hit():Controller.Switch(13) = 1:UpdateTrough:End Sub
Sub sw13_UnHit():Controller.Switch(13) = 0:UpdateTrough:End Sub

Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
	If BallRelease.BallCntOver = 0 Then sw12.kick 60, 10
	If sw12.BallCntOver = 0 Then sw13.kick 60, 10	
	Me.Enabled = 0
End Sub


'******************************************************
'				DRAIN & RELEASE
'******************************************************

Sub Drain_Hit()
	RandomSoundDrain Drain
	Controller.Switch(9) = 1
	UpdateTrough
End Sub

Sub Drain_UnHit()
	Controller.Switch(9) = 0
End Sub

Sub SolBallRelease(enabled)
	If enabled Then 
		RandomSoundBallRelease BallRelease
		BallRelease.kick 60, 12
		UpdateTrough
	End If
End Sub

Sub SolOutHole(enabled)
	If enabled Then 
		Drain.kick 60, 16
		UpdateTrough
		SoundSaucerKick 1, Drain
	End If
End Sub


'*******************************************
'  Flippers
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


' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate

	' maybe move primitive flippers
	If FlipperColorMod > 1 Then
		pLeftFlipperBat.ObjRotZ  = LeftFlipper.CurrentAngle + 1
		pRightFlipperBat.ObjRotZ = RightFlipper.CurrentAngle + 1
	ElseIf FlipperColorMod = 1 Then
		pLeftFlipperBally.ObjRotZ  = LeftFlipper.CurrentAngle - 90
		pRightFlipperBally.ObjRotZ = RightFlipper.CurrentAngle + 90
	End If
	pLeftFlipperShadow.ObjRotZ  = LeftFlipper.CurrentAngle + 1
	pRightFlipperShadow.ObjRotZ = RightFlipper.CurrentAngle + 1

End Sub


'****************************************************************
'  Slingshots
'****************************************************************

' RStep and LStep are the variables that increment the animation

Dim LeftStep, RightStep

Sub LeftSlingShot_Slingshot()
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 55
	RandomSoundSlingshotLeft LeftSlingHammer
    LeftStep = 0
	LeftSlingShot.TimerInterval = 20
    LeftSlingShot.TimerEnabled  = True
	LeftSlingShot_Timer
End Sub
Sub LeftSlingShot_Timer()
    Select Case LeftStep
		Case 0: LeftSling1.Visible = False : LeftSling3.Visible = True : LeftSlingHammer.TransZ = -18
        Case 1: LeftSling3.Visible = False : LeftSling2.Visible = True : LeftSlingHammer.TransZ = -8
        Case 2: LeftSling2.Visible = False : LeftSling1.Visible = True : LeftSlingHammer.TransZ = 0 : LeftSlingShot.TimerEnabled = 0
    End Select
    LeftStep = LeftStep + 1
End Sub

Sub RightSlingShot_Slingshot()
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 56
	RandomSoundSlingshotRight RightSlingHammer
    RightStep = 0
	RightSlingShot.TimerInterval = 20
    RightSlingShot.TimerEnabled  = True
	RightSlingShot_Timer
End Sub
Sub RightSlingShot_Timer()
    Select Case RightStep
		Case 0: RightSling1.Visible = False : RightSling3.Visible = True : RightSlingHammer.TransZ = -18
        Case 1: RightSling3.Visible = False : RightSling2.Visible = True : RightSlingHammer.TransZ = -8
        Case 2: RightSling2.Visible = False : RightSling1.Visible = True : RightSlingHammer.TransZ = 0 : RightSlingShot.TimerEnabled = 0
    End Select
    RightStep = RightStep + 1
End Sub


' ****************************************************
' kick back
' ****************************************************
Sub SolKickback(Enabled)
   	Kickback.Enabled = Enabled
End Sub 

Dim KickbackBallVel : KickbackBallVel = 1

Sub Kickback_Hit()
' Corner condition code if during multiball, more than one ball goes to the kickback 
  if Kickback.TimerEnabled  = True Then
	pKickback.TransY = 0
	Kickback.Kick 0, Int(35 + KickbackBallVel*3/4 + Rnd()*20)
	Kickback.TimerEnabled = False
  end If
	Kickback.TimerEnabled  = False
	KickbackBallVel = BallVel(ActiveBall)
	Kickback.TimerInterval = 40
   	Kickback.TimerEnabled  = True
End Sub

Sub Kickback_Timer()
	If Kickback.TimerInterval = 40 Then
		Kickback.Kick 0, Int(35 + KickbackBallVel*3/4 + Rnd()*20)
		PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, pKickback
		Kickback.TimerInterval = 20
	ElseIf Kickback.TimerInterval = 20 Then
		pKickback.TransY = pKickback.TransY + 10
		If pKickback.TransY = 30 Then Kickback.TimerInterval = 500
	ElseIf Kickback.TimerInterval = 500 Then
		Kickback.TimerInterval = 50
	ElseIf Kickback.TimerInterval = 50 Then
		pKickback.TransY = pKickback.TransY - 3
		If pKickback.TransY = 00 Then Kickback.TimerEnabled = False
	End If

End Sub


' ****************************************************
' orbit gate
' ****************************************************
Sub SolOrbitGate(Enabled)
	GateTopRight.Open = Enabled
End Sub


' ****************************************************
' switches
' ****************************************************

' stand-up targets
Sub sw17_Hit()   : StandUpTarget 17, sw17, pTarget17, pTargetPost17, True  : TargetBouncer Activeball, 1: End Sub
Sub sw17_Timer() : StandUpTarget 17, sw17, pTarget17, pTargetPost17, False : End Sub
Sub sw18_Hit()   : StandUpTarget 18, sw18, pTarget18, pTargetPost18, True  : TargetBouncer Activeball, 1: End Sub
Sub sw18_Timer() : StandUpTarget 18, sw18, pTarget18, pTargetPost18, False : End Sub
Sub sw19_Hit()   : StandUpTarget 19, sw19, pTarget19, pTargetPost19, True  : TargetBouncer Activeball, 1: End Sub
Sub sw19_Timer() : StandUpTarget 19, sw19, pTarget19, pTargetPost19, False : End Sub
Sub sw20_Hit()   : StandUpTarget 20, sw20, pTarget20, pTargetPost20, True  : TargetBouncer Activeball, 1: End Sub
Sub sw20_Timer() : StandUpTarget 20, sw20, pTarget20, pTargetPost20, False : End Sub
Sub sw21_Hit()   : StandUpTarget 21, sw21, pTarget21, pTargetPost21, True  : TargetBouncer Activeball, 1: End Sub
Sub sw21_Timer() : StandUpTarget 21, sw21, pTarget21, pTargetPost21, False : End Sub
Sub sw25_Hit()   : StandUpTarget 25, sw25, pTarget25, pTargetPost25, True  : TargetBouncer Activeball, 1: End Sub
Sub sw25_Timer() : StandUpTarget 25, sw25, pTarget25, pTargetPost25, False : End Sub
Sub sw26_Hit()   : StandUpTarget 26, sw26, pTarget26, pTargetPost26, True  : TargetBouncer Activeball, 1: End Sub
Sub sw26_Timer() : StandUpTarget 26, sw26, pTarget26, pTargetPost26, False : End Sub
Sub sw27_Hit()   : StandUpTarget 27, sw27, pTarget27, pTargetPost27, True  : TargetBouncer Activeball, 1: End Sub
Sub sw27_Timer() : StandUpTarget 27, sw27, pTarget27, pTargetPost27, False : End Sub
Sub sw28_Hit()   : StandUpTarget 28, sw28, pTarget28, pTargetPost28, True  : TargetBouncer Activeball, 1: End Sub
Sub sw28_Timer() : StandUpTarget 28, sw28, pTarget28, pTargetPost28, False : End Sub
Sub sw36_Hit()   : StandUpTarget 36, sw36, pTarget36, pTargetPost36, True  : TargetBouncer Activeball, 1: End Sub
Sub sw36_Timer() : StandUpTarget 36, sw36, pTarget36, pTargetPost36, False : End Sub

' standup-targets in front of the motor bank
Sub sw29_Hit()   : StandUpTarget 29, sw29, pTarget29, pTargetPost29, True  : TargetBouncer Activeball, 1: End Sub
Sub sw29_Timer() : StandUpTarget 29, sw29, pTarget29, pTargetPost29, False : End Sub
Sub sw30_Hit()   : StandUpTarget 30, sw30, pTarget30, pTargetPost30, True  : TargetBouncer Activeball, 1: End Sub
Sub sw30_Timer() : StandUpTarget 30, sw30, pTarget30, pTargetPost30, False : End Sub
Sub sw31_Hit()   : StandUpTarget 31, sw31, pTarget31, pTargetPost31, True  : TargetBouncer Activeball, 1: End Sub
Sub sw31_Timer() : StandUpTarget 31, sw31, pTarget31, pTargetPost31, False : End Sub

' top lanes
Sub sw22_Hit()
	Controller.Switch(22) = True
	If orbit = 1 OR orbit2 = 1 Then
		orbit = 0
		orbit2 =0
	End If
End Sub

Sub sw22_Unhit() : Controller.Switch(22) = False : SolOrbitGate True : End Sub

Sub sw23_Hit()
	Controller.Switch(23) = True
	If orbit = 1 OR orbit2 = 1 Then
		orbit = 0
		orbit2 =0
	End If
End Sub

Sub sw23_Unhit() : Controller.Switch(23) = False : SolOrbitGate True : End Sub

Sub sw24_Hit()
	Controller.Switch(24) = True
	If orbit = 1 OR orbit2 = 1 Then
		orbit = 0
		orbit2 =0
	End If
End Sub

Sub sw24_Unhit() : Controller.Switch(24) = False : SolOrbitGate True : End Sub


' orbit lanes

Sub sw15_Hit()
	Controller.Switch(15) = True
	If orbit2 = 1 Then
		orbit = 0
		orbit2 = 0
	Else
		If orbit = 1 Then
			orbit = 0
			orbit2 = 1
		Else
			orbit = 0
			orbit2 = 0
		End If
	End If
End Sub


Sub sw15_Unhit() : Controller.Switch(15) = False : End Sub

Sub OrbitStart_hit
	orbit = 1
	orbit2 = 0
End Sub

Sub sw16_Hit()
	Controller.Switch(16) = True
	If orbit = 1 OR orbit2 = 1 Then
		orbit = 0
		orbit2 = 0
	End If
End Sub

Sub sw16_Unhit() : Controller.Switch(16) = False : End Sub

' inlanes and outlanes
Sub sw38_Hit() :   Controller.Switch(38) = True  : End Sub
Sub sw38_Unhit() : Controller.Switch(38) = False : End Sub
Sub sw39_Hit() :   Controller.Switch(39) = True  : End Sub
Sub sw39_Unhit() : Controller.Switch(39) = False : End Sub
Sub sw40_Hit() :   Controller.Switch(40) = True  : End Sub
Sub sw40_Unhit() : Controller.Switch(40) = False : End Sub

' kickback at left outlane
Sub sw51_Hit() :   Controller.Switch(51) = True  : End Sub
Sub sw51_Unhit() : Controller.Switch(51) = False : End Sub

' plunger switch
Sub sw14_Hit()
	Controller.Switch(14) = True
	BIPL = True
End Sub   

Sub sw14_UnHit()
	Controller.Switch(14) = False
	BIPL = False
End Sub

' mouse hole switches
Sub sw61_Hit()   : Controller.Switch(61) = True  : End Sub
Sub sw61_Unhit() : Controller.Switch(61) = False : End Sub
Sub sw60_Hit()
	Controller.Switch(60) = True
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub sw60_Unhit() : Controller.Switch(60) = False : End Sub

' bumpers
Sub Bumper1_Hit() : vpmTimer.PulseSw 52 : RandomSoundBumperTop Bumper1 : End Sub
Sub Bumper2_Hit() : vpmTimer.PulseSw 53 : RandomSoundBumperTop Bumper2 : End Sub
Sub Bumper3_Hit() : vpmTimer.PulseSw 54 : RandomSoundBumperBottom Bumper3 : End Sub

' ramp triggers
Sub sw47_Hit()
	vpmTimer.PulseSw 47
End Sub

Sub sw46_Hit()
	vpmTimer.PulseSw 46
	pRampTrigger46.transz=-6
End Sub

Sub sw46_Unhit()
	pRampTrigger46.transz=0
End Sub

Sub sw37_Hit()
	vpmTimer.PulseSw 37
	pRampTrigger37.transz=-6
End Sub

Sub sw37_Unhit()
	pRampTrigger37.transz=0
End Sub

Sub sw35_Hit()
	vpmTimer.PulseSw 35
	SlowDownBall ActiveBall,1
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub Triggeronrampleft_Hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub Triggeronrampleft2_Hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub Triggeronrampleft2_UnHit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub Triggerofframpright_Hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub Triggerofframpleft_Hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub Triggeronramprighttop_Hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub swRightRampStopper_Hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub Triggerofframpmouseout_Hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub


Sub sw44_Hit() : vpmTimer.PulseSw 44 : CheckGuideToTop : End Sub
Sub sw32_Hit() : vpmTimer.PulseSw 32 : End Sub


' ************************************************
' Stand Up Target Physics and Animation Code (mix if Herweh's and VPW's)
' ************************************************

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance


Sub StandUpTarget(id, sw, prim, primPost, isHit)
	If isHit And sw.TimerEnabled Then Exit Sub

'*****
' alternate way to achieve VPW target physics while maintaining old prims and animation

	if isHit = True Then
		DTBallPhysics Activeball, sw.orientation, STMass
	end If

'*****

	If id = 29 Or id = 30 Or id = 31 Or id = 36 Then prim.TransX = prim.TransX + 1 Else prim.TransZ = prim.TransZ + 1
	primPost.TransZ = primPost.TransZ + 1
	If primPost.TransZ >= 7 Then 'was originally 5
		If id = 29 Or id = 30 Or id = 31 Or id = 36 Then prim.TransX = 0 Else prim.TransZ = 0
		primPost.TransZ = 0
		sw.TimerEnabled = False
		Exit Sub
	End If
	If Not sw.TimerEnabled Then
		vpmTimer.PulseSw id
		sw.TimerInterval = 11
		sw.TimerEnabled = True
	End If
End Sub

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


' *********************************************************************
' mouse hole
' *********************************************************************
Sub MouseHole_Hit()
	SoundSaucerLock
	Controller.Switch(59) = True
End Sub
Sub SolMouseHoleRelease(Enabled)
	If Enabled Then
		SoundSaucerKick 1, MouseHole
		MouseHole.Kick 135, 2
		Controller.Switch(59) = False
	End If
End Sub

Sub TriggerRamp5Enter_Hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub


' *********************************************************************
' ball traps up and down
' *********************************************************************
Dim ballInLeftTrap  : Set ballInLeftTrap 	= Nothing
Dim ballInRightTrap : Set ballInRightTrap 	= Nothing
Dim isLeftTrapUp	: isLeftTrapUp			= False

Sub InitTraps()
	SolLeftTrapDown True
	SolRightTrapDown True
End Sub

' traps going up or down
Sub SolLeftTrapDown(Enabled)
	If Enabled Then
		TrapDown 34, leftTrap42, pLeftTrap, ballInLeftTrap
	End If
End Sub
Sub SolLeftTrapUp(Enabled)
	If Enabled Then
		TrapUp 34, 42, leftTrap42, pLeftTrap, ballInLeftTrap
	End If
End Sub
Sub SolRightTrapDown(Enabled)
	If Enabled Then
		TrapDown 33, rightTrap41, pRightTrap, ballInRightTrap
	End If
End Sub
Sub SolRightTrapUp(Enabled)
	If Enabled Then
		TrapUp 33, 41, rightTrap41, pRightTrap, ballInRightTrap
	End If
End Sub

' trap is hit by a ball
Sub leftTrap42_Hit()
	BallInTrap 34, 42, leftTrap42, pLeftTrap, ballInLeftTrap, ActiveBall
End Sub
Sub leftTrap42_Timer()
	MoveTrap 42, leftTrap42, pLeftTrap, ballInLeftTrap, 0
End Sub
Sub rightTrap41_Hit()
	BallInTrap 33, 41, rightTrap41, pRightTrap, ballInRightTrap, ActiveBall
End Sub
Sub rightTrap41_Timer()
	MoveTrap 41, rightTrap41, pRightTrap, ballInRightTrap, 0
End Sub

Sub TrapDown(id, trap, prim, ball)
	trap.Enabled = False
	PlaySoundAtLevelStatic ("fx_solenoid"), 1, trap
	Controller.Switch(id) = True
	' move trap prim and ball down
	trap.TimerEnabled = False
	MoveTrap 0, trap, prim, ball, 15
End Sub
Sub TrapUp(id, idBallInTrap, trap, prim, ball)
	trap.Enabled = True
	PlaySoundAtLevelStatic ("fx_solenoid"), 1, trap
	Controller.Switch(id) = False
	' move trap prim and ball up
	trap.TimerEnabled = False
	MoveTrap 0, trap, prim, ball, 11
End Sub

Sub BallInTrap(id, idBallInTrap, trap, prim, ball, ballA)
	PlaySoundAtLevelStatic ("fx_solenoid"), 1, trap
	Set ball = ballA : ball.VelX = 0 : ball.VelY = 0 : ball.VelZ = 0
	Controller.Switch(idBallInTrap) = True
	TrapDown id, trap, prim, ball
End Sub

Sub MoveTrap(idBallInTrap, trap, prim, ball, interval)
	If Not trap.TimerEnabled Then
		trap.TimerInterval = interval
		trap.TimerEnabled  = True
End If
    If trap.TimerInterval = 15 Then
        ' trap goes down
        prim.TransZ = prim.TransZ - 6
        If Not ball Is Nothing Then ball.Z = prim.transz + 25 
        If prim.TransZ <= -60 Then 
            trap.TimerEnabled = False
        End If
    ElseIf trap.TimerInterval = 11 Then
        ' trap goes up
        prim.TransZ = prim.TransZ + 12
        If Not ball Is Nothing Then ball.Z = prim.transz + 25 
        If prim.TransZ >= 0 Then
            trap.TimerInterval = 100
        End If
    Else
		trap.TimerEnabled = False
		' kick the trapped ball
		If Not ball Is Nothing Then
			trap.Kick 185, 0.5
			Controller.Switch(idBallInTrap) = False
			Set ball = Nothing
		End If
	End If
End Sub


' *********************************************************************
' ball diverter on ramp
' *********************************************************************
Sub InitMouseHoleDiverter()
	Controller.Switch(49) = False
	SolMouseHoleDiverter True
End Sub

Sub SolMouseHoleDiverter(Enabled)
	If Enabled Then
		Controller.Switch(49) = Not Controller.Switch(49)
		MoveMouseHoleDiverter
	End If
End Sub

Dim DivStep : DivStep = 0
Sub MoveMouseHoleDiverter()
	MouseHoleDiverterTimer.Enabled	= False
	PlaySoundAt "fx_popper", pDiverterArm
	WallDiverter.IsDropped			= Not (Controller.Switch(49))
	CheckGuideToTop
	DivStep							= IIF(Controller.Switch(49), -3, 3)
	MouseHoleDiverterTimer.Interval	= 11
	MouseHoleDiverterTimer_Timer
End Sub
Sub MouseHoleDiverterTimer_Timer()
	If Not MouseHoleDiverterTimer.Enabled Then MouseHoleDiverterTimer.Enabled = True
	If pDiverterArm.ObjRotZ > 23 Then
		pDiverterArm.ObjRotZ = 23
		MouseHoleDiverterTimer.Enabled	= False
	ElseIf pDiverterArm.ObjRotZ < 0 Then
		pDiverterArm.ObjRotZ = 0
		MouseHoleDiverterTimer.Enabled	= False
	End If
	If MouseHoleDiverterTimer.Enabled Then
		pDiverterArm.ObjRotZ = pDiverterArm.ObjRotZ + DivStep
	End If
End Sub

Sub CheckGuideToTop()
	WallGuideToTop.IsDropped = Not WallDiverter.IsDropped
End Sub
Sub TriggerGuideToTop_Hit()
	WallGuideToTop.IsDropped = True
End Sub

' *********************************************************************
' target motor bank
' *********************************************************************
Dim MBStep : MBStep = -1
Dim MotorIsOn: MotorIsOn = False
Dim isMotorbankUp: isMotorbankUp = True

Sub InitMotorBank()
	Controller.Switch(43) 			= True
	Controller.Switch(50) 			= False
	MotorBankTimer.Interval 		= 39
	MotorBankTimer.Enabled  		= False
End Sub

Sub SolMotorBank(Enabled)
'	debug.print "MotorBank: " & Enabled & " " & Controller.Switch(43)  & " " & Controller.Switch(50)

	If Enabled Then
		MotorBankTimer.Interval = 150
		MotorBankTimer.Enabled = True
		MotorIsOn = True
	Else
		MotorIsOn = False
	End If

End Sub

Sub MotorBankTimer_Timer()
'	debug.print pTarget29.TransY
	If MotorisOn = False and (pTarget29.TransY = 0 or pTarget29.TransY = -53)  Then 
		me.enabled = false
		StopSound "fx_motor"
		Exit Sub
	Elseif me.interval <> 39 then
		PlayLoopSoundAtVol "fx_motor", sw30, 1
		me.interval = 39
	End If

	Dim obj

	For Each obj In Array(pTarget29, pTarget30, pTarget31, pTargetPost29, pTargetPost30, pTargetPost31)
		obj.TransY = obj.TransY + MBStep
	Next
	pMotorbankWall.TransZ = pMotorbankWall.TransZ + MBStep

	If pTarget29.TransY = -53 Or pTarget29.TransY = 0 Then
		MBStep = - MBStep
	End If

	If pTarget29.TransY <= -44 Then
		Controller.Switch(50) = True
		For Each obj In Array(sw29, sw30, sw31, MotorbankWall) : obj.Collidable = False : Next
	Else
		Controller.Switch(50) = False
	End If

	If pTarget29.TransY >= -9 Then
		Controller.Switch(43) = True
		isMotorbankUp = True
		For Each obj In Array(sw29, sw30, sw31, MotorbankWall) : obj.Collidable = True : Next
	Else
		Controller.Switch(43) = False
		isMotorbankUp = False
	End If
	UpdateGILightsUnderCheeseLoop
End Sub

' *********************************************************************
' Slow down orbit
' *********************************************************************

Sub trSlowdownOrbit_Hit()
  If orbit2 = 1 Then
	SlowDownBall ActiveBall,2
	orbit = 0
	orbit2 = 0
  End If
End Sub

Sub SlowDownBall(aBall, mode)
	If aBall.VelY > 0 Then
		If mode = 1 And BallVel(aBall) >= 8 Then
			With aBall : .VelX = .VelX / 1.5 : .VelY = .VelY / 1.5 : End With
		ElseIf mode = 2 Then
			With aBall : .VelX = .VelX / 4 : .VelY = .VelY / 4 : End With
		End If
	End If
End Sub


' *********************************************************************
' lamps and illumination
' *********************************************************************

' inserts
Dim currentGILevel, isGIOn

' flasher
Const minFlasherNo = 15
Const maxFlasherNo = 32
ReDim fValue(maxFlasherNo,14) : For i = minFlasherNo To maxFlasherNo : fValue(i,0) = 0 : Next

Sub InitFlasher()
	fValue(15,4) 		= Array(fLight15,fBulb15)
	fValue(15,5) 		= WhiteFlasher
	set	fValue(25,1) 	= BGFlS25
	fValue(25,4) 		= Array(fLight25,fPlasticsLight25,fBulb25)
	fValue(25,5) 		= WhiteFlasher
	set fValue(26,1) 	= BGFlS26
	fValue(26,4) 		= Array(fLight26,fPlasticsLight26,fBulb26)
	fValue(26,5) 		= WhiteFlasher
	set fValue(27,1) 	= BGFlS27
	fValue(27,4) 		= Array(fLight27,fPlasticsLight27,fBulb27,fLight27a,fPlasticsLight27a,fBulb27a,fLight27b)
	fValue(27,5) 		= WhiteFlasher
	fValue(29,4) 		= Array(fLight29,fPlasticsLight29,fBulb29)
	fValue(29,5) 		= WhiteFlasher
	set fValue(30,1) 	= BGFlS30
	fValue(30,4) 		= Array(fLight30,fPlasticsLight30,fBulb30,fLight30a,fPlasticsLight30a,fBulb30a,fLight30b,fPlasticsLight30b,fBulb30b)
	fValue(30,5) 		= WhiteFlasher
	set fValue(31,1) 	= BGFlS31
	fValue(31,4) 		= Array(fLight31,fPlasticsLight31,fBulb31,fLight31a,fPlasticsLight31a,fBulb31a)
	fValue(31,5) 		= WhiteFlasher
	Set fValue(32,4) 	= fLight32
	fValue(32,5) 		= WhiteFlasher
	' start flasher timer
	FlasherTimer.Interval = 25
	If Not FlasherTimer.Enabled Then FlasherTimer_Timer
End Sub

Sub Flash(flasherNo, flasherValue)
	If EnableFlasher = 0 Then Exit Sub
	' set value
	fValue(flasherNo,0) = IIF(flasherValue,1,fDecrease(fValue(flasherNo,7)))
	' start flasher timer
	If Not FlasherTimer.Enabled Then FlasherTimer_Timer
End Sub
Sub FlasherTimer_Timer()	
	Dim ii, allZero, flashx3, matdim, obj
	allZero = True
	If Not FlasherTimer.Enabled Then 
		FlasherTimer.Enabled = True
	End If
	For ii = minFlasherNo To maxFlasherNo
		If (IsObject(fValue(ii,1)) Or IsObject(fValue(ii,4)) Or IsArray(fValue(ii,4))) And fValue(ii,0) >= 0 Then
			allZero = False
			if BGFlashers = 1 Then
				If IsObject(fValue(ii,1)) Then
					If Not fValue(ii,1).Visible Then 
						fValue(ii,1).Visible = True : If IsObject(fValue(ii,2)) Then fValue(ii,2).Visible = True
					End If
			end If
			End If
			' calc values
			flashx3 = fValue(ii,0) ^ 3
			matdim 	= Round(10 * fValue(ii,0))
			' set flasher object values
			if BGFlashers = 1 Then
				If IsObject(fValue(ii,1)) Then fValue(ii,1).Opacity = fOpacity(BGFlasher) * flashx3
			end If
			If IsObject(fValue(ii,2)) Then fValue(ii,2).BlendDisableLighting = 10 * flashx3 : fValue(ii,2).Material = "domelit" & matdim : fValue(ii,2).Visible = Not (fValue(ii,0) < 0.15)
			If IsObject(fValue(ii,3)) Then fValue(ii,3).BlendDisableLighting =  flashx3
			If IsObject(fValue(ii,4)) Then fValue(ii,4).IntensityScale = flashx3 : fValue(ii,4).State = IIF(fValue(ii,4).IntensityScale<=0,0,1)
			If IsArray(fValue(ii,4)) Then
				For Each obj In fValue(ii,4) : obj.IntensityScale = flashx3 : obj.State = IIF(obj.IntensityScale<=0,0,1) : Next
			End If
			' decrease flasher value
			If fValue(ii,0) < 1 Then fValue(ii,0) = fValue(ii,0) * fDecrease(fValue(ii,5)) - 0.01
			' some special handling for flasher
			If Not isGIOn Then
				If ii = 31 Then
					pLeftTrap.Material = "Plastic with an image" & fMaterial(fValue(ii,0))
				ElseIf ii = 30 Then
					pRightTrap.Material = "Plastic with an image" & fMaterial(fValue(ii,0))
				End If
			End If
		End If
	Next
	If allZero Then
		FlasherTimer.Enabled = False
	End If
End Sub
Function fOpacity(fColor)
	If fColor = RedFlasher Then
		fOpacity = RedFlasherOpacity
	ElseIf fColor = BlueFlasher Then
		fOpacity = BlueFlasherOpacity
	ElseIf fColor = BGFlasher Then
		fOpacity = BGFlasherOpacity
	Else
		fOpacity = WhiteFlasherOpacity
	End If
End Function
Function fDecrease(fColor)
	If fColor = RedFlasher Then
		fDecrease = RedFlasherDecrease
	ElseIf fColor = BlueFlasher Then
		fDecrease = BlueFlasherDecrease
	Else
		fDecrease = WhiteFlasherDecrease
	End If
End Function
Function fMaterial(fValue)
	If fValue > 0.8 Then
		fMaterial = ""
	ElseIf fValue > 0.6 Then
		fMaterial = " 0.7"
	ElseIf fValue > 0.4 Then
		fMaterial = " 0.5"
	ElseIf fValue > 0.2 Then
		fMaterial = " 0.2"
	Else
		fMaterial = " Dark"
	End If
End Function

Dim WhiteFlasher, WhiteFlasherOpacity, WhiteFlasherDecrease, BGFlasher, BGFlasherOpacity
WhiteFlasher			= 1
WhiteFlasherOpacity		= 300
WhiteFlasherDecrease	= 0.9

Dim RedFlasher, RedFlasherOpacity, RedFlasherDecrease
RedFlasher				= 2
RedFlasherOpacity		= 500
RedFlasherDecrease		= 0.85

Dim BlueFlasher, BlueFlasherOpacity, BlueFlasherDecrease
BlueFlasher				= 3
BlueFlasherOpacity		= 2000
BlueFlasherDecrease		= 0.85

Dim YellowInsertFlasher
YellowInsertFlasher		= 4


'*****************
'Flasher Caps subs
'*****************

Sub Flash28(Enabled)
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
	End If
	Sound_Flash_Relay enabled, Flasherbase1
End Sub

' general illumination
Sub ResetGI()
	InitGI False
	BGFlasherOpacity		= VRBGFlashHigh
End Sub
Sub InitGI(startUp)
	If startUp Then
		isGIOn = False
		SolGI False
		BGFlasherOpacity		= VRBGFlashHigh
	End If
	Dim coll, obj
	' init GI overhead
	With GIOverhead
		.IntensityScale 	= IIF(isGIOn,1,0)
		.visible			= 1
		If GIColorMod = 1 Then
			.Color			= YellowOverhead
'			.ColorFull 		= YellowOverheadFull
'			.Intensity 		= YellowOverheadI
		ElseIf GIColorMod = 2 Then
			.Color			= RedOverhead
'			.ColorFull 		= RedOverheadFull
'			.Intensity 		= RedOverheadI
		ElseIf GIColorMod = 3 Then
			.Color			= BlueOverhead
'			.ColorFull 		= BlueOverheadFull
'			.Intensity 		= BlueOverheadI
		Else
			.Color			= WhiteOverhead
'			.ColorFull 		= WhiteOverheadFull
'			.Intensity 		= WhiteOverheadI
		End If
	End With
	' init GI bulbs
	For Each obj In GIBulbs
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 Then
			obj.Color		= YellowBulbs
			obj.ColorFull	= YellowBulbsFull
			obj.Intensity 	= YellowBulbsI * EnableGI
		ElseIf GIColorMod = 2 Then
			obj.Color		= RedBulbs
			obj.ColorFull	= RedBulbsFull
			obj.Intensity 	= RedBulbsI * EnableGI
		ElseIf GIColorMod = 3 Then
			obj.Color		= BlueBulbs
			obj.ColorFull	= BlueBulbsFull
			obj.Intensity 	= BlueBulbsI * EnableGI
		Else
			obj.Color		= WhiteBulbs
			obj.ColorFull 	= WhiteBulbsFull
			obj.Intensity 	= WhiteBulbsI * EnableGI
		End If
	Next
	' init GI lights
	For Each obj in GILeft
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 Then
			obj.Color		= Yellow
			obj.ColorFull	= YellowFull
			obj.Intensity 	= YellowI * EnableGI
		ElseIf GIColorMod = 2 Then
			obj.Color		= Red
			obj.ColorFull	= RedFull
			obj.Intensity 	= RedI * EnableGI
		ElseIf GIColorMod = 3 Then
			obj.Color		= Blue
			obj.ColorFull	= BlueFull
			obj.Intensity 	= BlueI * EnableGI
		Else
			obj.Color		= White
			obj.ColorFull	= WhiteFull
			obj.Intensity 	= WhiteI * EnableGI
		End If
	Next
	For Each obj In GIPlasticsLeft
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 Then
			obj.Color		= Yellow
			obj.ColorFull	= YellowFull
			obj.Intensity 	= YellowPlasticI * EnableGI
		ElseIf GIColorMod = 2 Then
			obj.Color		= Red
			obj.ColorFull	= RedFull
			obj.Intensity 	= RedPlasticI * EnableGI
		ElseIf GIColorMod = 3 Then
			obj.Color		= Blue
			obj.ColorFull	= BlueFull
			obj.Intensity 	= BluePlasticI * EnableGI
		Else
			obj.Color		= White
			obj.ColorFull	= WhiteFull
			obj.Intensity 	= WhitePlasticI * EnableGI
		End If
	Next
	For Each obj in GIRight
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 Then
			obj.Color		= Yellow
			obj.ColorFull	= YellowFull
			obj.Intensity 	= YellowI * EnableGI
		ElseIf GIColorMod = 2 Then
			obj.Color		= Blue
			obj.ColorFull	= BlueFull
			obj.Intensity 	= BlueI * EnableGI
		ElseIf GIColorMod = 3 Then
			obj.Color		= Red
			obj.ColorFull	= RedFull
			obj.Intensity 	= RedI * EnableGI
		Else
			obj.Color		= White
			obj.ColorFull	= WhiteFull
			obj.Intensity 	= WhiteI * EnableGI
		End If
	Next
	For Each obj In GIPlasticsRight
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 Then
			obj.Color		= Yellow
			obj.ColorFull	= YellowFull
			obj.Intensity 	= YellowPlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		ElseIf GIColorMod = 2 Then
			obj.Color		= Blue
			obj.ColorFull	= BlueFull
			obj.Intensity 	= BluePlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		ElseIf GIColorMod = 3 Then
			obj.Color		= Red
			obj.ColorFull	= RedFull
			obj.Intensity 	= RedPlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		Else
			obj.Color		= White
			obj.ColorFull	= WhiteFull
			obj.Intensity 	= WhitePlasticI * EnableGI * IIF(obj.TimerInterval=-2,0.5,1)
		End If
	Next
	' init flasher bulbs
	For Each obj In FlasherBulbs
		obj.IntensityScale 	= IIF(isGIOn,1,0)
		obj.State			= LightStateOn
		If GIColorMod = 1 Then
			obj.Color		= YellowBulbs
			obj.ColorFull	= YellowBulbsFull
			obj.Intensity 	= YellowBulbsI * EnableGI
		ElseIf GIColorMod = 2 Then
			obj.Color		= RedBulbs
			obj.ColorFull	= RedBulbsFull
			obj.Intensity 	= RedBulbsI * EnableGI
		ElseIf GIColorMod = 3 Then
			obj.Color		= BlueBulbs
			obj.ColorFull	= BlueBulbsFull
			obj.Intensity 	= BlueBulbsI * EnableGI
		Else
			obj.Color		= WhiteBulbs
			obj.ColorFull 	= WhiteBulbsFull
			obj.Intensity 	= WhiteBulbsI * EnableGI
		End If
	Next
End Sub

dim gilvl:gilvl = 1
Dim GIDir : GIDir = 0
Dim GIStep : GIStep = 0
Sub SolGI(IsOff)
	If EnableGI = 0 And Not isGIOn Then Exit Sub
	If isGIOn <> (Not IsOff) Then
		isGIOn = Not IsOff
		If isGIOn Then
			' GI goes on
			Sound_GI_Relay IsOff, Relay_GI
			Sound_Flash_Relay IsOff, Relay_Backglass_Center
			FlBumperFadeTarget(1) = .9
			FlBumperFadeTarget(2) = .9
			FlBumperFadeTarget(3) = .9
			GIDir = 1 : GITimer_Timer
			DOF 101, DOFOn
			Pincab_Backglass.image = "Mousin Around Illuminated"
			PinCab_Backglass.blenddisablelighting = .7
			if VR_Room = 0 and cab_mode = 0 Then
				L_DT_LadyMouse.state = 1
				L_DT_Mouse.state = 1
			Else
				L_DT_LadyMouse.state = 0
				L_DT_Mouse.state = 0
			End If
			fMouseHole.opacity = 800
			SetLamp 111, 1 'Prim and Ball GI Updates
			SetLamp 104, 0 'Insert GI Updates
			BGFlasherOpacity		= VRBGFlashLow
			gilvl = 1
		Else
			' GI goes off
			GIDir = -1 : GITimer_Timer
			DOF 101, DOFOff
			FlBumperFadeTarget(1) = .1
			FlBumperFadeTarget(2) = .1
			FlBumperFadeTarget(3) = .1
			Pincab_Backglass.image = "Mousin Around Dark"
			PinCab_Backglass.blenddisablelighting = .4
			L_DT_LadyMouse.state = 0
			L_DT_Mouse.state = 0
			fMouseHole.opacity = 80
			SetLamp 111, 0 'Prim and Ball GI Updates
			SetLamp 104, 1 'Insert GI Updates
			BGFlasherOpacity		= VRBGFlashHigh
			gilvl = 0
		End If
	End If
End Sub

Sub GITimer_Timer()
	If Not GITimer.Enabled Then GITimer.Enabled = True
	GIStep 			= GIStep + GIDir
	' set opacity of the shadow overlays
	fGIOff.Opacity 	= (ShadowOpacityGIOff / 4) * (4 - GIStep)
	fGIOn.Opacity 	= (ShadowOpacityGIOn / 4) * GIStep
	' set GI illumination
	Dim coll, obj
	For Each coll In Array(GILeft,GIPlasticsLeft,GIRight,GIPlasticsRight,GIBulbs)
		For Each obj In coll
			If obj.TimerInterval <> -1 Then obj.IntensityScale = GIStep/4
		Next
	Next
	UpdateGILightsUnderCheeseLoop
	GIOverhead.IntensityScale = GIStep/4

	' targets, posts, pegs etc
	If GIStep = 4 Then
		For Each obj In GIYellowTargets : obj.Material = "TargetYellow" : Next : sw36.Material = "Plastic Yellow1"
		For Each obj In GIWhiteTargets : obj.Material = "TargetWhite" : Next
		For Each obj In GIYellowPosts : obj.Material = "Plastic White" : Next
		For Each obj In GIWhitePosts : obj.Material = "Plastic White" : Next
		For Each obj In GIPlasticPegs : obj.Material = "TransparentPlasticWhite2" : Next
		For Each obj In GIRubbers : obj.Material = "Rubber White" : Next
		For Each obj In GITopLanes : obj.Material = "Plastic Blue" : Next
		For Each obj In GIWireTrigger: obj.Material = "Metal0.8" : Next
		For Each obj In GILocknuts: obj.Material = "Metal0.8" : Next
		For Each obj In GIScrews: obj.Material = "Metal0.8" : Next
		For Each obj In GIGates: obj.Material = "Metal Wires" : Next
		For Each obj In GIRampDecals : obj.Material = "Plastics Light Light Dark" : Next
		For Each obj In Array(RampSideDecalLeft,RampSideDecalRight) : obj.SideMaterial = "Plastics Light Light Dark" : Next
		For Each obj In Array(RampDecalYow1,RampDecalYow2) : obj.Material = "Plastics Light Light Dark" : Next
		For Each obj In Array(RampStopper1,RampStopper2,RampStopper3) : obj.Material = "Plastics" : Next
		For Each obj In Array(pLeftTrap,pRightTrap,pCheeseLoopHut,pGateTopLeft,pGateTopRight,pLeftFlipperBally,pRightFlipperBally) : obj.Material = "Plastic with an image" : Next
		For Each obj In Array(pDiverterAxisA,pDiverterAxisB,rBBF1,rBBF2,rBBF3) : obj.Material = "Metal Chrome S34" : Next
		For Each obj In Array(rMetalWall1,rMetalWall2,rMetalWall3,rMetalWall4,rMetalWall5,rMetalWall6,rMetalWall7,rMetalWall8,rMetalWall9) : obj.Material = "Metal Chrome S34" : Next
		For Each obj In Array(pCheeseLoopRamp,pGuideLaneLeft,pGuideLaneRight,pRampTrigger37,pRampTrigger46) : obj.Material = "Metal S34" : Next
		For Each obj In Array(pMetalGate,pDiverterArm,pDiverterFixer) : obj.Material = "Metal Diverter" : Next
		For Each obj In Array(pCheeseLoopGate,pCheeseLoopFixer,pCheeseLoopNutC,pCheeseLoopNutD) : obj.Material = "Metal" : Next
		pLevelPlate.Material = "Metal0.8"
		If FlipperColorMod = 2 Then
			pLeftFlipperBat.Material = "flipperbatyellow" : pRightFlipperBat.Material = "flipperbatyellow"
		ElseIf FlipperColorMod = 3 Then
			pLeftFlipperBat.Material = "flipperbatred" : pRightFlipperBat.Material = "flipperbatred"
		ElseIf FlipperColorMod = 4 Then
			pLeftFlipperBat.Material = "flipperbatblue" : pRightFlipperBat.Material = "flipperbatblue"
		End If
		wBackboard.IsDropped = False : wBackboardDark.IsDropped = True
	ElseIf GIStep = 0 Then
		For Each obj In GIYellowTargets : obj.Material = "TargetYellow Dark" : Next : sw36.Material = "Plastic Yellow Dark"
		For Each obj In GIWhiteTargets : obj.Material = "TargetWhite Dark" : Next
		For Each obj In GIYellowPosts : obj.Material = "Plastic White Dark" : Next
		For Each obj In GIWhitePosts : obj.Material = "Plastic White Dark" : Next
		For Each obj In GIPlasticPegs : obj.Material = "TransparentPlasticWhite2 Dark" : Next
		For Each obj In GIRubbers : obj.Material = "Rubber White Dark" : Next
		For Each obj In GIWireTrigger: obj.Material = "Metal0.8 Dark" : Next
		For Each obj In GILocknuts: obj.Material = "Metal0.8 Dark" : Next
		For Each obj In GIScrews: obj.Material = "Metal0.8 Dark" : Next
		For Each obj In GITopLanes : obj.Material = "Plastic Blue Dark" : Next
		For Each obj In GIGates: obj.Material = "Metal Wires Dark" : Next
		For Each obj In GIRampDecals : obj.Material = "Plastics Light Dark" : Next
		For Each obj In Array(RampSideDecalLeft,RampSideDecalRight) : obj.SideMaterial = "Plastics Light Dark" : Next
		For Each obj In Array(RampDecalYow1,RampDecalYow2) : obj.Material = "Plastics Dark" : Next
		For Each obj In Array(RampStopper1,RampStopper2,RampStopper3) : obj.Material = "Plastics Light Dark" : Next
		For Each obj In Array(pLeftTrap,pRightTrap,pCheeseLoopHut,pGateTopLeft,pGateTopRight,pLeftFlipperBally,pRightFlipperBally) : obj.Material = "Plastic with an image Dark" : Next
		For Each obj In Array(pDiverterAxisA,pDiverterAxisB,rBBF1,rBBF2,rBBF3) : obj.Material = "Metal Chrome S34 Dark" : Next
		For Each obj In Array(rMetalWall1,rMetalWall2,rMetalWall3,rMetalWall4,rMetalWall5,rMetalWall6,rMetalWall7,rMetalWall8,rMetalWall9) : obj.Material = "Metal Chrome S34 Dark" : Next
		For Each obj In Array(pCheeseLoopRamp,pGuideLaneLeft,pGuideLaneRight,pRampTrigger37,pRampTrigger46) : obj.Material = "Metal S34 Dark" : Next
		For Each obj In Array(pMetalGate,pDiverterArm,pDiverterFixer) : obj.Material = "Metal Diverter Dark" : Next
		For Each obj In Array(pCheeseLoopGate,pCheeseLoopFixer,pCheeseLoopNutC,pCheeseLoopNutD) : obj.Material = "Metal Dark" : Next
		pLevelPlate.Material = "Metal0.8 Dark"
		If FlipperColorMod = 2 Then
			pLeftFlipperBat.Material = "flipperbatyellow Dark" : pRightFlipperBat.Material = "flipperbatyellow Dark"
		ElseIf FlipperColorMod = 3 Then
			pLeftFlipperBat.Material = "flipperbatred Dark" : pRightFlipperBat.Material = "flipperbatred Dark"
		ElseIf FlipperColorMod = 4 Then
			pLeftFlipperBat.Material = "flipperbatblue Dark" : pRightFlipperBat.Material = "flipperbatblue Dark"
		End If
		wBackboard.IsDropped = True : wBackboardDark.IsDropped = False
	End If

	' ramps
	pRampLeft.Material = "rampsGI" & (GIStep * 2)
	pRampRight.Material = "rampsGI" & (GIStep * 2)
	pRampCenter.Material = "rampsGI" & (GIStep * 2)

	pRampLeft.blenddisablelighting = GIStep*.1
	pRampRight.blenddisablelighting = GIStep*.05
	pRampCenter.blenddisablelighting = GIStep*.1

	' GI on/off goes in 4 steps so maybe stop timer
	If (GIDir = 1 And GIStep = 4) Or (GIDir = -1 And GIStep = 0) Then
		GITimer.Enabled = False
	End If
End Sub

Sub UpdateGILightsUnderCheeseLoop()
	GILightL8.IntensityScale 	= IIF(isMotorbankUp, GIStep/4, 0)
	GILightL8a.IntensityScale 	= IIF(isMotorbankUp, 0, GIStep/4)
	GILightR8.IntensityScale 	= IIF(isMotorbankUp, GIStep/4, 0)
	GILightR8a.IntensityScale 	= IIF(isMotorbankUp, 0, GIStep/4)
End Sub


' *********************************************************************
' colors
' *********************************************************************
Dim White, WhiteFull, WhiteI, WhiteP, WhitePlastic, WhitePlasticFull, WhitePlasticI, WhiteBumper, WhiteBumperFull, WhiteBumperI, WhiteBulbs, WhiteBulbsFull, WhiteBulbsI, WhiteOverheadFull, WhiteOverhead, WhiteOverheadI
WhiteFull = rgb(255,255,255) 
White = rgb(255,255,180)
WhiteI = 10 '15
WhitePlasticFull = rgb(255,255,180) 
WhitePlastic = rgb(255,255,180)
WhitePlasticI = 20 '25
WhiteBumperFull = rgb(255,255,180) 
WhiteBumper = rgb(255,255,180)
WhiteBumperI = 1
WhiteBulbsFull = rgb(255,255,180)
WhiteBulbs = rgb(255,255,180)
WhiteBulbsI = 100 * ShadowOpacityGIOff
WhiteOverheadFull = rgb(255,255,180)
WhiteOverhead = rgb(255,255,180)
WhiteOverheadI = .25

Dim Yellow, YellowFull, YellowI, YellowPlastic, YellowPlasticFull, YellowPlasticI, YellowBumper, YellowBumperFull, YellowBumperI, YellowBulbs, YellowBulbsFull, YellowBulbsI, YellowOverheadFull, YellowOverhead, YellowOverheadI
YellowFull = rgb(255,255,0)
Yellow = rgb(255,255,0)
YellowI = 4 '5
YellowPlasticFull = rgb(255,255,0)
YellowPlastic = rgb(255,255,0)
YellowPlasticI =  25 '40
YellowBumperFull = rgb(255,255,0)
YellowBumper = rgb(255,255,0)
YellowBumperI = 1
YellowBulbsFull = rgb(255,255,0)
YellowBulbs = rgb(255,255,0)
YellowBulbsI = 250 * ShadowOpacityGIOff
YellowOverheadFull = rgb(255,255,10)
YellowOverhead = rgb(255,255,10)
YellowOverheadI = 0.5

Dim Red, RedFull, RedI, RedPlastic, RedPlasticFull, RedPlasticI, RedBumper, RedBumperFull, RedBumperI, RedBulbs, RedBulbsFull, RedBulbsI, RedOverheadFull, RedOverhead, RedOverheadI
RedFull = rgb(255,75,75)
Red = rgb(255,75,75)
RedI = 10 '15
RedPlasticFull = rgb(255,75,75)
RedPlastic = rgb(255,75,75)
RedPlasticI = 20
RedBumperFull = rgb(255,0,0)
RedBumper = rgb(255,0,0)
RedBumperI = 2
RedBulbsFull = rgb(255,75,75)
RedBulbs = rgb(255,75,75)
RedBulbsI = 250 * ShadowOpacityGIOff
RedOverheadFull = rgb(255,10,10)
RedOverhead = rgb(255,10,10)
RedOverheadI = 1

Dim Blue, BlueFull, BlueI, BluePlastic, BluePlasticFull, BluePlasticI, BlueBumper, BlueBumperFull, BlueBumperI, BlueBulbs, BlueBulbsFull, BlueBulbsI,  BlueOverheadFull, BlueOverhead, BlueOverheadI
BlueFull = rgb(75,75,255)
Blue = rgb(75,75,255)
BlueI = 20 '50
BluePlasticFull = rgb(75,75,255)
BluePlastic = rgb(75,75,255)
BluePlasticI = 20
BlueBumperFull = rgb(0,0,255)
BlueBumper = rgb(0,0,255)
BlueBumperI = 1
BlueBulbsFull = rgb(75,75,255)
BlueBulbs = rgb(75,75,255)
BlueBulbsI = 125 * ShadowOpacityGIOff
BlueOverheadFull = rgb(10,10,255)
BlueOverhead = rgb(10,10,255)
BlueOverheadI = .8


'********************************************
'              Display Output
'********************************************

dim DisplayColor, DisplayColorG
DisplayColor =  RGB(255,40,1)

Sub Displaytimer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In Digits(num)
 '                  If chg And 1 Then obj.visible=stat And 1    'if you use the object color for off; turn the display object visible to not visible on the playfield, and uncomment this line out.
					If chg And 1 Then FadeDisplay obj, stat And 1	
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If
End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 1200
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 600
	End If
End Sub

Dim Digits(32)

Digits(0)=Array(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15)
Digits(1)=Array(D16,D17,D18,D19,D20,D21,D22,D23,D24,D25,D26,D27,D28,D29,D30)
Digits(2)=Array(D31,D32,D33,D34,D35,D36,D37,D38,D39,D40,D41,D42,D43,D44,D45)
Digits(3)=Array(D46,D47,D48,D49,D50,D51,D52,D53,D54,D55,D56,D57,D58,D59,D60)
Digits(4)=Array(D61,D62,D63,D64,D65,D66,D67,D68,D69,D70,D71,D72,D73,D74,D75)
Digits(5)=Array(D76,D77,D78,D79,D80,D81,D82,D83,D84,D85,D86,D87,D88,D89,D90)
Digits(6)=Array(D91,D92,D93,D94,D95,D96,D97,D98,D99,D100,D101,D102,D103,D104,D105)
Digits(7)=Array(D106,D107,D108,D109,D110,D111,D112,D113,D114,D115,D116,D117,D118,D119,D120)
Digits(8)=Array(D121,D122,D123,D124,D125,D126,D127,D128,D129,D130,D131,D132,D133,D134,D135)
Digits(9)=Array(D136,D137,D138,D139,D140,D141,D142,D143,D144,D145,D146,D147,D148,D149,D150)
Digits(10)=Array(D151,D152,D153,D154,D155,D156,D157,D158,D159,D160,D161,D162,D163,D164,D165)
Digits(11)=Array(D166,D167,D168,D169,D170,D171,D172,D173,D174,D175,D176,D177,D178,D179,D180)
Digits(12)=Array(D181,D182,D183,D184,D185,D186,D187,D188,D189,D190,D191,D192,D193,D194,D195)
Digits(13)=Array(D196,D197,D198,D199,D200,D201,D202,D203,D204,D205,D206,D207,D208,D209,D210)
Digits(14)=Array(D211,D212,D213,D214,D215,D216,D217,D218,D219,D220,D221,D222,D223,D224,D225)
Digits(15)=Array(D226,D227,D228,D229,D230,D231,D232,D233,D234,D235,D236,D237,D238,D239,D240)

Digits(16)=Array(D241,D242,D243,D244,D245,D246,D247,D248,D249,D250,D251,D252,D253,D254,D255)
Digits(17)=Array(D256,D257,D258,D259,D260,D261,D262,D263,D264,D265,D266,D267,D268,D269,D270)
Digits(18)=Array(D271,D272,D273,D274,D275,D276,D277,D278,D279,D280,D281,D282,D283,D284,D285)
Digits(19)=Array(D286,D287,D288,D289,D290,D291,D292,D293,D294,D295,D296,D297,D298,D299,D300)
Digits(20)=Array(D301,D302,D303,D304,D305,D306,D307,D308,D309,D310,D311,D312,D313,D314,D315)
Digits(21)=Array(D316,D317,D318,D319,D320,D321,D322,D323,D324,D325,D326,D327,D328,D329,D330)
Digits(22)=Array(D331,D332,D333,D334,D335,D336,D337,D338,D339,D340,D341,D342,D343,D344,D345)
Digits(23)=Array(D346,D347,D348,D349,D350,D351,D352,D353,D354,D355,D356,D357,D358,D359,D360)
Digits(24)=Array(D361,D362,D363,D364,D365,D366,D367,D368,D369,D370,D371,D372,D373,D374,D375)
Digits(25)=Array(D376,D377,D378,D379,D380,D381,D382,D383,D384,D385,D386,D387,D388,D389,D390)
Digits(26)=Array(D391,D392,D393,D394,D395,D396,D397,D398,D399,D400,D401,D402,D403,D404,D405)
Digits(27)=Array(D406,D407,D408,D409,D410,D411,D412,D413,D414,D415,D416,D417,D418,D419,D420)
Digits(28)=Array(D421,D422,D423,D424,D425,D426,D427,D428,D429,D430,D431,D432,D433,D434,D435)
Digits(29)=Array(D436,D437,D438,D439,D440,D441,D442,D443,D444,D445,D446,D447,D448,D449,D450)
Digits(30)=Array(D451,D452,D453,D454,D455,D456,D457,D458,D459,D460,D461,D462,D463,D464,D465)
Digits(31)=Array(D466,D467,D468,D469,D470,D471,D472,D473,D474,D475,D476,D477,D478,D479,D480)


Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(Digits)
		if IsArray(Digits(x) ) then
			For each obj in Digits(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

If VR_Room=1 Then
	InitDigits
End If


 '**********************************************************************************************************
'Digital Display on backdrop for desktop mode
'**********************************************************************************************************

Dim DTDigits(32)

DTDigits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
DTDigits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
DTDigits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
DTDigits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
DTDigits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
DTDigits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
DTDigits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
DTDigits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
DTDigits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
DTDigits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
DTDigits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
DTDigits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
DTDigits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
DTDigits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
DTDigits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
DTDigits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
DTDigits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
DTDigits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
DTDigits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
DTDigits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
DTDigits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
DTDigits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
DTDigits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
DTDigits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
DTDigits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
DTDigits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
DTDigits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
DTDigits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
DTDigits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
DTDigits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
DTDigits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
DTDigits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)

Sub DisplayTimerDT
    Dim chgLED, ii, num, chg, stat, obj
	chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(chgLED) Then
			For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
				if (num < 32) then
					For Each obj In DTDigits(num)
						If chg And 1 Then obj.State = stat And 1
						chg = chg\2 : stat = stat\2
					Next
				End If
			Next
    End If
End Sub
 

'*******************************************
'  Flipper Bat colors and styles
'*******************************************

Sub ResetFlippers()
	InitFlippers
End Sub
Sub InitFlippers()
	' initialize flipper colors
	pLeftFlipperBat.Visible 	= (FlipperColorMod > 1)
	pRightFlipperBat.Visible 	= pLeftFlipperBat.Visible
	pLeftFlipperBally.Visible 	= (FlipperColorMod = 1)
	pRightFlipperBally.Visible 	= pLeftFlipperBally.Visible
	LeftFlipper.Visible			= (Not pLeftFlipperBat.Visible And Not pLeftFlipperBally.Visible)
	RightFlipper.Visible		= LeftFlipper.Visible
	Select Case FlipperColorMod
	Case 2
		pLeftFlipperBat.Image  		= "flipperbatyellow"
		pLeftFlipperBat.Material  	= "flipperbatyellow"
		pRightFlipperBat.Image 		= "flipperbatyellow"
		pRightFlipperBat.Material 	= "flipperbatyellow"
	Case 3
		pLeftFlipperBat.Image  		= "flipperbatred"
		pLeftFlipperBat.Material  	= "flipperbatred"
		pRightFlipperBat.Image 		= "flipperbatred"
		pRightFlipperBat.Material 	= "flipperbatred"
	Case 4
		pLeftFlipperBat.Image  		= "flipperbatblue"
		pLeftFlipperBat.Material 	= "flipperbatblue"
		pRightFlipperBat.Image 		= "flipperbatblue"
		pRightFlipperBat.Material	= "flipperbatblue"
	End Select
End Sub


'*******************************************
'  Other Solenoids
'*******************************************

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub




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


' ***														***

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(4), objrtx2(4)
dim objBallShadow(4)
Dim OnPF(4)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3)
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
'	Dim gBOT: gBOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

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
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
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
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************



'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity



'*******************************************
'  Late 80's early 90's

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -5
	AddPt "Polarity", 2, 0.4, -5
	AddPt "Polarity", 3, 0.6, -4.5
	AddPt "Polarity", 4, 0.65, -4.0
	AddPt "Polarity", 5, 0.7, -3.5
	AddPt "Polarity", 6, 0.75, -3.0
	AddPt "Polarity", 7, 0.8, -2.5
	AddPt "Polarity", 8, 0.85, -2.0
	AddPt "Polarity", 9, 0.9,-1.5
	AddPt "Polarity", 10, 0.95, -1.0
	AddPt "Polarity", 11, 1, -0.5
	AddPt "Polarity", 12, 1.1, 0
	AddPt "Polarity", 13, 1.3, 0

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
	Dim b', BOT
'	BOT = GetBalls

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
Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

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
		Dim b', BOT
'		BOT = GetBalls

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
'  SUPPORTING FUNCTIONS 
'******************************************************

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

Function InCircle(pX, pY, centerX, centerY, radius)
	Dim route
	route = Sqr((pX - centerX) ^ 2 + (pY - centerY) ^ 2)
	InCircle = (route < radius)
End Function

Function Minimum(val1, val2)
	Minimum = IIF(val1<val2,val2,val1)
End Function

Function IIF(bool, obj1, obj2)
	If bool Then
		IIF = obj1
	Else
		IIF = obj2
	End If
End Function




'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************
'

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
	Dim b', BOT
'	BOT = GetBalls

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
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
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

Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
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




'******************************************************
'*****   FLUPPER DOMES 
'******************************************************
' Based on FlupperDomes2.2


Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = MousinAround		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.3		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.2		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white", "orange" and "yellow"

InitFlasherDome 1, "Orange"


Sub InitFlasherDome(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
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

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 


'******************************************************
'******  END FLUPPER DOMES
'******************************************************


'******************************************************
'******  FLUPPER BUMPERS
'******************************************************
' Based on FlupperBumpers 0.145 final

' In a GI subroutine you can then call set the bumperlight intensity with the "FlBumperFadeTarget(nr) = value" command
' where nr is the number of the bumper, value is between 0 (off) and 1 (full on) (so you can also use 0.3 0.4 etc).

' Notes:
' - There is only one color for the disk; you can photoshop it to a different color
' - The bumpertops are angle independent up to a degree; my estimate is -45 to + 45 degrees horizontally, 0 (topview) to 70-80 degrees (frontview)
' - I built in correction for the day-night slider; this might not work perfectly, depending on your table lighting
' - These elements, textures and materials do NOT integrate with any of the lighting routines I have seen in use in many VPX tables
'   (just find the GI handling routine and insert the FlBumperFadeTarget statement)
' - If you want to use VPX native bumperdisks just copy my bumperdisk but make it invisible



' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(3), FlBumperFadeTarget(3), FlBumperColor(3), FlBumperTop(3), FlBumperSmallLight(3), Flbumperbiglight(3)
Dim FlBumperDisk(3), FlBumperBase(3), FlBumperBulb(3), FlBumperscrews(3), FlBumperActive(3), FlBumperHighlight(3)
Dim cnt : For cnt = 1 to 3 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

Dim ind : For ind = 1 to 3 : FlInitBumper ind, "orange" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
'			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			Flbumperbiglight(nr).intensity = 5 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z*50, 0)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 3
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub


'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************



'********************************************
' Hybrid code for VR, Cab, and Desktop
'********************************************

Dim VRThings

if VR_Room = 0 and cab_mode = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 1:Next
Elseif VR_Room = 0 and cab_mode = 1 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = WallClock:Next
	for each VRThings in DTRails:VRThings.visible = 0:Next
'Custom Walls, Floor, and Roof
	if CustomWalls = 1 Then
		VR_Wall_Left.image = "VR_Wall_Left"
		VR_Wall_Right.image = "VR_Wall_Right"
		VR_Floor.image = "VR_Floor"
		VR_Roof.image = "VR_Roof"
	end if

	If topper = 1 Then
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


'********************************************
' VR Clock 
'********************************************

Dim CurrentMinute ' for VR clock

Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
	Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub


'*******************************************
'  Ball brightness code
'*******************************************

if BallLightness = 0 Then
	MousinAround.BallImage="ball-dark"
	MousinAround.BallFrontDecal="JPBall-Scratches"
elseif BallLightness = 1 Then
	MousinAround.BallImage="ball-HDR"
	MousinAround.BallFrontDecal="Scratches"
elseif BallLightness = 2 Then
	MousinAround.BallImage="ball-light-hf"
	MousinAround.BallFrontDecal="g5kscratchedmorelight"
else
	MousinAround.BallImage="ball-lighter-hf"
	MousinAround.BallFrontDecal="g5kscratchedmorelight"
End if


'*******************************************
'  Custom Side Blades
'*******************************************

If sideblades = False Then 'Use blade sideblades
	PinCab_Blades.image = "Black" 
'	PinCab_Blades.image = "PinCab_Blades_Black" 
End If


'**********************************************
'*******	Set Up Backglass Flashers	*******
'**********************************************
' this is for lining up the backglass flashers on top of a backglass image

Sub SetBackglass()
	Dim obj

	For Each obj In BackglassLow
		obj.x = obj.x
		obj.height = - obj.y + 0
		obj.y = 94 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassMid
		obj.x = obj.x
		obj.height = - obj.y + 0
		obj.y = 80 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassHigh
		obj.x = obj.x
		obj.height = - obj.y + 0
		obj.y = 70 'adjusts the distance from the backglass towards the user
	Next

	For Each obj In BackglassVRDisplay
		obj.x = obj.x
		obj.height = - obj.y + 0
		obj.y = 102 'adjusts the distance from the backglass towards the user
	Next

End Sub

'**********************************************


' ***************************************************************************
'                                  LAMP CALLBACK
' ****************************************************************************

if VR_Room = 1 Then
	Set LampCallback = GetRef("UpdateMultipleLamps")
Elseif VR_Room = 0 AND cab_mode = 0 Then
	Set LampCallback = GetRef("UpdateMultipleLampsDT")
End If

Sub UpdateMultipleLamps()
	If Controller.Lamp(57) = 0 Then: BGFl57.visible  =0: else: BGFl57.visible  =1
	If Controller.Lamp(58) = 0 Then: BGFl58.visible  =0: else: BGFl58.visible  =1
	If Controller.Lamp(59) = 0 Then: BGFl59.visible  =0: else: BGFl59.visible  =1
	If Controller.Lamp(60) = 0 Then: BGFl60.visible  =0: else: BGFl60.visible  =1
	If Controller.Lamp(61) = 0 Then: BGFl61.visible  =0: else: BGFl61.visible  =1
	If Controller.Lamp(62) = 0 Then: BGFl62.visible  =0: else: BGFl62.visible  =1
	If Controller.Lamp(63) = 0 Then: BGFl63.visible  =0: else: BGFl63.visible  =1
	If Controller.Lamp(8)  = 0 Then: BGFl8.visible   =0: else: BGFl8.visible   =1
End Sub

Sub UpdateMultipleLampsDT()
	If Controller.Lamp(57) = 0 Then: r57.setValue(0):Else: r57.setValue(1)
	If Controller.Lamp(58) = 0 Then: r58.setValue(0):Else: r58.setValue(1)
	If Controller.Lamp(59) = 0 Then: r59.setValue(0):Else: r59.setValue(1)
	If Controller.Lamp(60) = 0 Then: r60.setValue(0):Else: r60.setValue(1)
	If Controller.Lamp(61) = 0 Then: r61.setValue(0):Else: r61.setValue(1)
	If Controller.Lamp(62) = 0 Then: r62.setValue(0):Else: r62.setValue(1)
	If Controller.Lamp(63) = 0 Then: r63.setValue(0):Else: r63.setValue(1)
End Sub

'******************************************************
'****  LAMPZ by nFozzy
'******************************************************



Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments

Sub LampTimer()
	dim x, chglamp, nr, obj
	chglamp = Controller.ChangedLamps
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

const insert_dl_on_white = 200
const insert_dl_on_red = 60
const insert_dl_on_red_mini = 120
const insert_dl_on_blue = 200
const insert_dl_on_green_tri = 200
const insert_dl_on_green = 120
const insert_dl_on_orange = 200
const insert_dl_on_yellow = 100
const insert_dl_on_yellow_tri = 180
const insert_dl_on_whiteBB = 120
const insert_dl_on_orangeBB = 250
const insert_dl_on_yellowBB = 150


Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (max level / full MS fading time). The Modulate property must be set to 1 / max level if lamp is modulated.
	dim x : for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/120 : Lampz.Modulate(x) = 1 : next

' GI Fading
	Lampz.FadeSpeedUp(104) = 1/4 : Lampz.FadeSpeedDown(104) = 1/16 : Lampz.Modulate(104) = 1
	Lampz.FadeSpeedUp(111) = 1/4 : Lampz.FadeSpeedDown(111) = 1/16 : Lampz.Modulate(111) = 1

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(1)= l1
	Lampz.MassAssign(1)= l1a
	Lampz.Callback(1) = "DisableLighting p1, insert_dl_on_red,"
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= l2a
	Lampz.Callback(2) = "DisableLighting p2, insert_dl_on_orange,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= l3a
	Lampz.Callback(3) = "DisableLighting p3, insert_dl_on_green,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(4)= l4a
	Lampz.Callback(4) = "DisableLighting p4, insert_dl_on_green,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= l5a
	Lampz.Callback(5) = "DisableLighting p5, insert_dl_on_green,"
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(6)= l6a
	Lampz.Callback(6) = "DisableLighting p6, insert_dl_on_green,"
	Lampz.MassAssign(7)= l7
	Lampz.MassAssign(7)= l7a
	Lampz.Callback(7) = "DisableLighting p7, insert_dl_on_yellow,"
	Lampz.MassAssign(9)= l9
	Lampz.MassAssign(9)= l9a
	Lampz.Callback(9) = "DisableLighting p9, insert_dl_on_yellow,"
	Lampz.MassAssign(10)= l10
	Lampz.MassAssign(10)= l10a
	Lampz.Callback(10) = "DisableLighting p10, insert_dl_on_yellow,"
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= l11a
	Lampz.Callback(11) = "DisableLighting p11, insert_dl_on_yellow,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= l12a
	Lampz.Callback(12) = "DisableLighting p12, insert_dl_on_yellow,"
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(13)= l13a
	Lampz.Callback(13) = "DisableLighting p13, insert_dl_on_yellow,"
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= l14a
	Lampz.Callback(14) = "DisableLighting p14, insert_dl_on_yellow,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15a
	Lampz.Callback(15) = "DisableLighting p15, insert_dl_on_blue,"
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= l16a
	Lampz.Callback(16) = "DisableLighting p16, insert_dl_on_red,"
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(17)= l17a
	Lampz.Callback(17) = "DisableLighting p17, insert_dl_on_white,"
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= l18a
	Lampz.Callback(18) = "DisableLighting p18, insert_dl_on_white,"
	Lampz.MassAssign(19)= l19
	Lampz.MassAssign(19)= l19a
	Lampz.Callback(19) = "DisableLighting p19, insert_dl_on_white,"
	Lampz.MassAssign(20)= l20
	Lampz.MassAssign(20)= l20a
	Lampz.Callback(20) = "DisableLighting p20, insert_dl_on_white,"
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= l21a
	Lampz.Callback(21) = "DisableLighting p21, insert_dl_on_white,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= l22a
	Lampz.Callback(22) = "DisableLighting p22, insert_dl_on_white,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= l23a
	Lampz.Callback(23) = "DisableLighting p23, insert_dl_on_white,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= l24a
	Lampz.Callback(24) = "DisableLighting p24, insert_dl_on_white,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= l25a
	Lampz.Callback(25) = "DisableLighting p25, insert_dl_on_white,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26a
	Lampz.Callback(26) = "DisableLighting p26, insert_dl_on_white,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27a
	Lampz.Callback(27) = "DisableLighting p27, insert_dl_on_white,"
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= l28a
	Lampz.Callback(28) = "DisableLighting p28, insert_dl_on_white,"
	Lampz.MassAssign(29)= l29
	Lampz.MassAssign(29)= l29a
	Lampz.Callback(29) = "DisableLighting p29, insert_dl_on_red,"
	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(30)= l30a
	Lampz.Callback(30) = "DisableLighting p30, insert_dl_on_red,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= l31a
	Lampz.Callback(31) = "DisableLighting p31, insert_dl_on_red,"
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(32)= l32a
	Lampz.Callback(32) = "DisableLighting p32, insert_dl_on_white,"
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= l33a
	Lampz.Callback(33) = "DisableLighting p33, insert_dl_on_orange,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= l34a
	Lampz.Callback(34) = "DisableLighting p34, insert_dl_on_white,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= l35a
	Lampz.Callback(35) = "DisableLighting p35, insert_dl_on_yellow,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= l36a
	Lampz.Callback(36) = "DisableLighting p36, insert_dl_on_green_tri,"
	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= l37a
	Lampz.Callback(37) = "DisableLighting p37, insert_dl_on_red,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= l38a
	Lampz.Callback(38) = "DisableLighting p38, insert_dl_on_white,"
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= l39a
	Lampz.Callback(39) = "DisableLighting p39, insert_dl_on_white,"
	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(40)= l40a
	Lampz.Callback(40) = "DisableLighting p40, insert_dl_on_red,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= l41a
	Lampz.Callback(41) = "DisableLighting p41, insert_dl_on_white,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42a
	Lampz.Callback(42) = "DisableLighting p42, insert_dl_on_red,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= l43a
	Lampz.Callback(43) = "DisableLighting p43, insert_dl_on_yellow,"
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(44)= l44a
	Lampz.Callback(44) = "DisableLighting p44, insert_dl_on_white,"
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(45)= l45a
	Lampz.Callback(45) = "DisableLighting p45, insert_dl_on_red,"
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(46)= l46a
	Lampz.Callback(46) = "DisableLighting p46, insert_dl_on_yellow,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= l47a
	Lampz.Callback(47) = "DisableLighting p47, insert_dl_on_yellow,"
	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= l48a
	Lampz.Callback(48) = "DisableLighting p48, insert_dl_on_white,"

	Lampz.Callback(8) = "DisableLighting pBB_Jackpot, insert_dl_on_orangeBB,"
	Lampz.Callback(56) = "DisableLighting pBB_BuildJackpot, insert_dl_on_yellowBB,"
	Lampz.Callback(55) = "DisableLighting pBB_M, insert_dl_on_whiteBB,"
	Lampz.Callback(54) = "DisableLighting pBB_I, insert_dl_on_whiteBB,"
	Lampz.Callback(53) = "DisableLighting pBB_L, insert_dl_on_whiteBB,"
	Lampz.Callback(52) = "DisableLighting pBB_L2, insert_dl_on_whiteBB,"
	Lampz.Callback(51) = "DisableLighting pBB_I2, insert_dl_on_whiteBB,"
	Lampz.Callback(50) = "DisableLighting pBB_O, insert_dl_on_whiteBB,"
	Lampz.Callback(49) = "DisableLighting pBB_N, insert_dl_on_whiteBB,"

	Lampz.Callback(104) = "InsertIntensityUpdate"
	Lampz.Callback(111) = "GIUpdates"

if VR_Room = 0 AND cab_mode = 0 Then
	Lampz.MassAssign(8)= L_DTJackPot
End If

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


'******************
' GI fading
'******************

dim ballbrightness

'GI callback

Sub GIUpdates(ByVal aLvl)	'argument is unused
	dim x

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

' ball
	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)

	gilvl = alvl

End Sub



'******************************************************
'****  Ball GI Brightness and Inserts Level Code
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


Sub InsertIntensityUpdate(ByVal aLvl)
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim bulb
	for each bulb in InsertLightsInPrim
		bulb.intensity = bulb.uservalue*(1 + aLvl*1)
	next
End Sub




' - Herweh: Original Development by Herweh 2019 for Visual Pinball 10
' - Those that helped Herweh in original release:  
'   - Schreibi34: For awesome 3D objects and so many very helpful hints
'   - OldSkoolGamer, ICPjuggla, Herweh (:-): For creating a very cool VP9 build from where I borrowed a few table elements
'   - OldSkoolGamer: For providing me all the wonderful artwork he designed for the VP9 build
'   - Cosmic80: For his awesome color reworking of the playfield
'   - Flupper: For his ramp tutorial, for always having an open ear for Blender noobs like me and of course for resources like the bumper caps, the target prim and some more
'   - JPSalas: For giving me the decisive hint when the Jackpot in the mouse hole didn't work like it should
'   - 32assassin: For his desktop table scoring displays
'   - Sliderpoint: For the flasher dome from his 'Cyclone' build
'   - Ninuzzu: For his flipper shadows
'   - Dark: For the gates primitives template
'   - DJRobX, RothbauerW and maybe some other ones: For some code in my script like the SSF routines
'   - tttttwii, Bord and Thalamus: For beta testing and some good hints
'   - VPX development team: For always lifting VP to the next level
'
'******** Revisions done by Herweh version 1.1 *********
' v1.1	Some minor VP, graphical and coding bugfixes, like
'		Added a kickback post and animation
'		Moved the apron wires
'		Added two trigger prims
'		Drilled a hole into the left plastic
'		Moved the center ramp trigger a bit (thx Thalamus)
'		Added an experimental 5 ball multiball option 
'
'******** Revisions done by UnclePaulie on initial VR version 1.0 *********
' v1.0: Added minimal VR room.  
'		Sixtoe assisted in several areas including a couple z fighting areas, 
'		 Changed GIOverhead to a flasher from a light.  Looked wierd in VR, and wasn't angled correctly.  
'		 Color corrected side blades by adjusting static rendering and disable light below 1.
'		 Changed the right ramp ending.  It clipped the plastic drop hole of ramp.  Essentially resized ramp and removed drop hole... bandaid fix.  (UnclePaulie put permanent fix in later)

'******** Revisions done by UnclePaulie on Hybrid version 0.1 - 2.0 *********
' v0.1 	Added hybrid mode for VR room, cabinet, and desktop
'		Added other VR elements (power cord, several options: clock, posters, topper, side blades)
'		Added 3 ball options, dark, bright, and brightest
' 		Added DTRails collection for left and right rails in desktop mode
'		Added new wood floor for VR with a shadow around trim
'		Animated plunger, start button, and flippers in VR.  Had to assign individual materials to each.
'		Adjusted the digital tilt sensitivity to 6, and the nudge strength to 1.
'		Aligned the triggers per the real table, and adjusted the height.
' 		Readjusted the plunger trigger shape to see it animated.
'		Added groove to plunger lane, and small ball release ramp to get up.  
'		Moved plunger and mini plunger ball stop to the left slightly to align with table correctly
' v0.2	Readjusted flipper size down to be 147 VP units per VPW recommendations.  (was 151)
' v0.3	Edited playfield to remove two more trigger holes, and target hole.  Also added cutout prims.
' v0.4	Added Updated/more recent Rothbauerw/nFozzy physics and Flipper physics and basic Fleep Sounds
' 		Added dynamic shadows solution to the solution created by iaakki, apophis, Wylte
'		Removed rollingtimer, removed debug mode code and ballcontroltimer
'		Flipper shadows and options moved to FlipperVisualUpdate timer.  Removed graphicstimer.
'		Added knocker solenoid and position for sound
'		Corrected tnob to 3, and right sized the number of ball shadows
'		VPW ball rolling sounds implemented
' v0.5	Completed the collections for Fleep sounds and table physics
'		Added cutout prims for mousetraps
'		Positioned the sling and wall primatives correctly.  Adjusted sling rubbers, and moved those and all rubbers up by 5 VP units.
' 		Added updated ramprolling sounds solution created by nFozzy and VPW
' v0.6	Removed some older options now that physics updated.
'		Updated playfield physics, bumpers, slings, ramps per VPW recommendations.
'		With updated physics, I put the right/centerramp6 and middleramp stopper back the way it was, however changed the height to match the left one. Also made rampstoppers collidable.
' v0.7	Removed ramprubber, and adjusted ramp3 and 4 so ball doesn't catch
'		Changed mousehole.kick to 2 (was 0.1), new physics didn't allow it to go out.
'		Adjusted right ramp physics to get up to mousehole.  (Removed friction on portions of ramp.)
'		Removed MouseHoleSlowDown
'		Moved the light under left ramp to match location on playfield
'		Noticed that something caused the traps to drop below playfield.  Changed the transZ to 0; and initialized to 0
'		Added a peg under the left ramp 
'		Modified the b2s call for special LEDs in multimousehole mode to ensure in cab_mode
' v0.8	Updated some other sounds, and got permission from Herweh to remove 5 ball multiball.
'		Removed cheeseloopsound sub and timer, and changed metal ramp entry to metal sounds and metal physics.
'		Moved motorbank / entry walls in to align with primitive.
'		Added GI and backglass relay sounds, and trap sounds 
'		Implemented an alternate way to achieve VPW standup target physics while maintaining old prims and animation.
'		Increased standup target animation slightly.  Looks much better in VR.
' V0.9	Added Flupper dome.  Also added targetbouncer in
' v0.10 Cleaned some excess code out
' v0.11 Added Flupper Bumpers, and updated the backglass for VR.  Also removed ball search sub.
' v0.12 Removed Herweh's 5 ball Option
' v0.13	Updated VR cabinet graphics.  Aligned VR Displays.  Lots of code cleanup.
' 		Cut holes in the back wall for the mouse hole, and added VR blocking walls.
'		Adjusted the pov for desktop to increase the table slightly.
'		Turned the fMouseHole flasher to visible (it was previously used as Herweh's indicator of the 5 ball option.
' v0.14	Adjusted the intensities of the lights down slightly (WhiteI, YellowI, BlueI, and RedI).  Also the white and yellow plastic light intensity.
' 		Added option to turn magnasave option on and off 
'		Replaced rest of a_physics materials to new VPW physics materials
'		Adjusted the flupper dome light to stay in playfield.  Could see outside of cab in VR.
'		Modified the apron primitive image to include cutouts for the ball release and the left kicker/plunger
' v0.15 Changed the apron wall structure to create a trough
'		Completely changed the trough logic to handle balls on table
'		Removed startcontrol manual ball control trigger.  No longer needed.
' v0.16	No longer destroy balls.  Use a global gBOT throughout all of script.
'		Removed all the getballs calls and am only useing gBOT.  Will help eliminate stutter on slower CPU machines
'		Lowered intensity of mouseramp bonus when lit
'		Lowered the white, blue, and red flasher opacity by about 60%.
' v0.17 Adjusted the material on leftRamp1 to remove friction.  Too hard to get the ball up that ramp.
'		Adjusted the rightOuterRamp5 end and the ramp6 down by 1 VP unit. And increased the height of ramp5 by mousehole. The ball could stop on the end of the ramp.
'		Changed the return strength on the flippers to 0.07 from .066
'		The sleeve near the right side of the left ramp wasn't positioned correctly.  Moved slightly, as almost impossible to get up that ramp.
' v0.18 Adjusted the flipper start and end angle slightly, as well a couple physics parameters.
' v0.19	Adjusting the ramp caused the right trap to not come up.  Put the ramp back, but adjusted the friction down.
' v0.20	Sixtoe found some ramp and trap heights could cause a stuck ball on ramp5.  I raised ramp5 by 2 VP units to 62.  Also put the friction back.
'		Also raised right outer ramp6 by 2 VP untis.
' v0.21 Tomate added new plastic ramps and apron textures
' v0.22 Sixtoe recommended changing all the flashers on layer 11 to transmit at 0.1.  Changed flashers, bulbs, and plastics.
'		Adjusted the material and the disablelighting for the 3 ramp primitives on a GI intensity.  
' v0.23	Tomate updated the ramp textures slightly to look better in 4k
' v0.24 Adjusted the ramptrigger primitives to not collidable, as if the ball rolled slowly it could get stuck.  One more flipper physics adjustment.
' v0.25	Rothbauerw modified the trap move code slightly to avoid the ball jumping out of right trap onto ramp for some users.
' v0.26	Still tweaking flippers for better ball passes, and ability to get ball up ramps: changed the end angle to 72 and the power to 3100.
' 		Adjusted plunger lane wall and plunger to stop ball "vibrating" in plunger lane
'		Corrected corner condition to ensure motorbank goes up if a ball is trapped before motorbank done moving
' 		Corrected another corner condition in code if during multiball and more than one ball goes to the kickback pkickback didn't go back
' v0.27	Animated the pRamptriggers
'		Added one more ball for different brightness levels
'		Ensured that 4xAA, post proc AA, and in game AO are set to off.  Some users reported performance if they had on.
'		Adjusted the intensity of the plastics light slightly up, as well as the GI intensity.
' v0.28	Widened the bottom of the left ramp slightly.  Rampdecal5 was collidable, turned off.  Moved post to left of ramp slightly.
'		Slowed down the activeball only on orbit... a bit too fast.  Had to add code for several triggers
' v0.29	The Million lights did not stay lit after the 5th hit.  Redid code with LampCallback's.
' v0.30	Removed dead code for old mousehole flasher subs.  This also eliminated a couple of associated timers.
'		There was a weird spin of ball coming off of left ramp if it were a weak shot.  Added a rolloff wall to avoid, and adjusted physics on sleeve in front.
' v0.31	Put SlowDownBall sub back in, and controlled the ball speed on center ramp and orbit as Herweh had, but adjusted centerramp differently. Facotred orbit variable in.
' v0.32	Rothbauerw rewrote the motorbank code to control the motorbank going up and down.  Also corrected the corner condition.
'		Able to remove the MotorBankSwitch timer as well.
'		Added the diverter noise back in.
' v0.33	Rothbauerw suggested I try to further avoid the issue on the right trap with the ball popping up the ramp.  
'		 To do this I copied the L1, R1, R3, and R4 plastics, made one set only visible, and one set only collidable.
' v2.0	Released.  
' 			Thank you to Rothbauerw, PinStratsDan, Bord, Tomate, Sixtoe, Thalamus, apophis, smaug, and Benji for improvements, testing, and feedback!
' v2.01	Added Sling Corrections
'		Updated to latest VPW code for dynamic shadows, flipper physics, and other physics code.
'		Updated to most recent Fleep sounds and code.
'		Added Lampz routines
' v2.02	Added 3D prims, and adjusted insert lighting.
'		Automated desktop, cab, and VR mode.
'		Adjusted the dynamic shadows... were too many before.
' v2.03	Changed the playfield backwall inserts to 3D inserts.  
'		Ajusted the flupper bumper biglight intensity down by 75%.  Was too much on playfield.
'		Changed the desktop background to something a little less busy, and some GI.
'		Added GI levels to the ball and mousehole light.  Added code to increase insert intensities slightly when GI off.
'		Changed anti biff snubber rails to a primitive.
'		Added apron wall physics.
' 		Adjust how far slings come out.
'		Adjusted the day/night slider down a little.
'		Adjusted the cab pov.
'		Adjusted the playfield flasher lights down significantly.  
'		Removed GILightR11... as not needed, and was causing too much brightness on inserts.
' v2.1	Updated VR Backglass with flashers
'		Updated the elasticity falloff and strength of the flippers.  And increased the friction of the ramp entrances.  The friction change caused the ball to slow down when rolling back on playfield.
'		Changed VR cab material.
'		Updated the flipper triggers
'		Changed SSF for the pKickback
' 		Put new ball images in
'		Released