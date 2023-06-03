'==================================================================================================================================================='
'																								                                                    ' 			  	 
'        	                 		     APOLLO 13	     			                			                                                    '
'          	              		    Sega Pinball (1995)             		                   	                                                    '
'		  				  http://www.ipdb.org/machine.cgi?id=3592								                                                    '
'																								                                                    '
' 	  		 	 	  		  Originally Created by ICPjuggla and Herweh	vp9								                                        '
'										 and later Balater vpx, and later UnclePaulie										                        '
'								Contributions	Thalamus, JP's, DJRobX, Rothbauerw, Amgrim, 32assassin,ClarKent, Chucky87, agentEighty6     		'
'	Version 2.0 by UnclePaulie 2021-2023																											'
'	Table includes 	Hybrid VR/desktop/cabinet modes, VPW physics, Fleep sounds, Lampz, 3D inserts, new GI, sling corrections						'
'					targets, saucers, playfield mesh, etc.  																						'
'	Complete implementation details found at the bottom of the script.																				'
'  	2.0 Contributors:	retro27 - VR graphics updates, oqqsan - Flupper domes, moon corrections, Tomate - ramps, primitives, ' Skitso - GI			'
' 						Sixtoe - some lighting and graphics tweaks, apophis - Lampz, 3D inserts, shadow updates, 									'
'						Sheltemke - updated playfield / text overlay, Wylte - dynamic shadows														'
'	2.1 UnclePaulie		Updated scripts, GI, bugs, slingshot corrections, standup targets, changed to gBOT - no destroying balls, playfield mesh	'
'						Primitive lighting, Insert lighting and glow, physics walls to avoid ball bouncing onto apron and plastics.					'
'						Also physics updates, flipper updates, new bumpers, and lots of other updates.
'==================================================================================================================================================='

Option Explicit
Randomize


'*******************************************
' Desktop, Cab, and VR OPTIONS
'*******************************************

' Desktop, Cab, and VR Room are automatically selected.  However if in VR Room mode, you can change the environment with the magna save buttons.

const BallLightness = 2 ' 0 = dark, 1 = not as dark, 2 = bright, 3 = medium, 4 = brightest (2 is default)
const GreenDMD = 1		' 1 Shows a green DMD for Virtual DMD (desktop) and VR users.  Only used with LUT 14 
const bkgg=1			' 1 GI Animation on Backglass

Dim LUTset, DisableLUTSelector, LutToggleSound, LutToggleSoundLevel, bLutActive

LutToggleSound = True
LutToggleSoundLevel = .1
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

' *** If using VR Room:

const WallClock = 1	  	'1 Shows the clock in the VR minimal rooms only
const CustomWalls = 0 	'0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const topper = 0		'0 = Off 1= On - Topper Picture visible in VR Room only  (NOTE:  this is only a simple picture, not the Fan: and disabled if VRFanON is selected)
const poster = 1		'1 Shows the flyer posters in the VR room only 
const poster2 = 1		'1 Shows the flyer posters in the VR room only 


' ****************************************************

'----- Shadow Options -----
Const DynamicBallShadowsOn 	= 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn 	= 1		'0 = Static shadow under ball ("flasher" image, like JP's)
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
Const VolumeDial 			= 0.8	' Recommended values should be no greater than 1.
Const BallRollVolume 		= 0.5 	'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume 		= 0.5 	'Level of ramp rolling volume. Value between 0 and 1

'----- Phsyics Mods -----
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled 	= 1		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor 	= 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled


' ***********************************************************************************************
' OPTIONS END
' ***********************************************************************************************


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const UseVPMModSol = 1

'*******************************************
' Constants and Global Variables
'*******************************************

Dim UseVPMDMD, UseVPMColoredDMD, varhidden, VR_Room, cab_mode, DesktopMode: DesktopMode = Table1.ShowDT
Dim  bgsko: bgsko=0

If RenderingMode = 2 Then VR_Room=1 Else VR_Room=0      'VRRoom set based on RenderingMode in version 10.72
If Not DesktopMode and VR_Room=0 Then cab_mode=1 Else cab_mode=0

Dim I, x, obj, drp, mmoon, lfd, rfd
DIM InitDMD_Color, InitDMD_Opacity

InitDMD_Color = DMD.color
InitDMD_Opacity = DMD.opacity

IF VR_Room = 0 Then
	If Table1.ShowDT = True then
		UseVPMColoredDMD = True
		varhidden=1
	Else
		UseVPMColoredDMD = False
		varhidden=0
	End If
Else
	UseVPMColoredDMD = False
	UseVPMDMD = True
End If


Const UsingROM = True				'The UsingROM flag is to indicate code that requires ROM usage. Mostly for instructional purposes only.

Const BallSize = 50
Const BallMass = 1
Const tnob = 13						'Total number of balls
Const lob = 0						'Locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim BIPL : BIPL = False				'Ball in plunger lane

Const cGameName="apollo13",UseLamps=0,UseGI=0 ',UseSolenoids=2 already in vbs file


LoadVPM  "01560000", "SEGA2.VBS", 3.56

' ****************************************************

' LUT Loading

LoadLUT
'LUTset = 16			' Override saved LUT for debug
SetLUT
ShowLUT_Init



'//////////////---- LUT (Colour Look Up Table) ----//////////////
'0 = Fleep Natural Dark 1
'1 = Fleep Natural Dark 2
'2 = Fleep Warm Dark
'3 = Fleep Warm Bright
'4 = Fleep Warm Vivid Soft
'5 = Fleep Warm Vivid Hard
'6 = Skitso Natural and Balanced
'7 = Skitso Natural High Contrast
'8 = 3rdaxis Referenced THX Standard
'9 = CalleV Punchy Brightness and Contrast
'10 = HauntFreaks Desaturated
'11 = Tomate Washed Out 
'12 = VPW Original 1 to 1
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book
'16 = Skitso New Warmer LUT, optimized for Apollo 13


'**********************************************************************************************************
'Solenoids
'**********************************************************************************************************

SolCallBack(1) 			= "SolBallRelease"
SolCallback(2) 			= "SolAPlunger"
SolCallback(3) 			= "SolRightEject"
SolCallback(4) 			= "SolTopEject"
SolCallback(5) 			= "SolRocketEject"   'Rocket Ball Eject
SolCallback(7) 			= "ShakeRocket"
SolCallback(8) 			= "SolKnocker"
SolCallback(14)         = "SolTroughSuperVUKOut"' VUK under apron controlling rocket ball from subway to TroughVUKOut
SolCallback(15)         = "SolLFlipperd" 'for DOF callout
SolCallback(16)         = "SolRFlipperd" 'for DOF callout
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(17) 		= "SolTroughLock"
SolCallback(18)			= "SolRampLift"
SolCallback(19) 		= "SolmoonLift"
SolCallback(20) 		= "SolRocketLift"  '  Rocket motor relay
SolCallback(21) 		= "SolTrapDoor"
SolCallback(22)			= "Sol8BLockPlunger"
SolCallback(23)			= "SolDrainPost"
SolCallback(34)			= "SolMoon"

' flasher
SolCallback(25)			= "SolFLamp1"
SolCallback(26)			= "SolFLamp2"
SolCallback(27)			= "SolFLamp3"
SolCallback(28)			= "SolFLamp4"
SolCallback(29)			= "SolFLamp5"
SolCallback(30)			= "SolFLamp6"
SolCallback(31)			= "SolFLamp7"
SolCallback(32)			= "SolFLamp8"

'flipper DOF callouts
Sub sollFlipperd(Enabled)
     lfd=Enabled
 End Sub
Sub solrFlipperd(Enabled)
     rfd=Enabled
End Sub

'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	Cor.Update						'update ball tracking
	RollingUpdate					'update rolling sounds
	DoSTAnim						'handle stand up target animations
	SpinnerTimer					'handle the spinner animation
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	UpdateBallBrightness			'GI for the ball 
	LampTimer						'used for Lampz.  No need for separate timer.
End Sub

' This subroutine updates the flipper shadows and visual primitives
sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
End Sub


'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim gBOT, A13Ball1, A13Ball2, A13Ball3, A13Ball4, A13Ball5, A13Ball6, A13Ball7, A13Ball8, A13Ball9, A13Ball10, A13Ball11, A13Ball12, A13Ball13

Sub Table1_Init
	On Error Resume Next
	With Controller
		.GameName=cGameName
		.SplashInfoLine = "Sega Apollo 13"&chr(13)&"by UnclePaulie"
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.Hidden=varhidden
		.Games(cGameName).Settings.Value("rol")	= 0
		.Games(cGameName).Settings.Value("sound") = 1 ' Set sound (0=OFF, 1=ON)	
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0

	If b2son Then bgsko = bkgg	

'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
	Controller.SolMask(0)=0
    vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 

	If Err Then MsgBox Err.Description
	On Error Goto 0

	' basic pinmame timer
	PinMAMETimer.Interval	= PinMAMEInterval
	PinMAMETimer.Enabled	= True

	' nudging
	vpmNudge.TiltSwitch		= 1
	vpmNudge.Sensitivity	= 3
	vpmNudge.TiltObj 		= Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
 

' upper trough kickers enabled 
	sw24b.enabled=1
	sw23b.enabled=1
	sw22b.enabled=1
	sw21b.enabled=1
	sw20b.enabled=1
	sw19b.enabled=1
	sw18b.enabled=1
	sw17b.enabled=1

' preload 5 balls in trough
	Set A13Ball1 = drain.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball2 = sw11.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball3 = sw12.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball4 = sw13.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball5 = sw14.CreateSizedballWithMass(BallSize/2,Ballmass)
' preload 8 balls in upper trough
	Set A13Ball6 = sw24b.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball7 = sw23b.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball8 = sw22b.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball9 = sw21b.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball10 = sw20b.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball11 = sw19b.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball12 = sw18b.CreateSizedballWithMass(BallSize/2,Ballmass)
	Set A13Ball13 = sw17b.CreateSizedballWithMass(BallSize/2,Ballmass)

	gBOT = Array(A13Ball1, A13Ball2, A13Ball3, A13Ball4, A13Ball5, A13Ball6, A13Ball7, A13Ball8, A13Ball9, A13Ball10, A13Ball11, A13Ball12, A13Ball13)

	Controller.Switch(10) = 1
	Controller.Switch(11) = 1
	Controller.Switch(12) = 1
	Controller.Switch(13) = 1
	Controller.Switch(14) = 1
	Controller.Switch(15) = 0
	Controller.Switch(16) = 0

' Preload 8 Balls on upper trough
	Controller.Switch(17) = 1
	Controller.Switch(18) = 1
	Controller.Switch(19) = 1
	Controller.Switch(20) = 1
	Controller.Switch(21) = 1
	Controller.Switch(22) = 1
	Controller.Switch(23) = 1
	Controller.Switch(24) = 1

     ' magnet
	Set mmoon = New cvpmMagnet
		With mmoon
			.InitMagnet mMoonLock, 90
			.GrabCenter = True
			.MagnetOn = 0
			.CreateEvents "mmoon"
		End With

		wall173.collidable=1
		moonballstop.collidable=0
		moonballstopb.collidable=0
		mmoon.MagnetOn = 0
		moonlock.Enabled=0
		Ramp139.collidable=1
		Ramp030.collidable=0
		MultiBallPost.IsDropped = 0

	Wallpost001.collidable=0
	InitMoon
	InitRocket

	SetBackglass_Low
	SetBackglass_Mid
	SetBackglass_High
	SetBackglass_BG

' initialize the attirbutes of the bumpers
	initbumpers


End Sub


Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_Exit()
	SaveLUT
	Controller.Stop
End Sub


'**********************************************************************************************************
' Keys and Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal keycode)

	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter

	if keycode=StartGameKey then soundStartButton()

	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If keycode = PlungerKey Or keycode = LockBarKey Then Controller.Switch(9) = True

	If keycode = LeftMagnaSave Then
		bLutActive = True
	end if

	If keycode = RightMagnaSave Then
		If bLutActive Then 
			if DisableLUTSelector = 0 then
				If LutToggleSound Then
					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
				End If
				LUTSet = LUTSet  + 1
				if LutSet > 16 then LUTSet = 0
				SetLUT
				ShowLUT
			end if
		end If
	end if

' VR Animation of flipper buttons and launcher

	If keycode = LeftFlipperKey Then 
		FlipperActivate LeftFlipper, LFPress
		Primary_flipper_button_left.X = 2112 + 8
	End If
	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		Primary_flipper_button_right.X = 2090.5 - 8
	End If

	If keycode = PlungerKey Or keycode = LockBarKey Then 
		Handle_Knob.RotZ = -90
	End If

	If KeyDownHandler(keycode) Then Exit Sub

End Sub

Sub Table1_KeyUp(ByVal keycode)

	If keycode = PlungerKey Or keycode = LockBarKey Then Controller.Switch(9) = False

    If keycode = LeftMagnaSave Then 
		bLutActive = False
	End If

' VR Animation of flipper buttons and launcher

	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		Primary_flipper_button_left.X = 2112
	End If
	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		Primary_flipper_button_right.X = 2090.5
	End If

	If keycode = PlungerKey Or keycode = LockBarKey Then 
		Handle_Knob.RotZ = 0
	End If

	If KeyUpHandler(keycode) Then Exit Sub

End Sub


'*******************************************
' Flippers 
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled)
	If Enabled Then

		if lfd or rfd Then PlaySound SoundFXDOF("",101,DOFOn,DOFFlippers)   ' for DOF callout

		LF.Fire  'leftflipper.rotatetoend
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else

		PlaySound SoundFXDOF("",101,DOFOff,DOFFlippers)   ' for DOF callout

		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then

		if lfd or rfd Then PlaySound SoundFXDOF("",102,DOFOn,DOFFlippers)   ' for DOF callout

		RF.Fire 'rightflipper.rotatetoend
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else

		PlaySound SoundFXDOF("",102,DOFOff,DOFFlippers)   ' for DOF callout

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
		Flasher001.visible = 1
		Flasher002.visible = 1
		Flasher003.visible = 1
		Flasher004.visible = 1

		if vr_room = 0 and cab_mode = 0 then
			L_DT_Apollo.state = 1
			L_DT_Apollo2.state = 1
		end if

		Sound_GI_Relay 1, Relay_GI

		SetLamp 110, 1 'Controls GI
		SetLamp 111, 1 'Prim and Ball GI Updates

	Else 
		For each xx in GI:xx.State = 0: Next
		gilvl=0
		Flasher001.visible = 0
		Flasher002.visible = 0
		Flasher003.visible = 0
		Flasher004.visible = 0

		if vr_room = 0 and cab_mode = 0 then
			L_DT_Apollo.state = 0
			L_DT_Apollo2.state = 0
		end if

		Sound_GI_Relay 0, Relay_GI

		SetLamp 110, 0 'Controls GI
		SetLamp 111, 0 'Prim and Ball GI Updates

	End If

	If bgsko Then
		If enabled Then
			Controller.b2ssetdata 79,1
		Else
			Controller.b2ssetdata 79,0
		End If
	End If

' *** Backglass flasher lamps for GI ***

	For each xx in VRBackglassGI:xx.visible = enabled: Next

End Sub



'********************************************
' Drain hole and Trough
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
		sw15.Kick 70, 25
	End If
End Sub

Sub SolTroughLock(Enabled)
	If Enabled Then
		sw14.Kick 60,12
		UpdateTrough
	End If
End Sub


'********************************************
' Saucers
'********************************************

' top saucer

Sub sw54k_Hit()
	Controller.Switch(54) 	= True
	SoundSaucerLock
End Sub
Sub sw54k_Unhit()
	Controller.Switch(54) 	= False
End Sub
 
Sub SolTopEject(Enabled)
    If Enabled Then
		sw54k.Kick 220,10+ Int(Rnd()*5)
		SoundSaucerKick 1, sw54k
	End If
End Sub

' right saucer

Sub sw47k_Hit()
	Controller.Switch(47) 	= True
	SoundSaucerLock
End Sub
Sub sw47k_Unhit()
	Controller.Switch(47) 	= False
End Sub
 
Sub SolRightEject(Enabled)
    If Enabled Then
		sw47k.Kick 200,10+ Int(Rnd()*5)
		SoundSaucerKick 1, sw47k
	End If
End Sub


'********************************************
' drain post
'********************************************

Sub SolDrainPost(Enabled)
	If enabled Then
		DrainPost.TransZ = 30
		wallpost.HeightTop = 31
		Wallpost001.collidable=1
		PlaySoundAtLevelStatic SoundFX("DropTarget_Up", DOFContactors), 1, DrainPost
	Else
		DrainPost.TransZ = 0
		wallpost.HeightTop = 1
		Wallpost001.collidable=0
		PlaySoundAtLevelStatic SoundFX("DropTarget_Down", DOFContactors), 1, DrainPost
	end If
End Sub


'********************************************
' super VUK and VUK trough logic
'********************************************

Sub SubwayBallReleaseSuperVUK_Hit()
	Controller.Switch(48) = 1
	Playsound SoundFX("Ball_Bounce_Playfield_Soft_1",DOFContactors)
	UpdateSubTrough
End Sub	

Sub SubwayBallReleaseSuperVUK_UnHit()
	Controller.Switch(48) = 0
	UpdateSubTrough
End Sub	

Sub SolTroughSuperVUKOut(Enabled)
	If Enabled Then
		SubwayBallReleaseSuperVUK.kick 120,25, 1.56
	End If
End Sub

Sub SubSlot_008_Hit : 	UpdateSubTrough : End Sub
Sub SubSlot_008_UnHit : UpdateSubTrough : End Sub
Sub SubSlot_007_Hit : 	UpdateSubTrough : End Sub
Sub SubSlot_007_UnHit : UpdateSubTrough : End Sub
Sub SubSlot_006_Hit : 	UpdateSubTrough : End Sub
Sub SubSlot_006_UnHit : UpdateSubTrough : End Sub
Sub SubSlot_005_Hit : 	UpdateSubTrough : End Sub
Sub SubSlot_005_UnHit : UpdateSubTrough : End Sub
Sub SubSlot_004_Hit : 	UpdateSubTrough : End Sub
Sub SubSlot_004_UnHit : UpdateSubTrough : End Sub
Sub SubSlot_003_Hit : 	UpdateSubTrough : End Sub
Sub SubSlot_003_UnHit : UpdateSubTrough : End Sub
Sub SubSlot_002_Hit : 	UpdateSubTrough : End Sub
Sub SubSlot_002_UnHit : UpdateSubTrough : End Sub
Sub SubSlot_001_Hit : 	UpdateSubTrough : End Sub
Sub SubSlot_001_UnHit : UpdateSubTrough : End Sub


Sub UpdateSubTrough
	UpdateSubTroughTimer.Interval = 50
	UpdateSubTroughTimer.Enabled = 1
End Sub

Sub UpdateSubTroughTimer_Timer
	If SubwayBallReleaseSuperVUK.BallCntOver = 0 Then SubSlot_008.kick 185, 3
	If SubSlot_008.BallCntOver = 0 Then SubSlot_007.kick 185, 3
	If SubSlot_007.BallCntOver = 0 Then SubSlot_006.kick 185, 3
	If SubSlot_006.BallCntOver = 0 Then SubSlot_005.kick 185, 3
	If SubSlot_005.BallCntOver = 0 Then SubSlot_004.kick 185, 3
	If SubSlot_004.BallCntOver = 0 Then SubSlot_003.kick 185, 3
	If SubSlot_003.BallCntOver = 0 Then SubSlot_002.kick 185, 3
	If SubSlot_002.BallCntOver = 0 Then SubSlot_001.kick 185, 3
	Me.Enabled = 0
End Sub


'********************************************
' trap door to 8-ball lock
'********************************************

dim TrapDoorDir, TrapDoorHeight
Sub SolTrapDoor(Enabled)
	if enabled Then
		Ramp139.collidable=0
		ramp030.collidable=1
		TrapDoorDir=1 'Go Up
		TrapDoorHeight= 155
 		TrapDoorTimer.Interval=5
		TrapDoorTimer.Enabled=True
	Else
		Ramp139.collidable=1
		TrapDoorDir=0 'Go Down
		TrapDoorHeight= 260
		TrapDoorTimer.Interval=5
		TrapDoorTimer.Enabled=True
		ramp030.collidable=0
	end If
End Sub

Sub TrapDoorTimer_Timer()
	if TrapDoorDir=1 then
		TrapDoorHeight=TrapDoorHeight+TrapDoorTimer.Interval
		If TrapDoorHeight >= 260 then TrapDoorTimer.enabled=False:PlaySound SoundFX("DropTarget_Down",DOFContactors)
		Ramp029.HeightBottom=TrapDoorHeight
		trapDoorPrim.RotZ=-45
	Else
		TrapDoorHeight=TrapDoorHeight-TrapDoorTimer.Interval
		If TrapDoorHeight <= 155 then TrapDoorTimer.enabled=False:PlaySound SoundFX("DropTarget_Down",DOFContactors)
		Ramp029.HeightBottom=TrapDoorHeight
		trapDoorPrim.RotZ=0
	end If
	Ramp029.HeightBottom=TrapDoorHeight
End Sub


sub Sol8BLockPlunger(Enabled)
		MultiBallPost.IsDropped = 1
		wall173.collidable= 0
		drp=1
		sw24b.Kick 180,1
		sw24b.enabled=false
		sw23b.Kick 180,1
		sw23b.enabled=false
		sw22b.Kick 180,1
		sw22b.enabled=false
		sw21b.Kick 180,1
		sw21b.enabled=false
		sw20b.Kick 180,1
		sw20b.enabled=false
		sw19b.Kick 180,1
		sw19b.enabled=false
		sw18b.Kick 180,1
		sw18b.enabled=false
		sw17b.Kick 180,1
		sw17b.enabled=false
		Controller.Switch(17) = 0
		Controller.Switch(18) = 0
		Controller.Switch(19) = 0
		Controller.Switch(20) = 0
		Controller.Switch(21) = 0
		Controller.Switch(22) = 0
		Controller.Switch(23) = 0
		Controller.Switch(24) = 0
End Sub

Sub sw16b_hit()
	If drp=1 Then
		drp=0
		MultiBallPost.IsDropped = 0
		wall173.collidable= 1
		sw24b.enabled=1
		sw23b.enabled=0
		sw22b.enabled=0
		sw21b.enabled=0
		sw20b.enabled=0
		sw19b.enabled=0
		sw18b.enabled=0
		sw17b.enabled=0
	End If
End Sub


'********************************************
'  Rollovers
'********************************************

Sub sw16_Hit
	Controller.Switch(16) = 1 
	BIPL = True
End Sub 

Sub sw16_UnHit
	Controller.Switch(16) = 0
	BIPL = False
End Sub

Sub sw17b_Hit()
	Controller.Switch(17) 	= True
End Sub
Sub sw17b_Unhit()
	Controller.Switch(17) 	= False
End Sub
Sub sw18b_Hit()
	Controller.Switch(18) 	= True
	sw17b.enabled=1
End Sub
Sub sw18b_Unhit()
	Controller.Switch(18) 	= False
	sw17b.enabled=0
End Sub
Sub sw19b_Hit()
	Controller.Switch(19) 	= True
	sw18b.enabled=1
End Sub
Sub sw19b_Unhit()
	Controller.Switch(19) 	= False
	sw18b.enabled=0
End Sub
Sub sw20b_Hit()
	Controller.Switch(20) 	= True
	sw19b.enabled=1
End Sub
Sub sw20b_Unhit()
	Controller.Switch(20) 	= False
	sw19b.enabled=0
End Sub
Sub sw21b_Hit()
	Controller.Switch(21) 	= True
	sw20b.enabled=1
End Sub
Sub sw21b_Unhit()
	Controller.Switch(21) 	= False
	sw20b.enabled=0
End Sub
Sub sw22b_Hit()
	Controller.Switch(22) 	= True
	sw21b.enabled=1
End Sub
Sub sw22_Unhit()
	Controller.Switch(22) 	= False
	sw21b.enabled=0
End Sub
Sub sw23b_Hit()
	Controller.Switch(23) 	= True
	sw22b.enabled=1
End Sub
Sub sw23b_Unhit()
	Controller.Switch(23) 	= False
	sw22b.enabled=0
End Sub
Sub sw24b_Hit()
	Controller.Switch(24) 	= True
	sw23b.enabled=1
End Sub
Sub sw24b_Unhit()
	Controller.Switch(24) 	= False
	sw23b.enabled=0
End Sub



'********************************************
'  Targets
'********************************************

' Round Targets 

Sub sw25_Hit
	STHit 25
End Sub

Sub sw25o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw26_Hit
	STHit 26
End Sub

Sub sw26o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw27_Hit
	STHit 27
End Sub

Sub sw27o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw28_Hit
	STHit 28
End Sub

Sub sw28o_Hit
	TargetBouncer Activeball, 1
End Sub


' Vertical Rectangle Targets

Sub sw29_Hit
	STHit 29
End Sub

Sub sw29o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw30_Hit
	STHit 30
End Sub

Sub sw30o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw31_Hit
	STHit 31
End Sub

Sub sw31o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw32_Hit
	STHit 32
End Sub

Sub sw32o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw38_Hit
	STHit 38
End Sub

Sub sw38o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw40_Hit
	STHit 40
End Sub

Sub sw40o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw49a_Hit
	STHit 491
End Sub

Sub sw49ao_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw49b_Hit
	STHit 492
End Sub

Sub sw49bo_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw49c_Hit
	STHit 493
End Sub

Sub sw49co_Hit
	TargetBouncer Activeball, 1
End Sub


'********************************************
' bumpers
'********************************************
Sub Bumper1_Hit()
	vpmTimer.PulseSw 35	
	RandomSoundBumperTop Bumper1
End Sub

Sub Bumper2_Hit()
	vpmTimer.PulseSw 33	
	RandomSoundBumperBottom Bumper2
End Sub

Sub Bumper3_Hit()
	vpmTimer.PulseSw 34	
	RandomSoundBumperMiddle Bumper3
End Sub


'********************************************
' ramp diverter
'********************************************

Dim diverterAdd

InitDiverter

Sub InitDiverter()
	Diverter2.collidable = 0
	Diverter1.collidable = 1
	diverterAdd			= 1
End Sub

Sub DiverterTriggerLeft_Hit()
	ActiveBall.VelY 			= 0
	Diverter1.collidable 		= 1
	Diverter2.collidable 		= 0
	diverterAdd					= -3
	RampDiverterTimer.Enabled 	= True
End Sub
Sub DiverterTriggerRight_Hit()
	ActiveBall.VelY 			= 0
	Diverter1.collidable 		= 0
	Diverter2.collidable 		= 1
	diverterAdd					= 3
	RampDiverterTimer.Enabled 	= True
End Sub

Sub RampDiverterTimer_Timer()
	RampDiverter.RotY = RampDiverter.RotY + diverterAdd
	If RampDiverter.RotY <= 0 Or RampDiverter.RotY => 30 Then RampDiverterTimer.Enabled = False
End Sub


'********************************************
' spinner
'********************************************

Sub sw41_Spin()
	vpmTimer.PulseSw 41
	SoundSpinner sw41
End Sub

'***********Rotate Spinner

Sub SpinnerTimer
	sw41Prim.Rotx = -sw41.CurrentAngle-90
End Sub


Sub sw55_Hit()
	vpmTimer.PulseSw 55
	GatesWire_hit(sw55)
End Sub


'********************************************
' ramp lift - Rewritten by UnclePaulie to get the back ramp lift to work correctly with orbit and moon gravity mission
'********************************************

' RampLiftVisual is the visibleramp, and it's not collidable. 
' RampLiftDown is a non visible ramp.  Bottom is at zero.  And set at not collidable
' RampLiftUP is the other non visible ramp.  Bottom is at 70.  And set at collidable.

dim RampLiftPos, RampLiftHeight

Sub SolRampLift(Enabled)
	if enabled then 
		RampLiftPos=1
		RampLiftHeight= 70
		RampLiftTimer.Interval=5
		RampLiftTimer.Enabled=1
		PlaySound SoundFX("DropTarget_Down",DOFContactors)
	Else
		RampLiftPos=2
		RampLiftHeight= 0
		RampLiftTimer.Interval=5
		RampLiftTimer.Enabled=1
		PlaySound SoundFX("DropTarget_Down",DOFContactors)
	end if
End Sub

Sub RampLiftTimer_Timer()
Select Case RampLiftPos
	Case 1:			'ramp is going down
		RampLiftHeight=RampLiftHeight-RampLiftTimer.Interval
		RampLiftDown.collidable = 1
		RampLiftUp.collidable = 0
		RampLiftVisual.HeightBottom=RampLiftHeight
		If RampLiftHeight <= 0 then RampLiftTimer.enabled=False
	Case 2:			'ramp is going up
		RampLiftHeight=RampLiftHeight+RampLiftTimer.Interval
		RampLiftDown.collidable = 0
		RampLiftUp.collidable = 1
		RampLiftVisual.HeightBottom=RampLiftHeight
		If RampLiftHeight >= 70 then RampLiftTimer.enabled=False
End Select
End Sub


'********************************************
' rocket  - Rewritten by UnclePaulie to get multiball functions to work with subway and timing
'********************************************

Sub sw43k_Hit()
	Controller.Switch(43) 	= True
End Sub

Sub sw43k_Unhit()
	Controller.Switch(43) 	= False
End Sub


Dim RocketDirection '0 = ready to go up or moving up, 1 = ready to go down or moving down

Sub InitRocket()
	If RocketDirection = 1 Then
		RocketTimer.Interval = 24
		RocketTimer.Enabled  = True	
		PlaySound SoundFX("Motor_long",DOFContactors)
	Else
		RocketDirection = 0
		Controller.Switch(50) = True
		Controller.Switch(51) = False
		RocketTrough.Enabled = 0
		RocketTroughHole.Enabled = 0
	End If
End Sub


Sub SolRocketEject(Enabled)
	If Enabled Then
		sw43k.kick 155+int(rnd(10)), 0.1
		SoundSaucerKick 0, sw43k
	End If
End Sub


Sub SolRocketLift(Enabled)
	If Enabled Then
		RocketTimer.Interval = 24
		RocketTimer.Enabled  = True	
		PlaySound SoundFX("Motor_long",DOFContactors)
	End If
End Sub

Sub RocketTimer_Timer()
	If RocketDirection = 0 Then
		If Rocket.RotX <= -18 Then
			PlaySound SoundFX("DropTarget_Down",DOFContactors)
			sw43k.kick 155+int(rnd(10)), 0.1
			SoundSaucerKick 0, sw43k
			RocketTimer.Enabled 	= False
			Controller.Switch(51) 	= True  ' it's up
			Controller.Switch(50) 	= False  ' it's not home
			RocketTrough.Enabled = True
			RocketTroughHole.Enabled = True ' open rocket hole when all the way up
			RocketDirection = 1
			Exit Sub
		Else
			Controller.Switch(51) 	= False  ' it's not up
			Controller.Switch(50) 	= False  ' it's not home
			RocketTimer.Enabled 	= True
			RocketTrough.Enabled = True
			RocketTroughHole.Enabled = True ' open rocket hole when all the way up
			Rocket.RotX = Rocket.RotX - 0.1
		End If

	Else
		If Rocket.RotX >=0 Then 
			PlaySound SoundFX("DropTarget_Down",DOFContactors)
			RocketTimer.Enabled 	= False
			Controller.Switch(51) 	= False  ' it's not up
			Controller.Switch(50) 	= True  ' it's home
			RocketTrough.Enabled = False
			RocketTroughHole.Enabled = False ' rocket hole closed
			RocketDirection = 0
			Rocket.RotX =0
			Exit Sub
		Elseif Rocket.RotX <-12 Then
			Controller.Switch(51) 	= False  ' it's not up
			Controller.Switch(50) 	= False  ' it's not home
			RocketTrough.Enabled = True
			RocketTroughHole.Enabled = True ' open rocket hole when all the way up
			Rocket.RotX = Rocket.RotX + 0.1
		Else
			Controller.Switch(51) 	= False  ' it's not up
			Controller.Switch(50) 	= False  ' it's not home
			RocketTrough.Enabled = False
			RocketTroughHole.Enabled = False ' open rocket hole when all the way up
			Rocket.RotX = Rocket.RotX + 0.1
		End If
	InitRocket
	End If
End Sub


'Sub ShakeRocket(direction)
Sub ShakeRocket (Enabled)
	If Not RocketTimer.Enabled Then
		RocketShakeTimer.Interval = 5+Int(Rnd(5))
		RocketShakeTimer.Enabled  = True
	End If
End Sub

Sub RocketShakeTimer_Timer()
	' nudging
	Select Case RocketShakeTimer.Interval
	Case 0
		Rocket.RotX = Rocket.RotY - 1
	Case 1, 2
		Rocket.RotX = Rocket.RotX + 1
	Case 3
		Rocket.RotX = Rocket.RotY - 1
	Case 5
		Rocket.RotY = Rocket.RotY - 1
	Case 6, 7
		Rocket.RotY = Rocket.RotY + 1
	Case 8
		Rocket.RotY = Rocket.RotY - 1
	Case 10
		Rocket.RotY = Rocket.RotY + 1
	Case 11, 12
		Rocket.RotY = Rocket.RotY - 1
	Case 13
		Rocket.RotY = Rocket.RotY + 1
	Case Else
		RocketShakeTimer.Enabled = False
	End Select
	RocketShakeTimer.Interval = RocketShakeTimer.Interval + 1
End Sub


'********************************************
' moon
'********************************************

Dim MoonBall, angle, radiansm, radius,angleb, baly, balyb, rada, radab

Sub InitMoon()
	' rotation stuff
	radiansm 				= 3.1415926 / 180
	angle 					= 0
	angleb					=180-Moon.objrotz
	radius 					= 126

	' moon is at home
	Controller.Switch(36) 	= True
	Controller.Switch(37) 	= False
End Sub


Sub SolMoon(Enabled)
	if enabled Then
		if Controller.Switch(36) Then
			mmoon.MagnetOn = 1
			MoonLock.Enabled = 1
		End If
	Else
		mmoon.MagnetOn = 0
		MoonLock.Enabled = 0
		moonballstop.collidable=0
		moonballstopb.collidable=0
		if Controller.Switch(36) Then
			If Controller.Switch(19) = 0 Then
				actvb=0
				set MoonBall = Nothing
			End If
		End If
	End If
End Sub

Dim actvb


Sub MoonLock_Hit()
	If Controller.Switch(36) Then
		moonballstop.collidable=1
		moonballstopb.collidable=1
		actvb=1
		PlaySound SoundFX("magnet_catch",DOFContactors)
		Set MoonBall 	= ActiveBall
		MoonBall.VelX	= 0
		MoonBall.VelY	= 0
		MoonBall.VelZ	= 0
	End If
		mmoon.MagnetOn = 0
		MoonLock.Enabled = 0	
End Sub


sub SolmoonLift(Enabled)
	if Enabled Then
	MoonTimer.Interval 	= 10 ' was 20    'oqqsan modified
	MoonTimer.Enabled  	= True
	SoundSaucerKick 1, Moon
	End If
End Sub

Sub MoonTimer_Timer()
	mmoon.MagnetOn = 0	
	moonballstop.collidable=0
	moonballstopb.collidable=0
	moonlock.enabled=0
	if controller.Solenoid(19) Then
		If Moon.RotX <= -180 Then
				Controller.Switch(37) 	= True
			If Controller.Solenoid(34)=0 Then
				set MoonBall = Nothing
				If actvb = 1 Then
					PlaySound SoundFX("Ball_Bounce_Playfield_Soft_1",DOFContactors)
					actvb=0
				End If
				Exit Sub
			End If
		Else
			Controller.Switch(36) = False
			Moon.RotX 	= Moon.RotX - .5 ' was 1  'oqqsan modified
		End If
	Else
		If Moon.RotX >= Moon.ObjRotX Then
				Controller.Switch(36) 	= True
			If Controller.Solenoid(34)=0 Then
				MoonTimer.Enabled 		= False
				set MoonBall = Nothing
				PlaySound SoundFX("Ball_Bounce_Playfield_Soft_1",DOFContactors)
				actvb=0
				Exit Sub
			End If
		Else
			Controller.Switch(37) = False
			Moon.RotX 	= Moon.RotX +1  '   was 2 -actvb	'oqqsan modified
		End If
	End If
			If actvb Then
				angle= abs(moon.rotX)
				rada=radiansm*angle
				radab=radiansm*angleb
				MoonBall.VelX	= 0
				MoonBall.VelY	= 0
				MoonBall.VelZ	= 0
				MoonBall.z= sin (rada)*radius+moon.z
				balyb=cos( rada)*radius
				MoonBall.y= balyb*cos(radab)+Moon.y
				MoonBall.x=Sin(radab)*balyb+Moon.x
			End If
End Sub



'*******************************************
'  Ramp Triggers
'*******************************************

Sub Trigger026_hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub TriggerRR4_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerLiftr_hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub TriggerOR1_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerEBR1_hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub TriggerEBR2_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerRREN1_hit()
	WireRampOn True ' Play Plastic Ramp Sound
End Sub

Sub TriggerRREN2_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerRRO_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerRRO1_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerRRER1_hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub TriggerRREL1_hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub TriggerOR3_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub TriggerRRE_hit()
	Playsound("Metal_Touch_1") ' Play a metal hit sound
End Sub


'*******************************************
' switches
'*******************************************

' top lanes
Sub sw52_Hit()
	controller.Switch(52)=1
End Sub
Sub sw52_Unhit()
	controller.Switch(52)=0
End Sub
Sub sw53_Hit()
	controller.Switch(53)=1
End Sub
Sub sw53_Unhit()
	controller.Switch(53)=0
End Sub

' orbit
Sub sw45_Hit()
	controller.Switch(45)=1
End Sub
Sub sw45_Unhit()
	controller.Switch(45)=0
End Sub
Sub sw46_Hit()
	controller.Switch(46)=1
End Sub
Sub sw46_Unhit()
	controller.Switch(46)=0
End Sub

' return lanes
Sub sw59_Hit()
	controller.Switch(59)=1
End Sub
Sub sw59_Unhit()
	controller.Switch(59)=0
End Sub
Sub sw60_Hit()
	controller.Switch(60)=1
End Sub
Sub sw60_Unhit()
	controller.Switch(60)=0
End Sub

' out lanes
Sub sw57_Hit()
	controller.Switch(57)=1
End Sub
Sub sw57_Unhit()
	controller.Switch(57)=0
End Sub
Sub sw58_Hit()
	controller.Switch(58)=1
End Sub
Sub sw58_Unhit()
	controller.Switch(58)=0
End Sub

' right ramp
Sub sw44_Hit()
	controller.Switch(44)=1
End Sub
Sub sw44_Unhit()
	controller.Switch(44)=0
End Sub

Sub sw56_Hit()
	controller.Switch(56)=1
End Sub
Sub sw56_Unhit()
	controller.Switch(56)=0
End Sub

'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolAPlunger(Enabled)
	If Enabled Then
		AutoPlunger.Kick 0, 35-int(Rnd(30))
		SoundSaucerKick 1, AutoPlunger
	end If
End Sub

Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub


'*******************************************
' Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'*******************************************
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
	vpmTimer.PulseSw 61
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
dim objrtx1(13), objrtx2(13)
dim objBallShadow(13)
Dim OnPF(13)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11,BallShadowA12)

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
			If gBOT(s).Z < 30 And gBOT(s).X < 850 And gBOT(s).Z > 20 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane; or in subway
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

Dim ST25, ST26, ST27, ST28, ST29, ST30, ST31, ST32, ST38, ST40, ST491, ST492, ST493	'Rectangle Stand Up Targets

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


Set ST25 = (new StandupTarget)(sw25, psw25,25, 0)
Set ST26 = (new StandupTarget)(sw26, psw26,26, 0)
Set ST27 = (new StandupTarget)(sw27, psw27,27, 0)
Set ST28 = (new StandupTarget)(sw28, psw28,28, 0)
Set ST29 = (new StandupTarget)(sw29, psw29,29, 0)
Set ST30 = (new StandupTarget)(sw30, psw30,30, 0)
Set ST31 = (new StandupTarget)(sw31, psw31,31, 0)
Set ST32 = (new StandupTarget)(sw32, psw32,32, 0)
Set ST38 = (new StandupTarget)(sw38, psw38,38, 0)
Set ST40 = (new StandupTarget)(sw40, psw40,40, 0)
Set ST491 = (new StandupTarget)(sw49a, psw49a,49, 0)
Set ST492 = (new StandupTarget)(sw49b, psw49b,49, 0)
Set ST493 = (new StandupTarget)(sw49c, psw49c,49, 0)

Dim STArray
STArray = Array(ST25, ST26, ST27, ST28, ST29, ST30, ST31, ST32, ST38, ST40, ST491, ST492, ST493)


'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit
Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

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
	If switch = 25 Then
		STArrayID = 0:Exit Function
	ElseIf switch = 26 Then
		STArrayID = 1:Exit Function
	ElseIf switch = 27 Then
		STArrayID = 2:Exit Function
	ElseIf switch = 28 Then
		STArrayID = 3:Exit Function
	ElseIf switch = 29 Then
		STArrayID = 4:Exit Function
	ElseIf switch = 30 Then
		STArrayID = 5:Exit Function
	ElseIf switch = 31 Then
		STArrayID = 6:Exit Function
	ElseIf switch = 32 Then
		STArrayID = 7:Exit Function
	ElseIf switch = 38 Then
		STArrayID = 8:Exit Function
	ElseIf switch = 40 Then
		STArrayID = 9:Exit Function
	ElseIf switch = 491 Then
		STArrayID = 10:Exit Function
	ElseIf switch = 492 Then
		STArrayID = 11:Exit Function
	ElseIf switch = 493 Then
		STArrayID = 12:Exit Function
	Else
		Exit Function
	End If
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

Function STAnimate(primary, prim, switch, animate)
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
dim RampBalls(14,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(14)	

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


'******************************************************
'*****   FLUPPER DOMES 
'******************************************************

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity ', FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.3		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.3		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.3		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
'FlasherOffBrightness = 	0.5	' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************
Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "red"
InitFlasher 2, "red"
InitFlasher 3, "white"
InitFlasher 4, "white"
InitFlasher 5, "red"
InitFlasher 6, "red"
InitFlasher 7, "red"



' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 


Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	If nr = 3 or nr = 4 then Set objbloom(nr) = Eval("Flasherbloom2") else Set objbloom(nr) = Eval("Flasherbloom1")
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
'	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	' set the texture and color of all objects
'	select case objbase(nr).image
'		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
'		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
'		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
'	end select

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

' adding some GI to the flasher bases
	Select case nr
		case 1,2: 
			if gilvl = 1 Then
				objbase(nr).BlendDisableLighting =  2 + 10 * ObjLevel(nr)^3	
			else
				objbase(nr).BlendDisableLighting =  0.5 + 10 * ObjLevel(nr)^3	
			end If
		case 3,4: 
			if gilvl = 1 Then
				objbase(nr).BlendDisableLighting =  2 + 10 * ObjLevel(nr)^3	
			else
				objbase(nr).BlendDisableLighting =  0.5 + 10 * ObjLevel(nr)^3	
			end If
		case 5,6,7: 
			if gilvl = 1 Then
				objbase(nr).BlendDisableLighting =  0.5 + 10 * ObjLevel(nr)^3	
			else
				objbase(nr).BlendDisableLighting =  0.3 + 10 * ObjLevel(nr)^3	
			end If
	end Select

	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub 


'**********************************************
'*******	Set Up Backglass Flashers	*******
'**********************************************
' this is for lining up the backglass flashers on top of a backglass image

Sub SetBackglass_Low()
	Dim obj

	For Each obj In Backglass_Low
		obj.x = obj.x
		obj.height = - obj.y + 350
		obj.y = -95 'adjusts the distance from the backglass towards the user
	Next
End Sub


Sub SetBackglass_Mid()
	Dim obj

	For Each obj In Backglass_Mid
		obj.x = obj.x
		obj.height = - obj.y + 320
		obj.y = -110 'adjusts the distance from the backglass towards the user
	Next
End Sub


Sub SetBackglass_High()
	Dim obj

	For Each obj In Backglass_High
		obj.x = obj.x
		obj.height = - obj.y + 290
		obj.y = -135 'adjusts the distance from the backglass towards the user
	Next
End Sub

Sub SetBackglass_BG()
	Dim obj

	For Each obj In Backglass_BG
		obj.x = obj.x
		obj.height = - obj.y + 320
		obj.y = -110 'adjusts the distance from the backglass towards the user
	Next
End Sub

'**********************************************


Sub SolFLamp1(Enabled)
	Lampz.state(101) = Enabled

' *** Backglass Solenoid Controlled Flashers ***
	If Enabled Then
		BGFlamp1.visible = 1
	Else
		BGFlamp1.visible = 0
	End If

End Sub

Sub SolFLamp2(Enabled)
	Lampz.state(102) = Enabled

' *** Backglass Solenoid Controlled Flashers ***
	If Enabled Then
		BGFlamp2.visible = 1
	Else
		BGFlamp2.visible = 0
	End If

End Sub

Sub SolFLamp3(Enabled)
	Lampz.state(103) = Enabled

' *** Backglass Solenoid Controlled Flashers ***
	If Enabled Then
		BGFlamp3.visible = 1
	Else
		BGFlamp3.visible = 0
	End If

End Sub

Sub SolFLamp4(Enabled)
	Lampz.state(104) = Enabled
	Objlevel(6) = 1 : FlasherFlash6_Timer

' *** Backglass Solenoid Controlled Flashers ***
	If Enabled Then
		BGFlamp4.visible = 1
	Else
		BGFlamp4.visible = 0
	End If

End Sub
Sub SolFLamp5(Enabled)
	If enabled Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
		Objlevel(2) = 1 : FlasherFlash2_Timer
		Objlevel(5) = 1 : FlasherFlash5_Timer
	End If
End Sub

Sub SolFLamp6(Enabled)
	If enabled Then
		Objlevel(3) = 1 : FlasherFlash3_Timer
		Objlevel(4) = 1 : FlasherFlash4_Timer
	End if
End Sub

Sub SolFLamp7(Enabled)
	Lampz.state(107) = Enabled
	Objlevel(7) = 1 : FlasherFlash7_Timer

' *** Backglass Solenoid Controlled Flashers ***
	If Enabled Then
		BGFlamp7.visible = 1
	Else
		BGFlamp7.visible = 0
	End If
End Sub

Sub SolFLamp8(Enabled)
	lf8a.state= Enabled
	lf8b.state= Enabled
	lf8c.state= Enabled

' *** Backglass Solenoid Controlled Flashers ***
	If Enabled Then
		BGFlamp8a.visible = 1
		BGFlamp8b.visible = 1
	Else
		BGFlamp8a.visible = 0
		BGFlamp8b.visible = 0
	End If

End Sub


'**********************************************
'*******	Calls the playfield display	*******
'**********************************************

Set LampCallback = GetRef("Lamps")
Sub Lamps
	pUpdateLED ()
End Sub


'******************************************************
'****  LAMPZ by nFozzy
'******************************************************

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments

Sub LampTimer()
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



Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/120 : Lampz.Modulate(x) = 1 : next

' GI Fading
	Lampz.FadeSpeedUp(110) = 1/4 : Lampz.FadeSpeedDown(110) = 1/16 : Lampz.Modulate(110) = 1
	Lampz.FadeSpeedUp(111) = 1/4 : Lampz.FadeSpeedDown(111) = 1/16 : Lampz.Modulate(111) = 1

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(1)= l1
	Lampz.MassAssign(1)= l1a
	Lampz.Callback(1) = "DisableLighting p1, 150,"
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= l2a
	Lampz.Callback(2) = "DisableLighting p2, 150,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= l3a
	Lampz.Callback(3) = "DisableLighting p3, 150,"
	Lampz.Callback(4) = "DisableLighting p4, 150,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= l5a
	Lampz.Callback(5) = "DisableLighting p5, 150,"
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(6)= l6a
	Lampz.Callback(6) = "DisableLighting p6, 150,"
	Lampz.MassAssign(7)= l7
	Lampz.MassAssign(7)= l7a
	Lampz.Callback(7) = "DisableLighting p7, 150,"
	Lampz.MassAssign(8)= l8
	Lampz.MassAssign(8)= l8a
	Lampz.Callback(8) = "DisableLighting p8, 150,"
	Lampz.MassAssign(9)= l9
	Lampz.MassAssign(9)= l9a
	Lampz.Callback(9) = "DisableLighting p9, 150,"
	Lampz.MassAssign(10)= l10
	Lampz.MassAssign(10)= l10a
	Lampz.Callback(10) = "DisableLighting p10, 150,"
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= l11a
	Lampz.Callback(11) = "DisableLighting p11, 150,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= l12a
	Lampz.Callback(12) = "DisableLighting p12, 150,"
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(13)= l13a
	Lampz.Callback(13) = "DisableLighting p13, 150,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= l15a
	Lampz.Callback(15) = "DisableLighting p15, 150,"
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= l16a
	Lampz.Callback(16) = "DisableLighting p16, 150,"
	Lampz.Callback(17) = "DisableLighting p17, 250,"    'flat prim
	Lampz.Callback(18) = "DisableLighting p18, 250,"    'flat prim
	Lampz.Callback(19) = "DisableLighting p19, 250,"    'flat prim
	Lampz.Callback(20) = "DisableLighting p20, 250,"    'flat prim
	Lampz.Callback(21) = "DisableLighting p21, 250,"    'flat prim
	Lampz.Callback(22) = "DisableLighting p22, 250,"    'flat prim
	Lampz.Callback(23) = "DisableLighting p23, 250,"    'flat prim
	Lampz.Callback(24) = "DisableLighting p24, 250,"    'flat prim
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= l25a
	Lampz.Callback(25) = "DisableLighting p25, 80,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= l26a
	Lampz.Callback(26) = "DisableLighting p26, 80,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= l27a
	Lampz.Callback(27) = "DisableLighting p27, 80,"
	Lampz.MassAssign(28)= L28
	Lampz.Callback(29) = "DisableLighting p29, 250,"    'flat prim
	Lampz.Callback(30) = "DisableLighting p30, 250,"    'flat prim
	Lampz.Callback(31) = "DisableLighting p31, 250,"    'flat prim
	Lampz.Callback(32) = "DisableLighting p32, 250,"    'flat prim
	Lampz.MassAssign(33)= l33a
	Lampz.Callback(33) = "DisableLighting p33, 250,"    'flat prim
	Lampz.MassAssign(34)= l34a
	Lampz.Callback(34) = "DisableLighting p34, 250,"    'flat prim
	Lampz.MassAssign(35)= l35a
	Lampz.Callback(35) = "DisableLighting p35, 250,"    'flat prim
	Lampz.MassAssign(36)= l36a
	Lampz.Callback(36) = "DisableLighting p36, 250,"    'flat prim
	Lampz.MassAssign(37)= l37a
	Lampz.Callback(37) = "DisableLighting p37, 250,"    'flat prim
	Lampz.MassAssign(38)= l38a
	Lampz.Callback(38) = "DisableLighting p38, 250,"    'flat prim
	Lampz.MassAssign(39)= l39a
	Lampz.Callback(39) = "DisableLighting p39, 250,"    'flat prim
	Lampz.MassAssign(40)= l40a
	Lampz.Callback(40) = "DisableLighting p40, 250,"    'flat prim
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= l41a
	Lampz.Callback(41) = "DisableLighting p41, 150,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= l42a
	Lampz.Callback(42) = "DisableLighting p42, 150,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= l43a
	Lampz.Callback(43) = "DisableLighting p43, 80,"
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(44)= l44a
	Lampz.Callback(44) = "DisableLighting p44, 80,"
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(45)= l45a
	Lampz.Callback(45) = "DisableLighting p45, 80,"
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(46)= l46a
	Lampz.Callback(46) = "DisableLighting p46, 80,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= l47a
	Lampz.Callback(47) = "DisableLighting p47, 80,"
	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= l48a
	Lampz.Callback(48) = "DisableLighting p48, 150,"
	Lampz.Callback(49) = "DisableLighting p49, 20,"
	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(50)= l50a
	Lampz.Callback(50) = "DisableLighting p50, 150,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= l51a
	Lampz.Callback(51) = "DisableLighting p51, 150,"
	Lampz.Callback(52) = "DisableLighting p52, 250,"    'flat prim
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= l53a
	Lampz.Callback(53) = "DisableLighting p53, 150,"


' bumpers
	Lampz.MassAssign(54)= bumpersmalllight1
	Lampz.MassAssign(54)= bumperbiglight1
	Lampz.MassAssign(54)= bumpershadow1
	Lampz.MassAssign(54)= bumperhighlight1
	Lampz.Callback(54) = "bumperbulb1flash"
	Lampz.MassAssign(55)= bumpersmalllight3
	Lampz.MassAssign(55)= bumperbiglight3
	Lampz.MassAssign(55)= bumpershadow3
	Lampz.MassAssign(55)= bumperhighlight3
	Lampz.Callback(55) = "bumperbulb3flash"
	Lampz.MassAssign(56)= bumpersmalllight2
	Lampz.MassAssign(56)= bumperbiglight2
	Lampz.MassAssign(56)= bumpershadow2
	Lampz.MassAssign(56)= bumperhighlight2
	Lampz.Callback(56) = "bumperbulb2flash"


	Lampz.MassAssign(60)= l60
	Lampz.MassAssign(60)= l60a
	Lampz.Callback(60) = "DisableLighting p60, 150,"
	Lampz.MassAssign(61)= L61
	Lampz.MassAssign(62)= l62
	Lampz.MassAssign(62)= l62a
	Lampz.Callback(62) = "DisableLighting p62, 150,"
	Lampz.MassAssign(63)= L63
	Lampz.MassAssign(63)= F63
	Lampz.Callback(64) = "DisableLighting p64, 20,"

	' *** Start Button Flasher ***
	Lampz.MassAssign(57)= F57
	' *** BLASTOFF Flashers ***
	Lampz.MassAssign(65)= F65
	Lampz.MassAssign(66)= F66
	Lampz.MassAssign(67)= F67
	Lampz.MassAssign(68)= F68
	Lampz.MassAssign(69)= F69
	Lampz.MassAssign(70)= F70
	Lampz.MassAssign(71)= F71
	Lampz.MassAssign(72)= F72

	'Handle solenoid activated flashers
	Lampz.MassAssign(101)= lf1a
	Lampz.MassAssign(101)= lf1b
	Lampz.MassAssign(101)= lf1c
	Lampz.Callback(101) = "DisableLighting p1a, 250,"
	Lampz.Callback(101) = "DisableLighting p1b, 250,"
	Lampz.Callback(101) = "DisableLighting p1c, 250,"
	Lampz.MassAssign(102)= lf2a
	Lampz.MassAssign(102)= lf2b
	Lampz.MassAssign(102)= lf2c
	Lampz.Callback(102) = "DisableLighting p2a, 250,"
	Lampz.Callback(102) = "DisableLighting p2b, 250,"
	Lampz.Callback(102) = "DisableLighting p2c, 250,"
	Lampz.MassAssign(103)= lf3a
	Lampz.MassAssign(103)= lf3b
	Lampz.MassAssign(103)= lf3c
	Lampz.Callback(103) = "DisableLighting p3a, 250,"
	Lampz.Callback(103) = "DisableLighting p3b, 250,"
	Lampz.Callback(103) = "DisableLighting p3c, 250,"
	Lampz.Callback(104) = "DisableLighting p4a, 250,"
	Lampz.MassAssign(104)= l4a
	Lampz.MassAssign(104)= l4aa
	Lampz.Callback(104) = "DisableLighting p4b, 250,"
	Lampz.MassAssign(104)= l4b
	Lampz.MassAssign(104)= l4ba
	Lampz.Callback(107) = "DisableLighting p7f, 250,"
	Lampz.MassAssign(107)= l7f
	Lampz.MassAssign(107)= l7fa

	for each x in GI
		Lampz.MassAssign(110)= x
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


'******************
' Bumper fading
'******************

sub initbumpers()

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

End Sub

Sub bumperbulb1flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim Z
	Z = aLvl

	BumperBase1.BlendDisableLighting = 0.5*Z
	BumperDisk1.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat1", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
	BumperSmallLight1.intensity = 30 + 30 * Z
	bumperbiglight1.intensity = 4 * Z
	BumperTop1.BlendDisableLighting = Z *.35 + .01
	MaterialColor "bumpertopmat1", RGB(255,235 - Z*36,220 - Z*90)
	BumperBulb1.BlendDisableLighting = 10*Z + 3
	BumperHighlight1.opacity = 1000 * (Z^3)
	BumperSmallLight1.color = RGB(255,255 - 20*Z,255-65*Z)
	BumperSmallLight1.colorfull = RGB(255,255 - 20*Z,255-65*Z)

End Sub

Sub bumperbulb2flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim Z
	Z = aLvl

	BumperBase2.BlendDisableLighting = 0.5*Z
	BumperDisk2.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat2", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
	BumperSmallLight2.intensity = 30 + 30 * Z
	bumperbiglight2.intensity = 4 * Z
	BumperTop2.BlendDisableLighting = Z *.35 + .01
	MaterialColor "bumpertopmat2", RGB(255,235 - Z*36,220 - Z*90)
	BumperBulb2.BlendDisableLighting = 10*Z + 3
	BumperHighlight2.opacity = 1000 * (Z^3)
	BumperSmallLight2.color = RGB(255,255 - 20*Z,255-65*Z)
	BumperSmallLight2.colorfull = RGB(255,255 - 20*Z,255-65*Z)

End Sub

Sub bumperbulb3flash(ByVal aLvl)	'argument is unused
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim Z
	Z = aLvl

	BumperBase3.BlendDisableLighting = 0.5*Z
	BumperDisk3.BlendDisableLighting = 0.75*Z + 0.75
	UpdateMaterial "bumperbulbmat3", 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
	BumperSmallLight3.intensity = 30 + 30 * Z
	bumperbiglight3.intensity = 4 * Z
	BumperTop3.BlendDisableLighting = Z *.35 + .01
	MaterialColor "bumpertopmat3", RGB(255,235 - Z*36,220 - Z*90)
	BumperBulb3.BlendDisableLighting = 10*Z + 3
	BumperHighlight3.opacity = 1000 * (Z^3)
	BumperSmallLight3.color = RGB(255,255 - 20*Z,255-65*Z)
	BumperSmallLight3.colorfull = RGB(255,255 - 20*Z,255-65*Z)

End Sub


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


'******************************************************
' GI fading
'******************************************************

dim ballbrightness

'GI callback

Sub GIUpdates(ByVal aLvl)	'argument is unused
	dim x, girubbercolor, giflippercolor ',girampcolor

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
	LFLogo.blenddisablelighting = 0.1 * alvl + .025
	RFLogo.blenddisablelighting = 0.1 * alvl + .025

' targets
	For each x in GITargets
		x.blenddisablelighting = 0.25 * alvl + .1
	Next

' prims (Upper Lane Red Plastics)
	For each x in GIPrims
		x.blenddisablelighting = .1 * alvl + .05
	Next

' prims (Blue Pegs)
	For each x in GIPegs
		x.blenddisablelighting = .025 * alvl + .025
	Next

' prims (Cutouts)
	For each x in GICutoutPrims
		x.blenddisablelighting = .25 * alvl + .15
	Next

	For each x in GICutoutWalls
		x.blenddisablelighting = .1 * alvl + .2
	Next

' prims (Rocket, Moon, MoonRamp, Spinner, lander, apron, lamps, brackets)
	MoonRamp.blenddisablelighting = 0.05 * alvl + .05
	Rocket.blenddisablelighting = 0.3 * alvl + .05
	Moon.blenddisablelighting = 0.15 * alvl + .05
	Lander.blenddisablelighting = 0.125 * alvl + .05
	sw41Prim.blenddisablelighting = 0.075 * alvl
	Primitive038.blenddisablelighting = 0.025 * alvl
	Apron_new.blenddisablelighting = 0.2 * alvl + .3
	CardSx.blenddisablelighting = 0.2 * alvl + .3
	Exhaust001.blenddisablelighting = 0.15 * alvl + .15
	Primitive145.blenddisablelighting = 0.05 * alvl + .05
	FullGate_minusPosts001.blenddisablelighting = 0.025 * alvl
	FullGate_minusPosts002.blenddisablelighting = 0.025 * alvl
	FullGate_minusPosts003.blenddisablelighting = 0.025 * alvl
	Primitive144.blenddisablelighting = 0.025 * alvl
	MiniLamp1.blenddisablelighting = 0.025 * alvl + .05
	MiniLamp2.blenddisablelighting = 0.025 * alvl + .05
	MiniLamp3.blenddisablelighting = 0.025 * alvl + .05
	MiniLamp4.blenddisablelighting = 0.025 * alvl + .05
	trapDoorPrim.blenddisablelighting = 0.025 * alvl + .025
	TopRamp.blenddisablelighting = 0.025 * alvl + .025
	Primitive047.blenddisablelighting = 0.1 * alvl + .15
	TRKickerPrim.blenddisablelighting = 0.2 * alvl + .1
	TRKickerPrim001.blenddisablelighting = 0.2 * alvl + .1

	flasherbase1.BlendDisableLighting =  1.5 * alvl +0.5
	flasherbase2.BlendDisableLighting =  1.5 * alvl +0.5
	flasherbase3.BlendDisableLighting =  1.5 * alvl +0.5
	flasherbase4.BlendDisableLighting =  1.5 * alvl +0.5
	flasherbase5.BlendDisableLighting =  0.2 * alvl +0.3
	flasherbase6.BlendDisableLighting =  0.2 * alvl +0.3
	flasherbase7.BlendDisableLighting =  0.2 * alvl +0.3

' rubbers
	girubbercolor = 128*alvl + 64
	MaterialColor "Rubber White",RGB(girubbercolor,girubbercolor,girubbercolor)

' ball
	if ballbrightness <> ballbrightMax Or ballbrightness <> ballbrightMin Or ballbrightness <> -1 then ballbrightness = INT(alvl * (ballbrightMax - ballbrightMin) + ballbrightMin)

	gilvl = alvl

End Sub


'******************************************************
' LED's display on playfield
'******************************************************

Dim Digits(0), digitState, nummb,mb0, mb1, mb2, mb3, mb4, mb5, mb6, mb7, mb8, mb9, mbl

Digits(0) 	= Array(led0,led1,led2,led3,led4,led5,led6)
digitState	= Array(0,0,0,0,0,0,0)

Sub pUpdateLED()
	Dim chgLED, ii, iii, num, chg, stat, digit
	chgLED = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(chgLED) Then
		For ii = 0 To UBound(chgLED)
			num  = chgLED(ii, 0)
			chg  = chgLED(ii, 1)
			stat = chgLED(ii, 2)
			iii  = 0
			For Each digit In Digits(num)
				If Num = 0 Then
					If (chg And 1) Then digit.State = (stat And 1)
					digitState(iii) = digit.State
					chg  = chg \ 2
					stat = stat \ 2
				End If
				iii = iii + 1
			Next
		Next
	End If
	nummb=led0.state+led1.state*2+led2.state*2+led3.state*3+led4.state*4+led5.state+led6.state-3
	mbl=1
	Select Case nummb
       Case 1:mb.image= "pf1 apollo13"
        Case 8:mb.image= "pf2 apollo13"
        Case 6:mb.image= "pf3 apollo13"
        Case 3:mb.image= "pf4 apollo13"
        Case 5:mb.image= "pf5 apollo13"
        Case 9:mb.image= "pf6 apollo13"
        Case 2:mb.image= "pf7 apollo13"
        Case 11:mb.image= "pf8 apollo13"
        Case 7:mb.image= "pf9 apollo13"
        Case 10:mb.image= "pf0 apollo13"
		Case Else  : mbl=0:	mb.image="pf00 apollo13"
	End Select
	mb.state=mbl
End Sub



'******************************************************
' 					LUT
'******************************************************


Sub SetLUT  'AXS
	Table1.ColorGradeImage = "LUT" & LUTset
	VRFlashLUT.imageA = "FlashLUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
	VRFlashLUT.opacity = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	VRFlashLUT.opacity = 100
	Select Case LUTSet
		Case 0: LUTBox.text = "Fleep Natural Dark 1"
		Case 1: LUTBox.text = "Fleep Natural Dark 2"
		Case 2: LUTBox.text = "Fleep Warm Dark"
		Case 3: LUTBox.text = "Fleep Warm Bright"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard"
		Case 6: LUTBox.text = "Skitso Natural and Balanced"
		Case 7: LUTBox.text = "Skitso Natural High Contrast"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 10: LUTBox.text = "HauntFreaks Desaturated"
  		Case 11: LUTBox.text = "Tomate washed out"
        Case 12: LUTBox.text = "VPW original 1on1"
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"
        Case 15: LUTBox.text = "B&W Comic Book"
        Case 16: LUTBox.text = "Skitso New Warmer LUT"
	End Select

	If LUTset = 14 Then
		If GreenDMD = 1 Then
			DMD.color = RGB(0,255,50)
			DMD.opacity =150
			ScoreText.fontcolor = RGB(0,255,50)
		Else
			DMD.color = InitDMD_Color
			DMD.opacity =InitDMD_Opacity
			ScoreText.fontcolor = InitDMD_Color
		End If
	Else
			DMD.color = InitDMD_Color
			DMD.opacity =InitDMD_Opacity
			ScoreText.fontcolor = InitDMD_Color
	End If

	LUTBox.TimerEnabled = 1
End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if LUTset = "" then LUTset = 16 'failsafe to Skitso's original warm LUT for Apollo13

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "Apollo13LUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=16
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "Apollo13LUT.txt") then
		LUTset=16
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "Apollo13LUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=16
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub


Sub ShowLUT_Init
	LUTBox.visible = 0
	If LUTset = 14 Then
		If GreenDMD = 1 Then
			DMD.color = RGB(0,255,50)
			DMD.opacity =150
			ScoreText.fontcolor = RGB(0,255,50)
		Else
			DMD.color = InitDMD_Color
			DMD.opacity =InitDMD_Opacity
			ScoreText.fontcolor = InitDMD_Color
		End If
	Else
			DMD.color = InitDMD_Color
			DMD.opacity =InitDMD_Opacity
			ScoreText.fontcolor = InitDMD_Color
	End If
End Sub


'*******************************************
'  Ball brightness code
'*******************************************

if BallLightness = 0 Then
	table1.BallImage="ball-dark"
	table1.BallFrontDecal="JPBall-Scratches"
elseif BallLightness = 1 Then
	table1.BallImage="ball_HDR"
	table1.BallFrontDecal="Scratches"
elseif BallLightness = 2 Then
	table1.BallImage="ball-light-hf"
	table1.BallFrontDecal="g5kscratchedmorelight"
elseif BallLightness = 3 Then
	table1.BallImage="pinball3w"
	table1.BallFrontDecal="JPBall-Scratches"
else
	table1.BallImage="ball-lighter-hf"
	table1.BallFrontDecal="g5kscratchedmorelight"
End if

' ****************************************************



' ***********************************************************************************************
' VR Room Code
' ***********************************************************************************************

Dim VRThings

if VR_Room = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in Blasters:VRThings.height = 335:Next
	for each VRThings in Blasters:VRThings.y = 0:Next
	for each VRThings in Backglass_High:VRThings.opacity=0: Next
	for each VRThings in Backglass_Mid:VRThings.opacity=0: Next
	for each VRThings in Backglass_Low:VRThings.opacity=0: Next
	if Cab_Mode = 1 Then
		Ramp031.visible=0
		Ramp032.visible=0
		Ramp033.visible=0
		Ramp034.visible=0
		ScoreText.visible=0
		PinCab_Blades.visible=1
		for each VRThings in Blasters:VRThings.visible=0: Next
		L_DT_Apollo.visible = 0
		L_DT_Apollo.state = 0
		L_DT_Apollo2.visible = 0
		L_DT_Apollo2.state = 0
	Else	
		Ramp031.visible=1
		Ramp032.visible=1
		Ramp033.visible=1
		Ramp034.visible=1
		ScoreText.visible=1
		PinCab_Blades.visible=0
		L_DT_Apollo.visible = 1
		L_DT_Apollo2.visible = 1
	End If

Else
	for each VRThings in VRStuff:VRThings.visible = 1:Next
	for each VRThings in VRClock:VRThings.visible = WallClock:Next
	for each VRThings in Blasters:VRThings.height = 454:Next
	for each VRThings in Blasters:VRThings.y = -42.27:Next
	for each VRThings in Backglass_High:VRThings.opacity=350: Next
	for each VRThings in Backglass_Mid:VRThings.opacity=350: Next
	for each VRThings in Backglass_Low:VRThings.opacity=350: Next
	PinCab_Backglass.blenddisablelighting = .05
	Ramp031.visible=0
	Ramp032.visible=0
	Ramp033.visible=0
	Ramp034.visible=0
	ScoreText.visible=0
	PinCab_Blades.visible=1
		L_DT_Apollo.visible = 0
		L_DT_Apollo.state = 0
		L_DT_Apollo2.visible = 0
		L_DT_Apollo2.state = 0
'Custom Walls, Floor, and Roof
	if CustomWalls = 1 Then
		VR_Wall_Left.image = "VR_Wall_Left"
		VR_Wall_Right.image = "VR_Wall_Right"
		VR_Floor.image = "VR_Floor"
		VR_Roof.image = "VR_Roof"
	end if

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

'*************************************************************************************** 
' CODE BELOW IS FOR THE VR CLOCK.  This was originally taken from Rascal VP9 clock table 
'***************************************************************************************

Dim CurrentMinute ' for VR clock

Sub ClockTimer_Timer()

' ***ClockHands Below *********************************************************

	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())

End Sub



'******************************************************************
'******** Revisions done on VR version 1.0 by UnclePaulie *********
'******************************************************************
' - Added all the VR Cab primatives
' - Added two different VR room environments
' - Backglass Image from chucky87's B2S file... Note:  ONLY used the dark the image, but redrew the backglass to remove BLASTOFF
' - Added the grill/dmd to the backbox
' - Moved and resized the BLASTOFF flashers up to the grill.  Completely interactive like the real table.
' - Rails on left and right removed
' - Changed the ball image and scratches.  Looked better when rolling around.
' - Changed backwall height to 345 from 440
' - Matched colors for flipper buttons to cabinet - made red
' - The lights on the bumpers too bright; lowered intensity from 15 to 5.
' - Primative 16,31,34,45 and 037 (red domes on Right) changed depth bias to -200.  They were too transparent.  Also changed disable lighting to 0.7.
' - Primatives 031,034,045 changed the material to octadome_red to match other primatives.
' - Also changed the intensity of red light flashes above them to 5 (was 20)
' - Changed the intensity of other white and red light flashes to 5 (was 20)... lf5,5a,5b,6a,6b,6c
' - Changed lights: GI47,48, 49 intensity from 4 to 2.  The lanes were too bright
' - Flasher001-004 modified the opacity to 50% (was 150, and -002 was 300)
' - Resized lamps L64 and L49 to be inside LED sign primative, and changed intensity to 80.  
' - LED on sign flashers F64 and F49 resized, rotated to 135, changed image, and adjusted opacity to 50.  Makes the LED look more real in VR.
' - Raised the apron from -58 to -43 z height.  The ball looked like it was going through a wall when going to drain.  
' - Also moved the cards up from 56.2 to 71.2
' - Apron walls changed to side visible and added image to block seeing under apron 
' - Raised pincab_bottom to 60 (was -5)
' - Modified the pf image to get rid of all the white on sides and under apron.  It was so easily seen in VR.
' - Changed image on Ramp036 (the metal piece on right side) to image: scratched metal.  Looks more realistic.
' - Adjusted the rolling sounds down. Inserted the constant, VolDiv, into the code, and changed value to 20,000 (was 2,000)
' - Changed material of top left trigger to match the others.  Original was trying to get more reflective.  Wanted it to match others.
' - Back wall left light needed to be brighter, as well as the back flasher lights.  Changed f63 to 80% opacity, and l63 to 5 intensity.
' - Also changed f4,5, and 7 to 100% opacity.  
' - Changed Primitive140 depth bias to -1500.  It was causing a weird flashing issue.
' - All the walls are ramps, on layer1.  They're conflicting with the playfield, changed top and bottom heights from 0 to 0.01
' - The rollover triggers were too tall and spikey.  Scaled them on the Y by 1.05.  Also droped to switch height wall surface of -10, and changed hit height to 40.
' - Made the drop holes transparent to be more realistic when the ball drops in
' - Made playfield material active, so you can see through the drop holes
' - Replaced some of the sounds from Fleep (flippers, bumpers, slings), and adjusted their sounds to match table
' - Animated the flipper buttons
' - First attempt at blender... made a new primitive launcher similar to what the real one has. 
' - Animated the Launcher 
' - Adjusted some posts and screws to align better in VR
' - Changed the price card on the right

'******** Revisions done on VR version 1.0 by retro27 *********
' - Modified the graphics on the side, front and backbox. 
' - Added a topper 
' - Added a start button flasher.  
' - Added a rod grommet.

'******** Revisions done on VR version 1.1 by UnclePaulie *********
' - Animated the Backglass

'******** Revisions done by UnclePaulie Version 1.2 ***************
' - Hybrid VPX Modes:  Works for Cab, Desktop, and VR.  VR_Room 1 or 0 to enable 

'******** Revisions done by UnclePaulie Version 1.3 ***************
' - Added nFozzy and Roth physics and flippers
' - Added Fleep Sounds
' - Modified the table physics, especially the gravity constant.  Also sling and bumper thresholds changed to match table physics
' - Added ramp triggers for ball rolling ramp sounds 
' - Adjusted ramp012 slightly towards saucer as ball would get stuck if ball going too slow.
' - Adjusted physics for 5 ramps to zCol_Ramp physics material to reduce spin coming off ramps.
' - Adjusted wall005 to get a little better angle to get balls drained into trough faster
' - Changed sounds of drain post to a drop target sound.  Modified sounds for moon lift, rocket, ramplift 
' - Modified playfield to include trigger, saucer, and sling holes, and associated plywood inset holes

'******** Revisions done by UnclePaulie Version 1.4 ***************
' - Added dynamic ball shadows by Iakki, Apophis, and Wylte

'******** Revisions done by UnclePaulie Version 1.5 ***************
' - Ball sometimes gets stuck in the right saucer.  Curved Ramp012 a little towards hole
' - Issue with DOF equipped cabs, for both flippers and the left sligshot not triggering DOF solenoids. 
'   - VPMPulse switch was incorrect for sling (was 59, should have been 61)
'	- Added the SolLFlipperd solenoid sub scripts for DOF callouts for left and right flippers

'******** Revisions done by oqqsan Version 1.6 ***************
' - The moon magnet stutters the ball when rotating.  oqqsan found that if the values of the moon timer and the rotation lengths are shortened, it smooths out.

'******** Revisions done by oqqsan Version 1.7 ***************
' - oqqsan modified added Flupper dome lights and flashers
' - Tomate created new ramps, and oqqsan added to the table.
' - The 13 ball multiball had an issue where a ball would not release.  Adjusted the SolRocketLift sub for a timer to be shorter.

'******** Revisions done by Skitso Version 1.8 ******************
' - Redone GI lighting, spot lights and inserts, changed ball image and scratches, new warmer LUT, tweaked a lot of different textures

'******** Revisions done by Sixtoe Version 1.9 ******************
' - Updated ball reflections, hooked up moon, rocket and plastic ramp to DL for the GI, trimmed all lights so they don't stick out the VR cab, turned collidable status off on loads of things, removed a couple of lights from dynamicshadows, locked unlocked assets.

'******** Revisions done by Apophis Version 1.10 ******************
' - Installed Lampz

'******** Revisions done by UnclePaulie Version 1.11 ******************
' - Set blasters to visible to work with Lampz
' - Also changed lamptimer subroutine to NOT have logic for b2son in the chglamp = Controller.ChangedLamps

'******** Revisions done by tomate Version 1.12 ******************
' - New top metal ramps primitives added
' - New moon ramp prim added
' - WireRamps primitives fixed to match the new geometry
' - New moon ramp and wire ramps low poly collidable primitives added
' - Some fixes on posts, gates and stuff to match the new moon ram geometry
' - Main ramp Triggers repositioned
' - Ramps textures and metal plate still missing

'******** Revisions done by UnclePaulie Version 1.13 ******************
' - Corrected sounds on updated ramps
' - Corrected diverter triggers on new ramps
' - Slings weren't animating correctly.  Fixed the animation, and added a couple sling walls 
' - Modded the playfield to add holes under slings.  added plywood hole textures

'******** Revisions done by Tomate Version 1.13 ******************
' - Metal plate ramp and moontexture ramp added

'******** Revisions done by Tomate Version 1.14 ******************
' - Added plastic ramp texture

'******** Revisions done by Apophis Version 1.15 ******************
' - Updated PF image with alpha cutouts for primitive inserts
' - Added insert primitives and hooked them up to Lampz, including some solenoid activated inserts.
' - Various insert lighting adjustments.

'******** Revisions done by UnclePaulie Version 1.16 ******************
' - Added a surface wall to the moon ramp to activate ball lock trigger.
' - New ramps needed to have hit event checked to enable wall sounds.  Also the right ramp spinner needed a surface, as it wasn't active or seen.
' - Moved the day/night slider +1 based of RIK's recommendation, as it was a bit too dark.
' - Inserted new translite backglass image for VR backglass that was provided by Sheltemke
' - Updated PF image with cutouts for sling/rubber walls (they were missed in the prior pf update.)

'******** Revisions done by Apophis Version 1.17 ******************
' - Added missing "Master Alarm" and "A13" inserts.

'******** Revisions done by UnclePaulie Version 1.18 ******************
' - Changed top lane walls to be closer to real primative.  Ball bumped into nearby wall.
' - Adjusted shape of ramp009 and ramp010 slightly to better follow the playfield alignment.  
' - Changed right wire gate (sw55) to gate sound.  (was a spinner sound)
' - After Lampz was added, the lights on the moon ramp (L49,F49,L64,f64) no longer needed, and caused them to be too bright.  
'   Using only the Prims now, and adjusted the intensity in the Lampz script.  
' - GI70 was too bright, causing the upper playfield lanes to be very bright.  Lowered the intensity from 8 to 2.
' - GI67 over the video inset too bright.  Lowered intensity from 7 to 2.

'******** Revisions done by Sheltemke and UnclePaulie Version 1.18text a ******************
' - Sheltemke changed the pf image to block out text.  Added a text flasher to make text more readable
' - UnclePaulie adjusted the z height of 86 insert primatives (all the trianfgles, rounds, and rectangles) to z position of -0.1.

'******** Revisions done by UnclePaulie Version 1.18text b ******************
' - Removed all the lights that used to be where the new primatives were.
' - Adjusted the intensity of some of the prims

'******** Revisions done by UnclePaulie Version 1.19 ******************
' - Changed the material of the insertships on to white.  Some inserts were green tint. 
' - Changed the intensity of the pop up at bottom of playfield from 7 to 2

'******** Revisions done by UnclePaulie Version 1.20 ******************
' - Fixed the back ramp issue of not allowing the ball to go up the ramp during orbit mission and moon gravity mission.
' - Added cabinet pov per recommendation by apophis

'******** Revisions done by Tomate Version 1.21 ******************
' - Missing texture for Multiball post added
' - EightBallRamp001 height fixed (so that the balls are at the correct height when they are inside the ramp)
' - MoonRamp texture Updated with some scratches
' - trapDoorPrim separeted
' - metal prims at the en of the ramp redone
' - swap all the metals material to metal 0.8

'******** Revisions done by UnclePaulie Version 1.22 ******************
' - Had to put the eightballramp001 back to original value.  8-ball lock was intermittent.

'******** Revisions done by UnclePaulie Version 1.23 ******************
' - Adjusted material of white insert on tri

'******** Revisions done by UnclePaulie Version 1.24 ******************
' - Animated the trap door for 8-ball lock

'******** Revisions done by Wylte Version 1.25 ******************
' - Optimized ball shadows, rounded pov.  Removed shadows when in the subway.

'******** Revisions done by Apophis Version 1.26 ******************
' - Got dynamic shadows to work. Reduced the number of GI lights in DynamicSources to six.
' - Fixed flipper nudge

'******** Revisions done by UnclePaulie Version 1.27 ******************
' - Rewrote the code for the rocket lift to get all multiball modes to work correctly.  
' - Fixed the throughholes for the subway, and the subwaysuperVUK
' - Adjusted the timing of the rocket movement to match real table timing

'******** Revisions done by apophis Version 1.28 ******************
' - Small correction to the subway ball shadows being seen

'******** Revisions done by tomate Version 1.29 ******************
' - Fixed wireRamps Primitive
' - New wireRamps textures
' - Fixed missing materials in some screws and metal ramp
' - New apron primitive and texture

'******** Revisions done by Wylte Version 1.29b ******************
' - Shadow test - Trying to optimize the code for better performance
'	 - Added 3 more lights to collection, 1% script performance hit on my end (9 up to 10%)
' - Ambient shadows set to invisible by default
' - Separated dynamic and ambient ball shadows options, skipping section of code within the sub
'	 - both must be disabled to completely avoid the timer calling the sub!
' - Added LockBarKey as plunger alternate

'******** Revisions done by UnclePaulie Version 1.30 ******************
' - Added LUT options

'******** Revisions done by UnclePaulie Version 2.0 ******************
' - Put option for prior apron back in.  Desktop looks better with old, and VR looks better with new 
' - Tweaked red insert material off, as was too dark when not lit
' - A couple other inserts had incorrect images.  Corrected.
' - Turned the Blastoff flasher off in cab mode
' - Release version 

'******** Revisions done by UnclePaulie Version 2.01 ******************
' - Unchecked static rendering on ApronS Primative.  Wasn't toggling in desktop mode.  Only VR.  Now Apron toggle works in both

'******** Revisions done by Tomate Version 2.02 ******************
' - Created a new apron primative.  Looks even better than the prior one.

'******** Revisions done by UnclePaulie Version 2.03 ******************
' - Added SaveLUT to table exit sub.  The LUT wasn't saving on exit
' - Rawd suggested a green dmd mod to the blacklight LUT 14 for the DMD look more visible in VR and Virtual DMD's
' - Moved VPMDMD script

'******** Revisions done by UnclePaulie Version 2.04 ******************
' - Added a InitDMD to look up the user's DMD color and opacity for use in the LUT script  

'******** Revisions done by UnclePaulie Version 2.05 ******************
' - Moved VPMDMD script back.

'******** Revisions done by UnclePaulie Version 2.06 ******************
'v2.06	Removed duplicate functions
'		Added apron physics walls.  Bug found that a ball could jump onto apron during play.
'		Added a game rom setting to enforce sound is on.  Someone reported ROM sounds weren't playing.  Forced sounds to be on.
'		Updated desktop backglass and desktop pov, and changed to a desktop backglass dmd.  Added GI Flasher lamps to desktop backglass
' 		Automated the VR, desktop, and cabinet mode.
'		Added slingshot corrections solution created by apophis
'		Disabled 4XAA, post proc AA, in game AO and scsp reflections
'		Changed LUT selection to hold down left magna and change with right.
'		Added posters in the room for VR, as well as outlet and cord, and VR Lut images.
'		Added several ball brightness options
'		Reduced reflection of elements on playfield from 40 down to 10
'		Changed black rubbers to white
'		Added some disable lighting to moon, rocket, flippers, spinner, balls, rubbers, lamp prims, lander, brackets.  And then made them interactive with GI.
'		Combined some timers, and removed some not used.
'		Updated to latest fleep sounds and routines
'		Added GI relay
'		Added GI fading routine to be controlled via Lampz.
'		Corrected spinner rotation angle.
'		Changed physics on small vertical rectangle targets to z_targets
'2.0.7	Changed standup targets to Rothbaurer's solution.  Added to GI fading routines.
'		Added GI bulb primitives, and fine adjusted a few GI playfield lights.
'		Modified Flupper flasher domes to accomodate for GI.  The domes will adjust based on GI levels.
'		Added GI to the large back red light.
'		Added more realistic sounds to drainpost
'		Adjusted GI66 to not be so bright coming through the plastic hole on left side.
'		Added a low intensity GI light over the playfield moon and earth.  I wanted it lit a little more... more of a glow when GI on.
'		Changed the playfield alpha channel to 132.  Helps with the edges of inserts.
'		Added additional physics walls under some plastics.
'2.0.8	Changed the ball trough to not destroy balls.
'		Removed option to preload 8 balls.  Automatically has balls loaded.
'2.0.9	Redid the subway logic and kickers.  No longer destroy balls.
'		I put in a subway trough of 8 balls.  It'll never happen, but just in case if fills with several balls, it'll be controlled.
'		Put in a basic playfield_mesh to accomodate for the VUK hole inside the apron from the sub.  Will do saucers later.
'		All prepped for full gBOT implementation, and updated VPW physics.
'		Added a VUK ramp to allow for subway VUK.  Also added a gate to ensure ball doesn't fall back into subway trough from under apron.
'2.0.10	Updated playfield mesh for saucers.
'		Updated saucer images, primitives, and strength of kickouts.  
'		Updated the cutout prim images and material.  Added GI to new kickers.
'		Added a physics wall under the moon.  Also one under flippers in case there are issues with ball cutting under playfield_mesh.
'2.0.11	Updated cab pov, reflection strength
'		Saucers were too bright of gray; darkened some
'		Added some GI to the plunger cover.
'		Bulb in left plastic could see too bright in cab mode, lower intensity slightly.
'		Changed the material and removed image for blue pegs.  Was too transparent before.
'		Moved back wall slightly.  Too far back... could see in VR.
'		Redid the slingshot animations.  And put a new sling prim in.
'		Adjusted the depth bias of the white flasher domes.  You could see thorugh the plastics a little too much... such that you see right through the flupper flasher domes.
'		Stayed with one apron, removed the old Option
'		Added cutout holes for targets, and associated prims.
'2.0.12	Changed the code to accomodate for gBOT and updated to VPW physics.  Also cleaned code up.
'		Some VR material updates. Added black walls inside cabinet.
'		Added bulb cutouts, and put holes in playfield fo them.
'		Cut holes in GI around any playfield hole (inserts, rollovers, slings)
'2.0.13	Added insert lights wiht low intensity, but insert glow lights with a little higher insensity.
'2.0.14	Added stand up target physics to the verticle targets.  Had to slightly rewrite the STArrayID function, since three targets pulse same switch.  Also, Used standard targets as images.
' 		Ensured glow lights did not go over the holes.
'		Removed and combined some timers.
'		Corrected some wall, ramp, metal, and arpon sounds and hit thresholds.  As well as sleeves and rubber physics.
'		Moved the normal insert prims to z height of zero.  The flat prims are still at -0.1, and -0.2.
' 		Put instructions in table.info.
'2.0.15	Adjusted the flippers to be more real size (too long before.), and precise location.
'		Lots of online play to ensure shots are replicated as close as possible.  Adjusted some wall locations slightly to accomodate.
'		Corrected total number of balls for ramprollingfx.
' 		Added an additional rocket orange/red flasher inside the rocket ramp.  Adjusted the color and intensity of the outside of rocket flashers.
'		Redid the insert light and glow sizes.  Could see them in VR.  Also glow intensity.  Had to put the insert prims back to -0.1, could see them in VR and 4k.
' 		Moved the apron up a little, and the other viewable walls there.
'2.0.16 Changed the bumpers extensively.  Reused a lot of Flupper bumpers, but needed to tie into lampz.
'		Implemented some equations from flupper bumper fading routines
' 		Modded the top bumper image for the A13 sticker.
'2.0.17	Updated the table lighting, height, scale, etc.
'		Updated the table friction to 0.2.
'		Added GI to the cutouts
'2.1	Updated sling rubbers, and physics to align with playfield better, as well as the rounding shape of rubber corners.
'		Corrected some rubber collections, material, shapes.
'2.1.1	Widened the bottom of the RampLiftDown from 100 to 120.  On a fast orbit shot, the ball could hit the bottom of the ramp.  
'		DOF for flippers quit working.  Needed to add code in for DOF from prior version.  
'		Changed material on lifter ramp to active. (couldn't see moving)
'		Moved on ramp trigger sound from active ramp to OrbitRamp1.  
'		Slight mod to the apron visible under wall... could slightly see in VR.
