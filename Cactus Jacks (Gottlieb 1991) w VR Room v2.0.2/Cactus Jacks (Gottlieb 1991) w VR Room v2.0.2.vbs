'Cactus Jacks VPX table by bodydump and borgdog

'******** Revisions done by Borgdog and Bodydump on version 0.95 *********
' borgdog for creating the work on the 0.95 table
' bodydump for the initial working version with his great redraws of the playfield and plastics
' dark for all the cactus models, including the animations of the big 3 back cacti
' bord for all the lighting and shadow stuff he does that he did for this
' rothbauerw for his physics tutorial and updates and play testing and tweaking

'******** Revisions done on VR version 1.0 by UnclePaulie *********
' - Added all the VR Cab primatives
' - Backglass Image from Wildman's B2S file 
' - Added the alphanumeric displays from Class of 1812
' - Added a VR apron wall to block seeing through
' - Added two different VR room environments
' - Rails on left and right removed
' - Changed the plunger postion slightly to hit ball better
' - Increased size of the plunger cover for VR
' - Rotated the triggers in outlanes and top lanes by 180 degrees
' - Matched colors for flipper buttons to cabinet
' - Changed the ball image and added jp_scratches for decal to enhance the ball rolling realism
' - Added the five fruit flashers for the backglass
' - Changed GI Lights 1-9 to intensity=1.  It was 4, and was too bright.
' - Changed GI Lights 10-20 to intensity=1.  It was 6, and was too bright.
' - Implemented a fully animated backglass based on solenoid callouts.  Flashers on layer 11
' - Changed material on outer primitive to "Plastic with an image".  It was white plastic, and you could see through.
' - Animated the flipper buttons
' - Increased the day/night mode a couple ticks.  
' - Changed a few sounds from fleep's sounds (bumper, flippers, slings, and coin in)

'******** Revisions done on VR version 1.0 by Sixtoe *********
' - redid the cabinet artwork
' - fixed the ramps so they're transparent
' - unified nearly all the timers
' - dropped the plunger lane trigger
' - replaced the blade prim
' - changed the colour of the drop holes slightly
' - dropped the primitive flippers as they were too high
' - bumped the DL for the transparencies and dropped the DL for cabinet things

'******** Revisions done by UnclePaulie on Hybrid version 0.1 - 2.0 *********
' v0.1 	Added hybrid mode for VR room, cabinet, and desktop
' 		Added DTRails for left and right rails in desktop mode
' v0.2	Added Updated/more recent Rothbauerw/nFozzy physics and Flipper physics and Fleep Sounds
' 		Added dynamic shadows solution to the solution created by iaakki, apophis, Wylte
'		Added GI relay sounds 
'		Added knocker solenoid and position for sound
' v0.3	Added updated ramprolling sounds solution created by nFozzy and VPW
' v0.4	Changed to new droptargets and standup targets to solution created by Rothbauerw
'		Added VR clock 
' 		Increased the plunger pull speed slightly, and changed the mech strength and momemtum transfer
'		Corrected Drop target switches from original VPX table.  They were reverse order on the upper drops.
' v0.5	Added drop target shadows
'		Added a standup target shadow 
'		Adjusted the length of the flippers down by 2 VP units, and slightly adjusted placement.
'		Also slight adjustment to top of sling pegs.
'		Adjusted the strength of the VUK near the top, as a couple times it wouldn't make it up the ramp.
'		Added 3 ball options, dark, bright, and brightest
'		Moved the day/night mode back a tick
' v0.6	Slight adjustment of the outerlane pegs to align better with playfield hole.  
'		Adjusted heights of apron walls, and added several other invisible walls to block ball jumping over in corner conditions
'		Adjusted outer ramp pegs slightly to match close up pictures of a real table.
'		Readjusted flippers to match original size.
' v0.7	Changed tilt relay switch to 151.  It was set to 149 and never tilted.
'		Added drop ball sound when ball falls into the subway from UpperHoleTrigger
'		Changed desktop pov to see more of playfield 
'		Several physics updates including bumper scatter angle, sling physics to z_sling, flipper physics updates to VPW recommendations.
'		Also updated the playfield friction to 0.22 form 0.25 (VPW recommend setting)
'		Changed the subway eject angle slightly to match the rollout to real table.
' v0.8	Changed all physics to zCol_xyz, insteand of the a_physics.  Walls, posts, sleeves, ramps, etc. Matched VPW recommendations.
' v0.9	Adjusted the peg and post under apron to turn them to be collidable with physics
' 		Adjusted the right sling pegs, sling, and rubbers up slightly to better aling with playfield.  Helped issue with right outlane.
'		Added groove to plunger lane, and small ball release ramp to get up.  Had to increase strength of ballrelease from 8 to 12 to get up groove.
'		Changed the VR backglass general lamps to be controlled by the GI on or off, rather than game on/off
'		Dimmed the backglass in VR a little more to show lamps better
'		Added additional relay sounds to VR backglass 
'		Adjusted some of the GI lights (10-20) back up in intensity to 3
'		Added new wood floor for VR with a shadow around trim
' 		Adjusted the day/night cycle back to 2.5
' v0.10	Changed the backglass relay sounds to flash relay sounds 
' v0.11	Completely changed the trough logic to handle balls on table
'		No longer destroy balls.  Use a global gBOT throughout all of script.
'		Removed all the getballs calls and am only useing gBOT.  Will help eliminate stutter on slower CPU machines
' v0.12	Ball would get stuck on bottom bumper and not go into subway.  Moved rail and it's physics wall slightly.
'		Corrected tnob to 2, and right sized the number of ball shadows
'		Code Cleanup
' v0.13	Added LUT options
'		Added other VR elements (power cord, optional posters, optional topper)
'		Corrected the tilt switch.  It wouldn't tilt in VR with switch 149 or 151.  Desktop worked, but not VR.  Changed to the GTS3.vbs defined SWTilt switch of -7... fixed it.
'		Slight adjustment of Wall15, above the plunger.  Spacing between the plunger and the ball was a bit too much gap.
' v0.14 Added an option for the cacti wobble on top of bumpers moving for slower systems to improve performance
' v0.15	Added rules sheet to table info
'		Adjusted digital nudge sensitivity and force 
'		Testing by Rik, PinStratsDan, Apophis, and Wylte.
' v2.0	Release Version
' v2.0.1 Minor additional code to correct corner conditions where ball could get stuck behind bottom bumper and on left ramp 
'		 metalrail13 had a height a little too high, and was sticking above the ramp.  Reduced height from 75 to 50.  
'		 Added another wall block as a protector for other ball bounces.
' v2.0.2 Two non visible ramps had visibility checked.  Hard to see, but if you look close you can see...
'		 Tomate created a new ramp object that is smoother to go up and around ramp.
'		 Removed the code to block against it getting stuck on the ramp.  Shouldn't get stuck there anymore.

' *******************************************
' ******  Gotlieb Add 3 Credits Issue  ******
' *******************************************

' - If your ROM is adding 3 credits... you need to go into service menu and change center coin slot to 1 credit
'   - Press 7 when starting the game to enter ROM menu
'   - Immediately Press LeftFlipper to enter game adjustments
'   - Press 7 to advance to the adjustment you want to modify.  The center slot credit menu is adjustment number 11. 
'   - Press left or right flipper to increase or decrease the setting.  (In this case, hit left flipper twice, to go from 3 to 1)
'   - Press F3 to save


Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01120100", "gts3.VBS", 3.15


'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'******************************************************************************************

' *** Desktop, VR, or Full Cabinet Options *** These HAVE to be selected depending on the mode you are playing.

const VR_Room = 0 ' 1 = VR Room; 0 = desktop or cab mode, 
const cab_mode = 0 ' 1 = cabinet mode (Turns off the desktop backglass score lights)
const BallBrightness = 1 '0 = dark, 1 = brighter, 2 = brightest

' *** If on a slower machine and experience performance
const Wobbleon = 1 ' 0 Turns the cacti wobble off on top of the bumpers.  1 = Wobble effect on

' *** If using VR Room: ***

const CustomWalls = 0 	'set to 0 for Modern Minimal Walls, floor, and roof, 1 for Sixtoe's original walls and floor
const WallClock = 1	  	'1 Shows the clock in the VR room only   
const topper = 1		'0 = Off 1= On - Topper visible in VR Room only
const poster = 1		'1 Shows the flyer posters in the VR room only 
const poster2 = 1		'1 Shows the flyer posters in the VR room only 

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's
Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

'----- General Sound Options -----
Const VolumeDial = 0.8				' Recommended values should be no greater than 1.

'/////////////////////-----BallRoll Sound Amplification -----/////////////////////
Const BallRollAmpFactor = 1 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)


'-----RampRoll Sound Amplification -----/////////////////////
Const RampRollAmpFactor = 2 ' 0 = no amplification, 1 = 2.5db amplification, 2 = 5db amplification, 3 = 7.5db amplification, 4 = 9db amplification (aka: Tie Fighter)

'----- Phsyics Mods -----
Const Rubberizer = 1		     	'0 - disable, 1 - rothbauerw version, 2 - iaakki version
Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled


'**********************************************************
'********       OPTIONS     *******************************
'**********************************************************
 
Dim CardColor: CardColor=1				'*********** set to 1 for black apron cards, 0 for white (stock)
Dim cactiwiggle: cactiwiggle=2			'***********	factor for how much cacti on bumpers wiggle when bumper is hit, larger number more wiggle

Dim ROMSounds: ROMSounds=1				'*********** set to 0 for no rom sounds, 1 to play rom sounds.. mostly used for testing



'*******************************************
' LUT setup and options
'*******************************************

Dim LUTset, DisableLUTSelector, LutToggleSound, LutToggleSoundLevel

LutToggleSound = True
LutToggleSoundLevel = .1
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

LoadLUT
'LUTset = 17			' Override saved LUT for debug
SetLUT
ShowLUT_Init


'************************************************
'************************************************

'*******************************************
'  Constants and Global Variables
'*******************************************

Const UsingROM = True				'The UsingROM flag is to indicate code that requires ROM usage. 

Dim Ballsize,BallMass
BallSize = 50
BallMass = 1

Const tnob = 2						'Total number of balls
Const lob = 0						'Locked balls
Const cGameName = "cactjack" 		'Cactus Jacks

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim BIPL : BIPL = False				'Ball in plunger lane
Dim CJBall1, CJBall2, gBOT

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 0
Const UseGI = 0
Const HandleMech = 0


Dim LAnim, MAnim, RAnim
Dim dtsh16on, dtsh17on, dtsh26on, dtsh27on, dtsh36on, dtsh37on, dtsh46on, dtsh47on, stsh1on

'************
' Table init.
'************

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Cactus Jack's" & vbNewLine & "VP10 table by bodydump"
        .Games(cGameName).Settings.Value("sound") = 1: 'ensure the sound is on
        .Games(cGameName).Settings.Value("rol") = 0
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
		.Games(cGameName).Settings.Value("sound") = ROMSounds
        .Hidden = 1
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With


    ' Nudging
    vpmNudge.TiltSwitch = -7 ' this was set at 149, but it's 151 in all the other gottlieb tables of this era.  But GTS3.vpb shows the switch is "-7"
    vpmNudge.Sensitivity = 6 '1 
    vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingShot, RightSlingShot)



	'************  Trough	**************
	Set CJBall1 = BallRelease.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set CJBall2 = Slot.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(CJBall1,CJBall2)

	Controller.Switch(44) = 1

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1


    if CardColor=1 then
        instcard.image = "instcard"
		repcard.image = "repcard"
       else
        instcard.image = "instcardw"
		repcard.image = "repcardw"
    end if

If VR_Room = 1 Then
    SetBackglass
End If

PinCab_Backglass.blenddisablelighting = 0.5

' Drop Target and Stand Up Target initialized to off
	dtsh16on = 0
	dtsh26on = 0
	dtsh36on = 0
	dtsh46on = 0
	dtsh17on = 0
	dtsh27on = 0
	dtsh37on = 0
	dtsh47on = 0
	stsh1on = 0

End Sub

Sub Table1_Exit()
	SaveLUT
	Controller.Pause = False
	Controller.Stop
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
	DTShadowUpdate					'update drop target shadows
	BGLamps
	LampTimer

	If VR_Room = 1 Then
		DisplayTimer
	End If

	If VR_Room = 0 AND cab_mode = 0 Then
		DisplayTimerDT
	End If

	Fcacti1a.state=Fcacti1.state
	Fcacti2a.state=Fcacti2.state
	Fcacti3a.state=Fcacti3.state

	if l46.state=1 then
		P_Cactus1.image="CactusBumperMap_On3"
	  else
		P_Cactus1.image="CactusBumperMap"
	end if

	if l44.state=1 then
		P_Cactus2.image="CactusBumperMap_On3"
	  else
		P_Cactus2.image="CactusBumperMap"
	end if

	if l45.state=1 then
		P_Cactus3.image="CactusBumperMap_On3"
	  else
		P_Cactus3.image="CactusBumperMap"
	end if

End Sub


Sub TimerVRPlunger_Timer
  If VR_Primary_plunger.Y < 0 then
       VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerVRPlunger1_Timer
  VR_Primary_plunger.Y = -76 + (5* Plunger.Position) -20
End Sub


'**********************************************
'*******	Set Up Backglass Flashers	*******
'**********************************************
' this is for lining up the backglass flashers on top of a backglass image

Sub SetBackglass()
	Dim obj

	For Each obj In BackglassLow
		obj.x = obj.x
		obj.height = - obj.y + -20
		obj.y = -142 'adjusts the distance from the backglass towards the user
	Next


	For Each obj In BackglassMid
		obj.x = obj.x
		obj.height = - obj.y + 10
		obj.y = -172 'adjusts the distance from the backglass towards the user
	Next


	For Each obj In BackglassHigh
		obj.x = obj.x
		obj.height = - obj.y + 40
		obj.y = -190 'adjusts the distance from the backglass towards the user
	Next

End Sub

'**********************************************


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
	LFlip.RotY = LeftFlipper.CurrentAngle-90
	RFlip.RotY = RightFlipper.CurrentAngle-90
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
End Sub




'**********
' Keys
'**********

Sub table1_KeyDown(ByVal Keycode)
    If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull
		TimerVRPlunger.Enabled = True
		TimerVRPlunger1.Enabled = False
		VR_Primary_plunger.Y = -76
	End If

	if keycode = StartGameKey then 
		vpmTimer.PulseSw 3
		Primary_start_button.y= 1130.331 - 5
	End If

	If keycode = LeftFlipperKey  Then 
		FlipperActivate LeftFlipper, LFPress
		Controller.Switch(4) = 1
		Controller.Switch(6) = 1
		VR_FB_Left.X = VR_FB_Left.X +10
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		Controller.Switch(5) = 1
		Controller.Switch(7) = 1
		VR_FB_Right.X = VR_FB_Right.X - 10
	End If

	'If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then 'Use this for ROM based games
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft  	'was 90, 5
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight	'was 270, 6
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter	'was 0, 3

	If keycode = StartGameKey Then 
		SoundStartButton
	End If

	If keycode = RightMagnaSave Then 
		if DisableLUTSelector = 0 then
			If LutToggleSound Then
				Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
			End If
            LUTSet = LUTSet  + 1
			if LutSet > 17 then LUTSet = 0
			SetLUT
			ShowLUT
		end if
	end if

	If keycode = LeftMagnaSave Then
		if DisableLUTSelector = 0 then
			If LutToggleSound Then
				Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
			End If
			LUTSet = LUTSet - 1
			if LutSet < 0 then LUTSet = 17
			SetLUT
			ShowLUT
		end if
	end if

    If KeyDownHandler(keycode) Then Exit Sub

End Sub

Sub table1_KeyUp(ByVal Keycode)

    If keycode = PlungerKey Then 
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
		TimerVRPlunger.Enabled = False
		TimerVRPlunger1.Enabled = True
		VR_Primary_plunger.Y = -76
	end if

	If keycode = LeftFlipperKey  Then
		FlipperDeActivate LeftFlipper, LFPress
		Controller.Switch(4) = 0
		Controller.Switch(6) = 0
		VR_FB_Left.X = VR_FB_Left.X -10
	End If
	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		Controller.Switch(5) = 0
		Controller.Switch(7) = 0
		VR_FB_Right.X = VR_FB_Right.X +10
	End If

	if keycode = StartGameKey then 
		Primary_start_button.y= 1130.331
	End If

    If KeyUpHandler(keycode) Then Exit Sub

End Sub

'***********
' Solenoids
' the commented solenoids are not in used in this script
'***********

'SolCallback(1)		= 			    				      	' Left Bumper
'SolCallback(2)		=      									' Right Bumper
'SolCallback(3)		=      									' Bottom Bumper
'SolCallback(4)		= 							 			' Left Sling
'SolCallback(5)    	=										' Right Sling
SolCallback(6)   = "VUKEject"  				  				' Upper VUK
SolCallback(7)   = "SubwayEject" 							' Lower Upkick Hole
SolCallback(8)	 = "dropresetu"								' Reset Upper Drop Bank
SolCallback(9)	 = "dropresetl"								' Reset Lower Drop Bank
SolCallback(10)	 = "LDance"									' Cactus Anim #1
SolCallback(11)	 = "MDance" 								' Cactus Anim #2
SolCallback(12)	 = "RDance" 								' Cactus Anim #3
SolCallBack(13)	 = "MelonFlasher" 							' Watermelon (Center of Banjo)
SolCallBack(14)  = "Runway1Flasher"							' "Runway Flasher #1
SolCallBack(15)  = "Runway2Flasher"							' "Runway Flasher #2
SolCallBack(16)  = "Runway3Flasher"							' "Runway Flasher #3
SolCallBack(17)  = "Ramp1Flasher"							' "Ramp Flasher #1
SolCallBack(18)  = "Ramp2Flasher"							' "Ramp Flasher #2
SolCallBack(19)  = "Ramp3Flasher"							' "Ramp Flasher #3
SolCallback(20)  = "Sol20"									' Cactus Flash #1 
SolCallback(21)  = "Sol21"									' Cactus Flash #2 / "Jacks" (Backbox Flasher?)
SolCallback(22)  = "Sol22" 									' Cactus Flash #3 / "Jacks" (Backbox Flasher?)
'SolCallback(23) = "" 										' "Cactus" (2) (Backbox Flasher?)
SolCallback(24)  = "Sol24" 									' "Cactus" (2) (Another Backbox Flasher?)
SolCallBack(25)	 = "ThrowFlasher" 							' "Throw" Lights
SolCallback(26)	 = "GICircuit"								' General Illumination
'SolCallback(27) = ""				 						' Ticket / Coin Meter Relay
SolCallback(28)	 = "SolRelease" 							' Ball Release
SolCallback(29)	 = "SolOuthole"								' Outhole
SolCallback(30)  = "SolKnocker"								' Knocker
'SolCallback(31) = "TiltRelay"					  			' Tilt Relay
SolCallback(32)  = "vpmNudge.SolGameOn" 			 		' Game Over Relay (Game On)
'*************


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"


Sub MelonFlasher(enabled)
	If Enabled then
		SetLamp 120, enabled
	else
		SetLamp 120, 0
	End If
End Sub
Sub Runway1Flasher(enabled)
	If Enabled then
		SetLamp 122, enabled
	else
		SetLamp 122, 0
	End If
End Sub
Sub Runway2Flasher(enabled)
	If Enabled then
		SetLamp 123, enabled
	else
		SetLamp 123, 0
	End If
End Sub
Sub Runway3Flasher(enabled)
	If Enabled then
		SetLamp 124, enabled
	else
		SetLamp 124, 0
	End If
End Sub
Sub Ramp1Flasher(enabled)
	If Enabled then
		SetLamp 125, enabled
	else
		SetLamp 125, 0
	End If
End Sub
Sub Ramp2Flasher(enabled)
	If Enabled then
		SetLamp 126, enabled
	else
		SetLamp 126, 0
	End If
End Sub
Sub Ramp3Flasher(enabled)
	If Enabled then
		SetLamp 127, enabled
	else
		SetLamp 127, 0
	End If
End Sub
Sub ThrowFlasher(enabled)
	If Enabled then
		SetLamp 121, enabled
	else
		SetLamp 121, 0
	End If
End Sub

'****************************************************************
'  GI
'****************************************************************
dim gilvl:gilvl = 1
Sub GICircuit (enabled)
	If enabled Then
		gilvl = 0
		GIoff
	Else
		gilvl = 1
		stsh1on = 1 ' Standup Target Shadow On 
		GIon
	End If
	Sound_GI_Relay enabled, Relay_GI
	Sound_Flash_Relay enabled, Relay_GI_BG
End Sub

Sub MDance (enabled)
	If Enabled Then
		SetLamp 129, enabled
		Fcacti2.state=1
		cactus.image="CactusBLK-hatMap_FLASH3"
		cactusa.image="CactusBLK-hatMap_FLASH3"
		cactusb.image="CactusBLK-hatMap_FLASH3"
		Dim mmax,mmin
		mmax=6
		mmin=1
		MAnim = Int((mmax-mmin+1)*Rnd+mmin)
		Select Case MAnim
			Case 1:Cactus.Visible = 1:Cactusa.Visible = 0:Cactusb.Visible = 0:cactus.PlayAnim 0, 0.25
			Case 2:Cactus.Visible = 0:Cactusa.Visible = 1:Cactusb.Visible = 0:cactusa.PlayAnim 0, 0.25
			Case 3:Cactus.Visible = 0:Cactusa.Visible = 0:Cactusb.Visible = 1:cactusb.PlayAnim 0, 0.25
			Case 4:Cactus.Visible = 1:Cactusa.Visible = 0:Cactusb.Visible = 0:cactus.PlayAnim 0, 0.15
			Case 5:Cactus.Visible = 0:Cactusa.Visible = 1:Cactusb.Visible = 0:cactusa.PlayAnim 0, 0.15
			Case 6:Cactus.Visible = 0:Cactusa.Visible = 0:Cactusb.Visible = 1:cactusb.PlayAnim 0, 0.15
		End Select
	Else	
		SetLamp 129, 0
		Fcacti2.state=0
		If GI20.state=1 then
			cactus.image="CactusBLK-hatMap_ON3"
			cactusa.image="CactusBLK-hatMap_ON3"
			cactusb.image="CactusBLK-hatMap_ON3"
		Else
			cactus.image="CactusBLK-hatMap_ON0"
			cactusa.image="CactusBLK-hatMap_ON0"
			cactusb.image="CactusBLK-hatMap_ON0"
		End If
	End If
End Sub
			

Sub RDance (enabled)
	If Enabled Then
		cactus2.image="CactusCompleteMapright_FLASH3"
		cactus2a.image="CactusCompleteMapright_FLASH3"
		cactus2b.image="CactusCompleteMapright_FLASH3"
		SetLamp 130, enabled
		Fcacti3.state=1
		Dim rmax,rmin
		rmax=6
		rmin=1
		RAnim = Int((rmax-rmin+1)*Rnd+rmin)
		Select Case RAnim
			Case 1:Cactus2.Visible = 1:Cactus2a.Visible = 0:Cactus2b.Visible = 0:cactus2.PlayAnim 0, 0.25
			Case 2:Cactus2.Visible = 0:Cactus2a.Visible = 1:Cactus2b.Visible = 0:cactus2a.PlayAnim 0, 0.25
			Case 3:Cactus2.Visible = 0:Cactus2a.Visible = 0:Cactus2b.Visible = 1:cactus2b.PlayAnim 0, 0.25
			Case 4:Cactus2.Visible = 1:Cactus2a.Visible = 0:Cactus2b.Visible = 0:cactus2.PlayAnim 0, 0.15
			Case 5:Cactus2.Visible = 0:Cactus2a.Visible = 1:Cactus2b.Visible = 0:cactus2a.PlayAnim 0, 0.15
			Case 6:Cactus2.Visible = 0:Cactus2a.Visible = 0:Cactus2b.Visible = 1:cactus2b.PlayAnim 0, 0.15
		End Select
	Else
		SetLamp 130, 0
		Fcacti3.state=0
		If GI20.state=1 Then
			cactus2.image="CactusCompleteMapright_ON3"
			cactus2a.image="CactusCompleteMapright_ON3"
			cactus2b.image="CactusCompleteMapright_ON3"
		Else
			cactus2.image="CactusCompleteMapright_ON0"
			cactus2a.image="CactusCompleteMapright_ON0"
			cactus2b.image="CactusCompleteMapright_ON0"
		End If
	End If
End Sub

Sub LDance (enabled)
	If Enabled Then
		cactus1.image="CactusCompleteMapleft_FLASH3"
		cactus1a.image="CactusCompleteMapleft_FLASH3"
		cactus1b.image="CactusCompleteMapleft_FLASH3"
		SetLamp 131, enabled
		Fcacti1.state=1
		Dim lmax,lmin
		lmax=6
		lmin=1
		LAnim = Int((lmax-lmin+1)*Rnd+lmin)
		Select Case LAnim
			Case 1:Cactus1.Visible = 1:Cactus1a.Visible = 0:Cactus1b.Visible = 0:cactus1.PlayAnim 0, 0.25
			Case 2:Cactus1.Visible = 0:Cactus1a.Visible = 1:Cactus1b.Visible = 0:cactus1a.PlayAnim 0, 0.25
			Case 3:Cactus1.Visible = 0:Cactus1a.Visible = 0:Cactus1b.Visible = 1:cactus1b.PlayAnim 0, 0.25
			Case 4:Cactus1.Visible = 1:Cactus1a.Visible = 0:Cactus1b.Visible = 0:cactus1.PlayAnim 0, 0.15
			Case 5:Cactus1.Visible = 0:Cactus1a.Visible = 1:Cactus1b.Visible = 0:cactus1a.PlayAnim 0, 0.15
			Case 6:Cactus1.Visible = 0:Cactus1a.Visible = 0:Cactus1b.Visible = 1:cactus1b.PlayAnim 0, 0.15
		End Select
	Else	
		Fcacti1.state=0
		SetLamp 131, 0
		If GI20.state=1 Then
			cactus1.image="CactusCompleteMapleft_ON3"
			cactus1a.image="CactusCompleteMapleft_ON3"
			cactus1b.image="CactusCompleteMapleft_ON3"
		Else
			cactus1.image="CactusCompleteMapleft_ON0"
			cactus1a.image="CactusCompleteMapleft_ON0"
			cactus1b.image="CactusCompleteMapleft_ON0"
		End If
	End If
End Sub

		
'****************************************************************
'  Slingshots
'****************************************************************

' RStep and LStep are the variables that increment the animation
Dim RStep, LStep

Sub RightSlingShot_Slingshot
    vpmtimer.PulseSw(14)
    RSling.Visible = 0
    RSling1.Visible = 1
    slingR.objroty = -15
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	RandomSoundSlingshotRight slingR
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:slingR.objroty = -7
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:slingR.objroty = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    vpmtimer.pulsesw(13)
    LSling.Visible = 0
    LSling1.Visible = 1
    slingL.objroty = 15
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	RandomSoundSlingshotLeft slingL
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:slingL.objroty = 7
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:slingL.objroty=0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub




'***************
'   Bumpers
'***************


Sub Bumper1_Hit
	vpmTimer.PulseSw 12
	RandomSoundBumperBottom Bumper1
  if Wobbleon = 1 Then
	cBall1.velx = cactiwiggle * (2*RND(1)-1)
    cBall1.vely = cactiwiggle * (2*RND(1)-1)
  End If
End Sub

Sub Bumper2_Hit
	vpmTimer.PulseSw 10
	RandomSoundBumperMiddle Bumper2
  If Wobbleon = 1 Then
	cBall2.velx = cactiwiggle * (2*RND(1)-1)
    cBall2.vely = cactiwiggle * (2*RND(1)-1)
  End If
End Sub

Sub Bumper3_Hit
	vpmTimer.PulseSw 11
	RandomSoundBumperTop Bumper3
  If Wobbleon = 1 Then
	cBall3.velx = cactiwiggle * (2*RND(1)-1)
    cBall3.vely = cactiwiggle * (2*RND(1)-1)
  End If
End Sub

'*********************
' Switches & Rollovers
'*********************

Sub sw15_Hit:Controller.Switch(15) = 1:End Sub
Sub sw15_unHit:Controller.Switch(15) = 0:End Sub

Sub sw23_Hit:Controller.Switch(23) = 1:End Sub
Sub sw23_unHit:Controller.Switch(23) = 0:End Sub

Sub sw24_Hit:Controller.Switch(24) = 1:End Sub
Sub sw24_unHit:Controller.Switch(24) = 0:End Sub

Sub sw25_Hit:Controller.Switch(25) = 1:End Sub
Sub sw25_unHit:Controller.Switch(25) = 0:End Sub

Sub sw31_Hit:Controller.Switch(31) = 1:End Sub
Sub sw31_unHit:Controller.Switch(31) = 0:End Sub

Sub sw33_Hit:Controller.Switch(33) = 1:End Sub
Sub sw33_unHit:Controller.Switch(33) = 0:End Sub

Sub sw34_Hit:Controller.Switch(34) = 1:End Sub
Sub sw34_unHit:Controller.Switch(34) = 0:End Sub

Sub sw35_Hit:Controller.Switch(35) = 1:End Sub
Sub sw35_unHit:Controller.Switch(35) = 0:End Sub

Sub sw42_Hit:Controller.Switch(42) = 1:End Sub
Sub sw42_unHit:Controller.Switch(42) = 0:End Sub

Sub sw45_Hit:Controller.Switch(45) = 1:End Sub
Sub sw45_unHit:Controller.Switch(45) = 0:End Sub

Sub sw50_Hit:Controller.Switch(50) = 1:End Sub
Sub sw50_unHit:Controller.Switch(50) = 0:End Sub

Sub sw51_Hit:Controller.Switch(51) = 1:End Sub
Sub sw51_unHit:Controller.Switch(51) = 0:End Sub


'******************************************************
' TROUGH And Drain
'******************************************************

Sub Slot_Hit():Controller.Switch(44) = 1:UpdateTrough:End Sub
Sub Slot_UnHit():Controller.Switch(44) = 0:UpdateTrough:End Sub
Sub BallRelease_UnHit():UpdateTrough:End Sub

Sub UpdateTrough()
	BallRelease.TimerInterval = 300
	BallRelease.TimerEnabled = 1
End Sub

Sub BallRelease_Timer()
	Me.TimerEnabled = 0
	If BallRelease.BallCntOver = 0 Then Slot.kick 60, 8
End Sub

Sub Drain_Hit
	RandomSoundDrain Drain
	UpdateTrough
	Controller.Switch(43) = 1
End Sub

Sub Drain_UnHit()
	Controller.Switch(43) = 0
End Sub

Sub SolOuthole(enabled)
	If enabled Then 
		Drain.kick 60,18
		SoundSaucerKick 1, Drain
	End If
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		RandomSoundBallRelease BallRelease
		BallRelease.kick 60, 12
		UpdateTrough
	End If
End Sub

'****************************
' Vuks & saucers
'****************************

Sub UpperHoleTrigger_Hit()
	RandomSoundBallBouncePlayfieldHard activeball
	vpmTimer.PulseSw 32
End Sub

Sub LowerHole_Hit()
	SoundSaucerLock
	controller.Switch(22)=1
End Sub

Sub UpperVUK_Hit
	SoundSaucerLock
	controller.Switch(21)=1
End Sub

Sub VUKEject (Enabled)
  if enabled Then
	SoundSaucerKick 1, UpperVUK
	UpperVUK.kick 0, 30, 1.55 '1.56
	controller.switch(21) = 0
  end if
End sub

Sub SubwayEject (Enabled)
  if enabled Then
	SoundSaucerKick 1, LowerHole
	LowerHole.kick 0, 65, 1.56  	' was 45, 1.56
	controller.switch(22) = 0
  end if
End sub


'***************
'  Standup Targets
'***************

Sub sw20_Hit
	STHit 20
End Sub

Sub sw20o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw30_Hit
	STHit 30
End Sub

Sub sw30o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw40_Hit
	STHit 40
End Sub

Sub sw40o_Hit
	TargetBouncer Activeball, 1
End Sub

Sub sw41_Hit
 	STHit 41
End Sub

Sub sw41o_Hit
	TargetBouncer Activeball, 1
End Sub


'***************
' Droptargets
'***************

Sub sw16_Hit
	DTHit 16
	TargetBouncer Activeball, 1
	dtsh16on = 0
End Sub

Sub sw26_Hit
	DTHit 26
	TargetBouncer Activeball, 1
	dtsh26on = 0
End Sub

Sub sw36_Hit
	DTHit 36
	TargetBouncer Activeball, 1
	dtsh36on = 0
End Sub

Sub sw46_Hit
	DTHit 46
	TargetBouncer Activeball, 1
	dtsh46on = 0
End Sub

Sub sw17_Hit
	DTHit 17
	TargetBouncer Activeball, 1
	dtsh17on = 0
End Sub

Sub sw27_Hit
	DTHit 27
	TargetBouncer Activeball, 1
	dtsh27on = 0
End Sub

Sub sw37_Hit
	DTHit 37
	TargetBouncer Activeball, 1
	dtsh37on = 0
End Sub

Sub sw47_Hit
	DTHit 47
	TargetBouncer Activeball, 1
	dtsh47on = 0
End Sub

' Bank of drop targets to be reset at once

Sub dropresetu(enabled)
	dim xx
	if enabled then
		RandomSoundDropTargetReset sw17p
		DTRaise 17
		DTRaise 27
		DTRaise 37
		DTRaise 47
		dtsh17on = 1
		dtsh27on = 1
		dtsh37on = 1
		dtsh47on = 1
	end if
End Sub

Sub dropresetl(enabled)
	dim xx
	if enabled then
		RandomSoundDropTargetReset sw16p
		DTRaise 16
		DTRaise 26
		DTRaise 36
		DTRaise 46
		dtsh16on = 1
		dtsh26on = 1
		dtsh36on = 1
		dtsh46on = 1
	end if
End Sub


'***************
' Knocker
'***************
Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub

'***************
'*******Ramp sounds
'***************

Sub VUKstart_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub VUKend_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub VUKrampstart_Hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub VUKrampend_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub rightrampstart_Hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub rightrampstart1_Hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub rightrampend_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub leftrampstart_Hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub rampstart_Hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub rampend_Hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
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
Dim ChangeBGLamps(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
'LampTimer.Interval = 10 'lamp fading speed
'LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
            ChangeBGLamps(chgLamp(ii, 0)) = chgLamp(ii, 1)
        Next
    End If
    UpdateLamps

If VR_Room = 1 Then
	Update_BGFlashers
End If

End Sub


' **********************************
' ***** Backglass Lights Setup *****
' **********************************


Sub Update_BGFlashers


' backdrop lights
	NumLamp 113, FlBG113 'egg
	NumLamp 114, FlBG114 'banana
	NumLamp 115, FlBG115 'tomato
	NumLamp 116, FlBG116 'pumpkin
	NumLamp 117, FlBG117 'watermelon

End Sub


Sub NumLamp(nr, object)
    Select Case ChangeBGLamps(nr)
        Case 0:object.visible = 0:ChangeBGLamps(nr) = 0
        Case 1:object.visible = 1:ChangeBGLamps(nr) = 1
    End Select
End Sub

'**********************************************


Sub UpdateLamps
	NFadeL 0, l0
    NFadeL 1, l1
    NFadeL 2, l2
    NFadeL 3, l3
    NFadeL 4, l4
    NFadeL 5, l5
    NFadeL 6, l6
    NFadeL 7, l7
    NFadeL 10, l10
    NFadeL 11, l11
    NFadeL 12, l12
    NFadeL 13, l13
    NFadeL 14, l14
    NFadeL 15, l15
    NFadeL 16, l16
	NFadeL 17, l17
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
	NFadeLm 44, l44a
    NFadeL 44, l44
	NFadeLm 45, l45a
    NFadeL 45, l45
	NFadeLm 46, l46a
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
 '   NFadeL 76, l76
 '   NFadeL 77, l77
    NFadeL 80, l80
    NFadeL 81, l81
    NFadeL 82, l82
    NFadeL 83, l83
    NFadeL 84, l84
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
    NFadeL 110, l110
    NFadeL 112, l112

If VR_Room = 0 AND cab_mode = 0 Then
    NFadeL 113, L113
    NFadeL 114, L114
    NFadeL 115, L115
    NFadeL 116, L116
    NFadeL 117, L117
End If

	NFadeL 120, lmelon
	NFadeLm 121, lthrow6
	NFadeLm 121, lthrow5
	NFadeLm 121, lthrow4
	NFadeLm 121, lthrow3
	NFadeLm 121, lthrow2
	NFadeLm 121, lthrow1
	NFadeL 121, lthrow
    NFadeL 122, lrunway1
    NFadeL 123, lrunway2
    NFadeL 124, lrunway3
    NFadeL 125, lramp1
    NFadeL 126, lramp2
    NFadeL 127, lramp3
	Flash 128, gion_flash
	Flashm 128, FlasherGI
	SetLamp 132, 1-LampState(128)
	Flashm 129, FlasherM
	Flash 129, FlasherM2
	Flash 131, FlasherL
	Flashm 131, FlasherL2
	Flash 130, FlasherR
	Flashm 130, FlasherR2
	Flash 132, gioff_flash


    'Flashers

End Sub

' div lamp subs

Sub InitLamps()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
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
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
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

' Desktop Objects: Reels & texts (you may also use lights on the desktop)


Sub FadeR(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.SetValue 1:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.SetValue 0:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1              'wait
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


' #####################################
' ***** Backglass Flasher Objects *****
' #####################################


Sub Sol20(Enabled)
	If Enabled Then
		FlBG20.visible = 1 
		Sound_Flash_Relay enabled, Relay_GI_BG1
	Else
		FlBG20.visible = 0
	End If 
End Sub

Sub Sol21(Enabled)
	If Enabled Then
		FlBG21.visible = 1 
		Sound_Flash_Relay enabled, Relay_GI_BG2
	Else
		FlBG21.visible = 0
	End If 
End Sub

Sub Sol22(Enabled)
	If Enabled Then
		FlBG22.visible = 1 
		Sound_Flash_Relay enabled, Relay_GI_BG3
	Else
		FlBG22.visible = 0
	End If 
End Sub

Sub Sol24(Enabled)
	If Enabled Then
		FlBG24.visible = 1 
		Sound_Flash_Relay enabled, Relay_GI_BG4
	Else
		FlBG24.visible = 0
	End If 
End Sub

Sub BGLamps
	If gilvl = 1 Then
		FlBG1.visible = 1 
		FlBG2.visible = 1 
		FlBG3.visible = 1 
		FlBG4.visible = 1 
		FlBG5.visible = 1 
		FlBG6.visible = 1 
		FlBG7.visible = 1 
		FlBG8.visible = 1 
		FlBG9.visible = 1 
		FlBG10.visible = 1 
		FlBG11.visible = 1 

	Else
		FlBG1.visible = 0
		FlBG2.visible = 0
		FlBG3.visible = 0 
		FlBG4.visible = 0
		FlBG5.visible = 0
		FlBG6.visible = 0 
		FlBG7.visible = 0 
		FlBG8.visible = 0 
		FlBG9.visible = 0 
		FlBG10.visible = 0 
		FlBG11.visible = 0
	End If 
End Sub

' *************************************

'*****GI Lights On
dim xx
Sub GIon()
	For each xx in GI:xx.State = 1: Next
			cactus.image="CactusBLK-hatMap_ON3"
			cactusa.image="CactusBLK-hatMap_ON3"
			cactusb.image="CactusBLK-hatMap_ON3"
			cactus1.image="CactusCompleteMapleft_ON3"
			cactus1a.image="CactusCompleteMapleft_ON3"
			cactus1b.image="CactusCompleteMapleft_ON3"
			cactus2.image="CactusCompleteMapright_ON3"
			cactus2a.image="CactusCompleteMapright_ON3"
			cactus2b.image="CactusCompleteMapright_ON3"
	SetLamp 128, 1
	outers.image="outers"
End Sub

Sub GIoff()
	For each xx in GI:xx.State = 0: Next
			cactus.image="CactusBLK-hatMap_ON0"
			cactusa.image="CactusBLK-hatMap_ON0"
			cactusb.image="CactusBLK-hatMap_ON0"
			cactus1.image="CactusCompleteMapleft_ON0"
			cactus1a.image="CactusCompleteMapleft_ON0"
			cactus1b.image="CactusCompleteMapleft_ON0"
			cactus2.image="CactusCompleteMapright_ON0"
			cactus2a.image="CactusCompleteMapright_ON0"
			cactus2b.image="CactusCompleteMapright_ON0"
	SetLamp 128, 0
	outers.image="outersoff"
End Sub


'*********************************************************
'					bumper cacti shake
'*********************************************************

if Wobbleon <> 1 Then
	aWobbleTimer.enabled = 0
End If

Dim cBall3, cBall1, cBall2, pMod, rmmod


Dim mMagnet, mMagnet2, mMagnet3

If wobbleon = 1 Then

 Set mMagnet = new cvpmMagnet
 With mMagnet
	.InitMagnet WobbleMagnet, 5
	.Size = 100
	.MagnetOn = True
 End With

Set mMagnet2 = new cvpmMagnet
 With mMagnet
	.InitMagnet WobbleMagnet2, 5
	.Size = 100
	.MagnetOn = True
 End With

Set mMagnet3 = new cvpmMagnet
 With mMagnet
	.InitMagnet WobbleMagnet3, 5
	.Size = 100
	.MagnetOn = True
 End With

 WobbleInit

'Includes stripped down version of my reverse slope scripting for a single ball
 Dim ngrav, ngravmod, slopemod, nslope	',pslope

Sub WobbleInit
	nslope = Table1.SlopeMin +((Table1.SlopeMax - Table1.SlopeMin) * Table1.GlobalDifficulty)
									'	nslope = pslope
'Pcup1.rotx=nslope
'Pcup2.rotx=nslope
'Pcup3.rotx=nslope

	slopemod = 2*nslope			'pslope + nslope
	ngravmod = 60/aWobbleTimer.interval
	ngrav = slopemod * .0905 * Table1.Gravity / ngravmod
	pMod = .15					'percentage of hit power transfered to captive wobble ball
	Set cBall3 = cKicker3.createball:cball3.image = "blank":cKicker3.Kick 0,0	':mMagnet3.addball cball3
	Set cBall1 = cKicker1.createball:cball1.image = "blank":cKicker1.Kick 0,0	':mMagnet1.addball cball1
	Set cBall2 = cKicker2.createball:cball2.image = "blank":ckicker2.Kick 0,0	':mMagnet2.addball cball2
	aWobbleTimer.enabled = 1
 End Sub


 Sub aWobbleTimer_Timer
	cBall3.Vely = cBall3.VelY-ngrav					'modifier for slope reversal/cancellation
	cBall1.Vely = cBall1.VelY-ngrav
	cBall2.Vely = cBall2.VelY-ngrav
	P_cactus1.objrotx = (ckicker1.y - cball1.y)'*rmmod
	P_cactus1.objroty = (cball1.x - ckicker1.x)'*rmmod
	P_cactus2.objrotx = -(cKicker2.y - cBall2.y)'*rmmod
	P_cactus2.objroty = -(cBall2.x - cKicker2.x)'*rmmod
	P_cactus3.objrotx = -(cKicker3.y - cBall3.y)'*rmmod
	P_cactus3.objroty = -(cBall3.x - cKicker3.x)'*rmmod
 End Sub

End If

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************


' *** Required Functions, enable these if they are not already present elswhere in your table
Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function


'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","")
currentShadowCount = Array (0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(3), objrtx2(3)
dim objBallShadow(3)
Dim BallShadowA




BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2)

DynamicBSInit



sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		iii = iii + 1
	Next
	numberofsources = iii
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, iii


'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = gBOT(s).X
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = gBOT(s).X
				BallShadowA(s).Y = gBOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=gBOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 Then 'And gBOT(s).Y < (TableHeight - 200) Then 'Or gBOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((gBOT(s).x-DSSources(iii)(0)),(gBOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y + fovY
	'						objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), gBOT(s).X, gBOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							AnotherSource = sourcenames(s)
							objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y + fovY
	'						objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), gBOT(s).X, gBOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((gBOT(s).x-DSSources(AnotherSource)(0)),(gBOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + fovY
	'						objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), gBOT(s).X, gBOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
							End If
						end if
					Else
						currentShadowCount(s) = 0
						BallShadowA(s).Opacity = 100*AmbientBSFactor
					End If
				Next
			Else									'Hide dynamic shadows everywhere else
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
    elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub




'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
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

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

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
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************

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
		debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(gBOT(b).x, gBOT(b).y, Flipper1) Then
					Debug.Print "ball in flip1. exit"
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
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, Rubberizer
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
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0000001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm, ver) 'Rubberizer is handle here
		If ver = 1 Then
			dim RealCOR, DesiredCOR, str, coef
			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
			End If
		Elseif ver = 2 Then
			If parm < 10 And parm > 2 And Abs(aball.angmomz) < 15 And aball.vely < 0 then
				aball.angmomz = aball.angmomz * 1.2
				aball.vely = aball.vely * (1.1 + (parm/50))
			Elseif parm <= 2 and parm > 0.2 And aball.vely < 0 Then
				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        	aball.angmomz = aball.angmomz * -0.7
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				aball.vely = aball.vely * (1.2 + (parm/10))
			End if
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

		for each b in allBalls
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allBalls
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
' This solution improves the physics for drop targets to create more realistic behavior. It allows the ball 
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full drop target animation, including deflection on hit and a slight lift when the drop 
' targets raise, switch handling, bricking, and popping the ball up if it's over the drop target when it raises.
'
' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and   
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target. 
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield 
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded 
' target mesh in this table's example. It uses the same texture map as the VP drop targets.


'******************************************************
'  DROP TARGETS INITIALIZATION
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

'Define a variable for each drop target



Dim DT16, DT26, DT36, DT46, DT17, DT27, DT37, DT47



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




Set DT16 = (new DropTarget)(sw16, sw16a, sw16p, 16, 0, false)
Set DT26 = (new DropTarget)(sw26, sw26a, sw26p, 26, 0, false)
Set DT36 = (new DropTarget)(sw36, sw36a, sw36p, 36, 0, false)
Set DT46 = (new DropTarget)(sw46, sw46a, sw46p, 46, 0, false)
Set DT17 = (new DropTarget)(sw17, sw17a, sw17p, 17, 0, false)
Set DT27 = (new DropTarget)(sw27, sw27a, sw27p, 27, 0, false)
Set DT37 = (new DropTarget)(sw37, sw37a, sw37p, 37, 0, false)
Set DT47 = (new DropTarget)(sw47, sw47a, sw47p, 47, 0, false)


Dim DTArray
DTArray = Array(DT16, DT26, DT36, DT46, DT17, DT27, DT37, DT47)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 2 '8 'max degrees primitive rotates when hit
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
	DTArray(i).animate = DTCheckBrick(Activeball,DTArray(i).prim)
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
			controller.Switch(Switchid) = 1
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


Dim ST20, ST30, ST40, ST41


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


Set ST20 = (new StandupTarget)(sw20, psw20,20, 0)
Set ST30 = (new StandupTarget)(sw30, psw30,30, 0)
Set ST40 = (new StandupTarget)(sw40, psw40,40, 0)
Set ST41 = (new StandupTarget)(sw41, psw41,41, 0)


'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
'STAnimationArray = Array(ST20, ST30, ST40, ST41)

Dim STArray

STArray = Array(ST20, ST30, ST40, ST41)


'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
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
		vpmTimer.PulseSw switch
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

Dim ampFactor

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
	Select Case BallRollAmpFactor
		Case 0
			ampFactor = "_amp0"
		Case 1
			ampFactor = "_amp2_5"
		Case 2
			ampFactor = "_amp5"
		Case 3
			ampFactor = "_amp7_5"
		Case 4
			ampFactor = "_amp9"
		Case Else
			ampFactor = "_amp0"
	End Select
End Sub

Sub RollingUpdate()
	Dim b

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b & ampFactor)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(gBOT)
		If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b & ampFactor), -1, VolPlayfieldRoll(gBOT(b)) * 1.1 * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b & ampFactor)
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

' Code to correct corner condition where ball can get stuck behind bottom bumper near subway saucer (reused and modified from Rothbauerw from Comet)
		if Distance(gBOT(b).x, gBOT(b).y, 715, 700) < 50 Then  
            if gBOT(b).vely < 0 and ballvel(gBOT(b)) < 1 Then
                gBOT(b).angmomy = gBOT(b).angmomy * 2
                gBOT(b).angmomx = gBOT(b).angmomx * 2
                gBOT(b).velx = gBOT(b).velx + 0.1
                gBOT(b).vely = gBOT(b).vely - 0.1
            end if
        End If

		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If gBOT(b).Z > 30 Then
				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = gBOT(b).X
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

dim rampAmpFactor

InitRampRolling

Sub InitRampRolling()
	Select Case RampRollAmpFactor
		Case 0
			rampAmpFactor = "_amp0"
		Case 1
			rampAmpFactor = "_amp2_5"
		Case 2
			rampAmpFactor = "_amp5"
		Case 3
			rampAmpFactor = "_amp7_5"
		Case 4
			rampAmpFactor = "_amp9"
		Case Else
			rampAmpFactor = "_amp0"
	End Select
End Sub

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(3,2)
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
					PlaySound("RampLoop" & x & rampAmpFactor), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x & rampAmpFactor)
				Else
					StopSound("RampLoop" & x & rampAmpFactor)
					PlaySound("wireloop" & x & rampAmpFactor), -1, VolPlayfieldRoll(RampBalls(x,0)) * 1.1 * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x & rampAmpFactor)
				StopSound("wireloop" & x & rampAmpFactor)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x & rampAmpFactor)
				StopSound("wireloop" & x & rampAmpFactor)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x & rampAmpFactor)
			StopSound("wireloop" & x & rampAmpFactor)
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
	PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
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

Const RelayFlashSoundLevel = .6 '0.315								'volume level; range [0, 1];
Const RelayGISoundLevel = 1.5 '1.05									'volume level; range [0, 1];

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





'********************************************
'              Display Output
'********************************************

dim DisplayColor, DisplayColorG
DisplayColor =  RGB(0,255,30)
'DisplayColor =  RGB(255,40,1)


Sub Displaytimer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In Digits(num)
'                   If chg And 1 Then obj.visible=stat And 1    'if you use the object color for off; turn the display object visible to not visible on the playfield, and uncomment this line out.
					If chg And 1 Then FadeDisplay obj, stat And 1	
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If
End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 800
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 800
	End If
End Sub

Dim Digits(40)

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

Digits(32)=Array(D481,D482,D483,D484,D485,D486,D487,D488,D489,D490,D491,D492,D493,D494,D495)
Digits(33)=Array(D496,D497,D498,D499,D500,D501,D502,D503,D504,D505,D506,D507,D508,D509,D510)
Digits(34)=Array(D511,D512,D513,D514,D515,D516,D517,D518,D519,D520,D521,D522,D523,D524,D525)
Digits(35)=Array(D526,D527,D528,D529,D530,D531,D532,D533,D534,D535,D536,D537,D538,D539,D540)
Digits(36)=Array(D541,D542,D543,D544,D545,D546,D547,D548,D549,D550,D551,D552,D553,D554,D555)
Digits(37)=Array(D556,D557,D558,D559,D560,D561,D562,D563,D564,D565,D566,D567,D568,D569,D570)
Digits(38)=Array(D571,D572,D573,D574,D575,D576,D577,D578,D579,D580,D581,D582,D583,D584,D585)
Digits(39)=Array(D586,D587,D588,D589,D590,D591,D592,D593,D594,D595,D596,D597,D598,D599,D600)

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
 Dim DigitsDT(40)
'segment ID a,  b,  c,  d,  e,  f,  m, COM, n,  g,  h,  i,  j,  k,  l,  DP
'number ID  00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15

	DigitsDT(0)=Array(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)	
	DigitsDT(1)=Array(a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31)
	DigitsDT(2)=Array(a32, a33, a34, a35, a36, a37, a38, a39, a40, a41, a42, a43, a44, a45, a46, a47)
	DigitsDT(3)=Array(a48, a49, a50, a51, a52, a53, a54, a55, a56, a57, a58, a59, a60, a61, a62, a63)
	DigitsDT(4)=Array(a64, a65, a66, a67, a68, a69, a70, a71, a72, a73, a74, a75, a76, a77, a78, a79)
	DigitsDT(5)=Array(a80, a81, a82, a83, a84, a85, a86, a87, a88, a89, a90, a91, a92, a93, a94, a95)
	DigitsDT(6)=Array(a96, a97, a98, a99, a100, a101, a102, a103, a104, a105, a106, a107, a108, a109, a110, a111)
	DigitsDT(7)=Array(a112, a113, a114, a115, a116, a117, a118, a119, a120, a121, a122, a123, a124, a125, a126, a127)
	DigitsDT(8)=Array(a128, a129, a130, a131, a132, a133, a134, a135, a136, a137, a138, a139, a140, a141, a142, a143)
	DigitsDT(9)=Array(a144, a145, a146, a147, a148, a149, a150, a151, a152, a153, a154, a155, a156, a157, a158, a159)
	DigitsDT(10)=Array(a160, a161, a162, a163, a164, a165, a166, a167, a168, a169, a170, a171, a172, a173, a174, a175)
	DigitsDT(11)=Array(a176, a177, a178, a179, a180, a181, a182, a183, a184, a185, a186, a187, a188, a189, a190, a191)
	DigitsDT(12)=Array(a192, a193, a194, a195, a196, a197, a198, a199, a200, a201, a202, a203, a204, a205, a206, a207)
	DigitsDT(13)=Array(a208, a209, a210, a211, a212, a213, a214, a215, a216, a217, a218, a219, a220, a221, a222, a223)
	DigitsDT(14)=Array(a224, a225, a226, a227, a228, a229, a230, a231, a232, a233, a234, a235, a236, a237, a238, a239)
	DigitsDT(15)=Array(a240, a241, a242, a243, a244, a245, a246, a247, a248, a249, a250, a251, a252, a253, a254, a255)
	DigitsDT(16)=Array(a256, a257, a258, a259, a260, a261, a262, a263, a264, a265, a266, a267, a268, a269, a270, a271)
	DigitsDT(17)=Array(a272, a273, a274, a275, a276, a277, a278, a279, a280, a281, a282, a283, a284, a285, a286, a287)
	DigitsDT(18)=Array(a288, a289, a290, a291, a292, a293, a294, a295, a296, a297, a298, a299, a300, a301, a302, a303)
	DigitsDT(19)=Array(a304, a305, a306, a307, a308, a309, a310, a311, a312, a313, a314, a315, a316, a317, a318, a319)

	DigitsDT(20)=Array(b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)
	DigitsDT(21)=Array(b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, b30, b31)
	DigitsDT(22)=Array(b32, b33, b34, b35, b36, b37, b38, b39, b40, b41, b42, b43, b44, b45, b46, b47)
	DigitsDT(23)=Array(b48, b49, b50, b51, b52, b53, b54, b55, b56, b57, b58, b59, b60, b61, b62, b63)
	DigitsDT(24)=Array(b64, b65, b66, b67, b68, b69, b70, b71, b72, b73, b74, b75, b76, b77, b78, b79)
	DigitsDT(25)=Array(b80, b81, b82, b83, b84, b85, b86, b87, b88, b89, b90, b91, b92, b93, b94, b95)
	DigitsDT(26)=Array(b96, b97, b98, b99, b100, b101, b102, b103, b104, b105, b106, b107, b108, b109, b110, b111)
	DigitsDT(27)=Array(b112, b113, b114, b115, b116, b117, b118, b119, b120, b121, b122, b123, b124, b125, b126, b127)
	DigitsDT(28)=Array(b128, b129, b130, b131, b132, b133, b134, b135, b136, b137, b138, b139, b140, b141, b142, b143)
	DigitsDT(29)=Array(b144, b145, b146, b147, b148, b149, b150, b151, b152, b153, b154, b155, b156, b157, b158, b159)
	DigitsDT(30)=Array(b160, b161, b162, b163, b164, b165, b166, b167, b168, b169, b170, b171, b172, b173, b174, b175)
	DigitsDT(31)=Array(b176, b177, b178, b179, b180, b181, b182, b183, b184, b185, b186, b187, b188, b189, b190, b191)
	DigitsDT(32)=Array(b192, b193, b194, b195, b196, b197, b198, b199, b200, b201, b202, b203, b204, b205, b206, b207)
	DigitsDT(33)=Array(b208, b209, b210, b211, b212, b213, b214, b215, b216, b217, b218, b219, b220, b221, b222, b223)
	DigitsDT(34)=Array(b224, b225, b226, b227, b228, b229, b230, b231, b232, b233, b234, b235, b236, b237, b238, b239)
	DigitsDT(35)=Array(b240, b241, b242, b243, b244, b245, b246, b247, b248, b249, b250, b251, b252, b253, b254, b255)
	DigitsDT(36)=Array(b256, b257, b258, b259, b260, b261, b262, b263, b264, b265, b266, b267, b268, b269, b270, b271)
	DigitsDT(37)=Array(b272, b273, b274, b275, b276, b277, b278, b279, b280, b281, b282, b283, b284, b285, b286, b287)
	DigitsDT(38)=Array(b288, b289, b290, b291, b292, b293, b294, b295, b296, b297, b298, b299, b300, b301, b302, b303)
	DigitsDT(39)=Array(b304, b305, b306, b307, b308, b309, b310, b311, b312, b313, b314, b315, b316, b317, b318, b319)


 Sub DisplayTimerDT
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 40) then
              For Each obj In DigitsDT(num)
                   If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
			Else
			end if
       Next
    End If
 End Sub



'********************************************
' Hybrid code for VR, Cab, and Desktop
'********************************************

Dim VRThings

if VR_Room = 0 and cab_mode = 0 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
	for each VRThings in DTRails:VRThings.visible = 1:Next
Elseif VR_Room = 0 and cab_mode = 1 Then
	for each VRThings in VRStuff:VRThings.visible = 0:Next
	for each VRThings in VRClock:VRThings.visible = 0:Next
	for each VRThings in VRBackglass:VRThings.visible = 0:Next
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


'********************************************
' Drop Target Shadows
'********************************************

sub DTShadowUpdate
	If gilvl = 0 Then

		dim xx
		for each xx in ShadowDT
			xx.visible=False
		Next

	Else
		If dtsh16on = 1 Then
			dtsh16.visible = True
		Else
			dtsh16.visible = False
		End If

		If dtsh17on = 1 Then
			dtsh17.visible = True
		Else
			dtsh17.visible = False
		End If

		If dtsh26on = 1 Then
			dtsh26.visible = True
		Else
			dtsh26.visible = False
		End If

		If dtsh27on = 1 Then
			dtsh27.visible = True
		Else
			dtsh27.visible = False
		End If

		If dtsh36on = 1 Then
			dtsh36.visible = True
		Else
			dtsh36.visible = False
		End If

		If dtsh37on = 1 Then
			dtsh37.visible = True
		Else
			dtsh37.visible = False
		End If

		If dtsh46on = 1 Then
			dtsh46.visible = True
		Else
			dtsh46.visible = False
		End If

		If dtsh47on = 1 Then
			dtsh47.visible = True
		Else
			dtsh47.visible = False
		End If

		If stsh1on = 0 Then
			stsh1.visible = False
		Else
			stsh1.visible = True
		End If
	End if
End Sub


'*******************************************
'  Ball brightness code
'*******************************************

if BallBrightness = 0 Then
	Table1.BallImage="ball-dark"
	Table1.BallFrontDecal="JPBall-Scratches"
elseif BallBrightness = 1 Then
	Table1.BallImage="ball-light-hf"
	Table1.BallFrontDecal="g5kscratchedmorelight"
else
	Table1.BallImage="ball-lighter-hf"
	Table1.BallFrontDecal="g5kscratchedmorelight"
End if




'*******************************************
'  LUT
'*******************************************

Sub SetLUT
	Table1.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
    LUTBack.visible = 0
	VRLutdesc.visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
    LUTBack.visible = 1
    VRLutdesc.visible = 1

	Select Case LUTSet
        Case 0: LUTBox.text = "Fleep Natural Dark 1": VRLUTdesc.imageA = "LUTcase0"
		Case 1: LUTBox.text = "Fleep Natural Dark 2": VRLUTdesc.imageA = "LUTcase1"
		Case 2: LUTBox.text = "Fleep Warm Dark": VRLUTdesc.imageA = "LUTcase2"
		Case 3: LUTBox.text = "Fleep Warm Bright": VRLUTdesc.imageA = "LUTcase3"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft": VRLUTdesc.imageA = "LUTcase4"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard": VRLUTdesc.imageA = "LUTcase5"
		Case 6: LUTBox.text = "Skitso Natural and Balanced": VRLUTdesc.imageA = "LUTcase6"
		Case 7: LUTBox.text = "Skitso Natural High Contrast": VRLUTdesc.imageA = "LUTcase7"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard": VRLUTdesc.imageA = "LUTcase8"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast": VRLUTdesc.imageA = "LUTcase9"
		Case 10: LUTBox.text = "HauntFreaks Desaturated" : VRLUTdesc.imageA = "LUTcase10"
  		Case 11: LUTBox.text = "Tomate washed out": VRLUTdesc.imageA = "LUTcase11"
        Case 12: LUTBox.text = "VPW original 1on1": VRLUTdesc.imageA = "LUTcase12"
        Case 13: LUTBox.text = "bassgeige": VRLUTdesc.imageA = "LUTcase13"
        Case 14: LUTBox.text = "blacklight": VRLUTdesc.imageA = "LUTcase14"
        Case 15: LUTBox.text = "B&W Comic Book": VRLUTdesc.imageA = "LUTcase15"
        Case 16: LUTBox.text = "Skitso New Warmer LUT": VRLUTdesc.imageA = "LUTcase16"
        Case 17: LUTBox.text = "Original LUT": VRLUTdesc.imageA = "LUTcase17"
	End Select

	LUTBox.TimerEnabled = 1

End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if LUTset = "" then LUTset = 17 'failsafe to original

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "Cactus_Jacks_LUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=17
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "Cactus_Jacks_LUT.txt") then
		LUTset=17
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "Cactus_Jacks_LUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=17
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub

Sub ShowLUT_Init
	LUTBox.Visible = 0
    LUTBack.visible = 0
	VRLutdesc.visible = 0
End Sub

